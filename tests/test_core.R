# Pruebas de decisiones estadísticas y exportación; no requieren navegador.
library(readxl);library(jsonlite);library(vegan);library(iNEXT);library(openxlsx)
source("app/core.R")
stopifnot(isTRUE(all.equal(classic_indices(matrix(c(2,2),1,dimnames=list("A",c("s1","s2"))))$Hill_q1,2)))
d <- data.frame(Fecha=as.Date(c("2025-01-01","2025-01-01","2025-01-02","2025-01-01")),Clase="Aves",Especie=c("s1","s1","s2","s3"),ID=c("p1","p1","p1","p2"),Cobertura=c("A","A","A","B"),Método="Punto",check.names=FALSE)
d[["N° Individuo"]]<-c(10,5,2,3);d<-prepare_records(d)
e <- data.frame(Fecha=as.Date(c("2025-01-01","2025-01-02","2025-01-03","2025-01-01","2025-01-02","2025-01-01")),Clase="Aves",ID=c("p1","p1","p1","p2","p2","p3"),Cobertura=c("A","A","A","B","B","C"),check.names=FALSE)
b <- build_matrices(d,e,"Cobertura",c("ID","Fecha"))
stopifnot(sum(b$abundance)==20,ncol(b$incidence$A)==3,b$incidence$A["s1",1]==1,sum(b$incidence$A["s1",])==1,sum(b$incidence$C)==0)
f<-frequency_table(b);stopifnot(f$Muestras_presencia[f$Comunidad=="A"&f$Especie=="s1"]==1,f$Total_muestras[f$Comunidad=="A"&f$Especie=="s1"]==3)
bt<-beta_analysis(b,"incidence","jaccard");stopifnot(bt$matrix["A","B"]==1,identical(bt$removed,"C"))
set.seed(123);h<-hill_analysis(b,"abundance",c(0,1,2),10,2);stopifnot(length(h$inputs)==2,"C" %in% h$omitted,all(c(0,1,2) %in% h$out$iNextEst$size_based$Order.q))
set.seed(123);hi<-hill_analysis(b,"incidence",c(0,1,2),10,2);stopifnot(hi$inputs$A[1]==3,hi$inputs$A[2]==1)
acc<-accumulation_analysis(b,20);stopifnot(max(acc$Riqueza[acc$Comunidad=="A"])==2)
# Comunidades solapadas con columnas de muestra también son válidas (sin duplicar columnas).
b2<-build_matrices(d,NULL,"ID","ID");stopifnot(ncol(b2$incidence[[1]])==1)
# Debe rechazar registros sin correspondencia con esfuerzo.
bad<-try(build_matrices(d,e[e$Cobertura!="A",],"Cobertura",c("ID","Fecha")),silent=TRUE);stopifnot(inherits(bad,"try-error"))
# Fechas no válidas y conteos negativos no deben procesarse.
x<-d;x[["N° Individuo"]][1]<- -1;stopifnot(inherits(try(prepare_records(x),silent=TRUE),"try-error"))
# Nombres ambiguos no multiplican ni asignan categorías; ninguna coincidencia no implica LC/NE.
cats<-read_catalogs("app/data")
cats$mads<-data.frame(scientificName=c("s1","s1"),threatStatus=c("EN","VU"))
en<-enrich_species(d,cats);stopifnot(nrow(en)==3,en$MADS[en$Especie=="s1"]=="",grepl("Ambigua",en$MADS_estado[en$Especie=="s1"]))
# Prueba integrada con plantilla real y dominios ANLA.
real<-prepare_records(read_excel("app/data/BD_muestra.xlsx",sheet="BD"));real<-real[real$Clase==unique(real$Clase)[1],]
r<-build_matrices(real,NULL,"Cobertura",c("ID","Fecha"));stopifnot(sum(r$abundance)==sum(real$Conteo))
a<-anla_tables(real,enrich_species(real,read_catalogs("app/data")),list(proyecto="Prueba",expediente="",conteos="individuos"),read_anla("app/data"))
stopifnot(sum(a$tables$MuestreoFaunaResultadosTB$ABUND_ABS)==sum(real$Conteo),sum(a$tables$MuestreoFaunaTB$ABUND_ABS)==sum(real$Conteo),nrow(a$issues)>0)
for(cov in unique(a$tables$MuestreoFaunaResultadosTB$N_COBERT)) stopifnot(abs(sum(a$tables$MuestreoFaunaResultadosTB$ABUND_REL[a$tables$MuestreoFaunaResultadosTB$N_COBERT==cov])-100)<1e-9)
# Asegurar que la aplicación se puede cargar y ejecutar un análisis de ejemplo.
old<-getwd();setwd("app");app_env<-new.env(parent=globalenv());app<-sys.source("app.R",envir=app_env)
shiny::testServer(app_env$server,{
 session$setInputs(example=TRUE,sheet="BD",class=unique(real$Clase)[1],groups="Cobertura",units=c("ID","Fecha"),kind="abundance",meaning="individuos",modules=c("classic","frequency","hill","beta","accum"),qs=c("0","1","2"),nboot=10,mult=2,seed=2026,beta_metric="bray",project="Prueba",expediente="",effort_complete=FALSE)
 session$flushReact();session$setInputs(run=1);session$flushReact()
 stopifnot(!is.null(result()),!is.null(result()$hill))
 file<-tempfile(fileext=".xlsx");excel_tables(result(),file);stopifnot(file.exists(file),"MuestreoFaunaTB" %in% readxl::excel_sheets(file));unlink(file)
 for(t in c("1","2","3","accum")) {p<-make_curve(result(),t);f<-tempfile(fileext=".png");ggplot2::ggsave(f,p,width=8,height=5);stopifnot(file.info(f)$size>1000);unlink(f)}
 session$setInputs(kind="incidence");session$flushReact();stopifnot(is.null(result()))
})
setwd(old)
cat("OK: matrices, Hill, beta, acumulación, catálogos, ANLA, Shiny, tablas Excel y figuras.\n")
