# Motor de análisis: funciones independientes de Shiny.
clean_text <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; trimws(gsub("[[:space:]]+", " ", x)) }
name_key <- function(x) tolower(clean_text(x))
label_key <- function(x) tolower(iconv(clean_text(x), to="ASCII//TRANSLIT"))
get_col <- function(d, name, default="") if (name %in% names(d)) clean_text(d[[name]]) else rep(default, nrow(d))
one_value <- function(x) { x <- unique(clean_text(x)); x <- x[nzchar(x)]; if(length(x)==1) x else "" }
parse_dates <- function(x) {
 if(inherits(x,"Date")) return(x)
 if(inherits(x,"POSIXt")) return(as.Date(x))
 if(is.numeric(x)) return(as.Date(x,origin="1899-12-30"))
 x <- clean_text(x); out <- as.Date(rep(NA_character_,length(x)))
 for(f in c("%Y-%m-%d","%d/%m/%Y","%d-%m-%Y")) { j <- is.na(out)&nzchar(x); out[j] <- as.Date(x[j],format=f) }
 out
}
prepare_records <- function(d) {
 names(d) <- clean_text(names(d))
 # Ignore only unnamed, completely empty columns left by spreadsheet formatting.
 unnamed <- !nzchar(names(d)) | grepl("^[.][.][.][0-9]+$", names(d))
 empty <- vapply(d,function(x) all(!nzchar(clean_text(x))),logical(1))
 d <- d[,!(unnamed & empty),drop=FALSE]
 required <- c("Fecha","Clase","Especie","N° Individuo","ID","Cobertura","Método")
 missing <- setdiff(required,names(d)); if(length(missing)) stop("Faltan columnas: ",paste(missing,collapse=", "))
 d <- as.data.frame(d,check.names=FALSE,stringsAsFactors=FALSE)
 d <- d[!Reduce(`&`,lapply(d,function(x) !nzchar(clean_text(x)))),,drop=FALSE]
 if(!nrow(d)) stop("La hoja no contiene registros.")
 for(n in names(d)) if(!inherits(d[[n]],c("Date","POSIXt"))&&!is.numeric(d[[n]])) d[[n]] <- clean_text(d[[n]])
 d$Fecha <- parse_dates(d$Fecha)
 if(anyNA(d$Fecha)) stop("Hay fechas vacías o inválidas. Use AAAA-MM-DD o fechas de Excel.")
 d$Especie <- clean_text(d$Especie); d$Clase <- clean_text(d$Clase)
 d$Conteo <- suppressWarnings(as.numeric(d[["N° Individuo"]]))
 if(any(!is.finite(d$Conteo)|d$Conteo<=0|d$Conteo!=floor(d$Conteo))) stop("N° Individuo debe contener enteros positivos; los muestreos sin detecciones van en Esfuerzo.")
 if(any(!nzchar(d$Especie)|!nzchar(d$Clase)|!nzchar(clean_text(d$ID)))) stop("Especie, Clase e ID no pueden quedar vacíos.")
 d
}
make_key <- function(d,cols) {
 if(!length(cols)) stop("Seleccione al menos una columna.")
 if(length(setdiff(cols,names(d)))) stop("Faltan columnas en la hoja Esfuerzo: ",paste(setdiff(cols,names(d)),collapse=", "))
 vals <- lapply(d[cols],clean_text)
 if(any(vapply(vals,function(x) any(!nzchar(x)),logical(1)))) stop("Hay valores vacíos en las columnas elegidas para agrupar o definir muestras.")
 # Longitud prefijada evita colisiones aun si el contenido tiene separadores.
 do.call(paste,c(lapply(vals,function(x) paste0(nchar(x),":",x)),sep="|"))
}
make_label <- function(d,cols) do.call(paste,c(lapply(d[cols],clean_text),sep=" · "))
build_matrices <- function(d,effort,community_cols,unit_cols) {
 d$Community <- make_key(d,community_cols); d$Unit <- make_key(d,unit_cols)
 d$Label <- make_label(d,community_cols)
 if(is.null(effort)) {
  e <- unique(d[unique(c(community_cols,unit_cols))]); message_note <- "Sin hoja Esfuerzo: solo se conocen las muestras con detecciones. Incidencias y completitud son provisionales."
 } else {
  e <- as.data.frame(effort,check.names=FALSE,stringsAsFactors=FALSE)
  e <- e[!Reduce(`&`,lapply(e,function(x) !nzchar(clean_text(x)))),,drop=FALSE]
  if("Fecha" %in% names(e)) e$Fecha <- parse_dates(e$Fecha)
  message_note <- "Esfuerzo declarado: incluye las muestras sin detecciones."
 }
 e$Community <- make_key(e,community_cols); e$Unit <- make_key(e,unit_cols)
 e$Label <- make_label(e,community_cols)
 e <- unique(e[unique(c(community_cols,unit_cols,"Community","Unit","Label"))])
 if(any(!paste(d$Community,d$Unit) %in% paste(e$Community,e$Unit))) stop("Hay registros sin muestra correspondiente en Esfuerzo.")
 species <- sort(unique(d$Especie)); communities <- unique(e$Community)
 labels <- vapply(communities,function(x) e$Label[match(x,e$Community)],character(1))
 if(anyDuplicated(labels)) stop("Las etiquetas de comunidades no son únicas.")
 abundance <- matrix(0,length(communities),length(species),dimnames=list(labels,species))
 incidence <- vector("list",length(communities)); names(incidence) <- labels
 units <- list()
 for(i in seq_along(communities)) {
  x <- d[d$Community==communities[i],,drop=FALSE]; u <- e$Unit[e$Community==communities[i]]
  m <- matrix(0,length(species),length(u),dimnames=list(species,u))
  if(nrow(x)) for(j in seq_len(nrow(x))) {
   s <- match(x$Especie[j],species); v <- match(x$Unit[j],u)
   abundance[i,s] <- abundance[i,s]+x$Conteo[j]; m[s,v] <- 1
  }
  incidence[[i]] <- m; units[[labels[i]]] <- data.frame(Unidad=u,Comunidad=unname(labels[i]))
 }
 list(abundance=abundance,incidence=incidence,effort=e,records=d,note=message_note,units=units)
}
classic_indices <- function(m) {
 rows <- lapply(seq_len(nrow(m)),function(i) {
  x<-m[i,]; x<-x[x>0]; n<-sum(x); s<-length(x)
  p<-if(n>0) x/n else numeric(); h<-if(n>0) -sum(p*log(p)) else NA_real_; c<-if(n>0) sum(p^2) else NA_real_
  data.frame(Comunidad=rownames(m)[i],N=n,Riqueza=s,Shannon_ln=h,Simpson_concentracion=c,Simpson_1_menos=1-c,Hill_q1=exp(h),Hill_q2=if(n>0) 1/c else NA_real_,Pielou=if(s>1) h/log(s) else NA_real_,Margalef=if(n>1) (s-1)/log(n) else NA_real_,Menhinick=if(n>0) s/sqrt(n) else NA_real_)
 }); do.call(rbind,rows)
}
frequency_table <- function(b) {
 do.call(rbind,lapply(seq_len(nrow(b$abundance)),function(i) {
  x <- b$abundance[i,]; m <- b$incidence[[i]]; n <- ncol(m)
  data.frame(Comunidad=rownames(b$abundance)[i],Especie=colnames(b$abundance),Conteo=as.numeric(x),Porcentaje_conteo=if(sum(x)>0) 100*x/sum(x) else NA_real_,Muestras_presencia=rowSums(m),Total_muestras=n,Frecuencia_incidencia=if(n>0) rowSums(m)/n else NA_real_,row.names=NULL)
 }))
}
beta_analysis <- function(b,kind="incidence",metric="jaccard") {
 m <- if(kind=="incidence") t(vapply(b$incidence,rowSums,numeric(ncol(b$abundance)))) else b$abundance
 if(kind=="incidence") m <- (m>0)*1
 keep <- rowSums(m)>0; removed <- rownames(m)[!keep]; m<-m[keep,,drop=FALSE]
 if(nrow(m)<2) return(list(matrix=NULL,note="Se requieren al menos dos comunidades con detecciones.",removed=removed))
 if(kind=="incidence"&&metric=="bray") metric<-"jaccard"
 if(kind=="abundance"&&metric!="bray") metric<-"bray"
 dis <- vegan::vegdist(m,method=if(metric=="sorensen") "bray" else metric,binary=kind=="incidence")
 mat <- as.matrix(dis)
 list(matrix=mat,dist=dis,metric=metric,note=paste("Disimilitud",metric,"(0 = iguales; 1 = completamente diferentes)."),removed=removed)
}
hill_analysis <- function(b,kind,qs=c(0,1,2),nboot=50,multiplier=2) {
 if(!length(qs)) stop("Seleccione al menos un orden q.")
 if(kind=="abundance") {
  dat<-lapply(seq_len(nrow(b$abundance)),function(i) { x<-b$abundance[i,]; x[x>0] }); names(dat)<-rownames(b$abundance); type<-"abundance"
 } else { dat<-lapply(b$incidence,function(m) c(ncol(m),rowSums(m)[rowSums(m)>0]));type<-"incidence_freq" }
 valid<-vapply(dat,function(x) if(type=="abundance") sum(x)>=2 else x[1]>=2&&length(x)>1,logical(1))
 omitted<-names(dat)[!valid]; dat<-dat[valid]
 if(!length(dat)) stop("Hill requiere al menos 2 individuos o 2 muestras y alguna detección por comunidad.")
 endpoints<-vapply(dat,function(x) ceiling(multiplier*if(type=="abundance") sum(x) else x[1]),numeric(1))
 notes<-character()
 pieces<-lapply(seq_along(dat),function(i) tryCatch(withCallingHandlers(iNEXT::iNEXT(dat[i],q=qs,datatype=type,endpoint=endpoints[i],nboot=nboot,conf=0.95,se=TRUE),warning=function(w) {notes<<-c(notes,paste(names(dat)[i],conditionMessage(w)));invokeRestart("muffleWarning")}),error=function(err) {notes<<-c(notes,paste(names(dat)[i],"no estimable:",conditionMessage(err)));NULL}))
 failed<-vapply(pieces,is.null,logical(1));omitted<-c(omitted,names(dat)[failed]);pieces<-pieces[!failed]
 if(!length(pieces)) return(list(out=NULL,omitted=omitted,type=type,inputs=dat,warnings=unique(notes)))
 out<-pieces[[1]]
 out$DataInfo<-do.call(rbind,lapply(pieces,function(x) x$DataInfo))
 out$AsyEst<-do.call(rbind,lapply(pieces,function(x) x$AsyEst))
 for(n in names(out$iNextEst)) out$iNextEst[[n]]<-do.call(rbind,lapply(pieces,function(x) x$iNextEst[[n]]))
 list(out=out,omitted=omitted,type=type,inputs=dat,warnings=unique(notes))
}
accumulation_analysis <- function(b,permutations=100) {
 result <- list()
 for(n in names(b$incidence)) {
  m<-t(b$incidence[[n]])
  if(nrow(m)<2||sum(m)==0) next
  a<-vegan::specaccum(m,method="random",permutations=permutations)
  result[[n]]<-data.frame(Comunidad=n,Muestras=a$sites,Riqueza=a$richness,SD_permutaciones=a$sd)
 }
 if(!length(result)) return(data.frame())
 do.call(rbind,result)
}
read_catalogs <- function(path="data") {
 read_csv<-function(n) read.csv(file.path(path,n),check.names=FALSE,stringsAsFactors=FALSE,na.strings="")
 list(mads=read_csv("mads_2024.csv"),iucn=read_csv("iucn.csv"),cites=read_csv("cites.csv"),birds=read_csv("aves_2022.csv"),endemic=read_csv("endemicas_candidatas.csv"),synonyms=read_csv("sinonimos.csv"))
}
category_code <- function(x) {
 english<-c("Least Concern"="LC","Near Threatened"="NT","Vulnerable"="VU","Endangered"="EN","Critically Endangered"="CR","Extinct in the Wild"="EW","Extinct"="EX","Data Deficient"="DD","Not Evaluated"="NE")
 vapply(clean_text(x),function(v) {
  if(v %in% names(english)) return(unname(english[v]))
  if(v %in% unname(english)) return(v)
  matches<-regmatches(v,regexpr("\\b(LC|NT|VU|EN|CR|EW|EX|DD|NE)\\b",v,perl=TRUE));if(length(matches)&&nzchar(matches)) matches else ""
 },character(1),USE.NAMES=FALSE)
}
enrich_species <- function(d,cats) {
 sp<-unique(d$Especie)
 result<-lapply(sp,function(s) {
  x<-d[d$Especie==s,,drop=FALSE]; k<-name_key(s); accepted<-s; syn<-cats$synonyms[name_key(cats$synonyms$original)==k,,drop=FALSE]
  synonym_status<-"Nombre original"
  if(nrow(syn)) { ac<-unique(clean_text(syn$aceptado)); if(length(ac)==1&&nzchar(ac)) { accepted<-ac;k<-name_key(ac);synonym_status<-paste("Equivalencia:",one_value(syn$fuente)) } else synonym_status<-"Equivalencia ambigua; no aplicada" }
  unresolved<-grepl("\\b(sp|spp|cf|aff)\\.?\\b",accepted,ignore.case=TRUE,perl=TRUE)
  find<-function(tbl,namecol,valcol) {
   y<-tbl[name_key(tbl[[namecol]])==k,,drop=FALSE];vals<-unique(clean_text(y[[valcol]]));vals<-vals[nzchar(vals)]
   list(value=if(!unresolved&&length(vals)==1) vals else "",status=if(unresolved) "Identificación incompleta" else if(!nrow(y)) "Sin coincidencia" else if(length(vals)!=1) "Ambigua / sin valor" else "Coincidencia exacta")
  }
  md<-find(cats$mads,"scientificName","threatStatus")
  iu<-cats$iucn[grepl("Global",cats$iucn$scopes,ignore.case=TRUE)&!is.na(cats$iucn$scopes),,drop=FALSE]
  iu<-iu[name_key(iu$scientificName)==k,,drop=FALSE]
  if(nrow(iu)&&any(is.finite(suppressWarnings(as.numeric(iu$yearPublished))))) iu<-iu[as.numeric(iu$yearPublished)==max(as.numeric(iu$yearPublished),na.rm=TRUE),,drop=FALSE]
  ic<-find(iu,"scientificName","redlistCategory")
  ci<-find(cats$cites,"FullName","CurrentListing")
  bird<-find(cats$birds,"Nombre 2022","Estado2022")
  data.frame(Especie=s,Nombre_cruce=accepted,Equivalencia=synonym_status,Clase=one_value(x$Clase),Orden=one_value(get_col(x,"Orden")),Familia=one_value(get_col(x,"Familia")),Genero=one_value(get_col(x,"Genero")),Nombre_comun=one_value(get_col(x,"N. común")),IUCN=category_code(ic$value),IUCN_estado=ic$status,IUCN_ano=one_value(iu$yearPublished),MADS=category_code(md$value),MADS_estado=md$status,CITES=ci$value,CITES_estado=ci$status,CITES_anotacion=if(ci$status=="Coincidencia exacta") one_value(cats$cites$FullAnnotationEnglish[name_key(cats$cites$FullName)==k]) else "",Migracion_catalogo=bird$value,Endemismo_candidato=if(!unresolved&&k %in% name_key(cats$endemic$scientificName)) "Candidato: requiere verificación" else "Sin coincidencia",stringsAsFactors=FALSE)
 });do.call(rbind,result)
}
read_anla <- function(path="data") list(schema=jsonlite::fromJSON(file.path(path,"anla_schema.json")),domains=jsonlite::fromJSON(file.path(path,"anla_domains.json")))
domain_value <- function(x,dom,domains) {
 values<-domains[[dom]]; if(is.null(values)) return(NA_real_)
 x<-one_value(x); if(!nzchar(x)) return(NA_real_)
 numeric_x<-suppressWarnings(as.numeric(x)); if(is.finite(numeric_x)&&numeric_x %in% values$code) return(numeric_x)
 idx<-match(label_key(x),label_key(values$label)); if(is.na(idx)) NA_real_ else values$code[idx]
}
anla_tables <- function(d,enriched,config,anla) {
 numeric_cat<-function(x) unname(c(LC=321,NT=322,VU=323,EN=324,CR=325,EW=326,EX=327,DD=328,NE=329)[x])
 blank<-function(table,n) { fields<-anla$schema[[table]]; out<-as.data.frame(setNames(lapply(fields$field,function(x) rep(NA,n)),fields$field),check.names=FALSE);out }
 taxonomy<-function(x) list(DIVISION=one_value(get_col(x,"Division","Chordata")),CLASE=one_value(x$Clase),ORDEN=one_value(get_col(x,"Orden")),FAMILIA=one_value(get_col(x,"Familia")),GENERO=one_value(get_col(x,"Genero")),ESPECIE=one_value(x$Especie),N_COMUN=one_value(get_col(x,"N. común")))
 # ANLA table rows aggregate per point/transect, species and determination.
 d$ID_MUES_PT<-get_col(d,"ID_MUES_PT");d$ID_MUES_TR<-get_col(d,"ID_MUES_TR")
 kind<-label_key(get_col(d,"Tipo_unidad"));d$ID_MUES_PT[!nzchar(d$ID_MUES_PT)&kind=="punto"]<-d$ID[!nzchar(d$ID_MUES_PT)&kind=="punto"]
 d$ID_MUES_TR[!nzchar(d$ID_MUES_TR)&kind=="transecto"]<-d$ID[!nzchar(d$ID_MUES_TR)&kind=="transecto"]
 d$DETERM_FINAL<-vapply(seq_len(nrow(d)),function(i) {
  direct<-domain_value(get_col(d,"DETERM")[i],"Dom_Deter",anla$domains)
  if(!is.na(direct)) return(direct)
  domain_value(get_col(d,"T. Registro")[i],"Dom_Deter",anla$domains)
 },numeric(1))
 key<-paste(d$ID,d$Especie,get_col(d,"T. Registro"),d$DETERM_FINAL,sep="\r")
 groups<-split(seq_len(nrow(d)),key);tb<-blank("MuestreoFaunaTB",length(groups))
 j<-0
 for(ix in groups) { j<-j+1;x<-d[ix,,drop=FALSE]; tx<-taxonomy(x);for(n in names(tx)) tb[j,n]<-tx[[n]]
  tb$EXPEDIENTE[j]<-config$expediente;tb$ID_MUES_PT[j]<-one_value(x$ID_MUES_PT);tb$ID_MUES_TR[j]<-one_value(x$ID_MUES_TR);tb$DETERM[j]<-unique(x$DETERM_FINAL)[1];tb$OT_DETERM[j]<-one_value(get_col(x,"OT_DETERM"));tb$ABUND_ABS[j]<-sum(x$Conteo);tb$OBSERV[j]<-one_value(get_col(x,"Observación"))
 }
 groups<-split(seq_len(nrow(d)),paste(d$Cobertura,d$Especie,sep="\r"));res<-blank("MuestreoFaunaResultadosTB",length(groups)); j<-0
 totals<-tapply(d$Conteo,paste(d$Clase,d$Cobertura,sep="\r"),sum)
 for(ix in groups) {j<-j+1;x<-d[ix,,drop=FALSE];tx<-taxonomy(x);for(n in names(tx)) res[j,n]<-tx[[n]]
  en<-enriched[match(tx$ESPECIE,enriched$Especie),,drop=FALSE]
  res$EXPEDIENTE[j]<-config$expediente;res$PROYECTO[j]<-config$proyecto;res$N_COBERT[j]<-one_value(x$Cobertura)
  res$NOMENCLAT[j]<-suppressWarnings(as.numeric(one_value(get_col(x,"NOMENCLAT"))))
  res$CATEG_UICN[j]<-numeric_cat(en$IUCN);res$CATE_MINIS[j]<-numeric_cat(en$MADS)
  res$CATEG_CIT[j]<-unname(c(I=100,II=200,III=300)[en$CITES])
  # Ausencia de coincidencia no equivale a No aplica o NE; quedan pendientes.
  res$T_DISTRIB[j]<-domain_value(get_col(x,"T_DISTRIB"),"Dom_Tipo_Distribu",anla$domains)
  for(n in c("MIGRACION","TIPO_MIGR","VEDA","ENTID_VEDA","VIGEN_VEDA")) {
   dom<-anla$schema$MuestreoFaunaResultadosTB$domain[match(n,anla$schema$MuestreoFaunaResultadosTB$field)]
   res[j,n]<-domain_value(get_col(x,n),dom,anla$domains)
  }
  res$USO[j]<-domain_value(get_col(x,"Uso"),"Dom_Uso_Fauna",anla$domains)
  res$DIETA[j]<-domain_value(get_col(x,"Gremio"),"Dom_Dieta",anla$domains)
  res$DISTR_ALT[j]<-one_value(get_col(x,"Dist_Alt"));if(!nzchar(res$DISTR_ALT[j])) res$DISTR_ALT[j]<-"Sin información"
  res$RESOLUCION[j]<-one_value(get_col(x,"RESOLUCION"));res$ABUND_ABS[j]<-sum(x$Conteo)
  total<-totals[[paste(tx$CLASE,res$N_COBERT[j],sep="\r")]];res$ABUND_REL[j]<-100*res$ABUND_ABS[j]/total
  res$FECHA_IMUE[j]<-as.character(min(d$Fecha[d$Cobertura==res$N_COBERT[j]]));res$FECHA_FMUE[j]<-as.character(max(d$Fecha[d$Cobertura==res$N_COBERT[j]]));res$OBSERV[j]<-one_value(get_col(x,"Observación"))
 }
 tables<-list(MuestreoFaunaTB=tb,MuestreoFaunaResultadosTB=res)
 issues<-list(); add<-function(table,row,field,msg) {issues[[length(issues)+1]]<<-data.frame(Tabla=table,Fila=row,Campo=field,Problema=msg)}
 for(t in names(tables)) {
  tab<-tables[[t]]; sch<-anla$schema[[t]]
  for(i in seq_len(nrow(tab))) for(j in seq_len(nrow(sch))) {
   f<-sch$field[j];v<-tab[[f]][i];empty<-is.na(v)||!nzchar(clean_text(v))
   if(empty&&sch$requirement[j]=="Obligatorio") add(t,i,f,"Campo obligatorio pendiente")
   if(!empty&&sch$type[j] %in% c("Integer","SmallInteger")&&(!is.finite(suppressWarnings(as.numeric(v)))||as.numeric(v)!=floor(as.numeric(v)))) add(t,i,f,"Se requiere un entero válido")
   if(!empty&&sch$type[j]=="String"&&nchar(as.character(v))>sch$size[j]) add(t,i,f,"Supera longitud del diccionario")
   if(!empty&&sch$domain[j]!="N/A"&&!is.null(anla$domains[[sch$domain[j]]])&&!v %in% anla$domains[[sch$domain[j]]]$code) add(t,i,f,"Código fuera del dominio")
  }
 }
 for(i in seq_len(nrow(tb))) { nids<-sum(nzchar(clean_text(c(tb$ID_MUES_PT[i],tb$ID_MUES_TR[i])))); if(nids!=1) add("MuestreoFaunaTB",i,"ID_MUES_PT / ID_MUES_TR","Debe indicar exactamente un punto o transecto; geometrías se incorporan por separado") }
 if(config$conteos!="individuos") add("Ambas",NA,"ABUND_ABS","Conteos declarados como detecciones: no equivalen automáticamente a individuos ANLA")
 list(tables=tables,issues=if(length(issues)) do.call(rbind,issues) else data.frame(Tabla=character(),Fila=integer(),Campo=character(),Problema=character()))
}
