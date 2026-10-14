library(shiny)
library(readxl)
library(openxlsx)
library(jsonlite)
library(vegan)
library(iNEXT)
library(ggplot2)
library(zip)
# Ajuste oficial de Posit para descargas de Shinylive en Chromium.
# https://shiny.posit.co/r/components/inputs/download-button/
downloadButton <- function(...) {
 tag <- shiny::downloadButton(...)
 tag$attribs$download <- NULL
 tag
}
source("core.R",local=TRUE)
source("template.R",local=TRUE)
source("methods.R",local=TRUE)
options(shiny.maxRequestSize=30*1024^2)
catalogs <- read_catalogs()
anla <- read_anla()
css <- "body{background:#f3f6f1;color:#23362d;font-family:system-ui,sans-serif}.container-fluid{max-width:1450px;margin:auto}.well{background:white;border:1px solid #d9e3d7;border-radius:14px;box-shadow:none}.hero{background:#163e32;color:white;padding:30px;border-radius:18px;margin:24px 0}.hero h1{font-size:32px;margin:0 0 10px}.hero p{max-width:850px;color:#d9e9db}.eyebrow{text-transform:uppercase;letter-spacing:3px;color:#b7d5a6;font-size:12px}.btn-primary{background:#276445;border-color:#276445}.nav-tabs>li.active>a{color:#276445;font-weight:600}.panelbox{padding:20px;background:white;border:1px solid #d9e3d7;border-radius:12px;margin:16px 0}.note{border-left:4px solid #8ca95e;background:#eff4e8;padding:12px;margin:12px 0}table{font-size:12px}h3{font-size:21px}.shiny-output-error-validation{color:#7c4a28}.small{font-size:12px;color:#53665c}.table-wrap{overflow:auto;max-height:550px}label{font-weight:600}.brand{float:right;margin:0 0 16px 28px;padding:12px;background:white;border-radius:12px;width:260px}.brand img{display:block;width:100%;height:auto}.hero:after{content:'';display:table;clear:both}.methods-copy p{line-height:1.75;font-size:15px;margin-bottom:18px}.methods-state{font-weight:600;color:#276445}@media(max-width:650px){.brand{float:none;margin:0 0 20px;width:210px}}"
ui <- fluidPage(tags$head(tags$style(HTML(css))),
 div(class="hero",div(class="brand",tags$img(src="cunaguaro.png",alt="Cunaguaro — Biodiversidad y Cultura")),div(class="eyebrow","Ecología · Inventarios · ANLA"),h1("Diversidad de fauna"),p("De los registros de campo a resultados reproducibles. Compara comunidades, define tus muestras y exporta análisis y tablas por grupo taxonómico.")),
 sidebarLayout(sidebarPanel(width=3,
  h3("1 · Datos"),fileInput("file","Cargar Excel",accept=c(".xlsx")),checkboxInput("example","Usar ejemplo ficticio (Narnia y Matrix)",TRUE),uiOutput("sheet_ui"),
  downloadButton("template","Plantilla básica"),tags$br(),tags$br(),downloadButton("template_effort","Plantilla con esfuerzo"),tags$hr(),uiOutput("classes_ui"),
  h3("2 · Diseño"),uiOutput("group_ui"),uiOutput("units_ui"),
  selectInput("kind","Tipo de datos",c("Abundancias / conteos"="abundance","Incidencias por muestra"="incidence")),
  selectInput("meaning","Los conteos representan",c("Individuos"="individuos","Detecciones o eventos"="detecciones")),
  checkboxInput("effort_complete","Confirmo que Esfuerzo incluye todas las muestras de este grupo, incluso sin detecciones",FALSE),
  tags$hr(),h3("3 · Análisis"),checkboxGroupInput("modules","Calcular",c("Índices clásicos"="classic","Frecuencias"="frequency","Hill"="hill","Diversidad beta"="beta","Acumulación de especies"="accum"),selected=c("classic","frequency","hill","beta","accum")),
  checkboxGroupInput("qs","Órdenes de Hill",c("q = 0"="0","q = 1"="1","q = 2"="2"),selected=c("0","1","2"),inline=TRUE),
  numericInput("nboot","Remuestreos para Hill",50,min=10,max=500,step=10),numericInput("mult","Extrapolar hasta × esfuerzo observado",2,min=1,max=2,step=.25),numericInput("seed","Semilla reproducible",2026,min=1,step=1),
  selectInput("beta_metric","Índice beta",c("Jaccard (incidencia)"="jaccard","Sørensen (incidencia)"="sorensen","Bray–Curtis (abundancia)"="bray")),
  tags$hr(),h3("4 · Estilo de gráficos"),
  sliderInput("font_title","Título",min=8,max=32,value=16),
  sliderInput("font_axis_title","Nombres de ejes",min=8,max=28,value=13),
  sliderInput("font_axis_text","Valores de ejes",min=6,max=24,value=11),
  sliderInput("font_legend","Leyenda",min=6,max=24,value=11),
  sliderInput("font_facet","Etiquetas de paneles (q)",min=8,max=28,value=13),
  sliderInput("font_caption","Notas de figura",min=6,max=18,value=9),
  numericInput("export_width","Ancho exportado (pulgadas)",11,min=4,max=24),
  numericInput("export_height","Alto exportado (pulgadas)",7,min=3,max=20),
  numericInput("export_dpi","Resolución PNG (dpi)",180,min=72,max=600),
  tags$hr(),textInput("project","Proyecto"),textInput("expediente","Expediente (si corresponde)"),actionButton("run","Analizar grupo seleccionado",class="btn-primary",width="100%"),
  tags$p(class="small","R se ejecuta en tu navegador en la versión GitHub Pages. Los archivos de campo no se envían a un servidor de análisis.")),
 mainPanel(width=9,
  div(class="note",strong("Una comunidad no es una muestra.")," Por ejemplo: comparar coberturas; definir muestras como ID × Fecha; analizar individuos o presencias. Las muestras sin detecciones se declaran en Esfuerzo."),
  verbatimTextOutput("status"),
  tabsetPanel(id="tab",
   tabPanel("Datos",div(class="panelbox",h3("Registros del grupo"),div(class="table-wrap",tableOutput("data_preview"))),div(class="panelbox",h3("Esfuerzo declarado"),div(class="table-wrap",tableOutput("effort_preview")))),
   tabPanel("Resumen",div(class="panelbox",h3("Índices clásicos sobre conteos"),div(class="table-wrap",tableOutput("classic_table"))),div(class="panelbox",h3("Frecuencias"),div(class="table-wrap",tableOutput("frequency_table")))),
   tabPanel("Curvas",selectInput("curve","Mostrar",c("Hill · tamaño de muestra"="1","Hill · completitud"="2","Hill · cobertura común"="3","Acumulación de especies"="accum")),plotOutput("curves",height="540px"),verbatimTextOutput("hill_notes"),div(class="table-wrap",tableOutput("hill_info"))),
   tabPanel("Beta",verbatimTextOutput("beta_note"),plotOutput("beta_plot",height="480px"),div(class="table-wrap",tableOutput("beta_table"))),
   tabPanel("Catálogos",div(class="note","Catálogos de referencia suministrados: MADS 2024, CITES enero 2025, aves 2022 y UICN con año registrado. No hay actualización automática. CITES con anotaciones requiere revisión. Endemismo se presenta como candidato."),div(class="table-wrap",tableOutput("species_table"))),
   tabPanel("ANLA",div(class="note","Se generan las tablas MuestreoFaunaTB y MuestreoFaunaResultadosTB. Las capas de puntos y transectos se incorporan por separado. Una exportación con pendientes es un borrador, no una GDB validada."),h3("Pendientes y validación"),div(class="table-wrap",tableOutput("anla_issues")),h3("Resultados por cobertura"),div(class="table-wrap",tableOutput("anla_results"))),
   tabPanel("Descargas",div(class="panelbox",h3("Paquete de resultados"),p("Incluye tablas Excel, matrices, curvas PNG/PDF cuando se calcularon, registros preparados, tablas ANLA, catálogos cruzados, validación y parámetros."),downloadButton("results_zip","Descargar ZIP"),tags$br(),tags$br(),downloadButton("excel_results","Descargar tablas Excel")),div(class="panelbox",h3("Exportar una .gdb real"),p("El notebook de PC puede generar la GDB desde R con tools/export_gdb.R y sf/GDAL. También se conserva el conversor Python. La exportación se ejecuta fuera del navegador; no genera geometrías ni completa campos pendientes."))),
   tabPanel("Métodos empleados",div(class="panelbox",h3("Redacción automática de métodos"),
    p(class="small","El borrador se actualiza con las opciones. Después de Analizar, el texto describe la ejecución vigente y registra exclusiones y advertencias."),
    textOutput("methods_state"),tags$br(),uiOutput("methods_preview"),downloadButton("methods_txt","Descargar métodos (.txt)"))),
   tabPanel("Guía de métodos",div(class="panelbox",h3("Qué calcula esta versión"),tags$ul(
    tags$li("Abundancia: suma de conteos por especie y comunidad. Si son detecciones, q > 0 describe la distribución de detecciones, no abundancia poblacional."),
    tags$li("Incidencia: cada especie cuenta una vez por muestra. iNEXT recibe el número total de muestras seguido de las frecuencias de incidencia."),
    tags$li("Hill: q = 0 riqueza; q = 1 diversidad efectiva de Shannon; q = 2 diversidad efectiva de Simpson. Intervalos bootstrap al 95%."),
    tags$li("Acumulación: vegan::specaccum, orden aleatorio, con desviación entre permutaciones; esa banda no es un intervalo de confianza."),
    tags$li("Beta: composición observada por comunidad. Las disimilitudes no están corregidas por completitud ni detectabilidad. Comunidades vacías se excluyen y se reportan."),
    tags$li("Los catálogos usan coincidencia exacta normalizada o equivalencias explícitas de data/sinonimos.csv; no hay asignación automática a sp., cf. o aff."),
    tags$li("ANLA: abundancia relativa dentro de cada cobertura y clase, en porcentaje. Campos desconocidos quedan pendientes; no se asigna NE ni No aplica por ausencia de coincidencia.")),
    p("Referencias: Chao et al. (2014), Ecological Monographs 84:45–67, doi:10.1890/13-0133.1; documentación de iNEXT y vegan; diccionario ANLA suministrado.")))
  )
 )))
server <- function(input,output,session) {
 source_path <- reactive({if(!is.null(input$file)) input$file$datapath else if(isTRUE(input$example)) "data/BD_muestra.xlsx" else NULL})
 sheets <- reactive({req(source_path());readxl::excel_sheets(source_path())})
 output$sheet_ui<-renderUI({req(sheets());selectInput("sheet","Hoja de registros",sheets(),selected=if("BD" %in% sheets()) "BD" else sheets()[1])})
 raw<-reactive({req(source_path(),input$sheet);prepare_records(readxl::read_excel(source_path(),sheet=input$sheet))})
 effort<-reactive({
  req(source_path());if(!"Esfuerzo" %in% sheets()) return(NULL)
  e<-as.data.frame(readxl::read_excel(source_path(),sheet="Esfuerzo"),check.names=FALSE)
  if(!nrow(e)) return(NULL)
  e<-e[!Reduce(`&`,lapply(e,function(x) !nzchar(clean_text(x)))),,drop=FALSE]
  if(!nrow(e)) NULL else e
 })
 output$classes_ui<-renderUI({req(raw());selectInput("class","Grupo taxonómico",sort(unique(raw()$Clase)))})
 selected<-reactive({req(raw(),input$class);raw()[raw()$Clase==input$class,,drop=FALSE]})
 selected_effort<-reactive({e<-effort();if(is.null(e)) return(NULL);if(!"Clase" %in% names(e)) stop("Esfuerzo debe incluir Clase para identificar las muestras realizadas para cada grupo.");e<-e[clean_text(e$Clase)==input$class,,drop=FALSE];if(!nrow(e)) stop("Esfuerzo no contiene muestras del grupo seleccionado.");e})
 options_cols<-reactive({setdiff(names(raw()),c("Especie","N° Individuo","Conteo","Observación"))})
 output$group_ui<-renderUI({req(options_cols());selectInput("groups","Comparar comunidades por",options_cols(),selected="Cobertura",multiple=TRUE)})
 output$units_ui<-renderUI({req(options_cols());selectInput("units","Definir muestras por",options_cols(),selected=c("ID","Fecha"),multiple=TRUE)})
 output$data_preview<-renderTable({head(selected(),25)},striped=TRUE)
 output$effort_preview<-renderTable({e<-selected_effort();if(is.null(e)) data.frame(Aviso="No hay hoja Esfuerzo. Completar la plantilla para analizar incidencias.") else head(e,25)},striped=TRUE)
 result<-reactiveVal(NULL);status<-reactiveVal("Carga tus registros o explora el ejemplo. Pulsa Analizar después de cambiar las opciones.")
 observeEvent(input$run,{
  result(NULL)
  tryCatch({
   d<-selected();e<-selected_effort()
   if(!length(input$modules)) stop("Seleccione al menos un análisis.")
   if(input$kind=="incidence"&&(is.null(e)||!isTRUE(input$effort_complete))) stop("Para Hill por incidencia, complete Esfuerzo y confirme que incluye todas las muestras del grupo.")
   if(!is.finite(input$nboot)||input$nboot<10||input$nboot>500) stop("Remuestreos debe estar entre 10 y 500.")
   if(!is.finite(input$mult)||input$mult<1||input$mult>2) stop("El multiplicador debe estar entre 1 y 2.")
   withProgress(message="Analizando en R",value=0,{
    b<-build_matrices(d,e,input$groups,input$units);set.seed(input$seed);incProgress(.15)
    en<-enrich_species(d,catalogs);an<-anla_tables(d,en,list(proyecto=input$project,expediente=input$expediente,conteos=input$meaning),anla)
    a<-list(b=b,enriched=en,anla=an,params=list(grupo=input$class,comunidades=input$groups,muestras=input$units,tipo=input$kind,conteos=input$meaning,q=as.numeric(input$qs),nboot=input$nboot,extrapolacion=input$mult,semilla=input$seed,proyecto=input$project,expediente=input$expediente,esfuerzo_confirmado=input$effort_complete,hoja_esfuerzo=!is.null(e),fecha=as.character(Sys.time()),R=R.version.string,iNEXT=as.character(packageVersion("iNEXT")),vegan=as.character(packageVersion("vegan"))))
    a$warnings<-character()
    if("classic" %in% input$modules) a$classic<-classic_indices(b$abundance)
    if("frequency" %in% input$modules) a$frequencies<-frequency_table(b)
    incProgress(.15)
    if("hill" %in% input$modules) {a$hill<-hill_analysis(b,input$kind,as.numeric(input$qs),input$nboot,input$mult);a$warnings<-c(a$warnings,a$hill$warnings)}
    incProgress(.45)
    if("beta" %in% input$modules) a$beta<-beta_analysis(b,input$kind,input$beta_metric)
    if("accum" %in% input$modules) a$accum<-accumulation_analysis(b)
    if(is.null(e)) a$warnings<-c(a$warnings,"Frecuencias y acumulación omiten muestras sin detecciones; proporcionar Esfuerzo.")
    if(input$meaning=="detecciones") a$warnings<-c(a$warnings,"Conteos de detecciones: no interpretar como abundancia de individuos.")
    a$settings<-list(class=input$class,groups=input$groups,units=input$units,kind=input$kind,meaning=input$meaning,modules=input$modules,qs=input$qs,nboot=input$nboot,mult=input$mult,seed=input$seed,beta_metric=input$beta_metric,project=input$project,expediente=input$expediente,effort_complete=input$effort_complete)
    result(a);incProgress(.25)
   })
   status(paste("Completado:",nrow(d),"registros ·",length(unique(d$Especie)),"especies ·",nrow(result()$b$abundance),"comunidades.",result()$b$note,paste(result()$warnings,collapse="\n"),sep="\n"));updateTabsetPanel(session,"tab",selected="Resumen")
  },error=function(err) status(paste("No se pudo completar:",conditionMessage(err))))
 })
 # Invalida resultados cuando cambian datos u opciones; evita exportar resultados obsoletos.
 observeEvent(list(input$file,input$example,input$sheet,input$class,input$groups,input$units,input$kind,input$meaning,input$modules,input$qs,input$nboot,input$mult,input$seed,input$beta_metric,input$project,input$expediente,input$effort_complete),{if(!is.null(result())) {result(NULL);status("Opciones modificadas. Pulsa Analizar para actualizar resultados.")}},ignoreInit=TRUE)
 current_method_config<-reactive(list(class=input$class,groups=input$groups,units=input$units,kind=input$kind,meaning=input$meaning,modules=input$modules,qs=input$qs,nboot=input$nboot,mult=input$mult,seed=input$seed,beta_metric=input$beta_metric,effort_complete=input$effort_complete))
 methods_current<-reactive({a<-result();if(is.null(a)) method_summary(current_method_config()) else method_summary(a$settings,a)})
 output$methods_state<-renderText(if(is.null(result())) "Borrador de configuración: pendiente de ejecutar." else "Métodos correspondientes a los resultados vigentes.")
 output$methods_preview<-renderUI({parts<-strsplit(methods_current(),"\n\n",fixed=TRUE)[[1]];div(class="methods-copy",lapply(parts,function(x) tags$p(x)))})
 output$methods_txt<-downloadHandler(filename=function() if(is.null(result())) "Metodos_borrador.txt" else paste0("Metodos_",result()$params$grupo,".txt"),content=function(file) writeLines(methods_current(),file,useBytes=TRUE))
 output$status<-renderText(status())
 output$classic_table<-renderTable({req(result());shiny::validate(shiny::need(!is.null(result()$classic),"Selecciona Índices clásicos y vuelve a analizar."));result()$classic},digits=4,striped=TRUE)
 output$frequency_table<-renderTable({req(result());shiny::validate(shiny::need(!is.null(result()$frequencies),"Selecciona Frecuencias y vuelve a analizar."));head(result()$frequencies,100)},digits=4,striped=TRUE)
 plot_settings<-function() {
  v<-function(x,default) if(is.null(x)||!length(x)||!is.finite(x)) default else as.numeric(x)
  list(title=v(input$font_title,16),axis_title=v(input$font_axis_title,13),axis_text=v(input$font_axis_text,11),legend=v(input$font_legend,11),facet=v(input$font_facet,13),caption=v(input$font_caption,9),width=v(input$export_width,11),height=v(input$export_height,7),dpi=v(input$export_dpi,180))
 }
 plot_theme<-function() {
  z<-plot_settings()
  ggplot2::theme_minimal(base_size=z$axis_text)+ggplot2::theme(
   plot.title=ggplot2::element_text(size=z$title),plot.subtitle=ggplot2::element_text(size=z$caption+1),
   axis.title=ggplot2::element_text(size=z$axis_title),axis.text=ggplot2::element_text(size=z$axis_text),
   legend.title=ggplot2::element_text(size=z$legend),legend.text=ggplot2::element_text(size=z$legend),
   strip.text=ggplot2::element_text(size=z$facet),plot.caption=ggplot2::element_text(size=z$caption))
 }
 make_curve<-function(a,type) {
  if(type=="accum") {
   if(is.null(a$accum)||!nrow(a$accum)) stop("Sin acumulación: se requieren al menos dos muestras con alguna detección.")
   ggplot(a$accum,aes(Muestras,Riqueza,color=Comunidad,fill=Comunidad))+geom_ribbon(aes(ymin=pmax(0,Riqueza-SD_permutaciones),ymax=Riqueza+SD_permutaciones),alpha=.12,color=NA)+geom_line(linewidth=.8)+labs(x="Número de muestras",y="Riqueza observada",color="Comunidad",fill="Comunidad",subtitle="Banda: ±1 desviación estándar entre órdenes aleatorios")+plot_theme()
  } else {
   if(is.null(a$hill)||is.null(a$hill$out)) stop("Hill no disponible. Revisa las advertencias y el tamaño de muestra.")
   iNEXT::ggiNEXT(a$hill$out,type=as.integer(type),facet.var="Order.q",color.var="Assemblage")+plot_theme()+
    labs(x=if(as.integer(type)==3) "Cobertura del muestreo" else if(a$hill$type=="abundance") "Número de individuos o detecciones" else "Número de unidades de muestreo",
         y=if(as.integer(type)==2) "Cobertura del muestreo" else "Diversidad (número efectivo de especies)",
         color="Comunidad",fill="Comunidad",linetype="Método",shape="Método",
         caption="Intervalos bootstrap al 95%; interpretación condicionada al diseño y al tipo de conteos.")
  }
 }
 output$curves<-renderPlot({req(result());tryCatch(print(make_curve(result(),input$curve)),error=function(e) shiny::validate(shiny::need(FALSE,conditionMessage(e))))})
 output$hill_notes<-renderText({req(result());if(is.null(result()$hill)) return("");paste("Datos:",result()$hill$type,"\nComunidades omitidas por tamaño insuficiente:",paste(result()$hill$omitted,collapse=", "))})
 output$hill_info<-renderTable({req(result()$hill);result()$hill$out$DataInfo},digits=4)
 output$beta_note<-renderText({req(result()$beta);paste(result()$beta$note,"\nComunidades vacías excluidas:",paste(result()$beta$removed,collapse=", "))})
 output$beta_table<-renderTable({req(result()$beta$matrix);data.frame(Comunidad=rownames(result()$beta$matrix),result()$beta$matrix,check.names=FALSE)},digits=4)
 beta_plot<-function(a) {if(is.null(a$beta$matrix)) stop("Beta no disponible.");m<-a$beta$matrix;x<-as.data.frame(as.table(m));names(x)<-c("Comunidad1","Comunidad2","Disimilitud");ggplot(x,aes(Comunidad1,Comunidad2,fill=Disimilitud))+geom_tile()+scale_fill_gradient(low="#f0f5e9",high="#1c6249",limits=c(0,1))+coord_equal()+plot_theme()+theme(axis.text.x=element_text(angle=45,hjust=1))+labs(x="Comunidad",y="Comunidad",fill="Disimilitud",title=paste("Diversidad beta ·",a$beta$metric))}
 output$beta_plot<-renderPlot({req(result()$beta$matrix);print(beta_plot(result()))})
 output$species_table<-renderTable({req(result());result()$enriched},striped=TRUE)
 output$anla_issues<-renderTable({req(result());if(nrow(result()$anla$issues)) result()$anla$issues else data.frame(Estado="Sin pendientes detectados en tablas. Verificar relaciones con las capas y condiciones de entrega.")},striped=TRUE)
 output$anla_results<-renderTable({req(result());result()$anla$tables$MuestreoFaunaResultadosTB},digits=4,striped=TRUE)
 output$template<-downloadHandler(filename=function() "Plantilla_diversidad_basica.xlsx",content=function(file) write_input_template(file,include_effort=FALSE))
 output$template_effort<-downloadHandler(filename=function() "Plantilla_diversidad_con_esfuerzo.xlsx",content=function(file) write_input_template(file,include_effort=TRUE))
 excel_tables<-function(a,file) {
  wb<-createWorkbook();add<-function(n,d) {if(is.null(d)||!nrow(d)) return();addWorksheet(wb,n);writeData(wb,n,d);freezePane(wb,n,firstRow=TRUE)}
  method_parts<-strsplit(method_summary(a$settings,a),"\n\n",fixed=TRUE)[[1]]
  add("Metodos",data.frame(Parrafo=method_parts,stringsAsFactors=FALSE))
  openxlsx::setColWidths(wb,"Metodos",cols=1,width=110)
  openxlsx::addStyle(wb,"Metodos",openxlsx::createStyle(wrapText=TRUE,valign="top"),rows=seq_along(method_parts)+1,cols=1,gridExpand=TRUE)
  openxlsx::setRowHeights(wb,"Metodos",rows=seq_along(method_parts)+1,heights=pmin(350,pmax(30,ceiling(nchar(method_parts)/100)*17)))
  add("Clasicos",a$classic);add("Frecuencias",a$frequencies);add("Especies_catalogos",a$enriched);add("Amenazadas",a$enriched[a$enriched$IUCN %in% c("VU","EN","CR")|a$enriched$MADS %in% c("VU","EN","CR"),,drop=FALSE]);add("Pendientes_ANLA",a$anla$issues)
  for(n in names(a$anla$tables)) add(n,a$anla$tables[[n]])
  add("Abundancias",data.frame(Comunidad=rownames(a$b$abundance),a$b$abundance,check.names=FALSE))
  if(!is.null(a$hill)) {add("Hill_DataInfo",a$hill$out$DataInfo);add("Hill_Asintoticos",a$hill$out$AsyEst);for(n in names(a$hill$out$iNextEst)) add(paste0("Hill_",n),a$hill$out$iNextEst[[n]])}
  if(!is.null(a$beta$matrix)) add("Beta",data.frame(Comunidad=rownames(a$beta$matrix),a$beta$matrix,check.names=FALSE))
  add("Acumulacion",a$accum);saveWorkbook(wb,file,overwrite=TRUE)
 }
 output$excel_results<-downloadHandler(filename=function() paste0("Resultados_",input$class,".xlsx"),content=function(file) {req(result());excel_tables(result(),file)})
 output$results_zip<-downloadHandler(filename=function() paste0("Diversidad_",input$class,".zip"),content=function(file) {
  req(result());a<-result();a$params$estilo_grafico<-plot_settings();folder<-tempfile("diversidad_");dir.create(folder);on.exit(unlink(folder,recursive=TRUE),add=TRUE)
  writeLines(method_summary(a$settings,a),file.path(folder,"Metodos_empleados.txt"),useBytes=TRUE)
  excel_tables(a,file.path(folder,"Resultados.xlsx"));jsonlite::write_json(a$params,file.path(folder,"parametros.json"),pretty=TRUE,auto_unbox=TRUE)
  write.csv(a$b$records,file.path(folder,"Registros_preparados.csv"),row.names=FALSE,na="")
  write.csv(a$b$effort,file.path(folder,"Muestras_utilizadas.csv"),row.names=FALSE,na="")
  for(n in names(a$anla$tables)) write.csv(a$anla$tables[[n]],file.path(folder,paste0(n,".csv")),row.names=FALSE,na="")
  write.csv(a$anla$issues,file.path(folder,"Validacion_ANLA.csv"),row.names=FALSE,na="")
  write.csv(a$enriched,file.path(folder,"Especies_catalogos.csv"),row.names=FALSE,na="")
  for(i in seq_along(a$b$incidence)) {m<-a$b$incidence[[i]];write.csv(data.frame(Especie=rownames(m),m,check.names=FALSE),file.path(folder,paste0("Incidencia_",i,".csv")),row.names=FALSE);write.csv(a$b$units[[i]],file.path(folder,paste0("Unidades_",i,".csv")),row.names=FALSE)}
  if(!is.null(a$hill$out)) for(t in 1:3) for(ext in c("png","pdf")) ggplot2::ggsave(file.path(folder,paste0("Hill_",t,".",ext)),make_curve(a,as.character(t)),width=plot_settings()$width,height=plot_settings()$height,dpi=plot_settings()$dpi)
  if(!is.null(a$accum)&&nrow(a$accum)) {write.csv(a$accum,file.path(folder,"Acumulacion.csv"),row.names=FALSE);ggsave(file.path(folder,"Acumulacion.png"),make_curve(a,"accum"),width=plot_settings()$width,height=plot_settings()$height,dpi=plot_settings()$dpi)}
  if(!is.null(a$beta$matrix)) {write.csv(a$beta$matrix,file.path(folder,"Beta.csv"));ggsave(file.path(folder,"Beta.png"),beta_plot(a),width=plot_settings()$width,height=plot_settings()$height,dpi=plot_settings()$dpi)}
  writeLines(c("RESULTADOS DE DIVERSIDAD",a$b$note,a$warnings,"Hill: intervalos bootstrap al 95%. Acumulación: ±1 SD entre órdenes aleatorios.","Beta sobre composición observada, sin corrección por completitud.","Tablas ANLA: borrador si Validacion_ANLA.csv contiene pendientes. Geometrías no incluidas.","MADS 2024; CITES enero 2025; aves 2022; UICN según año/alcance en Especies_catalogos.csv.","Para convertir a .gdb usar tools/export_gdb.py del repositorio."),file.path(folder,"LEEME.txt"))
  zip::zipr(file,files=list.files(folder,full.names=TRUE),root=folder)
 })
}
shinyApp(ui,server)
