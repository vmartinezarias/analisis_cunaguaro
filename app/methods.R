# Redacción metodológica independiente de Shiny: borrador y ejecución verificada.
method_summary <- function(config, result=NULL, versions=NULL) {
 final <- !is.null(result)
 val <- function(n, default="sin especificar") {
  x<-config[[n]];if(is.null(x)||!length(x)||all(is.na(x))) return(default)
  paste(as.character(x),collapse=" × ")
 }
 modules<-config$modules
 paragraphs<-character()
 add<-function(x) paragraphs<<-c(paragraphs,x)
 if(final) {
  add("MÉTODOS DE LA EJECUCIÓN VIGENTE")
  add(sprintf("Se analizaron %s registros de %s, correspondientes a %s especies y %s comunidades. Los datos del archivo Excel se leyeron con readxl y se verificaron fechas, identificadores, nombres y conteos enteros positivos. La estructura tabular se preparó con funciones de R base.",nrow(result$b$records),val("class"),length(unique(result$b$records$Especie)),nrow(result$b$abundance)))
 } else {
  add("BORRADOR SEGÚN LA CONFIGURACIÓN: ANÁLISIS AÚN NO EJECUTADO")
  add(paste0("Se prevé analizar el grupo ",val("class")," a partir de registros Excel, leídos con readxl. Se verificarán fechas, identificadores, nombres y conteos enteros positivos, y se preparará la estructura tabular con funciones de R base."))
 }
 add(paste0(if(final) "Las comunidades se definieron mediante " else "Las comunidades se definirán mediante ",val("groups"),
   if(final) "; las unidades de muestreo se identificaron con la combinación " else "; las unidades de muestreo se identificarán con la combinación ",val("units"),". Los análisis se realizan por grupo taxonómico."))
 if(val("kind")=="incidence") {
  add(paste0(if(final) "Se construyó" else "Se construirá"," una matriz interna de especies × unidades de muestreo, asignando una presencia cuando una especie tenga uno o varios registros en la misma unidad. La frecuencia de incidencia corresponde al número de unidades con presencia. Para iNEXT, el vector de entrada contiene el número total de muestras seguido de las frecuencias por especie."))
 } else {
  add(paste0(if(final) "Se construyó" else "Se construirá"," una matriz de comunidades × especies sumando los conteos dentro de cada comunidad. Los conteos se declararon como ",val("meaning"),"."))
 }
 if(val("meaning")=="detecciones") add("Los conteos representan detecciones o eventos; los índices calculados sobre ellos describen la distribución de detecciones y no equivalen automáticamente a diversidad basada en abundancias poblacionales de individuos.")
 effort_ok<-if(final) isTRUE(result$params$esfuerzo_confirmado) else isTRUE(config$effort_complete)
 if(effort_ok) add(paste0(if(final) "El usuario confirmó" else "La configuración requiere confirmar"," que la hoja Esfuerzo incluye todas las muestras realizadas para el grupo, incluso aquellas sin detecciones. Esta declaración no demuestra por sí sola la equivalencia del esfuerzo entre muestras."))
 else if(final&&isTRUE(result$params$hoja_esfuerzo)) add("Se utilizó una hoja Esfuerzo con las muestras declaradas, pero el usuario no confirmó explícitamente que fuera completa. Los resultados se condicionan a la exhaustividad de esa declaración.")
 else add("No se ha confirmado esfuerzo completo. Sin una hoja Esfuerzo válida solo se conocen las muestras con detecciones: las frecuencias de incidencia y la acumulación quedan condicionadas a esa omisión. Hill por incidencia requiere declarar y confirmar todas las muestras.")
 if("classic" %in% modules) add(paste0(if(final) "Se calcularon" else "Se calcularán"," sobre los conteos la riqueza observada, Shannon con logaritmos naturales, concentración de Simpson y su complemento (1 − concentración), Pielou, Margalef, Menhinick y las diversidades efectivas observadas exp(Shannon) y 1/concentración de Simpson. El cálculo utiliza funciones explícitas de R base. Estos índices clásicos se basan en conteos incluso cuando el módulo de Hill se configura por incidencia."))
 if("frequency" %in% modules) add(paste0(if(final) "Se resumieron" else "Se resumirán"," por especie y comunidad los conteos, su porcentaje relativo, el número de muestras con presencia y la frecuencia de incidencia (muestras con presencia / total de muestras). La tabulación utiliza R base y las matrices internas."))
 if("hill" %in% modules) {
  unavailable<-final&&is.null(result$hill$out)
  if(unavailable) add("Se solicitó estimar diversidad de Hill con iNEXT, pero no se obtuvieron curvas estimables para las comunidades de esta ejecución. Las advertencias se conservan con los resultados; no se presenta este módulo como un análisis completado.")
  else {
   add(paste0(if(final) "Se estimó" else "Se estimará"," la diversidad de Hill mediante iNEXT::iNEXT para q = ",paste(config$qs,collapse=", "),
    ". Los órdenes q = 0, 1 y 2 corresponden, respectivamente, a riqueza, diversidad efectiva de Shannon y diversidad efectiva de Simpson; solo se calculan los órdenes seleccionados. Se utilizaron datos de ",if(val("kind")=="incidence") "frecuencia de incidencia" else "abundancia/conteos",
    ", curvas de rarefacción y extrapolación por tamaño de muestra y por cobertura, con un límite de extrapolación de ",val("mult")," veces el tamaño observado de cada comunidad. Los intervalos bootstrap de las curvas se configuran al 95%, con ",val("nboot")," remuestreos. La completitud se describe mediante la cobertura del muestreo."))
   if(!is.null(versions)&&"iNEXT" %in% names(versions)&&versions[["iNEXT"]]=="3.0.2") add("En iNEXT 3.0.2, las estimaciones asintóticas generadas internamente por iNEXT utilizan 100 remuestreos, mientras que el número seleccionado por el usuario controla los intervalos de las curvas.")
  }
  if(final&&length(result$hill$omitted)) add(paste("Comunidades omitidas en Hill:",paste(result$hill$omitted,collapse=", "),". Se excluyeron por tamaño insuficiente o imposibilidad de estimación, sin sustituir los datos."))
 }
 if("accum" %in% modules) {
  if(final&&(is.null(result$accum)||!nrow(result$accum))) add("Se solicitó acumulación de especies, pero no hubo comunidades con datos suficientes para generar las curvas.")
  else add(paste0(if(final) "Se generaron" else "Se generarán"," curvas de acumulación de especies mediante vegan::specaccum, usando presencia/ausencia por muestra, orden aleatorio y 100 permutaciones solicitadas; en conjuntos pequeños el algoritmo puede enumerar las permutaciones disponibles. La banda gráfica representa ±1 desviación estándar entre órdenes de muestreo y no un intervalo de confianza."))
 }
 if("beta" %in% modules) {
  metric<-if(final&&!is.null(result$beta$metric)) result$beta$metric else if(val("kind")=="abundance") "bray" else if(val("beta_metric")=="sorensen") "sorensen" else "jaccard"
  names_metric<-c(bray="Bray–Curtis",sorensen="Sørensen",jaccard="Jaccard")
  if(final&&is.null(result$beta$matrix)) add("Se solicitó diversidad beta, pero no hubo al menos dos comunidades con detecciones para generar la matriz de disimilitudes.")
  else add(paste0(if(final) "Se evaluó" else "Se evaluará"," la diversidad beta con disimilitud de ",unname(names_metric[metric])," mediante vegan::vegdist, usando ",if(metric=="bray") "conteos por especie y comunidad" else "composición binaria (presencia/ausencia) por comunidad",
    if(metric=="sorensen") ". Sørensen se obtiene como Bray–Curtis aplicada a datos binarios." else ".",
    " La matriz representa composición observada, sin corrección por completitud ni detectabilidad; las comunidades completamente vacías se excluyen."))
  if(final&&length(result$beta$removed)) add(paste("Comunidades vacías excluidas de beta:",paste(result$beta$removed,collapse=", "),"."))
 }
 add(paste0(if(final) "Los nombres de especies se cruzaron" else "Los nombres de especies se cruzarán"," con los catálogos suministrados, mediante coincidencia exacta tras normalizar espacios y mayúsculas o mediante equivalencias explícitas documentadas. Se utilizan MADS 2024, CITES enero de 2025, aves de 2022 y evaluaciones UICN del archivo de referencia. Para UICN se selecciona el año más reciente entre evaluaciones de alcance Global; discrepancias del mismo año quedan sin asignación automática. Ausencia de coincidencia no equivale a LC, NE, ausencia de listado CITES o ausencia de endemismo. Los candidatos de endemismo y las anotaciones CITES requieren revisión; no se consulta automáticamente la vigencia de las fuentes."))
 add(paste0(if(final) "Se prepararon" else "Se prepararán"," las tablas MuestreoFaunaTB y MuestreoFaunaResultadosTB, mapeando campos y dominios del diccionario ANLA con R base y jsonlite. La abundancia relativa se expresa como porcentaje por cobertura dentro del grupo taxonómico. Los atributos desconocidos se conservan como pendientes. Esta etapa no genera geometrías ni demuestra conformidad completa de la GDB."))
 if(final&&nrow(result$anla$issues)) add(paste("La validación de tablas identificó",nrow(result$anla$issues),"pendientes, disponibles en la exportación ANLA."))
 add(paste0("Las figuras se producen con ggplot2 e iNEXT::ggiNEXT cuando corresponde. Las exportaciones de tablas Excel utilizan openxlsx, los parámetros JSON utilizan jsonlite y los paquetes ZIP utilizan zip. La semilla de aleatoriedad seleccionada es ",val("seed"),"."))
 used<-c("readxl",if("hill" %in% modules) "iNEXT",if(any(c("beta","accum") %in% modules)) "vegan","ggplot2","openxlsx","jsonlite","zip","shiny")
 if(is.null(versions)) versions<-vapply(used,function(p) if(requireNamespace(p,quietly=TRUE)) as.character(utils::packageVersion(p)) else "no disponible",character(1))
 used<-intersect(used,names(versions))
 add(paste0("Paquetes y versiones: ",paste(paste0(used," ",versions[used]),collapse="; "),". Entorno: ",R.version.string,"."))
 refs<-character()
 if("hill" %in% modules) refs<-c(refs,"Chao et al. (2014), Ecological Monographs 84:45–67, DOI: 10.1890/13-0133.1; Hsieh, Ma y Chao (2016), Methods in Ecology and Evolution, DOI: 10.1111/2041-210X.12613")
 if(any(c("beta","accum") %in% modules)) refs<-c(refs,"documentación de vegan::vegdist y vegan::specaccum")
 refs<-c(refs,"diccionario ANLA y catálogos versionados suministrados")
 add(paste0("Referencias metodológicas: ",paste(refs,collapse="; "),"."))
 if(final&&length(result$warnings)) add(paste0("Advertencias de la ejecución: ",paste(unique(result$warnings),collapse=" ")))
 paste(paragraphs,collapse="\n\n")
}
