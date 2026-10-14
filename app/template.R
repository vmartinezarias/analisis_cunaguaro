# Plantilla básica basada en las 17 columnas del Excel suministrado.
write_input_template <- function(file, include_effort=FALSE, data_path="data/BD_muestra.xlsx") {
 base_columns <- c("Fecha","Departamento","Municipio","Vereda","Temporada","Cobertura","ID","Método","Clase","Orden","Familia","Genero","Epiteto","Especie","Autor","N. común","N° Individuo")
 example <- as.data.frame(readxl::read_excel(data_path),check.names=FALSE)
 if(length(setdiff(base_columns,names(example)))) stop("El ejemplo no contiene las columnas básicas de la plantilla.")
 example <- example[base_columns]
 wb <- openxlsx::createWorkbook()
 sheets <- c("BD",if(include_effort) "Esfuerzo","Instrucciones","Ejemplo")
 for(s in sheets) openxlsx::addWorksheet(wb,s)
 openxlsx::writeData(wb,"BD",example[0,,drop=FALSE])
 openxlsx::writeData(wb,"Ejemplo",example)
 e <- data.frame(Clase=character(),ID=character(),Fecha=character(),Cobertura=character(),Temporada=character(),Método=character(),Esfuerzo=double(),Unidad_esfuerzo=character(),check.names=FALSE)
 if(include_effort) openxlsx::writeData(wb,"Esfuerzo",e)
 instructions <- data.frame(Tema=c("BD","Obligatorios del programa","Esfuerzo","Fechas","Conteos","Catálogos","ANLA","Ejemplo"),Instruccion=c(
  "Las 17 columnas de BD corresponden al formato básico suministrado. Una fila por registro de especie.",
  "Fecha, Clase, Especie, N° Individuo, ID, Cobertura y Método son obligatorios para procesar. Las demás columnas aportan contexto y taxonomía.",
  if(include_effort) "Una fila por muestra realizada y Clase, incluidas las muestras sin detecciones. Añadir cualquier columna elegida como comunidad o unidad. No derivar automáticamente esfuerzo completo de los registros positivos." else "Para Hill por incidencia descargar la plantilla con esfuerzo y completar todas las muestras realizadas. La plantilla básica basta para abundancia. Frecuencias/acumulación sin esfuerzo omiten muestras sin detecciones.",
  "Usar fechas de Excel o AAAA-MM-DD.",
  "N° Individuo: enteros positivos. Declarar si son individuos o detecciones. Los muestreos sin detecciones no llevan especies ficticias ni conteos cero en BD.",
  "Las categorías de amenaza no se requieren en BD: se cruzan con los catálogos versionados por nombre científico. Sin coincidencia no equivale a LC o NE.",
  "La exportación ANLA puede quedar como borrador: campos geográficos, códigos Corine, dieta, uso y otros atributos desconocidos se reportan como pendientes. Los campos opcionales del diccionario pueden añadirse a BD sin modificar las columnas básicas.",
  "Ejemplo contiene registros ilustrativos. BD se entrega vacía para tus datos."),stringsAsFactors=FALSE)
 openxlsx::writeData(wb,"Instrucciones",instructions)
 style <- openxlsx::createStyle(fgFill="#163E32",fontColour="white",textDecoration="bold")
 for(s in sheets) {
  n <- if(s=="Instrucciones") ncol(instructions) else if(s=="Esfuerzo") ncol(e) else ncol(example)
  openxlsx::addStyle(wb,s,style,rows=1,cols=seq_len(n),gridExpand=TRUE)
  openxlsx::freezePane(wb,s,firstRow=TRUE)
  openxlsx::setColWidths(wb,s,cols=seq_len(n),widths=if(s=="Instrucciones") c(26,95) else 23)
 }
 openxlsx::saveWorkbook(wb,file,overwrite=TRUE)
 invisible(file)
}
