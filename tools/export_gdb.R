# Exportación local en R: sf utiliza GDAL, sin Python ni comandos de terminal.
export_anla_gdb_R <- function(tablas, pendientes, salida, plantilla_zip,
                              permitir_borrador = FALSE) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Instala el paquete sf para exportar la GDB.")
  if (nrow(pendientes) && !permitir_borrador) {
    stop("Hay ", nrow(pendientes), " pendientes ANLA. Corrígelos o habilita explícitamente permitir_borrador = TRUE.")
  }
  if (!grepl("\\.gdb$", salida, ignore.case = TRUE)) stop("La salida debe terminar en .gdb")
  if (dir.exists(salida) || file.exists(salida)) stop("La salida ya existe; no se sobrescribe.")
  if (!file.exists(plantilla_zip)) stop("No se encontró la plantilla GDB: ", plantilla_zip)
  drivers <- sf::st_drivers()
  if (!any(drivers$name == "OpenFileGDB" & drivers$write)) stop("Tu GDAL no permite escribir OpenFileGDB. Usa GDAL >= 3.6.")
  tmp <- tempfile("anla_plantilla_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(plantilla_zip, exdir = tmp)
  plantilla <- file.path(tmp, "ANLA_template.gdb")
  if (!dir.exists(plantilla)) stop("La plantilla no contiene ANLA_template.gdb")
  dir.create(dirname(salida), recursive = TRUE, showWarnings = FALSE)
  dir.create(salida)
  archivos <- list.files(plantilla, full.names = TRUE)
  if (!length(archivos) || !all(file.copy(archivos, salida, overwrite = FALSE))) {
    unlink(salida, recursive = TRUE)
    stop("No se pudo copiar la plantilla GDB")
  }
  exito <- FALSE
  on.exit(if (!exito && dir.exists(salida)) unlink(salida, recursive = TRUE), add = TRUE)
  esquemas <- list(
    MuestreoFaunaTB = list(entero = character(), reales = c("DETERM", "ABUND_ABS"), fechas = character()),
    MuestreoFaunaResultadosTB = list(entero = c("NOMENCLAT", "TIPO_MIGR", "USO", "DIETA"),
      reales = c("CATEG_CIT", "CATEG_UICN", "CATE_MINIS", "T_DISTRIB", "MIGRACION", "VEDA", "ENTID_VEDA", "VIGEN_VEDA", "ABUND_ABS", "ABUND_REL"),
      fechas = c("FECHA_IMUE", "FECHA_FMUE")))
  filas <- list()
  for (nombre in names(esquemas)) {
    if (is.null(tablas[[nombre]])) stop("Falta la tabla ", nombre)
    x <- as.data.frame(tablas[[nombre]], check.names = FALSE)
    original <- sf::st_read(salida, layer = nombre, quiet = TRUE)
    if (!setequal(names(x), names(original))) stop("Los campos no coinciden con la plantilla: ", nombre)
    x <- x[names(original)]
    tipo <- esquemas[[nombre]]
    for (n in names(x)) {
      if (n %in% tipo$entero) {
        v <- suppressWarnings(as.numeric(x[[n]]))
        if (any(!is.na(v) & (!is.finite(v) | v != floor(v)))) stop("Campo entero inválido: ", n)
        x[[n]] <- as.integer(v)
      } else if (n %in% tipo$reales) x[[n]] <- as.numeric(x[[n]])
      else if (n %in% tipo$fechas) x[[n]] <- as.POSIXct(as.Date(x[[n]]), tz = "UTC")
      else x[[n]] <- as.character(x[[n]])
    }
    if (nrow(x)) sf::st_write(x, dsn = salida, layer = nombre, driver = "OpenFileGDB",
                              append = TRUE, delete_layer = FALSE, quiet = TRUE)
    leida <- sf::st_read(salida, layer = nombre, quiet = TRUE)
    if (nrow(leida) != nrow(x)) stop("Número de filas incorrecto en ", nombre)
    for (n in intersect(c("ABUND_ABS", "ABUND_REL"), names(x))) {
      if (!isTRUE(all.equal(as.numeric(leida[[n]]), as.numeric(x[[n]]), tolerance = 1e-8))) stop("Valores incorrectos en ", nombre, ": ", n)
    }
    filas[[nombre]] <- nrow(x)
  }
  jsonlite::write_json(list(estado = if (nrow(pendientes)) "BORRADOR" else "Tablas convertidas; geometrías y relaciones pendientes",
    filas = filas, pendientes = pendientes, GDAL = unname(sf::sf_extSoftVersion()["GDAL"]),
    dominios = "Conservados de la plantilla al añadir registros sin recrear las tablas"),
    paste0(salida, ".validacion.json"), pretty = TRUE, auto_unbox = TRUE)
  exito <- TRUE
  if (nrow(pendientes)) warning("GDB de BORRADOR: contiene ", nrow(pendientes), " pendientes ANLA.")
  invisible(salida)
}
