# Diversidad de fauna - R

Versión on line: https://vmartinezarias.github.io/analisis_cunaguaro/

Primera versión para inventarios de aves, mamíferos, anfibios y reptiles. Carga Excel; compara comunidades; define muestras; analiza abundancia e incidencia; genera tablas ANLA y cruces de catálogos.

## Carpeta data

Está en **app/data/**, porque Shinylive debe incluir los archivos en el sistema de archivos del navegador. Su README identifica versiones y contenidos. Puedes sustituirlos manteniendo las columnas. No subir bases reales de campo al repositorio público; los visitantes las cargan localmente.

## Ejecutar en R local

Desde la raíz:

```r
install.packages(c("shiny","readxl","openxlsx","jsonlite","vegan","iNEXT","ggplot2","zip"))
shiny::runApp("app")
```

Para ejecutar las verificaciones: `Rscript tests/test_core.R`.

## Uso

1. Descargar la plantilla desde la aplicación. `BD` contiene registros y `Esfuerzo` las muestras realizadas. `Ejemplo` es únicamente ilustrativo.
2. Esfuerzo debe incluir **Clase** y todas las columnas utilizadas para comunidad y muestra. Registrar también muestras sin detecciones. Si una visita no muestreó un grupo, no declararla para ese grupo.
3. Elegir clase. Comunidad, por ejemplo **Cobertura**; muestra, por ejemplo **ID + Fecha**. Puede elegirse fecha o ID solos, pero eso cambia el significado del análisis.
4. Elegir abundancia o incidencia. Hill por incidencia requiere esfuerzo completo confirmado. Hill por abundancia usa conteos; si son eventos/detecciones, la interpretación es diversidad de detecciones, no abundancia poblacional.
5. Elegir módulos y pulsar Analizar. Cambiar opciones invalida resultados previos.
6. Descargar Excel o ZIP. ANLA con pendientes es un borrador.

Muestras deben tener esfuerzo comparable. El campo Esfuerzo permite documentarlo, pero no se usa para corregir conteos automáticamente. No hay corrección por detectabilidad ni comparación estadística entre grupos.

## Análisis

- Clásicos sobre conteos: S, Shannon (ln), concentración de Simpson, 1−Simpson, Pielou, Margalef, Menhinick y diversidades efectivas observadas.
- iNEXT: q 0, 1 y 2, interpolación/extrapolación por tamaño y cobertura, intervalos al 95%. Entrada de incidencia: número de muestras + incidencias por especie. Se exportan ambas matrices para auditoría. iNEXT 3.0.2 calcula estimaciones asintóticas con 100 remuestreos internamente; el selector nboot controla las curvas.
- Acumulación: specaccum con órdenes aleatorios (100 permutaciones). Banda ±1 SD entre permutaciones, no IC.
- Beta: Jaccard/Sørensen binarios o Bray–Curtis sobre conteos. Sin corrección por completitud. No se incluyen aún partición recambio/anidamiento, NMDS ni PERMANOVA.
- Una comunidad sin registros mantiene su muestra en Esfuerzo; se reporta como vacía y se excluye de beta y Hill si no permite estimación.

## Catálogos

Cruce exacto tras normalizar espacios y mayúsculas. Equivalencias documentadas en `sinonimos.csv`. Sin asignaciones difusas ni equiparación de `sp.`, `cf.` o `aff.` a una especie. Coincidencias ambiguas no multiplican registros.

UICN: se selecciona el año más reciente entre evaluaciones de alcance Global del archivo suministrado; discrepancias del mismo año quedan ambiguas. MADS: listado de amenazadas 2024 suministrado. No encontrada **no significa** LC o NE. CITES: solo coincidencias exactas; listados por género/familia, poblaciones, países y anotaciones deben revisarse. El candidato de endemismo no confirma endemismo. No se integran consultas en tiempo real a servicios externos.

## ANLA

Campos y dominios proceden del diccionario suministrado. Se exportan `MuestreoFaunaTB` y `MuestreoFaunaResultadosTB`. Se dejan pendientes puntos/transectos y relaciones espaciales. ABUND_REL = porcentaje por cobertura **dentro de la clase seleccionada**. Fechas inicial/final por cobertura en ese grupo. Taxonomía contradictoria, métodos combinados sin código, campos no conocidos y categorías ambiguas requieren revisión.

Especificar Tipo_unidad (Punto/Transecto) o los ID geográficos. Se acepta el código explícito DETERM para registros combinados. NOMENCLAT debe contener un código Corine válido; la aplicación comprueba enteros, pero no certifica la correspondencia cartográfica. Proyecto y campos obligatorios faltantes figuran en Validacion_ANLA.csv. Los registros de muestreo se agregan por ID, especie y determinación; no elimina encuentros potencialmente repetidos: eso es parte del control de campo.

### File Geodatabase real

En Python de QGIS/OSGeo4W o un entorno con GDAL >= 3.6:

```bash
python tools/export_gdb.py Diversidad_Amphibia.zip salida/Amphibia.gdb
# Solo para revisión de borradores con pendientes:
python tools/export_gdb.py Diversidad_Amphibia.zip salida/Amphibia.gdb --allow-draft
```

Crea **tablas no espaciales** con tipos y dominios; reabre la GDB y comprueba número de filas. No crea capas de puntos/transectos. No implica conformidad completa con ANLA. No sobrescribe salidas existentes. El conversor no se ejecuta en GitHub Pages.

## Límites de esta versión

- Carga inicial de R y paquetes puede tardar. Cálculos y memoria dependen del dispositivo. Límite de carga configurado: 30 MB.
- Paquetes usados disponibles en el índice WebAssembly consultado durante preparación; la primera exportación y el uso en navegador deben verificarse en tu despliegue.
- El ejemplo adjunto no contiene esfuerzo independiente: sirve para abundancia, tablas y exploración; incidencia requiere completar Esfuerzo.
- Catálogos versionados, no necesariamente vigentes. El usuario debe verificar actualización y permisos de redistribución antes de publicar los catálogos.

Referencias: Chao et al. 2014, Ecological Monographs 84:45–67, doi:10.1890/13-0133.1; Hsieh et al. 2016, Methods in Ecology and Evolution, doi:10.1111/2041-210X.12613; documentación iNEXT, vegan, Shinylive y GDAL OpenFileGDB.
