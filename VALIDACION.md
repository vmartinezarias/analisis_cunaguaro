# Validación de la primera versión

## Verificado localmente

Pruebas ejecutadas con R 4.3.3, iNEXT 3.0.2 y vegan 2.6-4:

- Conteos y presencias son distintos: dos registros en una muestra suman individuos, pero una incidencia.
- Muestras sin detecciones cuentan en el esfuerzo; comunidades completamente vacías se conservan y se omiten en estimaciones que no admiten ceros.
- Hill por abundancia e incidencia; órdenes q 0, 1 y 2; manejo de estimaciones no disponibles.
- Jaccard de comunidades sin especies compartidas = 1.
- Acumulación termina en la riqueza observada del conjunto.
- Rechazo de conteos negativos y registros sin correspondencia con Esfuerzo.
- Coincidencias ambiguas en catálogos no duplican especies ni asignan categorías.
- Conservación de conteos al generar las dos tablas ANLA; porcentajes por cobertura suman 100 dentro del grupo.
- Carga y ejecución del servidor Shiny con el ejemplo.
- Excel de resultados escrito y reabierto; figuras de tres tipos Hill y acumulación generadas.
- Cambios en opciones invalidan los resultados previos.
- FileGDB real generada y reabierta con GDAL 3.8.4: 23 filas en MuestreoFaunaTB y 18 en MuestreoFaunaResultadosTB, con dominios asociados. El ejemplo tiene 137 pendientes, por lo que se verificó como borrador explícito.

## Pendiente en el despliegue del usuario

- Exportación completa con Shinylive y ejecución visual en navegador/GitHub Pages. El índice WebAssembly consultado incluye shiny, readxl, openxlsx, jsonlite, vegan, iNEXT, ggplot2 y zip. Eso no sustituye una prueba integral del navegador.
- Integración del conversor con el entorno GDAL del usuario. La escritura de tablas y dominios ya se probó localmente.
- Integración de capas de puntos/transectos, referencias de IDs y condiciones de entrega ANLA.
- Revisión de vigencia, alcances y casos especiales de catálogos.

El workflow de GitHub ejecuta nuevamente las pruebas antes de publicar y se detiene si fallan. No se ha creado ni publicado un repositorio en la cuenta del usuario.
