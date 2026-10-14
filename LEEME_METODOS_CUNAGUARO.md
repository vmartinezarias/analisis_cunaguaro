# Métodos automáticos y logo de Cunaguaro

La pestaña Métodos empleados redacta un borrador según las opciones. Al ejecutar Analizar describe los resultados vigentes, incluidos los módulos no estimables, comunidades excluidas y advertencias. Cambiar opciones invalida los resultados y vuelve al borrador. La guía anterior permanece disponible.

El resumen incluye grupo, comunidades, unidades, naturaleza de los conteos, esfuerzo, índices, órdenes de Hill, remuestreos, extrapolación, beta efectiva, acumulación, catálogos, ANLA y paquetes/versiones. Puede descargarse como TXT y se incorpora al Excel (hoja Metodos) y al ZIP (Metodos_empleados.txt).

El logo original se conserva sin alteraciones en app/www/cunaguaro.png, con fondo blanco para mantener su legibilidad.

GitHub: reemplazar app/app.R; añadir app/methods.R y app/www/cunaguaro.png. Actualizar tests/test_core.R, añadir tests/test_methods.R y actualizar .github/workflows/pages.yml para incluir las comprobaciones nuevas. No hacen falta nuevos paquetes. El ZIP contiene el proyecto completo actualizado.

Validación: sintaxis de la aplicación revisada con R; pruebas del generador de métodos ejecutadas para borrador/final, selección de módulos y parámetros, índice beta efectivo y análisis no estimables. Verificación de la interfaz completa y descargas en el navegador pendiente del despliegue.
