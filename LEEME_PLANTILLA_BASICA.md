# Plantilla básica actualizada

BD conserva las 17 columnas del archivo suministrado. Siete son obligatorias para procesar: Fecha, Clase, Especie, N° Individuo, ID, Cobertura, Método. Las demás aportan contexto y taxonomía.

Shiny ofrece Plantilla básica (BD vacía, instrucciones y ejemplo) y Plantilla con esfuerzo (añade Esfuerzo). Las categorías de amenaza se cruzan por nombre con los catálogos; no se exigen en la entrada.

Para incidencias se requiere esfuerzo completo, incluidas muestras sin detecciones. Una hoja Esfuerzo vacía se trata como ausente: permite abundancia, pero no Hill por incidencia.

ANLA: el formato básico permite generar tablas preliminares. Los campos que no se pueden obtener de registros o catálogos quedan pendientes. No se inventan dieta, uso, códigos geográficos ni categorías.

GitHub: reemplazar app/app.R, app/core.R, app/template.R, app/data/BD_muestra.xlsx y tests/test_core.R. El paquete conserva los ejes en español y los controles de letra.

PC: utilizar Diversidad_fauna_PC_es.Rmd con las carpetas del paquete.
