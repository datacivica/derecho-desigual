# Plataforma Abogadas MX

## Descripción

Este repositorio crea una plataforma interactiva de datos sobre las abogadas de México y la desigualdad de género en el ámbito del derecho.

Importa, limpia y prepara datos de la ENOE, la ENDIREH y el Censo de Población y Vivienda y visualiza estos datos a través de una página web.

## Directorios

* `enoe`

  * `import-enoe`: importa datos crudos de la ENOE y selecciona tablas y variables de interés

  * `join-enoe`: vincula varias tablas y periodos de la ENOE

  * `clean-enoe`: procesa datos de la ENOE


* `endireh`

  * `import-endireh`: importa datos crudos de la ENDIREH y selecciona tablas y variables de interés

  * `join-endireh`: vincula varias tablas de la ENDIREH

  * `clean-endireh`: procesa datos de la ENDIREH

* `censo`
    
    * `import-enoe`: importa datos crudos del Censo de Población y Vivienda y selecciona variables de interés

    * `clean-censo`: procesa datos del Censo

* `prep-app`: prepara pequeñas tablas de datos para su uso en la elaboración de gráficas y textos parametrizados de acuerdo con estas cifras en el directorio `app`. Este directorio fue creado para evitar el procesamiento repetido de las bases limpias de la ENOE, la ENDIREH y/o el Censo en cada uno de los módulos de `app`, ya que estas bases son bastante grandes y su procesamiento repetido retardarían el funcionamiento de la página web en vivo.

* `app`: incorpora los datos procesados de `prep-app` a través del uso de Shiny, HTML y CSS

    * `modules/`: genera los módulos de Shiny que crean las gráficas y textos parametrizados (con cifras específicas) de la plataforma web. La organización de estos módulos es como sigue:

        * `mod_perfiles.R`: corresponde a las gráficas y cifras de la página entitulada "¿Quiénes son las abogadas en México?"

        * `mod_genero.R`: corresponde a la página "Desigualdad de género en el derecho"

        * `mod_abogadas.R`: corresponde a la página "Disparidades entre las abogadas"

        * `mod_cuidados.R`: corresponde a la página "Las abogadas y el cuidado"

        * `mod_violencia.R`: corresponde a la página "La violencia de género en el derecho"

        * `grafs-theme-fns.R`: define la identidad temática de las gráficas de la página, así como los tamaños de textos utilizados en estas gráficas de acuerdo con el tamaño de la pantalla de la persona usuaria

    * `www/` 

        * `assets/`: contiene archivos .png de imagenes e íconos utilizados en la página web

        * `template.html`: contiene el código HTML utilizado para la estructuración de la página web

        * `styles.css`: define las clases utilizadas en `template.html` para el formateo de la página web

        * `ui.R` compila y define las distintas UIs definidas en los módulos de Shiny (`modules/src/`) y las vincula con el contenido de `template.html`

        * `server.R` compila los distintos servidores definidas en los módulos de Shiny

        * `run-app.R` junta las UIs y servidores (a través de los scripts de `ui.R` y `server.R`) y corre la aplicación Shiny que sirve como base de la plataforma


## Organización

Con la excepción del directorio `app/` y sus subdirectorios, cada uno de los directorios de este repositorio antes mencionados contiene los siguientes subdirectorios para organizar y estandarizar el flujo de datos en este proyecto:

* `input`: datos de insumo antes de ser transformados

  * ej. `join-endireh/input` contiene los datos de la ENDIREH antes de ser vinculados; `clean-enoe/input`, los datos de la ENOE antes de ser procesados; etc.

* `src`: contiene los scripts utilizados para transformar los datos de `input` en los datos de `output`

* `output`: contiene los datos de `input` ya transformados por los scripts de `src`

  * ej. `join-endireh/output` contiene los datos de `join-endireh/input` después de ser transformados (vinculados) por el script de R `join-endireh/src/join-endireh.R` 

Además, algunos directorios contienen el siguiente subdirectorio:

* `hand`: contiene diccionarios o otros archivos utilizados en los scripts de `src`

## Actualización de datos

* ENOE: La ENOE se actualizará anualmente de forma automática tras la publicación de los microdatos del cuestionario ampliado de esta encuesta.

* ENDIREH: se tendrá que actualizar de forma manual, ya que los nombres de las variables y tablas cambian cada vez que se realiza la encuesta. Los microdatos de esta encuesta se publican aproximadamente cada 5 años. La última versión fue realizada en 2021.

* Censo de Población y Vivienda: se tendrá que actualizar de forma manual. 

## Deploy de la plataforma
Para subir el contenido de la plataforma de los archivos locales a una página web, se utiliza la aplicación Docker. Los siguientes archivos/directorios que pertienen a este proceso son los siguientes:

* `nginx`

* `shinyproxy`

* `Dockerfile`

* `Dockerfile.BAK`

* `dockercompose.yaml`

Para más detalles sobre el deploy de la plataforma a través de Docker, favor de consultar el archivo `Dockerfile`.

<!-- done. -->
