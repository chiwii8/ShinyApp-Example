library(shiny)
library(sf)
library(sp)
library(DT) #Permite añadir una tabla interactiva
library(dygraphs)
library(xts)
library(leaflet)
library(dplyr)

#
# Aplicación Shiny para visualizaciónde datos espacio-temporales
# Basado en el  Capítulo 15 de Paula Moraga - Geospatial Health Data: Modeling and Visualization with R-INLA and Shiny
# Autor: Alejandro Sánchez Rodríguez



# La lectura se realiza fuera de la función server, debido a que este código no es necesario
# ejecutarlo más de una vez y no reducir el rendimiento de la aplicación.


# Lectura de datos del fichero csv y shp
# Conjunto de datos ohio
## En el conjunto de datos se han modificado las columnas y e n
## por casos y poblacion
## Se ha detectado múltiples datos repetidos por tanto se hace una limpieza de los años repetidos tomando el primer valor
data <- read.csv("data/Ohio/dataohiocomplete.csv")
data <- data[!duplicated(data[, c("NAME", "year")]), ]

# Lectura del mapa
## Debido a que el paquete rgdal está obsoleto, se carga el mapa en el formato sf
## Se requiere una conversión del formato WGS84 a CRS
## Para finalizar, se puede transformar a un sp
## Por tanto, no es requerido modificar el código
map <- st_read("data/Ohio/fe_2007_39_county/fe_2007_39_county.shp")
map <- st_transform(map, crs = 4326)
map <- as(map,"Spatial")

# ui object
ui <- fluidPage(
  titlePanel(p("Aplicación Espacial", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      img(
        src = "imageShiny.png",
        width = "70px", height = "70px"
      ),
      ## 1 Botón: Selección de variable
      selectInput(
        inputId = "Variable_seleccionada",
        label = "Seleccionar variable",
        choices = c("casos"="cases", "población"="population")
      ),
      
      ## 2 Botón: Año Seleccionado
      selectInput(
        inputId = "anio_seleccionado",
        label = "Seleccionar año",
        choices = 1968:1988
      ),
      
      ## 3 Cargar: Ficheros csv
      fileInput(
        inputId = "filedata",
        label = "Sube los datos. Elige El fichero csv",
        accept = c(".csv")
      ),
      
      ## 4 Cargar: Mapa
      fileInput(
        inputId = "filemap",
        label = "Sube el mapa. Elige Shapefile",
        accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')
      )

      
    ),
    mainPanel(
      # Nueva distribución: En columnas para poder observar todo el contenido
      fluidRow(
        column(width = 6, 
               h4("Distribución Espacial"),
               leafletOutput(outputId = "mapa", height = "400px")
        ),
        column(width = 6, 
               h4("Tendencia Histórica"),
               dygraphOutput(outputId = "grafico_temporal", height = "400px")
        )
      ),
      hr(), # Línea divisoria horizontal
      fluidRow(
        column(width = 12, 
               h4("Explorador de Datos"),
               DTOutput(outputId = "tabla")
        )
      )
    )
  )
)

# server()
server <- function(input, output){ 
  
  # Salida 1: Tabla Interactiva ==================================================================
  output$tabla <- renderDT({
    datatable(
      data,
      rownames = TRUE, # Mantenemos los índices
      colnames = c(
        "ID Condado" = "county", 
        "Género" = "gender", 
        "Raza" = "race", 
        "Año" = "year", 
        "Casos" = "cases", 
        "Población" = "population", 
        "Nombre" = "NAME"
      ),
      options = list(
        language = list(
          search = "Buscar en la tabla:",
          lengthMenu = "Mostrar _MENU_ registros por página",
          info = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_",
          infoEmpty = "No hay registros disponibles",
          infoFiltered = "(filtrado de un total de _MAX_ registros)",
          paginate = list(
            previous = "Anterior",
            `next` = "Siguiente"
          )
        )
      )
    )
  })
  
  # Salida 2: Gráfico temporal  ==================================================================
  output$grafico_temporal <- renderDygraph({
    
    # Esto te mostrará una tabla con el nombre, el año y cuántas veces aparece
    dataxts <- NULL
    counties <- unique(data$NAME)
    for (l in 1:length(counties)) {
      
      datacounty <- data[data$NAME == counties[l], ]
      dd <- xts(
        datacounty[, input$Variable_seleccionada],
        as.Date(paste0(datacounty$year, "-01-01"))
      )
      dataxts <- cbind(dataxts, dd)
    }
    colnames(dataxts) <- counties
    
    dygraph(dataxts) %>%
      dyEvent(paste0(input$anio_seleccionado, "-01-01"), "Año en Mapa", labelLoc = "top", color = "red", strokePattern = "dashed") %>%
      dyRangeSelector() %>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.2) -> d1
    
    d1$x$css <- "
 .dygraph-legend > span {display:none;}
 .dygraph-legend > span.highlight { display: inline; }
 "
    d1
  })
  
  ## Salida 3: Mapa interactivo  ==================================================================
  output$mapa <- renderLeaflet({
    
    # Add data to map
    datafiltered <- data[which(data$year == input$anio_seleccionado), ]
    
    ## Modificación: debido a que los ficheros utilizados son diferentes, 
    ## se ha modificado para que coincidan por el nombre del condado
    ordercounties <- match(map@data$NAME, datafiltered$NAME)
    
    map@data <- datafiltered[ordercounties, ]
    
    
    # Crear variableplot
    map$variableplot <- as.numeric(
      map@data[, input$Variable_seleccionada]
    )
    
    # Create leaflet
    pal <- colorBin("magma", domain = map$variableplot, bins = 7)
    
    labels <- sprintf("%s: %g", map$county, map$variableplot) %>%
      lapply(htmltools::HTML)
    
    l <- leaflet(map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(variableplot),
        color = "#D4AF37",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~variableplot,
        opacity = 0.7, title = NULL
      )
  })
  
    
      
  
  
  #fin funcion server
  }

# shinyApp()
shinyApp(ui = ui, server = server)
