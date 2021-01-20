library(shiny)

shinyUI(fluidPage(

    titlePanel(title = NULL,
               windowTitle = "Constitución"),
    
    #tipografías
    tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Open Sans');")), # Importar Open Sans

    #header ----
    fluidRow(
        column(12,
               h1("Constitución"),
               HTML("<p>Esta herramienta permite destacar conceptos presentes en el texto de la 
                     <a href='https://www.oas.org/dil/esp/Constitucion_Chile.pdf' 
                        target='_blank'
                        style='color: #999999'>
                    Constitución Política de la República de Chile.</a></p>"),
               HTML("<p>Escriba en los campos disponibles abajo para que se seleccionen dichos 
                 conceptos en el gráfico. El gráfico representa la constitución línea 
                 por línea, separada en bloques por capítulo, donde cada punto equivale 
                 a una palabra. La opción <em>parcial</em> permite coincidir con términos levemente distintos al escrito (por ejemplo, escribir <em>educación</em> con la opción activada también coincidirá con <em>educacional).</em></p>"),
               #htmlOutput("texto_filtro"),
               hr(),
        )
    ),
    
    #inputs ----
    fluidRow(
        column(4,
               
               switch_parcial("1", "Salud"),
               
               switch_parcial("2", "Educación"),
               
               switch_parcial("3"),
               
               switch_parcial("4"),
               
               switch_parcial("5"),
               
               switch_parcial("6"),
               
               switch_parcial("7"),
               
               # actionButton(
               #     inputId = "más",
               #     label = NULL,
               #     #style = "simple", 
               #     #color = "primary",
               #     icon = icon("plus")
               # )
        ),
        
        #gráfico ----
        column(8,
               
               girafeOutput("grafico_constitucion") %>% shinycssloaders::withSpinner()
               
        )
    ),
    
    #footer ----
    fluidRow(
        column(12, align = "center",
               
               hr(),
               p("Plataforma desarrollada por el equipo DATA UC, usando R Shiny"),
               
               HTML("<p>Diseño y desarrollo: 
                    <a href='http://bastian.olea.biz' 
                       style='color: #999999'>
                    Bastián Olea Herrera.</a></p>"),
               
               p("Facultad de Matemáticas"),
               p("Pontificia Universidad Católica de Chile"),
               tags$a(img(
                   src = "logodatauc.png",
                   width = 200, style = "padding: 10px"
               ),
               #href = "http://www.mat.uc.cl"
               hreg = "http://datascience.uc.cl"
               )
               
               
        )) #end footer
))
