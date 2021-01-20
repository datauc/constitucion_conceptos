library(dplyr)
library(ggplot2)
library(ggiraph)
library(shinyWidgets)


#datos ----
load("stopwords.Rdata")

#texto <- pdftools::pdf_text("constitucion.pdf")

constitucion <- readr::read_rds("constitucion.rds")

#funciones ----

#switch <- 

switch_parcial <- function(numero = NULL, valor = "") {
  
  column(12, style = "padding: 0px;", 
  column(8, style = paste0("padding: 0px;"),
         textInput(inputId = paste0("palabra_", numero), 
                   label = paste0("Palabra ", numero), 
                   value = valor, width = "90%", 
                   placeholder = "Ingrese un término"),
  ),
  column(4, style = "padding: 0px; margin-top: 5px;",
         br(),
         switchInput(
           inputId = paste0("palabra_", numero, "_switch"),
           label = "parcial", 
           width = "100%",
           onLabel = "sí",
           offLabel = "no",
           inline = TRUE
         )
  )
  )
  
}

#función que marca en una columna si se detecta la palabra entregada
destacar_palabra_elegida <- function(datos, palabra_elegida = "", partial_match = FALSE) {
  #por si no hay palabra
  #if (is.null(palabra_elegida) | palabra_elegida == "" ) {
  if (palabra_elegida == "" ) {
    
  } else {
    
    #match exacto con str_detect
    if (partial_match == FALSE) {
      
      datos <- datos %>%
        ungroup() %>%
        mutate(palabras_clave = case_when(stringr::str_detect(palabra, stringr::regex(as.character(palabra_elegida), ignore_case = T)) ~ stringr::str_to_sentence(palabra_elegida),
                                          TRUE ~ as.character(palabras_clave)),
               palabras_clave = as.factor(palabras_clave)) #%>%
        #mutate(palabras_clave = forcats::fct_relevel(palabras_clave, "Otros", after = 0, )) %>%
        #mutate(palabras_alpha = if_else(palabras_clave == "Otros", "No", "Sí"))
      
      #match parcial con stringdist
    } else if (partial_match == TRUE) {
      
      datos <- datos %>%
        ungroup() %>%
        mutate(palabras_clave = case_when(stringdist::ain(tolower(palabra), tolower(palabra_elegida), maxDist=4) ~ stringr::str_to_sentence(palabra_elegida),
                                          TRUE ~ as.character(palabras_clave)),
               palabras_clave = as.factor(palabras_clave)) #%>%
        #mutate(palabras_clave = forcats::fct_relevel(palabras_clave, "Otros", after = 0, )) %>%
        #mutate(palabras_alpha = if_else(palabras_clave == "Otros", "No", "Sí"))
      
    }
  }
  return(datos)
}

#tema ----
tema_constitucion <- theme(#strip.background = element_blank(),
  #strip.text = element_blank(),
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  legend.position = "right",
  legend.title = element_blank(),
  legend.text = element_text(size = 10, margin = margin(r = 12, l = -4)),
  strip.text = element_text(hjust=0.5, vjust=0, size = 12),
  panel.spacing.y = unit(0.1, "cm"))
