library(tidytext)
library(readr)
library(dplyr)
library(ggplot2)

#descargar pdf, de ser necesario
#download.file("https://www.oas.org/dil/esp/Constitucion_Chile.pdf",
#              "constitucion.pdf")

#importar pdf
texto <- pdftools::pdf_text("constitucion.pdf")


#importar stopwords ----
# descargarlas
# stopwords <- readtext::readtext("https://raw.githubusercontent.com/stopwords-iso/stopwords-es/master/stopwords-es.txt") %>%
#   unnest_tokens(palabra, text) %>% 
#   tibble() %>%
#   select(palabra) %>%
#   filter(nchar(palabra) > 1) %>% 
#   pull()

#cargar
load("stopwords.Rdata")



#constitución por líneas ----
#procesa la constitución para producir un data frame que contenga las líneas del pdf y también cada palabra en cada línea
constitucion <- texto %>%
  #convertir a un data frame tipo tibble (por comodidad)
  tibble() %>%
  #generar número de cada párrafo (vienen del pdf)
  mutate(n_parrafo = 1:n()) %>%
  rename(caja = 1) %>%
  #desarmar el texto en líneas
  unnest_tokens(linea, caja, 
                token = "regex", pattern = "\n", #separar palabras en los espacios
                to_lower = FALSE) %>% #sin tirar a minúsculas
  mutate(n_linea = 1:n()) %>% #crear número de cada línea
  mutate(linea = stringr::str_trim(linea)) %>% #borrar espacios al principio de las líneas
  #recortar índice al inicio, e índice analítico al final, para no considerarlos
  filter(n_linea >= 48) %>% #título de la constitución
  filter(n_linea < 2903) %>% #título del índice analítico
  mutate(n_linea = 1:n()) %>% #rehacer líneas
  #desarmar las líneas en palabras
  unnest_tokens(input = linea, 
                output = palabra,
                token = "words", to_lower = FALSE, drop = FALSE) %>%
  group_by(n_linea) %>%
  mutate(n_palabra = 1:n()) %>% #numero de palabra en cada línea
  #detectar y marcar capítulos
  ungroup() %>%
  mutate(linea_capitulo = case_when(palabra %in% c("CAPITULO", "CAPÍTULO") ~ n_linea)) %>% #detectar palabras
  tidyr::fill(linea_capitulo) %>% #rellenar datos hacia abajo
  mutate(linea_capitulo = as.numeric(as.factor(as.character(linea_capitulo)))) %>% #convertir secuencia a numeros de 1 en 1
  mutate(linea_capitulo = tidyr::replace_na(linea_capitulo, 1)) %>% #capítulo 0 son las líneas antes del capítulo 1
  #líneas inversas y escala horizontal
  ungroup() %>%
  mutate(n_linea_rev = max(n_linea)-n_linea) %>% #orden de línea inverso para eje y (la primera línea es el máximo, y la última línea es 1, cosa que al graficar el "texto" se vea en orden de arriba hacia abajo
  group_by(n_linea) %>%
  #aplicar escala a la posición de palabra, para "justificar" los puntos igual como se haría en Word
  mutate(n_palabra_escala = (n_palabra - min(n_palabra) ) / (max(n_palabra) - min(n_palabra) ), 
         n_palabra_escala = replace(n_palabra_escala, is.na(n_palabra_escala), 0.1),
         n_palabra_escala = replace(n_palabra_escala, n_palabra_escala==0, 0.03),
         n_palabra_escala = replace(n_palabra_escala, n_palabra_escala==1, 0.97)) %>%
  ungroup() %>%
  #anexar líneas contextuales ----
left_join(constitucion %>%
  filter(!(palabra %in% stopwords)) %>% #filtrar stopwords
  filter(nchar(palabra) >= 4) %>%
  select(!c(palabra, n_palabra, n_palabra_escala)) %>%
  distinct() %>%
  mutate(linea_contexto = paste0(lag(linea, default = ""), " ",
                                 linea, " ",
                                 lead(linea, default = ""))) %>%
    mutate(linea_contexto = stringr::str_trim(linea_contexto, side="left")) %>%
  select(n_linea, linea_contexto), by = "n_linea")


constitucion
glimpse(constitucion)

readr::write_rds(constitucion, file = "constitucion.rds")

#funciones ----
destacar_palabras_clave <- function(datos) {
  datos %>%
    mutate(palabras_clave = case_when(stringr::str_detect(palabra, "famili") ~ "Familia",
                                      #stringr::str_detect(palabra, "chil") ~ "Chile",
                                      stringr::str_detect(palabra, "social|sociedad") ~ "Social",
                                      stringr::str_detect(palabra, "trabaj|empleo") ~ "Trabajo",
                                      stringr::str_detect(palabra, "derech") ~ "Derechos",
                                      stringr::str_detect(palabra, "empre|negoc") ~ "Empresas",
                                      stringr::str_detect(palabra, "priva") ~ "Privado",
                                      stringr::str_detect(palabra, "econ") ~ "Economía",
                                      stringr::str_detect(palabra, "merca") ~ "Mercado",
                                      TRUE ~ "Otros"),
           palabras_clave = forcats::fct_relevel(after = 0, palabras_clave, "Otros")) %>%
    mutate(palabras_alpha = if_else(palabras_clave=="Otros", "No", "Sí"))
}

#función que marca en una columna si se detecta la palabra entregada
destacar_palabra_elegida <- function(datos, palabra_elegida = NULL, partial_match = FALSE) {
  #por si no hay palabra
  if (is.null(palabra_elegida) | palabra_elegida == "" ) {
    
  } else {
    
    #match exacto con str_detect
    if (partial_match == FALSE) {
      
      datos <- datos %>%
        ungroup() %>%
        mutate(palabras_clave = case_when(stringr::str_detect(palabra, stringr::regex(as.character(palabra_elegida), ignore_case = T)) ~ stringr::str_to_sentence(palabra_elegida),
                                          TRUE ~ as.character(palabras_clave)),
               palabras_clave = as.factor(palabras_clave)) %>%
        mutate(palabras_clave = forcats::fct_relevel(palabras_clave, "Otros", after = 0, )) %>%
        mutate(palabras_alpha = if_else(palabras_clave == "Otros", "No", "Sí"))
      
      #match parcial con stringdist
    } else if (partial_match == TRUE) {
      
      datos <- datos %>%
        ungroup() %>%
        mutate(palabras_clave = case_when(stringdist::ain(tolower(palabra), tolower(palabra_elegida), maxDist=2) ~ stringr::str_to_sentence(palabra_elegida),
                                          TRUE ~ as.character(palabras_clave)),
               palabras_clave = as.factor(palabras_clave)) %>%
        mutate(palabras_clave = forcats::fct_relevel(palabras_clave, "Otros", after = 0, )) %>%
        mutate(palabras_alpha = if_else(palabras_clave == "Otros", "No", "Sí"))
      
    }
  }
  return(datos)
}

#prueba
constitucion %>%
  mutate(palabras_clave = "Otros") %>%
  destacar_palabra_elegida("política") %>%
  destacar_palabra_elegida("santiago")

#prueba
constitucion %>%
  mutate(palabras_clave = "Otros") %>%
  destacar_palabra_elegida("política", partial_match = T) %>%
  destacar_palabra_elegida("santiago")

#output: líneas cercanas
constitucion %>%
  mutate(palabras_clave = "No") %>%
  destacar_palabra_elegida("hogar") %>%
  #destacar_palabra_elegida("santiago") %>%
  ###
  mutate(linea_palabras_clave = case_when(palabras_clave != "No" ~ n_linea)) %>%
  group_by(n_linea) %>%
  mutate(linea_palabras_clave = case_when(!is.na(lead(linea_palabras_clave)) ~ n_linea,
                                          !is.na(lag(linea_palabras_clave)) ~ n_linea,
                                          TRUE ~ linea_palabras_clave)) %>%
  filter(!is.na(linea_palabras_clave))
filter(n_linea >= linea_palabras_clave-3 & n_linea <= linea_palabras_clave+3)

#obtener líneas donde salen las palabras
constitucion %>%
  mutate(palabras_clave = "No") %>%
  destacar_palabra_elegida("facultades") %>%
  #destacar_palabra_elegida("santiago") %>%
  ###
  mutate(linea_palabras_clave = case_when(palabras_clave != "No" ~ n_linea)) %>%
  select(linea_palabras_clave) %>% na.omit() %>% pull() -> lineas_2


#repetir con un magen de 3 arriba y abajo
lineas_3_arriba_abajo <- c(lineas_2 + rep(0:3, each = length(lineas_2)),
                           lineas_2 + rep(0:-3, each = length(lineas_2)))

#filtrar
constitucion %>%
  select(-c(palabra, n_palabra, n_palabra_escala)) %>%
  distinct() %>%
  filter(n_linea %in% lineas_3_arriba_abajo) %>%
  print(n=200)


#Tema ----
tema_constitucion <- theme(#strip.background = element_blank(),
  #strip.text = element_blank(),
  panel.spacing.y = unit(0, "cm"),
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.text = element_text(margin = margin(r = 12, l = -4)))

#Graficar palabras clave ----
constitucion %>%
  filter(linea_capitulo == 5) %>%
  #filter(n_linea < 500) %>%
  ungroup() %>%
  filter(!(palabra %in% stopwords)) %>% #filtrar stopwords
  filter(nchar(palabra) > 1) %>% 
  filter(palabra != "Artículo") %>%
  #destacar
  destacar_palabras_clave() %>%
  #graficar
  ggplot(aes(n_palabra_escala, y = n_linea_rev)) +
  geom_jitter(aes(color = palabras_clave,
                  #size = nchar(palabra),
                  size = palabras_alpha,
                  alpha = palabras_alpha), 
              height = 0, width=0.02) +
  #geom_hline(yintercept=lineas, alpha=0.4) + #agregar capítulos
  coord_cartesian(clip="off") +
  #scale_color_discrete(h = c(0, 450), c=70, h.start = 100, l = 75, 
  #                     na.value = "black") +
  scale_color_brewer(palette = "Set2", direction = -1) +
  scale_alpha_manual(values = c(0.1, 1)) +  
  scale_size_manual(values = c(1, 2.5)) +
  theme_minimal() +
  tema_constitucion +
  guides(size = FALSE,
         alpha = FALSE,
         col = guide_legend(reverse = TRUE, 
                            override.aes = list(size=2, fill=NA, text=NA)))


#Graficar palabras elegidas ----
constitucion %>%
  #filter(linea_capitulo == 5) %>%
  filter(!(palabra %in% stopwords)) %>% #filtrar stopwords
  filter(nchar(palabra) >= 4) %>% 
  filter(palabra != "Artículo") %>%
  #destacar
  mutate(palabras_clave = "Otros") %>%
  destacar_palabra_elegida("política", partial_match = T) %>%
  destacar_palabra_elegida("salud") %>%
  destacar_palabra_elegida("agua") %>%
  destacar_palabra_elegida("guerra") %>%
  destacar_palabra_elegida("enemigo", partial_match = T) %>%
  #graficar
  ggplot(aes(n_palabra_escala, y = n_linea_rev)) +
  #capa de abajo
  geom_jitter(data = . %>% filter(palabras_clave == "Otros"), 
              aes(size = palabras_alpha,
                  alpha = palabras_alpha), 
              color = "gray70",
              height = 0, width=0.02) +
  #capa de arriba
  geom_jitter(data = . %>% filter(palabras_clave != "Otros"), aes(color = palabras_clave,
                                                                  #size = nchar(palabra),
                                                                  size = palabras_alpha,
                                                                  alpha = palabras_alpha), 
              height = 0, width=0.02) +
  facet_wrap(~linea_capitulo, ncol = 1, scales="free",
             strip.position = "left") +
  coord_cartesian(clip="off") +
  scale_color_brewer(palette = "Set2", direction = -1) +
  scale_alpha_manual(values = c(0.1, 1)) +  
  scale_size_manual(values = c(1, 2.5)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  theme_minimal() +
  tema_constitucion +
  theme(strip.text = element_text(hjust=0.5, vjust=0, size = 7),
        panel.spacing.y = unit(0.1, "cm")) +
  guides(size = FALSE,
         alpha = FALSE,
         col = guide_legend(reverse = TRUE, 
                            override.aes = list(size=2, fill=NA, text=NA)))

#pasar a ggiraph
#agrear interactive a la capa de arriba

stringdist::ain("constitución", "destitucional", maxDist=4)
