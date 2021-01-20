library(shiny)

shinyServer(function(input, output) {
  
  #gráfico ----
  output$grafico_constitucion <- ggiraph::renderGirafe({
    
    p <- constitucion %>%
      #filter(linea_capitulo == 5) %>%
      filter(!(palabra %in% stopwords)) %>% #filtrar stopwords
      filter(nchar(palabra) >= 4) %>% 
      filter(palabra != "Artículo") %>%
      #destacar
      mutate(palabras_clave = "Otros") %>%
      # destacar_palabra_elegida("política", partial_match = T) %>%
      # destacar_palabra_elegida("salud") %>%
      # destacar_palabra_elegida("agua") %>%
      # destacar_palabra_elegida("guerra") %>%
      # destacar_palabra_elegida("enemigo", partial_match = T) %>%
      destacar_palabra_elegida(input$palabra_1, partial_match = input$palabra_1_switch) %>%
      destacar_palabra_elegida(input$palabra_2, partial_match = input$palabra_2_switch) %>%
      destacar_palabra_elegida(input$palabra_3, partial_match = input$palabra_3_switch) %>%
      destacar_palabra_elegida(input$palabra_4, partial_match = input$palabra_4_switch) %>%
      destacar_palabra_elegida(input$palabra_5, partial_match = input$palabra_5_switch) %>%
      destacar_palabra_elegida(input$palabra_6, partial_match = input$palabra_6_switch) %>%
      destacar_palabra_elegida(input$palabra_7, partial_match = input$palabra_7_switch) %>%
      #graficar
      ggplot(aes(n_palabra_escala, y = n_linea_rev)) +
      #capa de abajo
      geom_jitter(data = . %>% filter(palabras_clave == "Otros"), 
                  # aes(size = palabras_alpha,
                  #      alpha = palabras_alpha), 
                  color = "gray70",
                  alpha=0.1,
                  size = 1.5,
                  height = 0, width=0.02) +
      #capa de arriba
      geom_jitter(data = . %>% filter(palabras_clave != "Otros"), aes(color = palabras_clave),
                  #size = nchar(palabra),
                  #size = palabras_alpha,
                  #alpha = palabras_alpha), 
                  alpha = 1,
                  size = 3,
                  height = 0, width=0.02) +
      facet_wrap(~linea_capitulo, ncol = 1, scales="free",
                 strip.position = "left") +
      coord_cartesian(clip="off") +
      scale_color_brewer(palette = "Set2", direction = -1) +
      #scale_alpha_manual(values = c(0.1, 1)) +  
      #scale_size_manual(values = c(1, 2.5)) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
      theme_minimal() +
      tema_constitucion +
      guides(size = FALSE,
             alpha = FALSE,
             col = guide_legend(reverse = TRUE, ncol = 1,
                                override.aes = list(size=4, fill=NA, text=NA)))
    
    #output girafe ----
    girafe(ggobj = p,
           pointsize = 12,
           width_svg =10, height_svg = 7,
           fonts = list(sans = "Open Sans"),
           options = list(
             opts_sizing(rescale = FALSE, width = 1),
             opts_toolbar(position = "topright", saveaspng = FALSE)))
    
  })
  
})
