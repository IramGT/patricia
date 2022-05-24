## LIBRARIES

library(ggplot2)
library(dplyr)
library(readr)
library(shiny)
library(shinyjs)
library(DT)

## DATA BASES IN GOOGLE SHEETS

seccionCSV <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSRiEMncSYN59z1lcOSDVmfm7bi1l3esl5Wc8fUMWW7c6ZFgUJaX3tZetQQKmB4Vgi87XF8V96KEQMZ/pub?gid=1588389477&single=true&output=csv")
Objetivos <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRLELZEMRj9seabP6vCTMvfZ4Z4RMvMtW_NwGduzmT8QS9oRhHZaE6m_9ea9aLeYK6EWoRDaCwIKjRZ/pub?gid=0&single=true&output=csv")
seccionesHidalgo <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS7bZ9fVqxd2Yl5HtsS-6kXvWAkv0vgBaN6Zb9v8cQzwZSwLumSBSY-rATijw5NDg_1IcipjeDoprBL/pub?gid=0&single=true&output=csv")
seccionApp <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQlBcu3mg4wYIR2YPAeoV1S8oHRmaLk_g75J-eOeIVs8g2f0u5HpAXmmfQRbHG7d_lp2WnI1qba1frj/pub?gid=0&single=true&output=csv")
segmento <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQXwO1NLMok59vOSTR-obAguqFQfoop5X96D1fZSU5teNigONPL91gvM1KcP0GmNw3f8-S9pJFdaFp0/pub?gid=0&single=true&output=csv")

## CHANGE THE NAME OF THE SECCION DATA BASE

seccionCSV <- seccionCSV %>% 
  rename(MarcaTemporal = `Marca temporal`, Promotor = `Nombre completo del promotor`, Promovido = `Nombre completo del promovido en orden de APELLIDO PATERNO, MATERNO Y NOMBRE(S)`,
         Celular = `Celular del promovido`, CPostal = `CODIGO POSTAL de la credencial de elector`) 

seccionApp <- seccionApp %>% 
  rename(Promotor = 'Promovente')

## THE VARIABLE IS NEEDED AS NUMERIC FOR THE LEFT JOIN

seccionCSV$seccion <- as.numeric(seccionCSV$seccion)
seccionCSV$Celular <- as.numeric(seccionCSV$Celular)
seccionApp$Celular <- as.numeric(seccionApp$Celular)
seccionCSV$CPostal <- as.numeric(seccionCSV$CPostal)
seccionApp$CPostal <- as.numeric(seccionApp$CPostal)
seccionesHidalgo$sede <- as.numeric(seccionesHidalgo$sede)

seccionCSV <- bind_rows(seccionCSV, seccionApp)

#### UI ####

ui <- (
  # Choices for drop-downs
  navbarPage(h4("IramG", style='color:#B3282D'), 
             id="nav",
             tabPanel("PROMOTOR", h3(img(src = "JM.jpg", height = 94, width = 186)) , tableOutput("Total"), DT::dataTableOutput("ziptable")),
             tabPanel("DESCARGA", h3(img(src = "JM.jpg", height = 94, width = 186)) , DT::dataTableOutput("downloadtable"))
  ) ) 




#### SERVER ####

server <- function(input, output) {
  
    
   
    output$Total <- renderTable({
     Total <- seccionCSV %>%
       group_by(Promotor) %>%
       summarise(suma = n()) %>%
       na.omit() %>% 
       left_join(segmento) %>% 
       filter(Segmento == 'PATRICIA HERNANDEZ') %>% 
       summarise(Total = sum(suma))
    }    )
    
    output$ziptable <- DT::renderDataTable({
      tabla <- seccionCSV %>% 
        group_by(Promotor) %>% 
        summarise(Resultados = n()) %>% 
        mutate(Porcentaje = round(100*Resultados / sum(Resultados),1)) %>% 
        na.omit() %>% 
        left_join(segmento) %>% 
        filter(Segmento == 'PATRICIA HERNANDEZ') %>% 
        select(Promotor, Resultados ) %>% 
        arrange(-Resultados) 
      
      DT::datatable(tabla, options = list(pageLength = 100))
    })
    
    
    output$downloadtable <- DT::renderDataTable({
      tabla2 <- seccionCSV %>% 
        left_join(segmento) %>% 
        filter(Segmento == 'PATRICIA HERNANDEZ') %>% 
        select(Promotor,  Promovido, "Domicilio de la credencial de elector", Celular, seccion, CPostal, MunicipioN )
      
      DT::datatable(tabla2, extensions = 'Buttons', options = list(pageLength = 1000, dom = 'Bfrtip', buttons = list('copy', 'print', list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'), text = 'Download'))))
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
