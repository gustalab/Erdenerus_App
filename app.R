
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(tidyquant)
library(shinythemes)
library(shinydashboard)
#install.packages("shinyBS")
library(shinyBS)
library(reactable)
library(formattable)
library(plotly)
library(scales)
library(shinyalert)
library(DT)
library(writexl)
library(readxl)
library(collapsibleTree)
library(highcharter)
library(DT)
library(lubridate)
# Map
library(sf)
#library(mapview)
library(leaflet)
library(modelr)
library(caret)
library(billboarder)

#install.packages("shinyMatrix")
library(shinyMatrix)
#install.packages("DT")
library(DT)
#install.packages("rhandsontable")
library(rhandsontable)

# library(bslib)
# install.packages("showtext")
# library(showtext)
# install.packages("thematic")
# library(thematic)
# Load Data Sources

# source("../app_bilirkisi/hesaplamalar.R")

# Functions & Tables----

kaza_t <- as.Date("2018-01-01")

input_data <- data.frame(Brand = seq(from = kaza_t , to = Sys.Date(), by = "year"),
                         Gelir = 0,
                         stringsAsFactors = FALSE) %>%
  mutate(Yıl = format(Brand, format("%Y"))) %>%
  select(Yıl, Gelir)


### Module
modFunction <- function(input, output, session, data, reset) {
  
  v <- reactiveValues(data = data)
  
  proxy = dataTableProxy("mod_table")
  
  observeEvent(input$mod_table_cell_edit, {
    print(names(v$data))
    info = input$mod_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    k = info$value
    str(info)
    
    isolate(
      if (j %in% match("Gelir", names(v$data))) {
        print(match("Gelir", names(v$data)))
        v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])
        print(v$data)
        
      } else {
        print("You are not supposed to change this column.") # check to stop the user from editing only few columns
      }
    )
    replaceData(proxy, v$data, resetPaging = FALSE)  # replaces data displayed by the updated table
  })
  
  ### Reset Table
  observeEvent(reset(), {
    v$data <- data # your default data
  })
  
  print(isolate(colnames(v$data)))
  output$mod_table <- DT::renderDataTable({
    DT::datatable(v$data, options = list(paging = F, searching = F), editable = TRUE)
    
  })
  
  return(v)
}

modFunctionUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("mod_table"))
  
}

# Theme 

# my_theme <- bs_theme(bootswatch = "darkly",
#                      base_font = font_google("Righteous"))

ui <- fluidPage(
  
  # CSS ----
  #shinythemes::themeSelector(),
  
  # JS ----
  shinyjs::useShinyjs(), # Include shinyjs
  
  # theme = my_theme,

  theme = shinytheme("sandstone"),
  
  navbarPage(
    
    #theme = shinytheme("spacelab"), 
    title = "Sigortacılık Hesaplama App", 
    
    
    
    navbarMenu("Hesaplamalar", icon = icon("list-ul"),
               
               tabPanel(title = "Maluliyet",
                        
                        div(
                          
                          div(
                            
                            column(
                              width = 1,
                              actionBttn(inputId = "action_button_verigirisi", 
                                         label = "Veri Girişi",
                                         style = "gradient",
                                         color = "primary",
                                         icon = icon("cog"))
                            ),
                            
                            column(
                              width = 1,
                              actionBttn(inputId = "action_button_sonuclar", 
                                         label = "Sonuçlar",
                                         style = "gradient",
                                         color = "primary",
                                         icon = icon("cog"))
                            )
                            
                            
                          ),
                          
                          br(),
                          hr(),
                          br(),
                          
                          div(
                            
                            id= "dataentry",
                            
                            fluidRow(
                              
                              column(
                                width = 2,
                                wellPanel(
                                  
                                  div(
                                    id = "hesap_yontem",
                                    actionButton(inputId = "action_button5", label = "Hesaplama Yöntemi", icon = icon("cog"))
                                  ),
                                  
                                  hr(),
                                  
                                  div(
                                    id="settings_toggle5",
                                    # prettyRadioButtons(
                                    #   inputId = "yontem",
                                    #   label = "Hesaplama Yöntemi", 
                                    #   choices = c("Progresif Rant", "Aktüeryal", "Mortalite Tablosu"),
                                    #   icon = icon("table"), outline = T, thick = T, bigger = T, width = '400px',fill = T,
                                    #   animation = "jelly"
                                    # ),
                                    
                                    radioGroupButtons(
                                      inputId = "yontem",
                                      # label = "Hesaplama Yöntemi",
                                      choices = c("Progresif Rant", "Aktüeryal"),
                                      individual = T,
                                      width = '400px',
                                      justified = T, 
                                      direction = "vertical",
                                      size = "sm",
                                      checkIcon = list(
                                        yes = tags$i(class = "fa fa-circle",
                                                     style = "color: red"),
                                        no = tags$i(class = "fa fa-circle-o",
                                                    style = "color: steelblue"))
                                    ),
                                    
                                    
                                    conditionalPanel(
                                      condition = "input.yontem == 'Progresif Rant'",
                                      
                                      awesomeCheckboxGroup(
                                        inputId = "tablo1",
                                        label = "Tablo", 
                                        choices = c("PMF", "TRH-2010", "CSO-1980"),
                                        selected = "TRH-2010",
                                        inline = TRUE, 
                                        status = "danger"
                                      ),
                                      
                                      textOutput("teknikfaiztext"),
                                      actionBttn(inputId = "info_button1", 
                                                 #label = "Veri Girişi",
                                                 style = "minimal", #"simple" , #"minimal", “bordered”, “stretch”, “jelly”, “gradient”, “fill”, “material-circle”, “material-flat”, “pill”, “float”, “unite”  
                                                 size = "sm",
                                                 color = "royal",
                                                 icon = icon("fa-solid fa-percent")),
                                      div(
                                        id="info1",
                                        helpText("Progresif Rant yönteminde Teknik Faiz % 0 olarak hesaplanmaktadır.")
                                        
                                      ) %>% hidden(),
                                      
                                      #infoBoxOutput("ratio")
                                      
                                      
                                    ),
                                    
                                    conditionalPanel(
                                      condition = "input.yontem == 'Aktüeryal'" ,
                                      
                                      awesomeCheckboxGroup(
                                        inputId = "tablo2",
                                        label = "Tablo", 
                                        choices = c("PMF", "TRH-2010", "CSO-1980"),
                                        selected = "TRH-2010",
                                        inline = TRUE, 
                                        status = "danger"
                                      ),
                                      
                                      numericInputIcon(
                                        inputId = "teknik_faiz",
                                        label = "Teknik Faiz",
                                        value = 1.8, step = 0.1,
                                        min = 0, max = 20,
                                        icon = icon("percent")
                                      )
                                    )
                                    
                                  ) %>% hidden()
                                  
                                  
                                )
                                
                              ), # Hesaplam Yöntemi end
                              
                              
                              column(
                                width = 2,
                                
                                wellPanel(
                                  div(
                                    id = "genel_bilgiler",
                                    actionButton(inputId = "action_button1", label = "Genel Bilgiler", icon = icon("cog"))
                                  ),
                                  
                                  hr(),
                                  
                                  
                                  div(
                                    id="settings_toggle1",
                                    p(tags$b("Genel Bilgiler", style = "font-weight: bold; color: red;")),
                                    
                                    
                                    textInput(inputId = "dosya",label = "Dosya No", width = 250),
                                    textInput(inputId = "isim",label = "Ad-Soyad", width = 250),
                                    prettyRadioButtons(
                                      inputId = "cinsiyet",
                                      label = "Cinsiyet", 
                                      choices = c("Erkek", "Kadın"),
                                      icon = icon("check"), 
                                      bigger = TRUE,
                                      status = "warning",
                                      animation = "jelly", selected = "Erkek", fill = FALSE, inline = TRUE
                                    ),
                                    
                                    p(tags$b("Doğum Tarihi")),
                                    dateInput("dogumtarihi", label = NULL),
                                    br(),
                                    p(tags$b("Kaza Tarihi")),
                                    dateInput("kazatarihi", label = NULL),
                                    br(),
                                    p(tags$b("Maluliyet Oranı")),
                                    # sliderInput("maluliyet", label = NULL, min = 0, 
                                    #             max = 100, value = 50,step = 1),
                                    
                                    numericInputIcon(
                                      inputId = "maluliyet",
                                      label = NULL,
                                      value = 0, step = 0.1,
                                      min = 0, max = 100,
                                      icon = icon("calendar")
                                    ),
                                    p(tags$b("Kusur Oranı")),
                                    sliderInput("kusur", label = NULL, min = 0, 
                                                max = 100, value = 50,step = 5),
                                    p(tags$b("Geçici Maluliyet (Ay)")),
                                    awesomeRadio("gecici_maluliyet", label = NULL,
                                                 choices = c("Var", "Yok"),
                                                 selected = "Yok", inline=TRUE, checkbox = TRUE),
                                    conditionalPanel(
                                      condition = "input.gecici_maluliyet == 'Var'",
                                      
                                      numericInputIcon(
                                        inputId = "maluliyet_sure",
                                        label = NULL,
                                        value = 0, step = 0.1,
                                        min = 0, max = 120,
                                        icon = icon("calendar")
                                      )
                                      
                                    )
                                    
                                    #p(tags$b("Teknik Faiz")),
                                    
                                    
                                  ) %>% hidden()
                                  
                                )
                                
                                
                              ), # Genel Bilgiler end
                              
                              column(
                                width = 2,
                                
                                wellPanel(
                                  div(
                                    id = "sirket_bilgiler",
                                    actionButton(inputId = "action_button2", label = "Şirket Ödemeleri", icon = icon("cog"))
                                  ),
                                  
                                  hr(),
                                  
                                  div(
                                    id="settings_toggle2",
                                    p(tags$b("Şirket Ödemeleri", style = "font-weight: bold; color: red;")),
                                    
                                    p(tags$b("Kısmi Ödeme Sayısı")),
                                    numericInput("kısmiodeme", label = NULL, value = 0, min = 0, max = 3),
                                    
                                    conditionalPanel(
                                      condition = "input.kısmiodeme == '1'",
                                      
                                      dropdownButton(label = "Kısmi Ödeme Bilgileri",
                                                     
                                                     div( id = "1",
                                                          wellPanel(
                                                            p(tags$b("Kısmi Ödeme Tarihi-1")),
                                                            dateInput("kısmiodemetarihi1", label = NULL),
                                                            numericInputIcon(
                                                              inputId = "kö1",
                                                              label = "Kısmi Ödeme Tutarı-1",
                                                              value = 0, step = 0.1,
                                                              icon = list(icon("turkish-lira-sign"))
                                                            )
                                                          )
                                                     ),
                                                     circle =FALSE, status = "danger",
                                                     icon = icon("turkish-lira-sign"), width = "300px"
                                      )
                                    ),
                                    
                                    conditionalPanel(
                                      condition = "input.kısmiodeme == '2'",
                                      
                                      
                                      dropdownButton(label = "Kısmi Ödeme Bilgileri",
                                                     
                                                     div( id = "2",
                                                          wellPanel(
                                                            p(tags$b("Kısmi Ödeme Tarihi-1")),
                                                            dateInput("kısmiodemetarihi2", label = NULL),
                                                            numericInputIcon(
                                                              inputId = "kö2",
                                                              label = "Kısmi Ödeme Tutarı-1",
                                                              value = 0, step = 0.1,
                                                              icon = list(icon("turkish-lira-sign"))
                                                            )
                                                          ),
                                                          hr(),
                                                          wellPanel(
                                                            p(tags$b("Kısmi Ödeme Tarihi-2")),
                                                            dateInput("kısmiodemetarihi3", label = NULL),
                                                            numericInputIcon(
                                                              inputId = "kö3",
                                                              label = "Kısmi Ödeme Tutarı-2",
                                                              value = 0, step = 0.1,
                                                              icon = list(icon("turkish-lira-sign"))
                                                            )
                                                          )
                                                          
                                                     ),
                                                     
                                                     circle =FALSE, status = "danger",
                                                     icon = icon("turkish-lira-sign"), width = "300px"
                                                     
                                      )
                                      
                                    ),
                                    
                                    conditionalPanel(
                                      condition = "input.kısmiodeme == '3'",
                                      
                                      
                                      dropdownButton(label = "Kısmi Ödeme Bilgileri",
                                                     
                                                     div( id = "3",
                                                          wellPanel(
                                                            p(tags$b("Kısmi Ödeme Tarihi-1")),
                                                            dateInput("kısmiodemetarihi4", label = NULL),
                                                            numericInputIcon(
                                                              inputId = "kö4",
                                                              label = "Kısmi Ödeme Tutarı-1",
                                                              value = 0,
                                                              icon = list(icon("turkish-lira-sign"), ".00")
                                                            )
                                                          ),
                                                          hr(),
                                                          wellPanel(
                                                            p(tags$b("Kısmi Ödeme Tarihi-2")),
                                                            dateInput("kısmiodemetarihi5", label = NULL),
                                                            numericInputIcon(
                                                              inputId = "kö5",
                                                              label = "Kısmi Ödeme Tutarı-2",
                                                              value = 0,
                                                              icon = list(icon("turkish-lira-sign"), ".00")
                                                            )
                                                          ),
                                                          hr(),
                                                          wellPanel(
                                                            p(tags$b("Kısmi Ödeme Tarihi-3")),
                                                            dateInput("kısmiodemetarihi6", label = NULL),
                                                            numericInputIcon(
                                                              inputId = "kö6",
                                                              label = "Kısmi Ödeme Tutarı-3",
                                                              value = 0,
                                                              icon = list(icon("turkish-lira-sign"), ".00")
                                                            )
                                                          )
                                                          
                                                     ),
                                                     
                                                     circle =FALSE, status = "danger",
                                                     icon = icon("turkish-lira-sign"), width = "300px"
                                                     
                                      )
                                      
                                    )
                                    
                                    
                                  ) %>% hidden()
                                  
                                )
                                
                                
                              ), # Şirket Bilgileri end
                              
                              # Aile Bilgileri ----
                              column(
                                width = 2,
                                
                                wellPanel(
                                  div(
                                    id = "aile_bilgiler",
                                    actionButton(inputId = "action_button3", label = "Aile Bilgileri", icon = icon("cog"))
                                  ),
                                  
                                  hr(),
                                  
                                  div(
                                    id="settings_toggle3",
                                    
                                    p(tags$b("Aile Bilgileri", style = "font-weight: bold; color: red;")),
                                    ## Eş ----
                                    div(
                                      p(tags$b("Eş")),
                                      awesomeRadio("es", label = NULL,
                                                   choices = c("Var", "Yok"),
                                                   selected = "Yok", inline=TRUE, checkbox = TRUE),
                                      conditionalPanel(
                                        condition = "input.es == 'Var'",
                                        
                                        textInput(inputId = "es_isim",label = "Eş Ad-Soyad", width = 250),
                                  
                                        dropdownButton(label = "Eş Doğum Tarihi",
                                                       
                                                       div( id = "1",
                                                            wellPanel(
                                                              p(tags$b("Doğum Tarihi")),
                                                              dateInput("esdogumtarihi", label = NULL)
                                                            )
                                                       ),
                                                       circle =FALSE, status = "danger",
                                                       icon = icon("fa-thin fa-calendar"), width = "300px"
                                        )
                                      )
                                    ), 
                                    ## Anne ----
                                    div(
                                      p(tags$b("Anne")),
                                      awesomeRadio("anne", label = NULL,
                                                   choices = c("Var", "Yok"),
                                                   selected = "Yok", inline=TRUE, checkbox = TRUE),
                                      conditionalPanel(
                                        condition = "input.anne == 'Var'",
                                        
                                        textInput(inputId = "anne_isim",label = "Anne Ad-Soyad", width = 250),
                                        
                                        dropdownButton(label = "Anne Doğum Tarihi",
                                                       
                                                       div( id = "1",
                                                            wellPanel(
                                                              p(tags$b("Doğum Tarihi")),
                                                              dateInput("annedogumtarihi", label = NULL)
                                                            )
                                                       ),
                                                       circle =FALSE, status = "danger",
                                                       icon = icon("fa-thin fa-calendar"), width = "300px"
                                        )
                                      )
                                    ), 
                                    ## Baba ----
                                    div(
                                      p(tags$b("Baba")),
                                      awesomeRadio("baba", label = NULL,
                                                   choices = c("Var", "Yok"),
                                                   selected = "Yok", inline=TRUE, checkbox = TRUE),
                                      conditionalPanel(
                                        condition = "input.baba == 'Var'",
                                        
                                        textInput(inputId = "baba_isim",label = "Baba Ad-Soyad", width = 250),
                                        
                                        
                                        dropdownButton(label = "Baba Doğum Tarihi",
                                                       
                                                       div( id = "1",
                                                            wellPanel(
                                                              p(tags$b("Doğum Tarihi")),
                                                              dateInput("babadogumtarihi", label = NULL)
                                                            )
                                                       ),
                                                       circle =FALSE, status = "danger",
                                                       icon = icon("fa-thin fa-calendar"), width = "300px"
                                        )
                                      )
                                    ), 
                                    
                                    ## Çocuk ----
                                    div(
                                      # p(tags$b("Çocuk Durumu")),
                                      # awesomeRadio(
                                      #   inputId = "cocuksay",
                                      #   label = "Çocuk Durumu",
                                      #   choices = c("Yok", "1", "2","3","4","5"),
                                      #   inline=FALSE, checkbox = TRUE),
                                      
                                      # conditionalPanel(
                                      #   condition = "input.cocuk == 'Var'",
                                      #   p(tags$b("Çocuk Sayısı")),
                                      #   numericInput("cocuksay", label = NULL, value = 1, min = 1, max = 5),
                                      
                                      
                                      p(tags$b("1. Çocuk")),
                                      awesomeRadio("cocuk1", label = NULL,
                                                   choices = c("Var", "Yok"),
                                                   selected = "Yok", inline=TRUE, checkbox = TRUE),
                                      conditionalPanel(
                                        condition = "input.cocuk1 == 'Var'",
                                        
                                        textInput(inputId = "cocuk1_isim",label = "1.Çocuk Ad-Soyad", width = 250),
                                        
                                        
                                        dropdownButton(label = "1. Çocuk Doğum Tarihi",
                                                       
                                                       div( id = "1",
                                                            wellPanel(
                                                              p(tags$b("Doğum Tarihi")),
                                                              dateInput("cocukdogumtarihi11", label = NULL)
                                                            )
                                                       ),
                                                       circle =FALSE, status = "danger",
                                                       icon = icon("fa-thin fa-calendar"), width = "300px"
                                        )
                                      ),
                                      
                                      
                                      p(tags$b("2. Çocuk")),
                                      awesomeRadio("cocuk2", label = NULL,
                                                   choices = c("Var", "Yok"),
                                                   selected = "Yok", inline=TRUE, checkbox = TRUE),
                                      conditionalPanel(
                                        condition = "input.cocuk2 == 'Var'",
                                        
                                        textInput(inputId = "cocuk2_isim",label = "2.Çocuk Ad-Soyad", width = 250),
                                        
                                        
                                        dropdownButton(label = "2. Çocuk Doğum Tarihi",
                                                       
                                                       div( id = "2",
                                                            wellPanel(
                                                              p(tags$b("Doğum Tarihi")),
                                                              dateInput("cocukdogumtarihi22", label = NULL)
                                                            )
                                                       ),
                                                       circle =FALSE, status = "danger",
                                                       icon = icon("fa-thin fa-calendar"), width = "300px"
                                        )
                                      ),
                                      
                                      p(tags$b("3. Çocuk")),
                                      awesomeRadio("cocuk3", label = NULL,
                                                   choices = c("Var", "Yok"),
                                                   selected = "Yok", inline=TRUE, checkbox = TRUE),
                                      conditionalPanel(
                                        condition = "input.cocuk3 == 'Var'",
                                        
                                        textInput(inputId = "cocuk3_isim",label = "3.Çocuk Ad-Soyad", width = 250),
                                        
                                        dropdownButton(label = "3. Çocuk Doğum Tarihi",
                                                       
                                                       div( id = "3",
                                                            wellPanel(
                                                              p(tags$b("Doğum Tarihi")),
                                                              dateInput("cocukdogumtarihi33", label = NULL)
                                                            )
                                                       ),
                                                       circle =FALSE, status = "danger",
                                                       icon = icon("fa-thin fa-calendar"), width = "300px"
                                        )
                                      ),
                                      
                                      
                                      p(tags$b("4. Çocuk")),
                                      awesomeRadio("cocuk4", label = NULL,
                                                   choices = c("Var", "Yok"),
                                                   selected = "Yok", inline=TRUE, checkbox = TRUE),
                                      conditionalPanel(
                                        condition = "input.cocuk4 == 'Var'",
                                        
                                        textInput(inputId = "cocuk4_isim",label = "4.Çocuk Ad-Soyad", width = 250),
                                        
                                        dropdownButton(label = "4. Çocuk Doğum Tarihi",
                                                       
                                                       div( id = "4",
                                                            wellPanel(
                                                              p(tags$b("Doğum Tarihi")),
                                                              dateInput("cocukdogumtarihi44", label = NULL)
                                                            )
                                                       ),
                                                       circle =FALSE, status = "danger",
                                                       icon = icon("fa-thin fa-calendar"), width = "300px"
                                        )
                                      ),
                                      
                                      
                                      p(tags$b("5. Çocuk")),
                                      awesomeRadio("cocuk5", label = NULL,
                                                   choices = c("Var", "Yok"),
                                                   selected = "Yok", inline=TRUE, checkbox = TRUE),
                                      conditionalPanel(
                                        condition = "input.cocuk5 == 'Var'",
                                        
                                        textInput(inputId = "cocuk5_isim",label = "5.Çocuk Ad-Soyad", width = 250),
                                        
                                        dropdownButton(label = "5. Çocuk Doğum Tarihi",
                                                       
                                                       div( id = "5",
                                                            wellPanel(
                                                              p(tags$b("Doğum Tarihi")),
                                                              dateInput("cocukdogumtarihi55", label = NULL)
                                                            )
                                                       ),
                                                       circle =FALSE, status = "danger",
                                                       icon = icon("fa-thin fa-calendar"), width = "300px"
                                        )
                                      ),
                                      
                                      
                                      
                                        
                                        # conditionalPanel(
                                        #   condition = "input.cocuksay == '1'",
                                        #   
                                        #   dropdownButton(label = "Çocuk Doğum Tarihi",
                                        #                  
                                        #                  div( id = "11",
                                        #                       wellPanel(
                                        #                         p(tags$b("Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi11", label = NULL)
                                        #                         
                                        #                       )
                                        #                  ),
                                        #                  circle =FALSE, status = "danger",
                                        #                  icon = icon("fa-thin fa-calendar"), width = "300px"
                                        #   )
                                        # ),
                                        # conditionalPanel(
                                        #   condition = "input.cocuksay == '2'",
                                        #   
                                        #   dropdownButton(label = "Çocukların Doğum Tarihleri",
                                        #                  
                                        #                  div( id = "22",
                                        #                       wellPanel(
                                        #                         p(tags$b("1. Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi11", label = NULL)
                                        #                         
                                        #                       ),
                                        #                       
                                        #                       wellPanel(
                                        #                         p(tags$b("2. Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi22", label = NULL)
                                        #                         
                                        #                       )
                                        #                       
                                        #                  ),
                                        #                  circle =FALSE, status = "danger",
                                        #                  icon = icon("fa-thin fa-calendar"), width = "300px"
                                        #   )
                                        # ),
                                        # conditionalPanel(
                                        #   condition = "input.cocuksay == '3'",
                                        #   
                                        #   dropdownButton(label = "Çocukların Doğum Tarihleri",
                                        #                  
                                        #                  div( id = "33",
                                        #                       wellPanel(
                                        #                         p(tags$b("1. Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi11", label = NULL)
                                        #                         
                                        #                       ),
                                        #                       
                                        #                       wellPanel(
                                        #                         p(tags$b("2. Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi22", label = NULL)
                                        #                         
                                        #                       ),
                                        #                       
                                        #                       wellPanel(
                                        #                         p(tags$b("3. Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi33", label = NULL)
                                        #                         
                                        #                       )
                                        #                       
                                        #                  ),
                                        #                  circle =FALSE, status = "danger",
                                        #                  icon = icon("fa-thin fa-calendar"), width = "300px"
                                        #   )
                                        # ),
                                        # conditionalPanel(
                                        #   condition = "input.cocuksay == '4'",
                                        #   
                                        #   dropdownButton(label = "Çocukların Doğum Tarihleri",
                                        #                  
                                        #                  div( id = "44",
                                        #                       wellPanel(
                                        #                         p(tags$b("1. Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi11", label = NULL)
                                        #                         
                                        #                       ),
                                        #                       
                                        #                       wellPanel(
                                        #                         p(tags$b("2. Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi22", label = NULL)
                                        #                         
                                        #                       ),
                                        #                       
                                        #                       wellPanel(
                                        #                         p(tags$b("3. Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi33", label = NULL)
                                        #                         
                                        #                       ),
                                        #                       
                                        #                       wellPanel(
                                        #                         p(tags$b("4. Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi44", label = NULL)
                                        #                         
                                        #                       )
                                        #                       
                                        #                  ),
                                        #                  circle =FALSE, status = "danger",
                                        #                  icon = icon("fa-thin fa-calendar"), width = "300px"
                                        #   )
                                        # ),
                                        # conditionalPanel(
                                        #   condition = "input.cocuksay == '5'",
                                        #   
                                        #   dropdownButton(label = "Çocukların Doğum Tarihleri",
                                        #                  
                                        #                  div( id = "55",
                                        #                       wellPanel(
                                        #                         p(tags$b("1. Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi11", label = NULL)
                                        #                         
                                        #                       ),
                                        #                       
                                        #                       wellPanel(
                                        #                         p(tags$b("2. Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi22", label = NULL)
                                        #                         
                                        #                       ),
                                        #                       
                                        #                       wellPanel(
                                        #                         p(tags$b("3. Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi33", label = NULL)
                                        #                         
                                        #                       ),
                                        #                       
                                        #                       wellPanel(
                                        #                         p(tags$b("4. Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi44", label = NULL)
                                        #                         
                                        #                       ),
                                        #                       
                                        #                       wellPanel(
                                        #                         p(tags$b("5. Çocuk Doğum Tarihi")),
                                        #                         dateInput("cocukdogumtarihi55", label = NULL)
                                        #                         
                                        #                       )
                                        #                       
                                        #                  ),
                                        #                  circle =FALSE, status = "danger",
                                        #                  icon = icon("fa-thin fa-calendar"), width = "300px"
                                        #   )
                                        # )
                                      )
                                    # )
                                    
                                    
                                  ) %>% hidden()
                                  
                                )
                                
                                
                              ), # Aile Bilgiler end
                              
                              column(
                                width = 2,
                                
                                wellPanel(
                                  div(
                                    id = "gelir_bilgisi",
                                    actionButton(inputId = "action_button4", label = "Gelir Bilgisi", icon = icon("cog"))
                                  ),
                                  
                                  hr(),
                                  
                                  div(
                                    id="settings_toggle4",
                                    p(tags$b("Gelir Durumu", style = "font-weight: bold; color: red;")),
                                    awesomeRadio("gelir", label = NULL,
                                                 choices = c("Asgari ücret", "Diğer"),
                                                 inline=TRUE, checkbox = TRUE),
                                    
                                    
                                    conditionalPanel(
                                      condition = "input.gelir == 'Diğer'",
                                      
                                      modFunctionUI("editable"),
                                      h6("Ortalama Gelir", style = "font-weight: bold; color: black;"),
                                      verbatimTextOutput("ort_gelir")
                                      
                                    ),
                                    
                                    
                                    conditionalPanel(
                                      condition = "input.gelir == 'Asgari ücret'",
                                      
                                      awesomeRadio(
                                        inputId = "asgari_durum",
                                        label = "Durumu",
                                        choices = c("Bekar", "evli_cocuksuz", "1cocuk","2cocuk","3cocuk","4cocuk"),
                                        inline=FALSE, checkbox = TRUE)
                                      
                                      
                                    )
                                    
                                    
                                    
                                    
                                    
                                  ) %>% hidden()
                                  
                                )
                                
                                
                              ), # Gelir bilgisi end
                              
                              column(
                                width = 2,
                                
                                wellPanel(
                                  div(
                                    id = "bakici_bilgiler",
                                    actionButton(inputId = "action_button6", label = "Bakıcı Bilgisi", icon = icon("cog"))
                                  ),
                                  
                                  hr(),
                                  
                                  
                                  div(
                                    id="settings_toggle6",
                                    
                                    p(tags$b("Bakıcı Gideri", style = "font-weight: bold; color: red;")),
                                    awesomeRadio("bakici_gider", label = NULL,
                                                 choices = c("Var", "Yok"),
                                                 selected = "Yok", inline=TRUE, checkbox = TRUE),
                                    
                                    p(tags$b("Bakıcı Tutuldu mu?", style = "font-weight: bold; color: red;")),
                                    awesomeRadio("bakici_tut", label = NULL,
                                                 choices = c("Evet", "Hayır"),
                                                 selected = "Hayır", inline=TRUE, checkbox = TRUE),
                                    p(tags$b("Bakıcı Gideri Süresi (Gün)")),
                                    conditionalPanel(
                                      condition = "input.bakici_gider == 'Var'",
                                      
                                      numericInputIcon(
                                        inputId = "bakici_sure",
                                        label = NULL,
                                        value = 0, step = 0.1,
                                        min = 0, max = 365,
                                        icon = icon("calendar")
                                      )
                                      
                                    )
                                    
                                  ) %>% hidden()
                                  
                                )
                                
                                
                              ), # Bakıcı end
                              
                              
                              
                            ) # fluid row end
                          )  %>% hidden() # fluid row div end 
                          
                        ),
                        
                        
                        div(
                          id = "sonuclar",
                          
                          column(
                            width = 3,
                            wellPanel(
                              p(tags$b("Hesaplama Bilgileri", style = "font-weight: bold; color: red;")),
                              textOutput("yontem"),
                              conditionalPanel(
                                condition = "input.yontem == 'Progresif Rant'",
                                textOutput("tablo1")
                              ),
                              conditionalPanel(
                                condition = "input.yontem == 'Aktüeryal'",
                                textOutput("tablo2")
                              ),
                              
                              conditionalPanel(
                                condition = "input.yontem == 'Progresif Rant'",
                                textOutput("standartfaiz")
                              ),
                              
                              conditionalPanel(
                                condition = "input.yontem == 'Aktüeryal'",
                                textOutput("teknikfaiz")
                              ),
                              
                              
                              br(),
                              p(tags$b("Genel Bilgiler", style = "font-weight: bold; color: red;")),
                              textOutput("dosya_no"),
                              textOutput("isim"),
                              textOutput("cinsiyet"),
                              textOutput("dogumtarihi"),
                              textOutput("kazatarihi"),
                              textOutput("maluliyetoranı"),
                              textOutput("kusuroranı"),
                              textOutput("gecicimaluliyet"),
                              textOutput("bakicisure"),

                              style = "background: #fff"
                            )
                            
                          ),
                          
                          column(
                            width = 3,
                            dataTableOutput('table')
                          ),
                          
                          column(
                            width = 3,
                            dataTableOutput('table3'),
                            downloadButton("dl", "Download Table")
                          ),
                          column(
                            width = 2,
                            dataTableOutput('table4'),
                            downloadButton("dl2", "Download Table")
                          )
                          
                        ) %>% hidden(), # textoutput div end
                        
                        div(
                          column(
                            width = 2,
                            radioGroupButtons(
                              inputId = "rapor",
                              label = "Rapor Seç",
                              choices = c("Tüm Rapor", "Tüm Rapor (Şirket Ödemesiz)", "Sürekli+Geçici (Şirket Ödemesiz)", 
                                          "Geçici (Şirket Ödemesiz)", "Sürekli (Şirket Ödemesiz)", "Destek"),
                              justified = TRUE,
                              direction = "vertical",
                              status = "danger",
                              checkIcon = list(
                                yes = icon("ok", 
                                           lib = "glyphicon"))
                            ),
                            
                         
                            downloadButton("download_button", label = "Generate report")
                

                          )
                        )

               ), # Maluliyet tabpanel end
               
               tabPanel(title = "Destekten Yoksun Kalma"
                        
   
                        
               ) # Destek tabpanel end
               
               
    )  # Tabpanel - Hesaplamalar end
    
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  


# Params List ----
  
  output$download_button <- downloadHandler(
        filename = "rendered_report.docx",
        content = function(file) {
          
          if(input$rapor == "Tüm Rapor") {
            res <- rmarkdown::render(
              "tum_report.Rmd",
              params = list(
                # draw_plot = draw_plot,
                PDosya_No = input$dosya,
                PAd_Soyad = input$isim,
                PCinsiyet = input$cinsiyet,
                PMaluliyet_Oranı = input$maluliyet,
                PKusur_Oranı = input$kusur,
                PKaza_Tarihi = input$kazatarihi,
                PDogum_Tarihi = input$dogumtarihi,
                PKısmi_Odeme_Sayısı = input$kısmiodeme,
                PKısmi_Odeme_Tarihi_1 = input$kısmiodemetarihi2,
                PKısmi_Odeme_Tutarı_1 = input$kö2,
                # PKısmi_Odeme_Tarihi_2 = input$kısmiodemetarihi2,
                # PKısmi_Odeme_Tutarı_2 = input$kö2,
                PKısmi_Odeme_Tarihi_2 = input$kısmiodemetarihi3,
                PKısmi_Odeme_Tutarı_2 = input$kö3,
                PGeçici_Maluliyet_Sure = input$maluliyet_sure,
                PBakici = input$bakici_gider,
                PBakici_Sure = input$bakici_sure,
                PGelir = input$asgari_durum,
                PYasam_Tablosu = input$tablo2
                
              )
            )
            file.rename(res, file)
            
          } else if (input$rapor == "Tüm Rapor (Şirket Ödemesiz)") {
            
            res <- rmarkdown::render(
              "tum_report_sirket_odemesiz1.Rmd",
              params = list(
                # draw_plot = draw_plot,
                PDosya_No = input$dosya,
                PAd_Soyad = input$isim,
                PCinsiyet = input$cinsiyet,
                PMaluliyet_Oranı = input$maluliyet,
                PBakici_Sure = input$bakici_sure,
                PKusur_Oranı = input$kusur,
                PKaza_Tarihi = input$kazatarihi,
                PDogum_Tarihi = input$dogumtarihi,
                PGeçici_Maluliyet_Sure = input$maluliyet_sure,
                PGelir = input$asgari_durum,
                PYasam_Tablosu = input$tablo2
              )
            )
            file.rename(res, file)
            
           } else if (input$rapor == "Sürekli+Geçici (Şirket Ödemesiz)") {
              
              res <- rmarkdown::render(
                "surekli_gecici_sirket_odemesiz.Rmd",
                params = list(
                  # draw_plot = draw_plot,
                  PDosya_No = input$dosya,
                  PAd_Soyad = input$isim,
                  PCinsiyet = input$cinsiyet,
                  PMaluliyet_Oranı = input$maluliyet,
                  PBakici_Sure = input$bakici_sure,
                  PKusur_Oranı = input$kusur,
                  PKaza_Tarihi = input$kazatarihi,
                  PDogum_Tarihi = input$dogumtarihi,
                  PGeçici_Maluliyet_Sure = input$maluliyet_sure,
                  PGelir = input$asgari_durum,
                  PYasam_Tablosu = input$tablo2
                )
              )
              file.rename(res, file)
            
          } else if (input$rapor == "Geçici (Şirket Ödemesiz)") {
            
            res <- rmarkdown::render(
              "gecici_sirket_odemesiz.Rmd",
              params = list(
                PDosya_No = input$dosya,
                PAd_Soyad = input$isim,
                PCinsiyet = input$cinsiyet,
                PMaluliyet_Oranı = input$maluliyet,
                PBakici_Sure = input$bakici_sure,
                PKusur_Oranı = input$kusur,
                PKaza_Tarihi = input$kazatarihi,
                PDogum_Tarihi = input$dogumtarihi,
                PGeçici_Maluliyet_Sure = input$maluliyet_sure,
                PGelir = input$asgari_durum,
                PYasam_Tablosu = input$tablo2
              )
            )
            file.rename(res, file)
            
          } else if (input$rapor == "Sürekli (Şirket Ödemesiz)") {
            
            res <- rmarkdown::render(
              "surekli_sirket_odemesiz.Rmd",
              params = list(
                # draw_plot = draw_plot,
                PDosya_No = input$dosya,
                PAd_Soyad = input$isim,
                PCinsiyet = input$cinsiyet,
                PMaluliyet_Oranı = input$maluliyet,
                PBakici_Sure = input$bakici_sure,
                PKusur_Oranı = input$kusur,
                PKaza_Tarihi = input$kazatarihi,
                PDogum_Tarihi = input$dogumtarihi,
                PGeçici_Maluliyet_Sure = input$maluliyet_sure,
                PGelir = input$asgari_durum,
                PYasam_Tablosu = input$tablo2
              )
            )
            file.rename(res, file)
            
          } else {
            
            res <- rmarkdown::render(
              "Report_Template_Vefat_Shiny.Rmd",
              params = list(
                # draw_plot = draw_plot,
                PDosya_No = input$dosya,
                PAd_Soyad = input$isim,
                PCinsiyet = input$cinsiyet,
                PKusur_Oranı = input$kusur,
                PKaza_Tarihi = input$kazatarihi,
                PDogum_Tarihi = input$dogumtarihi,
                PKısmi_Odeme_Sayısı = input$kısmiodeme,
                PKısmi_Odeme_Tarihi_1 = input$kısmiodemetarihi1,
                PKısmi_Odeme_Tutarı_1 = input$kö1,
                PGelir = input$asgari_durum,
                PYasam_Tablosu = input$tablo2,
                PEs = input$es,
                PEsAd = input$es_isim,
                PEsDT = input$esdogumtarihi,
                PAnne = input$anne,
                PAnneAd = input$anne_isim,
                PAnneDT = input$annedogumtarihi,
                PBaba = input$baba,
                PBabaAd = input$baba_isim,
                PBabaDT = input$babadogumtarihi,
                PCocuksay = input$cocuksay,
                PCocuk1_DT = input$cocukdogumtarihi11,
                PCocuk1_Ad = input$cocuk1_isim,
                PCocuk2_DT = input$cocukdogumtarihi22,
                PCocuk2_Ad = input$cocuk2_isim,
                PCocuk3_DT = input$cocukdogumtarihi33,
                PCocuk3_Ad = input$cocuk3_isim,
                PCocuk4_DT = input$cocukdogumtarihi44,
                PCocuk4_Ad = input$cocuk4_isim,
                PCocuk5_DT = input$cocukdogumtarihi55,
                PCocuk5_Ad = input$cocuk5_isim
              )
            )
            file.rename(res, file)
          }
          
          

        }
      )
      

  
  ###########################################
  
  
  ####
  
  # output$ratio = renderInfoBox({
  #   infoBox("ratio",10*2, icon = icon("users"))
  # })
  
  ####
  
  # Data entry
  observeEvent(input$info_button1, {
    toggle(id = "info1", anim = TRUE)
  })
  
  ####
  
  output$table <- DT::renderDataTable({
    DT::datatable(son_hesaplama_tablosu, options = list(paging = F, searching = F))
  })
  
  
  # Reactive expression to create data frame of all input values ----
  
  sliderValues <- reactive({
    
    köt1 <- ifelse(as.character(input$kısmiodemetarihi1) == Sys.Date(),"-",as.character(input$kısmiodemetarihi1))
    köt2 <- ifelse(as.character(input$kısmiodemetarihi2) == Sys.Date(),"-",as.character(input$kısmiodemetarihi2))
    köt3 <- ifelse(as.character(input$kısmiodemetarihi3) == Sys.Date(),"-",as.character(input$kısmiodemetarihi3))
    köt4 <- ifelse(as.character(input$kısmiodemetarihi4) == Sys.Date(),"-",as.character(input$kısmiodemetarihi4))
    köt5 <- ifelse(as.character(input$kısmiodemetarihi5) == Sys.Date(),"-",as.character(input$kısmiodemetarihi5))
    köt6 <- ifelse(as.character(input$kısmiodemetarihi6) == Sys.Date(),"-",as.character(input$kısmiodemetarihi6))
    
    excel_data <- if(input$kısmiodeme == '1' & input$gelir == "Asgari ücret") {
      
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Asgari Ucret Durumu",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Bakıcı Süresi (ay)",
                 "Kısmi Ödeme Sayısı",
                 "Kısmi Ödeme Tarihi-1",
                 "Kısmi Ödeme Tutarı-1"
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  input$asgari_durum,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$bakici_sure,
                  input$kusur,
                  input$maluliyet_sure,
                  input$kısmiodeme,
                  köt1,
                  input$kö1
        )
      )
    } 
    
    else if(input$kısmiodeme == '2' & input$gelir == "Asgari ücret") {

      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Asgari Ucret Durumu",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Bakıcı Süresi (ay)",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Kısmi Ödeme Sayısı",
                 "Kısmi Ödeme Tarihi-1",
                 "Kısmi Ödeme Tutarı-1",
                 "Kısmi Ödeme Tarihi-2",
                 "Kısmi Ödeme Tutarı-2"
        ),

        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  input$asgari_durum,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$kusur,
                  input$maluliyet_sure,
                  input$bakici_sure,
                  input$kısmiodeme,
                  köt2,
                  input$kö2,
                  köt3,
                  input$kö3
        )
      )
    }
    
    else if(input$kısmiodeme == '3' & input$gelir == "Asgari ücret") {
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Asgari Ucret Durumu",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Bakıcı Süresi (ay)",
                 "Kısmi Ödeme Sayısı",
                 "Kısmi Ödeme Tarihi-1",
                 "Kısmi Ödeme Tutarı-1",
                 "Kısmi Ödeme Tarihi-2",
                 "Kısmi Ödeme Tutarı-2",
                 "Kısmi Ödeme Tarihi-3",
                 "Kısmi Ödeme Tutarı-3"
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  input$asgari_durum,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$kusur,
                  input$maluliyet_sure,
                  input$bakici_sure,
                  input$kısmiodeme,
                  köt4,
                  input$kö4,
                  köt5,
                  input$kö5,
                  köt6,
                  input$kö6
        )
      )
    }
    
    else if(input$kısmiodeme == '1' & input$gelir == "Diğer") {
      
      
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Bakıcı Süresi (ay)",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Kısmi Ödeme Sayısı",
                 "Kısmi Ödeme Tarihi-1",
                 "Kısmi Ödeme Tutarı-1"
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$bakici_sure,
                  input$kusur,
                  input$maluliyet_sure,
                  input$kısmiodeme,
                  köt1,
                  input$kö1
        )
      )
    }
    
    
    
    else if(input$kısmiodeme == '2' & input$gelir == "Diğer") {
      
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Bakıcı Süresi (ay)",
                 "Kısmi Ödeme Sayısı",
                 "Kısmi Ödeme Tarihi-1",
                 "Kısmi Ödeme Tutarı-1",
                 "Kısmi Ödeme Tarihi-2",
                 "Kısmi Ödeme Tutarı-2"
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$bakici_sure,
                  input$kusur,
                  input$maluliyet_sure,
                  input$kısmiodeme,
                  köt2,
                  input$kö2,
                  köt3,
                  input$kö3
        )
      )
    } 
    
    else if(input$kısmiodeme == '3' & input$gelir == "Diğer") {
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Bakıcı Süresi (ay)",
                 "Kısmi Ödeme Sayısı",
                 "Kısmi Ödeme Tarihi-1",
                 "Kısmi Ödeme Tutarı-1",
                 "Kısmi Ödeme Tarihi-2",
                 "Kısmi Ödeme Tutarı-2",
                 "Kısmi Ödeme Tarihi-3",
                 "Kısmi Ödeme Tutarı-3"
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$bakici_sure,
                  input$kusur,
                  input$maluliyet_sure,
                  input$kısmiodeme,
                  köt4,
                  input$kö4,
                  köt5,
                  input$kö5,
                  köt6,
                  input$kö6
        )
      )
    }
    
    
    else if(input$kısmiodeme == '0' & input$gelir == "Diğer") {
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Bakıcı Süresi (ay)",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Kısmi Ödeme Sayısı"
                 
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$bakici_sure,
                  input$kusur,
                  input$maluliyet_sure,
                  input$kısmiodeme
                  
        )
      )
    }
    
    
    else {
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Asgari Ucret Durumu",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Bakıcı Süresi (ay)",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Kısmi Ödeme Sayısı"
                 
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  input$asgari_durum,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$bakici_sure,
                  input$kusur,
                  input$maluliyet_sure,
                  input$kısmiodeme
                  
        )
      )
    }
    
    
  })
  
  # Show the values in an HTML table ----
  
  output$table3 <- DT::renderDataTable({
    sliderValues()
  })
  
  output$dl <- downloadHandler(
    filename = function() {"genel_bilgiler_tablosu.xlsx"},
    content = function(file) {write_xlsx(sliderValues(), path = file)}
  )
  
  #### Edited Data Table ----
  
  
  
  # Asgari_Tablo <- read_excel("../app_bilirkisi/data/Asgari_Ucret_Tablosu.xlsx", sheet = "Program")
  # 
  # manuel_gelir_tablosu <-  reactive({
  #   
  #   x <- Asgari_Tablo %>% 
  #     separate(Donem, sep = "-", into = c("Donem_Baslangic", "Donem_Son")) %>%
  #     mutate(D_B = as.Date(dmy(Donem_Baslangic), "%d/%m/%Y"),
  #            D_S = as.Date(dmy(Donem_Son), "%d/%m/%Y")) %>% 
  #     filter(input$kazatarihi >= D_B)
  #   
  #   x
  # })
  # 
  # input_data <- data.frame(Brand = manuel_gelir_tablosu()$D_B,
  #                          Brand2 = manuel_gelir_tablosu()$D_S,
  #                          Gelir = 0,
  #                          stringsAsFactors = FALSE)
  # 
  
  
  # 
  demodata<-input_data
  edited <- callModule(modFunction,"editable", demodata,
                       reset = reactive(input$reset))
  observe(print(edited$data))
  
  output$ort_gelir<- renderText(paste0(round(mean(edited$data$Gelir),digits = 1), " ", "TL"))
  
  output$teknikfaiztext<- renderText("Teknik Faiz % 0 'dır.")
  
  
  # Gelir Tablosu
  
  
  kisi_gelir_tablosu <- reactive({
    
    Asgari_Tablo <- read_excel("../app_bilirkisi/data/Asgari_Ucret_Tablosu.xlsx", sheet = "Program")
    
    excel_data <- if(input$gelir == 'Diğer') {
      edited$data
    } 
    else {
      Asgari_Tablo
    } 
    
  })
  
  output$table4 <- DT::renderDataTable({
    kisi_gelir_tablosu()
  })
  
  # Asgari_Tablo <- read_excel("../data/Asgari Ucret Tablosu.xlsx", sheet = "Program")
  
  file2 <- ("../data/")
  
  output$dl2 <- downloadHandler(
    filename = function() {"gelir_tablosu.xlsx"},
    content = function(file2) {write_xlsx(kisi_gelir_tablosu(), path = file2)}
  )
  
  
  
  # Data entry
  observeEvent(input$action_button_verigirisi, {
    toggle(id = "dataentry", anim = TRUE)
  })
  
  # Hesaplama Yöntemi
  observeEvent(input$action_button5, {
    toggle(id = "settings_toggle5", anim = TRUE)
  })
  
  
  # Genel Bilgiler
  observeEvent(input$action_button1, {
    toggle(id = "settings_toggle1", anim = TRUE)
  })
  
  # Şirket Ödeme Bilgileri
  observeEvent(input$action_button2, {
    toggle(id = "settings_toggle2", anim = TRUE)
  })
  
  # Aile Bilgileri
  observeEvent(input$action_button3, {
    toggle(id = "settings_toggle3", anim = TRUE)
  })
  # Gelir Bilgisi
  observeEvent(input$action_button4, {
    toggle(id = "settings_toggle4", anim = TRUE)
  })
  
  # Bakıcı Bilgisi
  observeEvent(input$action_button6, {
    toggle(id = "settings_toggle6", anim = TRUE)
  })
  
  
  output$yontem <- renderText({
    paste("Hesaplama Yöntemi :",  as.character(input$yontem))
  })
  output$tablo1 <- renderText({
    paste("Hesaplama Tablosu :",  as.character(input$tablo1))
  })
  output$tablo2 <- renderText({
    paste("Hesaplama Tablosu :",  as.character(input$tablo2))
  })
  output$dosya_no <- renderText({
    paste("Dosya No :",  as.character(input$dosya))
  })
  output$isim <- renderText({
    paste("Ad-Soyad :",  as.character(input$isim))
  })
  output$cinsiyet <- renderText({
    paste("Cinsiyet :",  as.character(input$cinsiyet))
  })
  output$dogumtarihi <- renderText({
    paste("Doğum Tarihi :", as.character(input$dogumtarihi))
  })
  output$kazatarihi <- renderText({
    paste("Kaza Tarihi :", as.character(input$kazatarihi))
  })
  output$maluliyetoranı <- renderText({
    paste("Maluliyet Oranı :", as.character(input$maluliyet))
  })
  output$kusuroranı <- renderText({
    paste("Kusur Oranı :", as.character(input$kusur))
  })
  output$teknikfaiz <- renderText({
    paste("Teknik Faiz :", "%", as.character(input$teknik_faiz))
  })
  output$standartfaiz <- renderText({
    paste("Teknik Faiz :", "% 0")
  })
  output$gecicimaluliyet <- renderText({
    paste("Geçici Maluliyet :", as.character(input$maluliyet_sure), "ay" )
  })
  output$bakicisure <- renderText({
    paste("Bakıcı Süresi :", as.character(input$bakici_sure), "ay" )
  })
  
  
  
  # Sonuçlar
  observeEvent(input$action_button_sonuclar, {
    toggle(id = "sonuclar", anim = TRUE)
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
