#install.packages(c("shiny","shinydashboard","shinyjs","plotly"))

#Carregando Pacote para fazer aplicativos shiny
library(shiny)
library(shinydashboard)
library(plotly)
library(lubridate)
library(shinydashboardPlus)
library(rgdal)
library(sf)
library(leaflet)


#Preparando algumas listas necessarias
seqano <- as.list(2002:2018) ; names(seqano) <- 2002:2018
Anoatual <- 2017
pib_municipios <- read.csv2("pib_municipios.csv", dec = ",")
municipiolist <- pib_municipios$MUNICIPIO
setorlist <- as.list(c("categoria_percentual","categoria_percentual_agro", "categoria_percentual_ind", "categoria_percentual_serv"))

#ui
dashboardPagePlus(skin = "blue",
                  header = dashboardHeaderPlus(
                    title = tagList(
                      span(class = "logo-lg", "PIB"), 
                         img(src = "SEI_transparente.png",
                          width = "30px", height = "13px")
                    )
                  ),
                  sidebar = dashboardSidebar(collapsed = TRUE,
                    
                    sidebarMenu(
                      menuItem("Anual", tabName = "aba1", icon = icon("chart-bar")),
                      #menuItem("Trimestral", tabName = "aba2", icon = icon("chart-line")),
                      menuItem("Municipal", tabName = "aba3", icon = icon("map-marked-alt")),           
                      #menuItem("Temático", tabName = "aba4", icon = icon("chart-pie"),
                              #menuSubItem("Agronegócio", tabName = "aba6", icon = icon("leaf")),
                              #menuSubItem("Agricultura Familiar", tabName = "aba7", icon = icon("seedling")),
                              #menuSubItem("Turismo", tabName = "aba8", icon = icon("luggage-cart")),
                              #menuSubItem("Cultura", tabName = "aba9", icon = icon("theater-masks")),
                              #menuSubItem("Saúde", tabName = "aba10", icon = icon("ambulance")),
                      menuItem("Desenvolvedores", tabName = "aba5", icon = icon("code"))
                    )
                    
                  ),   
                  dashboardBody(
                    
                    ###################################################
                    #declarando o CSS
                    ##################################################
                    
                    #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                    
                    ###################################################
                    #iniciando as paginas
                    ##################################################
                    
                    
                    tabItems(
                      
                      #################################################################################
                      #Pagina Anual
                      #################################################################################
                      
                      tabItem(tabName = "aba1", h3("PIB Anual"),
                              selectInput(inputId="selectano", label=h4("Selecione o Ano"), choices = seqano, selected = 2017),
                              fluidRow(valueBoxOutput("PIBtotalBA", width=3), 
                                       valueBoxOutput("particip_Bahia_Brasil", width=3),
                                       valueBoxOutput("particip_Bahia_NE", width = 3),
                                       valueBoxOutput("tx_cresc", width = 3)), br(),
                              fluidRow(
                                column(width=6,plotOutput("radar_pib")),
                                column(width=6,plotlyOutput("tx_setores"))
                              ), br(),
                              fluidRow(
                                column(width=6,plotOutput("tx_bahia")),
                                column(width=6,plotOutput("serie_ba_br_ne"))
                                      )
                      ),
                      
                      #################################################################################
                      #Pagina Trimestral
                      #################################################################################
                      tabItem(tabName = "aba2", 
                              sliderInput(inputId="sliderano", label=h4("Selecione o ano"), min=2010, max=2060, value=Anoatual, step = NULL, round = TRUE,
                                          ticks = TRUE, animate = TRUE,
                                          width = 1080, sep = ".", pre = NULL, post = NULL, timeFormat = NULL,
                                          timezone = NULL, dragRange = TRUE),
                              fluidRow(valueBoxOutput("poptotalproj", width=3), 
                                       valueBoxOutput("tftproj", width=3),
                                       valueBoxOutput("EVN", width=3), 
                                       valueBoxOutput("MortInf", width=3)
                              ),
                              fluidRow(
                                column(width=6,
                                       fluidRow(plotOutput("piramide"))
                                ),
                                column(width=6,plotOutput("EVNporSexo"))
                              ), br(),
                              fluidRow(plotOutput("GrafPopTotal")),br(),
                              fluidRow(plotOutput("TaxasBrutas"))
                      ),
                      
                      #################################################################################
                      #Pagina Municipal
                      #################################################################################
                      tabItem(tabName = "aba3", 
                              sliderInput(inputId="sliderano2", label=h4("Selecione o ano"), min=2002, max=2017, 
                                          value=2002, step = NULL, round = TRUE, ticks = TRUE, 
                                          animate = animationOptions(interval=2500), width = 1080, sep = ".", pre = NULL, 
                                          post = NULL, timeFormat = NULL,timezone = NULL, dragRange = TRUE),
                              fluidRow(column(width = 2,selectInput(inputId="selectmunicipio", label=h4("Selecione o Município"), choices = municipiolist, selected = 2000)),
                                       column(width = 4,selectInput(inputId="selectsetor", label = h4("Selecione o Setor"), choices = setorlist))),
                              fluidRow(valueBoxOutput("PIBtotalMunicipio", width=3),
                                       valueBoxOutput("PIBpercapita", width=3),
                                       valueBoxOutput("PIBparticip", width=3),
                                       valueBoxOutput("IDEM", width = 3)),
                              fluidRow(
                                column(width=6, leafletOutput("mapa_pib"), br()),
                                column(width=6, plotOutput("municip_pizza"))
                                )
                      ),
                              
                      
                      #################################################################################
                      #Pagina resultados do registro civil
                      #################################################################################
                      
                      tabItem(tabName = "aba6", "Aqui estara os resultados do RC"),
                      
                      #################################################################################
                      #Desenvolvedores
                      #################################################################################
                      
                      tabItem(tabName = "aba5", h3("Desenvolvedores"), br(),
                              fluidRow(column(width=6,
                                              widgetUserBox(
                                                title = "Rodrigo Barbosa de Cerqueira",
                                                subtitle = "Especialista em Produção de Informações Econômicas",
                                                type = NULL,
                                                width = 12,
                                                src = "jonatas.jpg",
                                                color = "aqua-active",
                                                closable = FALSE,
                                                "",
                                                footer = "Mestre em Economia pela UFBA. Técnico da Coordenação de Contas
                                                Regionais e Finanças Públicas (COREF) da SEI."
                                              )),
                                       column(width=6,
                                              widgetUserBox(
                                                title = "Bruno Brasil",
                                                subtitle = "Estagiário",
                                                type = NULL,
                                                width = 12,
                                                src = "cleiton.jpg",
                                                color = "aqua-active",
                                                closable = FALSE,
                                                " ",
                                                footer = "Estudante de Economia da UNIFACS. Estagiário da Coordenação de Contas
                                                Regionais e Finanças Públicas (COREF) da SEI."
                                              )))
                      )


                    )
                    
                  )
)
