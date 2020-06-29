################################################################################################### 
# SEIDataLab - Laboratorio de Dados da Superintendencia de Estudos Economicos e Sociais da Bahia
################################################################################################### 
#####   DESCRIÇÃO:        dashboard PIB do InfoVis Bahia
#####   ESCRITO POR:      Rodrigo Cerqueira
#####   SITE:             https://infovis.sei.ba.gov.br/
#####   LICENÇA:          MIT
#####   PROJETO:          https://github.com/RodrigoCerqueira/infovis_pib

# Pacotes ----------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(plotly)
library(lubridate)
library(shinydashboardPlus)
library(rgdal)
library(sf)
library(leaflet)
library(shinycssloaders)
<<<<<<< HEAD
=======
library(markdown)
>>>>>>> 86bb41ee8ac2a4d9f3b00e575739ffba072d6fed


# Listas -----------------------------------------------------------------------

seqano <- as.list(2002:2018) ; names(seqano) <- 2002:2018
Anoatual <- 2017
pib_municipios <- read.csv2("pib_municipios.csv", dec = ",")
municipiolist <- pib_municipios$MUNICIPIO
setorlist <- as.list(c("PIB","Agropecuária", "Indústria", "Serviços"))
<<<<<<< HEAD

# ui.R --------------------------------------------------------------------------

=======
box_height = "20em"

# ui.R --------------------------------------------------------------------------

>>>>>>> 86bb41ee8ac2a4d9f3b00e575739ffba072d6fed
dashboardPagePlus(skin = "blue", title = "SEI - PIB",
                  header = dashboardHeaderPlus(
                    title = tagList(
                      span(class = "logo-lg", img(src = "LogoGovBaTransp.png", width = "147.46px", height = "40px")), 
                         img(src = "SEI_transparente.png",
                          width = "30px", height = "30px")
                    )
                  ),
                  sidebar = dashboardSidebar(collapsed = TRUE,
                    
                    sidebarMenu(
                      menuItem("Apresentação", tabName = "aba11", icon = icon("file-alt")),
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
                    
                    tags$head(
                      tags$link(rel="sortcut icon", href="LogoGovBa.png", type="image/png"),
                      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")#, includeHTML("google-analytics.html")
                    ),
                    
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
                                column(width=6,withSpinner(plotOutput("radar_pib"))),
                                column(width=6,withSpinner(plotlyOutput("tx_setores")))
                              ), br(),
                              fluidRow(
                                column(width=6,withSpinner(plotOutput("tx_bahia"))),
                                column(width=6,withSpinner(plotOutput("serie_ba_br_ne")))
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
                                          animate = animationOptions(interval=2500), width = 1080, sep = "", pre = NULL, 
                                          post = NULL, timeFormat = NULL,timezone = NULL, dragRange = TRUE),
                              fluidRow(column(width = 2,selectInput(inputId="selectmunicipio", label=h4("Selecione o Município"), choices = municipiolist, selected = 2000)),
                                       column(width = 4,selectInput(inputId="selectsetor", label = h4("Selecione o Setor"), choices = setorlist))),
                              fluidRow(valueBoxOutput("PIBtotalMunicipio", width=3),
                                       valueBoxOutput("PIBpercapita", width=3),
                                       valueBoxOutput("PIBparticip", width=3),
                                       valueBoxOutput("IDEM", width = 3)),
                              fluidRow(
                                column(width=6, withSpinner(leafletOutput("mapa_pib")), br()),
                                column(width=6, withSpinner(plotOutput("municip_pizza")))
                                )
                      ),
                              
                      
                      #################################################################################
                      #Páginas PIB temático
                      #################################################################################
                      
                      tabItem(tabName = "aba6", "Aqui estarão os resultados do PIB do Agronegócio"),
<<<<<<< HEAD
=======
                      
                      #################################################################################
                      #Apresentação
                      #################################################################################
                      
                      tabItem(tabName = "aba11", 
                              titlePanel("Apresentação"),
                              fluidRow(column(width=6,
                                              box(width = NULL, 
                                                  status = "primary",
                                              fluidRow(includeMarkdown("PIBAnual.md")),
                                              footer = p("Para mais informações acesse", 
                                                         a(href="http://www.sei.ba.gov.br/index.php?option=com_content&view=article&id=2256&Itemid=328", "aqui"))
                                              )
                                              ),
                                          column(width=6,
                                                 box(width = NULL,
                                                  status = "primary",
                                                  fluidRow(includeMarkdown("PIBMunicipios.md")),
                                                  footer = p("Para mais informações acesse", 
                                                             a(href="http://www.sei.ba.gov.br/index.php?option=com_content&view=article&id=2255&Itemid=311", "aqui"))
                                                  )
                                              )
                                       )
                              ),
>>>>>>> 86bb41ee8ac2a4d9f3b00e575739ffba072d6fed
                      
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
                                              ))))
                      ),
                    
                    #################################################################################
                    #Rodapé - logos
                    #################################################################################
                    
                      hr(),
                      fluidRow(
                        column(width=5,
                               align="center",
                               a(href="http://seplan.ba.gov.br", target="_blank",
                                 img(class="align-middle", src = "Seplancol.png",width = "351px", height = "100px")
                               )
                        ),
                        column(width=4,
                               align="center",
                               a(href="http://sei.ba.gov.br", target="_blank",
                                 img(class="align-middle", src = "sei.png",width = "201.33px", height = "100px")
                               )
                        ),
                        column(width=3,
                               align="center",
                               a(href="http://sei.ba.gov.br", target="_blank",
                                 img(class="align-middle", src = "SeiDataLab.png",width = "225.35px", height = "100px")
                               )
                        )
                      )

                    ################## Fim do rodapé
                    
                    )
                    
                  )

