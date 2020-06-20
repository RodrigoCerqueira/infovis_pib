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
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(rgdal)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(shinydashboardPlus)
library(sp)
library(viridis)
library(leaflet)

# Datasets ---------------------------------------------------------------------

PIBanual <- read.csv2("pib_anual.csv", dec=",", h=T)
PIBanualBA <- subset(PIBanual, CodUF ==29)
PIBanualBA$cor <- ifelse(PIBanualBA$tx <0, "positivo", "negativo")
PIBsetores <- read.csv2("pib_setores.csv", dec=",", h=T)
PIBsetores$cor <- ifelse(PIBsetores$tx <0, "positivo", "negativo")
PIBbrneba <- subset(PIBanual, UF=="Bahia" | UF=="Brasil" | UF=="Nordeste")
PIBbrneba <- PIBbrneba %>% filter(Ano!=2018)
f <- list(size=11)
pibmunicipios <-read.csv2("pib_municipios.csv", dec=",")

# Tidy -------------------------------------------------------------------------

fonte_plotly <- list(family = "sans serif",size = 12, color = 'black')

# Invertendo banco para gráfico de pizza
pibmunicipios_pizza <- pibmunicipios %>% gather(SETOR, PARTICIP,AGRICULTURA:SERVIÇOS)

# Importando shape-file
municipio_bahia <- rgdal::readOGR(dsn=getwd(), layer="DPA_A_GEN_2019_05_14_GCS_SIR_SEI", encoding = "ISO-8859-1")

# Ajustando colunas dos datasets do mapa
municipio_bahia@data <- municipio_bahia@data %>% rename(CD_GEOCMU=Codigo)

pibmunicipios <- pibmunicipios %>% rename(CD_GEOCMU=cd_geocmu)

# transformando factor em num na shapefile
municipio_bahia@data[["CD_GEOCMU"]] <- as.numeric(as.character((municipio_bahia@data[["CD_GEOCMU"]])))


# Criando variavel categorizando o pibpercentual
pibmunicipios$categoria_percentual <- cut(pibmunicipios$pib_percentual, 
                                          breaks=c(0,0.05,0.10,0.5,1,5,100), 
                                          labels=c("0 - 0,05%", "0,05% - 0,1%", "0,1% - 0,5%", 
                                                   "0,5% - 1%","1% - 5%", ">5%"))

pibmunicipios$categoria_percentual_agro <- cut(pibmunicipios$PERCENTUAL_AGRO_BA, 
                                          breaks=c(0,0.0005,0.0010,0.005,0.01,0.05,1), 
                                          labels=c("0 - 0,05%", "0,05% - 0,1%", "0,1% - 0,5%", 
                                                   "0,5% - 1%","1% - 5%", ">5%"))

pibmunicipios$categoria_percentual_ind <- cut(pibmunicipios$PERCENTUAL_IND_BA, 
                                        breaks=c(0,0.0005,0.0010,0.005,0.01,0.05,1), 
                                        labels=c("0 - 0,05%", "0,05% - 0,1%", "0,1% - 0,5%", 
                                                 "0,5% - 1%","1% - 5%", ">5%"))

pibmunicipios$categoria_percentual_serv <- cut(pibmunicipios$PERCENTUAL_SERV_BA, 
                                          breaks=c(0,0.0005,0.0010,0.005,0.01,0.05,1), 
                                          labels=c("0 - 0,05%", "0,05% - 0,1%", "0,1% - 0,5%", 
                                                   "0,5% - 1%","1% - 5%", ">5%"))

# Paleta de cores para o mapa
wardpal <- colorFactor (palette = ("Blues"), pibmunicipios$categoria_percentual)
warpal2 <- colorFactor(palette = ("Greens"), pibmunicipios$categoria_percentual_agro)
wardpal3 <- colorFactor(palette = ("Oranges"), pibmunicipios$categoria_percentual_ind)
wardpal4 <- colorFactor(palette = ("Red"), pibmunicipios$catetoria_percentual_serv)

# PIB anual com separador de milhar - RODRIGO
PIBanual$PIB <- format(round(as.numeric(PIBanual$PIB), 1), nsmall=0,  big.mark=".", decimal.mark=",")

# Merge do dataset com o mapa
dados_e_mapa <- merge(municipio_bahia, pibmunicipios, by="CD_GEOCMU", duplicateGeoms = T) 

# Criando variável que será exibida no mapa
dados_e_mapa$textomapa <- paste0(dados_e_mapa$MUNICIPIO, " = ", round(dados_e_mapa$pib_percentual,2), "%")


# Server ----------------------------------------------------------------------------

function(input, output, session) {
  
  ######################################################################################
  # PAGINA PIB ANUAL
  ######################################################################################
  
  # Caixa com PIB Anual da Bahia
  output$PIBtotalBA <- renderValueBox({
    valueBox(
      paste("R$",subset(x=PIBanual,subset=(CodUF==29 & Ano==input$selectano),select = c(PIB)),"Milhões"),"Produto Interno Bruto do Estado", 
      icon = icon("dollar-sign"),
      color = "navy"
    )
  })
  # Caixa com a posicao da Bahia no ranking do Brasil
  output$particip_Bahia_Brasil <- renderValueBox({
    valueBox(
      if (is.na(subset(x=PIBanual,subset=(CodUF==29 & Ano==input$selectano),select = c(particip_br)))) {
        paste0("-")
        } else {
        paste0(format(subset(x=PIBanual,subset=(CodUF==29 & Ano==input$selectano),select = c(particip_br)),nsmall=0,  big.mark=".", decimal.mark=","),"% do Brasil")
      },
      "participação no Brasil", icon = icon("globe-americas"),
      color = "purple"
    )
  })
  
  # Caixa com a posicao da Bahia no ranking do Nordeste
  output$particip_Bahia_NE <- renderValueBox({
    valueBox(
      if (is.na(subset(x=PIBanual,subset=(CodUF==29 & Ano==input$selectano), select=c(particip_ne)))) {
        paste0("-") 
      } else {
        paste0(format(subset(x=PIBanual,subset=(CodUF==29 & Ano==input$selectano),select =c(particip_ne)),nsmall=0,  big.mark=".", decimal.mark=","),"% do NE")
      },
      #paste0(format(subset(x=PIBanual,subset=(CodUF==29 & Ano==input$selectano),select =c(particip_ne)),nsmall=0,  big.mark=".", decimal.mark=","),"% do NE"),
      "participação no Nordeste", icon = icon("umbrella-beach"),
      color = "maroon"
    )
  })
  
  # Caixa com a taxa de fecundidade em 2019
  output$tx_cresc <- renderValueBox({
    valueBox(
      paste0(format(subset(x=PIBanual,subset=(CodUF==29 & Ano==input$selectano),select = c(tx)), nsmall=0,  big.mark=".", decimal.mark=","),"%"),"em relação ao ano anterior", icon = icon("percent"),
      color = "orange"
    )
  })
  
  # Gráficos de Barras - Crescimento do VA dos Setores
  output$radar_pib <- renderPlot({
    ggplot(subset(PIBsetores, subset = (Ano==input$selectano)), aes(x=setor, y=tx))+
      geom_bar(stat= "identity", aes(fill =cor), width = 0.9, show.legend = F)+
      geom_text(aes(x=setor, y=tx, label = tx))+
      theme_bw()+
      labs(title= "Taxa de Crescimento do Valor Adicionado dos setores no ano", x = "", y= "Taxa de Crescimento (%)",
           caption = "Fonte: SEI-IBGE")+
      theme(axis.text.x = element_text(angle = 65, hjust = 1, size=11), axis.text.y = element_text())+
      scale_fill_manual(values = c("#0fabbc", "#fa163f"))
    
  })
  
  #Grafico de Radar (teia de aranha) estrutura dos setores
  output$tx_setores <- renderPlotly({ 
    PIBsetores %>% subset(Ano==input$selectano) %>%
      plot_ly() %>%
      add_trace(type = "scatterpolar",
              mode = "lines",
              r= ~estrutura,
              theta= ~setor,
              fill="toself",
              fillcolor="#83af70",
              opacity=0.9,
              line=list(color="#488f31", width=4),
              hoverinfo="estrutura") %>%
      layout(polar=list(radialaxis=list(visible=T, range = c(0,20))), showlegend=F, 
             title=list(text="Participação percentual (%) dos Setores no Valor Adicionado", x=0,
                        font=list(size=12, face="bold")))
  })
  
  #Grafico de barras - crescimento PIB
  output$tx_bahia <- renderPlot({
    ggplot(PIBanualBA, aes(x=factor(Ano), y=tx)) +
      theme_bw()+
      geom_bar(stat="identity", aes(fill = cor), show.legend = F)+
      geom_text(aes(x=factor(Ano), y=tx, label =tx))+
      labs(x = "Ano", y="Taxa de Crescimento (%)")+
      labs(title = "Taxa de Crescimento do PIB anual (2002 - 2017)", caption = "Fonte: SEI-IBGE")+
      theme(axis.text.x = element_text(vjust = 0.6), axis.text.y = element_text())+
      scale_fill_manual(values = c("#0fabbc", "#fa163f"))
    
  })
  
  #Série encadeada do volume do PIB - BA, BR, NE
  output$serie_ba_br_ne <- renderPlot({ 
    ggplot(PIBbrneba, aes(x=Ano, y=indice))+
      geom_point(aes(color=UF), size=rel(3.5), shape=16)+
      geom_line(aes(color=UF), size=rel(1.4))+ 
      scale_x_continuous(breaks =seq( from=2002, to=2017, by=1))+
      labs(x="Ano", y="",title =
             "Série encadeada do volume do Produto interno bruto (Base: 2002=100)", color="",caption="Fonte: SEI-IBGE")+
      theme_bw()+
      scale_colour_manual(values = c("#ffa600","#58508d","#bc5090"))
    
    
  })
  

  #######################################################################################
  # PAGINA TRIMESTRAL
  #######################################################################################
  
  # Em desenvolvimento
  
  #######################################################################################
  # PAGINA MUNICIPAL
  #######################################################################################  
  
  #Caixa com PIB do Município
  output$PIBtotalMunicipio <- renderValueBox({
    valueBox(
      paste("R$",format(subset(x=pibmunicipios, subset=(MUNICIPIO==input$selectmunicipio & ANO==input$sliderano2),select = c(PIB)),nsmall=0,  big.mark=".", decimal.mark=","),"Milhões"),"Produto Interno Bruto do Município", 
      icon = icon("dollar-sign"),
      color = "navy"
    )
  })
  
  #Caixa com PIB per capita
  output$PIBpercapita <- renderValueBox({
    valueBox(
      paste0("R$",format(subset(x=pibmunicipios, subset=(MUNICIPIO==input$selectmunicipio & ANO==input$sliderano2), select = c(PIB_PERCAPITA)),nsmall=0,  big.mark=".", decimal.mark=",")),"Produto Interno Bruto per capita", 
      icon = icon("users"),
      color = "purple"
    )
  })
  
  #Caixa com Participação dos Municípios Bahia
  output$PIBparticip <- renderValueBox({
    valueBox(
      paste0(format(subset(x=pibmunicipios, subset=(MUNICIPIO==input$selectmunicipio & ANO==input$sliderano2),select = c(pib_percentual)),nsmall=0,  big.mark=".", decimal.mark=","),"%"),"Participação do Município no PIB BA", 
      icon = icon("percent"),
      color = "maroon"
    )
  })
  
  #Caixa com IDEM dos Municípios Bahia
  output$IDEM <- renderValueBox({
    valueBox(
      paste0(format(subset(x=pibmunicipios, subset=(MUNICIPIO==input$selectmunicipio & ANO==input$sliderano2),select = c(IDEM)),nsmall=0,  big.mark=".", decimal.mark=",")),"IDEM do Município no PIB BA", 
      icon = icon("chart-line"),
      color = "orange"
    )
  })
  
  #MAPA PIB
  output$mapa_pib <- renderLeaflet({
    leaflet(subset(x=dados_e_mapa, subset=(ANO==input$sliderano2))) %>%
      #addTiles() %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      setView(lat = -13.591215, lng = -37.979077, zoom = 5.5) %>% 
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~wardpal(categoria_percentual),
                  label = ~paste0(MUNICIPIO.x, ": ", format(pib_percentual, big.mark = ".",decimal.mark=","), "%")) %>%
      addLegend("bottomright",pal = wardpal, values = ~categoria_percentual, opacity = 1.0, title = "Tamanho da População")
    
  })
  
  #PIZZA Municípios
  output$municip_pizza <- renderPlot({
    ggplot(subset(pibmunicipios_pizza, subset=(ANO==input$sliderano2 & MUNICIPIO==input$selectmunicipio)), aes(x="", y=PARTICIP, fill= SETOR)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) + 
    theme_bw() + 
    theme_void()+
    ggtitle("Participação dos setores da economia") +
    geom_text(aes(y = PARTICIP, label = SETOR), color = "white", size=6)
  })
  
  
}