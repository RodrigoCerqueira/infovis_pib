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
pibmunicipios <-read.csv2("pib_municipios.csv", dec=",")

# Tidy -------------------------------------------------------------------------

fonte_plotly <- list(family = "sans serif",size = 12, color = 'black')

# Invertendo banco para gráfico de pizza
pibmunicipios_pizza <- pibmunicipios %>% gather(SETOR, PARTICIP,AGRO:SERV)

# Importando shape-file
municipio_bahia <- rgdal::readOGR(dsn=getwd(), layer="DPA_A_GEN_2019_05_14_GCS_SIR_SEI", encoding = "UTF-8")

# Ajustando colunas dos datasets do mapa
municipio_bahia@data <- municipio_bahia@data %>% rename(CD_GEOCMU=Codigo)

pibmunicipios <- pibmunicipios %>% rename(CD_GEOCMU=cd_geocmu)

# transformando factor em num na shapefile
municipio_bahia@data[["CD_GEOCMU"]] <- as.numeric(as.character((municipio_bahia@data[["CD_GEOCMU"]])))


# Criando variavel categorizando o pibpercentual
pibmunicipios$PIB <- cut(pibmunicipios$pib_percentual, 
                                          breaks=c(0,0.05,0.10,0.5,1,5,100), 
                                          labels=c("0 - 0,05%", "0,05% - 0,1%", "0,1% - 0,5%", 
                                                   "0,5% - 1%","1% - 5%", ">5%"))

pibmunicipios$Agropecuária <- cut(pibmunicipios$PERCENTUAL_AGRO_BA, 
                                          breaks=c(0,0.0005,0.0010,0.005,0.01,0.05,1), 
                                          labels=c("0 - 0,05%", "0,05% - 0,1%", "0,1% - 0,5%", 
                                                   "0,5% - 1%","1% - 5%", ">5%"))

pibmunicipios$Indústria <- cut(pibmunicipios$PERCENTUAL_IND_BA, 
                                        breaks=c(0,0.0005,0.0010,0.005,0.01,0.05,1), 
                                        labels=c("0 - 0,05%", "0,05% - 0,1%", "0,1% - 0,5%", 
                                                 "0,5% - 1%","1% - 5%", ">5%"))

pibmunicipios$Serviços <- cut(pibmunicipios$PERCENTUAL_SERV_BA, 
                                          breaks=c(0,0.0005,0.0010,0.005,0.01,0.05,1), 
                                          labels=c("0 - 0,05%", "0,05% - 0,1%", "0,1% - 0,5%", 
                                                   "0,5% - 1%","1% - 5%", ">5%"))

# Paleta de cores para o mapa
wardpal <- colorFactor (palette = ("Blues"), pibmunicipios$PIB)
wardpal2 <- colorFactor(palette = ("Greens"), pibmunicipios$Agropecuária)
wardpal3 <- colorFactor(palette = ("Oranges"), pibmunicipios$Indústria)
wardpal4 <- colorFactor(palette = ("Reds"), pibmunicipios$Serviços)

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
  
  # ValueBoxes ------------------------------------------------------------------------
  
  # Caixa com PIB Anual da Bahia
  output$PIBtotalBA <- renderValueBox({
    valueBox(
      paste("R$",format(round(as.numeric(subset(x=PIBanual,subset=(CodUF==29 & Ano==input$selectano),select = c(PIB))),2),nsmall=0,  big.mark=".", decimal.mark=","),"Bi"),"Produto Interno Bruto do Estado", 
      icon = icon("dollar-sign"),
      color = "navy"
    )
  })
  # Caixa com a participação do PIB da Bahia no Brasil
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
  
  # Caixa com a participação do PIB da Bahia no Nordeste
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
  
  # Caixa com a taxa crescimento do PIB em relação ao ano anterior
  output$tx_cresc <- renderValueBox({
    valueBox(
      paste0(format(subset(x=PIBanual,subset=(CodUF==29 & Ano==input$selectano),select = c(tx)), nsmall=0,  big.mark=".", decimal.mark=","),"%"),"em relação ao ano anterior", icon = icon("percent"),
      color = "orange"
    )
  })
  
  # outputs text reativos _________________ -------------------------------------------
  
  output$ano <- renderText({
    paste(input$selectano)
  })
  
  output$ano2 <- renderText({
    paste(input$selectano)
  })
  
  output$setor <- renderText({
    paste(input$selectsetor)
  })
  
  output$municipio <- renderText({
    paste(input$selectmunicipio)
  })
  
  # Gráficos -------------------------------------------------------------------------
  
  # Gráficos de Barras - Crescimento do VA dos Setores
  output$radar_pib <- renderPlot({
    ggplot(subset(PIBsetores, subset = (Ano==input$selectano)), aes(x=setor, y=tx))+
      geom_bar(stat= "identity", aes(fill =cor), width = 0.9, show.legend = F)+
      coord_flip()+ 
      geom_text(aes(x=setor, y=tx, label = paste0(tx, "%")),position = position_dodge(width = 1), 
                hjust = "inward", fontface = "bold")+
      theme_classic()+
      labs(x = "", y= "Taxa de Crescimento (%)")+
      theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 11))+
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
              fillcolor="#01a9b4",
              opacity=0.9,
              line=list(color="#086972", width=4),
              hoverinfo="estrutura") %>%
      layout(polar=list(radialaxis=list(visible=T, range = c(0,20))), showlegend=F)
  })
  
  #Grafico de barras - crescimento PIB
  output$tx_bahia <- renderPlot({
    ggplot(PIBanualBA, aes(x=factor(Ano), y=tx)) +
      theme_classic()+
      geom_bar(stat="identity", aes(fill = cor), show.legend = F)+
      geom_text(aes(x=factor(Ano), y=tx, label =paste0(tx, "%")), 
                vjust = -0.5, fontface="bold")+
      labs(x = "Ano", y="Taxa de Crescimento (%)")+
      theme(axis.text.x = element_text(vjust = 0.6, size = 9), axis.text.y = element_text(),
            axis.title.x = element_blank())+
      scale_fill_manual(values = c("#0fabbc", "#fa163f"))
    
  })
  
  #Série encadeada do volume do PIB - BA, BR, NE
  output$serie_ba_br_ne <- renderPlot({ 
    ggplot(PIBbrneba, aes(x=Ano, y=indice))+
      geom_point(aes(color=UF), size=rel(3.5), shape=16)+
      geom_line(aes(color=UF), size=rel(1.4))+ 
      scale_x_continuous(breaks =seq( from=2002, to=2017, by=1))+
      labs(x="Ano", y="", color="")+
      theme_classic()+
      theme(axis.text.x = element_text(vjust = 0.6, size = 9), axis.text.y = element_text(),
           axis.title.x = element_blank())+
      scale_colour_manual(values = c("#ffa600","#58508d","#bc5090"))
    
    
  })
  

  #######################################################################################
  # PAGINA TRIMESTRAL
  #######################################################################################
  
  # Em desenvolvimento
  
  #######################################################################################
  # PAGINA MUNICIPAL
  #######################################################################################  
  
  # ValueBoxes --------------------------------------------------------------------------
  
  #Caixa com PIB do Município
  output$PIBtotalMunicipio <- renderValueBox({
    valueBox(
      paste("R$",format(subset(x=pibmunicipios, subset=(MUNICIPIO==input$selectmunicipio & ANO==input$sliderano2),select = c(PIB_TOTAL)),nsmall=0,  big.mark=".", decimal.mark=","),"Mi"),"Produto Interno Bruto do Município", 
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
  
  # Gráficos e Mapas --------------------------------------------------------------------
  
  # Mapa dos PIBs dos municípios
  output$mapa_pib <- renderLeaflet({
    leaflet(subset(x=dados_e_mapa, subset=(ANO==input$sliderano2))) %>%
      addProviderTiles("CartoDB.PositronNoLabels", options = providerTileOptions(minZoom = 5, maxZoom = 8)) %>%
      setView(lat = -13.800000, lng = -41.559343, zoom = 5.5) %>% 
      addPolygons(stroke = T, opacity =1, color = "black", weight = 0.5, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~ if (input$selectsetor == "PIB") {
                    wardpal(PIB)
                  } else if (input$selectsetor == "Agropecuária") {
                    wardpal2(Agropecuária)
                  } else if (input$selectsetor == "Indústria") {
                    wardpal3(Indústria)
                  } else {wardpal4(Serviços)},
                  label = ~paste0(MUNICIPIO.x, ": ", format(if (input$selectsetor == "PIB") {
                    pib_percentual
                  } else if (input$selectsetor == "Agropecuária") {
                    PERCENTUAL_AGRO_BA*100
                  } else if (input$selectsetor == "Indústria") {
                    PERCENTUAL_IND_BA*100
                  } else {PERCENTUAL_SERV_BA*100}, big.mark = ".",decimal.mark=","), "%")) %>%
      addLegend("bottomright",pal = if (input$selectsetor == "PIB") {
        wardpal
      } else if (input$selectsetor == "Agropecuária") {
        wardpal2
      } else if (input$selectsetor == "Indústria") {
        wardpal3
      } else {wardpal4},
      values = ~ if (input$selectsetor == "Serviços") {
        PIB
      } else if (input$selectsetor == "Agropecuária") {
        Agropecuária
      } else if (input$selectsetor == "Indústria") {
        Indústria
      } else {Serviços}, opacity = 1.0, title = if (input$selectsetor == "PIB") {
        "Participação no PIB da Bahia"
      } else if (input$selectsetor == "Agropecuária") {
        "Participação no VA da Agropecuária"
      } else if (input$selectsetor == "Indústria") {
        "Participação no VA da Indústria"
      } else {"Participação no VA dos Serviços"})
    
  })
  
  # Gráfico de pizza dos Municípios
  output$municip_pizza <- renderPlot({
    ggplot(subset(pibmunicipios_pizza, subset=(ANO==input$sliderano2 & MUNICIPIO==input$selectmunicipio)), aes(x="", y=PARTICIP, fill= SETOR)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) + 
    theme_hc() + 
    theme_void()
    #geom_text(aes(y = PARTICIP/2, label = PARTICIP), color = "white", size=6)
  })
  
  
}