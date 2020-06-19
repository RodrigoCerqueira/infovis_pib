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
library(treemap)
library(viridis)
library(leaflet)


#Fonte do grafico PLOTLY - Cleiton
fonte_plotly <- list(family = "sans serif",size = 12, color = 'black')

#DataSets necessários para geração dos gráficos PIB ANUAL - RODRIGO
PIBanual <- read.csv2("pib_anual.csv", dec=",", h=T)
PIBanualBA <- subset(PIBanual, CodUF ==29)
PIBanualBA$cor <- ifelse(PIBanualBA$tx <0, "positivo", "negativo")
PIBsetores <- read.csv2("pib_setores.csv", dec=",", h=T)
PIBsetores$cor <- ifelse(PIBsetores$tx <0, "positivo", "negativo")
PIBbrneba <- subset(PIBanual, UF=="Bahia" | UF=="Brasil" | UF=="Nordeste")
PIBbrneba <- PIBbrneba %>% filter(Ano!=2018)
f <- list(size=11)
pibmunicipios <-read.csv2("pib_municipios.csv", dec=",")
#dadosmapa <- read.csv2("dados_e_mapas.csv")


#invertendo banco para gráfico de pizza
pibmunicipios_pizza <- pibmunicipios %>% gather(SETOR, PARTICIP,AGRICULTURA:SERVIÇOS)

#importando arquivo json - BRUNO
municipio_bahia <- rgdal::readOGR(dsn=getwd(), layer="DPA_A_GEN_2019_05_14_GCS_SIR_SEI", encoding = "ISO-8859-1")
municipio_bahia@data <- municipio_bahia@data %>% rename(CD_GEOCMU=Codigo)

pibmunicipios <- pibmunicipios %>% rename(CD_GEOCMU=cd_geocmu)

# transformando factor em num na shapefile
municipio_bahia@data[["CD_GEOCMU"]] <- as.numeric(as.character((municipio_bahia@data[["CD_GEOCMU"]])))
#convertendo variavel de caracter em numerica - ABA 1 - BRUNO
#municipio_bahia$cd_geocmu <- as.numeric(municipio_bahia$cd_geocmu)

#calcular pontos nos quais plotar etiquetas - ABA 1 - BRUNO
#centroids <- municipio_bahia %>% st_centroid() %>% bind_cols(as_data_frame(st_coordinates(.)))

#criando variavel categorizando o pibpercentual - ABA 1 - BRUNO
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

#paleta
wardpal <- colorFactor (palette = ("Blues"), pibmunicipios$categoria_percentual)
warpal2 <- colorFactor(palette = ("Greens"), pibmunicipios$categoria_percentual_agro)
wardpal3 <- colorFactor(palette = ("Oranges"), pibmunicipios$categoria_percentual_ind)
wardpal4 <- colorFactor(palette = ("Red"), pibmunicipios$catetoria_percentual_serv)

#banco unindo mapa com os dados - ABA 1 # - BRUNO
dados_e_mapa <- merge(municipio_bahia, pibmunicipios, by="CD_GEOCMU", duplicateGeoms = T) 

dados_e_mapa$textomapa <- paste0(dados_e_mapa$MUNICIPIO, " = ", round(dados_e_mapa$pib_percentual,2), "%")

#dadosmapa <- dados_e_mapa %>% filter (ANO==2016)

#PIB anual com separador de milhar - RODRIGO
PIBanual$PIB <- format(round(as.numeric(PIBanual$PIB), 1), nsmall=0,  big.mark=".", decimal.mark=",")

#server
function(input, output, session) {
  
  ######################################################################################
  # PAGINA INFORMACOES GERAIS
  ######################################################################################
  
  #Caixa com Populacao total da Bahia
  output$PIBtotalBA <- renderValueBox({
    valueBox(
      paste("R$",subset(x=PIBanual,subset=(CodUF==29 & Ano==input$selectano),select = c(PIB)),"Milhões"),"Produto Interno Bruto do Estado", 
      icon = icon("dollar-sign"),
      color = "navy"
    )
  })
  #Caixa com a posicao da Bahia no ranking do Brasil
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
  
  #Caixa com a posicao da Bahia no ranking do Nordeste
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
  
  #Caixa com a taxa de fecundidade em 2019
  output$tx_cresc <- renderValueBox({
    valueBox(
      paste0(format(subset(x=PIBanual,subset=(CodUF==29 & Ano==input$selectano),select = c(tx)), nsmall=0,  big.mark=".", decimal.mark=","),"%"),"em relação ao ano anterior", icon = icon("percent"),
      color = "orange"
    )
  })
  
  #Gráficos de Barras - Crescimento do VA dos Setores
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
  
  #Box com populacao total
  output$poptotalproj <- renderValueBox({
    valueBox(
      subset(x=proj18,subset=(Ano==input$sliderano),select = c(PopTotal)),"populacao baiana", icon = icon("globe"),
      color = "blue"
    )
  })
  
  #Box com tft projetada
  output$tftproj <- renderValueBox({
    valueBox(
      format(subset(x=proj18,subset=(Ano==input$sliderano),select = c(TFT)), nsmall=0,  big.mark=".", decimal.mark=","),"filhos por mulher", icon = icon("globe"),
      color = "purple"
    )
  })
  
  #Box EVN
  output$EVN <- renderValueBox({
    valueBox(
      subset(x=proj18,subset=(Ano==input$sliderano),select = c(EVNAmbos)),"Em relação ao ano anterior", icon = icon("globe"),
      color = "green"
    )
  })
  
  #Box com Mortalidade Infantil
  output$MortInf <- renderValueBox({
    valueBox(
      subset(x=proj18,subset=(Ano==input$sliderano),select = c(Pop60Mais)),"dos baianos tem 60+ anos", icon = icon("globe"),
      color = "maroon"
    )
  })
  
  
  #Imprime a piramide da pagina projecoes
  output$piramide <- renderPlot({   
              ggplot(subset(popfaixaquinquenal, subset= (Ano==input$sliderano)), aes(x=FaixaEtaria, y=Populacao, fill=Genero)) +
               geom_bar(stat="identity") +
               coord_flip() +
               #theme_bw() +
               scale_fill_manual(values = c("#1b6db5","#b51b8f"))+
               labs(x="Faixa Etaria", y="Ano", title = "Piramide Etaria", caption = "Fonte: IBGE - Projecao (2010-2060)") +
               theme(legend.position="bottom", legend.title = element_text(colour="Black", size=12, face="bold"), legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"))+
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_text(size=12, face="bold", colour = "black"),
            axis.title.y = element_text(size=12, face="bold", colour = "black"),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=7),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=7)) +
      scale_y_continuous(breaks = seq(-700000,700000, 200000),
                         labels = paste0(as.character(c(seq(700, 0, -200),
                                                        seq(100, 700, 200))), "Mil"))
    
   
                                }) 
  
  #Imprime o grafico expectativa de vida ao nascer
  output$EVNporSexo <- renderPlot({ 
   
    ggplot(proj18, aes(x=Ano)) +
      geom_area(aes(y=EVNFem),fill="violetred3",alpha=0.3) +
      geom_area(aes(y=EVNMasc),fill="royalblue4",alpha=0.8) +
      geom_line(aes(y=EVNFem,color="red")) +
      geom_line(aes(y=EVNMasc,color="blue")) +           
      labs(title = "Esperanca de vida, em anos",
           caption = "Fonte: IBGE - Projecao (2010-2060)",
           x="Ano", y="Anos de Vida") +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=7),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=9)) +
      scale_colour_manual(name = 'Legenda', 
                          values =c('red'='red','blue'='blue'), labels = c('Homem','Mulher')) +
      theme(legend.position = "bottom", legend.background = element_rect(fill="ghostwhite",
                                                                         size=0.5, linetype="blank"))

   })
  
  #Imprime o grafico da populacao geral por ano
  output$GrafPopTotal <- renderPlot({ 
    ggplot(data=subset(x=proj18,subset=(Ano %in% seq(2010,2060,by=5)))) + 
      geom_col(aes(x=as.factor(Ano),y=PopTotal,fill=as.factor(Ano)), show.legend = FALSE) + scale_fill_manual(values = paleta2) +
      labs(x = "Ano", y = "Pessoas",
           title ="Populacao projetada pelo IBGE, Bahia, 2010-2060.",
           caption = "Fonte: IBGE") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_text(size=12, face="bold", colour = "black"),
            axis.title.y = element_text(size=12, face="bold", colour = "black"),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=7),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=7))
  })
  
  #Imprime o grafico de TBN e TBM
  output$TaxasBrutas <- renderPlot({      
    ggplot(proj18,aes(x=Ano)) +
      geom_area(aes(y=TBM), fill="brown4", alpha=0.6) +
      geom_area(aes(y=TBN), fill="orange1",alpha=0.3) +
      geom_line(aes(y=TBM,color="red")) +
      geom_line(aes(y=TBN,color="orange")) +           
      labs(title = "Taxa Bruta de Natalidade (TBN) e\n Taxa Bruta de Mortalidade (TBM), Bahia, 2010-2060.",
           caption = "Fonte: IBGE - Projecao (2010-2060)") +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=7),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=9)) +
      scale_colour_manual(name = 'Legenda', 
                          values =c('red'='red','orange'='orange'), labels = c('TBN','TBM')) +
      theme(legend.position = "bottom", legend.background = element_rect(fill="ghostwhite",
                                                                         size=0.5, linetype="blank"))
    
  })
  
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
      addTiles() %>%
      setView(lat = -13.591215, lng = -37.979077, zoom = 5.5) %>% 
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~wardpal(categoria_percentual),
                  label = ~paste0(MUNICIPIO, ": ", format(pib_percentual, big.mark = ".",decimal.mark=","), "%")) %>%
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