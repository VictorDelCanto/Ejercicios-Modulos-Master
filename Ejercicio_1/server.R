library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
# Cargamos los datos desde el repositorio github del módulo 

dataset <- get(load(url("https://github.com/pedroconcejero/master_UNED/blob/master/datos_4510_vehiculos_2016.rda?raw=true")))

#Una pequeña modificacion en el tipo  
dataset$Tipo[dataset$FuelType == "Electricity / Petrol"] <- "Hibrido Gasolina"
dataset$Tipo[dataset$FuelType == "Electricity / Diesel"] <- "Hibrido Diesel"
dataset$Tipo[dataset$FuelType == "Diesel Electric"] <- "Hibrido Diesel"
dataset$Tipo[dataset$FuelType == "Petrol Electric"] <- "Hibrido Gasolina"
dataset$Tipo[dataset$FuelType == "Petrol Hybrid"] <- "Hibrido Gasolina"
dataset$Tipo<-as.factor(dataset$Tipo)   #Conversion Tipo en factor para multilinea
levels(dataset$Tipo) <- gsub(" ", "\n", levels(dataset$Tipo))

# Y una clarificacion en las cajas de cambio. Las semiautomaticas se consideran automaticas
dataset$Transm="Manual"
dataset$Transm[grep("A",dataset$Transmission,ignore.case = TRUE)]<-"Automatico"

# funcion para el numero de observaciones 
give.n <- function(x){
  return(c(y = median(x)*1.3, label = length(x)))
}


shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot({
    
    p <- ggplot(dataset, 
                aes_string(x=input$x, y=input$y)) + geom_point() 
    
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, "~ .")
    print(facets)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if (input$lm)
      p <- p + geom_smooth(method='lm',formula=y~x, na.rm = T)
    if (input$smooth)
      p <- p + geom_smooth(method='loess',formula=y~x, na.rm = T)
    
    print(p)
    
  })
  output$plot2<-renderPlot({
    p<-ggplot(dataset,aes(x=dataset$Manufacturer,fill=dataset$Transm))
    p<-p+geom_bar(position=position_dodge())+labs(x="Marcas",y="Cantidad modelos")+guides(fill=guide_legend(title = "TRANSMISION"))
    p<-p+theme(axis.text.x = element_text(angle=90))
    p<-p+theme(legend.title =element_text(face="bold",size = 10))+theme(legend.position = c(.5,.9))
    p<-p+theme(axis.title = element_text(color="blue",size=12,lineheight = 1))
    print(p)
  })
  output$plot3<-renderPlot({
    p<-ggplot(dataset,aes(x=dataset$Transm,fill=dataset$Transm))
    p<-p+geom_bar(position=position_dodge())+labs(x=NULL, fill="Tipo Transmision",y="Total modelos")
    p<-p+theme(legend.title =element_text(face="bold",size = 10))
    p<-p+theme(axis.title = element_text(color="blue",size=12,lineheight = 1))+geom_text(stat="count",aes(label=..count..),position = position_dodge(width = .9),vjust=-1)
    print(p)
  })
  output$plot4<-renderPlot({
    p<-ggplot(dataset,aes(x=dataset$Transm,y=dataset$EngineCapacity,fill=dataset$Transm))
    p<-p+geom_boxplot()+labs(x="Tipo Transmision",y="Cilindrada")+guides(fill=guide_legend(title = "TRANSMISION"))
    p<-p+theme(legend.title =element_text(face="bold",size = 12)) + theme(legend.position = c(.5,.9),legend.background = element_rect(fill="transparent"))
    p<-p+theme(axis.title = element_text(color="blue",size=14,lineheight = 1))
    p<-p+theme(axis.text = element_text(size=12,lineheight = 1))
    print(p)
  })
  output$plot5<-renderPlot({
    subdatas<-dataset%>%filter(!is.na(EmissionsCOmgkm))
    p<-ggplot(subdatas,aes(y=subdatas$EmissionsCOmgkm,x=subdatas$Tipo,fill=subdatas$Tipo))
    p<-p+geom_boxplot(varwidth = TRUE)+labs(title="Emisiones Monoxido de Carbono (CO)",y="Cantidad")
    p<-p+theme(axis.text.x = element_text(color="blue",size=12,lineheight = 1))+guides(fill=guide_legend(title = "TIPO"))
    p<-p+theme(plot.title = element_text(face="bold",color="#b30000",size=15,margin(14),vjust = 1,hjust=0.5))
    p<-p+theme(axis.title.x = element_blank())+theme(legend.position = c(.75,.85))
    p<-p+stat_summary(fun.data = give.n, geom = "text", fun.y = median)
    print(p)
  })
  output$plot6<-renderPlot({
    subdatas<-dataset%>%filter(!is.na(EmissionsCOmgkm))
    p<-ggplot(subdatas,aes(y=subdatas$EmissionsNOxmgkm,x=subdatas$Tipo,fill=subdatas$Tipo))
    p<-p+geom_boxplot(varwidth = TRUE)+labs(title="Emisiones Oxido Nitroso (NOx)",y="Cantidad")
    p<-p+theme(axis.text.x = element_text(color="blue",size=12,lineheight = 1))+guides(fill=guide_legend(title = "TIPO"))
    p<-p+theme(plot.title = element_text(face="bold",color="#b30000",size=15,margin(14),vjust = 1,hjust=0.5))
    p<-p+theme(axis.title.x = element_blank())+theme(legend.position = c(.75,.85))
    p<-p+stat_summary(fun.data = give.n, geom = "text", fun.y = median)
    print(p)
    
  })
  output$plot7<-renderPlot({
    subdatas<-dataset%>%filter(!is.na(THCEmissionsmgkm))
    p<-ggplot(subdatas,aes(y=subdatas$THCEmissionsmgkm,x=subdatas$Tipo,fill=subdatas$Tipo))
    p<-p+geom_boxplot(varwidth = TRUE)+labs(title="Emisiones Totales Hidrocarburos (THC)",y="Cantidad")
    p<-p+theme(axis.text.x = element_text(color="blue",size=12,lineheight = 1))+guides(fill=guide_legend(title = "TIPO"))
    p<-p+theme(plot.title =  element_text(face="bold",color="#b30000",size=15,margin(14),vjust = 1,hjust=0.5))
    p<-p+theme(axis.title.x = element_blank())+theme(legend.position = c(.75,.85))
    p<-p+stat_summary(fun.data = give.n, geom = "text", fun.y = median)
    print(p)
  })
  output$plot8<-renderPlot({
    subdatas<-dataset%>%filter(!is.na(`THC+NOxEmissionsmgkm`))
    p<-ggplot(subdatas,aes(y=subdatas$`THC+NOxEmissionsmgkm`,x=subdatas$Tipo,fill=subdatas$Tipo))
    p<-p+geom_boxplot(varwidth = TRUE)+labs(title="Emisiones THC+NOx",y="Cantidad")
    p<-p+theme(axis.text.x = element_text(color="blue",size=12,lineheight = 1))+guides(fill=guide_legend(title = "TIPO"))
    p<-p+theme(plot.title = element_text(face="bold",color="#b30000",size=15,margin(14),vjust = 1,hjust=0.5))
    p<-p+theme(axis.title.x = element_blank())+theme(legend.position = c(.75,.85))
    p<-p+stat_summary(fun.data = give.n, geom = "text", fun.y = median)
    print(p)
  })
  output$plot9<-renderPlot({
    subdatas<-dataset%>%filter(!is.na(ParticulatesNo_mgkm))
    p<-ggplot(subdatas,aes(y=subdatas$ParticulatesNo_mgkm ,x=subdatas$Tipo,fill=subdatas$Tipo))
    p<-p+geom_boxplot(varwidth = TRUE)+labs(title="Particulas",y="Cantidad")
    p<-p+theme(axis.text.x = element_text(color="blue",size=12,lineheight = 1))+guides(fill=guide_legend(title = "TIPO"))
    p<-p+theme(plot.title = element_text(face="bold",color="#b30000",size=15,margin(14),vjust = 1,hjust=0.5))
    p<-p+theme(axis.title.x = element_blank())+theme(legend.position = c(.75,.85))
    p<-p+stat_summary(fun.data = give.n, geom = "text", fun.y = median)
    print(p)
  })
  output$plot10<-renderPlot({
    subdatas<-dataset%>%filter(!is.na(CO2gkm))
    p<-ggplot(subdatas,aes(y=subdatas$CO2gkm,x=subdatas$Tipo,fill=subdatas$Tipo))
    p<-p+geom_boxplot(varwidth = TRUE)+labs(title="Emisiones CO2",y="Cantidad gr/Km")
    p<-p+theme(axis.text.x = element_text(color="blue",size=12,lineheight = 1))+guides(fill=guide_legend(title = "TIPO"))
    p<-p+theme(plot.title = element_text(face="bold",color="#b30000",size=15,margin(14),vjust = 1,hjust=0.5))
    p<-p+theme(axis.title.x = element_blank())+theme(legend.position = c(.75,.85))
    p<-p+stat_summary(fun.data = give.n, geom = "text", fun.y = median)
    print(p)
  })
  output$plot11<-renderPlot({
    subdatas<-dataset%>%filter(!is.na(EngineCapacity))
    p<-ggplot(subdatas,aes_string(x="EngineCapacity",y="CO2gkm"))+geom_point()
    p<-p+facet_grid(subdatas$Tipo ~ .)
    p<-p+aes_string(color="Tipo")+labs(title="RELACION LINEAL ENTRE CILINDRADA Y DIOXIDO DE CARBONO",x="Cilindrada",y="CO2")
    p<-p+geom_smooth(method='lm',formula=y~x, na.rm = T)
    p<-p+theme(plot.title = element_text(face="bold",color="#b30000",size=15,margin(14),vjust = 1,hjust=0.5))
    p<-p+theme(axis.title = element_text(color="blue",size=12,lineheight = 1))
    print(p)
  })
  output$plot12<-renderPlot({
    subdatas<-dataset%>%filter(!is.na(EngineCapacity))
    
    p<-ggplot(subdatas,aes_string(x="EngineCapacity",y="Totalcost12000miles"))
    p<-p+geom_point()
    p<-p+facet_grid(subdatas$Tipo ~ .)
    p<-p+aes_string(color="Tipo")+labs(title="RELACION LINEAL ENTRE CILINDRADA Y COSTE CARBURANTE (12000 millas)",x="Cilindrada",y="libras esterlinas")
    p<-p+geom_smooth(method='lm',formula=y~x, na.rm = T)
    p<-p+theme(plot.title = element_text(face="bold",color="#b30000",size=15,margin(14),vjust = 1,hjust=0.5))
    p<-p+theme(axis.title = element_text(color="blue",size=12,lineheight = 1))
    print(p)
  })
  output$plot13<-renderPlot({
    subdatas<-dataset%>%filter(!is.na(EngineCapacity))
    
    p<-ggplot(subdatas,aes_string(x="EngineCapacity",y="Totalcost12000miles"))
    p<-p+aes_string(color="Tipo")+labs(title="RELACION LINEAL ENTRE CILINDRADA Y COSTE CARBURANTE (12000 millas)",x="Cilindrada",y="libras esterlinas",subtitle="COMPARATIVA ENTRE TIPOS DE CARBURANTE (Pendiente de la recta)")
    p<-p+geom_smooth(method='lm',formula=y~x, na.rm = T)
    p<-p+theme(plot.title = element_text(face="bold",color="#b30000",size=15,margin(14),vjust = 1,hjust=0.5))
    p<-p+theme(plot.subtitle =  element_text(face="italic",color="#b30000",size=12,margin(14),vjust = 1,hjust=0.5))
    p<-p+theme(axis.title = element_text(color="blue",size=12,lineheight = 1))
    print(p)
  })
  output$plot14<-renderPlot({
    subdatas1<-dataset%>%filter(!is.na(whkm))
    
    p<-ggplot(subdatas1,aes_string(x="whkm",y="MaximumrangeKm"))+geom_point()
    p<-p+facet_grid(subdatas1$Tipo ~ .)
    p<-p+aes_string(color="Tipo")+labs(title="RELACION ENTRE POTENCIA ELECTRICA Y AUTONOMIA",x="Wh/Km",y="Km")
    p<-p+geom_smooth(method='lm',formula=y~x, na.rm = T)
    p<-p+theme(plot.title = element_text(face="bold",color="#b30000",size=15,margin(14),vjust = 1,hjust=0.5))
    p<-p+theme(axis.title = element_text(color="blue",size=12,lineheight = 1))
    print(p)
  })
  output$plot15<-renderPlot({
    subdatas1<-dataset%>%filter(!is.na(whkm))
    
    p<-ggplot(subdatas1,aes_string(x="whkm",y="Totalcost12000miles"))
    p<-p+aes_string(color="Tipo")+labs(title="RELACION LINEAL ENTRE POTENCIA ELECTRICA Y COSTE ENERGIA (12000 millas)",x="Potencia Wh/kM",y="libras esterlinas")
    p<-p+geom_smooth(method='lm',formula=y~x, na.rm = T)+geom_point()
    p<-p+theme(plot.title = element_text(face="bold",color="#b30000",size=15,margin(14),vjust = 1,hjust=0.5))
    p<-p+theme(axis.title = element_text(color="blue",size=12,lineheight = 1))
    print(p)
    
  })
  output$plot16<-renderPlot({
    subdata<-dataset%>%filter(Manufacturer==input$marca)
    
    p<-ggplot(subdata,aes(x=subdata$Transm,fill=subdata$Transm))
    p<-p+geom_bar(aes(x=subdata$Transm,y=..count..), stat="count", position=position_dodge())+labs(x="TOTAL",y="Cantidad modelos")+guides(fill=guide_legend(title = "TRANSMISION"))
    p<-p+theme(legend.title =element_text(face="bold",size = 12)) + theme(legend.position = c(.5,.9),legend.background = element_rect(fill="transparent"))
    p<-p+theme(axis.title = element_text(color="blue",size=14,lineheight = 1))
    p<-p+theme(axis.text = element_text(size=12,lineheight = 1))+geom_text(stat="count",aes(label=..count..),position = position_dodge(width = .9),vjust=-1)
    print(p)
  })
  output$plot17<-renderPlot({
    subdata1<-dataset%>%filter(Manufacturer==input$marca)
    
    p<-ggplot(subdata1,aes(x=subdata1$Tipo,fill=subdata1$Transm))
    p<-p+geom_bar(aes(x=subdata1$Tipo,y=..count..), stat="count", position=position_dodge())+labs(x="TIPO",y="Cantidad modelos")+guides(fill=guide_legend(title = "TRANSMISION"))
    p<-p+theme(legend.title =element_text(face="bold",size = 12)) + theme(legend.position = c(.5,.9),legend.background = element_rect(fill="transparent"))
    p<-p+theme(axis.title = element_text(color="blue",size=14,lineheight = 1))
    p<-p+theme(axis.text = element_text(size=12,lineheight = 1))+geom_text(stat="count",aes(label=..count..),position = position_dodge(width = .9),vjust=-1)
    print(p)
  })
  output$plot18<-renderPlot({
    subdata1<-dataset%>%select(EngineCapacity,MetricUrbanCold,MetricExtraUrban,Tipo)
    subdata2<-subdata1%>%gather(key,value,MetricUrbanCold,MetricExtraUrban,-EngineCapacity)
    subdata2<-subdata2%>%filter(!is.na(value))
    
    p<-ggplot(subdata2)
    p<-p+aes_string(x="EngineCapacity",y="value",color="key")
    p<-p+geom_smooth(method='lm',formula=y~x, na.rm = T)+labs(x="Cilindrada",y="Consumo litros")
    p<-p+facet_grid(~Tipo)+scale_color_discrete (name="CONSUMO",breaks=c("MetricExtraUrban","MetricUrbanCold"),labels=c("Consumo Extra Urbano","Consumo Urbano"))
    p<-p+theme(legend.title =element_text(face="bold",size = 12)) + theme(legend.position = c(.6,.75))
    p<-p+theme(axis.title = element_text(color="blue",size=14,lineheight = 1))
    p<-p+theme(axis.text = element_text(size=12,lineheight = 1))
    print(p)
  })
  output$plot19<-renderPlot({
    subCOdata<-dataset
    subCOdata[,"CO2level"]<-cut(subCOdata$CO2gkm,breaks = c(-1,100,110,120,130,140,150,165,175,185,200,225,255,380),labels = c("A","B","C","D","E","F","G","H","I","J","K","L","M"))
    subCOdata<-subCOdata%>%arrange(Tipo,CO2level)
    
    p<-ggplot(subCOdata,aes(x=subCOdata$CO2level,fill=subCOdata$Tipo))
    p<-p+geom_bar()+geom_text(stat="count",aes(label=..count..),vjust=-1)
    p<-p+facet_grid(~subCOdata$Tipo)+labs(x="Banda",y="Cantidad modelos")+guides(fill=guide_legend(title = "TIPO"))
    p<-p+theme(axis.title = element_text(color="blue",size=12,lineheight = 1))
    print(p)
  })

})