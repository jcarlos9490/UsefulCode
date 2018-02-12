librerias<-c("ggplot2","plyr","plotly","gapminder")
lapply(librerias,require,character.only=1)

D<-read.csv("data.csv")
head(D)
D$mesl<- factor(D$mesl,levels = c("Ene", "Feb", "Mar", "Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))
a<-ggplot(D,aes(x=mesl,y=promedio))+ theme(title =  element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15))
a<-a+ggtitle("Nivel de CO2 por año - Partes por Millón")
a<-a+theme(axis.text.x =element_text(face="bold",size=9, color="Black",angle = 90, hjust = 1))
a<-a+labs(x="Mes",y="Promedio de Concentración de CO2",legend="Tendencia (ppm)")
a<-a+geom_jitter(aes(frame=Año,color=tendencia,size=tendencia))+scale_color_continuous(name = "Tendencia (ppm)")
p<-ggplotly(a)%>%animation_slider(currentvalue = list(prefix = "Año ", font = list(color="Black")))
p

S<-read.csv("data2.csv")
G<-filter(S,cont=="Americas")
G$population<-as.numeric(G$population)
b<-ggplot(G,aes(population,concentration))+geom_jitter(aes(color=concentration))
b<-b+scale_x_continuous(breaks=seq(0,max(G$population),2000))
b<-b+geom_text(aes(label=ifelse(G$concentration>65|G$City=="Cd. de México",as.character(G$City),'')),hjust=-.3,vjust=0,color=rgb(100,100,100, maxColorValue=255),size=2)
b
t<-ggplotly(b)
t
max(G$population)
