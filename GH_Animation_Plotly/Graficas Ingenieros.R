setwd("C:\\Users\\jcarl\\Documents\\SESPEC\\Ingenieros")
librerias<-c("dplyr","ggplot2","plyr","plotly","gapminder")
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
b<-b+geom_text(aes(label=ifelse(G$City=="Cd. De México"|G$concentration>=100,as.character(G$City),'')),hjust=-.2,vjust=0,color=rgb(100,100,100, maxColorValue=255),size=3)
b
t<-ggplotly(b)
t
max(G$population)
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


K<-read.csv("datosclima2.csv")
names(K)<-c("AÑO","Proy15","Proy10","Proy5")
head(K)
L<-K[1:2] 
M<-K[1:3]
M$Proy15=NULL
N<-K
N$Proy15=NULL
N$Proy10=NULL

ML<-M %>%
  accumulate_by(~AÑO)
LL<-L %>%
  accumulate_by(~AÑO)
NL<-N %>%
  accumulate_by(~AÑO)

p<-ggplot()+geom_line(data=ML,aes(AÑO,Proy10,frame=frame,color="10 Años"))
p<-p+geom_line(data=LL,aes(AÑO,Proy15,frame=frame,color="15 Años"))
p<-p+geom_line(data=NL,aes(AÑO,Proy5,frame=frame,color="5 Años"))
p<-p+ggtitle("Proyección Clima Promedio en México - Escenarios 5,10,15 años")
p<-p+theme(title =  element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15))
p<-p+theme(legend.title=element_blank())+scale_y_continuous(breaks=seq(0,26.5,1))
p<-p+ annotate("text", x = c(2038,2038,2038), y =c(26,25,23.5), label = c("9% vs. '17","11% vs. '17","16% vs. '17"), fontface="bold")

r<-ggplotly(p)%>%
layout(  yaxis = list(
    title = "Temperatura Promedio en México (°C)",
    zeroline = F,
    tickprefix = "$"
  ),
  xaxis = list(
    title = "Año",
    zeroline = F, 
    showgrid = F
  )
) %>% 
  animation_opts(
    frame = 120, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Año ",font = list(color="Black")
    )
  )


r

