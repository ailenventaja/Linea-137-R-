Código Línea 137
================

El siguiente código corresponde a los gráficos publicados
[aquí](https://rpubs.com/ailuvee/648118). En ese enlace está la fuente
de datos. No se incluyen los resultados de plots en este documento
porque son interactivos.

``` r
#Librerías usadas
library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(treemap)
library(highcharter) 
library(viridis)
library(leaflet)
library(rCharts)
library(plyr)
library(plotly)
library(RColorBrewer)
library(rvest)
library(waffle)

#Carga
linea137a <- read.csv('llamados-atendidos-abuso-sexual-2020-trimestre-1.csv', header = T, sep = ',', encoding = "UTF-8")
linea137b <- read.csv('llamados-atendidos-abuso-sexual-202004.csv', header = T, sep = ',', encoding = "UTF-8")
linea137c <- read.csv('llamados-atendidos-abuso-sexual-202005.csv', header = T, sep = ',', encoding = "UTF-8")
linea137d <- read.csv('llamados-atendidos-abuso-sexual-202006.csv', header = T, sep = ',', encoding = "UTF-8")

#Unión
linea137 <- rbind(linea137a,linea137b,linea137c,linea137d)

#Nuevo DF contabilizando vínculo con agresor
agresores <- data.frame(table(linea137$victima_vinculo_agresor))

#Elimino NS/NC
agresores <- agresores[-9,] 

#Setteo opciones de Highcharter
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

#Treemap
agresortree <- agresores %>% 
  hchart(
    "treemap", 
    hcaes(x = Var1, value = Freq, color=Freq)
  )

#Estilo, título y créditos
agresortree <- agresortree %>% 
  hc_colorAxis(stops = color_stops(colors = viridis::inferno(10,direction = -1))) %>% 
  hc_title(text='Vínculo con el agresor',margin = 20, align = "center")%>% 
  hc_credits(enabled = TRUE,
             text = "Fuente: datos.gob.ar",
             style = list(fontSize = "15px"))%>% 
  hc_tooltip(valueDecimals=0)

#Nuevo DF contabilizando lugar del hecho
lugar <- data.frame(table(linea137$hecho_lugar))

#Elimino NS/NC
lugar <- lugar[-7,] 

#Elimino Otro (son muy pocos y están en un registro aparte)
lugar <- lugar[-17,] 

#Treemap
lugartree <- lugar %>% 
  hchart(
    "treemap", 
    hcaes(x = Var1, value = Freq, color=Freq) 
  )

#Estilo, título y créditos
lugartree <- lugartree %>%  
  hc_colorAxis(stops = color_stops(colors = viridis::viridis(10,direction = -1)))%>% 
  hc_title(text='Lugar de los hechos',margin = 20, align = "center") %>% 
  hc_credits(enabled = TRUE,
             text = "Fuente: datos.gob.ar",
             style = list(fontSize = "15px"))%>% 
  hc_tooltip(valueDecimals=0)

#Nuevo DF contabilizando llamadas por provincia
provincia <- data.frame(table(linea137$llamado_provincia))

#Elimino NS/NC
provincia <- provincia[-15,] 

#Cambio el nombre de columna para que coincida con el DF del mapa
provincia <- dplyr::rename(provincia, name=Var1)

#Cambio nombres de provincias para que coincidan con el DF del mapa
provincia <- provincia %>% mutate(name = str_replace(name,'Santa Fé', 'Santa Fe'))
provincia <- provincia %>% mutate(name = str_replace(name,'Ciudad Autónoma de Buenos Aires', 'Ciudad de Buenos Aires'))

#Cargo mapa
hcmap("countries/ar/ar-all")
mapdata <- get_data_from_map(download_map_data("countries/ar/ar-all"))

#Uno mapa con frecuencias
data_ar <- merge(x = mapdata, y = provincia, by = "name")

    
#Grafico mapa
hcmap("countries/ar/ar-all", data = data_ar, value = "Freq",name='Llamados:',
      joinBy = c("name", "name"),
      dataLabels = list(enabled = TRUE, format = '{point.name}')) %>% 
hc_colorAxis(stops = color_stops(colors = viridis::inferno(10,direction = -1))) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text='Poveniencia de las llamadas',margin = 20, align = "center") %>% 
  hc_credits(enabled = TRUE,
             text = "Fuentes: datos.gob.ar",
             style = list(fontSize = "15px")) %>% 
  hc_tooltip(valueDecimals=0)

#Trabajo género y edad
genero <- linea137$victima_genero
generoedad <- data.frame(genero)
generoedad$edad <- linea137$victima_edad
generoedad <- generoedad %>% filter(edad!='Sin dato')
generoedad <- generoedad %>% filter(genero!='Ns/Nc')
#Categoría Trans solo 6 casos, parece que no es algo registrado en todas las llamadas, quedaría subrepresentado
generoedad <- generoedad %>% filter(genero!='Trans')
edad <- as.numeric(generoedad$edad)
generoedad$edad <- edad

#Creo intervalos de edad
intervalos <- cut(generoedad$edad, seq(0, max(generoedad$edad), by=5))
edadgenero <-table(intervalos, generoedad$genero)
edadgenero <- as.data.frame(edadgenero)

#Grafico género y edad
generos <- ggplot(edadgenero, aes(x=intervalos, Freq, fill= Var2,text = paste(
  "Género: ", Var2, "\n",
  "Intervalo de edad: ", intervalos, "\n",
  "Llamadas: ", Freq, "\n",
  sep = ""))) +
  geom_bar(stat = "identity") +
  labs(title = "Llamadas recibidas según género y edad de la víctima", fill='Género') +
  theme_classic()+
  ylab("Cantidad de llamadas") +
  xlab('Intervalo de edad')+
  scale_fill_brewer(palette="Accent",direction = -1)+
  coord_flip()

#Interacción
ggplotly(generos,tooltip = "text")

#Tipos de delito (solo trabajos los de más casos)
tipo <- c('violacion','tocamiento', "grooming", "explotación sexual", "obligación de fotografiarse", "acoso sexual")
delitos <-data.frame(tipo=tipo)
delitos$cant <- 0
delitos$cant [c(1,1)] <- nrow(linea137 %>% filter(vs_violacion_via_vaginal=='SI')) + nrow(linea137 %>% filter(vs_violacion_via_anal=='SI')) + nrow(linea137 %>% filter(vs_violacion_via_oral=='SI'))
delitos$cant [c(1,2)] <- nrow(linea137 %>% filter(vs_tocamiento_sexual=='SI'))
delitos$cant [c(1,3)] <- nrow(linea137 %>% filter(vs_grooming=='SI'))
delitos$cant [c(1,4)] <- nrow(linea137 %>% filter(vs_explotacion_sexual=='SI')) + nrow(linea137 %>% filter(vs_explotacion_sexual_comercial=='SI')) + nrow(linea137 %>% filter(vs_explotacion_sexual_viajes_turismo=='SI'))
delitos$cant [c(1,5)] <- nrow(linea137 %>% filter(vs_obligacion_sacarse_fotos_pornograficas=='SI'))
delitos$cant [c(1,6)] <- nrow(linea137 %>% filter(vs_acoso_sexual=='SI'))
delitos$cant<- as.double(delitos$cant)
as.vector(delitos)
cant <- delitos$cant
cant <- as.double(cant)
delitos2<-c('Violación = 598'=cant[1],'Tocamiento = 1061'=cant[2], "Grooming = 438"=cant[3], "Explotación sexual = 270"=cant[4], "Obligación de fotografiarse = 146"=cant[5], "Acoso sexual = 80"=cant[6]) 

#Grafico de Waffles para tipo de delito
 waffle(
   delitos2 / 20, rows = 6, size = 0.1, legend_pos = "bottom",
   colors =c("#c7d4b6", "#a3aabd", "#bdaaac", "#97cfad","#97b5cf","#a0d0de"),
   title="Llamadas según tipo de delito",
   xlab="1 cuadrado = 20 llamadas", pad=8)

#Agrupo por meses las llamadas de ambos géneros
meses <- linea137 %>% 
  group_by(month(llamado_fecha_hora)) %>%                      
  summarise(Hombres=sum(`llamante_genero`=='Masculino'),       
            Mujeres=sum(`llamante_genero`=='Femenino') 
            ) %>% 
  rename(Mes=`month(llamado_fecha_hora)`)
meses <- meses[-7,]
meses$letras<-c("Enero", "Febrero", "Marzo", "Abril", "Mayo","Junio")

#Grafico los meses
mesesg<-ggplot(meses, aes(x=Mes)) + 
  geom_line(aes(y = Hombres,size=2), color = "#97cfad") +
  geom_line(aes(y = Mujeres,size=2), color="#bdaaac") +
  geom_point(aes(y = Hombres),color='black') +
  geom_point(aes(y = Mujeres),color='black') +
  ylab('Cantidad de llamadas')+
  xlab('Meses')+
 ggtitle("Llamadas por mes según género")+
  guides(size=F)

#Cambio número de mes por nombre  
mesesg <- mesesg+
scale_x_continuous(breaks = c(1,2,3,4,5,6), labels=meses$letras)

#Interacción
ggplotly(mesesg)
```
