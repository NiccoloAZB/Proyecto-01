#Proyecto 01

##Seleccionamos el directorio donde queremos ubicar la ruta donde guardaremos todos los datos y el proyecto.

setwd("C:/Users/Admin/Desktop/UCR/Procesamiento Geo/Proyecto 01/Proyecto-01")

#Seleccionamos nuestro documento de entrada, selecionamos los separadores y el dec para no tener problemas con el documento de datos.
inp <- read.csv("liberia_datos_climaticos.csv",sep = ",", na.strings = "", dec =",")

#verificamos los datos y como en este caso hay que eliminar los datos en blanco con el comando na.omit
inp[!complete.cases(inp),]
inp3 <- na.omit(inp)
inp3[!complete.cases(inp3),]

#Cargamos las bibliotecas, que necesitamos para trabajar con los datos y en relacion con que haremos en este caso los siguintes:

# Carga de dplyr
library(dplyr)

# Carga de ggplot2
library(ggplot2)

# Carga de hrbrthemes (temas adicionales para ggplot2)
library(hrbrthemes)

#Carga de Grid.Extra
library(gridExtra)

#Convertir las variables a numeros, los datos tienen formato de caracter lo que no nos permite trabajar, entonces los vamos a cambiar a el formato que necesitamos.
inp3$Temperatura..Celsius.<-as.numeric(inp3$Temperatura..Celsius.)
inp3$HumedadRelativa....<-as.numeric(inp3$HumedadRelativa....)
inp3$VelocidadViento..m.s.<-as.numeric(inp3$VelocidadViento..m.s.)
inp3$Lluvia..mm.<-as.numeric(inp3$Lluvia..mm..)
inp3$Irradiacion..W.m2.<-as.numeric(inp3$Irradiacion..W.m2.)
inp3$EvapoTranspiracion..mm.<-as.numeric(inp3$EvapoTranspiracion..mm.)

#renombrar las variables, para trabajar de una manera mas ordenada vamos a asignarles un nombre más accesible a las variables que tengan nombres muy extensos.


inp4<-inp3 %>%
rename(Temperatura = Temperatura..Celsius.,
       Humedad = HumedadRelativa....,
       Velocidad = VelocidadViento..m.s.,
       Lluvia = Lluvia..mm.,
       Irradiacion = Irradiacion..W.m2.,
       Evapotraspiracion = EvapoTranspiracion..mm.
       )

# Primer Histograma con las 6 variables e intervalos apropiados, ejes con nombre e unidades en un solo gráfico.

#Creamos los Histogramas por aparte cada uno como un nuevo DATA con sus respectivos ejes con nombres y unidades.


Temp <- ggplot(inp4, aes(x = Temperatura)) +
  geom_histogram(
    color = "blue",
    binwidth = 1,
    fill = "gray") +
  ggtitle("Temperatura") +
  ylab("Recuento") +
 facet_grid()
    
Humd <- ggplot(inp4, aes(x = Humedad)) +
  geom_histogram(
    color = "green",
    binwidth = 1,
    fill = "gray") +
  ggtitle("Humedad Relativa") +
  ylab("mm") +
  facet_grid()

VelV <- ggplot(inp4, aes(x = Velocidad)) +
  geom_histogram(
    color = "purple",
    binwidth = 1,
    fill = "gray") +
  ggtitle("Velocidad del viento") +
  ylab("m.s") +
  facet_grid()

Irrad <- ggplot(inp4, aes(x = Irradiacion)) +
  geom_histogram(
    color = "orange",
    binwidth = 1,
    fill = "gray") +
  ggtitle("Irradiacion") +
  ylab("W.m2") +
  facet_grid()

Evapt <- ggplot(inp4, aes(x = Evapotraspiracion)) +
  geom_histogram(
    color = "red",
    binwidth = 1,
    fill = "gray") +
  ggtitle("Evotraspiracion") +
  ylab("mm") +
  facet_grid()

Llvi <- ggplot(inp4, aes(x = Lluvia)) +
  geom_histogram(
    color = "Brown",
    binwidth = 1,
    fill = "gray") +
  ggtitle("LLuvia") +
  ylab("mm") +
  facet_grid()

#Con el comando grid.arrange unimos los DATA para ver  histogramas con todas las caracteristicas.
grid.arrange(Evapt,Humd, Irrad, Temp, VelV, Llvi)





#Promedio mensual con los ejes correspondientes:
#Establecemos el formato de la fecha, luego los promedios y las sumas, para luego hacer los graficos de lineas como un DATA cada uno.
Promedio_mensual <- inp4 %>%
  select(Date,
         Humedad,
         Lluvia,
         Irradiacion,
         Velocidad,
         Evapotraspiracion,
         Temperatura) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%y")) %>%
  group_by(Date = format(Date,"%m")) %>%
  summarise(
    Humedad = mean(Humedad),
    Velocidad = mean(Velocidad),
    Lluvia = sum(Lluvia),
    Irradiacion = mean(Irradiacion),
    Evapotraspiracion = sum(Evapotraspiracion),
    Temperatura = mean(Temperatura)
  )

Pm_Temp <- ggplot(Promedio_mensual, aes(x=Date, y=Temperatura, group = 1)) +
  ggtitle("Promedio de Temperatura") +
  ylab("℃") +
  xlab("Mensual") + geom_line()

Pm_Hum <- ggplot(Promedio_mensual, aes(x=Date, y=Humedad, group = 1)) +
  ggtitle("Promedio de Humedad Relativa ") +
  ylab("Humedad") +
  xlab("Mensual") + geom_line()

Pm_Velv <- ggplot(Promedio_mensual, aes(x=Date, y=Velocidad, group = 1)) +
  ggtitle("Promedio de Velocidad del Viento ") +
  ylab("m.s") +
  xlab("Mensual") + geom_line()

Pm_Irad <- ggplot(Promedio_mensual, aes(x=Date, y=Irradiacion, group = 1)) +
  ggtitle("Promedio de Irradiacion") +
  ylab("W.m2") +
  xlab("Mensual") + geom_line()

Sm_Lluvia <- ggplot(Promedio_mensual, aes(x=Date, y=Lluvia, group = 1)) +
  ggtitle("Suma de la Lluvua") +
  ylab("mm") +
  xlab("Suma") + geom_line()

Sm_EvapT <- ggplot(Promedio_mensual, aes(x=Date, y=Evapotraspiracion, group = 1)) +
  ggtitle("Suma de la Evapotraspiracion") +
  ylab("mm") +
  xlab("Suma") + geom_line()

#Utilizamos el comando Grid.arrange para unir los DATAS y visualizar los graficos juntos.
grid.arrange(Pm_Temp,Pm_Hum,Pm_Irad,Pm_Velv, Sm_EvapT, Sm_Lluvia)


#graficos de nube de puntos
#Hacemos los graficos individuales y les asignamos un nombre para crear un DATA para cada uno con las caracteristicas que necesitamos

Gnp_EvapT <- ggplot(inp4, aes(x = Date, y = Evapotraspiracion, group = 1)) +
  geom_point(color = "red") +
  ylab("mm") +
  xlab("Evapotranspiracion")

Gnp_Lluvia <- ggplot(inp4, aes(x = Date, y = Lluvia, group = 1)) +
  geom_point(color = "Brown") +
  ylab("mm") +
  xlab("Lluvia")

Gnp_Irad <- ggplot(inp4, aes(x = Date, y = Irradiacion, group = 1)) +
  geom_point(color = "orange") +
  ylab("W.m2") +
  xlab("Irradiacion")

Gnp_Velv <- ggplot(inp4, aes(x = Date, y = Velocidad, group = 1)) +
  geom_point(color = "purple") +
  ylab("m.s") +
  xlab("Velocidad del viento")

Gnp_Hum <- ggplot(inp4, aes(x = Date, y = Humedad, group = 1)) +
  geom_point(color = "green") +
  ylab("mm") +
  xlab("Humedad Relativa")

Gnp_Temp <- ggplot(inp4, aes(x = Date, y = Temperatura, group = 1)) +
  geom_point(color = "blue") +
  ylab("℃") +
  xlab("Temperatura en ℃")

#Utilizamos el comando Grid.arrange para unir los DATAS y visualizar los graficos juntos.
grid.arrange(Gnp_Temp, Gnp_Hum, Gnp_Irad, Gnp_Velv, Gnp_EvapT, Gnp_Lluvia, nrow = 6, ncol = 1)





