#00_limpieza-base-de-datos

getwd() #obtener si el directorio de trabajo es el correcto

#librerias
library(tidyverse)
library(haven)
library(readr)
library(ggplot2)
library(dplyr)

########################################################## cargar base de datos ##########################################################
base_inicial <- read.csv("00_Bases-de-datos/base ini paper multiple imputation 181011.csv") #base correcta, coinciden estadísticas descriptivas

base_inicial <- read.csv("~/Desktop/Github_Proyectos/Multiple_Imputation-Temuco/00_Bases-de-datos/base_inicial.csv")



#revisar estas bases de datos y encontrar los datos de PM de Museo Ferroviario


#Base de datos con las imputaciones realizadas (base con resultados)
db_1 <- read_dta("~/Desktop/Github_Proyectos/Multiple_Imputation-Temuco/00_Base-de-datos/BASE INI.dta")


db_1 <- read_dta("~/Desktop/Proyecto - Multiple Imputation/Proyecto/Bases de Datos/BASE INI.dta")
#Base de datos con la que vamos a trabajar las imputaciones mediante los diferentes metodos de imputacion
db_desafio<- read_dta("~/Desktop/Proyecto - Multiple Imputation/Proyecto/Bases de Datos/BASE.DESAFIO.dta")

db_3<- read_dta("~/Desktop/Proyecto - Multiple Imputation/Proyecto/Bases de Datos/m0.dta")


#Análisis exploratorio de la base de datos
summary(base_inicial)
colnames(base_inicial) #revisamos los nombres de las columnas de la base de datos

#grafico serie de tiempo PM10LLEE
ggplot(data=base_inicial, aes(x = fecha, y = PM10LLEE)) + #revisamos las variables de interes, MP10, MP2,5
  geom_line() +
  scale_y_log10() + ylab("Ln(PM10LLEE)")  #realizamos una transformación logaritmica, debido a que los datos tienen muchas dispersión

ggplot(data = base_inicial , aes(x = fecha , y = PM10LLEE)) + # esta linea es lo mismo que le de arriba, solo tiene los datos cambiados
  geom_line() +
  scale_y_log10()


#grafico serie de tiempo PM25LLEE
ggplot(data=base_inicial, aes(x = fecha, y = PM25LLEE)) + #revisamos las variables de interes, MP10, MP2,5
  geom_line() +
  scale_y_log10() + ylab("Ln(PM25LLEE)")

########################################################## limpieza de los datos ##########################################################

#primero revisamos el tipo de datos
glimpse(base_inicial)

# para cada una de las variables se debe definir de manera correcta el tipo de dato (esto nos puede generar problemas a futuro y comprometer nuestro análisis)
class(base_inicial$fecha) # la fecha se encuentra como character, es necesario cambiarla????

########################################################## Cambio formato de Fechas  #############################################
library(lubridate)

?strptime #nos permite ver todos los formatos de fecha

#primera forma de cambiar el formato de fecha
base_inicial <- base_inicial %>%
  mutate(Fecha = make_date( year, month, day)) #creamos una nueva columna date, con los datos de Fecha (COMBINANDO), solo si tenemos Año, Mes y Dia en columnas separadas

class(base_inicial$Fecha) #comprobamos el formato

#tambien se puede cambiar a partir de la columna fecha
base_inicial <- base_inicial %>%
  mutate(fecha = dmy(fecha))

base_inicial$fecha <- format(base_inicial$fecha, format = "%d-%m-%Y") #dejamos el formato de fecha igual que en el SINCA, pero queda como CHARACTER

class(base_inicial$fecha)

#cada vez que trabajamos con fechas, es importante revisar si existen fechas futuras, lo que indica que los datos estan mal
install.packages("assertive")
library(assertive)
assert_all_are_in_past(base_inicial$fecha) #revisa si todas las fechas tienen congruencia, necesita que este en formato de fecha as.date()


##### revision de otras bases de datos 


db4 <- read_sas("~/Desktop/Proyecto - Multiple Imputation/Analisis TEMUCO/bases/temucomulti.sas7bdat")

db5 <- db4 %>% dplyr::filter(year %in% c(2009,2010,2011,2012,2013.2014))

db5 %>% dplyr::select(PM25LLEE, PM10LLEE, PM25MMFF, PM10MMFF, year) %>% 
  skimr::skim()


temuco <- read_sas("~/Desktop/Proyecto - Multiple Imputation/Analisis TEMUCO/Preparar BaseDatos.sas")
