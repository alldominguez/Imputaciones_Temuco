#01_perdida-de-datos

#Paquetes estadisticos
install.packages("dlookr")
install.packages("naniar")
install.packages("skimr")

library(dlookr) #para diagnostico de missing data
library(tidyverse)
library(dplyr)
library(naniar) #para visualizar missing data (uno de los mejores)
library(skimr)
library(VIM)
library(mice) #paquete libro Stef Van Buuren
library(visdat)

############################

base_inicial %>%
  count(year) %>%
  colSums()

short_base <-  base_inicial %>%
  dplyr::select(PM25LLEE, PM10LLEE, PM25MMFF, PM10MMFF)

#tabla-1 -

#todos los contaminantes PM25, PM10 (2009:2014
base_inicial %>%
  group_by(year) %>%
  summarise(comp_PM25LLEE= round(mean(complete.cases(PM25LLEE)), digits = 2),
            comp_PM25MMFF= round(mean(complete.cases(PM25MMFF)), digits = 2),
            comp_PM10LLEE= round(mean(complete.cases(PM10LLEE)), digits = 2),
            comp_PM10MMFF= round(mean(complete.cases(PM10MMFF)), digits = 2),
            comp_NNOXLLEE= round(mean(complete.cases(NNO2LLEE)), digits = 2))


#periodo total tabla-1
base_inicial %>%
  summarise(comp_PM25LLEE = round(mean(complete.cases(PM25LLEE)), digits = 2),
            comp_PM25MMFF = round(mean(complete.cases(PM25MMFF)), digits = 2),
            comp_PM10LLEE = round(mean(complete.cases(PM10LLEE)), digits = 2),
            comp_PM10MMFF = round(mean(complete.cases(PM10MMFF)), digits =2))


#tabla-2 - Miising data patterns for las Encinas, Museo Ferroviario y Manquehue.

par(mar=c(0,0,0,0)) #seteamos los margenes del grafico

base_inicial %>%
  dplyr::select(PM25LLEE, PM10LLEE, TT2MLLEE, HR2MLLEE, VV10LLEE, PPSILLEE) %>%
  mice::md.pattern(rotate.names = TRUE)


################ algunas pruebas para la presentación #############

# primera aproximación a mi base de datos con los datos perdidos que tengo

base_inicial %>%
  dplyr::select(PM25LLEE, PM10LLEE, PM25MMFF, PM10MMFF) %>%
  is.na() %>%
  colSums()

## visualización del tipo de mecanismo de perdida de datos

base_inicial %>%
  dplyr::select(PM25LLEE, PM10LLEE, PM25MMFF, PM10MMFF) %>%
  visdat::vis_miss(cluster= TRUE)

## visualización

base_inicial %>%
  dplyr::select(PM25LLEE, PM10LLEE, PM25MMFF, PM10MMFF, year) %>%
  naniar::gg_miss_var(facet = year )


base_inicial %>%
  dplyr::select(PM25LLEE, PM10LLEE, PM25MMFF, PM10MMFF, year) %>%
  naniar::gg_miss_fct(fct = year)


### algunos comandos utiles

base_inicial %>%
  dplyr::select(PM25LLEE, PM10LLEE, PM25MMFF, PM10MMFF, year) %>% group_by(year) %>%
  group_by(year) %>%
  naniar::miss_var_summary()


base_inicial %>%
  dplyr::select(PM25LLEE, PM10LLEE, PM25MMFF, PM10MMFF, year) %>%
  naniar::miss_var_table()

base_inicial %>%
  dplyr::select(PM25LLEE, PM10LLEE, PM25MMFF, PM10MMFF, year) %>%
  count(PM25LLEE)

#### super util resumen anual de

base_inicial %>%
  dplyr::select(PM25LLEE, PM10LLEE, PM25MMFF, PM10MMFF, year) %>%
  dplyr::group_by(year) %>%
  skimr::skim()

#grafico de series de tiempo

base_inicial %>%
  dplyr::select(PM25LLEE, Fecha, year) %>%
  ggplot2::ggplot(mapping= aes( x= Fecha,
                                y= PM25LLEE)) + geom_point() + theme_light() +
  naniar::geom_miss_point()


# buscando NA ocultos

base_inicial %>%
  dplyr::select(PM25LLEE, PM10LLEE, PM25MMFF, PM10MMFF)  %>%
  naniar::miss_scan_count(search= list("n/a","na", "N/A"))

#para reemplazar esos NA ocultos
base_inicial %>%
  dplyr::select(PM25LLEE, PM10LLEE, PM25MMFF, PM10MMFF)  %>%
  naniar::replace_with_na(replace = list(PM25MMFF = c("n/a","na", "N/A")))


# shadow matrix and nabular data

base_inicial %>%
  dplyr::select(PM25LLEE, PM10LLEE, PM25MMFF, PM10MMFF)  %>%
  naniar::bind_shadow()


# imputacion de medias
naniar::impute_mean(base_inicial$PM25LLEE) %>% # aumenta la media artificialmente y reduce la varianza
  mean()

#imputaciones multiples

library(mice)

mice(short_base, m=5)










