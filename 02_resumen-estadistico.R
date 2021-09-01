#02_resumen-estadistico

#cargar librerias
library(tidyverse)
library(dplyr)
install.packages("kabbleExtra")
library(kableExtra)

############## Table 3. Summary statistics for PM25 and PM10 by year and air quality station ##############

######### PM25 #########

# Resumen anual - Material Particulado 2,5 - Estación las Encinas (PM25LLEE) / Estación Museo Ferroviario (PM25MMFF)

resumen_PM25LLEE <- base_inicial %>% #base de datos
  dplyr::group_by(year)%>% #agrupamos por año (2009:2014)
  dplyr::summarise(mean_PM25LLEE = mean(PM25LLEE , na.rm = TRUE),#cálculo de la media PM25LLEE
            sd_PM25LLEE = sd(PM25LLEE, na.rm = TRUE), #cálculo de la desviación standard PM25LLEE
            mean_PM25MMFF = mean(PM25MMFF, na.rm = TRUE), #cálculo de la media PM25MMFF
            sd_PM25MMFF = sd(PM25MMFF, na.rm = TRUE))   #cálculo de la desviación standard PM25MMFF
          

#  Resumen para Las Encinas PM25
LLEE_PM25 <-  base_inicial %>% #base de datos
  dplyr::group_by(year)%>% #agrupamos por año (2009:2014)
  dplyr::summarise(Mean = round(mean(PM25LLEE , na.rm = TRUE), digits = 1),#cálculo de la media PM25LLEE
                   sd = round(sd(PM25LLEE, na.rm = TRUE), digits = 1)) #cálculo de la desviación standard PM25MMFF

# Resumen para Museo Ferroviario PM25
MMFF_PM25 <- base_inicial %>% #base de datos
  dplyr::group_by(year)%>% #agrupamos por año (2009:2014)
  dplyr::summarise(Mean = round(mean(PM25MMFF, na.rm = TRUE), digits = 1), #cálculo de la media PM25MMFF
                   sd = round(sd(PM25MMFF, na.rm = TRUE), digits = 1)) 

#  Resumen para Las Encinas PM10
LLEE_PM10 <-  base_inicial %>% #base de datos
  dplyr::group_by(year)%>% #agrupamos por año (2009:2014)
  dplyr::summarise(Mean = round(mean(PM10LLEE , na.rm = TRUE), digits = 1),#cálculo de la media PM25LLEE
                   sd = round(sd(PM10LLEE, na.rm = TRUE), digits = 1)) #cálculo de la desviación standard PM25MMFF

# Resumen para Museo Ferroviario PM10
MMFF_PM10 <- base_inicial %>% #base de datos
  dplyr::group_by(year)%>% #agrupamos por año (2009:2014)
  dplyr::summarise(Mean = round(mean(PM10MMFF, na.rm = TRUE), digits = 1), #cálculo de la media PM25MMFF
                   sd = round(sd(PM10MMFF, na.rm = TRUE), digits = 1)) 

PM25_table <- inner_join(LLEE_PM25, MMFF_PM25 ,by = "year") #generamos una tabla para pm25

PM10_table <- inner_join(LLEE_PM10, MMFF_PM10, by = "year") #generamos una tabla para pm10

inner_join(PM25_table, PM10_table, by ="year")

######## Tabla 3. Summary statistics 

# Unimos ambas TABLAS usando inner_join y kable
inner_join(PM25_table, PM10_table, by ="year")%>% 
  kbl(caption = "**Table.3** \n Sumary statistics for   ", col.names = c("Year", "Mean", "SD", "Mean", "SD", "Mean", "SD", "Mean", "SD")) %>% #cambiamos nombre de las columnas
  kable_classic(full_width = F, html = "Cambria") %>% #damos formato a la tabla
  add_header_above(c(" " = 1, "Las Encinas" = 2, "Museo Ferroviario" = 2, "Las Encinas" = 2, "Museo Ferroviario " = 2)) %>% 
  kableExtra::add_header_above(c("$PM_{2.5}$" = 5, "$PM_{10}$" = 4), escape = FALSE) #










# Resumen periodo - Material Particulado 2,5
base_inicial %>% summarise(
  mean_PM25LLEE_period = round(mean(PM25LLEE , na.rm = TRUE), digits = 1),
  sd_PM25LLEE_period = round(sd(PM25LLEE, na.rm = TRUE), digits = 1),
  mean_PM25MMFF_period = round(mean(PM25MMFF, na.rm = TRUE), digits = 1),
  sd_PM25MMFF_period = round(sd(PM25MMFF, na.rm = TRUE), digits = 1))

######### PM10 #########

# Resumen anual - Material Particulado 10 - Estación las Encinas (PM25LLEE) / Estación Museo Ferroviario (PM25MMFF)

base_inicial %>% #base de datos
  group_by(year)%>% #agrupamos por año (2009:2014)
  summarise(mean_PM10LLEE = mean(PM10LLEE , na.rm = TRUE),#calculo de la media PM10LLEE
            sd_PM10LLEE = sd(PM10LLEE, na.rm = TRUE), #calculo de la desviación estandard PM10LLEE
            mean_PM10MMFF = mean(PM10MMFF, na.rm = TRUE), #calculo de la media PM10MMFF
            sd_PM10MMFF = sd(PM10MMFF, na.rm = TRUE)) #calculo de la desviación estandard PM10MMFF

# Resumen periodo - Material Particulado 10
base_inicial %>% summarise(
  mean_PM10LLEE_period = round(mean(PM10LLEE , na.rm = TRUE), digits = 1),
  sd_PM10LLEE_period = round(sd(PM10LLEE, na.rm = TRUE), digits = 1),
  mean_PM10MMFF_period = round(mean(PM10MMFF, na.rm = TRUE), digits = 1),
  sd_PM10MMFF_period = round(sd(PM10MMFF, na.rm = TRUE), digits = 1))
