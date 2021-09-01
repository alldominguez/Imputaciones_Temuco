#04_construcción modelos estadisticos

#librerias 
library(tidyverse)
library(patchwork)
library(broom)
library(performance)

#####Modelo 1##### 

m1 <- lm(formula = logPM25LLEE ~ logPM10LLEE + TT2MLLEE + VV10LLEE + HR2MLLEE + PP2MLLEE + factor(year) + factor(month) +
           factor(weekday) + factor(feriado), data = base_inicial)
summary(m1)

#usamos el toolkit de broom para ordenar el modelo 1
broom::tidy(m1) 
broom::glance(m1)
broom::augment(m1)

#revisión de supuestos 
performance::check_model(m1)


#podemos ver como se ajustan nuestros datos a los datos predecidos por el modelo

augment(m1) %>% 
  ggplot(mapping = aes(x = logPM10LLEE)) + 
  geom_point(mapping=aes(y= logPM25LLEE)) + 
  geom_line(mapping =aes (y=.fitted), color  ="red") + theme_light() 


#####Modelo 2##### 

#agregar PM10MMFF y PM25MMFF

m2 <- lm(formula = logPM25LLEE ~ logPM10LLEE + TT2MLLEE + VV10LLEE + HR2MLLEE + PP2MLLEE + factor(year) + factor(month) +
           factor(weekday) + factor(feriado) + logPM10MMFF + logPM25MMFF, data = base_inicial)
summary(m2)

#usamos el toolkit de broom para ordenar el modelo 2
broom::tidy(m2) 
broom::glance(m2)
broom::augment(m2)

#revisión de supuestos 
performance::check_model(m2)

#comparación de modelos 

comp <- performance::compare_performance(m1, m2)

install.packages("see")
library(see)
plot(performance::compare_performance(m2, m1))









