# 05_imputaciones 
base_inicial <- read.csv("~/Desktop/Github_Proyectos/Multiple_Imputation-Temuco/00_Bases-de-datos/base_inicial.csv")
#### mean imputation --------------
library(tidyverse)
library(VIM)
library(naniar)
library(mice)
library(broom)

naniar::mcar_test() #diagnostico de MCAR

# forma basica
base_inicial <- base_inicial %>% 
  mutate(replace_PM25_mean = replace_na(logPM25LLEE, mean(logPM25LLEE, na.rm = T)))

# otra forma si nos interesa etiquetar variables

# creamos un indicador binario para ver si el valor esta perdido originalmente

base_inicial <- base_inicial %>% 
  mutate(logPM25LLEE_imp = ifelse(is.na(logPM25LLEE), TRUE, FALSE)) %>% 
  mutate(logPM10LLEE_imp = ifelse(is.na(logPM10LLEE), TRUE, FALSE))

#reemplazamos los valores perdidos con la media 

base_inicial <- base_inicial %>% 
  mutate(logPM25LLEE = ifelse(is.na(logPM25LLEE), mean(logPM25LLEE, na.rm = TRUE), logPM25LLEE)) %>% 
  mutate(logPM10LLEE = ifelse(is.na(logPM10LLEE), mean(logPM10LLEE, na.rm = TRUE), logPM10LLEE))

# revisamos las imputaciones 
base_inicial_imp %>% 
  select(logPM25LLEE,logPM25LLEE_imp, logPM10LLEE, logPM10LLEE_imp)

# si queremos visluaizar la calidad de nuestra imputacion, podemos graficar los datos imputados vs las observaciones originales

base_inicial %>%  dplyr::select(logPM25LLEE, logPM25LLEE_imp, logPM10LLEE, logPM10LLEE_imp ) %>% 
                      VIM::marginplot(delimiter = "imp")


#### knn imputation --------------
library(VIM)

# 1.- encuentra k observaciones (donantes, vecinos) que so nlo mas similar o cercanos a la observacion perdida

# 2.- reemplaza los valores perdidos con valores agregados de los k vecinos elegidos (utilziando media, mediana, moda). Para elegir los vecinos
# mas cercanos utiliza medidas de distancia, por ejemplo la euclediana (numericos), la  manhattan (factores) , la distancia de hamming (categoricas)
# luego se combinan en una medida agregada llamada la distancia de grower

base_inicial_knn_imp <- VIM::kNN(base_inicial, k = 10, variable = "logPM25LLEE")
head(base_inicial_knn_imp)

#agregando peso a los vecinos más cercanos 

VIM::kNN(base_inicial, k =10, variable ="logPM25LLEE",
                              numFun = weighted.mean, #agrega variables numericas como "ponderado"
                              weightDist =  TRUE) #usar las distancias como peso


#### model based imputation --------------
library(simputation)
require(hotdeck)

#linear models

base_model_imp <- simputation::impute_lm(base_inicial, logPM25LLEE ~ logPM10LLEE + TT2MLLEE + VV10LLEE + HR2MLLEE + PP2MLLEE + year + month +
                       weekday + feriado + logPM10MMFF + logPM25MMFF)


base_model_imp <- hotdeck(base_inicial) # generamos etiqueta

missing_logPM25LLEE <- base_model_imp$logPM25LLEE_imp #guardamos los indicadores 

# generamos el loop para iterar 
for (i in 1:5) {
  base_model_imp$logPM25LLEE_imp[missing_logPM25LLEE] <- NA
  base_model_imp <- impute_lm(base_inicial, logPM25LLEE ~ logPM10LLEE + TT2MLLEE + VV10LLEE + HR2MLLEE + PP2MLLEE + year + month +
                           weekday + feriado + logPM10MMFF + logPM25MMFF)
}


# para ver las diferencias entre itereaciones podemos: 

diff_logPM25LLEE <- c() #creamos un vector vacio para guardar las diferencias

# creamos una funcion que nos permita ver el cambio absoluto medio de cambio
mapc <- function(a, b) {
  mean(abs(b - a) / a, na.rm = TRUE)
}

for (i in 1:5) {
  prev_iter <- base_model_imp #al comienzo de cada iteracion, copiamos los datos imputados del anterior
  base_model_imp$logPM25LLEE_imp[missing_logPM25LLEE] <- NA
  base_model_imp <- simputation::impute_lm(base_inicial, logPM25LLEE ~ logPM10LLEE + TT2MLLEE + VV10LLEE + HR2MLLEE + PP2MLLEE + 
                                           year + month + weekday + feriado + logPM10MMFF + logPM25MMFF)
  diff_logPM25LLEE < c(diff_logPM25LLEE, mapc(prev_iter$logPM25LLEE,  base_model_imp$logPM25LLEE))
}

#### Multiple imputation by chained equations  --------------

base_inicial_mice <- base_inicial %>% 
  dplyr::select(logPM25LLEE, logPM10LLEE, TT2MLLEE, VV10LLEE, HR2MLLEE, PP2MLLEE, 
                  year, month, weekday, feriado, logPM10MMFF, logPM25MMFF)

library(mice) 
library(mitools) 
install.packages("miceadds")
library(miceadds)

#### 1. Establecemos las variables categoricas a factores

# Seteamos a factor todas las variables que vamos a tratar como categoricas (y tienen >2 categorias)

## year ---
base_inicial_mice$year <- as.factor(base_inicial_mice$year) 

#corroboramos niveles
levels(base_inicial_mice$year)

## month ---
base_inicial_mice$month <- as.factor(base_inicial_mice$month) 

#corroboramos niveles month
levels(base_inicial_mice$month)

## weekday ---
base_inicial_mice$weekday <- as.factor(base_inicial_mice$weekday) 

#corroboramos niveles weekday
levels(base_inicial_mice$weekday)

## feriado

base_inicial_mice$feriado <- as.factor(base_inicial_mice$feriado) 

#corroboramos niveles feriado
levels(base_inicial_mice$feriado)

#### 2. Do a dry run of the model

base_inicial_multimp <- mice(base_inicial_mice, maxit= 0) # hacemos un simulacro seteando maxit = 0 

#### 3. Check and Change
base_inicial_multimp$pred

# no queremos imputaciones para year, month, weekday, feriado, o variables meteorologicas 

pred <- base_inicial_multimp$pred # copiamos en una matriz llamada pred 

# para las variables que no quiero que sean predictoras de ningun modelo asigno 0 a las filas correspondientes 

pred["year",] <- 0
pred["month",] <- 0
pred["weekday",] <- 0
pred["feriado",] <- 0
pred["TT2MLLEE",] <- 0
pred["VV10LLEE",] <- 0
pred["HR2MLLEE",] <- 0
pred["PP2MLLEE",] <- 0
pred["logPM10MMFF",] <- 0
pred["logPM25MMFF",] <- 0
pred["logPM10LLEE",] <- 0

# Check model used
base_inicial_multimp$method

#si queremos cambiar el modelo 
method <- base_inicial_multimp$method

method["logPM25LLEE"] <- "norm" # cambiamos el modelo a regresion


#### 4. Imputamos con las nuevas especificaciones 
base_inicial_multimp <- mice(base_inicial_mice, pred = pred, method = method,
                             maxit = 10, m = 10, seed = 84329)
#### 5. Resultados

# extract the original dataset (with missing values)
orig <- complete(base_inicial_multimp ,0)
summary(orig)
# extract the first imputed dataset
imp1 <- complete(base_inicial_multimp ,1)
summary(imp1)
# extract the 6th imputed dataset
imp6 <- complete(base_inicial_multimp ,6)
summary(imp6)


#### Calculamos las medias 

# calculamos la media de la base original 
mean(orig$logPM25LLEE, na.rm = TRUE)

# calculamos la media de la primera base imputada
mean(imp1$logPM25LLEE, na.rm = TRUE)

# calculate the mean of a variable and pool the results # of all imputed datasets
res <- with(base_inicial_multimp , mean(logPM25LLEE))

# pool estimates
miceadds::withPool_MI(res)

complete<-complete(base_inicial_multimp,"long")
mean(complete_imp1$logPM25LLEE, na.rm = TRUE)


### modelos --------

# modelo in the original dataset 
mod_orig <- lm (logPM25LLEE ~ logPM10LLEE + TT2MLLEE + VV10LLEE + HR2MLLEE + PP2MLLEE + factor(year) + factor(month) +
                 factor(weekday) + factor(feriado) + logPM10MMFF + logPM25MMFF, data = base_inicial)
summary(mod_orig)

#model in the firs imputed dataset 
mod_imp1 <- lm (logPM25LLEE ~ logPM10LLEE + TT2MLLEE + VV10LLEE + HR2MLLEE + PP2MLLEE + factor(year) + factor(month) +
                  factor(weekday) + factor(feriado) + logPM10MMFF + logPM25MMFF, data = imp1)
summary(mod_imp1)

#estimate in each imputed dataset and pool results 
mod_imp <- with(base_inicial_multimp, lm (logPM25LLEE ~ logPM10LLEE + TT2MLLEE + VV10LLEE + HR2MLLEE + PP2MLLEE + factor(year) + factor(month) +
                                            factor(weekday) + factor(feriado) + logPM10MMFF + logPM25MMFF))

summary(pool(mod_imp))


#grafico para ver la calidad de los datos 

mice::stripplot(base_inicial_multimp, 
                logPM25LLEE ~ logPM10LLEE | .imp,
                pch = 20, cex =2)
