#03_resumenes-graficos

#librerias
library(tidyverse)
library(broom)
library(ggplot2)
library(patchwork)
library(ggpubr)

#asociación grafica entre PM2,5 Las Encinas y sus Covariables -------------------------------------------

colnames(base_inicial)

# (a) regresión lineal logPM2,5 y logPM10 - Las Encinas (escala logaritmica)

plot_a <- ggplot(data = base_inicial, mapping = aes(x = logPM10LLEE, y = logPM25LLEE)) +
  geom_point(aes(alpha="Observed"), color="darkblue", shape=1) +
  geom_smooth(method = "lm" , color= "green", se= FALSE, aes(alpha="Fitted Values")) +
  xlab("Ln(PM10)") + ylab("Ln(PM2.5)") +
  scale_alpha_manual(name=NULL,
                     values= c(1,1),
                     breaks = c("Observed", "Fitted Values"),
                     guide=guide_legend(override.aes = list(linetype = c(0,1),
                                                            shape = c(1,NA),
                                                            color= c("darkblue","green")))) +
  theme(legend.position="bottom") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 3, p.accuracy = 0.001, r.accuracy = 0.01) + theme_minimal()


# (b) regresión lineal logPM2,5 y Temperatura - Las Encinas

plot_b <- ggplot(data = base_inicial, mapping = aes(x = TT2MLLEE, y = logPM25LLEE)) +
  geom_point(aes(alpha="Observed"), color="darkblue", shape=1) +
  geom_smooth(method = "lm" , color= "green", se= FALSE, aes(alpha="Fitted Values")) +
  xlab("Temperature (ºC)") + ylab("Ln(PM2.5)") +
  scale_alpha_manual(name=NULL,
                     values= c(1,1),
                     breaks = c("Observed", "Fitted Values"),
                     guide=guide_legend(override.aes = list(linetype = c(0,1),
                                                            shape = c(1,NA),
                                                            color= c("darkblue","green")))) +
  theme(legend.position="bottom") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 3, p.accuracy = 0.001, r.accuracy = 0.01) + theme_minimal()

# (c) regresión lineal logPM2,5 y Humedad Relativa - Las Encinas

plot_c <- ggplot(data = base_inicial, mapping = aes(x = HR2MLLEE, y = logPM25LLEE)) +
  geom_point(aes(alpha="Observed"), color="darkblue", shape=1) +
  geom_smooth(method = "lm" , color= "green", se= FALSE, aes(alpha="Fitted Values")) +
  xlab("Relative Humidity (%)") + ylab("Ln(PM2.5)") +
  scale_alpha_manual(name=NULL,
                     values= c(1,1),
                     breaks = c("Observed", "Fitted Values"),
                     guide=guide_legend(override.aes = list(linetype = c(0,1),
                                                            shape = c(1,NA),
                                                            color= c("darkblue","green")))) +
  theme(legend.position="bottom") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 3, p.accuracy = 0.001, r.accuracy = 0.01) + theme_minimal()


# (d) regresión lineal logPM2,5 y Velocidad del Viento - Las Encinas

plot_d <-ggplot(data = base_inicial, mapping = aes(x = VV10LLEE, y = logPM25LLEE)) +
  geom_point(aes(alpha="Observed"), color="darkblue", shape=1) +
  geom_smooth(method = "lm" , color= "green", se= FALSE, aes(alpha="Fitted Values")) +
  xlab("Wind Speed (Knot)") + ylab("Ln(PM2.5)") +
  scale_alpha_manual(name=NULL,
                     values= c(1,1),
                     breaks = c("Observed", "Fitted Values"),
                     guide=guide_legend(override.aes = list(linetype = c(0,1),
                                                            shape = c(1,NA),
                                                            color= c("darkblue","green")))) +
  theme(legend.position="bottom") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 3, p.accuracy = 0.001, r.accuracy = 0.01) + theme_minimal()




# (e) regresión lineal logPM2,5 y Precipitaciones  - Las Encinas

plot_e <- ggplot(data = base_inicial, mapping = aes(x = PP2MLLEE, y = logPM25LLEE)) +
  geom_point(aes(alpha="Observed"), color="darkblue", shape=1) +
  geom_smooth(method = "lm" , color= "green", se= FALSE, aes(alpha="Fitted Values")) +
  xlab("Precipitations (mm)") + ylab("Ln(PM2.5)") +
  scale_alpha_manual(name=NULL,
                     values= c(1,1),
                     breaks = c("Observed", "Fitted Values"),
                     guide=guide_legend(override.aes = list(linetype = c(0,1),
                                                            shape = c(1,NA),
                                                            color= c("darkblue","green")))) +
  theme(legend.position="bottom") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 3, p.accuracy = 0.001, r.accuracy = 0.001) + theme_minimal()



# (f) Boxplot logPM2,5 by Year - Las Encinas
plot_f <- ggplot(data=base_inicial,
       mapping = aes(x=factor(year, labels = c("2009", "2010", "2011",
                                               "2012", "2013", "2014")) ,
                     y = logPM25LLEE)) + geom_boxplot() +
  xlab("Years") + ylab("Ln(PM2.5)") + theme_minimal()



# (g) Boxplot logPM2,5 by Month of the year - Las Encinas
plot_g <- ggplot(data=base_inicial,
       mapping = aes(x=factor(month, labels = c("1", "2", "3", "4", "5", "6",
                                                "7", "8", "9", "10", "11", "12")) ,
                     y = logPM25LLEE)) + geom_boxplot() +
  xlab("Months") + ylab("Ln(PM2.5)") + theme_minimal()


# (h) Boxplot logPM2,5 by Day of the week - Las Encinas
plot_h <- ggplot(data=base_inicial,
       mapping = aes(x=factor(weekday, labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                                "Saturday")) ,
                     y = logPM25LLEE)) + geom_boxplot() +
  xlab("Day of the Week") + ylab("Ln(PM2.5)") + theme_minimal()

# (i) Boxplot logPM2,5 by Holiday - Las Encinas
plot_i <- ggplot(data=base_inicial,
       mapping = aes(x=factor(feriado, labels = c("No", "Yes")) ,
                     y = logPM25LLEE)) + geom_boxplot() +
  xlab("Holidays") + ylab("Ln(PM2.5)") + theme_minimal()

################################ Figuras ################################

library(patchwork)

fig_1 <- (plot_a + plot_b + plot_c) / (plot_d + plot_e + plot_f) / (plot_g + plot_h + plot_i)

fig_2 <- plot_d + plot_e + plot_f

fig_3 <- plot_g + plot_h + plot_i


################################ Regresiones lineales Las Encinas y Covariables ################################

# (a) regresión lineal PM2,5 y PM10 - Las Encinas
regresion_lineal_PM <- lm(logPM25LLEE ~ logPM10LLEE, data = base_inicial)
summary(regresion_lineal_PM)

# (b) regresión lineal PM2,5 y Temperatura - Las Encinas
regresion_lineal_TT <- lm(logPM25LLEE ~ TT2MLLEE, data = base_inicial)
summary(regresion_lineal_TT)

# (c) regresión lineal PM2,5 y Temperatura - Las Encinas
regresion_lineal_HR <- lm(logPM25LLEE ~ HR2MLLEE, data = base_inicial)
summary(regresion_lineal_HR)

# (d) regresión lineal PM2,5 y Velocidad del viento - Las Encinas
regresion_lineal_VV <- lm(logPM25LLEE ~ VV10LLEE, data = base_inicial)
summary(regresion_lineal_VV)

# (e) regresión lineal PM2,5 y Precipitaciones - Las Encinas
regresion_lineal_PP <- lm(logPM25LLEE ~ PP2MLLEE, data = base_inicial)
summary(regresion_lineal_PP)

# (f) ANOVA PM2,5 y Year - Las Encinas
Anova_Year <- aov(logPM25LLEE ~ factor(year), data = base_inicial)
summary(Anova_Year)

# (g) ANOVA PM2,5 y Month - Las Encinas
Anova_Month <- aov(logPM25LLEE ~ factor(month), data = base_inicial)
summary(Anova_Month)

# (h) ANOVA PM2,5 y Day of the Week - Las Encinas
Anova_weekday <- aov(logPM25LLEE ~ factor(weekday) , data = base_inicial)
summary(Anova_weekday)

# (i) ANOVA PM2,5 y Holiday - Las Encinas
Anova_holiday <- aov(logPM25LLEE ~ factor(feriado) , data = base_inicial)
summary(Anova_holiday )



