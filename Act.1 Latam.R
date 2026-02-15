#Actividad 1
#Panel de datos

#Libreria
library(tidyverse)
library(ggplot2)
library(tidyverse)
library(ggplot2)
library(plm)
library(lmtest)
library(sandwich)

#Cargar base de datos
panel_edu
#Calculamos numero de paises
panel_edu %>% 
  summarise(total_paises=n_distinct(pais))#ni_distonc, cuenta los distintos de pais
panel_edu %>% 
  distinct(pais)

print(panel_edu,n=3854)

#Filtrar paises
Panel_edu_filtrado <- panel_edu %>% 
  filter(pais %in% c("Mexico", "Argentina", "Brasil", "Chile", "Ecuador", "Peru",
                     "Paraguay", "Panama", "Uruguay", "Cuba", "Costarica", "Colombia"))
Panel_edu_filtrado

# Calcular medias generales por país
Panel_edu_filtrado %>%
  group_by(pais) %>%
  summarise(media_edu = mean(edu, na.rm = TRUE),
            media_gdp_edu = mean(gdp_edu, na.rm = TRUE)) %>%
  print(n = Inf)


# Crear gráfico de líneas Pate 2 Grafique los años de escolaridad promedio de todos los países(variable edu) para el periodo de 1970 a 2015 y comente la gráfica. 
ggplot(data = Panel_edu_filtrado, aes(x = periodo, y = edu, color = pais, group = pais)) +
  geom_line() +
  geom_point() +
  labs(title = "Años de Escolaridad por País (1970-2015)",
       x = "Año",
       y = "Años de Escolaridad",
       color = "País") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Media de años de escolaridad por año (todos los países)
Panel_edu_filtrado %>%
  group_by(periodo) %>%
  summarise(media_edu_anual = mean(edu, na.rm = TRUE)) %>%
  print(n = Inf)


# 3. Grafique los años de escolaridad promedio para cada uno de los países (variable edu) parael periodo de 1970 a 2015 y comente la gráfica.
#grafico
Panel_edu_filtrado %>% 
  ggplot(aes(x=periodo,y=(edu)))+
  geom_line()+#es para pedirlo en lineas
  facet_wrap(~pais,scale="free_y")+#esto es para hacerlos por separado
  theme_dark()

# Media de escolaridad por país para el periodo
Panel_edu_filtrado %>%
  group_by(pais) %>%
  summarise(media_edu_total = mean(edu, na.rm = TRUE)) %>%
  print(n = Inf)


# 4 Gráfico del gasto público promedio en educación (gdp_edu) de todos los países
ggplot(data = Panel_edu_filtrado, aes(x = periodo, y = gdp_edu)) +
  stat_summary(fun = mean, geom = "line", color = "purple") +
  labs(title = "Gasto Público Promedio Edu % del PIB - Latinoamerica (1970-2015)",
       x = "Año",
       y = "Gasto Público Edu (% del PIB)") +
  theme_bw()

# Media anual de gasto público en educación (todos los países)
Panel_edu_filtrado %>%
  group_by(periodo) %>%
  summarise(media_gdp_edu_anual = mean(gdp_edu, na.rm = TRUE)) %>%
  print(n = Inf)


#5 Gráfico de gasto público en educación (% del PIB) para cada país
Panel_edu_filtrado %>% 
  ggplot(aes(x = periodo, y = gdp_edu)) +
  geom_line(color = "red") +
  facet_wrap(~pais, scales = "free_y") +
  labs(title = "Gasto Público en Educación % del PIB por País (1970-2015)",
       x = "Año",
       y = "Gasto Público en Educación % del PIB") +
  theme_bw()

# Media del gasto público en educación por país
Panel_edu_filtrado %>%
  group_by(pais) %>%
  summarise(media_gdp_edu_total = mean(gdp_edu, na.rm = TRUE)) %>%
  print(n = Inf)


#  6. Estime el siguiente modelo de panel de datos:eduit = αi + αigdp_eduit + εit

# a) Estimación con Pooling vs Efectos Fijos
# Pooling
modelo_pooling <- plm(edu ~ gdp_edu,
                      data = Panel_edu_filtrado,
                      model = "pooling",
                      index = c("pais", "periodo"))
summary(modelo_pooling)

# Efectos fijos (within)
modelo_fijos <- plm(edu ~ gdp_edu,
                    data = Panel_edu_filtrado,
                    model = "within",
                    index = c("pais", "periodo"))
summary(modelo_fijos)

# Prueba F de efectos fijos vs pooling
pFtest(modelo_fijos, modelo_pooling)

# b) Efectos Fijos vs Efectos Aleatorios
# Efectos aleatorios
modelo_aleatorios <- plm(edu ~ gdp_edu,
                         data = Panel_edu_filtrado,
                         model = "random",
                         index = c("pais", "periodo"))
summary(modelo_aleatorios)

# Prueba de Hausman
phtest(modelo_fijos, modelo_aleatorios)

# c) Pruebas de Autocorrelación y Heterocedasticidad
# Prueba de heterocedasticidad (Breusch-Pagan)
bptest(modelo_fijos)

# Prueba de autocorrelación (Breusch-Godfrey)
bgtest(modelo_fijos)

# d) Corrección del modelo si hay problemas
# Varianzas robustas tipo Arellano (HAC para panel)
vcov_robust <- vcovHC(modelo_fijos, method = "arellano", type = "HC1", cluster = "group")

# Reestimación con errores estándar robustos
coeftest(modelo_fijos, vcov. = vcov_robust)

# Medias generales (toda la base filtrada)
Panel_edu_filtrado %>%
  summarise(media_edu_total = mean(edu, na.rm = TRUE),
            media_gdp_edu_total = mean(gdp_edu, na.rm = TRUE))


