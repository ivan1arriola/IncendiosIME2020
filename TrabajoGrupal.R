#### Informacion adjunta en el dataset  ####

### X - x-axis spatial coordinate within the Montesinho park map: 1 to 9
### Y - y-axis spatial coordinate within the Montesinho park map: 2 to 9
### month - month of the year: "jan" to "dec" 
### day - day of the week: "mon" to "sun"
### FFMC - FFMC index from the FWI system: 18.7 to 96.20
### DMC - DMC index from the FWI system: 1.1 to 291.3 
### DC - DC index from the FWI system: 7.9 to 860.6 
### ISI - ISI index from the FWI system: 0.0 to 56.10
### temp - temperature in Celsius degrees: 2.2 to 33.30
### RH - relative humidity in %: 15.0 to 100
### wind - wind speed in km/h: 0.40 to 9.40 
###. rain - outside rain in mm/m2 : 0.0 to 6.4 
###. area - the burned area of the forest (in ha): 0.00 to 1090.84

#### Definiciones respecto a las variables ####

# El Código de Humedad de Combustible Fino (FFMC) 
# representa la humedad de combustible de los combustibles 
# de la basura forestal bajo la sombra de un dosel forestal. 
# Está destinado a representar las condiciones de humedad para 
# los combustibles de arena sombreada, el equivalente a un 
# lapso de tiempo de 16 horas. Va de 0 a 101. 
# Restar el valor de FFMC de 100 puede proporcionar una 
# estimación del contenido de humedad del combustible equivalente 
# (aproximadamente 10 h), más precisa cuando los valores de 
# FFMC están aproximadamente por encima de 80.

# El Código de Humedad de Duff (DMC) representa la 
# humedad del combustible de material orgánico descompuesto 
# debajo de la cama. Los diseñadores del sistema sugieren 
# que representa las condiciones de humedad para el equivalente 
# a combustibles de intervalo de tiempo de 15 días (o 360 horas). 
# No tiene unidad y tiene final abierto. Puede proporcionar 
# información sobre el estrés por humedad del combustible en vivo.

# El Código de Sequía (DC) , al igual que el Índice de Sequía 
# de Keetch-Byrum, representa un secado profundo en el suelo. 
# Se aproxima a las condiciones de humedad para el equivalente 
# a combustibles con retraso de 53 días (1272 horas). 
# No tiene unidades, con un valor máximo de 1000. 
# Las condiciones de sequía extrema han producido valores de CC cercanos a 800.

# El índice de propagación inicial (ISI) es análogo al componente de propagación 
# (SC) de NFDRS. Integra la humedad del combustible para los combustibles muertos 
# finos y la velocidad del viento en la superficie para 
# estimar un potencial de propagación. ISI es una entrada clave 
# para las predicciones del comportamiento del fuego en el sistema FBP. 
# No tiene unidad y tiene final abierto


#### Limpia el ambiente de trabajo      ####
rm(list = ls())

#### Librerias                          ####
library(readr)
library(tidyverse)
library(reshape)

#### Importacion de los datos           ####
incendios <- read_csv('http://www.dsi.uminho.pt/~pcortez/forestfires/forestfires.csv')

#### Para observar los datos            ####
head(incendios) 
tail(incendios)
#View(incendios)

####### Estructura de dataset incendios    ########

glimpse(incendios)

#### Etiquetas para month y day         ####

monthlevels <- c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug',
                 'sep', 'oct', 'nov', 'dec')

monthlabels <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo','Junio', 'Julio',
                 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')

daylevels <- c('mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun')

daylabels <- c('Lunes', 'Martes', 'Miercoles', 'Jueves', 'Viernes', 'Sabado', 'Domingo')

#### Coercionar caracteres a factor     ####

incendios <- incendios %>% mutate ( X = as.factor(X),
                                    Y = as.factor(Y),
                                    month = factor(month, levels = monthlevels,
                                                   labels = monthlabels),
                                    day = factor(day, levels = daylevels,
                                                 labels = daylabels)
                                   )

rm(list = c('daylabels', 'daylevels', 'monthlabels', 'monthlevels'))

#### Contar NA en incendios             ####

apply(X = is.na(incendios), MARGIN = 2, FUN = sum) 
# no hay datos faltantes #

####### Analisis Exploratorio Univariado   ########




####### Analisis Exploratorio Multivariado ########

##### Frecuencia absoluta de incendios por mes por dia de la semana ###
### Creo un dataframe con las variables mes, dia y cantidad de incendios ###

mesxdia <- incendios %>% {table(.$month, .$day)} %>% melt()

colnames(mesxdia) <- c('mes', 'dia', 'cant')

#### Mapa de calor del dataframe mesxdia   ####

ggplot(data = mesxdia, aes(x=mes, 
                           y=dia, 
                           fill=cant)) + 
  geom_tile(color='black') +
  scale_fill_gradient(low = "white", high = "red") +
  guides(fill = guide_colourbar(title = "Cant. de Incendios"))

# el grafico parece indicar que la cantidad de incendios varia mucho mas por
# el mes del anio que por el dia de la semana. Tambien los incendios por dia
# de la semana varian de igual forma a traves de los meses del anio

#### Mapa de los incendios                ####

## ya que X e Y son cordenadas espaciales, hare un mapa para visualizar donde se han
## concentrado los incendios

## Hago la frecuencia absoluta de la caantidad de incendios por ubicacion
## y los pongo en un data frame, cada fila es una ubicacion
mapa <- incendios %>% {table(.$X, .$Y)} %>% melt()

## la columnas son la coordenada en X, la coordenada en Y y la cantidad de incendios
colnames(mapa) <- c('coordX', 'coordY', 'cant')

## corrijo los tipos de datos del data frame
mapa <- mapa %>% mutate(coordX = as.factor(coordX),
                coordY = as.factor(coordY),
                cant= as.integer(cant))

## grafico
ggplot(data=mapa, aes(x=coordX, y=coordY, fill=cant)) +  
  geom_tile(color='black') +
  scale_fill_gradient(low = "black", high = "red") +
  guides(fill = guide_colourbar(title = "Cant. de Incendios"))
