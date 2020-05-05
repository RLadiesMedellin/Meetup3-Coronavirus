
# Descargar paquetes
install.packages(c("readr", "dplyr", "ggplot2", "agricolae", "Hmisc", "stringi", "stringr"))

# Cargar paquetes
library(readr)
library(dplyr)
library(ggplot2)
library(agricolae) 
library(Hmisc)
library(stringr)


# BASE DE DATOS OUR WORLD IN DATA -----------------------------------------

# Importar base de datos con url
url <- "https://covid.ourworldindata.org/data/ecdc/full_data.csv"
datos <- read_csv(url)

# Explorar la base de datos
dim(datos) # Para conocer la dimensión de la base de datos
glimpse(datos) # Para explorar las variables
head(datos) # Seis primeras observaciones de la base de datos
tail(datos) # Últimas seis observaciones de la base de datos
describe(datos) # Resumen mas completo de las variables

# Ver cuales observaciones tienen valores negativos en la columna new cases
datos[datos$new_cases < 0, ]

# Modificar los valores de la columna new cases por sus valores absolutos
datos$new_cases <- abs(datos$new_cases)

# Para saber cuales observaciones tienen valores faltantes (SI APLICA)
datos[which(is.na(datos$new_cases)), ]


# Ver datos a nivel mundial

# Seleccionar solo las filas cuya locación es igual a World
mundo <- datos[datos$location == "World", ]

# Observar los datos del día de hoy
tail(mundo, 1)

# Resumen de las variables numéricas
summary(mundo$new_cases)
summary(mundo$new_deaths)

# Filtrar datos por fechas
mundofecha <- mundo %>% filter(date >= "2020-03-01" & date <= "2020-04-30")
mundofecha

# En que día se registraron el mayor número de casos confirmados y de muertes
mundo$date[which.max(mundo$new_cases)] # Para el número de casos confirmados
mundo$date[which.max(mundo$new_deaths)] # Para el número de muertes


# Gráfica número de casos confirmados por fecha en el mundo
ggplot(data = mundo,
       aes(x = date, y = new_cases)) +
        geom_line(color = "green") + geom_point(color = "green") +
        theme_bw() +
        labs(x = "Fecha", y = "Número de casos confirmados")

# Gráfica número de muertes por fecha en el mundo
ggplot(data = mundo,
       aes(x = date, y = new_deaths)) +
        geom_line(color = "blue") + geom_point(color = "blue") +  
        theme_bw() +
        labs(x = "Fecha", y = "Número de muertes")


# Ver datos por países

paises <- datos[datos$location != "World", ]

# Para el máximo número de casos confirmados en un día
max(paises$new_cases)
# País con el máximo número de casos confirmados en un día
paises$location[which.max(paises$new_cases)]       


# Para el máximo número de muertes en un día
max(paises$new_deaths)
# País con el máximo número de muertes en un día
paises$location[which.max(paises$new_deaths)]


# El comportamiento del virus en una país en particular
# Filtrar por variable location
italia <- filter(paises, paises$location == "Italy") 

# Gráfica número de casos confirmados por fecha en Italia
ggplot(data = italia, aes(x = date, y = new_cases)) +
        geom_line(color = "red") + geom_point(color = "red") + theme_bw() + 
        labs(x = "Fecha", y = "Número de casos confirmados")

# Gráfica número de muertes por fecha en Italia
ggplot(data = italia, aes(x = date, y = new_deaths)) +
        geom_line(color = "red") + geom_point(color = "red") + theme_bw() + 
        labs(x = "Fecha", y = "Número de casos confirmados")



# Promedio y desviación estándar por países 
resumenpaises <- paises %>% group_by(location) %>% summarise(mediacasos = mean(new_cases),
                                                      mediamuertes = mean(new_deaths),
                                                      sdcasos = sd(new_cases),
                                                      sdmuertes = sd(new_deaths))

resumenpaises


# Ver los 10 países con el mayor promedio de casos confirmados y de muertes

# Gráfica de los 10 países con el mayor promedio de casos confirmados
resumenpaises %>% top_n(10, mediacasos) %>%
        ggplot(aes(x = reorder(location, -mediacasos), y = mediacasos)) + 
        geom_bar(stat = "identity", width = 0.5, fill = "limegreen") +
        theme_bw() + labs(x = "Países", y = "Promedio Casos Confirmados") +
        geom_text(size = 3.5, aes(label = round(mediacasos, 2)), vjust = -0.5)


#Gráfica de los 10 países con el mayor promedio de muertes 
resumenpaises %>% top_n(10, mediamuertes) %>%
        ggplot(aes(x = reorder(location, -mediamuertes), y = mediamuertes)) + 
        geom_bar(stat = "identity", width = 0.5, fill = "limegreen") +
        theme_bw() + labs(x = "Países", y = "Promedio muertes") +
        geom_text(size = 3.5, aes(label = round(mediamuertes, 2)), vjust = -0.5)


# Hallar el top 5 de países con mayor número de casos y de muertes

# Seleccionar el último registro de cada país
acumpaises <- paises %>% group_by(location) %>% summarise(tail(total_cases, 1),
                                                          tail(total_deaths, 1)) 

# Cambiar el nombre de las columnas
colnames(acumpaises) <- c("País", "Casos", "Muertes")


# Hallar el top 5 de países con mayor número de casos confirmados (acumulado)
top5casos <- acumpaises %>% top_n(5, Casos) 

filter(paises, location %in% top5casos$País) %>%
        filter(date >= "2020-03-01") %>%
        ggplot(aes(x = date, y = new_cases, colour = location)) +
        geom_line() + geom_point() + theme_bw() + 
        theme(legend.position = "top", 
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 6)) +
        labs(x = "Fecha", y = "Número de casos confirmados",
             color = "Country")

# Hallar el top 5 de países con mayor número de muertes (acumulado)
top5muertes <- acumpaises %>% top_n(5, Muertes)

filter(paises, location %in% top5muertes$País)  %>% 
        filter(date >= "2020-03-01") %>% 
        ggplot(aes(x = date, y = new_deaths, colour = location)) +
        geom_line() + geom_point() + theme_bw() + 
        theme(legend.position = "top", 
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 6)) +
        labs(x = "Fecha", y = "Número de muertes",
             color = "Country")


# Añadir nueva variable CONTINENTE
# Base de datos tomada de OUR WORLD IN DATA

# Importar base de datos con url
continentes <- read_csv("https://covid.ourworldindata.org/data/ecdc/locations.csv")

# Explorar la base de datos
head(continentes)
table(continentes$continent)

# Seleccionar de la base de datos, las columnas que nos interesan
continentesnuevo <- select(continentes, location, continent)

# Unir las bases de datos
paisesnuevo <- paises %>% left_join(continentesnuevo, by = "location")

# Países con mayor número de casos confirmados por continentes
top5sudamerica <- paisesnuevo %>% 
        filter(continent == "South America") %>% 
        group_by(location) %>% 
        summarise(casos = tail(total_cases, 1)) %>%
        top_n(5, casos)

top5sudamerica                



# BASE DE DATOS COLOMBIA --------------------------------------------------

# URL  BASE DE DATOS: https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD

# Importar base de datos alojada en el computador
datacolombia <- read_csv("C:/Users/ASUS/Desktop/Casos_positivos_de_COVID-19_en_Colombia (1).csv")

# Explorar la base de datos
dim(datacolombia) # Para conocer la dimensión de la base de datos
glimpse(datacolombia) # Para explorar las variables
head(datacolombia) # Seis primeras observaciones de la base de datos
tail(datacolombia) # Últimas seis observaciones de la base de datos
describe(datacolombia) # Resumen mas completo de las variables

# Análisis de variables numéricas
summary(datacolombia$Edad)

# Crear tabla con intervalos de edades
tablaedad <- table.freq(hist(datacolombia$Edad, breaks=11, right=FALSE, plot=FALSE))
tablaedad

# Gráfica de barras con el número de infectados pertenecientes a cada intervalo de edad
ggplot(data = tablaedad, aes(x = factor(Upper), y = Frequency)) +
        geom_bar(stat = "identity", fill = "aquamarine3") +  
        labs(x = "Edad", y ="Frecuencia") +  theme_bw() +
        geom_text(size = 3.5, aes(label = Frequency), vjust = -0.5) +
        scale_x_discrete(labels = c("0-9", "10-19",
                                    "20-29", "30-39",
                                    "40-49", "50-59", 
                                    "60-69", "70-79", 
                                    "80-89", "90-99",
                                    "100-109"))

# Crear nueva variable que asigne un intervalo de edad a cada registro
datacolombia <- datacolombia %>% 
        mutate(intervaloedad = case_when(between(Edad, 0, 9) ~ "0-9",
                                     between(Edad, 10, 19) ~ "10-19", 
                                     between(Edad, 20, 29) ~ "20-29",
                                     between(Edad, 30, 39) ~ "30-39",
                                     between(Edad, 40, 49) ~ "40-49",
                                     between(Edad, 50, 59) ~ "50-59",
                                     between(Edad, 60, 69) ~ "60-69",
                                     between(Edad, 70, 79) ~ "70-79",
                                     between(Edad, 80, 89) ~ "80-89",
                                     between(Edad, 90, 99) ~ "90-99",
                                     between(Edad, 100, 109) ~ "100-109")) 



# Análisis de variables categóricas

# DEPARTAMENTO O DISTRITO
# Lista de todos los departamentos con casos registrados
unique(datacolombia$`Departamento o Distrito`)
# Nombres de departamentos en mayúscula
datacolombia$`Departamento o Distrito` <- toupper(datacolombia$`Departamento o Distrito`)
# Número de casos registrados en cada departamento
table(datacolombia$`Departamento o Distrito`)


# CIUDAD
# Lista de todos las ciudades con casos registrados
unique(datacolombia$`Ciudad de ubicación`)
# Nombres de ciudades en mayúscula
datacolombia$`Ciudad de ubicación` <- toupper(datacolombia$`Ciudad de ubicación`)
# Número de casos registrados en cada ciudad
table(datacolombia$`Ciudad de ubicación`)

# PAÍS DE PROCEDENCIA
# Lista de todos los países de procedencia
unique(datacolombia$`País de procedencia`)
# Nombres de países en mayúscula y quitar acentos
datacolombia$`País de procedencia` <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(datacolombia$`País de procedencia`))
# Número de casos registrados para cada país
table(datacolombia$`País de procedencia`)

# SEXO
# Lista de sexo
unique(datacolombia$Sexo)
# Sexo en mayúscula
datacolombia$Sexo <- toupper(datacolombia$Sexo)
# Número de casos registrados de cada sexo
table(datacolombia$Sexo)

# ESTADO
# Lista de categorías variable estado
unique(datacolombia$Estado)
# Poner la primera letra en mayúscula y el resto en minúscula
str_sub(datacolombia$Estado, 1, 1) <- str_to_upper(str_sub(datacolombia$Estado, 1, 1))
str_sub(datacolombia$Estado, 2) <- str_to_lower(str_sub(datacolombia$Estado, 2))
table(datacolombia$Estado)


# ATENCIÓN
# Lista de categorías variable atención
unique(datacolombia$atención)
# Poner la primera letra en mayúscula
str_sub(datacolombia$atención, 1, 1) <- str_to_upper(str_sub(datacolombia$atención, 1, 1))
str_sub(datacolombia$atención, 2) <- str_to_lower(str_sub(datacolombia$atención, 2))
table(datacolombia$atención)


# TIPO
# Lista de categorías variable tipo
unique(datacolombia$Tipo)
# Poner la primera letra en mayúscula y el resto en minúscula
str_sub(datacolombia$Tipo, 1, 1) <- str_to_upper(str_sub(datacolombia$Tipo, 1, 1))
str_sub(datacolombia$Tipo, 2) <- str_to_lower(str_sub(datacolombia$Tipo, 2))
table(datacolombia$Tipo)



# Tabla intervaloedad vs estado
prop.table(table(datacolombia$intervaloedad, datacolombia$Estado), 1)


# Gráfico de barras variable Estado (PORCENTAJE)
datacolombia %>% group_by(Estado) %>% 
        summarise(Frecuencia = n()) %>% 
        mutate(prop = Frecuencia/sum(Frecuencia)) %>%
        ggplot(aes(x = reorder(Estado, -Frecuencia), y = prop)) +
        geom_bar(stat = "identity", fill = "aquamarine3") +  
        labs(x = "Estado", y = "Porcentaje") +  theme_bw() +
        geom_text(aes(label = scales::percent(prop), 
                      y = prop), vjust = -0.5, size = 3.5)


# Gráfico de barras variable Tipo (FRECUENCIA)
datacolombia %>% group_by(Tipo) %>% 
        summarise(Frecuencia = n()) %>%
        ggplot(aes(x = reorder(Tipo, -Frecuencia), y = Frecuencia)) +
        geom_bar(stat = "identity", fill = "aquamarine3") +  
        labs(x = "Tipo", y = "Frecuencia") + theme_bw() +
        geom_text(size = 3.5, aes(label = Frecuencia), vjust = -0.5)


# Boxplot Edad vs Estado
ggplot(data = datacolombia, aes(x = Estado, y = Edad, fill = Estado)) +
        geom_boxplot() + 
        theme_bw() + #Tema de fondo del gráfico
        theme(legend.position = "top", plot.title = element_text(face = "bold", 
                                                               size = 12,
                                                               hjust = 0.5), 
              plot.subtitle = element_text(size = 10, hjust = 0.5)) + 
        labs(y = "Edad", title = "Estado vs Edad")


# Para ver cuales observaciones tienen datos faltantes
datacolombia[datacolombia$Estado == "N/a", ]
# Para eliminar filas con datos faltantes si deseamos
newcolombia <- datacolombia[datacolombia$Estado != "N/a", ]


# Top 5 de departamentos con más casos confirmados
top5dep <- datacolombia %>% group_by(`Departamento o Distrito`) %>% 
                summarise(Frecuencia = n()) %>%
                top_n(5, Frecuencia)

top5dep

# Gráfico Top5 departamentos
filter(datacolombia, datacolombia$`Departamento o Distrito` %in% top5dep$`Departamento o Distrito`) %>%
        group_by(`Fecha diagnostico`, `Departamento o Distrito`) %>% summarise(Frecuencia = n()) %>%
        ggplot(aes(x = `Fecha diagnostico`, y = Frecuencia, colour = `Departamento o Distrito`)) +
        geom_line() + geom_point() + theme_bw() + 
        theme(legend.position = "top", 
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 6)) +
        labs(x = "Fecha", y = "Número de casos confirmados",
             color = "Departamento o distrito")


# Top 5 de ciudades con más casos confirmados
top5ciudad <- datacolombia %>% group_by(`Ciudad de ubicación`) %>% 
                summarise(Frecuencia = n()) %>%
                top_n(5, Frecuencia)
top5ciudad

# Fecha de primer caso en cada ciudad o departamento (top 5)
datostop5 <- filter(datacolombia, datacolombia$`Ciudad de ubicación` %in% top5ciudad$`Ciudad de ubicación`) %>%
                group_by(`Ciudad de ubicación`) %>% 
                summarise(minfecha = min(FIS))

datostop5


# Analizar solo los datos de las personas fallecidas a causa del virus
fallecidos <- datacolombia[datacolombia$atención == "Fallecido", ]

summary(fallecidos$Edad)

fallecidos %>% group_by(`Departamento o Distrito`) %>% 
        summarise(Frecuencia = n()) %>%
        top_n(5, Frecuencia)

# Número de fallecidos en un día particular
fallecidos %>% filter(str_sub(fallecidos$`Fecha de muerte`, 1, 10) == "2020-05-02")
