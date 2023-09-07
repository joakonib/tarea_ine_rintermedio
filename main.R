#*************************************************************************************************************************************************************
# 0. Identificación ----------------------------------
#Título: Tarea de R Intermedio
#Institucion: Instituto Nacional de Estadísticas
#Encargado: Joaquín E Galdames Hernández - Analista Socioeconómico
#*************************************************************************************************************************************************************

# 1. Cargar paquetes ------------------------
if (!require("pacman")) install.packages("pacman")  #si falta pacman, instalar

pacman::p_load(
  tidyverse,
  glue,
  purrr,
  stringr,
  srvyr,
  microbenchmark,
  data.table,
  rlang
)

##Eliminar notación científica
options(scipen=999)

##Abrir funciones
source(file = "functions.R", encoding = "UTF-8")

### Ejercicio 1: descargar archivos-------

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

walk(urls, ~ download_esi_data(.x, "data", extract_name(.x)))
  

### Ejercicio 2: leer archivos-------

esi <- map(file.path("data",list.files(path = "data/")), ~ read_esi_data(.x)) 

### creo un vector para ponerle los nombres
nombres <- str_extract(list.files(path = "data/"), pattern = "esi-\\d{4}") %>% str_replace("-","_")

### pongo nombres al objeto dentro de la lista
esi <- esi %>% set_names(nombres)

remove(nombres)

### Ejercicio 3: obtener datos-------

# Tabla que contenga 3 columnas: version, n_personas (idrph) y n_hogares (id_identificacion). 
# En la columna version debes usar la siguiente estructura: esi_{año}. Ejemplo: esi_2017

tabla_3.1 <- map_dfr(esi , ~ .x %>% 
          summarize(
            n_personas = nrow(.x),
            n_hogares = length(unique(id_identificacion))),
          .id = "version"
          )

# Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 del factor de expansión (fact_cal_esi) para cada versión. Debes considerar una fila por hogar (id_identificacion) 
# e incluir la columna version ¿Se observan algunos pesos de muestreo atípicos?
tabla_3.2 <- map_dfr(esi , ~ .x %>% 
           summarize(
            "mínimo" = min(fact_cal_esi,na.rm=T),
            "máximo" = max(fact_cal_esi,na.rm=T),
            "media" = mean(fact_cal_esi,na.rm=T),
            "mediana" = median(fact_cal_esi,na.rm=T),
            "p10" = quantile(fact_cal_esi, probs = 0.1),
            "p90" = quantile(fact_cal_esi, probs = 0.9)
             ), .id = "version")

# Tabla que contenga el número de estratos (estrato) con una sola unidad primaria de muestro (conglomerado). 
# Debes incluir la columna version.
tabla_3.3 <- map_dfr(esi , ~ .x %>% 
                        group_by(estrato) %>% 
                        count(conglomerado) %>% 
                        ungroup %>% filter(n==1) %>%  select(-n), .id = "version")

# Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 de los ingresos del trabajo principal (ing_t_p) para cada versión. 
# Esta tabla debe ser construida a nivel persona, utilizando el factor de expansión (fact_cal_esi).

tabla_3.4 <- map_dfr(esi , ~ .x %>% 
                        as_survey(
                          weights = fact_cal_esi) %>% 
                        summarize("mínimo" = min(ing_t_d,na.rm = T),
                                  "máximo" = max(ing_t_d,na.rm = T),
                                  "media" = mean(ing_t_d,na.rm=T),
                                  "mediana" = median(ing_t_d,na.rm=T),
                                  "p10" = quantile(ing_t_d, probs = 0.1),
                                  "p90" = quantile(ing_t_d, probs = 0.9)),
                      .id = "version"
                      )


### Ejercicio 4: mejorando el código-------

esi_df <- do.call(bind_rows, c(esi, .id = "version"))

microbenchmark(
  map_dfr(esi , ~ .x %>% summarize("media" = mean(ing_t_d,na.rm=T)),.id = "version"), ##purrr
  esi_df %>% group_by(version) %>% summarise("media" = mean(ing_t_d,na.rm=T)) %>% ungroup(), ##en df con summarise
  map_dfr(esi , ~ fun_media(.x,ing_t_d,version),.id = "version"), ###purrr + data.table
  as.data.table(esi_df)[,mean(ing_t_d, na.rm=T),by = version], ###promedio simple con dt
  times = 5)


###Parece que siempre es mejor usar la base de datos tal cual :o
#### Díganle a Agustín que me debe un pasaje 

