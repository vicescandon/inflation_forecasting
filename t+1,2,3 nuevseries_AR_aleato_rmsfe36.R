###########################################
# DATOS
###########################################

#Directorio y paquetes
setwd("C:/Users/victo/OneDrive/Documents/Banxico")
library(readxl)
library(tidyverse)
library(dplyr)
library(forecast)


#Leer base
datos<-read_excel("datos en IQY dependiente inflacion general.xlsx", sheet = "Datos", na = c("","NA","#N/D", "N/E","Nan", "NaN"))

#### Limpieza y separación por data frames ####
datos$...2<-NULL #elimino una columna de fecha, ya que la base tenía dos
datos <- datos %>%
  rename(Fecha = ...1) #renombro variable a "Fecha"
datos$Fecha<-as.Date(datos$Fecha) #formato a clase fecha

#Dummies estacionales, se agregan al final de la base
#agregar variable mes
datos$Mes<-format(datos$Fecha, "%m")

#agregar dummies
datos <- datos %>% 
  mutate(enero = ifelse(Mes == "01", 1, 0),
         febrero = ifelse(Mes == "02", 1, 0),
         marzo = ifelse(Mes == "03", 1, 0),
         abril = ifelse(Mes == "04", 1, 0),
         mayo = ifelse(Mes == "05", 1, 0),
         junio = ifelse(Mes == "06", 1, 0),
         julio = ifelse(Mes == "07", 1, 0),
         agosto = ifelse(Mes == "08", 1, 0),
         septiembre = ifelse(Mes == "09", 1, 0),
         octubre = ifelse(Mes == "10", 1, 0),
         noviembre = ifelse(Mes == "11", 1, 0),
         diciembre = ifelse(Mes == "12", 1, 0))

# #################################################CAMBIO A REALIZAR PARA ACTUALIZAR PREDICCIÓN##########################################
datos<-datos[331:543,] #AGREGAR UNA FILA DE OBSERVACIONES
future_months <- c("04", "05", "06") #DEFINICIÓN DE DUMMIES DE MESES A PREDECIR, EN ESTE CASO SON LOS MESES t+1,2,3
#TAMBIÉN CONSIDERE SE SI HARÁ UN FORECAST DE INFL GENERAL, SUBYACENTE O NO SUBYACENTE CAMBIE EL NOMBRE DE LA BASE DE DATOS
#########################################################################################################################################

#Generación de data frames por secciones, además agrega fechas a cada data frame

datos_produccion<-datos[,1:21] #df producción

datos_precios<-datos[,c(1,22:31)] #df precios

datos_agrmonetarios<-datos[,c(1,32:35)] #df agregados monetarios

datos_financieros<-datos[,c(1,36:41)] #df variables financieras

datos_genericos<-datos[,c(1,56:346)] #df variables balanza de pagos

datos_volatiles<-datos[,c("Fecha","wti","propano","agricultura","fertilizantes","metales_minerales","sequia_d0",
                          "sequia_d1","sequia_d2","sequia_d3","sequia_d4","sin_sequia", "precipitacion", "temperatura")] #df variables volátiles

#### Transformación de variables ####

#producción, todas primeras diferencias logarítmicas
datos_produccion<-cbind(datos[,1], sapply(datos_produccion[,-1], function(x) c(NA,diff(log(x))))) #agrega NA al inicio de columnas y calcula primeras diferencias logarítmicas excepto colmna de fechas 

#precios, todas primeras diferencias logarítmicas
datos_precios<- cbind(datos[,1], sapply(datos_precios[,-1], function(x) c(NA,diff(log(x))))) #une vector de fechas con variables transformadas con primeras diferencias logaritmicas

#agregados monetarios, todas primeras diferencias logarítmicas
datos_agrmonetarios<- cbind(datos[,1], sapply(datos_agrmonetarios[,-1], function(x) c(NA,diff(log(x))))) #une vector de fechas con variables transformadas con primeras diferencias logaritmicas

#variables financieras, todas primeras diferencias logarítmicas excepto cetes a 91 días
datos_financieros <- cbind(datos[,1],
                           sapply(datos_financieros[,2], function(x) c(NA,diff(x))),
                           sapply(datos_financieros[,3:7], function(x) c(NA,diff(log(x)))))

#datos genericos, todas primeras diferencias logarítmicas
datos_genericos <- cbind(datos[,1], sapply(datos_genericos[,-1], function(x) c(NA,diff(log(x)))))

#datos volátiles, peimeras diferencias logartimicas a precios (wti y propano) pero ninguna transformación a series de clima
datos_volatiles <- cbind(datos_volatiles[,c("Fecha","sequia_d0","sequia_d1","sequia_d2","sequia_d3","sequia_d4","sin_sequia", "precipitacion", "temperatura")],
                         sapply(datos_volatiles[,c("wti","propano","agricultura","fertilizantes","metales_minerales")],
                                function(x) c(NA,diff(log(x)))))



## PROVISIONAL quitar NA's , se quita variable de fechas y primera fila con NA
datos_produccion<-datos_produccion[-1,-1] 
datos_produccion <- datos_produccion[, -which(colSums(is.na(datos_produccion[1:(nrow(datos_produccion)-2),])) != 0)]

datos_precios<-datos_precios[-1,-1]
datos_precios<-datos_precios[,-which(colSums(is.na(datos_precios[1:(nrow(datos_precios)-2),])) !=0)]

datos_financieros<-datos_financieros[-1,-1]
datos_financieros<-datos_financieros[,-which(colSums(is.na(datos_financieros[1:(nrow(datos_financieros)-2),])) !=0)]

datos_agrmonetarios<-datos_agrmonetarios[-1,-1]
datos_agrmonetarios<-datos_agrmonetarios[,-which(colSums(is.na(datos_agrmonetarios[1:(nrow(datos_agrmonetarios)-2),])) !=0)]

datos_genericos<-datos_genericos[-1,-1]
datos_genericos<-datos_genericos[,-which(colSums(is.na(datos_genericos[1:(nrow(datos_genericos)-2),])) !=0)]

datos_volatiles<-datos_volatiles[-1,-1]
datos_volatiles<-datos_volatiles[,-which(colSums(is.na(datos_volatiles[1:(nrow(datos_volatiles)-2),])) !=0)]

#AR para predecir valores faltantes

# # Función para ajustar el modelo AR y predecir valores faltantes
ajustar_ar <- function(serie) {
ts_serie <- ts(serie, frequency = 12, start = c(2007, 8))  # Convertir a serie temporal, los datos inician en 2007/08
sreg <- seasonaldummy(ts_serie, h = NULL)  # Dummies estacionales
model <- auto.arima(ts_serie, d = 0, max.q = 0, ic = "bic", xreg = sreg, seasonal = FALSE)  # Ajustar modelo AR
h <- sum(is.na(serie[(length(serie)-1):length(serie)]))  # Contar NA en las últimas observaciones, puede ser 1 o hasta 2 datos desactualizados en las series, por tanto este p´rametro define el horizonte de predicción
sreg_f <- seasonaldummy(ts_serie, h = h)  # Generar dummies para predicción
forecast_values <- forecast(model, xreg = sreg_f, h = h)  # Predecir 1 o hasta 2 valores
serie[(length(serie)-h+1):length(serie)] <- forecast_values$mean[1:h]  # Reemplazar NA con predicciones, 1 o 2 en las últimas 1 o 2 entradas.
return(serie)
}

#Función para aplicar AR a todas las series de un data frame donde el data frame es un subcojunto de series (precios, financieras, etc.); se aplica a AR solo a las series dentro del df que contengan NA en sus últimas una o dos observaciones
ajustar_ar_dataframe <- function(df) {
for (col in 1:ncol(df)) {  # Procesar todas las columnas
if (any(is.na(df[, col][(length(df[, col])-1):length(df[, col])]))) {  # Verificar serie donde NA en las últimas observaciones 1 o 2 observaciones
df[, col] <- ajustar_ar(df[, col])  # a esa serie ajustar AR solo si hay NA
}
}
return(df)
}

#Aplicar a cada data frame ambas funciones
datos_produccion <- ajustar_ar_dataframe(datos_produccion)
datos_precios <- ajustar_ar_dataframe(datos_precios)
datos_financieros <- ajustar_ar_dataframe(datos_financieros)
datos_agrmonetarios <- ajustar_ar_dataframe(datos_agrmonetarios)
datos_genericos <- ajustar_ar_dataframe(datos_genericos)
datos_volatiles <- ajustar_ar_dataframe(datos_volatiles)



##


#### Componentes principales ####
set.seed(100) #realizar la misma combinación en el mismo orden

# ChatGPT implementation to know which variables are ommited and saves PC's that captures 70% of variation
###################################################################
# List of names of data frames
data_frame_names <- c("datos_produccion", "datos_precios", "datos_agrmonetarios", "datos_financieros", "datos_genericos", "datos_volatiles")

# Initialize a list to store the results for each combination
results <- list()

# Function to calculate number of PCs explaining 70% of variance and return their loadings
get_pcs_for_70_variance <- function(pca_result, dates) {
  # Calculate cumulative proportion of variance explained
  cum_var_explained <- cumsum(summary(pca_result)$importance[2, ])
  
  # Find number of components that explain at least 70% of variance
  num_pcs <- which(cum_var_explained >= 0.70)[1]
  
  # Limit to a maximum of 5 PCs
  num_pcs <- min(num_pcs, 5)
  
  # Return the loadings (vectors) for those selected components
  selected_pcs <- pca_result$x[, 1:num_pcs]
  
  # Convertir en df y agregar fechas
  selected_pcs_df <- as.data.frame(selected_pcs)
  selected_pcs_df$Fecha <- dates
  
  return(list(num_pcs = num_pcs, selected_pcs = selected_pcs_df))
}

dates <- datos$Fecha[-1] #Fechas a agrgar en cada PC, asegúrese de que las rows sean las fechas a usar, se quota siempre primera fila (fecha) para que al unir fechas las dimensiones hagan sentido
#es decir un numero más que las filas tomadas al inicio del script, siempre un numero más al inicio


####################### EXPLICACIÓN DE CÓMO SE HICIERON LOS DATA FRAMES PARA ALETORIZAR CIERTO % DE LAS SERIES SIGUIENDO LAS COMBINACIONES DE DATOS ###################

# === RESUMEN RÁPIDO DE LAS 3 FORMAS DE GENERAR DATA FRAMES: ===
# 1) k:         Usar todos los subconjuntos => sin combinaciones => 10 objetos ("k_combination_1" a "k_combination_10").
# 2) k-1:       Omitir 1 de los 6 subconjuntos => 6 formas x 10 iteraciones = 60 objetos.
# 3) k-2:       Omitir 2 de los 6 subconjuntos => C(6,2)=15 formas x 10 iteraciones = 150 objetos.
# => Total: 10 + 60 + 150 = 220 objetos en la lista 'all_selected_series', en ese orden.

# === EJEMPLO DE ÍNDICES: ===
# - Los primeros 10 (k) van en índices 1..10.
# - k-1 (60 objetos) van en índices 11..70.
# - k-2 (150 objetos) van en índices 71..220.
# Así "k-2_combination_2_iteration_9" está en la posición 89 dentro de la lista,
# aunque su nombre solo indique “combination_2” (es la 2a combinación de k-2,
# pero en el conteo global es la n.º 89).



#### For the combination of k data frames ####
dfcombinado <- cbind(datos_produccion, datos_precios, datos_agrmonetarios, datos_financieros, datos_genericos, datos_volatiles)

################ sacar el 60% de cada combinación, aquí será para el los 6 data frames, usar el numero de variables que corresponde al 60% y tomar aleatoriamente y 10,000 ese numero de variables, se obtendrá predicción con esas 10,000 combinaciones #######0#############
# dado que para el data frame completo no se sacan combinaciones, no se necesita loop para sacar el número de series que corresponde al 60
frac_datos<-40
iterations<-10

contador_global<-1

n<-round((frac_datos*nrow(dfcombinado))/100) #numero de series que corresponden al 60%, se usó regla de 3 (se redondea el valor)
combinaciones_aleatorias<-vector("list",iterations) # lista vacía con nombres de data frames
all_selected_series<-list() #lista vacía para almacenar nombres de series de cada combinación
all_omitted_subsets<-list() #lista vacía para almacenar series omitidas de cada combinación

#loop para tomar 10,000 veces 60% de las series con selección aleatoria y sin reemplazo
for (i in 1:iterations) { #loop para obtener las 100000 combinaciones de datos
  combinaciones_aleatorias[[i]]<-cbind(dfcombinado[,sample(names(dfcombinado),n)]) #muestra aleatoria del 60% de las series 10,000 veces, genera ya todos los data frames
}

for (j in 1:iterations) {
  pca_result <- prcomp(combinaciones_aleatorias[[j]], scale. = TRUE) #correr pc para las n=iteracines data frames 
  selected_series<-names(combinaciones_aleatorias[[j]])
  pca_info_k <- get_pcs_for_70_variance(pca_result, dates) #tomar las que explican el 70 de variación
  results[[paste0("k_data_frames_combination_", j,"_idglobal_",contador_global)]] <- list( #escribir en resulta el pc y el numero de pc seleccionados
    "global_id"=contador_global,
    "num_pcs_70_variance" = pca_info_k$num_pcs,
    "selected_pcs" = pca_info_k$selected_pcs,
    "selected_series" = selected_series
  )
  contador_global<-contador_global+1
  all_omitted_subsets[[paste0("k_combination_")]] <- "ninguna"
}

# # Get the number of PCs explaining 70% variance and the selected PC loadings for 6 data frames
# pca_info_6 <- get_pcs_for_70_variance(resultados6_pca, dates)
# 
# # Store result for 6 data frames
# results[["6_data_frames"]] <- list(
#   "included" = data_frame_names,
#   "omitted" = NULL,  # No data frames omitted
#   "num_pcs_70_variance" = pca_info_6$num_pcs,
#   "selected_pcs" = pca_info_6$selected_pcs  # Store the selected PCs (loadings)
# )

#### For the combinations of k-1 data frames, donde k es el numero de
#### subconjuntos que tiene la base entera ####

dfcombinado_k1 <- cbind(datos_produccion, datos_precios, datos_agrmonetarios, datos_financieros, datos_genericos, datos_volatiles)

k<-length(data_frame_names)-1

list_datos<-list(datos_produccion,datos_precios,datos_agrmonetarios,datos_financieros,datos_genericos,datos_volatiles) #genero lista de datos

combinacionesk_1 <- combn(x=seq_along(list_datos), m=k, simplify = FALSE)

n<-vector("numeric",length(seq_along(combinacionesk_1))) #vector de n del largo de las combinaciones diferentes

combinaciones_aleatorias_1dfomit<-vector("list",iterations)

all_combinaciones_aleatorias_1dfomit<-vector("list",length(combinacionesk_1)*iterations)

for (f in seq_along(combinacionesk_1)) { #un solo bucle ya que n y el numero de combinaciones de dataframes tienen la misma dimensión
  # Combinar data frames seleccionados usando índices
  dfcombinado_k1[[f]] <- do.call(cbind, list_datos[combinacionesk_1[[f]]]) #aquí ya son todos los data frames que salen de la combinacion de eliminar un solo subconjunto
  
  n[f]<-round(frac_datos*ncol(dfcombinado_k1[[f]])/100) #regla de 3 para la valor de n correspondiente
  
  # Determinar cuáles data frames se incluyeron y cuáles se omitieron
  included <- data_frame_names[combinacionesk_1[[f]]]
  omitted_k_1 <- setdiff(data_frame_names, included)
  
  # Listas para almacenar las submuestras aleatorias y resultados PCA de esta combinación
  combinaciones_aleatorias_1dfomit <- vector("list", iterations)
  pca_results_for_this_combination <- vector("list", iterations)
  
  for (j in 1:iterations) {
    selected_series<-sample(names(dfcombinado_k1[[f]]),n[f])
    df_aleatorio<-dfcombinado_k1[[f]][,selected_series]
    combinaciones_aleatorias_1dfomit[[j]] <- df_aleatorio
    
    # Calcular PCA para esta submuestra
    pca_result <- prcomp(combinaciones_aleatorias_1dfomit[[j]], scale. = TRUE)
    pca_info_k_1 <- get_pcs_for_70_variance(pca_result, dates)
    
    # Guardar resultado del PCA para esta submuestra
    results[[paste0("k-1_data_frames_combination_", f,"_iteration_",j,"_idglobal_",contador_global)]] <- list(
      "idglobal" = contador_global,
      "included" = included,
      "omitted" = omitted_k_1,
      "num_pcs_70_variance" = pca_info_k_1$num_pcs,
      "selected_pcs" = pca_info_k_1$selected_pcs,
      "selected series" = selected_series
    )
    contador_global<-contador_global+1
    all_selected_series[[paste0("k-1_combination_", f, "_iteration_", j)]] <- selected_series
  }
  
  # Guardar las <iteraciones> submuestras aleatorias en la lista principal
  all_combinaciones_aleatorias_1dfomit[[f]] <- combinaciones_aleatorias_1dfomit
}

#RESULTADO DE ESTE DOBLE LOOP ANTERIOR
#all_combinaciones_aleatorias tienen 6 entradas (cada combinacion posible, y cada entrada tiene las
#<n=iteracioes> diferentes bases con cierto % de series tomadas aleatoriamente)
#para acceder haga: objeto[[i]][[j]]

# #Qué sigue? se volverá a hacer lo mismo pero con las combinaciones posibles
# omitiendo solo dos. Así, se unirán todos los resultados de los 22*intraciones
# PCA que se corrieron. El número 22 proviene de todas las combinaciones posibles 
# son elimnar ningún sobconjunto de datos, eliminando uno y eliminando dos.



#### For the combinations of k-2 data frames, donde k es el numero de
#### subconjuntos que tiene la base entera ####

k<-length(data_frame_names)-2

dfcombinado_k2 <- cbind(datos_produccion, datos_precios, datos_agrmonetarios, datos_financieros, datos_genericos, datos_volatiles)

list_datos<-list(datos_produccion,datos_precios,datos_agrmonetarios,datos_financieros,datos_genericos,datos_volatiles) #genero lista de datos

combinacionesk_2 <- combn(x=seq_along(list_datos), m=k, simplify = FALSE)

n<-vector("numeric",length(seq_along(combinacionesk_2))) #vector de n del largo de las combinaciones diferentes

combinaciones_aleatorias_1dfomit<-vector("list",iterations)

all_combinaciones_aleatorias_1dfomit<-vector("list",length(combinacionesk_2)*iterations)

for (m in seq_along(combinacionesk_2)) { #un solo bucle ya que n y el numero de combinaciones de dataframes tienen la misma dimensión
  # Combinar data frames seleccionados usando índices
  dfcombinado_k2[[m]] <- do.call(cbind, list_datos[combinacionesk_2[[m]]]) #aquí ya son todos los data frames que salen de la combinacion de eliminar un solo subconjunto
  
  n[m]<-round(frac_datos*ncol(dfcombinado_k2[[m]])/100) #regla de 3 para la valor de n correspondiente
  
  # Determinar cuáles data frames se incluyeron y cuáles se omitieron
  included <- data_frame_names[combinacionesk_2[[m]]]
  omitted_k_2 <- setdiff(data_frame_names, included)
  
  # Listas para almacenar las submuestras aleatorias y resultados PCA de esta combinación
  combinaciones_aleatorias_1dfomit <- vector("list", iterations)
  pca_results_for_this_combination <- vector("list", iterations)
  
  for (j in 1:iterations) {
    selected_series<-sample(names(dfcombinado_k2[[m]]),n[m])
    df_aleatorio<-dfcombinado_k2[[m]][,selected_series]
    combinaciones_aleatorias_1dfomit[[j]] <- df_aleatorio
    
    # Calcular PCA para esta submuestra
    pca_result <- prcomp(combinaciones_aleatorias_1dfomit[[j]], scale. = TRUE)
    pca_info_k_2 <- get_pcs_for_70_variance(pca_result, dates)
    
    # Guardar resultado del PCA para esta submuestra
    results[[paste0("k-2_data_frames_combination_", m,"_iteration_",j,"_idglobal_",contador_global)]] <- list(
      "idglobal" = contador_global,
      "included" = included,
      "omitted" = omitted_k_2,
      "num_pcs_70_variance" = pca_info_k_2$num_pcs,
      "selected_pcs" = pca_info_k_2$selected_pcs,
      "selected series" = selected_series
    )
    contador_global<-contador_global+1
    all_selected_series[[paste0("k-2_combination_", m, "_iteration_", j)]] <- selected_series
  }
  
  # Guardar las <iteraciones> submuestras aleatorias en la lista principal
  all_combinaciones_aleatorias_1dfomit[[m]] <- combinaciones_aleatorias_1dfomit
}


# DESENLISTAR EL OBJETO COMPLETO DE results PARA PODER CALCULAR TODO LO SIGUIENTE A CADA UNO DE ELLOS

# Normalizar `results` en una estructura plana y uniforme
results_normalized <- list()
for (key in names(results)) {
  # Extraer cada lista de resultados dentro de `results`
  combination_results <- results[[key]]
  
  # Si el resultado ya está plano, lo agregamos directamente
  if ("selected_pcs" %in% names(combination_results)) {
    results_normalized[[key]] <- combination_results
  } else {
    # Si es una lista de subresultados, los aplanamos
    for (subkey in seq_along(combination_results)) {
      results_normalized[[paste0(key, "_sub_", subkey)]] <- combination_results[[subkey]]
    }
  }
}

#### Uso de BIC ####

#### ChatGPT recomedation to use BIC for inflation and pc separated
# Separar la selección de lags para los PCs y la inflación
library(vars)

df_optlag_pcs <- list()  # Para almacenar resultados de lags de PCs
df_optlag_inflacion <- list()  # Para almacenar resultados de lags de Inflación
lag_opt_pcs <- list()  # Para almacenar los lags seleccionados de PCs
lag_opt_inflacion <- list()  # Para almacenar los lags seleccionados de Inflación

# Loop para calcular los lags de PCs e Inflación por separado
for (i in seq_along(results_normalized)) {
  # Combinar PCs y la inflación, y asignar un nombre a la columna de inflación
  df_optlag_comb <- data.frame(results_normalized[[i]]$selected_pcs, 
                               Inflación = datos$Inflación[-1])
  
  # Eliminar NA's antes de aplicar VARselect
  #df_optlag_comb <- na.omit(df_optlag_comb)
  
  # Selección de lags solo para los PCs
  df_optlag_pcs[[i]] <- df_optlag_comb %>% dplyr::select(-Fecha, -Inflación)  # Remover la inflación y la fecha
  lag_opt_pcs[[i]] <- VARselect(df_optlag_pcs[[i]], lag.max = 10, type = "const")$selection["SC(n)"]
  
  # Selección de lags solo para la inflación
  df_optlag_inflacion[[i]] <- df_optlag_comb %>% dplyr::select(Inflación)  # Seleccionar solo la inflación
  lag_opt_inflacion[[i]] <- VARselect(df_optlag_inflacion[[i]], lag.max = 10, type = "const")$selection["SC(n)"]
}

# PRINTEAR ESTO CUANDO SEA NECESARIO, SON RESULTADOS DE LAGS Y PC

# # Mostrar los resultados separados para los lags de PCs y de Inflación
# for (i in seq_along(results)) {
#   cat("\nResults for", names(results)[i], ":\n")
#   cat("Included data frames:", paste(results[[i]]$included, collapse = ", "), "\n")
#   cat("Omitted data frames:", ifelse(is.null(results[[i]]$omitted), "None", paste(results[[i]]$omitted, collapse = ", ")), "\n")
#   cat("Number of PCs explaining 70% variance:", results[[i]]$num_pcs_70_variance, "\n")
#   
#   # Mostrar los lags seleccionados para los PCs y para la inflación por separado
#   cat("Number of lags chosen by BIC for PCs:", lag_opt_pcs[[i]], "\n")
#   cat("Number of lags chosen by BIC for inflation:", lag_opt_inflacion[[i]], "\n")
# }

# Definir dummies estacionales y eliminar una (por ejemplo, enero)
dummies <- datos %>%
  dplyr::select(febrero, marzo, abril, mayo, junio, julio, agosto, septiembre, octubre, noviembre, diciembre) %>%
  .[-1, ]  # Eliminar la primera fila desde el inicio

# Función para predecir la inflación para cada combinación de datos
library(vars)

var_prediccion_inflacion <- function(pcs, inflacion, lags_pcs, lags_inflacion, dummies, max_horizon = 3,  ci      = 0.90) {
  # Combinar las PCs e inflación en un solo data frame
  datos_var <- cbind(pcs, Inflacion = inflacion)
  
  # Seleccionar el número máximo de lags
  max_lag <- max(lags_pcs, lags_inflacion)
  
  # Ajustar el modelo VAR con el número máximo de lags
  var_model <- VAR(datos_var, p = max_lag, type = "const", season = 12)
  
  # Seleccionar las dummies correspondientes a esos meses
  dummies_future <- dummies[format(datos$Fecha[-1], "%m") %in% future_months, ]
  dummies_future <- head(dummies_future, max_horizon)
  
  # Predecir los valores de inflación usando las dummies correspondientes
  predicciones <- predict(var_model, n.ahead = max_horizon, dumvar = dummies_future)
  
  # Extraer las predicciones de inflación para t+1, t+2, t+3
  pred_inflacion <- sapply(1:max_horizon, function(h) predicciones$fcst$Inflacion[h, "fcst"])
  pred_lo        <- sapply(1:max_horizon, function(h) predicciones$fcst$Inflacion[h, "lower"])  # ← NUEVO
  pred_hi        <- sapply(1:max_horizon, function(h) predicciones$fcst$Inflacion[h, "upper"])  # ← NUEVO
  
  return(list(mean = pred_inflacion,
              lo   = pred_lo,
              hi   = pred_hi,
              predicciones = predicciones))
  }

#### Calculo de RMSFE usando pseudo out of sample forecast ####
# Función para calcular el RMSFE utilizando pseudo out-of-sample para las últimas 36 observaciones
pseudo_out_of_sample_RMSFE <- function(pcs, inflacion,
                                       lags_pcs, lags_inflacion,
                                       dummies, max_horizon = 3) {
  start_index <- nrow(pcs) - 36 + 1
  T <- nrow(pcs)
  
  errors <- matrix(NA, 36, max_horizon)   # guarda TODOS los errores
  
  for (t in start_index:(T - max_horizon)) {
    training_pcs  <- pcs[1:t, ]
    training_infl <- inflacion[1:t]
    var_model <- VAR(cbind(training_pcs, Inflacion = training_infl),
                     p = max(lags_pcs, lags_inflacion),
                     type = "const", season = 12)
    
    dummies_future <- head(dummies[format(datos$Fecha[-1], "%m") %in%
                                     future_months, ], max_horizon)
    
    fc <- predict(var_model, n.ahead = max_horizon,
                  dumvar = dummies_future)
    
    for (h in 1:max_horizon) {
      pred <- fc$fcst$Inflacion[h, "fcst"]
      real <- inflacion[t + h]
      errors[(t - start_index + 1), h] <- real - pred
    }
  }
  
  rmsfe <- sqrt(colMeans(errors^2, na.rm = TRUE))
  list(rmsfe = rmsfe, errors = errors)    # ⟵ devuelvo ambos
}


# Ejecutar la predicción de inflación y el cálculo de RMSE para cada combinación de datos
resultados_prediccion_inflacion <- list()
resultados_rmsfe_pseudo <- list() #inica lista para errores pseudo

errores_t1 <- list()
for (i in seq_along(results)) {
  combination_name <- names(results_normalized)[i]
  pcs <- results_normalized[[i]]$selected_pcs[, -ncol(results_normalized[[i]]$selected_pcs)]  # Excluir la columna de fechas
  inflacion <- datos$Inflación[-1]  # Ajustar para que coincida con las dimensiones de los PCs
  lags_pcs <- lag_opt_pcs[[i]]
  lags_inflacion <- lag_opt_inflacion[[i]]
  
  # Realizar la predicción de inflación para los horizontes t+1, t+2 y t+3
  predicciones_result <- var_prediccion_inflacion(pcs, inflacion, lags_pcs, lags_inflacion, dummies, max_horizon = 3)
  
  # Almacenar las predicciones de inflación para la combinación actual
  # Después (guarda media y extremos para t+1)
  resultados_prediccion_inflacion[[i]] <- c(mean = predicciones_result$mean[1],
                                            lo90 = predicciones_result$lo[1],
                                            hi90 = predicciones_result$hi[1])
  
  # Calcular el RMSFE utilizando las últimas 36 observaciones para evaluación pseudo out-of-sample
  oos <- pseudo_out_of_sample_RMSFE(pcs, inflacion,
                                    lags_pcs, lags_inflacion,
                                    dummies, max_horizon = 3)
  
  resultados_rmsfe_pseudo[[i]] <- oos$rmsfe
  errores_t1[[i]]              <- oos$errors[, 1]   # solo la columna h = 1
  
  # cat("\n====================================================\n")
  # cat("Resultados para la combinación", i, ":\n")
  # cat("Included data frames:", paste(results[[i]]$included, collapse = ", "), "\n")
  # cat("Omitted data frames:", ifelse(is.null(results[[i]]$omitted), "None", paste(results[[i]]$omitted, collapse = ", ")), "\n")
  # cat("====================================================\n")
  # cat("RMSFE (Pseudo out-of-sample, últimas 36 observaciones) para t+1:", round(rmsfe_pseudo[1], 6), "\n")
  # cat("RMSFE (Pseudo out-of-sample, últimas 36 observaciones) para t+2:", round(rmsfe_pseudo[2], 6), "\n")
  # cat("RMSFE (Pseudo out-of-sample, últimas 36 observaciones) para t+3:", round(rmsfe_pseudo[3], 6), "\n")
  # cat("Predicción de inflación a t+1:", round(predicciones_result$pred_inflacion[1], 6), "\n")
  # cat("Predicción de inflación a t+2:", round(predicciones_result$pred_inflacion[2], 6), "\n")
  # cat("Predicción de inflación a t+3:", round(predicciones_result$pred_inflacion[3], 6), "\n")
  # cat("====================================================\n\n")
}
pred_t1_dist <- sapply(resultados_prediccion_inflacion, function(v) v[1])
pred_t1_df <- data.frame(
  Comb   = seq_along(pred_t1_dist),
  Pred_t1 = pred_t1_dist
)
pred_t1_df_ord<-order(pred_t1_df$Pred_t1)
# Calcular MSE para cada combinación en errores_t1
error_t1_rmsfe <- sapply(errores_t1, function(x) sqrt(mean(x^2, na.rm = TRUE)))
# Crear data frame con índice original
df_rmsfe <- data.frame(
  indice = seq_along(error_t1_rmsfe),
  rmsfe = error_t1_rmsfe
)

# Ordenar por RMSFE de menor a mayor
df_rmsfe_ordenado <- df_rmsfe[order(df_rmsfe$rmsfe), ]

# Mostrar las top 3 combinaciones con menor error
head(error_t1_ordenado, 3)

#3 combinaciones con mejor rsmfe
# Calcular el promedio de RMSFE para cada combinación
promedios_rmsfe <- sapply(resultados_rmsfe_pseudo, function(x) mean(x, na.rm = TRUE))

# Ordenar las combinaciones por RMSFE promedio
mejores_combinaciones<-data.frame(Combinación= 1:((1+(length(combinacionesk_1)+length(combinacionesk_2)))*iterations),Promedios_RMSFE= promedios_rmsfe)
mejores_combinaciones<- mejores_combinaciones[order(mejores_combinaciones$Promedios_RMSFE),]

df_errors <- data.frame(
  Combinación_1 = resultados_rmsfe_pseudo[[mejores_combinaciones$Combinación[1]]],
  Combinación_2 = resultados_rmsfe_pseudo[[mejores_combinaciones$Combinación[2]]],
  Combinación_3 = resultados_rmsfe_pseudo[[mejores_combinaciones$Combinación[3]]]
)
df_errors<-t(df_errors)

error_means<-colMeans(df_errors)

#print combinaciones con RMSFE de menor a mayor


# # Para acceder a los detalles de de cada combinación: si la mejor predicción es 89, acceda de la siguiente forma:
# # results_normalized$BUSQUE: IDGLOBAL
# 
# #impresión de forecast y errores de top 3 combinaciones
# resultados_prediccion_inflacion[(mejores_combinaciones[1:3,1])] #imprime siempre las predicciones de las top 3 combinaciones
# 
# # Calcular los promedios para t+2, t+3 y t+4
# promedio_t2 <- mean(sapply(resultados_prediccion_inflacion, function(x) x[1]))
# promedio_t3 <- mean(sapply(resultados_prediccion_inflacion, function(x) x[2]))
# promedio_t4 <- mean(sapply(resultados_prediccion_inflacion, function(x) x[3]))
# 
# # Crear el data frame con los promedios
# df_promedios <- t(data.frame(t2 = promedio_t2, t3 = promedio_t3, t4 = promedio_t4))
# 
# # Ver el data frame
# print(df_promedios)

#inputar en excel
library(openxlsx)
evaluacion_excel<-loadWorkbook("C:/Users/victo/OneDrive/Documents/Banxico/evaluacion_factormodel.xlsx")
writeData(evaluacion_excel, sheet = "evaluacion", error_means, startCol = 60, startRow = 245, colNames = FALSE)
saveWorkbook(evaluacion_excel, file= "C:/Users/victo/OneDrive/Documents/Banxico/evaluacion_factormodel.xlsx", overwrite = TRUE)

# mayo del 17 y cada 4, la siguente es sept
