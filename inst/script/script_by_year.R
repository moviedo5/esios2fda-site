library(esios2fd)
source("./R/esios2df.R")
source("./R/esios2csv.R")
source("./R/esios2ldata.R")
source("./R/esios2fdata.R")
source("./R/esios2resolution.R")
source("./R/esios2indicators.R")
source("./R/date2calendar.R")
source("./R/zzz.R")

library(lubridate)
library(fda.usc)
library(dplyr)
library(jsonlite)
library(httr)
## 1. Setup
api_key <- "71a4c4a853b9e2de2e43ebc92e458cf84cab3bfde9b2bc9f33e0949e30239607"

# ## 2. Download Indicator Catalog
# inds <- esios2indicators(api_key, 
#                         output_dir = "data_indicator")
# 
# inds_res <- esios2resolution(
#   indicators = inds,
#   api_key     = api_key,
# #  na.action   = na.omit,
#   csv_file    = "data/inds_resolution.csv",
#   verbose     = TRUE
# )

inds_res <- read.csv("./data/inds_resolution_full2.csv",sep=";")
dim(inds_res)
summary(inds_res)
inds_res[inds_res$res1min==1,4]
inds_res[inds_res$res5min==1,4]
inds_res[inds_res$res10min==1,4]
inds_res[inds_res$res30min==1,4]
inds_res[inds_res$res1hour==1,4]
inds_res[inds_res$res4hour==1,4]
inds_res[inds_res$res1day==1,4]
names(inds_res)
# "1min"  = "minute",
# "5min"  = "five_minutes",
# "10min" = "ten_minutes",
# "15min" = "fifteen_minutes",
# "30min" = "thirty_minutes",
# "1hour" = "hour",
# "4hour" = "four_hours",
# "1day"  = "day"
inds_res[inds_res$short_name_en=="Wind_551",]

# prueba inicial  ( `Wind_551`, `Solar_PV_119`), 

esios2csv("Wind_551", 2020, api_key,
              resolution = "1hour",
              output_dir  = "data_csv2",
              verbose     = TRUE,
              encoding = "UTF-8")
esios2csv("Wind_551", 2020, api_key,
          resolution = "10min",
          output_dir  = "data_csv2",
          verbose     = TRUE,
          encoding = "UTF-8")
esios2csv("Wind_551", 2020, api_key,
          resolution = "15min",
          output_dir  = "data_csv2",
          verbose     = TRUE,
          encoding = "UTF-8")
esios2csv("Wind_551", 2020, api_key,
          resolution = "5min",
          output_dir  = "data_csv2",
          verbose     = TRUE,
          encoding = "UTF-8")

# list.files("data_csv")

# flags <- c("res1min","res5min","res10min","res15min","res30min","res1hour","res4hour","res1day")

# res <- c("1min" ,"5min" ,"10min" ,"15min","30min", "1hour", "4hour" , "1day")

res <- c("5min") #no tenemos sale solo cada 10 min, 15 min y cada hora
## 3. Fetch Raw Time-Series Data
names(inds_res)
years <- c(2018:2024)
for (i in 1){ # i = 12
  print(i)
  ires <- "15min" #res[i]
  print(ires)
  vars <- inds_res[inds_res[,10]==1,]$short_name_en
  print(vars)
 # print(vars[49])  
  esios2csv(vars[-7], years, api_key,
          resolution = ires,
          output_dir  = "data_csv",
          verbose     = T,
          encoding = "UTF-8")
}

## 4. Convert to Functional Objects
#' Convert the downloaded CSV files into `fdata` objects, 
#' handling DST transitions automatically.
var <- "Real_time_CO2_10355"
res1h <- esios2fdata(var, years[5],
                     resolution = "hour",
                     input_dir  = "data_csv",
                     output_dir = "data_rda",
                     mode       = "csv",
                     verbose    = F)
plot(res1h[[1]][1])
res10m <- esios2fdata(var, years[5],
                      resolution = "10m",
                      input_dir  = "data_csv",
                      output_dir = "data_rda",
                      mode       = "csv",
                      verbose    = F)
traceback()
plot(res10m[[1]][1])  # ojo hay dos series

aa<-read.csv2("data_csv/Real_time_CO2_10355_10m2022.csv",sep=";")
plot(aa$val[1:100])
aa<-read.csv2("data_csv/Real_time_CO2_10355_15m2022.csv",sep=";")
plot(aa$val[1:100])

res15m <- esios2fdata(var, years[5],
            resolution = "15m",
            input_dir  = "data_csv",
            output_dir = "data_rda",
            mode       = "csv",
            verbose    = F)
plot(res15m[[1]][1])

res1h[[1]]$data[1,1:2]
res10m[[1]]$data[1,1:12]
res15m[[1]]$data[1,1:8]

res1h[[1]]$data[1:3,1];rowSums(res10m[[1]]$data[1:3,1:6]);rowSums(res15m[[1]]$data[1:3,1:4])



res1 <- esios2fdata(vars, years,
                    resolution = "hour",
                    input_dir  = "data_csv",
                    output_dir = "data_rda",
                    mode       = "csv",
                    verbose    = F)
plot(res1$Solar_PV_119_m2018)
names(res1)
# claramente esto está mal!
# plot(res1$Solar_PV_119_m2018)

## 5. Combine into Unified Dataset
#' Finally, assemble all variables and years into a single `ldata` object.
res2 <- esios2ldata(years, vars,
                     resolution = "hour",
                     input_dir  = "data_rda",
                     mode       = "rda",
                     verbose    = F)

# View common flags
head(ldata$df)

# Plot a variable
plot(ldata$Wind_551)

# quitar todas estas librerias de dentro de la función?
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(fda.usc)
source("./R/esios2csv.R")


esios2csv(vars[1], years, api_key,
          resolution = "hour",
          output_dir  = "data_csv",
          verbose     = FALSE)
#probar para diferentes rango y para diferentes variables y resoluciones (minutales)
# decir que se hace con el cambio horario y los años bisiestos

source("./R/esios_date2ldata.R")
res2 <- esios2ldata(2019, vars,
                    resolution = "hour",
                    input_dir  = "data_rda",
                    mode       = "rda",
                    verbose    = F)
#res2$Solar_PV_119$data[1:2,]
res3 <- esios_date2ldata(
  var_names  = vars,
  start_date = "2019-01-6",
  end_date   = "2019-11-20",
  api_key    = api_key,
  resolution = "hour",
  verbose    = FALSE
)
# 3) Debe dar TRUE exactamente:
all.equal(res2$Wind_551$data[ind,],   res3$Wind_551$data[ind,])
all.equal(res2$Solar_PV_119$data[ind,], res3$Solar_PV_119$data[ind,])

ind <- 1:15+5
ind3 <- 1:15
res2$Wind_551$data[ind,]
res3$Wind_551$data[ind3,]
res2$Wind_551$data[ind,] - res3$Wind_551$data[ind3,]

plot(res3[ind,row=T])
res2$Solar_PV_119$data[ind,]
res3$Solar_PV_119$data[ind3,]
res2$Solar_PV_119$data[ind,1]-res3$Solar_PV_119$data[ind3,1]

res2$Solar_PV_119$data[ind+1,1]-res3$Solar_PV_119$data[ind,1]
res2$Solar_PV_119$data[ind,20]-res3$Solar_PV_119$data[ind,20]
res2$Solar_PV_119$data[1:10,]-res3$Solar_PV_119$data[1:10,]

all.equal(
  res2$Solar_PV_119$data[1:15, ],
  res3$Solar_PV_119$data[1:15, ],
  check.attributes = FALSE
)

plot(res3[297:298,row=T])
res3$Solar_PV_119$data[,1:10]
res2$Solar_PV_119$data[1:297,1]-res3$Solar_PV_119$data[1:297,1]
res2$Solar_PV_119$data[2:298,1]-res3$Solar_PV_119$data[1:297,1]
res2$Solar_PV_119$data[1:298,10]-res3$Solar_PV_119$data[1:298,10]
res2$Solar_PV_119$data[1:298,20]-res3$Solar_PV_119$data[1:298,20]


plot(res2$Solar_PV_119$data[2:298]-res3$Solar_PV_119[2:298]
)
class(res3)
# traceback()
plot(res3)
sapply(res3,dim)
sapply(res3,class)
res1$Solar_PV_119_h2018$data[1:5,1:14]
res2$Solar_PV_119$data[1:5,1:14]
res2$df
res3$df

res1$Solar_PV_119_h2018$data[296:298,1:12]
res3$Solar_PV_119$data[296:298,1:12]


res1$Solar_PV_119_h2018$data[2:298,1]-res3$Solar_PV_119$data[1:297,1]
res1$Solar_PV_119_h2018$data[296:298,1:12]
res3$Solar_PV_119$data[296:298,1:12]
res2$df
plot(res3$Solar_PV_119)
################################################################################
################################################################################
################################################################################
combined <- esios2indicators(api_key, output_dir = "./")
ldat <- esios_date2ldata(
  var_names     = c("Solar_PV_119"),
  start_date    = "2023-01-02",
  end_date      = "2023-01-06",
  api_key       = api_key,
  resolution    = "hour",
  combined_inds = combined,
  verbose       = TRUE
)

rm(ldat)
ldat <- esios_date2ldata(
  var_names  = c("Solar_PV_119"),
  start_date = "2023-01-02",
  end_date   = "2023-01-06",
  api_key    = api_key,
  resolution = "hour",
  verbose    = TRUE
)
ldat$Solar_PV_119$data[,1:4];ldat2$Solar_PV_119[]$data[2:6,1:4]
ldat2 <- esios2ldata(
  years = 2024,
  var_names =  c("Solar_PV_119"),
  resolution = "hour",
  input_dir = "data_rda",
  mode = "rda",
  verbose = TRUE
)
ldat$Solar_PV_119$data[,1:4];ldat2$Solar_PV_119[]$data[2:6,1:4]
(ldat$Solar_PV_119-ldat2$Solar_PV_119[2:6])$data
ldat$df;ldat2$df[1:5,]
plot(ldat$Solar_PV_119)
plot(ldat2$Solar_PV_119)
ldat

(res$Solar_PV_119_h2024[1:5] - ldat$Solar_PV_119)
ldat2$Solar_PV_119[1:5]
ldat$df
tracebacldat$Solar_PV_119$argvals
sapply(ldat,dim)
k()
ldat$df
dim(ldat$Solar_PV_119$data)


api_key <- "71a4c4a853b9e2de2e43ebc92e458cf84cab3bfde9b2bc9f33e0949e30239607"

library(httr)
library(lubridate)
library(esios2fd)
library(dplyr)
library(jsonlite)
library(fda.usc)
args(esios_date2ldata)

ldat <- esios_date2ldata(
  var_names  = c("Solar_PV_119"),
  start_date = "2024-01-01",
  end_date   = "2024-01-05",
  api_key    =  api_key,
  resolution = "hour",
  verbose    = TRUE
)


# combined <- esios2indicators(api_key, output_dir = "./")
ldat <- esios_date2ldata(
  var_names     = c("Solar_PV_119"),
  start_date    = "2024-01-01",
  end_date      = "2024-01-05",
  api_key       = api_key,
  resolution    = "hour",
  combined_inds = combined,
  verbose       = TRUE
)

sapply(ldat,dim)
sapply(ldat,class)
ldat$Wind_551$data
class(ldat)
plot(ldat)
plot(ldat$Solar_PV_119)
rm(ldat2)
ldat2 <- esios2ldata(
  years = 2024,
  var_names =  c("Wind_551","Solar_PV_119"),
  resolution = "hour",
  input_dir = "data_rda",
  mode = "rda",
  verbose = TRUE
)
ldat2$Solar_PV_119$data[1:3,]

#plot(ldat2)
plot(ldat2$Solar_PV_119[1:5])
plot(ldat$Solar_PV_119)
plot(ldat2$Solar_PV_119[1:5] - ldat$Solar_PV_119)
(ldat2$Solar_PV_119[1:5] - ldat$Solar_PV_119)

res <- esios2fdata( years = 2024,
                    var_names =  c("Wind_551","Solar_PV_119"),
                    resolution = "hour",
                    input_dir  = "data_csv",
                    output_dir = "data_rda",
                    mode       = "csv",
                    verbose    = TRUE)
(res$Solar_PV_119_h2024[1:5] - ldat$Solar_PV_119)



D:/GitHub/esios2fda
# 1) Establecer ruta al directorio CSV que tu esios2csv imprimió:
csv_dir <- "D:/GitHub/esios2fda/data_csv"

# 2) Leer el CSV de 2023 para Solar_PV_119
file <- file.path(csv_dir, "Solar_PV_119_h2023.csv")
df_all <- read.csv2(file, stringsAsFactors = FALSE)

# 3) Mostrar los valores de medianoche (00:00) y la hora siguiente
midnight_vals <- df_all[df_all$instant == "2023-01-02 00:00:00", c("instant","val")]
h1_vals       <- df_all[df_all$instant == "2023-01-02 01:00:00", c("instant","val")]
print(midnight_vals)
print(h1_vals)

# 4) Pivot manual de esas dos filas:
library(tidyr)
df_sub <- df_all[df_all$instant %in% c("2023-01-02 00:00:00","2023-01-02 01:00:00"), ]
df_sub$day  <- as.Date(df_sub$instant)
df_sub$hour <- as.integer(format(strptime(df_sub$instant, "%Y-%m-%d %H:%M:%S"), "%H"))
wide <- pivot_wider(
  df_sub,
  id_cols     = day,
  names_from  = hour,
  values_from = val,
  values_fill = list(val = 0)
)
print(wide)


ldat <- esios_date2ldata(
  var_names  = c("Solar_PV_119"),
  start_date = "2025-07-10",
  end_date   = "2025-07-10",
  api_key    = api_key,
  resolution = "hour",
  verbose    = TRUE
)
####################### CODIGO A PELO ##############################
# Paquetes necesarios
library(httr)
library(jsonlite)
library(lubridate)

# Parámetros
var    <- "Real_time_CO2_10355"
var    <- "Wind_551"
#api_key <- "TU_API_KEY_AQUÍ"  # pon tu clave
tz      <- "Europe/Madrid"
year    <- 2022

# Extraer ID de la variable
id <- as.integer(sub(".*_(\\d+)$", "\\1", var))

# Definir rango completo del año
start_dt <- as.POSIXct(sprintf("%d-01-01 00:00:00", year), tz = tz)
end_dt   <- as.POSIXct(sprintf("%d-01-03 23:59:59", year), tz = tz)

# Función helper para formatear a UTC ISO
to_iso_utc <- function(dt) {
  format(with_tz(dt, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
}

# Llamada a la API
resp <- GET(
  sprintf(
    "https://api.esios.ree.es/indicators/%d?start_date=%s&end_date=%s&time_trunc=fifteen_minutes&locale=es",
   # "https://api.esios.ree.es/indicators/%d?start_date=%s&end_date=%s&time_trunc=ten_minutes&locale=es",
    id,
    to_iso_utc(start_dt),
    to_iso_utc(end_dt)
  ),
  add_headers(
    Accept    = "application/json; application/vnd.esios-api-v1+json",
    `x-api-key` = api_key
  ),
  timeout(120)
)
stop_for_status(resp)

# Parsear JSON y extraer la tabla de valores
j <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
vals_raw <- j$indicator$values
vals_raw
# Convertir a data.frame y ver nombres de columnas
df_raw <- as.data.frame(vals_raw, stringsAsFactors = FALSE)
print(names(df_raw))
#   por ejemplo: "datetime", "value", "max_value", "min_value", "geocode", "geolabel"

# Convertir datetime a POSIXct en Europa/Madrid
df_raw$datetime <- ymd_hms(df_raw$datetime, tz = "UTC") %>% with_tz(tz)

# Mostrar las primeras filas (todas las columnas)
head(df_raw, 20)
plot(df_raw$datetime,       df_raw$value,type="l")
plot(   df_raw$value[seq(1,(4*24*3),by=2)],type="l",ylim=c(3000,20000))
lines(   df_raw$value[seq(1,(4*24*3),by=2)+1],type="l",col="red")

df_raw[1:4,]
Agrupar en
La agrupación en periodos temporales superiores a la resolución utilizada por defecto para la publicación de indicadores de precios, capacidad y potencia se realizará haciendo la media aritmética, salvo indicación expresa en otro sentido. Para el resto de indicadores, se realizará haciendo suma.
realmente en la web este indicador no tiene resolucion ni 5 minutal ni 15 minutal!
  GENERACIÓN T.REAL EÓLICA
Generación medida en tiempo real del tipo de producción eólica.

Los datos representados en este indicador se refieren a datos Peninsulares.

Publicación: cada 5 minutos con la información de las tres últimas horas del día D-1 hasta la hora actual del día D.
