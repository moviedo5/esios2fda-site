
# Example script: Query metadata and sample data resolutions for Wind_551 (ID = 551)

# Load required libraries
# escarga de todos los fichero
library(esios2fd)
## 1. Setup
api_key <- "71a4c4a853b9e2de2e43ebc92e458cf84cab3bfde9b2bc9f33e0949e30239607"
# ## 2. Download Indicator Catalog
inds <- esios2indicators(api_key, 
                        output_dir = "data_indicator")
inds_res <- esios2resolution(
  indicators = inds,
  api_key     = api_key,
  csv_file    = "data/inds_resolution_full2.csv",
  verbose     = TRUE
)
sapply(inds_res[,7:13],table)

inds_res2 <- esios2resolution(
  indicators = inds[c(537:560,1890:1910),],
  api_key     = api_key,
  csv_file    = "data/inds_resolution_full2.csv",
  verbose     = TRUE
)
inds_res2[,c(4,7:ncol(inds_res))]
max(rowSums(inds_res2[,c(8:ncol(inds_res2))]))
sapply(inds_res2[,7:13],table)

vars10m <- inds_res2[inds_res2$res10min==1,]$short_name_en
vars10m
vars5m <- inds_res2[inds_res2$res5min==1,]$short_name_en
vars5m
# vars[-c(1,4,5)] no funcionan

## 3. Fetch Raw Time-Series Data
#' With the short names (e.g. `Wind_551`, `Solar_PV_119`), 
#' download hourly or 10-minute data.
#vars <- c("Wind_551", "Solar_PV_119")
inds_res2[inds_res2$short_name_en =="Coal_547",]
years <- 2024
vars

df5  <- esios2df("Wind_551", "2025-07-01", "2025-07-02", api_key,
                 resolution = "5min", verbose = TRUE)
df5
df10  <- esios2df("Wind_551", "2025-07-01", "2025-07-02", api_key,
                 resolution = "10min", verbose = TRUE)
df10
df60 <- esios2df("Wind_551", "2025-07-01", "2025-07-02", api_key,
                 resolution = "hour", verbose = TRUE)

# 10-minute data for "Solar_PV_119" indicator
res0m <- esios2df(vars[6], "2020-01-01", "2020-01-11",
                  api_key, "five_minutes", F)
ts.plot(res0m$value)

res0m <- esios2df(vars5m[4], "2020-01-01", "2020-01-11",
                  api_key, "min", F)
res0m



esios2csv(vars[6], years, api_key,
          resolution = "min",
          output_dir = "data_csv",
          verbose    = FALSE)


  esios2csv(vars, years, api_key,
          resolution = "hour",
          output_dir = "data_csv",
          verbose    = FALSE)

res0h <- esios2df(vars, "2020-01-01", "2020-12-31",
                  api_key, "hour", F)
names(res0h)
par(mfrow=c(2,1))
ii <- res0h$var_name == vars[1]
plot(res0h$datetime[ii], res0h$value[ii],type="l")
ii <- res0h$var_name == vars[2]
plot(res0h$datetime[ii], res0h$value[ii],type="l")
par(mfrow=c(1,1))
boxplot(value~var_name, data=res0h)

# No 10-minute data for "Solar_PV_119" indicator
res0m <- esios2df(vars, "2020-01-01", "2020-12-31",
                  api_key, "min", F)
plot(res0m$datetime, res0m$value,type="l")


########################


## 4. Convert to Functional Objects
#' Convert the downloaded CSV files into `fdata` objects, 
res1h <- esios2fdata(vars, years,
                     resolution = "hour",
                     input_dir  = "data_csv",
                     output_dir = "data_rda",
                     mode       = "csv",
                     verbose    = F)
# plot(res1$Solar_PV_119_h2018)


res1m <- esios2fdata(vars, years,
                     resolution = "min",
                     input_dir  = "data_csv",
                     output_dir = "data_rda",
                     mode       = "csv",
                     verbose    = F)
names(res1m)
res1m <- esios2fdata(vars[1], years,
                     resolution = "min",
                     input_dir  = "data_csv",
                     output_dir = "data_rda",
                     mode       = "csv",
                     verbose    = F)


## 5. Combine into Unified Dataset
#' Finally, assemble all variables and years into a single `ldata` object.
res2h <- esios2ldata(years, vars,
                    resolution = "hour",
                    input_dir  = "data_rda",
                    mode       = "rda",
                    verbose    = F)
class(res2h)
plot(res2h)
sapply(res2h,dim)
plot(res2h$Wind_551)
plot(res2h$Solar_PV_119)

res2m <- esios2ldata(years, vars[2],
                     resolution = "min",
                     input_dir  = "data_rda",
                     mode       = "rda",
                     verbose    = F)
class(res2m)
plot(res2m)
sapply(res2m,dim)


res2m <- esios2ldata(years, vars,
                     resolution = "min",
                     input_dir  = "data_csv",
                     mode       = "csv",
                     verbose    = F)
class(res2m)
# aquí hay un bug, en la funcion plot.ldata
# plot(res2m,col="red")

sapply(res2m,dim)
plot(res2m$Wind_551)

plot(res2m$Wind_551,col=2)
sapply(res2m,dim)


#####################
res4h <- esios2lfdata(vars, "2020-01-01", "2020-12-31",
                  api_key, "hour", F)
sapply(res4h,dim)
plot(res4h)

res4m <- esios2lfdata(vars, "2020-01-01", "2020-12-31",
                      api_key, "min", F)

sapply(res4m,dim)
plot(res4m)
