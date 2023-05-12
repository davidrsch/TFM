#Loading required libraries----
library(readxl)
library(readr)
library(quantmod)
library(zoo)
library(lubridate)
library(roll)
library(dplyr)

# Loading companies table----
#   In the process the companies table is loaded, column 
#   "SECTOR-SUBSECTOR" is separated in individuals columns
#   called sector and subsector. This columns are transformed to factors.

empresas <- read_excel("data/000_empresas.xlsx")
empresas <- empresas |>
  rowwise() |> 
  mutate(
    sector = strsplit(`SECTOR-SUBSECTOR`,"-")[[1]][1],
    subsector = strsplit(`SECTOR-SUBSECTOR`,"-")[[1]][2]) |> 
  mutate(
    sector = as.factor(sector),
    subsector = as.factor(subsector)
  )
levels(empresas$sector) <- 1:length(levels(empresas$sector))
levels(empresas$subsector) <- 1:length(levels(empresas$subsector))

## Pulling ticks----
#   The ticks of the companies are extracted to make easier 
#   retrieve of the data.
   
ticks <- empresas |> 
  select(TICKERS) |> 
  pull()

# Downloading and saving data----
#   Download data from yahoo finace and save the individual data in
#   .csv files in da/...

colnames <- c("Open","High","Low","Close","Volume","Adjusted")
ticks |>
  lapply(function(x, coln = colnames){
    data <- getSymbols(x,
                       from = "1990-01-01",
                       to = "2023-02-28",
                       auto.assign = F) |> 
      as.data.frame()
    names(data) <- colnames
    data <- cbind(Date = row.names(data), data)
    row.names(data) <- NULL
    write.csv(data, paste0("data/",x,".csv"),
              row.names = F)
  }) |> 
  invisible()

# Check for missing values----
## Creting NA data frame----
nadf <- ticks |> 
  lapply(function(x){
    data <- read.csv(
      paste0("data/",x,".csv"))
    NAS <- data |> 
      filter(any(is.na(c(Open,High,Low,Close)))) |> 
      nrow()
    #nadf[which(ticks==x),] <<- NAS
  }) |> 
  invisible()
sum(as.data.frame(nadf))

#Creating monthly data----
ticks |> 
  lapply(function(x){
    data <- read.csv(
      paste0("data/",x,".csv")) |> 
      na.omit()
    data$Date <- strftime(data$Date, "%Y-%m")
    Open <- data[,2] |> 
      aggregate(by=list(data$Date), first)
    High <- data[,3] |> 
      aggregate(by=list(data$Date), max)
    Low <- data[,4] |> 
      aggregate(by=list(data$Date), min)
    Close <- data[,5] |> 
      aggregate(by=list(data$Date), last)
    Volume <- data[,6] |> 
      aggregate(by=list(data$Date), sum)
    Adjusted <- data[,7] |> 
      aggregate(by=list(data$Date), last)
    data <- data.frame(
      "Date" = Open[,1],
      "Open" = Open[,2],
      "High" = High[,2],
      "Low" = Low[,2],
      "Close" = Close[,2],
      "Volume" = Volume[,2],
      "Adjusted" = Adjusted[,2])
    write.csv(data, paste0("data/",x,"m.csv"),
                row.names = F)
  }) |> 
  invisible()

## Creting NA data frame----
nadf <- ticks |> 
  lapply(function(x){
    data <- read.csv(
      paste0("data/",x,"m.csv"))
    NAS <- data |> 
      filter(any(is.na(c(Open,High,Low,Close)))) |> 
      nrow()
    #nadf[which(ticks==x),] <<- NAS
  }) |> 
  invisible()
sum(as.data.frame(nadf))

#Creating and storing adjusted close price----
ticks |> 
  lapply(function(x){
    data <- read.csv(
      paste0("data/",x,"m.csv"))
    Close <- data |> 
      Ad()
    data <- data.frame(
      "Date" = data$Date,
      "Close" = Close
    )
    write.csv(data, paste0("data/",x,"_ad.csv"),
              row.names = F)
  }) |> 
  invisible()

#Downloading IBEX data----
IBEX <- getSymbols("^IBEX",
                   from = "1990-01-01",
                   to = "2023-02-28",
                   auto.assign = F) |>
  as.data.frame()

IBEX <- cbind(
  Date = rownames(IBEX),
  Close = IBEX$IBEX.Close) |>
  as.data.frame()

IBEX <- IBEX |>
  na.omit() |>
  as.data.frame() |>
  mutate(
    Date = strftime(Date, "%Y-%m")) |>
  group_by(Date) |>
  summarise(CloseI = last(Close))|>
  mutate(CloseI = as.numeric(CloseI)) |>
  as.data.frame()

write.csv(IBEX, "data/IBEXm.csv",
          row.names = F)
#Storing together with IBEX and correlation----
# In addition the computation for the calculation of 
# the correlation, beta, standard deviation and R^2 is done
ticks |> 
  lapply(function(x, Ibex = IBEX){
    data <- read.csv(
      paste0("data/",x,"_ad.csv"))
    data <- left_join(data, Ibex, by = 'Date')
    data <- data |>
      mutate(
        Close = Delt(log(data$Close)),
        CloseI = Delt(log(data$CloseI))
        ) |>
      na.omit()
    compu <- data |>
      apply(1, function(x, bd=data){
        date <- x[1]
        D <- bd |>
          filter(Date <= date)
        correlation <- cor(D$Close, D$CloseI)
        beta <- (cov(D$Close, D$CloseI)/ var(D$CloseI))
        sdC <- sd(D$Close)
        sdI <- sd(D$CloseI)
        r2 <- correlation ^ 2
        cmp <- cbind(correlation, beta, sdC, sdI, r2)
        return(cmp)
      }) |> 
      t()|> 
      as.data.frame()
    names(compu) <- c(
      "correlation", "beta", "sdC", "sdI", "r2")
    data <- cbind(data, compu)
    data <- na.omit(data)
    write.csv(data, paste0("data/",x,"_logadI.csv"),
               row.names = F)
  }) |> 
  invisible()

