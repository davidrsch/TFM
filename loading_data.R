#Libraries----
library(readxl)
library(readr)
library(quantmod)
library(dplyr)
library(zoo)
library(lubridate)
library(roll)

#Loading entities----
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

##pulling ticks----
ticks <- empresas |> 
  select(TICKERS) |> 
  pull()
#Downloading and saving data----
colnames <- c("Open","High","Low","Close","Volume","Adjusted")
ticks |>
  lapply(function(x){
    data <- getSymbols(x,
                       from = "1990-01-01",
                       to = "2023-02-28",
                       auto.assign = F) |> 
      as.data.frame()
    names(data) <- colnames
    data <- cbind(Date = row.names(data), data)
    row.names(data) <- NULL
    #csv
    write.csv(data, paste0("data/",x,".csv"),
              row.names = F)
  }) |> 
  invisible()

#Counting Nas----
##creting counting na df----
nadf <- data.frame(
  "Nas" = rep(NA, length(ticks)),
  row.names = ticks
)
##saving counted nas----
ticks |> 
  lapply(function(x){
    data <- read.csv(
      paste0("data/",x,".csv"))
    NAS <- data |> 
      filter(any(is.na(c(Open,High,Low,Close)))) |> 
      nrow()
    nadf[which(ticks==x),] <<- NAS
  }) |> 
  invisible()
sum(nadf)

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
    write.csv(data, paste0("data/",x,".csv"),
                row.names = F)
  }) |> 
  invisible()

#Saving counted nas (again)----

#Creating and storing----
##closing price----
ticks |> 
  lapply(function(x){
    data <- read.csv(
      paste0("data/",x,".csv"))
    Return <- data |> 
      Cl()
    data <- data.frame(
      "Date" = data$Date,
      "Close" = data$Close
    )
    write.csv(data, paste0("data/",x,"_close.csv"),
              row.names = F)
  }) |> 
  invisible()

#IBEX----
IBEX <- getSymbols("^IBEX",
                   from = "1990-01-01",
                   to = "2023-02-28",
                   auto.assign = F) |>
  as.data.frame()

IBEX <- cbind(Date = rownames(IBEX), Close = IBEX$IBEX.Close)
rownames(IBEX) <- NA

IBEX <- IBEX |>
  na.omit() |>
  as.data.frame() |>
  mutate(
    Date = strftime(Date, "%Y-%m")) |>
  group_by(Date) |>
  summarise(CloseI = last(Close))|>
  mutate(CloseI = as.numeric(CloseI)) |>
  as.data.frame()

#Storing together with IBEX and correlation----
ticks |> 
  lapply(function(x, Ibex = IBEX){
    data <- read.csv(
      paste0("data/",x,"_close.csv"))
    data <- left_join(data, Ibex, by = "Date")
    correlation <- data |>
       apply(1, function(x, bd=data){
         date <- x[1]
         D <- bd |>
           filter(Date <= date)
         corre <- cor(D$Close, D$CloseI)
         return(corre)
     })
    beta <- data |>
      apply(1, function(x, bd=data){
        date <- x[1]
        D <- bd |>
          filter(Date <= date)
        bet <- (cov(D$Close, D$CloseI)/ var(D$CloseI))
        return(bet)
      })
    r2 <- correlation ^ 2
    data <- cbind(data, correlation, beta, r2)
    data <- na.omit(data)
    write.csv(data, paste0("data/",x,"_close.csv"),
               row.names = F)
  }) |> 
  invisible()

