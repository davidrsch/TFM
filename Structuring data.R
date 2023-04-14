# Structuring data for supervised problem----
##Setting parameters----
timesteps <- 1
timehorizon <- 1

##Lagging data function----
laggindata <- function(data, ts, th) {
  totals <- 0:(ts+th)
  datanew <-c()
  namey <- c()
  for (x in totals) {
    y <- lag(data,x)
    stepp <- ts+th-x
    if(stepp > ts){
      namey <- c(paste0("Y",(th-x)),namey)
    }else{
      namey <- c(paste0("X",stepp),namey)
    }
    datanew <- cbind(y,datanew)
  }
  datanew <- as.data.frame(datanew)
  names(datanew) <- namey
  datanew <- datanew[-c(1:(timesteps+timehorizon-1)),-1]
  return(datanew)
}

###Lagging data----
####Adjusted data----
ticks |> 
  lapply(function(x,ts = timesteps, th = timehorizon){
    data <- read.csv(
      paste0("data/",x,"_adjclose.csv"))
    lagClose <- laggindata(data$Close,ts,th)
    lagCloseI <- laggindata(data$CloseI,ts,th)
    lagCorre <- laggindata(data$correlation,ts,th)
    lagbeta <- laggindata(data$beta,ts,th)
    lagr2 <- laggindata(data$r2,ts,th)
    data <- cbind(
      "Date" = data$Date[-c(1:(ts+th-1))],
      lagClose,
      lagCloseI,
      lagCorre,
      lagbeta,
      lagr2
    )
    write.csv(data, paste0("data/",x,"_lagadjclose.csv"),
              row.names = F)
  }) |> 
  invisible()
####Close----
ticks |> 
  lapply(function(x,ts = timesteps, th = timehorizon){
    data <- read.csv(
      paste0("data/",x,"_close.csv"))
    lagClose <- laggindata(data$Close,ts,th)
    lagCloseI <- laggindata(data$CloseI,ts,th)
    lagCorre <- laggindata(data$correlation,ts,th)
    lagbeta <- laggindata(data$beta,ts,th)
    lagr2 <- laggindata(data$r2,ts,th)
    data <- cbind(
      "Date" = data$Date[-c(1:(ts+th-1))],
      lagClose,
      lagCloseI,
      lagCorre,
      lagbeta,
      lagr2
    )
    write.csv(data, paste0("data/",x,"_lagclose.csv"),
              row.names = F)
  }) |> 
  invisible()

##Joining lagged data----
ordereddata <- c()
ticks |> 
  lapply(function(x, e = empresas){
    data <- read.csv(
      paste0("data/",x,"_lagadjclose.csv"))
    tickid <- which(ticks==x)
    #data <- data |>
    #  filter(X1 != 0 & Y1 !=0)
    data <- cbind(
       data,
       "ID" = rep(tickid,dim(data)[1]),
       sector = e$sector[tickid],
       subsector = e$subsector[tickid]
     )
    ordereddata <<- bind_rows(ordereddata,data)
  }) |> 
  invisible()

ordereddata <- ordereddata |> 
  arrange(Date,ID)

logordereddata <- cbind(
  "Date" = ordereddata$Date,
  log(ordereddata[,-c(1,dim(ordereddata)[2])]),
  "ID" = ordereddata$ID
)

write.csv(ordereddata, "data/readjwithibxcorr.csv", row.names = F)
