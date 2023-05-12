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
      paste0("data/",x,"_logadI.csv"))
    lagClose <- laggindata(data$Close,ts,th)
    lagCloseI <- laggindata(data$CloseI,ts,th)
    lagCorre <- laggindata(data$correlation,ts,th)
    lagbeta <- laggindata(data$beta,ts,th)
    lagsdC <- laggindata(data$sdC,ts,th)
    lagsdI <- laggindata(data$sdI,ts,th)
    lagr2 <- laggindata(data$r2,ts,th)
    data <- cbind(
      "Date" = data$Date[-c(1:(ts+th-1))],
      lagClose,
      lagCloseI,
      lagCorre,
      lagbeta,
      lagsdC,
      lagsdI,
      lagr2
    )
    write.csv(data, paste0("data/",x,"_loglagadI.csv"),
              row.names = F)
  }) |> 
  invisible()

##Joining lagged data----
ordereddata <- c()
ticks |> 
  lapply(function(x, e = empresas){
    data <- read.csv(
      paste0("data/",x,"_loglagadI.csv"))
    tickid <- which(ticks==x)
    #data <- data |>
    #  filter(X1 != 0 & Y1 !=0)
    data <- cbind(
       data,
       "ID" = rep(tickid, dim(data)[1]),
       sector = rep(e$sector[tickid], dim(data)[1]),
       subsector = rep(e$subsector[tickid], dim(data)[1])
     )
    ordereddata <<- bind_rows(ordereddata,data)
  }) |> 
  invisible()

ordereddata <- ordereddata |> 
  arrange(Date,ID)

write.csv(ordereddata, "data/logreadjwithibxcorr.csv", row.names = F)
plot(model)

