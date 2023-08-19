# Creating ANN model an evaluating it process----

## Loading required libraries----
library(abind)
library(keras)
library(Metrics)
library(readr)
library(scales)
library(tidyr)
library(tensorflow)
library(tseries)
library(lubridate)
library(dplyr)
library(quadprog)
library(Matrix)
library(tibble)

## Loading data----
#load("F:/APPs/TFM/data/data.Rdata")
#load("F:/APPs/TFM/data/modeling.Rdata")
load("data/results.Rdata")

data <- bind_rows(vecs3d2e)
data <- data  |>
  arrange(Date)
inputsinfo <- data|>
  select(inputs) |>
  pull() |>
  dim()
outputsinfo <- data|>
  select(outputs) |>
  pull() |>
  dim()

## Building model ----
### Parameters ----
steps <- inputsinfo[2]
tsperset <- inputsinfo[3]
thsteps <- outputsinfo[2]

### Model structure ----
#### Input layers----
inp <- layer_input(
  shape = c(NULL,steps,tsperset))
#### Hidden layers----
cnn <- inp |>
  layer_conv_1d(
    filters = 64,
    kernel_size = 1,
    activation = layer_activation_leaky_relu())
lstm <- cnn |>
  layer_lstm(64)
#### Output layers----
out <- lstm |> 
  layer_dense(
    thsteps*1)
### Building model---- 
model <- keras_model(inp, out)
#### Compiling model ----
model |> 
  compile(loss = "mse", optimizer = optimizer_sgd(0.0005))
#### Summary ----  
model

model |>
  getconfig() |>
  plot_modelk() |>
  grViz()

## Testing model----
predictions <- c()
means <- c()
listdates <- data |>
  select(Date) |>
  pull() |>
  unique()

for (i in 1:length(listdates)) {
  d <- listdates[i]
  inputs <- data |>
    filter(Date == d) |>
    select(inputs) |>
    pull()
  inputspred <- data |>
    filter(Date == d) |>
    select(inputs) |>
    pull()
  outputs <- data |>
    filter(Date == d) |>
    select(outputs) |>
    pull()
  outputs <- outputs[,,1]
  
  ids <- data |> 
    filter(Date == d) |>
    select(ID) |>
    pull()
  
  if(i==1){
    dfmeans <- inputspred[,,1] |>
      as.data.frame() |>
      cbind(ID = ids)
  }else{
    dfmeansupd <- inputspred[,dim(inputs)[2],1] |>
      as.data.frame() |>
      cbind(ID = ids)
    names(dfmeansupd)[1] <- paste0("V",(dim(dfmeans)[2]))
    idsdf <- unique(c(ids, dfmeans$ID))
    idsdf <- data.frame(
      ID = idsdf
    )
    dfmeansupd <- dplyr::left_join(idsdf, dfmeansupd, by = "ID")
    ifelse(
      dim(dfmeansupd)[1] > dim(dfmeans)[1],
      dfmeans <- dplyr::left_join(dfmeansupd, dfmeans, by = "ID"),
      dfmeans <- dplyr::left_join(dfmeans, dfmeansupd, by = "ID")
    )
  }
  
  if(i > 1){
    pred <- model |> predict(
      list(inputspred))
    if(length(which(pred==0)>10)){
      stop()
    }
    predictions <- rbind(predictions, pred)
    MEANS <-  dfmeans |>
      rowwise() |>
      mutate(
        means = mean(c_across(-ID), na.rm = T)
      ) |>
      slice(
        match(ids,ID)
      ) |>
      pull(means) |>
      as.matrix()
    means <- rbind(means, MEANS)
  }
  
  ## Training the model ----
  model |>
    fit(
      inputs,
      outputs,
      epochs = 1,
      batch_size = 10,
      shuffle = F,
      verbose = 0
    )
}

## Evaluating ----
#### Values that performed badly ----
y1 <- data |>
  filter(
    Date > listdates[1]
  ) |>
  select(outputs) |>
  pull()
y1 <- y1[,,1]
data |>
  filter(
    Date > listdates[1]
  ) |>
  cbind(predictions = predictions[,1]) |>
  cbind(means = means) |>
  cbind(Y1 = y1) |>
  group_by(ID) |>
  summarise(
    mse_model = mse(predictions, Y1),
    mse_actmeans = mse(means, Y1),
    mse_generalmean = mse(mean(Y1), Y1),
    r2_vsactmean = 1 - (sum((Y1 - predictions)^2)/sum((Y1 - means)^2)),
    r2_vsgmean = 1 - (sum((Y1 - predictions)^2)/sum((Y1 - mean(Y1))^2))
  ) |>
  dplyr::filter(r2_vsactmean < 0)

#### Mean R^2 model----
arsqrd <- data |>
  filter(
    Date > listdates[1]
  ) |>
  cbind(predictions = predictions[,1]) |>
  cbind(means = means) |>
  cbind(Y1 = y1) |>
  group_by(ID) |>
  summarise(
    mse_model = mse(predictions, Y1),
    mse_actmeans = mse(means, Y1),
    mse_generalmean = mse(mean(Y1), Y1),
    r2_vsactmean = 1 - (sum((Y1 - predictions)^2)/sum((Y1 - means)^2)),
    r2_vsgmean = 1 - (sum((Y1 - predictions)^2)/sum((Y1 - mean(Y1))^2))
  ) |>
  ungroup() |>
  select(r2_vsactmean) |>
  pull() |>
  mean()


Yhat <- data |>
  left_join(IBEXsel, by ="Date") |>
  mutate(IBEX = Return_I) |>
  arrange(Date) |>
  filter(
    Date > listdates[1]
  ) |>
  mutate(yhat = predictions[,1]) |>
  mutate(
    Real = y1,
    Yhat = yhat,
    Means = means
  ) |>
  select(Date, Real, IBEX, Yhat, Means, ID)

pvtYhat <- Yhat |>
  select(Date, Yhat, ID) |>
  pivot_wider(
    names_from = ID,
    values_from = Yhat
  )

pvtReal <- Yhat |>
  select(Date, Real, ID) |>
  pivot_wider(
    names_from = ID,
    values_from = Real
  )

pvtMeans <- Yhat |>
  select(Date, Means, ID) |>
  pivot_wider(
    names_from = ID,
    values_from = Means
  )

weightse <- data.frame()
weightsm <- data.frame()

for (i in 1:length(listdates[-1])) {
  if(i>1){
    
    dataeQP <- pvtReal |>
      filter(Date < listdates[-1][i]) |>
      rbind(pvtYhat |>
              filter(Date == listdates[-1][i])
            )
    datamQP <- pvtReal |>
      filter(Date < listdates[-1][i]) |>
      rbind(pvtMeans |>
              filter(Date == listdates[-1][i])
      )
    nare <- which(is.na(dataeQP[dim(dataeQP)[1],]))
    naremo <- which(is.na(dataeQP[(dim(dataeQP)[1]-1),]))
    nare <- c(nare,naremo)
    nare <- unique(nare)
    if(length(nare) != 0){
      carterae <- dataeQP[, - nare]
      carteram <- datamQP[, - nare]
    }else{
      carterae <- dataeQP
      carteram <- datamQP
    }
    returne <- carterae[dim(carterae)[1], -1] |>
      as.matrix() |>
      t()
    returnm <- carteram[dim(carteram)[1], -1] |>
      as.matrix() |>
      t()
    covme <- cov(carterae[, -1], use = "complete.obs")
    npcovme <- nearPD(covme)$mat |> 
      as.matrix()
    covmm <- cov(carteram[, -1], use = "complete.obs")
    npcovmm <- nearPD(covmm)$mat |> 
      as.matrix()
    n <- ncol(npcovme)
    
    qp_oute <- solve.QP(
      Dmat = 2*npcovme,
      dvec = rep(0,n),
      Amat = cbind(-1, diag(n)),
      bvec = c(-1, rep(0,n)),
      meq = 1)
    qp_outm <- solve.QP(
      Dmat = 2*npcovmm,
      dvec = rep(0,n),
      Amat = cbind(-1, diag(n)),
      bvec = c(-1, rep(0,n)),
      meq = 1)
    qp_oute <- qp_oute$solution
    qp_oute <- floor(qp_oute*100)/100
    qp_outm <- qp_outm$solution
    qp_outm <- floor(qp_outm*100)/100
    for(j in 1:length(qp_oute)){
      if(qp_oute[j] < 0.001){
        qp_oute[j] <- 0
      }else{}
      if(qp_outm[j] < 0.001){
        qp_outm[j] <- 0
      }else{}
    }
    names(qp_oute) <- names(carterae[, -1])
    names(qp_outm) <- names(carteram[, -1])
    weightse <- bind_rows(weightse, qp_oute)
    weightsm <- bind_rows(weightsm, qp_outm)
  }
}

pvtReal <- pvtReal |>
  select(-Date)

pvtReal[is.na(pvtReal)] <- 0
weightse[is.na(weightse)] <- 0
weightsm[is.na(weightsm)] <- 0
resultscnnlstmssing2[["resultspvtReal"]] <- pvtReal

returndfe <- pvtReal[-1,] * weightse
returndfe <- rowSums(returndfe)
returndfm <- pvtReal[-1,] * weightsm
returndfm <- rowSums(returndfm)
mg <- 1
returndf <- data.frame(
  Date = listdates[-c(1:mg)],
  Portre = c(1,returndfe[mg:length(returndfe)]),
  Means = c(1,returndfm[mg:length(returndfm)]),
  IBEX = c(1, IBEXsel |>
    filter(Date > listdates[mg+1]) |>
    select(2) |>
    pull())
) |>
  rename(
    IBEX = 4
  )
resultscnnlstmssing2[["resultsMEANS"]] <- returndf$Means

returndf <- returndf |>
  mutate(
    Portreinv = cumsum(Portre),
    Meansinv = cumsum(Means),
    IBEXinv = cumsum(IBEX)
  )

library(ggplot2)
returndf |>
  mutate(
    Date = as.Date(Date)) |>
  ggplot(aes(x = Date, group = 1)) +
  geom_line(aes(y = Portreinv), color = "blue") +
  geom_line(aes(y = Meansinv), color = "green") +
  geom_line(aes(y = IBEXinv), color = "red") +
  theme(axis.text.x = element_text(angle = 90))

returndf$Portreinv[212-mg+1]-returndf$Meansinv[212-mg+1]

#names(resultscnnlstmssing1$resultsweightse) <- c("001","002","003",
#                                          "004","005","006",
#                                          "007","008","009",
#                                          "010")

try <- 10
if(try<10){
  try <- paste0("00",try)
}else if(try>10 && try<100){
  try <- paste0("0",try)
}

tryresults <- data |>
  filter(Date > listdates[1]) |>
  cbind(predictions = predictions) |>
  cbind(means = means) |>
  mutate(Y1 = y1) |>
  select(Date, predictions, means, Y1) |>
  group_by(Date) |>
  summarise(
    rsqrd = 1 - (sum((Y1 - predictions)^2)/sum((Y1 - means)^2)),
    mse = mse(predictions, Y1),
  ) |>
  left_join(returndf, by = 'Date') |>
  select(Date, Portre, IBEX, rsqrd, mse)

resultscnnlstmssing3$resultsD[[try]] <- tryresults

tryresultsID <- data |>
  filter(Date > listdates[1]) |>
  cbind(predictions = predictions) |>
  cbind(means = means) |>
  mutate(Y1 = y1) |>
  select(Date, predictions, means, Y1, ID) |>
  group_by(ID) |>
  summarise(
    rsqrd = 1 - (sum((Y1 - predictions)^2)/sum((Y1 - means)^2)),
    mse = mse(predictions, Y1)
  ) |>
  select(ID, rsqrd, mse)

resultscnnlstmssing3$resultsID[[try]] <- tryresultsID

trypredictions <- pvtYhat |>
  mutate_if(is.double, as.double)
trypredictions
resultscnnlstmssing3$resultspredictions[[try]] <- trypredictions

tryweightse <- tryresults |>
  filter(Date > Date[1]) |>
  select(Date) |>
  cbind(weightse)

resultscnnlstmssing3$resultsweightse[[try]] <- tryweightse

save(
  resultscnnlstmssing1,
  resultscnnlstmssing2,
  resultscnnlstmssing3,
  file = "data/results.Rdata"
)

 # resultscnnlstmssing3 <- list()
 # resultscnnlstmssing3[["resultsD"]] <- list()
 # resultscnnlstmssing3[["resultsID"]] <- list()
 # resultscnnlstmssing3[["resultspredictions"]] <- list()
 # resultscnnlstmssing3[["resultsweightse"]] <- list()
 # resultscnnlstmssing3[["resultsweightsm"]] <- weightsm

