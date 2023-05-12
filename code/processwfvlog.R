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

#####
library(quantmod)

## Loading data----
data <- read.csv("data/logreadjwithibxcorr.csv")
names(data) <- c(
  "Date", "X1", "Y1", "X2", "Y2", "X3", "Y3", "X4", "Y4",
  "X5", "Y5", "X6", "Y6", "X7", "Y7", "ID", "sector", "subsector")
eqlen <- data |>
  group_by(ID) |>
  count(ID) |>
  filter(n == 275) |>
  filter(ID != 25) |>
  select(ID) |>
  pull()
data <- data |>
  filter(is.element(ID, eqlen)) |>
  mutate(Y0 = if_else(Y1 < 0,0,1))

## Setting parameters ----
timesteps = 1
timehorizon = 1
data <- data |>
  arrange(Date)
datan <- data
datan <- datan |>
  group_by(ID) |>
  mutate(
    means = cummean(X1))
datat <- datan |>
  ungroup()# |>
listdates <- datat |>
  select(Date) |>
  pull() |>
  unique()
#set_random_seed(12.3)

## Building model ----
### Parameters ----
steps <- 1
tsperset <- 7
thsteps <- 1

### Model structure ----
#### Input layers----
inp <- layer_input(
  shape = c(NULL,steps,tsperset))# |>
 #layer_dropout(0.8)
inp2 <- layer_input(
  shape = c(NULL,steps,4))# |>
  #layer_dropout(0.8)

#### Hiden layers----
cnn <- inp |>
  layer_conv_1d(
    filters = 64,
    kernel_size = 1,
    #kernel_regularizer = regularizer_l1(0.01),
    #bias_regularizer = regularizer_l1(0.4),
    #activity_regularizer = regularizer_l1(0.4),
    activation = layer_activation_selu()) |>
  #layer_dropout(0.8) |>
  layer_dense(
    1)
hidd <- cnn |>
  layer_concatenate(inp2) |>
  layer_lstm(
    64,
    activation = "tanh")

#### Output layers----
out <- hidd |>
  layer_dense(thsteps*1)

### Building model----
model <- keras_model(c(inp, inp2),c(cnn, out))

#### Compiling model ----
model |>
  compile(loss = "mse", optimizer = optimizer_adam())

#### Summary ----
model

## Testing model----
predictions <- c()
nopredm <- 36

for(i in 1:length(listdates)) {
  datatrain <- datat |>
    filter(Date <= listdates[i])
  np <- datat |>
    filter(Date == listdates[i])
  np <- dim(np)[1]
  ### Creating inputs and outputs for train----
  input_train <- datatrain |>
    select(contains("X")) |>
    as.matrix()
  dim(input_train) <- c(dim(input_train)[1],timesteps,dim(input_train)[2])
  sector <- datatrain |>
    #  mutate(sector = log(sector)) |>
    select(sector)
  subsector <- datatrain |>
    #  mutate(subsector = log(subsector)) |>
    select(subsector)
  #sectors <- abind(sector, subsector, along = 3)
  #input_train <- abind(input_train, sectors, along = 3)
  output_signt <- datatrain |>
    select(Y0) |>
    as.matrix()
  dim(output_signt) <- c(dim(output_signt)[1],timehorizon,dim(output_signt)[2])
  output_train <- datatrain |>
    select(Y1) |>
    as.matrix()
  if(i > nopredm){
    diminp <- dim(input_train)[1]
    top <- (diminp-np+1):diminp
    pred <- model |> predict(
      list(
        input_train[top,,,drop = F],
        input_train[top,, c(1:4), drop=F]
        ))
    pred <- pred[[2]]
    predictions <- rbind(predictions, pred)
    }
## Training the model ----
model |>
  fit(
    x = list(
      input_train,
      input_train[,, c(1:4), drop=F]
      ),
    y = list(
      output_signt,
      output_train
      ),
    epochs = 1,
    batch_size = 8,
    shuffle = F,
    verbose = 0)
}

## Evaluating ----
#### Values that performed badly ----
datat |>
  filter(Date > listdates[nopredm]) |>
  cbind(predictions = predictions) |>
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
arsqrd <- datat |>
  filter(Date > listdates[nopredm]) |>
  cbind(predictions = predictions) |>
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

Yhat <- datat |>
  filter(Date > listdates[nopredm]) |>
  mutate(yhat = as.vector(predictions)) |>
  mutate(
    Real = Y1,
    IBEX = Y2,
    Means = means,
    Yhat = yhat
    ) |>
  select(Date, Real, IBEX, Yhat, Means, ID)

realdj <- read.csv("data/ad.csv")
sttd <- realdj |>
  filter(Date < Yhat$Date[1]) |>
  tail(1) |>
  select(Date) |>
  pull()
# end <- realdj |>
#   tail(1) |>
#   select(Date) |>
#   pull()
realdj <- realdj |>
  filter(is.element(ID, eqlen)) |>
  filter(Date >= sttd)

Yhat2 <- Yhat
for(i in 1:length(unique(realdj$ID))){
  logyhat <- Yhat |>
    filter(ID == unique(realdj$ID)[i]) |>
    select(Yhat) |>
    pull()
  logmean <- Yhat |>
    filter(ID == unique(realdj$ID)[i]) |>
    select(Means) |>
    pull()
  realval <- realdj |>
    filter(ID == unique(realdj$ID)[i]) |>
    select(Close) |>
    pull()
  logmrealval <- realval[-length(realval)]
  yhatval <- exp(Delt(logyhat)[2] * log(logmrealval) + log(logmrealval))
  meansval <- exp(Delt(logmean)[2] * log(logmrealval) + log(logmrealval))
  yhatreturns <- ((logmrealval - yhatval)/(logmrealval))
  meanreturns <- ((logmrealval - meansval)/(logmrealval))
  returns <- Delt(realval)[-1]
  piece <- Yhat2 |>
    filter(ID != unique(realdj$ID)[i])
  returnsdf <- Yhat |>
    filter(ID == unique(realdj$ID)[i]) |>
    mutate(
      Yhat = yhatreturns,
      Means = meanreturns,
      Real = returns) 
  Yhat2 <-  rbind(piece,returnsdf)
  names(Yhat2) <- names(Yhat)
}

pvtYhat <- Yhat2 |>
  select(Date, Yhat, ID) |>
  pivot_wider(
    names_from = ID,
    values_from = Yhat
  )

pvtReal <- Yhat2 |>
  select(Date, Real, ID) |>
  pivot_wider(
    names_from = ID,
    values_from = Real
  )

pvtMeans <- Yhat2 |>
  select(Date, Means, ID) |>
  pivot_wider(
    names_from = ID,
    values_from = Means
  )

noeval <- listdates[(nopredm + 1)]

for(i in 1:length(listdates[-c(1:nopredm)])){
  x <- listdates[-c(1:nopredm)][i]
  
  if(x != noeval){
    dataeQP <- pvtReal |>
      filter(Date < x) |>
      rbind(pvtYhat |>
              filter(Date == x)
      )
    
    datamQP <- pvtReal |>
      filter(Date < x) |>
      rbind(pvtMeans |>
              filter(Date == x)
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
    return <- carterae[dim(carterae)[1],-1] |> 
      t() |> 
      as.vector()
    means <- carteram[dim(carteram)[1],-1] |> 
      t() |> 
      as.vector()
    covme <- cov(carterae[, -1], use = "complete.obs")
    npcovme <- nearPD(covme)$mat |> 
      as.matrix()
    covmm <- cov(carteram[, -1], use = "complete.obs")
    npcovmm <- nearPD(covmm)$mat |> 
      as.matrix()
    n <- ncol(npcovme)
    
    qp_oute <- solve.QP(
      Dmat = 2*npcovme,
      dvec = rep(0, n),
      Amat = cbind(-1, diag(n)),
      bvec = c(-1, rep(0, n)),
      meq = 1)
    qp_outm <- solve.QP(
      Dmat = 2*npcovmm,
      dvec = rep(0, n),
      Amat = cbind(-1, diag(n)),
      bvec = c(-1, rep(0, n)),
      meq = 1)
    qp_oute <- qp_oute$solution
    qp_oute <- floor(qp_oute * 100)/100
    qp_outm <- qp_outm$solution
    qp_outm <- floor(qp_outm * 100)/100
    
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
    if(i == 2){
      weightse <- qp_oute
      weightsm <- qp_outm
    }else{
      weightse <- bind_rows(weightse, qp_oute)
      weightsm <- bind_rows(weightsm, qp_outm)
    }
  }
  
}


pvtReal <- pvtReal |>
  select(-Date)

pvtReal[is.na(pvtReal)] <- 0
weightse[is.na(weightse)] <- 0
weightsm[is.na(weightsm)] <- 0

returndfe <- pvtReal[-1,] * weightse
returndfe <- rowSums(returndfe)
returndfm <- pvtReal[-1,] * weightsm
returndfm <- rowSums(returndfm)
returndf <- data.frame(
  Date = listdates[-c(1:(nopredm))],
  Portre = c(1,returndfe),
  Means = c(1,returndfm),
  IBEX = c(1, IBEX |>
             mutate(CloseI = Delt(CloseI)) |>
             filter(Date > listdates[(nopredm+1)]) |>
             select(2) |>
             pull()) 
) |>
  rename(
    IBEX = 4
  )
returndf <- returndf |>
  mutate(
    Portreinv = cumsum(Portre),
    Meansinv = cumsum(Means),
    IBEXinv = cumsum(IBEX)
  )

library(ggplot2)
returndf |>
  mutate(
    Date = ym(Date)) |>
  ggplot(aes(x = Date, group = 1)) +
  geom_line(aes(y = Portreinv), color = "blue") +
  geom_line(aes(y = Meansinv), color = "green") +
  geom_line(aes(y = IBEXinv), color = "red") +
  theme(axis.text.x = element_text(angle = 90))

# returndf |>
#   filter(Date >= "2020-01") |>
#   mutate(
#     Portreinv = cumsum(Portre),
#     IBEXinv = cumsum(IBEX)
#   ) |>
#   mutate(
#     Portreinv = Portreinv + 1,
#     IBEXinv = IBEXinv + 1
#   ) |>
#   mutate(
#     Date = ym(Date)) |>
#   ggplot(aes(x = Date, group = 1)) +
#   geom_line(aes(y = Portreinv), color = "blue") +
#   geom_line(aes(y = IBEXinv), color = "red") +
#   theme(axis.text.x = element_text(angle = 90))

arsqrd

plot(model)
?keras::plot.keras.engine.training.Model
reticulate::conda_install(packages = "graphviz")
reticulate::conda_update()
