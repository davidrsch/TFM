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

#####
library(quantmod)
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
######

## Loading data----
data = read.csv("data/readjwithibxcorr.csv")

## Giving format to data
data <- data |>
  filter(!ID %in% c(28,77,30,35))
names(data) <- c(
  "Date", "X1", "Y1", "X2", "Y2", "X3", "Y3", "X4", "Y4",
  "X5", "Y5", "X6", "Y6", "X7", "Y7", "ID", "sector", "subsector")
data <- data |>
  mutate(Y0 = if_else(Y1 < 0,0,1))
head(data)

## Setting parameters ----
timesteps = 1
timehorizon = 1

## Creating train and test sets----
data <- data |> 
  arrange(Date)
datan <- data
datan <- datan |>
  group_by(ID) |>
  mutate(
    means = cummean(X1))
datat <- datan |>
  ungroup()
datatrain <- datat |>
  dplyr::filter(Date >= "2003-01" & Date < "2016-06")
dataval <- datat |>
  dplyr::filter(Date >= "2016-06" & Date < "2020-01")
datatest <- datat |>
  dplyr::filter(Date >= "2020-01")

dim(datat)
dim(datatrain)
dim(dataval)
dim(datatest)

## Creating inputs and outputs for train----
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
sectors <- abind(sector, subsector, along = 3)
input_train <- abind(input_train, sectors, along = 3)
output_signt <- datatrain |> 
  select(Y0) |> 
  as.matrix()
dim(output_signt) <- c(dim(output_signt)[1],timehorizon,dim(output_signt)[2])
output_train <- datatrain |> 
  select(Y1) |> 
  as.matrix()

## Creating inputs and outputs for validation----
input_val <- dataval |> 
  select(contains("X")) |> 
  as.matrix()
dim(input_val) <- c(dim(input_val)[1],timesteps,dim(input_val)[2])
sector <- dataval |>
  #  mutate(sector = log(sector)) |> 
  select(sector)
subsector <- dataval |>
  #  mutate(subsector = log(subsector)) |>
  select(subsector)
sectors <- abind(sector, subsector, along = 3)
input_val <- abind(input_val, sectors, along = 3)
output_signv <- dataval |> 
  select(Y0) |> 
  as.matrix()
dim(output_signv) <- c(dim(output_signv)[1],timehorizon,dim(output_signv)[2])
output_val <- dataval |> 
  select(Y1) |> 
  as.matrix()

dim(input_train)
dim(output_signt)
dim(output_train)
dim(input_val)
dim(output_signv)
dim(output_val)

#set_random_seed(12.3)
## Building model ----
### Parameters ----
steps <- dim(input_train)[2]
tsperset <- dim(input_train)[3]
thsteps <- dim(output_train)[2]

### Model structure ----
#### Input layers----
inp <- layer_input(
  shape = c(NULL,steps,tsperset))
inp2 <- layer_input(
  shape = c(NULL,steps,4))
#### Hiden layers----
cnn <- inp |>
  layer_conv_1d(
    filters = 64,
    kernel_size = 1,
    kernel_regularizer = regularizer_l1(0.005),
    bias_regularizer = regularizer_l1(0.01),
    activation = layer_activation_selu()) |>
  layer_dense(1)
hidd <- cnn |>
  layer_concatenate(inp2) |>
  layer_lstm(64)
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


## Fitting model ----
model |>
  fit(
    x = list(
      input_train,
      input_train[,,c(1:4), drop = F]
    ),
    y = list(
      output_signt,
      output_train
    ),
    validation_data = list(
      x = list(
        input_val,
        input_val[,,c(1:4), drop = F]
      ),
      y = list(
        output_signv,
        output_val
      )
    ),
    epochs = 10,
    batch_size = 16,
    shuffle = F,
    verbose = 1)

## Testing ----
### Train set ----
trainpred <- model |> predict(
  list(
    input_train,
    input_train[,,c(1:4), drop = F]
  ))
trainpred <- trainpred[[2]]
#### Values that perform badly ----
datatrain |> 
  cbind(trainpred = trainpred) |>
  mutate(
    Y1 = Y1
  ) |>
  group_by(ID) |>
  summarise(
    mse_model = mse(trainpred, Y1),
    mse_actmeans = mse(means, Y1),
    mse_generalmean = mse(mean(Y1), Y1),
    r2_vsactmean = 1 - (sum((Y1 - trainpred)^2)/sum((Y1 - means)^2)),
    r2_vsgmean = 1 - (sum((Y1 - trainpred)^2)/sum((Y1 - mean(X1))^2))
  ) |>
  dplyr::filter(r2_vsactmean < 0)
### val set ----
valpred <- model |> predict(
  list(
    input_val,
    input_val[,,c(1:4), drop = F]
  ))
valpred <- valpred[[2]]
#### Values that perform badly ----
dataval |> 
  cbind(valpred = valpred) |>
  mutate(
    Y1 = Y1
  ) |>
  group_by(ID) |>
  summarise(
    mse_model = mse(valpred, Y1),
    mse_actmeans = mse(means, Y1),
    mse_generalmean = mse(mean(Y1), Y1),
    r2_vsactmean = 1 - (sum((Y1 - valpred)^2)/sum((Y1 - means)^2)),
    r2_vsgmean = 1 - (sum((Y1 - valpred)^2)/sum((Y1 - mean(X1))^2))
  ) |>
  dplyr::filter(r2_vsactmean < 0)
### Test set ----
#### Creating inputs and outputs ----
input_test <- datatest |> 
  select(contains("X")) |> 
  as.matrix()
dim(input_test) <- c(dim(input_test)[1],timesteps,dim(input_test)[2])
sector <- datatest |> 
  mutate(sector = log(sector)) |>
  select(sector)
subsector <- datatest |> 
  mutate(subsector = log(subsector)) |>
  select(subsector)
sectors <- abind(sector, subsector, along = 3)
input_test <- abind(input_test, sectors, along = 3)
output_signtt <- datatest |> 
  select(Y0) |> 
  as.matrix()
dim(output_signtt) <- c(dim(output_signtt)[1],timehorizon,dim(output_signtt)[2])
output_test <- datatest |> 
  select(Y1) |> 
  as.matrix()

#### Getting predictions
predictions <- c()
model2 <- model
model2 |>
  compile(optimizer = optimizer_adam(), loss = "mse")

Dates <- datatest |>
  select(Date) |>
  arrange(Date) |>
  pull() |>
  unique()

Dates |>
  lapply(function(x, data = datatest, ts = timesteps, tsps = tsperset, th = timehorizon){
    testdata <- data |>
      dplyr::filter(Date == x)
    
    input_test <- testdata |>
      select(contains("X")) |>
      mutate_if(is.character, as.numeric) |>
      as.matrix()
    dim(input_test) <- c(dim(input_test)[1], ts, dim(input_test)[2])
    
    sector <- testdata |> 
      mutate(sector = log(sector)) |>
      select(sector)
    
    subsector <- testdata |> 
      mutate(subsector = log(subsector)) |>
      select(subsector)
    
    sectors <- abind(sector, subsector, along = 3)
    input_test <- abind(input_test, sectors, along = 3)
    
    pred <- predict(model2,
                    list(
                      input_test, 
                      input_test[,,c(1:4), drop = F]
                    ))
    pred <- pred[[2]]
    predictions <<- abind(predictions, pred, along = 1)
    
    output_signtt <- testdata |> 
      select(Y0) |>
      as.matrix()
    dim(output_signtt) <- c(dim(output_signtt)[1],timehorizon,dim(output_signtt)[2])
    
    output_test <- testdata |>
      select(Y1) |>
      mutate_if(is.character, as.numeric) |>
      as.matrix()
    
    model2 |>
      fit(
        list(
          input_test,
          input_test[,,c(1:4), drop = F]
        ),
        list(
          output_signtt,
          output_test
        ),
        epochs = 1,
        batch_size = 64,
        shuffle = F,
        verbose = 0)
  }) |>
  invisible()
#### Values that performed badly ----
datatest |> 
  cbind(predictions = predictions) |>
  mutate(
    Y1 = Y1
  ) |>
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
datatest |> 
  cbind(predictions = predictions) |>
  mutate(
    Y1 = Y1
  ) |>
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

Pred <- as.data.frame(predictions)
names(Pred) <- "yhat"
Yhat <- datatrain |> 
  mutate(yhat = trainpred) |>
  rbind(dataval |>
          mutate(yhat = valpred)) |>
  select(yhat) |>
  rbind(Pred) |>
  cbind(datat |>
          filter(Date >= "2000-04")) |>
  mutate(
    Real = Y1,
    IBEX = Y2,
    Yhat = yhat
  ) |>
  select(Date, Real, IBEX, Yhat, ID)

pvtYhat <- Yhat |>
  select(Date, Yhat, ID) |>
  pivot_wider(
    names_from = ID,
    values_from = Yhat
  )

datlist <- datat |> 
  #filter(Date >= "2000-04") |>
  select(Date) |>
  unique() |>
  pull()

weights <- data.frame()
 
datlist |>
  lapply(function(x, data = pvtYhat){
    if(x != "2000-04"){
      data <- data |>
        filter(Date <= x)
      print(x)
      nare <- which(is.na(data[dim(data)[1],]))
      naremo <- which(is.na(data[(dim(data)[1]-1),]))
      nare <- c(nare,naremo)
      nare <- unique(nare)
      if(length(nare) != 0){
        cartera <- data[, - nare]
      }else{
        cartera <- data
      }
      return <- cartera[dim(cartera)[1], -1] |>
        as.matrix() |>
        t()
      covm <- cov(cartera[, -1], use = "complete.obs")
      npcovm <- nearPD(covm)$mat |> 
        as.matrix()
      n <- ncol(npcovm)
      D.mat <- npcovm 
      d.vec <- rep(0,n) 
      A.mat <- matrix(1,nrow=nrow(D.mat))
      A.mat <- cbind(A.mat,d.vec)
      A.mat <- cbind(A.mat,diag(nrow(D.mat)))
      b.vec <- c(1,0,rep(0,nrow(D.mat)))
      qp.out <- solve.QP(Dmat=D.mat, dvec=d.vec,
                         Amat=A.mat, bvec=b.vec, meq=1)
      
      weig <- round(qp.out$solution, 3)
      for(i in 1:length(weig)){
        if(weig[i] < 0.001){
          weig[i] <- 0
        }else{}
      }
      weig <- as.data.frame(t(weig))
      names(weig) <- names(cartera[, -1])
      weights <<- bind_rows(weights, weig)
    }
    
  }) |> 
  invisible()

pvtReal <- Yhat |>
  select(Date, Real, ID) |>
  pivot_wider(
    names_from = ID,
    values_from = Real
  ) |>
  filter(Date >= datlist[1]) |>
  select(-Date)

pvtReal[is.na(pvtReal)] <- 0
weights[is.na(weights)] <- 0

returndf <- pvtReal[-1,] * weights
returndf <- rowSums(returndf)
returndf <- data.frame(
  Date = datlist[-1],
  Portre = returndf,
  IBEX = IBEX |>
    mutate(CloseI = Delt(CloseI)) |>
    filter(Date >= datlist[-1][1]) |>
    select(2) |>
    pull()
) |>
  rename(
    IBEX = 3
  )
returndf <- returndf |>
  mutate(
    Portreinv = cumsum(Portre),
    IBEXinv = cumsum(IBEX)
    ) |>
  mutate(
    Portreinv = Portreinv + 1,
    IBEXinv = IBEXinv + 1
  )

library(ggplot2)
returndf |>
  mutate(
    Date = ym(Date)) |>
  ggplot(aes(x = Date, group = 1)) +
  geom_line(aes(y = Portreinv), color = "blue") +
  geom_line(aes(y = IBEXinv), color = "red") +
  theme(axis.text.x = element_text(angle = 90))

returndf |>
  filter(Date >= datatest$Date[1]) |>
  mutate(
    Portreinv = cumsum(Portre),
    IBEXinv = cumsum(IBEX)
  ) |>
  mutate(
    Portreinv = Portreinv + 1,
    IBEXinv = IBEXinv + 1
  ) |>
  mutate(
    Date = ym(Date)) |>
  ggplot(aes(x = Date, group = 1)) +
  geom_line(aes(y = Portreinv), color = "blue") +
  geom_line(aes(y = IBEXinv), color = "red") +
  theme(axis.text.x = element_text(angle = 90))

