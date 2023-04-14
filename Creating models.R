library(keras)
library(tensorflow)
library(deepviz)

steps <- 3
tsperset <- 1
thsteps <- 1

# CNN 1D ----
## 1 Input - 1 Model ----
## Input layers
inp <- layer_input(
  shape = c(NULL,steps,tsperset))

## Hidden layers 
cnn <- inp |> 
  layer_conv_1d(
    filters = 64,       # Amount of filter in which the data
                        # is divided
    kernel_size = 1,    # Dimension of the window that moves
                        # through the filters
    activation = "relu" #,
    #strides = 1,       # Steps in which the window move
                        # through the filters, this stretch
                        # the dimensions of the output
    #padding = 1,       # Padding added to the input data 
                        # (zeros surrounding the input data)
                        # to avoid reducing dimensionality
  )

conc <- layer_concatenate(
  c(cnn, inp)
)
  # How to calculate the output dimension:
  #     O = ((I-K+2P)/S) + 1
  # Where:
  #   - I: Inputs
  #   - K: kernel size
  #   - P: padding
  #   - S: Stride
lstm <- conc |>
  layer_lstm(
   units = 64)

## Ouput layers
out <- lstm |> 
layer_dense(thsteps) 
layer_reshape(c(thsteps,tsperset))

## Buildin model 
model <- keras_model(inp,out)
## Compiling model
model |> 
  compile(loss = "mse", optimizer='adam')

## Summary model 
### ploting 
plot_model(model)
### summary  
model


## 1 Input - 1 Model Max pooling ----
                     #layer_average_pooling()
## Input layer
inp <- layer_input(
  shape = c(NULL,steps,tsperset))

## Hidden layers
hidd <- inp |> 
  layer_conv_1d(
    filters = 64, 
    kernel_size = 1,
    activation = "relu"
    ) |> 
  layer_max_pooling_1d(
    pool_size = 1      #, Reducing the dimension of the conv 
                       #  layer, optional values (1,2) 
                       #  because it outputs a 3d vector. 
                       #  It's the size of the max pooling
                       #  window.
    #strides = 1,
    #padding = "valid" # Use when input size is not an 
                       # integer multiple of the kernel size.
                       #  - "same" padding is added as 
                       #    required to make up for overlaps
                       #  - "valid" will drop the left over 
                       #    inputs
    ) |> 
  layer_flatten() |> 
  layer_dense(
    units = steps*tsperset,
    activation = "softmax" # Asociate a probability to the 
                           # obtained output
    ) |> 
  layer_reshape(
    target_shape = c(steps, tsperset)
    ) |> 
  layer_lstm(
    units = 64)

## Output layer
out <- hidd |> 
  layer_dense(thsteps*tsperset) |> 
  layer_reshape(c(thsteps,tsperset))

## Building model
model <- keras_model(inp,out)
## compiling model
model |> 
  compile( loss = "mse", optimizer='adam')

## Summary model
### ploting
plot_model(model)
### summary  
model




## 2 Inputs - 2 Models ----

## Inputs layers
### individual inputs
inp <- layer_input(
  shape = c(NULL,steps,tsperset))
inp2 <- layer_input(
  shape = c(NULL,steps,tsperset))

## Hidden Layers 
### CNN
cnn <- inp |> 
  layer_conv_1d(
    filters = 64,
    kernel_size = 1,
    activation = "relu"
  ) |> 
  layer_dense(1)
### LSTM
#### new input
newinp <- layer_concatenate(c(inp2,cnn))
#### lstm from new input
lstm <- newinp |> 
  layer_lstm(
    units = 64,
    return_sequences = T)

## Output Layer
out <- lstm |> 
  layer_dense(tsperset)

## Building model
model <- keras_model(c(inp,inp2), out)
## compiling model
model |> 
  compile( loss = "mse", optimizer='adam')

## Summary model
### ploting
plot_model(model)
### summary
model

## 2 Inputs - 2 Models Max pooling ----
                #layer_average_pooling()

## Inputs layers
### individual inputs
inp <- layer_input(
  shape = c(NULL,steps,tsperset))
inp2 <- layer_input(
  shape = c(NULL,steps,tsperset))

## Hidden Layers 
### CNN
cnn <- inp |> 
  layer_conv_1d(
    filters = 64,
    kernel_size = 1,
    activation = "relu"
  ) |> layer_max_pooling_1d(
    pool_size = 1
  ) |> 
  layer_dense(
    units = 1,
    activation = "softmax"
  )

### LSTM
#### new input
newinp <- layer_concatenate(c(inp2,cnn))
#### lstm from new input
lstm <- newinp |> 
  layer_lstm(
    units = 64,
    return_sequences = T)

## Output Layer
out <- lstm |> 
  layer_dense(tsperset)

## Building model
model <- keras_model(c(inp,inp2), out)
## compiling model
model |> 
  compile( loss = "mse", optimizer='adam')

## Summary model
### ploting
plot_model(model)
### summary
model



# CNN 2D ----
filters <- 64
## 1 Input - 1 Model ----
## Input layers
inp <- layer_input(
  shape = c(NULL,steps,tsperset,1))

## Hidden layers 
hidd <- inp |> 
  layer_conv_2d(
    filters = filters,       
    kernel_size = c(1,tsperset),    
    activation = "relu"
  ) |> 
  layer_reshape(c(steps,filters)) |> 
  layer_lstm(
    units = 64)

## Ouput layers
out <- hidd |> 
  layer_dense(thsteps*tsperset) |> 
  layer_reshape(c(thsteps,tsperset))

## Buildin model 
model <- keras_model(inp,out)
## Compiling model
model |> 
  compile( loss = "mse", optimizer='adam')

## Summary model 
### ploting 
plot_model(model)
### summary  
model


## 1 Input - 1 Model Max pooling ----
#layer_average_pooling()
## Input layer
inp <- layer_input(
  shape = c(NULL,steps,tsperset,1))

## Hidden layers
hidd <- inp |> 
  layer_conv_2d(
    filters = 64, 
    kernel_size = c(1,tsperset),
    activation = "relu"
  ) |> 
  layer_max_pooling_2d(
    pool_size = 1
  ) |> 
  layer_flatten() |> 
  layer_dense(
    units = steps*tsperset,
    activation = "softmax" # Asociate a probability to the 
    # obtained output
  ) |> 
  layer_reshape(
    target_shape = c(steps, tsperset)
  ) |> 
  layer_lstm(
    units = 64)

## Output layer
out <- hidd |> 
  layer_dense(thsteps*tsperset) |> 
  layer_reshape(c(thsteps,tsperset))

## Building model
model <- keras_model(inp,out)
## compiling model
model |> 
  compile( loss = "mse", optimizer='adam')

## Summary model
### ploting
plot_model(model)
### summary  
model




## 2 Inputs - 2 Models ----

## Inputs layers
### individual inputs
inp <- layer_input(
  shape = c(NULL,steps,tsperset,1))
inp2 <- layer_input(
  shape = c(NULL,steps,tsperset))

## Hidden Layers 
### CNN
cnn <- inp |> 
  layer_conv_2d(
    filters = filters,
    kernel_size = c(1,tsperset),
    activation = "relu"
  ) |> 
  layer_reshape(c(steps,filters))
### LSTM
#### new input
newinp <- layer_concatenate(c(inp2,cnn))
#### lstm from new input
lstm <- newinp |> 
  layer_lstm(
    units = 64,
    return_sequences = T)

## Output Layer
out <- lstm |> 
  layer_dense(tsperset)

## Building model
model <- keras_model(c(inp,inp2), out)
## compiling model
model |> 
  compile( loss = "mse", optimizer='adam')

## Summary model
### ploting
plot_model(model)
### summary
model

## 2 Inputs - 2 Models Max pooling ----
#layer_average_pooling()

## Inputs layers
### individual inputs
inp <- layer_input(
  shape = c(NULL,steps,tsperset,1))
inp2 <- layer_input(
  shape = c(NULL,steps,tsperset))

## Hidden Layers 
### CNN
cnn <- inp |> 
  layer_conv_2d(
    filters = 64,
    kernel_size = c(1,tsperset),
    activation = "relu"
  ) |> layer_max_pooling_2d(
    pool_size = 1
  ) |> 
  layer_dense(
    units = 1,
    activation = "softmax"
  ) |> 
  layer_reshape(
    c(2,1)
  )

### LSTM
#### new input
newinp <- layer_concatenate(c(inp2,cnn))
#### lstm from new input
lstm <- newinp |> 
  layer_lstm(
    units = 64,
    return_sequences = T)

## Output Layer
out <- lstm |> 
  layer_dense(tsperset)

## Building model
model <- keras_model(c(inp,inp2), out)
## compiling model
model |> 
  compile( loss = "mse", optimizer='adam')

## Summary model
### ploting
plot_model(model)
### summary
model
