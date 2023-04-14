#Training models----
input <- ordereddata |> 
  select(contains("X")) |> 
  as.matrix()
dim(input) <- c(dim(input)[1],timesteps,tsperset)

output <- ordereddata |> 
  select(contains("Y")) |> 
  as.matrix()
dim(output) <- c(dim(output)[1],timehorizon,tsperset)

history <- model |> 
  fit(
    input,
    output,
    epochs = 50,
    batch_size = 1,
    shuffle = F,
    verbose = 1)
plot(history)