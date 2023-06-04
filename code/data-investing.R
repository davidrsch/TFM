library(reticulate)
source_python("code/data-investing.py")
df <- obt_stock_hist(
  stock ='bbva',
  country = 'spain',
  from_d = '01/01/2002',
  to_d = '01/01/2022',
  as_json = F,
  order = 'ascending',
  interval = 'Monthly') 

x <- select_emps[[1]] |>
  select(Date, Adjusted) |>
  mutate(Return_Ad = Delt(Adjusted)[,1]) |>
  na.omit() |>
  select(Date, Return_Ad)

