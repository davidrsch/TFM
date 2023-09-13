library(dplyr)

load("data/results.Rdata")
View(resultscnnlstmssing1$resultsD)
data <- do.call(cbind,resultscnnlstmssing1$resultsD)
data |>
  rowwise() |>
  mutate(
    Date = `001.Date`,
    meanmse = mean(c_across(contains("mse"))),
    meanrsqrd = mean(c_across(contains("rsqrd")))
    ) |>
  select(
    Date, meanmse,meanrsqrd
  )|>
  mutate(
    Date = as.Date(Date)) |>
  ggplot(aes(x = Date, group = 1)) +
  geom_line(aes(y = meanmse, color = "MSE")) +
  geom_line(aes(y = meanrsqrd, color = "R2")) +
  scale_color_manual(values = c("blue", "green")) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Fecha", y = "Valores", color = "Indicadores")

Model1 <- rep(1,dim(data1)[1])

datameans <- data |>
  rowwise() |>
  mutate(
    Date = `001.Date`,
    meanmse = mean(c_across(contains("mse"))),
    meanrsqrd = mean(c_across(contains("rsqrd")))
  ) |>
  select(Date, meanmse, meanrsqrd)
mean(datameans$meanmse)
mean(datameans$meanrsqrd)

data <- do.call(rbind,resultscnnlstmssing1$resultsID)
data <- data |>
  group_by(ID) |>
  summarize(
    rsqrd = mean(rsqrd),
    mse= mean(mse)) |>
  ungroup() |>
  arrange(desc(rsqrd))
data1 <- rbind(data[1:10,],data[94:103,])

data <- do.call(cbind,resultscnnlstmssing1$resultsD)
data_summary <- data |>
  mutate(
    Date = `001.Date`,
    IBEX = `001.IBEX`,
    Means = resultscnnlstmssing1$resultsMEANS) |>
  mutate_at(vars(contains("Portre")), ~ cumsum(.)) |>
  mutate(
    IBEX = cumsum(IBEX),
    Means = cumsum(Means)) |>
  group_by(Date) |>
  summarize(
    meanPortre = mean(c_across(contains("Portre"))),
    max_y = max(c_across(contains("Portre"))),
    min_y = min(c_across(contains("Portre"))),
    min_5 = unname(quantile(c_across(contains("Portre")),0.05)),
    max_95 = unname(quantile(c_across(contains("Portre")),0.95)),
    IBEX = IBEX,
    Means = Means)
data_summary |>
  mutate(
    Date = as.Date(Date)) |>
ggplot(aes(x = Date)) +
  geom_ribbon(aes(ymin = min_y, ymax = min_5), fill = "blue", alpha = 0.3) +
  geom_ribbon(aes(ymin = max_y, ymax = max_95), fill = "blue", alpha = 0.3) +
  geom_ribbon(aes(ymin = min_5, ymax = max_95), fill = "blue", alpha = 0.6) +
  geom_line(
    aes(y = meanPortre, color = "Media RNA1"),
    linetype = "dashed") +
  geom_line(aes(y = max_y), color = "blue") +
  geom_line(aes(y = min_y), color = "blue") +
  geom_line(aes(y = max_95), color = "blue") +
  geom_line(aes(y = min_5, color = "RNA1")) +
  geom_line(aes(y = IBEX, color = "IBEX")) +
  geom_line(aes(y = Means, color = "Medias")) +
  scale_color_manual(
    values = c(
      "Media RNA1"="blue",
      "RNA1" = "blue",
      "IBEX" = "red",
      "Medias" = "green")) +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype = c("solid","dashed","solid","solid"))))+
  labs(x = "Fecha",
       y = "Valores",
       color = "Leyenda")+
  theme_minimal()

obs <- dim(data_summary)[1]
meanrnare <- data_summary[["meanPortre"]][obs]
meanre <- data_summary[["Means"]][obs]
IBEXre <- data_summary[["IBEX"]][obs]
(meanrnare-meanre)/meanre
(meanrnare-IBEXre)/IBEXre

####2----
data <- do.call(cbind,resultscnnlstmssing2$resultsD)
data |>
  rowwise() |>
  mutate(
    Date = `001.Date`,
    meanmse = mean(c_across(contains("mse"))),
    meanrsqrd = mean(c_across(contains("rsqrd")))
  ) |>
  select(
    Date, meanmse,meanrsqrd
  )|>
  mutate(
    Date = as.Date(Date)) |>
  ggplot(aes(x = Date, group = 1)) +
  geom_line(aes(y = meanmse, color = "MSE")) +
  geom_line(aes(y = meanrsqrd, color = "R2")) +
  scale_color_manual(values = c("blue", "green")) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Fecha", y = "Valores", color = "Indicadores")

Model2 <- rep(2,dim(data2)[1])

datameans <- data |>
  rowwise() |>
  mutate(
    Date = `001.Date`,
    meanmse = mean(c_across(contains("mse"))),
    meanrsqrd = mean(c_across(contains("rsqrd")))
  ) |>
  select(Date, meanmse, meanrsqrd)
mean(datameans$meanmse)
mean(datameans$meanrsqrd)


data <- do.call(rbind,resultscnnlstmssing2$resultsID)
data <- data |>
  group_by(ID) |>
  summarize(
    rsqrd = mean(rsqrd),
    mse= mean(mse)) |>
  ungroup() |>
  arrange(desc(rsqrd))
data2 <- rbind(data[1:10,],data[94:103,])

data <- do.call(cbind,resultscnnlstmssing2$resultsD)
data_summary <- data |>
  mutate(
    Date = `001.Date`,
    IBEX = `001.IBEX`,
    Means = resultscnnlstmssing2$resultsMEANS) |>
  mutate_at(vars(contains("Portre")), ~ cumsum(.)) |>
  mutate(
    IBEX = cumsum(IBEX),
    Means = cumsum(Means)) |>
  group_by(Date) |>
  summarize(
    meanPortre = mean(c_across(contains("Portre"))),
    max_y = max(c_across(contains("Portre"))),
    min_y = min(c_across(contains("Portre"))),
    min_5 = unname(quantile(c_across(contains("Portre")),0.05)),
    max_95 = unname(quantile(c_across(contains("Portre")),0.95)),
    IBEX = IBEX,
    Means = Means)
data_summary |>
  mutate(
    Date = as.Date(Date)) |>
  ggplot(aes(x = Date)) +
  geom_ribbon(aes(ymin = min_y, ymax = min_5), fill = "blue", alpha = 0.3) +
  geom_ribbon(aes(ymin = max_y, ymax = max_95), fill = "blue", alpha = 0.3) +
  geom_ribbon(aes(ymin = min_5, ymax = max_95), fill = "blue", alpha = 0.6) +
  geom_line(aes(y = meanPortre), color = "blue", linetype = "dashed") +
  geom_line(aes(y = max_y), color = "blue") +
  geom_line(aes(y = min_y), color = "blue") +
  geom_line(aes(y = max_95), color = "blue") +
  geom_line(aes(y = min_5, color = "RNA2")) +
  geom_line(aes(y = IBEX, color = "IBEX")) +
  geom_line(aes(y = Means, color = "Medias")) +
  scale_color_manual(
    values = c("RNA2" = "blue", "IBEX" = "red", "Medias" = "green")) +
  labs(x = "Fecha",
       y = "Valores",
       color = "Leyenda")+
  theme_minimal()

obs <- dim(data_summary)[1]
meanrnare <- data_summary[["meanPortre"]][obs]
meanre <- data_summary[["Means"]][obs]
IBEXre <- data_summary[["IBEX"]][obs]
(meanrnare-meanre)/meanre
(meanrnare-IBEXre)/IBEXre


####3----

data <- do.call(cbind,resultscnnlstmssing3$resultsD)
data |>
  rowwise() |>
  mutate(
    Date = `001.Date`,
    meanmse = mean(c_across(contains("mse"))),
    meanrsqrd = mean(c_across(contains("rsqrd")))
  ) |>
  select(
    Date, meanmse,meanrsqrd
  )|>
  mutate(
    Date = as.Date(Date)) |>
  ggplot(aes(x = Date, group = 1)) +
  geom_line(aes(y = meanmse, color = "MSE")) +
  geom_line(aes(y = meanrsqrd, color = "R2")) +
  scale_color_manual(values = c("blue", "green")) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Fecha", y = "Valores", color = "Indicadores")

Model3 <- rep(3, dim(data3)[1])

datameans <- data |>
  rowwise() |>
  mutate(
    Date = `001.Date`,
    meanmse = mean(c_across(contains("mse"))),
    meanrsqrd = mean(c_across(contains("rsqrd")))
  ) |>
  select(Date, meanmse, meanrsqrd)
mean(datameans$meanmse)
mean(datameans$meanrsqrd)

data <- do.call(rbind,resultscnnlstmssing3$resultsID)
data <- data |>
  group_by(ID) |>
  summarize(
    rsqrd = mean(rsqrd),
    mse= mean(mse)) |>
  ungroup() |>
  arrange(desc(rsqrd))
data3 <- rbind(data[1:10,],data[94:103,])
data3

data <- do.call(cbind,resultscnnlstmssing3$resultsD)
data_summary <- data |>
  mutate(
    Date = `001.Date`,
    IBEX = `001.IBEX`,
    Means = resultscnnlstmssing3$resultsMEANS) |>
  mutate_at(vars(contains("Portre")), ~ cumsum(.)) |>
  mutate(
    IBEX = cumsum(IBEX),
    Means = cumsum(Means)) |>
  group_by(Date) |>
  summarize(
    meanPortre = mean(c_across(contains("Portre"))),
    max_y = max(c_across(contains("Portre"))),
    min_y = min(c_across(contains("Portre"))),
    min_5 = unname(quantile(c_across(contains("Portre")),0.05)),
    max_95 = unname(quantile(c_across(contains("Portre")),0.95)),
    IBEX = IBEX,
    Means = Means)
data_summary |>
  mutate(
    Date = as.Date(Date)) |>
  ggplot(aes(x = Date)) +
  geom_ribbon(aes(ymin = min_y, ymax = min_5), fill = "blue", alpha = 0.3) +
  geom_ribbon(aes(ymin = max_y, ymax = max_95), fill = "blue", alpha = 0.3) +
  geom_ribbon(aes(ymin = min_5, ymax = max_95), fill = "blue", alpha = 0.6) +
  geom_line(aes(y = meanPortre), color = "blue", linetype = "dashed") +
  geom_line(aes(y = max_y), color = "blue") +
  geom_line(aes(y = min_y), color = "blue") +
  geom_line(aes(y = max_95), color = "blue") +
  geom_line(aes(y = min_5, color = "RNA3")) +
  geom_line(aes(y = IBEX, color = "IBEX")) +
  geom_line(aes(y = Means, color = "Medias")) +
  scale_color_manual(
    values = c("RNA3" = "blue", "IBEX" = "red", "Medias" = "green")) +
  labs(x = "Fecha",
       y = "Valores",
       color = "Leyenda")+
  theme_minimal()

obs <- dim(data_summary)[1]
meanrnare <- data_summary[["meanPortre"]][obs]
meanre <- data_summary[["Means"]][obs]
IBEXre <- data_summary[["IBEX"]][obs]
(meanrnare-meanre)/meanre
(meanrnare-IBEXre)/IBEXre

# agrupando resultados
Es <- c(Model1, Model2, Model3)
names(data1) <- c("Date","meanmse1","meanrsqrd1")
names(data2) <- c("Date","meanmse2","meanrsqrd2")
names(data3) <- c("Date","meanmse3","meanrsqrd3")
indicadores <- left_join(data1,data2) |>
  left_join(data3)
write.csv(indicadores, "indicadores.csv")

# Agrupando resultados resumen
names(datas1) <- c(
  "Date","meanPortre1","max_y1","min_y1", "min_51",
  "max_951", "IBEX1", "Means1")
names(datas2) <- c(
  "Date","meanPortre2","max_y2","min_y2", "min_52",
  "max_952", "IBEX2", "Means2")
names(datas3) <- c(
  "Date","meanPortre3","max_y3","min_y3", "min_53",
  "max_953", "IBEX3", "Means3")
results <- left_join(datas1,datas2) |>
  left_join(datas3)
write.csv(results, "results.csv")
