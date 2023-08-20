## Cargando librerias ----
library(readxl)
library(simplermarkdown)
library(abind)
library(DiagrammeR)
library(readr)
library(gt)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(quantmod)
library(forecast)
library(jsonlite)
library(stringr)
library(Metrics)
library(Matrix)
library(knitr)
library(keras)
library(tensorflow)
library(tibble)
library(tidyr)
library(dplyr)
library(quadprog)
library(kableExtra)
library(xml2)

## Cargando datos ----
empresas <- read_excel("data/000_empresas.xlsx")
IBEX <- read_csv("data/IBEXm.csv")
load("data/data.Rdata")
load("data/modeling.Rdata")
load("data/results.Rdata")

## Función para obtener las características de un modelo de keras ----
getconfig <- function(model){
  
  # Almacenando las características en un objeto de r
  a <- gsub("'",'"',get_config(model))
  a <- gsub("None","null",a)
  a <- gsub("\\(","[",a)
  a <- gsub(")","]",a)
  a <- gsub("False","false",a)
  a <- gsub("True","true",a)
  a <- gsub(",]","]",a)
  jsonmodeldata <- fromJSON(a,flatten = T)
  modeldata <- jsonmodeldata$layers
  #Extrayendo las características principales
  # Nombre de la capa
  Layer <- modeldata$name
  # Tipo de capa
  Type <- modeldata$class_name
  # A que capas esta conectada
  Connected <- sapply(modeldata$inbound_nodes, function(x){
    y <- unlist(x)
    y <- y[which(y!=0)]
    if(length(y)>1){
      y <- str_flatten_comma(y)
    }else if(is.null(y)){
      y <- "NULL"
    }
    return(y)
  }) |> unlist()
  # Tipo de nodo y cantidad de nodos
  filters <- modeldata$config.filters
  units <- modeldata$config.units
  inputs <- modeldata$config.batch_input_shape
  returnseq <- modeldata$config.return_sequences
  targetshape <- modeldata$config.target_shape
  
  #Creando un data frame con las características principales
  modelconfig <- tibble(
    Layer,
    Type,
    Connected,
    filters,
    units,
    inputs,
    returnseq,
    targetshape
   )
  
  #Definiendo Output Shape
  Shape <- c()
  for (i in 1:dim(modelconfig)[1]) {
    if(modelconfig$Connected[i] == "NULL" &&
       modelconfig$Type[i] == "InputLayer"){
      Shape[i] <- modelconfig$inputs[i]
    }else if(modelconfig$Type[i] == "Flatten"){
      Shape[i][[1]] <- c(NA,prod(na.omit(Shape[i-1][[1]])))
    }else if(modelconfig$Type[i] == "Reshape"){
      Shape[i][[1]] <- c(NA,modelconfig$targetshape[i][[1]])
    }else {
      
      conected <- modelconfig$Connected[i]
      
      if(length(strsplit(conected,", ")[[1]])==1){
        fu <- c(modelconfig$filters[i],
                modelconfig$units[i])
        fu <- na.omit(fu)
        indx <- which(modelconfig$Layer==conected)
        preSO <- Shape[indx]
        lpreSO <- length(preSO[[1]])
        preSO[[1]][lpreSO] <- fu
        Shape[i] <- preSO
        
      }else {
        
        actconec <- strsplit(conected,", ")[[1]]
        
        for (j in 1:length(actconec)) {
          
          indx <- which(modelconfig$Layer== actconec[j])
          preSO <- Shape[indx]
          lpreSO <- length(preSO[[1]])
          
          if(j == 1){
            fu <- preSO[[1]][lpreSO]
          }else{
            subfu <- preSO[[1]][lpreSO]
            fu <- c(fu,subfu)
          }
        }
        fu <- sum(fu)
        preSO[[1]][lpreSO] <- fu
        Shape[i] <- preSO
      }
    }
    if(modelconfig$returnseq[i] == "FALSE" &&
       modelconfig$Type[i] == "LSTM"){
      Shape[i] <- list(c(Shape[i][[1]][1],Shape[i][[1]][3]))
    }
  }
  
  for (i in 1:length(Shape)) {
    Shape[i] <- str_flatten_comma(Shape[i])
  }
  Shape <- unlist(Shape)
  
  modelconfig <- modelconfig |>
    mutate(
      OShape = Shape
    ) |>
    select(
      Layer,
      Type,
      Connected,
      OShape
    )
  return(modelconfig)
  
}

# Función para graficar un modelo de keras----
plot_modelk <- function(modelca){
  #starting graph
  
  graph_start <- '
  digraph{
  fontname="Helvetica,Arial,sans-serif"
  node [fontname="Helvetica,Arial,sans-serif"]
  edge [fontname="Helvetica,Arial,sans-serif"]
  concentrate=True;
  rankdir=TB;
  node [shape=record];
  '
  #Nodes
  for (i in 1:dim(modelca)[1]) {
    if(i == 1){
      nodes <- paste0(
      i,'[label="',modelca[i,1],
      ': ',modelca[i,2],'\n|{entrada:|salida:}|{{}|{',
      modelca[i,4],'}}"];
      ')
    }else{
      cantens <- length(strsplit(as.character(modelca[i,3]),", ")[[1]])
      entradas <- strsplit(as.character(modelca[i,3]),", ")[[1]]
      ifelse(cantens==1,enoens<-'entrada',enoens<-'entradas')
      for (j in 1:cantens) {
        if(j==1){
          ENTR <- modelca[which(modelca[[1]]==entradas[j]),4]
          if(dim(ENTR)[1]==0){
            ENTR <- ' '
          }
        }else{
          SENTR <- modelca[which(modelca[[1]]==entradas[j]),4]
          ENTR <- paste0(ENTR,paste0(', ',SENTR))
        }
      }
      subno <- paste0(
        i,'[label="',modelca[i,1],
        ': ',modelca[i,2],'\n|{',enoens,':|salida:}|{{',
        ENTR,'}|{',
        modelca[i,4],'}}"];
                      ')
      nodes <- paste0(nodes,subno)
    }
  }
  #Connections
  conectedlayers <- which(modelca[[3]]!="NULL")
  
  for (i in 1:length(conectedlayers)) {
    n <- conectedlayers[i]
    cantens <- length(strsplit(as.character(modelca[n,3]),", ")[[1]])
    entradas <- strsplit(as.character(modelca[n,3]),", ")[[1]]
    salida <- n
    for (j in 1:cantens) {
      if(j==1){
        ENTR <- which(modelca[[1]]==entradas[j])
        ENTR <- paste0(ENTR,'->',salida,';')
      }else{
        SENTR <- which(modelca[[1]]==entradas[j])
        SENTR <- paste0(SENTR,'->',salida,';')
        ENTR <- paste0(ENTR,SENTR)
      }
    }
    if(i == 1){
      connections <- ENTR
    }else{
      connections <- paste0(connections,ENTR)
    }
  }
    endgraph <- '
  }
  '
  
  graph <- paste0(graph_start,nodes,connections,endgraph)
  return(graph)
}
