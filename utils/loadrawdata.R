library(tidyverse)
library(plyr)
library(lubridate)
library(plotly)
library(dplyr)

LoadFromFilePaths <- function(filePathMeta, filePathEvent, filePathSample) {
  print(filePathMeta)
  dataset_meta <- read.csv(filePathMeta, na.strings="NULL")
  dataset_event <- read.csv(filePathEvent, na.strings="NULL")
  dataset_sample <- read.csv(filePathSample, na.strings="NULL")
  dataset_meta <- PreprocessMeta(dataset_meta)
  dataset <- MergeDatasets(dataset_meta, dataset_event, dataset_sample)
  return(dataset)
}

LoadFromDirectory <- function(dir, separator) {
  # Manual Trigger:
  # dir = "logData/testControllerTrigger"
  datasets = list.files(dir, pattern=paste(".csv",sep=''))
  Metasets = list.files(dir, pattern=paste("Meta",sep=''))
  Eventsets = list.files(dir, pattern=paste("Event",sep=''))
  Samplesets = list.files(dir, pattern=paste("Sample",sep=''))
  datasets_meta <- lapply(paste(dir, "/", Metasets, sep=''), read.csv, na.strings="NULL")
  datasets_event <- lapply(paste(dir, "/", Eventsets, sep=''), read.csv, na.strings="NULL")
  datasets_sample <- lapply(paste(dir, "/", Samplesets, sep=''), read.csv, na.strings="NULL")
  
  # Preprocess meta dataset
  for (i in 1:length(Metasets)) {
    datasets_meta[[i]] <- PreprocessMeta(datasets_meta[[i]])
  }
  
  # Merge
  datasets = list()
  for (i in 1:length(Metasets)) {
    df <- MergeDatasets(datasets_meta[[i]], datasets_event[[i]], datasets_sample[[i]])
    datasets[[i]] <- df
    names(datasets)[i] <- i
  }
  return(datasets)
}

PreprocessMeta <- function(dataset_meta) {
  dataset_meta <- dataset_meta[1,]
  dataset_meta <- dataset_meta %>%
    mutate(Timestamp = NULL,
           SessionID = NULL,
           Email = NULL,
           Framecount = NULL)
  return(dataset_meta)
}

MergeDatasets <- function(dataset_meta, dataset_event, dataset_sample) {
  df = data.frame()
  df = df %>% bind_rows(dataset_event)
  df = df %>% bind_rows(dataset_sample)
  df = df %>% bind_cols(dataset_meta)
  return(df)
}
