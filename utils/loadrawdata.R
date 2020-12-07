library(tidyverse)
library(plyr)
library(lubridate)
library(plotly)
library(dplyr)

LoadFromFilePaths <- function(filePathMeta, filePathEvent, filePathSample) {
  print(filePathMeta)
  dataset_meta <- read.csv(filePathMeta, na.strings="NULL", sep=";")
  dataset_event <- read.csv(filePathEvent, na.strings="NULL", sep=";")
  dataset_sample <- read.csv(filePathSample, na.strings="NULL", sep=";")
  dataset_meta <- PreprocessMeta(dataset_meta)
  dataset <- MergeDatasets(dataset_meta, dataset_event, dataset_sample)
  return(dataset)
}

LoadFromDirectory <- function(dir, event = "Event", sample = "Sample", meta = "Meta") {
  dir = "logData/testModifiers"
  event = "Event"
  sample = "Sample"
  meta = "Meta"
  paste("*",event,".csv",sep="")
  #main game event data
  df_event <- list.files(recursive=TRUE ,path = dir,
                      pattern = event,
                      full.names = T) %>% 
    tibble(filename = .) %>%   
    mutate(file_contents = map(filename,~ read_csv(file.path(.),na = "NULL")))  %>% 
    unnest(cols=-filename) %>%
    mutate(file_contents = NULL)

  #sample data
  df_sample <- list.files(recursive=TRUE ,path = dir,
                            pattern = sample, 
                            full.names = T) %>% 
    tibble(filename = .) %>%   
    mutate(file_contents = map(filename,~ read_csv(file.path(.),na = "NULL")))  %>% 
    unnest(cols=-filename) %>%
    mutate(file_contents = NULL)
  
  #meta data
  df_meta <- list.files(recursive=TRUE ,path = dir,
                             pattern = meta, 
                             full.names = T) %>% 
    tibble(filename = .) %>%   
    mutate(file_contents = map(filename,~ read_csv(file.path(.),na = "NULL")))  %>% 
    unnest(cols=-filename) %>% 
    separate(col=filename,sep="_",into=c("i5","i6","i7","i8","i9"), remove=F) %>%
    separate(col=filename,sep="/",into=c("i0","i1","i2","i3","i4"), remove=T) %>%
    mutate(file_contents = NULL)
  
  
  df_meta <- PreprocessMeta(df_meta)
  dataset <- MergeDatasets(df_meta, df_event, df_sample)
  
  # Manual Trigger:
  #dir = "logdata"
  #datasets = list.files(dir, pattern=paste(".csv",sep=''))
  #Metasets = list.files(dir, pattern=paste("Meta",sep=''))
  #Eventsets = list.files(dir, pattern=paste("Event",sep=''))
  #Samplesets = list.files(dir, pattern=paste("Sample",sep=''))
  #datasets_meta <- lapply(paste(dir, "/", Metasets, sep=''), read.csv, na.strings="NULL", sep=";")
  #datasets_event <- lapply(paste(dir, "/", Eventsets, sep=''), read.csv, na.strings="NULL", sep=";")
  #datasets_sample <- lapply(paste(dir, "/", Samplesets, sep=''), read.csv, na.strings="NULL", sep=";")
  # Preprocess meta dataset
  #for (i in 1:length(Metasets)) {
  #  datasets_meta[[i]] <- PreprocessMeta(datasets_meta[[i]])
  #}
  
  # Merge
  #datasets = list()
  #for (i in 1:length(Metasets)) {
  #  df <- MergeDatasets(datasets_meta[[i]], datasets_event[[i]], datasets_sample[[i]])
  #  datasets[[i]] <- df
  #  names(datasets)[i] <- i
  #}
  return(dataset)
}

PreprocessMeta <- function(dataset_meta) {
  dataset_meta <- dataset_meta %>%
    mutate(Timestamp = NULL,
           Email = NULL,
           Framecount = NULL)
  return(dataset_meta)
}

MergeDatasets <- function(dataset_meta, dataset_event, dataset_sample) {
  df = data.frame()
  df = dataset_event %>% bind_rows(dataset_sample) %>% left_join(dataset_meta, by = "SessionID")
  return(df)
}
