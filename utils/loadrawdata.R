library(tidyverse)
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
  
  return(dataset)
}

PreprocessMeta <- function(dataset_meta) {
  dataset_meta <- dataset_meta %>%
    rename(MetaTimestamp = Timestamp,
           MetaEmail = Email,
           MetaFramecount = Framecount)
  return(dataset_meta)
}

MergeDatasets <- function(dataset_meta, dataset_event, dataset_sample) {
  df = data.frame()
  df = dataset_event %>% bind_rows(dataset_sample) %>% left_join(dataset_meta, by = "SessionID")
  return(df)
}
