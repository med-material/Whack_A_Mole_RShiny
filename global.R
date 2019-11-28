library(RMySQL)

my_data <- read.csv("credentials.csv", header=TRUE,sep=",", colClasses=c("character","character","character","character"))
print(my_data[1, "host"])

lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

mydb = dbConnect(MySQL(),
                 user=my_data[1, "username"],
                 # rstudioapi::askForPassword("Database user"),
                 password=my_data[1, "password"],
                 # rstudioapi::askForPassword("Database password"),
                 dbname=my_data[1, "dbname"],
                 host=my_data[1, "host"])

# print(FetchDatas(list(list("TargetId = 204"), list("Id=3", "Id=2"))))
# print(CountField(fieldName = "ParticipantNo"))
# print(GenerateQuery(list()))
# dbDisconnect(mydb)

FetchDatas <- function(conditionLists = list(), option = "*")
{
  queryString = GenerateQuery(conditionLists, option)
  return(dbGetQuery(mydb, queryString))
}


GenerateQuery <- function(conditionLists, option)
{
  queryString = paste("SELECT", option, sep = " ")
  queryString = paste(queryString, "FROM whack_vr", sep = " ")
  
  if (length(conditionLists) == 0)
  {
    print(queryString)
    return(queryString)
  }
  
  conditionLink = "OR"
  listLink = "AND"
  
  queryString = paste(queryString, "WHERE", sep = " ")
  
  for (i in 1:length(conditionLists)){
    queryString = paste(queryString, "(", sep = "")
    for (j in 1:length(conditionLists[[i]])){
      queryString = paste(queryString, conditionLists[[i]][[j]], sep = " ")
      if (j < length(conditionLists[[i]]))
      {
        queryString = paste(queryString, conditionLink, sep = " ")
      }
    }
    queryString = paste(queryString, ")", sep = "")
    if (i < length(conditionLists))
    {
      queryString = paste(queryString, listLink, sep = " ")
    }
  }
  print(queryString)
  return(queryString)
}


GetField <- function(fieldName, fetchResult)
{
  return(fetchResult[[fieldName]])
}


CountField <- function(fieldName = "*", conditions = list())
{
  tempField <- paste("COUNT(DISTINCT ", fieldName, sep = "")
  tempField <- paste(tempField, ")", sep = "")
  return(GetField(tempField, FetchDatas(conditions, tempField)))
}


GenerateSelectChoices <- function(default = "", text = "", fieldName, conditions = list(), extraInfo = list())
{
  tempList <- list()
  tempList[[default]] <- -1
  fieldList <- GetField(fieldName, FetchDatas(conditions, paste("DISTINCT", fieldName)))
  extraTextString = ""
  
  if(length(fieldList) == 0)
    return(tempList)
  
  for(i in 1:length(fieldList))
  {
    if(length(extraInfo) != 0)
    {
      extraTextString = ""
      extraText <- list()
      for(j in 1:length(extraInfo))
      {
        print(j)
        tempContitions <- conditions
        tempContitions[[length(tempContitions) + 1]] <- paste(toString(fieldName), " = ", fieldList[[i]], sep = "")
        extraText[[j]] <- GetField(extraInfo[[j]], FetchDatas(tempContitions, paste("DISTINCT", extraInfo[[j]])))[[1]]
        print(extraText)
      }
      
      
      for(j in 1:length(extraText))
      {
        extraTextString <- paste(extraTextString, toString(extraText[[j]]), sep = "")
      }
    }
    
    resultString <- ""
    
    if (is.numeric(fieldList[[i]]))
    {
      resultString <- paste(text, fieldList[[i]], sep = " ")
    }
    else
    {
      resultString <- fieldList[[i]]
    }
    
    if(extraTextString != "")
    {
      extraTextString <- paste("(", extraTextString, ")", sep = "")
      resultString <- paste(resultString, extraTextString, sep = " ")
    }
    
    tempList[[resultString]] <- fieldList[[i]]
  }
  return(tempList)
}


GenerateMatrix <- function(xParam = "", xLength = 1, yParam = "", yLength = 1, valueParam = "", conditions = list(), scoringFunction = function(matrixList, x, y){return(0)}, normalized = FALSE)
{
  optionString = paste(xParam, yParam, valueParam, sep = ", ")
  matrixList <- FetchDatas(conditions, optionString)
  
  returnMatrix = matrix(nrow = yLength, ncol = xLength, data = NA)
  
  maxScore = 1
  minScore = -1
  
  for(x in 1:xLength)
  {
    for(y in 1:yLength)
    {
      score <- scoringFunction(matrixList, x, y)
      returnMatrix[y, x] <- score
    }
  }
  return(returnMatrix)
}


CheckPropertyValue <- function(fieldName, conditions = list())
{
  propertyValues <- GetField(fieldName, FetchDatas(conditions, paste("DISTINCT", fieldName)))
  
  returnString <- ""
  
  if(length(propertyValues) == 0)
  {
    return("")
  }
  
  for(i in 1:length(propertyValues))
  {
    if(i == 1)
    {
      returnString <- propertyValues[[1]]
    }
    else
    {
      returnString <- paste(returnString, propertyValues[[i]], sep = " / ")
    }
  }
  print(returnString)
  return(returnString)
}



