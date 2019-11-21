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


# testFunc <- function(matrixList, x, y)
# {
#   minusScore <- length(which((matrixList$MoleIndexX == x) & (matrixList$MoleIndexY == y) & (matrixList$Event == "Mole Expired")))
#   plusScore <- length(which((matrixList$MoleIndexX == x) & (matrixList$MoleIndexY == y) & (matrixList$Event == "Mole Hit")))
# 
#   return(plusScore - minusScore)
# }
# 
# GenerateMatrix(xParam = "MoleIndexX", yParam = "MoleIndexY", xLength = 9, yLength = 7, valueParam = "Event", conditions = list(list("Event = 'Mole Expired'", "Event = 'Mole Hit'")), scoringFunction = testFunc)


FetchDatas <- function(conditionLists = list(), option = "*")
{
  queryString = GenerateQuery(conditionLists, option)
  return(fetch(dbSendQuery(mydb, queryString)))
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


GenerateSelectChoices <- function(default = "", text = "", fieldName, conditions = list())
{
  tempList <- list()
  tempList[[default]] <- -1
  fieldList <- GetField(fieldName, FetchDatas(conditions, paste("DISTINCT", fieldName)))
  for(i in 1:length(fieldList))
  {
    if (is.numeric(fieldList[[i]]))
    {
      tempList[[paste(text, fieldList[[i]], sep = " ")]] <- fieldList[[i]]
    }
    else
    {
      tempList[[fieldList[[i]]]] <- fieldList[[i]]
    }
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



