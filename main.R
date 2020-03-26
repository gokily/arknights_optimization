#setwd("~/Documents/perso/R/arknights_optiization")

library(jsonlite)

getCleanMaterialDF <- function()
{
  df_items <- fromJSON("items.json")
  material = subset(df_items, df_items$itemType == "MATERIAL")
  material = material[order(material$sortId),]
  material = material[-1,]
  material = subset(material, material$sortId < 200006)
 
  material$dropable = NA
  return(material)
}


fillConversionMatrix <- function(matrix, productId, nIngredient,  ...)
{
  itemId = colnames(matrix)
  index = which(itemId == productId)
  print(index)
  for (i in 1: nIngredient)
  {
    index2 = which(itemId == ...elt(2 * i - 1))
    matrix[index, index2] = ...elt(2 * i)
  }
  return(matrix)
}




###################################################################################


###################################################################################

material <- getCleanMaterialDF()
material2 <- material[,c(1,8,9)]

df_droprates <- fromJSON("item_drop_rate.json")$matrix

# dropableItems = unique(df_droprates$itemId)
# dropableItems = as.data.frame(dropableItems)
# colnames(dropableItems) <- c("itemId")
# 
dropableItems = subset(dropableItems, is.element(dropableItems$itemId, material$itemId))
# 
# dropableItems$name = NA
# for (i in 1:dim(dropableItems)[1])
# {
#   dropableItems$name[i] = material$name_i18n$en[material$itemId == dropableItems$itemId[i]]
# }

itemId = material$itemId
conversionMatrix <- matrix(0, nrow = dim(material)[1], ncol = dim(material)[1], dimnames = list(material$itemId, material$itemId))
for (i in 1:6)
{
  baseVal = 30000 + i * 10
  conversionMatrix <- fillConversionMatrix(conversionMatrix ,baseVal + 2, 1, baseVal + 1, 3)
  if (i == 1)
  {
    conversionMatrix <- fillConversionMatrix(conversionMatrix, baseVal + 3, 1, baseVal + 2, 5)
    conversionMatrix <- fillConversionMatrix(conversionMatrix, baseVal + 5, 1, baseVal + 3, 4)
  }
  else 
  {
    conversionMatrix <- fillConversionMatrix(conversionMatrix, baseVal + 3, 1, baseVal + 2, 4)
  }
}
conversionMatrix <- fillConversionMatrix(conversionMatrix, 30104, 3, 30103, 1, 30013, 2, 30053, 2)
conversionMatrix <- fillConversionMatrix(conversionMatrix, 30094, 3, 30093, 1, 30043, 1, 30063, 1)
conversionMatrix <- fillConversionMatrix(conversionMatrix, 30084, 3, 30083, 2, 30033, 1, 30073, 1)
conversionMatrix <- fillConversionMatrix(conversionMatrix, 30074, 3, 30073, 1, 30023, 1, 30103, 1)
conversionMatrix <- fillConversionMatrix(conversionMatrix, 30064, 3, 30063, 1, 30013, 1, 30093, 1)
conversionMatrix <- fillConversionMatrix(conversionMatrix, 30054, 3, 30053, 2, 30023, 1, 30083, 1)
conversionMatrix <- fillConversionMatrix(conversionMatrix, 30044, 3, 30043, 2, 30063, 1, 30033, 1)
conversionMatrix <- fillConversionMatrix(conversionMatrix, 30034, 3, 30033, 2, 30053, 1, 30073, 1)
conversionMatrix <- fillConversionMatrix(conversionMatrix, 30024, 3, 30023, 2, 30043, 1, 30083, 1)

dim(conversionMatrix)[1]


convert2BasicMat <- function(mat, itemVector)
{
  # mat = conversionMatrix
  # itemVector = c(3)
  for (i in 1:length(itemVector))
  {
    print(i)
    substrat = which(mat[itemVector[i],] != 0)
    print(substrat)
    if (length(substrat) == 0)
    {
      next()
    }
    for (j in 1:length(substrat))
    {
      print(j)
      substrat2 = which(mat[substrat[j],] != 0)
      print(substrat2)
      if (length(substrat2) == 0)
      {
        next()
      }
      for (k in 1:length(substrat2))
      {
        mat[itemVector[i], substrat2[k]] = mat[substrat[j],substrat2[k]] * mat[itemVector[i],substrat[j]]
      }
      mat[itemVector[i],substrat[j]] = 0;
    }
    
  }
  return(mat)
}
conversionMatrix <- convert2BasicMat(conversionMatrix, which(as.numeric(itemId)%%10 == 3))
conversionMatrix <- convert2BasicMat(conversionMatrix, which(as.numeric(itemId)%%10 == 4))


baseMatId <- c(30073, 30083, 30093, 30103, 30011, 30061, 30031, 30021, 30041, 30051)
for (i in 1:length(baseMatId))
{
  index = which(colnames(conversionMatrix) == baseMatId[i])
  conversionMatrix[index,index] = 1
}
convMatrix <- conversionMatrix[,is.element(itemId, baseMatId)]

stages <- fromJSON("stages.json")

dim(stages)
stages2 <- subset.data.frame(stages, stages$stageType == "MAIN" | stages$stageType == "SUB")

stages3 <- stages2[,1:5]

stageID = stages3$stageId

baseMatDrop = matrix(0, nrow = length(stageID), ncol = length(baseMatId))
colnames(baseMatDrop) <- colnames(convMatrix)
baseMatDrop = as.data.frame(baseMatDrop)
stages4 = cbind(stages3, baseMatDrop)

df_droprates2 = subset(df_droprates, is.element(df_droprates$itemId, dropableItems$itemId))

for (i in 1:length(stageID))
{
  drops = subset(df_droprates2, df_droprates2$stageId == stageID[i])
  if(nrow(drops) == 0)
  {
    next()
  }
  for (j in 1:nrow(drops))
  {
    item = drops[j,]
    stages4[i, 6:15] = stages4[i, 6:15] + convMatrix[which(rownames(convMatrix) == item$itemId),] * item$quantity / item$times
  }
}
stages4 = subset(stages4, stages4$apCost != 0)

mod1 = lm(formula = apCost ~ `30073` + `30083` + `30093` + `30103`  + `30011` + `30061` + `30031` + `30021` + `30041` + `30051`, data = stages4)
sum1 = summary(mod1)
val1 = sum1$coefficients[-1,1]
base1 =  sum1$coefficients[1,1]
stages4$realCost = stages4$apCost

for (i in 1:10)
{
  stages4$realCost = stages4$realCost + stages4[, i +5] * (val1[i])
}
stages4$margin = (stages4$realCost - stages4$apCost) / stages4$apCost

stages5 <- subset(stages4, stages4$margin > median(stages4$margin))

mod2 = lm(formula = apCost ~ `30073` + `30083` + `30093` + `30103`  + `30011` + `30061` + `30031` + `30021` + `30041` + `30051`, data = stages5)
sum2 = summary(mod2)
val2 = sum2$coefficients[-1,1]
base2 =  sum1$coefficients[1,1]
stages5$realCost = stages5$apCost

for (i in 1:10)
{
  stages5$realCost = stages5$realCost + stages5[, i +5] * (val2[i])
}

stages5$margin = (stages5$realCost - stages5$apCost) / stages5$apCost






#write.table(stages4, "stages4.txt")
#write.table(stages5, "stages5.txt")


##############################################################################################################################
stages4 = read.table("stages4.txt")
stages5 = read.table("stages5.txt")

