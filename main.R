#setwd("~/Documents/perso/R/arknights_optiization")

library(jsonlite)



df_items <- fromJSON("items.json")
df_droprates <- fromJSON("item_drop_rate.json")$matrix
craft <- fromJSON("formula.json")
