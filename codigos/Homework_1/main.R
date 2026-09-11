# importing rstudioapi package
library("rstudioapi") 

# retrieving path from getSourceEditorContext() 
# using $ operator 
k <- getSourceEditorContext()$path 

# Move to your working directory
setwd(dirname(k))    				# change to your directory

#554934
#546474
matricula <- 578412

dados <- read.csv("HW1_bike_sharing.csv")

print(dados)

seed <- 1+ (matricula %% 100)

datagroup <- dados[[2]] 
datagroup
