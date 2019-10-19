library(readr)

# Run the file that contain all the functions
source("Riesgo Mercado/Articulo/Functions.R")

# Import data from Metatrader
path = "C:/Users/Diego Oquendo/AppData/Roaming/MetaQuotes/Terminal/D0E8209F77C8CF37AD8BF550E51FF075/MQL5/Files/Dataset.csv"
Data = read.csv(path, header = TRUE, sep = ";", encoding = "UTF-16LE", skipNul = TRUE)

p = 4 # Number of delays for the AR model
Data = getDataBase(Data, p)

# Run the GA to find the best coefficients for the AR model
coef = getARCoefficients(Data, p)
coef = t(coef)

# Run the Madaline model
X = Data[,2:length(Data[1,])]
yd = cbind(Data[,1])
# NO: Hidden neurons
# NO = ND/(1+p) - Represent the max number of hidden neurons
NO = 10
ND = length(X[,1])
alfa = 0.0001
NIT = 100 # Number of iterations
modelValues = runMadaline(X, yd, p, NO, ND, alfa, NIT)

# Write the data in a .csv
path1 = "C:/Users/Diego Oquendo/AppData/Roaming/MetaQuotes/Terminal/D0E8209F77C8CF37AD8BF550E51FF075/MQL5/Files/ModelValues.csv"
write.csv(modelValues, file = path1, row.names = FALSE)

path2 = "C:/Users/Diego Oquendo/AppData/Roaming/MetaQuotes/Terminal/D0E8209F77C8CF37AD8BF550E51FF075/MQL5/Files/GeneticValues.csv"
write.csv(coef[1:(length(coef)-2)], file = path2, row.names = FALSE)
