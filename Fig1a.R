library(rworldmap)

# example script for plotting countries on world map

theCountries=c("DEU","COD","BFA") # these are ISO3 codes for countries
triDF = data.frame(country=c("DEU","COD","BFA"),dates=c(1998,1999,2000)) # dataframe for countries with multiple colours
triMap=joinCountryData2Map(triDF,joinCode="ISO3",nameJoinColumn="country") # merge
mapCountryData(triMap,nameColumnToPlot="dates",catMethod="categorical",missingCountryCol=gray(.8),colourPalette=c("#fffbff","#efe3f1","#decbe4","#ccb4d7","#b99ecb","#a589c0","#8f75b6","#7861ac","#5e4fa2")) # specify colours via hex codes

# For one colour, the data frame is all the same (e.g. 1,1,1) and use the same hex code
