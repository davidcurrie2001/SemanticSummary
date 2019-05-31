library(SPARQL)
library(plotly)

library(rredlist)
library(jsonlite)
library(RCurl)
library(data.table)

# My functions are stored in this file to make the sevrer file tidier and easier to read
source("functions.R")

speciesForList <- LoadSpeciesList("")
saveRDS(speciesForList, file = "speciesData.rds")

speciesInfoFrame <- LoadSpeciesInfoFrame(speciesForList$name,"")
saveRDS(speciesInfoFrame, file = "speciesInfoFrame.rds")

stockFrame <- LoadStockList("")
saveRDS(stockFrame, file = "stockData.rds")

AreaFrame<-LoadAreaList("")
saveRDS(AreaFrame, file = "areaData.rds")

GearFrame <- LoadGearFrame("")
saveRDS(GearFrame, file = "gearData.rds")

StatusFrame <- LoadConservationFrame("")
saveRDS(StatusFrame, file = "conservationData.rds")

summaryData <- LoadSummaryData("")
saveRDS(summaryData, file = "summaryData.rds")

fishRedListFrame<-LoadRedList("")
saveRDS(fishRedListFrame, file = "fishRedList.rds" )

# TODO remove once sorted

summaryData <- LoadSummaryData("summaryData.rds")
speciesToCheck <- sort(unique(summaryData$SciName))

apiKey <- readRDS(file = "apiKey.rds")
result <- lapply(speciesToCheck[c(1:10)], getRedListAPIDataForSpecies)

result1 <- rbindlist(result)

View(result1)

                      