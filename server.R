
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(SPARQL)
library(plotly)

# Most of my functions are stored in this file to make the sevrer file tidier and easier to read
source("functions.R")



shinyServer(function(input, output, session) {
  

  # IMPORTANT: Change this value to "true" if you want to load from a SPARQL end-point (define the address in functions.R)
  useLiveData<-"false"

  # Load our data first
  showModal(modalDialog("Please wait - loading data.
                          This app displys a semantically-enabled summary of the Marine Institute's commercial fishery sampling data.", footer=NULL))
  
  if(useLiveData=="true"){
    speciesForList <- LoadSpeciesList("")
    speciesInfoFrame <- LoadSpeciesInfoFrame(speciesForList$name,"")
    speciesInfoFrame <- merge(x=speciesForList, y=speciesInfoFrame, by.x="name", by.y="untypedName", all.x = FALSE)
    stockFrame <- LoadStockList("")
    AreaFrame<-LoadAreaList("")
    AreasForList<-unique(AreaFrame[order(AreaFrame$higherAreaTrimmed),c("higherAreaTrimmed")])
    GearFrame <- LoadGearFrame("")
    GearForList <- sort(unique(GearFrame$gearDisplayName))
    StatusFrame <- LoadConservationFrame("")
    StatusForList <- unique(StatusFrame[order(StatusFrame$statusLonger),c("statusLonger")])
    summaryData <- LoadSummaryData("")
  } 
  # else load from RDS files
  else {
    speciesForList <- LoadSpeciesList("speciesData.rds")
    speciesInfoFrame <- LoadSpeciesInfoFrame(speciesForList$name,"speciesInfoFrame.rds")
    speciesInfoFrame <- merge(x=speciesForList, y=speciesInfoFrame, by.x="name", by.y="untypedName", all.x = FALSE)
    stockFrame <- LoadStockList("stockData.rds")
    AreaFrame<-LoadAreaList("areaData.rds")
    AreasForList<-unique(AreaFrame[order(AreaFrame$higherAreaTrimmed),c("higherAreaTrimmed")])
    GearFrame <- LoadGearFrame("gearData.rds")
    GearForList <- sort(unique(GearFrame$gearDisplayName))
    StatusFrame <- LoadConservationFrame("conservationData.rds")
    StatusForList <- unique(StatusFrame[order(StatusFrame$statusLonger),c("statusLonger")])
    summaryData <- LoadSummaryData("summaryData.rds")
  }
  
  # Finished loading data
  removeModal()

  
  DefaultText <- "(Any)"
  
  
  # This function is stored here because it uses soem of the objects loaded in by the app
  FilterSummaryData<-function(selectedSpecies,selectedStock,selectedArea,selectedStatus,selectedWG,selectedGear){
    
    # Filter the data using the selected values
    currentData <- summaryData
    
    if (selectedSpecies != DefaultText){
      currentData <- currentData[currentData$CommonName==selectedSpecies,]
    }
    if (selectedStock != DefaultText){
      currentData <- currentData[regexpr(selectedStock, currentData$stock) > -1,]
    }
    if (selectedArea != DefaultText){
      SelectedAreasFromFrame<-AreaFrame[AreaFrame$higherAreaTrimmed==selectedArea,]
      currentData<-currentData[currentData$div %in% SelectedAreasFromFrame$area,]
    }
    if(selectedStatus!=DefaultText){
      SelectedSpeciesFromFrame<-StatusFrame[StatusFrame$statusLonger==selectedStatus,]
      currentData<-currentData[currentData$species %in% SelectedSpeciesFromFrame$species,]
    }
    if (selectedWG != DefaultText){
      # Need to handle NA values
      NoNaStockFrame<-stockFrame[!is.na(stockFrame$wg),]
      SelectedStocksFromFrame<-NoNaStockFrame[trimIRI(NoNaStockFrame$wg)==selectedWG,]
      currentData<-currentData[currentData$stock %in% SelectedStocksFromFrame$stock,]
    }
    if(selectedGear != DefaultText){
      SelectedGearsFromFrame <- GearFrame[GearFrame$gearDisplayName==selectedGear,]
      currentData<-currentData[currentData$gear %in% SelectedGearsFromFrame$gear,]
    }
    
    currentData
    
  }
  


  # handle drop down list filters
  observe({
    

    selectedSpecies <- input$SpeciesSelect
    selectedStock <- input$StockSelect
    selectedArea <- input$AreaSelect
    selectedStatus <- input$StatusSelect
    selectedWG <- input$WGSelect
    selectedGear <- input$GearSelect
    
    # If we have summaryData
    if(length(summaryData)>0){
    
      currentData <- FilterSummaryData(selectedSpecies,selectedStock,selectedArea,selectedStatus,selectedWG,selectedGear)
      
      # Now generate our drop-down lists based on the values in the data
      
      newSpeciesList <- sort(unique(currentData$CommonName))
      newStockList <- sort(unique(trimIRI(currentData$stock)))
      
      divs <- unique(currentData$div)
      newAreaList <- sort(unique(AreaFrame[AreaFrame$area %in% divs,"higherAreaTrimmed"]))
      
      species <- unique(currentData$species)
      newStatusList <- sort(StatusFrame[StatusFrame$species %in% species,"statusLonger"])
      
      stockData <- stockFrame[stockFrame$stock %in% currentData$stock,]
      newWGList <- sort(unique(trimIRI(stockData$wg)))
      
      gearData <- GearFrame[GearFrame$gear %in% currentData$gear,]
      newGearList <- sort(unique(gearData$gearDisplayName))
      
      
      updateSelectInput(session,"SpeciesSelect",choices=c(DefaultText,newSpeciesList),selected = selectedSpecies)
      updateSelectInput(session, "StockSelect", choices = c(DefaultText, newStockList),selected = selectedStock)
      updateSelectInput(session,"AreaSelect",choices=c(DefaultText, newAreaList),selected = selectedArea)
      updateSelectInput(session,"StatusSelect",choices=c(DefaultText,newStatusList),selected = selectedStatus)
      updateSelectInput(session,"WGSelect",choices=c(DefaultText,newWGList),selected = selectedWG)
      updateSelectInput(session,"GearSelect",choices=c(DefaultText,newGearList),selected = selectedGear)
    
    }
    
  })
  
  
  # Display list of stocks
  output$StockList <- renderDataTable({

  selectedSpecies <- input$SpeciesSelect
  selectedStock <- input$StockSelect
  selectedArea <- input$AreaSelect
  selectedStatus <- input$StatusSelect
  selectedWG <- input$WGSelect
  selectedGear <- input$GearSelect
  
  # If we have summaryData
  if(length(summaryData)>0){
    
    currentData <- FilterSummaryData(selectedSpecies,selectedStock,selectedArea,selectedStatus,selectedWG,selectedGear)
    stockData <- stockFrame[stockFrame$stock %in% currentData$stock,]
      
  
    stockData$stock <- trimIRI(stockData$stock)
    stockData$wg <- trimIRI(stockData$wg)
    # sort the data
    stockData<-stockData[order(stockData$stock),c("stock","StockName","wg")]
    # if we are not showing the division field it will look liek we have duplicate stock names so use unique
    stockData<-unique(stockData)
  
    stockData
    
  }

  }, options = list(pageLength = 5))
  
  # Display summary data table
  output$SummaryDataTable <- renderDataTable({

    selectedSpecies <- input$SpeciesSelect
    selectedStock <- input$StockSelect
    selectedArea <- input$AreaSelect
    selectedStatus <- input$StatusSelect
    selectedWG <- input$WGSelect
    selectedGear <- input$GearSelect
    
    # If we have summaryData
    if(length(summaryData)>0){
    
      dataToGroup <- FilterSummaryData(selectedSpecies,selectedStock,selectedArea,selectedStatus,selectedWG,selectedGear)
  
      # If we have soem data then aggregate it
      if (nrow(dataToGroup)>0){
        
        dataToGroup$TotalLengths<-dataToGroup$PortLengths + dataToGroup$DemLengths + dataToGroup$PelLengths
        dataToGroup$TotalBios<-dataToGroup$PortBios + dataToGroup$DemBios + dataToGroup$PelBios
        dataToGroup$TotalAges<-dataToGroup$PortAges + dataToGroup$DemAges + dataToGroup$PelAges
  
        #groupedData<-aggregate(cbind(PortLengths,PortAges,PortBios)~year+SciName+CommonName, dataToGroup, sum)
        #groupedData <- groupedData[order(-as.numeric(groupedData$year) ),c("year","SciName","CommonName", "PortLengths", "PortBios", "PortAges")]
        
        groupedData<-aggregate(cbind(TotalLengths,TotalAges,TotalBios)~year+SciName+CommonName, dataToGroup, sum)
        groupedData <- groupedData[order(-as.numeric(groupedData$year) ),c("year","SciName","CommonName", "TotalLengths", "TotalBios", "TotalAges")]
  
        colnames(groupedData)[colnames(groupedData)=="year"] <- "Year"
        colnames(groupedData)[colnames(groupedData)=="SciName"] <- "Species"
        colnames(groupedData)[colnames(groupedData)=="CommonName"] <- "Common Name"
        #colnames(groupedData)[colnames(groupedData)=="PortLengths"] <- "Length Measurements"
        #colnames(groupedData)[colnames(groupedData)=="PortBios"] <- "Biological Measurements"
        #colnames(groupedData)[colnames(groupedData)=="PortAges"] <- "Age Measurements"
        colnames(groupedData)[colnames(groupedData)=="TotalLengths"] <- "Length Measurements"
        colnames(groupedData)[colnames(groupedData)=="TotalBios"] <- "Biological Measurements"
        colnames(groupedData)[colnames(groupedData)=="TotalAges"] <- "Age Measurements"
  
      } else {
        groupedData <- NA
      }
  
      groupedData
    
    }

  })
  
  # Display bar chart
  output$SummaryPlot <- renderPlotly({

    selectedSpecies <- input$SpeciesSelect
    selectedStock <- input$StockSelect
    selectedArea <- input$AreaSelect
    selectedStatus <- input$StatusSelect
    selectedWG <- input$WGSelect
    selectedGear <- input$GearSelect
    
    # If we have summaryData
    if(length(summaryData)>0){
    
    dataToGroup <- FilterSummaryData(selectedSpecies,selectedStock,selectedArea,selectedStatus,selectedWG,selectedGear)

      # If we have soem data - group it
      if (nrow(dataToGroup)>0) {
  
        dataToGroup$TotalLengths<-dataToGroup$PortLengths + dataToGroup$DemLengths + dataToGroup$PelLengths
        dataToGroup$TotalBios<-dataToGroup$PortBios + dataToGroup$DemBios + dataToGroup$PelBios
        dataToGroup$TotalAges<-dataToGroup$PortAges + dataToGroup$DemAges + dataToGroup$PelAges
        
        #groupedData<-aggregate(cbind(PortLengths,PortAges,PortBios)~year, dataToGroup, sum)
        groupedData<-aggregate(cbind(TotalLengths,TotalAges,TotalBios)~year, dataToGroup, sum)
  
        # if we have some grouped data - plot it
        if (nrow(dataToGroup)>0){
  
          x <- list(title = "Year")
          y <- list(title = "Number")
  
          #plot_ly(groupedData, x = ~year, y = ~PortLengths, type = 'bar', name = 'Length Measurements')  %>%
          #     add_trace(y = ~PortBios, name = 'Biological Measurements') %>%
          #     add_trace(y = ~PortAges, name = 'Age Measurements') %>%
          #     layout(xaxis = x, yaxis = y)
          
          plot_ly(groupedData, x = ~year, y = ~TotalLengths, type = 'bar', name = 'Length Measurements')  %>%
            add_trace(y = ~TotalBios, name = 'Biological Measurements') %>%
            add_trace(y = ~TotalAges, name = 'Age Measurements') %>%
            layout(xaxis = x, yaxis = y)
          
        }
      
      } 
    }
    # empty chart as a placeholder
    else {
      plotly_empty(type = "bar")

    }

  })
  
  # try and get the DBpedia abstract
  output$abstract <- renderText({
    
    selectedSpecies <- input$SpeciesSelect
    
    outputText <- ""
    
    if(selectedSpecies != DefaultText && selectedSpecies != ""){
      
      selectedSpecies <- speciesInfoFrame[speciesInfoFrame$CommonName==input$SpeciesSelect,]
      if (length(selectedSpecies)>0){
        outputText <- selectedSpecies$Abstract
      }

    } 
    
    outputText
    
  })
  
  # try and get the DBpedia image
  output$thumbnail <- renderText({
    
    selectedSpecies <- input$SpeciesSelect
    
    outputText <- ""
    
    if(selectedSpecies != DefaultText && selectedSpecies != ""){
      
      selectedSpecies <- speciesInfoFrame[speciesInfoFrame$CommonName==input$SpeciesSelect,]
      if (length(selectedSpecies)>0){
        outputImage <- selectedSpecies$image
        # need to remove the first and last characters
        outputImage <- substring(outputImage,2)
        outputImage <- substring(outputImage,1,nchar(outputImage)-1)
        outputText <- c('<img src="',outputImage,'">')
      }
      
    } 
    
    outputText
    
  })
  
  # try and get the DBpedia link
  output$title <- renderText({
    
    selectedSpecies <- input$SpeciesSelect
    
    outputText <- ""
    myLink <-
    
    if(selectedSpecies != DefaultText && selectedSpecies != ""){
      
      myTitle <- input$SpeciesSelect
      
      selectedSpecies <- speciesInfoFrame[speciesInfoFrame$CommonName==input$SpeciesSelect,]
      
      if (length(selectedSpecies)>0){
        
        myLink <- selectedSpecies$page
        myLink <- substring(myLink,2)
        myLink <- substring(myLink,1,nchar(myLink)-1)
      
      }
      
      if (length(myLink)>0){
        outputText<- paste('<h2><a href="',myLink,'" target="_blank">',myTitle,'</a></h2>',sep='')
      } else {
        outputText<- paste('<h2>',myTitle,'</h2>',sep='')
      }
      
    } 
    
  
    
    outputText
    
  })
  
  output$speciesdash <- renderText({
    
    selectedSpecies <- input$SpeciesSelect
    
    outputText <- ""
    speciesDashURL<-"https://shiny.marine.ie/speciesdash/"

    if(selectedSpecies != DefaultText && selectedSpecies != ""){
      
      IC_speciesnames<-unique(na.omit(summaryData[,c("CommonName","ICspecies")]))
      
      ThreeLetterCode<-trimIRI(IC_speciesnames[IC_speciesnames$CommonName==selectedSpecies,"ICspecies"])
      
      if(length(ThreeLetterCode)>0){
        speciesDashURL<-paste(speciesDashURL,"?species=",ThreeLetterCode,sep="")
      }
      
      outputText<-paste('<a href="',speciesDashURL,'" target="_blank"> MI Species Dashboard link </a>',sep='')

    } 
    
    outputText
    
  })
  
  


})
