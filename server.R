
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
  

  output$About <- renderText({"This app displys a semantically-enabled summary of the Marine Institute's commercial fishery sampling data.  The source code can be found at <a href='https://github.com/davidcurrie2001/SemanticSummary' target='_blank'>davidcurrie2001/SemanticSummary</a>.  <br/><br/>IUCN Red List data is provided by the EU Directorate-General for Environment (DG ENV) and the International Union for Conservation of Nature (IUCN) and remains their copyright (<a href='https://www.eea.europa.eu/legal/copyright' target='_blank'>https://www.eea.europa.eu/legal/copyright</a>).  More information can be found at <a href='http://ec.europa.eu/environment/nature/conservation/species/redlist/index_en.htm' target='_blank'>http://ec.europa.eu/environment/nature/conservation/species/redlist/index_en.htm</a>."})
  
  # Load our data first

  
  if(useLiveData=="true"){
    # Takes longer to load live data - show loading message
    showModal(modalDialog("Please wait - loading data.", footer=NULL))
    
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
    #fishRedListFrame<-LoadRedList("")
    #shortRedList <-fishRedListFrame[fishRedListFrame$name %in% summaryData$SciName,]
    shortRedList <- LoadRedListFromAPI("",unique(summaryData$SciName))
    
    # Finished loading data
    removeModal()
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
    #fishRedListFrame<-LoadRedList("fishRedList.rds")
    #shortRedList <-fishRedListFrame[fishRedListFrame$name %in% summaryData$SciName,]
    shortRedList <- LoadRedListFromAPI("RedListAPI.rds",NA)
  }
  
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
      #SelectedSpeciesFromFrame<-StatusFrame[StatusFrame$statusLonger==selectedStatus,]
      #currentData<-currentData[currentData$species %in% SelectedSpeciesFromFrame$species,]
      SelectedSpeciesFromFrame<-shortRedList[shortRedList$statusLonger==selectedStatus,]
      # need to handle NAs
      SelectedSpeciesFromFrame<-na.omit(SelectedSpeciesFromFrame)
      currentData<-currentData[currentData$SciName %in% SelectedSpeciesFromFrame$name,]
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
  
  # Remove some rubbish from text - needs to be done in this file because the special characters get lost when using "source"
  cleanString <- function(myText){
    
    output <- myText
    output<- gsub("Â","",output)
    output<- gsub("â€“","",output)
    output<- gsub("â","",output)
    
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
      
      #species <- unique(currentData$species)
      #newStatusList <- sort(StatusFrame[StatusFrame$species %in% species,"statusLonger"])
      #newStatusList <- sort(unique(shortRedList[shortRedList$name %in% currentData$SciName,"statusLonger"]))
      newStatusListFrame <- unique(shortRedList[shortRedList$name %in% currentData$SciName,"statusLonger"])
      newStatusListFrame <- newStatusListFrame[order(newStatusListFrame$statusLonger),]
      newStatusListFrame <- newStatusListFrame[newStatusListFrame$statusLonger != "",]
      newStatusList <- newStatusListFrame$statusLonger
      
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
  
  stockbookURL<-"https://shiny.marine.ie/stockbook?stock="
  icesGroupsURL<- "https://www.ices.dk/community/groups/Pages/"
  
  # If we have summaryData
  if(length(summaryData)>0){
    
    currentData <- FilterSummaryData(selectedSpecies,selectedStock,selectedArea,selectedStatus,selectedWG,selectedGear)
    stockData <- stockFrame[stockFrame$stock %in% currentData$stock,]
      
  
    stockData$stock <- trimIRI(stockData$stock)
    stockData$StockCode  <- stockData$stock
    #stockData[is.na(stockData$StockCode),"StockCode"] <- ""

    #stockData$StockCode <- paste("<a href='",stockbookURL,stockData$StockCode,"' target='_blank'  >",stockData$StockCode,"</a>",sep="")

    stockData$wg <- trimIRI(stockData$wg)
    stockData$WorkingGroup <- stockData$wg

    # Create the hyperlinks (if we have data)
    if(nrow(stockData)>0){
      stockData$StockCode <- paste("<a href='",stockbookURL,stockData$StockCode,"' target='_blank'  >",stockData$StockCode,"</a>",sep="")
      stockData$WorkingGroup <-paste("<a href='",icesGroupsURL,stockData$WorkingGroup,".aspx' target='_blank'  >",stockData$WorkingGroup,"</a>",sep="")
    }
    
    # Working group can sometimes be blank - need to handle this
    #stockData$WorkingGroup <-paste("<a href='",icesGroupsURL,stockData$WorkingGroup,".aspx' target='_blank'  >",stockData$WorkingGroup,"</a>",sep="")

    # sort the data
    stockData<-stockData[order(stockData$stock),c("StockCode","StockName","WorkingGroup")]
    # if we are not showing the division field it will look liek we have duplicate stock names so use unique
    stockData<-unique(stockData)
  
    stockData
    
  }

  }, options = list(pageLength = 5),escape=FALSE)
  
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
    
    outputText <- formatDBPediaAbstract(selectedSpecies, speciesInfoFrame, TRUE)
    
    outputText <- paste(outputText,"<br/><br/>",sep="")
    
    outputText
    
  })
  
  # try and get the DBpedia image
  output$thumbnail <- renderText({
    
    selectedSpecies <- input$SpeciesSelect
    
    outputText <- formatDBPediaImage(selectedSpecies, speciesInfoFrame)
    
    if (outputText!= "") {
      outputText <- c('<img src="',outputText,'">')
      
    }
    
    outputText
    
  })
  
  # Set the title
  output$title <- renderText({
    
    selectedSpecies <- input$SpeciesSelect
    
    outputText <- ""

    if(selectedSpecies != DefaultText && selectedSpecies != ""){
      
      myTitle <- selectedSpecies

      outputText<- paste('<h2>',myTitle,'</h2>',sep='')

    } 
    
    outputText
    
  })
  
  # Get the Species Dashboard link
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
      
      outputText<-paste('<a href="',speciesDashURL,'" target="_blank"> Marine Institute Species Dashboard: ',selectedSpecies,'</a>',sep='')

    } 
    
    outputText
    
  })
  
  
  # try and get the Red List status
  output$redListStatus <- renderText({
    
    selectedSpecies <- input$SpeciesSelect
    
    outputText <- formatRedListStatus(selectedSpecies,summaryData,shortRedList)
    
    outputText
    
  })
  
  # try and get the Red List rationale
  output$redListRationale <- renderText({
    
    selectedSpecies <- input$SpeciesSelect
    
    outputText <- formatRedListRationale(selectedSpecies,summaryData,shortRedList, TRUE)

    outputText <- cleanString(outputText)
    
    outputText
    
  })
  

  
  
  # Handle the "More" button
  observeEvent(input$More, {
    
    selectedSpecies <- input$SpeciesSelect
    
    modalBody <- ""
    myTitle <- ""
    myAbstract <-""
    myImage <- ""
    myRedListValue <- ""
    myRedListRationale <- ""
    
    
    if(selectedSpecies != DefaultText && selectedSpecies != ""){
      
      # Title
      myTitle <- selectedSpecies
      myAbstract <- formatDBPediaAbstract(selectedSpecies, speciesInfoFrame, FALSE)
      myImage <- formatDBPediaImage(selectedSpecies, speciesInfoFrame)
      myRedListValue <- formatRedListStatus(selectedSpecies,summaryData, shortRedList)
      myRedListRationale <- formatRedListRationale(selectedSpecies,summaryData,shortRedList, FALSE)
      myRedListRationale<- cleanString(myRedListRationale)
      

      if (myTitle != ""){
        modalBody<- paste(modalBody,'<h2>',myTitle,'</h2></br></br>',sep='')
      }
      if (myImage != ""){
        modalBody<- paste(modalBody,'<img src="',myImage,'"></br></br>',sep='')
      }
      if (myAbstract != ""){
        modalBody<- paste(modalBody,myAbstract,'</br></br>',sep='')
      }
      if (myRedListValue != ""){
        modalBody<- paste(modalBody,myRedListValue,'</br></br>',sep='')
      }
      if (myRedListRationale != ""){
        modalBody<- paste(modalBody,myRedListRationale,'</br></br>',sep='')
      }

    }

    showModal(modalDialog( HTML(modalBody),title="Species information"))
    
  })
  
  


})
