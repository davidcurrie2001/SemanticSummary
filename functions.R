#This file will be included in the Shiny app

# Sparql endpoint 
myEndpoint <- "http://10.11.1.70:3030/test/query"

myPrefix <- "PREFIX mi:   <http://www.marine.ie/SemanticFishData#>
PREFIX IC_Sub-divisions: <https://vocab.ices.dk/services/rdf/collection/IC_Sub-divisions/>
PREFIX IC_Divisions: <https://vocab.ices.dk/services/rdf/collection/IC_Divisions/>
PREFIX IC_Sub-areas: <https://vocab.ices.dk/services/rdf/collection/IC_Sub-areas/>
PREFIX IC_AreaTopLevel: <https://vocab.ices.dk/services/rdf/collection/IC_AreaTopLevel/>
PREFIX IC_Species: <https://vocab.ices.dk/services/rdf/collection/IC_Species/>
PREFIX IC_GearType: <https://vocab.ices.dk/services/rdf/collection/IC_GearType/>
PREFIX SpecWoRMS: <https://vocab.ices.dk/services/rdf/collection/SpecWoRMS/>
PREFIX SpecASFIS: <https://vocab.ices.dk/services/rdf/collection/SpecASFIS/>
PREFIX ices: <https://vocab.ices.dk/services/rdf/collection/>
PREFIX skos:<http://www.w3.org/2004/02/skos/core#>
PREFIX dbo: <http://dbpedia.org/ontology/>
PREFIX dbp: <http://dbpedia.org/property/>
PREFIX dbr: <http://dbpedia.org/resource/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"

# Get the end part of IRIs for display
trimIRI<- function(IRI_List){
  
  matches <- regexpr("\\/[^\\/]*$", IRI_List)
  trimmedList <- substring(IRI_List,matches+1,nchar(IRI_List)-1)
}

# run the supplied sparql with the prefix and return the results
runSparql<-function(mySpaqrl){
  myQuery <- paste(myPrefix,mySpaqrl,sep = " ")
  qd <- SPARQL(myEndpoint,myQuery)
  df <- qd$results
  return (df)
}

# For a given untyped, lower-case scientific name try and retrive species infromation from dbpedia
getSpeciesInfoFromDBPedia<-function(sciName){
  
  # First part of sparql query
  mySparql1 <- 'SELECT ?species (str(?name) as ?Name) ?image (str(?abstract) as ?Abstract) ?page ?origPage ?map ("'
  
  # second part of sparql query
  mySparql2 <- '" as ?untypedName) WHERE
  {  
  ?species mi:binomialUntyped "'
  
  # third part of sparql query
  mySparql3 <- '" .
  ?species dbp:binomial ?name
  SERVICE <http://dbpedia.org/sparql> {
  ?species dbo:thumbnail ?image  .  
  ?species dbo:abstract ?abstract . 
  OPTIONAL { ?species <http://xmlns.com/foaf/0.1/isPrimaryTopicOf> ?page . }
  OPTIONAL { ?species <http://www.w3.org/ns/prov#wasDerivedFrom> ?origPage . }
  OPTIONAL { ?species dbp:rangeMap ?map . }
  }
  FILTER (lang(?abstract) = "en")
  }
  LIMIT 1'
  
  mySparqlAll <- paste(mySparql1,sciName,mySparql2,sciName,mySparql3, sep = "")
  #print(mySparqlAll)
  
  r <- runSparql(mySparqlAll)
  
}

LoadSpeciesList<- function(fileName){
  
  # Try reading from an RDS file if a filename is given
  if (fileName!= ""){
    
    speciesForList<-readRDS(file = fileName)
    
  }
  # else run a sparql query
  else {

    # get our list of species for a drop down list
    r <- runSparql('
                   SELECT ?species ?name ?prefLabel (Str(?commonName) as ?CommonName) (sum(xsd:double(?lengths)) as ?lengthCount)
                   WHERE
                   {  ?sample mi:isOfSpecies ?species .
                   ?sample mi:PortSchemeLengthObs ?lengths .
                   ?species mi:binomialUntyped ?name .
                   ?species skos:prefLabel ?prefLabel .
                   ?species <http://purl.org/dc/terms/description> ?commonName .
                   
                   }
                   GROUP BY ?species ?name ?prefLabel ?commonName
                   ORDER BY DESC(?lengthCount)
                   LIMIT 30
                   ')
    speciesForList <- r
    speciesForList<-speciesForList[order(speciesForList$CommonName),]
  
  }
  
  speciesForList
  
}

# Load in the extra species information from DBpedia
LoadSpeciesInfoFrame<-function(listOfSpeciesNames, fileName){
  
  
  # Try reading from an RDS file if a filename is given
  if (fileName!= ""){
    
    speciesInfoFrame<-readRDS(file = fileName)
    
  }
  # else run a sparql query
  else {
  
    speciesInfo <- lapply(listOfSpeciesNames, getSpeciesInfoFromDBPedia)
    names(speciesInfo) <- listOfSpeciesNames
    # Stick the non-empty list elements together into a data frame
    speciesInfoFrame <- do.call(rbind, speciesInfo)
  
  }
  
  speciesInfoFrame
  
}

LoadStockList <- function(fileName){
  
  # Try reading from an RDS file if a filename is given
  if (fileName!= ""){
    
    output<-readRDS(file = fileName)
    
  }
  # else run a sparql query
  else {
  
    output <- runSparql('
                        SELECT distinct ?species ?prefLabel (Str(?commonName) as ?CommonName) ?name ?ICspecies ?div ?stock (Str(?stockName) as ?StockName) ?wg
                      WHERE  {    ?sample a mi:SamplingSummary .
                        ?sample mi:isOfArea ?div .
                        ?sample mi:isOfSpecies ?species .
                        OPTIONAL {
                        ?species <http://purl.org/dc/terms/description> ?commonName . 
                        }
                        ?species skos:prefLabel ?prefLabel .
                        ?species mi:binomialUntyped ?name . 
                        ?species owl:sameAs ?ASFISspecies .
                        ices:SpecASFIS skos:member ?ASFISspecies .
                        ?ASFISspecies owl:sameAs ?ICspecies .
                        ices:IC_Species skos:member ?ICspecies .
                        ?ICspecies skos:narrower ?stock . 
                        ices:IC_Stock skos:member ?stock . 
                        ?stock skos:related ?div . 
                        ?stock skos:prefLabel ?stockName . 
                        OPTIONAL {
                        ?stock skos:broader ?wg . 
                        ices:IC_WorkingGroup skos:member ?wg . 
                        
                        }
                        }  
                        ')
  
  }
  
  output
}

# Load the list of areas that our samples are from
LoadAreaList<- function(fileName){
  
  
  # Try reading from an RDS file if a filename is given
  if (fileName!= ""){
    
    AreaFrame<-readRDS(file = fileName)
    
  }
  # else run a sparql query
  else {
  
    r <- runSparql('
                 SELECT distinct ?higherArea ?area ?IC_group
                 WHERE
                 {  ?Sample mi:isOfArea ?area .
                 ?higherArea skos:narrower* ?area .
                 OPTIONAL {
                 ?IC_group skos:member ?higherArea
                 }
                 }
                 ')
    #View(r)
    AreaFrame <- r
    
    
    AreaFrame$IC_group<-trimIRI(AreaFrame$IC_group)
    AreaFrame$higherAreaTrimmed<-trimIRI(AreaFrame$higherArea)
    
  
  }
  
  AreaFrame
  
}

LoadGearFrame<- function(fileName){
  
  # Try reading from an RDS file if a filename is given
  if (fileName!= ""){
    
    output<-readRDS(file = fileName)
    
  }
  # else run a sparql query
  else {
  
    output <- runSparql("SELECT distinct ?gear (Str(?gearName) as ?GearName) ?gearType (Str(?gearTypeName) as ?GearTypeName) ?IC_group
              WHERE  {
                        ?sample a mi:SamplingSummary .
                        ?sample mi:isOfGear ?gear .
                        ?gear skos:prefLabel ?gearName . 
                        ?gearType skos:narrower* ?gear .
                        ?gearType skos:prefLabel ?gearTypeName . 
                        OPTIONAL {
                        ?IC_group skos:member ?gearType
                        }
                        
  }"
            )
    output$gearTrim<-trimIRI(output$gear)
    output$gearTypeTrim<-trimIRI(output$gearType)
    output$IC_groupTrim<-trimIRI(output$IC_group)
    
    #View(output[output$IC_groupTrim!="IC_GearType",])
    
    output[output$IC_groupTrim=="IC_GearClass",c("gearDisplayName")]<-paste("All", output[output$IC_groupTrim=="IC_GearClass",c("GearTypeName")])
    output[output$IC_groupTrim=="IC_GearGroup",c("gearDisplayName")]<-paste("All", output[output$IC_groupTrim=="IC_GearGroup",c("GearTypeName")])
    output[output$IC_groupTrim=="IC_GearType",c("gearDisplayName")]<-paste(output[output$IC_groupTrim=="IC_GearType",c("GearTypeName")]," (", output[output$IC_groupTrim=="IC_GearType",c("gearTypeTrim")], ")", sep="")
  
  }
  
  output
  

}


# Load the conservation status data from from DBPedia
LoadConservationFrame<- function(fileName){
  
  # Try reading from an RDS file if a filename is given
  if (fileName!= ""){
    
    output<-readRDS(file = fileName)
    
  }
  # else run a sparql query
  else {
  
    output <- runSparql('
                        SELECT  *
                        WHERE
                        { 
                        SERVICE <http://dbpedia.org/sparql>
                        { 
                        ?db_species  dbo:conservationStatus  ?status ;
                        dbo:conservationStatusSystem  ?system
                        }
                        { 
                        SELECT DISTINCT  ?species ?db_species ?bin_name
                        WHERE
                        { 
                        ?sample   rdf:type            mi:SamplingSummary .
                        ?sample   mi:isOfSpecies      ?species .
                        ?species  mi:binomialUntyped  ?bin_name .
                        ?db_species  mi:binomialUntyped  ?bin_name .
                        ?db_species dbp:binomial        ?typed_name .
                        }
                        }
                        }
                        LIMIT   1000
                        ')
    output$status<-toupper(output$status)
    output$statusLonger<-output$status
    output[output$status=='EX','statusLonger']<-'01) EX Extinct'
    output[output$status=='PE','statusLonger']<-'02) PE Probably extinct'
    output[output$status=='EW','statusLonger']<-'03) EW Extinct in the wild'
    output[output$status=='PEW','statusLonger']<-'04) PEW Probably extinct in the wild'
    output[output$status=='CR','statusLonger']<-'05) CR Critically Endangered'
    output[output$status=='EN','statusLonger']<-'06) EN Endangered'
    output[output$status=='VU','statusLonger']<-'07) VU Vulnerable'
    output[output$status=='NT','statusLonger']<-'08) NT Near Threatened'
    output[output$status=='LR/CD','statusLonger']<-'09) LR/CD Lower Risk (Conservation Dependent)'
    output[output$status=='LR/NT','statusLonger']<-'10) LR/NT Lower Risk (Near Threatened)'
    output[output$status=='LC','statusLonger']<-'11) LC Least Concern'
    output[output$status=='DD','statusLonger']<-'12) DD Data deficient'
    output[output$status=='NE','statusLonger']<-'13) NE Not evaluated'
    output[output$status=='NR','statusLonger']<-'14) NR Not recognized'
  
  }
  

  output
  
}

LoadRedList <-function(fileName){
  
  # Try reading from an RDS file if a filename is given
  if (fileName!= ""){
    
    output<-readRDS(file = fileName)
    
  }
  # else load from csv
  else {
  
    redList <-read.csv(file="European_Red_List_2017_December_csv/European_Red_List_2017_December.csv", header=TRUE, sep=",")
    colnames(redList)[1]<-"speciesGroup"
    output <- redList[as.character(redList$speciesGroup) %in% c("Freshwater_Fishes", "Marine_Fishes") ,]
    output$name <- paste(output$taxonomicRankGenus,output$taxonomicRankSpecies)
    
    output$status <- as.character(output$europeanRegionalRedListCategory)
    
    output$status<-toupper(output$status)
    output$statusLonger<-output$status
    output[output$status=='EX' & !is.na(output$status),'statusLonger']<-'01) EX Extinct'
    output[output$status=='EW'& !is.na(output$status),'statusLonger']<-'02) EW Extinct in the wild'
    output[output$status=='RE' & !is.na(output$status),'statusLonger']<-'03) RE Regionally Extinct'
    output[output$status=='CR' & !is.na(output$status),'statusLonger']<-'04) CR Critically Endangered'
    output[output$status=='EN' & !is.na(output$status),'statusLonger']<-'05) EN Endangered'
    output[output$status=='VU' & !is.na(output$status),'statusLonger']<-'06) VU Vulnerable'
    output[output$status=='NT'& !is.na(output$status),'statusLonger']<-'07) NT Near Threatened'
    output[output$status=='LC'& !is.na(output$status),'statusLonger']<-'08) LC Least Concern'
    output[output$status=='DD'& !is.na(output$status),'statusLonger']<-'09) DD Data deficient'

    
    
  }
  
  
  output
  
}

LoadRedListFromAPI <- function(fileName, speciesList){
  
  
  # Try reading from an RDS file if a filename is given
  if (fileName!= ""){
    
    output<-readRDS(file = fileName)
    
  }
  # else query the Red List API
  else 
  {
  
    result <- lapply(speciesList, getRedListAPIDataForSpecies)
  
    output <- rbindlist(result)
  }
  
  output
  
}

getRedListAPIDataForSpecies <- function(sciName){

  #print(sciName)
  output <- NA
  
  # Load the api key
  apiKey <- readRDS(file = "apiKey.rds")
  
  # Get the entry details
  entry <- rl_search(key = apiKey, name = sciName, region='europe')
  
  # If there is an entry for this species
  if (length(entry)>0) {
    
    #returnValue <- entry
    
    # Get the narrative details
    narrative <- rl_narrative(key = apiKey, name = sciName, region='europe')
    
    # rename the entries to avoid confusion later
    names(narrative)<-paste("narrative_",names(narrative),sep="")
    
    # get the citation
    citation <- rl_sp_citation(key = apiKey, name = sciName, region='europe')
    
    # rename the entries to avoid confusion later
    names(citation)<-paste("citation_",names(citation),sep="")
    
    # Get the web site link (no rredlist finction for this...)
    RedListAPIURL <- 'https://apiv3.iucnredlist.org/api/v3/'
    APIfunction <- 'weblink/'
    speciesNameEncoded <- URLencode(sciName, reserved=TRUE)  
    weblink <- fromJSON(getURL(paste(RedListAPIURL,APIfunction,speciesNameEncoded, sep = "")))
    
    # rename the entries to avoid confusion later
    names(weblink)<-paste("weblink_",names(weblink),sep="")
    

    if (length(entry$result$category) == 0){
      category <- ""
    } else {
      category <- entry$result$category
    }
    
    if (length(entry$result$assessor) == 0){
      assessor <- ""
    } else {
      assessor <- entry$result$assessor
    }
    
    if (length(narrative$narrative_result$rationale) == 0){
      rationale <- ""
    } else {
      rationale <- narrative$narrative_result$rationale
    }
    
    if (length(citation$citation_result$citation) == 0){
      myRef <- ""
    } else {
      myRef <- citation$citation_result$citation
    }
    
    if (length(weblink$weblink_rlurl) == 0 || weblink$weblink_rlurl =="0" ){
      myURL <- ""
    } else {
      myURL <- weblink$weblink_rlurl
    }
    
    output <- data.frame(name = sciName, status = category, assessor = assessor, citation=myRef, rationale = rationale, URL = myURL, stringsAsFactors = FALSE)
    
    output$statusLonger<-output$status
    output[output$status=='EX' & !is.na(output$status),'statusLonger']<-'01) EX Extinct'
    output[output$status=='EW'& !is.na(output$status),'statusLonger']<-'02) EW Extinct in the wild'
    output[output$status=='RE' & !is.na(output$status),'statusLonger']<-'03) RE Regionally Extinct'
    output[output$status=='CR' & !is.na(output$status),'statusLonger']<-'04) CR Critically Endangered'
    output[output$status=='EN' & !is.na(output$status),'statusLonger']<-'05) EN Endangered'
    output[output$status=='VU' & !is.na(output$status),'statusLonger']<-'06) VU Vulnerable'
    output[output$status=='NT'& !is.na(output$status),'statusLonger']<-'07) NT Near Threatened'
    output[output$status=='LC'& !is.na(output$status),'statusLonger']<-'08) LC Least Concern'
    output[output$status=='DD'& !is.na(output$status),'statusLonger']<-'09) DD Data deficient'
    


  } 
  
  output
  
  
}


LoadSummaryData <-function(fileName){
  
  # Try reading from an RDS file if a filename is given
  if (fileName!= ""){
 
    output<-readRDS(file = fileName)
    
  }
  # else run a sparql query
  else 
  {
  
  output <- runSparql('SELECT ?sample ?year ?species (str(?sciname) as ?SciName) (str(?commonName) as ?CommonName) ?ICspecies ?gear ?div ?stock  
(sum(xsd:double(?demlengths)) as ?DemLengths)  (sum(xsd:double(?demages)) as ?DemAges)  (sum(xsd:double(?dembios)) as ?DemBios)  
                      (sum(xsd:double(?pellengths)) as ?PelLengths)  (sum(xsd:double(?pelages)) as ?PelAges)  (sum(xsd:double(?pelbios)) as ?PelBios)  
                      (sum(xsd:double(?portlengths)) as ?PortLengths)  (sum(xsd:double(?portages)) as ?PortAges)  (sum(xsd:double(?portbios)) as ?PortBios) 
                      WHERE  {    ?sample a mi:SamplingSummary .
                      ?sample mi:isOfYear ?year .
                      ?sample mi:isOfQuarter ?quarter .
                      ?sample mi:isOfSpecies ?species .
                      ?species skos:prefLabel ?sciname . 
                      OPTIONAL {
                      ?species <http://purl.org/dc/terms/description> ?commonName . 
                      }
                      OPTIONAL {
                      ?species owl:sameAs ?ASFISspecies .
                      ices:SpecASFIS skos:member ?ASFISspecies .
                      ?ASFISspecies owl:sameAs ?ICspecies .
                      ices:IC_Species skos:member ?ICspecies .
                      ?stock skos:related ?div . 
                      ?stock skos:broader ?ICspecies . 
                      }
                      ?sample mi:isOfGear ?gear .
                      ?sample mi:isOfArea ?div .
                      ?sample mi:DemSeaSchemeLengthObs ?demlengths .
                      ?sample mi:DemSeaSchemeAgeObs ?demages .
                      ?sample mi:DemSeaSchemeBioObs ?dembios .
                      ?sample mi:PelSeaSchemeLengthObs ?pellengths .
                      ?sample mi:PelSeaSchemeAgeObs ?pelages .
                      ?sample mi:PelSeaSchemeBioObs ?pelbios .
                      ?sample mi:PortSchemeLengthObs ?portlengths .
                      ?sample mi:PortSchemeAgeObs ?portages .
                      ?sample mi:PortSchemeBioObs ?portbios .
                      
                      }
                      GROUP BY ?sample ?year ?species ?sciname ?commonName ?ICspecies ?gear ?div ?stock  
                      ')
  
  }
  
  output
}

# get the DBpedia abstract text
formatDBPediaAbstract <- function(selectedSpecies, speciesInfoFrame, short=FALSE){
  
  output <- ""
  selectedSpeciesFrame <- speciesInfoFrame[speciesInfoFrame$CommonName==selectedSpecies,]
  
  if (length(selectedSpeciesFrame)>0 && nrow(selectedSpeciesFrame)>0){
    if ( !is.na(selectedSpeciesFrame$Abstract) ) {
      
      output <- selectedSpeciesFrame$Abstract 
      
      # truncate if required
      if (short){
        abtstractLimit <- 500
        if (nchar(output)> abtstractLimit){
          output <- substr(output,1,abtstractLimit)
          output <- paste(output,"...(continues)", sep="")
        }
      }
      
      # append the reference link
      if ( !is.na(selectedSpeciesFrame$origPage) ) {
        myLink <- selectedSpeciesFrame$origPage
        myLink <- substring(myLink,2)
        myLink <- substring(myLink,1,nchar(myLink)-1)
        myLink<- paste('<a href="',myLink,'" target="_blank">',myLink,'</a>',sep='')
        output <- paste(output,"<br/><b>Source:</b> ",myLink)
        
      }
      
      
    }
  }
  
  # Default to blank text if we don't get a value
  if (length(output)==0){
    output <- ""
  }
  
  output
}

# get the DBpedia image
formatDBPediaImage <- function(selectedSpecies, speciesInfoFrame){
  
  output <- ""
  selectedSpeciesFrame <- speciesInfoFrame[speciesInfoFrame$CommonName==selectedSpecies,]
  
  if (length(selectedSpeciesFrame)>0 && nrow(selectedSpeciesFrame)>0){
    if ( !is.na(selectedSpeciesFrame$image) ) {
      myImage <- selectedSpeciesFrame$image
      # need to remove the first and last characters
      myImage <- substring(myImage,2)
      myImage <- substring(myImage,1,nchar(myImage)-1)
      output <- myImage
    }
  }
  
  # Default to blank text if we don't get a value
  if (length(output)==0){
    output <- ""
  }
  
  output
}

# get the Red List status 
formatRedListStatus <- function(selectedSpecies,summaryData,shortRedList){
  
  output <- ""
  selectedSpeciesName <- unique(summaryData[summaryData$CommonName==selectedSpecies & !is.na(summaryData$CommonName),"SciName"])
  if (length(selectedSpeciesName)>0){
    output <- shortRedList[shortRedList$name==selectedSpeciesName,"statusLonger"]
    
    if (length(output) >0){
      output <- paste("<b>IUCN European Red List Status:</b>",output)
    }
    
  }
  
  # Default to blank text if we don't get a value
  if (length(output)==0){
    output <- ""
  }
  
  output
}

# get the Red List rationale
formatRedListRationale <- function(selectedSpecies,summaryData,shortRedList, short=FALSE){
  
  output <- ""
  selectedSpeciesName <- unique(summaryData[summaryData$CommonName==selectedSpecies & !is.na(summaryData$CommonName),"SciName"])
  if (length(selectedSpeciesName)>0){
    output <- shortRedList[shortRedList$name==selectedSpeciesName,"redListCategoryRationale"]
    # Remove some rubbish from the text
    output<- gsub("Â","",output)
    
    # if we want to truncate the text
    if (short){
      charLimit <- 500
    
      if (length(output) >0 && nchar(output)> charLimit){
        
        output <- substr(output,1,charLimit)
        output <- paste(output,"...(continues)", sep="")
        
      }
    }
    
    if (length(output) >0){
      output <- paste("<b>IUCN European Red List rationale:</b>",output)
    }
    
  }
  
  # Default to blank text if we don't get a value
  if (length(output)==0){
    output <- ""
  }
  
  output
}





