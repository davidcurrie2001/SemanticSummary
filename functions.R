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

# Read the summary data from an R data file to speed things up
#LoadSummaryDataFromFile<-function(fileName){
#  
#  output<-readRDS(file = fileName)
#  
#}



