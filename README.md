# SemanticSummary
Shiny app which displays a semantically-enabled summary of commercial fisheries data.  The RDF format data was generated using the code in the davidcurrie2001/SemanticFishData repo, and can either be provided: 
1. Dynamically via a SPARQL end-point (e.g. by building the Docker image described by the Dockerfile at davidcurrie2001/SemanticFishData), or 
2. Statically via pre-generated RDS R data files (e.g. using the GenerateSummaryData.R script in this repo).
