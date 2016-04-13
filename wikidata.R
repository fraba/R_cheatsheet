# Integrate with Wikidata data

## Add data to dataframe based on Wikidata ID

QueryWikidataWithSPARQL <- function(instance_of, attributes, lang) {
  require(SPARQL)
  endpoint <- 'https://query.wikidata.org/sparql'
  query <- 
    paste0(
  "
  SELECT *  WHERE {
  ?resource wdt:P31 wd:", instance_of, ".
  ?resource rdfs:label ?label.
  FILTER(LANG(?label) = '' || LANGMATCHES(LANG(?label), '", lang, "'))")
  
  for (attribute in attributes) {
    query <- paste0(query, 
                    "OPTIONAL{?resource wdt:", attribute, " ?", attribute, "}")
  }
  query <- paste0(query, "}")
  response <- 
    SPARQL(endpoint, query, curl_args = list(.encoding = 'UTF-8'))$results
  return(response)
} 

AugmentWithWikiData <- function(df, wid_vector, instance_of, attributes, lang) {
  
  query_response_df <- 
    QueryWikidataWithSPARQL(instance_of, 
                            attributes,
                            lang)
  
  query_response_df$wid <- gsub("^(.*)/|>$", "", query_response_df$resource)
  
  require(stringr)
  df$tmp_wid <- str_extract(df[[wid_vector]], "Q\\d+")
  
  df <- merge(df, query_response_df[,c("wid", attributes)], by.x = 'tmp_wid', by.y = 'wid', 
              all.x = TRUE, 
              all.y = FALSE)
  
  # Rename attributes
  require(jsonlite)
  for (i in 1:length(attributes)) {
    json_data <- fromJSON(paste0('http://www.wikidata.org/wiki/Special:EntityData/', attributes[i], ".json"))
    names(attributes)[i] <-
      json_data$entities[[attributes[i]]]$labels$en$value
  }
  
  require(plyr)
  names(df)[names(df) %in% attributes] <- names(attributes)
  
  return(df)
}


