# Integrate with Wikidata data

## Get Wikidata IDs from name of entity

getWikidataIdsFromString <- function(string, wikipedia_project = "it") {
  
  cat(paste0("Searching: [", string, "]..."))
  
  wikidata_entities <- data.frame()
  
  getWikidataEntity <- function(id, wikipedia_project) {
    require(WikidataQueryServiceR)
    base_url <- 
      'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> 
      PREFIX wd: <http://www.wikidata.org/entity/> 
      SELECT ?entityLabel ?instanceOf ?instanceOfLabel
      WHERE 
        {
        wd:%s rdfs:label ?entityLabel .
        wd:%s wdt:P31 ?instanceOf .
        ?instanceOf rdfs:label ?instanceOfLabel .
        FILTER (langMatches( lang(?entityLabel), "%s" ) )
        FILTER (langMatches( lang(?instanceOfLabel), "%s" ) )
    }'
    res <- query_wikidata(sprintf(base_url, id, id, 
                                  toupper(wikipedia_project), 
                                  toupper(wikipedia_project)))
    return(res)
  }
  
  search_string_1 <-
    "https://%s.wikipedia.org/w/api.php?action=query&list=search&srsearch=%s&format=json&srqiprofile=classic"
  
  library(jsonlite)
  res1 <- 
    try({fromJSON(sprintf(search_string_1, wikipedia_project, URLencode(string)))})
  if (class(res1) == 'try-error') 
    return(wikidata_entities)
  
  titles <- 
    gsub("https:\\/\\/([a-z]{2})\\.wikipedia\\.org\\/wiki\\/", "", res1$query$search[[2]])
  
  search_string_2 <- 
    "https://%s.wikipedia.org/w/api.php?action=query&prop=pageprops&titles=%s&format=json"
  
  for (i in 1:1) {
    this_res <- 
      try({fromJSON(sprintf(search_string_2, wikipedia_project, URLencode(titles[i])))})
    if (class(this_res) == 'try-error') next
    this_wikidata_id <- 
      this_res$query$pages[[1]]$pageprops$wikibase_item
    this_query_res <- getWikidataEntity(this_wikidata_id, wikipedia_project)
    if (length(this_query_res)>0) {
      if (nrow(this_query_res)>0) {
        this_query_res$id <- this_wikidata_id
        this_query_res$search_string <- string
        wikidata_entities <- rbind(wikidata_entities, this_query_res)
      }
    }
  }
  return(wikidata_entities)
}

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


