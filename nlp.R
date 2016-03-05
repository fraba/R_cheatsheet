
# Creat a junction table based on string distance (will match closest)
# As arguments, it expects two dataframes with two columns each text to match (`text_`) 
# and unique id (`text_id`)
getJunctTable <- function(df1, df2) {
  
  rematched <- data.frame()
  
  require(stringdist)
  
  require(plyr)
  df1$desc_regione_minint <- NULL
  df2$desc_regione_istat <- NULL
  
  df1$desc_comune_minint <- tolower(as.character(df1$desc_comune_minint))
  df2$desc_comune_istat <- tolower(as.character(df2$desc_comune_istat))
  
  print("Simple merge")
  junct_table <- merge(df1, df2, 
                       by.x = c("cod_regione_istat", "desc_comune_minint"),
                       by.y = c("cod_regione_istat", "desc_comune_istat"),
                       all = TRUE)
  
  missing_matches <- sum(is.na(junct_table$cod_comune_istat))
  
  while (missing_matches > 0) {
    
    print("Attempting match with stringdist")
    
    no_match_minint <- subset(df1, cod_comune_minint %in% 
                                junct_table[is.na(junct_table$cod_comune_istat),
                                            'cod_comune_minint'])
    no_match_minint$cod_comune_istat <- NA
    no_match_minint$desc_comune_istat <- NA
    
    no_match_istat <- subset(df2, cod_comune_istat %in% 
                               junct_table[is.na(junct_table$cod_comune_minint),
                                           'cod_comune_istat'])
    
    for (i in 1:nrow(no_match_minint)) {
      cod_reg <- no_match_minint$cod_regione_istat[i]
      istat_reg <- subset(no_match_istat, cod_regione_istat == cod_reg)
      istat_reg$dist <-
        stringdist(no_match_minint$desc_comune_minint[i], istat_reg$desc_comune_istat)
      i_min_dist <- which.min(istat_reg$dist)
      no_match_minint$cod_comune_istat[i] <- istat_reg$cod_comune_istat[i_min_dist]
      no_match_minint$desc_comune_istat[i] <- istat_reg$desc_comune_istat[i_min_dist]
    }
    
    # View(no_match_minint[,c("desc_comune_minint","desc_comune_istat")])
    
    # Check for one-to-many relations
    no_match_minint_one_to_many <- 
      subset(no_match_minint, cod_comune_istat %in%
               with(no_match_minint, cod_comune_istat[duplicated(cod_comune_istat)]))
    istat_ids <- unique(no_match_minint_one_to_many$cod_comune_istat)
    
    for (istat_id in istat_ids){
      
      conflicting_minint_ids <- 
        subset(no_match_minint_one_to_many, cod_comune_istat == istat_id)$cod_comune_minint
      
      closest_minint_id <- 
        with(no_match_minint_one_to_many,
             conflicting_minint_ids[
               which.min(
                 stringdist(
                   desc_comune_minint[cod_comune_minint %in% conflicting_minint_ids], 
                   desc_comune_istat[cod_comune_istat == istat_id]))])
      
      no_match_minint_one_to_many$cod_comune_istat[no_match_minint_one_to_many$cod_comune_minint %in% conflicting_minint_ids] <- 'NA'
      no_match_minint_one_to_many$desc_comune_istat[no_match_minint_one_to_many$cod_comune_minint %in% conflicting_minint_ids] <- 'NA'
      
      no_match_minint_one_to_many$cod_comune_istat[no_match_minint_one_to_many$cod_comune_minint == closest_minint_id] <- istat_id
      no_match_minint_one_to_many$desc_comune_istat[no_match_minint_one_to_many$cod_comune_minint == closest_minint_id] <- 
        with(df2, desc_comune_istat[cod_comune_istat == istat_id])
      
    }
    
    rematched <- 
      rbind(rematched, 
            rbind(no_match_minint[!(no_match_minint$cod_comune_minint %in% no_match_minint_one_to_many$cod_comune_minint),], 
                  no_match_minint_one_to_many[no_match_minint_one_to_many$cod_comune_istat != 'NA',]))
    
    junct_table <- 
      rbind(
        subset(junct_table, !(cod_comune_minint %in% rematched$cod_comune_minint) & 
                 !(cod_comune_istat %in% rematched$cod_comune_istat)),
        rematched[,-which(names(rematched) == "desc_comune_istat")])
    
    missing_matches <- sum(is.na(junct_table$cod_comune_istat))
    
  }
  
  print(rematched[,c("desc_comune_minint","desc_comune_istat")])
  
  junct_table <- merge(junct_table, df2[,c("cod_comune_istat","desc_comune_istat")])
  junct_table$stringdist <- mapply(stringdist, junct_table$desc_comune_minint, junct_table$desc_comune_istat)
  
  return(junct_table)
  
}