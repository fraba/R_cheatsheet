addDailyRollMeans <- function(df, 
                              date_var, 
                              value_var, 
                              win_vec = c(7,30,60,90),
                              by_seq = 'day',
                              date_format = "%Y-%m-%d", 
                              do_melt = TRUE) {
  
  # Check 
  if (!date_var %in% names(df)) {
    stop(paste0("`", date_var, "` not found"))
  }
  if (!value_var %in% names(df)) {
    stop(paste0("`", value_var, "` not found"))
  }
  
  require(zoo)
  
  df[[date_var]] <- as.Date(df[[date_var]], format = date_format, origin = "1970-01-01")
  
  all_dates <- 
    data.frame(date = seq(from = min(df[[date_var]], na.rm=T), 
                          to = max(df[[date_var]]), na.rm=T,
                          by = by_seq), 
               stringsAsFactors = FALSE)
  
  df[[date_var]] <- as.character(df[[date_var]])
  all_dates[['date']] <- as.character(all_dates[['date']])
  
  df <- merge(df, all_dates, by.x = date_var, by.y = 'date', all.y = TRUE)
  
  df[[value_var]][is.na(df[[value_var]])] <- 0
  
  df <- df[order(df[[date_var]]),]
  
  for (win in win_vec) {
    
    df[[paste0('rollmean', win)]] <- 
      rollmean(df[[value_var]], win, fill = NA)
    
  }
  
  if (do_melt == FALSE) {
    return(df)
  } else {
    require(reshape2)
    df <- melt(df, id.vars = 'Var1')
  }
  
  
}