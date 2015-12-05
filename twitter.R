# Twitter analysis

## Regex

regex_hashtag <- "(?<=^|\\s)#\\w\\w+"
regex_user <- "^\\.?@[a-z0-9_]{1,25}"


## Parse

parseRegex <- function (messages, regexpr) {
  library(stringr)
  message <- tolower(message)
  hashtags <- str_extract_all(message, regexpr)
  return (hashtags)
}

## Create networks

buildTwitterNetworkAT <- function (user, message) {
  
  # Based on cornelius 2010
  # Downloaded from http://blog.ynada.com/339
  
  require(igraph)
  
  # Get @-messages, senders, receivers
  ats <- grep("^\\.?@[a-z0-9_]{1,25}", tolower(message), perl=T, value=T);
  at.sender <- tolower(as.character(user[grep("^\\.?@[a-z0-9_]{1,25}", tolower(message), perl=F)]));
  at.receiver <- gsub("^\\.?@([a-z0-9_]{1,25})[^a-z0-9_]?.*$", "\\1", ats, perl=F);
  # print(paste(length(ats), " @-messages from ", length(unique(at.sender)), " senders and ", length(unique(at.receiver)), " receivers.", sep=""));
  
  # This is necessary to avoid problems with empty entries, usually caused by encoding issues in the source files
  at.sender[at.sender==""] <- "<NA>";
  at.receiver[at.receiver==""] <- "<NA>";
  
  # Create a data frame from the sender-receiver information
  ats.df <- data.frame(at.sender, at.receiver);
  
  # Transform data frame into a graph
  ats.g <- graph.data.frame(ats.df, directed=T);
  
  return(ats.g)
}

buildTwitterNetworkRT <- function (user, message) { 
  
  # Based on cornelius 2010
  # Downloaded http://blog.ynada.com/339
  
  require(igraph)
  
  # Get RTs, senders, receivers
  rts <- grep("^rt @[a-z0-9_]{1,25}", tolower(message), perl=T, value=T);
  rt.sender <- tolower(as.character(user[grep("^rt @[a-z0-9_]{1,25}", tolower(message), perl=T)]));
  rt.receiver <- gsub("^rt @([a-z0-9_]{1,25})[^a-z0-9_]?.*$", "\\1", rts, perl=F);
  # print(paste(length(rts), " RTs from ", length(unique(rt.sender)), " senders and ", length(unique(rt.receiver)), " receivers.", sep=""));
  
  # This is necessary to avoid problems with empty entries, usually caused by encoding issues in the source files
  rt.sender[rt.sender==""] <- "<NA>";
  rt.receiver[rt.receiver==""] <- "<NA>";
  
  # Create a data frame from the sender-receiver information
  rts.df <- data.frame(rt.sender, rt.receiver);
  
  # Transform data frame into a graph
  rts.g <- graph.data.frame(rts.df, directed=T);
  
  return(rts.g)
}

