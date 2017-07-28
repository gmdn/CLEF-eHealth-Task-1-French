acronym_table <- read.delim(file = "./utils/acronym_table.txt", 
                            header = FALSE, 
                            sep = "\t", 
                            stringsAsFactors = FALSE)

names(acronym_table) <- c("acronym", "expansion")
#str(acronym_table)


expand_acronym <- function(line_tokens) {

  for (i in 1:length(line_tokens)) {
    
    if (!is.na(line_tokens[i])) {
      
      idx_acronym <- which(acronym_table$acronym == line_tokens[i])
      
      if(length(idx_acronym) > 0) {
        line_tokens[i] <- acronym_table$expansion[idx_acronym]  
      }
      
    }
    
  }
  
  return(line_tokens)
  
}





