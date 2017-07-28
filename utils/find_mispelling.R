library(tm)

source("expand_acronym.R")

load("index_FR_translit.RData")
vocabulary_terms <- tdm_translit$dimnames$Terms

load("./causes_brutes_2014_FR.RData")

lines <- numeric()
tokens <- character()
matched_terms <- list()

jth_element <- 1

# 8960
for (line in 8960:nrow(causes_brutes_2014_FR)) {
  
  #line <- 8978
  
  raw_text <- causes_brutes_2014_FR[line, "RawText"]
  #print(raw_text)
  
  # skip NA lines
  if(raw_text == "N/A" | raw_text == "NA" | raw_text == "-" | is.na(raw_text)) {
    next
  }
  
  # to lowercase
  raw_text <- tolower(raw_text)
  
  # transofm &amp;
  raw_text <- gsub(pattern = "&amp;", replacement = " and ", x = raw_text)
  
  ##### ADDED FOR FRENCH
  raw_text <- gsub(pattern = "d\"|d\'", replacement = " ", x = raw_text)
  
  raw_text <- gsub("^\\s+|\\s+$", "", raw_text)
  
  if(nchar(raw_text) == 0)
    next
  
  ########## SPLIT
  #for(rank in 1:length(raw_text)) {}
  #rank <- 1
  
  line_tokens <- scan_tokenizer(raw_text)
  
  # remove punctuation (check French acronym table)
  line_tokens <- removePunctuation(line_tokens, preserve_intra_word_dashes = TRUE)
  
  # expand acronym
  line_tokens <- expand_acronym(line_tokens)
  
  # translit line
  line_tokens <- iconv(line_tokens, to = "ASCII//TRANSLIT")
  
  # remove transliteration chars
  line_tokens <- gsub(pattern = "[`|'|\"|^]", replacement = "", x = line_tokens)
  
  for (token in line_tokens) {
    
    if(nchar(token) < 3) 
      next
    
    #print(token)
    
    #library(stringdist)
    
    #adist(x = "artial", y = vocabulary_terms)
    
    distances <- adist(token, vocabulary_terms, fixed = T)
    
    if (min(distances) == 1) {
      matched_term <- vocabulary_terms[which(distances == min(distances))]
      print(paste(line, token, paste(matched_term, collapse = " "), sep = ";"))
      
      lines <- c(lines, line)
      tokens <- c(tokens, token)
      matched_terms[[jth_element]] <- paste(matched_term, collapse = " ")
      jth_element <- jth_element + 1
      #print(vocabulary_terms[which(distances == min(distances))])
      #vocabulary_terms[which(distances == min(distances) + 1)]
    }
    
    
    #drop(attr(adist("kitten", "sitting", counts = TRUE), "counts"))
  }
  
}