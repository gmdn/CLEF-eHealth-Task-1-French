# this file transliterate accents
library(SnowballC)
library(tm)

source("./utils/expand_acronym.R")

load("./data/causes_brutes_2014_FR.RData")

####################
load("./index/index_FR_binary_translit.RData")
####################

# load French 2015 dictionary   
french_dictionary <- read.csv2("../data/CLEFeHealth2017Task1_training_FR/dictionaries/Dictionnaire2015.csv",
                               colClasses = c(rep("character", 7),
                                              rep("numeric", 2), 
                                              #rep(c("date", "character"), 2),
                                              rep("character", 8)),
                               stringsAsFactors = FALSE)

# load run file
filename <- "./runs/run_binary_translit.csv"

run <- read.csv2(file = filename,
                 colClasses = c(rep("character", 1),
                                rep("numeric", 2), 
                                "character",
                                rep("numeric", 2), 
                                #rep(c("date", "character"), 2),
                                rep("character", 4)),
                 stringsAsFactors = FALSE, 
                 #nrows = 9450,
                 header = FALSE,
                 quote = "")
# rename columns
names(run) <- c("DocID", "YearCoded", "LineID", "RawText", "IntType", "IntValue", "CauseRank", "StandardText", "ICD10", "CleanedText")
#str(run)

print(paste0("start classification of ", nrow(run), " elements"))

for(line in 1:nrow(run)) {
  #line <- 39
  if(line %% 10000 == 0)
    print(line)
  
  # get line raw text
  line_rawtext <- run[line, "CleanedText"]
  
  if(line_rawtext != "") {
    
    # remove punctuation
    #line_tokens <- removePunctuation(line_rawtext, preserve_intra_word_dashes = TRUE)
    
    # tokenize text
    line_tokens <- scan_tokenizer(line_rawtext)
    
    # remove stopwords
    #line_tokens <- setdiff(line_tokens, stopwords("english"))
    
    # expand acronym
    line_tokens <- expand_acronym(line_tokens)
    
    # eztract single tokens
    tokens <- unique(scan_tokenizer(line_tokens))
    #tokens <- scan_tokenizer(line_tokens)
    
    # get number of tokens
    #num_of_tokens <- length(tokens)
    
    ###################################
    ###################################
    # transliterate words 
    tokens_translit <- unlist(lapply(X = tokens, 
                                     FUN = iconv, 
                                     to = "ASCII//TRANSLIT"))
    
    tokens_translit <- gsub(pattern = "[`|'|\"|^]", 
                            replacement = "", 
                            x = tokens_translit)
    
    tokens_translit <- unique(tokens_translit)
    ###################################
    ###################################
    
    ###################################
    # compute dictionary scores
    # scores <- tm_term_score(tdm, tokens)
    scores <- tm_term_score(tdm_translit, tokens_translit)
    ###################################
    
    
    # find scores equal to the number of tokens
    results <- which(scores == max(scores))
    
    # compute frequency of ICd codes with maximum score
    tb <- table(french_dictionary[which(scores == max(scores)), "Icd1"])
    
    # order decreasingly
    tb <- tb[order(tb, decreasing = TRUE)]
    
    if (length(tb) > 0) {
      
      if (length(tb) == 1) {
        icd <- names(tb)[1] 
      } else {
        icd <- dimnames(tb)[[1]][1] #dimnames(tb)[[1]][which(tb == max(tb))][1]  
      }
      # get first code
      
      run$ICD10[line] <- icd
      run$StandardText[line] <- paste(tokens, collapse = " ")
      
    }
    
  }
  
}

save(run, file = "./runs/run.RData")
#load("run.RData")
# run
write.csv2(x = run[, c(1:3, 7:9)],
           file = paste0(filename, "_unofficial.csv"),
           quote = FALSE,
           #sep = ";",
           #eol = "\r\n",
           eol = "\n",
           na = "",
           row.names = FALSE)
