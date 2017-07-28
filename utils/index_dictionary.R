library(tm)
library(NLP)

# load French 2015 dictionary   
print("loading corpus...")
french_dictionary <- read.csv2("../data/CLEFeHealth2017Task1_training_FR/dictionaries/Dictionnaire2015.csv",
                               colClasses = c(rep("character", 7),
                                              rep("numeric", 2), 
                                              #rep(c("date", "character"), 2),
                                              rep("character", 8)),
                               stringsAsFactors = FALSE)

# build corpus
corpus <- VCorpus(VectorSource(french_dictionary$DiagnosisText))
print("...done")

# build term-document matrix with binary weighting
print("building binary term-document matrix...")
tdm <- TermDocumentMatrix(corpus,
                          control = list(removeNumbers = FALSE,
                                         removePunctuation = FALSE,
                                         stopwords = FALSE,
                                         stemming = FALSE, 
                                         weighting = weightBin,
                                         wordLengths = c(1, Inf)))

# save file
save(tdm, file = "./index/index_FR_binary.RData")
print("...done")

# save translitterated version
print("building translitterated binary term-document matrix...")
translit_dictionary <- unlist(lapply(X = french_dictionary$DiagnosisText, 
                                     FUN = iconv, 
                                     to = "ASCII//TRANSLIT"))

# clean characters
translit_dictionary <- gsub(pattern = "[`|'|\"|^]", 
                            replacement = "", 
                            x = translit_dictionary)

# build corpus
corpus <- VCorpus(VectorSource(translit_dictionary))

# build translitterated matrix
tdm_translit <- TermDocumentMatrix(corpus,
                                   control = list(removeNumbers = FALSE,
                                                  removePunctuation = FALSE,
                                                  stopwords = FALSE,
                                                  stemming = FALSE, 
                                                  weighting = weightBin,
                                                  wordLengths = c(1, Inf)))

# save index translitterated
save(tdm_translit, file = "./index/index_FR_binary_translit.RData")
print("...done")
