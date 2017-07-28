rm(list = ls())

#source("./utils/load_test_data_EN.R")
source("./utils/expand_acronym.R")
#source("./utils/normalize_line.R")

load("./data/causes_brutes_2014_FR.RData")

# load matched terms
matched_terms <- read.csv2(file = "./utils/matched_terms_8.csv", 
                           header = T, sep = ";", 
                           quote = "\"",
                           stringsAsFactors = FALSE)



# set the type of indexing for this run, choose either "binary" or "binary_translit"
weight <- "binary_translit"
#weight <- "binary"

# set run identifier
if (weight == "binary") {
  run_id <- "run_binary.csv"  
} else if(weight == "binary_translit") {
  run_id <- "run_binary_translit.csv"  
} else {
  stop("check type of weight on line 6 and 7")
}

# run will be saved in the runs folder
run_name <- paste0("./runs/", run_id)

# stop execution if a run with the same id exists
if(file.exists(run_name)) {
  stop(paste("run with id", run_id , "already exists"))
}

# for each line of the causes brutes file
for (line in 1:nrow(causes_brutes_2014_FR)) {
  
  # print output to check
  if(line %% 1000 == 0) {
    print(paste0("reading line ", line, " of ", nrow(causes_brutes_2014_FR)))
  }
  
  #line <- 1
  raw_text <- causes_brutes_2014_FR[line, "RawText"]
  #raw_text
  # skip NA lines
  if(raw_text == "N/A" | raw_text == "NA" | raw_text == "-" | is.na(raw_text)) {
    next
  }
  
  ################### CHANGED FROM UNOFFICIAL RUNS
  # to lower case
  raw_text <- tolower(raw_text)
  ################### CHANGED FROM UNOFFICIAL RUNS
  
  # transofm &amp;
  raw_text <- gsub(pattern = "&amp;", replacement = " and ", x = raw_text)
  raw_text <- gsub(pattern = "d\"", replacement = "dé", x = raw_text)
  
  ##### ADDED FOR FRENCH
  
  raw_text <- gsub(pattern = "d\"|d\'", replacement = " ", x = raw_text)
  
  # split sentence
  if (grepl("[/;,]", raw_text)) {
    raw_text <- strsplit(raw_text, split = "[/;,]")
  } else if (grepl(" avec ", raw_text)) {
    raw_text <- strsplit(raw_text, split = "avec")
  } else if (grepl(" sur ", raw_text)) {
    raw_text <- strsplit(raw_text, split = "sur")
  } else if (grepl(" par ", raw_text)) {
    raw_text <- strsplit(raw_text, split = "par")
  } else if (grepl("suite à un|suite à une", raw_text)) {
    raw_text <- strsplit(raw_text, split = "suite à un|suite à une")
  } else if (grepl("dans un contexte de", raw_text)) {
    raw_text <- strsplit(raw_text, split = "dans un contexte de")
  }  else if (grepl(" après ", raw_text)) {
    raw_text <- strsplit(raw_text, split = "après")
  }
  
  # unlist text (if necessary)
  raw_text <- unlist(raw_text)
  
  # remove leading and trailing white spaces
  raw_text <- gsub("^\\s+|\\s+$", "", raw_text)
  
  # for each subpart 
  for(rank in 1:length(raw_text)) {
    # build run line
    run <- data.frame(DocID = causes_brutes_2014_FR[line, "DocID"],
                      YearCoded = causes_brutes_2014_FR[line, "YearCoded"],
                      LineID = causes_brutes_2014_FR[line, "LineID"],
                      RawText = causes_brutes_2014_FR[line, "RawText"],
                      IntType = causes_brutes_2014_FR[line, "IntType"],
                      IntValue = causes_brutes_2014_FR[line, "IntValue"],
                      CauseRank = raw_text[rank],
                      StandardText = NA,
                      IDC10 = NA,
                      CleanedText = NA) #added this line to check
    
    
    #scan line
    line_tokens <- scan_tokenizer(raw_text[rank])
    
    # skip empty tokens
    if (length(line_tokens) == 0) 
      next
    
    ################
    # remove punctuation (check French acronym table)
    line_tokens <- removePunctuation(line_tokens, preserve_intra_word_dashes = TRUE)
    
    # expand acronym
    line_tokens <- expand_acronym(line_tokens)
    
    ########### AT THIS POINT CORRECT SPELLING
    find_matches <- match(line_tokens, matched_terms$token)
    
    # check if there is any valid substitute
    substitute <- which(!is.na(find_matches))
    
    # if so, change token with matched terms
    if(length(substitute) > 0) {
      line_tokens[substitute] <- matched_terms$matched[substitute]
    }
    
    ###########
    
    # rebuild line
    line_collapsed <- paste(line_tokens, collapse = " ")
    
    ################
    # remove extra punctuation (if any)
    line_collapsed <- removePunctuation(line_collapsed, preserve_intra_word_dashes = TRUE)
    
    # correct (remove white space)
    line_collapsed <- gsub(pattern = "cardio ", replacement = "cardio-", x = line_collapsed)
    line_collapsed <- gsub(pattern = "insulino ", replacement = "insulino-", x = line_collapsed)
    line_collapsed <- gsub(pattern = "broncho ", replacement = "broncho-", x = line_collapsed)
    line_collapsed <- gsub(pattern = "fronto ", replacement = "fronto-", x = line_collapsed)
    line_collapsed <- gsub(pattern = "aorto ", replacement = "aorto-", x = line_collapsed)
    line_collapsed <- gsub(pattern = "proteino ", replacement = "proteino-", x = line_collapsed)
    line_collapsed <- gsub(pattern = "loco ", replacement = "loco-", x = line_collapsed)
    line_collapsed <- gsub(pattern = "ethylo ", replacement = "ethylo-", x = line_collapsed)
    line_collapsed <- gsub(pattern = "meningo ", replacement = "meningo-", x = line_collapsed)
    line_collapsed <- gsub(pattern = "arterio ", replacement = "arterio-", x = line_collapsed)
    
    line_collapsed <- gsub(pattern = "poly ", replacement = "poly", x = line_collapsed)
    
    # correct (missing apostrophe)
    line_collapsed <- gsub(pattern = "letat", replacement = "etat", x = line_collapsed)
    line_collapsed <- gsub(pattern = "laorte", replacement = "aorte", x = line_collapsed)
    line_collapsed <- gsub(pattern = "loesophage", replacement = "oesophage", x = line_collapsed)
    line_collapsed <- gsub(pattern = "lendometre", replacement = "endometre", x = line_collapsed)
    line_collapsed <- gsub(pattern = "lestomac", replacement = "estomac", x = line_collapsed)
    line_collapsed <- gsub(pattern = "lartere", replacement = "artere", x = line_collapsed)
    line_collapsed <- gsub(pattern = "luterus", replacement = "uterus", x = line_collapsed)
    line_collapsed <- gsub(pattern = "lamygdale", replacement = "amygdale", x = line_collapsed)
    line_collapsed <- gsub(pattern = "loreillette", replacement = "oreillette", x = line_collapsed)
    line_collapsed <- gsub(pattern = "lamiante", replacement = "amiant", x = line_collapsed)
    line_collapsed <- gsub(pattern = "loropharynx", replacement = "oropharynx", x = line_collapsed)
    line_collapsed <- gsub(pattern = "ladulte", replacement = "adulte", x = line_collapsed)
    line_collapsed <- gsub(pattern = "dalzheimer", replacement = "alzheimer", x = line_collapsed)
    line_collapsed <- gsub(pattern = "lenfance", replacement = "enfance", x = line_collapsed)
    line_collapsed <- gsub(pattern = "lalimentation", replacement = "alimentation", x = line_collapsed)
    line_collapsed <- gsub(pattern = "lheparine", replacement = "heparine", x = line_collapsed)
    
    # correct character
    line_collapsed <- gsub(pattern = "lovaire", replacement = "lobaire", x = line_collapsed)
    line_collapsed <- gsub(pattern = "hemodialyse", replacement = "hemodialysee", x = line_collapsed)
    line_collapsed <- gsub(pattern = "cardique", replacement = "cardiaque", x = line_collapsed)
    line_collapsed <- gsub(pattern = "insulinorequerant", replacement = "insulino-requerant", x = line_collapsed)
    line_collapsed <- gsub(pattern = "recent", replacement = "recente", x = line_collapsed)
    line_collapsed <- gsub(pattern = "repiratoire", replacement = "respiratoire", x = line_collapsed)
    line_collapsed <- gsub(pattern = "multivicerale", replacement = "multiviscerale", x = line_collapsed)
    line_collapsed <- gsub(pattern = "deffaillance", replacement = "defaillance", x = line_collapsed)
    line_collapsed <- gsub(pattern = "insuffissance", replacement = "insuffisance", x = line_collapsed)
    line_collapsed <- gsub(pattern = "stente", replacement = "stent", x = line_collapsed)
    line_collapsed <- gsub(pattern = "terminales", replacement = "terminal", x = line_collapsed)
    line_collapsed <- gsub(pattern = "evolues", replacement = "evoluee", x = line_collapsed)
    line_collapsed <- gsub(pattern = "cardipathie", replacement = "cardiopathie", x = line_collapsed)
    line_collapsed <- gsub(pattern = "innondation", replacement = "inondation", x = line_collapsed)
    line_collapsed <- gsub(pattern = "ethylotabagisme", replacement = "ethylo-tabagisme", x = line_collapsed)
    line_collapsed <- gsub(pattern = "ischiemique", replacement = "ischemique", x = line_collapsed)
    line_collapsed <- gsub(pattern = "multimetastasee", replacement = "multimetastase", x = line_collapsed)
    line_collapsed <- gsub(pattern = "defailance", replacement = "defaillance", x = line_collapsed)
    line_collapsed <- gsub(pattern = "hemoragique", replacement = "hemorragique", x = line_collapsed)
    line_collapsed <- gsub(pattern = "oedematoascitique", replacement = "oedemato-ascitique", x = line_collapsed)
    line_collapsed <- gsub(pattern = "poly-metastase", replacement = "polymetastase", x = line_collapsed)
    line_collapsed <- gsub(pattern = "fibrilation", replacement = "fibrillation", x = line_collapsed)
    line_collapsed <- gsub(pattern = "infactus", replacement = "infarctus", x = line_collapsed)
    line_collapsed <- gsub(pattern = "postanoxique", replacement = "post-anoxique", x = line_collapsed)
    line_collapsed <- gsub(pattern = "levy", replacement = "lewy", x = line_collapsed)
    line_collapsed <- gsub(pattern = "metatases", replacement = "metastases", x = line_collapsed)
    line_collapsed <- gsub(pattern = "metatstases", replacement = "metastases", x = line_collapsed)
    line_collapsed <- gsub(pattern = "oxygenodependante", replacement = "oxygeno-dependante", x = line_collapsed)
    line_collapsed <- gsub(pattern = "infartus", replacement = "infarctus", x = line_collapsed)
    line_collapsed <- gsub(pattern = "insiffisance", replacement = "insuffisance", x = line_collapsed)
    line_collapsed <- gsub(pattern = "soin", replacement = "soins", x = line_collapsed)
    line_collapsed <- gsub(pattern = "associe", replacement = "associee", x = line_collapsed)
    line_collapsed <- gsub(pattern = "denutrtion", replacement = "denutrition", x = line_collapsed)
    line_collapsed <- gsub(pattern = "pneumpathie", replacement = "pneumopathie", x = line_collapsed)
    line_collapsed <- gsub(pattern = "alzeimer", replacement = "alzheimer", x = line_collapsed)
    line_collapsed <- gsub(pattern = "cardio-repiratoire", replacement = "cardio-respiratoire", x = line_collapsed)
    line_collapsed <- gsub(pattern = "isuffisance", replacement = "insuffisance", x = line_collapsed)
    line_collapsed <- gsub(pattern = "metatstatique", replacement = "metastatique", x = line_collapsed)
    line_collapsed <- gsub(pattern = "multiviserale", replacement = "multiviscerale", x = line_collapsed)
    line_collapsed <- gsub(pattern = "cardiorepiratoire", replacement = "cardiorespiratoire", x = line_collapsed)
    line_collapsed <- gsub(pattern = "hemmorragie", replacement = "hemorragie", x = line_collapsed)
    line_collapsed <- gsub(pattern = "insuffiance", replacement = "insuffisance", x = line_collapsed)
    line_collapsed <- gsub(pattern = "insuffisnce", replacement = "insuffisance", x = line_collapsed)
    line_collapsed <- gsub(pattern = "cirhose", replacement = "cirrhose", x = line_collapsed)
    line_collapsed <- gsub(pattern = "hemoragie", replacement = "hemorragie", x = line_collapsed)
    line_collapsed <- gsub(pattern = "metastatses", replacement = "metastases", x = line_collapsed)
    line_collapsed <- gsub(pattern = "pumonaire", replacement = "pulmonaire", x = line_collapsed)
    line_collapsed <- gsub(pattern = "vigile", replacement = "vigil", x = line_collapsed)
    line_collapsed <- gsub(pattern = "caner", replacement = "cancer", x = line_collapsed)
    line_collapsed <- gsub(pattern = "grabatairisation", replacement = "grabatarisation", x = line_collapsed)
    line_collapsed <- gsub(pattern = "inhallation", replacement = "inhalation", x = line_collapsed)
    line_collapsed <- gsub(pattern = "insufffisance", replacement = "insuffisance", x = line_collapsed)
    line_collapsed <- gsub(pattern = "klebsielle", replacement = "klebsiella", x = line_collapsed)
    line_collapsed <- gsub(pattern = "reprises", replacement = "reprise", x = line_collapsed)
    line_collapsed <- gsub(pattern = "ressource", replacement = "ressources", x = line_collapsed)
    
    
    line_collapsed <- gsub(pattern = "aret ", replacement = "arret", x = line_collapsed)
    
    # expand contraction
    line_collapsed <- gsub(pattern = "pulm ", replacement = "pulmonaire", x = line_collapsed)
    line_collapsed <- gsub(pattern = "dte", replacement = "droite", x = line_collapsed)
    line_collapsed <- gsub(pattern = "gche", replacement = "gauche", x = line_collapsed)
    line_collapsed <- gsub(pattern = "metas ", replacement = "metastases", x = line_collapsed)
    line_collapsed <- gsub(pattern = "sdr ", replacement = "syndrome de détresse respiratoire", x = line_collapsed) 
    
    
    # remove "'s"
    #line_collapsed <- gsub(pattern = "'s", replacement = "", x = line_collapsed)
    
    # remove parenthesis
    #line_collapsed <- gsub(pattern = "[(|)]", replacement = "", x = line_collapsed)
    
    # remove "consistent" stop word
    line_collapsed <- gsub(pattern = "consistent", replacement = "", x = line_collapsed)
    
    # remove "patient" stop word
    line_collapsed <- gsub(pattern = "patient", replacement = "", x = line_collapsed)
    
    # remove "pt" stop word (patient)
    line_collapsed <- gsub(pattern = "$pt^", replacement = "", x = line_collapsed)
    
    # remove "illegible" stop word
    line_collapsed <- gsub(pattern = "illegible", replacement = " ", x = line_collapsed)
    
    # change
    line_collapsed <- gsub(pattern = "type one", replacement = "type i", x = line_collapsed)
    line_collapsed <- gsub(pattern = "type 2", replacement = "type ii", x = line_collapsed)
    
    # remove " - " sequence
    #line_collapsed <- gsub(pattern = " - ", replacement = " ", x = line_collapsed)
    
    # correct (remove white space)
    line_collapsed <- gsub(pattern = "uro ", replacement = "uro", x = line_collapsed)
    
    # correct (remove white space)
    line_collapsed <- gsub(pattern = "hypo ", replacement = "hypo", x = line_collapsed)
    
    # correct (remove white space)
    line_collapsed <- gsub(pattern = "mega ", replacement = "mega", x = line_collapsed)
    
    # correct (remove white space)
    line_collapsed <- gsub(pattern = "adeno ", replacement = "adeno", x = line_collapsed)
    
    # correct (remove white space)
    line_collapsed <- gsub(pattern = "hydro ", replacement = "hydro", x = line_collapsed)
    
    # correct (remove white space)
    line_collapsed <- gsub(pattern = "myelo ", replacement = "myelo", x = line_collapsed)
    
    # correct (remove white space)
    line_collapsed <- gsub(pattern = "nonst", replacement = "non st", x = line_collapsed)
    
    # correct (remove white space)
    line_collapsed <- gsub(pattern = "osteo ", replacement = "osteo", x = line_collapsed)
    
    # correct
    line_collapsed <- gsub(pattern = "atiral", replacement = "atrial", x = line_collapsed)
    line_collapsed <- gsub(pattern = "atrail", replacement = "atrial", x = line_collapsed)
    line_collapsed <- gsub(pattern = "llung", replacement = "lung", x = line_collapsed)
    line_collapsed <- gsub(pattern = "pnuemonia", replacement = "pneumonia", x = line_collapsed)
    line_collapsed <- gsub(pattern = "satge", replacement = "stage", x = line_collapsed)
    line_collapsed <- gsub(pattern = "sever", replacement = "severe", x = line_collapsed)
    line_collapsed <- gsub(pattern = "shcok", replacement = "shock", x = line_collapsed)
    
    run$CleanedText <- line_collapsed
    
    write.table(x = run,
                file = run_name, 
                quote = FALSE, 
                append = T, 
                sep = ";", 
                row.names = FALSE, 
                col.names = FALSE)
    
  }
  
}

# clean environment
rm(list = ls())
