# define number of lines to read (default 91953, the whole dataset)
ac_2014 <- 91953

# create data frame
causes_brutes_2014_FR <- data.frame(DocID = rep("DocID", ac_2014),
                                    YearCoded = rep(2000, ac_2014),
                                    LineID = rep(0, ac_2014),
                                    RawText = rep("RawText", ac_2014),
                                    IntType = rep(0, ac_2014),
                                    IntValue = rep(0, ac_2014),
                                    CauseRank = rep("CauseRank", ac_2014),
                                    StandardText = rep("StandardText", ac_2014),
                                    ICD10 = rep("ICD10", ac_2014), 
                                    stringsAsFactors = FALSE)
#head(causes_brutes_2014_FR)

# path to file
file_path <- "../data/CLEFeHealth2017Task1_test_FR/raw/corpus/CausesBrutes_FR_test2014.csv"

# just rename variable
num_lines <- ac_2014

con <- file(file_path, "r")

count_wrong_lines <- 0
count_check_lines <- 0

# skip header
readLines(con, n = 1)

#i <- 1253
for (i in 1:num_lines) {
  
  if ((i %% 1000) == 0) {
    print(paste0("reading line ", i))
  }
  
  line <- readLines(con, n = 1)
  if ( length(line) == 0 ) {
    break
  }
  
  # count semicolon
  count_semicolon <- gregexpr(pattern = ";", text = line)  
  #str(count_semicolon[1])
  #print(paste0("line ", i, " = ", length(count_semicolon[[1]])))
  
  split_line <- strsplit(x = line, split = ";")
  
  if (length(count_semicolon[[1]]) > 5) {
    
    print(paste0("line ", i, " = ", length(count_semicolon[[1]])))
    count_wrong_lines <- count_wrong_lines + 1
    
    # docid
    field_1 <- split_line[[1]][1]
    causes_brutes_2014_FR[i, 1] <- field_1
    
    # year coded
    field_2 <- split_line[[1]][2]
    causes_brutes_2014_FR[i, 2] <- as.numeric(field_2)
    
    # line id 
    field_3 <- split_line[[1]][3]
    causes_brutes_2014_FR[i, 3] <- as.numeric(field_3)

    # raw text
    field_4 <- split_line[[1]][4]
    
    count_semi <- length(count_semicolon[[1]])
    
    for (j in 1:(count_semi - 5)) {
      # j <- 2
      field_4 <- paste(field_4, split_line[[1]][4 + j], sep = ",")
      
    }
    
    causes_brutes_2014_FR[i, 4] <- field_4
    
    field_5 <- split_line[[1]][count_semi]
    # # check field 5 
    if(field_5 != "NULL") {
      # manage NA warning
      tryCatch({
        field_5 <- as.numeric(field_5)
      }, warning = function(w) {
        # manage NA warning
        field_5 <<- NA
        count_check_lines <<- count_check_lines + 1
        #print(paste0("check field 8 line = ", i))
      }, error = function(e) {
        print("error-handler-code")
      }, finally = {
        #print("cleanup-code")
        causes_brutes_2014_FR[i, 5] <- field_5
      })
      
    } else {
      causes_brutes_2014_FR[i, 5] <- NA
    }
    
    
    field_6 <- split_line[[1]][count_semi + 1]
    # Int Value
    if(field_6 != "NULL") {
      # manage NA warning
      tryCatch({
        field_6 <- as.numeric(field_6)
      }, warning = function(w) {
        # manage NA warning
        field_6 <<- NA
        count_check_lines <<- count_check_lines + 1
        #print(paste0("check field 8 line = ", i))
      }, error = function(e) {
        print("error-handler-code")
      }, finally = {
        #print("cleanup-code")
        causes_brutes_2014_FR[i, 6] <- field_6
      })
      
    } else {
      causes_brutes_2014_FR[i, 6] <- NA
    }
    
    
  } else if (length(count_semicolon[[1]]) == 5) {
    
    split_line <- strsplit(x = line, split = ";")
    
    # docid
    field_1 <- split_line[[1]][1]
    causes_brutes_2014_FR[i, 1] <- field_1
    
    # year coded
    field_2 <- split_line[[1]][2]
    causes_brutes_2014_FR[i, 2] <- as.numeric(field_2)
    
    # line id 
    field_3 <- split_line[[1]][3]
    causes_brutes_2014_FR[i, 3] <- as.numeric(field_3)
    
    # raw text
    field_4 <- split_line[[1]][4]
    causes_brutes_2014_FR[i, 4] <- field_4

    field_5 <- split_line[[1]][5]
    # # check field 5 
    if(field_5 != "NULL") {
      # manage NA warning
      tryCatch({
        field_5 <- as.numeric(field_5)
      }, warning = function(w) {
        # manage NA warning
        field_5 <<- NA
        count_check_lines <<- count_check_lines + 1
        #print(paste0("check field 8 line = ", i))
      }, error = function(e) {
        print("error-handler-code")
      }, finally = {
        #print("cleanup-code")
        causes_brutes_2014_FR[i, 5] <- field_5
      })

    } else {
      causes_brutes_2014_FR[i, 5] <- NA
    }
    # # merge fields if necessary
    # if(is.na(field_5)) {
    #   field_4 <- paste(field_4, split_line[[1]][5], sep = ",")
    # }
    
    # Int Value
    field_6 <- split_line[[1]][6]
    if(field_6 != "NULL") {
      # manage NA warning
      tryCatch({
        field_6 <- as.numeric(field_6)
      }, warning = function(w) {
        # manage NA warning
        field_6 <<- NA
        count_check_lines <<- count_check_lines + 1
        #print(paste0("check field 8 line = ", i))
      }, error = function(e) {
        print("error-handler-code")
      }, finally = {
        #print("cleanup-code")
        causes_brutes_2014_FR[i, 6] <- field_6
      })

    } else {
      causes_brutes_2014_FR[i, 6] <- NA
    }

  } else {
    
    print(paste0("*** check line ", i, " = ", length(count_semicolon[[1]])))
  }
}

close(con)
#}

#print(count_wrong_lines)
#print(count_check_lines)

save(causes_brutes_2014_FR, file = "./data/causes_brutes_2014_FR.RData")

