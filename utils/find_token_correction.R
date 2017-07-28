mispellings <- data.frame(line = lines, 
                          token = tokens, 
                          matched_term = unlist(matched_terms),
                          stringsAsFactors = FALSE)

str(mispellings)

mispellings_8960 <- mispellings

save(mispellings_8960, file = "./mispellings_8960.RData")

mispellings <- rbind(mispellings_8959, mispellings_8960)

str(mispellings)

save(mispellings, file = "./mispellings.RData")

counts <- table(mispellings$token)
#counts <- table(mispellings[, 2:3])

str(counts)

#head(sort(counts, decreasing = T))

counts_sorted <- data.frame(sort(counts, decreasing = T))
names(counts_sorted) <- c("token", "count")
head(counts_sorted)

counts <- data.frame(counts)

token_and_correction <- data.frame(unique(mispellings[, 2:3]))
str(token_and_correction)
head(token_and_correction)

counts_correction <- merge(x = counts_sorted, y = token_and_correction)
counts_correction <- counts_correction[, c(2, 1, 3)]
head(counts_correction)

counts_correction_sorted <- counts_correction[order(counts_correction$count, decreasing = T), ]

write.csv2(counts_correction_sorted, file = "./count_token.csv")
