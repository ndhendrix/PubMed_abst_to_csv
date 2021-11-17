# set working directory
setwd("D:/ndhen/Dropbox (UW)/Postdoc/Health system strenghtening review/Manuscript/Lit review 2")

# read in txt file
textfile <- file("abstract-20100101Da-set.txt")
lines <- readLines(textfile, warn = F)
close(textfile)

# get start of each entry
starts <- grep("^[[:digit:]]+[.][[:space:]]", lines)
starts <- append(starts, length(lines) + 1)

# create empty data frame for export
out <- data.frame(citation = character(),
                  title = character(),
                  authors = character(),
                  author_info = character(),
                  abstract = character())

# process data
for(i in 1:(length(starts) - 1)) {
  
  # select all data within one entry
  temp_entry <- lines[starts[i]:starts[i+1] - 1]
  print(starts[i])
  print(starts[i+1] - 1)
  
  # create blank vectors to add data to
  combined <- character()
  tempchar <- character()
  
  # combine data from different lines
  for(i in 1:length(temp_entry)) {
    if(temp_entry[i] != "") {
      tempchar <- append(tempchar, temp_entry[i])
    } else {
      combined <- append(combined, paste(tempchar, collapse = ""))
      tempchar <- character()
    }
  }
  
  # remove blank data
  combined <- combined[combined != ""]
  
  # prepare for addition to export dataframe
  temp_out <- data.frame(citation = substr(combined[1],
                                           regexpr("[[:space:]]", combined[1]),
                                           nchar(combined[1])),
                         title = combined[2],
                         authors = combined[3],
                         author_info = substr(combined[4],
                                              20,
                                              nchar(combined[4])),
                         abstract = if(!grepl("^Comment in|Erratum in", combined[5])) {
                           combined[5]
                         } else if (!grepl("^Comment in|Erratum in", combined[6])) {
                           combined[6]
                         } else {
                           combined[7]
                         })
                         
  # add to export data frame
  out <- rbind(out, temp_out)
}

# write to storage
write.csv(out, "out.csv")