# set working directory
setwd("~~~~YOUR WORKING DIRECTORY HERE~~~~")

# read in txt file
textfile <- file("~~~~~YOUR FILE NAME.txt~~~~~")
lines <- readLines(textfile, warn = F)
close(textfile)

# remove lines with language data
lines <- lines[!grepl("[Article", lines, fixed = T)]

# get start of each entry
starts <- grep("^[[:digit:]]+[.][[:space:]][[:print:]]+[doi:]", lines)
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
  
  # remove lines with copyright, erratum, update, and comment info
  #combined <- combined[!grepl("^Comment in|Erratum in|Update of", combined)]
  #combined <- combined[!grepl("©", combined)]
  
  # prepare for addition to export dataframe
  temp_out <- data.frame(citation = substr(combined[1],
                                           regexpr("[[:space:]]", combined[1]),
                                           nchar(combined[1])),
                         title = combined[2],
                         authors = combined[3],
                         author_info = ifelse(length(grep("Author information:", combined)) > 0,
                                              substr(combined[grep("Author information:", combined)],
                                                     20,
                                                     nchar(combined[grep("Author information:", combined)])),
                                              NA),
                         abstract = combined[which(nchar(combined) == max(nchar(combined)))])
                         
  # add to export data frame
  out <- rbind(out, temp_out)
}

# write to storage
write.csv(out, "out.csv")