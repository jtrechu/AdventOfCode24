file_path <- "C:/Users/..."
text <- readLines(file_path)
text_combined <- paste(text, collapse = "")
library(stringr)

##Part 1: Using Regular Expresions

matches <- str_extract_all(text_combined, "mul\\(\\d+,\\d+\\)")


extract_multiplicands <- function(mul_string) {
  matches <- str_extract_all(mul_string, "\\d+")
  as.numeric(matches[[1]])
}

total_sum<-0
for (i in 1:length(matches)){
  for(j in 1:length(matches[[i]])){
    total_sum<- total_sum+ prod(extract_multiplicands((matches[[i]])[j]))
    
  }
}
total_sum 

##PART 2

state <- TRUE 
total_sum <- 0


matches <- str_extract_all(text_combined, "do\\(\\)|don't\\(\\)|mul\\(\\d+,\\d+\\)")

for (instruction in matches[[1]]) {
  if (str_detect(instruction, "do\\(\\)")) {
    state <- TRUE  # Enable mul instructions
  } else if (str_detect(instruction, "don't\\(\\)")) {
    state <- FALSE  # Disable mul instructions
  } else if (str_detect(instruction, "mul\\(\\d+,\\d+\\)")) {
    if (state) {
      total_sum <- total_sum + prod(extract_multiplicands(instruction))
    }
  }
}
total_sum
