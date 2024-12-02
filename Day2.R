file_path <- "C:/Users/..."
lines <- readLines(file_path)
vectors <- lapply(lines, function(line) {
  as.numeric(unlist(strsplit(line, "\\s+")))
})

is_safe <- function(v){
  d <- (diff(v))
  is_increasing <- (max(d)<0)&(min(d)>-4) 
  is_decreasing <-  (max(d)<4)&(min(d)>0) 
  safe <- is_increasing || is_decreasing  # Safe if either condition is true
  return(safe)
} 

is_safe_damp <- function(v){
  if (is_safe(v)) return(TRUE)
  sub_lists <- lapply(seq_along(v), function(i) v[-i])
  for (sub_v in sub_lists) {
    if (is_safe(sub_v)) return(TRUE)
    } 
  return(FALSE)
  }


s<-0
c<-c()

for (i in (1:length(vectors))){
  safe <- is_safe((vectors[[i]]))
  c <- c(c, safe)
  s <- ifelse(safe,s+1,s)
}

cd <- c()
sd <- 0

for (i in (1:length(vectors))){
  safe <- is_safe_damp((vectors[[i]]))
  cd <- c(cd, safe)
  sd <- ifelse(safe,sd+1,sd)
}
