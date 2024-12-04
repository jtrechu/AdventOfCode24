file_path <- "C:/Users/..."
lines <- readLines(file_path)
char_matrix <- do.call(rbind, lapply(lines, function(line) strsplit(line, "")[[1]]))


n <- nrow(char_matrix)
m <- ncol(char_matrix)
total <- 0

#We will define the 8 directions we can use (horizontal, vertical, and both 
# diagonals forwards and backwards)

# Define the directions for word search: horizontal, vertical, diagonal
directions <- list(
  list(dx = 0, dy = 1),  # Right
  list(dx = 0, dy = -1), # Left
  list(dx = 1, dy = 0),  # Down
  list(dx = -1, dy = 0), # Up
  list(dx = 1, dy = 1),  # Down-right (diagonal)
  list(dx = -1, dy = -1),# Up-left (diagonal)
  list(dx = 1, dy = -1), # Down-left (diagonal)
  list(dx = -1, dy = 1)  # Up-right (diagonal)
)

# Function to check if the word "XMAS" exists in a given direction
check_word <- function(x, y, dx, dy) {
  word <- c("X", "M", "A", "S")
  for (i in 1:4) {
    nx <- x + (i - 1) * dx
    ny <- y + (i - 1) * dy
    if (nx < 1 || nx > n || ny < 1 || ny > m || char_matrix[nx, ny] != word[i]) {
      return(FALSE) 
      #It is false if it moves to out of the matrix or it does not find the word
    }
  }
  return(TRUE)
}

# Loop over the matrix and the directions
for (i in 1:n) {
  for (j in 1:m) {
    for (dir in directions) {
      if (check_word(i, j, dir$dx, dir$dy)) {
        total <- total + 1
      }
    }
  }
}


#Part 2: We drop the directions here and do it differently

#We will check X-MAS by looking at the A (which can only be in inner cells)
#and the check the M,S are in an acceptable pattern

check_xmas <- function(x, y) {
  if (char_matrix[x, y] == "A") {
    # Check the first MAS
    if ((char_matrix[x-1, y-1] == "M" && char_matrix[x+1, y+1] == "S") ||
        (char_matrix[x-1, y-1] == "S" && char_matrix[x+1, y+1] == "M")) {
      
      # Check the second MAS
      if ((char_matrix[x+1, y-1] == "S" && char_matrix[x-1, y+1] == "M") ||
          (char_matrix[x+1, y-1] == "M" && char_matrix[x-1, y+1] == "S")) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}
total2 <- 0
# Loop through the matrix and search for X-MAS with A as the center
for (i in 2:(n-1)) { 
  for (j in 2:(m-1)) { 
    if (check_xmas(i, j)) {
      total2 <- total2 + 1
    }
  }
}
total
total2
