instr <- gsub(".*: ", "", readLines("04.txt", warn = FALSE)) |>
  lapply(function(x) strsplit(x, "\\|")[[1]]) 

score <- function(c1, c2) {
  ol <- length(intersect(card2vec(c1), card2vec(c2)))
  ifelse(ol > 0, 2 ** (ol - 1), 0)
}
card2vec <- function(s) as.numeric(setdiff(strsplit(s, " +")[[1]], ""))
nwin <- function(v1, v2, n) length(intersect(v1, v2))
points <- function(instr) vapply(instr, function(x) score(x[[1]], x[[2]]), c(1))

deal <- function(instr, hand = rep(1, length(instr))) {
  for (i in 1:length(instr)) {
    n <- nwin(card2vec(instr[[i]][1]), card2vec(instr[[i]][2]), hand[i])
    if (n > 0) hand[(i+1):(i+n)] <- (hand[(i+1):(i+n)]) + hand[i]
  }
  hand
}

message("Part 1: ", sum(points(instr)))
message("Part 2: ", sum(deal(instr)))
