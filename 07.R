instr <- suppressWarnings(read.delim("07.txt", sep = " ", header = F))
colnames(instr) <- c("hand", "bid")

score_hand <- function(s, joker = FALSE) {
  cards <- strsplit(s, "")[[1]]
  tab <- table(cards)
  counts <- as.vector(unname(sort(tab)))
  if (joker && grepl("J", s)) { 
    if (identical(counts, 5L)) 7
    else if (identical(counts, c(1L, 1L, 1L, 1L, 1L))) 2
    else if (identical(counts, c(1L, 4L))) 7
    else if (identical(counts, c(2L, 3L))) 7
    else if (identical(counts, c(1L, 1L, 3L))) 6
    else if (tab[["J"]] == 1) {
      if (identical(counts, c(1L, 2L, 2L))) return(5)
      if (identical(counts, c(1L, 1L, 1L, 2L))) return(4)
    }
    else if (identical(counts, c(1L, 2L, 2L))) 6
    else if (identical(counts, c(1L, 1L, 1L, 2L))) 4
  } else {
    if (identical(counts, 5L)) 7
    else if (identical(counts, c(1L, 4L))) 6
    else if (identical(counts, c(2L, 3L))) 5
    else if (identical(counts, c(1L, 1L, 3L))) 4
    else if (identical(counts, c(1L, 2L, 2L))) 3
    else if (identical(counts, c(1L, 1L, 1L, 2L))) 2
    else if (identical(counts, c(1L, 1L, 1L, 1L, 1L))) 1
  }
} 

rank_card <- function(hand, joker = FALSE) {
  ranks <- strsplit(ifelse(joker, "J23456789TQKA", "23456789TJQKA"), "")[[1]]
  vapply(strsplit(hand,"")[[1]], function(x) which(ranks == x), numeric(1))
}

winnings <- function(instr, joker = FALSE) {
  instr$score <- vapply(instr$hand, score_hand, numeric(1), joker)
  instr[,4:8] <- t(vapply(instr$hand,rank_card,numeric(5),joker,USE.NAMES = F))
  sorted <- instr[with(instr, order(score, V4, V5, V6, V7, V8)),]
  sorted$rank <- 1:nrow(sorted)
  sum(sorted$rank * sorted$bid)
}

message("Part 1: ", winnings(instr, joker = FALSE))
message("Part 2: ", winnings(instr, joker = TRUE))
