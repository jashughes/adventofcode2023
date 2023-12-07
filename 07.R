library(dplyr)
library(tidyr)
instr <- data.frame(readLines("07.txt", warn = FALSE))
colnames(instr) <- "inpt"

score_hand <- function(s) {
  cards <- strsplit(s, "")[[1]]
  counts <- as.vector(unname(sort(table(cards))))
  if (identical(counts, 5L)) 7
  else if (identical(counts, c(1L, 1L, 1L, 1L, 1L))) 1
  else if (identical(counts, c(1L, 4L))) 6
  else if (identical(counts, c(2L, 3L))) 5
  else if (identical(counts, c(1L, 1L, 3L))) 4
  else if (identical(counts, c(1L, 2L, 2L))) 3
  else if (identical(counts, c(1L, 1L, 1L, 2L))) 2
}  

rank_card <- function(s) {
  which(rev(strsplit("AKQJT98765432", "")[[1]]) == s)
}

instr |>
  separate(inpt, sep = " ", into = c("hand", "bid")) %>%
  rowwise() %>%
  mutate(
    score = score_hand(hand),
    c1 = rank_card(substr(hand, 1,1)),
    c2 = rank_card(substr(hand, 2,2)),
    c3 = rank_card(substr(hand, 3,3)),
    c4 = rank_card(substr(hand, 4,4)),
    c5 = rank_card(substr(hand, 5,5)),
  ) %>%
  ungroup() %>%
  arrange(score, c1, c2, c3, c4, c5) %>%
  mutate(
    hand_rank = row_number(),
    winnings = as.numeric(bid) * hand_rank
  ) %>%
  summarize(tot = sum(winnings))



score_hand_j <- function(s) {
  cards <- strsplit(s, "")[[1]]
  if (length(unique(cards)) == 1) return(7)
  if (length(unique(cards)) == 5) return(2)
  tab <- table(cards)
  counts <- as.vector(unname(sort(table(cards))))
  if (identical(counts, c(1L, 4L))) return(7) 
  if (identical(counts, c(2L, 3L))) return(7)
  if (identical(counts, c(1L, 1L, 3L))) return(6)
  if (tab[["J"]] == 1) {
    if (identical(counts, c(1L, 2L, 2L))) return(5)
    if (identical(counts, c(1L, 1L, 1L, 2L))) return(4)
  }
  if (identical(counts, c(1L, 2L, 2L))) return(6) # two J become other pair
  if (identical(counts, c(1L, 1L, 1L, 2L))) return(4) # two J become 1
  NULL
}  

# 7 (5oaK)
# 6 4oaK
# 5 full house 2/3
# 4 3oaK
# 3 2pair
# 2 1pair

rank_card_j <- function(s) {
  which(rev(strsplit("AKQT98765432J", "")[[1]]) == s)
}

instr |>
  separate(inpt, sep = " ", into = c("hand", "bid")) %>%
  rowwise() %>%
  mutate(
    score = ifelse(grepl("J", hand), score_hand_j(hand), score_hand(hand)),
    c1 = rank_card_j(substr(hand, 1,1)),
    c2 = rank_card_j(substr(hand, 2,2)),
    c3 = rank_card_j(substr(hand, 3,3)),
    c4 = rank_card_j(substr(hand, 4,4)),
    c5 = rank_card_j(substr(hand, 5,5)),
  ) %>%
  ungroup() %>%
  arrange(score, c1, c2, c3, c4, c5) %>%
  mutate(
    hand_rank = row_number(),
    winnings = as.numeric(bid) * hand_rank
  ) %>%
  summarize(tot = sum(winnings))



