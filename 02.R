instr <- gsub(".*: ", "", readLines("02.txt", warn = FALSE))

maxes <- c(red = 12, green = 13, blue = 14)
part1 <- function(ns, cols) all(ns <= maxes[cols])
part2 <- function(ns, cols) {
  max(ns[cols == "green"]) * max(ns[cols == "red"]) * max(ns[cols == "blue"])
}

check_game <- function(s, summary_function) {
  rounds <- strsplit(s, ";")[[1]]
  pulls <- unlist(lapply(rounds, strsplit, ","))
  cols <- gsub(".* ", "", pulls)
  ns <- as.numeric(gsub(" *(\\d+).*", "\\1", pulls))
  summary_function(ns, cols)
}

message(
  "Part 1: ",
  sum(which(vapply(instr, check_game, part1, FUN.VALUE = logical(1))))
)
message(
  "Part 2: ",
  sum(vapply(instr, check_game, part2, FUN.VALUE = numeric(1)))
)

