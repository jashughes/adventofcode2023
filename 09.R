instr <- readLines("09.txt", warn = FALSE) |>
  strsplit(" ") |>
  lapply(as.numeric)

pred <- function(v) {
  dv <- diff(v)
  if (length(unique(dv)) == 1) return(tail(v, 1) + dv[1])
  tail(v, 1) + pred(dv)
}

message("Part 1: ", sum(vapply(instr, function(x) pred(x), 1)))
message("Part 2: ", sum(vapply(instr, function(x) pred(rev(x)), 1)))

