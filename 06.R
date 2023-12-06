instr <- gsub(".*: +", "", readLines("06.txt", warn = FALSE)) 

parse1 <- function(l) lapply(strsplit(l, " +"), as.numeric)
parse2 <- function(l) lapply(gsub(" +", "", l), as.numeric)

distance <- function(time) { holds <- 0:time; (time - holds) * holds }

win_conditions <- function(instr) {
  vapply(
    1:length(instr[[1]]), 
    function(x) sum(distance(instr[[1]][x]) > instr[[2]][x]),
    numeric(1)
  )
}

message("Part 1:", prod(win_conditions(parse1(instr))))
message("Part 2:", prod(win_conditions(parse2(instr))))


