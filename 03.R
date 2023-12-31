instr <- readLines("03.txt", warn = FALSE)
instrl <- unlist(strsplit(instr, ""))
inc_syms <- setdiff(unique(instrl), c(".", as.character(0:9)))
pos <- lapply(instr, function(x) gregexpr("[0-9]+", x, perl = F)[[1]])
starts <- lapply(pos, as.vector)
ends <- lapply(
  pos, 
  function(x) as.vector(attr(x, "match.length")) + as.vector(x) - 1
)

get_positions <- function(v, syms) {
  which(matrix(v %in% syms, length(instr), byrow = T), arr.ind = T)
}
check_pos <- function(start, end, y) (start - 1 <= y) & (end + 1 >= y) 

# Part 1
check_row <- function(sts, eds, y, s) {
  tot <- 0
  for (j in 1:length(sts)) {
    if (check_pos(sts[j], eds[j], y)) {
      tot = tot + as.numeric(substr(s, sts[j], eds[j]))
    }
  }
  tot
}

cd <- get_positions(instrl, setdiff(unique(instrl), c(".", as.character(0:9))))
tot1 <- 0
for (i in 1:nrow(cd)) {
  for (x in setdiff(c(cd[i,1] - 1, cd[i,1], cd[i,1] + 1), c(0, length(instr) + 1))) {
    tot1 <- tot1 + check_row(starts[[x]], ends[[x]], cd[i,2], instr[[x]])
  }
}

# Part 2
cd <- get_positions(instrl, "*")

find_adj <- function(sts, eds, y, s) {
  adj <- c()
  for (j in 1:length(sts)) {
    if (check_pos(sts[j], eds[j], y)) {
      adj <- c(adj, as.numeric(substr(s, sts[j], eds[j])))
    }
  }
  adj
}

tot2  <- 0
for (i in 1:nrow(cd)) {
  new_adj <- c()
  for (x in setdiff(c(cd[i,1] - 1, cd[i,1], cd[i,1] + 1), c(0, length(instr) + 1))) {
    new_adj <- c(new_adj, find_adj(starts[[x]], ends[[x]], cd[i,2], instr[[x]]))
  }
  if (length(new_adj) == 2) tot2 <- tot2 + prod(new_adj)
}

message("Part 1: ", tot1)
message("Part 1: ", tot2)
