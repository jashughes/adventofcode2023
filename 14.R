instr <- readLines("14.txt", warn = FALSE) |>
  strsplit("") 

m <- matrix(unlist(instr), nrow = length(instr), byrow = T)

tilt <- function(v) {
  s <- strsplit(paste0(gsub("#", "S#S", v), collapse = ""), "S")[[1]]
  vapply(s, function(x) strsplit(x, "")[[1]] |> stone_sort() |> paste0(collapse = ""), character(1)) |>
    strsplit("") |>
    unlist() |>
    unname()
}
stone_sort <- function(v) as.character(sort(factor(v, levels = c("O", ".", "#"))))
score <- function(m) sum((nrow(m) + 1) - which(m == "O", arr.ind = T)[,1])
rotate <- function(m) t(apply(m, 2, rev))
tilt_board <- function(m) {
  for (dir in 1:4) { m <- rotate(apply(m, 2, tilt)) }
  m
}

# See code below for extracting patterns
cycle_score <- function(N)  {
  reps <- c(100079, 100064, 100047, 100034, 100024, 100016, 100008, 100011, 
            100025, 100043, 100071, 100084, 100084, 100086, 100084, 100086, 
            100086)
  cycle_pos <- (N - 90) %% length(reps)
  reps[ifelse(cycle_pos == 0, length(reps), cycle_pos)]
}

message("Part 1: ", score(apply(m, 2, tilt)))
message("Part 2: ", cycle_score(1000000000))

# Empirical identification of cycle patterns.
scores <- c()
for (i in 1:250) { # cycles start after aroun 90 tilts.
  m <- tilt_board(m)
  scores <- c(scores, score(m))
}
table(scores) # some numbers repeat multiple times a cycle
reps <- which(scores == 100079) # this one repeats only once per cycle


