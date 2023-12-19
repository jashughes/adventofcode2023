instr <- readLines("14.txt", warn = FALSE) |>
  strsplit("") 

m <- t(matrix(unlist(instr), nrow = length(instr), byrow = T))

#v <- m[6,]
tilt <- function(v) {
  s <- strsplit(paste0(gsub("#", "S#S", v), collapse = ""), "S")[[1]]
  lapply(s, function(x) strsplit(x, "")[[1]] |> stone_sort() |> paste0(collapse = "")) |>
    unlist() |>
    paste0(collapse = "")
    
}
stone_sort <- function(v) as.character(sort(factor(v, levels = c("O", ".", "#"))))

tot <- 0
maxx <- length(instr)
for (i in 1:nrow(m)) {
  tilted <- strsplit(tilt(m[i,]), "")[[1]]
  which(tilted == "O")
  tot <- tot + sum(maxx + 1 - which(tilted == "O")) * (length(which(tilted == "O")) > 0)
}
tot
