instr <- readLines("11.txt", warn = FALSE) |>
  strsplit("")

manhattan <- function(p1, p2) sum(abs(p1 - p2))

galaxy_deltas <- function(instr, expand) {
  m <- matrix(unlist(instr) == "#", nrow = length(instr), byrow = TRUE)

  dr <- which(rowSums(m) == 0)
  dc <- which(colSums(m) == 0)
  
  gal <- which(m, arr.ind = T)
  out <- c()
  
  for (g1 in 1:nrow(gal)) {
    for (g2 in g1:nrow(gal)) {
      p1 <- gal[g1,,drop=F]
      p2 <- gal[g2,,drop=F]
      nr <- sum(dr > pmin(p1[1], p2[1]) & dr < pmax(p1[1], p2[1]))
      nc <- sum(dc > pmin(p1[2], p2[2]) & dc < pmax(p1[2], p2[2]))
      out <- c(manhattan(p1, p2) + nr * (expand -1) + nc * (expand - 1), out)
    }
  }
  sum(out)
}

message("Part 1:", galaxy_deltas(instr, 2))
message("Part 1:", galaxy_deltas(instr, 1000000))

