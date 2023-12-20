instr <- strsplit(readLines("18.txt", warn = FALSE), " +")
parse_n <- function(s) strtoi(substr(s,2,nchar(s)-1),16)
parse_d <- function(s) c("R","D","L","U")[as.numeric(substr(s,nchar(s),nchar(s)))+1]
part2 <- lapply(
  instr, 
  function(x) {
    s <- gsub("\\(|\\)", "", x[3])
    c(parse_d(s), parse_n(s))
  }
)
find_corners <- function(instr) {
  pos <- c(1, 1)
  pts <- matrix(pos, ncol = 2)
  for (i in instr) {
    dir <- switch(i[1], "R"=c(0,1), "L"=c(0,-1), "U"=c(-1, 0), "D"=c(1, 0))
    mag <- dir * as.numeric(i[2])
    pts <- rbind(pts, matrix(c(pos[1] + mag[1], pos[2] + mag[2]), ncol = 2))
    pos <- pos + mag
  }
  pts
}
shoelace <- function(pts) {
  tot <- 0
  for (p in 2:nrow(pts)) {
    tot <- tot + pts[p-1, 1] * pts[p, 2] - pts[p-1, 2] * pts[p, 1] 
  }
  0.5 * abs(tot)
}
perimeter <- function(pts) {
  tot <- 0
  for (p in 2:nrow(pts)) {
    tot <- tot + abs(pts[p, 1] - pts[p-1, 1]) + abs(pts[p, 2] - pts[p-1, 2]) 
  }
  tot
}
shoelace_and_laces <- function(pts) shoelace(pts) + perimeter(pts) / 2 + 1

message("Part 1: ", shoelace_and_laces(find_corners(instr)))
message("Part 2: ", shoelace_and_laces(find_corners(part2)))

