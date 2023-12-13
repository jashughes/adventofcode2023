instr <- readLines("13.txt", warn = FALSE) 

symmetry <- function(m1, m2) all(m1 == m2)
smidge <- function(m1, m2) sum(m1 != m2) == 1

scan <- function(m, f) {
  nc <- ncol(m)
  for (cut in 1:(nc-1)) {
    lhs <- 1:cut
    rhs <- nc:(cut+1)
    if (f(m[,tail(lhs,length(rhs)),drop=F], m[,tail(rhs,length(lhs)),drop=F])) {
      return(cut)
    }
  }
  NULL
}

solve <- function(instr, f) {
  starts <- c(1, which(instr == "") + 1)
  ends <- c(which(instr == "") - 1, length(instr))
  tot <- 0
  for (i in 1:(length(starts))) {
    m <- instr[starts[i]:ends[i]]
    m <- matrix(unlist(strsplit(m, "")) == "#", nrow = length(m), byrow = T)
    cut <- scan(m, f)
    if (is.null(cut)) cut <- 100 * scan(t(m), f)
    tot <- tot + cut
  }
  tot
}

message("Part 1:", solve(instr, smidge))
message("Part 2:", solve(instr, symmetry))

