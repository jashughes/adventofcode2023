instr <- readLines("24.txt", warn = FALSE) |>
  lapply(to_vec)

to_vec <- function(s) as.numeric(strsplit(gsub(" +@ +", ",", s,), ", *")[[1]])

#px1 + vx1 * a = px2 + vx2 * b
#py1 + vy1 * a = py2 + vy2 * b

int <- function(v1,v2) {
  px1 <- v1[1]
  px2 <- v2[1]
  vx1 <- v1[4]
  vx2 <- v2[4]
  py1 <- v1[2]
  py2 <- v2[2]
  vy1 <- v1[5]
  vy2 <- v2[5]

  c1 <- vy1/vx1
  c2 <- py1 - py2 + c1 * px2 -c1 * px1
  c3 <- vy2 - c1 * vx2

  b <- c2/c3
  a <- (px2 + vx2 * b - px1)/vx1
  if (a < 0 || b < 0 || is.infinite(a) || is.infinite(b)) return(NA)

  c(px1 + vx1 * a, py1 + vy1 * a)
}


mn <- 200000000000000
mx <- 400000000000000

tot <- 0
for (i in 1:(length(instr) - 1)) {
  for (j in (i+1):length(instr)) {
    ints <- int(instr[[i]], instr[[j]])
    if (isTRUE(min(ints) >= mn & max(ints) <= mx)) tot <- tot + 1
  }
}
tot


# part 2

#px1 + vx1 * a = px2 + vx2 * b
#py1 + vy1 * a = py2 + vy2 * b

#vx1 * a - vx2 * b = px2-px1
#vy1 * a - vy2 * b = py2 - py1


v1 = instr[[1]]
v2 = instr[[2]]

px1 <- v1[1]
px2 <- v2[1]
vx1 <- v1[4]
vx2 <- v2[4]
py1 <- v1[2]
py2 <- v2[2]
vy1 <- v1[5]
vy2 <- v2[5]


m1 <- matrix(c(
  vx1, -vx2,
  vy1, -vy2
), byrow = T, nrow = 2)
a <- c(px2-px1, py2-py1)
solve(m1, a)


m <- matrix(unlist(instr), nrow = length(instr), byrow = T)

fn <- function(px,py,pz,vx,vy,vz,m) {


}

optim(rep(1,length(instr)), fn)
