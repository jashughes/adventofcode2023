library(igraph)

instr <- readLines("10.txt", warn = FALSE) |>
  strsplit("")

key <- list(
  "F" = list(c( 1, 0), c( 0, 1)),     # down:  c( 1, 0)
  "J" = list(c(-1, 0), c( 0,-1)),     # up:    c(-1, 0)
  "L" = list(c(-1, 0), c( 0, 1)),     # left:  c( 0,-1)
  "7" = list(c( 0,-1), c( 1, 0)),     # right: c( 0, 1)
  "-" = list(c( 0,-1), c( 0, 1)),
  "|" = list(c( 1, 0), c(-1, 0))
)

zip <- function(m) paste0(m[,1], "_", m[,2])

get_idx <- function(pos, l, maxx = dim(m)[1], maxy = dim(m)[2]) {
  idx <- rbind(pos + l[[1]], pos + l[[2]])
  idx[idx[,1] > 0 & idx[,1] <= maxx & idx[,2] > 0 & idx[,2] <= maxy,,drop = F]
}

create_graph <- function(m, pipes) {
  g <- make_undirected_graph(NULL)
  g <- add_vertices(g, nrow(pipes))
  seen <- c()
  for (i in 1:nrow(pipes)) {
    if (m[pipes[i,,drop=F]] == "S") next
    nbs <- get_idx(pipes[i,,drop=F], key[[m[pipes[i,,drop=F]]]])
    if (nrow(nbs) == 0) next
    # check neighbours are valid
    for (nb in 1:nrow(nbs)) {
      if (m[nbs[nb,,drop=F]] == ".") next
      if (m[nbs[nb,,drop=F]] == "S") {
        node <- which(zip(pipes) %in% zip(nbs[nb,,drop=F]))
        nl <- c(paste0(node, "_", i), paste0(i, "_", node))
        if (!any(nl %in% seen)) {
          seen <- c(seen, nl[1])
          g <- add_edges(g, c(i, node))
        }
      } else {
        nnb <- get_idx(nbs[nb,,drop=F], key[[m[nbs[nb,,drop=F]]]])
        if (zip(pipes[i,,drop=F]) %in% zip(nnb)) {
          # add to graph
          node <- which(zip(pipes) %in% zip(nbs[nb,,drop=F]))
          nl <- c(paste0(node, "_", i), paste0(i, "_", node))
          if (!any(nl %in% seen)) {
            seen <- c(seen, nl[1])
            g <- add_edges(g, c(i, node))
          }
        }
      }
    }
  }
  g
}

pos_around <- function(pos, maxx = dim(m)[1], maxy = dim(m)[2]) {
  p <- rbind(pos + c(-1, 0), pos + c(0, -1), pos + c(1, 0),  pos + c(0, 1))
  p[p[,1] > 0 & p[,1] <= maxx & p[,2] > 0 & p[,2] <= maxy,]
}

erode <- function(m) {
  d <- 1
  m[c(1, nrow(m)), ][m[c(1, nrow(m)), ] == 0] <- -1
  m[ , c(1, ncol(m))][m[,c(1, ncol(m))] == 0] <- -1
  while (d > 0) {
    d <- 0
    for (x in 2:(nrow(m)-1)) {
      for (y in 2:(ncol(m)-1)) {
        if (m[x,y] == 0 & any(m[pos_around(matrix(c(x, y), nrow = 1))] == -1)) {
          d <- d + 1
          m[x,y] <- -1
        }
      }
    }
  }
  m
}

expand <- function(mainpath, pipes, g) {
  m <- matrix(0, nrow = max((pipes[,1]+1) * 2 ), ncol = max((pipes[,2]+1) * 2))
  verts <- intersect(as.vector(V(g)), mainpath)
  for (v in verts) {
    nbs <- adjacent_vertices(g, v)[[1]]
    p1 <- pipes[v,,drop=F]
    for (nb in nbs) {
      p2 <- pipes[nb,,drop=F]
      m[(p1[,1]*2):(p2[,1]*2),p1[,2]*2] <- 1
      m[p1[,1]*2,(p1[,2]*2):(p2[,2]*2)] <- 1
    }
  }
  m
}

shrink <- function(m) sum(m[seq(2, nrow(m),2),seq(2, ncol(m),2)] == 0)

m <- matrix(unlist(instr), nrow = length(instr), byrow = TRUE)
st <- which(m == "S", arr.ind = T)
pipes <- which(m != ".", arr.ind = T)
g <- create_graph(m, pipes)
paths <- suppressWarnings(shortest_paths(g, which(zip(pipes) == zip(st))))

message("Part 1: ", max(vapply(paths$vpath, length, numeric(1)) - 1))
message("Part 2: ", shrink(erode(expand(unique(unlist(paths$vpath)),pipes,g))))
