library(igraph)
instr <- readLines("25.txt", warn = FALSE) |>
  strsplit(": +")

l <- list()
for (i in 1:length(instr)) {
  s <- instr[[i]]
  s2 <- strsplit(s[2], " +")[[1]]
  l[[s[1]]] <- c(l[[s[1]]], s2)
  for (sn in s2) {
    l[[sn]] <- c(l[[sn]], s[1])
  }
}

nodes <- unique(unlist(l))
g <- make_undirected_graph(NULL)
g <- add_vertices(g, length(nodes))

for (n in nodes) {
  nbs <- l[[n]]
  for (nb in nbs) {
    g <- add_edges(g, c(which(nodes == n), which(nodes == nb)))
  }
}

cuts <- min_cut(g, value.only = FALSE)
message("Part 1: ", length(cuts$partition1) * length(cuts$partition2))
