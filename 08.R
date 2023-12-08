library(pracma)
instr <- readLines("08.txt", warn = FALSE)
dir <- c(L = 1, R = 2)[strsplit(instr[1], "")[[1]]]
get_forks <- function(s) c(gsub("\\((.*),.*","\\1",s),gsub(".*, (.*)\\)","\\1",s))
locs <- strsplit(instr[3:length(instr)], " = ")
keys <- lapply(locs, function(x) get_forks(x[2])) |>
  setNames(nm = vapply(locs, `[[`, 1, FUN.VALUE = character(1)))

find_cycles <- function(r, keys, dir) {
  starting_pos <- names(keys)[which(grepl(r, names(keys)))]
  rec <- setNames(lapply(starting_pos, function(x) NULL), starting_pos)
  for (l in names(rec)) {
    rec[[l]][["loc"]] <- l
    rec[[l]][["pos"]] <- 1
    pos <- 1
    search <- TRUE
    while(search) {
      loc <- keys[[tail(rec[[l]][["loc"]], 1)]][dir[pos]]
      if (any(rec[[l]]$loc == loc & rec[[l]]$pos == pos)) search <- FALSE
      rec[[l]]$loc <- c(rec[[l]]$loc, loc)
      rec[[l]]$pos <- c(rec[[l]]$pos, pos)
      pos <- ifelse(pos + 1 > length(dir), 1, pos + 1)
    }
  }
  rec
}

find_zzzs <- function(rec) {
  unlist(lapply(rec, function(x) min(which(grepl("ZZZ", x$loc)))))
}

find_loops <- function(rec) {
  Reduce(pracma::Lcm, vapply(
    rec,
    function(x) {
      abs(diff(which(x$loc == tail(x$loc,1) & x$pos == tail(x$pos,1) )))
    },
    FUN.VALUE = numeric(1)
  )) |> as.character()
}

message("Part 1:", find_zzzs(find_cycles("AAA", keys, dir)))
message("Part 2:", find_loops(find_cycles("A$", keys, dir)))
