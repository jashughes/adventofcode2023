instr <- strsplit(readLines("15.txt", warn = FALSE), ",")[[1]] 

hash <- function(s) {
  v <- strsplit(s, "")[[1]] 
  tot <- 0
  for (letter in v) tot <- ((tot + utf8ToInt(letter)) * 17) %% 256
  tot
}
score <- function(l) vapply(1:length(l), \(x) x*sum(1:length(l[[x]])*l[[x]]), 1)

shuffle <- function(instr) {
  boxes <- lapply(1:256, \(x) NULL)
  for (i in instr) {
    label <- gsub("(=|-).*", "", i)
    pos <- hash(label) + 1
    focus <- as.numeric(gsub(".*(-|=)", "", i))
    if (grepl("=", i)) {
      if (label %in% names(boxes[[pos]])) {
        boxes[[pos]][[label]] <- focus
      } else {
        boxes[[pos]] <- c(boxes[[pos]], setNames(c(focus), label))
      }
    } else if (label %in% names(boxes[[pos]])) {
      boxes[[pos]] <- boxes[[pos]][setdiff(names(boxes[[pos]]), label)]
    }
  }
  boxes
}

message("Part 1: ", sum(vapply(instr, hash, numeric(1))))
message("Part 2: ", sum(score(shuffle(instr))))
