instr <- strsplit(readLines("15.txt", warn = FALSE), ",")[[1]] 

hash <- function(s) {
  v <- strsplit(s, "")[[1]] 
  tot <- 0
  for (letter in v) {
    tot <- ((tot + utf8ToInt(letter)) * 17) %% 256
  }
  tot
}

hash("HASH")

vapply(instr, hash, 1) |> sum()

# t2

boxes <- lapply(1:256, \(x) NULL)
#instr <- strsplit("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7", ",")[[1]]

for (x in 1:length(instr)) {
  i <- instr[x]
  label <- gsub("(=|-).*", "", i)
  pos <- hash(label) + 1
  focus <- as.numeric(gsub(".*(-|=)", "", i))
  if (grepl("=", i)) {
    if (label %in% names(boxes[[pos]])) {
      boxes[[pos]][[label]] <- focus
    } else {
      boxes[[pos]] <- c(boxes[[pos]], setNames(c(focus), label))
    }
  } else if (!label %in% names(boxes[[pos]])) {
    boxes[[pos]] <- boxes[[pos]][setdiff(names(boxes[[pos]]), label)]
  }
  if (length(boxes) < 256) print(x)
}

l <- boxes

score <- function(l) {
  vapply(1:length(l), \(x) {
    x * sum(1:length(l[[x]]) * l[[x]])
  }, numeric(1)) |> sum()
}
