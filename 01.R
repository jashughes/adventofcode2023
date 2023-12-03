instr <- readLines("01.txt", warn = FALSE)

numbers <- c("one","two","three","four","five","six","seven","eight","nine")
part1 <- paste0("(?=(", paste0(1:9, collapse="|"), "))")
part2 <- paste0("(?=(", paste0(c(numbers,1:9), collapse="|"), "))")

find_and_sum <- function(instr, part) {
  vapply(
    instr, 
    function(x) {
      first_and_last <- find_matches(x, part)
      convert_number(first_and_last[1]) * 10 + convert_number(first_and_last[2])
    },
    numeric(1)
  ) |> sum()
}

find_matches <- function(s, part) {
  pos <- gregexpr(part, s, perl = T)[[1]]
  pos_starts <- as.vector(pos)
  pos_ends <- as.vector(attr(pos, "capture.length")) + pos_starts - 1
  first <- substr(s, pos_starts[1],  pos_ends)
  last <- substr(s, pos_starts[length(pos_starts)],  pos_ends[length(pos_ends)])
  c(first, last)
}
convert_number <- function(s) {
  sn <- suppressWarnings(as.numeric(s))
  if (!is.na(sn)) return(sn)
  which(numbers == s)
}

message("Part 1: ", find_and_sum(instr, part1))
message("Part 2: ", find_and_sum(instr, part2))
