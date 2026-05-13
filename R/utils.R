new_id <- function(prefix = "") {
  rand <- paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = "")
  paste0(prefix, format(Sys.time(), "%Y%m%d%H%M%S"), "_", rand)
}

now_utc <- function() {
  format(Sys.time(), tz = "UTC", usetz = FALSE)
}

# Serialize an R function to character (for JSON storage)
fn_to_char <- function(fn) {
  if (is.null(fn)) return(NULL)
  paste(deparse(fn, width.cutoff = 500L), collapse = "\n")
}

# Restore an R function from character
char_to_fn <- function(src) {
  if (is.null(src) || identical(src, "NULL")) return(NULL)
  eval(parse(text = src), envir = baseenv())
}

stop_mac <- function(...) rlang::abort(paste0(...), call = NULL)
