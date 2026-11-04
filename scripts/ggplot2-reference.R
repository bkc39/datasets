args <- commandArgs(TRUE)
data <- args[1]; out <- args[2]
hex <- function(s) paste(charToRaw(enc2utf8(s)), collapse='')
tokens <- function(x) {
  kind <- class(x)[1]
  values <- switch(kind,
    integer = sprintf('i%d', x),
    numeric = sprintf('f%a', x),
    Date = paste0('s', vapply(format(x, '%Y-%m-%d'), hex, '', USE.NAMES=FALSE)),
    factor = , ordered = , character = paste0('s', vapply(as.character(x), hex, '', USE.NAMES=FALSE)),
    stop('unsupported column class ', kind))
  replace(values, is.na(x), 'm')
}
for (id in args[-(1:2)]) {
  object <- chartr('-', '_', id)
  e <- new.env()
  load(file.path(data, paste0(object, '.rda')), envir=e)
  d <- get(object, envir=e)
  writeLines(vapply(names(d), function(n) paste(c(n, tokens(d[[n]])), collapse='\t'), '', USE.NAMES=FALSE),
             file.path(out, paste0(id, '.cells')))
}
