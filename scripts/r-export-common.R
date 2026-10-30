args <- commandArgs(TRUE)
root <- args[1]; out <- args[2]
options(digits=17)
precise <- function(d) {
  d[] <- lapply(d, function(x) if (is.numeric(x)) ifelse(is.na(x), NA, sprintf('%.17g', x)) else x)
  d
}
rendered <- function(x) {
  if (inherits(x, 'Date')) return(format(x))
  if (inherits(x, 'POSIXct')) return(format(x, '%Y-%m-%dT%H:%M:%SZ', tz='UTC'))
  x
}
write.csv.file <- function(d, file)
  write.table(d, file.path(out, file), sep=',', row.names=FALSE, qmethod='double')
export <- function(d, id) {
  d <- as.data.frame(d)
  d[] <- lapply(d, rendered)
  text <- vapply(d, function(x) is.character(x) || is.factor(x), TRUE)
  stopifnot(!any(unlist(lapply(d[text], function(x) as.character(x) == 'NA')), na.rm=TRUE))
  write.csv.file(precise(d), paste0(id, '.csv'))
  factors <- names(d)[vapply(d, is.factor, TRUE)]
  levels <- do.call(rbind, c(list(data.frame(column=character(), level=character())),
                             lapply(factors, function(n) data.frame(column=n, level=levels(d[[n]])))))
  write.csv.file(levels, paste0(id, '.levels'))
  write.csv.file(data.frame(column=names(d), type=vapply(d, function(x) if (is.factor(x)) 'factor' else typeof(x), '')),
                 paste0(id, '.types'))
}
load.RData <- function(dir, files=list.files(dir)) {
  e <- new.env()
  for (f in files) load(file.path(dir, f), envir=e)
  e
}
train.test <- function(n.train, n.test) factor(rep(c('train','test'), c(n.train, n.test)), levels=c('train','test'))
