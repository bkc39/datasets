args <- commandArgs(TRUE)
root <- args[1]; out <- args[2]
data <- file.path(root, 'R-4.3.3/src/library/datasets/data')
options(digits=17)

r <- new.env()
for (f in list.files(data)) {
  p <- file.path(data, f)
  if (grepl('\\.R$', f)) sys.source(p, envir=r)
  else if (grepl('\\.rda$', f)) load(p, envir=r)
  else assign(sub('\\.tab$', '', f), read.table(p, header=TRUE), envir=r)
}

core <- c(AirPassengers='air-passengers', airquality='airquality', anscombe='anscombe',
          faithful='faithful', iris='iris', mtcars='mtcars', PlantGrowth='plant-growth',
          Titanic='titanic', ToothGrowth='tooth-growth', USArrests='us-arrests')
skipped <- c(
  iris3='The iris measurements (datasets-core iris) as a 50 x 4 x 3 array.',
  freeny.x='The four regressor columns of freeny.',
  freeny.y='The y column of freeny.',
  stack.x='The three regressor columns of stackloss.',
  stack.loss='The stack-loss column of stackloss.',
  euro.cross='Derived from euro: euro.cross[i, j] is euro[j] / euro[i].')
with(as.list(r), stopifnot(
  all(vapply(seq_along(levels(iris$Species)), function(k)
    all(iris3[, , k] == as.matrix(iris[iris$Species == levels(iris$Species)[k], 1:4])), TRUE)),
  all(freeny.x == as.matrix(freeny[, 2:5])), all(freeny.y == freeny$y),
  all(stack.x == as.matrix(stackloss[, 1:3])), all(stack.loss == stackloss$stack.loss),
  isTRUE(all.equal(euro.cross, outer(1 / euro, euro), check.attributes=FALSE))))
folds <- list(
  state=c('state.name', 'state.abb', 'state.region', 'state.division', 'state.area',
          'state.center', 'state.x77'),
  'bj-sales'=c(sales='BJsales', lead='BJsales.lead'),
  'uk-lung-deaths'=c(ldeaths='ldeaths', mdeaths='mdeaths', fdeaths='fdeaths'))
renamed <- c(BJsales='bj-sales', CO2='co2-uptake', DNase='dnase', UKgas='uk-gas',
             UScitiesD='us-cities-d', WWWusage='www-usage')
row.ids <- c(LifeCycleSavings='country', swiss='province', USJudgeRatings='judge',
             VADeaths='age-group', WorldPhones='year', USPersonalExpenditure='category',
             ability.cov='variable', Harman23.cor='variable', Harman74.cor='variable')
dim.names <- list(crimtab=c('finger-length', 'height'))

kebab <- function(s) {
  s <- gsub('([a-z0-9])([A-Z])', '\\1-\\2', s)
  s <- gsub('([A-Z])(?=[A-Z][a-z])', '\\1-', s, perl=TRUE)
  gsub('^-+|-+$', '', gsub('[^a-z0-9]+', '-', tolower(s)))
}
dataset.id <- function(n) if (n %in% names(renamed)) renamed[[n]] else kebab(n)
numeric.labels <- function(x) !anyNA(suppressWarnings(as.numeric(x)))
plain <- function(x) if (is.factor(x)) x else as.vector(x)

ts.index <- function(x) {
  f <- frequency(x); tm <- as.vector(time(x)); year <- floor(tm + 1e-8)
  cycle <- as.integer(round((tm - year) * f)) + 1L
  index <- if (f == 1 && start(x)[1] == 1) list(time=as.integer(tm))
           else if (f <= 1) list(year=as.integer(year))
           else setNames(list(as.integer(year), cycle),
                         c('year', c('4'='quarter', '12'='month', '260'='business-day')[[as.character(f)]]))
  description <- c(time='Time point', year='Calendar year', quarter='Quarter, 1-4',
                   month='Calendar month, January=1', 'business-day'='Business day of the year')
  n <- length(index)
  list(frame=data.frame(index, check.names=FALSE), original=names(index),
       description=unname(description[names(index)]),
       series=list(frequency=f, start=head(start(x), n), end=head(end(x), n), index=names(index)))
}

shape <- function(n, x) {
  if (is.data.frame(x)) {
    rn <- attr(x, 'row.names')
    series <- Filter(function(c) is.ts(x[[c]]), names(x))
    d <- data.frame(lapply(x, plain), check.names=FALSE)
    names(d) <- kebab(names(x))
    keep <- !is.integer(rn) && !numeric.labels(rn)
    note <- c(if (inherits(x, 'groupedData')) 'nlme groupedData attributes dropped.',
              if (keep) sprintf('Row names kept as the %s column.', row.ids[[n]])
              else if (!is.integer(rn)) 'Numeric row names dropped.')
    s <- list(frame=d, original=names(x), description=names(x), identifiers=character(),
              notes=paste(c('Data frame.', note), collapse=' '))
    if (keep) s <- prepend(s, setNames(data.frame(rownames(x)), row.ids[[n]]), 'row names', TRUE)
    if (length(series)) s <- with.index(s, ts.index(x[[series]]))
    s
  } else if (is.ts(x)) {
    m <- as.matrix(x)
    d <- if (is.null(colnames(x))) data.frame(value=as.vector(x))
         else setNames(as.data.frame(unclass(m)), kebab(colnames(x)))
    s <- list(frame=d, original=if (is.null(colnames(x))) n else colnames(x),
              description=if (is.null(colnames(x))) n else colnames(x), identifiers=character(),
              notes=sprintf('Time series of frequency %s; index columns from the ts time.',
                            format(frequency(x))))
    with.index(s, ts.index(x))
  } else if (inherits(x, 'dist')) {
    lab <- attr(x, 'Labels'); ij <- which(lower.tri(matrix(0, attr(x, 'Size'), attr(x, 'Size'))), arr.ind=TRUE)
    list(frame=data.frame(from=lab[ij[, 2]], to=lab[ij[, 1]], distance=as.vector(x)),
         original=c('labels', 'labels', n), description=c('First city', 'Second city', 'Distance'),
         identifiers=character(),
         notes='Distance matrix as one row per pair (lower triangle) in R dist order.')
  } else if (is.table(x)) {
    names(dimnames(x)) <- if (is.null(dim.names[[n]])) names(dimnames(x)) else dim.names[[n]]
    d <- as.data.frame(x, responseName='count', stringsAsFactors=TRUE)
    list(frame=setNames(d, kebab(names(d))), original=names(d), description=names(d),
         identifiers=character(),
         notes='Contingency table in long form: one row per cell in R array order, zero cells kept.')
  } else if (n == 'volcano') {
    list(frame=data.frame(row=as.vector(row(x)), column=as.vector(col(x)), value=as.vector(x)),
         original=c('row', 'column', n), description=c('Grid row', 'Grid column', 'Height'),
         identifiers=character(), notes='Grid in long form: one row per cell, column-major.')
  } else if (is.matrix(x) || is.list(x) && !is.null(x$cov)) {
    m <- if (is.matrix(x)) x else x$cov
    stopifnot(is.matrix(x) || all(x$center == 0))
    notes <- if (is.matrix(x)) 'Matrix.'
             else sprintf(paste('Covariance list: the cov matrix, from %d observations; center is all zero;',
                                'variable names kebab-cased like the columns.'), as.integer(x$n.obs))
    s <- list(frame=setNames(as.data.frame(unclass(m)), kebab(colnames(m))), original=colnames(m),
              description=colnames(m), identifiers=character(), notes=notes)
    ids <- if (is.matrix(x)) rownames(m) else kebab(rownames(m))
    prepend(s, setNames(data.frame(if (numeric.labels(ids)) as.integer(ids) else ids), row.ids[[n]]),
            'row names', !numeric.labels(ids))
  } else if (is.numeric(x) && !is.null(names(x))) {
    list(frame=data.frame(name=names(x), value=unname(x)), original=c('names', n),
         description=c('Name', n), identifiers='name', notes='Named vector as name and value columns.')
  } else {
    list(frame=data.frame(value=x), original=n, description=n, identifiers=character(),
         notes='Vector as a value column.')
  }
}
prepend <- function(s, column, original, identifier) {
  s$frame <- cbind(column, s$frame)
  s$original <- c(original, s$original)
  s$description <- c(original, s$description)
  if (identifier) s$identifiers <- c(names(column), s$identifiers)
  s
}
with.index <- function(s, index) {
  s$frame <- cbind(index$frame, s$frame)
  s$original <- c(index$original, s$original)
  s$description <- c(index$description, s$description)
  s$series <- index$series
  s
}

fold <- function(id, members) {
  if (id == 'state') {
    x77 <- r$state.x77
    stopifnot(identical(rownames(x77), r$state.name))
    cols <- kebab(colnames(x77)); cols[cols == 'area'] <- 'land-area'
    frame <- cbind(data.frame(name=r$state.name, abb=r$state.abb, region=r$state.region,
                              division=r$state.division, area=r$state.area,
                              'center-x'=r$state.center$x, 'center-y'=r$state.center$y,
                              check.names=FALSE),
                   setNames(as.data.frame(unclass(x77)), cols))
    original <- c(members[1:5], 'state.center$x', 'state.center$y',
                  paste0('state.x77[, "', colnames(x77), '"]'))
    list(frame=frame, original=original, description=original, identifiers='name',
         notes='One row per state, joining the state.* vectors and the columns of state.x77; state.x77 Area renamed land-area.')
  } else {
    s <- shape(id, do.call(cbind, mget(members, envir=r)))
    s$frame <- setNames(s$frame, c(s$series$index, names(members)))
    s$original <- c(head(s$original, length(s$series$index)), members)
    s$description <- s$original
    s$notes <- paste(s$notes, 'Series that share one index joined as columns.')
    s
  }
}

precise <- function(d) {
  d[] <- lapply(d, function(x) if (is.numeric(x)) ifelse(is.na(x), NA, sprintf('%.17g', x)) else x)
  d
}
manifest <- list()
write.dataset <- function(id, objects, s) {
  d <- s$frame
  stopifnot(!anyDuplicated(names(d)), length(s$original) == ncol(d), length(s$description) == ncol(d))
  write.table(precise(d), file.path(out, paste0(id, '.csv')), sep=',', row.names=FALSE, na='NA',
              qmethod='double')
  factors <- names(d)[vapply(d, is.factor, TRUE)]
  levels <- do.call(rbind, c(list(data.frame(column=character(), level=character())),
                             lapply(factors, function(n) data.frame(column=n, level=levels(d[[n]])))))
  write.table(levels, file.path(out, paste0(id, '.levels')), sep=',', row.names=FALSE, qmethod='double')
  write.table(data.frame(name=names(d), original=s$original, description=s$description),
              file.path(out, paste0(id, '.columns')), sep=',', row.names=FALSE, qmethod='double')
  series <- s$series
  manifest[[id]] <<- data.frame(
    id=id, objects=paste(objects, collapse='|'), identifiers=paste(s$identifiers, collapse='|'),
    notes=s$notes, frequency=if (is.null(series)) '' else format(series$frequency),
    start=paste(series$start, collapse='|'), end=paste(series$end, collapse='|'),
    index=paste(series$index, collapse='|'))
}

objects <- ls(r)
folded <- unlist(folds, use.names=FALSE)
dispositions <- do.call(rbind, lapply(objects, function(n) {
  x <- get(n, envir=r)
  fold.id <- Find(function(id) n %in% folds[[id]], names(folds))
  data.frame(object=n, class=paste(class(x), collapse='/'),
             disposition=if (n %in% names(core)) 'core' else if (n %in% names(skipped)) 'skipped' else 'dataset',
             target=if (n %in% names(core)) core[[n]] else if (n %in% names(skipped)) skipped[[n]]
                    else if (!is.null(fold.id)) fold.id else dataset.id(n))
}))
write.table(dispositions, file.path(out, 'r-objects.csv'), sep=',', row.names=FALSE, qmethod='double')
for (n in setdiff(objects, c(names(core), names(skipped), folded)))
  write.dataset(dataset.id(n), n, shape(n, get(n, envir=r)))
for (id in names(folds)) write.dataset(id, unname(folds[[id]]), fold(id, folds[[id]]))
write.table(do.call(rbind, manifest), file.path(out, 'r-datasets.csv'), sep=',', row.names=FALSE,
            qmethod='double')
