source(file.path(dirname(sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value=TRUE))), 'r-export-common.R'))
package <- if (length(args) > 2) args[3] else 'datasets-core'
nycflights13 <- function(tables) {
  for (table in tables) {
    e <- new.env()
    load(file.path(root,'nycflights13/data',paste0(table,'.rda')),envir=e)
    export(get(table,envir=e), paste0('nycflights13-',table))
  }
}
if (package == 'nycflights13') {
  nycflights13(c('airlines','airports','flights','planes','weather'))
  quit(save='no')
}
if (package == 'datasets-islr') {
  islr <- new.env()
  for (f in list.files(file.path(root,'ISLR2/data'),full.names=TRUE)) load(f,envir=islr)
  with(islr, {
    stopifnot(!anyDuplicated(rownames(College)), !anyDuplicated(rownames(Hitters)))
    sorted.factor <- function(x) factor(x, levels=sort(unique(x), method='radix'))
    genes <- function(x) { colnames(x) <- seq_len(ncol(x)); as.data.frame(x, check.names=FALSE) }
    sets <- list(
      auto=Auto, bikeshare=Bikeshare, boston=Boston, 'brain-cancer'=BrainCancer, caravan=Caravan,
      carseats=Carseats, college=cbind(name=rownames(College), College), credit=Credit,
      default=Default, fund=Fund, hitters=cbind(player=rownames(Hitters), Hitters),
      khan=cbind(split=factor(rep(c('train','test'), c(nrow(Khan$xtrain), nrow(Khan$xtest))),
                              levels=c('train','test')),
                 y=c(Khan$ytrain, Khan$ytest), genes(rbind(Khan$xtrain, Khan$xtest))),
      nci60=cbind(label=sorted.factor(NCI60$labs), genes(NCI60$data)),
      nyse=transform(NYSE, day_of_week=factor(day_of_week, levels=c('mon','tues','wed','thur','fri')),
                     train=as.integer(train)),
      oj=OJ, portfolio=Portfolio, publication=transform(Publication, mech=sorted.factor(mech)),
      smarket=Smarket, wage=Wage, weekly=Weekly)
    for (id in names(sets)) export(sets[[id]], id)
  })
  esl <- new.env()
  for (f in list.files(file.path(root,'ElemStatLearn/data'),pattern='[.]RData$',full.names=TRUE)) load(f,envir=esl)
  with(esl, {
    train.test <- function(n.train, n.test) factor(rep(c('train','test'), c(n.train, n.test)), levels=c('train','test'))
    prostate$train <- factor(ifelse(prostate$train, 'train', 'test'), levels=c('train','test'))
    names(prostate)[names(prostate) == 'train'] <- 'split'
    export(prostate, 'prostate')
    export(SAheart, 'sa-heart')
    export(cbind(rbind(vowel.train, vowel.test), split=train.test(nrow(vowel.train), nrow(vowel.test))), 'vowel')
    export(ozone, 'ozone')
    export(spam, 'esl-spam')
  })
  quit(save='no')
}
if (package == 'esl-zip') {
  esl <- new.env()
  for (f in c('zip.train','zip.test')) load(file.path(root,'ElemStatLearn/data',paste0(f,'.RData')),envir=esl)
  book <- lapply(c('zip.train','zip.test'), function(f) as.matrix(read.table(gzfile(file.path(args[4],paste0(f,'.gz'))))))
  stopifnot(all(book[[1]] == esl$zip.train), all(book[[2]] == esl$zip.test))
  pixels <- rbind(book[[1]], book[[2]])
  stopifnot(all(pixels[,1] == round(pixels[,1])))
  d <- data.frame(digit=as.integer(pixels[,1]), pixels[,-1])
  names(d) <- c('digit', paste0('pixel', seq_len(ncol(pixels)-1)))
  d$split <- factor(rep(c('train','test'), c(nrow(book[[1]]), nrow(book[[2]]))), levels=c('train','test'))
  export(d, 'zip')
  quit(save='no')
}
if (package == 'datasets-r') {
  load(file.path(root,'palmerpenguins/R/sysdata.rda'))
  export(penguins_df, 'penguins')
  export(penguins_raw_df, 'penguins-raw')
  load(file.path(root,'gapminder/data/gapminder.rdata'))
  export(gapminder, 'gapminder')
  load(file.path(root,'datasauRus/data/datasaurus_dozen.rda'))
  export(datasaurus_dozen, 'datasaurus-dozen')
  load(file.path(root,'titanic/data/titanic_train.rda'))
  titanic_train$Cabin[titanic_train$Cabin == ''] <- NA
  titanic_train$Embarked[titanic_train$Embarked == ''] <- NA
  export(titanic_train, 'titanic-passengers')
  nycflights13(c('airlines','airports','planes','weather'))
  quit(save='no')
}
names <- c(mtcars='mtcars', faithful='faithful', anscombe='anscombe', airquality='airquality', 'us-arrests'='USArrests', 'plant-growth'='PlantGrowth', 'tooth-growth'='ToothGrowth', titanic='Titanic', 'air-passengers'='AirPassengers')
for (id in names(names)) {
  original <- names[[id]]
  path <- file.path(root, 'R-4.3.3/src/library/datasets/data', paste0(original,'.R'))
  if (file.exists(path)) { source(path); d <- get(original) } else { d <- read.table(sub('\\.R$', '.tab',path),header=TRUE) }
  if (id == 'titanic') d <- as.data.frame(d)
  if (id == 'air-passengers') d <- data.frame(year=rep(1949:1960,each=12), month=rep(1:12,12), passengers=as.numeric(d))
  if (id == 'mtcars') d <- cbind(model=rownames(d),d)
  if (id == 'us-arrests') d <- cbind(state=rownames(d),d)
  export(d, id)
}
load(file.path(root,'lars/data/diabetes.RData'))
d <- as.data.frame(diabetes$x)
d$response <- diabetes$y
export(d, 'diabetes')
