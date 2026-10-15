args <- commandArgs(TRUE)
root <- args[1]; out <- args[2]
package <- if (length(args) > 2) args[3] else 'datasets-core'
options(digits=17)
precise <- function(d) {
  d[] <- lapply(d, function(x) if (is.numeric(x)) ifelse(is.na(x), NA, sprintf('%.17g',x)) else x)
  d
}
export <- function(d, id) {
  d <- as.data.frame(d)
  d[] <- lapply(d, function(x) if (inherits(x, 'Date')) format(x) else x)
  text <- vapply(d, function(x) is.character(x) || is.factor(x), TRUE)
  stopifnot(!any(unlist(lapply(d[text], function(x) as.character(x) == 'NA')), na.rm=TRUE))
  write.table(precise(d),file.path(out,paste0(id,'.csv')),sep=',',row.names=FALSE,na='NA',qmethod='double')
  factors <- names(d)[vapply(d, is.factor, TRUE)]
  levels <- do.call(rbind, c(list(data.frame(column=character(), level=character())),
                             lapply(factors, function(n) data.frame(column=n, level=levels(d[[n]])))))
  write.table(levels,file.path(out,paste0(id,'.levels')),sep=',',row.names=FALSE,qmethod='double')
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
  write.table(precise(d),file.path(out,paste0(id,'.csv')),sep=',',row.names=FALSE,na='NA',qmethod='double')
}
load(file.path(root,'lars/data/diabetes.RData'))
d <- as.data.frame(diabetes$x)
d$response <- diabetes$y
write.table(precise(d),file.path(out,'diabetes.csv'),sep=',',row.names=FALSE,qmethod='double')
