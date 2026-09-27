args <- commandArgs(TRUE)
root <- args[1]; out <- args[2]
options(digits=17)
precise <- function(d) {
  d[] <- lapply(d, function(x) if (is.numeric(x)) ifelse(is.na(x), NA, sprintf('%.17g',x)) else x)
  d
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
