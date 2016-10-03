args <- commandArgs(trailingOnly=TRUE)

args <- grep('--args', unlist(strsplit(args, ' ')), value = TRUE, invert = TRUE)

if(length(args)==0){
  cat("No arguments supplied. Positional arguments: \n
      [dir] : directory where mask is located \n
      [model] : model \n
      [effect] : independent variable of interest \n
      [demog] : csv file with demographics and paths to necessary files \n
      [seed] : seed number \n
      [roiid] : ROI ID numbers \n
      [t] : print out t-values? y/n \n")
  quit()
}else{
  for(i in 1:length(args)){
    #eval(parse(text=args[[i]]))
  }
}

print(args)

.libPaths( c( .libPaths(), "/Library/Frameworks/R.framework/Versions/3.2/Resources/library"))
require(doParallel)
require(foreach)
require(SyncRNG)
require(oro.nifti)
#require(data.table)

# run in parallel
#parallel::makeCluster(detectCores())
cat("initializing parallel functions...\n")
cl<-parallel::makeCluster(detectCores())
registerDoParallel(cl)

BS_dir <- args[1]
model <- args[2]
effect <- args[3]
demog <- args[4]
seednum <- as.numeric(args[5])
roiid <- as.numeric(unlist(strsplit(args[6], ",")))
YN <- args[7]

# required columns
vars <- strsplit(model, "\\+|\\*")

#subset demographics and drop na
demog_df <- read.csv(demog)
demog_df <- subset(demog_df, select=c(vars[[1]], 'File_tbm'))
demog_df <- na.omit(demog_df)
                   

#rthalamus <- oro.nifti::readNIfTI(paste(BS_dir, '/mri.r.thalamus.mask.nii.gz', sep=''))
label <- oro.nifti::readNIfTI(paste(BS_dir, '/mri.label.nii.gz', sep=''))
label.idx <- which(label %in% roiid)

## acquire paths to files
fileid <- demog_df$File_tbm    
#print(fileid)
                                  
## function for loading nii
nload <- function(fid){
  cat(sprintf("%s \n", as.character(fid)))
  img <- oro.nifti::readNIfTI(as.character(fid))
  roi <- t((as.vector(img@.Data[label.idx])))
  return(roi)
  rm(img)
}

cat("loading images... \n")
#load serially
imglist <- lapply(fileid, function(x) nload(x))

# reformat
imgmat <- data.frame(do.call('rbind', imglist))
vnames <- colnames(imgmat)

#form one data.frame
dat <- cbind(demog_df[, vars[[1]]], imgmat)

# initialize tmax null distribution list
tvalues <- list()

# assign data frame to be shuffled
datshuff <- dat

# set seed
s <- SyncRNG(seed=seednum)

#shuffle data
if(grepl(":", effect)){
  shuffvars <- strsplit(effect, ":")
  s <- SyncRNG(seed=seednum)
  datshuff[[shuffvars[[1]][1]]] <- s$shuffle(dat[[shuffvars[[1]][1]]])
  s <- SyncRNG(seed=seednum+1)
  datshuff[[shuffvars[[1]][2]]] <- s$shuffle(dat[[shuffvars[[1]][2]]])
}else {datshuff[[effect]] <- s$shuffle(datshuff[[effect]])}

# function for linear regression and acquiring t statistics

mfit <- function(model, df, voxel, term){
  tvalue <- summary(lm(as.formula(sprintf('%s ~ %s', voxel, model)), data = df))$coefficients[term,'t value']
  se <- summary(lm(as.formula(sprintf('%s ~ %s', voxel, model)), data = df))$coefficients[term,'Std. Error']
  beta <- summary(lm(as.formula(sprintf('%s ~ %s', voxel, model)), data = df))$coefficients[term,'Estimate']
  coefs <- c(tvalue, se, beta)
  return(coefs)
}

# lapply over voxels
tvalues <- foreach(y=1:length(label.idx), .export=c('datshuff', 'model', 'effect', 'vars', 'vnames')) %dopar% {
  datreg <- datshuff[, c(vars[[1]], vnames[y])]
  t <- mfit(model, datreg, vnames[y], effect)
}

totable <- data.frame(do.call('rbind',(tvalues)))
tmax <- max((totable$X1))
print(sprintf("maximum t-value: %s", tmax))

if(grepl('*', model)){
  modelname <- paste(vars[[1]], collapse = '-')
} else {modelname <- paste(vars[[1]], collapse = '_')}

if(YN == "y"){
  write.table(totable, file=sprintf("%s_perm_coefs_R_output_seed%s.csv", modelname,args[5]), 
              col.names = F, quote = F, row.names = F, sep=",")
}