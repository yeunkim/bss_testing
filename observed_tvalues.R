args <- commandArgs(trailingOnly=TRUE)

args <- grep('--args', unlist(strsplit(args, ' ')), value = TRUE, invert = TRUE)

if(length(args)==0){
  cat("No arguments supplied. Positional arguments: \n
      [dir] : directory where atlas files are located \n
      [model] : model \n
      [effect] : independent variable of interest \n
      [demog] : csv file with demographics and paths to necessary files \n
      [roiid] : ROI ID number(s) \n"
      )
  quit()
}else{
  for(i in 1:length(args)){
    #eval(parse(text=args[[i]]))
  }
}

print(args)

#.libPaths( c( .libPaths(), "/Library/Frameworks/R.framework/Versions/3.2/Resources/library"))
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
roiid <- as.numeric(unlist(strsplit(args[5], ",")))

# required columns
vars <- strsplit(model, "\\+|\\*")

#subset demographics and drop na
demog_df <- read.csv(demog)
# demog_df <- subset(demog_df, select=c(vars[[1]], 'File_tbm'))
# demog_df <- na.omit(demog_df)


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


mfit <- function(model, df, voxel, term){
  tvalue <- summary(lm(as.formula(sprintf('%s ~ %s', voxel, model)), data = df))$coefficients[term,'t value']
  se <- summary(lm(as.formula(sprintf('%s ~ %s', voxel, model)), data = df))$coefficients[term,'Std. Error']
  beta <- summary(lm(as.formula(sprintf('%s ~ %s', voxel, model)), data = df))$coefficients[term,'Estimate']
  coefs <- c(tvalue, se, beta)
  return(coefs)
}

# lapply over voxels
tvalues <- foreach(y=1:length(label.idx), .export=c('dat', 'model', 'effect', 'vars', 'vnames')) %dopar% {
  datreg <- dat[, c(vars[[1]], vnames[y])]
  t <- mfit(model, datreg, vnames[y], effect)
  t_order <- c(t, y)
  return(t_order)
}

totable <- data.frame(do.call('rbind',(tvalues)))
totable <- totable[ order(totable$X4), ]
coefs <- subset(totable, select = c('X1', 'X2', 'X3'))

if(grepl('*', model)){
  modelname <- paste(vars[[1]], collapse = '-')
} else {modelname <- paste(vars[[1]], collapse = '_')}

# write.table((tvalues), file=sprintf("%s_observed_tvalues_R_output.csv", modelname), 
#             col.names = F, quote = F, row.names = F)

write.table((coefs), file=sprintf("%s_observed_coefs_R_output.csv", modelname), 
              col.names = F, quote = F, row.names = F, sep=",")
