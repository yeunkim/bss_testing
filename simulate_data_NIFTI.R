args <- commandArgs(trailingOnly=TRUE)

args <- grep('--args', unlist(strsplit(args, ' ')), value = TRUE, invert = TRUE)

if(length(args) < 5){
  cat("No arguments supplied. Positional arguments: \n
      [sample_nifti] : sample NIFTI file to use as template \n
      [BS_dir] : BrainSuite directory \n
      [ROI_mask] : ROI mask volume \n
      [output_dir] : output directory \n
      [datatype] : type of data (var2_roi1_interaction, var3_roi1_all, var3_roi2_nointeraction, var3_roi1_nosig) \n
      "
  )
  quit()
}else{
  print(args)
}

sample_nii <- args[1]
BS_dir <- args[2]
ROI_mask <- args[3]
odir <- args[4]
datatype <- args[5]

.libPaths(c( .libPaths(), "/Library/Frameworks/R.framework/Versions/3.2/Resources/library"))
#require(SyncRNG)
require(oro.nifti)
#require(data.frame)

# load sample file
sample <- oro.nifti::readNIfTI(sample_nii)

# load brain mask file
brainmask <- oro.nifti::readNIfTI(paste(BS_dir,'/svreg/BrainSuiteAtlas1/mri.mask.nii.gz', sep=""))
brainmask.idx <- which(brainmask == 255, arr.ind = T)
nonbrainmask.idx <- which(brainmask != 255, arr.ind = T)

#extract set sig roi
roilabel <- oro.nifti::readNIfTI(ROI_mask)
roilabel.idx <- which(roilabel == 1, arr.ind = T)
roilabel.idx2 <- which(roilabel == 2, arr.ind = T)

####### make data for 1 significant roi, testing interaction effect (group*continuous)

if(datatype == "var2_roi1_interaction"){
  dir.create(paste(odir, '/Simulated_Data_var2_roi1_interaction', sep=""))
  setwd(paste(odir, '/Simulated_Data_var2_roi1_interaction', sep=""))
  
  seed <- 22
  ### make demographics file
  set.seed(seed)
  x1 <- sort(rbinom(100, 1, 0.5))
  set.seed(seed)
  x2 <- jitter(c(seq(1,50), seq(50,1)), factor=2)
  
  id <- paste("SIMvar2_roi1_", seq(1,100), sep="")
  filepaths <- paste(odir, '/Simulated_Data_var2_roi1_interaction/SIMvar2_roi1_', seq(1,100), '/SIMvar2_roi1_', seq(1,100),
                     ".nii.gz", sep="")
  demog <- data.frame(cbind(id,x1,x2, filepaths))
  colnames(demog) <- c("subjID", "Sex", "Score", "File_tbm")
  write.table(demog, 'simulated_data.csv', sep=',', row.names=F)
  
  #initialize outcome list
  yvalues <- list()
  yvalues <- lapply(seq(1,(length(roilabel.idx)/3)), function(seed){
    
    set.seed(seed)
    e <- rnorm(100,0,5)
    b0 <- 1
    b1 <- 0.3
    b2 <- 0.4
    b3 <- -1.0
    y <- b0 + b1*x1 + b2*x2 + b3*x1*x2 + e
    return(y)
  })
  
  # reformat outcome values
  sigroi <- data.frame(do.call('cbind', yvalues))
  sigroi <- data.matrix(sigroi)
  
  # create nifti files
  voxels <- list()
  voxels <- lapply(seq(1,100), function(seed){
    
    set.seed(seed)
    values <- jitter(rnorm(length(brainmask.idx),0,5), factor=1)
    sample@.Data[brainmask.idx] <- values
    sample@.Data[nonbrainmask.idx] <- 0
    sample@.Data[roilabel.idx] <- sigroi[seed,]
    dir.create(sprintf('SIMvar2_roi1_%s', seed))
    oro.nifti::writeNIfTI(sample, sprintf('SIMvar2_roi1_%s/SIMvar2_roi1_%s', seed, seed), verbose=T)
    return(seed)
  })
}

####### make data for 1 significant roi, 3 variables, testing for interaction and main effects (group*continous, group, continous)
if(datatype == "var3_roi1_all"){
  dir.create(paste(odir, '/Simulated_Data_var3_roi1_all', sep=""))
  setwd(paste(odir, '/Simulated_Data_var3_roi1_all', sep=""))
  
  seed <- 1
  ### make demographics file
  set.seed(seed)
  x1 <- sort(rbinom(100, 1, 0.5))
  set.seed(seed)
  x2 <- jitter(c(seq(1,50), seq(50,1)), factor=2)
  set.seed(seed)
  x3 <- jitter(c(seq(101,150), rnorm(50,0,2)), factor=2)
  
  id <- paste("SIMvar3_roi1_", seq(1,100), sep="")
  id <- paste("SIMvar3_roi1_", seq(1,100), sep="")
  filepaths <- paste(odir, '/Simulated_Data_var3_roi1_all/SIMvar3_roi1_', seq(1,100), '/SIMvar3_roi1_', seq(1,100),
                     ".nii.gz", sep="")
  demog <- data.frame(cbind(id,x1,x2,x3, filepaths))
  colnames(demog) <- c("subjID", "Sex", "Score", "VLD", "File_tbm")
  write.table(demog, 'simulated_data_multi.csv', sep=',', row.names=F)
  
  #initialize outcome list
  yvalues <- list()
  yvalues <- lapply(seq(1,(length(roilabel.idx)/3)), function(seed){
    
    set.seed(seed)
    e <- rnorm(100,0,5)
    b0 <- 1
    b1 <- 0.3
    b2 <- 0.4
    b3 <- -1.0
    b4 <- 0.2
    y <- b0 + b1*x1 + b2*x2 + b3*x1*x2 + b4*x3 + e
    return(y)
  })
  
  # reformat outcome values
  sigroi <- data.frame(do.call('cbind', yvalues))
  sigroi <- data.matrix(sigroi)
  
  # create nifti files
  voxels <- list()
  voxels <- lapply(seq(1,100), function(seed){
    
    set.seed(seed)
    values <- jitter(rnorm(length(brainmask.idx),0,5), factor=1)
    sample@.Data[brainmask.idx] <- values
    sample@.Data[nonbrainmask.idx] <- 0
    sample@.Data[roilabel.idx] <- sigroi[seed,]
    dir.create(sprintf('SIMvar3_roi1_%s', seed))
    oro.nifti::writeNIfTI(sample, sprintf('SIMvar3_roi1_%s/SIMvar3_roi1_%s', seed, seed), verbose=T)
    return(seed)
  })
}

####### make data for 1 non-significant roi, 3 variables, testing for no significance
if(datatype == "var3_roi1_nosig"){
  dir.create(paste(odir, '/Simulated_Data_var3_roi1_nosig', sep=""))
  setwd(paste(odir, '/Simulated_Data_var3_roi1_nosig', sep=""))
  
  seed <- 1
  ### make demographics file
  set.seed(seed)
  x1 <- sort(rbinom(100, 1, 0.5))
  set.seed(seed)
  x2 <- jitter(c(seq(1,50), seq(50,1)), factor=2)
  set.seed(seed)
  x3 <- jitter(c(seq(101,150), rnorm(50,0,2)), factor=2)
  
  id <- paste("SIMvar3_roi1_nosig", seq(1,100), sep="")
  filepaths <- paste(odir, '/Simulated_Data_var3_roi1_nosig/SIMvar3_roi1_nosig', seq(1,100), '/SIMvar3_roi1_nosig', seq(1,100),
                     ".nii.gz", sep="")
  demog <- data.frame(cbind(id,x1,x2,x3,filepaths))
  colnames(demog) <- c("subjID", "Sex", "Score", "VLD", "File_tbm")
  write.table(demog, 'simulated_data_nosig.csv', sep=',', row.names=F)
  
  #initialize outcome list
  yvalues <- list()
  yvalues <- lapply(seq(1,(length(roilabel.idx)/3)), function(seed){
    
    set.seed(seed)
    e <- rnorm(100,0,5)
    b0 <- 1
    b1 <- 0.3
    b2 <- 0.4
    b3 <- -1.0
    b4 <- 0.2
    y <- b0 + b1*x1 + b2*x2 + b3*x1*x2 + b4*x3 + e
    return(y)
  })
  
  # reformat outcome values
  sigroi <- data.frame(do.call('cbind', yvalues))
  sigroi <- data.matrix(sigroi)
  
  # create nifti files
  voxels <- list()
  voxels <- lapply(seq(1,100), function(seed){
    
    set.seed(seed)
    values <- jitter(rnorm(length(brainmask.idx),0,5), factor=1)
    sample@.Data[brainmask.idx] <- values
    sample@.Data[nonbrainmask.idx] <- 0
    #sample@.Data[roilabel.idx] <- sigroi[seed,]
    dir.create(sprintf('SIM_var3_roi1_nosig%s', seed))
    oro.nifti::writeNIfTI(sample, sprintf('SIM_var3_roi1_nosig%s/SIM_var3_roi1_nosig%s', seed, seed), verbose=T)
    return(seed)
  })
}


####### make data for 2 significant rois, 3 variables, testing for multiple clusters
if(datatype == "var3_roi2_nointeraction"){
  dir.create(paste(odir, '/Simulated_Data_var3_roi2_nointeraction', sep=""))
  setwd(paste(odir, '/Simulated_Data_var3_roi2_nointeraction', sep=""))
  
  seed <- 1
  ### make demographics file
  set.seed(seed)
  x1 <- sort(rbinom(100, 1, 0.5))
  set.seed(seed)
  x2 <- jitter(c(seq(1,50), seq(50,1)), factor=2)
  set.seed(seed)
  x3 <- jitter(c(seq(101,150), rnorm(50,0,2)), factor=2)
  
  id <- paste("SIMvar3_roi2_", seq(1,100), sep="")
  filepaths <- paste(odir, '/Simulated_Data_var3_roi2_nointeraction/SIMvar3_roi2_', seq(1,100), '/SIMvar3_roi2_', seq(1,100),
                     ".nii.gz", sep="")
  demog <- data.frame(cbind(id,x1,x2,x3,filepaths))
  colnames(demog) <- c("subjID", "Sex", "Score", "VLD", "File_tbm")
  write.table(demog, 'simulated_data_rois.csv', sep=',', row.names=F)
  
  #initialize outcome list
  yvalues <- list()
  yvalues <- lapply(seq(1,(length(roilabel.idx)/3)), function(seed){
    
    set.seed(seed)
    e <- rnorm(100,0,5)
    b0 <- 1
    b1 <- 0.3
    b2 <- 0.4
    b3 <- -1.0
    b4 <- 0.2
    y <- b0 + b1*x1 + b2*x2 + b3*x1*x2 + b4*x3 + e
    return(y)
  })
  
  #initialize second outcome list
  yvalues2 <- list()
  yvalues2 <- lapply(seq(1,(length(roilabel.idx2)/3)), function(seed){
    
    set.seed(seed)
    e <- rnorm(100,0,5)
    b0 <- 1
    b1 <- 0.3
    b2 <- 0.4
    b3 <- 1.0
    b4 <- -0.2
    y <- b0 + b1*x1 + b2*x2 + b3*x1*x2 + b4*x3 + e
    return(y)
  })
  
  # reformat outcome values
  sigroi <- data.frame(do.call('cbind', yvalues))
  sigroi <- data.matrix(sigroi)

  # reformat second outcome values
  sigroi2 <- data.frame(do.call('cbind', yvalues2))
  sigroi2 <- data.matrix(sigroi2)
  
  # create nifti files
  voxels <- list()
  voxels <- lapply(seq(1,100), function(seed){
    
    set.seed(seed)
    values <- jitter(rnorm(length(brainmask.idx),0,5), factor=1)
    sample@.Data[brainmask.idx] <- values
    sample@.Data[nonbrainmask.idx] <- 0
    sample@.Data[roilabel.idx] <- sigroi[seed,]
    sample@.Data[roilabel.idx2] <- sigroi2[seed,]
    dir.create(sprintf('SIMvar3_roi2_%s', seed))
    oro.nifti::writeNIfTI(sample, sprintf('SIMvar3_roi2_%s/SIMvar3_roi2_%s', seed, seed), verbose=T)
    return(seed)
  })
}


# 
# 
# dat <- data.frame(cbind(x1, x2, x3, y))
# fit <- lm(y ~ x1 + x2 + x3, data = dat)
# summary(fit)
# quartz();ggplot(dat, aes(x2, y, colour=as.factor(x1))) + stat_smooth(method='lm') + geom_point()
