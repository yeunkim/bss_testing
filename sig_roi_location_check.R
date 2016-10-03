args <- commandArgs(trailingOnly=TRUE)

args <- grep('--args', unlist(strsplit(args, ' ')), value = TRUE, invert = TRUE)

if(length(args)==0){
  cat("No arguments supplied. Positional arguments: \n
      [roiid] : ROI ID number(s) \n
      [tobsR] : full path to observed t-value file from R \n
      [tnull] : full path to observed null distribution file from R \n
      [sample nii] : sample nifti file \n
      [BS dir]: BS directory \n
      [sig roi file] : significant roi file \n
      [python nifti output] : full path to python nifti output file that contains only significant t-values"
  )
  quit()
}else{
  print(args)
}

roiid <- as.numeric(unlist(strsplit(args[1], ",")))
tobsR <- args[2]
t_null <- args[3]
samplenii <- args[4]
BS_dir <- args[5]
sig_roi <- args[6]
t_output <- args[7]

require(oro.nifti)

roi <- oro.nifti::readNIfTI(paste(BS_dir, '/svreg/BrainSuiteAtlas1/mri.label.nii.gz', sep=''))
roi.index <- which(roi %in% roiid, arr.ind = T)

tvalues <- read.csv(tobsR, header = F)
tvalues <- as.vector(tvalues$V1)
t_null <- read.csv(t_null, header = F)

pvals <- c()
for(i in 1:length(tvalues)){
  pvalue <- (sum(abs(as.vector(t_null$V1)) >= abs(tvalues[i])) +1) / (length(t_null$V1) +1)
  pvals <- append(pvals, pvalue)
}

cat('loading nifti files...\n')
samplenii <- oro.nifti::readNIfTI('1237.svreg.inv.map.jacdet.smooth2.5mm.nii.gz')
brainmask <- oro.nifti::readNIfTI(paste(BS_dir, '/svreg/BrainSuiteAtlas1/mri.mask.nii.gz', sep=""))
brainmask.idx <- which(brainmask == 255, arr.ind = T)
nonbrainmask.idx <- which(brainmask != 255, arr.ind = T)
sig_roi_label <- oro.nifti::readNIfTI(sig_roi)
sig_roi_label.idx <- which(sig_roi_label == 1, arr.ind = T)
sig_roi_concat <- as.vector(as.numeric(paste(sig_roi_label.idx[,1], sig_roi_label.idx[,2], sig_roi_label.idx[,3], sep="")))

samplenii@.Data[brainmask.idx] <- 10
samplenii@.Data[nonbrainmask.idx] <- 10

cat('checking significant voxel location in R... \n')
sampleniiR <- samplenii
sampleniiR@.Data[roi.index] <- pvals
locs <- which(sampleniiR < 0.05, arr.ind = T)

R <- as.vector(as.numeric(paste(locs[,1], locs[,2], locs[,3], sep="")))
bools <- sig_roi_concat %in% R
idx <- which(bools == F)
cat('differences between sig_roi_file location and analysis test in R is : \n')
length(idx)
rm(sampleniiR)

cat('\nchecking significant voxel location in python... \n')
toutput <- oro.nifti::readNIfTI(t_output)
locs2 <- which( abs(toutput) > 0 , arr.ind = T)

py <- as.vector(as.numeric(paste(locs2[,1], locs2[,2], locs2[,3], sep="")))
bools <- sig_roi_concat %in% py
idx <- which(bools == F)
cat('differences between sig_roi_file location and analysis test in python is : \n')
length(idx)

