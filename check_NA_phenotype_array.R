require(oro.nifti)

.libPaths(c(.libPaths(), "/Library/Frameworks/R.framework/Versions/3.2/Resources/library"))

demog <- '/Users/yeunkim/Data/Yeun/Simulated_Data/simulated_data_with_NA2.csv'
BS_dir <- '/Applications/BrainSuite16a1_0914/svreg/BrainSuiteAtlas1/'


demog_df <- read.csv(demog)
demog_df <- subset(demog_df, select=c('Sex', 'Score', 'File_tbm'))
demog_df <- na.omit(demog_df)

lthalamus <- oro.nifti::readNIfTI(paste(BS_dir, '/mri.label.nii.gz', sep=''))
lthalamus.index <- which(lthalamus == 641)

fileid <- demog_df$File_tbm 

nload <- function(fid){
  cat(sprintf("%s \n", as.character(fid)))
  img <- oro.nifti::readNIfTI(as.character(fid))
  roi <- t((as.vector(img@.Data[lthalamus.index])))
  return(roi)
  rm(img)
}

cat("loading images... \n")
#load serially
imglist <- lapply(fileid, function(x) nload(x))

# reformat
imgmat <- data.frame(do.call('rbind', imglist))

range(imgmat)
mean(unlist(imgmat))
