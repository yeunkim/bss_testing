args <- commandArgs(trailingOnly=TRUE)

args <- grep('--args', unlist(strsplit(args, ' ')), value = TRUE, invert = TRUE)

if(length(args)==0){
  cat("No arguments supplied. Positional arguments: \n
      [obs] : full path to observed t values file \n
      [perm] : full path to null distribution file \n"
  )
  quit()
}else{
  for(i in 1:length(args)){
    #eval(parse(text=args[[i]]))
  }
}

print(args)

t_obs_file <- args[1]
t_null_file <- args[2]

t_obs <- read.csv(as.character(t_obs_file), header = F)
tvalues <- as.vector(t_obs$V1)
t_null <- read.csv(as.character(t_null_file), header = F)
t_null <- as.vector(t_null$V1)

cat("calculating p values...\n")
pvals <- c()
for(i in 1:length(tvalues)){
  pvalue <- (sum(abs(t_null) >= abs(tvalues[i])) +1) / (length(t_null) +1)
  pvals <- append(pvals, pvalue)
}


cat("counting number of significant p values... \n")
numofsigvox <- sum( pvals < 0.05)
cat(sprintf("number of significant voxels: %s \n", numofsigvox))
