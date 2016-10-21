#!/usr/bin/Rscript

library(powtran)

##################################

read.from.zip <-function(zipfilename,skip=1,n=NA){
  if(!file.exists(zipfilename)){
    stop("File ",zipfilename,"does not exists!");
  }
  files = subset(unzip(zipfilename,list=TRUE),Length>0)

  if(dim(files)[1]==1){
    n=1
  }
  if(is.na(n)){
    stop("Must select a file index inside the zip.\n",
         "Available files:\n",
         paste(seq(dim(files)[1]),":",files$Name,collapse="\n")
    )
  }
  filename =files$Name[n]
  conn = unz(zipfilename,filename)

  samples <- read.table(conn, dec=",", header=TRUE, col.names="P", skip=7)
  # close(conn)  
  return(samples)
}

########## FIXTURES ############
period = 1/1000
################################

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Missing dirname.", call.=FALSE)
}

dirname = args[1]

filename_plain = paste(dirname, "Power.txt", sep = "/")
filename_zip = paste(dirname, "Power.zip", sep = "/")
outfile = paste(dirname, "daq.pdf", sep = "/")

##################################

if(file.exists(filename_zip)){
  print("ZIP")
  samples = read.from.zip(filename_zip)
} else {
  if(file.exists(filename_plain)){
    print("PLAIN")
    samples = read.table(filename_plain, dec=",", header=TRUE, col.names="P", skip=7)
  } else {
    print("ERROR FILE NOT FOUND")
    print
    q()
  }
}
res = extract.power(samples, period, marker.length=8)

summary(res)

pdf(file=outfile)
plot(res)
dev.off()

#samples = read.from.zip(filename_zip)
# samples = read.table(filename, dec=".", header=TRUE, col.names="P", skip=0)
# samples/1000

# res = extract.power(samples/1000, period, marker.length=7, include.rawdata=TRUE)
# #res
# plot(res)
# summary(res)