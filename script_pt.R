#!/usr/bin/Rscript

library(powtran)

##################################
MARKER_LENGTH = 8
OUTFILE_EXT = ".pdf"
adjust_param=1
##################################

read.from.zip <-function(zipfilename,skip=1,n=NA){
  if(!file.exists(zipfilename)){
    stop("File ",zipfilename,"does not exists!");
  }
  files = subset( unzip(zipfilename,list=TRUE),Length>0)
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
  samples = read.csv(conn, header=FALSE, col.names= c("It", "Pall", "Papp"))
  close(conn)
  return(samples)
}

########## FIXTURES ############
period = 1

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Missing dirname.", call.=FALSE)
}

dirname = args[1]

filename_plain = paste(dirname, "/out.csv", sep = "")
filename_zip = paste(dirname, "/out.zip", sep = "")
outfile_app = paste(dirname, "/pt_app", OUTFILE_EXT, sep = "")
outfile_all = paste(dirname, "/pt_all", OUTFILE_EXT, sep = "")

##################################



if(file.exists(filename_zip)){
  samples = read.from.zip(filename_zip)
} else {
  if(file.exists(filename_plain)){
    samples = read.csv(filename_plain, header=FALSE, col.names= c("It", "Pall", "Papp"))
  } else {
    print("ERROR FILE NOT FOUND")
    q()
  }
}

#samples/1000

res = extract.power(samples$Pall / 1000, period, marker.length=MARKER_LENGTH, adjust=adjust_param)
summary(res)
pdf(file=outfile_all)
plot(res)
dev.off()

res = extract.power(samples$Papp / 1000, period, marker.length=MARKER_LENGTH, adjust=adjust_param)
summary(res)
pdf(file=outfile_app)
plot(res)
dev.off()

#extract.power(samples/1000, period, marker.length=7, include.rawdata=TRUE)
# res = extract.power(samples/1000, period, marker.length=7, include.rawdata=TRUE)
# #res
# plot(res)
# summary(res)