## This script featurizes a large number of images, using the FACES dataset, on Spark

##########################################################################################
#### Environment setup

install.packages(c("devtools", "curl", "httr"));

# CRAN prerequisities for AzureSMR
install.packages(c('assertthat', 'XML', 'base64enc', 'shiny', 'miniUI', 'DT', 'lubridate'));
devtools::install_github("Microsoft/AzureSMR");

# add Microsoft goodness to path on the DSVMs
#.libPaths( c( "/data/mlserver/9.2.1/libraries/RServer", .libPaths()))
#library(RevoScaleR)
#library(MicrosoftML)


##########################################################################################
## Data locations, names, keys...

storageAccount = "storage4tomasbatch";
# for listing, rotate key after tutorial
storageKey = "WpJqUKKq+8dgOGIXNlubRVrLu6vdNArNW9sE+cAGdwss1ETSb3P9ihjcSbFBQitAMs7RX/avXtGAYRORhuhHZA=="; 
# container = "strata2018";
container = "tutorial";
BLOB_URL_BASE = paste0("https://", storageAccount, ".blob.core.windows.net/", container, '/');

CALTECH_FEATURIZED_DATA='Caltech.Rds'
KNOTS_FEATURIZED_DATA='knots.Rds'
FACES_SMALL_FEATURIZED_DATA='faces_small.Rds'


##########################################################################################
## list blob contents in a single directory and parse it into constituent parts

library(AzureSMR)   # AzureSMR is only needed on the head node - to list files in blob

get_blob_info <- function(storageAccount, storageKey, container, prefix) {
    marker = NULL;
    blob_info = NULL;
    repeat {
        info <- azureListStorageBlobs(NULL,
                                   storageAccount = storageAccount,
                                   storageKey = storageKey,
                                   container = container,
                                   marker = marker,
                                   prefix = prefix)

        if (is.null(blob_info)) {
            blob_info = info;
        } else {
            blob_info = rbind(blob_info, info)
        }

        marker <- attr(info, 'marker');
        print(paste0("Have ", nrow(blob_info), " blobs"));
        if (marker == "") {
            break
        } else {
            print("Still more blobs to get")
        }
    }
    # end blob directory read loop

    # preprocess the file names into urls and class (person) names
    blob_info$url <- paste(BLOB_URL_BASE, sep = '', blob_info$name)
    blob_info$fname <- sapply(strsplit(blob_info$name, '/'), function(l) { l[length(l)] })
    blob_info$bname <- sapply(strsplit(blob_info$fname, ".", fixed = TRUE), function(l) l[1])
    blob_info$pname <- sapply(strsplit(blob_info$fname, "_", fixed = TRUE),
                          function(l) paste(l[1:(length(l) - 1)], collapse = " "))

    return(blob_info);
}
# end get_blob_info

##########################################################################################
# Parallel kernel for featurization
parallel_kernel <- function(blob_info) {
  
  library(MicrosoftML)
  library(utils)
  
  # get the images from blob and do them locally
  DATA_DIR <- file.path(getwd(), 'localdata');
  if(!dir.exists(DATA_DIR)) dir.create(DATA_DIR);
  
  # do this in paralell, too
  for (i in 1:nrow(blob_info)) {
    targetfile <- file.path(DATA_DIR, blob_info$fname[[i]]);
    if (!file.exists(targetfile)) {
      download.file(blob_info$url[[i]], destfile = targetfile, mode="wb", quiet=TRUE)
    }
  }
  
  blob_info$localname <- paste(DATA_DIR, sep='/', blob_info$fname);
  # print(blob_info$localname)
  
  image_features <- rxFeaturize(data = blob_info,
                                mlTransforms = list(loadImage(vars = list(Image = "localname")),
                                                    resizeImage(vars = list(Features = "Image"),
                                                                width = 224, height = 224,
                                                                resizingOption = "IsoPad"),
                                                    extractPixels(vars = "Features"),
                                                    featurizeImage(var = "Features",
                                                                   dnnModel = 'resnet18')),
                                mlTransformVars = c("localname"),
                                reportProgress=1)
  
  image_features$url <- blob_info$url;
  return(image_features)
}

##########################################################################################
## Splitting the dataset for parallel run

# returns a list of data frame shards (smaller data frames), each enclosed in one-element list
#
# Parallel execution expects a list of argument vectors. Since parallel_kernel has 1 argument,
# each argument vector has length one. That is the "extra" layer of list.
#
shardDataFrame <- function(df, shardcount) {
  N <- dim(df)[1]  
  batch_size <- ceiling(N/shardcount);
  
  shards <- lapply(1:shardcount, function(i) {
    fromRow = (i-1)*batch_size+1;
    toRow = min(i*batch_size, N);  
    return(list(df[fromRow:toRow,]));
  } )
}


##########################################################################################
## Featurize the faces dataset

# Get the list of images
blob_info <- get_blob_info(storageAccount, storageKey, container, prefix = "faces_small");

# test that things work locally
start_time <- Sys.time()
output <- parallel_kernel(blob_info);
end_time <- Sys.time()
print(paste0("Local ran for ", round(as.numeric(end_time - start_time, units="secs")), " seconds"))

SLOTS=4 # parallelism level. We have 4 nodes. They have 8 cores each, but workload is multithreaded.
shards <- shardDataFrame(blob_info, SLOTS)      # create a list of dataframes, a uniform partition of blob_info

# run the featurization locally on singlecore
rxSetComputeContext(RxLocalSeq());
start_time <- Sys.time()
outputs <- rxExec(FUN=parallel_kernel, elemArgs=shards)
end_time <- Sys.time()
print(paste0("Local sequential ran for ", round(as.numeric(end_time - start_time, units="secs")), " seconds"))
## The sharding adds about 3 seconds to serial execution


# run the featurization locally on multicore
rxOptions(numCoresToUse=SLOTS);
rxSetComputeContext(RxLocalParallel());
start_time <- Sys.time()
outputs <- rxExec(FUN=parallel_kernel, elemArgs=shards)
end_time <- Sys.time()
print(paste0("Local parallel ran for ", round(as.numeric(end_time - start_time, units="secs")), " seconds"))


# Get connection to Spark cluster.
# 1. locally when running in the cluster's RStudio Server
mySparkCluster <- rxSparkConnect()
# 2. remotely
#mySparkCluster <- rxSparkConnect(sshUsername='sshuser',
#                                 sshHostname='stratbl;ob_infoa2018.azurehdinsight.net'
#                                  , private/public key pair needs to be set up 
#                                 )

# run the featurization on the cluster
rxSetComputeContext(mySparkCluster);
start_time <- Sys.time()
outputs <- rxExec(FUN=parallel_kernel, elemArgs=shards)
end_time <- Sys.time()
print(paste0("Spark ran for ", round(as.numeric(end_time - start_time, units="secs")), " seconds"))


# the output is a list of length SLOTS, collect back into one dataframe
faces_small_df <- Reduce(rbind, outputs)


#####################################################################################
# makes sense?

library(tidyr)
library(ggplot2)
library(magrittr)
features <- single_df %>% gather(featname, featval, -bname, -pname)        # plot features by file
plottable <- features[startsWith(features$featname, 'Feature'),];
plottable$featval <- type.convert(plottable$featval);                       # make numeric again

(
  p <- ggplot(plottable, aes(featname, pname)) + 
    geom_tile(aes(fill = featval), colour = "white") +
    scale_fill_gradient(low = "white",high = "steelblue")
)

##########################################################################################
## Featurize the Caltech dataset

## the caltech dataset has a two-level directory structure, so the parsing 
## will be a bit different from get_blob_info
get_caltech_info <- function(storageAccount, storageKey, container, prefix) {
  marker = NULL;
  blob_info = NULL;
  repeat {
    info <- azureListStorageBlobs(NULL,
                                  storageAccount = storageAccount,
                                  storageKey = storageKey,
                                  container = container,
                                  marker = marker,
                                  prefix = prefix)
    
    if (is.null(blob_info)) {
      blob_info = info;
    } else {
      blob_info = rbind(blob_info, info)
    }
    
    marker <- attr(info, 'marker');
    print(paste0("Have ", nrow(blob_info), " blobs"));
    if (marker == "") {
      break
    } else {
      print("Still more blobs to get")
    }
  }
  # end blob directory read loop
  
  # preprocess the file names into urls and class (person) names
  blob_info$url <- paste(BLOB_URL_BASE, sep = '', blob_info$name)
  blob_info$fname <- sapply(strsplit(blob_info$name, '/'), function(l) { l[length(l)] })
  blob_info$bname <- sapply(strsplit(blob_info$fname, ".", fixed = TRUE), function(l) l[1])
  blob_info$pname <- sapply(strsplit(blob_info$name, '/'), function(l) { 
                                  strsplit(l[2],'.')[2] # the second part of the "001.ak47" string, which is the second directory
                            })
  
  return(blob_info);
}

### load or make featurized data
if( file.exists(CALTECH_FEATURIZED_DATA)){
  
  caltech_df <- readRDS(CALTECH_FEATURIZED_DATA)
  
} else {

  caltech_info <- get_caltech_info(storageAccount, storageKey, container, prefix = "256_ObjectCategories");

  # Featurize Caltech on my 4 available local cores  
  
  SLOTS=4
  rxOptions(numCoresToUse=SLOTS);
  rxSetComputeContext(RxLocalParallel());
  
  caltech_shards <- shardDataFrame(caltech_info, SLOTS)      
  start_time <- Sys.time()
  outputs <- rxExec(FUN=parallel_kernel, elemArgs=caltech_shards)
  end_time <- Sys.time()
  print(paste0("Local parallel ran for ", round(as.numeric(end_time - start_time, units="secs")), " seconds"))
  caltech_df <- Reduce(rbind, outputs)
 
  saveRDS(caltech_df, CALTECH_FEATURIZED_DATA);
}


##########################################################################################
## Knots dataset

if( file.exists(KNOTS_FEATURIZED_DATA)){
  
  knots_df <- readRDS(KNOTS_FEATURIZED_DATA)
  
} else {
  
  knots_info <- get_blob_info(storageAccount, storageKey, container, prefix = "knot_images_png");

  # Featurize Caltech on my 6 local cores  
  SLOTS=6 
  rxOptions(numCoresToUse=SLOTS);
  rxSetComputeContext(RxLocalParallel());

  knots_shards <- shardDataFrame(knots_info, SLOTS);      # create a list of dataframes, a uniform partition of blob_info
  start_time <- Sys.time()
  outputs <- rxExec(FUN=parallel_kernel, elemArgs=knots_shards);
  end_time <- Sys.time()
  print(paste0("Local parallel ran for ", round(as.numeric(end_time - start_time, units="secs")), " seconds"))
  
  knots_df <- Reduce(rbind, outputs);
  
  saveRDS(knots_df, KNOTS_FEATURIZED_DATA);
  
}


##########################################################################################
## Do something clever with it

# What 




