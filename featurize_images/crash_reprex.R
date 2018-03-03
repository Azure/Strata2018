
# get two files

blob_info = data.frame(url=c("https://storage4tomasbatch.blob.core.windows.net/tutorial/faces_small/Aaron_Eckhart_0001.jpg"),
                       fname=c("Aaron_Eckhart_0001.jpg"),
                       stringsAsFactors = FALSE);


# get the images from blob and do them locally
DATA_DIR <- file.path(getwd(), 'localdata');
if(!dir.exists(DATA_DIR)) dir.create(DATA_DIR);

# do this in paralell, too
for (i in 1:nrow(blob_info)) {
  targetfile <- file.path(DATA_DIR, blob_info$fname[[i]]);
  if (!file.exists(targetfile)) {
    download.file(blob_info$url[[i]], destfile = targetfile, mode="wb")
  }
}

blob_info$localname <- paste(DATA_DIR, sep='/', blob_info$fname);
# print(blob_info$localname)

image_features <- rxFeaturize(data = blob_info,
                              mlTransforms = list(loadImage(vars = list(Image = "localname")),
                                                  resizeImage(vars = list(ResImage = "Image"),
                                                              width = 224, height = 224),
                                                  extractPixels(vars = list(Pixels = "ResImage"))
                                                  , featurizeImage(var = "Pixels", dnnModel = 'resnet18')
                              ),
                              mlTransformVars = c("localname"))


