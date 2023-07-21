library(aws.s3)

# Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJUAAKBDHG",
#            "AWS_SECRET_ACCESS_KEY" = "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",
#            "AWS_DEFAULT_REGION" = "us-east-1")
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)
bucketlist()

write.csv(iris, file.path(tempdir(), "iris.csv"))


# PUT OBJECT
put_object(
  file = file.path(tempdir(), "iris.csv"),
  object = "iris.csv",
  bucket = "cryptomlbucket"
)

# READ OBJECT
GETBOOST = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/bsts", object = "bst_ACHUSDT1day1.rds")


# AUTOMATED PUT TO AWS
tic()
x = list.files(path = 'C:/Users/xbox/Desktop/Rstuff/bsts-7-10-2023')

for(i in 1:length(x)){
  put_object(
    file = file.path("C:/Users/xbox/Desktop/Rstuff/bsts-7-10-2023", x[i]), 
    object = x[i], 
    bucket = "cryptomlbucket/bsts2"
  )
}
toc()
