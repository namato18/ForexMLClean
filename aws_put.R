library(tictoc)
library(aws.s3)

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

# AUTOMATED PUT TO AWS
tic()
x = list.files(path = 'C:/Users/xbox/Desktop/Rstuff/bsts-8-21-2023')

for(i in 1:length(x)){
  put_object(
    file = file.path('C:/Users/xbox/Desktop/Rstuff/bsts-8-21-2023', x[i]), 
    object = x[i], 
    bucket = "cryptomlbucket/FXCleanBoosts"
  )
  print(i)
}
toc()