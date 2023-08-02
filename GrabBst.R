symbol = "AUDUSD_1day"
prediction = "BreakH"

bst = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"_",prediction,".rds"))

t = xgb.importance(bst$feature_names, bst)
