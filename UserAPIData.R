credentials <- data.frame(
  User = c('gentlemam1','gentlemam2','gentlemam3','nick'),
  Password = c("gentlemam1234","gentlemam1234","gentlemam1234","123"),
  APIKey = c("wZpij1rDxXsrnyRyuNmuaoLPsVSgJKvmmgt0rzi44GZB03za9GBFqeB6chXi1p0T",
             "3VSV3sbcbDS5DFnYHnpqqKZwQOjFG5hiFXEB7r6Kaev0wTBDQlvyEpOLFZgAhZZD",
             "HbKcjXOHLS0yseTvMnwX7jxltI0ugk2ZXoiYZHeDRZr9b2XWbiCBkOODsPu6xpSp",
             "UWG67pA2SI65uA3ZzqEzSQZbU9poUYHtOiZ5YAdV3lJXhi6dUSeanbxLlcTFrN3w"
             
             ),
  APISecret = c("9qhPtPDePdBJnWL5zThAxqrUWXNcv37NYbyDHdkDctoJZGa0CZS6IyPqmqOdIh3i",
                "KECWzTynzt47MdHyFdY28l06G43odgzjXyOKf52VaiA4mEs7x68MTRHpLNl2XH0E",
                "xghtE9HU3aNHkMojdVe3jxgAzBu5Xz0EqiuAoifbM9b0rY09KjZntuSJzsCj5gvC",
                "rEg9vqo61kMpB7up3kbp2Huy1mMyYQFpAdyc3OBO32dwE8m32eHcr3185aEa2d7k"
                ),
  stringsAsFactors = FALSE
)

saveRDS(object = credentials, file = paste0(tempdir(),"/credentials.rds"))

put_object(
  file = file.path(tempdir(), "credentials.rds"), 
  object = "credentials.rds", 
  bucket = "cryptomlbucket/APIKeys"
)
