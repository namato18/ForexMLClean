library(blastula)
library(rmarkdown)
library(pandoc)
library(keyring)
library(glue)
library(rvest)

# setwd("/home/rstudio/CryptoWatching")


create_smtp_creds_file(file = "stock_watcher_creds",
                       user = "stock_watcher@outlook.com",
                       provider = "outlook")

#Sys.setenv(RSTUDIO_PANDOC = Sys.getenv("RSTUDIO_PANDOC"))

my_email_object = render_email("blastula_email.qmd")

smtp_send(my_email_object,
          from = "stock_watcher@outlook.com",
          to = "nickamato608@gmail.com",
          subject = paste0("testing email from blastula_",Sys.time()),
          credentials = creds_file("stock_watcher_creds"))
###########################################################


html.object = read_html('plots/TESTIMAGE.HTML')

email <- compose_email(
  body = md(glue("Your email message.
    {html.object}"
  )))
smtp <- create_smtp_creds_key(
  id = "outlook",
  user = "stock_watcher@outlook.com",
  provider = "office365",
  host = "smtp.office365.com",
  use_ssl = TRUE)
email %>%
  smtp_send(to = 'nickamato608@gmail.com',
            from = 'stock_watcher@outlook.com',
            subject = 'henlo',
            credentials = creds_key(id = "outlook"))
