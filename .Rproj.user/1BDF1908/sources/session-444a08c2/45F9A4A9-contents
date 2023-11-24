options("install.lock"=FALSE)
#install.packages(c("rjson","slackr","sqldf","httr"))
library(rjson)
library(slackr)
library(sqldf)
library(httr)

slackr_bot1 <- function(...,
                        channel=Sys.getenv("SLACK_CHANNEL"),
                        username=Sys.getenv("SLACK_USERNAME"),
                        icon_emoji=Sys.getenv("SLACK_ICON_EMOJI"),
                        incoming_webhook_url=Sys.getenv("SLACK_INCOMING_URL_PREFIX")) {
  
  if (incoming_webhook_url == "") {
    stop("No incoming webhook URL specified. Did you forget to call slackr_setup()?", call. = FALSE)
  }
  
  if (icon_emoji != "") { icon_emoji <- sprintf(', "icon_emoji": "%s"', icon_emoji)  }
  
  resp_ret <- ""
  
  if (!missing(...)) {
    
    # mimics capture.output
    
    # get the arglist
    args <- substitute(list(...))[-1L]
    
    # setup in-memory sink
    rval <- NULL
    fil <- textConnection("rval", "w", local = TRUE)
    
    sink(fil)
    on.exit({
      sink()
      close(fil)
    })
    
    # where we'll need to eval expressions
    pf <- parent.frame()
    
    # how we'll eval expressions
    evalVis <- function(expr) withVisible(eval(expr, pf))
    
    # for each expression
    for (i in seq_along(args)) {
      
      expr <- args[[i]]
      
      # do something, note all the newlines...Slack ``` needs them
      tmp <- switch(mode(expr),
                    # if it's actually an expresison, iterate over it
                    expression = {
                      cat(sprintf("> %s\n", deparse(expr)))
                      lapply(expr, evalVis)
                    },
                    # if it's a call or a name, eval, printing run output as if in console
                    call = ,
                    name = {
                      cat(sprintf("> %s\n", deparse(expr)))
                      list(evalVis(expr))
                    },
                    # if pretty much anything else (i.e. a bare value) just output it
                    integer = ,
                    double = ,
                    complex = ,
                    raw = ,
                    logical = ,
                    numeric = cat(sprintf("%s\n\n", as.character(expr))),
                    character = cat(sprintf("%s\n\n", expr)),
                    stop("mode of argument not handled at present by slackr"))
      
      for (item in tmp) if (item$visible) { print(item$value, quote = FALSE); cat("\n") }
    }
    
    on.exit()
    
    sink()
    close(fil)
    
    # combined all of them (rval is a character vector)
    output <- paste0(rval, collapse="\n")
    
    loc <- Sys.getlocale('LC_CTYPE')
    Sys.setlocale('LC_CTYPE','C')
    on.exit(Sys.setlocale("LC_CTYPE", loc))
    
    resp <- POST(url = incoming_webhook_url, encode = "form",
                 add_headers(`Content-Type` = "application/x-www-form-urlencoded",
                             Accept = "*/*"), body = URLencode(sprintf("payload={\"channel\": \"%s\", \"username\": \"%s\", \"text\": \"%s\"%s}",
                                                                       channel, username, output, icon_emoji)))
    warn_for_status(resp)
  }
  return(invisible())
}

while (TRUE)
{
  x = fromJSON(file="https://cdn.wnba.com/static/json/staticData/WNBATransactions.json")
  y = as.data.frame(x[1])
  for (a in 2:length(x))
  {
    y = rbind(y,as.data.frame(x[a]))
  }
  
  y$TEAM_SLUG[which(y$TEAM_SLUG == 'stars')] = 'aces'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Other' & grepl("exercised their option",y$TRANSACTION_DESCRIPTION))] = 'Option'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Other' & grepl("extended a.*qualifying offer",y$TRANSACTION_DESCRIPTION))] = 'Qualifying Offer'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Other' & grepl("was set active",y$TRANSACTION_DESCRIPTION))] = 'Activated'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Other' & grepl("temporarily suspended",y$TRANSACTION_DESCRIPTION))] = 'Temp Suspension'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Other' & grepl("suspended for the full season",y$TRANSACTION_DESCRIPTION))] = 'Team Full Suspension'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Other' & grepl("Pregnancy/Childbirth",y$TRANSACTION_DESCRIPTION))] = 'Pregnancy/Childbirth'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Other' & grepl("season due to a Personal Decision",y$TRANSACTION_DESCRIPTION))] = 'Personal Full Suspension'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Other' & grepl("season due to a Non-WNBA Injury",y$TRANSACTION_DESCRIPTION))] = 'Injury Full Suspension'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Signing' & grepl("Contract Extension",y$TRANSACTION_DESCRIPTION))] = 'Contract Extension'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Released' & grepl("renounced their rights",y$TRANSACTION_DESCRIPTION))] = 'Renounced Rights'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Released' & grepl("voluntarily retired",y$TRANSACTION_DESCRIPTION))] = 'Retired'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Signing' & grepl("Training Camp Contract",y$TRANSACTION_DESCRIPTION))] = 'Signed TCC'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Signing' & grepl("Rookie Scale Contract",y$TRANSACTION_DESCRIPTION))] = 'Signed Rookie Scale'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Signing' & grepl("Contract Amendment - Divorce",y$TRANSACTION_DESCRIPTION))] = 'Contract Amendment - Divorce'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Signing' & grepl("Contract Amendment - Time-Off Bonus",y$TRANSACTION_DESCRIPTION))] = 'Contract Amendment - Time-Off Bonus'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Signing' & grepl("Contract Amendment - Trade Bonus",y$TRANSACTION_DESCRIPTION))] = 'Contract Amendment - Trade Bonus'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Signing' & grepl("Core Contract",y$TRANSACTION_DESCRIPTION))] = 'Signed Core Contract'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Signing' & grepl("signed a Contract",y$TRANSACTION_DESCRIPTION))] = 'Signed Standard Contract'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Signing' & grepl("Rest-of-Season Hardship Contract",y$TRANSACTION_DESCRIPTION))] = 'Signed ROS Hardship Contract'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Signing' & grepl("Rest-of-Season Contract",y$TRANSACTION_DESCRIPTION))] = 'Signed ROS Standard Contract'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Signing' & grepl("7-Day Contract",y$TRANSACTION_DESCRIPTION))] = 'Signed 7-Day Contract'
  y$TRANSACTION_TYPE[which(y$TRANSACTION_TYPE == 'Signing' & grepl("signed a Contract",y$TRANSACTION_DESCRIPTION))] = 'Signed Standard Contract'
  
  new_compare = y[,c(3,2)]
  #old_compare = read.csv("home/pi/WNBA-Transactions/last_update.csv",row.names = 1)
  old_compare = read.csv("last_update.csv",row.names = 1)
  new_rows <- sqldf('SELECT * FROM new_compare EXCEPT SELECT * FROM old_compare')
  deleted_rows <- sqldf('SELECT * FROM old_compare EXCEPT SELECT * FROM new_compare')
  deleted_transactions = capture.output(invisible(apply(deleted_rows[,1:2], 1,cat, "\n")))
  added_transactions = capture.output(invisible(apply(new_rows[,1:2], 1,cat, "\n")))
  
  #write.csv(new_compare,'home/pi/WNBA-Transactions/last_update.csv')
  write.csv(new_compare,'last_update.csv')
  
  the_next_webhook = readLines("C:/Users/jacob/Desktop/Transactions Log/webhook.txt")
  hhs_webhook = readLines("C:/Users/jacob/Desktop/Transactions Log/webhook-hhs.txt")
  
  #the_next_webhook = readLines("home/pi/WNBA-Transactions/webhook.txt")
  #hhs_webhook = readLines("home/pi/WNBA-Transactions/webhook-hhs.txt")
  
  if (length(deleted_transactions) > 1 || deleted_transactions != "FALSE FALSE ")
  {
    slackr_bot1(deleted_transactions, incoming_webhook_url = the_next_webhook)
    slackr_bot1(deleted_transactions, incoming_webhook_url = hhs_webhook)
  }
  if (length(added_transactions) > 1 ||added_transactions != "FALSE FALSE ")
  {
    slackr_bot1(added_transactions, incoming_webhook_url = the_next_webhook)
    slackr_bot1(added_transactions, incoming_webhook_url = hhs_webhook)
  }
  print("waiting...")
  Sys.sleep(90)
}

