library(rjson)
library(slackr)
library(httr)
library(lubridate)
library(stringr)
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
                      lapply(expr, evalVis)
                    },
                    # if it's a call or a name, eval, printing run output as if in console
                    call = ,
                    name = {
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
      
      for (item in tmp) if (item$visible) { cat(item$value); cat("\n") }
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


calc_wait <- function() {
  today = as.character(Sys.time())
  
  day = substr(today,9,10)
  month = substr(today,6,7)
  year = substr(today,1,4)
  time_now = force_tz(strptime(substr(today,12,16),"%H:%M"), tzone = "America/Chicago")
  if (time_now <= force_tz(strptime("08:00","%H:%M"), tzone = "America/Chicago"))
  {
    next_game = force_tz(strptime(paste0(substr(today,1,8),as.numeric(substr(today,9,10)),"08:00"),"%Y-%m-%d %H:%M"), tzone = "America/Chicago")
    wait_time = as.numeric(difftime(next_game, time_now, units = "secs"))
  } else {
    wait_time = sample(25:60, 1)
  }
  
  return(wait_time)
}

while (TRUE)
{
  date = format(Sys.Date(),"%Y%m%d")
  data = fromJSON(file=paste0("https://site.api.espn.com/apis/site/v2/sports/basketball/womens-college-basketball/scoreboard?groups=50&dates=",as.character(date)))
  games = data[["events"]]
  
  games_previous = read.csv('all_games.csv')
  games_table = as.data.frame(do.call(rbind, games))
  
  games_table$competitions[[1]][["broadcasts"]]
  
  has_odds = NULL
  
  for (a in 1:length(games))
  {
    has_odds = as.vector(c(has_odds,0 < length(games_table$competitions[[a]][[1]][["odds"]])))
  }
  
  games_table$has_odds = has_odds
  
  games_clean = games_table[,c("id","shortName","has_odds","date")]
  
  games_clean$date = as.character(with_tz((strptime(games_clean$date, "%Y-%m-%dT%H:%MZ",tz = "UTC")),"America/Chicago"))
  
  games_clean <- as.data.frame(apply(games_clean,2,as.character))
  
  games_clean = games_clean[order(games_clean$date),]
  rownames(games_clean) = NULL
  
  games_clean = games_clean[which(games_clean$has_odds == 'TRUE'),]
  
  games_new = subset(games_clean, !(id %in% games_previous$id))[,c(2,4)]
  colnames(games_new) = c("Game","Date Time CT")
  
  games_output = unique(rbind(games_previous,games_clean))
  write.csv(games_output,'all_games.csv', row.names = F)
  
  if (length(games_new$Game > 0)){
    games_new$Time = format(as.POSIXct(games_new$`Date Time CT`), format = "%H:%M")
    games_new$all_together = paste0(games_new$Game," - ",games_new$Time)
    webhook = str_conv(readLines("C:/Users/jacob/Desktop/Bet Scraping Hooks/webhook.txt"), "utf-8")
    webhook1 = str_conv(readLines("C:/Users/jacob/Desktop/Bet Scraping Hooks/webhook-1.txt"), "utf-8")
    #webhook = readLines("/home/pi/ESPN-BET/webhook.txt")
    #webhook1 = readLines("/home/pi/ESPN-BET/webhook-1.txt")
    relevant_games = paste(games_new$all_together,"\n")
    finished_output = c(paste("New Games Added to ESPN BET:\n",sep = " "),
                        relevant_games)
    
    slackr_bot1(finished_output, incoming_webhook_url = webhook1)
    Sys.sleep(90)
    slackr_bot1(finished_output, incoming_webhook_url = webhook)
  } else {
    wait_time = calc_wait()
    cat("waiting more...",wait_time)
    Sys.sleep(wait_time)  }
}
