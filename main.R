library(rvest)

html = read_html("https://espnbet.com/sport/basketball/organization/united-states/competition/wncaab/section/lines")

x <- html %>% html_elements(xpath="//div")