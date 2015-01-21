#Initialize pull with date of your choice
json <- paste0("http://en.wikipedia.org/w/api.php?action=query&list=recentchanges&format=json&rcstart=", "2014-08-14T22:51:01Z", "&rcdir=newer&rcprop=user%7Ctimestamp&rcshow=anon&rclimit=50&rctype=edit")
json_data <- rjson::fromJSON(file = json)

iteration<-0

#Increase number of iterations to get more data
while(iteration<3)
{

#Grab the initial data

  json <- paste0("http://en.wikipedia.org/w/api.php?action=query&list=recentchanges&format=json&rcstart=", json_data$query$recentchanges[[5]]$timestamp, "&rcdir=newer&rcprop=user%7Ctimestamp&rcshow=anon&rclimit=1&rctype=edit")
  json_data <- rjson::fromJSON(file = json)
  user <- vector()
  user <- sapply(json_data$query$recentchanges, function(x) c(user, x$user))
  num_edits <- 1

  while(num_edits<5)
  {

#Grab the rest of the data

    new_json <- paste0("http://en.wikipedia.org/w/api.php?action=query&list=recentchanges&format=json&rcstart=", json_data$query$recentchanges[[1]]$timestamp, "&rcdir=newer&rcprop=user%7Ctimestamp&rcshow=anon&rclimit=1&rctype=edit")
    json_data <- rjson::fromJSON(file = new_json)
    print(json_data$query$recentchanges[[1]]$timestamp)
    num_edits <- num_edits+length(json_data$query$recentchanges)
    print(num_edits)
    new_user <- vector()
    new_user <- sapply(json_data$query$recentchanges, function(x) c(new_user, x$user))
    user <- c(user, new_user)

  }

#Process IP urls and download geolocations

  user <- subset(user, nchar(user)<16)
  urls <- paste0("http://freegeoip.net/json/",user)
  download.file(urls[1], "temp")
  df <- as.data.frame( fromJSON(file("temp")) , stringsAsFactors=FALSE)
  for ( i in 2:length(urls) ) {download.file(urls[i], "temp", quiet=TRUE); df <- rbind( df, fromJSON( file("temp") )  )}
  print("Geolocating finished")
  
#Throw out non-U.S.
  df <- subset(df, df$country_code=="US")

#Throw out nonspecific-U.S.
  df <- subset(df, df$region_code!="")
  getcounties <- paste0("http://data.fcc.gov/api/block/find?format=json&latitude=",df$latitude,"&longitude=",df$longitude)
  county <- vector()
  for ( i in 1:length(getcounties) ) {county <- c(county, list(fromJSON(getcounties[i])))}
  county_edits <- vector()
  for ( i in 1:length(county) ) {county_edits <- c(county_edits, county[[i]]$County[[1]])}
  county_table <- table(county_edits)
  print(head(county_table))
  print("Iteration finished")
  iteration <- iteration + 1
  Sys.sleep(5)
}
