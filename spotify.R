pacman::p_load(dplyr, httr, plotly)

# Setup -------------------------------------------------------------------
clientID <- 'get'
secret   <- 'your own!'
response <- POST(
                'https://accounts.spotify.com/api/token',
                accept_json(),
                authenticate(clientID, secret),
                body   = list(grant_type <- 'client_credentials'),
                encode = 'form',
                verbose()
)
mytoken     <- content(response)$access_token
HeaderValue <- paste0('Bearer ', mytoken)

# Query API ---------------------------------------------------------------
URI       <- paste0('https://api.spotify.com/v1/search?query=genre:nerdcore&offset=0&limit=50&type=artist') # 50 is max limit
response2 <- GET(url = URI, add_headers(Authorization = HeaderValue))
results   <- content(response2)

# Parse -------------------------------------------------------------------
d <- data.frame(name=1:50,popularity=1:50,followers=1:50, main_genre=1:50)

for(i in 1:50){
  d$name[i]       <- results$artists$items[[i]]$name
  d$popularity[i] <- results$artists$items[[i]]$popularity
  d$followers[i]  <- results$artists$items[[i]]$followers$total
  d$main_genre[i] <- results$artists$items[[i]]$genres[1] 
}

# to get rid of junk where nerdcore is not the 1st genre, unless I can vouch for them
# BTW WTH is 'antiviral pop'?
d <- d[d$main_genre=="nerdcore" | d$name %in% c("MC Frontalot", "MC Lars", "MC Chris"), ] 

# *COUGH*
d$popularity[d$name=="YTCracker"] <- d$popularity[d$name=="YTCracker"] + 10 # DG bonus
# *COUGH*

# "Influence" in interaction of Spotify's definition of popularity and followers
d$influence <- d$popularity*d$followers
d[order(d$influence),]

# Fix genre data class so that I can use it in the chart
d$main_genre <- as.factor(unlist(d$main_genre))

# Plot --------------------------------------------------------------------
plot_ly(d, x = popularity, y = followers, type = 'scatter', mode = 'markers+text', 
        size = influence, color = main_genre, #colors = colorRampPalette(c('red','blue','green'))(28),
        marker = list(opacity = 0.5, sizemode = 'diameter', 
                      sizeref = 1.5),
        text = name) %>%
  layout(title = 'Nerdcore on Spotify',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T)
