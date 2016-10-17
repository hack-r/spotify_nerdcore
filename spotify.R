pacman::p_load(dplyr, httr, plotly)

# Setup -------------------------------------------------------------------
clientID <- '24f3a86d3b164c7591f7f994ce59bc36'
secret   <- '380c2096a62a494eb8c04cfc18bf86bd'
response <- POST(
                'https://accounts.spotify.com/api/token',
                accept_json(),
                authenticate(clientID, secret),
                body   <- list(grant_type <- 'client_credentials'),
                encode <- 'form',
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
  d$main_genre[i]  <- results$artists$items[[i]]$genres[1] 
}

# to get rid of junk where nerdcore is not the 1st genre, unless I can vouch for them
# BTW WTH is 'antiviral pop'?
d <- d[d$main_genre=="nerdcore" | d$name %in% c("MC Frontalot", "MC Lars"), ] 

# *COUGH*
d$popularity[d$name=="YTCracker"] <- d$popularity[d$name=="YTCracker"] + 10 # DG bonus
# *COUGH*

# "Influence" in interaction of Spotify's definition of popularity and followers
d$influence <- d$popularity*d$followers
d[order(d$influence),]

# Plot --------------------------------------------------------------------
plot_ly(d, x = ~popularity, y = ~followers, type = 'scatter', mode = 'markers', 
        size = ~influence, color = ~name, colors = colorRampPalette(c('red','blue','green'))(28),
#         marker = list(opacity = 0.5, sizemode = 'diameter', 
#                       sizeref = 1.5),
        text = ~paste('Name:', ~name, '<br>Influence (followers*popularity):', ~influence)) %>%
  layout(title = 'Nerdcore on Spotify',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = FALSE)
