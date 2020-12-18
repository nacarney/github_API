#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)

# Choosing API
oauth_endpoints("github")

myapp <- oauth_app(appname = "nacarney_github_API",
                   key = "a69177084e2a2370582f",
                   secret = "631a04fb1b70d81f7f562d4c63b3e81828508de3")

# Getting OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API to get user 'phadej's followers
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/phadej/followers", gtoken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Extracted Info
req

# Convert to a data.frame (json)
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
gitDF

# Display follower logins as an example
gitDF$login


