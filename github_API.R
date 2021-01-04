#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)
#install.packages("httpuv")
library(httpuv)
#install.packages("plotly")
library(plotly)
require(devtools)

# Choosing API
oauth_endpoints("github")

myapp <- oauth_app(appname = "nacarney_github_API",
                   key = "a69177084e2a2370582f",
                   secret = "631a04fb1b70d81f7f562d4c63b3e81828508de3")

# Getting OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API to get my repos
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/nacarney/repos", gtoken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Extracted Info
req

# Convert to a data.frame (json)
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Display names of my repos as an example
gitDF$name




## Part 2 - Visualization

## Getting my raw data set

completeData = GET("https://api.github.com/users/andrew/followers?per_page=100;", gtoken) #subset of users whose info I will be using, 
# taken from the follower base of GitHubs second most active user, Andrew
                                                                                  
stop_for_status(completeData)
extract = content(completeData)
#converts into dataframe
fullDf = jsonlite::fromJSON(jsonlite::toJSON(extract))

# Retrieves list of usernames
id = fullDf$login
user_ids = c(id)

# Creating empty vector and data.frame for all user information to be easier to visualize
users = c()
userInfoDB = data.frame(
  username = integer(),
  following = integer(),
  followers = integer(),
  repos = integer(),
  dateCreated = integer()
)

## Visualization 1 (Number of User Repos vs Number of users current user is following)

#adds each user to a list
for(i in 1:length(user_ids))
{
  
  followingURL = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  followingRequest = GET(followingURL, gtoken)
  followingContent = content(followingRequest)
  
  #Does not add users if they have no followers
  if(length(followingContent) == 0)
  {
    next
  }
  
  followingDF = jsonlite::fromJSON(jsonlite::toJSON(followingContent))
  followingLogin = followingDF$login
  
  #Loop to retrieve following users
  for (j in 1:length(followingLogin))
  {
    # Check that the user is not already in the list of users (duplicates)
    if (is.element(followingLogin[j], users) == FALSE)
    {
      #Add user to current list
      users[length(users) + 1] = followingLogin[j]
      
      #Retrieve data on each user
      followingUrl2 = paste("https://api.github.com/users/", followingLogin[j], sep = "")
      following2 = GET(followingUrl2, gtoken)
      followingContent2 = content(following2)
      followingDF2 = jsonlite::fromJSON(jsonlite::toJSON(followingContent2))
      
      #Retrieve who each user follows
      followingNumber = followingDF2$following
      
      #Retrieve each user's followers
      followersNumber = followingDF2$followers
      
      #Retrieve each user's number of repositories
      reposNumber = followingDF2$public_repos
      
      #Retrieve year which each user joined Github
      yearCreated = substr(followingDF2$created_at, start = 1, stop = 4)
      
      #Add users data to a new row in dataframe
      userInfoDB[nrow(userInfoDB) + 1, ] = c(followingLogin[j], followingNumber, followersNumber, reposNumber, yearCreated)
      
    }
    next
  }
  #Stop at 100 users to bring down runtime
  if(length(users) > 100)
  {
    break
  }
  next
}

## Visualization 2 (Top 5 Languages)

languages = c()

for (i in 1:length(users))
{
  RepositoriesUrl = paste("https://api.github.com/users/", users[i], "/repos", sep = "")
  Repositories = GET(RepositoriesUrl, gtoken)
  RepositoriesContent = content(Repositories)
  RepositoriesDF = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent))
  RepositoriesNames = RepositoriesDF$name
  
  #Loop through all the repositories of an individual user
  for (j in 1: length(RepositoriesNames))
  {
    #Find all repositories and save in data frame
    RepositoriesUrl2 = paste("https://api.github.com/repos/", users[i], "/", RepositoriesNames[j], sep = "")
    Repositories2 = GET(RepositoriesUrl2, gtoken)
    RepositoriesContent2 = content(Repositories2)
    RepositoriesDF2 = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent2))
    language = RepositoriesDF2$language
    
    #Not including repos with no language
    if (length(language) != 0 && language != "<NA>")
    {
      languages[length(languages)+1] = language
    }
    next
  }
  next
}

#Puts 5 most popular languages into table 
languagesList = sort(table(languages), increasing=TRUE)
top5Languages = languagesList[(length(languagesList)-4):length(languagesList)]

#converting top 5 languages to a dataframe
languageDF = as.data.frame(top5Languages)

## Visualization 3 - Popular Users with more than 1000 followers against the number that they're following

#Extracting data for users with over 1000 followers
popularUsers <- userInfoDB[which(userInfoDB$followers>=1000), ]
popularUsers$code = 1

combined <- rbind(popularUsers,userInfoDB$following)
Followers = combined$followers
Following = combined$following

## Plots

plot = plot_ly(data = userInfoDB, x = ~repos, y = ~following, 
                text = ~paste("Following: ", following, "<br>Repositories: ", 
                              repos, "<br>Date Created:", dateCreated), color = ~dateCreated)

pieChart <- plot_ly(data =languageDF, labels = ~languageDF$languages, values = ~Freq, type = 'pie') %>%
  layout(title = 'Languages Visualization',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

scatterPlot <- plot_ly(data = combined, x = ~Followers, y = ~Following, color = ~code, colors = "Set1",
                       text = ~paste("User: ", combined$username, '<br>Followers: ', combined$followers, '<br>Following:', combined$following)) %>%
  layout(title = 'Popular Users Followers vs Following',yaxis = list(zeroline = FALSE),xaxis = list(zeroline = FALSE),
         plot_bgcolor='rgba(63, 191, 165,0.2)')

## Below I am uploading all of my visualizations to plot.ly

Sys.setenv("plotly_username"="nathanc283")
Sys.setenv("plotly_api_key"="XUen1mP7giynrphsgfKf")

api_create(plot, filename = "Repositories vs Following")
api_create(pieChart, filename = "Top 5 Languages")
api_create(scatterPlot, filename = "Popular Users Followers vs Following")
