library(dplyr)
library(rminka)
library(jsonlite)
library(httr)
library(leaflet)
#library(magick)
library(tibble)
library(knitr)


user_project <- mnk_user_proj(4)

user_project


user_obs <- mnk_user_obs(user_id= 4, year = 2025, month = 8)

user_obs


obs_sf <- mnk_obs_sf(user_obs,"observed_on", "url_picture","id")

obs_sf
