library(dplyr)
library(reshape2)
library(stringi)
library(assertthat)

### DECLARE PARAMS ##############
nr_redundant_symbols = 2 # 2 symbols match co-players, how many do not match nobody?
black_n_white = FALSE # just for testing - if TRUE, number will be plotted next to the symbol to immitate color
seed = 950707
### DECLARE PARAMS ##############

# DELETES pics folder content
do.call(file.remove, list(list.files("pics", full.names = TRUE)))
set.seed(seed)
# RUNS the algorithm, exports pictures to the pics folder
runEverything(nr_redundant_symbols, black_n_white)


runEverything <- function(nr_redundant_symbols, black_n_white) {
  
  # load guests
  assert_that(file.exists("data/guests.csv"))
  guests_secret <- read.csv("data/guests.csv", encoding = "UTF-8", stringsAsFactors = FALSE) 
  assert_that(cols(guests_secret))
  
  
}

# set of guests from csv - cols Name and CategoryInternalName







# set of categories - CategoryId, CategoryInternalName, CategoryExternalName
CategoryInternalNames <- guests_secret %>% pull(CategoryInternalName) %>% unique()
categories <- data.frame(
  CategoryId =
    seq(length(CategoryInternalNames)),
  CategoryInternalName =
    CategoryInternalNames, 
  CategoryExternalName = 
    c("Jahoda", "Avokado", "Mango", "Citron", "Merunka", "Pomeranc", "Mandarinka", "Boruvka", "Dyne",
      "Jablko", "Banan", "Meloun", "Malina", "Broskev")[1:length(CategoryInternalNames)],
  stringsAsFactors = FALSE
)

# set of teams - TeamId, CategoryId1, , CategoryId2 ... , CategoryId5/6
teams <- read.csv("DOBBLE/teams.csv", encoding = "UTF-8", stringsAsFactors = FALSE) %>%
  melt(id.vars = "TeamId", value.name = "CategoryInternalName") %>%
  left_join(categories, by = "CategoryInternalName") %>%
  select(TeamId, CategoryExternalName) %>%
  arrange(TeamId)

nr_teams = teams %>% pull(TeamId) %>% unique() %>% length()
nr_players = (nrow(guests_secret)) %/% nr_teams
if (((nrow(guests_secret)) %% nr_teams)>0) { nr_players = nr_players + 1 }

# set of guests - Name, CategoryId
guests_public <- guests_secret %>%
  left_join(categories, by = "CategoryInternalName") %>%
  select(Name, CategoryExternalName)

# set of colors
colors = c("black", "dodgerblue3", "forestgreen", "red", "gold2", "darkorchid2", "grey", "brown")[1:nr_teams]
# set of symbols
symbols = c(
  "265F", "2600", "2601", "2602", "2605", "260E",
  "262F", "263B", "2638", "2660", "2663", "2665",
  "2666", "266B", "2708", "273F", "265B", "265A",
  "2702", "270E", "265C", "265E", "265D", "2622",
  "26D6", "26BD", "26AB", "0024", "00A3", "25A0"
)

makePlot <- function(dfToPlot, main, team_order, member_order, nr_to_display, black_n_white) {
  
  dfToPlot_inner = dfToPlot[sample(nrow(dfToPlot)),]
  
  n = nrow(dfToPlot_inner)
  
  wide = floor(sqrt(n))
  long = ceiling(n/wide)
  
  dfToPlot_inner$x = rep(seq(wide), long)[1:n]
  dfToPlot_inner$y = rep(seq(long), each = wide)[1:n]
  
  #png(paste0("DOBBLE/pics/", team_order, "_", member_order, "_", main, ".png"))
  png(paste0("DOBBLE/pics/", nr_to_display, "_", main, ".png"))
  plot(
    dfToPlot_inner$x,dfToPlot_inner$y,
    main = main,
    xlim = c(0.5, wide+0.5),
    ylim = c(0.5, long+0.5),
    cex = 10, 
    col = dfToPlot_inner$color,
    pch = stri_unescape_unicode(gsub("\\U","\\u", paste0("\\U", dfToPlot_inner$symbol), fixed=TRUE)),
    xaxt='n', yaxt='n', frame.plot=FALSE, xlab = "", ylab = ""
  )
  if (black_n_white) {
    points(
      pch = which(colors, dfToPlot_inner$color),
      0.3+dfToPlot_inner$x,dfToPlot_inner$y,
      main = main,
      xlim = c(0.5, wide+0.5),
      ylim = c(0.5, long+0.5),
      cex = 2, 
      xaxt='n', yaxt='n', xlab = "", ylab = ""
    )
  }
  dev.off()
  
}

db <- data.frame(
  symbol = rep(symbols, length(colors)),
  color = rep(colors, each = length(symbols)),
  stringsAsFactors = FALSE
)
  
db <- db[sample(nrow(db)),]

used_symbols = 0
team_order = 1
tournamentmember_order = 1
tournamentmember_randomnumbers = sample(nr_teams*nr_players)

TeamIds <- teams %>% pull(TeamId) %>% unique()
for (team_id in TeamIds) {
  
  member_order = 1
  
  TeamContents <- teams %>% filter(TeamId == team_id) %>% pull(CategoryExternalName) %>% sample()
  
  team_points <- data.frame(
    CategoryExternalName = rep(TeamContents, 2+nr_redundant_symbols),
    symbol = NA,
    color = NA
  ) 
  
  common_symbols <- db[(used_symbols+1):(used_symbols+nr_players),]
  
  team_points[1:nr_players,"symbol"] = common_symbols$symbol
  team_points[1:nr_players,"color"] = common_symbols$color
  team_points[(nr_players+2):(2*nr_players),"symbol"] = common_symbols$symbol[1:(nr_players-1)]
  team_points[(nr_players+2):(2*nr_players),"color"] = common_symbols$color[1:(nr_players-1)]
  team_points[(nr_players+1),"symbol"] = common_symbols$symbol[nr_players]
  team_points[(nr_players+1),"color"] = common_symbols$color[nr_players]
  
  eigen_symbols <- db[(used_symbols+nr_players+1):(used_symbols+(nr_players*(1+nr_redundant_symbols))),]
  
  team_points[((2*nr_players)+1):((2+nr_redundant_symbols)*(nr_players)),"symbol"] = eigen_symbols$symbol
  team_points[((2*nr_players)+1):((2+nr_redundant_symbols)*(nr_players)),"color"] = eigen_symbols$color
  
  for (TeamMember in TeamContents) {
    makePlot(
      team_points %>% filter(CategoryExternalName == TeamMember), 
      TeamMember,
      team_order,
      member_order,
      tournamentmember_randomnumbers[tournamentmember_order],
      black_n_white
    )
    
    member_order = member_order + 1
    tournamentmember_order = tournamentmember_order + 1
  }
  
  used_symbols = used_symbols + (nr_players*(1+nr_redundant_symbols))
  team_order = team_order + 1
 
}

save.image("20190712.RData")




