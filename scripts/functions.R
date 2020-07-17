findit2<-function(x,y) grepl(paste(x,collapse=";"),paste(y,collapse=";"))

makePlot <- function(dfToPlot, main, team_order, member_order, nr_to_display, black_n_white) {
  
  dfToPlot_inner = dfToPlot[sample(nrow(dfToPlot)),]
  
  n = nrow(dfToPlot_inner)
  
  wide = floor(sqrt(n))
  long = ceiling(n/wide)
  
  dfToPlot_inner$x = rep(seq(wide), long)[1:n]
  dfToPlot_inner$y = rep(seq(long), each = wide)[1:n]
  
  #png(paste0("pics/", team_order, "_", member_order, "_", main, ".png"))
  png(paste0("pics/", nr_to_display, "_", main, ".png"))
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
      pch = as.character(dfToPlot_inner$colorNumber),
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

runEverything <- function(nr_redundant_symbols, black_n_white) {
  
  library(dplyr)
  
  # set of guests from csv - cols Name and CategoryInternalName
  assert_that(file.exists("data/guests.csv"))
  guests_secret <- read.csv("data/guests.csv", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = "character")
  colnames(guests_secret)[1] = "Name"
  assert_that(
    findit2(colnames(guests_secret), c("Name", "CategoryInternalName")), 
    msg = "data guests.csv do not contain column Name or CategoryInternalName"
  )
  
  CategoryInternalNames <- guests_secret %>% pull(CategoryInternalName) %>% unique()
  
  fruits <- c("Jahoda", "Avokado", "Mango", "Citron", "Merunka", "Pomeranc", "Mandarinka", "Boruvka", "Dyne",
              "Jablko", "Banan", "Meloun", "Malina", "Broskev")
  
  assert_that(length(CategoryInternalNames)<=length(fruits),
              msg = "Too much categories, add fruits")
  
  # set of categories - CategoryId, CategoryInternalName, CategoryExternalName
  categories <<- data.frame(
    CategoryId =
      seq(length(CategoryInternalNames)),
    CategoryInternalName =
      CategoryInternalNames, 
    CategoryExternalName = 
      fruits[1:length(CategoryInternalNames)],
    stringsAsFactors = FALSE
  )
  
  # set of teams - TeamId, CategoryId1, , CategoryId2 ... , CategoryId4/5/6/7
  assert_that(file.exists("data/teams.csv"))
  teams <- read.csv("data/teams.csv", encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = "character")
  colnames(teams)[1] = "TeamId"
  assert_that(
    "TeamId" %in% colnames(teams),
    msg = "data teams.csv do not contain column TeamId"
  )
  teams <- teams %>%
    melt(id.vars = "TeamId", value.name = "CategoryInternalName") %>%
    left_join(categories, by = "CategoryInternalName") %>%
    select(TeamId, CategoryExternalName) %>%
    arrange(TeamId)
  
  teams_public <<- teams
  
  nr_teams = teams %>% pull(TeamId) %>% unique() %>% length()
  nr_players = (nrow(guests_secret)) %/% nr_teams
  if (((nrow(guests_secret)) %% nr_teams)>0) { nr_players = nr_players + 1 }
  
  # set of guests - Name, CategoryId
  guests_public <<- guests_secret %>%
    left_join(categories, by = "CategoryInternalName") %>%
    select(Name, CategoryExternalName)
  
  # set of colors
  colors = c("black", "dodgerblue3", "forestgreen", "red", "gold2", "darkorchid2", "grey", "brown")[1:nr_teams]
  # set of symbols (unicode numbers)
  symbols = c(
    "265F", "2600", "2601", "2602", "2605", "260E",
    "262F", "263B", "2638", "2660", "2663", "2665",
    "2666", "266B", "2708", "273F", "265B", "265A",
    "2702", "270E", "265C", "265E", "265D", "2622",
    "26D6", "26BD", "26AB", "0024", "00A3", "25A0"
  )
  
  # assigning numbers to a color (for BnW)
  color_numbers <- data.frame(
    color = colors,
    colorNumber = seq(length(colors)),
    stringsAsFactors = FALSE
  )
  
  assert_that(nr_teams<=length(colors),
              msg = "Too much teams, add colors")
  
  assert_that(nr_players<=length(symbols),
              msg = "Too much players, add symbols")
  
  # db of symbol X colors
  db <- data.frame(
    symbol = rep(symbols, length(colors)),
    color = rep(colors, each = length(symbols)),
    stringsAsFactors = FALSE
  )
  
  # shuffle
  db <- db[sample(nrow(db)),]
  
  # some iterators
  used_symbols = 0
  team_order = 1
  tournamentmember_order = 1
  tournamentmember_randomnumbers = sample(nr_teams*nr_players)
  
  TeamIds <- teams %>% pull(TeamId) %>% unique()
  # we create team by team
  for (team_id in TeamIds) {
    
    member_order = 1
    
    # what categories are in the team
    TeamContents <- teams %>% filter(TeamId == team_id) %>% pull(CategoryExternalName) %>% sample()
    
    # assigning symbols
    team_points <- data.frame(
      CategoryExternalName = rep(TeamContents, 2+nr_redundant_symbols),
      symbol = NA,
      color = NA
    ) 
    
    # symbols that will match
    common_symbols <- db[(used_symbols+1):(used_symbols+nr_players),]
    
    # 1-2, 2-3, ... P-1
    team_points[1:nr_players,"symbol"] = common_symbols$symbol
    team_points[1:nr_players,"color"] = common_symbols$color
    team_points[(nr_players+2):(2*nr_players),"symbol"] = common_symbols$symbol[1:(nr_players-1)]
    team_points[(nr_players+2):(2*nr_players),"color"] = common_symbols$color[1:(nr_players-1)]
    team_points[(nr_players+1),"symbol"] = common_symbols$symbol[nr_players]
    team_points[(nr_players+1),"color"] = common_symbols$color[nr_players]
    
    # symbols that wont match
    eigen_symbols <- db[(used_symbols+nr_players+1):(used_symbols+(nr_players*(1+nr_redundant_symbols))),]
    
    # each player gets nr_redundant_symbols
    team_points[((2*nr_players)+1):((2+nr_redundant_symbols)*(nr_players)),"symbol"] = eigen_symbols$symbol
    team_points[((2*nr_players)+1):((2+nr_redundant_symbols)*(nr_players)),"color"] = eigen_symbols$color
    
    # generating card for each player
    for (TeamMember in TeamContents) {
      makePlot(
        # we add color_numbers as a column for BnW
        team_points %>% filter(CategoryExternalName == TeamMember) %>% left_join(color_numbers, by="color"), 
        TeamMember,
        team_order,
        member_order,
        # assigning random number to the player (and output file name)
        tournamentmember_randomnumbers[tournamentmember_order],
        black_n_white
      )
      
      # increasing iterators
      member_order = member_order + 1
      tournamentmember_order = tournamentmember_order + 1
    }
    
    # increasing iterators
    used_symbols = used_symbols + (nr_players*(1+nr_redundant_symbols))
    team_order = team_order + 1
    
  }
  
}













