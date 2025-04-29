library(tidyverse)
library(rvest)

# We'll scrape every meet competed at by any of these teams
# The list contains all 40 teams which were ranked at some point in the 2024 season
# https://www.ustfccca.org/team-rankings-polls-central/polls-and-rankings-week-by-week?season=2024&sport=3&div=2032&pritype=491

datasets = c() # list of all results datasets

scrape_meet <- function(url, index_on_page){
  html <- url %>%
    read_html()
  
  data <- html_elements(html, "table")[index_on_page][1] %>% html_table() %>% as.data.frame()
  
  return(data)
}

add_to_scrape <- function(df, url, index_on_page){
  data <- rbind(df, c(url, index_on_page))
  return(data)
}

to_scrape <- data.frame() 

## 1. All MIT Meets

# Maribel Sanchez Souther Invite results
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24416/Maribel_Sanchez_Souther_Invite", 1)

# D3 Pre-Nationals results
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23323/D3_Pre-Nationals#event159305", 1)

# Connecticut College Invitational results
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24515/Connecticut_College_Invitational#event161132", 5)

# 2024 NEWMAC Championships results
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24719/2024_NEWMAC_Championships", 1)

# NCAA Division III East Region Cross Country Championships results
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25055/NCAA_Division_III_East_Region_Cross_Country_Championships", 1)

## 2. All U Chicago Meets

# Twilight Invite results
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24067/Saints_Twilight_Invite", 1)

# ONU Midwest Classic results
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24782/ONU_Midwest_Classic", 1)

# John McNichols Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23545/John_McNichols_Invitational", 1)

# Interregional Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23323/D3_Pre-Nationals", 3)

# 2024 UAA XC Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24493/2024_UAA_XC_Championships", 1)

# Chiburg 5k 
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25313/Chiburg_5k", 3)

# WILLIAMS 

# Little 3 Invite
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24579/Little_3_Invite", 1)

# Purple Valley XC Invite
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24793/Purple_Valley_XC_Invite", 1 )

# RPI HARVEST CLASSIC XC IMVITE
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24883/RPI_HARVEST_CLASSIC_XC_IMVITE", 1 )

# Keene State College Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25013/Keene_State_College_Invitational", 1 )

# James Earley Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24851/James_Earley_Invitational", 1 )

# 2024 NESCAC Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25200/2024_NESCAC_Championships", 1 )

# NYU

# Vassar Season Starter 2024
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24071/Vassar_Season_Starter_2024",1 )

# Vassar Ron Stonitsch Invitational 2024
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24072/Vassar_Ron_Stonitsch_Invitational_2024", 3 )

# Lehigh Paul Short Run (College)
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24390/Lehigh_Paul_Short_Run_College", 1 )

# NCAA Division III Niagara Region Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25057/NCAA_Division_III_Niagara_Region_Cross_Country_Championships", 3 )

# JHU

# 28th ANNUAL SALISBURY UNIVERSITY FALL CLASSIC
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24015/28th_ANNUAL_SALISBURY_UNIVERSITY_FALL_CLASSIC", 1 )

# Main Line Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24356/Main_Line_Invitational", 3)

# Interregional Border Battle
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23963/Interregional_Border_Battle", 1)

# Centennial Conference Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24999/Centennial_Conference_Cross_Country_Championships", 1)


# ECAC DIII Mid Atlantic XC Championship
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25099/ECAC_DIII_Mid_Atlantic_XC_Championship", 1 )

# NCAA Division III Mid-Atlantic Region Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25058/NCAA_Division_III_Mid-Atlantic_Region_Cross_Country_Championships", 1)

# 17th Annual UCCS XC Rust-Buster 6109 Elevation
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23866/17th_Annual_UCCS_XC_Rust-Buster_6109_Elevation", 1)

# Ted Castaneda XC Classic
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23689/Ted_Castaneda_XC_Classic", 1)

# 2024 Colorado Springs XC Open
# THIS ONLY HAS INDIVIDUAL RESULTS
# to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23867/2024_Colorado_Springs_XC_Open", 0 )

# Southern Collegiate Athletic Conference XC Championship
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25077/Southern_Collegiate_Athletic_Conference_XC_Championship", 3)

# NCAA Division III West Region Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25064/NCAA_Division_III_West_Region_Cross_Country_Championships", 1)

# EMORY

# Kennesaw State Stan Sims Cross Country Opener
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24107/Kennesaw_State_Stan_Sims_Cross_Country_Opener", 3)

# Queen City Invite
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23757/Queen_City_Invite", 1)

# Georgia Tech XC Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23967/Georgia_Tech_XC_Invitational", 1)

# Upstate Invite
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23715/Upstate_Invite_", 1)

# NCAA Division III South Region Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25061/NCAA_Division_III_South_Region_Cross_Country_Championships", 1)


# WASHINGTON AND LEE

# Eagle Cross Country Challenge
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23395/Eagle_Cross_Country_Challenge", 1) 

# Virginia Tech Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23980/Virginia_Tech_Invitational_", 1)

# Dickinson Long-Short Invitational
# THIS IS ONLY A TEAM EVENT
#to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24029/Dickinson_Long-Short_Invitational_",)

# Royals Challenge
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23758/Royals_Challenge", 5)

# Roanoke XC Invitational/ODAC Preview
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24760/Roanoke_XC_Invitational_Preview",1)

# 2024 ODAC Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24876/2024_ODAC_Cross_Country_Championships",3)


# SUNY GENESSO

# Fisher-Rodenbeck Invite
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23752/Fisher-Rodenbeck_Invite", 3)

# Yellowjacket XC Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23622/Yellowjacket_XC_Invitational",3)

# Harry F. Anderson Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24150/Harry_F_Anderson_Invitational",3)

# Empire 8 XC Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24624/Empire_8_XC_Championships", 3)

# 2024 Kara Hall Memorial Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24204/2024_Kara_Hall_Memorial_Invitational",3)

# SUNY Geneseo Mike Woods Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24490/SUNY_Geneseo_Mike_Woods_Invitational",3)

# WASNHINTON CO.
# IS THIS EAST CENTRAL?


# CLAREMONT-MUDD-SCRIPTS
# Mark Covert Classic
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24057/Mark_Covert_Classic", 3)

# 43rd Annual UCR Cross Country Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23821/43rd_Annual_UCR_Cross_Country_Invitational", 3)

# The Master's University Cross Country Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23548/_The_Masters_University_Cross_Country_Invitational", 1)

# 2024 Pomona-Pitzer XC Invite
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24459/2024_Pomona-Pitzer_XC_Invite", 1)

# 2024 SCIAC XC Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25210/2024_SCIAC_XC_Championships", 1)

# RPI

# Liberty League Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24878/Liberty_League_Championships", 3)

# NCAA Division III Mideast Region Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25056/NCAA_Division_III_Mideast_Region_Cross_Country_Championships", 1)

# WHAT IS UW-La Crosse

# AMHERST 

# Wesleyan University XC Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24583/Wesleyan_University_XC_Invitational", 1)

# CALVIN

# Calvin Knight Invite
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23769/Calvin_Knight_Invite", 1)

# Michigan Intercollegiate Athletic Association Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25265/Michigan_Intercollegiate_Athletic_Association_Championships", 1)

# NCAA Division III Great Lakes Region Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25060/NCAA_Division_III_Great_Lakes_Region_Cross_Country_Championships", 1)


# TUFTS

# CBBT Opener
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24197/CBBT_Opener", 3)

# 2024 NEICAAA Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24695/2024_NEICAAA_Cross_Country_Championships", 1)

# ST OLAF

# St. Olaf Ole Opener
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24477/St_Olaf_Ole_Opener", 1)

# St. Olaf Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24797/St_Olaf_Invitational", 1)

# Carleton Running of the Cows
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25015/Carleton_Running_of_the_Cows", 1)

# Drews/Neubauer Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25188/Drews_Neubauer_Invitational", 1)

# MIAC Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25267/MIAC_Championships", 1)

# Cannon River Closer/AECMR
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25288/Cannon_River_Closer_AECMR", 1)

# NCAA Division III North Region Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25062/NCAA_Division_III_North_Region_Cross_Country_Championships", 1)

# CARLETON

# UWRF Rivertown Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24581/UWRF_Rivertown_Invitational", 3)

# Linfield George Oja Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24330/Linfield_George_Oja_Invitational", 3)

# UC SANTA CRUZ 

# San Francisco Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24470/San_Francisco_Invitational", 3)

# Stump Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24187/Stump_Invitational", 3)

# D3 Pre-Nationals
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23323/D3_Pre-Nationals", 3)

# Santa Clara Bronco Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24164/Santa_Clara_Bronco_Invitational_", 1)

# Coast-to-Coast Athletic Conference Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23539/Coast-to-Coast_Athletic_Conference_Cross_Country_Championships", 1)

# VASSAR

# GEORGE FOX 

# Linfield Harrier Classic
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24270/Linfield_Harrier_Classic_", 3)

# 2024 PLU Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23365/2024_PLU_Invitational", 1)

# Mike Johnson Classic
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24563/Mike_Johnson_Classic", 3)

# L&C Invite
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25037/LC_Invite", 1)

# 2024 Northwest Conference Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24610/2024_Northwest_Conference_Cross_Country_Championships",1)


# MIDDLEBURY


# Aldrich Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24706/Aldrich_Invitational", 1 )

# Hoffmann Invitational / Liberty League Preview
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23355/Hoffmann_Invitational___Liberty_League_Preview", 3)

# CONNECTICUT

# Marist Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24085/Marist_Invitational", 3)

# Harry Groves Spiked Shoe Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24693/Harry_Groves_Spiked_Shoe_Invitational", 1)

# 12th Annual Bruce Kirsh Cross Country Cup
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23489/12th_Annual_Bruce_Kirsh_Cross_Country_Cup", 1)

# Wisconsin Pre-Nationals
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25189/Wisconsin_Pre-Nationals", 5)

# 2024 BIG EAST Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25152/2024_BIG_EAST_Cross_Country_Championships", 1)

# WESLEYAN

# CARNIGIE MELLON

# Panthers Opener
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24462/Panthers_Opener", 3)

# Robert Morris University Colonial Cross Country Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23648/Robert_Morris_University_Colonial_Cross_Country_Invitational", 1 )

# CMU Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25118/CMU_Invitational", 1)

# WARTBURG

# Mount Mercy Mustang Gallop
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24065/_Mount_Mercy_Mustang_Gallop", 1 )

# John Kurtt Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24504/John_Kurtt_Invitational", 3 )

# Dan Huston Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24829/Dan_Huston_Invitational", 3 )

# American Rivers Conference Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24926/American_Rivers_Conference_Championships", 1 )

# Saga Cup
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25130/Saga_Cup", 3 )

# LYNCHBURG

# 2024 Liberty XC Challenge
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23795/2024_Liberty_XC_Challenge", 1 )

# adidas XC Challenge
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24784/adidas_XC_Challenge", 1 )

# Roanoke XC Invitational/ODAC Preview
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24760/Roanoke_XC_Invitational_ODAC_Preview", 1)

# TRINE

# Franklin College Grizzly XC Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23615/Franklin_College_Grizzly_XC_Invitational_", 1)

# 2024 Auto-Owners Spartan Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23546/2024_Auto-Owners_Spartan_Invitational", 3)

# 2024 Mastodon Alumni Open
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23500/2024_Mastodon_Alumni_Open", 3 )


# DEPAUW 

# Gil Dodds Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/24497/Gil_Dodds_Invitational", 1 )


# COAST GUARD


# Trinity College Bantam Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23747/Trinity_College_Bantam_Invitational", 1)

# UMass Dartmouth Cross Country Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23936/UMass_Dartmouth_Cross_Country_Invitational", 5 )

# ROWAN

# 2024 Herb Lorenz XC Invitational
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/23749/2024_Herb_Lorenz_XC_Invitational_", 3 )

# NJAC Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25206/NJAC_Cross_Country_Championships",  3 )

# NCAA Division III Metro Region Cross Country Championships
to_scrape <- add_to_scrape(to_scrape,"https://www.tfrrs.org/results/xc/25059/NCAA_Division_III_Metro_Region_Cross_Country_Championships", 3 ) 


# drop duplicates
to_scrape <- unique(to_scrape)


# get the datasets 
for(i in 1:nrow(to_scrape)) {
  data <- scrape_meet(toString(to_scrape[i,1]), as.integer(to_scrape[i,2]) )
  datasets[[i]] <- data
}



## Wrangling the datasets into one

adj_mat_list <- list()

for(i in 1:length(datasets)){
  df <- datasets[[i]]
  adj_matrix <- df %>%
    select(PL, Team) %>%
    mutate(team2 = Team) %>%
    pivot_wider(names_from = team2, values_from = PL)

  for(row in 1:nrow(adj_matrix)){
    for(col in 2:(row + 1)){
      adj_matrix[row, col] <- 1
    }
    for(col in (row + 1):ncol(adj_matrix)){
      adj_matrix[row, col] <- 0
    }
  }
  mat <- data.matrix(adj_matrix %>% select(-1))
  dimnames(mat) <- list(adj_matrix[[1]], adj_matrix[[1]])
  adj_mat_list[[i]] <- mat
}


schools <- c()
c <- 1
for(i in 1:length(adj_mat_list)){
  for(school in colnames(adj_mat_list[[i]])){
    if(!(school %in% schools)){
      schools[c] <- school
      c = c + 1
    }
  }
}


mat <- matrix(0L, length(schools), length(schools))
dimnames(mat) <- list(schools, schools)

for(i in 1:length(adj_mat_list)){
  i_matrix <- adj_mat_list[[i]]
  for(row in 1:nrow(i_matrix)){
    for(col in 1:ncol(i_matrix)){
      mat[rownames(i_matrix)[row], rownames(i_matrix)[col]] <- mat[rownames(i_matrix)[row], rownames(i_matrix)[col]] + i_matrix[row, col]
    }
  }
}

mat # final adjacency matrix


# get list of all D3 programs from TFRRS
d3_schools <- scrape_meet("https://www.tfrrs.org/leagues/51.html", 1)[[2]]

temp_mat <- as.data.frame(mat)

# order is enforced by in operator, so dont need to order
# keep cols for schools
names.use1 <- names(temp_mat)[(names(temp_mat) %in% d3_schools)]
temp_mat<- temp_mat[, names.use1]
# get only the rows we care about
final <- temp_mat |>
  rownames_to_column() |>
  filter(rowname %in% d3_schools) |>
  select(-c("rowname"))

dimnames(final) <- list(colnames(final), colnames(final))


write_csv(final, "/Users/violetross/Desktop/Network Science/Project/Alexis Work/running_all_d3.csv")


totalDegree <- final %>% colSums() %>% as.data.frame() %>% rename("count" = ".")
