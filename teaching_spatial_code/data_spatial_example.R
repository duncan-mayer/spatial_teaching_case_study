# last updated 12/12/2021
library(tidyverse)
library(tidycensus)
library(tidybayes)
library(brms)

cuy_tracts <- tigris::tracts(state = "OH", county = "cuyahoga", year = 2016)
cuy_tracts$tract <- cuy_tracts$TRACTCE

'%!in%' <- function(x,y)!('%in%'(x,y))

# 
# bmf_16 <- read.csv("/Volumes/TOSHIBA_EXT/nccs_data/bmfs/bmf.bm1608.csv")
# names(bmf_16) <- tolower(names(bmf_16))
# bmf_16 <- bmf_16[bmf_16$fips == 39035 & is.na(bmf_16$fips) == FALSE,]

#(the first two being the state, next three the county, and the following six the tract)

# my first geo-coded try 
#orgs1 <- read.csv("/Users/duncanmayer/Desktop/dissertation_dat_2021/property_data_2016/geocoded_2016_OH_nonprofits.csv")

orgs <- read.csv("/Users/duncanmayer/Desktop/git_tries/geocode_nonprofits_cuyahoga/complete_cuyahoga_county/complete_2015_code.csv")[-1]
orgs <- orgs[!duplicated(orgs$ein),]
orgs <- orgs[orgs$year == 2016,]
orgs$tract <- as.character(orgs$tract_2015)
orgs <- orgs[!duplicated(orgs$ein),]

# -13, -26 : -.03 for all
orgs <- orgs[orgs$filer == "Y",]
dim(orgs)
# orgs <- orgs[orgs$zfiler == "N",]
# dim(orgs)
#orgs <- orgs[orgs$level2 != "S" & orgs$level1 != "PF",]
orgs <- orgs %>% filter(level1 %in% c("O", "PC") ) %>% filter(level2 != "S" )
##
# orgs$state <- substring(orgs$census_code, first = 1, last = 2)
# orgs$county <- substring(orgs$census_code, first = 3, last = 5)
# orgs$tract <- substring(orgs$census_code, first = 6, last = 11)

orgs <- orgs[orgs$tract %in% cuy_tracts$tract,]

orgs$nteefinal1 <- substring(orgs$nteefinal, first = 1, last =1)

### consider these also 
#I72 	Child Abuse Prevention 
# I73 	Sexual Abuse Prevention 
# O20 	Youth Centers & Clubs 
# O23 	Boys & Girls Clubs 
# P30 	Children & Youth Services 
# P32 	Foster Care 
# P33 	Child Day Care 
# P42 	Single Parent Agencies 
# P40 	Family Services 
# P62 	Victims Services
# P76 	Homes for Children & Adolescents 
###
# F20 	Substance Abuse Dependency, Prevention & Treatment 
# F21 	Substance Abuse Prevention 
# F22  Substance Abuse treatment 
plot_dat <- orgs %>% mutate(ntmaj5 = case_when(
  nteefinal1 %in% c("I", "J", "K", "L", "M", "N", "O", "P") ~ "Human Services",
  nteefinal1 %in% c("B") ~ "Education",
  nteefinal1 %in% c("X") ~ "Religion",
  nteefinal1 %in% c("C","D","Q","R","S","T","U","V","W","Y","Z") ~ "Other",
  nteefinal1 %in% c("A") ~ "Arts",
  nteefinal1 %in% c("E","F","G","H") ~ "Health"))

d16t <- orgs %>% group_by(tract) %>% summarise(human_services1 = sum(nteefinal1 %in% c("I", "J", "K", "L", "M", "N", "O", "P")),
                                               education1 = sum(nteefinal1 %in% c("B")),
                                               public1 = sum(nteefinal1 %in% c("R","S","T","U","V","W")),
                                               religion1 = sum(nteefinal1 %in% c("X")),
                                               oth_not_religion = sum(nteefinal1 %in% c("C","D","Q","R","S","T","U","V","W","Y","Z")),
                                               oth1 = sum(nteefinal1 %in% c("C","D","Q","R","S","T","U","V","W","X","Y","Z")),
                                               arts1 = sum(nteefinal1 %in% c("A")),
                                               health = sum(nteefinal1 %in% c("E","F","G","H")),
                                               total = n(), 
                                               substance_use_daycare = sum(nteefinal %in% c(
                                                 "F20","F21","F22","P33","P76","O20","O23")),
                                               wo_social_service_O = sum(nteefinal1 %in% c("O")),
                                               wo_social_service_other = sum(nteefinal %in% c("J21","J22","J20", "J30", "J32","J33",
                                                                                              "F20", "F21","F22","F30","F42","F50","F52","F53","F54",
                                                                                              "F60","F70","F80","I31","I40","I40","I43","I44",
                                                                                              "I70","I71","I72","I73")),
                                               wo_civic_t_r = sum(nteefinal1 %in% c("T","R")),
                                               wo_civic_other = sum(nteefinal %in% c("N20","N30","N31","N32", "N40","N50","I20",
                                                                                     "I21","I23","S20","S21","S22")),
                                               assets = sum(ifelse(cassets < 0, 0, cassets), na.rm = TRUE), rev =  sum(ifelse(ctotrev < 0, 0, ctotrev), na.rm = TRUE))


d16t <- d16t %>% mutate(social_service_wo = wo_social_service_O + wo_social_service_other,
                        civic_wo = wo_civic_t_r + wo_civic_other)
###


city <- readxl::read_excel("/Users/duncanmayer/Desktop/childmaltreatment_project/maltreat2.xlsx")

city$tract <- as.character(city$tract)
city$tract <- gsub("[.]","", as.character(city$tract))
city[nchar(city$tract) == 4,]$tract <- paste0(city[nchar(city$tract) == 4,]$tract, "00")
city[nchar(city$tract) == 5,]$tract <- paste0(city[nchar(city$tract) == 5,]$tract, "0")

#View(birth[,c("tract", "total_births", "addresses_total_q1", "total_violent_crime","total_violent_crime" )])
all <- dplyr::left_join(city, d16t ,by = c("tract"))

vars0 <- c("total","human_services1", "arts1","health","oth1","oth_not_religion", "public1", "religion1", "education1")

all[, vars0][is.na(all[,vars0])] <- 0

all <- all[all$tract %in% cuy_tracts$tract,]
### census dat 

#all <- all %>% rowwise() %>%  mutate(perc_vac = mean( vacantq1, vacantq2, vacantq3, vacantq4))
vars <- c(
  total_child_pop = "B09001_001",
  total_population = "B01001_001",
  # need
  median_income = "B19013_001",
  total_housing = "B25002_001",
  vacant_housing = "B25002_003",
  poverty_total = "B17001_001",
  poverty_below_100 = "B17001_002",
  ## labor force
  #male
  # total_employment = "B23001001",
  # civillian_1619m = B23001006,
  # unemployed_1619m = B23001008,
  # civillian_2021m = B23001013,
  # unemployed_2021m = B23001015,
  # civillian_2224m = B23001020,
  # unemployed_2224m = B23001022,
  # civillian_2529m = B23001027,
  # unemployed_2529m = B23001029,
  # civillian_3034m = B23001034,
  # unemployed_3034m = B23001036,
  # civillian_3544m = B23001041,
  # unemployed_3544m= B23001043,
  # civilian_4554m = B23001048,
  # unemployed_4554m= B23001050,
  # civillian_5559m = B23001055,
  # unemployed_5559m= B23001057,
  # civilian_6061m = B23001062,
  # unemployed_6061m = B23001064,
  # civillian_6264m = B23001069,
  # unemployed_6264m = B23001071,
  # 
  civilian_labor_force = "B23025_003",
  unemployed = "B23025_005",
  ## education 
  educational_total_25 = "B15003_001",
  hs_dip = "B15003_017",
  ged = "B15003_018",
  some_college = "B15003_019",
  some_college2 = "B15003_020",
  associates_degree = "B15003_021",
  bachelor_degree = "B15003_022",
  master_degree = "B15003_023",
  professional_degree = "B15003_024",
  doctorate_degree = "B15003_025",
  # race
  white = "B03002_003",
  black = "B03002_004",
  native = "B03002_005",
  asian = "B03002_006",
  latino = "B03002_012",
  ### mobility 
  mobility_total = "B07001_001",
  mobility_same1yr = "B07001_017",
  # ages
  male_under5 = "B01001_003",
  male_5_9 = "B01001_004",
  male_10_14 = "B01001_005",
  male_15_17 = "B01001_006",
  female_under5 = "B01001_027",
  female_5_9 = "B01001_028",
  female_10_14 = "B01001_029",
  female_15_17 = "B01001_030",
  ## seniors 
  male_65= "B01001_020",
  male_67= "B01001_021",
  male_70= "B01001_022",
  male_75= "B01001_023",
  male_80= "B01001_024",
  male_85= "B01001_025",
  # female
  female_65= "B01001_044",
  female_67= "B01001_045",
  female_70= "B01001_046",
  female_75= "B01001_047",
  female_80= "B01001_048",
  female_85= "B01001_049",
  # single parent households
  single_universe = "B09002_001",
  male_householder = "B09002_009",
  female_householder = "B09002_015",
  # owner occpied 
  owner_occ_tot = "B25003_001",
  owner_occ = "B25003_002",
  owner_occ_tot1 = "B07013_001",
  owner_occ2 = "B07013_002"
)

years <- lst(2016)

oh <- map_dfr(
  years,
  ~ get_acs(
    geography = "tract",
    variables = vars,
    state = "OH",
    county = "Cuyahoga",
    year = .x,
    survey = "acs5",
    geometry = FALSE
  ),
  .id = "year"
)

#oh <- oh %>% rowwise() %>% mutate(estimate = ifelse(estimate == 0, moe, estimate))

oh$moe <- NULL

oh$fips <- oh$GEOID
oh$state_code <- substring(oh$fips, first = 1, last = 2)
oh$tract <- substring(oh$GEOID, first = 6, last = nchar(oh$GEOID))

oh <- tidyr::pivot_wider(oh, names_from = variable,values_from = c(estimate)) %>% mutate(t = 2)


oh <- oh %>% rowwise() %>%  mutate(
  pop_under18 = 
    male_under5 + 
    male_5_9 +
    male_10_14 +
    male_15_17 +
    female_under5 +
    female_5_9 +
    female_10_14 +
    female_15_17, 
  voting_age_pop = total_population - pop_under18,
  ed_hs_or_greater =
    hs_dip +
    ged +
    some_college +
    some_college2 +
    associates_degree +
    bachelor_degree +
    master_degree +
    professional_degree +
    doctorate_degree,
  ed_college_or_greater = 
    bachelor_degree +
    master_degree +
    professional_degree +
    doctorate_degree,
  seniors =
    male_65 +
    male_67 +
    male_70 +
    male_75 +
    male_80 +
    male_85 +
    # female
    female_65 +
    female_67 +
    female_70 +
    female_75 +
    female_80 +
    female_85)

oh <- oh %>% rowwise() %>%   
  mutate(
    perc_poverty_100 = 
      poverty_below_100 / poverty_total,
    perc_mobile = 
      mobility_same1yr / mobility_total,
    perc_unemployed = 
      unemployed/  civilian_labor_force,
    perc_vac_census = 
      vacant_housing / total_housing,
    perc_senior = seniors / total_population,
    perc_asian = asian / total_population,
    perc_white = white / total_population,
    perc_black = black / total_population,
    perc_latinx = latino / total_population,
    perc_college_or_greater = ed_college_or_greater / educational_total_25,
    perc_hs_or_greater = ed_hs_or_greater / educational_total_25,
    perc_pop_under18 = pop_under18 / total_population)

oh <- oh %>%  rowwise() %>% mutate(single_households = male_householder +female_householder,
                                   perc_single = male_householder / single_universe,
                                   perc_owner = owner_occ/owner_occ_tot)

all <- left_join(all, oh, by = "tract")

vars0 <- c("perc_owner", "perc_single", "perc_college_or_greater", "perc_black", "perc_latinx",
           "perc_white", "perc_senior", "perc_vac_census","perc_unemployed", "perc_mobile",   "perc_poverty_100","median_income",
           "perc_pop_under18")

all[, vars0][is.na(all[,vars0])] <- 0


need <- all[,c("perc_college_or_greater", "perc_poverty_100", "perc_unemployed", "perc_single", "perc_pop_under18","median_income")] 
cor(need)
pr_need <- prcomp(need, scale = TRUE)
pr_need$rotation
ev <- pr_need$sdev^2
ev
all$need <- - pr_need$x[,1]
# 
housing <- all[,c("perc_mobile", "perc_vac_census","perc_owner")]
cor(housing)
pr_housing <- prcomp(housing, scale = TRUE)
pr_housing$rotation
ev <- pr_housing$sdev^2
ev
all$housing <- pr_housing$x[,1]

oh_tracts <- tigris::tracts(state = "OH", county = "Cuyahoga", cb = TRUE, year = 2017)
oh_tracts$tract <- substring(oh_tracts$GEOID, first = 6, last = nchar(oh_tracts$GEOID))
#oh_tracts$tracts <- oh_tracts$TRACTCE

### rewrite here
d_geo <- tigris::geo_join(oh_tracts, all, 'tract','tract')


w <- spdep::poly2nb(d_geo, row.names = d_geo$tract, queen = TRUE)
adj <- spdep::nb2mat(w, style = "B")
row.names(adj) <- d_geo$tract
### crime data


# mydir <- "/Users/duncanmayer/Desktop/dissertation_dat_2021/gun_violence/gun_violence_2016_located"
# 
# gv_files <-
#   list.files(path = mydir,
#              pattern = "*.csv", 
#              full.names = TRUE) %>% 
#   map_df(~read_csv(., col_types = cols(.default = "c"))) 
# 
# 
# gv <- bind_rows(gv_files)
# 
# gv$n_injured <-  as.numeric(gv$n_injured)
# gv$n_killed <-  as.numeric(gv$n_killed)
# gv$fips <- substring(gv$census_code, first = 1, last = 5)
# gv$tract <- substring(gv$census_code, first = 6, last = 11)
# agg_gv <- gv %>% filter(fips == 39035) %>% group_by(tract) %>% 
#   summarise(
#     events = n(),
#     injured = sum(n_injured),
#     n_killed = sum(n_killed))
###
## new tracts 
###
agg_gv <- read.csv("/Users/duncanmayer/Desktop/git_tries/dissertation-hedonic-study/hedonic_study_data/gun_violence_cuyahoga_agg_tracts_2015.csv")[-1]
agg_gv <- agg_gv %>% filter(year == 2016)

dim(agg_gv)
# tigris::lookup_code(state = "OH", county ="Cuyahoga")
#cuy_gv <- gv[gv$fips == 39035,] 


agg_gv <- agg_gv[agg_gv$tract %in% all$tract,]
rm(gv)
rm(gv_files)
rm(mydir)
###  join crime 
agg_gv$tract <- as.character(agg_gv$tract_2015)
mod <- dplyr::left_join(all, agg_gv, by = "tract")

# fill_0 <- function(data, col_label) {
#   data[is.na(data$col_label), ]$col_label <- 0  
# }

mod[is.na(mod$events),]$events <- 0

### cleveland 
#dcle <- d[is.na(d$total_violent_crime) == FALSE,]
# 
# 
#library(tmap)
# 
#tm_shape(d_geo) +
# tm_polygons("events")

#d <- d[is.na(d$total_violent_crime) == FALSE,]
mod <- mod[mod$total_population > 0,]
#mod <- mod[mod$total_child_pop > 0,]

d_geo <- d_geo[d_geo$tract %in% mod$tract,]
### create indicies 
# need
#library(tmap)
# 
#tm_shape(d_geo) +
# tm_polygons("events")

vars0 <- c("tract","total_population","rev",
           # nonprofits
           "human_services1","education1","public1","religion1","oth1","arts1","health","total",
           # need 
           "need","perc_poverty_100")
           
           
           # "perc_owner", "perc_college_or_greater", "perc_black", "perc_latinx", "perc_single_female",
           # "perc_white", "perc_senior", "perc_vac","perc_unemployed", "perc_mobile",   "perc_poverty_100","median_income",
           # "perc_pop_under18","perc_single") #ADI <- what to do about places with 0 pop?


mod[is.na(mod$total),]$total <- 0
mod[is.na(mod$human_services1),]$human_services1 <- 0
mod[is.na(mod$civic_wo),]$civic_wo <- 0
mod[is.na(mod$social_service_wo),]$social_service_wo <- 0
mod[is.na(mod$civic_wo),]$civic_wo <- 0
mod[is.na(mod$substance_use_daycare),]$substance_use_daycare <- 0

mod <- mod %>% mutate(total_less_substance = total - substance_use_daycare)

mod <- mod %>% rowwise() %>% mutate(t_child = sum(substantiated, indicated, unsubstantiated))

names(mod)

