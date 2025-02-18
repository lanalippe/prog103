library(marinecs100b)


# Review: write a function ------------------------------------------------

# P1 Describe succinctly what the following code does. This should be a
# high-level, one-sentence description, not a line-by-line breakdown.
#The code is defining the site as Nuka Pass and the season as late winter. Its
#telling us the average hours per day that the temperature is below -4 degrees.

site <- "Nuka_Pass"
season <- "Late winter"
n_cold <- sum(kefj_site == site &
                kefj_season == season &
                kefj_temperature <= -4 &
                kefj_exposure == "air")
n_total <- sum(kefj_site == site &
                 kefj_season == season)
hours_cold <- n_cold * 30 / 60
days_total <- n_total * 30 / 60 / 24
hours_cold_per_day <- hours_cold / days_total
hours_cold_per_day

# P2 Let's turn that code chunk into a function. What would you call that
# function? How many parameters should it take and what would you call them?

cold_temp <- function(site, season) {
  n_cold <- sum(kefj_site == site &
                  kefj_season == season &
                  kefj_temperature <= -4 &
                  kefj_exposure == "air")
  n_total <- sum(kefj_site == site &
                   kefj_season == season)
  hours_cold <- n_cold * 30 / 60
  days_total <- n_total * 30 / 60 / 24
  hours_cold_per_day <- hours_cold / days_total

  return(hours_cold_per_day)
}


# P3 Write a function to encapsulate the code chunk above. Check that it
# contains all five parts of a function.

cold_temp <- function(site, season) {
  n_cold <- sum(kefj_site == site &
                  kefj_season == season &
                  kefj_temperature <= -4 &
                  kefj_exposure == "air")
  n_total <- sum(kefj_site == site &
                   kefj_season == season)
  hours_cold <- n_cold * 30 / 60
  days_total <- n_total * 30 / 60 / 24
  hours_cold_per_day <- hours_cold / days_total
  return(hours_cold_per_day)
}
cold_temp("Nuka_Pass", "Late winter")

# Make an extreme choice --------------------------------------------------

# P4 Fill in the code below to create a logical vector indicating extreme
# temperatures.

extreme_type <- "cold"
if (extreme_type == "cold") {
  is_extreme <- kefj_temperature <= -4
} else if (extreme_type == "hot") {
  extreme_type <- kefj_temperature >= 25
}else {
  print("no")
}


# P5 Copy-paste the code from P1 and edit it to incorporate the is_extreme
# vector into the extreme temperature exposure procedure.

extreme_type <- "cold"
if (extreme_type == "cold") {
  is_extreme <- kefj_temperature <= -4
} else if (extreme_type == "hot") {
  extreme_type <- kefj_temperature >= 25
}else {
  print("no")
}

site <- "Nuka_Pass"
season <- "Late winter"
n_cold <- sum(kefj_site == site &
                kefj_season == season &
               is_extreme &
                kefj_exposure == "air")
n_total <- sum(kefj_site == site &
                 kefj_season == season)
hours_cold <- n_cold * 30 / 60
days_total <- n_total * 30 / 60 / 24
hours_cold_per_day <- hours_cold / days_total
hours_cold_per_day


# P6 Copy-paste the function you wrote in P3 and edit it to add a parameter that
# lets you switch between extreme heat and cold exposure.

extreme_temp <- function(site, season, extreme_type) {
  if (extreme_type == "cold") {
    is_extreme <- kefj_temperature <= -4
  }else if(extreme_type == "hot") {
    is_extreme <- kefj_temperature >= 25
  }else {
      print("no")
    }
  n_cold <- sum(kefj_site == site &
                  kefj_season == season &
                 is_extreme &
                  kefj_exposure == "air")
  n_total <- sum(kefj_site == site &
                   kefj_season == season)
  hours_cold <- n_cold * 30 / 60
  days_total <- n_total * 30 / 60 / 24
 result <- hours_cold / days_total
  return(result)
}
extreme_temp("Nuka_Pass", "Summer" , "hot")

# Season to taste ---------------------------------------------------------

# P7 What seasons are in the kefj dataset? What function would you use to
# identify them?
?kefj_season
#late winter, summer, Spring, Fall, Early Winter

# P8 Fill in the blanks below to make a for loop that prints the extreme hot and
# cold exposure across seasons at site Aialik.

unique_season <- unique(kefj_season)
seasons <- rep(0, length(unique_season))
  for (i in 1 : length (unique_season)) {
    heat_exposure <- extreme_temp("Aialik", unique_season[i], "hot")
    cold_exposure <- extreme_temp("Aialik", unique_season[i], "cold")
    print(paste("Aialik", unique_season, heat_exposure, cold_exposure))
}

# P9 Copy-paste your answer to P8 and add a nested for loop to iterate across
# sites as well as seasons.
unique_site <- unique(kefj_site)
unique_season <- unique(kefj_season)
seasons <- rep(0, length(unique_season))
for (i in 1 : length (unique_season))
  for (i in 1 : length (unique_site)){
  heat_exposure <- extreme_temp("Harris", unique_season[i], "hot")
  cold_exposure <- extreme_temp("Harris", unique_season[i], "cold")
  print(paste("Aialik", unique_season, heat_exposure, cold_exposure))
}



# P10 Examine your results from P9. You should find two outputs where both
# extreme heat and cold exposure were 0. What season were they in?

# "Aialik Summer 0 0"       "Aialik Fall 0 0"         "Aialik Early winter 0 0" "Aialik Late winter 0 0"
#"Aialik Spring 0 0"
