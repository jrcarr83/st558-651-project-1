ST558-651 Project 1
================

-   [Set Up Data Scrape](#set-up-data-scrape)
    -   [Base links](#base-links)
    -   [Get List of Teams](#get-list-of-teams)
    -   [Check Team ID](#check-team-id)
-   [Functions](#functions)
    -   [Record Functions](#record-functions)
    -   [Stat Functions](#stat-functions)
    -   [Wrapper Function](#wrapper-function)
-   [Summarizing](#summarizing)
    -   [Playoffs by Total Games
        Played](#playoffs-by-total-games-played)

## Set Up Data Scrape

### Base links

### Get List of Teams

### Check Team ID

## Functions

### Record Functions

### Stat Functions

### Wrapper Function

## Summarizing

### Playoffs by Total Games Played

``` r
#let's look at all franchises via franchise totals
data <- get_nhl_data('Franchise Totals')

#let's look at active franchises first
#and pivot wider so that playoffs and reg season 
#are on the same line of data
franchises <- data %>% filter(activeFranchise == 1) %>%
                mutate(game_type = if_else(gameTypeId == 2, 
                                           'regular', 'playoff')) %>%
                select(franchiseId, teamName, gamesPlayed, 
                       wins, losses, game_type) %>%
                pivot_wider(names_from = game_type, values_from = 
                              c(gamesPlayed, wins, losses))


franchises$win_pct_playoff <- franchises$wins_playoff / 
                        (franchises$wins_playoff + franchises$losses_playoff)

g <- ggplot(data=franchises, aes(x=gamesPlayed_regular, y=win_pct_playoff)) +
      geom_point(aes(name=teamName))
```

    ## Warning: Ignoring unknown aesthetics: name

``` r
p <- ggplotly(g)
```

<iframe src="C:/Users/jrcar/OneDrive/Documents/558/ST-558/st558-651-project-1/index.html" width="100%" height="600" scrolling="no" seamless="seamless" frameBorder="0"></iframe>
