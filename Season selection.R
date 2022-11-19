LeicesterWin <- EPL[EPL$year == '2015',]

LeicesterWin$game_index <- seq(from = 1, to = length(LeicesterWin$date))

print(sort(unique(LeicesterWin$team_home)))

LeicesterWin <- mutate(LeicesterWin, team_home_index = case_when(team_home == 'Arsenal'  ~ '1',
                                                                 team_home == 'Aston Villa'  ~ '2',
                                                                 team_home == 'Bournemouth'  ~ '3',
                                                                 team_home == 'Chelsea'  ~ '4',
                                                                 team_home == 'Crystal Palace'  ~ '5',
                                                                 team_home == 'Everton'  ~ '6',
                                                                 team_home == 'Leicester'  ~ '7',
                                                                 team_home == 'Liverpool'  ~ '8',
                                                                 team_home == 'Manchester City'  ~ '9',
                                                                 team_home == 'Manchester United'  ~ '10',
                                                                 team_home == 'Newcastle United'  ~ '11',
                                                                 team_home == 'Norwich'  ~ '12',
                                                                 team_home == 'Southampton'  ~ '13',
                                                                 team_home == 'Stoke'  ~ '14',
                                                                 team_home == 'Sunderland'  ~ '15',
                                                                 team_home == 'Swansea'  ~ '16',
                                                                 team_home == 'Tottenham'  ~ '17',
                                                                 team_home == 'Watford'  ~ '18',
                                                                 team_home == 'West Bromwich Albion'  ~ '19',
                                                                 team_home == 'West Ham'  ~ '20'))

LeicesterWin <- mutate(LeicesterWin, team_away_index = case_when(team_away == 'Arsenal'  ~ '1',
                                                                 team_away == 'Aston Villa'  ~ '2',
                                                                 team_away == 'Bournemouth'  ~ '3',
                                                                 team_away == 'Chelsea'  ~ '4',
                                                                 team_away == 'Crystal Palace'  ~ '5',
                                                                 team_away == 'Everton'  ~ '6',
                                                                 team_away == 'Leicester'  ~ '7',
                                                                 team_away == 'Liverpool'  ~ '8',
                                                                 team_away == 'Manchester City'  ~ '9',
                                                                 team_away == 'Manchester United'  ~ '10',
                                                                 team_away == 'Newcastle United'  ~ '11',
                                                                 team_away == 'Norwich'  ~ '12',
                                                                 team_away == 'Southampton'  ~ '13',
                                                                 team_away == 'Stoke'  ~ '14',
                                                                 team_away == 'Sunderland'  ~ '15',
                                                                 team_away == 'Swansea'  ~ '16',
                                                                 team_away == 'Tottenham'  ~ '17',
                                                                 team_away == 'Watford'  ~ '18',
                                                                 team_away == 'West Bromwich Albion'  ~ '19',
                                                                 team_away == 'West Ham'  ~ '20'))

