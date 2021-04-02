"""
File name: playground.py
Description: File to mess around with pre-written intervals of averages to get
"""
from NBA_Games import get_games
from averages import Averages

###Getting Season Averages for Lakers up to current point in current season
games = get_games('LAL', '12-01', season = '2021')

g1 = games.pop(list(games.keys())[0])
avg1 = Averages(g1)
for key,val in games.items():
    avg1.update_player_averages(val)
    avg1.update_team_avg(val)
#player data
print(avg1.players_games_played)
print(avg1.basic_player_data)
print(avg1.advanced_player_data)
#team data
print(avg1.teams_basic)
print(avg1.teams_advanced)


##Getting Averages for James Harden over 'Flamethrower stretch'
games = get_games('HOU', '12-13', '02-21', season = '2019')
g1 = games.pop(list(games.keys())[0])
avg1 = Averages(g1)
for key,val in games.items():
    avg1.update_player_averages(val)
    avg1.update_team_avg(val)

#player data
print(avg1.players_games_played['James Harden'])
is_james = avg1.basic_player_data['Players'] == 'James Harden'
bd = avg1.basic_player_data[is_james]
ad = avg1.advanced_player_data[is_james]
print(ad)
print(bd)
#team data
print(avg1.teams_basic)
print(avg1.teams_advanced)


###Getting Box score from Kobe 81 Points
game = get_games('LAL', '01-22', '01-22', '2006')
g = game[list(game.keys())[0]]
print(g.basic_player_data)
print('Teams basic Stats', g.teams_basic)
print('Teams advanced Stats', g.teams_advanced)
