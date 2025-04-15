"""
File name: sandbox.py
Description: A file to run and call user functions for viewing statistics
"""
import time
import user_functions as func
from NBA_Games import get_games
from csv_export import export_season_stats

##Getting Averages for James Harden over 'Flamethrower stretch'
#print('James Harden 12-13 to 02-05')
#func.print_player_averages('HOU', '2019', 'James Harden', '12-13', '02-21')

##Nets Players Averages this Season##
#print('2021 Brooklyn Nets')
#func.print_all_players_averages('BRK', '2021')

#print('2021 Lakers Team averages')
#func.print_teams_averages('LAL', '2021')

###Kobe 81 Point Game###
#print('Kobe 81 Point Game')
#func.print_players_box_score('LAL', '2006', '01-22')


print('Scrapping some recent games')
export_season_stats('PHO', '2024')


print('done')
