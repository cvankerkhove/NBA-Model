"""
File name: csv_export.py
Description: This file contains the function for exporting the box score data
    into csv format. When this file is run it will update with todays most
    recent games
"""
from NBA_Games import get_games
from averages import Averages
import pandas as pd
import os


def export_season_stats(Team, Season):
    """
    """
    #string month key for accessing month directories
    month_key = {'01': 'January', '02': 'February', '03': 'March', '04': 'April',
           '05': 'May', '06': 'June', '07': 'July', '08': 'August', '09': 'September',
           '10' : 'October', '11': 'November', '12': 'December'}
    path = 'Seasons_Data/' + Season + '/' + Team
    if not os.path.exists(os.path.join(os.getcwd(), path)):
        os.makedirs(path)
    #getting games
    games = get_games(Team, '01-01', '12-31', Season)
    #adding all games to their corresponding file path
    for key,val in games.items():
        game_path = path + '/' + month_key[key[:2]] + '/' + key
        if not os.path.exists(os.path.join(os.getcwd(), game_path)):
            os.makedirs(game_path)
        #adding data to relevant directory
        val.basic_player_data.to_csv(game_path+ '/' + 'basic_player_data.csv', index=False)
        val.advanced_player_data.to_csv(game_path+ '/' +'advanced_player_data.csv', index=False)
        val.teams_basic.to_csv(game_path+ '/' +'teams_basic_data.csv', index=False)
        val.teams_advanced.to_csv(game_path+ '/' +'teams_advanced_data.csv', index=False)


'''
#updated 04/01
teams = ['PHI', 'MIL', 'CHO', 'MIA', 'NYK', 'ATL', 'BOS', 'IND', 'CHI', 'TOR',
          'WAS', 'CLE', 'ORL', 'DET', 'UTA', 'PHO', 'DEN', 'POR', 'DAL', 'BRK',
          'SAS', 'GSW', 'MEM', 'SAC', 'NOP', 'OKC', 'HOU', 'MIN', 'LAL', 'LAC']

for x in teams:
    export_season_stats(x, '2021')
'''
