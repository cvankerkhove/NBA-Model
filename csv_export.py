from NBA_Games import get_games
from averages import Averages
import pandas as pd
import os

###Getting Box score from Kobe 81 Points
#game = get_games('LAL', '02-22', '01-12', '2012')






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
    games = get_games(Team, '01-01', '03-31', Season)
    #adding all games to their corresponding file path
    for key,val in games.items():
        game_path = path + '/' + month_key[key[:2]] + '/' + key
        if not os.path.exists(os.path.join(os.getcwd(), game_path)):
            os.makedirs(game_path)
        #adding data to relevant directory
        val.basic_player_data.to_csv(game_path+ '/' + 'basic_player_data.csv', index=False)
        val.advanced_player_data.to_csv(game_path+ '/' +'advanced_player_data.csv', index=False)




game = get_games('LAL', '03-31', '03-31', '2021')
g = game[list(game.keys())[0]]
print(g.teams_basic)
print(g.teams_advanced)

#print(game)
'''
g = game[list(game.keys())[0]]
df = g.basic_player_data
df.to_csv('Outputs/kob.csv', index=False)
'''
#x = pd.DataFrame(g.team_basic, index = [0])
#x.to_csv('Outputs/kob_team.csv',index=False)
