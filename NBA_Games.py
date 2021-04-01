"""
File name: NBA_Games.py
Description: This file contains the class defintion of Game_Info which stores
    information regarding a single NBA games box score and player data for a
    specified team. Also contains a local function that gets game utilizing the
    webscraper file and populates Game_Info objects.
Author: Chris VanKerkhove
"""
import pandas as pd
from game_log_scrape import team_log

class Game_Info:
    #class variables: (see __init__ for further descriptions)
    #date
    #team
    #opponent
    #loc
    #team_score
    #op_score
    #team_basic
    #team_advanced
    #op_basic
    #op_advanced

    #if player_data() is called:
    #basic_player_data
    #advanced_player_data
    def __init__(self, game_log, date):
        """
        Arg(s):
            game_log: game log from a single game for a team
            date: string of date of the game
        """
        #string of the date of game
        self.date = date
        #3 letter string of team
        self.team = game_log[7]
        self.opponent = game_log[8]
        #location of the game for team (home or away)
        if game_log[0]:
            self.loc = 'Away'
        else:
            self.loc = 'Home'
        #score of each team
        #team score
        self.team_score = int(game_log[2]['PTS'])
        #opponent score
        self.op_score = int(game_log[5]['PTS'])
        #team stats
        self.team_basic = game_log[2]
        self.team_advanced = game_log[4]
        #opponent stats
        self.op_basic = game_log[5]
        self.op_advanced = game_log[6]

    def player_data(self, game_log):
        """
        Populates class variables with pandas
        dataframe objects of the stats

        Arg(s):
            game_log: game log from a single game for a team
        """
        #basic player data
        b_data = game_log[1]
        b_data['Players'] = b_data['Players'][:len(b_data['MP'])]
        b_data.pop('Team', None)
        df = pd.DataFrame(b_data)
        self.basic_player_data = df

        #advanced player data
        a_data = game_log[3]
        a_data['Players'] = a_data['Players'][:len(a_data['MP'])]
        a_data.pop('Team', None)
        df = pd.DataFrame(a_data)
        self.advanced_player_data = df



def get_games(team, start_date, end_date ,season):
    """
    Gets games calling webscraping functions. Begin collecting games from input
    start date to input end date in a specified season.
    Returns a dictionary of
    Game_Info objects with keys being the date of corresponding game

    Arg(s):
        team: string of the input teams 3 letter abbreviation
        start_date: The starting date to begin collecting (ex: '12-25')
        end_date: The ending date to collect last game on
        season: The year of specificed season (single year, year season ends in)
    """
    # script to create list of game_info objects for a single team
    #log = team_log(team, date, date)
    log = team_log(team, start_date, end_date, season)
    games = {}

    for key, val in log.items():
        date = key[11:19]
        date = date[4:6] + '-' + date[6:] + '-' + date[:4]
        game = Game_Info(val, date)
        game.player_data(val)
        games[date] = game
    return games
