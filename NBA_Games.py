import pandas as pd
from game_log_scrape import team_log

class Game_Info:
    #class variables:
    #date: string of the date of game
    #team: 3 letter string of team
    #loc: location of the game for team (home or away)
    #team_score: score of the given team
    #o_score: score of opponent
    #team_basic: basic stats total for team
    #team_advanced: advanced stats total for team

    #if player_data() is called
    #basic_player_data: df of basic stats for all player
    #advanced_player_data: df of advanced stats for all players
    def __init__(self, game_log, date):
        """
        Arg(s):
            game_log: game log from a single game for a team
            date: string of date of the game
        """
        self.date = date
        self.team = game_log[7]
        self.opponent = game_log[8]
        if game_log[0]:
            self.loc = 'Away'
        else:
            self.loc = 'Home'
        #score of each team
        #team score
        self.team_score = int(game_log[2]['PTS'])
        #opponent score
        self.o_score = int(game_log[5]['PTS'])
        #team stats
        self.team_basic = game_log[2]
        self.team_advanced = game_log[4]
        #opponent stats
        self.o_basic = game_log[5]
        self.o_advanced = game_log[6]

    def player_data(self, game_log):
        """
        Populates class variables with pandas
        dataframe objects of the stats
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




#Script for getting games
def get_games(team, date, end_date ,year):
    # script to create list of game_info objects for a single team
    #log = team_log(team, date, date)
    log = team_log(team, date, end_date, year)
    games = {}

    for key, val in log.items():
        date = key[11:19]
        date = date[4:6] + '-' + date[6:] + '-' + date[:4]
        game = Game_Info(val, date)
        game.player_data(val)
        games[date] = game
        #print(game.team, game.team_score)
        #print(game.opponent, game.o_score)
        #print(game.basic_player_data)
        #print(game.team_basic)
        #print(game.advanced_player_data)
        #print(game.o_basic)
        #print(game.o_advanced)func('LAL', '01-06-2006', '2006')
    return games
