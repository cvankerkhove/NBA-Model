"""
File name: averages.py
Description: This file contains the class defintion of Averages, which stores
    the team,opponent,and team player averages as class variables in pandas df.
    There are class functions that help add games to the averages present in
    the object.
Author: Chris VanKerkhove
"""

class Averages:
    #Initializing an object of this class with an object of class game_info
    #will populate the class variables as dictionaries, remove unecesary columns
    #and convert the type of data from string to float (if necessary)

    #class variables: (see __init__ for further descriptions)
    #game_count
    #team_basic
    #team_advanced
    #op_basic
    #op_advanced
    #basic_player_data
    #advanced_player_data
    #player_games_played
    def __init__(self, game):
        """
        Arg(s):
            game: an object of class Game_Info for a single game
                (note this game should be the first you want to start averaging
                from)
        """
        ###Initializing team averages from input game###
        #the number of games this average represents
        self.game_count = 1
        #team, basic averages
        self.team_basic = {}
        del game.team_basic['+/-']
        for key, val in game.team_basic.items():
            self.team_basic[key] = float(val)
        #team, advanced averages
        self.team_advanced = {}
        del game.team_advanced['BPM']
        for key, val in game.team_advanced.items():
            self.team_advanced[key] = float(val)
        #opponents, basic averages
        self.op_basic = {}
        del game.op_basic['+/-']
        for key, val in game.op_basic.items():
            self.op_basic[key] = float(val)
        #opponents, advanced averages
        self.op_advanced = {}
        del game.op_advanced['BPM']
        for key, val in game.op_advanced.items():
            self.op_advanced[key] = float(val)

        ###Initializing player averages from input games###

        ##Basic Player Data
        #removing unecessary columns
        b_game = game.basic_player_data.drop(['FG%', '3P%', 'FT%'], axis=1)
        names = b_game.pop('Players')
        #converting time string
        MP = b_game.pop('MP')
        MP2 = []
        for time in MP:
            if len(time) < 5:
                min = int(time[:1])
                sec = int(time[2:])
            else:
                min = int(time[:2])
                sec = int(time[3:])
            MP2.append(round(min + (sec/60), 2))
        #converting numerical data
        b_game = b_game.astype(float)
        #Pandas DataFrame of players averages (basic stats)
        self.basic_player_data = b_game
        self.basic_player_data.insert(0, 'Players', names)
        self.basic_player_data.insert(1, 'MP', MP2)

        ##Advanced Player Data
        a_game = game.advanced_player_data.drop(['MP'], axis=1)
        names = a_game.pop('Players')
        #converting empty values to 0
        a_game = a_game.replace('', 0, regex=True)
        #converting to numerical data
        a_game = a_game.astype(float)
        #Pandas DataFrame of players averages (advanced stats)
        self.advanced_player_data = a_game
        self.advanced_player_data.insert(0, 'Players', names)

        #games played dicitonary for averages
        self.players_games_played = {}
        for p in names:
            self.players_games_played[p] = 1




    def update_team_avg(self, game):
        """
        Updates the averages of the teams stats and opponents stats (basic and
        advanced) based on the single input game
        Arg(s):
            game: an object of class Game_Info that you want to average in
        """

        #updating team basic averages
        del game.team_basic['+/-']
        for key, val in game.team_basic.items():
            self.team_basic[key] = (float(val) + (self.team_basic[key] * \
                                      self.game_count)) / (self.game_count + 1)
        #updating team advanced averages
        del game.team_advanced['BPM']
        for key, val in game.team_advanced.items():
            self.team_advanced[key] = (float(val) + (self.team_advanced[key] * \
                                      self.game_count)) / (self.game_count + 1)

        #updating oponent basic averages
        del game.op_basic['+/-']
        for key, val in game.op_basic.items():
            self.op_basic[key] = (float(val) + (self.op_basic[key] * \
                                      self.game_count)) / (self.game_count + 1)
        #updating opponents, advanced averages
        del game.op_advanced['BPM']
        for key, val in game.op_advanced.items():
            self.op_advanced[key] = (float(val) + (self.op_advanced[key] * \
                                      self.game_count)) / (self.game_count + 1)

        self.game_count += 1

    def update_player_averages(self, game):
        """
        Updates the averages of the players basic stats on given team, adds player to
        data container if players averages are not yet recorded for given games
        Arg(s):
            game: an object of class Game_Info that you want to average in
        """
        #dictionary providing the index of players in the current pandas dataframe
        names_ord = {k: n for n, k in enumerate(self.basic_player_data['Players'])}

        ##Basic Player Data Processing
        #removing unecessary columns
        b_game = game.basic_player_data.drop(['FG%', '3P%', 'FT%'], axis=1)
        #players names from new data
        names = b_game.pop('Players')
        #converting time string
        MP = b_game.pop('MP')
        MP2 = []
        for time in MP:
            if len(time) < 5:
                min = int(time[:1])
                sec = int(time[2:])
            else:
                min = int(time[:2])
                sec = int(time[3:])
            MP2.append(round(min + (sec/60), 2))
        #converting numerical data
        b_game = b_game.astype(float)
        #b_game.insert(0, 'Players', names)
        b_game.insert(1, 'MP', MP2)

        ##Advanced Player Data Processing
        #removing unecesary columns
        a_game = game.advanced_player_data.drop(['MP'], axis=1)
        names = a_game.pop('Players')
        #converting empty values to 0
        a_game = a_game.replace('', 0, regex=True)
        #converting to numerical data
        a_game = a_game.astype(float)

        ##Updating Averages
        for n,p in enumerate(names):
            #case where this player has already had a game in these averages
            if p in names_ord:
                for c in b_game.columns:
                    #basic averages update
                    tot1 = self.basic_player_data[c][names_ord[p]] * self.players_games_played[p]
                    tot1 += b_game[c][n]
                    self.basic_player_data[c][names_ord[p]] = tot1 / (self.players_games_played[p]+1)
                for c in a_game.columns:
                    #advanced averages update
                    tot2 = self.advanced_player_data[c][names_ord[p]] * self.players_games_played[p]
                    tot2 += a_game[c][n]
                    self.advanced_player_data[c][names_ord[p]] = tot2 / (self.players_games_played[p]+1)
                self.players_games_played[p] += 1
            #case otherwise
            else:
                b_data = {'Players': p}
                a_data = {'Players': p}
                for c in b_game.columns:
                    b_data[c] = b_game[c][n]
                for c in a_game.columns:
                    a_data[c] = a_game[c][n]
                self.players_games_played[p] = 1
                #adding new players to averages dataframe
                self.basic_player_data = self.basic_player_data.append(b_data, ignore_index = True)
                self.advanced_player_data = self.advanced_player_data.append(a_data, ignore_index=True)


    def update_player_basic_averages(self, game):
        """
        Updates the averages of the player advanced stats on given team,
        adds player to data container if players averages are not yet recorded
        for given games
        Arg(s):
            game: an object of class Game_Info that you want to average in
        """
        ##Advanced Player Data
        a_game = game.advanced_player_data
        #players names from new data
        names = b_game.pop('Players')
