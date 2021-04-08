"""
File name: user_functions.py
Description: This file contains the functions that provide user functionality
    with their input parameters.
"""
from NBA_Games import get_games
from averages import Averages



def get_averages(Team, Season, start = None, end = None):
    """
    Creates and updates an abject of class Averages for a given season. If just
    required arguments given, retrives the averages object for the entire season
    and if season selected is current season, gets averages of season so far up
    to current day. Returns the populated averages object

    Arg(s):
        Team: Three letter string abbreviation of team
        season: String representation of the year of this season
        start: String for starting date of averages 'MM-DD'
        end: String for ending date of averages "MM-DD"
    """
    #case where range is input
    if start == None or end == None:
        #get games for input range
        games = get_games(Team, start, end, Season)
    #case where no range, getting full season, or up until today if current season
    else:
        games = get_games(Team, '01-01', '12-31', Season)
    game = games.pop(list(games.keys())[0])
    avg = Averages(game)
    #iterating through games and updating averages
    for key,val in games.items():
        avg.update_player_averages(val)
        avg.update_team_avg(val)
    return avg

def print_all_players_averages(Team, Season, start = None,
                                end = None, type='basic'):
    """
    Prints all players Averages on a team for a period.
        If player is did not play
    for that team in the specified stretch and error message will return.
        If a start and end range are not specified, it will print the player averages for
    the entire season.
        If type is not specified it will return basic data averages

    Arg(s):
        Team: Three letter string abbreviation of team
        season: String representation of the year of this season
        start: String for starting date of averages 'MM-DD'
        end: String for ending date of averages "MM-DD"
        type: 'Basic', 'advanced', or 'both'
    """
    #cases for if input range is specifed
    if start ==None or end == None:
        games = get_games(Team, '01-01', '12-31', Season)
    else:
        #get games for input range
        games = get_games(Team, start, end, Season)

    game = games.pop(list(games.keys())[0])
    avg = Averages(game)
    #iterating through games and updating averages
    for key,val in games.items():
        avg.update_player_averages(val)
        avg.update_team_avg(val)

    print('Games Played: ', avg.players_games_played)

    if type == 'basic':
        print('Basic Stats: ' )
        print(avg.basic_player_data)
    elif type == 'advanced':
        print('Advanced Stats: ')
        print(avg.advanced_player_data)
    #any other string returns both box scores
    else:
        print('Basic Stats: ')
        print(avg.basic_player_data)
        print('Advanced Stats: ')
        print(avg.advanced_player_data)


def print_teams_averages(Team, Season, start = None, end = None, type='basic'):
    """
    Prints the averages for a team and its opponenets over a specified stretch.
    Same situation as print players for if default parameters not used

    Arg(s):
        Team: Three letter string abbreviation of team
        Season: String representation of the year of this season
        start: String for starting date of averages 'MM-DD'
        end: String for ending date of averages "MM-DD"
        type: 'Basic', 'advanced', or 'both'
    """
    #cases for if input range is specifed
    if start ==None or end == None:
        games = get_games(Team, '01-01', '12-31', Season)
    else:
        #get games for input range
        games = get_games(Team, start, end, Season)
    n = len(games)
    game = games.pop(list(games.keys())[0])
    avg = Averages(game)
    #iterating through games and updating averages
    for key,val in games.items():
        avg.update_player_averages(val)
        avg.update_team_avg(val)

    print('Games: ', n)
    #printing data
    if type == 'basic':
        print('Basic Stats: ' )
        print(avg.teams_basic)
    elif type == 'advanced':
        print('Advanced Stats: ')
        print(avg.teams_advanced)
    #any other string returns both box scores
    else:
        print('Basic Stats: ')
        print(avg.teams_basic)
        print('Advanced Stats: ')
        print(avg.teams_advanced)

def print_player_averages(Team, Season, player, start = None,
                         end = None, type='basic'):
    """
    Prints a players Averages on a team for a period.
    Same situation as print players for if default parameters not used

    Arg(s):
        Team: Three letter string abbreviation of team
        Season: String representation of the year of this season
        player: String of Specified Player
        start: String for starting date of averages 'MM-DD'
        end: String for ending date of averages "MM-DD"
        type: 'Basic', 'advanced', or 'both'
    """
    #cases for if input range is specifed
    if start ==None or end == None:
        games = get_games(Team, '01-01', '12-31', Season)
    else:
        #get games for input range
        games = get_games(Team, start, end, Season)

    game = games.pop(list(games.keys())[0])
    avg = Averages(game)
    #iterating through games and updating averages
    for key,val in games.items():
        avg.update_player_averages(val)
        avg.update_team_avg(val)
    try:
        #try block for if player played on specified team over interval
        print('Games Played: ', avg.players_games_played[player])
        is_player = avg.basic_player_data['Players'] == player
        if type == 'basic':
            print('Basic Stats: ' )
            print(avg.basic_player_data[is_player])
        elif type == 'advanced':
            print('Advanced Stats: ')
            print(avg.advanced_player_data[is_player])
        #any other string returns both box scores
        else:
            print('Basic Stats: ')
            print(avg.basic_player_data[is_player])
            print('Advanced Stats: ')
            print(avg.advanced_player_data[is_player])
    except:
        print(player + ' didnt play on team ' + Team + ' in given range')
        print('Double check spelling of player or that Team exists')


def print_players_box_score(Team, Season, date, type = 'basic'):
    """
    Prints players stats box score for a specified team on a specified date.
    If team did not play on specified date, error message will print.

    Arg(s):
        Team: Three letter string abbreviation of team
        season: String representation of the year of this season
        date: date of game in 'MM-DD' format
        type: type of box score
    """
    #cases for if input range is specifed
    games = get_games(Team, date, date, Season)
    #if there was a game on given date
    if len(games) > 0:
        game = games[list(games.keys())[0]]
        if type == 'basic':
            print('Basic Data: ')
            print(game.basic_player_data)
        elif type == 'advanced':
            print('Advanced Data: ')
            print(game.advanced_player_data)
        else:
            print('Basic Data: ')
            print(game.basic_player_data)
            print('Advanced Data: ')
            print(game.advanced_player_data)
    else:
        print('No game for' + Team + 'On' + date)

def print_teams_box_score(Team, Season, date, type = 'basic'):
    """
    Prints a teams stats box score on a specified date.
    If team did not play on specified date, error message will print.

    Arg(s):
        Team: Three letter string abbreviation of team
        season: String representation of the year of this season
        date: date of game in 'MM-DD' format
        type: type of box score
    """
    #cases for if input range is specifed
    games = get_games(Team, date, date, Season)
    #if there was a game on given date
    if len(games) > 0:
        game = games[list(games.keys())[0]]
        if type == 'basic':
            print('Basic Data: ')
            print(game.basic_player_data)
        elif type == 'advanced':
            print('Advanced Data: ')
            print(game.advanced_player_data)
        else:
            print('Basic Data: ')
            print(game.basic_player_data)
            print('Advanced Data: ')
            print(game.advanced_player_data)
    else:
        print('No game for' + Team + 'On' + date)
