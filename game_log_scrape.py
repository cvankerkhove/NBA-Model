"""
File name: game_log_scrape.py
Description: This file contains functions that use functions to scrape the site
    basketball-reference.com and collect box scores from any specified NBA
    game at a certain date in time.
Author: Chris VanKerkhove
"""
import requests
from bs4 import BeautifulSoup

def get_box_score(team, opponent, game, box_type):
    #TODO: Return who is inactive
    #refs?
    """
    Helper function that retrieves basic or advanced
    Box score stats for specificed team from boxscore
    page on basketball-reference.com

    Arg(s):
        team: string representation of team
        game: string of the game
        box_type: string that tells if its basic or advanced box score
    """
    URL = 'https://www.basketball-reference.com'
    URL = URL + game
    page = requests.get(URL)
    soup = BeautifulSoup(page.content, 'html.parser')
    ###PROCESS FOR RETRIEVING BOX SCORE
    #home box score basic content
    team_id = 'box-' + team + '-game-' + box_type
    results = soup.find(id = team_id)

    #Process for creating empty data structure
    #table header
    header = results.find('thead')
    key_dict ={}
    #home team data
    data = {'Team': team, 'Players': []}
    #columns in the header
    cols = header.find_all('th')
    for col in cols:
        key_dict[col.get('data-stat')] = col.text
        data[col.text] = []
    del data['']
    if box_type == 'basic':
        del data['Basic Box Score Stats']
    else:
        del data['Advanced Box Score Stats']
    del data['Starters']

    #Process for populating data tables
    #table with information
    body = results.find('tbody')

    t_data = body.find_all('tr')
    for row in t_data:
        if row.get('class') != ['thead']:
            player = row.find('th')
            data['Players'].append(player.text)
            col = row.find_all('td')
            for col in col:
                if col.get('data-stat') in key_dict.keys():
                    data[key_dict[col.get('data-stat')]].append(col.text)
    #getting stat team totals
    team_tot = {}
    footer = results.find('tfoot')
    row = footer.find_all('td')
    for x in row:
        team_tot[key_dict[x.get('data-stat')]] = x.text

    #TODO: getting team inactives
    x = soup.find('span')
    #TODO: getting opponent inactives

    # getting opponent totals
    o_id = 'box-' + opponent + '-game-' + box_type
    results = soup.find(id = o_id)
    opponent_tot = {}
    footer = results.find('tfoot')
    row = footer.find_all('td')
    for x in row:
        opponent_tot[key_dict[x.get('data-stat')]] = x.text

    return data, team_tot, opponent_tot


def team_log(team, start_date, end_date, year):
    """
    Takes an input of a string representation of a
    team and returns a dictionary of games with values being lists of info
    Arg(s):
        team: 3 letter string representation of Team
        start: start date to begin collecting games
            in the form of 'MM-DD-YYYY'
        end date: last game date to collect games
            in the form of 'MM-DD-YYYY'
        year: string of year of season YYYY
    """
    #helpful dictionaries
    num_to_str = {12: 'december', 1: 'january', 2: 'february', 3: 'march', 4: 'april', 5: 'may',
                     6: 'june', 7: 'july', 8: 'august', 9: 'september', 10: 'october', 11: 'november'}
    next_ = {'december': 'january', 'january': 'february', 'february': 'march', 'march': 'april',
           'april': 'may', 'may': 'june', 'june': 'july', 'july': 'august', 'august': 'september',
           'september' : 'october', 'october': 'november', 'november': 'december'}
    month_days = {'december': 31, 'january': 31, 'february': 28, 'march': 31}
    months = []
    game_logs = {}
    #finding the months over which to grab data
    start_month = num_to_str[int(start_date[:2])]
    end_month = num_to_str[int(end_date[:2])]
    current = start_month
    months.append(current)
    while current != end_month:
        current = next_[current]
        months.append(current)
    #starting dat (first month)
    start_day = int(start_date[3:5])
    #ending day (first month)
    if len(months) == 1:
        end_day = int(end_date[3:5])
    else:
        end_day = 31

    #iterating through different months
    stop = False
    while not stop:
        month = months[0]
        URL = 'https://www.basketball-reference.com/leagues/NBA_' + year + '_games-' + month + '.html'
        page = requests.get(URL)
        soup = BeautifulSoup(page.content, 'html.parser')

        #content
        results = soup.find(id = 'all_schedule')
        body = results.find('tbody')
        t_data = body.find_all('tr')

        #Grabbing all games for given month over specified days
        for row in t_data:
            #ensure row has data
            if row.get('class') != 'thead':
                teams = row.find_all('a')
                #ensure game has been played
                if len(teams) < 4:
                    break
                game = teams[3].get('href')
                #only games after specificed start date
                if start_day <= int(game[17:19]) <= end_day:
                    away = teams[1].get('href')[7:10]
                    home = teams[2].get('href')[7:10]
                    if team == home or team == away:
                        if team == home:
                            opponent = away
                        else:
                            opponent = home
                        visitor = (team == away)
                        print(game)
                        #retrieving basic data from game
                        b_data, b_tot, b_o_tot = get_box_score(team, opponent, game, 'basic')
                        #retrieving advanced data from game
                        a_data, a_tot, a_o_tot = get_box_score(team, opponent, game, 'advanced')
                        game_logs[game] = [visitor, b_data, b_tot, a_data, a_tot, b_o_tot, a_o_tot, team, opponent]

        #starting new month
        start_day = 1
        months.pop(0)
        if len(months) == 1:
            end_day = int(end_date[3:5])
        elif len(months) == 0:
            stop = True

    return game_logs
