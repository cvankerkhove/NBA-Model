from NBA_Games import get_games
from averages import Averages

games = get_games('OKC', '01-01', '01-31', '2011')

g1 = games.pop(list(games.keys())[0])
avg1 = Averages(g1)
for key,val in games.items():
    avg1.update_player_averages(val)

print(avg1.players_games_played)
print(avg1.basic_player_data)
print(avg1.advanced_player_data)
