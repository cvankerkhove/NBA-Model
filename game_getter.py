from NBA_Games import get_games


g = get_games('LAL', '01-06-2006', '2006')

print(g.team, game.team_score)
print(g.opponent, game.o_score)
print(g.basic_player_data)
print(g.team_basic)
print(g.advanced_player_data)
print(g.o_basic)
print(g.o_advanced)
