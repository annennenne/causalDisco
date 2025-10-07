### tetrad_graph() example ###

# A tiny 3-node PAG in Tetrad’s CSV-like string form:
# rows/cols are A,B,C; entries use pcalg’s PAG codes (0,1,2,3)
pag_txt <- "A,B,C
0,2,0
3,0,1
0,3,0
"

g <- tetrad_graph(pag_txt)

# Inspect the parsed object
class(g)
g$nodes
g$amat
