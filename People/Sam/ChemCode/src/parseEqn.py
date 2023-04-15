from regex import sub

from elementClass import E

def parse(eq):
	eq = eq.split(" -> ")
	for i in [0, 1]:
		eq[i] = eq[i].split(" + ")
		for j in range(len(eq[i])):
			# split letters and numbers
			eq[i][j] = sub(r"([A-Z][a-z]?)([0-9]+)", r"\g<1>-\g<2>-", eq[i][j])
			# split elements with no implicit subscript (and add it)
			eq[i][j] = sub(r"([A-Z]|[a-z])(?=[A-Z]|$)", r"\g<1>-1-", eq[i][j])
	print(eq)

parse("H3PO4 + N2H8MoO4 + HNO3 -> N3H12PO40Mo12 + N2H4O3 + H2O")