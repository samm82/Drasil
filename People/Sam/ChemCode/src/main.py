from convertMatrix import convert
from elementClass import E
from parseEqn import parse
from solveILP import solve

def main():
	# modified from https://www.wikihow.com/Balance-Chemical-Equations-Using-Linear-Algebra
	# to follow assumptions of ChemCode
	eq = "H3PO4 + N2H8MoO4 + HNO3 -> N3H12PO40Mo12 + N2H4O3 + H2O"

	c1 = [(E.H, 3), (E.P, 1), (E.O, 4)]
	c2 = [(E.N, 2), (E.H, 8), (E.Mo, 1), (E.O, 4)]
	c3 = [(E.H, 1), (E.N, 1), (E.O, 3)]
	c4 = [(E.N, 3), (E.H, 12), (E.P, 1), (E.O, 40), (E.Mo, 12)]
	c5 = [(E.N, 2), (E.H, 4), (E.O, 3)]
	c6 = [(E.H, 2), (E.O, 1)]

	r = ([c1, c2, c3], [c4, c5, c6])

	assert parse(eq) == r

	# mat rep of chemical equation
	A = convert(parse(eq))
	# A = [[3, 8, 1, -12, -4, -2], [1, 0, 0, -1, 0, 0], [4, 4, 3, -40, -3, -1], [0, 2, 1, -3, -2, 0], [0, 1, 0, -12, 0, 0]]

	res = solve(A)
	assert res == [1, 12, 21, 1, 21, 12]
	# print(res)

	for e in [
		("O2 -> O3", [3, 2]),
		("C2H6 + O2 -> CO2 + H2O", [2, 7, 4, 6]),
		("KMnO4 + HCl -> MnCl2 + KCl + Cl2 + H2O", [2, 16, 2, 2, 5, 8]),
		("Fe0.95O + O2 -> Fe2O3", [80, 17, 38])
		]:
		assert solve(convert(parse(e[0]))) == e[1]

if __name__ == '__main__':
	main()