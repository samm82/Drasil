from elementClass import E

c1 = [(E.H, 3), (E.P, 1), (E.O, 4)]
c2 = [(E.N, 2), (E.H, 8), (E.Mo, 1), (E.O, 4)]
c3 = [(E.H, 1), (E.N, 1), (E.O, 3)]
c4 = [(E.N, 3), (E.H, 12), (E.P, 1), (E.O, 40), (E.Mo, 12)]
c5 = [(E.N, 2), (E.H, 4), (E.O, 3)]
c6 = [(E.H, 2), (E.O, 1)]

r = ([c1, c2, c3], [c4, c5, c6])

def elems(r):
	out = set()
	for i in [0, 1]:
		for c in r[i]:
			for e in c:
				out.add(e[0])
	return out

def count(e, c):
	for c_i in c:
		if c_i[0] == e:
			return c_i[1]
	return 0

def convert(r):
	out = []
	for e in elems(r):
		row = []
		for c in r[0]:
			row.append(count(e, c))
		for c in r[1]:
			row.append(-count(e, c))
		out.append(row)
	return out

print(convert(r))


A = [[3, 8, 1, -12, -4, -2], [1, 0, 0, -1, 0, 0], [4, 4, 3, -40, -3, -1], [0, 2, 1, -3, -2, 0], [0, 1, 0, -12, 0, 0]]
