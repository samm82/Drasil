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
