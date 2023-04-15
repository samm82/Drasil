from regex import sub

from elementClass import E

def parse(eq):
	out = []
	eq = eq.split(" -> ")
	for i in [0, 1]:
		side = []
		sideRaw = eq[i].split(" + ")
		for j in range(len(sideRaw)):
			c = []
			# split letters and numbers
			cRaw = sub(r"([A-Z][a-z]?)([0-9]+)", r"\g<1>-\g<2>-", sideRaw[j])
			# split elements with no implicit subscript (and add it)
			cRaw = sub(r"([A-Z]|[a-z])(?=[A-Z]|$)", r"\g<1>-1-", cRaw)
			# split on added delimiter, removing empty strings
			cRaw = [x for x in cRaw.split('-') if x]
			for k in range(int(len(cRaw) / 2)):
				c.append((E[cRaw[2*k]], int(cRaw[2*k + 1])))
			side.append(c)
		out.append(side)
	return tuple(out)