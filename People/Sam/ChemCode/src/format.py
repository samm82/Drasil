from regex import sub

from elementClass import E

def splitEq(eq):
    out = []
    eq = eq.split(" -> ")
    for i in [0, 1]:
        side = eq[i].split(" + ")
        out.append(side)
    return out


def parse(eq):
    eq = splitEq(eq)
    for i in [0, 1]:
        for j in range(len(eq[i])):
            c = []
            # split letters and numbers
            cRaw = sub(r"([A-Z][a-z]?)([0-9]+(\.[0-9]+)?)", r"\g<1>-\g<2>-", eq[i][j])
            # split elements with no implicit subscript (and add it)
            cRaw = sub(r"([A-Z]|[a-z])(?=[A-Z]|$)", r"\g<1>-1-", cRaw)
            # split on added delimiter, removing empty strings
            cRaw = [x for x in cRaw.split('-') if x]
            for k in range(int(len(cRaw) / 2)):
                c.append((E[cRaw[2*k]], float(cRaw[2*k + 1])))
            eq[i][j] = c
    return tuple(eq)

def process(eq, out):
    if out == None:
        return "The inputted equation is infeasible."
    out = iter(out)
    eq = splitEq(eq)
    for i in [0, 1]:
        for j in range(len(eq[i])):
            coeff = next(out)
            if coeff != 1:
                eq[i][j] = str(coeff) + eq[i][j]
        eq[i] = " + ".join(eq[i])
    return " -> ".join(eq)

