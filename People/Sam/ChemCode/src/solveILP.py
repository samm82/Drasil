from scipy.optimize import Bounds, LinearConstraint, milp

from convertMatrix import convert
from elementClass import E

c1 = [(E.H, 3), (E.P, 1), (E.O, 4)]
c2 = [(E.N, 2), (E.H, 8), (E.Mo, 1), (E.O, 4)]
c3 = [(E.H, 1), (E.N, 1), (E.O, 3)]
c4 = [(E.N, 3), (E.H, 12), (E.P, 1), (E.O, 40), (E.Mo, 12)]
c5 = [(E.N, 2), (E.H, 4), (E.O, 3)]
c6 = [(E.H, 2), (E.O, 1)]

r = ([c1, c2, c3], [c4, c5, c6])

A = convert(r)

# mat rep of chemical equation
# A = [[3, 8, 1, -12, -4, -2], [1, 0, 0, -1, 0, 0], [4, 4, 3, -40, -3, -1], [0, 2, 1, -3, -2, 0], [0, 1, 0, -12, 0, 0]]

# weight vector
c = [1] * len(A[0])

constraints = LinearConstraint(A, [0] * len(A), [0] * len(A))

res = list(map(int, milp(c=c, constraints=constraints, integrality=[1] * len(A[0]), bounds=Bounds(lb=1)).x))
assert res == [1, 12, 21, 1, 21, 12]
# print(res)
