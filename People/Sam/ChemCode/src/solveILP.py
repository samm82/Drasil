from scipy.optimize import Bounds, LinearConstraint, milp

# mat rep of chemical equation
A = [[3, 8, 1, -12, -4, -2], [1, 0, 0, -1, 0, 0], [4, 4, 3, -40, -3, -1], [0, 2, 1, -3, -2, 0], [0, 1, 0, -12, 0, 0]]

# weight vector
c = [1] * len(A[0])

constraints = LinearConstraint(A, [0] * len(A), [0] * len(A))

res = list(map(int, milp(c=c, constraints=constraints, integrality=[1] * len(A[0]), bounds=Bounds(lb=1)).x))
print(res)
