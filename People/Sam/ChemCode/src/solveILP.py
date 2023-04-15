from scipy.optimize import Bounds, LinearConstraint, milp

def solve(A):
	# weight vector
	c = [1] * len(A[0])

	constraints = LinearConstraint(A, [0] * len(A), [0] * len(A))

	return list(map(int, milp(c=c, constraints=constraints, integrality=[1] * len(A[0]), bounds=Bounds(lb=1)).x))
