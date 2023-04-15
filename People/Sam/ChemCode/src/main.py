from convertMatrix import convert
from elementClass import E
from format import parse
from solveILP import solve

def main():
    c1 = [(E.H, 3), (E.P, 1), (E.O, 4)]
    c2 = [(E.N, 2), (E.H, 8), (E.Mo, 1), (E.O, 4)]
    c3 = [(E.H, 1), (E.N, 1), (E.O, 3)]
    c4 = [(E.N, 3), (E.H, 12), (E.P, 1), (E.O, 40), (E.Mo, 12)]
    c5 = [(E.N, 2), (E.H, 4), (E.O, 3)]
    c6 = [(E.H, 2), (E.O, 1)]

    r = ([c1, c2, c3], [c4, c5, c6])

    # modified from https://www.wikihow.com/Balance-Chemical-Equations-Using-Linear-Algebra
    # to follow assumptions of ChemCode
    assert parse("H3PO4 + N2H8MoO4 + HNO3 -> N3H12PO40Mo12 + N2H4O3 + H2O") == r

    for e in [
        ("H3PO4 + N2H8MoO4 + HNO3 -> N3H12PO40Mo12 + N2H4O3 + H2O", [1, 12, 21, 1, 21, 12]),
        ("O2 -> O3", [3, 2]),
        ("C2H6 + O2 -> CO2 + H2O", [2, 7, 4, 6]),
        ("KMnO4 + HCl -> MnCl2 + KCl + Cl2 + H2O", [2, 16, 2, 2, 5, 8]),
        ("Fe0.95O + O2 -> Fe2O3", [80, 17, 38]),
        ("C2H6 -> CO2 + H2O", None),
        ("K4FeC6N6 + K2S2O3 -> CO2 + K2SO4 + NO2 + FeS", None) # modified to follow assumptions
        ]:
        assert solve(convert(parse(e[0]))) == e[1]

if __name__ == '__main__':
    main()