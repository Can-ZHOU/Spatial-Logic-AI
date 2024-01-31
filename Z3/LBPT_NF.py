import z3

# Create an uninterpreted sort D
D = z3.DeclareSort('D')

# Create functions of sort D -> D -> Bool
NEAR = z3.Function('NEAR', D, D, z3.BoolSort())
FAR = z3.Function('FAR', D, D, z3.BoolSort())


# Create variables of sort D
x = z3.Const('x', D)
y = z3.Const('y', D)
#z = z3.Const('z', D)
#t = z3.Const('t', D)


# Create a solver instance
solver = z3.Solver()

# set unsat core generation
solver.set(unsat_core=True)



# add axioms
# forall x. NEAR(x, x)
solver.add(z3.ForAll([x], NEAR(x, x)))
# forall x, y. NEAR(x, y) -> NEAR(y, x)
solver.add(z3.ForAll([x, y], z3.Implies(NEAR(x, y), NEAR(y, x))))
# forall x, y. FAR(x, y) -> FAR(y, x)
solver.add(z3.ForAll([x, y], z3.Implies(FAR(x, y), FAR(y, x))))
# forall x, y. NEAR(x, y) -> not FAR(x, y)
solver.add(z3.ForAll([x, y], z3.Implies(NEAR(x, y), z3.Not(FAR(y, x)))))


# add relations
a1 = z3.Const('a1', D)
a2 = z3.Const('a2', D)
a3 = z3.Const('a3', D)
a4 = z3.Const('a4', D)
solver.assert_and_track(NEAR(a1, a2), "1")
solver.assert_and_track(FAR(a2, a3), "2")
solver.assert_and_track(z3.Not(NEAR(a3, a3)), "3")
solver.assert_and_track(NEAR(a3, a4), "4")
solver.assert_and_track(FAR(a4, a3), "5")
solver.assert_and_track(z3.Not(FAR(a3, a2)), "6")
solver.assert_and_track(z3.Not(NEAR(a2, a1)), "7")
#solver.assert_and_track(W(x2, x3), "4")
#solver.assert_and_track(z3.Not(dW(x1, x3)), "3")
#solver.assert_and_track(z3.Not(sW(x1, x3)), "4")


    

# Check for satisfiability
result = solver.check()

print(result)
#print(z3.sat)
#print(result==z3.sat)
#print(solver.unsat_core())

# Print the result
if (result == z3.sat):
    print("sat")
    print(solver.model())
    #print("sat")
else:
    print("unsat")
    print(solver.unsat_core())
