-- Tests the "dead-code apply" defunctionalization problem.

result = add 10 20
add a b = a + b
dead f = f 10
