using Convex, ECOS

function go()
    # A = [-1 -2  6  3 8;
    #      2  3 -2 -1 3]
    # x = Variable(2, Positive())

    A = [2 0 -2 0 3;
         0 5 -3 0 3;
         0 0 5 -1 8;
         0 -1 0 5 8]
    x = Variable(4, Positive())

    prob = maximize(sum(log((A' * x)[1:4])),
                    [sum(x) == 100,
                     (A' * x)[5] == 500
                     ])

    solve!(prob, ECOS.Optimizer)

    A' * round.(evaluate(x))
end

Int(prod(go()[1:4]))
