from rx import Observable

letters = Observable.of("Alpha", "Beta", "Gamma", "Delta", "Epsilon")

intervals = Observable.interval(1000)

Observable.zip(letters, intervals, lambda s, i: (s, i)) \
    .subscribe(lambda t: print(t))

input("Press any key to quit\n")

'''
Press any key to quit
('Alpha', 0)
('Beta', 1)
('Gamma', 2)
('Delta', 3)
('Epsilon', 4)
'''
