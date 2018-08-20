from rx import Observable

source = Observable.of("Alpha", "Beta", "Gamma", "Delta", "Epsilon")

source.subscribe(lambda value: print("Received {0}".format(value)))

'''
Received Alpha
Received Beta
Received Gamma
Received Delta
Received Epsilon
'''
