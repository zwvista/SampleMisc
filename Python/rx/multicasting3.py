from rx import Observable
from random import randint


three_emissions = Observable.range(1, 3)

three_random_ints = three_emissions.map(lambda i: randint(1, 100000)).publish().auto_connect(2)

three_random_ints.subscribe(lambda i: print("Subscriber 1 Received: {0}".format(i)))
three_random_ints.subscribe(lambda i: print("Subscriber 2 Received: {0}".format(i))) # second subscriber triggers firing

'''
Subscriber 1 Received: 72527
Subscriber 2 Received: 72527
Subscriber 1 Received: 53066
Subscriber 2 Received: 53066
Subscriber 1 Received: 32249
Subscriber 2 Received: 32249
'''
