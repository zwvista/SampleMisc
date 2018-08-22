from rx import Observable
from random import randint


three_emissions = Observable.range(1, 3)

three_random_ints = three_emissions.map(lambda i: randint(1, 100000)).publish()

three_random_ints.subscribe(lambda i: print("Subscriber 1 Received: {0}".format(i)))
three_random_ints.subscribe(lambda i: print("Subscriber 2 Received: {0}".format(i)))

three_random_ints.connect()

'''
Subscriber 1 Received: 83159
Subscriber 2 Received: 83159
Subscriber 1 Received: 82025
Subscriber 2 Received: 82025
Subscriber 1 Received: 72397
Subscriber 2 Received: 72397
'''

