from rx import Observable
from random import randint


three_emissions = Observable.range(1, 3)

three_random_ints = three_emissions.map(lambda i: randint(1, 100000))

three_random_ints.subscribe(lambda i: print("Subscriber 1 Received: {0}".format(i)))
three_random_ints.subscribe(lambda i: print("Subscriber 2 Received: {0}".format(i)))

'''
Subscriber 1 Received: 76795
Subscriber 1 Received: 19486
Subscriber 1 Received: 96809
Subscriber 2 Received: 79148
Subscriber 2 Received: 33423
Subscriber 2 Received: 3319
'''
