from rx import Observable

Observable.interval(1000) \
    .map(lambda i: "{0} Mississippi".format(i)) \
    .subscribe(lambda s: print(s))

input("Press any key to quit\n")

'''
Press any key to quit
0 Mississippi
1 Mississippi
2 Mississippi
3 Mississippi
4 Mississippi
...
'''
