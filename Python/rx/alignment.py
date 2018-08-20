from rx import Observable, Observer

xs = Observable.of(1,2,3)
ys = Observable.of(4,5,6)
zs = xs + ys  # Concatenate observables
zs.to_list().subscribe(lambda value: print(value))

xs = Observable.of(1,2,3)
ys = xs * 4
ys.to_list().subscribe(lambda value: print(value))

xs = Observable.of(1,2,3)
ys = xs[1:-1]
ys.to_list().subscribe(lambda value: print(value))

xs = Observable.of(1,2,3,4,5,6)
ys = xs.to_blocking()
zs = (x*x for x in ys if x > 3)
for x in zs:
    print(x)

'''
[1, 2, 3, 4, 5, 6]
[1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3]
[2]
16
25
36
'''
