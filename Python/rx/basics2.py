from rx import Observable, Observer

class PrintObserver(Observer):

    def on_next(self, value):
        print("Received {0}".format(value))

    def on_completed(self):
        print("Done!")

    def on_error(self, error):
        print("Error Occurred: {0}".format(error))

source = Observable.of("Alpha", "Beta", "Gamma", "Delta", "Epsilon")

source.subscribe(PrintObserver())

'''
Received Alpha
Received Beta
Received Gamma
Received Delta
Received Epsilon
Done!
'''
