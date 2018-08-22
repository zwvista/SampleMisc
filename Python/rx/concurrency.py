import multiprocessing
import random
import time
from threading import current_thread

from rx import Observable
from rx.concurrency import ThreadPoolScheduler


def intense_calculation(value):
    # sleep for a random short duration between 0.5 to 2.0 seconds to simulate a long-running calculation
    time.sleep(random.randint(5, 20) * .1)
    return value

# calculate number of CPU's, then create a ThreadPoolScheduler with that number of threads
optimal_thread_count = multiprocessing.cpu_count()
pool_scheduler = ThreadPoolScheduler(optimal_thread_count)

# Create Process 1
Observable.of("Alpha", "Beta", "Gamma", "Delta", "Epsilon") \
    .map(lambda s: intense_calculation(s)) \
    .subscribe_on(pool_scheduler) \
    .subscribe(on_next=lambda s: print("PROCESS 1: {0} {1}".format(current_thread().name, s)),
               on_error=lambda e: print(e),
               on_completed=lambda: print("PROCESS 1 done!"))

# Create Process 2
Observable.range(1, 10) \
    .map(lambda s: intense_calculation(s)) \
    .subscribe_on(pool_scheduler) \
    .subscribe(on_next=lambda i: print("PROCESS 2: {0} {1}".format(current_thread().name, i)),
               on_error=lambda e: print(e), on_completed=lambda: print("PROCESS 2 done!"))

# Create Process 3, which is infinite
Observable.interval(1000) \
    .map(lambda i: i * 100) \
    .observe_on(pool_scheduler) \
    .map(lambda s: intense_calculation(s)) \
    .subscribe(on_next=lambda i: print("PROCESS 3: {0} {1}".format(current_thread().name, i)),
               on_error=lambda e: print(e))

input("Press any key to exit\n")

'''
Press any key to exit
PROCESS 1: ThreadPoolExecutor-1_0 Alpha
PROCESS 1: ThreadPoolExecutor-1_0 Beta
PROCESS 2: ThreadPoolExecutor-1_1 1
PROCESS 3: ThreadPoolExecutor-1_2 0
PROCESS 2: ThreadPoolExecutor-1_1 2
PROCESS 1: ThreadPoolExecutor-1_0 Gamma
PROCESS 3: ThreadPoolExecutor-1_2 100
PROCESS 2: ThreadPoolExecutor-1_1 3
PROCESS 1: ThreadPoolExecutor-1_0 Delta
PROCESS 3: ThreadPoolExecutor-1_4 200
PROCESS 1: ThreadPoolExecutor-1_0 Epsilon
PROCESS 1 done!
PROCESS 2: ThreadPoolExecutor-1_1 4
PROCESS 2: ThreadPoolExecutor-1_1 5
PROCESS 3: ThreadPoolExecutor-1_5 300
PROCESS 2: ThreadPoolExecutor-1_1 6
PROCESS 3: ThreadPoolExecutor-1_2 400
PROCESS 3: ThreadPoolExecutor-1_2 500
PROCESS 2: ThreadPoolExecutor-1_1 7
PROCESS 3: ThreadPoolExecutor-1_2 600
PROCESS 2: ThreadPoolExecutor-1_1 8
PROCESS 3: ThreadPoolExecutor-1_2 700
PROCESS 2: ThreadPoolExecutor-1_1 9
PROCESS 3: ThreadPoolExecutor-1_2 800
PROCESS 2: ThreadPoolExecutor-1_1 10
PROCESS 2 done!
PROCESS 3: ThreadPoolExecutor-1_2 900
PROCESS 3: ThreadPoolExecutor-1_2 1000
PROCESS 3: ThreadPoolExecutor-1_2 1100
PROCESS 3: ThreadPoolExecutor-1_2 1200
PROCESS 3: ThreadPoolExecutor-1_2 1300
PROCESS 3: ThreadPoolExecutor-1_2 1400
PROCESS 3: ThreadPoolExecutor-1_2 1500
PROCESS 3: ThreadPoolExecutor-1_2 1600
PROCESS 3: ThreadPoolExecutor-1_2 1700
PROCESS 3: ThreadPoolExecutor-1_2 1800
...
'''
