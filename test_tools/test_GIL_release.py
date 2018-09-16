"""Simple code to test GIL release during Haskell code. Spawn n
threads, each of which executes a threadDelay from Haskell of m
seconds, then join all the threads. If we are releasing the GIL, this
should only take m seconds, because the threadDelays all go in
parallel, but if we are holding the GIL, then the threadDelays are
sequenced and it will take nm seconds.

"""
import time, sys, threading

sys.path[0] = sys.path[0] + '/..'

import hyphen
import hs.Control.Concurrent

GIL_mode, num_threads = sys.argv[1], int (sys.argv[2])
per_thread_wait, min_time_allowed, max_time_allowed = map(float, sys.argv[3:])

def thread_worker():
    hs.Control.Concurrent.threadDelay(int(per_thread_wait * 1e6)).act()


assert GIL_mode in ["lazy", "fancy"]
getattr(hyphen.hslowlevel, 'set_GIL_mode_' + GIL_mode)()

threads = [threading.Thread(target=thread_worker)
           for i in range(num_threads)]

start_time = time.time()

for t in threads:
    t.start()

for t in threads:
    t.join()

time_taken = time.time() - start_time
print("All threads done after running for {:.2f}s".format(time_taken))

if min_time_allowed < time_taken  < max_time_allowed:
    print("OK")
    sys.exit(0)
else:
    print("Not OK.")
    sys.exit(1)
