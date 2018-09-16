import time, sys

sys.path[0] = sys.path[0] + '/..'
start_time = time.time()

import hyphen
import hs.Control.Concurrent

load_time_taken = time.time() - start_time
print("Hyphen loaded after running for {:.2f}s".format(load_time_taken))

signal_mode = sys.argv[1]
assert signal_mode in ["lazy", "haskell", "python"]
getattr(hyphen.hslowlevel, 'set_signal_mode_' + signal_mode)()
wait_seconds, min_total_time_allowed_postload, max_total_time_allowed = map(float, sys.argv[2:])

try:
    hs.Control.Concurrent.threadDelay(int(wait_seconds * 1e6)).act()
except KeyboardInterrupt:
    pass

time_taken = time.time() - start_time
print("Interrupted after running for {:.2f}s".format(time_taken))

if min_total_time_allowed_postload + load_time_taken < time_taken  < max_total_time_allowed:
    sys.exit(0)
else:
    sys.exit(1)
