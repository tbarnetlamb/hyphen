
"""Simple wrapper to run another process and send it a keyboard
interrupt after a user-configurable delay; return+display the return
code of the subprocess. POSIX only.

"""

import subprocess, sys, time, signal

delay_before_interrupt_secs = int(sys.argv[1])
ps = subprocess.Popen(sys.argv[2:])
time.sleep(delay_before_interrupt_secs)
ps.send_signal(signal.SIGINT)
ps.wait()
print("Subprocess exited with code: {}".format(ps.returncode))
sys.exit(ps.returncode)
