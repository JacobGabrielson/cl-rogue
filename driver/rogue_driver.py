"""
rogue_driver.py — Headless driver for cl-rogue.

Spawns the game in a PTY so it sees a real terminal (raw mode, termios,
TIOCGWINSZ all work).  Output is fed into a pyte VT100 screen so we can
read the virtual display at any time without a physical terminal.

Usage:
    driver = RogueDriver("../cl-rogue", rows=24, cols=80)
    driver.start()
    driver.wait_stable()
    print(driver.snapshot_str())
    driver.send_key("l")   # move right
    driver.wait_stable()
    row, col = driver.find_player()
    driver.close()
"""

import os
import pty
import fcntl
import struct
import termios
import threading
import time
import pyte


class RogueDriver:
    def __init__(self, binary="../cl-rogue", rows=24, cols=80, idle_secs=0.15):
        self.binary = os.path.abspath(binary)
        self.rows = rows
        self.cols = cols
        self.idle_secs = idle_secs   # seconds of silence before wait_stable() returns

        self.screen = pyte.Screen(cols, rows)
        self.stream = pyte.ByteStream(self.screen)

        self._master_fd = None
        self._pid = None
        self._reader_thread = None
        self._lock = threading.Lock()
        self._last_output_time = 0.0
        self._running = False

    # ------------------------------------------------------------------ #
    # Lifecycle                                                            #
    # ------------------------------------------------------------------ #

    def start(self):
        """Fork the game into a PTY and start the background reader."""
        self._pid, self._master_fd = pty.fork()

        if self._pid == 0:
            # Child: set terminal size then exec the game.
            # STDOUT_FILENO = 1 (slave PTY is already stdin/stdout/stderr)
            self._set_winsize(1, self.rows, self.cols)
            os.execv(self.binary, [self.binary])
            os._exit(1)  # unreachable

        # Parent: make master non-blocking and start reader thread.
        fl = fcntl.fcntl(self._master_fd, fcntl.F_GETFL)
        fcntl.fcntl(self._master_fd, fcntl.F_SETFL, fl | os.O_NONBLOCK)

        self._running = True
        self._last_output_time = time.monotonic()
        self._reader_thread = threading.Thread(target=self._reader, daemon=True)
        self._reader_thread.start()

    def close(self):
        """Terminate the game and clean up."""
        self._running = False
        try:
            import signal
            os.kill(self._pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        try:
            os.close(self._master_fd)
        except OSError:
            pass
        if self._reader_thread:
            self._reader_thread.join(timeout=2)

    def __enter__(self):
        self.start()
        return self

    def __exit__(self, *_):
        self.close()

    # ------------------------------------------------------------------ #
    # Input                                                                #
    # ------------------------------------------------------------------ #

    def send_key(self, key):
        """Send a single key (or escape sequence string) to the game."""
        data = key.encode() if isinstance(key, str) else key
        while data:
            try:
                n = os.write(self._master_fd, data)
                data = data[n:]
            except BlockingIOError:
                time.sleep(0.01)

    def send_keys(self, keys, delay=0.0):
        """Send a sequence of keys, optionally waiting between each."""
        for k in keys:
            self.send_key(k)
            if delay:
                time.sleep(delay)

    # ------------------------------------------------------------------ #
    # Output / screen                                                      #
    # ------------------------------------------------------------------ #

    def wait_stable(self, timeout=5.0):
        """Block until no output has arrived for idle_secs, or timeout."""
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            elapsed = time.monotonic() - self._last_output_time
            if elapsed >= self.idle_secs:
                return True
            time.sleep(0.02)
        return False  # timed out

    def snapshot(self):
        """Return the current screen as a list of strings (one per row)."""
        with self._lock:
            return list(self.screen.display)

    def snapshot_str(self):
        """Return the current screen as a single string with newlines."""
        return "\n".join(self.snapshot())

    def find_player(self):
        """Return (row, col) of the '@' character, or None."""
        for r, line in enumerate(self.snapshot()):
            c = line.find("@")
            if c != -1:
                return (r, c)
        return None

    def find_char(self, ch):
        """Return list of (row, col) for every occurrence of ch on screen."""
        results = []
        for r, line in enumerate(self.snapshot()):
            for c, cell in enumerate(line):
                if cell == ch:
                    results.append((r, c))
        return results

    def cell(self, row, col):
        """Return the character at (row, col)."""
        return self.snapshot()[row][col]

    def status_line(self):
        """Return the bottom status line (last row) — shows Level/Gold/Hp/etc."""
        s = self.snapshot()
        return s[-1]

    def message_line(self):
        """Return the top message line (row 0) — shows combat/pickup messages."""
        return self.snapshot()[0]

    # ------------------------------------------------------------------ #
    # Internal                                                             #
    # ------------------------------------------------------------------ #

    def _reader(self):
        """Background thread: drain PTY output into the pyte screen."""
        while self._running:
            try:
                data = os.read(self._master_fd, 4096)
            except (OSError, BlockingIOError):
                time.sleep(0.01)
                continue
            except Exception:
                break
            if not data:
                break
            with self._lock:
                self.stream.feed(data)
            self._last_output_time = time.monotonic()

    @staticmethod
    def _set_winsize(fd, rows, cols):
        """Set the terminal window size via TIOCSWINSZ ioctl."""
        TIOCSWINSZ = 0x5414  # Linux
        packed = struct.pack("HHHH", rows, cols, 0, 0)
        try:
            fcntl.ioctl(fd, TIOCSWINSZ, packed)
        except OSError:
            pass
