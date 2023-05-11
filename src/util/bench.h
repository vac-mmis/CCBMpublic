#pragma once

#include <ctime>
#include <ostream>

/* Linux-dependent simple benchmark

One idiomatic way of use is:
Bench bench; // start
... // do stuff
cout << bench.reset() << " ms\n"; // print time used, at the same time resetting
... // do more stuff
cout << bench.stop() << " ms\n"; // print time used;
*/
class Bench {
    clock_t start;
    // measured time as of the last call to stop()
    unsigned long time;

public:
    Bench(): start(clock()), time(0) {}

    // first saves the time elapsed (as calling stop()), then resets the timer.
    Bench* reset() {
        stop();
        start = clock();
        return this;
    }

    // returns and saves time elapsed in ms
    unsigned long stop() {
        clock_t end = clock();
        time = (end-start) / ((double) CLOCKS_PER_SEC / 1000);
        return time;
    }

    unsigned long getTime() const {
        return time;
    }
};

// writing the Bench object means to write the saved time
inline std::ostream& operator<<(std::ostream& os, Bench const & bench) {
    os << bench.getTime();
    return os;
}

inline std::ostream& operator<<(std::ostream& os, Bench const * bench) {
    os << bench->getTime();
    return os;
}
