#ifndef TIMER_H_
#define TIMER_H_

#include <chrono>
#include <stdexcept>

/** 
 * Simple wrapper for chrono::steady_clock timer.
 * Based on https://github.com/FLAMEGPU/FLAMEGPU2/blob/master/include/flamegpu/detail/SteadyClockTimer.h (with permission)
 */
class  Timer {
 public:
    /**
     * Record the start steady clock time or reset an already started start without increasing duration
     */ 
    void start() {
        this->startTime = std::chrono::steady_clock::now();
        this->startEventRecorded = true;
        this->stopEventRecorded = false;
    }

    /**
     * Record the start steady clock stop time
     */
    void stop() {
        if (startEventRecorded && !stopEventRecorded) {
            this->stopTime = std::chrono::steady_clock::now();
            this->stopEventRecorded = true;
            this->durationMillis += getElapsedMilliseconds();
        } else {
            throw std::logic_error("start() must be called prior to calling stop()");
        }
    }

    /**
     * Get the time since start() in milliseconds
     * @return elapsed time in seconds
     */
    float getRunningMilliseconds() {
        if (!startEventRecorded) {
            throw std::logic_error("start() must be called prior to getRunning*()");
        }
        std::chrono::duration<double> elapsed = std::chrono::steady_clock::now() - this->startTime;
        float ms = static_cast<float>(std::chrono::duration_cast<std::chrono::milliseconds>(elapsed).count());
        return ms;
    }

    /**
     * Get the time since start() in seconds
     * @return running time in seconds
     */
    float getRunningSeconds() {
        return this->getRunningMilliseconds() / 1000.0f;
    }

    /**
     * Get the elapsed time between calls to start() and stop() in milliseconds
     * @return elapsed time in milliseconds
     */
    float getElapsedMilliseconds() {
        if (!startEventRecorded) {
            throw std::logic_error("start() must be called prior to getElapsed*()");
        }
        if (!stopEventRecorded) {
            throw std::logic_error("stop() must be called prior to getElapsed*()");
        }
        std::chrono::duration<double> elapsed = this->stopTime - this->startTime;
        float ms = static_cast<float>(std::chrono::duration_cast<std::chrono::milliseconds>(elapsed).count());
        return ms;
    }

    /**
     * Get the elapsed time between calls to start() and stop() in seconds
     * @return elapsed time in seconds
     */
    float getElapsedSeconds() {
        return this->getElapsedMilliseconds() / 1000.0f;
    }
    
    /**
     * Get the combined duration of each start() stop() pair since init in milliseconds
     * @return combined duration in milliseconds
     */
    float getDurationMilliseconds() {
        return durationMillis;
    }
    /**
     * Get the combined duration of each start() stop() pair since init in seconds
     * @return combined duration in seconds
     */
    float getDurationSeconds() {
        return durationMillis / 1000;
    }
    /**
     * Reset the combined duration tracker
     */
    void resetDuration() {
        durationMillis = 0;
    }

 private:
    /** 
     * Time point for the start event
     */
    std::chrono::time_point<std::chrono::steady_clock> startTime;
    /** 
     * Time point for the stop event
     */
    std::chrono::time_point<std::chrono::steady_clock> stopTime;
    /**
     *
     */
    float durationMillis = 0;
    /**
     * Flag indicating if the start event has been recorded or not.
     */
    bool startEventRecorded = false;
    /**
     * Flag indicating if the start event has been recorded or not.
     */
    bool stopEventRecorded = false;
};

#endif  // TIMER_H_