#pragma once

#include <condition_variable>
#include <cstddef>
#include <functional>
#include <mutex>
#include <thread>
#include <vector>

namespace libprojectM {
namespace MilkdropPreset {

/**
 * @brief A minimal fixed-size thread pool for fork-join parallelism.
 *
 * Built for the per-pixel mesh calculation: a small batch of indexed tasks is dispatched once
 * per frame and the calling thread blocks until all of them complete. The calling thread also
 * runs tasks, so @a workerCount worker threads provide up to @a workerCount + 1 way parallelism.
 *
 * Each index in a batch is executed exactly once, so a task may safely use per-index state
 * (e.g. its own evaluation context) without any additional locking.
 */
class ThreadPool
{
public:
    /**
     * @brief Spawns the worker threads.
     * @param workerCount Number of worker threads to create. May be 0, in which case Run()
     *                    executes every task on the calling thread.
     */
    explicit ThreadPool(size_t workerCount)
    {
        m_workers.reserve(workerCount);
        for (size_t i = 0; i < workerCount; i++)
        {
            m_workers.emplace_back([this] { WorkerLoop(); });
        }
    }

    ~ThreadPool()
    {
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            m_shutdown = true;
        }
        m_workAvailable.notify_all();
        for (auto& worker : m_workers)
        {
            worker.join();
        }
    }

    ThreadPool(const ThreadPool&) = delete;
    ThreadPool& operator=(const ThreadPool&) = delete;

    size_t WorkerCount() const
    {
        return m_workers.size();
    }

    /**
     * @brief Runs task(0)..task(taskCount-1), one call per index, and returns once all complete.
     *
     * Tasks run on the pool's worker threads and on the calling thread. Each index is executed
     * exactly once. The @a task reference must remain valid until Run() returns.
     */
    void Run(const std::function<void(size_t)>& task, size_t taskCount)
    {
        if (taskCount == 0)
        {
            return;
        }

        std::unique_lock<std::mutex> lock(m_mutex);
        m_task = &task;
        m_taskCount = taskCount;
        m_nextIndex = 0;
        m_pending = taskCount;
        m_workAvailable.notify_all();

        // The calling thread participates rather than idling while the workers run.
        while (m_nextIndex < m_taskCount)
        {
            const size_t index = m_nextIndex++;
            lock.unlock();
            task(index);
            lock.lock();
            if (--m_pending == 0)
            {
                m_workDone.notify_all();
            }
        }

        m_workDone.wait(lock, [this] { return m_pending == 0; });

        m_task = nullptr;
        m_taskCount = 0;
    }

private:
    void WorkerLoop()
    {
        std::unique_lock<std::mutex> lock(m_mutex);
        for (;;)
        {
            m_workAvailable.wait(lock, [this] {
                return m_shutdown || (m_task != nullptr && m_nextIndex < m_taskCount);
            });

            if (m_task == nullptr || m_nextIndex >= m_taskCount)
            {
                // Only reachable on shutdown, as otherwise the predicate guarantees work.
                if (m_shutdown)
                {
                    return;
                }
                continue;
            }

            const size_t index = m_nextIndex++;
            const std::function<void(size_t)>& task = *m_task;
            lock.unlock();
            task(index);
            lock.lock();
            if (--m_pending == 0)
            {
                m_workDone.notify_all();
            }
        }
    }

    std::vector<std::thread> m_workers;
    std::mutex m_mutex;
    std::condition_variable m_workAvailable;
    std::condition_variable m_workDone;

    const std::function<void(size_t)>* m_task{nullptr}; //!< Current batch task, valid while m_pending > 0.
    size_t m_taskCount{0};                              //!< Number of indices in the current batch.
    size_t m_nextIndex{0};                              //!< Next index to claim.
    size_t m_pending{0};                                //!< Indices not yet completed.
    bool m_shutdown{false};                             //!< Set during destruction to stop the workers.
};

} // namespace MilkdropPreset
} // namespace libprojectM
