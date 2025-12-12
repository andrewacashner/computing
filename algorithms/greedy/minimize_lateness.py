# Minimize lateness
# Schedule tasks with deadlines to reduce total late penalty
# Andrew Cashner
# 2025/12/12

class Task:
    duration: int
    deadline: int

    def __init__(self, duration: int, deadline: int) -> None:
        self.duration = duration
        self.deadline = deadline

    def __repr__(self) -> str:
        return f"{{dur: {self.duration}, deadline: {self.deadline}}}"

tasks = [
    Task(3, 5),
    Task(1, 2),
    Task(7, 8),
    Task(3, 4),
    Task(2, 7),
]

def penalty(tasks: list[Task]) -> int:
    penalty = 0
    time = 0
    for task in tasks:
        time += task.duration
        if time > task.deadline:
            penalty += time - task.deadline
    return penalty


def schedule(tasks: list[Task]) -> list[Task]:
    tasks.sort(key=lambda task: task.deadline)
    return tasks

def schedule_str(tasks: list[Task]) -> str:
    return "\n".join([str(task) for task in tasks])

def main():
    print("Unoptimized:")
    init_penalty = penalty(tasks)
    print(schedule_str(tasks))
    print(f"Penalty: {init_penalty}\n")

    print("Optimized:")
    sched = schedule(tasks)
    sched_penalty = penalty(sched)
    print(schedule_str(sched))
    print(f"Penalty: {sched_penalty}")

if __name__ == '__main__':
    main()
