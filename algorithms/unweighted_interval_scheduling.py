# Unweighted Interval Scheduling
# Choose the schedule with the most non-overlapping intervals
# Andrew Cashner
# 2025/12/12

class Interval:
    start: int
    end: int

    def __init__(self, start: int, end: int) -> None:
        self.start = start
        self.end = end

    def __repr__(self) -> str:
        return f"({self.start}, {self.end})"

intervals = [
    Interval(1, 4),
    Interval(3, 5),
    Interval(0, 6),
    Interval(5, 7),
    Interval(8, 9),
    Interval(5, 9)
]

def schedule(intervals: list[Interval]) -> list[Interval]:
    intervals.sort(key=lambda i: i.end)
    schedule = [intervals[0]]

    for interval in intervals[1:]:
        if interval.start >= schedule[-1].end:
            schedule.append(interval)

    return schedule

def main():
    sched = schedule(intervals)
    print(sched)

if __name__ == '__main__':
    main()


