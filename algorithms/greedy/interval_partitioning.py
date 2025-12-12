# Interval Partitioning
# Schedule all of the intervals using the minimum number of rooms
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
    Interval(3, 6),
    Interval(5, 8),
    Interval(7, 10),
    Interval(7, 9),
    Interval(11, 15),
    Interval(13, 16)
]

def schedule(intervals: list[Interval]) -> list[list[Interval]]:
    intervals.sort(key=lambda i: i.start)
    rooms = [[intervals[0]]]

    for interval in intervals[1:]:
        room = 0
        scheduled = False

        while not scheduled:
            if room >= len(rooms):
                rooms.append([interval])
                scheduled = True
            elif interval.start >= rooms[room][-1].end:
                rooms[room].append(interval)
                scheduled = True
            else:
                room += 1

    return rooms 

def main():
    sched = schedule(intervals)
    print(sched)

if __name__ == '__main__':
    main()


