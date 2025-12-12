# Weighted interval scheduling
#
# Given a set of weighted intervals, find the maximum-weight set of mutually
# compatible intervals
#
# Andrew Cashner
# 2025/12/12

from typing import Tuple, NamedTuple

class Interval(NamedTuple):
    start: int
    end: int
    weight: int

    def __repr__(self) -> str:
        return f"{{({self.start}, {self.end}) @ {self.weight}}}"

class WeightStep(NamedTuple):
    weight: int 
    index: int


interval_input = [
    (1, 5, 60),
    (2, 3, 20),
    (4, 7, 50),
    (6, 10, 40),
    (8, 9, 10)
]

intervals = [Interval(*in_data) for in_data in interval_input]

NO_PRIOR = -1

def prior_compatible(intervals: list[Interval], index: int) -> int:
    if index == 0:
        return NO_PRIOR

    i = index - 1
    found = False
    while not found and i > 0:
        if intervals[i].end <= intervals[index].start:
            found = True
        else:
            i -= 1

    return i if found else NO_PRIOR

def trace_schedule(intervals: list[Interval], 
                   weights: list[WeightStep]) -> list[Interval]:
 
    last = len(weights) - 1
    indices: list[int] = [last]

    i = last
    while i > -1:
        i = weights[i].index
        if i > -1:
            indices.append(i)

    print(f"Optimal indices: {indices}")
    return [intervals[i] for i in reversed(indices)]

class WeightSet(NamedTuple):
    intervals: list[Interval]
    weight: int

def intervals_str(intervals: list[Interval]) -> str:
    return "\n".join([str(interval) for interval in intervals])

def maximum_weight_set(intervals: list[Interval]) -> WeightSet:
    intervals.sort(key=lambda i: i.end)
    print("Sorted:")
    print(intervals_str(intervals))

    interval_count = len(intervals)

    previous = [prior_compatible(intervals, j) 
                for j in range(interval_count)]
    print(f"Previous: {previous}")

    weights: list[WeightStep] = [WeightStep(0, 0)] * interval_count

    for j in range(1, interval_count):
        prev = previous[j]
        this_weight = intervals[j].weight + weights[prev].weight
        prev_weight = weights[j - 1].weight

        if this_weight > prev_weight:
            weights[j] = WeightStep(this_weight, prev)
        else:
            weights[j] = WeightStep(prev_weight, j - 1)

    print(f"Weights: {weights}")
    optimal_weight = weights[-1].weight
    schedule = trace_schedule(intervals, weights)

    return WeightSet(schedule, optimal_weight)


def main() -> None:
    print("Input:")
    print(intervals_str(intervals))

    (schedule, weight) = maximum_weight_set(intervals)
    print("\nOptimal set:")
    print(intervals_str(schedule))
    print(f"Optimal weight: {weight}")

if __name__ == '__main__':
    main()

