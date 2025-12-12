# Optimal Caching
#
# Choose cache contents to minimize misses
# Farthest-in-future solutionA
#
# Andrew Cashenr
# 2025/12/12

from typing import Tuple

requests = "ABCDCDBACDE"

def farthest_in_future(cache: list[str], requests: str, index: int) -> str:
    distances: list[Tuple[str, int]] = []
    remainder = requests[index:]
    
    for val in cache:
        if val in remainder:
            distance = remainder.index(val) 
        else:
            distance = len(remainder)

        distances.append((val, distance))

#    print(distances)
    farthest = max(distances, key=lambda distance: distance[1])
    return farthest[0]

def show_optimal_cache(requests: str, cache_size: int) -> None:
    cache: list[str] = []

    for i in range(len(requests)):
        request = requests[i]

        report = ""
        if request in cache:
            report += f"{request}: HIT  "
        else:
            report += f"{request}: MISS "
            if len(cache) < cache_size:
                cache.append(request)
            else:
                evict = farthest_in_future(cache, requests, i)
                evict_index = cache.index(evict)
                cache[evict_index] = request

        report += str(cache)
        print(report)

def main():
    show_optimal_cache(requests, 3)

if __name__ == '__main__':
    main()
