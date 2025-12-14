# Topological Sort
#
# Given a directed graph in adjacency-list format,
# output the nodes in topological order.
#
# Andrew Cashner
# 2025/12/14

type Node = int

NUM_NODES = 6

graph: list[list[Node]] = [
    [1, 3],
    [2, 3, 5],
    [4],
    [4]
]

class Status(Enum):
    UNVISITED = 0
    VISITED = 1
    FINISHED = 2

def topological_sort(graph: list[list[Node]]) -> list[Node]:
    discovered: list[Status] = [Status.UNVISITED] * NUM_NODES

    current = 0
    discovered[current] = VISITED
    nodes = [current]

    order: list[Node] = []

    while any([node != Status.FINISHED for node in discovered]):
        # TODO




    

    return order



def main() -> None:
    order = topological_sort(graph)
    print(order)

if __name__ == '__main__':
    main()
