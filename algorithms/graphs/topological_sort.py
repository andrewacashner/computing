# Topological Sort
#
# Given a directed graph in adjacency-list format,
# output the nodes in topological order.
#
# Andrew Cashner
# 2025/12/14

from enum import Enum

type Node = int

NUM_NODES = 6

# 0 -> 1      -> 3
#      1 -> 2 -> 3      -> 5
#          2       -> 4
#                3 -> 4
#                     4 -> 5
#                          5
#
# [0, 1, 2, 3, 4, 5]

graph: list[list[Node]] = [
    [1, 3],
    [2, 3, 5],
    [4],
    [4],
    [5],
    []
]

def topological_sort(graph: list[list[Node]]) -> list[Node]:
    visited: list[Status] = [False] * NUM_NODES
    parents = [0] * NUM_NODES
    nodes = []
    
    order: list[list[Node]] = []
    
    while not all(visited):
        current = visited.index(False)
        nodes.append(current)

        while nodes:
            current = nodes.pop()
    
            if not visited[current]:
                visited[current] = True
                if not order:
                    order.append(current)
                else:
                    insert_index = order.index(parents[current])
                    order.insert(insert_index + 1, current)

                children = graph[current]

                for node in children:
                    if not visited[node]:
                        nodes.append(node)
                        parents[node] = current

    return order

def main() -> None:
    order = topological_sort(graph)
    print(order)

if __name__ == '__main__':
    main()
