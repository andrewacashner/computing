# Depth-First Search Tree
#
# Given an undirected graph in adjacency-list form, return a list of the 
# nodes in a depth-first traversal of the tree.
#
# Andrew Cashner
# 2025/12/14

type Node = int

NUM_NODES = 5

# Adjacency list
# Index = start node
# List contents = nodes adjacent to start node
graph = [
    [1],
    [0, 2],
    [1, 3, 4],
    [0, 2],
    [3]
]


def depth_first_search(graph: list[list[Node]], 
                         start: Node) -> list[Node]:
    discovered = [False] * NUM_NODES
    discovered[start] = True
    
    nodes = [start]
        
    tree: list[Node] = []

    while len(nodes) > 0:
        node = nodes.pop()
        for edge in graph[node]:
            if not discovered[edge]:
                discovered[edge] = True
                tree.append(edge)
                nodes.append(edge)

    return tree


def main():
    layers = depth_first_search(graph, 1)
    print(layers)

if __name__ == '__main__':
    main()
