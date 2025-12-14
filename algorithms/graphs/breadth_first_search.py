# Breadth-First Search Tree
#
# Given an undirected graph in adjacency-list form, return a list of the 
# nodes in a breadth-first traversal of the tree.
#
# Andrew Cashner
# 2025/12/14

from collections import deque

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


# Implementation using list of lists for nodes to examine
def breadth_first_search_list(graph: list[list[Node]], 
                              start: Node) -> list[Node]:
    discovered = [False] * NUM_NODES
    discovered[start] = True
    
    tree_layers = [[start]]
    layer = 0
        
    tree: list[Node] = []

    while len(tree_layers[layer]) > 0:
        tree_layers.append([])
        for node in tree_layers[layer]:
            for edge in graph[node]:
                if not discovered[edge]:
                    discovered[edge] = True
                    tree.append(edge)
                    tree_layers[layer].append(edge)
        layer += 1

    return tree

# Implementation using queue for nodes to examine
def breadth_first_search(graph: list[list[Node]], 
                         start: Node) -> list[Node]:
    discovered = [False] * NUM_NODES
    discovered[start] = True
    
    nodes = deque([start])
        
    tree: list[Node] = []

    while len(nodes) > 0:
        node = nodes.pop()
        for edge in graph[node]:
            if not discovered[edge]:
                discovered[edge] = True
                tree.append(edge)
                nodes.appendleft(edge)

    return tree


def main():
    layers = breadth_first_search(graph, 1)
    print(layers)

if __name__ == '__main__':
    main()
