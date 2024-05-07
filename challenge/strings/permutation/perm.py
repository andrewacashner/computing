"""
Write an algorithm to determine if two strings are permutations of each other.
2024/05/07
"""

def is_permutation(base_string, compare_string):
    """ 
    Given two strings, return True if the second is a permutation of the
    first, otherwise False. It is a permutation if it contains exactly the
    same letters in the same quantities.

    We register the count of letter in the first string in a dictionary, then
    for the second string we do the same in reverse. If the second is a
    permutation of the first, the result will be all zero values.
    """
    
    def register_letters(string, inventory = {}, increment = 1):
        for c in string:
            inventory[c] = inventory[c] + increment if c in inventory else 1
        return inventory

    def check_letters(string, inventory):
        return register_letters(string, inventory, -1)

    def compare_letter_inventories(s1, s2):
        return check_letters(s2, register_letters(s1))

    comparison = compare_letter_inventories(base_string, compare_string)
    return all(c == 0 for c in comparison.values())

def test(s1, s2):
    print(f"{s1} vs {s2}: {is_permutation(s1, s2)}")

test_values = [
    ('abba', 'baab'),
    ('dad', 'add'),
    ('bacon', 'eggs'),
    ('shut', 'tush')
]

for (s1, s2) in test_values:
    test(s1, s2)










