# Given an input string, print out a square of text where the string 
# is repeated on each side of the square, proceeding clockwise around 
# the edge. The right edge will read top to bottom, the bottom edge 
# right to left, the left edge bottom to top.
# Space out the horizontals to make something closer to a square.
def square(s):
    def spread(s):
        return ' '.join(list(s))

    top = spread(s)
    bottom = spread(s[::-1]) # reverse
    
    def row(i):
        gap = ' ' * (len(top) - 2)
        left = s[-(i + 1)] # bottom up
        right = s[i]       # top down
        return left + gap + right

    middle = [row(i) for i, c in enumerate(s)]

    return '\n'.join([top, *middle, bottom])





