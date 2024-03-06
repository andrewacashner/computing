# Given an input string, print out a square of text where the string 
# is repeated on each side of the square, proceeding clockwise around 
# the edge. The right edge will read top to bottom, the bottom edge 
# right to left, the left edge bottom to top.
# Space out the horizontals to make something closer to a square.
def square(s):
    def spread(s):
        return ' '.join(list(s))

    reverse = spread(s[::-1]) # reverse
    
    gap = ' ' * (len(forward) - 2)
   
    middle = [left + gap + right 
              for left, right in zip(reverse, forward)]

    return '\n'.join([forward, *middle, reverse])



def rect(s):
    reverse = reverse(list(s))

