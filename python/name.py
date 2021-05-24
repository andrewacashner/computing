# name.py
# Andrew Cashner, 2021/05/23

class Name():
    """Holds a name: first, middle, and last; can return strings for the full
    name or initials"""
    def __init__(self, first, middle, last):
        self.first  = first
        self.middle = middle
        self.last   = last

    def full(self):
        fullname = "{} {} {}".format(self.first, self.middle, self.last)
        return fullname

    def initials(self):
        initials = "{}. {}. {}.".format(self.first[0], self.middle[0],
                self.last[0])
        return initials

def main():
    """Test out the Name class with my name"""
    name     = Name("Andrew", "Aaron", "Cashner")
    fullname = name.full()
    initials = name.initials()

    print(f"{fullname} ({initials})")

if __name__ == "__main__":
    main()
