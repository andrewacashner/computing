# musarithmetic.py
# Andrew A. Cashner
# 2019-10-14

class note:
    pitch = 0
    octave = 0

    def __init__(self, p, o):
        self.pitch = self.name_to_number(p)
        self.octave = o

    def __str__(self):
        return self.number_to_name(self.pitch) + str(self.octave)

    def number_to_name(self, number):
        if number < 0 or number > 6:
            print('Pitch {} out of range'.format(number))
            return -1
        else:
            return 'cdefgab'[number]

    def name_to_number(self, name):
        letters = 'cdefgab'
        found = False
        for i in range(6):
            if letters[i] == name:
                number = i
                found = True
                break
        if found == False:
            print('Pitch {} not found'.format(name))
            return -1
        else: 
            return number

    def inc(self, n):
        pitch_inc = n % 7
        oct_inc = int(n / 7)
        self.pitch += pitch_inc
        self.octave += oct_inc



       

