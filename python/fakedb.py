import uuid

class Person:
    lastName = ""
    firstName = ""
    birthdate = None

    def __init__(self, lastName, firstName, birthdate):
        self.id = uuid.uuid4()
        self.lastName = lastName
        self.firstName = firstName
        self.birthdate = birthdate

# TODO doesn't work 
    def __str__(self):
        return f"{self.firstName} {self.lastName} ({self.birthdate})"


class PersonDB:
    people = []

    def __init__(self, people):
        self.people = map(lambda p: Person(*p) if isinstance(p, Person) else
                          p, people)

# TODO doesn't work 
    def __str__(self):
        result = "["
        for person in self.people:
            result += f"{person}, "

        result += "]"

        return result

#    def queryBy(self, field):
#        if field == id return self.find(lambda d: d.id == id)


data = [("Cashner", "Ann", "1983-03-24"),
        ("Cashner", "Andrew", "1981-04-11"),
        ("Cashner", "Ben", "2011-01-16"),
        ("Cashner", "Joy", "2014-03-10")]

db = PersonDB(data)
print(str(db))



