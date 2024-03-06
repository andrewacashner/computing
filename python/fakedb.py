# Fake DB to learn python
# Andrew Cashner, 2024/03/04
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

    def __str__(self):
        return f"{self.firstName}\t{self.lastName}\t({self.birthdate})"


class PersonDB:
    people = []

    def __init__(self, people):
        self.people = people

    def __str__(self):
        return "\n".join([str(p) for p in self.people])

    def queryBy(self, field, data):
        return [p for p in self.people if p[field] == data]

data = [("Cashner", "Ann", "1983-03-24"),
        ("Cashner", "Andrew", "1981-04-11"),
        ("Cashner", "Ben", "2011-01-16"),
        ("Cashner", "Joy", "2014-03-10")]

people = [Person(*d) for d in data]

db = PersonDB(people)
print(db)

andrew = db.queryBy("firstName", "Andrew")
print(andrew)

print(db.queryBy("firstName", "Ben"))


