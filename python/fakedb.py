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
        # Input can be a list of Person instances or a list of tuples [or
        # arrays] with the arguments to initialize Person instances
        def ensurePerson(p):
            return p if isinstance(p, Person) else Person(*p)

        self.people = [ensurePerson(p) for p in people]

    def __str__(self):
        return "\n".join([str(p) for p in self.people])

    def queryBy(self, field, data):
        matches = [p for p in self.people if getattr(p, field) == data]
        return PersonDB(matches)

data = [("Cashner", "Ann", "1983-03-24"),
        ("Cashner", "Andrew", "1981-04-11"),
        ("Cashner", "Ben", "2011-01-16"),
        ("Cashner", "Joy", "2014-03-10")]

db = PersonDB(data)
print(db)

andrew = db.queryBy("firstName", "Andrew")
print(andrew)

print(db.queryBy("firstName", "Ben"))


