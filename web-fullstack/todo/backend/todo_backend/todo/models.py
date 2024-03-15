from django.db import models

# Create your models here.
class ToDoItem(models.Model):
    task = models.TextField("Task description")
    deadline = models.DateTimeField()
    isDone = models.BooleanField()


