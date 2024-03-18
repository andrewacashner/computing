from django.db import models
from django import utils

# Create your models here.
class ToDoItem(models.Model):
    task = models.TextField(default="Task description")
    deadline = models.DateTimeField(default=utils.timezone.now)
    isDone = models.BooleanField(default=False)
    userOrder = models.IntegerField(default=0)


