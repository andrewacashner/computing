from django.db import models
from django.conf import settings
from django.utils import timezone

class ToDoItem(models.Model):
    uuid = models.CharField(primary_key=True, max_length=36)
    user = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.CASCADE)
    task = models.CharField(max_length=120)
    deadline = models.CharField(max_length=80, default='')
    deadlineDate = models.DateTimeField(default=timezone.now,
                                        null=True, blank=True)
    isDone = models.BooleanField(default=False)
    userOrder = models.PositiveIntegerField(default=0)


