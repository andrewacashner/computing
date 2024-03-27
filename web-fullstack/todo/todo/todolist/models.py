from django.db import models
from django.conf import settings
from django.utils import timezone

class ToDoItem(models.Model):
    web_id = models.CharField(primary_key=True, max_length=36)
    user = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.CASCADE)
    task = models.CharField(max_length=120)
    deadline = models.DateTimeField(default=timezone.now)
    is_done = models.BooleanField(default=False)
    user_order = models.PositiveIntegerField(default=0)


