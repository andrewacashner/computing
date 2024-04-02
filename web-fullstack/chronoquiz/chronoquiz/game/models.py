from django.db import models
from django.contrib.auth.models import AbstractUser
from django.conf import settings
from django.utils import timezone

class User(AbstractUser): 
    pass

class Timeline(models.Model):
    user = models.ForeignKey(settings.AUTH_USER_MODEL,
                             on_delete=models.CASCADE)
    title = models.CharField(max_length=80)

class TimelineEvent(models.Model):
    user = models.ForeignKey(settings.AUTH_USER_MODEL,
                             on_delete=models.CASCADE)
    timeline = models.ForeignKey(Timeline, on_delete=models.CASCADE)
    date = models.DateField(default=timezone.now)
    info = models.CharField(max_length=120)
    img = models.CharField(max_length=120, null=True, blank=True)

    class Meta:
        ordering = ['date']

