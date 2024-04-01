from django.db import models
from django.contrib.auth.models import AbstractUser
from django.conf import settings
from django.utils import timezone

class User(AbstractUser): 
    pass

class TimelineEvent(models.Model):
    user = models.ForeignKey(settings.AUTH_USER_MODEL,
                             on_delete=models.CASCADE)
    fact = models.CharField(max_length=120)
    img = models.CharField(max_length=120, null=True, blank=True)
    year = models.DateField(default=timezone.now)
    answered = models.BooleanField(default=False)

    class Meta:
        ordering = ['year']

