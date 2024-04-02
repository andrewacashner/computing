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

"""
class Timeline(models.Model):
    user    = models.ForeignKey(settings.AUTH_USER_MODEL,
                                on_delete=models.CASCADE)
    title   = models.CharField(max_length=80)

class TimelineEvent(models.Model):
    user     = models.ForeignKey(settings.AUTH_USER_MODEL,
                                 on_delete=models.CASCADE)
    timeline = models.ForeignKey(Timeline, on_delete=models.CASCADE)
    date     = models.DateField(default=timezone.now)
    info     = models.CharField(max_length=120)
    img      = models.CharField(max_length=120, null=True, blank=True)

    class Meta:
        ordering = ['date']
"""
"""
User: 
    andrewacashner, PW
    susan, PW
    ben, PW

Timeline:
    andrewacshner, music
    andrewacashner, wars
    susan, cat history
    ben, lemuria
    ben, future history

TimelineEvent
    1. andrewacashner, music, Birth of Bach
    2. andrewacashner, music, Birth of Mozart
    3. andrewacashner, wars, Birth of Hitler
    4. andrewacashner, wars, Birth of Churchill
    5. susan, cat history, Birth of Seti
    6. ben, future history, Birth of Jesouse

TimelineEvent.objects.filter(user=andrewacashner, timeline=music)
TimelineEvent.objects.filter(user=ben, timeline='future history')

---
- Could you have same event in multiple timelines? (affects on_delete value)

"""
