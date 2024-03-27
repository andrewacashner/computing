from rest_framework import serializers
from django.contrib.auth.models import User
from .models import ToDoItem

class UserSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = User
        fields = ['url', 'username', 'email', 'password']

class ToDoSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = ToDoItem
        fields = ['uuid', 'task', 'deadline', 'deadlineDate', 'isDone',
                  'userOrder']
