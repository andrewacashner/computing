from rest_framework import serializers
from .models import User, TimelineEvent

# TODO HyperlinkedModel necessary?
class UserSerializer(serializers.ModelSerializer): 
    class Meta:
        model = User
        fields = ['username', 'email', 'password']

class TimelineSerializer(serializers.ModelSerializer):
    class Meta:
        model = TimelineEvent
        fields = ['fact', 'img', 'year', 'answered']
