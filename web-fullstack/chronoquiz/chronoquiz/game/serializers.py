from rest_framework import serializers
from .models import User, Timeline, TimelineEvent

# TODO HyperlinkedModel necessary?
class UserSerializer(serializers.ModelSerializer): 
    class Meta:
        model = User
        fields = ['username', 'email', 'password']

class TimelineSerializer(serializers.ModelSerializer):
    class Meta:
        model = Timeline
        fields = ['id', 'title']

class TimelineEventSerializer(serializers.ModelSerializer):
    class Meta:
        model = TimelineEvent
        # TODO format date as YYYY?
        fields = ['date', 'info', 'img']
