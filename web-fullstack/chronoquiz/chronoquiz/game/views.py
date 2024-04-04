import json
from .models import User, Timeline, TimelineEvent

from .serializers import UserSerializer, TimelineSerializer,  TimelineEventSerializer
from django.http import Http404
from rest_framework.views import APIView
from rest_framework import viewsets
from rest_framework.response import Response
from rest_framework import status
from rest_framework import generics
from rest_framework.permissions import IsAuthenticated, IsAuthenticatedOrReadOnly, AllowAny

class UserExists(APIView):
    permission_classes = (AllowAny,)

    def post(self, request):
        data = json.loads(request.body)
        name = data['username']
        exists = User.objects.filter(username=name).exists()
        if exists:
            return Response(f"User {name} exists", status=status.HTTP_200_OK)
        else:
            return Response(f"User {name} not found",
                            status=status.HTTP_404_NOT_FOUND)

class Register(APIView):
    permission_classes = (AllowAny,)

    def post(self, request):
        data = json.loads(request.body)
        User.objects.create_user(
                username=data['username'],
                email=data['email'],
                password=data['password'])
        return Response(f"Created user {data['username']}")

class Timelines(APIView):
    permission_classes = (IsAuthenticatedOrReadOnly,)

    # Return all lists
    def get(self, request):
        timelines = Timeline.objects.all().order_by('title')
        response = TimelineSerializer(timelines, many=True)
        return Response(response.data)

    # Return only the authenticated user's lists
    def post(self, request):
        timelines = Timeline.objects.filter(user=request.user)
        response = TimelineSerializer(timelines, many=True)
        return Response(response.data)


class TimelineEvents(APIView):
    permission_classes = (AllowAny,)

    def get(self, request, id):
        quiz = TimelineEvent.objects.filter(timeline_id=id)
        if len(quiz) > 0:
            response = TimelineEventSerializer(quiz, many=True)
            return Response(response.data)
        else:
            return Response("Quiz not found",
                            status=status.HTTP_404_NOT_FOUND)


    def post(self, request):
        data = json.loads(request.body)
        
        username = data['username']
        user = User.objects.get(username=username) 
        
        title = data['title']
        timeline = Timeline.objects.get(title=title)
       
        events = TimelineEvent.objects.filter(user=user, timeline=timeline)
        response = TimelineEventSerializer(events, many=True)
        return Response(response.data)




