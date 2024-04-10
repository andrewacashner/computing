import json
from .models import User, Timeline, Fact 

from .serializers import UserSerializer, TimelineSerializer, FactSerializer, TimelineFullSerializer
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

    def delete(self, request, id):
        target = Timeline.objects.get(user=request.user, id=id)
        target.delete()
        return Response(f"Deleted timeline with id {id}")

class Facts(APIView):
    permission_classes = (AllowAny,)

    def get(self, request, id):
        quiz = Fact.objects.filter(timeline_id=id)
        if len(quiz) > 0:
            response = FactSerializer(quiz, many=True)
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
       
        facts = Fact.objects.filter(user=user, timeline=timeline)
        response = FactSerializer(facts, many=True)
        return Response(response.data)

class TimelineFull(APIView):
    permission_classes = (IsAuthenticated,)

    def get(self, request, id):
        timeline = Timeline.objects.get(user=request.user, id=id)
        facts = Fact.objects.filter(user=request.user, timeline=timeline)

        response = TimelineFullSerializer(timeline, 
                                          context = { 'facts': facts })
        return Response(response.data)

    def post(self, request):
        user_timeline = json.loads(request.body)

        new_timeline, created = Timeline.objects.update_or_create(
                user = request.user,
                title = user_timeline['title'],
                defaults = {
                    'description':  user_timeline['description'],
                    'keywords':     user_timeline['keywords'],
                    'creator':      user_timeline['creator']
                })
        action = "Create new" if created else "Updated"

# TODO validation? (before we did this:)
#            new_timeline = TimelineSerializer(data=user_timeline)
#            new_timeline.is_valid(raise_exception=True)
#            draft_timeline = new_timeline.save(user=request.user)
#            action = "Created new"

        facts_created = facts_updated = 0

        for event in user_timeline['facts']:
            new_event, created = Fact.objects.update_or_create(
                    user = request.user,
                    timeline = new_timeline,
                    defaults = {
                        'date': event['date'],
                        'info': event['info'],
                        'img':  event.get('img')
                    })
            if created:
                facts_created += 1
            else:
                facts_updated += 1

        count = Fact.objects.filter(user=request.user, 
                                    timeline=new_timeline).count()

        return Response(f"{action} timeline '{user_timeline['title']}'"
                        + f" with {count} items"
                        + f" ({facts_created} new and {facts_updated} updated)")
        


