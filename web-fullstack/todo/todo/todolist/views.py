import json
# from django.core import serializers
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.contrib.auth.models import User

class Users(APIView):
    permission_classes = (IsAuthenticated,)

#     queryset = User.objects.all().order_by('-date_joined')
# 
#     def get(self, request):
#         response = serializers.serialize("json", self.queryset.all())
#         return Response(response)

    def post(self, request):
        data = json.loads(request.body)
        User.objects.create_user(
                username=data['username'],
                password=data['password'])
        return Response(f"Added user {data} to database")


class Login(APIView):
    permission_classes = (IsAuthenticated,)
    
    def post(self, request):
        data = json.loads(request.body)
        return Response(f"Logged in user {data['username']}")

class Logout(APIView):
    permission_classes = (IsAuthenticated,)

    def post(self, request):
        data = json.loads(request.body)
        return Response(f"Logged out user {data['username']}")

