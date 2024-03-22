import json
from django.core import serializers
from rest_framework.views import APIView
from rest_framework.response import Response

from django.contrib.auth.models import User
from .serializers import UserSerializer
from rest_framework import viewsets
from rest_framework.authentication import SessionAuthentication
from rest_framework.permissions import IsAuthenticated, AllowAny, IsAdminUser

class AllUsers(APIView):
    queryset = User.objects.all().order_by('-date_joined')

    def get(self, request):
        response = serializers.serialize('json', self.queryset.all())
        return Response(response)

    def post(self, request):
        data = json.loads(request.body)
        User.objects.get_or_create(
                username=data['username'], 
                email=data['email'])
        return Response(f'Added user {data} to database')

class UserViewSet(viewsets.ModelViewSet):
    queryset = User.objects.all()
    serializer_class = UserSerializer
    authentication_classes = [SessionAuthentication]
    permission_classes = [IsAuthenticated]

#    def post(self, request):
#        username = request.data.get('username')
#        password = request.data.get('password')
#        # Validate credentials
#        # Login user
#        return Response(f"Authenticate user {username}")

class LogoutView(APIView):
    def post(self, request):
        # Logout user
        username = request.data.get('username');
        return Response(f"User {username} logged out");

# from rest_framework import permissions, viewsets
# from tutorial.quickstart.serializers import UserSerializer
# 
# class UserViewSet(viewsets.ModelViewSet):
#     queryset = User.objects.all().order_by('-date_joined')
#     serializer_class = UserSerializer
#     permission_classes = [permissions.IsAuthenticated]




