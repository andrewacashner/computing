import json
from .models import User, TimelineEvent

from .serializers import UserSerializer, TimelineSerializer
from django.http import Http404
from rest_framework.views import APIView
from rest_framework import viewsets
from rest_framework.response import Response
from rest_framework import status
from rest_framework import generics
from rest_framework.permissions import IsAuthenticated, IsAuthenticatedOrReadOnly, AllowAny

# class Login(APIView):
#     permission_classes = (IsAuthenticated,)
# 
#     def post(self, request):
#         return Response(f"User {request.user.get_username()} logged in",
#                         status=status.HTTP_200_OK)

# class Logout(APIView):
#     permission_classes = (IsAuthenticated,)
# 
#     def post(self, request):
#         return Response(f"User {request.user.get_username()} logged out",
#                         status=status.HTTP_200_OK)
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

# class Quizzes(APIView):
#     permission_classes = (IsAuthenticated)
# 
#     def get(self, request):
#         quizzes = TimelineEvent.objects.filter(user=request.user)
#         serializer = TimelineSerializer(quizzes, many=True)
#         return Response(serializer.data)
# 
#     def post(self, request):
#         serializer = TimelineSerializer(data=request.data)
#         if serializer.is_valid():
#             serializer.save()
#             return Response(serializer.data, status=status.HTTP_201_CREATED)
#         return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
#     

class TimelineViewSet(viewsets.ModelViewSet):
    queryset = TimelineEvent.objects.all()
    serializer_class = TimelineSerializer
    permission_classes = [IsAuthenticatedOrReadOnly]

    def perform_create(self, serializer):
        serializer.save(owner=self.request.user)



