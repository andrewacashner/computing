import json
# from django.core import serializers
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated

from django.contrib.auth.models import User
from .models import ToDoItem
from .serializers import ToDoSerializer

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

class ToDoList(APIView):
    permission_classes = (IsAuthenticated,)

    def get(self, request):
        items = ToDoItem.objects.filter(user=request.user)
        response = ToDoSerializer(items, 
                                  many=True,
                                  context={'request': request})
        return Response(response.data)

class AddToDoItem(APIView):
    permission_classes = (IsAuthenticated,)

    def post(self, request):
        new_item = json.loads(request.body)
        new_db_entry, created = ToDoItem.objects.update_or_create(
                user = request.user,
                web_id = new_item['id'],
                task = new_item['task'],
                deadline = new_item['deadlineDate'],
                is_done = new_item['isDone'])
#                user_order = new_item['user_order'])
        new_db_entry.save()
        didAction = "Added new" if created else "Updated"
        return Response(f"{didAction} item 'task: {new_item['task']}' to database");

class DeleteToDoItem(APIView):
    permission_classes = (IsAuthenticated,)

    def post(self, request):
        item = json.loads(request.body)
        this_id = item['id']
        print(this_id)
        match = ToDoItem.objects.get(web_id=this_id)
        match.delete()
        return Response(f"Deleted item with id {this_id} from database")



