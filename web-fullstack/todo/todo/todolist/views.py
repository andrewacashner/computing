import json
# from django.core import serializers
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated, AllowAny

from django.contrib.auth.models import User
from .models import ToDoItem
from .serializers import ToDoSerializer

# TODO are Login and Logout views even necessary?
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

class Register(APIView):
    permission_classes = (AllowAny,)

    def post(self, request):
        data = json.loads(request.body)
        User.objects.create_user(
                username=data['username'],
                password=data['password'])
        return Response(f"Added user {data} to database")


class ToDoList(APIView):
    permission_classes = (IsAuthenticated,)

    def get(self, request):
        items = ToDoItem.objects.filter(user=request.user).order_by('userOrder').values()
        response = ToDoSerializer(items, 
                                  many=True,
                                  context={'request': request})
        return Response(response.data)

    def post(self, request):
        data = json.loads(request.body)
        client_items = data.get('list', None) if data else None
        if client_items and len(client_items) > 0:
            itemsCreated = 0
            itemsUpdated = 0
            for new_item in client_items:
                new_entry, created = ToDoItem.objects.update_or_create(
                        user = request.user,
                        uuid = new_item['id'],
                        defaults = {
                            'task': new_item['task'],
                            'deadline': new_item['deadline'],
                            'deadlineDate': new_item['deadlineDate'],
                            'isDone': new_item['isDone'],
                            'userOrder': new_item['userOrder'],
                            })
                new_entry.save()

                if created:
                    itemsCreated += 1
                else:
                    itemsUpdated += 1

            msg = f"Created {itemsCreated} and updated {itemsUpdated} items in database"
        else:
            msg = "Received no data from client"
 
        print(msg)
        return Response(msg)

class DeleteToDoItem(APIView):
    permission_classes = (IsAuthenticated,)

    # TODO send only id?
    def post(self, request):
        item = json.loads(request.body)
        this_id = item['id']
        match = ToDoItem.objects.get(uuid=this_id)
        match.delete()

        msg = f"Deleted item with id {this_id}"
        print(msg)
        return Response(msg)

# TODO but would this be faster/better?
# class SetAllDoneStatus(APIView):
#     permission_classes = (IsAuthenticated,)
# 
#     def post(self, request):
#         data = json.loads(request.body)
#         ToDoItem.objects.update(isDone=data['status'])
#         msg = f"Set done status to {data['status']} for all items"
#         print(msg)
#         return Response(msg)

class DeleteAll(APIView):
    permission_classes = (IsAuthenticated,)

    def post(self, request):
        ToDoItem.objects.all().delete()
        msg = f"Deleted all items"
        print(msg)
        return Response(msg)

# class SortByDate(APIView):
#     permission_classes = (IsAuthenticated,)
# 
#     def post(self, request):
#         data = json.loads(request.body)
# 
#         # let client side sort by date and set userOrder for now; 
#         # just update userOrder 
#         ##  ordered = ToDoItem.objects.all().order_by('deadlineDate').values()
# 
#         for web_item in data['list']:
#             db_item = ToDoItem.objects.get(uuid=web_item['id'])
#             db_item.userOrder = web_item['userOrder']
#             db_item.save()
# 
#         msg = "Set user order for all items"
#         print(msg)
#         return Response(msg)



