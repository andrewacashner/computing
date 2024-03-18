from django.shortcuts import render
from django.http import HttpResponse
import json

from .models import ToDoItem

def todo(request):
    items = [{"task": i.task, 
              "deadline": f"{i.deadline}", 
              "isDone": i.isDone, 
              "userOrder": i.userOrder }
             for i in ToDoItem.objects.all()]
    items.sort(key=lambda i: i["userOrder"])
    jsonItems = json.dumps(items)
    response = HttpResponse(jsonItems)
    response["Content-Type"] = "text/plain"
    return response

def front(request):
    context = {}
    return render(request, "index.html", context)


