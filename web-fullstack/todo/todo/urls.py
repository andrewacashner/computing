"""
URL configuration for todo project.

The `urlpatterns` list routes URLs to views. For more information please see:
    https://docs.djangoproject.com/en/5.0/topics/http/urls/
Examples:
Function views
    1. Add an import:  from my_app import views
    2. Add a URL to urlpatterns:  path('', views.home, name='home')
Class-based views
    1. Add an import:  from other_app.views import Home
    2. Add a URL to urlpatterns:  path('', Home.as_view(), name='home')
Including another URLconf
    1. Import the include() function: from django.urls import include, path
    2. Add a URL to urlpatterns:  path('blog/', include('blog.urls'))
"""
from django.urls import path
from rest_framework.authtoken.views import obtain_auth_token
from todo.todolist import views

urlpatterns = [
    path('api_token_auth/', obtain_auth_token, name='api_token_auth'),
    path('login/', views.Login.as_view(), name='login'),
    path('logout/', views.Logout.as_view(), name='logout'),
    path('users/', views.Users.as_view(), name='user'),
    path('todo/', views.ToDoList.as_view(), name='todo'),
    path('todo/add/', views.AddToDoItem.as_view(), name='todo-add'),
    path('todo/delete/', views.DeleteToDoItem.as_view(), name='todo-delete'),
    path('todo/toggle/', views.ToggleDoneStatus.as_view(), name='todo-toggle'),
    path('todo/set_all_status/', views.SetAllDoneStatus.as_view(),
         name='todo-set_all_status'),
    path('todo/delete_all/', views.DeleteAll.as_view(), name='todo-delete_all'),
    path('todo/sort_by_date/', views.SortByDate.as_view(),
         name='todo-sort_by_date'),
]
