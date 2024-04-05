"""
URL configuration for chronoquiz project.

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
from django.urls import path, include
from rest_framework.authtoken.views import obtain_auth_token
from rest_framework.routers import DefaultRouter
from chronoquiz.game import views

# timeline_list = views.TimelineEvents.as_view({
#     'get': 'list',
#     'post': 'create'
# })
# 
# timeline_detail = views.TimelineEvents.as_view({
#     'get': 'retrieve',
#     'put': 'update',
#     'patch': 'partial_update',
#     'delete': 'destroy'
# })
# 
# router = DefaultRouter()
# router.register(r'timelines', views.Timelines, basename='timeline')
# router.register(r'events', views.TimelineEvents, basename='events')

urlpatterns = [
    path('login/',         obtain_auth_token,              name='login'),
    path('check_user/',    views.UserExists.as_view(),     name='check_user'),
    path('register/',      views.Register.as_view(),       name='register'),
    path('timelines/',     views.Timelines.as_view(),      name='timelines'),
    path('events/',        views.TimelineEvents.as_view(), name='events'),
    path('game/<int:id>/', views.TimelineEvents.as_view(), name='quiz'),
    path('create/',        views.CreateTimeline.as_view(), name='create'),

    # TODO could do something like this for user-readable URL
    # path('game/<str:username>/<str:title>/', views.TimelineEvents.as_view(), name='quiz'),

#    path('', include(router.urls)),

]
