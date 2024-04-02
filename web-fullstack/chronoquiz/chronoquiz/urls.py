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

timeline_list = views.TimelineViewSet.as_view({
    'get': 'list',
    'post': 'create'
})

timeline_detail = views.TimelineViewSet.as_view({
    'get': 'retrieve',
    'put': 'update',
    'patch': 'partial_update',
    'delete': 'destroy'
})

router = DefaultRouter()
router.register(r'timelines', views.TimelineViewSet, basename='timeline')

urlpatterns = [
    path('login/', obtain_auth_token, name='login'),
    path('check_user/', views.UserExists.as_view(), name='check_user'),
    path('register/', views.Register.as_view(), name='register'),
    #    path('accounts/', include('django.contrib.auth.urls')),
    path('', include(router.urls)),
#    path('login/', views.Login.as_view(), name='login'),
#    path('logout/', views.Logout.as_view(), name='logout'),
    # path('quizzes/', views.Quizzes.as_view(), name='quizzes')

]
