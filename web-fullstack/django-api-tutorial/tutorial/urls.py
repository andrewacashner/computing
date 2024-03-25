"""
URL configuration for tutorial project.

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

from tutorial.quickstart import views

urlpatterns = [
    path('users/', views.AllUsers.as_view(), name='users'),
    path('auth/api_token_auth/', obtain_auth_token, name='api_token_auth'),
    path('auth/login/', views.Login.as_view(), name='login'),
    path('auth/logout/', views.Logout.as_view(), name='logout'),
]
