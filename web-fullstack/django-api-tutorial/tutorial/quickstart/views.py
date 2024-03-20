from django.contrib.auth.models import Group, User
from django.views.generic import View
from django.http import JsonResponse, HttpResponse
from django.core import serializers
from rest_framework import permissions, viewsets, generics

from tutorial.quickstart.serializers import GroupSerializer, UserSerializer

# class UserViewSet(viewsets.ModelViewSet):
#     """
#     API endpoint that allows users to be viewed or edited.
#     """
#     queryset = User.objects.all().order_by('-date_joined')
#     serializer_class = UserSerializer
#     permission_classes = [permissions.AllowAny]
# #    permission_classes = [permissions.IsAuthenticated]

class AllUsers(View):
    queryset = User.objects.all().order_by('-date_joined')

    def get(self, request):
        response = serializers.serialize("json", self.queryset)
        return HttpResponse(response, content_type="appication/json")

    def post(self, request):
        if request.method == 'POST':
            return JsonResponse({"response": "received POST request"})


class GroupViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows groups to be viewed or edited.
    """
    queryset = Group.objects.all().order_by('name')
    serializer_class = GroupSerializer
    permission_classes = [permissions.IsAuthenticated]

