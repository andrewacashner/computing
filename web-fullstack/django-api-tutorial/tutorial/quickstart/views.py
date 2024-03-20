import json
from django.core import serializers
from rest_framework.views import APIView
from rest_framework.response import Response
from django.contrib.auth.models import User

class AllUsers(APIView):
    queryset = User.objects.all().order_by('-date_joined')

    def get(self, request):
        response = serializers.serialize("json", self.queryset.all())
        return Response(response)

    def post(self, request):
        data = json.loads(request.body)
        User.objects.get_or_create(
                username=data["username"], 
                email=data["email"])
        return Response(f"Added user {data} to database")

