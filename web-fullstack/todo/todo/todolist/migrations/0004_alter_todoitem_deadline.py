# Generated by Django 5.0.3 on 2024-03-26 14:28

import django.utils.timezone
from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('todolist', '0003_alter_todoitem_deadline'),
    ]

    operations = [
        migrations.AlterField(
            model_name='todoitem',
            name='deadline',
            field=models.DateTimeField(default=django.utils.timezone.now),
        ),
    ]
