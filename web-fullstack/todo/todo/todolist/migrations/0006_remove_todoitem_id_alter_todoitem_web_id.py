# Generated by Django 5.0.3 on 2024-03-27 13:07

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('todolist', '0005_todoitem_web_id'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='todoitem',
            name='id',
        ),
        migrations.AlterField(
            model_name='todoitem',
            name='web_id',
            field=models.CharField(max_length=36, primary_key=True, serialize=False),
        ),
    ]