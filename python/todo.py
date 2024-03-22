from datetime import datetime

"""
To Do list
Andrew Cashner
2024/03/22
"""

class ToDoItem:
    def __init__(self, task, deadline_str = '', is_done = False,
                 user_order = 0):
        self.task = task
        self.deadline = datetime.fromisoformat(deadline_str) \
                            if deadline_str else datetime.today()
        self.is_done = is_done
        self.user_order = user_order - 1

    @staticmethod
    def new_from_date(item):
        newItem = ToDoItem(item.task, 
                           deadline_str = '', 
                           is_done=item.is_done, 
                           user_order=item.user_order)
        newItem.deadline = item.deadline
        return newItem

    def clone(self):
        return ToDoItem.new_from_date(self)

    def __str__(self):
        return f'{self.task} ({self.deadline.ctime()})'

    def get_done_mark(self):
        return '[*]' if self.is_done else '[ ]'

    done_mark = property(get_done_mark)

class ToDoList:
    items = []

    def __init__(self, *items):
        self.items = [*items]
        for index, item in enumerate(self.items):
            if item.user_order == -1:
                item.user_order = index

    @staticmethod
    def new_from_list(ls):
        return ToDoList(*[ToDoItem(*i) for i in ls])

    def clone(self):
        return ToDoList(*[i.clone() for i in self.items])

    def sort_by_deadline(self):
        self.items.sort(key=lambda i: i.deadline)
        return self

    def sort_by_user_order(self):
        self.items.sort(key=lambda i: i.user_order)
        return self

    def __str__(self):
        def position(index, item):
            user_position = f' (user {item.user_order + 1})' \
                            if index != item.user_order else ''
            return f'{index + 1}' + user_position

        return '\n'.join([f'{item.done_mark} {position(index, item)}. {item}'
                          for index, item in enumerate(self.items)])

    def mark_all_completion_status(self, is_done):
        for item in self.items:
            item.is_done = is_done
        return self

    def mark_all_done(self):
        return self.mark_all_completion_status(is_done=True)

    def mark_all_not_done(self):
        return self.mark_all_completion_status(is_done=False)

    def is_all_done(self):
        return all([i.is_done == True for i in self.items])

    def is_all_not_done(self):
        return all([i.is_done == False for i in self.items])

    def is_any_done(self):
        return any([i.is_done == True for i in self.items])
    
    def is_any_not_done(self):
        return any([i.is_done == False for i in self.items])





