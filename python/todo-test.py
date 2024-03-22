from todo import ToDoItem, ToDoList
import sys
import argparse

parser = argparse.ArgumentParser(prog="Todo Test",
                                 description="Test the todo module")
parser.add_argument("-p", 
                    "--print", 
                    help="Include print statements", 
                    action="store_true")

def main(args=parser.parse_args()):
    def show(o):
        if args.print:
            print(o)

    # a = ToDoItem() # ERROR
    # show(a)

    b = ToDoItem('Wake up')
    show(b)

    c = ToDoItem('Come home', '2024-03-22T14:30')
    show(c)

    d = ToDoItem('Go to the store', '2024-03-22T13:30')
    show(d)

    todo = ToDoList(b, c, d)
    show(todo)

    show(todo.sort_by_deadline())
    show(todo.sort_by_user_order())

    first = todo.items[0]
    assert(first.is_done == False)

    todo.mark_all_done()
    assert(first.is_done == True)
    assert(todo.is_any_not_done() == False)
    assert(todo.is_all_not_done() == False)
    assert(todo.is_any_done() == True)
    assert(todo.is_all_done() == True)

    todo.mark_all_not_done()
    assert(first.is_done == False)
    assert(todo.is_any_not_done() == True)
    assert(todo.is_all_not_done() == True)
    assert(todo.is_any_done() == False)
    assert(todo.is_all_done() == False)

    first.is_done = True
    assert(first.is_done == True)
    assert(todo.is_any_done() == True)
    assert(todo.is_any_not_done() == True)
    assert(todo.is_all_done() == False)
    assert(todo.is_all_not_done() == False)

    show(todo.mark_all_done())
    show(todo.mark_all_not_done())
    show(todo)

    todo2 = todo.clone()
    assert(todo2 != todo)
    show(todo)
    show(todo2)

    todo.mark_all_done()
    assert(todo.is_all_done() == True)
    assert(todo2.is_all_done() == False)

    todo.mark_all_not_done()
    first.is_done = True
    show(todo)
    show(todo2)
    assert(todo.is_any_done() == True)
    assert(todo2.is_any_done() == False)

    config = [
        ('Wake up', '2024-03-22T06:00'),
        ('Brush teeth', '2024-03-22T06:30', False, 1),
        ('Make coffee', '2024-03-22T06:10', True),
    ]

    todo3 = ToDoList.new_from_list(config)
    show(todo3)
    show(todo3.sort_by_deadline())

    print("All tests passed")

if __name__ == "__main__":
    sys.exit(main())
