- (rejected) Only show Deadline input after new task is entered - (done) Rearranging entries
- (done) Button to mark all complete or incomplete
- (done) Button to clear all
- (done) Use Date for deadline
- (done) Put date in span for custom format (e.g. red if past due)
- (done) Show completed items in separate list (move when completed)
- (done) Sort by deadline
- (done) Make sort button inactive when list is already sorted
- (done) Add delete button for each item (shown on hover)
- (done) Add edit button for each item
- (done) Break up into separate class and component files
- (done) Use props consistently for components to pass state
- (done) Use Context instead to pass state even better
- (done) Work with ToDoList objects rather than applying ToDoList static methods to
-  arrays
- (done) Use ToDoItem as the form default when editing an item
- (rejected) Type new item directly into list instead of form
- (rejected) Editing entries in-list
- (rejected) Nested lists
- Better layout of entries & dates
- Log in and save lists
- Multiple named lists

# LOG IN, USER ACCOUNTS

- Allow users to create new login (create new users for themselves)
- After log in redirect to /todo 
    - (DONE) Move currentUser, token, etc. state up a level?
- (DONE) Redirect with user todo list(s) loaded
- (DONE) Redirect back to login with login form shown after logout
- (DONE?) Better conditional route switching?
    - (DONE) Conditional nav menu
- (NO) Use custom URLs per user?

# DB integration

- (DONE) Add entries
- (DONE) Delete entries
- (DONE) Edit entries
    - (YES) Preserve ID when going to edit draft?
- (DONE) Mark entries done/not done    
    - individually
    - all
- (DONE) Clear all entries
- (DONE) Sort entries manually (drag)
    - (DONE) Reorder userOrder on drop and update DB
- (DONE) Sort entries by date
- Ensure display is always synced to DB
    - (DONE)  Coordinate setItems client state vs. load data from fetch in routes/ToDo

# BUGS
- (DONE) Drag and drop doesn't work anymore (stray "uuid" instead of "id"
  field)
- **"DB Locked" error when editing item**
    - (too many updates to items?)
- (DONE - BY DESIGN) User order lost when editing and adding item 
    - Consider different edit interface?
- **When logging in, re-render loop on loading items**
    - Problem with update_or_create matching uuid when blank?
