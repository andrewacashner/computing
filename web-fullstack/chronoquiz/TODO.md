# LOGIN
- Is email necessary for login (or registration)?
- Add email verification to new user registration?

# DB
- (DONE) date input & output (Use string for date in DB, deal with "real"
  dates on client side)
- switch to MySQL instead of sqlite
- user-readable links (`/user/normalized_title`) instead of using ID nos
- (DONE) add timeline client fields (description, keywords, creator name) to DB

# TIMELINE LIST
- (DONE) unauthenticated users see list of all available quizzes
- timeline list: user creates uploading from csv or json to client, then
  stored in db
    - (DONE) JSON 
    - CSV
- (DONE) timeline list: user delete individual lists
- (DONE) In game chooser page, show username with title (+ description,
  keywords?)
- (DONE) timeline list creation: validate input before creating
    - How to handle invalid input on server vs. client:
        - "baddata.json" unexpected Django validation result
- Add try/catch so no timeline created if problem in creation process
- (DONE) creation: ensure no duplicates
- user can set timelines to public or private visibility
- (DONE) add keywords or longer description to timelines DB (not just title)
- "get shareable link" (and/or QR code) in admin panel

# Timeline builder
- (DONE) user creates new timeline item-by-item (like todo app)

# STYLE

- (DONE) NavLink active highlighting

# BUGS

## Create page
- (DONE) Adds two events to timeline only if the event is the default
  card
- Tab from one form field to next doesn't work as expected (related to onBlur)
- (DONE - needed to update Timeline component to check card.event.date) GAME NO LONGER WORKS!

- (DONE) Adding item deletes previous?
- (DONE) Ensure IDs are preserved
- Form input super slow and loses focus with every keystroke when using
  onChange in form (using onBlur instead)

