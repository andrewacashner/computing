# LOGIN
- Is email necessary for login (or registration)?
- Add email verification to new user registration?

# DB
- (DONE) date input & output (Use string for date in DB, deal with "real"
  dates on client side)
- switch to MySQL instead of sqlite
- user-readable links (`/user/normalized_title`) instead of using ID nos

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
- creation: ensure no duplicates
- user can set timelines to public or private visibility
- add keywords or longer description to timelines DB (not just title)
- "get shareable link" (and/or QR code) in admin panel

# Timeline builder
- user creates new timeline item-by-item (like todo app)

# STYLE

- (DONE) NavLink active highlighting
