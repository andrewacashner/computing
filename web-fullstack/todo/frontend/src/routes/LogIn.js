export default function LogIn() {
  return(
    <main>
      <form className="loginForm">
        <div id="username">
          <label htmlFor="username">Username</label>
          <input type="text" name="username" required />
        </div>
        <div id="password">
          <label htmlFor="password">Password (8-16 characters including at least one number, one lowercase letter, one uppercase letter, and one special character)</label>
          <input type="password" name="password" required 
                minLength="8" maxLength="16" 
                pattern="^(?=.*\d)(?=.*[a-z])(?=.*[A-Z])(?=.*\W).{8,16}$" />
        </div>
        <button type="submit">Log In</button>
      </form>
    </main>
  );
}
