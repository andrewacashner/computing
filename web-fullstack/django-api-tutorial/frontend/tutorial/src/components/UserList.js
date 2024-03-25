function displayUserData(user) {
  return(
    <tr key={user.username}>
      <td>{user.username}</td>
      <td>{user.password}</td>
    </tr>
  );
}

export default function UserList({ users }) {
  return(
    <table>
      <thead>
        <tr>
          <th>Username</th>
          <th>Password</th>
        </tr>
      </thead>
      <tbody>
        {users.map(displayUserData)}
      </tbody>
    </table>
  );
}
