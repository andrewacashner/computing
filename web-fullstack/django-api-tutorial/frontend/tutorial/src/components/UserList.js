function displayUserData(user) {
  return(
    <tr key={user.username}>
      <td>{user.username}</td>
      <td>{user.email}</td>
    </tr>
  );
}

export default function UserList({ users }) {
  return(
    <table>
      <thead>
        <tr>
          <th>Username</th>
          <th>Email</th>
        </tr>
      </thead>
      <tbody>
        {users.map(displayUserData)}
      </tbody>
    </table>
  );
}
