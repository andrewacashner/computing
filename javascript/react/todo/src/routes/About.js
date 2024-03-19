export default function About() {
  return(
    <main>
      <h1>About this App</h1>

      <section id="instructions">
        <h2>Instructions</h2>
        <p>
          Enter a task to do using the form. 
        </p>
        <p>
          The deadline is optional and can be entered in a variety of natural-language forms.
        </p>
        <p>
          Drag the items to rearrange, check the boxes to mark them done or not done, or click the menu commands to mark all tasks complete or incomplete.
          Click the X that appears by each item to delete it.
        </p>
      </section>
      <section id="about">
        <h2>Credits</h2>
        <p>
          This app was written by Andrew Cashner.
        </p>
        <p>
          The front end is in Javascript using the React framework.
          The back end is in Python using the Django framework and a MySQL database.
        </p>
      </section>
    </main>
  );
}
