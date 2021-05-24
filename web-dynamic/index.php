<!DOCTYPE html>
<?php
$name   = htmlspecialchars($_POST['user_name']);
$email  = htmlspecialchars($_POST['user_email']);
$msg    = htmlspecialchars($_POST['user_message']);
?>
<html>
<body>
<p>
Welcome <?php echo $name; ?>!<br />
Your email address is <?php echo $email; ?>.
</p>
<p>
You wrote:
<blockquote>
    <?php echo $msg?>
</blockquote>
</p>

</body>
</html>


