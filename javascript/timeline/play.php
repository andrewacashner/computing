<?php
$source = $_POST['source'];
$filename = $_FILES['upload']['name'];
error_log("Uploaded file $filename", 4);
switch ($source) {
case "existing" :
    error_log("Use existing timeline", 4);
    $topic = $_POST['topic'];
    $infile = htmlspecialchars("input/$topic.json");
    break;
case "upload" :
    error_log("Upload timeline", 4);
    $infile = "upload";
    break;
default:
    error_log("Bad input", 4);
    break;
}

include('play.html');
?>
