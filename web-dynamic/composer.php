<!DOCTYPE html>

<?php

$composer = $_POST['composer'];
$composer_lookup = array(
    "padilla"  => "Juan Gutiérrez de Padilla",
    "mozart"   => "Wolfgang Amadeus Mozart",
    "schubert" => "Franz Peter Schubert",
    "messiaen" => "Olivier Messiaen",
    "none"     => "No one"
);
$composer_name = htmlspecialchars($composer_lookup[$composer]);

?>

<html>
    <head>
        <title>Composer</title>
        <meta charset="utf-8"/>
    </head>
    <body>
        <section>
        <head>Composer</head>
        <p>You selected <strong><?php echo $composer_name?>!</strong></p>
        </section>
    </body>
</html>

