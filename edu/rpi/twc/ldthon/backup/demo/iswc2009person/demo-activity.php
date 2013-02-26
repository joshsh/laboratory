<?php
$url= "http://data-gov.tw.rpi.edu/ws/gvtab.php?config_uri=http://tw.rpi.edu/2009/ldthon/demo/iswc2009person/activity-config.json#tab_PieChart";
echo file_get_contents($url);

echo "<pre>";
$content = file_get_contents("http://tw.rpi.edu/2009/ldthon/demo/iswc2009person/activity-sparql.php");
$content= htmlentities($content);
echo $content;
echo "</pre>";

?>
