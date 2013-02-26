<?php
$url= "http://data-gov.tw.rpi.edu/ws/gvtab.php?config_uri=http://tw.rpi.edu/2009/ldthon/demo/iswc2009topic/tweet-config.json#tab_PieChart";
echo file_get_contents($url);

echo "<pre>";
include_once("tweet-sparql.php");
echo "</pre>";
?>