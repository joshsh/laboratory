<?php
$url= "http://data-gov.tw.rpi.edu/ws/gvtab.php?config_uri=http://tw.rpi.edu/2009/ldthon/demo/iswc2009topic/creator-config.json#tab_PieChart";
echo file_get_contents($url);

echo "<pre>";
include_once("creator-sparql.php");
echo "</pre>";
?>