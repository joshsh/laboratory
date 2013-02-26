<?php
$url = "http://data-gov.tw.rpi.edu/ws/sparqlproxy.php?sparql_uri=http://tw.rpi.edu/2009/ldthon/maintenance/list_hashtag.sparql&output=sparql";

$data = json_decode( file_get_contents($url));


foreach ($data->results->bindings as $obj){
	$hashtag = basename($obj->tag->value);
	
	//load data
	$url = "http://tw.rpi.edu/2009/ldthon/ws/twitter4rdf.php?hashtag=".$hashtag."&pagelimit=10";
	echo "loading from ".$url;

	$rdf = file_get_contents($url);


	$filename= dirname(__FILE__)."/../data/".$hashtag. ".rdf";
	echo "write to file ".$filename;


	file_put_contents($filename, $rdf);
}
?>