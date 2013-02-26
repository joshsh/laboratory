<?php header("Content-Type:text/plain"); ?>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/> 
PREFIX dc: <http://purl.org/dc/elements/1.1/> 
PREFIX swc: <http://data.semanticweb.org/ns/swc/ontology#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT ?e (count(distinct ?person) as ?count)
<?php
include_once(dirname(__FILE__)."/../../maintenance/list_sparql_from.php");
?>
WHERE 
{
  <http://data.semanticweb.org/conference/iswc/2009> swc:isSuperEventOf ?e .
  ?tag owl:sameAs ?e .
  ?s dc:relation ?tag .
  ?s dc:creator ?person. 
}
group by ?e
order by desc (?count)