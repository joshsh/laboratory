<?php

if ($handle = opendir(dirname(__FILE__)."/../data")) {
    while (false !== ($file = readdir($handle))) {
        if ($file != "." && $file != "..") {
            echo "FROM <http://tw.rpi.edu/2009/ldthon/data/$file>\n";
        }
    }
    closedir($handle);
}

?>
FROM <http://data.semanticweb.org/conference/iswc/2009/complete>
FROM <http://twitlogic.fortytwo.net/archive/twitlogic-full.rdf>
