
mkdir -p /tmp/datasets/markup

export JAVA_OPTIONS="-Xms2048M -Xmx2048M"
time ./generate-markup.sh


curl --data-urlencode query@../datasets.rq http://flux.franz.com/catalogs/testing/repositories/iogds > results.xml
xsltproc ../src/main/xslt/sparql-xml-to-csv.xslt results.xml > results.csv



curl -H "Accept: application/sparql-results+json" --data-urlencode query@src/main/sparql/datasets.rq http://flux.franz.com/catalogs/testing/repositories/iogds > results.json


xsltproc ../src/main/xslt/sparql-xml-to-csv.xslt results-partial.xml > results.csv
