


# Create Processing Maven modules
rm /tmp/processing-jars
mkdir /tmp/processing-jars
cp `find . -name "*.jar"` /tmp/processing-jars
VERSION=1.2.1
cd /tmp/processing-jars
for i in `ls -1 | sed 's/.jar//'`; do mvn install:install-file -DgroupId=org.processing -DartifactId=$i -Dversion=$VERSION -Dpackaging=jar -Dfile=$i.jar -DcreateChecksum=true; done

# Create dependency XML
PREFIX="<dependency><groupId>org.processing</groupId><artifactId>"
SUFFIX="</artifactId><version>$VERSION</version></dependency>"
echo "" > /tmp/dependencies.txt
for i in `ls -1 | sed 's/.jar//'`; do echo $PREFIX $i $SUFFIX >> /tmp/dependencies.txt; done

