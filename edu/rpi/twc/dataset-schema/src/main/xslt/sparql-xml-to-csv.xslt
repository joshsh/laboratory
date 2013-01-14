<!-- retrieved from http://pastebin.com/f736e264 on 2012-04-11 -->
<!-- 
     From SPARQL result XML to CSV.
-->
<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:res="http://www.w3.org/2005/sparql-results#">

  <xsl:output method="text" media-type="text/csv"/>

  <!--
      Conventions used in the output CSV: Field delimiter, record
      terminator, quote character.
  -->
  <xsl:param name="delim" select="','"/>
  <xsl:param name="eol" select="'&#x0a;'"/>
  <xsl:param name="quote" select="'&quot;'"/>

  <!--
      A template for generic text search-and-replace.
  -->
  <xsl:template name="replace-string">
    <xsl:param name="input"/>
    <xsl:param name="search"/>
    <xsl:param name="replace"/>    
    <xsl:choose>
	<xsl:when test="contains($input, $search)">
	  <xsl:variable name="head" select="substring-before($input, $search)"/>
	  <xsl:variable name="tail">
	    <xsl:call-template name="replace-string">
	      <xsl:with-param name="input" select="substring-after($input, $search)"/>
	      <xsl:with-param name="search" select="$search"/>
	      <xsl:with-param name="replace" select="$replace"/>
	    </xsl:call-template>
	  </xsl:variable>
	  <xsl:value-of select="concat($head, $replace, $tail)"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$input"/>
	</xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!--
      A template for enclosing a text value in quotes.
  -->
  <xsl:template name="csv-quote">
    <xsl:param name="input"/>
    <xsl:value-of select="$quote"/>
    <xsl:call-template name="replace-string">
      <xsl:with-param name="input" select="$input"/>
      <xsl:with-param name="search" select="$quote"/>
      <xsl:with-param name="replace" select="concat($quote,$quote)"/>
    </xsl:call-template>
    <xsl:value-of select="$quote"/>
  </xsl:template>

  <!--
      Output headers
  -->
  <xsl:template name="header">
    <xsl:for-each select="/res:sparql/res:head/res:variable">
      <xsl:variable name="name" select="@name"/>
      <!-- assuming header names never need to be quoted... -->
      <xsl:value-of select="$name"/>
      <xsl:if test="position() != last()">
	<xsl:value-of select="$delim"/>
      </xsl:if>
    </xsl:for-each>
    <xsl:value-of select="$eol"/>
  </xsl:template>

  <!--
      Output one result row.
  -->
  <xsl:template match="res:result">
    <xsl:variable name="result" select="."/>
    <xsl:for-each select="/res:sparql/res:head/res:variable">
      <xsl:variable name="name" select="@name"/>
      <xsl:call-template name="csv-quote">
	<xsl:with-param name="input" select="$result/res:binding[@name=$name]/*"/>
      </xsl:call-template>
      <xsl:if test="position() != last()">
	<xsl:value-of select="$delim"/>
      </xsl:if>
    </xsl:for-each>
    <xsl:value-of select="$eol"/>
  </xsl:template>

  <!--
      Main template
  -->
  <xsl:template match="/">
    <xsl:call-template name="header"/>
    <xsl:apply-templates select="res:sparql/res:results/res:result"/>
  </xsl:template>

</xsl:stylesheet>
