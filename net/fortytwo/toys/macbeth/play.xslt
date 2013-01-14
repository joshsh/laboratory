<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

    <xsl:template match="/PLAY">
        <xsl:for-each select="ACT">
            <xsl:for-each select="TITLE">
                <xsl:variable name="text">
                    <xsl:call-template name="sanitize">
                        <xsl:with-param name="text" select="."/>
                    </xsl:call-template>
                </xsl:variable>
                <xsl:text>echo &quot;</xsl:text>
                <xsl:value-of select="$text"/>
                <xsl:text>&quot;; say -v $Narrator &quot;</xsl:text>
                <xsl:value-of select="."/>
                <xsl:text>&quot;
</xsl:text>
            </xsl:for-each>
            <xsl:for-each select="SCENE">
                <xsl:for-each select="*">
                    <xsl:choose>
                        <xsl:when test="name() = 'TITLE'">
                            <xsl:variable name="text">
                                <xsl:call-template name="sanitize">
                                    <xsl:with-param name="text" select="."/>
                                </xsl:call-template>
                            </xsl:variable>
                            <xsl:text>echo &quot;</xsl:text>
                            <xsl:value-of select="$text"/>
                            <xsl:text>&quot;; say -v $Narrator &quot;</xsl:text>
                            <xsl:call-template name="sanitize">
                                <xsl:with-param name="text" select="."/>
                            </xsl:call-template>
                            <xsl:text>&quot;
</xsl:text>
                        </xsl:when>
                        <xsl:when test="name() = 'STAGEDIR'">
                            <xsl:variable name="text">
                                <xsl:call-template name="sanitize">
                                    <xsl:with-param name="text" select="."/>
                                </xsl:call-template>
                            </xsl:variable>
                            <xsl:text>echo &quot;</xsl:text>
                            <xsl:value-of select="$text"/>
                            <xsl:text>&quot;; say -v $Narrator &quot;</xsl:text>
                            <xsl:call-template name="sanitize">
                                <xsl:with-param name="text" select="."/>
                            </xsl:call-template>
                            <xsl:text>&quot;
</xsl:text>
                        </xsl:when>
                        <xsl:when test="name() = 'SPEECH'">
                            <xsl:variable name="speaker" select="translate(SPEAKER/text(), ' ', '_')"/>
                            <xsl:for-each select="LINE">
                                <xsl:variable name="text">
                                    <xsl:call-template name="sanitize">
                                        <xsl:with-param name="text" select="."/>
                                    </xsl:call-template>
                                </xsl:variable>
                                <xsl:text>echo &quot;</xsl:text>
                                <xsl:value-of select="$speaker"/>
                                <xsl:text>: </xsl:text><xsl:value-of select="$text"/>
                                <xsl:text>&quot;; say -v $</xsl:text>
                                <xsl:value-of select="$speaker"/>
                                <xsl:text> &quot;</xsl:text>
                                <xsl:call-template name="sanitize">
                                    <xsl:with-param name="text" select="."/>
                                </xsl:call-template>
                                <xsl:text>&quot;
</xsl:text>
                            </xsl:for-each>
                        </xsl:when>
                        <xsl:otherwise></xsl:otherwise>
                    </xsl:choose>
                </xsl:for-each>
            </xsl:for-each>
        </xsl:for-each>
    </xsl:template>

    <xsl:template name="sanitize">
        <xsl:param name="text"/>
        <xsl:value-of select="translate($text, '!', '..')"/>
    </xsl:template>
</xsl:stylesheet>