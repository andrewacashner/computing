<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
  version="2.0"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="text" encoding="utf-8" indent="no" />

  <xsl:strip-space elements="*" />

  <xsl:template match="comment()" />
    
  <xsl:template match="/">
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="xhtml:ul">
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="xhtml:li">
    <xsl:apply-templates />
    <xsl:if test="not(position()=last())">
      <xsl:text>, </xsl:text>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>
