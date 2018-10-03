<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
  version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xi="http://www.w3.org/2001/XInclude">

  <xsl:output
    method="xml"
    indent="yes"/>

  <xsl:strip-space elements="*"/>

  <xsl:template match="@* | node()">
    <xsl:copy>
      <xsl:apply-templates select="@* | node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="/">
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="lirio">
    <mei>
      <xsl:apply-templates/>
    </mei>
  </xsl:template>

  <xsl:template match="lyStaff | lyVoice" />

  <xsl:template match="@meter.sym"/>

  <xsl:template match="staffDef">
    <staffDef id="{@id}" clef.shape="{@clef.shape}" clef.line="{@clef.line}"
      labelAbbr="{labelAbbr}">
      <xsl:apply-templates/>
    </staffDef>
  </xsl:template>

  <xsl:template match="staffDef/labelAbbr" />

  <xsl:template match="make-measures" name="make-measures">
    <xsl:param name="start" select="@start"/>
    <xsl:param name="end" select="@end"/>
    <xsl:choose>
      <xsl:when test="$start > $end">
      </xsl:when>
      <xsl:otherwise>
        <measure n="{$start}">
        <xsl:for-each select="make-staff">
          <staff def="#{@id}">
            <xi:include href="{@href}" xpointer="{@id}-{$start}"/>
          </staff>
        </xsl:for-each>
      </measure>
        <xsl:call-template name="make-measures">
          <xsl:with-param name="start" select="$start + 1"/>
          <xsl:with-param name="end" select="$end"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


</xsl:stylesheet>

