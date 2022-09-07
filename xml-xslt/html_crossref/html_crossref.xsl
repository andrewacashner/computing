<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
  version="2.0" 
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  exclude-result-prefixes="xhtml">

  <xsl:output method="html" version="5.0" encoding="utf-8" indent="yes" include-content-type="no" />

  <xsl:strip-space elements="*" /> 

  <xsl:template match="comment()" />

  <xsl:template match="@* | node()">
    <xsl:copy>
      <xsl:apply-templates select="@* | node()" />
    </xsl:copy>
  </xsl:template>

  <xsl:variable name="figures">
    <xsl:for-each select="//xhtml:figure[not(@class)]">
      <xsl:copy>
        <xsl:attribute name="id" select="@id" />
        <xsl:attribute name="n">
          <xsl:number count="xhtml:figure[not(@class)]" />
        </xsl:attribute>
      </xsl:copy>
    </xsl:for-each>
  </xsl:variable>

  <xsl:variable name="music-examples">
    <xsl:for-each select="//xhtml:figure[@class='music']">
      <xsl:copy>
        <xsl:attribute name="id" select="@id" />
        <xsl:attribute name="n">
          <xsl:number count="xhtml:figure[@class='music']" />
        </xsl:attribute>
      </xsl:copy>
    </xsl:for-each>
  </xsl:variable>

  <xsl:variable name="tables">
    <xsl:for-each select="//xhtml:table">
      <xsl:copy>
        <xsl:attribute name="id" select="@id" />
        <xsl:attribute name="n">
          <xsl:number count="xhtml:table" />
        </xsl:attribute>
      </xsl:copy>
    </xsl:for-each>
  </xsl:variable>

  <xsl:template match="xhtml:a[@class='ref']">
    <xsl:variable name="label" select="substring(@href, 2)" />
   
    <xsl:variable name="source">
      <xsl:choose>
        <xsl:when test="@data-type='figure'">
          <xsl:copy-of select="$figures" />
        </xsl:when>
        <xsl:when test="@data-type='music'">
          <xsl:copy-of select="$music-examples" />
        </xsl:when>
        <xsl:when test="@data-type='table'">
          <xsl:copy-of select="$tables" />
        </xsl:when>
      </xsl:choose>
    </xsl:variable>
    
    <a class="ref" href="{@href}">
      <xsl:apply-templates />
      <xsl:text> </xsl:text>
      <xsl:value-of select="$source/*[@id=$label]/@n" />
    </a>
  </xsl:template>

  <xsl:template match="xhtml:figure[not(@class)]/xhtml:figcaption">
    <figcaption>
      <xsl:text>Figure </xsl:text>
      <xsl:number format="1. " count="xhtml:figure[not(@class)]" />
      <xsl:apply-templates />
    </figcaption>
  </xsl:template>

  <xsl:template match="xhtml:figure[@class='music']/xhtml:figcaption">
    <figcaption>
      <xsl:text>Music example </xsl:text>
      <xsl:number format="1. " count="xhtml:figure[@class='music']" />
      <xsl:apply-templates />
    </figcaption>
  </xsl:template>

  <xsl:template match="xhtml:table/xhtml:caption">
    <caption>
      <xsl:text>Table </xsl:text>
      <xsl:number format="1. " count="xhtml:table" />
      <xsl:apply-templates />
    </caption>
  </xsl:template>



</xsl:stylesheet>
