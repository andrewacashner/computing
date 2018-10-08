<?xml version="1.0" encoding="UTF-8"?>
<!-- lirio2mei -->
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
    <xsl:comment> Generated automatically from lirio XML </xsl:comment>
    <mei>
      <xsl:apply-templates/>
    </mei>
  </xsl:template>

  <xsl:template match="score">
    <score>
      <xsl:apply-templates select="scoreDef"/>
      <section>
        <xsl:apply-templates select="//score/mei/layer/layer"/>
      </section>
    </score>
  </xsl:template>

  <xsl:template match="layerDef" />

  <xsl:template match="@meter.sym"/>

  <xsl:template match="//score/mei/layer/layer">
    <xsl:param name="href" select="//score/mei/@xml:base"/>
    <xsl:param name="measurenum" select="@n"/>
    <measure n="{$measurenum}">
      <xsl:for-each select="//score/scoreDef/staffGrp/staffDef"> 
        <staff n="{@n}" def="#{@xml:id}">
          <xi:include href="{$href}" xpointer="{@xml:id}-{$measurenum}"/>
        </staff>
      </xsl:for-each>
    </measure>
  </xsl:template>

</xsl:stylesheet>

