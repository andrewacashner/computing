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
        <xsl:apply-templates select="lirioMusic/lirioVoice/layer"/>
      </section>
    </score>
  </xsl:template>

  <xsl:template match="@meter.sym"/>

  <xsl:template match="staffDef">
    <staffDef n="{@n}" id="{@id}" 
      lines="5" 
      clef.shape="{@clef.shape}" clef.line="{@clef.line}"
      labelAbbr="{labelAbbr}">
    </staffDef>
  </xsl:template>

  <xsl:template match="staffDef/labelAbbr" />

  
  <xsl:template match="lyVoice" />

  <xsl:template match="//score/lirioMusic/lirioVoice/layer">
    <xsl:param name="href" select="//score/lirioMusic/@xml:base"/>
    <xsl:param name="measurenum" select="@n"/>
    <measure n="{$measurenum}">
      <xsl:for-each select="//score/scoreDef/staffGrp/staffDef"> 
        <staff n="{@n}" def="#{@id}">
          <xi:include href="{$href}" xpointer="{@id}-{$measurenum}"/>
        </staff>
      </xsl:for-each>
    </measure>
  </xsl:template>

</xsl:stylesheet>

