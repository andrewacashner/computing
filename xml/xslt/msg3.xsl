<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0">
  <xsl:template match="/">
    <TEI>
      <xsl:apply-templates />
    </TEI>
  </xsl:template>
  <xsl:template match="msg">
    <p><xsl:apply-templates /></p>
  </xsl:template>
  <xsl:template match="note">
    <note place="end"><xsl:apply-templates /></note>
  </xsl:template>
  <xsl:template match="poemtranslation">
    <table cols="2"><row type="data"><xsl:apply-templates /></row></table>
  </xsl:template>
  <xsl:template match="poemtranslation/original">
    <cell><xsl:apply-templates /></cell>
  </xsl:template>
  <xsl:template match="poemtranslation/translation">
    <cell><xsl:apply-templates /></cell>
  </xsl:template>
</xsl:transform>
