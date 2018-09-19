<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
  version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output
    method="text"
    indent="no" />

  <xsl:strip-space elements="*"/>

  <xsl:template match="/">
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="version">
    <xsl:text>\version "</xsl:text>
    <xsl:value-of select="@n"/>
    <xsl:text>"&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="include">
    <xsl:text>\include "</xsl:text>
    <xsl:value-of select="@href"/>
    <xsl:text>"&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="score">
    <xsl:text>\score {&#xA;&lt;&lt;&#xA;</xsl:text>
    <xsl:apply-templates />
    <xsl:text>&gt;&gt;&#xA;}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="staffGroup">
    <xsl:text>\new StaffGroup = "</xsl:text>
    <xsl:value-of select="@id"/>
    <xsl:text>"&#xA;&lt;&lt;&#xA;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&gt;&gt;&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="staff">
    <xsl:text>\new Staff = "</xsl:text>
    <xsl:value-of select="@id"/>
    <xsl:text>"&#xA;&lt;&lt;&#xA;</xsl:text>
    <xsl:apply-templates />
    <xsl:text>&gt;&gt;&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="voice">
    <xsl:text>\new Voice = "</xsl:text>
    <xsl:value-of select="@id"/>
    <xsl:text>" { </xsl:text>
    <xsl:choose>
      <xsl:when test="@variable">
        <xsl:text>\</xsl:text>
        <xsl:value-of select="@variable"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text> }&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="music">
    <xsl:text>&#xA;{&#xA;</xsl:text>
    <xsl:apply-templates />
    <xsl:text>&#xA;}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="clef">
    <xsl:text>\clef "</xsl:text>
    <xsl:choose>
      <xsl:when test="(@key='g') and (@pos='2')">
        <xsl:text>treble</xsl:text>
      </xsl:when>
      <xsl:when test="(@key='c') and (@pos='3')">
        <xsl:text>alto</xsl:text>
      </xsl:when>
      <xsl:when test="(@key='f') and (@pos='4')">
        <xsl:text>bass</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message terminate="yes">Unrecognized clef value
          <xsl:value-of select="@type"/>
        </xsl:message>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>"&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="meter">
    <xsl:text>\time </xsl:text>
    <xsl:value-of select="@a"/>
    <xsl:text>/</xsl:text>
    <xsl:value-of select="@b"/>
    <xsl:text>&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="bar">
    <xsl:text>\bar "</xsl:text>
    <xsl:choose>
      <xsl:when test="@type='final'">
        <xsl:text>|.</xsl:text>
      </xsl:when>
      <xsl:when test="@type='middle'">
        <xsl:text>||</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message terminate="yes">Unrecognized bar value
          <xsl:value-of select="@type"/>
        </xsl:message>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>"&#xA;</xsl:text>
  </xsl:template>
</xsl:stylesheet>
    
