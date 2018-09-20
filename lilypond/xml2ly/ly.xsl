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

  <xsl:template match="header">
    <xsl:text>\header {&#xA;</xsl:text>
    <xsl:text>title = "</xsl:text>
    <xsl:apply-templates select="title"/>
    <xsl:text>"&#xA;</xsl:text>
    <xsl:text>subtitle = "</xsl:text>
    <xsl:apply-templates select="subtitle"/>
    <xsl:text>"&#xA;</xsl:text>
    <xsl:text>composer = "</xsl:text>
    <xsl:apply-templates select="composer"/>
    <xsl:text>"&#xA;</xsl:text>
    <xsl:text>copyright = "</xsl:text>
    <xsl:apply-templates select="copyright"/>
    <xsl:text>"&#xA;</xsl:text>
    <xsl:text>tagline = ##f&#xA;</xsl:text>
    <xsl:text>&#xA;}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="score">
    <xsl:text>\score {&#xA;&lt;&lt;&#xA;</xsl:text>
    <xsl:apply-templates />
    <xsl:text>&gt;&gt;&#xA;}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="staffGroup">
    <xsl:text>\new </xsl:text>
    <xsl:value-of select="@type"/>
    <xsl:text> = "</xsl:text>
    <xsl:value-of select="@id"/>
    <xsl:text>"&#xA;&lt;&lt;&#xA;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&gt;&gt;&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="staff">
    <xsl:text>\new Staff = "</xsl:text>
    <xsl:value-of select="@id"/>
    <xsl:text>" </xsl:text>
    <xsl:if test="@name">
      <xsl:text>\with { instrumentName = "</xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:text>" }&#xA;</xsl:text>
    </xsl:if>
    <xsl:text>&lt;&lt;&#xA;</xsl:text>
    <xsl:apply-templates />
    <xsl:text>&gt;&gt;&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="voice">
    <xsl:text>\new Voice = "</xsl:text>
    <xsl:value-of select="@id"/>
    <xsl:text>" </xsl:text>
    <xsl:if test="@pos">
      <xsl:choose>
        <xsl:when test="@pos='1'">
          <xsl:text>{ \voiceOne </xsl:text>
        </xsl:when>
        <xsl:when test="@pos='2'">
          <xsl:text>{ \voiceTwo </xsl:text>
        </xsl:when>
      </xsl:choose>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@variable">
        <xsl:text>\</xsl:text>
        <xsl:value-of select="@variable"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="@pos">
      <xsl:text> }</xsl:text>
    </xsl:if>
    <xsl:text>&#xA;</xsl:text>
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
      <xsl:when test="@key='c'">
        <xsl:choose>
          <xsl:when test="@pos='1'">
            <xsl:text>soprano</xsl:text>
          </xsl:when>
          <xsl:when test="@pos='2'">
            <xsl:text>mezzosoprano</xsl:text>
          </xsl:when>
          <xsl:when test="@pos='3'">
            <xsl:text>alto</xsl:text>
          </xsl:when>
          <xsl:when test="@pos='4'">
            <xsl:text>tenor</xsl:text>
          </xsl:when>
        </xsl:choose>
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
    <xsl:value-of select="@sum"/>
    <xsl:text>/</xsl:text>
    <xsl:value-of select="@base"/>
    <xsl:text>&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="barline">
    <xsl:text>\bar "</xsl:text>
    <xsl:choose>
      <xsl:when test="@type='final'">
        <xsl:text>|.</xsl:text>
      </xsl:when>
      <xsl:when test="@type='middle'">
        <xsl:text>||</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message terminate="yes">Unrecognized barline value
          <xsl:value-of select="@type"/>
        </xsl:message>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>"&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="bar">
    <xsl:text>% </xsl:text>
    <xsl:value-of select="@n"/>
    <xsl:text>&#xA;</xsl:text>
    <xsl:text>| </xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="mn">
    <xsl:value-of select="@p"/>
    <xsl:choose> <!-- octave -->
      <xsl:when test="@o=0">,,,</xsl:when>
      <xsl:when test="@o=1">,,</xsl:when>
      <xsl:when test="@o=2">,</xsl:when>
      <xsl:when test="@o=3"/>
      <xsl:when test="@o=4">'</xsl:when>
      <xsl:when test="@o=5">''</xsl:when>
      <xsl:when test="@o=6">'''</xsl:when>
      <xsl:when test="@o=7">''''</xsl:when>
      <xsl:otherwise>
        <xsl:message terminate="yes">
          Octave value <xsl:value-of select="@o"/> out of range or invalid.
        </xsl:message>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of select="@d"/>
    <xsl:text> </xsl:text>
  </xsl:template>

  <xsl:template match="rest[@type='bar']">
    <xsl:text>| s</xsl:text>
    <xsl:value-of select="@d"/>
    <xsl:text>*</xsl:text>
    <xsl:value-of select="@length"/>
    <xsl:text>&#xA;</xsl:text>
  </xsl:template>


</xsl:stylesheet>
    
