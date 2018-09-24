<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
  version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output
    method="text"
    indent="no" />

  <xsl:strip-space elements="*"/>

  <xsl:template match="/">
    <xsl:text>\include "villancico.ly"&#xA;</xsl:text>
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
    <xsl:for-each select="*">
      <xsl:value-of select="local-name()"/>
      <xsl:text> = "</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>"&#xA;</xsl:text>
    </xsl:for-each>
    <xsl:text>tagline = ##f&#xA;</xsl:text>
    <xsl:text>}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="header/composer/text()">
    <xsl:value-of select="upper-case(.)"/>
  </xsl:template>

  <xsl:template match="score">
    <xsl:text>\score {&#xA;&lt;&lt;&#xA;</xsl:text>
    <xsl:apply-templates />
    <xsl:text>&gt;&gt;&#xA;}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="staffgroup">
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
    <xsl:text>"&#xA;</xsl:text>
    <xsl:apply-templates select="name"/>
    <xsl:text>&#xA;&lt;&lt;&#xA;</xsl:text>
    <xsl:apply-templates select="voice"/>
    <xsl:text>&gt;&gt;&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="staff/name">
    <xsl:text>\with {&#xA;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="name/full">
    <xsl:text>instrumentName = </xsl:text>
    <xsl:choose>
      <xsl:when test="list">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>"</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>"&#xA;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="name/full//text()">
    <xsl:value-of select="upper-case(.)"/>
  </xsl:template>

  <xsl:template match="add">
    <xsl:text>[</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>]</xsl:text>
  </xsl:template>

  <xsl:template match="name/short">
    <xsl:text>shortInstrumentName = </xsl:text>
    <xsl:choose>
      <xsl:when test="list">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>"</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>"&#xA;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="full/list | short/list">
    <xsl:text>\markup {&#xA;\column {&#xA;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}&#xA;}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="line">
    <xsl:text>\line { </xsl:text>
    <xsl:apply-templates/>
    <xsl:text> }&#xA;</xsl:text>
  </xsl:template>

  <!-- TODO replace attributes in <voice> with subelements, avoid tests -->

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
      <xsl:when test="(@p='g') and (@pos='2')">
        <xsl:choose>
          <xsl:when test="@trans='-8'">
            <xsl:text>treble_8</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>treble</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="@p='c'">
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
      <xsl:when test="(@p='f') and (@pos='4')">
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
    <xsl:choose>
      <xsl:when test="@symbol">
        <xsl:if test="@symbol='C3'">
          <xsl:text>\MeterTriple</xsl:text>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>\time </xsl:text>
        <xsl:value-of select="@sum"/>
        <xsl:text>/</xsl:text>
        <xsl:value-of select="@base"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="key">
    <xsl:choose>
      <xsl:when test="@name='mollis'">
        <xsl:text>\CantusMollis</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>\key </xsl:text>
        <xsl:value-of select="@p"/>
        <xsl:text>\</xsl:text>
        <xsl:value-of select="@mode"/>
      </xsl:otherwise>
    </xsl:choose>
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
    <xsl:value-of select="@p"/> <!-- pitch -->

    <xsl:choose> <!-- accidental -->
      <xsl:when test="@acc='-'">es</xsl:when>
      <xsl:when test="@acc='='">!</xsl:when>
      <xsl:when test="@acc='+'">is</xsl:when>
    </xsl:choose>

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

    <xsl:value-of select="@d"/> <!-- duration -->

    <xsl:text> </xsl:text>
  </xsl:template>

  <xsl:template match="r">
    <xsl:choose>
      <xsl:when test="@type='full'">
        <xsl:text>R</xsl:text>
        <xsl:value-of select="@d"/>
        <xsl:text>*</xsl:text>
        <xsl:value-of select="@n"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>r</xsl:text>
        <xsl:value-of select="@d"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text> </xsl:text>
  </xsl:template>

  <xsl:template match="s">
    <xsl:text>| s</xsl:text>
    <xsl:value-of select="@d"/>
    <xsl:text>*</xsl:text>
    <xsl:value-of select="@length"/>
    <xsl:text>&#xA;</xsl:text>
  </xsl:template>


</xsl:stylesheet>
    
