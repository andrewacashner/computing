<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
  version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xi="http://www.w3.org/2001/XInclude">

  <xsl:output
    method="text"
    indent="no" />

  <xsl:strip-space elements="*"/>

  <xsl:template match="@* | node()">
    <xsl:copy>
      <xsl:apply-templates select="@* | node()"/>
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="/">
    <xsl:apply-templates select="//meiHead/encodingDesc"/> 
    <xsl:apply-templates select="//meiHead/fileDesc"/>
    <xsl:apply-templates select="//music"/>
  </xsl:template>

  <xsl:template match="meiHead/encodingDesc/appInfo">
    <xsl:apply-templates select="application[@label='lirio']"/>
    <xsl:apply-templates select="application[@label='lilypond']"/>
    <xsl:text>\include "villancico.ly"&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="meiHead/encodingDesc/appInfo/application[@label='lilypond']">
    <xsl:text>\version "</xsl:text>
    <xsl:value-of select="@version"/>
    <xsl:text>"&#xA;</xsl:text>
  </xsl:template>
  
  <xsl:template match="meiHead/encodingDesc/appInfo/application[@label='lirio']">
    <xsl:text>% Created from XML original by lirio&#xA;</xsl:text>
  </xsl:template>
  
  <xsl:template match="meiHead/fileDesc">
    <xsl:text>\header {&#xA;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}&#xA;</xsl:text>
  </xsl:template>
 
  <xsl:template match="meiHead/fileDesc/titleStmt/title[@type='main']">
    <xsl:text>title = \markup { "</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>" }&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="meiHead/fileDesc/titleStmt/title[@type='subtitle']">
    <xsl:text>subtitle = \markup { "</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>" }&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="meiHead/fileDesc/titleStmt/respStmt/composer">
    <xsl:text>composer = \markup { "</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>" }&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="meiHead/fileDesc/titleStmt/respStmt/lyricist">
    <xsl:text>poet = \markup { "</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>" }&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="meiHead/fileDesc/titleStmt/respStmt/editor">
    <xsl:text>editor = \markup { "Edited by </xsl:text>
    <xsl:apply-templates/>
    <xsl:text>" }&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="meiHead/fileDesc/pubStmt/availability">
    <xsl:text>copyright = \markup { "</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>" }&#xA;</xsl:text>
  </xsl:template>
 
  <xsl:template match="meiHead/fileDesc/sourceDesc/identifier">
    <xsl:text>source = \markup { \concat { "Source: " \italic "</xsl:text>
    <xsl:apply-templates select="repository"/>
    <xsl:text>" ": </xsl:text>
    <xsl:apply-templates select="idno"/>
    <xsl:text>" } }&#xA;</xsl:text>
  </xsl:template>


  <xsl:template match="meiHead/fileDesc/editionStmt" />

  <xsl:template match="meiHead/revisionDesc" />


  <xsl:template match="music">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="score">
    <xsl:text>\score {&#xA;&lt;&lt;&#xA;</xsl:text>
    <xsl:apply-templates select="scoreDef/staffGrp"/>
    <xsl:text>&#xA;&gt;&gt;&#xA;}&#xA;</xsl:text>
  </xsl:template>


  <xsl:template match="staffGrp">
    <xsl:text>\new </xsl:text>
    <xsl:value-of select="@type"/>
    <xsl:text> = "</xsl:text>
    <xsl:value-of select="@id"/>
    <xsl:text>"&#xA;&lt;&lt;&#xA;</xsl:text>
    <xsl:apply-templates select="staffDef/lyStaff"/>
    <xsl:text>&gt;&gt;&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="lyStaff">
    <xsl:text>\new Staff = "</xsl:text>
    <xsl:value-of select="@id"/>
    <xsl:text>"&#xA;&lt;&lt;&#xA;</xsl:text>
    <xsl:text>\InstrumentName "</xsl:text>
    <xsl:apply-templates select="../label"/>
    <xsl:text>" "</xsl:text>
    <xsl:apply-templates select="../labelAbbr"/>
    <xsl:text>"&#xA;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&gt;&gt;&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="lyVoice">
    <xsl:text>\new Voice = "</xsl:text>
    <xsl:value-of select="@xml:id"/>
    <xsl:text>" {&#xA;</xsl:text>
    <xsl:if test="ancestor::staffDef[(@clef.shape='G') and (@clef.line='2')]">
      <xsl:text>\clef "treble"&#xA;</xsl:text>
    </xsl:if>
    <xsl:if test="ancestor::scoreDef[@meter.sym='C3']">
      <xsl:text>\MeterTriple&#xA;</xsl:text>
    </xsl:if>
    <xsl:if test="ancestor::scoreDef[@key.sig='1f']">
      <xsl:text>\CantusMollis&#xA;</xsl:text>
    </xsl:if>
    <xsl:apply-templates/>
    <xsl:text>}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="layer">
    <xsl:text>% </xsl:text>
    <xsl:value-of select="@xml:id"/>
    <xsl:text>&#xA;| </xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="rest">
    <xsl:text>r</xsl:text>
    <xsl:value-of select="@dur"/>
    <xsl:if test="@dots='1'">
      <xsl:text>.</xsl:text>
    </xsl:if>
    <xsl:text> </xsl:text>
  </xsl:template>

  <xsl:template match="note">
    <xsl:value-of select="@pname"/>
    <xsl:choose>
      <xsl:when test="(@accid='f') or (@accid.ges='f')">
        <xsl:text>es</xsl:text>
      </xsl:when>
      <xsl:when test="(@accid='s') or (@accid.ges='s')">
        <xsl:text>is</xsl:text>
      </xsl:when>
      <xsl:otherwise>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="@oct='1'">
        <xsl:text>,,</xsl:text>
      </xsl:when>
      <xsl:when test="@oct='2'">
        <xsl:text>,</xsl:text>
      </xsl:when>
      <xsl:when test="@oct='3'">
      </xsl:when>
      <xsl:when test="@oct='4'">
        <xsl:text>'</xsl:text>
      </xsl:when>
      <xsl:when test="@oct='5'">
        <xsl:text>''</xsl:text>
      </xsl:when>
      <xsl:when test="@oct='6'">
        <xsl:text>'''</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message terminate="yes">Bad octave
          <xsl:value-of select="@oct"/></xsl:message>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of select="@dur"/>
    <xsl:if test="@dots='1'">
      <xsl:text>.</xsl:text>
    </xsl:if>
    <xsl:text> </xsl:text>
  </xsl:template>


  <xsl:template match="make-measures" />

  <!--
  <xsl:template match="score">
    <xsl:text>\score {&#xA;&lt;&lt;&#xA;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&gt;&gt;&#xA;}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="staffgroup">
    <xsl:text>\new </xsl:text>
    <xsl:value-of select="@type"/>
    <xsl:text> = "</xsl:text>
    <xsl:value-of select="@id"/>
    <xsl:text>"&#xA;&lt;&lt;&#xA;</xsl:text>
    <xsl:if test="@subtype='continuo'">
      <xsl:text>\ShowChoirStaffBracket</xsl:text>
    </xsl:if>
    <xsl:apply-templates/>
    <xsl:text>&gt;&gt;&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="staff">
    <xsl:text>\new Staff = "</xsl:text>
    <xsl:value-of select="@id"/>
    <xsl:text>"</xsl:text>
    <xsl:text>&#xA;&lt;&lt;&#xA;</xsl:text>
    <xsl:apply-templates select="staffDef"/>
    <xsl:apply-templates select="layer"/>
    <xsl:text>&gt;&gt;&#xA;&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="staff/staffDef">
    <xsl:choose>
      <xsl:when test="incipit">
        <xsl:text>\IncipitStaff </xsl:text>
        <xsl:apply-templates select="label"/>
        <xsl:text> </xsl:text>
        <xsl:apply-templates select="labelAbbr"/>
        <xsl:text>&#xA;{&#xA;</xsl:text>
        <xsl:apply-templates select="incipit"/>
        <xsl:text>&#xA;}&#xA;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>\InstrumentName </xsl:text>
        <xsl:apply-templates select="label"/>
        <xsl:text> </xsl:text>
        <xsl:apply-templates select="labelAbbr"/>
        <xsl:text>&#xA;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="staff/staffDef/incipit">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="staff/staffDef/label | staff/staffDef/labelAbbr">
    <xsl:choose>
      <xsl:when test="list">
        <xsl:text>\TwoLineName "</xsl:text>
        <xsl:apply-templates select="list/line[@n='1']"/>
        <xsl:text>" "</xsl:text>
        <xsl:apply-templates select="list/line[@n='2']"/>
        <xsl:text>"&#xA;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>"</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>"</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="clef">
    <xsl:choose>
      <xsl:when test="@type='incipit'">
        <xsl:text>\MSclef</xsl:text>
        <xsl:value-of select="@shape"/>
        <xsl:choose>
          <xsl:when test="@line='1'">
            <xsl:text>i</xsl:text>
          </xsl:when>
          <xsl:when test="@line='2'">
            <xsl:text>ii</xsl:text>
          </xsl:when>
          <xsl:when test="@line='3'">
            <xsl:text>iii</xsl:text>
          </xsl:when>
          <xsl:when test="@line='4'">
            <xsl:text>iv</xsl:text>
          </xsl:when>
          <xsl:when test="@line='5'">
            <xsl:text>v</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:message terminate="yes">Bad incipit clef</xsl:message>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>&#xA;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>\clef "</xsl:text>
        <xsl:choose>
          <xsl:when test="(@shape='G') and (@line='2')">
            <xsl:text>treble</xsl:text>
          </xsl:when>
          <xsl:when test="(@shape='F') and (@line='4')">
            <xsl:text>bass</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:message terminate="yes">Bad clef</xsl:message>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>"&#xA;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="meter">
    <xsl:choose>
      <xsl:when test="@type='incipit'">
        <xsl:text>\Meter</xsl:text>
        <xsl:choose>
          <xsl:when test="@sym='CZ'">
            <xsl:text>CZ</xsl:text>
          </xsl:when>
          <xsl:when test="@sym='Z'">
            <xsl:text>Z</xsl:text>
          </xsl:when>
          <xsl:when test="@sym='C'">
            <xsl:text>C</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:message terminate="yes">Bad incipit meter</xsl:message>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>&#xA;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="(@count='3') and (@unit='2') and (@sym='C3')">
            <xsl:text>\MeterTriple</xsl:text>
          </xsl:when>
          <xsl:when test="(@count='2') and (@unit='2') and (@sym='C')">
            <xsl:text>\MeterDuple</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:message terminate="yes">Bad meter</xsl:message>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>&#xA;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="key[@sig='1f']">
    <xsl:text>\CantusMollis&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="layer">
    <xsl:text>\new Voice = "</xsl:text>
    <xsl:value-of select="@id"/>
    <xsl:text>" </xsl:text>
    <xsl:text>&#xA;{&#xA;</xsl:text>
    <xsl:apply-templates select="ancestor::staff/staffDef/clef"/>
    <xsl:apply-templates select="ancestor::staff/staffDef/meter"/>
    <xsl:apply-templates select="ancestor::staff/staffDef/key"/>
    <xsl:apply-templates/>
    <xsl:text>&#xA;}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="music">
    <xsl:text>&#xA;{&#xA;</xsl:text>
    <xsl:apply-templates />
    <xsl:text>&#xA;}&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="bar">
    <xsl:text>% </xsl:text>
    <xsl:value-of select="@n"/>
    <xsl:text>&#xA;</xsl:text>
    <xsl:text>| </xsl:text>
    <xsl:apply-templates/>
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

  <xsl:template match="rest">
    <xsl:choose>
      <xsl:when test="@type='full'">
        <xsl:text>R</xsl:text>
        <xsl:value-of select="@dur"/>
        <xsl:text>*</xsl:text>
        <xsl:value-of select="@n"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>r</xsl:text>
        <xsl:value-of select="@dur"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text> </xsl:text>
  </xsl:template>

  -->
  
  <xsl:template match="composer//text() | label//text()">
    <xsl:value-of select="upper-case(.)"/>
  </xsl:template>

  <xsl:template match="add">
    <xsl:text>[</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>]</xsl:text>
  </xsl:template>

</xsl:stylesheet>
