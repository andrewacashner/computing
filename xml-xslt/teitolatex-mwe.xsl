<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	       xpath-default-namespace="http://www.tei-c.org/ns/1.0">
  <xsl:output method="text"/>
  
  <xsl:template match="/">
    % Converted from TEI-XML by Andrew's custom XSL teitolatex.xsl
    \documentclass{memoir}
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="teiHeader">
    <!-- Discard -->
  </xsl:template>
  
  <xsl:template match="text">
    \begin{document}
    \begin{verbatim}
    <xsl:apply-templates />
    \end{verbatim}
    \end{document}
  </xsl:template>
</xsl:transform>
