<?xml version="1.0"?>

<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text"/>


<xsl:template match="/"><xsl:apply-templates/></xsl:template>

<xsl:template match="param">{<xsl:apply-templates/>}</xsl:template>

<xsl:template match="param[@type='opt']">[<xsl:apply-templates/>]</xsl:template>

<xsl:template match="command">\<xsl:value-of select="@name"/><xsl:if test="@aster">*</xsl:if><xsl:apply-templates/></xsl:template>

<xsl:template match="environment">\begin{<xsl:value-of select="@name"/><xsl:if test="@aster">*</xsl:if>}<xsl:apply-templates/>\end{<xsl:value-of select="@name"/><xsl:if test="@aster">*</xsl:if>}</xsl:template>

<xsl:template match="mathmode[@type='dollar']">$<xsl:apply-templates/>$</xsl:template>

<xsl:template match="mathmode[@type='doubledollar']">$$<xsl:apply-templates/>$$</xsl:template>

<xsl:template match="mathmode[@type='parentheses']">\(<xsl:apply-templates/>\)</xsl:template>

<xsl:template match="mathmode[@type='brackets']">\[<xsl:apply-templates/>\]</xsl:template>

<xsl:template match="verb">\verb<xsl:value-of select="@delim"/><xsl:apply-templates/><xsl:value-of select="@delim"/></xsl:template>

<xsl:template match="comment()">%<xsl:value-of select="."/></xsl:template>
<!--<xsl:template match="comment">%<xsl:value-of select="."/></xsl:template>-->

<!-- add space after item -->
<xsl:template match="command[@name='item']"><xsl:variable name="posoftitleopt">
      <xsl:value-of select="
             if(node()[1] instance of element() and node()[local-name()='param'] and node()[@type='opt']) then 1
             else if(node()[2] instance of element() and node()[local-name()='param'] and node()[@type='opt']) then 2
            else 0 "/>
     </xsl:variable>\item<xsl:apply-templates select="node()[position() &lt;= $posoftitleopt]"/><xsl:text> </xsl:text>
<xsl:apply-templates select="node()[position() &gt; $posoftitleopt]"/></xsl:template>

</xsl:stylesheet> 
