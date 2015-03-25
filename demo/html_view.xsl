<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="html" />


<xsl:template match="/">
<html>
<head>
  <title><xsl:value-of select=".//command[@name='author']"/> <xsl:text>, </xsl:text> <xsl:value-of select=".//command[@name='title']"/></title>
</head>

<script type="text/x-mathjax-config">
  MathJax.Hub.Config({
    tex2jax: {
      inlineMath: [ ['$','$'], ["\\(","\\)"] ],
      processEscapes: true
    }
  });
</script>
<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>



<style type='text/css'>

p{
    text-indent: 1.5em; 
    margin:0pt;
    line-height:130%;
}

div.title p{
  text-align: center; 
   margin-bottom:1ex;
}


div.title {
      margin-top:5ex;
      margin-bottom:2ex; 
}



p.title{font-size:x-large; }

span.latexlogo{
   font-family:monospace, bold;
   color:#003366;
}

span.emph{font-style:italic;
          color:#333333;
         }







a.footnote {
  position: relative;
  display: inline;
}
a.footnote span {
  position: absolute;
  width:640px;
  color: #FFFFFF;
  background: #FF9933;
  height: 30px;
  line-height: 30px;
  text-align: center;
  visibility: hidden;
}

a:hover.footnote span {
  visibility: visible;
  bottom: 30px;
  left: 50%;
  margin-left: -76px;
  z-index: 999;
}


span.mathmode{
color:#003366;
}


div.quote{
margin-left:5em;
width:70%;
color:#333333;
font-style:italic;
}

div.verse{
white-space: pre-wrap;
}
</style>


<body>

<div class="title">

<p class="title"><xsl:value-of select=".//command[@name='title']"/></p>
<p class="author"><xsl:value-of select=".//command[@name='author']"/></p>
<p class="date"><xsl:value-of select=".//command[@name='date']"/></p>
</div>

<xsl:apply-templates select=".//environment[@name='document']"/>
</body>
</html>
</xsl:template>




<xsl:template match="p"><p><xsl:apply-templates/></p></xsl:template>


<xsl:template match="command[@name='section']">
    <h1><xsl:apply-templates select="param[not(@type)][1]"/></h1>
    <xsl:apply-templates select="p|environment|command"/>
</xsl:template>


<xsl:template match="command[not(param)][@name='LaTeX']"><span class="latexlogo"><xsl:value-of select="@name"/></span></xsl:template>
<xsl:template match="command[not(param)][@name=',']">&#8239;</xsl:template>
<xsl:template match="command[not(param)][@name=' ']">&#160;</xsl:template>
<xsl:template match="command[not(param)][@name='ldots']">&#x2026;</xsl:template>


<xsl:template match="command[@name='emph']/param"><span class="emph"><xsl:apply-templates/></span></xsl:template>

<xsl:template match="command[@name='footnote']/param"><a class="footnote" href="#">*<span><xsl:apply-templates/></span></a></xsl:template>


<xsl:template match="environment[@name='em']">
 <span class="emph"><xsl:apply-templates/></span>
</xsl:template>


<xsl:template match="environment[@name='quote' or @name='quotation']">
 <div class="quote"><xsl:apply-templates/></div>
</xsl:template>



<xsl:template match="environment[@name='itemize']">
 <ul><xsl:apply-templates/></ul>
</xsl:template>


<xsl:template match="environment[@name='enumerate']">
 <ol><xsl:apply-templates/></ol>
</xsl:template>

<xsl:template match="command[@name='item']"><li><xsl:apply-templates/></li></xsl:template>


<xsl:template match="mathmode[@type='parentheses']">
 <span class="mathmode">\(<xsl:apply-templates/>\)</span>
</xsl:template>

<xsl:template match="mathmode[@type='dollar']">
 <span class="mathmode">$<xsl:apply-templates/>$</span>
</xsl:template>

<xsl:template match="mathmode[@type='brackets']">
 <span class="mathmode">\[<xsl:apply-templates/>\]</span>
</xsl:template>


<xsl:template match="environment[@name='verse']">
 <div class="verse"><xsl:apply-templates/></div>
</xsl:template>






</xsl:stylesheet> 
