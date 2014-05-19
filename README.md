LaTeXQuery
==========

About
-----

LaTeXQuery is a script written in XQuery language for converting LaTeX to XML format.
This solution aims to produce clean XML document which elements correspond to the semantic structure of a LaTeX document.

This XML can be easly used for futher processing.

Installation
----

*  Dowload BaseX.zip from [basex.org](http://basex.org) and extract it
   to your system directory.

*  Edit latex2xml.sh and modify 'basexpath' by adding the location of basex directory.

Usage
-----
   Navigate to LaTeXQuery directory and run a command:

   ```
   ./latex2xml.sh latexinput.tex latexoutput.xml.
   ```

Limitations
-----

There may be cases in which the script recognizes the syntax wrongly.
It especially concerns the recognition of the number of LaTeX funcion parameters.
This inconvinience will be corrected in the next version of application.

