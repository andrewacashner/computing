<?xml version="1.0" encoding="UTF-8"?>
<html xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xsl:version="2.0">
<head>
	<title>Hello World Example</title>
</head>
<body>
	<p>
	<xsl:value-of select="/msg" />
	(<xsl:value-of select="/note" />)
	</p>
</body>
</html>
