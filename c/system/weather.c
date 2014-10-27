/* weather.c -- Get weather from weather.gov and print the forecast using LaTeX
 * AAC 2014-10-23 
 */

 #include <stdio.h>
 #include <stlib.h>
 #include <string.h>

/* Download weather forecast and save to text file
 * Locate start of forecast in file, strip out all before
 * Locate end, strip out after
 * Replace HTML with LaTeX markup
 * Process with LaTeX into PDF
 * Print PDF
 * Check for errors all along the way 
 */


