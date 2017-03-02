#! /bin/sh

set -e

NO_ARGS="0"
EXIT_ERROR="85"
PDF_MODE="0"
EPS_MODE="1"

# Function to display help information and exit
help_exit () {
	echo "Usage: lilycrop [-e] file.ly"
	echo "          Option -e : Produce EPS output"
	echo "          Default   : Produce PDF output"
	exit "$EXIT_ERROR"
}

# Check for command-line arguments
#   If none, explain proper usage and exit with error
if [ "$#" -eq "$NO_ARGS" ] # Script invoked with no command-line arguments?
then	
	help_exit
fi

# Set default output mode
mode="$PDF_MODE"

# Check for option argument to change output mode
while getopts "e" option
do
	case "$option" in
		e )	mode="$EPS_MODE"
			# Move to next argument (filename)
			shift $(($OPTIND - 1))
			;;
		* )	# Automatic error message for invalid option
			;; 
	esac
done

# Check for valid filename
if [ ! -f "$1" ]
then
	echo "ERROR: Invalid filename "$1""
	help_exit
fi

# Set filename
filename="$1"

# Pretend to make EPS
echo "Converting to "$filename".eps ..."

# If PDF_MODE, then pretend to make PDF
if [ "$mode" -eq "$PDF_MODE" ]
then
	echo "Converting to "$filename".pdf ..."
fi

echo "All done."

exit 0
