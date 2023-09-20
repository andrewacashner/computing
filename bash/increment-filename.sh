increment="1"
recent_pdfs=$(find $base_out-*.pdf 2> /dev/null)

if [ "$?" -eq 0 ]
then 
    last_pdf=$(basename -s '.pdf' $(tail -n 1 <<< "$recent_pdfs"))
    increment=$(($(grep -o -e '[0-9]*$' <<< "$last_pdf") + 1))
fi

pdf_out="$base_out-$increment.pdf"


