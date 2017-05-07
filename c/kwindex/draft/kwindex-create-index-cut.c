    /* Find keywords separated by keyword_delimiter and enter them into index
     * linked list in sorted order
     */
    while (fgets(line, sizeof(line), auxfile) != NULL) {
        /* Find filename in format "%s:" */
        strcpy(filename, strtok(line, filename_delimiter));
        convert_filename_format(format_filename, filename);
        
        /* Read another line to find keywords */
        if (fgets(line, sizeof(line), auxfile) != NULL) {

            /* Find group of keywords after filename in format "%s|" */
            strcpy(keyword_group, strtok(line, filegroup_delimiter)); 

            /* Find individual keywords within that group in format "%s;" */
            search_ptr = strtok(keyword_group, keyword_delimiter);
            while (search_ptr != NULL) {
                
                /* Insert this keyword, minus initial whitespace, and this
                 * filename into index */
                search_ptr = convert_trim_whitespace(search_ptr);
                strcpy(keyword, search_ptr);
                index = list_insert_sorted(index, keyword, format_filename);
                
                /* Find next keyword */
                search_ptr = strtok(NULL, keyword_delimiter);
            } 
        } 
    }
 
