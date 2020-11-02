static void
de_dotdot( char* f )
    {
    char* cp;
    char* cp2;
    int l;

    /* Collapse any multiple / sequences. */
    while ( ( cp = strstr( f, "//") ) != (char*) 0 )
	{
	for ( cp2 = cp + 2; *cp2 == '/'; ++cp2 )
	    continue;
	(void) ol_strcpy( cp + 1, cp2 );
	}

    /* Remove leading ./ and any /./ sequences. */
    while ( strncmp( f, "./", 2 ) == 0 )
	(void) ol_strcpy( f, f + 2 );
    while ( ( cp = strstr( f, "/./") ) != (char*) 0 )
	(void) ol_strcpy( cp, cp + 2 );

    /* Alternate between removing leading ../ and removing xxx/../ */
    for (;;)
	{
	while ( strncmp( f, "../", 3 ) == 0 )
	    (void) ol_strcpy( f, f + 3 );
	cp = strstr( f, "/../" );
	if ( cp == (char*) 0 )
	    break;
	for ( cp2 = cp - 1; cp2 >= f && *cp2 != '/'; --cp2 )
	    continue;
	(void) ol_strcpy( cp2 + 1, cp + 4 );
	}

    /* Also elide any xxx/.. at the end. */
    while ( ( l = strlen( f ) ) > 3 &&
	    strcmp( ( cp = f + l - 3 ), "/.." ) == 0 )
	{
	for ( cp2 = cp - 1; cp2 >= f && *cp2 != '/'; --cp2 )
	    continue;
	if ( cp2 < f )
	    break;
	*cp2 = '\0';
	}
    }
