
#if SQLITE_SHELL_IS_UTF8
int SQLITE_CDECL main(int argc, char **argv){
#else
int SQLITE_CDECL wmain(int argc, wchar_t **wargv){
  char **argv;
#endif
#ifdef SQLITE_DEBUG
  sqlite3_int64 mem_main_enter = 0;
#endif
  char *zErrMsg = 0;
#ifdef SQLITE_SHELL_FIDDLE
#  define data shellState
#else
  ShellState data;
#endif
  const char *zInitFile = 0;
  int i;
  int rc = 0;
  int warnInmemoryDb = 0;
  int readStdin = 1;
  int nCmd = 0;
  int nOptsEnd = argc;
  int bEnableVfstrace = 0;
  char **azCmd = 0;
  const char *zVfs = 0;           /* Value of -vfs command-line option */
#if !SQLITE_SHELL_IS_UTF8
  char **argvToFree = 0;
  int argcToFree = 0;
#endif
  setvbuf(stderr, 0, _IONBF, 0); /* Make sure stderr is unbuffered */

#ifdef SQLITE_SHELL_FIDDLE
  stdin_is_interactive = 0;
  stdout_is_console = 1;
  data.wasm.zDefaultDbName = "/fiddle.sqlite3";
#else
  stdin_is_interactive = isatty(0);
  stdout_is_console = isatty(1);
#endif
  atexit(sayAbnormalExit);
#ifdef SQLITE_DEBUG
  mem_main_enter = sqlite3_memory_used();
#endif
#if !defined(_WIN32_WCE)
  if( getenv("SQLITE_DEBUG_BREAK") ){
    if( isatty(0) && isatty(2) ){
      char zLine[100];
      sqlite3_fprintf(stderr,
            "attach debugger to process %d and press ENTER to continue...",
            GETPID());
      if( sqlite3_fgets(zLine, sizeof(zLine), stdin)!=0
       && cli_strcmp(zLine,"stop")==0
      ){
        exit(1);
      }
    }else{
#if defined(_WIN32) || defined(WIN32)
#if SQLITE_OS_WINRT
      __debugbreak();
#else
      DebugBreak();
#endif
#elif defined(SIGTRAP)
      raise(SIGTRAP);
#endif
    }
  }
#endif
  /* Register a valid signal handler early, before much else is done. */
#ifdef SIGINT
  signal(SIGINT, interrupt_handler);
#elif (defined(_WIN32) || defined(WIN32)) && !defined(_WIN32_WCE)
  if( !SetConsoleCtrlHandler(ConsoleCtrlHandler, TRUE) ){
    eputz("No ^C handler.\n");
  }
#endif

#if USE_SYSTEM_SQLITE+0!=1
  if( cli_strncmp(sqlite3_sourceid(),SQLITE_SOURCE_ID,60)!=0 ){
    sqlite3_fprintf(stderr,
          "SQLite header and source version mismatch\n%s\n%s\n",
          sqlite3_sourceid(), SQLITE_SOURCE_ID);
    exit(1);
  }
#endif
  main_init(&data);

  /* On Windows, we must translate command-line arguments into UTF-8.
  ** The SQLite memory allocator subsystem has to be enabled in order to
  ** do this.  But we want to run an sqlite3_shutdown() afterwards so that
  ** subsequent sqlite3_config() calls will work.  So copy all results into
  ** memory that does not come from the SQLite memory allocator.
  */
#if !SQLITE_SHELL_IS_UTF8
  sqlite3_initialize();
  argvToFree = malloc(sizeof(argv[0])*argc*2);
  shell_check_oom(argvToFree);
  argcToFree = argc;
  argv = argvToFree + argc;
  for(i=0; i<argc; i++){
    char *z = sqlite3_win32_unicode_to_utf8(wargv[i]);
    i64 n;
    shell_check_oom(z);
    n = strlen(z);
    argv[i] = malloc( n+1 );
    shell_check_oom(argv[i]);
    memcpy(argv[i], z, n+1);
    argvToFree[i] = argv[i];
    sqlite3_free(z);
  }
  sqlite3_shutdown();
#endif

  assert( argc>=1 && argv && argv[0] );
  Argv0 = argv[0];

#ifdef SQLITE_SHELL_DBNAME_PROC
  {
    /* If the SQLITE_SHELL_DBNAME_PROC macro is defined, then it is the name
    ** of a C-function that will provide the name of the database file.  Use
    ** this compile-time option to embed this shell program in larger
    ** applications. */
    extern void SQLITE_SHELL_DBNAME_PROC(const char**);
    SQLITE_SHELL_DBNAME_PROC(&data.pAuxDb->zDbFilename);
    warnInmemoryDb = 0;
  }
#endif

  /* Do an initial pass through the command-line argument to locate
  ** the name of the database file, the name of the initialization file,
  ** the size of the alternative malloc heap, options affecting commands
  ** or SQL run from the command line, and the first command to execute.
  */
#ifndef SQLITE_SHELL_FIDDLE
  verify_uninitialized();
#endif
  for(i=1; i<argc; i++){
    char *z;
    z = argv[i];
    if( z[0]!='-' || i>nOptsEnd ){
      if( data.aAuxDb->zDbFilename==0 ){
        data.aAuxDb->zDbFilename = z;
      }else{
        /* Excess arguments are interpreted as SQL (or dot-commands) and
        ** mean that nothing is read from stdin */
        readStdin = 0;
        nCmd++;
        azCmd = realloc(azCmd, sizeof(azCmd[0])*nCmd);
        shell_check_oom(azCmd);
        azCmd[nCmd-1] = z;
      }
      continue;
    }
    if( z[1]=='-' ) z++;
    if( cli_strcmp(z, "-")==0 ){
      nOptsEnd = i;
      continue;
    }else if( cli_strcmp(z,"-separator")==0
     || cli_strcmp(z,"-nullvalue")==0
     || cli_strcmp(z,"-newline")==0
     || cli_strcmp(z,"-cmd")==0
    ){
      (void)cmdline_option_value(argc, argv, ++i);
    }else if( cli_strcmp(z,"-init")==0 ){
      zInitFile = cmdline_option_value(argc, argv, ++i);
    }else if( cli_strcmp(z,"-interactive")==0 ){
    }else if( cli_strcmp(z,"-batch")==0 ){
      /* Need to check for batch mode here to so we can avoid printing
      ** informational messages (like from process_sqliterc) before
      ** we do the actual processing of arguments later in a second pass.
      */
      stdin_is_interactive = 0;
    }else if( cli_strcmp(z,"-utf8")==0 ){
    }else if( cli_strcmp(z,"-no-utf8")==0 ){
    }else if( cli_strcmp(z,"-no-rowid-in-view")==0 ){
      int val = 0;
      sqlite3_config(SQLITE_CONFIG_ROWID_IN_VIEW, &val);
      assert( val==0 );
    }else if( cli_strcmp(z,"-heap")==0 ){
#if defined(SQLITE_ENABLE_MEMSYS3) || defined(SQLITE_ENABLE_MEMSYS5)
      const char *zSize;
      sqlite3_int64 szHeap;

      zSize = cmdline_option_value(argc, argv, ++i);
      szHeap = integerValue(zSize);
      if( szHeap>0x7fff0000 ) szHeap = 0x7fff0000;
      verify_uninitialized();
      sqlite3_config(SQLITE_CONFIG_HEAP, malloc((int)szHeap), (int)szHeap, 64);
#else
      (void)cmdline_option_value(argc, argv, ++i);
#endif
    }else if( cli_strcmp(z,"-pagecache")==0 ){
      sqlite3_int64 n, sz;
      sz = integerValue(cmdline_option_value(argc,argv,++i));
      if( sz>70000 ) sz = 70000;
      if( sz<0 ) sz = 0;
      n = integerValue(cmdline_option_value(argc,argv,++i));
      if( sz>0 && n>0 && 0xffffffffffffLL/sz<n ){
        n = 0xffffffffffffLL/sz;
      }
      verify_uninitialized();
      sqlite3_config(SQLITE_CONFIG_PAGECACHE,
                    (n>0 && sz>0) ? malloc(n*sz) : 0, sz, n);
      data.shellFlgs |= SHFLG_Pagecache;
    }else if( cli_strcmp(z,"-lookaside")==0 ){
      int n, sz;
      sz = (int)integerValue(cmdline_option_value(argc,argv,++i));
      if( sz<0 ) sz = 0;
      n = (int)integerValue(cmdline_option_value(argc,argv,++i));
      if( n<0 ) n = 0;
      verify_uninitialized();
      sqlite3_config(SQLITE_CONFIG_LOOKASIDE, sz, n);
      if( sz*n==0 ) data.shellFlgs &= ~SHFLG_Lookaside;
    }else if( cli_strcmp(z,"-threadsafe")==0 ){
      int n;
      n = (int)integerValue(cmdline_option_value(argc,argv,++i));
      verify_uninitialized();
      switch( n ){
         case 0:  sqlite3_config(SQLITE_CONFIG_SINGLETHREAD);  break;
         case 2:  sqlite3_config(SQLITE_CONFIG_MULTITHREAD);   break;
         default: sqlite3_config(SQLITE_CONFIG_SERIALIZED);    break;
      }
    }else if( cli_strcmp(z,"-vfstrace")==0 ){
      vfstrace_register("trace",0,(int(*)(const char*,void*))sqlite3_fputs,
                        stderr,1);
      bEnableVfstrace = 1;
#ifdef SQLITE_ENABLE_MULTIPLEX
    }else if( cli_strcmp(z,"-multiplex")==0 ){
      extern int sqlite3_multiplex_initialize(const char*,int);
      sqlite3_multiplex_initialize(0, 1);
#endif
    }else if( cli_strcmp(z,"-mmap")==0 ){
      sqlite3_int64 sz = integerValue(cmdline_option_value(argc,argv,++i));
      verify_uninitialized();
      sqlite3_config(SQLITE_CONFIG_MMAP_SIZE, sz, sz);
#if defined(SQLITE_ENABLE_SORTER_REFERENCES)
    }else if( cli_strcmp(z,"-sorterref")==0 ){
      sqlite3_int64 sz = integerValue(cmdline_option_value(argc,argv,++i));
      verify_uninitialized();
      sqlite3_config(SQLITE_CONFIG_SORTERREF_SIZE, (int)sz);
#endif
    }else if( cli_strcmp(z,"-vfs")==0 ){
      zVfs = cmdline_option_value(argc, argv, ++i);
#ifdef SQLITE_HAVE_ZLIB
    }else if( cli_strcmp(z,"-zip")==0 ){
      data.openMode = SHELL_OPEN_ZIPFILE;
#endif
    }else if( cli_strcmp(z,"-append")==0 ){
      data.openMode = SHELL_OPEN_APPENDVFS;
#ifndef SQLITE_OMIT_DESERIALIZE
    }else if( cli_strcmp(z,"-deserialize")==0 ){
      data.openMode = SHELL_OPEN_DESERIALIZE;
    }else if( cli_strcmp(z,"-maxsize")==0 && i+1<argc ){
      data.szMax = integerValue(argv[++i]);
#endif
    }else if( cli_strcmp(z,"-readonly")==0 ){
      data.openMode = SHELL_OPEN_READONLY;
    }else if( cli_strcmp(z,"-nofollow")==0 ){
      data.openFlags = SQLITE_OPEN_NOFOLLOW;
#if !defined(SQLITE_OMIT_VIRTUALTABLE) && defined(SQLITE_HAVE_ZLIB)
    }else if( cli_strncmp(z, "-A",2)==0 ){
      /* All remaining command-line arguments are passed to the ".archive"
      ** command, so ignore them */
      break;
#endif
    }else if( cli_strcmp(z, "-memtrace")==0 ){
      sqlite3MemTraceActivate(stderr);
    }else if( cli_strcmp(z, "-pcachetrace")==0 ){
      sqlite3PcacheTraceActivate(stderr);
    }else if( cli_strcmp(z,"-bail")==0 ){
      bail_on_error = 1;
    }else if( cli_strcmp(z,"-nonce")==0 ){
      free(data.zNonce);
      data.zNonce = strdup(cmdline_option_value(argc, argv, ++i));
    }else if( cli_strcmp(z,"-unsafe-testing")==0 ){
      ShellSetFlag(&data,SHFLG_TestingMode);
    }else if( cli_strcmp(z,"-safe")==0 ){
      /* no-op - catch this on the second pass */
    }
  }
#ifndef SQLITE_SHELL_FIDDLE
  if( !bEnableVfstrace ) verify_uninitialized();
#endif


#ifdef SQLITE_SHELL_INIT_PROC
  {
    /* If the SQLITE_SHELL_INIT_PROC macro is defined, then it is the name
    ** of a C-function that will perform initialization actions on SQLite that
    ** occur just before or after sqlite3_initialize(). Use this compile-time
    ** option to embed this shell program in larger applications. */
    extern void SQLITE_SHELL_INIT_PROC(void);
    SQLITE_SHELL_INIT_PROC();
  }
#else
  /* All the sqlite3_config() calls have now been made. So it is safe
  ** to call sqlite3_initialize() and process any command line -vfs option. */
  sqlite3_initialize();
#endif

  if( zVfs ){
    sqlite3_vfs *pVfs = sqlite3_vfs_find(zVfs);
    if( pVfs ){
      sqlite3_vfs_register(pVfs, 1);
    }else{
      sqlite3_fprintf(stderr,"no such VFS: \"%s\"\n", zVfs);
      exit(1);
    }
  }

  if( data.pAuxDb->zDbFilename==0 ){
#ifndef SQLITE_OMIT_MEMORYDB
    data.pAuxDb->zDbFilename = ":memory:";
    warnInmemoryDb = argc==1;
#else
    sqlite3_fprintf(stderr,
                    "%s: Error: no database filename specified\n", Argv0);
    return 1;
#endif
  }
  data.out = stdout;
#ifndef SQLITE_SHELL_FIDDLE
  sqlite3_appendvfs_init(0,0,0);
#endif

  /* Go ahead and open the database file if it already exists.  If the
  ** file does not exist, delay opening it.  This prevents empty database
  ** files from being created if a user mistypes the database name argument
  ** to the sqlite command-line tool.
  */
  if( access(data.pAuxDb->zDbFilename, 0)==0 ){
    open_db(&data, 0);
  }

  /* Process the initialization file if there is one.  If no -init option
  ** is given on the command line, look for a file named ~/.sqliterc and
  ** try to process it.
  */
  process_sqliterc(&data,zInitFile);

  /* Make a second pass through the command-line argument and set
  ** options.  This second pass is delayed until after the initialization
  ** file is processed so that the command-line arguments will override
  ** settings in the initialization file.
  */
  for(i=1; i<argc; i++){
    char *z = argv[i];
    if( z[0]!='-' || i>=nOptsEnd ) continue;
    if( z[1]=='-' ){ z++; }
    if( cli_strcmp(z,"-init")==0 ){
      i++;
    }else if( cli_strcmp(z,"-html")==0 ){
      data.mode = MODE_Html;
    }else if( cli_strcmp(z,"-list")==0 ){
      data.mode = MODE_List;
    }else if( cli_strcmp(z,"-quote")==0 ){
      data.mode = MODE_Quote;
      sqlite3_snprintf(sizeof(data.colSeparator), data.colSeparator, SEP_Comma);
      sqlite3_snprintf(sizeof(data.rowSeparator), data.rowSeparator, SEP_Row);
    }else if( cli_strcmp(z,"-line")==0 ){
      data.mode = MODE_Line;
    }else if( cli_strcmp(z,"-column")==0 ){
      data.mode = MODE_Column;
    }else if( cli_strcmp(z,"-json")==0 ){
      data.mode = MODE_Json;
    }else if( cli_strcmp(z,"-markdown")==0 ){
      data.mode = MODE_Markdown;
    }else if( cli_strcmp(z,"-table")==0 ){
      data.mode = MODE_Table;
    }else if( cli_strcmp(z,"-box")==0 ){
      data.mode = MODE_Box;
    }else if( cli_strcmp(z,"-csv")==0 ){
      data.mode = MODE_Csv;
      memcpy(data.colSeparator,",",2);
#ifdef SQLITE_HAVE_ZLIB
    }else if( cli_strcmp(z,"-zip")==0 ){
      data.openMode = SHELL_OPEN_ZIPFILE;
#endif
    }else if( cli_strcmp(z,"-append")==0 ){
      data.openMode = SHELL_OPEN_APPENDVFS;
#ifndef SQLITE_OMIT_DESERIALIZE
    }else if( cli_strcmp(z,"-deserialize")==0 ){
      data.openMode = SHELL_OPEN_DESERIALIZE;
    }else if( cli_strcmp(z,"-maxsize")==0 && i+1<argc ){
      data.szMax = integerValue(argv[++i]);
#endif
    }else if( cli_strcmp(z,"-readonly")==0 ){
      data.openMode = SHELL_OPEN_READONLY;
    }else if( cli_strcmp(z,"-nofollow")==0 ){
      data.openFlags |= SQLITE_OPEN_NOFOLLOW;
    }else if( cli_strcmp(z,"-ascii")==0 ){
      data.mode = MODE_Ascii;
      sqlite3_snprintf(sizeof(data.colSeparator), data.colSeparator,SEP_Unit);
      sqlite3_snprintf(sizeof(data.rowSeparator), data.rowSeparator,SEP_Record);
    }else if( cli_strcmp(z,"-tabs")==0 ){
      data.mode = MODE_List;
      sqlite3_snprintf(sizeof(data.colSeparator), data.colSeparator,SEP_Tab);
      sqlite3_snprintf(sizeof(data.rowSeparator), data.rowSeparator,SEP_Row);
    }else if( cli_strcmp(z,"-separator")==0 ){
      sqlite3_snprintf(sizeof(data.colSeparator), data.colSeparator,
                       "%s",cmdline_option_value(argc,argv,++i));
    }else if( cli_strcmp(z,"-newline")==0 ){
      sqlite3_snprintf(sizeof(data.rowSeparator), data.rowSeparator,
                       "%s",cmdline_option_value(argc,argv,++i));
    }else if( cli_strcmp(z,"-nullvalue")==0 ){
      sqlite3_snprintf(sizeof(data.nullValue), data.nullValue,
                       "%s",cmdline_option_value(argc,argv,++i));
    }else if( cli_strcmp(z,"-header")==0 ){
      data.showHeader = 1;
      ShellSetFlag(&data, SHFLG_HeaderSet);
     }else if( cli_strcmp(z,"-noheader")==0 ){
      data.showHeader = 0;
      ShellSetFlag(&data, SHFLG_HeaderSet);
    }else if( cli_strcmp(z,"-echo")==0 ){
      ShellSetFlag(&data, SHFLG_Echo);
    }else if( cli_strcmp(z,"-eqp")==0 ){
      data.autoEQP = AUTOEQP_on;
    }else if( cli_strcmp(z,"-eqpfull")==0 ){
      data.autoEQP = AUTOEQP_full;
    }else if( cli_strcmp(z,"-stats")==0 ){
      data.statsOn = 1;
    }else if( cli_strcmp(z,"-scanstats")==0 ){
      data.scanstatsOn = 1;
    }else if( cli_strcmp(z,"-backslash")==0 ){
      /* Undocumented command-line option: -backslash
      ** Causes C-style backslash escapes to be evaluated in SQL statements
      ** prior to sending the SQL into SQLite.  Useful for injecting
      ** crazy bytes in the middle of SQL statements for testing and debugging.
      */
      ShellSetFlag(&data, SHFLG_Backslash);
    }else if( cli_strcmp(z,"-bail")==0 ){
      /* No-op.  The bail_on_error flag should already be set. */
    }else if( cli_strcmp(z,"-version")==0 ){
      sqlite3_fprintf(stdout, "%s %s (%d-bit)\n",
            sqlite3_libversion(), sqlite3_sourceid(), 8*(int)sizeof(char*));
      return 0;
    }else if( cli_strcmp(z,"-interactive")==0 ){
      /* Need to check for interactive override here to so that it can
      ** affect console setup (for Windows only) and testing thereof.
      */
      stdin_is_interactive = 1;
    }else if( cli_strcmp(z,"-batch")==0 ){
      /* already handled */
    }else if( cli_strcmp(z,"-utf8")==0 ){
      /* already handled */
    }else if( cli_strcmp(z,"-no-utf8")==0 ){
      /* already handled */
    }else if( cli_strcmp(z,"-no-rowid-in-view")==0 ){
      /* already handled */
    }else if( cli_strcmp(z,"-heap")==0 ){
      i++;
    }else if( cli_strcmp(z,"-pagecache")==0 ){
      i+=2;
    }else if( cli_strcmp(z,"-lookaside")==0 ){
      i+=2;
    }else if( cli_strcmp(z,"-threadsafe")==0 ){
      i+=2;
    }else if( cli_strcmp(z,"-nonce")==0 ){
      i += 2;
    }else if( cli_strcmp(z,"-mmap")==0 ){
      i++;
    }else if( cli_strcmp(z,"-memtrace")==0 ){
      i++;
    }else if( cli_strcmp(z,"-pcachetrace")==0 ){
      i++;
#ifdef SQLITE_ENABLE_SORTER_REFERENCES
    }else if( cli_strcmp(z,"-sorterref")==0 ){
      i++;
#endif
    }else if( cli_strcmp(z,"-vfs")==0 ){
      i++;
    }else if( cli_strcmp(z,"-vfstrace")==0 ){
      i++;
#ifdef SQLITE_ENABLE_MULTIPLEX
    }else if( cli_strcmp(z,"-multiplex")==0 ){
      i++;
#endif
    }else if( cli_strcmp(z,"-help")==0 ){
      usage(1);
    }else if( cli_strcmp(z,"-cmd")==0 ){
      /* Run commands that follow -cmd first and separately from commands
      ** that simply appear on the command-line.  This seems goofy.  It would
      ** be better if all commands ran in the order that they appear.  But
      ** we retain the goofy behavior for historical compatibility. */
      if( i==argc-1 ) break;
      z = cmdline_option_value(argc,argv,++i);
      if( z[0]=='.' ){
        rc = do_meta_command(z, &data);
        if( rc && bail_on_error ) return rc==2 ? 0 : rc;
      }else{
        open_db(&data, 0);
        rc = shell_exec(&data, z, &zErrMsg);
        if( zErrMsg!=0 ){
          shellEmitError(zErrMsg);
          if( bail_on_error ) return rc!=0 ? rc : 1;
        }else if( rc!=0 ){
          sqlite3_fprintf(stderr,"Error: unable to process SQL \"%s\"\n", z);
          if( bail_on_error ) return rc;
        }
      }
#if !defined(SQLITE_OMIT_VIRTUALTABLE) && defined(SQLITE_HAVE_ZLIB)
    }else if( cli_strncmp(z, "-A", 2)==0 ){
      if( nCmd>0 ){
        sqlite3_fprintf(stderr,"Error: cannot mix regular SQL or dot-commands"
              " with \"%s\"\n", z);
        return 1;
      }
      open_db(&data, OPEN_DB_ZIPFILE);
      if( z[2] ){
        argv[i] = &z[2];
        arDotCommand(&data, 1, argv+(i-1), argc-(i-1));
      }else{
        arDotCommand(&data, 1, argv+i, argc-i);
      }
      readStdin = 0;
      break;
#endif
    }else if( cli_strcmp(z,"-safe")==0 ){
      data.bSafeMode = data.bSafeModePersist = 1;
    }else if( cli_strcmp(z,"-unsafe-testing")==0 ){
      /* Acted upon in first pass. */
    }else{
      sqlite3_fprintf(stderr,"%s: Error: unknown option: %s\n", Argv0, z);
      eputz("Use -help for a list of options.\n");
      return 1;
    }
    data.cMode = data.mode;
  }

  if( !readStdin ){
    /* Run all arguments that do not begin with '-' as if they were separate
    ** command-line inputs, except for the argToSkip argument which contains
    ** the database filename.
    */
    for(i=0; i<nCmd; i++){
      if( azCmd[i][0]=='.' ){
        rc = do_meta_command(azCmd[i], &data);
        if( rc ){
          if( rc==2 ) rc = 0;
          goto shell_main_exit;
        }
      }else{
        open_db(&data, 0);
        echo_group_input(&data, azCmd[i]);
        rc = shell_exec(&data, azCmd[i], &zErrMsg);
        if( zErrMsg || rc ){
          if( zErrMsg!=0 ){
            shellEmitError(zErrMsg);
          }else{
            sqlite3_fprintf(stderr,
                            "Error: unable to process SQL: %s\n", azCmd[i]);
          }
          sqlite3_free(zErrMsg);
          if( rc==0 ) rc = 1;
          goto shell_main_exit;
        }
      }
    }
  }else{
    /* Run commands received from standard input
    */
    if( stdin_is_interactive ){
      char *zHome;
      char *zHistory;
      int nHistory;
#if CIO_WIN_WC_XLATE
# define SHELL_CIO_CHAR_SET (stdout_is_console? " (UTF-16 console I/O)" : "")
#else
# define SHELL_CIO_CHAR_SET ""
#endif
      extern char* sqlite3mc_version();
      sqlite3_fprintf(stdout,
            "SQLite version %s %.19s%s" /*extra-version-info*/
            " (%s)\n" /*SQLite3-Multiple-Ciphers-version-info*/
            "Enter \".help\" for usage hints.\n",
            sqlite3_libversion(), sqlite3_sourceid(), SHELL_CIO_CHAR_SET, sqlite3mc_version());
      if( warnInmemoryDb ){
        sputz(stdout, "Connected to a ");
        printBold("transient in-memory database");
        sputz(stdout, ".\nUse \".open FILENAME\" to reopen on a"
              " persistent database.\n");
      }
      zHistory = getenv("SQLITE_HISTORY");
      if( zHistory ){
        zHistory = strdup(zHistory);
      }else if( (zHome = find_home_dir(0))!=0 ){
        nHistory = strlen30(zHome) + 20;
        if( (zHistory = malloc(nHistory))!=0 ){
          sqlite3_snprintf(nHistory, zHistory,"%s/.sqlite_history", zHome);
        }
      }
      if( zHistory ){ shell_read_history(zHistory); }
#if HAVE_READLINE || HAVE_EDITLINE
      rl_attempted_completion_function = readline_completion;
#elif HAVE_LINENOISE
      linenoiseSetCompletionCallback(linenoise_completion, NULL);
#endif
      data.in = 0;
      rc = process_input(&data);
      if( zHistory ){
        shell_stifle_history(2000);
        shell_write_history(zHistory);
        free(zHistory);
      }
    }else{
      data.in = stdin;
      rc = process_input(&data);
    }
  }
#ifndef SQLITE_SHELL_FIDDLE
  /* In WASM mode we have to leave the db state in place so that
  ** client code can "push" SQL into it after this call returns. */
#ifndef SQLITE_OMIT_VIRTUALTABLE
  if( data.expert.pExpert ){
    expertFinish(&data, 1, 0);
  }
#endif
 shell_main_exit:
  free(azCmd);
  set_table_name(&data, 0);
  if( data.db ){
    session_close_all(&data, -1);
    close_db(data.db);
  }
  for(i=0; i<ArraySize(data.aAuxDb); i++){
    sqlite3_free(data.aAuxDb[i].zFreeOnClose);
    if( data.aAuxDb[i].db ){
      session_close_all(&data, i);
      close_db(data.aAuxDb[i].db);
    }
  }
  find_home_dir(1);
  output_reset(&data);
  data.doXdgOpen = 0;
  clearTempFile(&data);
#if !SQLITE_SHELL_IS_UTF8
  for(i=0; i<argcToFree; i++) free(argvToFree[i]);
  free(argvToFree);
#endif
  free(data.colWidth);
  free(data.zNonce);
  /* Clear the global data structure so that valgrind will detect memory
  ** leaks */
  memset(&data, 0, sizeof(data));
  if( bEnableVfstrace ){
    vfstrace_unregister("trace");
  }
#ifdef SQLITE_DEBUG
  if( sqlite3_memory_used()>mem_main_enter ){
    sqlite3_fprintf(stderr,"Memory leaked: %u bytes\n",
          (unsigned int)(sqlite3_memory_used()-mem_main_enter));
  }
#endif
#else /* SQLITE_SHELL_FIDDLE... */
  shell_main_exit:
#endif
  return rc;
}
