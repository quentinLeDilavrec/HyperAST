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
#ifdef SQLITE_SHELL_FIDDLE
#else /* SQLITE_SHELL_FIDDLE... */
  shell_main_exit:
#endif
  return rc;
}
