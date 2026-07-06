#if SQLITE_SHELL_IS_UTF8
int SQLITE_CDECL main(int argc, char **argv){
#else
int SQLITE_CDECL wmain(int argc, wchar_t **wargv){
  char **argv;
#endif
}
