//////////////////////////////////////////////////
//  TSQLite                                     //
//    Delphi SQLite3 wrapper                    //
//                                              //
//  https://github.com/stijnsanders/TSQLite     //
//////////////////////////////////////////////////

unit SQLite;

//based on sqlite.h 3.11.0 2016-01-20

interface

uses SysUtils;

type
  //object placeholders (=handles)
  HSQLiteDB=LongWord;//pointer? ^void?
  HSQLiteStatement=LongWord;
  HSQLiteValue=LongWord;
  HSQLiteContext=LongWord;
  HSQLiteBlob=LongWord;
  HSQLiteMutex=LongWord;
  HSQLiteBackup=LongWord;

type
  TSQLiteCallback=function(Context:pointer;N:integer;var Text:PAnsiChar;var Names:PAnsiChar):integer; cdecl;
  TSQLiteBusyHandler=function(Context:pointer;N:integer):integer; cdecl;
  TSQLiteAuthorizer=function(UserData:pointer;Action:integer;X1,X2,X3,X4:PAnsiChar):integer; cdecl;
  TSQLiteProcessHandler=function(Context:pointer):integer; cdecl;
  TSQLiteDestructor=procedure(Data:pointer); cdecl;
  TSQLiteFunctionHandler=procedure(Context:HSQLiteContext;Index:integer;Value:HSQLiteValue); cdecl;
  TSQLiteFunctionFinal=procedure(Context:HSQLiteContext); cdecl;
  TSQLiteCollationCompare=function(pArg:pointer;N:integer;X:PAnsiChar;Y:PAnsiChar):integer; cdecl;
  TSQLiteCollationCompare16=function(pArg:pointer;N:integer;X:PWideChar;Y:PWideChar):integer; cdecl;
  TSQLiteCollationNeeded=procedure(Context:pointer;SQLiteDB:HSQLiteDB;eTextRep:integer;X:PAnsiChar); cdecl;
  TSQLiteCollationNeeded16=procedure(Context:pointer;SQLiteDB:HSQLiteDB;eTextRep:integer;X:PWideChar); cdecl;
  TSQLiteHook=function(Context:pointer):integer; cdecl;
  TSQLiteUpdateHook=procedure(Context:pointer;N:integer;X:PAnsiChar;Y:PAnsiChar;Z:int64); cdecl;
  TSQLiteUnlockNotify=procedure(var apArg:pointer;nArg:integer); cdecl;
  TSQLiteWriteAheadLogHook=function(Context:pointer;SQLiteDB:HSQLiteDB;X:PAnsiChar;N:integer):integer; cdecl;

const
  SQLITE_OK          =  0 ;  // Successful result 
  SQLITE_ERROR       =  1 ;  // SQL error or missing database
  SQLITE_INTERNAL    =  2 ;  // Internal logic error in SQLite
  SQLITE_PERM        =  3 ;  // Access permission denied
  SQLITE_ABORT       =  4 ;  // Callback routine requested an abort
  SQLITE_BUSY        =  5 ;  // The database file is locked
  SQLITE_LOCKED      =  6 ;  // A table in the database is locked
  SQLITE_NOMEM       =  7 ;  // A malloc() failed
  SQLITE_READONLY    =  8 ;  // Attempt to write a readonly database
  SQLITE_INTERRUPT   =  9 ;  // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR       = 10 ;  // Some kind of disk I/O error occurred
  SQLITE_CORRUPT     = 11 ;  // The database disk image is malformed
  SQLITE_NOTFOUND    = 12 ;  // NOT USED. Table or record not found
  SQLITE_FULL        = 13 ;  // Insertion failed because database is full
  SQLITE_CANTOPEN    = 14 ;  // Unable to open the database file
  SQLITE_PROTOCOL    = 15 ;  // Database lock protocol error
  SQLITE_EMPTY       = 16 ;  // Database is empty
  SQLITE_SCHEMA      = 17 ;  // The database schema changed
  SQLITE_TOOBIG      = 18 ;  // String or BLOB exceeds size limit
  SQLITE_CONSTRAINT  = 19 ;  // Abort due to constraint violation
  SQLITE_MISMATCH    = 20 ;  // Data type mismatch
  SQLITE_MISUSE      = 21 ;  // Library used incorrectly
  SQLITE_NOLFS       = 22 ;  // Uses OS features not supported on host
  SQLITE_AUTH        = 23 ;  // Authorization denied
  SQLITE_FORMAT      = 24 ;  // Auxiliary database format error
  SQLITE_RANGE       = 25 ;  // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB      = 26 ;  // File opened that is not a database file 
  SQLITE_NOTICE      = 27 ;  // Notifications from sqlite3_log()
  SQLITE_WARNING     = 28 ;  // Warnings from sqlite3_log()
  SQLITE_ROW         = 100;  // sqlite3_step() has another row ready
  SQLITE_DONE        = 101;  // sqlite3_step() has finished executing 

  SQLITE_IOERR_READ              = SQLITE_IOERR or $0100;
  SQLITE_IOERR_SHORT_READ        = SQLITE_IOERR or $0200;
  SQLITE_IOERR_WRITE             = SQLITE_IOERR or $0300;
  SQLITE_IOERR_FSYNC             = SQLITE_IOERR or $0400;
  SQLITE_IOERR_DIR_FSYNC         = SQLITE_IOERR or $0500;
  SQLITE_IOERR_TRUNCATE          = SQLITE_IOERR or $0600;
  SQLITE_IOERR_FSTAT             = SQLITE_IOERR or $0700;
  SQLITE_IOERR_UNLOCK            = SQLITE_IOERR or $0800;
  SQLITE_IOERR_RDLOCK            = SQLITE_IOERR or $0900;
  SQLITE_IOERR_DELETE            = SQLITE_IOERR or $1A00;
  SQLITE_IOERR_BLOCKED           = SQLITE_IOERR or $1B00;
  SQLITE_IOERR_NOMEM             = SQLITE_IOERR or $1C00;
  SQLITE_IOERR_ACCESS            = SQLITE_IOERR or $1D00;
  SQLITE_IOERR_CHECKRESERVEDLOCK = SQLITE_IOERR or $1E00;
  SQLITE_IOERR_LOCK              = SQLITE_IOERR or $1F00;
  SQLITE_IOERR_CLOSE             = SQLITE_IOERR or $1000;
  SQLITE_IOERR_DIR_CLOSE         = SQLITE_IOERR or $1100;
  SQLITE_IOERR_SHMOPEN           = SQLITE_IOERR or $1200;
  SQLITE_IOERR_SHMSIZE           = SQLITE_IOERR or $1300;
  SQLITE_IOERR_SHMLOCK           = SQLITE_IOERR or $1400;
  SQLITE_IOERR_SHMMAP            = SQLITE_IOERR or $1500;
  SQLITE_IOERR_SEEK              = SQLITE_IOERR or $1600;
  SQLITE_IOERR_DELETE_NOENT      = SQLITE_IOERR or $1700;
  SQLITE_IOERR_MMAP              = SQLITE_IOERR or $1800;
  SQLITE_IOERR_GETTEMPPATH       = SQLITE_IOERR or $1900;
  SQLITE_IOERR_CONVPATH          = SQLITE_IOERR or $1A00;
  SQLITE_IOERR_VNODE             = SQLITE_IOERR or $1B00;
  SQLITE_IOERR_AUTH              = SQLITE_IOERR or $1C00;
  SQLITE_LOCKED_SHAREDCACHE      = SQLITE_LOCKED or $0100;
  SQLITE_BUSY_RECOVERY           = SQLITE_BUSY or $0100;
  SQLITE_BUSY_SNAPSHOT           = SQLITE_BUSY or $0200;
  SQLITE_CANTOPEN_NOTEMPDIR      = SQLITE_CANTOPEN or $0100;
  SQLITE_CANTOPEN_ISDIR          = SQLITE_CANTOPEN or $0200;
  SQLITE_CANTOPEN_FULLPATH       = SQLITE_CANTOPEN or $0300;
  SQLITE_CANTOPEN_CONVPATH       = SQLITE_CANTOPEN or $0400;
  SQLITE_CORRUPT_VTAB            = SQLITE_CORRUPT or $0100;
  SQLITE_READONLY_RECOVERY       = SQLITE_READONLY or $0100;
  SQLITE_READONLY_CANTLOCK       = SQLITE_READONLY or $0200;
  SQLITE_READONLY_ROLLBACK       = SQLITE_READONLY or $0300;
  SQLITE_READONLY_DBMOVED        = SQLITE_READONLY or $0400;
  SQLITE_ABORT_ROLLBACK          = SQLITE_ABORT or $0200;
  SQLITE_CONSTRAINT_CHECK        = SQLITE_CONSTRAINT or $0100;
  SQLITE_CONSTRAINT_COMMITHOOK   = SQLITE_CONSTRAINT or $0200;
  SQLITE_CONSTRAINT_FOREIGNKEY   = SQLITE_CONSTRAINT or $0300;
  SQLITE_CONSTRAINT_FUNCTION     = SQLITE_CONSTRAINT or $0400;
  SQLITE_CONSTRAINT_NOTNULL      = SQLITE_CONSTRAINT or $0500;
  SQLITE_CONSTRAINT_PRIMARYKEY   = SQLITE_CONSTRAINT or $0600;
  SQLITE_CONSTRAINT_TRIGGER      = SQLITE_CONSTRAINT or $0700;
  SQLITE_CONSTRAINT_UNIQUE       = SQLITE_CONSTRAINT or $0800;
  SQLITE_CONSTRAINT_VTAB         = SQLITE_CONSTRAINT or $0900;
  SQLITE_CONSTRAINT_ROWID        = SQLITE_CONSTRAINT or $0A00;
  SQLITE_NOTICE_RECOVER_WAL      = SQLITE_NOTICE or $0100;
  SQLITE_NOTICE_RECOVER_ROLLBACK = SQLITE_NOTICE or $0200;
  SQLITE_WARNING_AUTOINDEX       = SQLITE_WARNING or $0100;
  SQLITE_AUTH_USER               = SQLITE_AUTH or $0100;

  SQLITE_OPEN_READONLY         = $00000001;
  SQLITE_OPEN_READWRITE        = $00000002;
  SQLITE_OPEN_CREATE           = $00000004;
  SQLITE_OPEN_URI              = $00000040;
  SQLITE_OPEN_MEMORY           = $00000080;
  SQLITE_OPEN_NOMUTEX          = $00008000;
  SQLITE_OPEN_FULLMUTEX        = $00010000;
  SQLITE_OPEN_SHAREDCACHE      = $00020000;
  SQLITE_OPEN_PRIVATECACHE     = $00040000;

  SQLITE_IOCAP_ATOMIC                 = $00000001;
  SQLITE_IOCAP_ATOMIC512              = $00000002;
  SQLITE_IOCAP_ATOMIC1K               = $00000004;
  SQLITE_IOCAP_ATOMIC2K               = $00000008;
  SQLITE_IOCAP_ATOMIC4K               = $00000010;
  SQLITE_IOCAP_ATOMIC8K               = $00000020;
  SQLITE_IOCAP_ATOMIC16K              = $00000040;
  SQLITE_IOCAP_ATOMIC32K              = $00000080;
  SQLITE_IOCAP_ATOMIC64K              = $00000100;
  SQLITE_IOCAP_SAFE_APPEND            = $00000200;
  SQLITE_IOCAP_SEQUENTIAL             = $00000400;
  SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN  = $00000800;  SQLITE_IOCAP_POWERSAFE_OVERWRITE    = $00001000;  SQLITE_IOCAP_IMMUTABLE              = $00002000;

  SQLITE_LOCK_NONE         = 0;
  SQLITE_LOCK_SHARED       = 1;
  SQLITE_LOCK_RESERVED     = 2;
  SQLITE_LOCK_PENDING      = 3;
  SQLITE_LOCK_EXCLUSIVE    = 4;

  SQLITE_SYNC_NORMAL        = $00002;
  SQLITE_SYNC_FULL          = $00003;
  SQLITE_SYNC_DATAONLY      = $00010;

  SQLITE_CONFIG_SINGLETHREAD =  1;
  SQLITE_CONFIG_MULTITHREAD  =  2;
  SQLITE_CONFIG_SERIALIZED   =  3;
  SQLITE_CONFIG_MALLOC       =  4;
  SQLITE_CONFIG_GETMALLOC    =  5;
  SQLITE_CONFIG_SCRATCH      =  6;
  SQLITE_CONFIG_PAGECACHE    =  7;
  SQLITE_CONFIG_HEAP         =  8;
  SQLITE_CONFIG_MEMSTATUS    =  9;
  SQLITE_CONFIG_MUTEX        = 10;
  SQLITE_CONFIG_GETMUTEX     = 11;
  SQLITE_CONFIG_LOOKASIDE    = 13;
  SQLITE_CONFIG_PCACHE       = 14;
  SQLITE_CONFIG_GETPCACHE    = 15;
  SQLITE_CONFIG_LOG          = 16;
  SQLITE_CONFIG_URI          = 17;
  SQLITE_CONFIG_PCACHE2      = 18;
  SQLITE_CONFIG_GETPCACHE2   = 19;
  SQLITE_CONFIG_COVERING_INDEX_SCAN = 20;
  SQLITE_CONFIG_SQLLOG       = 21;
  SQLITE_CONFIG_MMAP_SIZE    = 22;
  SQLITE_CONFIG_WIN32_HEAPSIZE = 23;
  SQLITE_CONFIG_PCACHE_HDRSZ   = 24;
  SQLITE_CONFIG_PMASZ          = 25;

  SQLITE_DBCONFIG_LOOKASIDE       = 1001;
  SQLITE_DBCONFIG_ENABLE_FKEY     = 1002;
  SQLITE_DBCONFIG_ENABLE_TRIGGER  = 1003;

  SQLITE_DENY   = 1;
  SQLITE_IGNORE = 2;

  SQLITE_CREATE_INDEX        =  1;
  SQLITE_CREATE_TABLE        =  2;
  SQLITE_CREATE_TEMP_INDEX   =  3;
  SQLITE_CREATE_TEMP_TABLE   =  4;
  SQLITE_CREATE_TEMP_TRIGGER =  5;
  SQLITE_CREATE_TEMP_VIEW    =  6;
  SQLITE_CREATE_TRIGGER      =  7;
  SQLITE_CREATE_VIEW         =  8;
  SQLITE_DELETE              =  9;
  SQLITE_DROP_INDEX          = 10;
  SQLITE_DROP_TABLE          = 11;
  SQLITE_DROP_TEMP_INDEX     = 12;
  SQLITE_DROP_TEMP_TABLE     = 13;
  SQLITE_DROP_TEMP_TRIGGER   = 14;
  SQLITE_DROP_TEMP_VIEW      = 15;
  SQLITE_DROP_TRIGGER        = 16;
  SQLITE_DROP_VIEW           = 17;
  SQLITE_INSERT              = 18;
  SQLITE_PRAGMA              = 19;
  SQLITE_READ                = 20;
  SQLITE_SELECT              = 21;
  SQLITE_TRANSACTION         = 22;
  SQLITE_UPDATE              = 23;
  SQLITE_ATTACH              = 24;
  SQLITE_DETACH              = 25;
  SQLITE_ALTER_TABLE         = 26;
  SQLITE_REINDEX             = 27;
  SQLITE_ANALYZE             = 28;
  SQLITE_CREATE_VTABLE       = 29;
  SQLITE_DROP_VTABLE         = 30;
  SQLITE_FUNCTION            = 31;
  SQLITE_SAVEPOINT           = 32;
  SQLITE_COPY                =  0;  
  SQLITE_RECURSIVE           = 33;
  SQLITE_LIMIT_LENGTH                  =  0;
  SQLITE_LIMIT_SQL_LENGTH              =  1;
  SQLITE_LIMIT_COLUMN                  =  2;
  SQLITE_LIMIT_EXPR_DEPTH              =  3;
  SQLITE_LIMIT_COMPOUND_SELECT         =  4;
  SQLITE_LIMIT_VDBE_OP                 =  5;
  SQLITE_LIMIT_FUNCTION_ARG            =  6;
  SQLITE_LIMIT_ATTACHED                =  7;
  SQLITE_LIMIT_LIKE_PATTERN_LENGTH     =  8;
  SQLITE_LIMIT_VARIABLE_NUMBER         =  9;
  SQLITE_LIMIT_TRIGGER_DEPTH           = 10;
  SQLITE_LIMIT_WORKER_THREADS          = 11;

  SQLITE_INTEGER  = 1;
  SQLITE_FLOAT    = 2;
  SQLITE_TEXT     = 3;
  SQLITE_BLOB     = 4;
  SQLITE_NULL     = 5;

  SQLITE_UTF8           = 1;
  SQLITE_UTF16LE        = 2;
  SQLITE_UTF16BE        = 3;
  SQLITE_UTF16          = 4;    // Use native byte order
  SQLITE_ANY            = 5;    // sqlite3_create_function only
  SQLITE_UTF16_ALIGNED  = 8;    // sqlite3_create_collation only

  SQLITE_DETERMINISTIC    = $800;

  SQLITE_MUTEX_FAST            = 0;
  SQLITE_MUTEX_RECURSIVE       = 1;
  SQLITE_MUTEX_STATIC_MASTER   = 2;
  SQLITE_MUTEX_STATIC_MEM      = 3;  // sqlite3_malloc()
  SQLITE_MUTEX_STATIC_OPEN     = 4;  // sqlite3BtreeOpen()
  SQLITE_MUTEX_STATIC_PRNG     = 5;  // sqlite3_random()
  SQLITE_MUTEX_STATIC_LRU      = 6;  // lru page list
  SQLITE_MUTEX_STATIC_LRU2     = 7;
  SQLITE_MUTEX_STATIC_PMEM     = 7;  // sqlite3PageMalloc()
  SQLITE_MUTEX_STATIC_APP1     = 8;  // For use by application
  SQLITE_MUTEX_STATIC_APP2     = 9;  // For use by application
  SQLITE_MUTEX_STATIC_APP3     = 10;  // For use by application
  SQLITE_MUTEX_STATIC_VFS1     = 11;  // For use by built-in VFS
  SQLITE_MUTEX_STATIC_VFS2     = 12;  // For use by extension VFS
  SQLITE_MUTEX_STATIC_VFS3     = 13;  // For use by application VFS

  SQLITE_STATUS_MEMORY_USED         = 0;
  SQLITE_STATUS_PAGECACHE_USED      = 1;
  SQLITE_STATUS_PAGECACHE_OVERFLOW  = 2;
  SQLITE_STATUS_SCRATCH_USED        = 3;
  SQLITE_STATUS_SCRATCH_OVERFLOW    = 4;
  SQLITE_STATUS_MALLOC_SIZE         = 5;
  SQLITE_STATUS_PARSER_STACK        = 6;
  SQLITE_STATUS_PAGECACHE_SIZE      = 7;
  SQLITE_STATUS_SCRATCH_SIZE        = 8;
  SQLITE_STATUS_MALLOC_COUNT        = 9;

  SQLITE_DBSTATUS_LOOKASIDE_USED       = 0;
  SQLITE_DBSTATUS_CACHE_USED           = 1;
  SQLITE_DBSTATUS_SCHEMA_USED          = 2;
  SQLITE_DBSTATUS_STMT_USED            = 3;
  SQLITE_DBSTATUS_LOOKASIDE_HIT        = 4;
  SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE  = 5;
  SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL  = 6;
  SQLITE_DBSTATUS_CACHE_HIT            = 7;
  SQLITE_DBSTATUS_CACHE_MISS           = 8;
  SQLITE_DBSTATUS_CACHE_WRITE          = 9;
  SQLITE_DBSTATUS_DEFERRED_FKS         = 10;
  SQLITE_DBSTATUS_MAX                  = 10;  // Largest defined DBSTATUS

  SQLITE_STMTSTATUS_FULLSCAN_STEP    = 1;
  SQLITE_STMTSTATUS_SORT             = 2;
  SQLITE_STMTSTATUS_AUTOINDEX        = 3;  SQLITE_STMTSTATUS_VM_STEP          = 4;

  SQLITE_CHECKPOINT_PASSIVE  = 0;  SQLITE_CHECKPOINT_FULL     = 1;
  SQLITE_CHECKPOINT_RESTART  = 2;
  SQLITE_CHECKPOINT_TRUNCATE = 3;
  
type  ESQLiteException=class(Exception)
  private
    FErrorCode:integer;
  public
    constructor Create(ErrorCode:integer);
    constructor CreateDB(SQLiteDB:HSQLiteDB;ErrorCode:integer);
    property ErrorCode:integer read FErrorCode;
  end;

//ATTENTION: PAnsiChar's should point to UTF-8 strings

//ATTENTION: if you need to pass a NULL pointer to a 'var PAnsiChar' parameter, use 'PAnsiChar(nil^)'

procedure sqlite3_check(Res:integer); overload;
procedure sqlite3_check(SQLiteDB:HSQLiteDB;Res:integer); overload;

function sqlite3_libversion:PAnsiChar; cdecl;
function sqlite3_sourceid:PAnsiChar; cdecl;
function sqlite3_libversion_number:integer; cdecl;
//function sqlite3_compileoption_used(PAnsiChar:zOptName):integer; cdecl;
//function sqlite3_compileoption_get(N:integer):PAnsiChar; cdecl;
function sqlite3_threadsafe:integer; cdecl;
function sqlite3_close(SQLiteDB:HSQLiteDB):integer; cdecl;
function sqlite3_close_v2(SQLiteDB:HSQLiteDB):integer; cdecl;

function sqlite3_exec(SQLiteDB:HSQLiteDB;Sql:PAnsiChar;Callback:TSQLiteCallback;Context:pointer;
  var ErrorMessage:PAnsiChar):integer; cdecl;

function sqlite3_initialize:integer; cdecl;
function sqlite3_shutdown:integer; cdecl;
function sqlite3_os_init:integer; cdecl;
function sqlite3_os_end:integer; cdecl;

function sqlite3_config(op:integer):integer; cdecl; varargs;
function sqlite3_db_config(SQLiteDB:HSQLiteDB;op:integer):integer; cdecl; varargs;
function sqlite3_extended_result_codes(SQLiteDB:HSQLiteDB;onoff:integer):integer; cdecl;
function sqlite3_last_insert_rowid(SQLiteDB:HSQLiteDB):int64; cdecl;
function sqlite3_changes(SQLiteDB:HSQLiteDB):integer; cdecl;
function sqlite3_total_changes(SQLiteDB:HSQLiteDB):integer; cdecl;
procedure sqlite3_interrupt(SQLiteDB:HSQLiteDB); cdecl;
function sqlite3_complete(sql:PAnsiChar):integer; cdecl;
function sqlite3_complete16(sql:PWideChar):integer; cdecl;
function sqlite3_busy_handler(SQLiteDB:HSQLiteDB;Handler:TSQLiteBusyHandler;Context:pointer):integer; cdecl;
function sqlite3_busy_timeout(SQLiteDB:HSQLiteDB;ms:integer):integer; cdecl;

function sqlite3_get_table(SQLiteDB:HSQLiteDB;Sql:PAnsiChar;
  var Results:PAnsiChar;var Rows:integer;var Columns:integer;var ErrorMessage:PAnsiChar):integer; cdecl;
function sqlite3_free_table(Results:PAnsiChar):integer; cdecl;

//function sqlite3_mprintf(:PAnsiChar;...):PAnsiChar; cdecl;
//function sqlite3_vmprintf(:PAnsiChar;va_list):integer; cdecl;
//function sqlite3_snprintf(:integer;:PAnsiChar;:PAnsiChar;...):integer; cdecl;

function sqlite3_malloc(Size:integer):pointer; cdecl;
function sqlite3_malloc64(Size:uint64):pointer; cdecl;
function sqlite3_realloc(Mem:pointer;Size:integer):pointer; cdecl;
function sqlite3_realloc64(Mem:pointer;Size:uint64):pointer; cdecl;
procedure sqlite3_free(Mem:pointer); cdecl;
function sqlite3_msize(Mem:pointer):uint64; cdecl;
function sqlite3_memory_used:int64; cdecl;
function sqlite3_memory_highwater(resetFlag:longbool):int64; cdecl;
procedure sqlite3_randomness(N:integer;var P); cdecl;
function sqlite3_set_authorizer(SQLiteDB:HSQLiteDB;Auth:TSQLiteAuthorizer;UserData:pointer):integer; cdecl;
//sqlite3_trace
//sqlite3_profile
procedure sqlite3_progress_handler(SQLiteDB:HSQLiteDB;N:integer;Callback:TSQLiteProcessHandler;Context:pointer); cdecl;
function sqlite3_open(FileName:PAnsiChar;var SQLiteDB:HSQLiteDB):integer; cdecl;
function sqlite3_open16(FileName:PWideChar;var SQLiteDB:HSQLiteDB):integer; cdecl;
function sqlite3_open_v2(FileName:PAnsiChar;var SQLiteDB:HSQLiteDB;Flags:integer;VFSModule:PAnsiChar):integer; cdecl;
function sqlite3_uri_parameter(FileName,Param:PAnsiChar):PAnsiChar; cdecl;
function sqlite3_uri_boolean(FileName,Param:PAnsiChar;Default:integer):integer; cdecl;
function sqlite3_uri_int64(FileName,Param:PAnsiChar;Default:int64):int64 cdecl;
function sqlite3_errcode(SQLiteDB:HSQLiteDB):integer; cdecl;
function sqlite3_extended_errcode(SQLiteDB:HSQLiteDB):integer; cdecl;
function sqlite3_errmsg(SQLiteDB:HSQLiteDB):PAnsiChar; cdecl;
function sqlite3_errmsg16(SQLiteDB:HSQLiteDB):PWideChar; cdecl;
function sqlite3_errstr(ResultCode:integer):PAnsiChar; cdecl;
function sqlite3_limit(SQLiteDB:HSQLiteDB;id:integer;newVal:integer):integer; cdecl;

function sqlite3_prepare(SQLiteDB:HSQLiteDB;Sql:PAnsiChar;nByte:integer;
  var Statement:HSQLiteStatement;var Tail:PAnsiChar):integer; cdecl;
function sqlite3_prepare_v2(SQLiteDB:HSQLiteDB;Sql:PAnsiChar;nByte:integer;
  var Statement:HSQLiteStatement;var Tail:PAnsiChar):integer; cdecl;
function sqlite3_prepare16(SQLiteDB:HSQLiteDB;Sql:PWideChar;nByte:integer;
  var Statement:HSQLiteStatement;var Tail:PWideChar):integer; cdecl;
function sqlite3_prepare16_v2(SQLiteDB:HSQLiteDB;Sql:PWideChar;nByte:integer;
  var Statement:HSQLiteStatement;var Tail:PWideChar):integer; cdecl;
function sqlite3_sql(Statement:HSQLiteStatement):PAnsiChar; cdecl;
function sqlite3_stmt_readonly(Statement:HSQLiteStatement):integer; cdecl;
function sqlite3_stmt_busy(Statement:HSQLiteStatement):integer; cdecl;

function sqlite3_bind_blob(Statement:HSQLiteStatement;Index:integer;var X;N:integer;Z:TSQLiteDestructor):integer; cdecl;
function sqlite3_bind_blob64(Statement:HSQLiteStatement;Index:integer;var X;N:int64;Z:TSQLiteDestructor):integer; cdecl;
function sqlite3_bind_double(Statement:HSQLiteStatement;Index:integer;X:Double):integer; cdecl;
function sqlite3_bind_int(Statement:HSQLiteStatement;Index:integer;X:integer):integer; cdecl;
function sqlite3_bind_int64(Statement:HSQLiteStatement;Index:integer;X:uint64):integer; cdecl;
function sqlite3_bind_null(Statement:HSQLiteStatement;Index:integer):integer; cdecl;
function sqlite3_bind_text(Statement:HSQLiteStatement;Index:integer;
  X:PAnsiChar;N:integer;Z:TSQLiteDestructor):integer; cdecl;
function sqlite3_bind_text16(Statement:HSQLiteStatement;Index:integer;
  X:PWideChar;N:integer;Z:TSQLiteDestructor):integer; cdecl;
function sqlite3_bind_text64(Statement:HSQLiteStatement;Index:integer;
  X:PWideChar;N:uint64;Z:TSQLiteDestructor):integer; cdecl;
function sqlite3_bind_value(Statement:HSQLiteStatement;Index:integer;X:HSQLiteValue):integer; cdecl;
function sqlite3_bind_zeroblob(Statement:HSQLiteStatement;Index:integer;N:integer):integer; cdecl;
function sqlite3_bind_zeroblob64(Statement:HSQLiteStatement;Index:integer;N:uint64):integer; cdecl;
function sqlite3_bind_parameter_count(Statement:HSQLiteStatement):integer; cdecl;
function sqlite3_bind_parameter_name(Statement:HSQLiteStatement;Index:integer):PAnsiChar; cdecl;
function sqlite3_bind_parameter_index(Statement:HSQLiteStatement;Name:PAnsiChar):integer; cdecl;
function sqlite3_clear_bindings(Statement:HSQLiteStatement):integer; cdecl;

function sqlite3_column_count(Statement:HSQLiteStatement):integer; cdecl;
function sqlite3_column_name(Statement:HSQLiteStatement;Index:integer):PAnsiChar; cdecl;
function sqlite3_column_name16(Statement:HSQLiteStatement;Index:integer):PWideChar; cdecl;
function sqlite3_column_database_name(Statement:HSQLiteStatement;Index:integer):PAnsiChar; cdecl;
function sqlite3_column_database_name16(Statement:HSQLiteStatement;Index:integer):PWideChar; cdecl;
function sqlite3_column_table_name(Statement:HSQLiteStatement;Index:integer):PAnsiChar; cdecl;
function sqlite3_column_table_name16(Statement:HSQLiteStatement;Index:integer):PWideChar; cdecl;
function sqlite3_column_origin_name(Statement:HSQLiteStatement;Index:integer):PAnsiChar; cdecl;
function sqlite3_column_origin_name16(Statement:HSQLiteStatement;Index:integer):PWideChar; cdecl;
function sqlite3_column_decltype(Statement:HSQLiteStatement;Index:integer):PAnsiChar; cdecl;
function sqlite3_column_decltype16(Statement:HSQLiteStatement;Index:integer):PWideChar; cdecl;

function sqlite3_step(Statement:HSQLiteStatement):integer; cdecl;
function sqlite3_data_count(Statement:HSQLiteStatement):integer; cdecl;

function sqlite3_column_blob(Statement:HSQLiteStatement;Index:integer):pointer; cdecl;
function sqlite3_column_bytes(Statement:HSQLiteStatement;Index:integer):integer; cdecl;
function sqlite3_column_bytes16(Statement:HSQLiteStatement;Index:integer):integer; cdecl;
function sqlite3_column_double(Statement:HSQLiteStatement;Index:integer):Double; cdecl;
function sqlite3_column_int(Statement:HSQLiteStatement;Index:integer):integer; cdecl;
function sqlite3_column_int64(Statement:HSQLiteStatement;Index:integer):int64; cdecl;
function sqlite3_column_text(Statement:HSQLiteStatement;Index:integer):PAnsiChar; cdecl;
function sqlite3_column_text16(Statement:HSQLiteStatement;Index:integer):PWideChar; cdecl;
function sqlite3_column_type(Statement:HSQLiteStatement;Index:integer):integer; cdecl;
function sqlite3_column_value(Statement:HSQLiteStatement;Index:integer):HSQLiteValue; cdecl;

function sqlite3_finalize(Statement:HSQLiteStatement):integer; cdecl;
function sqlite3_reset(Statement:HSQLiteStatement):integer; cdecl;

function sqlite3_create_function(SQLiteDB:HSQLiteDB;FunctionName:PAnsiChar;
  nArg:integer;eTextRep:integer;pApp:pointer;
  xFunc:TSQLiteFunctionHandler;xStep:TSQLiteFunctionHandler;xFinal:TSQLiteFunctionFinal):integer; cdecl;
function sqlite3_create_function16(SQLiteDB:HSQLiteDB;FunctionName:PAnsiChar;
  nArg:integer;eTextRep:integer;pApp:pointer;
  xFunc:TSQLiteFunctionHandler;xStep:TSQLiteFunctionHandler;xFinal:TSQLiteFunctionFinal):integer; cdecl;
function sqlite3_create_function_v2(SQLiteDB:HSQLiteDB;FunctionName:PAnsiChar;
  nArg:integer;eTextRep:integer;pApp:pointer;
  xFunc:TSQLiteFunctionHandler;xStep:TSQLiteFunctionHandler;xFinal:TSQLiteFunctionFinal;
  xDestroy:TSQLiteDestructor):integer; cdecl;

function sqlite3_value_blob(Value:HSQLiteValue):pointer; cdecl;
function sqlite3_value_bytes(Value:HSQLiteValue):integer; cdecl;
function sqlite3_value_bytes16(Value:HSQLiteValue):integer; cdecl;
function sqlite3_value_double(Value:HSQLiteValue):double; cdecl;
function sqlite3_value_int(Value:HSQLiteValue):integer; cdecl;
function sqlite3_value_int64(Value:HSQLiteValue):int64; cdecl;
function sqlite3_value_text(Value:HSQLiteValue):PAnsiChar; cdecl;
function sqlite3_value_text16(Value:HSQLiteValue):PWideChar; cdecl;
function sqlite3_value_text16le(Value:HSQLiteValue):PWideChar; cdecl;
function sqlite3_value_text16be(Value:HSQLiteValue):PWideChar; cdecl;
function sqlite3_value_type(Value:HSQLiteValue):integer; cdecl;
function sqlite3_value_numeric_type(Value:HSQLiteValue):integer; cdecl;

function sqlite3_aggregate_context(Context:HSQLiteContext;nBytes:integer):pointer; cdecl;
function sqlite3_user_data(Context:HSQLiteContext):pointer; cdecl;
function sqlite3_context_db_handle(Context:HSQLiteContext):HSQLiteDB; cdecl;
function sqlite3_get_auxdata(Context:HSQLiteContext;N:integer):pointer; cdecl;
procedure sqlite3_set_auxdata(Context:HSQLiteContext;N:integer;var X;Z:TSQLiteDestructor); cdecl;

procedure sqlite3_result_blob(Context:HSQLiteContext;var X;N:integer;Z:TSQLiteDestructor); cdecl;
procedure sqlite3_result_blob64(Context:HSQLiteContext;var X;N:uint64;Z:TSQLiteDestructor); cdecl;
procedure sqlite3_result_double(Context:HSQLiteContext;X:Double); cdecl;
procedure sqlite3_result_error(Context:HSQLiteContext;X:PAnsiChar;N:integer); cdecl;
procedure sqlite3_result_error16(Context:HSQLiteContext;X:PWideChar;N:integer); cdecl;
procedure sqlite3_result_error_toobig(Context:HSQLiteContext); cdecl;
procedure sqlite3_result_error_nomem(Context:HSQLiteContext); cdecl;
procedure sqlite3_result_error_code(Context:HSQLiteContext;ErrorCode:integer); cdecl;
procedure sqlite3_result_int(Context:HSQLiteContext;X:integer); cdecl;
procedure sqlite3_result_int64(Context:HSQLiteContext;X:int64); cdecl;
procedure sqlite3_result_null(Context:HSQLiteContext); cdecl;
procedure sqlite3_result_text(Context:HSQLiteContext;X:PAnsiChar;N:integer;Z:TSQLiteDestructor); cdecl;
procedure sqlite3_result_text64(Context:HSQLiteContext;X:PWideChar;N:uint64;Z:TSQLiteDestructor); cdecl;
procedure sqlite3_result_text16(Context:HSQLiteContext;X:PWideChar;N:integer;Z:TSQLiteDestructor); cdecl;
procedure sqlite3_result_text16le(Context:HSQLiteContext;X:PWideChar;N:integer;Z:TSQLiteDestructor); cdecl;
procedure sqlite3_result_text16be(Context:HSQLiteContext;X:PWideChar;N:integer;Z:TSQLiteDestructor); cdecl;
procedure sqlite3_result_value(Context:HSQLiteContext;X:HSQLiteValue); cdecl;
procedure sqlite3_result_zeroblob(Context:HSQLiteContext;N:integer); cdecl;
function sqlite3_result_zeroblob64(Context:HSQLiteContext;N:uint64):integer; cdecl;

procedure sqlite3_result_subtype(Context:HSQLiteContext;N:cardinal); cdecl;

function sqlite3_create_collation(SQLiteDB:HSQLiteDB;Name:PAnsiChar;
  eTextRep:integer;pArg:pointer;xCompare:TSQLiteCollationCompare):integer; cdecl;
function sqlite3_create_collation_v2(SQLiteDB:HSQLiteDB;Name:PAnsiChar;
  eTextRep:integer;pArg:pointer;xCompare:TSQLiteCollationCompare;xDestroy:TSQLiteDestructor):integer; cdecl;
function sqlite3_create_collation16(SQLiteDB:HSQLiteDB;Name:PWideChar;
  eTextRep:integer;pArg:pointer;xCompare:TSQLiteCollationCompare16):integer; cdecl;

function sqlite3_collation_needed(SQLiteDB:HSQLiteDB;Context:pointer;CallBack:TSQLiteCollationNeeded):integer; cdecl;
function sqlite3_collation_needed16(SQLiteDB:HSQLiteDB;Context:pointer;CallBack:TSQLiteCollationNeeded16):integer; cdecl;

//sqlite3_key
//sqlite3_key_v2
//sqlite3_rekey
//sqlite3_rekey_v2
//sqlite3_activate_see
//sqlite3_activate_cerod

function sqlite3_sleep(ms:integer):integer; cdecl;
function sqlite3_get_autocommit(SQLiteDB:HSQLiteDB):integer; cdecl;
function sqlite3_db_handle(Statement:HSQLiteStatement):HSQLiteDB; cdecl;
function sqlite3_db_filename(SQLiteDB:HSQLiteDB;Name:PAnsiChar):PAnsiChar; cdecl;
function sqlite3_db_readonly(SQLiteDB:HSQLiteDB;Name:PAnsiChar):integer; cdecl;
function sqlite3_next_stmt(SQLiteDB:HSQLiteDB;Statement:HSQLiteStatement):HSQLiteStatement; cdecl;

function sqlite3_commit_hook(SQLiteDB:HSQLiteDB;X:TSQLiteHook;Context:pointer):pointer; cdecl;
function sqlite3_rollback_hook(SQLiteDB:HSQLiteDB;X:TSQLiteDestructor;Context:pointer):pointer; cdecl;
function sqlite3_update_hook(SQLiteDB:HSQLiteDB;X:TSQLiteUpdateHook;Context:pointer):pointer; cdecl;

function sqlite3_enable_shared_cache(X:integer):integer; cdecl;
function sqlite3_release_memory(X:integer):integer; cdecl;
function sqlite3_db_release_memory(SQLiteDB:HSQLiteDB):integer; cdecl;
function sqlite3_soft_heap_limit64(N:int64):int64; cdecl;

function sqlite3_table_column_metadata(SQLiteDB:HSQLiteDB;Name:PAnsiChar;TableName:PAnsiChar;ColumnName:PAnsiChar;
  var DataType:PAnsiChar;var CollationSequence:PAnsiChar;
  var NotNull:integer;var PrimaryKey:integer;var AutoInc:integer):integer; cdecl;
function sqlite3_load_extension(SQLiteDB:HSQLiteDB;xFile:PAnsiChar;xProc:PAnsiChar;
  var ErrorMessage:PAnsiChar):integer; cdecl;
function sqlite3_enable_load_extension(SQLiteDB:HSQLiteDB;onoff:integer):integer; cdecl;
//sqlite3_auto_extension
//sqlite3_cancel_auto_extension
//sqlite3_reset_auto_extension

//TODO: virtual table modules
//sqlite3_create_module
//sqlite3_create_module_v2
//sqlite3_declare_vtab
//sqlite3_overload_function

function sqlite3_blob_open(SQLiteDB:HSQLiteDB;DB:PAnsiChar;TableName:PAnsiChar;ColumnName:PAnsiChar;Row:int64;Flags:integer;Blob:HSQLiteBlob):integer; cdecl;
function sqlite3_blob_reopen(Blob:HSQLiteBlob;N:int64):integer; cdecl;
function sqlite3_blob_close(Blob:HSQLiteBlob):integer; cdecl;
function sqlite3_blob_bytes(Blob:HSQLiteBlob):integer; cdecl;
function sqlite3_blob_read(Blob:HSQLiteBlob;var Z;N:integer;Offset:integer):integer; cdecl;
function sqlite3_blob_write(Blob:HSQLiteBlob;var Z;N:integer;Offset:integer):integer; cdecl;

//sqlite3_vfs_find
//sqlite3_vfs_register
//sqlite3_vfs_unregister

function sqlite3_mutex_alloc(X:integer):HSQLiteBlob; cdecl;
procedure sqlite3_mutex_free(Mutex:HSQLiteMutex); cdecl;
procedure sqlite3_mutex_enter(Mutex:HSQLiteMutex); cdecl;
function sqlite3_mutex_try(Mutex:HSQLiteMutex):integer; cdecl;
procedure sqlite3_mutex_leave(Mutex:HSQLiteMutex); cdecl;
//sqlite3_mutex_held
//sqlite3_mutex_notheld
function sqlite3_db_mutex(SQLiteDB:HSQLiteDB):HSQLiteMutex; cdecl;

function sqlite3_file_control(SQLiteDB:HSQLiteDB;Name:PAnsiChar;Op:integer;X:pointer):integer; cdecl;
//sqlite3_test_control
function sqlite3_status(Op:integer;var Current:integer;var HighWater:integer;ResetFlag:integer):integer; cdecl;
function sqlite3_status64(Op:integer;var Current:int64;var HighWater:int64;ResetFlag:integer):integer; cdecl;
function sqlite3_db_status(SQLiteDB:HSQLiteDB;Op:integer;var Current:integer;var HighWater:integer;ResetFlag:integer):integer; cdecl;
function sqlite3_stmt_status(Statement:HSQLiteStatement;Op:integer;ResetFlag:integer):integer; cdecl;

function sqlite3_backup_init(Dest:HSQLiteDB;DestName:PAnsiChar;Source:HSQLiteDB;SourceName:PAnsiChar):HSQLiteBackup; cdecl;
function sqlite3_backup_step(Backup:HSQLiteBackup;Page:integer):integer; cdecl;
function sqlite3_backup_finish(Backup:HSQLiteBackup):integer; cdecl;
function sqlite3_backup_remaining(Backup:HSQLiteBackup):integer; cdecl;
function sqlite3_backup_pagecount(Backup:HSQLiteBackup):integer; cdecl;

function sqlite3_unlock_notify(Blocked:HSQLiteDB;xNotify:TSQLiteUnlockNotify;Context:pointer):integer; cdecl;

function sqlite3_stricmp(X:PAnsiChar;Y:PAnsiChar):integer; cdecl;
function sqlite3_strnicmp(X:PAnsiChar;Y:PAnsiChar;Z:integer):integer; cdecl;
function sqlite3_strglob(X:PAnsiChar;Y:PAnsiChar):integer; cdecl;

//sqlite3_log (cdecl)

function sqlite3_wal_hook(SQLiteDB:HSQLiteDB;Hook:TSQLiteWriteAheadLogHook;Context:pointer):pointer; cdecl;
function sqlite3_wal_autocheckpoint(SQLiteDB:HSQLiteDB;N:integer):integer; cdecl;
function sqlite3_wal_checkpoint(SQLiteDB:HSQLiteDB;DB:PAnsiChar):integer; cdecl;
function sqlite3_wal_checkpoint_v2(SQLiteDB:HSQLiteDB;DB:PAnsiChar;EMode:integer;var Log:integer;var Ckpt:integer):integer; cdecl;

//sqlite3_vtab_config (cdecl)
//sqlite3_vtab_on_conflict
//sqlite3_rtree_geometry_callback
//sqlite3_rtree_geometry
//sqlite3_rtree_query_callback
//sqlite3_rtree_query_info
//fts5

implementation

const
  Sqlite3Dll='sqlite3.dll';

function sqlite3_libversion; external Sqlite3Dll;
function sqlite3_sourceid; external Sqlite3Dll;
function sqlite3_libversion_number; external Sqlite3Dll;
//function sqlite3_compileoption_used; external Sqlite3Dll;
//function sqlite3_compileoption_get; external Sqlite3Dll;
function sqlite3_threadsafe; external Sqlite3Dll;
function sqlite3_close; external Sqlite3Dll;
function sqlite3_close_v2; external Sqlite3Dll;
function sqlite3_exec; external Sqlite3Dll;
function sqlite3_initialize; external Sqlite3Dll;
function sqlite3_shutdown; external Sqlite3Dll;
function sqlite3_os_init; external Sqlite3Dll;
function sqlite3_os_end; external Sqlite3Dll;
function sqlite3_config; external Sqlite3Dll;
function sqlite3_db_config; external Sqlite3Dll;
function sqlite3_extended_result_codes; external Sqlite3Dll;
function sqlite3_last_insert_rowid; external Sqlite3Dll;
function sqlite3_changes; external Sqlite3Dll;
function sqlite3_total_changes; external Sqlite3Dll;
procedure sqlite3_interrupt; external Sqlite3Dll;
function sqlite3_complete; external Sqlite3Dll;
function sqlite3_complete16; external Sqlite3Dll;
function sqlite3_busy_handler; external Sqlite3Dll;
function sqlite3_busy_timeout; external Sqlite3Dll;
function sqlite3_get_table; external Sqlite3Dll;
function sqlite3_free_table; external Sqlite3Dll;
//function sqlite3_mprintf; external Sqlite3Dll;
//function sqlite3_vmprintf; external Sqlite3Dll;
//function sqlite3_snprintf; external Sqlite3Dll;
function sqlite3_malloc; external Sqlite3Dll;
function sqlite3_malloc64; external Sqlite3Dll;
function sqlite3_realloc; external Sqlite3Dll;
function sqlite3_realloc64; external Sqlite3Dll;
procedure sqlite3_free; external Sqlite3Dll;
function sqlite3_msize; external Sqlite3Dll;
function sqlite3_memory_used; external Sqlite3Dll;
function sqlite3_memory_highwater; external Sqlite3Dll;
procedure sqlite3_randomness; external Sqlite3Dll;
function sqlite3_set_authorizer; external Sqlite3Dll;
//sqlite3_trace
procedure sqlite3_progress_handler; external Sqlite3Dll;
function sqlite3_open; external Sqlite3Dll;
function sqlite3_open16; external Sqlite3Dll;
function sqlite3_open_v2; external Sqlite3Dll;
function sqlite3_uri_parameter; external Sqlite3Dll;
function sqlite3_uri_boolean; external Sqlite3Dll;
function sqlite3_uri_int64; external Sqlite3Dll;
function sqlite3_errcode; external Sqlite3Dll;
function sqlite3_extended_errcode; external Sqlite3Dll;
function sqlite3_errmsg; external Sqlite3Dll;
function sqlite3_errmsg16; external Sqlite3Dll;
function sqlite3_errstr; external Sqlite3Dll;
function sqlite3_limit; external Sqlite3Dll;

function sqlite3_prepare; external Sqlite3Dll;
function sqlite3_prepare_v2; external Sqlite3Dll;
function sqlite3_prepare16; external Sqlite3Dll;
function sqlite3_prepare16_v2; external Sqlite3Dll;
function sqlite3_sql; external Sqlite3Dll;
function sqlite3_stmt_readonly; external Sqlite3Dll;
function sqlite3_stmt_busy; external Sqlite3Dll;

function sqlite3_bind_blob; external Sqlite3Dll;
function sqlite3_bind_blob64; external Sqlite3Dll;
function sqlite3_bind_double; external Sqlite3Dll;
function sqlite3_bind_int; external Sqlite3Dll;
function sqlite3_bind_int64; external Sqlite3Dll;
function sqlite3_bind_null; external Sqlite3Dll;
function sqlite3_bind_text; external Sqlite3Dll;
function sqlite3_bind_text16; external Sqlite3Dll;
function sqlite3_bind_text64; external Sqlite3Dll;
function sqlite3_bind_value; external Sqlite3Dll;
function sqlite3_bind_zeroblob; external Sqlite3Dll;
function sqlite3_bind_zeroblob64; external Sqlite3Dll;
function sqlite3_bind_parameter_count; external Sqlite3Dll;
function sqlite3_bind_parameter_name; external Sqlite3Dll;
function sqlite3_bind_parameter_index; external Sqlite3Dll;
function sqlite3_clear_bindings; external Sqlite3Dll;

function sqlite3_column_count; external Sqlite3Dll;
function sqlite3_column_name; external Sqlite3Dll;
function sqlite3_column_name16; external Sqlite3Dll;
function sqlite3_column_database_name; external Sqlite3Dll;
function sqlite3_column_database_name16; external Sqlite3Dll;
function sqlite3_column_table_name; external Sqlite3Dll;
function sqlite3_column_table_name16; external Sqlite3Dll;
function sqlite3_column_origin_name; external Sqlite3Dll;
function sqlite3_column_origin_name16; external Sqlite3Dll;
function sqlite3_column_decltype; external Sqlite3Dll;
function sqlite3_column_decltype16; external Sqlite3Dll;

function sqlite3_step; external Sqlite3Dll;
function sqlite3_data_count; external Sqlite3Dll;

function sqlite3_column_blob; external Sqlite3Dll;
function sqlite3_column_bytes; external Sqlite3Dll;
function sqlite3_column_bytes16; external Sqlite3Dll;
function sqlite3_column_double; external Sqlite3Dll;
function sqlite3_column_int; external Sqlite3Dll;
function sqlite3_column_int64; external Sqlite3Dll;
function sqlite3_column_text; external Sqlite3Dll;
function sqlite3_column_text16; external Sqlite3Dll;
function sqlite3_column_type; external Sqlite3Dll;
function sqlite3_column_value; external Sqlite3Dll;

function sqlite3_finalize; external Sqlite3Dll;
function sqlite3_reset; external Sqlite3Dll;

function sqlite3_create_function; external Sqlite3Dll;
function sqlite3_create_function16; external Sqlite3Dll;
function sqlite3_create_function_v2; external Sqlite3Dll;

function sqlite3_value_blob; external Sqlite3Dll;
function sqlite3_value_bytes; external Sqlite3Dll;
function sqlite3_value_bytes16; external Sqlite3Dll;
function sqlite3_value_double; external Sqlite3Dll;
function sqlite3_value_int; external Sqlite3Dll;
function sqlite3_value_int64; external Sqlite3Dll;
function sqlite3_value_text; external Sqlite3Dll;
function sqlite3_value_text16; external Sqlite3Dll;
function sqlite3_value_text16le; external Sqlite3Dll;
function sqlite3_value_text16be; external Sqlite3Dll;
function sqlite3_value_type; external Sqlite3Dll;
function sqlite3_value_numeric_type; external Sqlite3Dll;

function sqlite3_aggregate_context; external Sqlite3Dll;
function sqlite3_user_data; external Sqlite3Dll;
function sqlite3_context_db_handle; external Sqlite3Dll;
function sqlite3_get_auxdata; external Sqlite3Dll;
procedure sqlite3_set_auxdata; external Sqlite3Dll;

procedure sqlite3_result_blob; external Sqlite3Dll;
procedure sqlite3_result_blob64; external Sqlite3Dll;
procedure sqlite3_result_double; external Sqlite3Dll;
procedure sqlite3_result_error; external Sqlite3Dll;
procedure sqlite3_result_error16; external Sqlite3Dll;
procedure sqlite3_result_error_toobig; external Sqlite3Dll;
procedure sqlite3_result_error_nomem; external Sqlite3Dll;
procedure sqlite3_result_error_code; external Sqlite3Dll;
procedure sqlite3_result_int; external Sqlite3Dll;
procedure sqlite3_result_int64; external Sqlite3Dll;
procedure sqlite3_result_null; external Sqlite3Dll;
procedure sqlite3_result_text; external Sqlite3Dll;
procedure sqlite3_result_text64; external Sqlite3Dll;
procedure sqlite3_result_text16; external Sqlite3Dll;
procedure sqlite3_result_text16le; external Sqlite3Dll;
procedure sqlite3_result_text16be; external Sqlite3Dll;
procedure sqlite3_result_value; external Sqlite3Dll;
procedure sqlite3_result_zeroblob; external Sqlite3Dll;
function sqlite3_result_zeroblob64; external Sqlite3Dll;
procedure sqlite3_result_subtype; external Sqlite3Dll;

function sqlite3_create_collation; external Sqlite3Dll;
function sqlite3_create_collation_v2; external Sqlite3Dll;
function sqlite3_create_collation16; external Sqlite3Dll;

function sqlite3_collation_needed; external Sqlite3Dll;
function sqlite3_collation_needed16; external Sqlite3Dll;

//sqlite3_key
//sqlite3_rekey
//sqlite3_activate_cerod

function sqlite3_sleep; external Sqlite3Dll;
function sqlite3_get_autocommit; external Sqlite3Dll;
function sqlite3_db_handle; external Sqlite3Dll;
function sqlite3_db_filename; external Sqlite3Dll;
function sqlite3_db_readonly; external Sqlite3Dll;
function sqlite3_next_stmt; external Sqlite3Dll;

function sqlite3_commit_hook; external Sqlite3Dll;
function sqlite3_rollback_hook; external Sqlite3Dll;
function sqlite3_update_hook; external Sqlite3Dll;

function sqlite3_enable_shared_cache; external Sqlite3Dll;
function sqlite3_release_memory; external Sqlite3Dll;
function sqlite3_db_release_memory; external Sqlite3Dll;
function sqlite3_soft_heap_limit64; external Sqlite3Dll;

function sqlite3_table_column_metadata; external Sqlite3Dll;
function sqlite3_load_extension; external Sqlite3Dll;
function sqlite3_enable_load_extension; external Sqlite3Dll;
//sqlite3_auto_extension
//sqlite3_reset_auto_extension

//TODO: virtual table modules
//sqlite3_create_module
//sqlite3_create_module_v2
//sqlite3_declare_vtab
//sqlite3_overload_function

function sqlite3_blob_open; external Sqlite3Dll;
function sqlite3_blob_reopen; external Sqlite3Dll;
function sqlite3_blob_close; external Sqlite3Dll;
function sqlite3_blob_bytes; external Sqlite3Dll;
function sqlite3_blob_read; external Sqlite3Dll;
function sqlite3_blob_write; external Sqlite3Dll;

//sqlite3_vfs_find
//sqlite3_vfs_register
//sqlite3_vfs_unregister

function sqlite3_mutex_alloc; external Sqlite3Dll;
procedure sqlite3_mutex_free; external Sqlite3Dll;
procedure sqlite3_mutex_enter; external Sqlite3Dll;
function sqlite3_mutex_try; external Sqlite3Dll;
procedure sqlite3_mutex_leave; external Sqlite3Dll;
//sqlite3_mutex_held
//sqlite3_mutex_notheld
function sqlite3_db_mutex; external Sqlite3Dll;

function sqlite3_file_control; external Sqlite3Dll;
//sqlite3_test_control
function sqlite3_status; external Sqlite3Dll;
function sqlite3_status64; external Sqlite3Dll;
function sqlite3_db_status; external Sqlite3Dll;
function sqlite3_stmt_status; external Sqlite3Dll;

function sqlite3_backup_init; external Sqlite3Dll;
function sqlite3_backup_step; external Sqlite3Dll;
function sqlite3_backup_finish; external Sqlite3Dll;
function sqlite3_backup_remaining; external Sqlite3Dll;
function sqlite3_backup_pagecount; external Sqlite3Dll;

function sqlite3_unlock_notify; external Sqlite3Dll;

function sqlite3_stricmp; external Sqlite3Dll;
function sqlite3_strnicmp; external Sqlite3Dll;
function sqlite3_strglob; external Sqlite3Dll;

//sqlite3_log

function sqlite3_wal_hook; external Sqlite3Dll;
function sqlite3_wal_autocheckpoint; external Sqlite3Dll;
function sqlite3_wal_checkpoint; external Sqlite3Dll;
function sqlite3_wal_checkpoint_v2; external Sqlite3Dll;

//sqlite3_rtree_geometry_callback
//sqlite3_rtree_geometry

resourcestring
    SSQLiteException_ERROR      = 'SQL error or missing database';
    SSQLiteException_INTERNAL   = 'Internal logic error in SQLite';
    SSQLiteException_PERM       = 'Access permission denied';
    SSQLiteException_ABORT      = 'Callback routine requested an abort';
    SSQLiteException_BUSY       = 'The database file is locked';
    SSQLiteException_LOCKED     = 'A table in the database is locked';
    SSQLiteException_NOMEM      = 'A malloc() failed';
    SSQLiteException_READONLY   = 'Attempt to write a readonly database';
    SSQLiteException_INTERRUPT  = 'Operation terminated by sqlite3_interrupt()';
    SSQLiteException_IOERR      = 'Some kind of disk I/O error occurred';
    SSQLiteException_CORRUPT    = 'The database disk image is malformed';
    SSQLiteException_NOTFOUND   = 'NOT USED. Table or record not found';
    SSQLiteException_FULL       = 'Insertion failed because database is full';
    SSQLiteException_CANTOPEN   = 'Unable to open the database file';
    SSQLiteException_PROTOCOL   = 'Database lock protocol error';
    SSQLiteException_EMPTY      = 'Database is empty';
    SSQLiteException_SCHEMA     = 'The database schema changed';
    SSQLiteException_TOOBIG     = 'String or BLOB exceeds size limit';
    SSQLiteException_CONSTRAINT = 'Abort due to constraint violation';
    SSQLiteException_MISMATCH   = 'Data type mismatch';
    SSQLiteException_MISUSE     = 'Library used incorrectly';
    SSQLiteException_NOLFS      = 'Uses OS features not supported on host';
    SSQLiteException_AUTH       = 'Authorization denied';
    SSQLiteException_FORMAT     = 'Auxiliary database format error';
    SSQLiteException_RANGE      = '2nd parameter to sqlite3_bind out of range';
    SSQLiteException_NOTADB     = 'File opened that is not a database file';
    SSQLiteException_ROW        = 'sqlite3_step() has another row ready';
    SSQLiteException_DONE       = 'sqlite3_step() has finished executing';

{ ESQLiteException }

constructor ESQLiteException.Create(ErrorCode: integer);
var
  s:string;
begin
  case ErrorCode and $FF of
    SQLITE_ERROR       :s:=SSQLiteException_ERROR;
    SQLITE_INTERNAL    :s:=SSQLiteException_INTERNAL;
    SQLITE_PERM        :s:=SSQLiteException_PERM;
    SQLITE_ABORT       :s:=SSQLiteException_ABORT;
    SQLITE_BUSY        :s:=SSQLiteException_BUSY;//TODO: SQLITE_BUSY_RECOVERY
    SQLITE_LOCKED      :s:=SSQLiteException_LOCKED;
    SQLITE_NOMEM       :s:=SSQLiteException_NOMEM;
    SQLITE_READONLY    :s:=SSQLiteException_READONLY;
    SQLITE_INTERRUPT   :s:=SSQLiteException_INTERRUPT;
    SQLITE_IOERR       :s:=SSQLiteException_IOERR;//TODO: SQLITE_IOERR_*
    SQLITE_CORRUPT     :s:=SSQLiteException_CORRUPT;
    SQLITE_NOTFOUND    :s:=SSQLiteException_NOTFOUND;
    SQLITE_FULL        :s:=SSQLiteException_FULL;
    SQLITE_CANTOPEN    :s:=SSQLiteException_CANTOPEN;//TODO: SQLITE_CANTOPEN_NOTEMPDIR
    SQLITE_PROTOCOL    :s:=SSQLiteException_PROTOCOL;
    SQLITE_EMPTY       :s:=SSQLiteException_EMPTY;
    SQLITE_SCHEMA      :s:=SSQLiteException_SCHEMA;
    SQLITE_TOOBIG      :s:=SSQLiteException_TOOBIG;
    SQLITE_CONSTRAINT  :s:=SSQLiteException_CONSTRAINT;
    SQLITE_MISMATCH    :s:=SSQLiteException_MISMATCH;
    SQLITE_MISUSE      :s:=SSQLiteException_MISUSE;
    SQLITE_NOLFS       :s:=SSQLiteException_NOLFS;
    SQLITE_AUTH        :s:=SSQLiteException_AUTH;
    SQLITE_FORMAT      :s:=SSQLiteException_FORMAT;
    SQLITE_RANGE       :s:=SSQLiteException_RANGE;
    SQLITE_NOTADB      :s:=SSQLiteException_NOTADB;
    SQLITE_ROW         :s:=SSQLiteException_ROW;
    SQLITE_DONE        :s:=SSQLiteException_DONE;
    else s:='Unknown error '+IntToStr(ErrorCode);
  end;
  inherited create('SQLite: '+s);
  FErrorCode:=ErrorCode;
end;

constructor ESQLiteException.CreateDB(SQLiteDB: HSQLiteDB;
  ErrorCode: integer);
begin
  inherited Create('SQLite: '+sqlite3_errmsg(SQLiteDB));
  FErrorCode:=ErrorCode;
end;

procedure sqlite3_check(Res:integer);
begin
  if Res<>SQLITE_OK then raise ESQLiteException.Create(Res);
end;

procedure sqlite3_check(SQLiteDB:HSQLiteDB;Res:integer);
begin
  if Res<>SQLITE_OK then raise ESQLiteException.CreateDB(SQLiteDB,Res);
end;

end.
