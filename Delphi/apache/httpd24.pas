unit httpd24;

interface

uses WinSock;

{$A8}

const
  MODULE_MAGIC_COOKIE = $041503234; // "AP24"
  MODULE_MAGIC_NUMBER_MAJOR = 20120211; // Apache 2.4.x
  MODULE_MAGIC_NUMBER_MINOR = 0;

type
  PPool = ^TPool;
  TPool = record end; //opaque

  PConfVector = ^TConfVector;
  TConfVector = record end; //opaque

  PFile = ^TFile;
  TFile = record end; //opaque

  PProcess = ^TProcess;
  TProcess = record
    pool: PPool;
    pconf: PPool;
    argc: Integer;
    argv: PPAnsiChar;
    short_name: PAnsiChar;
  end;

  TPort = word;

  PSockAddr = ^TSockAddr;
  TSockAddr = record
    pool: PPool;
    hostname: PAnsiChar;
    servname: PAnsiChar;
    port: TPort;
    family: Integer;
    sin: sockaddr_in;
    salen: Integer;
    ipaddr_len: Integer;
    addr_str_len: Integer;
    ipaddr_ptr: Pointer;
    next: PSockAddr;
  end;

  PServerAddr = ^TServerAddr;
  TServerAddr = record
    next: PServerAddr;
    host_addr: PSockAddr;
    host_port: TPort;
    virthost: PAnsiChar;
  end;

  TIntervalTime = Int64;

  TTableEntry = record
    key: PAnsiChar;
    val: PAnsiChar;
    key_checksum: Cardinal;
  end;

  TTableEntries=array[0..0] of TTableEntry;
  PTableEntries=^TTableEntries;

  PArrayHeader = ^TArrayHeader;
  TArrayHeader = record
    pool: PPool;
    elt_size: Integer;
    nelts: Integer;
    nalloc: Integer;
    elts: PTableEntries;
  end;

  PTable = ^TTable;
  TTable = record
    a: TArrayHeader;
  end;

  PServer = ^TServer;
  TServer = record
    process: PProcess;
    next: PServer;
    defn_name: PAnsiChar;
    defn_line_number: Cardinal;
    server_admin: PAnsiChar;
    server_hostname: PAnsiChar;
    port: TPort;
    error_fname: PAnsiChar;
    error_log: PFile;
    loglevel: Integer;
    is_virtual: Integer;
    module_config: PConfVector;
    lookup_defaults: PConfVector;
    addrs: PServerAddr;
    timeout: TIntervalTime;
    keep_alive_timeout: TIntervalTime;
    keep_alive_max: Integer;
    keep_alive: Integer;
    path: PAnsiChar;
    pathlen: Integer;
    names: PArrayHeader;
    wild_names: PArrayHeader;
    limit_req_line: Integer;
    limit_req_fieldsize: Integer;
    limit_req_fields: Integer;
  end;

  TCommandHow = (
    RAW_ARGS,
    TAKE1,
    TAKE2,
    ITERATE,
    ITERATE2,
    FLAG,
    NO_ARGS,
    TAKE12,
    TAKE3,
    TAKE23,
    TAKE123,
    TAKE13);

  PCommand = ^TCommand;
  TCommand = record
    name: PAnsiChar;
    func: function: PAnsiChar; cdecl;
    cmd_data: Pointer;
    req_override: Integer;
    args_how: TCommandHow;
    errmsg: PAnsiChar;
  end;

  PModule = ^TModule;
  TModule = record
    version: Integer;
    minor_version: Integer;
    module_index: Integer;
    name: PAnsiChar;
    dynamic_load_handle: Pointer;
    next: PModule;
    magic: Cardinal;
    rewrite_args: procedure(process: PProcess); cdecl;
    create_dir_config: function(p: PPool; dir: PAnsiChar): Pointer; cdecl;
    merge_dir_config: function(p: PPool; base_conf: Pointer;
      new_conf: Pointer): Pointer; cdecl;
    create_server_config: function(p: PPool; s: PServer): Pointer; cdecl;
    merge_server_config: function(p: PPool; base_conf: Pointer;
      new_conf: Pointer): Pointer; cdecl;
    cmds: PCommand;
    register_hooks: procedure(p: PPool); cdecl;
  end;

  PFilter = ^TFilter;
  PRequest = ^TRequest;
  PConnection = ^TConnection;
  
  PBucketBrigade = type pointer;
  TStatus = Integer;
  TInputMode = (
    AP_MODE_READBYTES,
    AP_MODE_GETLINE,
    AP_MODE_EATCRLF,
    AP_MODE_SPECULATIVE,
    AP_MODE_EXHAUSTIVE,
    AP_MODE_INIT);
  TReadMode = (
    APR_BLOCK_READ,
    APR_NONBLOCK_READ);

  TFilterOutFunc = function(f: PFilter; b: PBucketBrigade): TStatus; cdecl;
  TFilterInFunc = function(f: PFilter; b: PBucketBrigade;
    mode: TInputMode; block: TReadMode; readbytes: Int64): TStatus; cdecl;

  PFilterFunc = ^TFilterFunc;
  TFilterFunc = record
    case Integer of
      0: (out_func: TFilterOutFunc);
      1: (in_func: TFilterInFunc);
  end;

  TFilterType = (
    AP_FTYPE_RESOURCE     = 10,
    AP_FTYPE_CONTENT_SET  = 20,
    AP_FTYPE_PROTOCOL     = 30,
    AP_FTYPE_TRANSCODE    = 40,
    AP_FTYPE_CONNECTION  = 50,
    AP_FTYPE_NETWORK     = 60);

  PFilterData = ^TFilterData;
  TFilterData = record
    name: PAnsiChar;
    filter_func: TFilterFunc;
    ftype: TFilterType;
    next: PFilterData;
  end;

  TFilter = record
    frec: PFilterData;
    ctx: Pointer;
    next: PFilter;
    r: PRequest;
    c: PConnection;
  end;

  TKeepAlive = (AP_CONN_UNKNOWN, AP_CONN_CLOSE, AP_CONN_KEEPALIVE);

  PBucketAlloc = ^TBucketAlloc;
  TBucketAlloc = record end; //opaque

  TConnection = record
    pool: PPool;
    base_server: PServer;
    vhost_lookup_data: Pointer;
    local_addr: PSockAddr;
    client_addr: PSockAddr;
    client_id: PAnsiChar;
    remote_host: PAnsiChar;
    remote_logname: PAnsiChar;
    local_ip: PAnsiChar;
    local_host: PAnsiChar;
    id: LongInt;
    conn_config: PConfVector;
    notes: PTable;
    input_filters: PFilter;
    output_filters: PFilter;
    sbh: Pointer;
    bucket_alloc: PBucketAlloc;
    cs: Pointer;
    data_in_input_filters: Integer;
    data_in_output_filters: Integer;
    flags1: Cardinal;
    //unsigned int clogging_input_filters:1;
    //signed int double_reverse:2;
    aborted: cardinal;
    keepalive: TKeepAlive;
    keepalives: Integer;
    //log: TLogConf
    //log_id: PAnsiChar;
  end;

  PMethodList=^TMethodList;
  TMethodList=record
    method_mask: Int64;
    method_list: PArrayHeader;
  end;

  PhtaccessResult = ^ThtaccessResult;
  ThtaccessResult = record
    dir: PAnsiChar;
    override_: Integer;
    htaccess: PConfVector;
    next: PhtaccessResult;
  end;

  TURI = record
    scheme: PAnsiChar;
    hostinfo: PAnsiChar;
    user: PAnsiChar;
    password: PAnsiChar;
    hostname: PAnsiChar;
    port_str: PAnsiChar;
    path: PAnsiChar;
    query: PAnsiChar;
    fragment: PAnsiChar;
    hostent: Phostent;
    port: TPort;
    // unsigned is_initialized:1;
    // unsigned dns_looked_up:1;
    // unsigned dns_resolved:1;
    properties: Cardinal;
  end;

  TFType = (
    APR_NOFILE = 0,     
    APR_REG,            
    APR_DIR,            
    APR_CHR,            
    APR_BLK,            
    APR_PIPE,           
    APR_LNK,            
    APR_SOCK);            

  TFInfo = record
    pool: PPool;
    valid: Integer;
    protection: Integer;
    filetype: TFType;
    user: pointer;
    group: pointer;
    inode: UInt64;
    device: Integer;
    nlink: Integer;
    size: Int64;
    csize: Int64;
    atime: Int64;
    mtime: Int64;
    ctime: Int64;
    fname: PAnsiChar;
    name: PAnsiChar;
    filehand: pointer;
  end;

  TRequest = record
    pool: PPool;
    connection: PConnection;
    server: PServer;
    next: PRequest;
    prev: PRequest;
    main: PRequest;
    the_request: PAnsiChar;
    assbackwards: Integer;
    proxyreq: Integer;
    header_only: Integer;
    protocol: PAnsiChar;
    proto_num: Integer;
    hostname: PAnsiChar;
    request_time: Int64;
    status_line: PAnsiChar;
    status: Integer;
    method: PAnsiChar;
    method_number: Integer;
    allowed: Int64;
    allowed_xmethods: PArrayHeader;
    allowed_methods: PMethodList;
    sent_bodyct: Int64;
    bytes_sent: Int64;
    mtime: Int64;
    range: PAnsiChar;
    clength: Int64;
    chunked: Integer;
    read_body: Integer;
    read_chunked: Integer;
    expecting_100: Cardinal;
    kept_body: PBucketBrigade;
    body_table: PTable;
    remaining: Int64;
    read_length: Int64;
    headers_in: PTable;
    headers_out: PTable;
    err_headers_out: PTable;
    subprocess_env: PTable;
    notes: PTable;
    content_type: PAnsiChar;
    handler: PAnsiChar;
    content_encoding: PAnsiChar;
    content_languages: PArrayHeader;
    vlist_validator: PAnsiChar;
    user: PAnsiChar;
    ap_auth_type: PAnsiChar;
    unparsed_uri: PAnsiChar;
    uri: PAnsiChar;
    filename: PAnsiChar;
    canonical_filename: PAnsiChar;
    path_info: PAnsiChar;
    args: PAnsiChar;
    used_path_info: Integer;
    eos_sent: Integer;
    per_dir_config: PConfVector;
    request_config: PConfVector;
    log: pointer;//TODO: Pap_logconf;
    log_id: PAnsiChar;
    htaccess: PhtaccessResult;
    output_filters: PFilter;
    input_filters: PFilter;
    proto_output_filters: PFilter;
    proto_input_filters: PFilter;
    no_cache: Integer;
    no_local_copy: Integer;
    invoke_mtx: pointer;//TODO: Papr_thread_mutex_t;
    parsed_uri: TURI;
    finfo: TFInfo;
    useragent_addr: PSockAddr;
    useragent_ip: PAnsiChar;
    trailers_in: PTable;
    trailers_out: PTable;
  end;

const
  AP_DECLINED = -1;
  AP_DONE = -2;
  AP_OK = 0;		

  REQUEST_NO_BODY          = 0;
  REQUEST_CHUNKED_ERROR    = 1;
  REQUEST_CHUNKED_DECHUNK  = 2;

  APR_SUCCESS = 0;

  APR_HOOK_REALLY_FIRST	= -10;
  APR_HOOK_FIRST		    = 0;
  APR_HOOK_MIDDLE		    = 10;
  APR_HOOK_LAST		      = 20;
  APR_HOOK_REALLY_LAST	= 30;

type
  THookHandler = function(r: PRequest): Integer; cdecl;
  TBrigadeFlush = function(bb: PBucketBrigade; ctx: Pointer): TStatus; cdecl;

function ap_setup_client_block(r: PRequest; read_policy: Integer): Integer; stdcall;
function ap_should_client_block(r: PRequest): Integer; stdcall;
function ap_get_client_block(r: PRequest; buffer: PAnsiChar; bufsiz: Integer): LongInt; stdcall;
function ap_rwrite(var buf; nbyte: Integer; r: PRequest): Integer; stdcall;
function ap_rflush(r: PRequest): Integer; stdcall;
function ap_fflush(f: PFilter; bb: PBucketBrigade): TStatus; stdcall;

function ap_construct_url(p: PPool; const uri: PAnsiChar; r: PRequest): PAnsiChar; stdcall;
function ap_get_server_description: PAnsiChar; stdcall;
function ap_get_server_banner: PAnsiChar; stdcall;
procedure ap_hook_handler(pf: THookHandler; const aszPre: PPAnsiChar;
  const aszSucc: PPAnsiChar; nOrder: Integer); stdcall;
function ap_get_brigade(next: PFilter; bb: PBucketBrigade; mode: TInputMode;
  block: TReadMode; readbytes: Int64): TStatus; stdcall;
procedure ap_remove_input_filter(f: PFilter); stdcall;
procedure ap_remove_output_filter(f: PFilter); stdcall;
procedure ap_send_interim_response(r: PRequest; send_header: Integer); stdcall;
procedure ap_lingering_close(c: PConnection); stdcall;

function apr_table_get(const t: PTable; key: PAnsiChar): PAnsiChar; stdcall;
procedure apr_table_set(t: PTable; const key: PAnsiChar; const val: PAnsiChar); stdcall;
procedure apr_table_add(t: PTable; const key: PAnsiChar; const val: PAnsiChar); stdcall;
function apr_pstrdup(p: PPool; s: PAnsiChar): PAnsiChar; stdcall;
function apr_brigade_create(p: PPool; a: PBucketAlloc): PBucketBrigade; stdcall;
function apr_brigade_destroy(b: PBucketBrigade): TStatus; stdcall;
function apr_brigade_flatten(bb: PBucketBrigade; c: PAnsiChar;
  len: PInteger): TStatus; stdcall;
function apr_brigade_write(b: PBucketBrigade; flush: TBrigadeFlush;
  ctx: Pointer; str: PAnsiChar; nbyte: Integer): TStatus; stdcall;

implementation

const
  la='libapr-1.dll';
  lu='libaprutil-1.dll';
  lh='libhttpd.dll';

function ap_setup_client_block; external lh name '_ap_setup_client_block@8';
function ap_should_client_block; external lh name '_ap_should_client_block@4';
function ap_get_client_block; external lh name '_ap_get_client_block@12';
function ap_rwrite; external lh name '_ap_rwrite@12';
function ap_rflush; external lh name '_ap_rflush@4';
function ap_fflush; external lh name '_ap_fflush@8';
function ap_construct_url; external lh name '_ap_construct_url@12';
function ap_get_server_description; external lh name '_ap_get_server_description@0';
function ap_get_server_banner; external lh name '_ap_get_server_description@0';
procedure ap_hook_handler; external lh name '_ap_hook_handler@16';
function ap_get_brigade; external lh name '_ap_get_brigade@24';
procedure ap_remove_input_filter; external lh name '_ap_remove_input_filter@4';
procedure ap_remove_output_filter; external lh name '_ap_remove_input_filter@4';
procedure ap_send_interim_response; external lh name '_ap_send_interim_response@8';
procedure ap_lingering_close; external lh name '_ap_lingering_close@4';

function apr_table_get; external la name '_apr_table_get@8';
procedure apr_table_set; external la name '_apr_table_set@12';
procedure apr_table_add; external la name '_apr_table_add@12';
function apr_pstrdup; external la name '_apr_pstrdup@8';

function apr_brigade_create; external lu name '_apr_brigade_create@8';
function apr_brigade_destroy; external lu name '_apr_brigade_destroy@4';
function apr_brigade_flatten; external lu name '_apr_brigade_flatten@12';
function apr_brigade_write; external lu name '_apr_brigade_write@20';

end.