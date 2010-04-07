{ *************************************************************************** }
{                                                                             }
{ Delphi and Kylix Cross-Platform Visual Component Library                    }
{ Internet Application Runtime                                                }
{                                                                             }
{ Copyright (C) 2002 Borland Software Corporation                             }
{                                                                             }
{ Licensees holding a valid Borland No-Nonsense License for this Software may }
{ use this file in accordance with such license, which appears in the file    }
{ license.txt that came with this Software.                                   }
{                                                                             }
{ This unit is a pascal wrapper for the Apache interface header files. Those  }
{ files were released under the following copyright:                          }
{                                                                             }
(* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2000-2002 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The names "Apache" and "Apache Software Foundation" must
 *    not be used to endorse or promote products derived from this
 *    software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache",
 *    nor may "Apache" appear in their name, without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 * Portions of this software are based upon public domain software
 * originally written at the National Center for Supercomputing Applications,
 * University of Illinois, Urbana-Champaign.
 *)

unit HTTPD2;

(*$HPPEMIT '#if defined(__WIN32__)'         *)
(*$HPPEMIT '#pragma link "libhttpd.lib"'    *)
(*$HPPEMIT '#pragma link "libapr-1.lib"'    *)
(*$HPPEMIT '#endif'                         *)

interface

{$IFDEF MSWINDOWS}
uses Windows, Winsock;
{$ENDIF}
{$IFDEF Linux}
uses Libc;
{$ENDIF}

const
{$IFDEF MSWINDOWS}
  LibHTTPD = 'libhttpd.dll'; {do not localize}
  LibAPR = 'libapr-1.dll';   {do not localize}
  {$A8}
{$ENDIF}

{$IFDEF LINUX}
  { By using a blank string, we will not require httpd to be built with
    a shared core on Linux. If you wish to build with a shared core,
    you must change the following two defines as required and rebuild
    HTTPD2.pas }
  LibHTTPD = '';
  LibAPR = '';
{$ENDIF}

type
  uid_t = Integer;              // system type
  {$EXTERNALSYM uid_t}
  gid_t = Integer;              // system type
  {$EXTERNALSYM gid_t}
  time_t = LongInt;             // system type
  {$EXTERNALSYM time_t}
  size_t = Integer;             // system type
  {$EXTERNALSYM size_t}

  apr_uint16_t = Word;
  {$EXTERNALSYM apr_uint16_t}
  apr_uint32_t = Cardinal;
  {$EXTERNALSYM apr_uint32_t}
  apr_int64_t = Int64;
  {$EXTERNALSYM apr_int64_t}
  apr_interval_time_t = apr_int64_t;
  {$EXTERNALSYM apr_interval_time_t}
  apr_uint64_t = Int64; // There is no unsigned version
  {$EXTERNALSYM apr_uint64_t}
  apr_port_t = apr_uint16_t;
  {$EXTERNALSYM apr_port_t}
  apr_status_t = Integer;
  {$EXTERNALSYM apr_status_t}
  apr_socklen_t = Integer;
  {$EXTERNALSYM apr_socklen_t}

  (* number of microseconds since 00:00:00 january 1, 1970 UTC *)
  apr_time_t = apr_int64_t;
  {$EXTERNALSYM apr_time_t}

{$HPPEMIT '#include <util_cfgtree.h>'}
(* From util_cfgtree.h *)
(*
 * @package Config Tree Package
 *)

(*
 * Structure used to build the config tree.  The config tree only stores
 * the directives that will be active in the running server.  Directives
 * that contain other directions, such as <Directory ...> cause a sub-level
 * to be created, where the included directives are stored.  The closing
 * directive (</Directory>) is not stored in the tree.
 *)
type
  Pap_directive_t = ^ap_directive_t;
  ap_directive_t = {$IFDEF LINUX}packed{$ENDIF} record
    (* The current directive *)
    directive: PChar;
    (* The arguments for the current directive, stored as a space
     *  separated list *)
    args: PChar;
    (* The next directive node in the tree
     *  @defvar ap_directive_t *next *)
    next: Pap_directive_t;
    (* The first child node of this directive
     *  @defvar ap_directive_t *first_child *)
    first_child: Pap_directive_t;
    (* The parent node of this directive
     *  @defvar ap_directive_t *parent *)
    parent: Pap_directive_t;
    (* directive's module can store add'l data here *)
    data: Pointer;
    (* ### these may go away in the future, but are needed for now *)
    (* The name of the file this directive was found in *)
    filename: PChar;
    (* The line number the directive was on *)
    line_num: Integer;
  end;
  {$EXTERNALSYM ap_directive_t}

(* End from util_cfgtree.h *)

{$HPPEMIT '#include <apr_pools.h>'}
(* From apr_pools.h *)

(* The fundamental pool type *)
  apr_pool_t = {$IFDEF LINUX}packed{$ENDIF} record end;
  {$EXTERNALSYM apr_pool_t}
  Papr_pool_t = ^apr_pool_t;

(* End from apr_pools.h *)

{$HPPEMIT '#include <apr_tables.h>'}
(* From apr_tables.h *)
type
  apr_array_header_t = {$IFDEF LINUX}packed{$ENDIF} record
    (* The pool the array is allocated out of *)
    pool: Papr_pool_t;
    (* The amount of memory allocated for each element of the array *)
    elt_size: Integer;
    (* The number of active elements in the array *)
    nelts: Integer;
    (* The number of elements allocated in the array *)
    nalloc: Integer;
    (* The elements in the array *)
    elts: PChar;
  end;
  Papr_array_header_t = ^apr_array_header_t;
  {$EXTERNALSYM apr_array_header_t} 


(*
 * The (opaque) structure for string-content tables.
 *)

(* The type for each entry in a string-content table *)
  apr_table_entry_t = {$IFDEF LINUX}packed{$ENDIF} record
    (* The key for the current table entry *)
    key: PChar;          (* maybe NULL in future;
                         * check when iterating thru table_elts
                         *)
    (* The value for the current table entry *)
    val: PChar;
    (* A checksum for the key, for use by the apr_table internals *)
    key_checksum: apr_uint32_t;
  end;
  {$EXTERNALSYM apr_table_entry_t}

(* End apr_tables.h *)

{$HPPEMIT '#include <http_config.h>'}
(*
 * @file http_config.h
 * @brief Apache Configuration
 *)
const
  MODULE_MAGIC_COOKIE = $041503232; (* "AP22" *)
  {$EXTERNALSYM MODULE_MAGIC_COOKIE}
  MODULE_MAGIC_NUMBER_MAJOR = 20051115; { Apache 2.2.x }
  {$EXTERNALSYM MODULE_MAGIC_NUMBER_MAJOR}
  MODULE_MAGIC_NUMBER_MINOR = 0;                   (* 0...n *)
  {$EXTERNALSYM MODULE_MAGIC_NUMBER_MINOR}
  MODULE_MAGIC_NUMBER = MODULE_MAGIC_NUMBER_MAJOR; (* backward compat *)
  {$EXTERNALSYM MODULE_MAGIC_NUMBER}


(* Hook orderings *)
(** run this hook first, before ANYTHING *)
  APR_HOOK_REALLY_FIRST	= -10;
  {$EXTERNALSYM APR_HOOK_REALLY_FIRST}
(** run this hook first *)
  APR_HOOK_FIRST		= 0;
  {$EXTERNALSYM APR_HOOK_FIRST}
(** run this hook somewhere *)
  APR_HOOK_MIDDLE		= 10;
  {$EXTERNALSYM APR_HOOK_MIDDLE}
(** run this hook after every other hook which is defined*)
  APR_HOOK_LAST		= 20;
  {$EXTERNALSYM APR_HOOK_LAST}
(** run this hook last, after EVERYTHING *)
  APR_HOOK_REALLY_LAST	= 30;
  {$EXTERNALSYM APR_HOOK_REALLY_LAST}

  
(*
 * The central data structures around here...
 *)

(* Command dispatch structures... *)

(*
 * How the directives arguments should be parsed.
 * @remark Note that for all of these except RAW_ARGS, the config routine is
 *      passed a freshly allocated string which can be modified or stored
 *      or whatever...
 *)

type
  cmd_how = (
    RAW_ARGS,			(*< cmd_func parses command line itself *)
    TAKE1,			(*< one argument only *)
    TAKE2,			(*< two arguments only *)
    ITERATE,			(*< one argument, occuring multiple times
				 * (e.g., IndexIgnore)
				 *)
    ITERATE2,			(*< two arguments, 2nd occurs multiple times
				 * (e.g., AddIcon)
				 *)
    FLAG,			(*< One of 'On' or 'Off' *)
    NO_ARGS,			(*< No args at all, e.g. </Directory> *)
    TAKE12,			(*< one or two arguments *)
    TAKE3,			(*< three arguments only *)
    TAKE23,			(*< two or three arguments *)
    TAKE123,			(*< one, two or three arguments *)
    TAKE13			(*< one or three arguments *)
  );
  {$EXTERNALSYM cmd_how}
(*
 * This structure is passed to a command which is being invoked,
 * to carry a large variety of miscellaneous data which is all of
 * use to *somebody*...
 *)
  cmd_func = function: PChar; cdecl;
  {$EXTERNALSYM cmd_func} 
  // typedef const char *(*cmd_func) ();

(*
 * The command record structure.  Each modules can define a table of these
 * to define the directives it will implement.
 *)

  command_struct  = {$IFDEF LINUX}packed{$ENDIF} record
    (* Name of this command *)
    name: PChar;
    (* The function to be called when this directive is parsed *)
    func: cmd_func;
    (* Extra data, for functions which implement multiple commands... *)
    cmd_data: Pointer;
    (* What overrides need to be allowed to enable this command. *)
    req_override: Integer;
    (* What the command expects as arguments
     *  @defvar cmd_how args_how*)
    args_how: cmd_how;
    (* 'usage' message, in case of syntax errors *)
    errmsg: PChar;
  end;
  {$EXTERNALSYM command_struct}
  Pcommand_struct = ^command_struct;
  command_rec = command_struct;
  {$EXTERNALSYM command_rec}

(*
 * @defgroup ConfigDirectives Allowed locations for configuration directives.
 *
 * The allowed locations for a configuration directive are the union of
 * those indicated by each set bit in the req_override mask.
 *
 * @{
 *)
const
  OR_NONE = 0;             (*< *.conf is not available anywhere in this override *)
  {$EXTERNALSYM OR_NONE}
  OR_LIMIT = 1;	     (*< *.conf inside <Directory> or <Location>
        and .htaccess when AllowOverride Limit *)
  {$EXTERNALSYM OR_LIMIT}
  OR_OPTIONS = 2;         (*< *.conf anywhere
                                and .htaccess when AllowOverride Options *)
  {$EXTERNALSYM OR_OPTIONS}
  OR_FILEINFO = 4;        (*< *.conf anywhere
        and .htaccess when AllowOverride FileInfo *)
  {$EXTERNALSYM OR_FILEINFO}
  OR_AUTHCFG = 8;         (*< *.conf inside <Directory> or <Location>
        and .htaccess when AllowOverride AuthConfig *)
  {$EXTERNALSYM OR_AUTHCFG}
  OR_INDEXES = 16;        (*< *.conf anywhere
        and .htaccess when AllowOverride Indexes *)
  {$EXTERNALSYM OR_INDEXES}
  OR_UNSET = 32;          (*< unset a directive (in Allow) *)
  {$EXTERNALSYM OR_UNSET}
  ACCESS_CONF = 64;       (*< *.conf inside <Directory> or <Location> *)
  {$EXTERNALSYM ACCESS_CONF}
  RSRC_CONF = 128;	     (*< *.conf outside <Directory> or <Location> *)
  {$EXTERNALSYM RSRC_CONF}
  EXEC_ON_READ = 256;     (*< force directive to execute a command
                which would modify the configuration (like including another
                file, or IFModule *)
  {$EXTERNALSYM EXEC_ON_READ}
(* this directive can be placed anywhere *)
  OR_ALL = OR_LIMIT or OR_OPTIONS or OR_FILEINFO or OR_AUTHCFG or OR_INDEXES;
  {$EXTERNALSYM OR_ALL}

(* @} *)

(*
 * This can be returned by a function if they don't wish to handle
 * a command. Make it something not likely someone will actually use
 * as an error code.
 *)
  DECLINE_CMD = #7#8; // In C: "\a\b"
  {$EXTERNALSYM DECLINE_CMD}


{$HPPEMIT '#include <remote_core.h>'}
(* From remote_core.h *)
(* ****************************************************************
 *
 * The most basic server code is encapsulated in a single module
 * known as the core, which is just *barely* functional enough to
 * serve documents, though not terribly well.
 *
 * Largely for NCSA back-compatibility reasons, the core needs to
 * make pieces of its config structures available to other modules.
 * The accessors are declared here, along with the interpretation
 * of one of them (allow_options).
 *)

  OPT_NONE = 0;
  {$EXTERNALSYM OPT_NONE}
  OPT_INDEXES = 1;
  {$EXTERNALSYM OPT_INDEXES}
  OPT_INCLUDES = 2;
  {$EXTERNALSYM OPT_INCLUDES}
  OPT_SYM_LINKS = 4;
  {$EXTERNALSYM OPT_SYM_LINKS}
  OPT_EXECCGI = 8;
  {$EXTERNALSYM OPT_EXECCGI}
  OPT_UNSET = 16;
  {$EXTERNALSYM OPT_UNSET}
  OPT_INCNOEXEC = 32;
  {$EXTERNALSYM OPT_INCNOEXEC}
  OPT_SYM_OWNER = 64;
  {$EXTERNALSYM OPT_SYM_OWNER}
  OPT_MULTI = 128;
  {$EXTERNALSYM OPT_MULTI}
  OPT_ALL = OPT_INDEXES or OPT_INCLUDES or OPT_SYM_LINKS or OPT_EXECCGI;
  {$EXTERNALSYM OPT_ALL}

(* options for get_remote_host() *)
(* REMOTE_HOST returns the hostname, or NULL if the hostname
 * lookup fails.  It will force a DNS lookup according to the
 * HostnameLookups setting.
 *)
  REMOTE_HOST = 0;
  {$EXTERNALSYM REMOTE_HOST}

(* REMOTE_NAME returns the hostname, or the dotted quad if the
 * hostname lookup fails.  It will force a DNS lookup according
 * to the HostnameLookups setting.
 *)
  REMOTE_NAME = 1;
  {$EXTERNALSYM REMOTE_NAME}

(* REMOTE_NOLOOKUP is like REMOTE_NAME except that a DNS lookup is
 * never forced.
 *)
  REMOTE_NOLOOKUP = 2;
  {$EXTERNALSYM REMOTE_NOLOOKUP}

(* REMOTE_DOUBLE_REV will always force a DNS lookup, and also force
 * a double reverse lookup, regardless of the HostnameLookups
 * setting.  The result is the (double reverse checked) hostname,
 * or NULL if any of the lookups fail.
 *)
  REMOTE_DOUBLE_REV = 3;
  {$EXTERNALSYM REMOTE_DOUBLE_REV}

  SATISFY_ALL = 0;
  {$EXTERNALSYM SATISFY_ALL}
  SATISFY_ANY = 1;
  {$EXTERNALSYM SATISFY_ANY}
  SATISFY_NOSPEC = 2;
  {$EXTERNALSYM SATISFY_NOSPEC}

(* Make sure we don't write less than 8000 bytes at any one time.
 *)
  AP_MIN_BYTES_TO_WRITE  = 8000;
  {$EXTERNALSYM AP_MIN_BYTES_TO_WRITE}
(* End from remote_core.h *)  

type
  {$IFDEF MSWINDOWS}
  apr_off_t = Int64;
  {$EXTERNALSYM apr_off_t}
  {$ENDIF}
  {$IFDEF LINUX}
  apr_off_t = Integer;
  {$EXTERNALSYM apr_off_t}
  {$ENDIF}

  apr_int32_t = Integer;
  {$EXTERNALSYM apr_int32_t}
  apr_fileperms_t = apr_int32_t;
  {$EXTERNALSYM apr_fileperms_t}
  apr_size_t = size_t;
  {$EXTERNALSYM apr_size_t}
  Papr_size_t = ^apr_size_t;
  {$EXTERNALSYM Papr_size_t}

  apr_filetype_e = (
    APR_NOFILE = 0,     (*< the file exists, but APR doesn't know its type *)
    APR_REG,            (*< a regular file *)
    APR_DIR,            (*< a directory *)
    APR_CHR,            (*< a character device *)
    APR_BLK,            (*< a block device *)
    APR_PIPE,           (*< a FIFO / pipe *)
    APR_LNK,            (*< a symbolic link *)
    APR_SOCK            (*< a [unix domain] socket *)
  );
  {$EXTERNALSYM apr_filetype_e}

{$IFDEF MSWINDOWS}
  apr_uid_t = PSID;
  {$EXTERNALSYM apr_uid_t}
  apr_gid_t = PSID;
  {$EXTERNALSYM apr_gid_t}
(*
 * Structure for determining the inode of the file.
 * @defvar apr_ino_t
 *)
  apr_ino_t = apr_uint64_t;
  {$EXTERNALSYM apr_ino_t}
(*
 * Structure for determining the device the file is on.
 *)
  apr_dev_t = apr_uint32_t;
  {$EXTERNALSYM apr_dev_t}
{$ELSE}
  apr_uid_t = uid_t;
  {$EXTERNALSYM apr_uid_t}
  apr_gid_t = gid_t;
  {$EXTERNALSYM apr_gid_t}
(* The inode of the file. *)
  apr_ino_t = ino_t;
  {$EXTERNALSYM apr_ino_t}
(* Structure for determining the device the file is on. *)
  apr_dev_t = dev_t;
  {$EXTERNALSYM apr_dev_t}
  va_list = Pointer;
  {$EXTERNALSYM va_list}
{$ENDIF}

  // Forward pointer declarations
  Pap_method_list_t = ^ap_method_list_t;
  Pserver_rec = ^server_rec;
  Pcommand_rec = ^command_rec;
  Pap_conf_vector_t = ^ap_conf_vector_t;
  Pprocess_rec = ^process_rec;
  Pconn_rec = ^conn_rec;
  Papr_table_t = ^apr_table_t;
  Papr_file_t = ^apr_file_t;
  apr_file_t = {$IFDEF LINUX}packed{$ENDIF} record end;
  {$EXTERNALSYM apr_file_t}
  Pap_filter_t = ^ap_filter_t;
  Prequest_rec = ^request_rec;
  apr_socket_t = {$IFDEF LINUX}packed{$ENDIF} record end;
  {$EXTERNALSYM apr_socket_t}
  Papr_socket_t = ^apr_socket_t;
  apr_hash_t = {$IFDEF LINUX}packed{$ENDIF} record end;
  {$EXTERNALSYM apr_hash_t}
  Papr_hash_t = ^apr_hash_t;

(* The opaque string-content table type *)
  apr_table_t = {$IFDEF LINUX}packed{$ENDIF} record
    (* This has to be first to promote backwards compatibility with
     * older modules which cast a apr_table_t * to an apr_array_header_t *...
     * they should use the table_elts() function for most of the
     * cases they do this for.
     *)
    (* The underlying array for the table *)
    a: apr_array_header_t;
  end;
  {$EXTERNALSYM apr_table_t}

(* Common structure for reading of config files / passwd files etc. *)
  ap_configfile_t = {$IFDEF LINUX}packed{$ENDIF} record
    getch: function(param: Pointer): Integer;	    (*< a getc()-like function *)
    getstr: function(buf: Pointer; bufsiz: size_t; param: Pointer): Pointer;
				    (*< a fgets()-like function *)
    close: function(param: Pointer): Integer;	    (*< a close handler function *)
    param: Pointer;                    (*< the argument passed to getch/getstr/close *)
    name: PChar;               (*< the filename / description *)
    line_number: Cardinal;           (*< current line number, starting at 1 *)
  end;
  {$EXTERNALSYM ap_configfile_t}
  Pap_configfile_t = ^ap_configfile_t;

(*
 * This structure is passed to a command which is being invoked,
 * to carry a large variety of miscellaneous data which is all of
 * use to *somebody*...
 *)
 cmd_parms_struct = {$IFDEF LINUX}packed{$ENDIF} record
    (* Argument to command from cmd_table *)
    info: Pointer;
    (* Which allow-override bits are set *)
    override: Integer;
    (* Which methods are <Limit>ed *)
    limited: apr_int64_t;
    (* methods which are limited *)
    limited_xmethods: Papr_array_header_t;
    (* methods which are xlimited *)
    xlimited: Pap_method_list_t;
    (* Config file structure. *)
    config_file: Pap_configfile_t;
    (* the directive specifying this command *)
    directive: Pap_directive_t;
    (* Pool to allocate new storage in *)
    pool: Papr_pool_t;
    (* Pool for scratch memory; persists during configuration, but
     *  wiped before the first request is served...  *)
    temp_pool: Papr_pool_t;
    (* Server_rec being configured for *)
    server: Pserver_rec;
    (* If configuring for a directory, pathname of that directory.
     *  NOPE!  That's what it meant previous to the existance of <Files>,
     * <Location> and regex matching.  Now the only usefulness that can be
     * derived from this field is whether a command is being called in a
     * server context (path == NULL) or being called in a dir context
     * (path != NULL).  *)
    path: PChar;
    (* configuration command *)
    cmd: Pcommand_rec;
    (* per_dir_config vector passed to handle_command *)
    context: Pap_conf_vector_t;
    (* directive with syntax error *)
    err_directive: Pap_directive_t;
  end;
 {$EXTERNALSYM cmd_parms_struct}
  cmd_parms = cmd_parms_struct;
  {$EXTERNALSYM cmd_parms}
  Pcmd_parms = ^cmd_parms;

(*
 * Module structures.  Just about everything is dispatched through
 * these, directly or indirectly (through the command and handler
 * tables).
 *)
  PModule = ^module;
  module = {$IFDEF LINUX}packed{$ENDIF} record
    (* API version, *not* module version; check that module is
     * compatible with this version of the server.
     *)
    version: Integer;
    (* API minor version. Provides API feature milestones. Not checked
     *  during module init *)
    minor_version: Integer;
    (* Index to this modules structures in config vectors.  *)
    module_index: Integer;
    (* The name of the module's C file *)
    name: PChar;
    (* The handle for the DSO.  Internal use only *)
    dynamic_load_handle: Pointer;
    (* A pointer to the next module in the list
     *  @defvar module_struct *next *)
    next: PModule;
    (* Magic Cookie to identify a module structure;  It's mainly
     *  important for the DSO facility (see also mod_so).  *)
    magic: Cardinal;
    (* Function to allow MPMs to re-write command line arguments.  This
     *  hook is only available to MPMs.
     *  @param The process that the server is running in.
     *)
    rewrite_args: procedure(process: Pprocess_rec); cdecl;
    (* Function to allow all modules to create per directory configuration
     *  structures.
     *  @param p The pool to use for all allocations.
     *  @param dir The directory currently being processed.
     *  @return The per-directory structure created
     *)
    create_dir_config: function(p: Papr_pool_t; dir: PChar): Pointer; cdecl;
    (* Function to allow all modules to merge the per directory configuration
     *  structures for two directories.
     *  @param p The pool to use for all allocations.
     *  @param base_conf The directory structure created for the parent directory.
     *  @param new_conf The directory structure currently being processed.
     *  @return The new per-directory structure created
     *)
    merge_dir_config: function(p: Papr_pool_t; base_conf: Pointer;
      new_conf: Pointer): Pointer; cdecl;
    (* Function to allow all modules to create per server configuration
     *  structures.
     *  @param p The pool to use for all allocations.
     *  @param s The server currently being processed.
     *  @return The per-server structure created
     *)
    create_server_config: function(p: Papr_pool_t; s: Pserver_rec): Pointer; cdecl;
    (* Function to allow all modules to merge the per server configuration
     *  structures for two servers.
     *  @param p The pool to use for all allocations.
     *  @param base_conf The directory structure created for the parent directory.
     *  @param new_conf The directory structure currently being processed.
     *  @return The new per-directory structure created
     *)
    merge_server_config: function(p: Papr_pool_t; base_conf: Pointer;
      new_conf: Pointer): Pointer; cdecl;

    (* A command_rec table that describes all of the directives this module
     * defines. *)
    cmds: Pcommand_rec;

    (* A hook to allow modules to hook other points in the request processing.
     *  In this function, modules should call the ap_hook_*() functions to
     *  register an interest in a specific step in processing the current
     *  request.
     *  @param p the pool to use for all allocations
     *)
    register_hooks: procedure(p: Papr_pool_t); cdecl;
  end;
  {$EXTERNALSYM module}

  Papr_sockaddr_t = ^apr_sockaddr_t;
  apr_sockaddr_t = {$IFDEF LINUX}packed{$ENDIF} record
    (* The pool to use... *)
    pool: Papr_pool_t;
    (* The hostname *)
    hostname: PChar;
    (* Either a string of the port number or the service name for the port *)
    servname: PChar;
    (* The numeric port *)
    port: apr_port_t;
    (* The family *)
    family: apr_int32_t;
    (* IPv4 sockaddr structure *)
    sin: sockaddr_in;
    (* How big is the sockaddr we're using? *)
    salen: apr_socklen_t;
    (* How big is the ip address structure we're using? *)
    ipaddr_len: Integer;
    (* How big should the address buffer be?  16 for v4 or 46 for v6
     *  used in inet_ntop... *)
    addr_str_len: Integer;
    (* This points to the IP address structure within the appropriate
     *  sockaddr structure.  *)
    ipaddr_ptr: Pointer;
    (* If multiple addresses were found by apr_sockaddr_info_get(), this
     *  points to a representation of the next address. *)
    next: Papr_sockaddr_t;
  end;
  {$EXTERNALSYM apr_sockaddr_t}

  Papr_bucket_alloc_t = ^apr_bucket_alloc_t;
  apr_bucket_alloc_t = {$IFDEF LINUX}packed{$ENDIF} record end;
  {$EXTERNALSYM apr_bucket_alloc_t}

  ap_conn_keepalive_e = (AP_CONN_UNKNOWN, AP_CONN_CLOSE, AP_CONN_KEEPALIVE);

(** Structure to store things which are per connection *)
  conn_rec = {$IFDEF LINUX}packed{$ENDIF} record
    (** Pool associated with this connection *)
    pool: Papr_pool_t;
    (** Physical vhost this conn came in on *)
    base_server: Pserver_rec;
    (** used by http_vhost.c *)
    vhost_lookup_data: Pointer;

    (* Information about the connection itself *)
    (** local address *)
    local_addr: Papr_sockaddr_t;
    (** remote address *)
    remote_addr: Papr_sockaddr_t;

    (** Client's IP address *)
    remote_ip: PChar;
    (** Client's DNS name, if known.  NULL if DNS hasn't been checked,
     *  "" if it has and no address was found.  N.B. Only access this though
     * get_remote_host() *)
    remote_host: PChar;
    (** Only ever set if doing rfc1413 lookups.  N.B. Only access this through
     *  get_remote_logname() *)
    remote_logname: PChar;

    (** Are we still talking? *)
    flags1: Cardinal;
    { The following are in the flags bitset:
    unsigned aborted:1;}

    (** Are we going to keep the connection alive for another request?
     *  -1 fatal error, 0 undecided, 1 yes   *)
    keepalive: ap_conn_keepalive_e;

    flags2: Cardinal;
    { The following are in the flags bitset:
    (** have we done double-reverse DNS? -1 yes/failure, 0 not yet,
     *  1 yes/success *)
    signed int double_reverse:2;
    }
    (** How many times have we used it? *)
    keepalives: Integer;
    (** server IP address *)
    local_ip: PChar;
    (** used for ap_get_server_name when UseCanonicalName is set to DNS
     *  (ignores setting of HostnameLookups) *)
    local_host: PChar;

    (** ID of this connection; unique at any point in time *)
    id: LongInt;
    (** Notes on *this* connection *)
    conn_config: Pap_conf_vector_t;
    (** send note from one module to another, must remain valid for all
     *  requests on this conn *)
    notes: Papr_table_t;
    (** A list of input filters to be used for this connection *)
    input_filters: Pap_filter_t;
    (** A list of output filters to be used for this connection *)
    output_filters: Pap_filter_t;
    (** handle to scoreboard information for this connection *)
    sbh: Pointer;
    (** The bucket allocator to use for all bucket/brigade creations *)
    bucket_alloc: Papr_bucket_alloc_t;
    // New for Apache 2.2
    (** The current state of this connection *)
    cs: Pointer;
    (** Is there data pending in the input filters? *)
    data_in_input_filters: Integer;
  end;
  {$EXTERNALSYM conn_rec}

(*
 * @defgroup ModuleInit Module structure initializers
 *
 * Initializer for the first few module slots, which are only
 * really set up once we start running.  Note that the first two slots
 * provide a version check; this should allow us to deal with changes to
 * the API. The major number should reflect changes to the API handler table
 * itself or removal of functionality. The minor number should reflect
 * additions of functionality to the existing API. (the server can detect
 * an old-format module, and either handle it back-compatibly, or at least
 * signal an error). See src/include/ap_mmn.h for MMN version history.
 * @{
 *)

(** Use this in all standard modules *)
(* STANDARD20_MODULE_STUFF	*)
//    version := MODULE_MAGIC_NUMBER_MAJOR;
//    minor_version := MODULE_MAGIC_NUMBER_MINOR;
//    module_index := -1;
//    name := ModuleName; // Typically, the source filename
//    dynamic_load_handle := nil;
//    next := nil;
//    magic := MODULE_MAGIC_COOKIE;
//    rewrite_args := nil;

(** Use this only in MPMs *)
(* MPM20_MODULE_STUFF *)
//    version := MODULE_MAGIC_NUMBER_MAJOR;
//    minor_version := MODULE_MAGIC_NUMBER_MINOR;
//    module_index := -1;
//    name := ModuleName; // Typically, the source filename
//    dynamic_load_handle := nil;
//    next := nil;
//    magic := MODULE_MAGIC_COOKIE;


(* CONFIGURATION VECTOR FUNCTIONS *)

(* configuration vector structure *)
  ap_conf_vector_t = {$IFDEF LINUX}packed{$ENDIF} record
  end;
  {$EXTERNALSYM ap_conf_vector_t}

(*
 * A structure to encompass all of the fields in a uri
 *)
  apr_uri_t = {$IFDEF LINUX}packed{$ENDIF} record
    (* scheme ("http"/"ftp"/...) *)
    scheme: PChar;
    (* combined [user[:password]@]host[:port] *)
    hostinfo: PChar;
    (* user name, as in http://user:passwd@host:port/ *)
    user: PChar;
    (* password, as in http://user:passwd@host:port/ *)
    password: PChar;
    (* hostname from URI (or from Host: header) *)
    hostname: PChar;
    (* port string (integer representation is in "port") *)
    port_str: PChar;
    (* the request path (or "/" if only scheme://host was given) *)
    path: PChar;
    (* Everything after a '?' in the path, if present *)
    query: PChar;
    (* Trailing "#fragment" string, if present *)
    fragment: PChar;

    (* structure returned from gethostbyname()
     *  @defvar struct hostent *hostent *)
    hostent: Phostent;

    (* The port number, numeric, valid only if port_str != NULL *)
    port: apr_port_t;

    (* has the structure been initialized *)
    // unsigned is_initialized:1;
    (* has the DNS been looked up yet *)
    // unsigned dns_looked_up:1;
    (* has the dns been resolved yet *)
    // unsigned dns_resolved:1;
    properties: Cardinal; { This takes up all the bits in the bitset }
  end;
  {$EXTERNALSYM apr_uri_t}


(* 
 * The file information structure.  This is analogous to the POSIX
 * stat structure.
 *)
  apr_finfo_t = {$IFDEF LINUX}packed{$ENDIF} record
    (* Allocates memory and closes lingering handles in the specified pool *)
    pool: Papr_pool_t;
    (* The bitmask describing valid fields of this apr_finfo_t structure
     *  including all available 'wanted' fields and potentially more *)
    valid: apr_int32_t;
    (* The access permissions of the file.  Mimics Unix access rights. *)
    protection: apr_fileperms_t;
    (* The type of file.  One of APR_NOFILE, APR_REG, APR_DIR, APR_CHR,
     *  APR_BLK, APR_PIPE, APR_LNK, APR_SOCK
     *)
    filetype: apr_filetype_e;
    (* The user id that owns the file *)
    user: apr_uid_t;
    (* The group id that owns the file *)
    group: apr_gid_t;
    (* The inode of the file. *)
    inode: apr_ino_t;
    (* The id of the device the file is on. *)
    device: apr_dev_t;
    (* The number of hard links to the file. *)
    nlink: apr_int32_t;
    (* The size of the file *)
    size: apr_off_t;
    (* The storage size consumed by the file *)
    csize: apr_off_t;
    (* The time the file was last accessed *)
    atime: apr_time_t;
    (* The time the file was last modified *)
    mtime: apr_time_t;
    (* The time the file was last changed *)
    ctime: apr_time_t;
    (* The full pathname of the file *)
    fname: PChar;
    (* The file's name (no path) in filesystem case *)
    name: PChar;
    (* The file's handle, if accessed (can be submitted to apr_duphandle) *)
    filehand: Papr_file_t;
  end;
  {$EXTERNALSYM apr_finfo_t}

(*
 * This represents the result of calling htaccess; these are cached for
 * each request.
 *)
  Phtaccess_result = ^htaccess_result;
  htaccess_result = {$IFDEF LINUX}packed{$ENDIF} record
    (* the directory to which this applies *)
    dir: PChar;
    (* the overrides allowed for the .htaccess file *)
    override: Integer;
    (* the configuration directives *)
    htaccess: Pap_conf_vector_t;
    (* the next one, or NULL if no more; N.B. never change this *)
    next: Phtaccess_result;
  end;
  {$EXTERNALSYM htaccess_result}


(* A list of buckets *)
  apr_bucket_brigade = Pointer; // apr_buckets.h
  {$EXTERNALSYM apr_bucket_brigade}
  Papr_bucket_brigade = ^apr_bucket_brigade;

  apr_read_type_e = (
    APR_BLOCK_READ,   (* block until data becomes available *)
    APR_NONBLOCK_READ (* return immediately if no data is available *)
  );
  {$EXTERNALSYM apr_read_type_e}


(*
 * input filtering modes
 *)
  ap_input_mode_t = (
    (* The filter should return at most readbytes data. *)
    AP_MODE_READBYTES,
    (* The filter should return at most one line of CRLF data.
     *  (If a potential line is too long or no CRLF is found, the
     *   filter may return partial data).
     *)
    AP_MODE_GETLINE,
    (* The filter should implicitly eat any CRLF pairs that it sees. *)
    AP_MODE_EATCRLF,
    (* The filter read should be treated as speculative and any returned
     *  data should be stored for later retrieval in another mode. *)
    AP_MODE_SPECULATIVE,
    (* The filter read should be exhaustive and read until it can not
     *  read any more.
     *  Use this mode with extreme caution.
     *)
    AP_MODE_EXHAUSTIVE,
    (* The filter should initialize the connection if needed,
     *  NNTP or FTP over SSL for example.
     *)
    AP_MODE_INIT
  );
  {$EXTERNALSYM ap_input_mode_t}

(*
 * @name Filter callbacks
 *
 * This function type is used for filter callbacks. It will be passed a
 * pointer to "this" filter, and a "bucket" containing the content to be
 * filtered.
 *
 * In filter->ctx, the callback will find its context. This context is
 * provided here, so that a filter may be installed multiple times, each
 * receiving its own per-install context pointer.
 *
 * Callbacks are associated with a filter definition, which is specified
 * by name. See ap_register_input_filter() and ap_register_output_filter()
 * for setting the association between a name for a filter and its 
 * associated callback (and other information).
 *
 * The *bucket structure (and all those referenced by ->next and ->prev)
 * should be considered "const". The filter is allowed to modify the
 * next/prev to insert/remove/replace elements in the bucket list, but
 * the types and values of the individual buckets should not be altered.
 *
 * The return value of a filter should be an APR status value.
 *
 * @ingroup filter
 * @{
 *)
  ap_out_filter_func = function(f: Pap_filter_t;
    b: Papr_bucket_brigade): apr_status_t; cdecl;
  {$EXTERNALSYM ap_out_filter_func}
  ap_in_filter_func = function(f: Pap_filter_t; b: Papr_bucket_brigade;
    mode: ap_input_mode_t; block: apr_read_type_e; readbytes: apr_off_t): apr_status_t; cdecl;
  {$EXTERNALSYM ap_in_filter_func}

  Pap_filter_func = ^ap_filter_func;
  ap_filter_func = {$IFDEF LINUX}packed{$ENDIF} record
    case Integer of
      0: (out_func: ap_out_filter_func);
      1: (in_func: ap_in_filter_func);
  end;
  {$EXTERNALSYM ap_filter_func}



(**
 * Filters have different types/classifications. These are used to group
 * and sort the filters to properly sequence their operation.
 *
 * The types have a particular sort order, which allows us to insert them
 * into the filter chain in a determistic order. Within a particular grouping,
 * the ordering is equivalent to the order of calls to ap_add_*_filter().
 *)

  ap_filter_type = (
    (** These filters are used to alter the content that is passed through
     *  them. Examples are SSI or PHP. *)
    AP_FTYPE_RESOURCE     = 10,
    (** These filters are used to alter the content as a whole, but after all
     *  AP_FTYPE_RESOURCE filters are executed.  These filters should not
     *  change the content-type.  An example is deflate.  *)
    AP_FTYPE_CONTENT_SET  = 20,
    (** These filters are used to handle the protocol between server and
     *  client.  Examples are HTTP and POP. *)
    AP_FTYPE_PROTOCOL     = 30,
    (** These filters implement transport encodings (e.g., chunking). *)
    AP_FTYPE_TRANSCODE    = 40,
    (** These filters will alter the content, but in ways that are
     *  more strongly associated with the connection.  Examples are
     *  splitting an HTTP connection into multiple requests and
     *  buffering HTTP responses across multiple requests.
     *
     *  It is important to note that these types of filters are not
     *  allowed in a sub-request. A sub-request's output can certainly
     *  be filtered by ::AP_FTYPE_RESOURCE filters, but all of the "final
     *  processing" is determined by the main request. *)
    AP_FTYPE_CONNECTION  = 50,
    (** These filters don't alter the content.  They are responsible for
     *  sending/receiving data to/from the client. *)
    AP_FTYPE_NETWORK     = 60
  );
  {$EXTERNALSYM ap_filter_type}

(*
 * This structure is used for recording information about the
 * registered filters. It associates a name with the filter's callback
 * and filter type.
 *
 * At the moment, these are simply linked in a chain, so a ->next pointer
 * is available.
 *)
  Pap_filter_rec_t = ^ap_filter_rec_t;
  ap_filter_rec_t = {$IFDEF LINUX}packed{$ENDIF} record
    (* The registered name for this filter *)
    name: PChar;
    (* The function to call when this filter is invoked. *)
    filter_func: ap_filter_func;
    (* The type of filter, either AP_FTYPE_CONTENT or AP_FTYPE_CONNECTION.
     * An AP_FTYPE_CONTENT filter modifies the data based on information
     * found in the content.  An AP_FTYPE_CONNECTION filter modifies the
     * data based on the type of connection.
     *)
    ftype: ap_filter_type;
    (* The next filter_rec in the list *)
    next: Pap_filter_rec_t;
  end;
  {$EXTERNALSYM ap_filter_rec_t}

(*
 * The representation of a filter chain.  Each request has a list
 * of these structures which are called in turn to filter the data.  Sub
 * requests get an exact copy of the main requests filter chain.
 *)
  ap_filter_t = {$IFDEF LINUX}packed{$ENDIF} record
     (* The internal representation of this filter.  This includes
      *  the filter's name, type, and the actual function pointer.
     *)
    frec: Pap_filter_rec_t;

    (* A place to store any data associated with the current filter *)
    ctx: Pointer;

    (* The next filter in the chain *)
    next: Pap_filter_t;

    (* The request_rec associated with the current filter.  If a sub-request
     *  adds filters, then the sub-request is the request associated with the
     *  filter.
     *)
    r: Prequest_rec;

    (* The conn_rec associated with the current filter.  This is analogous
     *  to the request_rec, except that it is used for input filtering.
     *)
    c: Pconn_rec;
  end;
  {$EXTERNALSYM ap_filter_t}


(*
 * Structure for handling HTTP methods.  Methods known to the server are
 * accessed via a bitmask shortcut; extension methods are handled by
 * an array.
 *)
  ap_method_list_t = {$IFDEF LINUX}packed{$ENDIF} record
    (* The bitmask used for known methods *)
    method_mask: apr_int64_t;
    (* the array used for extension methods *)
    method_list: Papr_array_header_t;
  end;
  {$EXTERNALSYM ap_method_list_t}

(* The following four types define a hierarchy of activities, so that
 * given a request_rec r you can write r->connection->server->process
 * to get to the process_rec.  While this reduces substantially the
 * number of arguments that various hooks require beware that in
 * threaded versions of the server you must consider multiplexing
 * issues.  */

(** A structure that represents one process *)
  process_rec = record
    (** Global pool. Please try to cleared on _all_ exits *)
    pool: Papr_pool_t;
    (** aka configuration pool, cleared on restarts *)
    pconf: Papr_pool_t;
    (** How many command line arguments were pass to the program *)
    argc: Integer;
    (** The command line arguments *)
    argv: PPChar;
    (** The program name used to execute the program *)
    short_name: PChar;
  end;
  {$EXTERNALSYM process_rec}

(** A structure that represents the current request *)
  request_rec = {$IFDEF LINUX}packed{$ENDIF} record
    (** The pool associated with the request *)
    pool: Papr_pool_t;
    (** The connection over which this connection has been read *)
    connection: Pconn_rec;
    (** The virtual host this request is for *)
    server: Pserver_rec;

    (** If we wind up getting redirected, pointer to the request we
     *  redirected to.  *)
    next: Prequest_rec;
    (** If this is an internal redirect, pointer to where we redirected
     *  *from*.  *)
    prev: Prequest_rec;

    (** If this is a sub_request (see request.h) pointer back to the
     *  main request.  *)
    main: Prequest_rec;

    (* Info about the request itself... we begin with stuff that only
     * protocol.c should ever touch...
     *)
    (** First line of request, so we can log it *)
    the_request: PChar;
    (** HTTP/0.9, "simple" request (e.g. GET /foo\n w/no headers) *)
    assbackwards: Integer;
    (** A proxy request (calculated during post_read_request/translate_name)
     *  possible values PROXYREQ_NONE, PROXYREQ_PROXY, PROXYREQ_REVERSE,
     *  PROXYREQ_RESPONSE
     *)
    proxyreq: Integer;
    (** HEAD request, as opposed to GET *)
    header_only: Integer;
    (** Protocol, as given to us, or HTTP/0.9 *)
    protocol: PChar;
    (** Number version of protocol; 1.1 = 1001 *)
    proto_num: Integer;
    (** Host, as set by full URI or Host: *)
    hostname: PChar;

    (** When the request started *)
    request_time: apr_time_t;

    (** Status line, if set by script *)
    status_line: PChar;
    (** In any case *)
    status: Integer;

    (* Request method, two ways; also, protocol, etc..  Outside of protocol.c,
     * look, but don't touch.
     *)

    (** GET, HEAD, POST, etc. *)
    method: PChar;
    (** M_GET, M_POST, etc. *)
    method_number: Integer;

    (**
     *  allowed is a bitvector of the allowed methods.
     *
     *  A handler must ensure that the request method is one that
     *  it is capable of handling.  Generally modules should DECLINE
     *  any request methods they do not handle.  Prior to aborting the
     *  handler like this the handler should set r->allowed to the list
     *  of methods that it is willing to handle.  This bitvector is used
     *  to construct the "Allow:" header required for OPTIONS requests,
     *  and HTTP_METHOD_NOT_ALLOWED and HTTP_NOT_IMPLEMENTED status codes.
     *
     *  Since the default_handler deals with OPTIONS, all modules can
     *  usually decline to deal with OPTIONS.  TRACE is always allowed,
     *  modules don't need to set it explicitly.
     *
     *  Since the default_handler will always handle a GET, a
     *  module which does *not* implement GET should probably return
     *  HTTP_METHOD_NOT_ALLOWED.  Unfortunately this means that a Script GET
     *  handler can't be installed by mod_actions.
     *)
    allowed: apr_int64_t;
    (** Array of extension methods *)
    allowed_xmethods: Papr_array_header_t;
    (** List of allowed methods *)
    allowed_methods: Pap_method_list_t;

    (** byte count in stream is for body *)
    sent_bodyct: apr_off_t;
    (** body byte count, for easy access *)
    bytes_sent: apr_off_t;
    (** Time the resource was last modified *)
    mtime: apr_time_t;

    (* HTTP/1.1 connection-level features *)

    (** sending chunked transfer-coding *)
    chunked: Integer;
    (** The Range: header *)
    range: PChar;
    (** The "real" content length *)
    clength: apr_off_t;

    (** Remaining bytes left to read from the request body *)
    remaining: apr_off_t;
    (** Number of bytes that have been read  from the request body *)
    read_length: apr_off_t;
    (** Method for reading the request body
     * (eg. REQUEST_CHUNKED_ERROR, REQUEST_NO_BODY,
     *  REQUEST_CHUNKED_DECHUNK, etc...) *)
    read_body: Integer;
    (** reading chunked transfer-coding *)
    read_chunked: Integer;
    (** is client waiting for a 100 response? *)
    expecting_100: Cardinal;

    (* MIME header environments, in and out.  Also, an array containing
     * environment variables to be passed to subprocesses, so people can
     * write modules to add to that environment.
     *
     * The difference between headers_out and err_headers_out is that the
     * latter are printed even on error, and persist across internal redirects
     * (so the headers printed for ErrorDocument handlers will have them).
     *
     * The 'notes' apr_table_t is for notes from one module to another, with no
     * other set purpose in mind...
     *)

    (** MIME header environment from the request *)
    headers_in: Papr_table_t;
    (** MIME header environment for the response *)
    headers_out: Papr_table_t;
    (** MIME header environment for the response, printed even on errors and
     * persist across internal redirects *)
    err_headers_out: Papr_table_t;
    (** Array of environment variables to be used for sub processes *)
    subprocess_env: Papr_table_t;
    (** Notes from one module to another *)
    notes: Papr_table_t;

    (* content_type, handler, content_encoding, and all content_languages
     * MUST be lowercased strings.  They may be pointers to static strings;
     * they should not be modified in place.
     *)
    (** The content-type for the current request *)
    content_type: PChar;	(* Break these out --- we dispatch on 'em *)
    (** The handler string that we use to call a handler function *)
    handler: PChar;	(* What we *really* dispatch on           *)

    (** How to encode the data *)
    content_encoding: PChar;
    (** array of (PChar) representing the content languages *)
    content_languages: Papr_array_header_t;

    (** variant list validator (if negotiated) *)
    vlist_validator: PChar;

    (** If an authentication check was made, this gets set to the user name. *)
    user: PChar;
    (** If an authentication check was made, this gets set to the auth type. *)
    ap_auth_type: PChar;

    (** This response is non-cache-able *)
    no_cache: Integer;
    (** There is no local copy of this response *)
    no_local_copy: Integer;

    (* What object is being requested (either directly, or via include
     * or content-negotiation mapping).
     *)

    (** the uri without any parsing performed *)
    unparsed_uri: PChar;
    (** the path portion of the URI *)
    uri: PChar;
    (** The filename on disk that this response corresponds to *)
    filename: PChar;
    (** The true filename, we canonicalize r->filename if these don't match *)
    canonical_filename: PChar;
    (** The path_info for this request if there is any. *)
    path_info: PChar;
    (** QUERY_ARGS, if any *)
    args: PChar;
    (** ST_MODE set to zero if no such file *)
    finfo: apr_finfo_t;
    (** components of uri, dismantled *)
    parsed_uri: apr_uri_t;

    (** Flag for the handler to accept or reject path_info on
     *  the current request.  All modules should respect the
     *  AP_REQ_ACCEPT_PATH_INFO and AP_REQ_REJECT_PATH_INFO
     *  values, while AP_REQ_DEFAULT_PATH_INFO indicates they
     *  may follow existing conventions.  This is set to the
     *  user's preference upon HOOK_VERY_FIRST of the fixups.
     *)
    used_path_info: Integer;

    (* Various other config info which may change with .htaccess files
     * These are config vectors, with one void* pointer for each module
     * (the thing pointed to being the module's business).
     *)

    (** Options set in config files, etc. *)
    per_dir_config: Pap_conf_vector_t;
    (** Notes on *this* request *)
    request_config: Pap_conf_vector_t;

(**
 * a linked list of the configuration directives in the .htaccess files
 * accessed by this request.
 * N.B. always add to the head of the list, _never_ to the end.
 * that way, a sub request's list can (temporarily) point to a parent's list
 *)
    htaccess: Phtaccess_result;

    (** A list of output filters to be used for this request *)
    output_filters: Pap_filter_t;
    (** A list of input filters to be used for this request *)
    input_filters: Pap_filter_t;

    (** A list of protocol level output filters to be used for this
     *  request *)
    proto_output_filters: Pap_filter_t;
    (** A list of protocol level input filters to be used for this
     *  request *)
    proto_input_filters: Pap_filter_t;

    (** A flag to determine if the eos bucket has been sent yet *)
    eos_sent: Integer;

(* Things placed at the end of the record to avoid breaking binary
 * compatibility.  It would be nice to remember to reorder the entire
 * record to improve 64bit alignment the next time we need to break
 * binary compatibility for some other reason.
 *)
  end;
  {$EXTERNALSYM request_rec}

(* A structure to be used for Per-vhost config *)

  Pserver_addr_rec = ^server_addr_rec;
  server_addr_rec = {$IFDEF LINUX}packed{$ENDIF} record
    (* The next server in the list *)
    next: Pserver_addr_rec;
    (* The bound address, for this server *)
    host_addr: Papr_sockaddr_t;
    (* The bound port, for this server *)
    host_port: apr_port_t;
    (* The name given in <VirtualHost> *)
    virthost: PChar;
  end;
  {$EXTERNALSYM server_addr_rec}

(* A structure to store information for each virtual server *)
  server_rec = {$IFDEF LINUX}packed{$ENDIF} record
    (* The process this server is running in *)
    process: Pprocess_rec;
    (* The next server in the list *)
    next: Pserver_rec;
    (* The name of the server *)
    defn_name: PChar;
    (* The line of the config file that the server was defined on *)
    defn_line_number: Cardinal;

    (* Contact information *)

    (* The admin's contact information *)
    server_admin: PChar;
    (* The server hostname *)
    server_hostname: PChar;
    (* for redirects, etc. *)
    port: apr_port_t;

    (* Log files --- note that transfer log is now in the modules... *)

    (* The name of the error log *)
    error_fname: PChar;
    (* A file descriptor that references the error log *)
    error_log: Papr_file_t;
    (* The log level for this server *)
    loglevel: Integer;

    (* Module-specific configuration for server, and defaults... *)

    (* true if this is the virtual server *)
    is_virtual: Integer;
    (* Config vector containing pointers to modules' per-server config
     *  structures. *)
    module_config: Pap_conf_vector_t;
    (* MIME type info, etc., before we start checking per-directory info *)
    lookup_defaults: Pap_conf_vector_t;

    (* Transaction handling *)
    (* I haven't got a clue *)
    addrs: Pserver_addr_rec;
    (* Timeout, in apr interval, before we give up *)
    timeout: apr_interval_time_t;
    (* The apr interval  we'll wait for another request *)
    keep_alive_timeout: apr_interval_time_t;
    (* Maximum requests per connection *)
    keep_alive_max: Integer;
    (* Use persistent connections? *)
    keep_alive: Integer;

    (* Pathname for ServerPath *)
    path: PChar;
    (* Length of path *)
    pathlen: Integer;

    (* Normal names for ServerAlias servers *)
    names: Papr_array_header_t;
    (* Wildcarded names for ServerAlias servers *)
    wild_names: Papr_array_header_t;

    (* limit on size of the HTTP request line    *)
    limit_req_line: Integer;
    (* limit on size of any request header field *)
    limit_req_fieldsize: Integer;
    (* limit on number of request header fields  *)
    limit_req_fields: Integer;
  end;
  {$EXTERNALSYM server_rec}

(*
 * Generic command handling function for strings
 * @param cmd The command parameters for this directive
 * @param struct_ptr pointer into a given type
 * @param arg The argument to the directive
 * @return An error string or NULL on success
 *)
function ap_set_string_slot(cmd: Pcmd_parms; struct_ptr: Pointer;
  const arg: PChar): PChar; cdecl;
{$EXTERNALSYM ap_set_string_slot}

(*
 * Generic command handling function for integers
 * @param cmd The command parameters for this directive
 * @param struct_ptr pointer into a given type
 * @param arg The argument to the directive
 * @return An error string or NULL on success
 *)
function ap_set_int_slot(cmd: Pcmd_parms; struct_ptr: Pointer;
  arg: PChar): PChar; cdecl;
{$EXTERNALSYM ap_set_int_slot}

(*
 * Return true if the specified method is limited by being listed in
 * a <Limit> container, or by *not* being listed in a <LimiteExcept>
 * container.
 *
 * @param   method  Pointer to a string specifying the method to check.
 * @param   cmd     Pointer to the cmd_parms structure passed to the
 *                  directive handler.
 * @return  0 if the method is not limited in the current scope
 *)
function ap_method_is_limited(cmd: Pcmd_parms; const method: PChar): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_method_is_limited}

(*
 * Generic command handling function for strings, always sets the value
 * to a lowercase string
 * @param cmd The command parameters for this directive
 * @param struct_ptr pointer into a given type
 * @param arg The argument to the directive
 * @return An error string or NULL on success
 *)
function ap_set_string_slot_lower(cmd: Pcmd_parms; struct_ptr: Pointer;
  const arg: PChar): PChar; cdecl;
{$EXTERNALSYM ap_set_string_slot_lower}

const
  SERVER_PROTOCOL = 'HTTP/1.1';  {do not localize}
  {$EXTERNALSYM SERVER_PROTOCOL}

  AP_DECLINED = -1;             ///* Module declines to handle */
  {$EXTERNALSYM AP_DECLINED}
  DECLINED = AP_DECLINED;
  {$EXTERNALSYM DECLINED}
  AP_DONE = -2;			///* Module has served the response completely
				// *  - it's safe to die() with no more output
				// */
  {$EXTERNALSYM AP_DONE}
  DONE = AP_DONE;
  {$EXTERNALSYM DONE}
  AP_OK = 0;			///* Module has handled this stage. */
  {$EXTERNALSYM AP_OK}
  OK = AP_OK;
  {$EXTERNALSYM OK}


//* ----------------------- HTTP Status Codes  ------------------------- */

//* The size of the static array in http_protocol.c for storing
// * all of the potential response status-lines (a sparse table).
// * A future version should dynamically generate the table at startup.
// */
  RESPONSE_CODES = 55;
  {$EXTERNALSYM RESPONSE_CODES}

  HTTP_CONTINUE                      = 100;
  {$EXTERNALSYM HTTP_CONTINUE}
  HTTP_SWITCHING_PROTOCOLS           = 101;
  {$EXTERNALSYM HTTP_SWITCHING_PROTOCOLS}
  HTTP_PROCESSING                    = 102;
  {$EXTERNALSYM HTTP_PROCESSING}
  HTTP_OK                            = 200;
  {$EXTERNALSYM HTTP_OK}
  HTTP_CREATED                       = 201;
  {$EXTERNALSYM HTTP_CREATED}
  HTTP_ACCEPTED                      = 202;
  {$EXTERNALSYM HTTP_ACCEPTED}
  HTTP_NON_AUTHORITATIVE             = 203;
  {$EXTERNALSYM HTTP_NON_AUTHORITATIVE}
  HTTP_NO_CONTENT                    = 204;
  {$EXTERNALSYM HTTP_NO_CONTENT}
  HTTP_RESET_CONTENT                 = 205;
  {$EXTERNALSYM HTTP_RESET_CONTENT}
  HTTP_PARTIAL_CONTENT               = 206;
  {$EXTERNALSYM HTTP_PARTIAL_CONTENT}
  HTTP_MULTI_STATUS                  = 207;
  {$EXTERNALSYM HTTP_MULTI_STATUS}
  HTTP_MULTIPLE_CHOICES              = 300;
  {$EXTERNALSYM HTTP_MULTIPLE_CHOICES}
  HTTP_MOVED_PERMANENTLY             = 301;
  {$EXTERNALSYM HTTP_MOVED_PERMANENTLY}
  HTTP_MOVED_TEMPORARILY             = 302;
  {$EXTERNALSYM HTTP_MOVED_TEMPORARILY}
  HTTP_SEE_OTHER                     = 303;
  {$EXTERNALSYM HTTP_SEE_OTHER}
  HTTP_NOT_MODIFIED                  = 304;
  {$EXTERNALSYM HTTP_NOT_MODIFIED}
  HTTP_USE_PROXY                     = 305;
  {$EXTERNALSYM HTTP_USE_PROXY}
  HTTP_TEMPORARY_REDIRECT            = 307;
  {$EXTERNALSYM HTTP_TEMPORARY_REDIRECT}
  HTTP_BAD_REQUEST                   = 400;
  {$EXTERNALSYM HTTP_BAD_REQUEST}
  HTTP_UNAUTHORIZED                  = 401;
  {$EXTERNALSYM HTTP_UNAUTHORIZED}
  HTTP_PAYMENT_REQUIRED              = 402;
  {$EXTERNALSYM HTTP_PAYMENT_REQUIRED}
  HTTP_FORBIDDEN                     = 403;
  {$EXTERNALSYM HTTP_FORBIDDEN}
  HTTP_NOT_FOUND                     = 404;
  {$EXTERNALSYM HTTP_NOT_FOUND}
  HTTP_METHOD_NOT_ALLOWED            = 405;
  {$EXTERNALSYM HTTP_METHOD_NOT_ALLOWED}
  HTTP_NOT_ACCEPTABLE                = 406;
  {$EXTERNALSYM HTTP_NOT_ACCEPTABLE}
  HTTP_PROXY_AUTHENTICATION_REQUIRED = 407;
  {$EXTERNALSYM HTTP_PROXY_AUTHENTICATION_REQUIRED}
  HTTP_REQUEST_TIME_OUT              = 408;
  {$EXTERNALSYM HTTP_REQUEST_TIME_OUT}
  HTTP_CONFLICT                      = 409;
  {$EXTERNALSYM HTTP_CONFLICT}
  HTTP_GONE                          = 410;
  {$EXTERNALSYM HTTP_GONE}
  HTTP_LENGTH_REQUIRED               = 411;
  {$EXTERNALSYM HTTP_LENGTH_REQUIRED}
  HTTP_PRECONDITION_FAILED           = 412;
  {$EXTERNALSYM HTTP_PRECONDITION_FAILED}
  HTTP_REQUEST_ENTITY_TOO_LARGE      = 413;
  {$EXTERNALSYM HTTP_REQUEST_ENTITY_TOO_LARGE}
  HTTP_REQUEST_URI_TOO_LARGE         = 414;
  {$EXTERNALSYM HTTP_REQUEST_URI_TOO_LARGE}
  HTTP_UNSUPPORTED_MEDIA_TYPE        = 415;
  {$EXTERNALSYM HTTP_UNSUPPORTED_MEDIA_TYPE}
  HTTP_RANGE_NOT_SATISFIABLE         = 416;
  {$EXTERNALSYM HTTP_RANGE_NOT_SATISFIABLE}
  HTTP_EXPECTATION_FAILED            = 417;
  {$EXTERNALSYM HTTP_EXPECTATION_FAILED}
  HTTP_UNPROCESSABLE_ENTITY          = 422;
  {$EXTERNALSYM HTTP_UNPROCESSABLE_ENTITY}
  HTTP_LOCKED                        = 423;
  {$EXTERNALSYM HTTP_LOCKED}
  HTTP_FAILED_DEPENDENCY             = 424;
  {$EXTERNALSYM HTTP_FAILED_DEPENDENCY}
  HTTP_INTERNAL_SERVER_ERROR         = 500;
  {$EXTERNALSYM HTTP_INTERNAL_SERVER_ERROR}
  HTTP_NOT_IMPLEMENTED               = 501;
  {$EXTERNALSYM HTTP_NOT_IMPLEMENTED}
  HTTP_BAD_GATEWAY                   = 502;
  {$EXTERNALSYM HTTP_BAD_GATEWAY}
  HTTP_SERVICE_UNAVAILABLE           = 503;
  {$EXTERNALSYM HTTP_SERVICE_UNAVAILABLE}
  HTTP_GATEWAY_TIME_OUT              = 504;
  {$EXTERNALSYM HTTP_GATEWAY_TIME_OUT}
  HTTP_VERSION_NOT_SUPPORTED         = 505;
  {$EXTERNALSYM HTTP_VERSION_NOT_SUPPORTED}
  HTTP_VARIANT_ALSO_VARIES           = 506;
  {$EXTERNALSYM HTTP_VARIANT_ALSO_VARIES}
  HTTP_INSUFFICIENT_STORAGE          = 507;
  {$EXTERNALSYM HTTP_INSUFFICIENT_STORAGE}
  HTTP_NOT_EXTENDED                  = 510;
  {$EXTERNALSYM HTTP_NOT_EXTENDED}

(*
 * Methods recognized (but not necessarily handled) by the server.
 * These constants are used in bit shifting masks of size int, so it is
 * unsafe to have more methods than bits in an int.  HEAD == M_GET.
 * This list must be tracked by the list in http_protocol.c in routine
 * ap_method_name_of().
 *)
  M_GET                  = 0;       (* RFC 2616: HTTP *)
  {$EXTERNALSYM M_GET}
  M_PUT                  = 1;       (*  :             *)
  {$EXTERNALSYM M_PUT}
  M_POST                 = 2;
  {$EXTERNALSYM M_POST}
  M_DELETE               = 3;
  {$EXTERNALSYM M_DELETE}
  M_CONNECT              = 4;
  {$EXTERNALSYM M_CONNECT}
  M_OPTIONS              = 5;
  {$EXTERNALSYM M_OPTIONS}
  M_TRACE                = 6;       (* RFC 2616: HTTP *)
  {$EXTERNALSYM M_TRACE}
  M_PATCH                = 7;       (* no rfc(!)  ### remove this one? *)
  {$EXTERNALSYM M_PATCH}
  M_PROPFIND             = 8;       (* RFC 2518: WebDAV *)
  {$EXTERNALSYM M_PROPFIND}
  M_PROPPATCH            = 9;       (*  :               *)
  {$EXTERNALSYM M_PROPPATCH}
  M_MKCOL                = 10;
  {$EXTERNALSYM M_MKCOL}
  M_COPY                 = 11;
  {$EXTERNALSYM M_COPY}
  M_MOVE                 = 12;
  {$EXTERNALSYM M_MOVE}
  M_LOCK                 = 13;
  {$EXTERNALSYM M_LOCK}
  M_UNLOCK               = 14;      (* RFC 2518: WebDAV *)
  {$EXTERNALSYM M_UNLOCK}
  M_VERSION_CONTROL      = 15;      (* RFC 3253: WebDAV Versioning *)
  {$EXTERNALSYM M_VERSION_CONTROL}
  M_CHECKOUT             = 16;      (*  :                          *)
  {$EXTERNALSYM M_CHECKOUT}
  M_UNCHECKOUT           = 17;
  {$EXTERNALSYM M_UNCHECKOUT}
  M_CHECKIN              = 18;
  {$EXTERNALSYM M_CHECKIN}
  M_UPDATE               = 19;
  {$EXTERNALSYM M_UPDATE}
  M_LABEL                = 20;
  {$EXTERNALSYM M_LABEL}
  M_REPORT               = 21;
  {$EXTERNALSYM M_REPORT}
  M_MKWORKSPACE          = 22;
  {$EXTERNALSYM M_MKWORKSPACE}
  M_MKACTIVITY           = 23;
  {$EXTERNALSYM M_MKACTIVITY}
  M_BASELINE_CONTROL     = 24;
  {$EXTERNALSYM M_BASELINE_CONTROL}
  M_MERGE                = 25;
  {$EXTERNALSYM M_MERGE}
  M_INVALID              = 26;      (* RFC 3253: WebDAV Versioning *)
  {$EXTERNALSYM M_INVALID}


(**
 * METHODS needs to be equal to the number of bits
 * we are using for limit masks.
 *)
  METHODS   = 64;
  {$EXTERNALSYM METHODS}

(**
 * The method mask bit to shift for anding with a bitmask.
 *)

  AP_METHOD_BIT: apr_int64_t = 1;
  {$EXTERNALSYM AP_METHOD_BIT}

  CGI_MAGIC_TYPE = 'application/x-httpd-cgi';
  {$EXTERNALSYM CGI_MAGIC_TYPE}
  INCLUDES_MAGIC_TYPE = 'text/x-server-parsed-html';
  {$EXTERNALSYM INCLUDES_MAGIC_TYPE}
  INCLUDES_MAGIC_TYPE3 = 'text/x-server-parsed-html3';
  {$EXTERNALSYM INCLUDES_MAGIC_TYPE3}
  MAP_FILE_MAGIC_TYPE = 'application/x-type-map';
  {$EXTERNALSYM MAP_FILE_MAGIC_TYPE}
  ASIS_MAGIC_TYPE = 'httpd/send-as-is';
  {$EXTERNALSYM ASIS_MAGIC_TYPE}
  DIR_MAGIC_TYPE = 'httpd/unix-directory';
  {$EXTERNALSYM DIR_MAGIC_TYPE}
  STATUS_MAGIC_TYPE = 'application/x-httpd-status';
  {$EXTERNALSYM STATUS_MAGIC_TYPE}

  // Error Level constants
//  APLOG_MARK    = __FILE__, __LINE__
  APLOG_EMERG   = 0;	///* system is unusable */
  {$EXTERNALSYM APLOG_EMERG}
  APLOG_ALERT   = 1;	///* action must be taken immediately */
  {$EXTERNALSYM APLOG_ALERT}
  APLOG_CRIT	= 2;	///* critical conditions */
  {$EXTERNALSYM APLOG_CRIT}
  APLOG_ERR	= 3;	///* error conditions */
  {$EXTERNALSYM APLOG_ERR}
  APLOG_WARNING	= 4;	///* warning conditions */
  {$EXTERNALSYM APLOG_WARNING}
  APLOG_NOTICE	= 5;	///* normal but significant condition */
  {$EXTERNALSYM APLOG_NOTICE}
  APLOG_INFO	= 6;	///* informational */
  {$EXTERNALSYM APLOG_INFO}
  APLOG_DEBUG	= 7;	///* debug-level messages */
  {$EXTERNALSYM APLOG_DEBUG}

  APLOG_LEVELMASK = 7;	///* mask off the level value */
  {$EXTERNALSYM APLOG_LEVELMASK}
  APLOG_NOERRNO	  = (APLOG_LEVELMASK + 1);
  {$EXTERNALSYM APLOG_NOERRNO}

  (* Send 413 error if message has any body *)
  REQUEST_NO_BODY          = 0;
  {$EXTERNALSYM REQUEST_NO_BODY}
  (* Send 411 error if body without Content-Length *)
  REQUEST_CHUNKED_ERROR    = 1;
  {$EXTERNALSYM REQUEST_CHUNKED_ERROR}
  (* If chunked, remove the chunks for me. *)
  REQUEST_CHUNKED_DECHUNK  = 2;
  {$EXTERNALSYM REQUEST_CHUNKED_DECHUNK}

(**
 * @defgroup ProxyReq Proxy request types
 *
 * Possible values of request_rec->proxyreq. A request could be normal,
 *  proxied or reverse proxied. Normally proxied and reverse proxied are
 *  grouped together as just "proxied", but sometimes it's necessary to
 *  tell the difference between the two, such as for authentication.
 * @{
 *)

  PROXYREQ_NONE  = 0;		(**< No proxy *)
  {$EXTERNALSYM PROXYREQ_NONE}
  PROXYREQ_PROXY = 1;	(**< Standard proxy *)
  {$EXTERNALSYM PROXYREQ_PROXY}
  PROXYREQ_REVERSE = 2;	(**< Reverse proxy *)
  {$EXTERNALSYM PROXYREQ_REVERSE}

(* @} *)

(* Per-vhost config... *)

(**
 * The address 255.255.255.255, when used as a virtualhost address,
 * will become the "default" server when the ip doesn't match other vhosts.
 *)
  DEFAULT_VHOST_ADDR = $ffffffff;
  {$EXTERNALSYM DEFAULT_VHOST_ADDR}


{$HPPEMIT '#include <apr_tables.h>'}
{ Table operations - from apr_tables.h }

(* flag for overlap to use apr_table_setn *)
  APR_OVERLAP_TABLES_SET = 0;
  {$EXTERNALSYM APR_OVERLAP_TABLES_SET}
(* flag for overlap to use apr_table_mergen *)
  APR_OVERLAP_TABLES_MERGE = 1;
  {$EXTERNALSYM APR_OVERLAP_TABLES_MERGE}


(*
 * Generic accessors for other modules to get at their own module-specific
 * data
 * @param conf_vector The vector in which the modules configuration is stored.
 *        usually r->per_dir_config or s->module_config
 * @param m The module to get the data for.
 * @return The module-specific data
 *)
function ap_get_module_config(const cv: Pap_conf_vector_t;
  const m: Pmodule): Pointer;
{$EXTERNALSYM ap_get_module_config}


(*
 * Generic accessors for other modules to set at their own module-specific
 * data
 * @param conf_vector The vector in which the modules configuration is stored.
 *        usually r->per_dir_config or s->module_config
 * @param m The module to set the data for.
 * @param val The module-specific data to set
 *)

procedure ap_set_module_config(cv: Pap_conf_vector_t; const m: Pmodule;
  val: Pointer);
{$EXTERNALSYM ap_set_module_config}

(*
 * Create an array
 * @param p The pool to allocate the memory out of
 * @param nelts the number of elements in the initial array
 * @param elt_size The size of each element in the array.
 * @return The new array
 *)

function apr_array_make(p: apr_pool_t; nelts: Integer;
  elt_size: Integer): Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_array_make}

(*
 * Add a new element to an array
 * @param arr The array to add an element to.
 * @return Location for the new element in the array.
 * @remark If there are no free spots in the array, then this function will
 *         allocate new space for the new element.
 *)
function apr_array_push(arr: Papr_array_header_t): Pointer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_array_push}

(*
 * Concatenate two arrays together
 * @param dst The destination array, and the one to go first in the combined 
 *            array
 * @param src The source array to add to the destination array
 *)
procedure apr_array_cat(dst: Papr_array_header_t; const src: Papr_array_header_t);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_array_cat}

(*
 * Copy the entire array
 * @param p The pool to allocate the copy of the array out of
 * @param arr The array to copy
 * @return An exact copy of the array passed in
 * @remark The alternate apr_array_copy_hdr copies only the header, and arranges
 *         for the elements to be copied if (and only if) the code subsequently
 *         does a push or arraycat.
 *)
function apr_array_copy(p: Papr_pool_t; const arr: Papr_array_header_t): Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_array_copy}

(*
 * Copy the headers of the array, and arrange for the elements to be copied if
 * and only if the code subsequently does a push or arraycat.
 * @param p The pool to allocate the copy of the array out of
 * @param arr The array to copy
 * @return An exact copy of the array passed in
 * @remark The alternate apr_array_copy copies the *entire* array.
 *)
function apr_array_copy_hdr(p: Papr_pool_t; const arr: Papr_array_header_t): Papr_array_header_t
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_array_copy_hdr}

(*
 * Append one array to the end of another, creating a new array in the process.
 * @param p The pool to allocate the new array out of
 * @param first The array to put first in the new array.
 * @param second The array to put second in the new array.
 * @param return A new array containing the data from the two arrays passed in.
*)
function apr_array_append(p: Papr_pool_t; const first: Papr_array_header_t;
  const second: Papr_array_header_t): Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_array_append}
(*
 * Generates a new string from the apr_pool_t containing the concatenated
 * sequence of substrings referenced as elements within the array.  The string
 * will be empty if all substrings are empty or null, or if there are no
 * elements in the array.  If sep is non-NUL, it will be inserted between
 * elements as a separator.
 * @param p The pool to allocate the string out of
 * @param arr The array to generate the string from
 * @param sep The separator to use
 * @return A string containing all of the data in the array.
 *)
function apr_array_pstrcat(p: Papr_pool_t; const arr: Papr_array_header_t;
	const sep: Char): PChar; {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_array_pstrcat}

(*
 * Make a new table
 * @param p The pool to allocate the pool out of
 * @param nelts The number of elements in the initial table.
 * @return The new table.
 * @warning This table can only store text data
 *)
function apr_table_make(p: Papr_pool_t; nelts: Integer): Papr_table_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_table_make}

(*
 * Create a new table and copy another table into it
 * @param p The pool to allocate the new table out of
 * @param t The table to copy
 * @return A copy of the table passed in
 *)
function apr_table_copy(p: Papr_pool_t; const t: Papr_table_t): Papr_table_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_table_copy}

(*
 * Delete all of the elements from a table
 * @param t The table to clear
 *)
procedure apr_table_clear(t: Papr_table_t); {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_table_clear}

(*
 * Get the value associated with a given key from the table.  After this call,
 * The data is still in the table
 * @param t The table to search for the key
 * @param key The key to search for
 * @return The value associated with the key
 *)
function apr_table_get(const t: Papr_table_t; key: PChar): PChar;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_table_get}
(*
 * Add a key/value pair to a table, if another element already exists with the
 * same key, this will over-write the old data.
 * @param t The table to add the data to.
 * @param key The key fo use
 * @param val The value to add
 * @remark When adding data, this function makes a copy of both the key and the
 *         value.
 *)
procedure apr_table_set(t: Papr_table_t; const key: PChar; const val: PChar);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_table_set}

(*
 * Add a key/value pair to a table, if another element already exists with the
 * same key, this will over-write the old data.
 * @param t The table to add the data to.
 * @param key The key fo use
 * @param val The value to add
 * @warning When adding data, this function does not make a copy of the key or
 *          the value, so care should be taken to ensure that the values will
 *          not change after they have been added..
 *)
procedure apr_table_setn(t: Papr_table_t; const key: PChar;
                                 const val: PChar);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_table_setn}

(*
 * Remove data from the table
 * @param t The table to remove data from
 * @param key The key of the data being removed
 *)
procedure apr_table_unset(t: Papr_table_t; const key: PChar);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_table_unset}

(*
 * Add data to a table by merging the value with data that has already been
 * stored
 * @param t The table to search for the data
 * @param key The key to merge data for
 * @param val The data to add
 * @remark If the key is not found; then this function acts like apr_table_add
 *)
procedure apr_table_merge(t: Papr_table_t; const key: PChar; const val: PChar);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_table_merge}

(*
 * Add data to a table by merging the value with data that has already been
 * stored
 * @param t The table to search for the data
 * @param key The key to merge data for
 * @param val The data to add
 * @remark If the key is not found; then this function acts like apr_table_addn
 *)
procedure apr_table_mergen(t: Papr_table_t; const key: PChar; const val: PChar);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_table_mergen}

(*
 * Add data to a table; regardless of whether there is another element with the
 * same key.
 * @param t The table to add to
 * @param key The key to use
 * @param val The value to add.
 * @remark When adding data; this function makes a copy of both the key and the
 *         value.
 *)
procedure apr_table_add(t: Papr_table_t; const key: PChar;
                                const val: PChar);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_table_add}

(*
 * Add data to a table; regardless of whether there is another element with the
 * same key.
 * @param t The table to add to
 * @param key The key to use
 * @param val The value to add.
 * @remark When adding data; this function does not make a copy of the key or the
 *         value; so care should be taken to ensure that the values will not
 *         change after they have been added..
 *)
procedure apr_table_addn(t: Papr_table_t; const key: PChar; const val: PChar);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_table_addn}

(*
 * Merge two tables into one new table
 * @param p The pool to use for the new table
 * @param overlay The first table to put in the new table
 * @param base The table to add at the end of the new table
 * @return A new table containing all of the data from the two passed in
 *)
function apr_table_overlay(p: Papr_pool_t; const overlay: Papr_table_t;
  const base: Papr_table_t): Papr_table_t
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_table_overlay}

(*
 * Iterate over a table running the provided function once for every
 * element in the table.  If there is data passed in as a vararg; then the
 * function is only run on those elements whose key matches something in
 * the vararg.  If the vararg is NULL; then every element is run through the
 * function.  Iteration continues while the function returns non-zero.
 * @param comp The function to run
 * @param rec The data to pass as the first argument to the function
 * @param t The table to iterate over
 * @param ... The vararg.  If this is NULL; then all elements in the table are
 *            run through the function; otherwise only those whose key matches
 *            are run.
 *)

{ apr_table_do not translated due to open args }
{ APR_DECLARE_NONSTD(void) apr_table_do(int (*comp)(void *; const char *; const char *);
                              void *rec; const t: Papr_table_t; ...); }

(* 
 * Iterate over a table running the provided function once for every
 * element in the table.  If there is data passed in as a vararg; then the 
 * function is only run on those element's whose key matches something in 
 * the vararg.  If the vararg is NULL; then every element is run through the
 * function.  Iteration continues while the function returns non-zero.
 * @param comp The function to run
 * @param rec The data to pass as the first argument to the function
 * @param t The table to iterate over
 * @param vp The vararg table.  If this is NULL; then all elements in the 
 *                table are run through the function; otherwise only those 
 *                whose key matches are run.
 *)
type
  TCompFunc = function(P: Pointer; PC: PChar; PC2: PChar): Integer; cdecl;
procedure apr_table_vdo(comp: TCompFunc; rec: Pointer; const t: Papr_table_t;
  list: va_list);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_table_vdo}

(*
 * For each element in table b, either use setn or mergen to add the data
 * to table a.  Which method is used is determined by the flags passed in.
 * @param a The table to add the data to.
 * @param b The table to iterate over, adding its data to table a
 * @param flags How to add the table to table a.  One of:
 *          APR_OVERLAP_TABLES_SET        Use apr_table_setn
 *          APR_OVERLAP_TABLES_MERGE      Use apr_table_mergen
 * @remark  This function is highly optimized, and uses less memory and CPU cycles
 *          than a function that just loops through table b calling other functions.
 *)
(*
 *<PRE>
 * Conceptually, apr_table_overlap does this:
 *
 *  apr_array_header_t *barr = apr_table_elts(b);
 *  apr_table_entry_t *belt = (apr_table_entry_t * )barr->elts;
 *  int i;
 *
 *  for (i = 0; i < barr->nelts; ++i) {
 *      if (flags & APR_OVERLAP_TABLES_MERGE) {
 *          apr_table_mergen(a, belt[i].key, belt[i].val);
 *      }
 *      else {
 *          apr_table_setn(a, belt[i].key, belt[i].val);
 *      }
 *  }
 *
 *  Except that it is more efficient (less space and cpu-time) especially
 *  when b has many elements.
 *
 *  Notice the assumptions on the keys and values in b -- they must be
 *  in an ancestor of a's pool.  In practice b and a are usually from
 *  the same pool.
 * </PRE>
 *)

procedure apr_table_overlap(a: Papr_table_t; const b: Papr_table_t;
  flags: Cardinal); {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM apr_table_overlap}


{$HPPEMIT '#include <http_core.h>'}
(* From http_core.h *)
(*
 * Retrieve the value of Options for this request
 * @param r The current request
 * @return the Options bitmask
 * @deffunc int ap_allow_options(request_rec *r)
 *)
function ap_allow_options(r: Prequest_rec): Integer;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_allow_options}

(*
 * Retrieve the value of the AllowOverride for this request
 * @param r The current request
 * @return the overrides bitmask
 * @deffunc int ap_allow_overrides(r: Prequest_rec)
 *)
function ap_allow_overrides(r: Prequest_rec): Integer;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_allow_overrides}

(*
 * Retrieve the value of the DefaultType directive, or text/plain if not set
 * @param r The current request
 * @return The default type
 * @deffunc const char *ap_default_type(r: Prequest_rec)
 *)
function ap_default_type(r: Prequest_rec): PChar;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_default_type}

(*
 * Retrieve the document root for this server
 * @param r The current request
 * @warning Don't use this!  If your request went through a Userdir, or 
 * something like that, it'll screw you.  But it's back-compatible...
 * @return The document root
 * @deffunc const char *ap_document_root(r: Prequest_rec)
 *)
function ap_document_root(r: Prequest_rec): PChar;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_document_root}

(*
 * Lookup the remote client's DNS name or IP address
 * @param conn The current connection
 * @param dir_config The directory config vector from the request
 * @param type The type of lookup to perform.  One of:
 * <pre>
 *     REMOTE_HOST returns the hostname, or NULL if the hostname
 *                 lookup fails.  It will force a DNS lookup according to the
 *                 HostnameLookups setting.
 *     REMOTE_NAME returns the hostname, or the dotted quad if the
 *                 hostname lookup fails.  It will force a DNS lookup according
 *                 to the HostnameLookups setting.
 *     REMOTE_NOLOOKUP is like REMOTE_NAME except that a DNS lookup is
 *                     never forced.
 *     REMOTE_DOUBLE_REV will always force a DNS lookup, and also force
 *                   a double reverse lookup, regardless of the HostnameLookups
 *                   setting.  The result is the (double reverse checked) 
 *                   hostname, or NULL if any of the lookups fail.
 * </pre>
 * @param str_is_ip unless NULL is passed, this will be set to non-zero on output when an IP address 
 *        string is returned
 * @return The remote hostname
 * @deffunc const char *ap_get_remote_host(conn_rec *conn, void *dir_config, int type, int *str_is_ip)
 *)
function ap_get_remote_host(conn: Pconn_rec; dir_config: Pointer;
  _type: Integer; str_is_ip: PInteger): PChar;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_get_remote_host}

(*
 * Retrieve the login name of the remote user.  Undef if it could not be
 * determined
 * @param r The current request
 * @return The user logged in to the client machine
 * @deffunc const char *ap_get_remote_logname(r: Prequest_rec)
 *)
function ap_get_remote_logname(r: Prequest_rec): PChar;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_get_remote_logname}

(* Used for constructing self-referencing URLs, and things like SERVER_PORT,
 * and SERVER_NAME.
 *)
(*
 * build a fully qualified URL from the uri and information in the request rec
 * @param p The pool to allocate the URL from
 * @param uri The path to the requested file
 * @param r The current request
 * @return A fully qualified URL
 * @deffunc char *ap_construct_url(apr_pool_t *p, const char *uri, r: Prequest_rec)
 *)
function ap_construct_url(p: Papr_pool_t; const uri: PChar; r: Prequest_rec): PChar;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_construct_url}
(*
 * Get the current server name from the request
 * @param r The current request
 * @return the server name
 * @deffunc const char *ap_get_server_name(r: Prequest_rec)
 *)
function  ap_get_server_name(r: Prequest_rec): PChar;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_get_server_name}
(*
 * Get the current server port
 * @param The current request
 * @return The server's port
 * @deffunc apr_port_t ap_get_server_port(const r: Prequest_rec)
 *)
function ap_get_server_port(const r: Prequest_rec): apr_port_t;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_get_server_port}
(*
 * Get the server version
 * @return The server software version
 * @deffunc const char *ap_get_server_version()
 *)
function ap_get_server_version: PChar;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_get_server_version}
(*
 * Return the limit on bytes in request msg body
 * @param r The current request
 * @return the maximum number of bytes in the request msg body
 * @deffunc apr_off_t ap_get_limit_req_body(const r: Prequest_rec)
 *)
function ap_get_limit_req_body(const r: Prequest_rec): apr_off_t;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_get_limit_req_body}
(*
 * Return the limit on bytes in XML request msg body
 * @param r The current request
 * @return the maximum number of bytes in XML request msg body
 * @deffunc size_t ap_get_limit_xml_body(const r: Prequest_rec)
 *)
function ap_get_limit_xml_body(const r: Prequest_rec): size_t;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_get_limit_xml_body}
(*
 * Install a custom response handler for a given status
 * @param r The current request
 * @param status The status for which the custom response should be used
 * @param string The custom response.  This can be a static string, a file
 *               or a URL
 *)
procedure ap_custom_response(r: Prequest_rec; status: Integer; const str: PChar);
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_custom_response}
(*
 * Check for a definition from the server command line
 * @param name The define to check for
 * @return 1 if defined, 0 otherwise
 * @deffunc int ap_exists_config_define(const char *name)
 *)
function ap_exists_config_define(const name: PChar): Integer;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_exists_config_define}
(* FIXME! See STATUS about how *)
function ap_core_translate(r: Prequest_rec): Integer; cdecl;
{$EXTERNALSYM ap_core_translate}

(* Authentication stuff.  This is one of the places where compatibility
 * with the old config files *really* hurts; they don't discriminate at
 * all between different authentication schemes, meaning that we need
 * to maintain common state for all of them in the core, and make it
 * available to the other modules through interfaces.
 *)

(*
 * Return the type of authorization required for this request
 * @param r The current request
 * @return The authorization required
 * @deffunc const char *ap_auth_type(r: Prequest_rec)
 *)
function ap_auth_type(r: Prequest_rec): PChar;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_auth_type}

(*
 * Return the current Authorization realm
 * @param r The current request
 * @return The current authorization realm
 * @deffunc const char *ap_auth_name(r: Prequest_rec)
 *)
function ap_auth_name(r: Prequest_rec): PChar;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_auth_name}

(*
 * How the requires lines must be met.
 * @param r The current request
 * @return How the requirements must be met.  One of:
 * <pre>
 *      SATISFY_ANY    -- any of the requirements must be met.
 *      SATISFY_ALL    -- all of the requirements must be met.
 *      SATISFY_NOSPEC -- There are no applicable satisfy lines
 * </pre>
 * @deffunc int ap_satisfies(r: Prequest_rec)
 *)
function ap_satisfies(r: Prequest_rec): Integer;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_satisfies}

(*
 * Retrieve information about all of the requires directives for this request
 * @param r The current request
 * @return An array of all requires directives for this request
 * @deffunc const apr_array_header_t *ap_requires(r: Prequest_rec)
 *)
function ap_requires(r: Prequest_rec): Papr_array_header_t;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_requires}

(* End from http_core.h *)

{$HPPEMIT '#include <http_protocol.h>'}
(* From http_protocol.h *)

(* Finish up stuff after a request *)

(*
 * Called at completion of sending the response.  It sends the terminating
 * protocol information.
 * @param r The current request
 * @deffunc void ap_finalize_request_protocol(r: Prequest_rec)
 *)
procedure ap_finalize_request_protocol(r: Prequest_rec);
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_finalize_request_protocol}

(*
 * Send error back to client.
 * @param r The current request
 * @param recursive_error last arg indicates error status in case we get 
 *      an error in the process of trying to deal with an ErrorDocument 
 *      to handle some other error.  In that case; we print the default 
 *      report for the first thing that went wrong; and more briefly report 
 *      on the problem with the ErrorDocument.
 * @deffunc void ap_send_error_response(r: Prequest_rec; recursive_error: Integer)
 *)
procedure ap_send_error_response(r: Prequest_rec; recursive_error: Integer);
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_send_error_response}

(* Set last modified header line from the lastmod date of the associated file.
 * Also; set content length.
 *
 * May return an error status; typically HTTP_NOT_MODIFIED (that when the
 * permit_cache argument is set to one).
 *)

(*
 * Set the content length for this request
 * @param r The current request
 * @param length The new content length
 * @deffunc void ap_set_content_length(r: Prequest_rec; apr_off_t length)
 *)
procedure ap_set_content_length(r: Prequest_rec; length: apr_off_t);
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_set_content_length}

(*
 * Set the keepalive status for this request
 * @param r The current request
 * @return 1 if keepalive can be set; 0 otherwise
 * @deffunc int ap_set_keepalive(r: Prequest_rec)
 *)
function ap_set_keepalive(r: Prequest_rec): Integer;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_set_keepalive}

(*
 * Return the latest rational time from a request/mtime pair.  Mtime is
 * returned unless it's in the future; in which case we return the current time.
 * @param r The current request
 * @param mtime The last modified time
 * @return the latest rational time.
 * @deffunc apr_time_t ap_rationalize_mtime(r: Prequest_rec; apr_time_t mtime)
 *)
function ap_rationalize_mtime(r: Prequest_rec; mtime: apr_time_t): apr_time_t;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_rationalize_mtime}

(*
 * Build the content-type that should be sent to the client from the
 * content-type specified.  The following rules are followed:
 *    - if type is NULL; type is set to ap_default_type(r)
 *    - if charset adding is disabled; stop processing and return type.
 *    - then; if there are no parameters on type; add the default charset
 *    - return type
 * @param r The current request
 * @return The content-type
 * @deffunc const char *ap_make_content_type(r: Prequest_rec; const char *type);
 *)
function ap_make_content_type(r: Prequest_rec; const _type: PChar): PChar;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_make_content_type}

(*
 * Construct an entity tag from the resource information.  If it's a real
 * file; build in some of the file characteristics.
 * @param r The current request
 * @param force_weak Force the entity tag to be weak - it could be modified
 *                   again in as short an interval.
 * @return The entity tag
 * @deffunc char *ap_make_etag(r: Prequest_rec; int force_weak)
 *)
function ap_make_etag(r: Prequest_rec; force_weak: Integer): PChar;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_make_etag}

(*
 * Set the E-tag outgoing header
 * @param The current request
 * @deffunc void ap_set_etag(r: Prequest_rec)
 *)
procedure ap_set_etag(r: Prequest_rec);
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_set_etag}

(*
 * Set the last modified time for the file being sent
 * @param r The current request
 * @deffunc void ap_set_last_modified(r: Prequest_rec)
 *)
procedure ap_set_last_modified(r: Prequest_rec);
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_set_last_modified}

(*
 * Implements condition GET rules for HTTP/1.1 specification.  This function
 * inspects the client headers and determines if the response fulfills
 * the requirements specified.
 * @param r The current request
 * @return 1 if the response fulfills the condition GET rules; 0 otherwise
 * @deffunc int ap_meets_conditions(r: Prequest_rec)
 *)
function ap_meets_conditions(r: Prequest_rec): Integer;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_meets_conditions}

(* Other ways to send stuff at the client.  All of these keep track
 * of bytes_sent automatically.  This indirection is intended to make
 * it a little more painless to slide things like HTTP-NG packetization
 * underneath the main body of the code later.  In the meantime; it lets
 * us centralize a bit of accounting (bytes_sent).
 *
 * These also return the number of bytes written by the call.
 * They should only be called with a timeout registered; for obvious reaasons.
 * (Ditto the send_header stuff).
 *)

(*
 * Send an entire file to the client; using sendfile if supported by the
 * current platform
 * @param fd The file to send.
 * @param r The current request
 * @param offset Offset into the file to start sending.
 * @param length Amount of data to send
 * @param nbytes Amount of data actually sent
 * @deffunc apr_status_t ap_send_fd(apr_file_t *fd; r: Prequest_rec; apr_off_t offset; apr_size_t length; apr_size_t *nbytes);
 *)
function ap_send_fd(fdL: Papr_file_t; r: Prequest_rec; offset: apr_off_t;
  length: apr_size_t; nbytes: Papr_size_t): apr_status_t;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_send_fd}


(*
 * Register a new request method; and return the offset that will be
 * associated with that method.
 *
 * @param p        The pool to create registered method numbers from.
 * @param methname The name of the new method to register.
 * @return         Ab int value representing an offset into a bitmask.
 *)
function ap_method_register(p: Papr_pool_t; const methname: PChar): Integer;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_method_register}

(*
 * Initialize the method_registry and allocate memory for it.
 *
 * @param p Pool to allocate memory for the registry from.
 *)
procedure ap_method_registry_init(p: Papr_pool_t);
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_method_registry_init}

(*
 * Create a new method list with the specified number of preallocated
 * slots for extension methods.
 *
 * @param   p       Pointer to a pool in which the structure should be
 *                  allocated.
 * @param   nelts   Number of preallocated extension slots
 * @return  Pointer to the newly created structure.
 * @deffunc ap_method_list_t ap_make_method_list(apr_pool_t *p; int nelts)
 *)
function ap_make_method_list(p: Papr_pool_t; nelts: Integer): ap_method_list_t;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_make_method_list}
procedure ap_copy_method_list(dest: Pap_method_list_t; src: Pap_method_list_t);
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_copy_method_list}

(*
 * Search for an HTTP method name in an ap_method_list_t structure; and
 * return true if found.
 *
 * @param   method  String containing the name of the method to check.
 * @param   l       Pointer to a method list; such as cmd->methods_limited.
 * @return  1 if method is in the list; otherwise 0
 * @deffunc int ap_method_in_list(const char *method; ap_method_list_t *l)
 *)
function ap_method_in_list(l: Pap_method_list_t; const method: PChar): Integer;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_method_in_list}

(*
 * Add an HTTP method name to an ap_method_list_t structure if it isn't
 * already listed.
 *
 * @param   method  String containing the name of the method to check.
 * @param   l       Pointer to a method list; such as cmd->methods_limited.
 * @return  None.
 * @deffunc void ap_method_in_list(l: Pap_method_list_t; const char *method)
 *)
procedure ap_method_list_add(l: Pap_method_list_t; const method: PChar);
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_method_list_add}
(*
 * Remove an HTTP method name from an ap_method_list_t structure.
 *
 * @param   l       Pointer to a method list; such as cmd->methods_limited.
 * @param   method  String containing the name of the method to remove.
 * @return  None.
 * @deffunc void ap_method_list_remove(l: Pap_method_list_t; const char *method)
 *)
procedure ap_method_list_remove(l: Pap_method_list_t; const method: PChar);
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_method_list_remove}

(*
 * Reset a method list to be completely empty.
 *
 * @param   l       Pointer to a method list; such as cmd->methods_limited.
 * @return  None.
 * @deffunc void ap_clear_method_list(l: Pap_method_list_t)
 *)
procedure ap_clear_method_list(l: Pap_method_list_t);
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_clear_method_list}
(*
 * Set the content type for this request (r->content_type).
 * Note:
 * This function must be called to set r->content_type in order
 * for the AddOutputFilterByType directive to work correctly.
 * @param r The current request
 * @param length The new content type
 * @deffunc void ap_set_content_type(r: Prequest_rec; const char* ct)
 *)
procedure ap_set_content_type(r: Prequest_rec; const ct: PChar);
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_set_content_type}
(* Hmmm... could macrofy these for now; and maybe forever; though the
 * definitions of the macros would get a whole lot hairier.
 *)

(*
 * Output one character for this request
 * @param c the character to output
 * @param r the current request
 * @return The number of bytes sent
 * @deffunc int ap_rputc(int c; r: Prequest_rec)
 *)
function ap_rputc(c: Integer; r: Prequest_rec): Integer;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_rputc}
(*
 * Output a string for the current request
 * @param str The string to output
 * @param r The current request
 * @return The number of bytes sent
 * @deffunc int ap_rputs(const char *str; r: Prequest_rec)
 *)
function ap_rputs(const str: PChar; r: Prequest_rec): Integer;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_rputs}
(*
 * Write a buffer for the current request
 * @param buf The buffer to write
 * @param nbyte The number of bytes to send from the buffer
 * @param r The current request
 * @return The number of bytes sent
 * @deffunc int ap_rwrite(const void *buf; int nbyte; r: Prequest_rec)
 *)
function ap_rwrite(var buf; nbyte: Integer; r: Prequest_rec): Integer;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_rwrite}
(*
 * Write an unspecified number of strings to the request
 * @param r The current request
 * @param ... The strings to write
 * @return The number of bytes sent
 * @deffunc int ap_rvputs(r: Prequest_rec; ...)
 *)
// not supported by Pascal translation
// AP_DECLARE_NONSTD(int) ap_rvputs(r: Prequest_rec;...); cdecl;

(*
 * Output data to the client in a printf format
 * @param r The current request
 * @param fmt The format string
 * @param vlist The arguments to use to fill out the format string
 * @return The number of bytes sent
 * @deffunc int ap_vrprintf(r: Prequest_rec; const char *fmt; va_list vlist)
 *)
function ap_vrprintf(r: Prequest_rec; const fmt: PChar; vlist: va_list): Integer;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_vrprintf}

(*
 * Flush all of the data for the current request to the client
 * @param r The current request
 * @return The number of bytes sent
 * @deffunc int ap_rflush(r: Prequest_rec)
 *)
function ap_rflush(r: Prequest_rec): Integer;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_rflush}
(*
 * Index used in custom_responses array for a specific error code
 * (only use outside protocol.c is in getting them configured).
 * @param status HTTP status code
 * @return The index of the response
 * @deffunc int ap_index_of_response(int status)
 *)
function  ap_index_of_response(status: Integer): Integer;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_index_of_response}
(*
 * Return the Status-Line for a given status code (excluding the
 * HTTP-Version field). If an invalid or unknown status code is
 * passed; "500 Internal Server Error" will be returned.
 * @param status The HTTP status code
 * @return The Status-Line
 * @deffunc const char *ap_get_status_line(int status)
 *)
function ap_get_status_line(status: Integer): PChar;
{$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_get_status_line}

(* Reading a block of data from the client connection (e.g.; POST arg) *)

(*
 * Setup the client to allow Apache to read the request body.
 * @param r The current request
 * @param read_policy How the server should interpret a chunked 
 *                    transfer-encoding.  One of: <pre>
 *    REQUEST_NO_BODY          Send 413 error if message has any body
 *    REQUEST_CHUNKED_ERROR    Send 411 error if body without Content-Length
 *    REQUEST_CHUNKED_DECHUNK  If chunked; remove the chunks for me.
 * </pre>
 * @return either OK or an error code
 * @deffunc int ap_setup_client_block(r: Prequest_rec; int read_policy)
 *)
function ap_setup_client_block(r: Prequest_rec; read_policy: Integer): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_setup_client_block}

(*
 * Determine if the client has sent any data.  This also sends a
 * 100 Continue response to HTTP/1.1 clients; so modules should not be called
 * until the module is ready to read content.
 * @warning Never call this function more than once.
 * @param r The current request
 * @return 0 if there is no message to read; 1 otherwise
 * @deffunc int ap_should_client_block(r: Prequest_rec)
 *)
function ap_should_client_block(r: Prequest_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_should_client_block}

(*
 * Call this in a loop.  It will put data into a buffer and return the length
 * of the input block
 * @param r The current request
 * @param buffer The buffer in which to store the data
 * @param bufsiz The size of the buffer
 * @return Number of bytes inserted into the buffer.  When done reading; 0
 *         if EOF; or -1 if there was an error
 * @deffunc long ap_get_client_block(r: Prequest_rec; char *buffer; apr_size_t bufsiz)
 *)
function ap_get_client_block(r: Prequest_rec; buffer: PChar; bufsiz: apr_size_t): LongInt;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_get_client_block}

(*
 * In HTTP/1.1; any method can have a body.  However; most GET handlers
 * wouldn't know what to do with a request body if they received one.
 * This helper routine tests for and reads any message body in the request;
 * simply discarding whatever it receives.  We need to do this because
 * failing to read the request body would cause it to be interpreted
 * as the next request on a persistent connection.
 * @param r The current request
 * @return error status if request is malformed; OK otherwise 
 * @deffunc int ap_discard_request_body(r: Prequest_rec)
 *)
function ap_discard_request_body(r: Prequest_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_discard_request_body}


(*
 * Setup the output headers so that the client knows how to authenticate
 * itself the next time; if an authentication request failed.  This function
 * works for both basic and digest authentication
 * @param r The current request
 * @deffunc void ap_note_auth_failure(r: Prequest_rec)
 *)
procedure ap_note_auth_failure(r: Prequest_rec);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_note_auth_failure}

(*
 * Setup the output headers so that the client knows how to authenticate
 * itself the next time; if an authentication request failed.  This function
 * works only for basic authentication
 * @param r The current request
 * @deffunc void ap_note_basic_auth_failure(r: Prequest_rec)
 *) 
procedure ap_note_basic_auth_failure(r: Prequest_rec);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_note_basic_auth_failure}

(*
 * Setup the output headers so that the client knows how to authenticate
 * itself the next time; if an authentication request failed.  This function
 * works only for digest authentication
 * @param r The current request
 * @deffunc void ap_note_digest_auth_failure(r: Prequest_rec)
 *)
procedure ap_note_digest_auth_failure(r: Prequest_rec);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_note_digest_auth_failure}

(*
 * Get the password from the request headers
 * @param r The current request
 * @param pw The password as set in the headers
 * @return 0 (OK) if it set the 'pw' argument (and assured
 *         a correct value in r->connection->user); otherwise it returns 
 *         an error code; either HTTP_INTERNAL_SERVER_ERROR if things are 
 *         really confused; HTTP_UNAUTHORIZED if no authentication at all 
 *         seemed to be in use; or DECLINED if there was authentication but 
 *         it wasn't Basic (in which case; the caller should presumably 
 *         decline as well).
 * @deffunc int ap_get_basic_auth_pw(r: Prequest_rec; const char **pw)
 *)
function ap_get_basic_auth_pw(r: Prequest_rec; const pw: PPChar): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_get_basic_auth_pw}

(*
 * Get the next line of input for the request
 * @param s The buffer into which to read the line
 * @param n The size of the buffer
 * @param r The request
 * @param fold Whether to merge continuation lines
 * @return The length of the line; if successful
 *         n; if the line is too big to fit in the buffer
 *         -1 for miscellaneous errors
 * @deffunc int ap_method_number_of(const char *method)
 *)
function ap_getline(s: PChar; n: integer; r: Prequest_rec; fold: Integer): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_getline}

(*
 * Get the method number associated with the given string; assumed to
 * contain an HTTP method.  Returns M_INVALID if not recognized.
 * @param method A string containing a valid HTTP method
 * @return The method number
 *)
function ap_method_number_of(const method: PChar): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_method_number_of}

(*
 * Get the method name associated with the given internal method
 * number.  Returns NULL if not recognized.
 * @param p A pool to use for temporary allocations.
 * @param methnum An integer value corresponding to an internal method number
 * @return The name corresponding to the method number
 *)
function ap_method_name_of(p: Papr_pool_t; methnum: Integer): PChar;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_method_name_of}

  
{$HPPEMIT '#include <http_config.h>'}
(* From http_config.h *)

(*
 * For modules which need to read config files, open logs, etc. this returns
 * the canonical form of fname made absolute to ap_server_root.
 * @param p pool to allocate data from
 * @param fname The file name
 *)
function ap_server_root_relative(p: Papr_pool_t; const fname: PChar): PChar;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_server_root_relative}

(* Hooks - From http_config.h*)

type
  ap_HOOK_header_parser_t = function(r: Prequest_rec): Integer; cdecl;
{$EXTERNALSYM ap_HOOK_header_parser_t}
procedure ap_hook_header_parser(pf: ap_HOOK_header_parser_t;
  const aszPre: PPChar; const aszSucc:
  PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_hook_header_parser}
function ap_run_header_parser(r: Prequest_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_run_header_parser}
function ap_hook_get_header_parser: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
{$EXTERNALSYM ap_hook_get_header_parser}
type
  ap_LINK_header_parser_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_header_parser_t;
    szName: PChar;
    aszPredecessors:
    PPChar; aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_header_parser_t}

type
  ap_HOOK_pre_config_t = function(pconf: Papr_pool_t; plog: Papr_pool_t;
    ptemp: Papr_pool_t): Integer; cdecl;
  {$EXTERNALSYM ap_HOOK_pre_config_t}
procedure ap_hook_pre_config(pf: ap_HOOK_pre_config_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_pre_config}
function ap_run_pre_config(pconf: Papr_pool_t; plog: Papr_pool_t; ptemp: Papr_pool_t): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_pre_config}
function ap_hook_get_pre_config: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_pre_config}
type
  ap_LINK_pre_config_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_pre_config_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_pre_config_t}

type
  ap_HOOK_post_config_t = function(pconf: Papr_pool_t; plog: Papr_pool_t;
    ptemp: Papr_pool_t; s: Pserver_rec): Integer; cdecl;
    {$EXTERNALSYM ap_HOOK_post_config_t}
procedure ap_hook_post_config(pf: ap_HOOK_post_config_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_post_config}
function ap_run_post_config(pconf: Papr_pool_t; plog: Papr_pool_t;
  ptemp: Papr_pool_t; s: Pserver_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_post_config}
function ap_hook_get_post_config: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_post_config}
type
  ap_LINK_post_config_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_post_config_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_post_config_t}
type
  ap_HOOK_open_logs_t = function(pconf: Papr_pool_t; plog: Papr_pool_t;
    ptemp: Papr_pool_t; s: Pserver_rec): Integer; cdecl;
    {$EXTERNALSYM ap_HOOK_open_logs_t}
procedure ap_hook_open_logs(pf: ap_HOOK_open_logs_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_open_logs}
function ap_run_open_logs(pconf: Papr_pool_t ;plog: Papr_pool_t; ptemp:
  Papr_pool_t; s: Pserver_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_open_logs}
function ap_hook_get_open_logs: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_open_logs}
type
  ap_LINK_open_logs_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_open_logs_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_open_logs_t}

type
  ap_HOOK_child_init_t = procedure(pchild: Papr_pool_t; s: Pserver_rec); cdecl;
  {$EXTERNALSYM ap_HOOK_child_init_t}
procedure ap_hook_child_init(pf: ap_HOOK_child_init_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_child_init}
procedure ap_run_child_init(pchild: Papr_pool_t; s: Pserver_rec);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_child_init}
function ap_hook_get_child_init: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_child_init}
type
  ap_LINK_child_init_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_child_init_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_child_init_t}

type
  ap_HOOK_handler_t = function(r: Prequest_rec): Integer; cdecl;
  {$EXTERNALSYM ap_HOOK_handler_t}
procedure ap_hook_handler(pf: ap_HOOK_handler_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_handler}
function ap_run_handler(r: Prequest_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_handler}
function ap_hook_get_handler: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_handler}
type
  ap_LINK_handler_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_handler_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_handler_t}

type
  ap_HOOK_quick_handler_t = function(r: Prequest_rec;
    lookup_uri: Integer): Integer; cdecl;
    {$EXTERNALSYM ap_HOOK_quick_handler_t}
procedure ap_hook_quick_handler(pf: ap_HOOK_quick_handler_t;
  const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_quick_handler}
function ap_run_quick_handler(r: Prequest_rec; lookup_uri: Integer): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_quick_handler}
function ap_hook_get_quick_handler: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_quick_handler}
type
  ap_LINK_quick_handler_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_quick_handler_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_quick_handler_t}

type
  ap_HOOK_optional_fn_retrieve_t = procedure; cdecl;
  {$EXTERNALSYM ap_HOOK_optional_fn_retrieve_t}
procedure ap_hook_optional_fn_retrieve(pf: ap_HOOK_optional_fn_retrieve_t;
  const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_optional_fn_retrieve}
procedure ap_run_optional_fn_retrieve;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_optional_fn_retrieve}
function ap_hook_get_optional_fn_retrieve: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_optional_fn_retrieve}
type
  ap_LINK_optional_fn_retrieve_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_optional_fn_retrieve_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_optional_fn_retrieve_t}

(* End from http_config.h *)


{$HPPEMIT '#include <http_connection.h>'}
(* Hooks - From http_connection.h*)

type
  ap_HOOK_create_connection_t = function(p: Papr_pool_t; server: Pserver_rec;
    csd: Papr_socket_t; conn_id: LongInt; sbh: Pointer;
    alloc: Papr_bucket_alloc_t): Pconn_rec; cdecl;
    {$EXTERNALSYM ap_HOOK_create_connection_t}
procedure ap_hook_create_connection(pf: ap_HOOK_create_connection_t;
  const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_create_connection}
function ap_run_create_connection(p: Papr_pool_t; server: Pserver_rec;
  csd: Papr_socket_t; conn_id: LongInt; sbh: Pointer;
  alloc: Papr_bucket_alloc_t): Pconn_rec;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_create_connection}
function ap_hook_get_create_connection: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_create_connection}
type
  ap_LINK_create_connection_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_create_connection_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_create_connection_t}

type
  ap_HOOK_pre_connection_t = function(c: Pconn_rec; csd: Pointer): Integer; cdecl;
  {$EXTERNALSYM ap_HOOK_pre_connection_t}
procedure ap_hook_pre_connection(pf: ap_HOOK_pre_connection_t;
  const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_pre_connection}
function ap_run_pre_connection(c: Pconn_rec; csd: Pointer): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_pre_connection}
function ap_hook_get_pre_connection: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_pre_connection}
type
  ap_LINK_pre_connection_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_pre_connection_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_pre_connection_t}

type
  ap_HOOK_process_connection_t = function(c: Pconn_rec): Integer; cdecl;
  {$EXTERNALSYM ap_HOOK_process_connection_t}
procedure ap_hook_process_connection(pf: ap_HOOK_process_connection_t;
  const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_process_connection}
function ap_run_process_connection(c: Pconn_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_process_connection}
function ap_hook_get_process_connection: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_process_connection}
type
  ap_LINK_process_connection_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_process_connection_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_process_connection_t}

{$HPPEMIT '#include <http_core.h>'}
(* Hooks from http_core.h *)
type
  ap_HOOK_get_mgmt_items_t = function(p: Papr_pool_t; const val: PChar;
    ht: Papr_hash_t): Integer; cdecl;
    {$EXTERNALSYM ap_HOOK_get_mgmt_items_t}
procedure ap_hook_get_mgmt_items(pf: ap_HOOK_get_mgmt_items_t;
  const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_mgmt_items}
function ap_run_get_mgmt_items(p: Papr_pool_t; const val: PChar; ht: Papr_hash_t): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_get_mgmt_items}
function ap_hook_get_get_mgmt_items: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_get_mgmt_items}
type
  ap_LINK_get_mgmt_items_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_get_mgmt_items_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_get_mgmt_items_t}
(* End hooks from http_core.h *)


{$HPPEMIT '#include <http_log.h>'}
(* Hooks from http_log.h *)
type
  ap_HOOK_error_log_t = procedure(const _file: PChar; line: Integer;
    level: Integer; status: apr_status_t; const s: Pserver_rec;
    const r: Prequest_rec; p: Papr_pool_t; const errstr: PChar); cdecl;
    {$EXTERNALSYM ap_HOOK_error_log_t}
procedure ap_hook_error_log(pf: ap_HOOK_error_log_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_error_log}
procedure ap_run_error_log(const _file: PChar; line: Integer; level: Integer;
  status: apr_status_t; const s: Pserver_rec; const r: Prequest_rec;
  p: Papr_pool_t; const errstr: PChar);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_error_log}
function ap_hook_get_error_log: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_error_log}
type
  ap_LINK_error_log_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_error_log_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_error_log_t}
(* End hooks from http_log.h *)

{$HPPEMIT '#include <http_protocol.h>'}
(* Hooks from http_protocol.h *)
type
  ap_HOOK_post_read_request_t = function(r: Prequest_rec): Integer; cdecl;
  {$EXTERNALSYM ap_HOOK_post_read_request_t}
procedure ap_hook_post_read_request(pf: ap_HOOK_post_read_request_t;
  const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_post_read_request}
function ap_run_post_read_request(r: Prequest_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_post_read_request}
function ap_hook_get_post_read_request: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_post_read_request}
type
  ap_LINK_post_read_request_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_post_read_request_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_post_read_request_t}

type
  ap_HOOK_log_transaction_t = function(r: Prequest_rec): Integer; cdecl;
  {$EXTERNALSYM ap_HOOK_log_transaction_t}
procedure ap_hook_log_transaction(pf: ap_HOOK_log_transaction_t;
  const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_log_transaction}
  function ap_run_log_transaction(r: Prequest_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_log_transaction}
function ap_hook_get_log_transaction: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_log_transaction}
type
  ap_LINK_log_transaction_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_log_transaction_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_log_transaction_t}

type
  ap_HOOK_http_method_t = function(const r: Prequest_rec): PChar; cdecl;
  {$EXTERNALSYM ap_HOOK_http_method_t}
procedure ap_hook_http_method(pf: ap_HOOK_http_method_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_http_method}
function ap_run_http_method(const r: Prequest_rec): PChar;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_http_method}
function ap_hook_get_http_method: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_http_method}
type
  ap_LINK_http_method_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_http_method_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_http_method_t}

type
  ap_HOOK_default_port_t = function(const r: Prequest_rec): apr_port_t; cdecl;
  {$EXTERNALSYM ap_HOOK_default_port_t}
procedure ap_hook_default_port(pf: ap_HOOK_default_port_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_default_port}
function ap_run_default_port(const r: Prequest_rec): apr_port_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_default_port}
function ap_hook_get_default_port: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_default_port}
type
  ap_LINK_default_port_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_default_port_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_default_port_t}
(* End hooks from http_protocol.h *)

{$HPPEMIT '#include <http_request.h>'}
(* Hooks from http_request.h *)
type
  ap_HOOK_create_request_t = function(r: Prequest_rec): Integer; cdecl;
  {$EXTERNALSYM ap_HOOK_create_request_t}
procedure ap_hook_create_request(pf: ap_HOOK_create_request_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_create_request}
function ap_run_create_request(r: Prequest_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_create_request}
function ap_hook_get_create_request: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_create_request}
type
  ap_LINK_create_request_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_create_request_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_create_request_t}
  
type
  ap_HOOK_translate_name_t = function(r: Prequest_rec): Integer; cdecl;
  {$EXTERNALSYM ap_HOOK_translate_name_t}
procedure ap_hook_translate_name(pf: ap_HOOK_translate_name_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_translate_name}
function ap_run_translate_name(r: Prequest_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_translate_name}
function ap_hook_get_translate_name: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_translate_name}
type
  ap_LINK_translate_name_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_translate_name_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_translate_name_t}
  
type
  ap_HOOK_map_to_storage_t = function(r: Prequest_rec): Integer; cdecl;
  {$EXTERNALSYM ap_HOOK_map_to_storage_t}
procedure ap_hook_map_to_storage(pf: ap_HOOK_map_to_storage_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_map_to_storage}
function ap_run_map_to_storage(r: Prequest_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_map_to_storage}
function ap_hook_get_map_to_storage: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_map_to_storage}
type
  ap_LINK_map_to_storage_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_map_to_storage_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_map_to_storage_t}
  
type
  ap_HOOK_check_user_id_t = function(r: Prequest_rec): Integer; cdecl;
  {$EXTERNALSYM ap_HOOK_check_user_id_t}
procedure ap_hook_check_user_id(pf: ap_HOOK_check_user_id_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_check_user_id}
function ap_run_check_user_id(r: Prequest_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_check_user_id}
function ap_hook_get_check_user_id: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_check_user_id}
type
  ap_LINK_check_user_id_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_check_user_id_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_check_user_id_t}
  
type
  ap_HOOK_fixups_t = function(r: Prequest_rec): Integer; cdecl;
  {$EXTERNALSYM ap_HOOK_fixups_t}
procedure ap_hook_fixups(pf: ap_HOOK_fixups_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_fixups}
function ap_run_fixups(r: Prequest_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_fixups}
function ap_hook_get_fixups: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_fixups}
type
  ap_LINK_fixups_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_fixups_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_fixups_t}
  
type
  ap_HOOK_type_checker_t = function(r: Prequest_rec): Integer; cdecl;
  {$EXTERNALSYM ap_HOOK_type_checker_t}
procedure ap_hook_type_checker(pf: ap_HOOK_type_checker_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_type_checker}
function ap_run_type_checker(r: Prequest_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_type_checker}
function ap_hook_get_type_checker: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_type_checker}
type
  ap_LINK_type_checker_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_type_checker_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_type_checker_t}
type
  ap_HOOK_access_checker_t = function(r: Prequest_rec): Integer; cdecl;
  {$EXTERNALSYM ap_HOOK_access_checker_t}
procedure ap_hook_access_checker(pf: ap_HOOK_access_checker_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_access_checker}
function ap_run_access_checker(r: Prequest_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_access_checker}
function ap_hook_get_access_checker: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_access_checker}
type
  ap_LINK_access_checker_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_access_checker_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_access_checker_t}
  
type
  ap_HOOK_auth_checker_t = function(r: Prequest_rec): Integer; cdecl;
  {$EXTERNALSYM ap_HOOK_auth_checker_t}
procedure ap_hook_auth_checker(pf: ap_HOOK_auth_checker_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_auth_checker}
function ap_run_auth_checker(r: Prequest_rec): Integer;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_auth_checker}
function ap_hook_get_auth_checker: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_auth_checker}
type
  ap_LINK_auth_checker_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_auth_checker_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_auth_checker_t}
  
type
  ap_HOOK_insert_filter_t = procedure(r: Prequest_rec); cdecl;
  {$EXTERNALSYM ap_HOOK_insert_filter_t}
procedure ap_hook_insert_filter(pf: ap_HOOK_insert_filter_t; const aszPre: PPChar;
  const aszSucc: PPChar; nOrder: Integer);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_insert_filter}
procedure ap_run_insert_filter(r: Prequest_rec);
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_run_insert_filter}
function ap_hook_get_insert_filter: Papr_array_header_t;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM ap_hook_get_insert_filter}
type
  ap_LINK_insert_filter_t = {$IFDEF LINUX}packed{$ENDIF} record
    pFunc: ap_HOOK_insert_filter_t;
    szName: PChar;
    aszPredecessors: PPChar;
    aszSuccessors: PPChar;
    nOrder: Integer;
  end;
  {$EXTERNALSYM ap_LINK_insert_filter_t}
(* End hooks from http_request.h *)


{$HPPEMIT '#include <apr_strings.h>'}
(* From apr_strings.h *)
function apr_pstrdup(p: Papr_pool_t; s: PChar): PChar;
  {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
  {$EXTERNALSYM apr_pstrdup}
(* End from apr_strings.h *)

implementation

const
{$IFDEF MSWINDOWS}
  LibNamePrefix = '_';       {do not localize}
  LibSuff0 = '@0';
  LibSuff4 = '@4';
  LibSuff8 = '@8';
  LibSuff12 = '@12';
  LibSuff16 = '@16';
  LibSuff24 = '@24';
  LibSuff32 = '@32';
{$ENDIF}

{$IFDEF LINUX}
  LibNamePrefix = '';
  LibSuff0 = '';
  LibSuff4 = '';
  LibSuff8 = '';
  LibSuff12 = '';
  LibSuff16 = '';
  LibSuff24 = '';
  LibSuff32 = '';  
{$ENDIF}

function ap_get_module_config(const cv: Pap_conf_vector_t;
  const m: Pmodule): Pointer;
begin
{ This is a translation of the basic C macro seen below: }
{ #define ap_get_module_config(v,m)	(((void **)(v))[(m)->module_index] }
  Result := Pointer(Integer(cv) + m^.module_index);
end;

procedure ap_set_module_config(cv: Pap_conf_vector_t; const m: Pmodule;
  val: Pointer);
var
  P: PPointer;
begin
{ This is a translation of the basic C macro seen below: }
{ #define ap_set_module_config(v,m,val)	((((void **)(v))[(m)->module_index]) = (val)) }
  P := PPointer(Integer(cv) + m^.module_index);
  P^ := val;
end;

function ap_set_string_slot; external LibHTTPD;
function ap_set_int_slot; external LibHTTPD;
function ap_method_is_limited; external LibHTTPD name LibNamePrefix + 'ap_method_is_limited' + LibSuff8;
function ap_set_string_slot_lower; external LibHTTPD;

// Table operations
function apr_array_make; external LibAPR name LibNamePrefix + 'apr_array_make' + LibSuff12;
function apr_array_push; external LibAPR name LibNamePrefix + 'apr_array_push' + LibSuff4;
procedure apr_array_cat; external LibAPR name LibNamePrefix + 'apr_array_cat' + LibSuff8;
function apr_array_copy; external LibAPR name LibNamePrefix + 'apr_array_copy' + LibSuff8;
function apr_array_copy_hdr; external LibAPR name LibNamePrefix + 'apr_array_copy_hdr' + LibSuff8;
function apr_array_append; external LibAPR name LibNamePrefix + 'apr_array_append' + LibSuff12;
function apr_array_pstrcat; external LibAPR name LibNamePrefix + 'apr_array_pstrcat' + LibSuff12;
function apr_table_make; external LibAPR name LibNamePrefix + 'apr_table_make' + LibSuff8;
function apr_table_copy; external LibAPR name LibNamePrefix + 'apr_table_copy' + LibSuff8;
procedure apr_table_clear; external LibAPR name LibNamePrefix + 'apr_table_clear' + LibSuff4;
function apr_table_get; external LibAPR name LibNamePrefix + 'apr_table_get' + LibSuff8;
procedure apr_table_set; external LibAPR name LibNamePrefix + 'apr_table_set' + LibSuff12;
procedure apr_table_setn; external LibAPR name LibNamePrefix + 'apr_table_setn' + LibSuff12;
procedure apr_table_unset; external LibAPR name LibNamePrefix + 'apr_table_unset' + LibSuff8;
procedure apr_table_merge; external LibAPR name LibNamePrefix + 'apr_table_merge' + LibSuff12;
procedure apr_table_mergen; external LibAPR name LibNamePrefix + 'apr_table_mergen' + LibSuff12;
procedure apr_table_add; external LibAPR name LibNamePrefix + 'apr_table_add' + LibSuff12;
procedure apr_table_addn; external LibAPR name LibNamePrefix + 'apr_table_addn' + LibSuff12;
function apr_table_overlay; external LibAPR name LibNamePrefix + 'apr_table_overlay' + LibSuff12;
procedure apr_table_vdo; external LibAPR name LibNamePrefix + 'apr_table_vdo' + LibSuff16;
procedure apr_table_overlap; external LibAPR name LibNamePrefix + 'apr_table_overlap' + LibSuff12;

// Core operations
function ap_allow_options; external LibHTTPD name LibNamePrefix + 'ap_allow_options' + LibSuff4;
function ap_allow_overrides; external LibHTTPD name LibNamePrefix + 'ap_allow_overrides' + LibSuff4;
function ap_default_type; external LibHTTPD name LibNamePrefix + 'ap_default_type' + LibSuff4;
function ap_document_root; external LibHTTPD name LibNamePrefix + 'ap_document_root' + LibSuff4;
function ap_get_remote_host; external LibHTTPD name LibNamePrefix + 'ap_get_remote_host' + LibSuff16;
function ap_get_remote_logname; external LibHTTPD name LibNamePrefix + 'ap_get_remote_logname' + LibSuff4;
function ap_construct_url; external LibHTTPD name LibNamePrefix + 'ap_construct_url' + LibSuff12;
function ap_get_server_name; external LibHTTPD name LibNamePrefix + 'ap_get_server_name' + LibSuff4;
function ap_get_server_port; external LibHTTPD name LibNamePrefix + 'ap_get_server_port' + LibSuff4;
function ap_get_server_version; external LibHTTPD name LibNamePrefix + 'ap_get_server_version' + LibSuff0;
function ap_get_limit_req_body; external LibHTTPD name LibNamePrefix + 'ap_get_limit_req_body' + LibSuff4;
function ap_get_limit_xml_body; external LibHTTPD name LibNamePrefix + 'ap_get_limit_xml_body' + LibSuff4;
procedure ap_custom_response; external LibHTTPD name LibNamePrefix + 'ap_custom_response' + LibSuff12;
function ap_exists_config_define; external LibHTTPD name LibNamePrefix + 'ap_exists_config_define' + LibSuff4;
function ap_core_translate; external LibHTTPD;
function ap_auth_type; external LibHTTPD name LibNamePrefix + 'ap_auth_type' + LibSuff4;
function ap_auth_name; external LibHTTPD name LibNamePrefix + 'ap_auth_name' + LibSuff4;
function ap_satisfies; external LibHTTPD name LibNamePrefix + 'ap_satisfies' + LibSuff4;
function ap_requires; external LibHTTPD name LibNamePrefix + 'ap_requires' + LibSuff4;

// Functions from http_config.h
function ap_server_root_relative; external LibHTTPD name LibNamePrefix + 'ap_server_root_relative' + LibSuff8;

// Functions from http_protocol.h
procedure ap_finalize_request_protocol; external LibHTTPD name LibNamePrefix + 'ap_finalize_request_protocol' + LibSuff4;
procedure ap_send_error_response; external LibHTTPD name LibNamePrefix + 'ap_send_error_response' + LibSuff8;
procedure ap_set_content_length; external LibHTTPD name LibNamePrefix + 'ap_set_content_length' + LibSuff12;
function ap_set_keepalive; external LibHTTPD name LibNamePrefix + 'ap_set_keepalive' + LibSuff4;
function ap_rationalize_mtime; external LibHTTPD name LibNamePrefix + 'ap_rationalize_mtime' + LibSuff12;
function ap_make_content_type; external LibHTTPD name LibNamePrefix + 'ap_make_content_type' + LibSuff8;
function ap_make_etag; external LibHTTPD name LibNamePrefix + 'ap_make_etag' + LibSuff8;
procedure ap_set_etag; external LibHTTPD name LibNamePrefix + 'ap_set_etag' + LibSuff4;
procedure ap_set_last_modified; external LibHTTPD name LibNamePrefix + 'ap_set_last_modified' + LibSuff4;
function ap_meets_conditions; external LibHTTPD name LibNamePrefix + 'ap_meets_conditions' + LibSuff4;
function ap_send_fd; external LibHTTPD name LibNamePrefix + 'ap_send_fd' + LibSuff24;
function ap_method_register; external LibHTTPD name LibNamePrefix + 'ap_method_register' + LibSuff8;
procedure ap_method_registry_init; external LibHTTPD name LibNamePrefix + 'ap_method_registry_init' + LibSuff4;
function ap_make_method_list; external LibHTTPD name LibNamePrefix + 'ap_make_method_list' + LibSuff8;
procedure ap_copy_method_list; external LibHTTPD name LibNamePrefix + 'ap_copy_method_list' + LibSuff8;
function ap_method_in_list; external LibHTTPD name LibNamePrefix + 'ap_method_in_list' + LibSuff8;
procedure ap_method_list_add; external LibHTTPD name LibNamePrefix + 'ap_method_list_add' + LibSuff8;
procedure ap_method_list_remove; external LibHTTPD name LibNamePrefix + 'ap_method_list_remove' + LibSuff8;
procedure ap_clear_method_list; external LibHTTPD name LibNamePrefix + 'ap_clear_method_list' + LibSuff4;
procedure ap_set_content_type; external LibHTTPD name LibNamePrefix + 'ap_set_content_type' + LibSuff8;
function ap_rputc; external LibHTTPD name LibNamePrefix + 'ap_rputc' + LibSuff8;
function ap_rputs; external LibHTTPD name LibNamePrefix + 'ap_rputs' + LibSuff8;
function ap_rwrite; external LibHTTPD name LibNamePrefix + 'ap_rwrite' + LibSuff12;
function ap_vrprintf; external LibHTTPD name LibNamePrefix + 'ap_vrprintf' + LibSuff12;
function ap_rflush; external LibHTTPD name LibNamePrefix + 'ap_rflush' + LibSuff4;
function ap_index_of_response; external LibHTTPD name LibNamePrefix + 'ap_index_of_response' + LibSuff4;
function ap_get_status_line; external LibHTTPD name LibNamePrefix + 'ap_get_status_line' + LibSuff4;
function ap_setup_client_block; external LibHTTPD name LibNamePrefix + 'ap_setup_client_block' + LibSuff8;
function ap_should_client_block; external LibHTTPD name LibNamePrefix + 'ap_should_client_block' + LibSuff4;
function ap_get_client_block; external LibHTTPD name LibNamePrefix + 'ap_get_client_block' + LibSuff12;
function ap_discard_request_body; external LibHTTPD name LibNamePrefix + 'ap_discard_request_body' + LibSuff4;
procedure ap_note_auth_failure; external LibHTTPD name LibNamePrefix + 'ap_note_auth_failure' + LibSuff4;
procedure ap_note_basic_auth_failure; external LibHTTPD name LibNamePrefix + 'ap_note_basic_auth_failure' + LibSuff4;
procedure ap_note_digest_auth_failure; external LibHTTPD name LibNamePrefix + 'ap_note_digest_auth_failure' + LibSuff4;
function ap_get_basic_auth_pw; external LibHTTPD name LibNamePrefix + 'ap_get_basic_auth_pw' + LibSuff8;
function ap_getline; external LibHTTPD name LibNamePrefix + 'ap_getline' + LibSuff16;
function ap_method_number_of; external LibHTTPD name LibNamePrefix + 'ap_method_number_of' + LibSuff4;
function ap_method_name_of; external LibHTTPD name LibNamePrefix + 'ap_method_name_of' + LibSuff8;

// From apr_strings.h
function apr_pstrdup; external LibAPR name LibNamePrefix + 'apr_pstrdup' + LibSuff8;

// Hooks from http_config.h
procedure ap_hook_header_parser; external LibHTTPD name LibNamePrefix + 'ap_hook_header_parser' + LibSuff16;
function ap_run_header_parser; external LibHTTPD name LibNamePrefix + 'ap_run_header_parser' + LibSuff4;
function ap_hook_get_header_parser; external LibHTTPD name LibNamePrefix + 'ap_hook_get_header_parser' + LibSuff0;
procedure ap_hook_pre_config; external LibHTTPD name LibNamePrefix + 'ap_hook_pre_config' + LibSuff16;
function ap_run_pre_config; external LibHTTPD name LibNamePrefix + 'ap_run_pre_config' + LibSuff12;
function ap_hook_get_pre_config; external LibHTTPD name LibNamePrefix + 'ap_hook_get_pre_config' + LibSuff0;
procedure ap_hook_post_config; external LibHTTPD name LibNamePrefix + 'ap_hook_post_config' + LibSuff16;
function ap_run_post_config; external LibHTTPD name LibNamePrefix + 'ap_run_post_config' + LibSuff16;
function ap_hook_get_post_config; external LibHTTPD name LibNamePrefix + 'ap_hook_get_post_config' + LibSuff0;
procedure ap_hook_open_logs; external LibHTTPD name LibNamePrefix + 'ap_hook_open_logs' + LibSuff16;
function ap_run_open_logs; external LibHTTPD name LibNamePrefix + 'ap_run_open_logs' + LibSuff16;
function ap_hook_get_open_logs; external LibHTTPD name LibNamePrefix + 'ap_hook_get_open_logs' + LibSuff0;
procedure ap_hook_child_init; external LibHTTPD name LibNamePrefix + 'ap_hook_child_init' + LibSuff16;
procedure ap_run_child_init; external LibHTTPD name LibNamePrefix + 'ap_run_child_init' + LibSuff8;
function ap_hook_get_child_init; external LibHTTPD name LibNamePrefix + 'ap_hook_get_child_init' + LibSuff0;
procedure ap_hook_handler; external LibHTTPD name LibNamePrefix + 'ap_hook_handler' + LibSuff16;
function ap_run_handler; external LibHTTPD name LibNamePrefix + 'ap_run_handler' + LibSuff4;
function ap_hook_get_handler; external LibHTTPD name LibNamePrefix + 'ap_hook_get_handler' + LibSuff0;
procedure ap_hook_quick_handler; external LibHTTPD name LibNamePrefix + 'ap_hook_quick_handler' + LibSuff16;
function ap_run_quick_handler; external LibHTTPD name LibNamePrefix + 'ap_run_quick_handler' + LibSuff8;
function ap_hook_get_quick_handler; external LibHTTPD name LibNamePrefix + 'ap_hook_get_quick_handler' + LibSuff0;
procedure ap_hook_optional_fn_retrieve; external LibHTTPD name LibNamePrefix + 'ap_hook_optional_fn_retrieve' + LibSuff16;
procedure ap_run_optional_fn_retrieve; external LibHTTPD name LibNamePrefix + 'ap_run_optional_fn_retrieve' + LibSuff0;
function ap_hook_get_optional_fn_retrieve; external LibHTTPD name LibNamePrefix + 'ap_hook_get_optional_fn_retrieve' + LibSuff0;

procedure ap_hook_create_connection; external LibHTTPD name LibNamePrefix + 'ap_hook_create_connection' + LibSuff16;
function ap_run_create_connection; external LibHTTPD name LibNamePrefix + 'ap_run_create_connection' + LibSuff24;
function ap_hook_get_create_connection; external LibHTTPD name LibNamePrefix + 'ap_hook_get_create_connection' + LibSuff0;
procedure ap_hook_pre_connection; external LibHTTPD name LibNamePrefix + 'ap_hook_pre_connection' + LibSuff16;
function ap_run_pre_connection; external LibHTTPD name LibNamePrefix + 'ap_run_pre_connection' + LibSuff8;
function ap_hook_get_pre_connection; external LibHTTPD name LibNamePrefix + 'ap_hook_get_pre_connection' + LibSuff0;
procedure ap_hook_process_connection; external LibHTTPD name LibNamePrefix + 'ap_hook_process_connection' + LibSuff16;
function ap_run_process_connection; external LibHTTPD name LibNamePrefix + 'ap_run_process_connection' + LibSuff4;
function ap_hook_get_process_connection; external LibHTTPD name LibNamePrefix + 'ap_hook_get_process_connection' + LibSuff0;
procedure ap_hook_get_mgmt_items; external LibHTTPD name LibNamePrefix + 'ap_hook_get_mgmt_items' + LibSuff16;
function ap_run_get_mgmt_items; external LibHTTPD name LibNamePrefix + 'ap_run_get_mgmt_items' + LibSuff12;
function ap_hook_get_get_mgmt_items; external LibHTTPD name LibNamePrefix + 'ap_hook_get_get_mgmt_items' + LibSuff0;
procedure ap_hook_error_log; external LibHTTPD name LibNamePrefix + 'ap_hook_error_log' + LibSuff16;
procedure ap_run_error_log; external LibHTTPD name LibNamePrefix + 'ap_run_error_log' + LibSuff32;
function ap_hook_get_error_log; external LibHTTPD name LibNamePrefix + 'ap_hook_get_error_log' + LibSuff0;
procedure ap_hook_post_read_request; external LibHTTPD name LibNamePrefix + 'ap_hook_post_read_request' + LibSuff16;
function ap_run_post_read_request; external LibHTTPD name LibNamePrefix + 'ap_run_post_read_request' + LibSuff4;
function ap_hook_get_post_read_request; external LibHTTPD name LibNamePrefix + 'ap_hook_get_post_read_request' + LibSuff0;
procedure ap_hook_log_transaction; external LibHTTPD name LibNamePrefix + 'ap_hook_log_transaction' + LibSuff16;
function ap_run_log_transaction; external LibHTTPD name LibNamePrefix + 'ap_run_log_transaction' + LibSuff4;
function ap_hook_get_log_transaction; external LibHTTPD name LibNamePrefix + 'ap_hook_get_log_transaction' + LibSuff0;
procedure ap_hook_http_method; external LibHTTPD name LibNamePrefix + 'ap_hook_http_method' + LibSuff16;
function ap_run_http_method; external LibHTTPD name LibNamePrefix + 'ap_run_http_method' + LibSuff4;
function ap_hook_get_http_method; external LibHTTPD name LibNamePrefix + 'ap_hook_get_http_method' + LibSuff0;
procedure ap_hook_default_port; external LibHTTPD name LibNamePrefix + 'ap_hook_default_port' + LibSuff16;
function ap_run_default_port; external LibHTTPD name LibNamePrefix + 'ap_run_default_port' + LibSuff4;
function ap_hook_get_default_port; external LibHTTPD name LibNamePrefix + 'ap_hook_get_default_port' + LibSuff0;
procedure ap_hook_create_request; external LibHTTPD name LibNamePrefix + 'ap_hook_create_request' + LibSuff16;
function ap_run_create_request; external LibHTTPD name LibNamePrefix + 'ap_run_create_request' + LibSuff4;
function ap_hook_get_create_request; external LibHTTPD name LibNamePrefix + 'ap_hook_get_create_request' + LibSuff0;
procedure ap_hook_translate_name; external LibHTTPD name LibNamePrefix + 'ap_hook_translate_name' + LibSuff16;
function ap_run_translate_name; external LibHTTPD name LibNamePrefix + 'ap_run_translate_name' + LibSuff4;
function ap_hook_get_translate_name; external LibHTTPD name LibNamePrefix + 'ap_hook_get_translate_name' + LibSuff0;
procedure ap_hook_map_to_storage; external LibHTTPD name LibNamePrefix + 'ap_hook_map_to_storage' + LibSuff16;
function ap_run_map_to_storage; external LibHTTPD name LibNamePrefix + 'ap_run_map_to_storage' + LibSuff4;
function ap_hook_get_map_to_storage; external LibHTTPD name LibNamePrefix + 'ap_hook_get_map_to_storage' + LibSuff0;
procedure ap_hook_check_user_id; external LibHTTPD name LibNamePrefix + 'ap_hook_check_user_id' + LibSuff16;
function ap_run_check_user_id; external LibHTTPD name LibNamePrefix + 'ap_run_check_user_id' + LibSuff4;
function ap_hook_get_check_user_id; external LibHTTPD name LibNamePrefix + 'ap_hook_get_check_user_id' + LibSuff0;
procedure ap_hook_fixups; external LibHTTPD name LibNamePrefix + 'ap_hook_fixups' + LibSuff16;
function ap_run_fixups; external LibHTTPD name LibNamePrefix + 'ap_run_fixups' + LibSuff4;
function ap_hook_get_fixups; external LibHTTPD name LibNamePrefix + 'ap_hook_get_fixups' + LibSuff0;
procedure ap_hook_type_checker; external LibHTTPD name LibNamePrefix + 'ap_hook_type_checker' + LibSuff16;
function ap_run_type_checker; external LibHTTPD name LibNamePrefix + 'ap_run_type_checker' + LibSuff4;
function ap_hook_get_type_checker; external LibHTTPD name LibNamePrefix + 'ap_hook_get_type_checker' + LibSuff0;
procedure ap_hook_access_checker; external LibHTTPD name LibNamePrefix + 'ap_hook_access_checker' + LibSuff16;
function ap_run_access_checker; external LibHTTPD name LibNamePrefix + 'ap_run_access_checker' + LibSuff4;
function ap_hook_get_access_checker; external LibHTTPD name LibNamePrefix + 'ap_hook_get_access_checker' + LibSuff0;
procedure ap_hook_auth_checker; external LibHTTPD name LibNamePrefix + 'ap_hook_auth_checker' + LibSuff16;
function ap_run_auth_checker; external LibHTTPD name LibNamePrefix + 'ap_run_auth_checker' + LibSuff4;
function ap_hook_get_auth_checker; external LibHTTPD name LibNamePrefix + 'ap_hook_get_auth_checker' + LibSuff0;
procedure ap_hook_insert_filter; external LibHTTPD name LibNamePrefix + 'ap_hook_insert_filter' + LibSuff16;
procedure ap_run_insert_filter; external LibHTTPD name LibNamePrefix + 'ap_run_insert_filter' + LibSuff4;
function ap_hook_get_insert_filter; external LibHTTPD name LibNamePrefix + 'ap_hook_get_insert_filter' + LibSuff0;

end.

