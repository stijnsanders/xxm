unit nsThreadUtils;

interface

uses
  nsXPCOM, nsTypes, nsGeckoStrings;

type
  nsIEnvironment = interface;
  nsIEventTarget = interface;
  nsIProcess = interface;
  nsIRunnable = interface;
  nsISupportsPriority = interface;
  nsIThread = interface;
  nsIThreadInternal = interface;
  nsIThreadObserver = interface;
  nsIThreadEventFilter = interface;
  nsIThreadManager = interface;
  nsIThreadPoolListener = interface;
  nsIThreadPool = interface;
  nsITimer = interface;

  nsIEnvironment = interface(nsISupports)
  ['{101d5941-d820-4e85-a266-9a3469940807}']
    procedure _set(aName: nsAString; aValue: nsAString); safecall;
    procedure get(aName: nsAString; aResult: nsAString); safecall;
    function exists(aName: nsAString): PRBool; safecall;
  end;

  nsIEventTarget = interface(nsISupports)
  ['{4e8febe4-6631-49dc-8ac9-308c1cb9b09c}']
    procedure dispatch(event: nsIRunnable; flags: PRUint32); safecall;
    function isOnCurrentThread(): PRBool; safecall;
  end;

  nsIProcess = interface(nsISupports)
  ['{9da0b650-d07e-4617-a18a-250035572ac8}']
    procedure init(aExecutable: nsIFile); safecall;
    procedure initWithPid(aPid: PRUint32); safecall;
    procedure kill(); safecall;
    function run(aBlocking: PRBool; aArgs: PAnsiCharArray; aCount: PRUint32): PRUint32; safecall;
    function getLocation(): nsIFile; safecall;
    property location: nsIFile read getLocation;
    function getPid(): PRUint32; safecall;
    property pid: PRUint32 read getPid;
    function getProcessName(): PAnsiChar; safecall;
    property processName: PAnsiChar read getProcessName;
    function getProcessSignature: PRUint32; safecall;
    property processSignature: PRUint32 read getProcessSignature;
    function getExitValue: PRInt32; safecall;
    property exitValue: PRInt32 read getExitValue;
  end;

  nsIRunnable = interface(nsISupports)
  ['{4a2abaf0-6886-11d3-9382-00104ba0fd40}']
    procedure run(); safecall;
  end;

  nsISupportsPriority = interface(nsISupports)
  ['{aa578b44-abd5-4c19-8b14-36d4de6fdc36}']
    function getPriority: PRInt32; safecall;
    procedure setPriority(aValue: PRInt32); safecall;
    property priority: PRInt32 read getPriority write setPriority;
    procedure adjustPriority(aDelta: PRInt32); safecall;
  end;

  nsIThread = interface(nsIEventTarget)
  ['{9c889946-a73a-4af3-ae9a-ea64f7d4e3ca}']
    function getPRThread: Pointer; safecall;
    property PRThread: Pointer read getPRThread;
    procedure shutdown(); safecall;
    function hasPendingEvents(): PRBool; safecall;
    function processNextEvent(mayWait: PRBool): PRBool; safecall;
  end;

  nsIThreadInternal = interface(nsISupports)
  ['{f89b5063-b06d-42f8-bf23-4dfcf2d80d6a}']
    function getObserver: nsIThreadObserver; safecall;
    procedure setObserver(aValue: nsIThreadObserver); safecall;
    property observer: nsIThreadObserver read getObserver write setObserver;
    procedure pushEventQueue(aFilter: nsIThreadEventFilter); safecall;
    procedure popEventQueue(); safecall;
  end;

  nsIThreadObserver = interface(nsISupports)
  ['{81D0B509-F198-4417-8020-08EB4271491F}']
    procedure onDispatchedEvent(aThread: nsIThreadInternal); safecall;
    procedure onProcessNextEvent(aThread: nsIThreadInternal; aMayWait: PRBool; aRecursionDepth: PRUint32); safecall;
    procedure afterProcessNextEvent(aThread: nsIThreadInternal; aRecursionDepth: PRUint32); safecall;
  end;

  nsIThreadEventFilter = interface(nsISupports)
  ['{a0605c0b-17f5-4681-b8cd-a1cd75d42559}']
    function acceptEvent(aEvent: nsIRunnable): PRBool; stdcall;
  end;

  nsIThreadManager = interface(nsISupports)
  ['{056216f5-8803-46b4-9199-d95bc1f0446f}']
    function newThread(creationFlag: PRUint32): nsIThread; safecall;
    function getThreadFromPRThread(prthread: Pointer): nsIThread; safecall;
    function getMainThread(): nsIThread; safecall;
    property mainThread: nsIThread read getMainThread;
    function getCurrentThread: nsIThread; safecall;
    property currentThread: nsIThread read getCurrentThread;
    function getIsMainThread: PRBool; safecall;
    property isMainThread: PRbool read getIsMainThread;
  end;

  nsIThreadPoolListener = interface(nsISupports)
  ['{ef194cab-3f86-4b61-b132-e5e96a79e5d1}']
    procedure onThreadCreated(); safecall;
    procedure onThreadShuttingDown(); safecall;
  end;

  nsIThreadPool = interface(nsIEventTarget)
  ['{394c29f0-225f-487f-86d3-4c259da76cab}']
    procedure shutdown(); safecall;
    function getThreadLimit(): PRUint32; safecall;
    procedure setThreadLimit(aValue: PRUint32); safecall;
    property threadLimit: PRUint32 read getThreadLimit write setThreadLimit;
    function getIdleThreadLimit(): PRUint32; safecall;
    procedure setIdleThreadLimit(aValue: PRUint32); safecall;
    property idleThreadLimit: PRUint32 read getIdleThreadLimit write setIdleThreadLimit;
    function getIdleThreadTimeout(): PRUint32; safecall;
    procedure setIdleThreadTimeout(aValue: PRUint32); safecall;
    property idleThreadTimeout: PRUint32 read getIdleThreadTimeout write setIdleThreadTimeout;
    function getListener: nsIThreadPoolListener; safecall;
    procedure setListener(aValue: nsIThreadPoolListener); safecall;
    property listener: nsIThreadPoolListener read getListener write setListener;
  end;

  nsITimer = interface(nsISupports)
  ['{a796816d-7d47-4348-9ab8-c7aeb3216a7d}']
    procedure notify(aTimer: nsITimer); safecall;
  end;

const
  NS_IEVENTTARGET_DISPATCH_NORMAL = 0;
  NS_IEVENTTARGET_DISPATCH_SYNC = 1;

  NS_ISUPPORTSPRIORITY_PRIORITY_HIGHEST = -20;
  NS_ISUPPORTSPRIORITY_PRIORITY_HIGH = -10;
  NS_ISUPPORTSPRIORITY_PRIORITY_NORMAL = 0;
  NS_ISUPPORTSPRIORITY_PRIORITY_LOW = 10;
  NS_ISUPPORTSPRIORITY_PRIORITY_LOWEST = 20;

const
  NS_THREADMANAGER_CONTRACTID = '@mozilla.org/thread-manager;1';

  NS_DISPATCH_NORMAL = NS_IEVENTTARGET_DISPATCH_NORMAL;
  NS_DISPATCH_SYNC = NS_IEVENTTARGET_DISPATCH_SYNC;

type
  nsTimerCallbackFunc = procedure (aTimer: nsITimer; aClosure: Pointer); cdecl;

function NS_NewThread(aInitialEvent: nsIRunnable = nil): nsIThread;
function NS_GetCurrentThread(): nsIThread;
function NS_GetMainThread(): nsIThread;
function NS_IsMainThread(): PRBool;
procedure NS_DispatchToCurrentThread(aEvent: nsIRunnable);
procedure NS_DispatchToMainThread(aEvent: nsIRunnable;
                aDispatchFlags: PRUint32 = NS_DISPATCH_NORMAL);
procedure NS_ProcessPendingEvents(aThread: nsIThread;
                aTimeout: PRUint32 = $ffffffff);
// const PR_INTERVAL_NO_TIMEOUT = $ffffffff;
function NS_HasPendingEvents(aThread: nsIThread = nil): PRBool;
function NS_ProcessNextEvent(aThread: nsIThread = nil;
                aMayWait: PRBool = True): PRBool;

implementation

uses
  nsXPCOMGlue, nsError, mmsystem;

function NS_NewThread(aInitialEvent: nsIRunnable): nsIThread;
var
  tm: nsIThreadManager;
  thread: nsIThread;
begin
  NS_GetService(NS_THREADMANAGER_CONTRACTID, nsIThreadManager, tm);
  thread := tm.newThread(0);
  if Assigned(aInitialEvent) then
    thread.dispatch(aInitialEvent, NS_DISPATCH_NORMAL);
  Result := thread;
end;

function NS_GetCurrentThread(): nsIThread;
var
  tm: nsIThreadManager;
begin
  NS_GetService(NS_THREADMANAGER_CONTRACTID, nsIThreadManager, tm);
  Result := tm.currentThread;
end;

function NS_GetMainThread(): nsIThread;
var
  tm: nsIThreadManager;
begin
  NS_GetService(NS_THREADMANAGER_CONTRACTID, nsIThreadManager, tm);
  Result := tm.mainThread;
end;

function NS_IsMainThread(): PRBool;
var
  tm: nsIThreadManager;
begin
  NS_GetService(NS_THREADMANAGER_CONTRACTID, nsIThreadManager, tm);
  Result := tm.isMainThread;
end;

procedure NS_DispatchToCurrentThread(aEvent: nsIRunnable);
var
  tm: nsIThreadManager;
  thread: nsIThread;
begin
  NS_GetService(NS_THREADMANAGER_CONTRACTID, nsIThreadManager, tm);
  thread := tm.currentThread;
  thread.dispatch(aEvent, NS_DISPATCH_NORMAL);
end;

procedure NS_DispatchToMainThread(aEvent: nsIRunnable;
                aDispatchFlags: PRUint32);
var
  tm: nsIThreadManager;
  thread: nsIThread;
begin
  NS_GetService(NS_THREADMANAGER_CONTRACTID, nsIThreadManager, tm);
  thread := tm.mainThread;
  thread.dispatch(aEvent, aDispatchFlags);
end;

procedure NS_ProcessPendingEvents(aThread: nsIThread;
                aTimeout: PRUint32);
var
  start: PRUint32;
  processedEvent: PRBool;
begin
  if not Assigned(aThread) then
    aThread := NS_GetCurrentThread;
  start := timeGetTime();
  while True do
  begin
    processedEvent := aThread.processNextEvent(False);
    if not ProcessedEvent then
      Break;
    if (timeGetTime()-start)>aTimeout then
      Break;
  end;
end;

function NS_HasPendingEvents(aThread: nsIThread): PRBool;
begin
  if not Assigned(aThread) then
    aThread := NS_GetCurrentThread;
  result := aThread.hasPendingEvents;
end;

function NS_ProcessNextEvent(aThread: nsIThread; aMayWait: PRBool): PRBool;
begin
  if not Assigned(aThread) then
    aThread := NS_GetCurrentThread;
  result := aThread.processNextEvent(aMayWait);
end;

end.
