unit xxmWebSocket;

{

  xxm WebSocket support

TXxmWebSocket is a base class to provide WebSocket support.
Inherit from TXxmWebSocket and override ReceiveText (or ReceiveBinary)
to handle incoming messages, send messages with SendText (or SendBinary).
It implements IXxmPage, so you can call xxmFReg's
  XxmFragmentRegistry.RegisterClass
with your inheriting class (or any alternative fragment registry you use)
to determine which URL the WebSocket will be available with.

  $Rev: 462 $ $Date: 2021-02-26 16:01:20 +0100 (vr, 26 feb 2021) $

}
{$D-}
{$L-}

interface

uses SysUtils, xxm;

type
  TXxmWebSocket=class(TXxmPage, IXxmRawSocket)
  private
    FMaxFragmentSize:int64;
    FSocket:IXxmRawSocket;
    FSockSus:IXxmSocketSuspend;
    FIncomplete:byte;//TWebSocketBuilding;
    FIncompleteData:UTF8String;
    procedure SendRaw(Op:byte;const Data:UTF8String);
  protected
    procedure ConnectSuccess; virtual;
    procedure ConnectionLost; virtual;
    procedure ReceiveText(const Data:UTF8String); virtual;
    procedure ReceiveBinary(const Data:UTF8String); virtual;
    procedure Disconnect;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    //IXxmPage
    procedure Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values: array of OleVariant; const Objects: array of TObject); override;

    //IXxmRawSocket
    function Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult;
      stdcall;
    function Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult;
      stdcall;
    function DataReady(TimeoutMS: cardinal): boolean;
    procedure ClosingSocket;
    procedure IXxmRawSocket.Disconnect=ClosingSocket;
    
    //
    procedure SendText(const Data:UTF8String); virtual;
    procedure SendBinary(const Data:UTF8String); virtual;

    property MaxFragmentSize:int64 read FMaxFragmentSize write FMaxFragmentSize;
  end;

  EWebSocketError=class(Exception);
  
implementation

uses xxmHeaders, ComObj, ActiveX;

const
  Base64Codes:array[0..63] of char=
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

function base64encode(var Data;DataSize:cardinal):string;
type
  PByte=^byte;
var
  i:integer;
  d1,d2,d3:byte;
  p,pl:PByte;
begin
  i:=(DataSize div 3);
  if (DataSize mod 3)<>0 then inc(i);
  SetLength(Result,i*4);//+(DataSize div 57)*2);
  p:=@Data;
  pl:=p;inc(pl,DataSize);
  i:=0;
  while cardinal(p)<cardinal(pl) do
   begin
    d1:=p^;inc(p);
    if p=pl then
     begin
      inc(i);Result[i]:=Base64Codes[  d1 shr  2];
      inc(i);Result[i]:=Base64Codes[((d1 and $3) shl 4)];
      inc(i);Result[i]:='=';
      inc(i);Result[i]:='=';
     end
    else
     begin
      d2:=p^;inc(p);
      if p=pl then
       begin
        inc(i);Result[i]:=Base64Codes[  d1 shr  2];
        inc(i);Result[i]:=Base64Codes[((d1 and $3) shl 4) or (d2 shr 4)];
        inc(i);Result[i]:=Base64Codes[((d2 and $F) shl 2)];
        inc(i);Result[i]:='=';
       end
      else
       begin
        d3:=p^;inc(p);
        inc(i);Result[i]:=Base64Codes[  d1 shr  2];
        inc(i);Result[i]:=Base64Codes[((d1 and $3) shl 4) or (d2 shr 4)];
        inc(i);Result[i]:=Base64Codes[((d2 and $F) shl 2) or (d3 shr 6)];
        inc(i);Result[i]:=Base64Codes[  d3 and $3F];
       end;
      //if ((cardinal(p)-cardinal(@Data)) mod 57)=0 then Result:=Result+#13#10;
     end;
   end;
end;

type
  TSHA1Hash=array[0..4] of cardinal;

{
function SwapEndian(Value: integer): integer; register; overload;
asm
  bswap eax
end;
}

function SwapEndian(Value: integer): integer; overload;
var
  x:array[0..3] of byte absolute Result;
  y:array[0..3] of byte absolute Value;
begin
  x[0]:=y[3];
  x[1]:=y[2];
  x[2]:=y[1];
  x[3]:=y[0];
end;

function SHA1Hash(x:UTF8String):TSHA1Hash;
const
  hex:array[0..15] of AnsiChar='0123456789abcdef';
var
  a:cardinal;
  dl,i,j:integer;
  d:array of cardinal;
  e:array[0..79] of cardinal;
  g,h:TSHA1Hash;
begin
  //based on http://www.ietf.org/rfc/rfc3174.txt
  a:=Length(x);
  dl:=a+9;
  if (dl and $3F)<>0 then dl:=(dl and $FFC0)+$40;
  i:=dl;
  dl:=dl shr 2;
  SetLength(d,dl);
  SetLength(x,i);
  j:=a+1;
  x[j]:=#$80;
  while j<i do
   begin
    inc(j);
    x[j]:=#0;
   end;
  Move(x[1],d[0],i);
  d[dl-1]:=SwapEndian(a shl 3);
  h[0]:=$67452301;
  h[1]:=$efcdab89;
  h[2]:=$98badcfe;
  h[3]:=$10325476;
  h[4]:=$c3d2e1f0;
  i:=0;
  while i<dl do
   begin
    j:=0;
    while j<16 do
     begin
      e[j]:=SwapEndian(d[i]);
      inc(i);
      inc(j);
     end;
    while j<80 do
     begin
      a:=e[j-3] xor e[j-8] xor e[j-14] xor e[j-16];
      e[j]:=((a shl 1) or (a shr 31));
      inc(j);
     end;
    g:=h;
    j:=0;
    while j<20 do
     begin
      a:=((g[0] shl 5) or (g[0] shr 27))+
        ((g[1] and g[2]) or (not(g[1]) and g[3]))+
        g[4]+e[j]+$5a827999;
      g[4]:=g[3];
      g[3]:=g[2];
      g[2]:=((g[1] shl 30) or (g[1] shr 2));
      g[1]:=g[0];
      g[0]:=a;
      inc(j);
     end;
    while j<40 do
     begin
      a:=((g[0] shl 5) or (g[0] shr 27))+
        (g[1] xor g[2] xor g[3])+
        g[4]+e[j]+$6ed9eba1;
      g[4]:=g[3];
      g[3]:=g[2];
      g[2]:=((g[1] shl 30) or (g[1] shr 2));
      g[1]:=g[0];
      g[0]:=a;
      inc(j);
     end;
    while j<60 do
     begin
      a:=((g[0] shl 5) or (g[0] shr 27))+
        ((g[1] and g[2]) or (g[1] and g[3]) or (g[2] and g[3]))+
        g[4]+e[j]+$8f1bbcdc;
      g[4]:=g[3];
      g[3]:=g[2];
      g[2]:=((g[1] shl 30) or (g[1] shr 2));
      g[1]:=g[0];
      g[0]:=a;
      inc(j);
     end;
    while j<80 do
     begin
      a:=((g[0] shl 5) or (g[0] shr 27))+
        (g[1] xor g[2] xor g[3])+
        g[4]+e[j]+$ca62c1d6;
      g[4]:=g[3];
      g[3]:=g[2];
      g[2]:=((g[1] shl 30) or (g[1] shr 2));
      g[1]:=g[0];
      g[0]:=a;
      inc(j);
     end;
    for j:=0 to 4 do inc(h[j],g[j]);
   end;
  for j:=0 to 4 do Result[j]:=SwapEndian(h[j]);
end;

const
  //TWebSocketBuilding=(
  wbNone = 0;
  wbText = 1;
  wbBinary = 2;

type
  TMaskingKey=array[0..3] of byte;
  PMaskingKey=^TMaskingKey;

const
  Frame_Op_Continuation = $0;
  Frame_Op_Text         = $1;
  Frame_Op_Binary       = $2;
  //3..7: reserved non-control
  Frame_Op_Close        = $7;
  Frame_Op_Ping         = $9;
  Frame_Op_Pong         = $A;
  //B..F: reserved control

{ TXxmWebSocket }

procedure TXxmWebSocket.AfterConstruction;
begin
  inherited;
  FMaxFragmentSize:=$10000;//default
  FSocket:=nil;
  FSockSus:=nil;
end;

procedure TXxmWebSocket.BeforeDestruction;
begin
  inherited;
  FSocket:=nil;//try except silent?
  FSockSus:=nil;
end;

procedure TXxmWebSocket.Build(const Context: IXxmContext;
  const Caller: IXxmFragment; const Values: array of OleVariant;
  const Objects: array of TObject);
var
  hReq,hRes:IxxmDictionaryEx;
  h:TSHA1Hash;
begin
  //inherited;

  //http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-17
  
  hReq:=(Context as IxxmHttpHeaders).RequestHeaders;
  hRes:=(Context as IxxmHttpHeaders).ResponseHeaders;
  if (CompareText(hReq['Connection'],'upgrade')=0)
    and (CompareText(hReq['Upgrade'],'websocket')=0) then
   begin
    //TODO: check HTTP 1.1 or more
    //TODO: check hReq['Origin']
    //TODO: check hReq['Sec-WebSocket-Protocol']
    //TODO: check hReq['Sec-WebSocket-Version']='13'
    //TODO: hReq['Sec-WebSocket-Extensions']?
    //TODO: hReq['Authorization']?
    Context.SetStatus(101,'Switching Protocols');
    hRes['Connection']:='Upgrade';
    hRes['Upgrade']:='websocket';
    h:=SHA1Hash(hReq['Sec-WebSocket-Key']
      +'258EAFA5-E914-47DA-95CA-C5AB0DC85B11');
    hRes['Sec-WebSocket-Accept']:=base64encode(h,20);

    //hRes['Sec-WebSocket-Protocol']:='chat';

    FSocket:=Context.PostData as IXxmRawSocket;
    if FSocket=nil then
      raise EWebSocketError.Create('xxm handler doesn''t support RawSocket');

    ConnectSuccess;
    FIncomplete:=wbNone;
    FIncompleteData:='';
    //try

    if Context.QueryInterface(IXxmSocketSuspend,FSockSus)=S_OK then
      FSockSus.SuspendSocket(Self)
    else
     begin       
      while Context.Connected do DataReady(0);
      ClosingSocket;
     end;

    //except? //EWebSocketError?

   end
  else
   begin
    //Context.Redirect?
    //Context.SetStatus(400,'Bad Request');?
    raise EWebSocketError.Create('Unexpected: not upgrading to websocket');
   end;
end;

function TXxmWebSocket.Read(pv: Pointer; cb: Integer;
  pcbRead: PLongint): HResult;
begin
  //IXxmRawSocket on TXxmWebSocket for suspend/resume only
  Result:=E_NOTIMPL;
end;

function TXxmWebSocket.Write(pv: Pointer; cb: Integer;
  pcbWritten: PLongint): HResult;
begin
  //IXxmRawSocket on TXxmWebSocket for suspend/resume only
  Result:=E_NOTIMPL;
end;

procedure TXxmWebSocket.Disconnect;
begin
  if FSocket<>nil then FSocket.Disconnect;
end;

function TXxmWebSocket.DataReady(TimeoutMS: cardinal): boolean;
var
  Frame:array of byte;
  FrameType:byte;
  FrameIndex:integer;
  FrameLength:integer;
  i,k:integer;
  Frame_Payload_Length:int64;
  Frame_Masked:boolean;
  Frame_Masking_Key:TMaskingKey;
begin
  SetLength(Frame,$10000);
  while FSocket.DataReady(50) do
   begin
    OleCheck(FSocket.Read(@Frame[0],$10000,@FrameLength));
    FrameIndex:=0;
    while FrameIndex<FrameLength do
     begin
      FrameType:=Frame[FrameIndex];//FIN1:RSV3:OP4
      inc(FrameIndex);
      Frame_Masked:=(Frame[FrameIndex] and $80)<>0;
      Frame_Payload_Length:=Frame[FrameIndex] and $7F;
      inc(FrameIndex);
      if Frame_Payload_Length=126 then
       begin
        Frame_Payload_Length:=Frame[FrameIndex] shl 8;
        inc(FrameIndex);
        Frame_Payload_Length:=Frame_Payload_Length or Frame[FrameIndex];
        inc(FrameIndex);
       end
      else
        if Frame_Payload_Length=127 then
         begin
          Frame_Payload_Length:=0;
          i:=8;
          while i<>0 do
           begin
            Frame_Payload_Length:=Frame_Payload_Length shl 8 or Frame[FrameIndex];
            inc(FrameIndex);
            dec(i);
           end;
         end;

      if Frame_Masked then
       begin
        Frame_Masking_Key:=PMaskingKey(@Frame[FrameIndex])^;
        inc(FrameIndex,4);

        i:=FrameIndex;
        k:=FrameIndex+Frame_Payload_Length;//assert<=FrameLength;
        while i<k do
         begin
          Frame[i]:=Frame[i] xor Frame_Masking_Key[(i-FrameIndex) mod 4];
          inc(i);
         end;

       end;

      if (FrameType and $70)<>0 then //RSV1,RSV2,RSV3
       begin
        FSocket.Disconnect;
        raise EWebSocketError.Create('Unexpected reserved flags');
       end;
      //if not Frame_Masked then?

      if (FrameType and $80)=0 then //FIN?
        case FrameType and $F of
          Frame_Op_Continuation:
           begin
            if FIncomplete=wbNone then
              raise EWebSocketError.Create('Unexpected continuation');
            i:=Length(FIncompleteData);
            SetLength(FIncompleteData,i+Frame_Payload_Length);
            Move(Frame[FrameIndex],FIncompleteData[i+1],Frame_Payload_Length);
           end;
          //TODO: support interleaved frames?
          Frame_Op_Text:
           begin
            if FIncomplete<>wbNone then
              raise EWebSocketError.Create('Unexpected interrupted sequence');
            FIncomplete:=wbText;
            SetLength(FIncompleteData,Frame_Payload_Length);
            Move(Frame[FrameIndex],FIncompleteData[1],Frame_Payload_Length);
           end;
          Frame_Op_Binary:
           begin
            if FIncomplete<>wbNone then
              raise EWebSocketError.Create('Unexpected interrupted sequence');
            FIncomplete:=wbBinary;
            SetLength(FIncompleteData,Frame_Payload_Length);
            Move(Frame[FrameIndex],FIncompleteData[1],Frame_Payload_Length);
           end;
          else
            raise EWebSocketError.Create('Unexpected continuation');
        end
      else
        case FrameType and $F of
          Frame_Op_Continuation:
           begin
            i:=Length(FIncompleteData);
            SetLength(FIncompleteData,i+Frame_Payload_Length);
            Move(Frame[FrameIndex],FIncompleteData[i+1],Frame_Payload_Length);
            case FIncomplete of
              wbNone:raise EWebSocketError.Create('Unexpected continuation');
              wbText:ReceiveText(FIncompleteData);
              wbBinary:ReceiveBinary(FIncompleteData);
            end;
            FIncomplete:=wbNone;
            FIncompleteData:='';
           end;
          Frame_Op_Text:
           begin
            //if Incomplete<>wbNone then?
            SetLength(FIncompleteData,Frame_Payload_Length);
            Move(Frame[FrameIndex],FIncompleteData[1],Frame_Payload_Length);
            ReceiveText(FIncompleteData);
           end;
          Frame_Op_Binary:
           begin
            //if Incomplete<>wbNone then?
            SetLength(FIncompleteData,Frame_Payload_Length);
            Move(Frame[FrameIndex],FIncompleteData[1],Frame_Payload_Length);
            ReceiveBinary(FIncompleteData);
           end;
          Frame_Op_Close:
           begin
            SetLength(FIncompleteData,Frame_Payload_Length);
            Move(Frame[FrameIndex],FIncompleteData[1],Frame_Payload_Length);
            SendRaw(Frame_Op_Close,FIncompleteData);
            //flush?
            FSocket.Disconnect;
            raise EWebSocketError.Create('Close frame received');
           end;
          Frame_Op_Ping:
           begin
            SetLength(FIncompleteData,Frame_Payload_Length);
            Move(Frame[FrameIndex],FIncompleteData[1],Frame_Payload_Length);
            SendRaw(Frame_Op_Pong,FIncompleteData);
           end;
          Frame_Op_Pong:
           begin
            //TODO: reset ping timeout
           end;
        end;
      //next frame if any
      inc(FrameIndex,Frame_Payload_Length);
     end;
   end;
  //TODO: timer to send ping frames

  if (FSockSus<>nil) then //and FSocket.Connected do
    FSockSus.SuspendSocket(Self);

  Result:=true;
end;

procedure TXxmWebSocket.ClosingSocket;
begin
  FSocket:=nil;//try except silent?
  FSockSus:=nil;
  ConnectionLost;
end;

procedure TXxmWebSocket.ConnectSuccess;
begin
  //inheritants handle new connections here
end;

procedure TXxmWebSocket.ConnectionLost;
begin
  //inheritants handle connection loss here
end;

procedure TXxmWebSocket.ReceiveText(const Data: UTF8String);
begin
  //inheritants handle messages here
end;

procedure TXxmWebSocket.ReceiveBinary(const Data: UTF8String);
begin
  //inheritants handle messages here
end;

procedure TXxmWebSocket.SendText(const Data: UTF8String);
begin
  SendRaw(Frame_Op_Text,Data);
end;

procedure TXxmWebSocket.SendBinary(const Data: UTF8String);
begin
  SendRaw(Frame_Op_Binary,Data);
end;

{
function NewMaskingKey:TMaskingKey;
begin
  //see also Randomize in xxmp.pas XxmProjectLoad
  Result[0]:=Trunc(Random*$100);
  Result[1]:=Trunc(Random*$100);
  Result[2]:=Trunc(Random*$100);
  Result[3]:=Trunc(Random*$100);
end;
}

procedure TXxmWebSocket.SendRaw(Op: byte; const Data: UTF8String);
var
  Frame:array of byte;
  DataSize,FrameSize,PayloadSize,i,j,k:integer;
  //Frame_Masking_Key:TMaskingKey;
begin
  //TODO: locking: make this thread safe
  //TODO: support MaxFragmentSize > 2^32 (insane!)
  if FSocket=nil then
    raise EWebSocketError.Create('WebSocket not currently active.');
  DataSize:=Length(Data);
  if DataSize>FMaxFragmentSize then
    PayloadSize:=FMaxFragmentSize
  else
    PayloadSize:=DataSize;
  if PayloadSize>126 then
    if PayloadSize>$FFFF then
      j:=10
    else
      j:=4
  else
    j:=2;
  FrameSize:=PayloadSize+j;
  //if Masked then inc(FrameSize,4);
  SetLength(Frame,FrameSize);//TODO: recycle buffers
  Frame[0]:=Op;
  if PayloadSize>126 then
   begin
    if PayloadSize>$FFFF then
      Frame[1]:=127
    else
      Frame[1]:=126;
    //j:=//see above!
    k:=PayloadSize;
    while j<>2 do
     begin
      dec(j);
      Frame[j]:=k and $FF;
      k:=k shr 8;
     end;
   end
  else
    Frame[1]:=PayloadSize;
  //if Masked then Frame[1]:=Frame[1] or $80;
  i:=0;
  while (i<DataSize) do
   begin
    if DataSize-i>PayloadSize then
     begin
      //last frame: update payload size
      PayloadSize:=DataSize-i;
      if PayloadSize>126 then
       begin
        if PayloadSize>$FFFF then
         begin
          j:=10;
          Frame[1]:=127;
         end
        else
         begin
          j:=4;
          Frame[1]:=126;
         end;
        FrameSize:=PayloadSize+j;
        SetLength(Frame,FrameSize);//TODO: recycle buffers
        k:=PayloadSize;
        while j<>2 do
         begin
          dec(j);
          Frame[j]:=k and $FF;
          k:=k shr 8;
         end;
       end
      else
       begin
        FrameSize:=PayloadSize+2;
        SetLength(Frame,FrameSize);//TODO: recycle buffers
        Frame[1]:=PayloadSize;
       end;
      //if Masked then inc(FrameSize,4);
      //if Masked then Frame[1]:=Frame[1] or $80;
     end;
    if DataSize-i=PayloadSize then Frame[0]:=Frame[0] or $80;//FIN
    {
    if Masked then
     begin
      Frame_Masking_Key:=NewMaskingKey;
      PMaskingKey(@Frame[PayloadIndex-4])^:=Frame_Masking_Key;
      j:=0;
      while j<PayloadSize do
       begin
        Frame[PayloadIndex+j]:=byte(Data[i+1+j]) xor Frame_Masking_Key[j mod 4];
        inc(j);
       end;
     end
    else
    }
    Move(Data[i+1],Frame[FrameSize-PayloadSize],PayloadSize);
    if FSocket.Write(@Frame[0],FrameSize,@j)<>S_OK then j:=0;
    if j<>FrameSize then
      raise EWebSocketError.Create('Transfer error');
    if i=0 then Frame[0]:=Frame_Op_Continuation;
    inc(i,PayloadSize);
   end;
end;

initialization
  IsMultiThread:=true;
end.
