unit xxmProtoInclude;

interface

uses xxm;

type
  TXxmProtoInclude=class(TXxmInclude)
  public
    procedure Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values:array of OleVariant;
      const Objects: array of TObject); override;
  end;

  TXxmProtoInclude2=class(TXxmInclude)
  public
    procedure Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values:array of OleVariant;
      const Objects: array of TObject); override;
  end;

implementation

uses xxmFReg, SysUtils;

{ TXxmProtoInclude }

procedure TXxmProtoInclude.Build(const Context: IXxmContext;
  const Caller: IXxmFragment; const Values:array of OleVariant;
  const Objects: array of TObject);
begin
  inherited;
  Context.SendHTML('<html><head><title>xxmProto</title></head><body>');
end;

{ TXxmProtoInclude2 }

procedure TXxmProtoInclude2.Build(const Context: IXxmContext;
  const Caller: IXxmFragment; const Values:array of OleVariant;
  const Objects: array of TObject);
begin
  inherited;
  Context.SendHTML('<p style="text-align: right; font-size: 0.5em;">');
  Context.Send(DateTimeToStr(Now));
  Context.SendHTML('</p></body></html>');
end;

initialization
  XxmFragmentRegistry.RegisterClass('head.xxmi',TXxmProtoInclude);
  XxmFragmentRegistry.RegisterClass('foot.xxmi',TXxmProtoInclude2);

end.
