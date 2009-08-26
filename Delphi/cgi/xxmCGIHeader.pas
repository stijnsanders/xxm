unit xxmCGIHeader;

interface

type
  TxxmCGIHeader=packed record
    Size:cardinal;
    ServerProcessID:cardinal;
    PipeRequest:THandle;
    PipeResponse:THandle;
  end;

implementation

end.
