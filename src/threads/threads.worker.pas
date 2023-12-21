unit Threads.Worker;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, laz.VirtualTrees
, Data.Scan
, Threads.Common
;

type
{ TScanThreadWorker }
  TScanThreadWorker = class(TThread)
  private
    FVirtualNode: PVirtualNode;
    FScanEntry: TScanEntry;
    FOnScanUpdate: TOnScanUpdate;

    procedure DoScanUpdate;

  protected
    procedure Execute; override;
  public
    constructor Create(const AOnThreadTerminate: TNotifyEvent;
      const AVirtualNode: PVirtualNode;
      const AScanEntry: TScanEntry;
      const AOnScanUpdate: TOnScanUpdate);
    destructor Destroy; override;
  published
  end;


implementation

{ TScanThreadWorker }

constructor TScanThreadWorker.Create(const AOnThreadTerminate: TNotifyEvent;
  const AVirtualNode: PVirtualNode;
  const AScanEntry: TScanEntry;
  const AOnScanUpdate: TOnScanUpdate);
begin
  inherited Create(True);
  FreeOnTerminate:= True;

  OnTerminate:= AOnThreadTerminate;
  FVirtualNode:= AVirtualNode;
  FScanEntry:= AScanEntry;
  fOnScanUpdate:= AOnScanUpdate;
end;

destructor TScanThreadWorker.Destroy;
begin
  FVirtualNode:= nil;
  FOnScanUpdate:= nil;
  inherited Destroy;
end;

procedure TScanThreadWorker.DoScanUpdate;
begin
  if Assigned(FOnScanUpdate) then
  begin
    FOnScanUpdate(FVirtualNode, FScanEntry.Status);
  end;
end;

procedure TScanThreadWorker.Execute;
begin
  FScanEntry.Status:='Starting...';
  Synchronize(@DoScanUpdate);
  Sleep(20);

  { #todo -ogcarreno : Actually implement the scans }
  FScanEntry.Status:='Scanning HTTP...';
  Synchronize(@DoScanUpdate);
  Sleep(1000);
  FScanEntry.Status:='Port HTTP open';
  Synchronize(@DoScanUpdate);

  FScanEntry.Status:='Scanning HTTPS...';
  Synchronize(@DoScanUpdate);
  Sleep(1000);
  FScanEntry.Status:='Port HTTPS closed';
  Synchronize(@DoScanUpdate);


  FScanEntry.Status:='Scanning DNS...';
  Synchronize(@DoScanUpdate);
  Sleep(1000);
  FScanEntry.Status:='Port DNSS closed';
  Synchronize(@DoScanUpdate);

  FScanEntry.Status:='Done!';
  Synchronize(@DoScanUpdate);
end;

end.

