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
  FScanEntry.Status:='Done!';
  Synchronize(@DoScanUpdate);
  Sleep(10);
end;

end.

