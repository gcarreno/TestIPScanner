unit Threads.Pool;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, syncobjs
, laz.VirtualTrees
, Data.Scan
, Threads.Common
, Threads.Worker
;

type
{ TScanThreadPool }
  TScanThreadPool = class(TThread)
  private
    FOnScanUpdate: TOnScanUpdate;
    FVirtualTree: TLazVirtualStringTree;
    FVirtualNode: PVirtualNode;
    FCriticalSection: TCriticalSection;
    FThreadsActive: Integer;

    procedure WorkerThreadTerminated(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(const AOnThreadTerminate: TNotifyEvent;
      const AOnScanUpdate: TOnScanUpdate);
    destructor Destroy; override;

    procedure StartScan(const AVirtualTree: TLazVirtualStringTree;
      const AVirtualNode: PVirtualNode);
  published
  end;

implementation

{ TScanThreadPool }

constructor TScanThreadPool.Create(const AOnThreadTerminate: TNotifyEvent;
  const AOnScanUpdate: TOnScanUpdate);
begin
  inherited Create(True);
  FreeOnTerminate:= False;
  OnTerminate:= AOnThreadTerminate;
  FOnScanUpdate:= AOnScanUpdate;
  FThreadsActive:= 0;
  FCriticalSection:= TCriticalSection.Create;
end;

destructor TScanThreadPool.Destroy;
begin
  FCriticalSection.Free;
  inherited Destroy;
end;

procedure TScanThreadPool.StartScan(const AVirtualTree: TLazVirtualStringTree;
  const AVirtualNode: PVirtualNode);
begin
  FVirtualTree:= AVirtualTree;
  FVirtualNode:= AVirtualNode;
  Start;
end;

procedure TScanThreadPool.WorkerThreadTerminated(Sender: TObject);
begin
  FCriticalSection.Acquire;
  try
    Dec(FThreadsActive);
  finally
    FCriticalSection.Release;
  end;
end;

procedure TScanThreadPool.Execute;
var
  scanEntry: PScanEntry;
  worker: TScanThreadWorker;
begin
  while not Terminated do
  begin
    FCriticalSection.Acquire;
    try
      if FThreadsActive < 4 then
      begin
        if Assigned(FVirtualNode) then
        begin
          scanEntry:= FVirtualTree.GetNodeData(FVirtualNode);
          if Assigned(scanEntry) then
          begin
            worker:= TScanThreadWorker.Create(
              @WorkerThreadTerminated,
              FVirtualNode,
              scanEntry^,
              FOnScanUpdate
            );
            worker.Start;
          end;
          FVirtualNode:= FVirtualNode^.NextSibling;
          if FVirtualNode = nil then break;
        end;
      end;
      Inc(FThreadsActive);
    finally
      FCriticalSection.Release;
    end;
    Sleep(100);
  end;
end;

end.

