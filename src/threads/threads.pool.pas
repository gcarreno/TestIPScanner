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
    FActiveThreads: Integer;

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
  FreeOnTerminate:= True;
  OnTerminate:= AOnThreadTerminate;
  FOnScanUpdate:= AOnScanUpdate;
  FActiveThreads:= 0;
  FCriticalSection:= TCriticalSection.Create;
end;

destructor TScanThreadPool.Destroy;
begin
  FOnScanUpdate:= nil;
  FVirtualTree:= nil;
  FVirtualNode:= nil;
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
    Dec(FActiveThreads);
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
      if FActiveThreads < 4 then
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
            Inc(FActiveThreads);
          end;
          FVirtualNode:= FVirtualNode^.NextSibling;
        end;
      end;
    finally
      FCriticalSection.Release;
    end;
    if (FActiveThreads = 0) and (FVirtualNode = nil) then break;
    Sleep(1);
  end;
end;

end.

