unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, Forms
, Controls
, Graphics
, Dialogs
, ComCtrls
, ActnList
, Menus
, StdActns
, StdCtrls
, ExtCtrls
, IniPropStorage
, IPEdit
, laz.VirtualTrees
;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actScanClear: TAction;
    actTraceStart: TAction;
    actPingStart: TAction;
    actScanStop: TAction;
    actMyIPFetch: TAction;
    actScanStart: TAction;
    alMain: TActionList;
    actFileExit: TFileExit;
    btnTraceStart: TButton;
    btnScanStart: TButton;
    btnMyIpFetch: TButton;
    btnScanStop: TButton;
    btnPingStart: TButton;
    btnScanClear: TButton;
    edtPingHost: TEdit;
    edtTraceHost: TEdit;
    edtScanStartIP: TIPEdit;
    edtScanEndIP: TIPEdit;
    ipsMain: TIniPropStorage;
    lblPingHost: TLabel;
    lblTraceHost: TLabel;
    memMyIPLog: TMemo;
    memPingLog: TMemo;
    memTraceLog: TMemo;
    panExitWarning: TPanel;
    panPingButtons: TPanel;
    panMyIPButtons: TPanel;
    panTraceButtons: TPanel;
    tsMyIP: TTabSheet;
    vstScan: TLazVirtualStringTree;
    lblScanStartIP: TLabel;
    lblScanEndIP: TLabel;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mmMain: TMainMenu;
    panScanButtons: TPanel;
    pcMain: TPageControl;
    tsTraceRoute: TTabSheet;
    tsPing: TTabSheet;
    tsScan: TTabSheet;
    procedure actMyIPFetchExecute(Sender: TObject);
    procedure actScanStartExecute(Sender: TObject);
    procedure actScanStopExecute(Sender: TObject);
    procedure actScanClearExecute(Sender: TObject);
    procedure actPingStartExecute(Sender: TObject);
    procedure actTraceStartExecute(Sender: TObject);
    procedure alMainUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure vstScanGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstScanGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);

    procedure EnablePropertyStorage;
    procedure DisablePropertyStorage;
    procedure InitShortcuts;
    procedure EnableControls;
    procedure DisableControls;
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  LCLType
, fphttpclient
, blcksock
, pingsend
;

const
  cVersion = {$I version.inc};
  cPropStorageIniSection = 'main';
  cMyIPURL: array [1..3] of String = (
    'ifconfig.me',
    'checkip.amazonaws.com',
    'ipecho.net/plain'
  );

type
  PScanEntry = ^TScanEntry;
  TScanEntry = record
    IP: String;
    Status: String;
  end;

var
  DisplayExitMessage: Boolean = False;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption:= Format('%s v%s', [ Application.Title, cVersion ]);
  if pcMain.ActivePageIndex <> 0 then pcMain.ActivePageIndex:= 0;
  EnablePropertyStorage;
  InitShortcuts;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  case pcMain.ActivePageIndex of
    0:begin // MyIP
      // Do Nothing
    end;
    1:begin // Scan
      edtScanStartIP.SetFocus;
    end;
    2:begin // Ping
      // Do nothing
    end;
    3:begin // Trate Route
      // Do Nothing
    end;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if DisplayExitMessage then
  begin
    pcMain.Visible:= False;
    panExitWarning.Align:= alClient;
    Application.ProcessMessages;
  end;
  CanClose:= True;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  DisablePropertyStorage;
end;

procedure TfrmMain.EnablePropertyStorage;
begin
{$IFDEF WINDOWS}
  if not DirectoryExists(GetAppConfigDir(False)) then
  begin
    ForceDirectories(GetAppConfigDir(False));
  end;
{$ENDIF}
  ipsMain.IniFileName:= GetAppConfigFile(False);
  ipsMain.IniSection:= cPropStorageIniSection;
  ipsMain.Active:= True;
end;

procedure TfrmMain.DisablePropertyStorage;
begin
  ipsMain.Save;
  ipsMain.Active:= False;
end;

procedure TfrmMain.InitShortcuts;
begin
{$IFDEF UNIX}
  actFileExit.ShortCut := KeyToShortCut(VK_Q, [ssCtrl]);
{$ENDIF}
{$IFDEF WINDOWS}
  actFileExit.ShortCut := KeyToShortCut(VK_X, [ssAlt]);
{$ENDIF}
end;

procedure TfrmMain.EnableControls;
begin
  pcMain.Enabled:= True;
end;

procedure TfrmMain.DisableControls;
begin
  pcMain.Enabled:= False;
end;

procedure TfrmMain.alMainUpdate(AAction: TBasicAction; var Handled: Boolean);
begin
  if AAction = actScanStop then
  begin
    actScanStop.Enabled:= not actScanStart.Enabled;
    Handled:= True;
  end;
  if AAction = actScanClear then
  begin
    actScanClear.Enabled:= (vstScan.RootNodeCount > 0) and (actScanStart.Enabled);
    Handled:= True;
  end;
end;

procedure TfrmMain.vstScanGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize:= SizeOf(TScanEntry);
end;

procedure TfrmMain.vstScanGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  entry: PScanEntry;
begin
  if Assigned(Node) then
  begin
    entry:= Sender.GetNodeData(Node);
    if Assigned(entry) then
    begin
      case Column of
        0:begin
          CellText:= entry^.IP;
        end;
        1:begin
          CellText:= entry^.Status;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.actMyIPFetchExecute(Sender: TObject);
var
  httpClient: TFPHTTPClient;
  httpResult: String;
  index: Integer;
  success: Boolean = false;
begin
  DisableControls;
  Application.ProcessMessages;

  httpClient:= TFPHTTPClient.Create(nil);
  try
    index:= 1;
    repeat
      try
        memMyIPLog.Append(Format('Attempting to contact "%s"', [ cMyIPURL[index] ]));
        Application.ProcessMessages;
        httpResult:= httpClient.SimpleGet(Format('http://%s', [ cMyIPURL[index] ]));
        success:= True;
      except
        on E: Exception do
        begin
          memMyIPLog.Append(Format('ERROR(%s): %s', [ cMyIPURL[index], E.Message ]));
          Application.ProcessMessages;
          success:= False;
        end;
      end;
      Inc(index);
    until (success) or (index > High(cMyIPURL));
    if success then
    begin
      memMyIPLog.Append(Format('From "%s", your IP is "%s"', [ cMyIPURL[index], Trim(httpResult) ]));
      Application.ProcessMessages;
    end
    else
    begin
      memMyIPLog.Append('Something went horribly wrong');
      Application.ProcessMessages;
    end;
  finally
    httpClient.Free;
  end;

  Application.ProcessMessages;
  EnableControls;
end;

procedure TfrmMain.actScanStartExecute(Sender: TObject);
var
  node: PVirtualNode = nil;
  entry: PScanEntry = nil;
  index0,
  index1,
  index2,
  index3: Integer;
  quadStart,
  quadEnd: TStringArray;
begin
  DisplayExitMessage:= True;

  actScanStart.Enabled:= False;
  Application.ProcessMessages;
  try

    quadStart:= edtScanStartIP.TextTrimmed.Split(['.']);
    quadEnd:= edtScanEndIP.TextTrimmed.Split(['.']);

    if (StrToInt(quadStart[0]) > StrToInt(quadEnd[0])) or
       (StrToInt(quadStart[1]) > StrToInt(quadEnd[1])) or
       (StrToInt(quadStart[2]) > StrToInt(quadEnd[2])) or
       (StrToInt(quadStart[3]) > StrToInt(quadEnd[3])) then
    begin
      ShowMessage('Start IP should be numerically before End IP');
      exit;
    end;
    vstScan.BeginUpdate;
    vstScan.TreeOptions.MiscOptions:= vstScan.TreeOptions.MiscOptions - [toReadOnly];

    if vstScan.RootNodeCount > 0 then
    begin
      vstScan.Clear;
    end;

    for index0:= StrToInt(quadStart[0]) to StrToInt(quadEnd[0]) do
    begin
      for index1:= StrToInt(quadStart[1]) to StrToInt(quadEnd[1]) do
      begin
        for index2:= StrToInt(quadStart[2]) to StrToInt(quadEnd[2]) do
        begin
          for index3:= StrToInt(quadStart[3]) to StrToInt(quadEnd[3]) do
          begin
            node:= vstScan.AddChild(vstScan.RootNode);
            if Assigned(node) then
            begin
              entry:= vstScan.GetNodeData(node);
              if Assigned(entry) then
              begin
                entry^.IP:= Format('%d.%d.%d.%d', [
                  index0,
                  index1,
                  index2,
                  index3
                ]);
                entry^.Status:= 'Waiting';
              end;
            end;
          end;
        end;
      end;
    end;

    vstScan.TreeOptions.MiscOptions:= vstScan.TreeOptions.MiscOptions + [toReadOnly];
    vstScan.EndUpdate;

  finally
    Application.ProcessMessages;
    //actScanStart.Enabled:= True;
  end;
end;

procedure TfrmMain.actScanStopExecute(Sender: TObject);
begin
  btnScanStop.Enabled:= False;
  Application.ProcessMessages;
  try

    vstScan.BeginUpdate;
    vstScan.TreeOptions.MiscOptions:= vstScan.TreeOptions.MiscOptions - [toReadOnly];

    vstScan.TreeOptions.MiscOptions:= vstScan.TreeOptions.MiscOptions + [toReadOnly];
    vstScan.EndUpdate;

  finally
    Application.ProcessMessages;
    actScanStart.Enabled:= True;
  end;
end;

procedure TfrmMain.actScanClearExecute(Sender: TObject);
begin
  if vstScan.RootNodeCount > 0 then
  begin
    vstScan.BeginUpdate;
    vstScan.TreeOptions.MiscOptions:= vstScan.TreeOptions.MiscOptions - [toReadOnly];
    vstScan.Clear;
    vstScan.TreeOptions.MiscOptions:= vstScan.TreeOptions.MiscOptions + [toReadOnly];
    vstScan.EndUpdate;
  end;
end;

procedure TfrmMain.actPingStartExecute(Sender: TObject);
var
  pingClient: TPINGSend;
  index: Integer;
begin
  if not (Length(edtPingHost.Text) > 0) then
  begin
    ShowMessage('The host field needs to have a value!');
    exit;
  end;

  DisableControls;
  Application.ProcessMessages;

  memPingLog.Append(Format('Will now Ping "%s" 3 times', [ edtPingHost.Text ]));
  Application.ProcessMessages;
  pingClient:= TPINGSend.Create;
  try
    for index:= 1 to 3 do
    begin
      memPingLog.Append(Format('Attempt %d...', [ index ]));
      Application.ProcessMessages;

      if pingClient.Ping(edtPingHost.Text) then
      begin
        memPingLog.Append(Format('Attempt %d: %s - %d ms', [
          index,
          pingClient.ReplyFrom,
          pingClient.PingTime
        ]));
        Application.ProcessMessages;
      end
      else
      begin
        memPingLog.Append(Format('Attempt %d failed: (%d) %s', [
          index,
          pingClient.ReplyCode,
          pingClient.ReplyErrorDesc
        ]));
        Application.ProcessMessages;
        break;
      end;
    end;
  finally
    pingClient.Free;
  end;

  Application.ProcessMessages;
  EnableControls;
end;

procedure TfrmMain.actTraceStartExecute(Sender: TObject);
var
  traceClient: TPINGSend;
  ttl: byte;
begin
  if not (Length(edtTraceHost.Text) > 0) then
  begin
    ShowMessage('The host field needs to have a value!');
    exit;
  end;

  DisableControls;
  Application.ProcessMessages;

  memTraceLog.Append(Format('Will now Trace Route "%s" (Max 29 Hops)', [ edtTraceHost.Text ]));
  Application.ProcessMessages;

  traceClient:= TPINGSend.Create;
  try
    ttl:= 1;
    repeat
      traceClient.TTL := ttl;
      inc(ttl);
      if ttl > 30 then break;
      if not traceClient.Ping(edtTraceHost.Text) then
      begin
        memTraceLog.Append(Format('Hop %.2d (%s): (%d) %s or Timeout', [
          Pred(ttl),
          cAnyHost,
          traceClient.ReplyCode,
          traceClient.ReplyErrorDesc
        ]));
        Application.ProcessMessages;
        continue;
      end;
      if (traceClient.ReplyError <> IE_NoError) and
         (traceClient.ReplyError <> IE_TTLExceed) then
      begin
        memTraceLog.Append(Format('Hop %.2d (%s): %s', [
          Pred(ttl),
          traceClient.ReplyFrom,
          traceClient.ReplyErrorDesc
        ]));
        Application.ProcessMessages;
        break;
      end;
      memTraceLog.Append(Format('Hop %.2d (%s): %d ms', [
        Pred(ttl),
        traceClient.ReplyFrom,
        traceClient.PingTime
      ]));
      Application.ProcessMessages;
    until traceClient.ReplyError = IE_NoError;
  finally
    traceClient.Free;
  end;

  Application.ProcessMessages;
  EnableControls;
end;

end.

