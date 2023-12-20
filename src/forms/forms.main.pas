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
, StdActns, StdCtrls, ExtCtrls, IniPropStorage, IPEdit, laz.VirtualTrees
;

type

  { TfrmMain }

  TfrmMain = class(TForm)
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
    edtTraceHost: TIPEdit;
    edtScanStartIP: TIPEdit;
    edtScanEndIP: TIPEdit;
    edtPingHost: TIPEdit;
    ipsMain: TIniPropStorage;
    lblPingHost: TLabel;
    lblTraceHost: TLabel;
    memMyIPLog: TMemo;
    memPingLog: TMemo;
    memTraceLog: TMemo;
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
    procedure actPingStartExecute(Sender: TObject);
    procedure actTraceStartExecute(Sender: TObject);
    procedure alMainUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure EnablePropertyStorage;
    procedure DisablePropertyStorage;
    procedure InitShortcuts;
    procedure EnableControls;
    procedure DisableControls;
    procedure pcMainChange(Sender: TObject);
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
      edtPingHost.SetFocus;
    end;
    3:begin // Trate Route
      edtTraceHost.SetFocus;
    end;
  end;
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

procedure TfrmMain.pcMainChange(Sender: TObject);
begin

end;

procedure TfrmMain.alMainUpdate(AAction: TBasicAction; var Handled: Boolean);
begin
  if AAction = actScanStop then
  begin
    actScanStop.Enabled:= not actScanStart.Enabled;
    Handled:= True;
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
begin
  actScanStart.Enabled:= False;
  Application.ProcessMessages;

  ShowMessage('Not Implemented yet.');

  Application.ProcessMessages;
  actScanStart.Enabled:= True;
end;

procedure TfrmMain.actScanStopExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.actPingStartExecute(Sender: TObject);
var
  pingClient: TPINGSend;
  index: Integer;
begin
  DisableControls;
  Application.ProcessMessages;

  memPingLog.Append(Format('Will now Ping "%s" 3 times', [ edtPingHost.TextTrimmed ]));
  Application.ProcessMessages;
  pingClient:= TPINGSend.Create;
  try
    for index:= 1 to 3 do
    begin
      memPingLog.Append(Format('Attempt %d...', [ index ]));
      Application.ProcessMessages;

      if pingClient.Ping(edtPingHost.TextTrimmed) then
      begin
        memPingLog.Append(Format('Attempt %d: %d ms', [ index, pingClient.PingTime ]));
        Application.ProcessMessages;
      end
      else
      begin
        memPingLog.Append(Format('Attempt %d failed. ', [ index ]));
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
  DisableControls;
  Application.ProcessMessages;

  memTraceLog.Append(Format('Will now Trace Route "%s" (Max 29 Hops)', [ edtTraceHost.TextTrimmed ]));
  Application.ProcessMessages;

  traceClient:= TPINGSend.Create;
  try
    ttl:= 1;
    repeat
      traceClient.TTL := ttl;
      inc(ttl);
      if ttl > 30 then break;
      if not traceClient.Ping(edtTraceHost.TextTrimmed) then
      begin
        memTraceLog.Append(Format('Hop %d "%s":  %s Timeout', [ Pred(ttl), cAnyHost, traceClient.ReplyFrom ]));
        Application.ProcessMessages;
        continue;
      end;
      if (traceClient.ReplyError <> IE_NoError) and
         (traceClient.ReplyError <> IE_TTLExceed) then
      begin
        memTraceLog.Append(Format('Hop %d "%s": %s', [ Pred(ttl), traceClient.ReplyFrom,  traceClient.ReplyErrorDesc ]));
        Application.ProcessMessages;
        break;
      end;
      memTraceLog.Append(Format('Hop %d "%s": %d ms', [ Pred(ttl), traceClient.ReplyFrom,  traceClient.PingTime ]));
      Application.ProcessMessages;
    until traceClient.ReplyError = IE_NoError;
  finally
    traceClient.Free;
  end;

  Application.ProcessMessages;
  EnableControls;
end;

end.

