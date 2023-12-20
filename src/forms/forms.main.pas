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
    actPingStart: TAction;
    actScanStop: TAction;
    actMyIPFetch: TAction;
    actScanStart: TAction;
    alMain: TActionList;
    actFileExit: TFileExit;
    btnScanStart: TButton;
    btnMyIpFetch: TButton;
    btnScanStop: TButton;
    btnPingStart: TButton;
    edtStartIP: TIPEdit;
    edtEndIP: TIPEdit;
    edtPingHost: TIPEdit;
    ipsMain: TIniPropStorage;
    lblPingHost: TLabel;
    memMyIPLog: TMemo;
    memPingLog: TMemo;
    panPingButtons: TPanel;
    Panel2: TPanel;
    panMyIPButtons: TPanel;
    tsMyIP: TTabSheet;
    vstScan: TLazVirtualStringTree;
    lblStartIP: TLabel;
    lblEndIP: TLabel;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mmMain: TMainMenu;
    panScanButtons: TPanel;
    pcMain: TPageControl;
    tsTraceRoute: TTabSheet;
    tsPing: TTabSheet;
    tsScan: TTabSheet;
    procedure actMyIPFetchExecute(Sender: TObject);
    procedure actPingStartExecute(Sender: TObject);
    procedure actScanStartExecute(Sender: TObject);
    procedure actScanStopExecute(Sender: TObject);
    procedure alMainUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
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
, pingsend
;

const
  cVersion = {$I version.inc};
  cPropStorageIniSection = 'main';
  cMyIPURL: array [1..4] of String = (
    'ifconfig.me',
    'ifconfig.co',
    'ipecho.net/plain',
    'checkip.amazonaws.com'
  );

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption:= Format('%s v%s', [ Application.Title, cVersion ]);
  pcMain.ActivePageIndex:= 0;
  EnablePropertyStorage;
  InitShortcuts;
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
    until (success) or (index > 4);
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

end.

