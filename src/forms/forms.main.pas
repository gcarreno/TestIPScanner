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
, StdActns, StdCtrls, ExtCtrls, IniPropStorage, laz.VirtualTrees
;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actScanStop: TAction;
    actMyIPFetch: TAction;
    actScanStart: TAction;
    alMain: TActionList;
    actFileExit: TFileExit;
    btnScanStart: TButton;
    btnMyIpFetch: TButton;
    btnScanStop: TButton;
    edtFromIP: TEdit;
    edtEndIP: TEdit;
    ipsMain: TIniPropStorage;
    memMyIPLog: TMemo;
    panMyIPButtons: TPanel;
    tsMyIP: TTabSheet;
    vstScan: TLazVirtualStringTree;
    lblFromIP: TLabel;
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
;

const
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
  actMyIPFetch.Enabled:= True;
end;

procedure TfrmMain.DisableControls;
begin
  actMyIPFetch.Enabled:= False;
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
        httpResult:= httpClient.SimpleGet(Format('http://%s', [ cMyIPURL[index] ]));
        success:= True;
      except
        on E: Exception do
        begin
          memMyIPLog.Append(Format('ERROR(%s): %s', [ cMyIPURL[index], E.Message ]));
          success:= False;
        end;
      end;
      Inc(index);
    until (success) or (index > 4);
    if success then
    begin
      memMyIPLog.Append(Format('From "%s", your IP is "%s"', [ cMyIPURL[index], Trim(httpResult) ]));
    end
    else
    begin
      memMyIPLog.Append('Something went horribly wrong');
    end;
  finally
    httpClient.Free;
  end;

  Application.ProcessMessages;
  EnableControls;
end;

procedure TfrmMain.actScanStartExecute(Sender: TObject);
begin
  //
end;

procedure TfrmMain.actScanStopExecute(Sender: TObject);
begin
  //
end;

end.

