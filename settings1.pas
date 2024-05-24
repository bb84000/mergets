{****************************************************************************** }
{ settings1 - Modify settings form and record                                                }
{ bb - sdtp - march 2024                                                     }
{*******************************************************************************}

unit settings1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, laz2_DOM, laz2_XMLRead, laz2_XMLWrite, lazbbutils, lazbbinifiles;

type

  // Define the classes in this Unit at the very start for clarity
  TFSettings = Class;          // This is a forward class definition

  // Settings record management
  TConfig = class
  private
    FOnChange: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    FSavSizePos: Boolean;
    FWState: string;
    FLastUpdChk: Tdatetime;
    FNoChkNewVer: Boolean;
    FStartup: Boolean;
    FStartMini: Boolean;
    FLangStr: String;
    FDataFolder: String;
    FffmpegPath: String;
    FAppName: String;
    FVersion: String;
    FLastVersion: String;
    FTranslateTimeDate: Boolean;
    function SaveItem(iNode: TDOMNode; sname, svalue: string): TDOMNode;
    procedure SetDataFolder(s: string);
    procedure SetffmpegPath(s: string);
  public
    constructor Create (AppName: string);
    procedure SetSavSizePos (b: Boolean);
    procedure SetWState (s: string);
    procedure SetLastUpdChk (dt: TDateTime);
    procedure SetNoChkNewVer (b: Boolean);
    procedure SetStartup (b: Boolean);
    procedure SetStartmini (b: Boolean);
    procedure SetLangStr (s: string);

    procedure SetVersion(s: string);
    procedure SetLastVersion(s: String);
    procedure SetTranslateTimeDate(b: Boolean);
    function SaveXMLnode(iNode: TDOMNode): Boolean;
    function SaveToXMLfile(filename: string): Boolean;
    function LoadXMLNode(iNode: TDOMNode): Boolean;
    function LoadXMLFile(filename: string): Boolean;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property SavSizePos: Boolean read FSavSizePos write SetSavSizePos;
    property WState: string read FWState write SetWState;
    property LastUpdChk: Tdatetime read FLastUpdChk write SetLastUpdChk;
    property NoChkNewVer: Boolean read FNoChkNewVer write SetNoChkNewVer;
    property Startup: Boolean read FStartup write SetStartup;
    property StartMini: Boolean read FStartMini write SetStartMini;
    property LangStr: String read FLangStr write SetLangStr;
    property DataFolder: string read FDataFolder write setDataFolder;
    property ffmpegPath: string read FffmpegPath write SetffmpegPath;
    property AppName: string read FAppName write FAppName;
    property Version: string read FVersion write SetVersion;
    property LastVersion: string read FLastVersion write SetLastVersion;
    property TranslateTimeDate: Boolean read FTranslateTimeDate write SetTranslateTimeDate;

end;


  { TFSettings }

  TFSettings = class(TForm)
    BtnCancel: TBitBtn;
    BtnOK: TBitBtn;
    CBSavePos: TCheckBox;
    CBNoChkNewVer: TCheckBox;
    CBLangue: TComboBox;
    EDataFolder: TEdit;
    EffmpegPath: TEdit;
    GroupBox1: TGroupBox;
    Lffmpegpath: TLabel;
    LLangue: TLabel;
    LDataFolder: TLabel;
    LStatus: TLabel;
    PnlButtons: TPanel;
    PnlStatus: TPanel;
    SBtnSelectTffmpeg: TSpeedButton;
    SDffmpeg: TSelectDirectoryDialog;
    procedure CBStartupChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SBtnSelectTffmpegClick(Sender: TObject);
  private

  public
    Settings: TConfig;
    procedure Translate(LngFile: TBbIniFile);
  end;

var
  FSettings: TFSettings;

implementation

{$R *.lfm}

constructor TConfig.Create(AppName: string);
begin
  inherited Create;
  FAppName:= AppName;
end;

procedure TConfig.SetSavSizePos(b: Boolean);
begin
  if FSavSizePos <> b then
  begin
    FSavSizePos:= b;
    if Assigned(FOnStateChange) then FOnStateChange(Self);
  end;
end;

procedure TConfig.SetWState(s: string);
begin
   if FWState <> s then
   begin
     FWState:= s;
     if Assigned(FOnStateChange) then FOnStateChange(Self);
   end;
end;

procedure TConfig.SetLastUpdChk(dt: TDateTime);
begin
   if FLastUpdChk <> dt then
   begin
     FLastUpdChk:= dt;
     if Assigned(FOnChange) then FOnChange(Self);
   end;
end;

procedure TConfig.SetNoChkNewVer(b: Boolean);
begin
   if FNoChkNewVer <> b then
   begin
     FNoChkNewVer:= b;
     if Assigned(FOnChange) then FOnChange(Self);
   end;
end;


procedure TConfig.SetStartup (b: Boolean);
begin
   if FStartup <> b then
   begin
     FStartup:= b;
     if Assigned(FOnChange) then FOnChange(Self);
   end;
end;

procedure TConfig.SetStartMini (b: Boolean);
begin
   if FStartMini <> b then
   begin
     FStartMini:= b;
     if Assigned(FOnChange) then FOnChange(Self);
   end;
end;

procedure TConfig.SetLangStr (s: string);
begin
  if FLangStr <> s then
  begin
    FLangStr:= s;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TConfig.SetDataFolder (s: string);
begin
  if FDataFolder = s then exit;
  FDataFolder:= s;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetffmpegPath (s: string);
begin
  if FffmpegPath = s then exit;
  FffmpegPath:= s;
  if Assigned(FOnChange) then FOnChange(Self);
end;


procedure TConfig.SetVersion(s:string);
begin
  if FVersion <> s then
  begin
    FVersion:= s;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TConfig.SetLastVersion(s:string);
begin
  if FLastVersion <> s then
  begin
    FLastVersion:= s;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TConfig.SetTranslateTimeDate(b: boolean);
begin
  if FTranslateTimeDate <> b then
  begin
    FTranslateTimeDate:= b;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

function TConfig.SaveItem(iNode: TDOMNode; sname, svalue: string): TDOMNode;
begin
  result:= iNode.OwnerDocument.CreateElement(sname);
  result.TextContent:= svalue;
end;

function TConfig.SaveXMLnode(iNode: TDOMNode): Boolean;
begin
  Try
    TDOMElement(iNode).SetAttribute ('version', FVersion);
    iNode.AppendChild(SaveItem(iNode, 'lastversion', FLastVersion));
    iNode.AppendChild(SaveItem(iNode, 'savsizepos', BoolToString(FSavSizePos)));
    iNode.AppendChild(SaveItem(iNode, 'wstate', FWState));
    iNode.AppendChild(SaveItem(iNode, 'lastupdchk', TimeDateToString(FLastUpdChk)));
    iNode.AppendChild(SaveItem(iNode, 'nochknewver', BoolToString(FNoChkNewVer)));
    iNode.AppendChild(SaveItem(iNode, 'translatetimedate', BoolToString(FTranslateTimeDate)));
    iNode.AppendChild(SaveItem(iNode, 'startup', BoolToString(FStartup)));
    iNode.AppendChild(SaveItem(iNode, 'startmini',BoolToString(FStartMini)));
    iNode.AppendChild(SaveItem(iNode, 'langstr', FLangStr));
    iNode.AppendChild(SaveItem(iNode, 'datafolder', FDataFolder));
    iNode.AppendChild(SaveItem(iNode, 'ffmpegpath', FffmpegPath));
    Result:= True;
  except
    result:= False;
  end;
end;

function TConfig.SaveToXMLfile(filename: string): Boolean;
var
  SettingsXML: TXMLDocument;
  RootNode, SettingsNode :TDOMNode;
begin
  result:= false;
  if FileExists(filename)then
  begin
    ReadXMLFile(SettingsXML, filename);
    RootNode := SettingsXML.DocumentElement;
  end else
  begin
    SettingsXML := TXMLDocument.Create;
    RootNode := SettingsXML.CreateElement(lowercase(FAppName));
    SettingsXML.Appendchild(RootNode);
  end;
  SettingsNode:= RootNode.FindNode('settings');
  if SettingsNode <> nil then RootNode.RemoveChild(SettingsNode);
  SettingsNode:= SettingsXML.CreateElement('settings');
  SaveXMLnode(SettingsNode);
  RootNode.Appendchild(SettingsNode);
  writeXMLFile(SettingsXML, filename);
  result:= true;
  if assigned(SettingsXML) then SettingsXML.free;
end;

function TConfig.LoadXMLNode(iNode: TDOMNode): Boolean;
var
  s: string;
  UpCaseSetting: string;
  subNode: TDOMNode;
begin
  Result := false;
  //if (iNode = nil) or (iNode.Attributes = nil) then exit;
  if (iNode = nil) then exit;
  try
    UpCaseSetting:=UpperCase(iNode.Attributes.Item[0].NodeName);
    if UpCaseSetting='VERSION' then FVersion:= iNode.Attributes.Item[0].NodeValue;
    subNode := iNode.FirstChild;
    while subNode <> nil do
    try
      upCaseSetting:= UpperCase(subNode.NodeName);
      s:= subNode.TextContent;
      if UpCaseSetting = 'LASTVERSION' then FLastVersion:= s;
      if upCaseSetting = 'SAVSIZEPOS' then FSavSizePos:= StringToBool(s);
      if upCaseSetting = 'WSTATE' then  FWState:= s;
      if upCaseSetting = 'LASTUPDCHK' then FLastUpdChk:= StringToTimeDate(s);
      if upCaseSetting = 'NOCHKNEWVER' then FNoChkNewVer:= StringToBool(s);
      if upCaseSetting = 'TRANSLATETIMEDATE' then FTranslateTimeDate:= StringToBool(s);
      if upCaseSetting = 'STARTUP' then FStartup:= StringToBool(s);
      if upCaseSetting = 'STARTMINI' then FStartMini:= StringToBool(s);
      if upCaseSetting = 'LANGSTR' then FLangStr:= s;
      if upCaseSetting = 'DATAFOLDER' then FDataFolder:= s;
      if upCaseSetting = 'FFMPEGPATH' then ffmpegPath:= s;
    finally
      subnode:= subnode.NextSibling;
    end;
    result:= true;
  except
    result:= false;
  end;

end;

function TConfig.LoadXMLFile(filename: string): Boolean;
var
  SettingsXML: TXMLDocument;
  RootNode,SettingsNode : TDOMNode;
begin
  result:= false;
  if not FileExists(filename) then
  begin
    SaveToXMLfile(filename);
  end;
  ReadXMLFile(SettingsXML, filename);
  RootNode := SettingsXML.DocumentElement;
  SettingsNode:= RootNode.FindNode('settings');
  if SettingsNode= nil then exit;
  LoadXMLnode(SettingsNode);
  If assigned(SettingsNode) then SettingsNode.free;
  result:= true;
end;

{ TFSettings : Settings dialog }

procedure TFSettings.CBStartupChange(Sender: TObject);
begin
  //CBMinimized.Enabled:= CBStartup.Checked;
end;

procedure TFSettings.FormCreate(Sender: TObject);
begin
  Settings:= TConfig.Create('appname');
end;

procedure TFSettings.SBtnSelectTffmpegClick(Sender: TObject);
begin
  SDffmpeg.filename:= EffmpegPath.Text;
  if SDffmpeg.Execute then
  EffmpegPath.Text:= SDffmpeg.filename;
end;

procedure TFSettings.Translate(LngFile: TBbIniFile);
var
  DefaultCaption: String;
begin
  if assigned (Lngfile) then
  with LngFile do
  begin
    BtnOK.Caption:= ReadString('Common', 'OKBtn', BtnOK.Caption);
    BtnCancel.Caption:= ReadString('Common', 'CancelBtn', BtnCancel.Caption);
    DefaultCaption:= ReadString('Common', 'DefaultCaption', '...');
    Caption:=Format(ReadString('FSettings','Caption','Préférences de %s'), [DefaultCaption]);
    GroupBox1.Caption:=ReadString('FSettings', 'GroupBox1.Caption',GroupBox1.Caption);
    LDataFolder.Caption:=ReadString('FSettings', 'LDataFolder.Caption',LDataFolder.Caption);
    //CBStartup.Caption:=ReadString('FSettings', 'CBStartup.Caption',CBStartup.Caption);
    //CBMinimized.Caption:=ReadString('FSettings', 'CBMinimized.Caption',CBMinimized.Caption);
    CBSavePos.Caption:=ReadString('FSettings', 'CBSavePos.Caption',CBSavePos.Caption);
    CBNoChkNewVer.Caption:=ReadString('FSettings', 'CBUpdate.Caption',CBNoChkNewVer.Caption);
    LLangue.Caption:=ReadString('FSettings', 'LLangue.Caption',LLangue.Caption);
  end;
end;

end.

