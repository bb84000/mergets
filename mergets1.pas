//******************************************************************************
// MergeTs : utility to assemble TS files parts
// Adapters supported : Strong 8211, Strong 8222 and clones
// bb - sdtp - march 2024
//******************************************************************************

unit mergets1;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Win32Proc,
  {$ENDIF}Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons, ExtCtrls, Menus, lazbbutils, lazbbOsVersion, lazbbcontrols,
  lazbbaboutdlg, lazbbinifiles, LazUTF8, settings1, fileutil, Translations ;

type
   { int64 or longint type for Application.QueueAsyncCall }
  {$IFDEF CPU32}
    iDays= LongInt;
  {$ENDIF}
  {$IFDEF CPU64}
    iDays= Int64;
  {$ENDIF}
  TSaveMode = (None, Setting, All);

  TSPacket = record
    syncbyte: longint;
    pid: longint;
    adapfield: longint;
    pcrflag: boolean;
    pcr: int64;
    pcrtime: int64;
  end;


  { TFMergeTS }

  TFMergeTS = class(TForm)
    Button1: TButton;
    LTime1: TLabel;
    LVersion: TLabel;
    MnuItemChangeTime: TMenuItem;
    OSVersion: TbbOsVersion;
    CBtsfiles: TComboBox;
    EMergedTS: TEdit;
    PButtons: TPanel;
    PMnuTime: TPopupMenu;
    SBtnQuit: TSpeedButton;
    SBtnMerge: TSpeedButton;
    SBtnSettings: TSpeedButton;
    SBtnAbout: TSpeedButton;
    LTime: TStaticText;
    TimerTime: TLFPTimer;
    Ltsfiles: TLabel;
    LMergedTS: TLabel;
    Memo1: TMemo;
    OD1: TOpenDialog;
    PStatus: TPanel;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    SD1: TSaveDialog;
    SBtnSelectTSFiles: TSpeedButton;
    SBtnMergedTS: TSpeedButton;
    procedure MnuItemChangeTimeClick(Sender: TObject);
    procedure SBtnMergedTSClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SBtnAboutClick(Sender: TObject);
    procedure SBtnMergeClick(Sender: TObject);
    procedure SBtnQuitClick(Sender: TObject);
    procedure SBtnSettingsClick(Sender: TObject);
    procedure TimerTimeTimer(Sender: TObject);
    procedure SBtnSelectTSFilesClick(Sender: TObject);
  private
    UserPath, UserAppsDataPath: String;
    sUse64bitcaption: String;
    MTSAppDataPath: String;
    CompileDateTime: TDateTime;
    version: string;
    Initialized: Boolean ;
    CurLangStr: string;
    LangFile: TBbIniFile;
    LangNums: TStringList;
    LangFound: boolean;
    crc_table: array[0..255] of Cardinal;
    crc_table_done: Boolean;
    ChkVerInterval: Integer;
    OS, OSTarget: string;
    sRetConfBack, sCreNewConf, sLoadConf: String;
    OKBtn, YesBtn, NoBtn, CancelBtn: String;
    progname: String;
    ConfigFileName: string;
    HttpErrMsgNames: array [0..16] of string;

    SettingsChanged, SettingsStateChanged: Boolean;
    sCannotGetNewVerList, sNoLongerChkUpdates, sUpdateAlertBox : String;
    myLongMonth : TMonthNameArray;
    myLongDay: TWeekNameArray;
    sCannotCreateVideoFolder: String;
    sAskReplaceFile, sExistingFile: String;
    sSelectedOneFile, sFilesToMerge: String;
    sFileFormatNotSupported: String;
    sCopyFile, sLastFilePCR, sJoinFile, sMergeComplete, sMergedFile: String;
    procedure make_crc_table;
    function crc32_block(crc: Cardinal; pData: PByte;blk_len: Integer): Cardinal;
    procedure Initialize;
    procedure Translate(LngFile: TBbInifile);
    procedure LoadSettings(Filename: string);
    function SaveSettings(Typ: TSaveMode): boolean;
    procedure SettingsOnChange(Sender: TObject);
    procedure SettingsOnStateChange(Sender: TObject);
    procedure CheckUpdate(days: iDays);
  public

  end;

var
  FMergeTS: TFMergeTS;

implementation

{$R *.lfm}

{ TFMergeTS }

procedure TFMergeTS.FormCreate(Sender: TObject);
var
  s: string;
 {$IFDEF Linux}
    x: Integer;
 {$ENDIF}
begin
  CompileDateTime:= StringToTimeDate({$I %DATE%}+' '+{$I %TIME%}, 'yyyy/mm/dd hh:nn:ss');
  TimerTime.StartTimer;
  ProgName:= 'MergeTS';
  LangNums := TStringList.Create;
  LangFile:= TBbIniFile.Create(ExtractFilePath(Application.ExeName) + 'lang'+PathDelim+'fr.lng');
  {$IFDEF CPU32}
     OSTarget := '32 bits';
  {$ENDIF}
  {$IFDEF CPU64}
     OSTarget := '64 bits';
  {$ENDIF}
    {$IFDEF Linux}
    OS := 'Linux';
    CurLangStr := GetEnvironmentVariable('LANG');
    x := pos('.', CurLangStr);
    CurLangStr := Copy(CurLangStr, 0, 2);
    wxbitsrun := 0;
    //OSTarget:= '';
    UserAppsDataPath := GetUserDir;
    // Get mail client
  {$ENDIF}
  {$IFDEF WINDOWS}
    OS := 'Windows ';
    // get user data folder
    s := ExtractFilePath(ExcludeTrailingPathDelimiter(GetAppConfigDir(False)));
    if Ord(WindowsVersion) < 7 then
      UserAppsDataPath := s                     // NT to XP
    else
    UserAppsDataPath := ExtractFilePath(ExcludeTrailingPathDelimiter(s)) + 'Roaming'; // Vista to W10
    //LazGetShortLanguageID(CurLangStr);
    CurLangStr:= GetLanguageID.LanguageCode;
  {$ENDIF}
  MTSAppDataPath := UserAppsDataPath + PathDelim + ProgName + PathDelim;
  if not DirectoryExists(MTSAppDataPath) then
  begin
    CreateDir(MTSAppDataPath);
  end;
  ConfigFileName:= MTSAppDataPath+'settings.xml';
  myLongMonth:= DefaultFormatSettings.LongMonthNames ;
  myLongDay:= DefaultFormatSettings.LongDayNames;
  Initialized:= false;
end;

procedure TFMergeTS.FormActivate(Sender: TObject);
begin
  if not Initialized then
  begin
    Initialize;
    Application.ProcessMessages;
    //if StartMini then PostMessage(Handle, WM_FORMSHOWN, 0, 0) ;
    Application.QueueAsyncCall(@CheckUpdate, ChkVerInterval);       // async call to let icons loading

end;

end;

procedure TFMergeTS.Initialize;
var
  IniFile: TBbIniFile;
  i: Integer;
  ConfigFileNameWoExt: String;
begin
  UserPath := GetUserDir;
  CBtsfiles.Text:= '';
  Memo1.Text:='';
  version := GetVersionInfo.ProductVersion;
  SD1.InitialDir:= Userpath+'Videos';

  IniFile:= TBbInifile.Create('mergets.ini');
  AboutBox.ChkVerURL := IniFile.ReadString('urls', 'ChkVerURL','https://github.com/bb84000/mergets/releases/latest');
  AboutBox.UrlWebsite:= IniFile.ReadString('urls', 'UrlWebSite','https://www.sdtp.com');
  AboutBox.UrlSourceCode:=IniFile.ReadString('urls', 'UrlSourceCode','https://github.com/bb84000/mergets');
  AboutBox.UrlProgSite:= IniFile.ReadString('urls', 'UrlSourceCode','https://github.com/bb84000/mergets');
  ChkVerInterval:= IniFile.ReadInt64('urls', 'ChkVerInterval', 3);
  if Assigned(IniFile) then IniFile.free;
  // Now, main settings
  FSettings.Settings.AppName:= LowerCase(ProgName);
  ConfigFileName:= MTSAppDataPath+'settings.xml';
   FSettings.Settings.LangStr:= CurLangStr;
  // Search previous backups
  if not FileExists(ConfigFileName) then
  begin
    ConfigFileNameWoExt:= TrimFileExt(ConfigFileName);
    if FileExists(ConfigFileNameWoExt+ '.bk0') then
    begin
      RenameFile(ConfigFileNameWoExt+ '.bk0', ConfigFileName);
      for i := 1 to 5 do
        if FileExists(ConfigFileNameWoExt+ '.bk' + IntToStr(i))
        // Renomme les précédentes si elles existent
        then
          RenameFile(ConfigFileNameWoExt+ '.bk' + IntToStr(i),
            ConfigFileNameWoExt+ '.bk' + IntToStr(i - 1));
    end else SaveSettings(All);              // or create a new one
  end;
  LangFile:= TBbIniFile.Create(ExtractFilePath(Application.ExeName) + 'lang'+PathDelim+FSettings.Settings.LangStr+'.lng');
  LoadSettings(ConfigFileName);                      // Translation done at the end of translate function
  // In case of program's first use
  if length(FSettings.Settings.LastVersion)=0 then FSettings.Settings.LastVersion:= version;
  Application.Title:=Caption;
  if (Pos('64', OSVersion.Architecture)>0) and (OsTarget='32 bits') then
    MsgDlg(Caption, sUse64bitCaption, mtInformation,  [mbOK], [OKBtn]);
  Application.ProcessMessages;
  if not DirectoryExists(SD1.InitialDir) then
    If Not CreateDir (SD1.InitialDir) Then ShowMessage(sCannotCreateVideoFolder);
  EMergedTS.Text:= Userpath+'Videos\merged.ts';
  // Language dependent variables are updated in ModLangue procedure
  AboutBox.Width:= 400; // to have more place for the long product name
  AboutBox.Image1.Picture.LoadFromResourceName(HInstance, 'ABOUTIMG');
  AboutBox.LCopyright.Caption := GetVersionInfo.CompanyName + ' - ' + DateTimeToStr(CompileDateTime);
  AboutBox.LVersion.Caption := 'Version: ' + Version + ' (' + OS + OSTarget + ')';
  AboutBox.LUpdate.Hint := AboutBox.sLastUpdateSearch + ': ' + DateToStr(FSettings.Settings.LastUpdChk);
  AboutBox.Version:= Version;
  AboutBox.ProgName:= ProgName;
  AboutBox.LastUpdate:= FSettings.Settings.LastUpdChk;
  FSettings.Settings.OnChange := @SettingsOnChange;
  FSettings.Settings.OnStateChange := @SettingsOnStateChange;
  FSettings.LStatus.Caption := OSVersion.VerDetail;
  LVersion.Caption:='Version : '+version+' - '+DateTimeToStr(CompileDateTime);
  LVersion.Hint:= OSVersion.VerDetail;
  Initialized:= true;
end;

procedure TFMergets.LoadSettings(Filename: string);
var
  winstate: TWindowState;
  i: integer;
begin
  With FSettings do
  begin
    Settings.LoadXMLFile(Filename);
    self.Position:= poDesktopCenter;
    try
      WinState := TWindowState(StrToInt('$' + Copy(Settings.WState, 1, 4)));
      self.Top := StrToInt('$' + Copy(Settings.WState, 5, 4));
      self.Left := StrToInt('$' + Copy(Settings.WState, 9, 4));
      self.Height := StrToInt('$' + Copy(Settings.WState, 13, 4));
      self.Width := StrToInt('$' + Copy(Settings.WState, 17, 4));
    except
    end;
    // Détermination de la langue (si pas dans settings, langue par défaut)
    if Settings.LangStr = '' then Settings.LangStr := CurLangStr;
    try
      FindAllFiles(LangNums, ExtractFilePath(Application.ExeName) + 'lang', '*.lng', true); //find all language files
      if LangNums.count > 0 then
      begin
        for i:= 0 to LangNums.count-1 do
        begin
          LangFile:= TBbInifile.Create(LangNums.Strings[i]);
          LangNums.Strings[i]:= TrimFileExt(ExtractFileName(LangNums.Strings[i]));
          FSettings.CBLangue.Items.Add(LangFile.ReadString('common', 'Language', 'Inconnu'));
          if LangNums.Strings[i] = Settings.LangStr then LangFound := True;
        end;
      end;
    except
      LangFound := false;
    end;
    // Si la langue n'est pas traduite, alors on passe en Anglais
    if not LangFound then
    begin
      Settings.LangStr := 'en';
    end;
    MnuItemChangeTime.Checked:= Settings.TranslateTimeDate;
  end;
  LangFile:= TBbIniFile.Create(ExtractFilePath(Application.ExeName) + 'lang'+PathDelim+FSettings.Settings.LangStr+'.lng');
  Translate(LangFile);
  SettingsChanged := false;
end;



function TFMergets.SaveSettings(Typ: TSaveMode): boolean;
var
  i: Integer;
  ConfigFileNameWoExt: String;
begin
  Result := False;
  if (Typ= None) then exit;
  with FSettings do
  begin
    Settings.DataFolder:= MTSAppDataPath;
    Settings.WState:= '';
    if self.Top < 0 then self.Top:= 0;
    if self.Left < 0 then self.Left:= 0;
    // Main form size and position
    Settings.WState:= IntToHex(ord(self.WindowState), 4)+IntToHex(self.Top, 4)+
                      IntToHex(self.Left, 4)+IntToHex(self.Height, 4)+IntToHex(self.width, 4);
    Settings.Version:= version;
  end;
   ConfigFileNameWoExt:= TrimFileExt(ConfigFileName);
  if FileExists (ConfigFileName) then     // en principe toujours, créé la premire fois
  begin
    if (Typ = All) then
    begin
      // On sauvegarde les versions précédentes parce que la config a changé
      if FileExists (ConfigFileNameWoExt+'.bk5')             // Efface la plus ancienne
        then  DeleteFile(ConfigFileNameWoExt+'.bk5');        // si elle existe
      For i:= 4 downto 0 do
        if FileExists (ConfigFileNameWoExt+'.bk'+IntToStr(i))     // Renomme les précédentes si elles existent
        then  RenameFile(ConfigFileNameWoExt+'.bk'+IntToStr(i), ConfigFileNameWoExt+'.bk'+IntToStr(i+1));
        RenameFile(ConfigFileName, ConfigFileNameWoExt+'.bk0');
    end;
    // la base n'a pas changé, on ne fait pas de backup

  end;
  FSettings.settings.SaveToXMLfile(ConfigFileName); ;
  result:= true;
end;

//Dernière recherche il y a "days" jours ou plus ?

procedure TFMergets.CheckUpdate(days: iDays);
var
  errmsg: string;
  sNewVer: string;
  CurVer, NewVer: int64;
  alertpos: TPosition;
  alertmsg: string;
begin
  //Dernière recherche il y a plus de 'days' jours ?
  errmsg := '';
  alertmsg:= '';
  if not visible then alertpos:= poDesktopCenter
  else alertpos:= poMainFormCenter;
  if (Trunc(Now)>Trunc(FSettings.Settings.LastUpdChk)+days) and (not FSettings.Settings.NoChkNewVer) then
  begin
     FSettings.Settings.LastUpdChk := Trunc(Now);
     AboutBox.Checked:= true;
     AboutBox.ErrorMessage:='';
     sNewVer:= AboutBox.ChkNewVersion;
     errmsg:= AboutBox.ErrorMessage;
     // Retry if nothing found the first time
     if (length(sNewVer)=0) and (length(errmsg)=0)then
     begin
       Application.ProcessMessages;
       sNewVer:= AboutBox.ChkNewVersion;
       errmsg:= AboutBox.ErrorMessage;
     end;
     if length(sNewVer)=0 then
     begin
       if length(errmsg)=0 then alertmsg:= sCannotGetNewVerList
       else alertmsg:= TranslateHttpErrorMsg(errmsg, HttpErrMsgNames);
       if AlertDlg(Caption,  alertmsg, [OKBtn, CancelBtn, sNoLongerChkUpdates],
                    true, mtError, alertpos)= mrYesToAll then FSettings.Settings.NoChkNewVer:= true;
        exit;
     end;
     NewVer := VersionToInt(sNewVer);
     // Cannot get new version
     if NewVer < 0 then exit;
     //CurVer := VersionToInt('0.1.0.0');     //Test version check
     CurVer := VersionToInt(version);
     if NewVer > CurVer then
     begin
       FSettings.Settings.LastVersion:= sNewVer;
       AboutBox.LUpdate.Caption := Format(AboutBox.sUpdateAvailable, [sNewVer]);
       AboutBox.NewVersion:= true;
       AboutBox.ShowModal;
     end else
     begin
       AboutBox.LUpdate.Caption:= AboutBox.sNoUpdateAvailable;
     end;
     FSettings.Settings.LastUpdChk:= now;
   end else
   begin
    if VersionToInt(FSettings.Settings.LastVersion)>VersionToInt(version) then
       AboutBox.LUpdate.Caption := Format(AboutBox.sUpdateAvailable, [FSettings.Settings.LastVersion]) else
       begin
         AboutBox.LUpdate.Caption:= AboutBox.sNoUpdateAvailable;
         // Already checked the same day
        if Trunc(FSettings.Settings.LastUpdChk) = Trunc(now) then AboutBox.checked:= true;
       end;
   end;
   AboutBox.Translate(LangFile);
end;

// Event fired by any change of settings values

procedure TFMergets.SettingsOnChange(Sender: TObject);
begin
  SettingsChanged := True;
end;

// Event fired by any state change (window state and position)

procedure TFMergets.SettingsOnStateChange(Sender: TObject);
begin
  SettingsStateChanged := True;
end;



procedure TFMergeTS.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if SettingsChanged then SaveSettings(All) else
  SaveSettings(Setting) ;
end;



procedure TFMergeTS.SBtnAboutClick(Sender: TObject);
begin
  AboutBox.Lastupdate:= FSettings.Settings.LastUpdChk;
  Aboutbox.ShowModal;
  if AboutBox.Lastupdate <> FSettings.Settings.LastUpdChk then
  FSettings.Settings.LastUpdChk:= AboutBox.Lastupdate;
end;

procedure TFMergeTS.SBtnMergeClick(Sender: TObject);
type
  tstype = (TS, MTS);
var
  filtyp: tstype;
  fhnd : THandle;
  mypaq : array [0..191] of byte;   //192 for M2TS
  paqsize: Integer;
  paqofs: Integer;
  mypacket: TSPacket;
  pcrbase, lastpcr: Int64;
  tsread, tspos: Int64;
  pcrbeg: Boolean;
  tmp: Int64;
  tssize, filesize: Int64;
  beginpos, endpos : Int64;
  progress2 : LongInt;
  fts: TFileStream;
  fos: TFileStream;
  x, y: LongInt;
  t: double;
  StartTime, EndTime : TDatetime;
  hms: String;
  Buf5: array [0..4] of Byte;
begin
  // if merged files exists, ask to delete
  if FileExists(EMergedTS.Text) then
  begin
    if MsgDlg(Caption, Format(sAskReplaceFile, [EMergedTS.Text]), mtWarning, [mbYes, mbNo], [YesBtn, NoBtn])= mrNo then
    begin
      Memo1.Lines.Add(Format(sExistingFile, [EMergedTS.Text]));
      exit;
    end else
    begin
      DeleteFile (EMergedTS.Text);
    end;
  end;
  SBtnquit.Enabled:= false;
  SBtnMerge.Enabled:= false;
  SBtnSelectTSFiles.Enabled:= False;
  SBtnMergedTS.Enabled:= false;
  EMergedTS.Enabled:= false;
  StartTime:= Now;
  fos:= TFileStream.Create(EMergedTS.Text, fmCreate);
  mypacket:= Default(TSPacket);
  pcrbase:= 0;
  lastpcr:= 0;
  endpos:= 0;
  filesize:=0;
  ProgressBar2.position:= 0;
  progress2:= 0;
  // check file type on first file
  fhnd:= FileOpen(CBtsfiles.Items[0], fmOpenRead);
  FileRead(fhnd, Buf5, 5 );
  FileClose(fhnd);
  if Buf5[0]= $47 then filtyp:= TS else
  if Buf5[4]= $47 then filtyp:= MTS else
  begin
    ShowMessage(sFileFormatNotSupported);
    memo1.Append(sFileFormatNotSupported);
    exit;
  end;
  if filtyp = TS then
    begin
      paqsize:= 188;
      paqofs:=0;
    end else
    begin
      paqsize:= 192;
      paqofs:= 4;
    end;
    Application.ProcessMessages ;
  // Now, we process each TS file
  for y:= 0 to CBtsfiles.Items.Count-1 do
  begin
    beginpos:= 0;
    pcrbeg:= false;
    memo1.lines.add(Format(sCopyFile, [CBtsfiles.Items[y]]));
    Progressbar1.Position:= 0;
    tsread:= 0;
    tspos:= 0;
    // Open ts file
    fts:= TFileStream.Create(CBtsfiles.Items[y], fmOpenRead) ;
    tssize:= fts.Size div paqsize;
    for x:= 0 to tssize-1 do
    begin
      tsread:= fts.Read(mypaq, paqsize);
      if (filtyp=TS) then
      begin
      mypacket.adapfield:=  ((mypaq[paqofs+3] and $0ff) and $030) shr 4;              //(mypaq[3]&0xff & 0x30) >> 4;
      if mypacket.adapfield > 1 then
      begin
        mypacket.pcrflag:= (((mypaq[paqofs+5] and $010) shr 4) <> 0);                 //(((mypaq[5] & 0x10) >> 4) != 0);
        if (mypacket.pcrflag) then
        begin
          endpos:= tspos;
	  // Calcul de la valeur du PCR
	  {tmp:=0;
	  for i:= 6 to 10 do                      //(int i = 6; i < 11; i++)
          begin
	    tmp:= tmp shl 8;		          //tmp <<= 8;
	    tmp:= tmp or (mypaq[i] and $0FF);	  //tmp |= (mypaq[i] & 0xFF);
	  end;
	  mypacket.pcr:= tmp shr 7; //33 bits  }
          tmp:= (mypaq[paqofs+6] and $0ff)* $1000000+(mypaq[paqofs+7] and $0ff)* $10000+(mypaq[paqofs+8] and $0ff)* $100+(mypaq[paqofs+9] and $0ff) ;
	  mypacket.pcr:= (tmp*$100+(mypaq[paqofs+10] and $0ff)) shr 7; //33 bits
          pcrbase:= mypacket.pcr;
          if ((y > 0) and (not pcrbeg) and (pcrbase = lastpcr)) then
          begin
	    beginpos:= tspos;
	    pcrbeg:= true;
	    mypacket.pcrtime:= (pcrbase div 90); // convert to milliseconds
            t := mypacket.pcrtime/ MSecsPerDay;
            hms:= FormatDateTime('[h]:nn:ss.zzz', t, [fdoInterval]);
	    memo1.append (Format(sJoinFile, [CBtsfiles.Items[y], hms]));
 	  end;
	end;
      end;
      end;
      if ((y=0) or pcrbeg or (filtyp=MTS)) then fos.write(mypaq, paqsize);         	// if (y==0 || pcrbeg) outChan.write(bpaq);
      tspos:= tspos+tsread;                                     //	pos+= read;
      if  (x mod 200) = 0 then
      begin
        ProgressBar1.Position:= Round(100*x / tssize);                                          // use mod to update
        Progressbar2.position:= progress2+ Round(ProgressBar1.Position/CBtsfiles.Items.Count) ;
        Application.ProcessMessages;
      end;
    end;
    ProgressBar1.position:= 100;
    if filtyp= TS then
    begin
      lastpcr:= pcrbase;
      t := (lastpcr div 90)/ MSecsPerDay;
      hms:= FormatDateTime('[h]:nn:ss.zzz', t, [fdoInterval]);
      memo1.Append(Format(sLastFilePCR, [CBtsfiles.Items[y], hms]));
      filesize:= filesize + endpos-beginpos;
      // on tronque le fichier
      fos.Size:= filesize;
    end;

    progress2:= ProgressBar2.position;
    if assigned(fts) then fts.Free;
  end;
  ProgressBar2.position:= 100;
  EndTime:= now;
  memo1.Append(Format(sMergeComplete, [FormatDateTime('[h]:nn:ss.zzz', EndTime-StartTime, [fdoInterval])]));
  memo1.Append(Format(sMergedFile, [EMergedTS.Text]));
  SBtnquit.Enabled:= true;
  SBtnMerge.Enabled:= true;
  SBtnSelectTSFiles.Enabled:= True;
  SBtnMergedTS.Enabled:= true;
  EMergedTS.Enabled:= true;
  if assigned(fos) then fos.Free;

end;

procedure TFMergeTS.SBtnQuitClick(Sender: TObject);
begin
  close;
end;

procedure TFMergeTS.SBtnSettingsClick(Sender: TObject);
var
  oldlng: Integer;
begin
  with FSettings do
  begin
    Edatafolder.Text := MTSAppDataPath;
    CBSavePos.Checked:= Settings.SavSizePos;
    CBNoChkNewVer.Checked:= Settings.NoChkNewVer;
    CBLangue.ItemIndex := LangNums.IndexOf(Settings.LangStr);
    oldlng := CBLangue.ItemIndex;
    if ShowModal <> mrOK then exit;
    Settings.SavSizePos := CBSavePos.Checked;
    Settings.NoChkNewVer := CBNoChkNewVer.Checked;
    Settings.LangStr := LangNums.Strings[CBLangue.ItemIndex];
    if (CBLangue.ItemIndex<>oldlng) then
    begin
      LangFile:= TBbIniFile.Create(ExtractFilePath(Application.ExeName) + 'lang'+PathDelim+Settings.LangStr+'.lng');
      self.Translate(LangFile);               // self is important !!! translate main form
      LVersion.Hint:= OSVersion.VerDetail;    // Need to change
    end;

  end;
end;

procedure TFMergeTS.TimerTimeTimer(Sender: TObject);
var
  s: string;
begin
  s:= FormatDateTime('dddd dd mmmm yyyy - hh:mm:ss', now);     // use TFormatsettings for translations, split to make arrays
  s[1]:= UpCase(S[1]);
  LTime.Caption:= s;
end;


procedure TFMergeTS.Button1Click(Sender: TObject);
var
  Buf: PByte;
  a: array of Byte =($00, $B0, $0D, $00, $06, $C7, $00, $00, $00, $01, $E0, $B1);
  crc: Cardinal;
begin

  Buf:= PByte(a);
  crc:= crc32_block($ffffffff, Buf, 12);
  Memo1.append(InttoHex(crc,4));
end;

procedure TFMergeTS.SBtnSelectTSFilesClick(Sender: TObject);
var
  filext: String;
begin
  CBtsfiles.Text:= '';
  SBtnMerge.Enabled:= false;
  if OD1.Execute then
  begin
    CBtsfiles.Items:= OD1.Files;
    CBtsfiles.ItemIndex:= 0;
    if CBtsfiles.Items.Count < 2 then      // We have selected only one file
    begin
      MsgDlg(Caption, sSelectedOneFile, mtError, [mbOK], [OKBtn]);
      memo1.Append(sSelectedOneFile);
      exit;
    end;
    Memo1.Lines.add(sFilesToMerge);
    Memo1.Lines.add(CBtsfiles.Items.Text);
    SBtnMerge.Enabled:= true;
    CBtsfiles.ItemIndex:= 0;
    CBtsfiles.Items[0];
    // Change merged file extension  to proper one
    filext:= ExtractFileExt(CBtsfiles.Items[0]);
    filext:= LowerCase(Copy(filext, 1, 3));
    if filext='.mt' then Filext:= '.mts';
    EMergedTS.text:= ChangeFileExt(EMergedTS.text, filext);
  end;
end;

procedure TFMergeTS.SBtnMergedTSClick(Sender: TObject);
begin
  SD1.Filename:= EMergedTS.Text;
  SD1.Execute;
  EMergedTS.Text:= SD1.Filename;
end;

procedure TFMergeTS.MnuItemChangeTimeClick(Sender: TObject);
begin
  MnuItemChangeTime.Checked:= not MnuItemChangeTime.Checked ;
  FSettings.Settings.TranslateTimeDate:= MnuItemChangeTime.Checked;
  Translate(LangFile);
end;





// Populate the CRC table. May safely be called more than once.
// The CRC Decoder Model specified in the Annex A of ISO/IEC 13818-1 is MSB (most significant bit first).
// Default polynomial is LSB (least significant bit first).
// You must use 0x04C11DB7 for the polynomial instead of the reversed form 0xEDB88320.r

procedure TFMergeTS.make_crc_table;
const
  CRC_POLY_32 = $04C11DB7; //$EDB88320;
var
  i, j: Integer;
  crc: Cardinal;
begin
  if (crc_table_done)then exit else
  begin
    for i:= 0 to 255 do
    begin
      crc := i shl 24;
      for j:= 0 to 7 do
      begin
        if (crc and $80000000) <> 0 then       //if (crc & 0x80000000L)
          crc := (crc shl 1) xor CRC_POLY_32  //crc = (crc << 1) ^ CRC32_POLY;
        else
          crc := crc shl 1;                    //crc = ( crc << 1 );
      end;
      crc_table[i]:= crc;
    end;
    crc_table_done:= true;
  end;
end;

// Compute CRC32 over a block of data, by table method.
// Returns a working value, suitable for re-input for further blocks
// Notes: Input value should be 0xffffffff for the first block,
//        else return value from previous call (not sure if that
//        needs complementing before being passed back in).

function TFMergeTS.crc32_block(crc: Cardinal; pData: PByte;blk_len: Integer): Cardinal;
var
  i, j: Integer;
begin
  if (not crc_table_done) then make_crc_table();  //if (!table_made) make_crc_table();
  for j:= 0 to blk_len-1 do
  begin
    i := ((crc shr 24) xor pData[j]) and  $ff;
    crc:= (crc shl 8) xor crc_table[i];
  end;
  result:= crc;
end;

procedure TFMergeTS.Translate(LngFile: TBbInifile);
var
  prgName: String;
  s: String;
  i: integer;
  a: array of String;
begin
  If Assigned(lngFile) then
  With LngFile do
  begin
    prgName:= ReadString('common', 'ProgName', 'Erreur');
    if prgName<>ProgName then ShowMessage(ReadString('common', 'ProgErr',
                         'Fichier de langue erroné. Réinstallez le programme'));
    OsVersion.Translate(LngFile);

     // general strings
    sRetConfBack:= ReadString('main','RetConfBack','Recharge la dernière configuration sauvegardée');
    sCreNewConf:= ReadString('main','CreNewConf','Création d''une nouvelle configuration');
    sLoadConf:= ReadString('main','LoadConf','Chargement de la configuration');
    OKBtn:= ReadString('common', 'OKBtn','OK');
    YesBtn:=ReadString('common','YesBtn','Oui');
    NoBtn:=ReadString('common','NoBtn','Non');
    CancelBtn:=ReadString('common','CancelBtn','Annuler');
    //Main Form  & components captions
    Caption:=ReadString('main','Caption','Fusion de fichiers TS');
    sUse64bitcaption:= ReadString('main', 'sUse64bitcaption)', 'Utilisez la version 64 bits de ce programme');

    SBtnMerge.Hint:=ReadString('main','SBtnMerge.Hint', SBtnMerge.Hint);
    SBtnSettings.Hint:=ReadString('main','SBtnSettings.Hint', SBtnSettings.Hint);
    SBtnAbout.Hint:=ReadString('main','SBtnAbout.Hint',SBtnAbout.Hint);
    SBtnQuit.Hint:=ReadString('main','SBtnQuit.Hint',SBtnQuit.Hint);
    Ltsfiles.Caption:= ReadString('main','Ltsfiles.Caption', Ltsfiles.Caption) ;
    LMergedTS.Caption:= ReadString('main','LMergedTS.Caption', LMergedTS.Caption);
    SBtnSelectTSFiles.Hint:=  ReadString('main','SBtnSelectTSFiles.Hint', SBtnSelectTSFiles.Hint);
    SBtnMergedTS.Hint:= ReadString('main','SBtnMergedTS.Hint', SBtnMergedTS.Hint);
    sCannotGetNewVerList:= ReadString('main','sCannotGetNewVerList','Liste des nouvelles versions indisponible');
    sCannotCreateVideoFolder:= ReadString('main','sCannotCreateVideoFolder', 'Impossible de créer le répertoire "Videos"');
    sAskReplaceFile:= ReadString('main','sAskReplaceFile', 'Le fichier "%s" existe, le remplacer ?') ;
    sExistingFile:= ReadString('main','sExistingFile', 'Fichier "%s" existant. Fusion abandonnée');
    sFilesToMerge:= ReadString('main','sFilesToMerge', 'Fichiers à fusionner');
    sSelectedOneFile:= ReadString('main','sSelectedOneFile', 'Vous n''avez sélectionné qu''un fichier. Fusion impossible !');
    sFileFormatNotSupported:= ReadString('main','sFileFormatNotSupported', 'Format de fichier non supporté');
    sCopyFile:= ReadString('main','sCopyFile', 'Copie du fichier "%s"');
    sLastFilePCR:= ReadString('main','sLastFilePCR', 'Dernier PCR du fichier %s : %s');
    sJoinFile:= ReadString('main','sJoinFile', 'Raccord du fichier "%s" au PCR %s');
    sMergeComplete:= ReadString('main','sMergeComplete', 'Fusion terminée en %s');
    sMergedFile:= ReadString('main','sMergedFile', 'Fichier fusionné : %s');
    MnuItemChangeTime.Caption:= ReadString('main','MnuItemChangeTime.Caption', MnuItemChangeTime.Caption);;
    LTime.Hint:= Format(ReadString('main','LTime.Hint', LTime.Hint),[#10]);
    If FSettings.Settings.TranslateTimeDate then
    begin
      s:= ReadString('main','DayNames','Dimanche,Lundi,Mardi,Mercredi,Jeudi,Vendredi,Samedi');
      a:= s.Split(',');
      for i:=1 to 7 do DefaultFormatSettings.LongDayNames[i]:= a[i-1];
      s:= ReadString('main','MonthNames','Janvier,Février,Mars,Avril,Mai,Juin,Juillet,Août,Septembre,Octobre,Novembre,Décembre');
      a:= s.Split(',');
      for i:=1 to 12 do DefaultFormatSettings.LongMonthNames[i]:= a[i-1];
    end else
    begin
      DefaultFormatSettings.LongMonthNames:= myLongMonth;
      DefaultFormatSettings.LongDayNames:= myLongDay;
    end;

    // About box
    AboutBox.LVersion.Hint:= OSVersion.VerDetail;
    AboutBox.Translate(LngFile);

    // Alert
    sUpdateAlertBox:=ReadString('main','sUpdateAlertBox','Version actuelle: %sUne nouvelle version %s est disponible. Cliquer pour la télécharger');
    sNoLongerChkUpdates:=ReadString('main','sNoLongerChkUpdates','Ne plus rechercher les mises à jour');


    //Settings
    FSettings.Translate(LngFile);
    FSettings.Lstatus.Caption:= OSVersion.VerDetail;

    // HTTP Error messages
    HttpErrMsgNames[0] := ReadString('HttpErr','SErrInvalidProtocol','Protocole "%s" invalide');
    HttpErrMsgNames[1] := ReadString('HttpErr','SErrReadingSocket','Erreur de lecture des données à partir du socket');
    HttpErrMsgNames[2] := ReadString('HttpErr','SErrInvalidProtocolVersion','Version de protocole invalide en réponse: %s');
    HttpErrMsgNames[3] := ReadString('HttpErr','SErrInvalidStatusCode','Code de statut de réponse invalide: %s');
    HttpErrMsgNames[4] := ReadString('HttpErr','SErrUnexpectedResponse','Code de statut de réponse non prévu: %s');
    HttpErrMsgNames[5] := ReadString('HttpErr','SErrChunkTooBig','Bloc trop grand');
    HttpErrMsgNames[6] := ReadString('HttpErr','SErrChunkLineEndMissing','Fin de ligne du bloc manquante');
    HttpErrMsgNames[7] := ReadString('HttpErr','SErrMaxRedirectsReached','Nombre maximum de redirections atteint: %s');
    // Socket error messages
    HttpErrMsgNames[8] := ReadString('HttpErr','strHostNotFound','Résolution du nom d''hôte pour "%s" impossible.');
    HttpErrMsgNames[9] := ReadString('HttpErr','strSocketCreationFailed','Echec de la création du socket: %s');
    HttpErrMsgNames[10] := ReadString('HttpErr','strSocketBindFailed','Echec de liaison du socket: %s');
    HttpErrMsgNames[11] := ReadString('HttpErr','strSocketListenFailed','Echec de l''écoute sur le port n° %s, erreur %s');
    HttpErrMsgNames[12]:=ReadString('HttpErr','strSocketConnectFailed','Echec de la connexion à %s');
    HttpErrMsgNames[13]:=ReadString('HttpErr','strSocketAcceptFailed','Connexion refusée d''un client sur le socket: %s, erreur %s');
    HttpErrMsgNames[14]:=ReadString('HttpErr','strSocketAcceptWouldBlock','La connexion pourrait bloquer le socket: %s');
    HttpErrMsgNames[15]:=ReadString('HttpErr','strSocketIOTimeOut','Impossible de fixer le timeout E/S à %s');
    HttpErrMsgNames[16]:=ReadString('HttpErr','strErrNoStream','Flux du socket non assigné');

  end;
end;

end.

