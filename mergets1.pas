unit mergets1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Buttons, lazbbutils ;

type

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
    BtnMerge: TButton;
    Btnquit: TButton;
    CBtsfiles: TComboBox;
    EMergedTS: TEdit;
    Ltsfiles: TLabel;
    LMergedTS: TLabel;
    Memo1: TMemo;
    OD1: TOpenDialog;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    SD1: TSaveDialog;
    SBTSFiles: TSpeedButton;
    BtnMergedTS: TSpeedButton;
    procedure BtnMergeClick(Sender: TObject);
    procedure BtnMergedTSClick(Sender: TObject);
    procedure BtnquitClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SBTSFilesClick(Sender: TObject);
  private
    UserPath: String;
  public

  end;

var
  FMergeTS: TFMergeTS;

implementation

{$R *.lfm}

{ TFMergeTS }

procedure TFMergeTS.FormActivate(Sender: TObject);
begin
  UserPath := GetUserDir;
  CBtsfiles.Text:= '';
  Memo1.Text:='';
  SD1.InitialDir:= Userpath+'Videos';
  EMergedTS.Text:= Userpath+'Videos\merged.ts';
end;

procedure TFMergeTS.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TFMergeTS.BtnquitClick(Sender: TObject);
begin
  close;
end;

procedure TFMergeTS.SBTSFilesClick(Sender: TObject);
begin
  CBtsfiles.Text:= '';
  BtnMerge.Enabled:= false;
  if OD1.Execute then
  begin
    CBtsfiles.Items:= OD1.Files;
    CBtsfiles.ItemIndex:= 0;
    if CBtsfiles.Items.Count < 2 then      // We have selected only one file
    begin
      ShowMessage('Vous n''avez sélectionné qu''un fichier. Fusion impossible !');
      exit;
    end;
    Memo1.Lines.add('Fichiers à fusioner :');
    Memo1.Lines.add(CBtsfiles.Items.Text);
    BtnMerge.Enabled:= true;
  end;
end;

procedure TFMergeTS.BtnMergedTSClick(Sender: TObject);
begin
  SD1.Filename:= EMergedTS.Text;
  SD1.Execute;
  EMergedTS.Text:= SD1.Filename;
end;

procedure TFMergeTS.BtnMergeClick(Sender: TObject);
var
  mypaq : array [0..187] of byte;
  mypacket: TSPacket;
  pcrbase, lastpcr: Int64;
  tsread, tspos: Int64;
  pcrbeg: Boolean;
  tmp: Int64;
  tssize, filesize: Int64;
  beginpos, endpos : Int64;
  progress1, progress2 : LongInt;
  fis: TMemoryStream;
  fos: TFileStream;
  x, y: LongInt;
  i: integer;
  t: double;
  hms: String;
begin
  if FileExists(EMergedTS.Text) then
  begin
    if MsgDlg(Caption, 'Le fichier existe, le remplacer ?', mtWarning, [mbYes, mbNo], ['Oui', 'Non'])= mrNo then
    begin
      Memo1.Lines.Add('Fichier '+EMergedTS.Text+' existant. Fusion abandonnée');
      exit;
    end else
    begin
      DeleteFile (EMergedTS.Text);

    end;
  end;
  fos:= TFileStream.Create(EMergedTS.Text, fmCreate);
  //mypacket:= TSPacket.Create;
  mypacket:= Default(TSPacket);
  pcrbase:= 0;
  lastpcr:= 0;
  endpos:= 0;
  filesize:=0;
  ProgressBar2.position:= 0;
  progress2:= 0;
  // Now, we process each TS file
  for y:= 0 to CBtsfiles.Items.Count-1 do
  begin
    beginpos:= 0;
    pcrbeg:= false;
    memo1.lines.add('Analyse du fichier '+CBtsfiles.Items[y]);
    Progressbar1.Position:= 0;
    tsread:= 0;
    tspos:= 0;
    // Open ts file
    fis:= TMemoryStream.Create;
    Fis.LoadFromFile(CBtsfiles.Items[y]);
    tssize:= fis.Size div 188;
    for x:= 0 to tssize-1 do
    begin
      //fis.Position:= 0;
      tsread:= fis.Read(mypaq, 188);
      //ProgressBar1.Position:= Round(100*x / tssize);
      //Progressbar2.position:= progress2+ Round(ProgressBar1.Position/CBtsfiles.Items.Count) ;
      mypacket.adapfield:=  ((mypaq[3] and $0ff) and $030) shr 4;              //(mypaq[3]&0xff & 0x30) >> 4;
      if mypacket.adapfield > 1 then
      begin
        mypacket.pcrflag:= (((mypaq[5] and $010) shr 4) <> 0);                 //(((mypaq[5] & 0x10) >> 4) != 0);
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
	  mypacket.pcr:= tmp shr 7; //33 bits}
          tmp:= (mypaq[6] and $0ff)* $1000000+(mypaq[7] and $0ff)* $10000+(mypaq[8] and $0ff)* $100+(mypaq[9] and $0ff) ;
	  mypacket.pcr:= (tmp*$100+(mypaq[10] and $0ff)) shr 7; //33 bits
          pcrbase:= mypacket.pcr;
          if ((y > 0) and (not pcrbeg) and (pcrbase = lastpcr)) then
          begin
	    beginpos:= tspos;
	    pcrbeg:= true;
	    mypacket.pcrtime:= (pcrbase div 90); // convert to milliseconds
            t := mypacket.pcrtime/ MSecsPerDay;
            hms:= FormatDateTime('[h]:nn:ss.zzz', t, [fdoInterval]);
	    memo1.append ('Raccord du fichier'+CBtsfiles.Items[y]+' au PCR '+hms); //                   //tsfiles[y].getName()+" au PCR : "+hms+"\n";
	  end;
	end;

      end;
      if ((y=0) or pcrbeg) then fos.write(mypaq, 188);         	// if (y==0 || pcrbeg) outChan.write(bpaq);
      tspos:= tspos+tsread;                                             //	pos+= read;
      ProgressBar1.Position:= Round(100*x / tssize);
      Progressbar2.position:= progress2+ Round(ProgressBar1.Position/CBtsfiles.Items.Count) ;
    end;
    lastpcr:= pcrbase;
    t := (lastpcr div 90)/ MSecsPerDay;
    hms:= FormatDateTime('[h]:nn:ss.zzz', t, [fdoInterval]);
    memo1.append('Dernier PCR du fichier '+CBtsfiles.Items[y]+': '+hms); //+ millitotime((int) (lastpcr/90))+"\n");
    filesize:= filesize + endpos-beginpos;
    //fos.Position:= filesize;					// on tronque le fichier
    fos.Size:= filesize;					//outChan.position(filesize);
					//outChan.truncate(filesize);

    progress2:= ProgressBar2.position;
    if assigned(fis) then fis.Free;
  end;
  //ProgressBar2.position:= 100;

  //if assigned(mypacket) then mypacket.Free;
  if assigned(fos) then fos.Free;
end;




end.

