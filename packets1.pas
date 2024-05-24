//******************************************************************************
// packets1 unit : Read and write TS/MTS packets headers
// bb - sdtp - March 2024
//******************************************************************************

unit packets1;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, definitions;

type
  tstype = (ERR, TS, MTS);

  TSArray = Array [0..191] of Byte;

  TAdapt_Field = record
     alen: Integer;             // 8 bits  Number of bytes in the adaptation field immediately following this byte
     adisci: Integer;           // 1 bit Discontinuity indicator 1 0x80 set if current TS packet is in a discontinuity state the continuity counter or the program clock reference
     aRandi: Integer;           // Random access indicator 1 0x40 Set when the stream may be decoded without errors from this point
     aElemi: Integer;           // Elementary stream priority indicator 1 0x20 	Set when this stream should be considered "high priority"
     aPCRf: Integer;            // PCR flag 1 0x10  Set when PCR field is present
     aOPCRf: Integer;           // OPCR flag 1 0x08 Set when OPCR field is present
     aspcf: Integer;            // Splicing point flag 	1 0x04 	Set when splice countdown field is present
     atpdf: Integer;            //Transport private data flag1 0x02 Set when transport private data is present
     aextf: Integer;            // Adaptation field extension flag 1 0x01 Set when adaptation extension data is present
     pcr: int64;
  end;

  TPrograms = record
    Program_num: Integer; 	//16 	Relates to the Table ID extension in the associated PMT. A value of 0 is reserved for a NIT packet identifier.
    Reserved_bits: Byte;        //3 	Set to 0x07 (all bits on)
    Program_map_PID: Integer;    //13 	The packet identifier that contains the associated PMT
  end;

  TPAT = record
    Is_Pat: Boolean;
    Pointer_Field: Byte;                  //8 Present at the start of the TS packet payload signaled by the payload_unit_start_indicator bit in the TS header.
    Pointer_filter_Bytes: array of Byte;  //When the pointer field is non-zero, this is the pointer field number of alignment padding bytes set to 0xFF
                                          //or the end of the previous table section spanning across TS packets (electronic program guide).
    Table_ID: Byte;                       //8  Table Identifier, that defines the structure of the syntax section and other contained data.
    Section_syntax_indicator: Byte;       //1 A flag that indicates if the syntax section follows the section length. The PAT, PMT, and CAT all set this to 1
    Private_bit: Byte;                    //1 The PAT, PMT, and CAT all set this to 0. Other tables set this to 1.
    Reserved_bits: Byte;                  //3 Set to 0x03 (all bits on)
    Section_length_unused_bits: Byte;     //2 Set to 0 (all bits off)
    Section_length: Integer;              //10 The number of bytes that follow for the syntax section (with CRC value) and/or table data.
    Section_table: array of Byte;         // When the section length is non-zero, this is the section length number of syntax and data bytes.
    Table_ID_extension: Integer;          // 2 byte	Informational only identifier. The PAT uses this for the transport stream identifier and the
                                          //PMT uses this for the Program number.
    Reserved_bits_Ex: Byte;                  //2 Set to 0x03 (all bits on)
    Version_number: Byte;                 //5 Syntax version number. Incremented when data is changed and wrapped around on overflow for values greater than 32.
    Cur_next_indicator: Byte;             //1 Indicates if data is current (1) in effect or is for future use (0)
    Section_number: Byte;
    Last_section_number: Byte;
    Progs_number: Integer;
    Progs: Array of TPrograms;
    Pmts: array of Integer;
  end;



  // Video :     Stream #0:0[0xdc]: Video: h264 (High), yuv420p(tv, bt709, top first), 1920x1080 [SAR 1:1 DAR 16:9], 25 fps, 25 tbr, 90k tbn, 50 tbc

{  Stream type 0
Rsrv 6
Elementary PID 0
Rsrv 7
ESIL 0
ES info length 0
AVC Video descriptor 0
Descriptor length 0
IDC profile 0
cs0 - cs5
AVCf0
IDC Level 0
sp
24h
Rsrv 8
ISO Language 0
Lang Text length 0
Language    }

  TElem_Stream= record
    Prog_Num: Integer;             // For use with ffprobe
    ES_Num: Integer;               // Id.
    ES_sdef: String;               // short definition
    ES_ldef: String;               // long definition
    ES_Type: Byte;
    ES_Rsrv_0: Byte;               // 3 bits high of first PID byte :111
    ES_PID: Integer;               // 13 bits PID
    ES_Rsrv_1: Byte;               // 4 bits high of first ESIL byte : 1111
    ES_IL_Rsrv: Byte;              // 2 bits next previous 00
    ES_info_length: Integer;       // 10 bits
    ES_Descriptor: Byte;
    ES_Desc_length: Byte;
    ES_IDC_profile: Byte;
    ES_Constrained_flags: Byte;    // 6 bits
    ES_AVC_compat_flag: Byte;      // 2 bits
    ES_IDC_level: Byte;            //  decimal divided by 10 to get level
    ES_sp: Byte;                   // 1 bit
    ES_24h: Byte;                  //1 bit
    ES_ISO_language: Byte;
    ES_Lang_text_length: Byte;
    ES_lang_text: Pchar;            //
  end;

  TPMT = record
    Is_Pmt: Boolean;
    Pointer_Field: Byte;                  //8 Present at the start of the TS packet payload signaled by the payload_unit_start_indicator bit in the TS header.
    Pointer_filter_Bytes: array of Byte;  //When the pointer field is non-zero, this is the pointer field number of alignment padding bytes set to 0xFF
                                          //or the end of the previous table section spanning across TS packets (electronic program guide).
    Table_ID: Byte;                       //8  Table Identifier, that defines the structure of the syntax section and other contained data.
    Section_syntax_indicator: Byte;       //1 A flag that indicates if the syntax section follows the section length. The PAT, PMT, and CAT all set this to 1
    Private_bit: Byte;                    //1 The PAT, PMT, and CAT all set this to 0. Other tables set this to 1.
    Reserved_bits: Byte;                  //3 Set to 0x03 (all bits on)
    Section_length_unused_bits: Byte;     //2 Set to 0 (all bits off)
    Section_length: Integer;              //10 The number of bytes that follow for the syntax section (with CRC value) and/or table data.
    Program_Number: Integer;              //16
    Reserved_bits_Ex: Byte;               //2 Set to 0x03 (all bits on)
    Version_number: Byte;
    Cur_next_indicator: Byte;
    Section_number: Byte;
    Last_section_number: Byte;
    Reserved3: byte;                        //3
    PCR_PID: Integer;                       //13
    reserved4: Byte;			    //4
    program_info_length: Integer;      	    //12
    aElem_Stream: array of TElem_Stream;
  end;


  {https://en.wikipedia.org/wiki/Program-specific_information
  Baseline profile_idc = 66 = 0x42, constraint_set1_flag = 0
  Constrained Baseline profile_idc = 66 = 0x42, constraint_set1_flag = 1
  Main profile_idc = 77 = 0x4D
  Extended profile_idc = 88 = 0x58
  High profile_idc = 100 = 0x64
    }

  {pointer_field – This is an 8-bit field whose value shall be the number of bytes,
   immediately following the pointer_field until the first byte of the first section
   that is present in the payload of the Transport Stream packet (so a value of 0x00
   in the pointer_field indicates that the section starts immediately after the pointer_field).
   When at least one section begins in a given Transport Stream packet,
   then the payload_unit_start_indicator (refer to 2.4.3.2) shall be set to 1 and
   the first byte of the payload of that Transport Stream packet shall contain the pointer.
   When no section begins in a given Transport Stream packet, then the payload_unit_start_indicator
   shall be set to 0 and no pointer shall be sent in the payload of that packet. }


  TSPacket = class
  private
    FOnChange: TNotifyEvent;
    // Extra M2TS Header
    fts_type: tstype;               // TS or MTS
    fts_length: Integer;            // Packet Length TS: 188 byte; MTS: 192 byte
    fcopy_perm_indic: Integer;      //2 bits
    farrival_timestamp: Double;     // 30 bit
    // TS Header
    fSyncByte : Byte;               // $47
    ftei: Integer;                  // transport error indicator 1 bit -  $80
    fpusi: Integer;                 // Payload unit start indicator 1 bit - $40
    ftpriority: Integer;            // Transport priority  1 bit - $20
    fPID: Integer;                  //  PID $1fff
    ftsc: Integer;                  // Transport scrambling control 0xc0 '00' = Not scrambled.  2 clé paire, 3 clé impaire
    fafc: Integer;                  // Adaptation field control  0x30  01 rien - 02 oui, pas d epayload  03 oui payload
    fcntc: Integer;                 // continuity counter
    ofset: Integer;
   // End of header
    procedure check_ts(Buffer: array of Byte);
    procedure set_ts_type(tst: tstype);
    procedure set_copy_perm_indic(i: Integer);  // 0: unencrypted, 1, 2: Reserved, 3: crypted
    procedure set_arrival_timestamp(d: double);
    procedure set_tei(i: Integer);
    procedure set_pusi(i: Integer);
    procedure set_tpriority(i: Integer);
    procedure set_PID(i: Integer);
    procedure set_tsc(i: Integer);
    procedure set_afc(i: Integer);
    procedure set_cntc(i: Integer);

  public
    Adapt_Field: TAdapt_Field;
    PAT: TPAT;
    aPMTPID: Array of Integer;
    PMT: TPMT;
    data: TSArray;

    constructor Create ;
    function ReadData(typ: tstype= ERR): Integer;
    function GetPAT: Boolean;
    function GetPMT: Boolean;
    function GetAdaptField: Boolean;
    function Write: TSArray;
    destructor Destroy;
    //property data: TSArray read fdata write setdata;
  published
    property ts_type: tstype read fts_type write set_ts_type;
    property ts_length: Integer read fts_length;
    property copy_perm_indic: Integer read fcopy_perm_indic write set_copy_perm_indic;
    property arrival_timestamp: Double read farrival_timestamp write set_arrival_timestamp;
    property tei: Integer read ftei write set_tei;
    property pusi: Integer read fpusi write set_pusi;
    property tpriority: Integer read ftpriority write set_tpriority;
    property PID: Integer read fPID write Set_PID;
    property tsc: Integer read ftsc write set_tsc;
    property afc: Integer read fafc write set_afc;
    property cntc: Integer read fcntc write set_cntc;

    //property Adsapt_field: TAdapt_Field read fAdsapt_field;
  end;

implementation

constructor TSPacket.Create;
begin
  inherited Create;
  fts_type:= TS;
  fts_length:= 188;
  fcopy_perm_indic:= 0;
  farrival_timestamp:= 0;
  fSyncByte:= $47;
  fTei:= 0;
  fpusi:= 0;
  ftpriority:= 0;
  fPID:= 0;
  ftsc:= 0;
  fafc:= 0;
  fcntc:= 0;
  Adapt_field:= Default(TAdapt_Field);
  data:= Default(TSArray);
  PAT:= Default(TPAT);
  PMT:= Default(TPMT);
end;

// Read TS packet. Return 0 if error

function TSPacket.ReadData(typ: tstype= ERR): Integer;
var
  //ofset: Integer;
  i: Integer;
  tmp: int64;
begin
  ofset:= 0;
  fts_length:= 0;
  fts_type:= typ;
  if typ= ERR then
  begin
    if (data[0]=$47) then fts_type:= TS
    else if (data[4]=$47) then fts_type:= MTS;
  end;
  Case fts_type of
    TS: fts_length:= 188;
    MTS: begin
      ofset:= 4;
      fts_length:= 192;
      fcopy_perm_indic:= (data[0] and $C) shr 6;
      // timestamp div 27 MHz to seconds
      farrival_timestamp:= (data[3]+(data[2] shl 8)+(data[1] shl 16)+(data[0] shl 24)) and $3FFFFFFF;
      farrival_timestamp:=  farrival_timestamp / 27000000;
    end;
    else exit;
  end;
  // Read TS Header
  fTei:= data[1+ofset] and $80 shr 7;
  fpusi:= data[1+ofset] and $40 shr 6;
  ftpriority:= data[1+ofset] and $20 shr 5;
  fPID:= data[2+ofset]+(data[1+ofset] and $1F) shl 8;
  ftsc:= (data[3+ofset] and $C0 shl 6);
  fafc:= (data[3+ofset] and $30 shl 4);    //Adaptation field control   10b no payload 11b with payload
  fcntc:=  data[3+ofset] and $F;           // Continuity counter
  // Adaptation field moved to procedure
  if fPID = 0 then GetPAT else
  begin
    if (fpusi=1) then
    for i:= 0 to PAT.Progs_number-1 do
       if fPID = PAT.Pmts[i]  then
       begin
         GetPMT;
         break;
       end;
  end;
  result:= fts_length;
end;

function TSPacket.GetPAT: Boolean;
var
  Patofset: Integer;
  progsnum: Integer;
  i: integer;
begin
  result:= false;
  if fPID = 0 then
  begin
    PAT:= Default(TPAT);
    PAT.Is_Pat:= true;
    PAT.Pointer_Field:= data[4+ofset];
    PatOfset:= ofset+PAT.Pointer_Field;                              //if PAT.Pointer_Field >0 change offset
    PAT.Table_ID:= data[5+Patofset];
    PAT.Section_syntax_indicator:= data[6+Patofset] and $80 shr 7;   // must be 1 for pat, pmt and cat
    PAT.Private_bit:= data[6+Patofset] and $40 shr 6;                //The PAT, PMT, and CAT all set this to 0. Other tables set this to 1.
    PAT.Reserved_bits:= data[6+Patofset] and $30 shr 4;              //Set to 0x03 (all bits on)
    PAT.Section_length_unused_bits:= data[6+Patofset] and $0C shr 2; // 	Set to 0 (all bits off)
    PAT.Section_length:= (data[6+Patofset] and $3 shl 8) + data[7+Patofset] ;
    If PAT.Section_length>0 then
    begin
      PAT.Table_ID_extension:= (data[8+Patofset] shl 8) + data[9+Patofset];
      PAT.Reserved_bits_Ex:= data[10+Patofset] and $C0 shr 6;            //Set to 0x03 (all bits on)
      PAT.Version_number:= data[10+Patofset] and $3E shr 1;
      PAT.Cur_next_indicator:= data[10+Patofset] and $1;
      PAT.Section_number:= data[11+Patofset] ;
      PAT.Last_section_number:= data[12+Patofset] ;
      // Enumerate programs (4 byte) section header 5 byte, CRC 4 byte must subsreact 9 tu section leght to get progfram lis length
      PAT.Progs_number:= (PAT.Section_length-9) div 4;
      if PAT.Progs_number >0 then
      begin
        SetLength(PAT.Progs, PAT.Progs_number);
        SetLength(PAT.Pmts,PAT.Progs_number);
        for i:= 0 to PAT.Progs_number -1 do
        begin
          PAT.Progs[i].Program_num:= (data[13+(i*4)] shl 8) + data[14+(i*4)] ;
          PAT.Progs[i].Reserved_bits:= data[15+(i*4)] and $E0 shr 5;
          PAT.Progs[i].Program_map_PID:= (data[15+(i*4)] and $1F shl 8) + data[16+(i*4)] ;
          PAT.Pmts[i]:= PAT.Progs[i].Program_map_PID;
        end;
      end;
    end;
    result:= true;
  end;
end;

function TSPacket.GetPMT: Boolean;
var
  Patofset: Integer;
  remain_bytes: Integer;
begin
  result:= False;
  PMT:= Default(TPMT);
  PMT.Pointer_Field:= data[4+ofset];
  PatOfset:= ofset+PMT.Pointer_Field;                              //if PMT.Pointer_Field >0 change offset
  PMT.Table_ID:= data[5+Patofset];
  if PMT.Table_ID =2 then
  begin
    PMT.Is_Pmt:= true;
    PMT.Section_syntax_indicator:= data[6+Patofset] and $80 shr 7;   // must be 1 for pat, pmt and cat
    PMT.Private_bit:= data[6+Patofset] and $40 shr 6;                //The PAT, PMT, and CAT all set this to 0. Other tables set this to 1.
    PMT.Reserved_bits:= data[6+Patofset] and $30 shr 4;              //Set to 0x03 (all bits on)
    PMT.Section_length_unused_bits:= data[6+Patofset] and $0C shr 2; // 	Set to 0 (all bits off)
    PMT.Section_length:= (data[6+Patofset] and $3 shl 8) + data[7+Patofset] ;
    If PMT.Section_length>0 then
    begin

      PMT.Program_Number:= (data[8+Patofset] shl 8) + data[9+Patofset];
      PMT.Reserved_bits_Ex:= data[10+Patofset] and $C0 shr 6;            //Set to 0x03 (all bits on)
      PMT.Version_number:= data[10+Patofset] and $3E shr 1;
      PMT.Cur_next_indicator:= data[10+Patofset] and $1;
      PMT.Section_number:= data[11+Patofset] ;
      PMT.Last_section_number:= data[12+Patofset] ;
      PMT.Reserved3:= data[13+Patofset] and $E0 shr 5;                       //3
      PMT.PCR_PID:= (data[13+Patofset] and $1F shl 8) + data[14+Patofset] ;                          //13
      PMT.reserved4:= data[15+Patofset] and $F0 shr 4;			      //4
      PMT.program_info_length:= (data[15+Patofset] and $0F shl 8) + data[16+Patofset];      	      //12
      if PMT.program_info_length=0 then
      begin
        Remain_bytes:= PMT.Section_length-9;
        setLength(PMT.aElem_Stream, 1);
        PMT.aElem_Stream[0].ES_Type:= data[17+Patofset];
        PMT.aElem_Stream[0].ES_sdef:= STREAM_TYPE_TABLE [PMT.aElem_Stream[0].ES_Type].sdes;
        PMT.aElem_Stream[0].ES_ldef:= STREAM_TYPE_TABLE [PMT.aElem_Stream[0].ES_Type].ldes;
        PMT.aElem_Stream[0].ES_Rsrv_0:= data[18+Patofset] and $E0 shr 5;
        PMT.aElem_Stream[0].ES_PID:= (data[18+Patofset] and $1F shl 8) + data[19+Patofset];
        PMT.aElem_Stream[0].ES_Rsrv_1:= data[20+Patofset] and $F0 shr 4;
        PMT.aElem_Stream[0].ES_IL_Rsrv:= data[20+Patofset] and $C shr 2;
        PMT.aElem_Stream[0].ES_info_length:= (data[20+Patofset] and $3 shl 8) + data[21+Patofset];
        PMT.aElem_Stream[0].ES_Descriptor:= data[22+Patofset];

        Remain_bytes:= Remain_bytes-5-PMT.aElem_Stream[0].ES_info_length;
      end;
    end;

    result:= true;
  end;



end;

function TSPacket.GetAdaptField: Boolean;
var
    tmp: int64;
    i: Integer;
begin
  // Adaptation field
  result:= False;
  Adapt_field:= Default(TAdapt_Field);
  if fafc>1 then
  begin
    Adapt_Field.alen:= data[4+ofset];
    Adapt_Field.adisci:= (data[5+ofset] and $80) shr 7;
    Adapt_Field.aRandi:= (data[5+ofset] and $40) shr 6;
    Adapt_Field.aElemi:= (data[5+ofset] and $20) shr 5;
    Adapt_Field.aPCRf := (data[5+ofset] and $10) shr 4;
    Adapt_Field.aOPCRf:= (data[5+ofset] and $8) shr 3;

    if Adapt_Field.aPCRf > 0 then         // Get PCR
    begin // Calcul de la valeur du PCR
      tmp:=0;
      for i:= 6 to 10 do                           //(int i = 6; i < 11; i++)
      begin
        tmp:= tmp shl 8;		           //tmp <<= 8;
        tmp:= tmp or (data[i+ofset] and $0FF);   //tmp |= (mypaq[i] & 0xFF);
      end;
      Adapt_Field.pcr:= tmp shr 7; //33 bits
    end;
    result:= true;
  end;
end;

function TSPacket.Write: TSArray;
begin

end;

procedure TSPacket.set_ts_type(tst: tstype);
begin
  if tst = fts_type then exit;
  fts_type:= tst;
  if fts_type= TS then fts_length:= 188
  else fts_length:= 192;
  //if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSPacket.set_copy_perm_indic(i: Integer);
begin
  if i = fcopy_perm_indic then exit;
  fcopy_perm_indic:= i;
  //if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSPacket.set_arrival_timestamp(d: double);
begin
  if d = farrival_timestamp then exit;
  farrival_timestamp:= d;
  //if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSPacket.set_tei(i: Integer);
begin
  if i <> ftei then exit;
  ftei:= i;
  //if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSPacket.set_pusi(i: Integer);
begin
  if i <> fpusi then exit;
  fpusi:= i;
  //if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSPacket.set_tpriority(i: Integer);
begin
  if i <> ftpriority then exit;
  ftpriority:= i;
  //if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSPacket.set_PID (i: Integer);
begin
  if i <> fPID then exit;
  fPID:= i;
  //if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSPacket.set_tsc(i: Integer);
begin
  if i <> ftsc then exit;
  ftsc:= i;
  //if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSPacket.set_afc(i: Integer);
begin
  if i <> fafc then exit;
  fafc:= i;
  //if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSPacket.set_cntc(i: Integer);
begin
  if i <> fcntc then exit;
  fcntc:= i;
  //if Assigned(FOnChange) then FOnChange(Self);
end;


// check if packet                         // Obsolete
procedure TSPacket.check_ts(Buffer: array of Byte);
begin
  fts_type:= ERR;
  fts_length:= 0;
  fcopy_perm_indic:=0;
  farrival_timestamp:=0;
  if (Buffer[0]=$47) and (length(Buffer)=188) then
  begin
    fts_type:= TS;
    fts_length:= 188;
  end else if (Buffer[4]=$47) and (length(Buffer)=192) then
  begin
    fts_type:= MTS;
    fts_length:= 192;
  end;
end;


destructor TSPacket.Destroy;
begin
  inherited;
end;

end.

