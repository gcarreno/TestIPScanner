unit Data.Scan;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils
;

type
  PScanEntry = ^TScanEntry;
  TScanEntry = record
    IP: String;
    Status: String;
  end;

implementation

end.

