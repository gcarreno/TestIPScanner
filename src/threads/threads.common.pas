unit Threads.Common;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils
, laz.VirtualTrees
;

type
{ TOnScanUpdate }
  TOnScanUpdate = procedure(const ANode: PVirtualNode;
    const AStatus: String) of object;

implementation

end.

