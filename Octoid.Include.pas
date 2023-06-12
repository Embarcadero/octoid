unit Octoid.Include;

interface

// Not sure why this needs to appear again here. It is already in Octoid.Debug.inc, which is in the project source
{.$DEFINE FastMM5}

{$IFDEF FastMM5}
uses
  FastMM5;
{$ENDIF}

implementation

end.
