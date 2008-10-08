{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar

  RuntimeSafety provides means of debugging runtime problems, or preventing said
  problems.

  For instance, Delphi's mechanism of informing of abstract errors is woefully
  inadequate. Should one use Factory patterns, it's not possible to warn of
  possiblly unimplemented abstract methods at compile time. Thus one can only
  find out about these methods at runtime, via an EAbstractError. However,
  Delphi doesn't tell one _which_ method is not implemented!

  Thus, don't ever use an abstract method. Make the method virtual, and in its
  body call RaiseAbstractError. This will result in a meaningful error message.
}
unit RuntimeSafety;

interface

procedure RaiseAbstractError(ClassName, MethodName: String);

resourcestring
  AbstractErrorMsg = '%s must override %s';

implementation

uses
  SysUtils;

procedure RaiseAbstractError(ClassName, MethodName: String);
begin
  raise EAbstractError.Create(Format(AbstractErrorMsg, [ClassName, MethodName]));
end;

end.
