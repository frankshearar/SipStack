unit IdSipTransaction;

interface

uses
  IdSipParser;

type
  TIdInviteTransactionStates = (itsCalling, itsProceeding, itsCompleted, itsTerminated);

  TIdSipClientTransaction = class(TObject)
  end;

  TIdSipClientInviteTransaction = class(TObject)
  private
    fState: TIdInviteTransactionStates;
  public
    property State: TIdInviteTransactionStates read fState write fState;
  end;

  TIdSipClientNonInviteTransaction = class(TObject)
  end;

  TIdSipServerTransaction = class(TObject)
  end;

implementation

end.
