library subdll;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{$ifNdef MMSharingEnabled}MMSharingEnabled must be defined!{$endif}
{$ifNdef MMSharingDLLOwnerEnabled}MMSharingDLLOwnerEnabled must be defined!{$endif}

uses
  ScaleMM2 in '..\ScaleMM2.pas',
  System.SysUtils,
  System.Classes,
  Shared.Types in 'Shared.Types.pas',
  Shared.Actions in 'Shared.Actions.pas';

{$R *.res}

function TestActions: ITestActions;
begin
  Result := TTestActions.Create;
end;

exports
  TestActions;

begin
  Assert(not ScaleMMIsMemoryManagerOwner);

  TThread.CreateAnonymousThread(
    procedure
    var
      s: string;
      i: Integer;
    begin
      TThread.NameThreadForDebugging('SubDll.AnonymousThread');

      i := 0;
      repeat
        Inc(i);
        s := 'test' + i.ToString;
        Sleep(1);
      until False;
    end).Start;
end.
