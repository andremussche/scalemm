program SimpleChallenge;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MMTests in 'MMTests.pas',
  JemallocFFMM in 'JemallocFFMM.pas',
  JemallocMM in 'JemallocMM.pas',
  HoardMM in 'HoardMM.pas',
  msvcrtMM in 'msvcrtMM.pas',
  TopMemory in 'TopMemory.pas',
  TopInstall in 'TopInstall.pas',
  ScaleMM2 in 'smm2\ScaleMM2.pas';

procedure ExecuteTests(const aName: string);
var
  tDuration_msec: Double;
begin
  tDuration_msec := MMTests.ExecuteAllTests_msec(1);
  Writeln(Format('%s, 1 thread  = %4.2f',[aName, tDuration_msec]));
  tDuration_msec := MMTests.ExecuteAllTests_msec(2);
  Writeln(Format('%s, 2 threads = %4.2f',[aName, tDuration_msec]));
  tDuration_msec := MMTests.ExecuteAllTests_msec(4);
  Writeln(Format('%s, 4 threads = %4.2f',[aName, tDuration_msec]));
  tDuration_msec := MMTests.ExecuteAllTests_msec(8);
  Writeln(Format('%s, 8 threads = %4.2f',[aName, tDuration_msec]));

  Writeln('');
end;

begin
  try
    ExecuteTests('D2010');
    Exit;

//    TCmallocMM.Install;
//    ExecuteTests('TCmallocMM');

    JemallocFFMM.Install;
    ExecuteTests('JemallocFFMM');

    JemallocMM.Install;
    ExecuteTests('JemallocMM');

    HoardMM.Install;
    ExecuteTests('HoardMM');

    msvcrtMM.Install;
    ExecuteTests('msvcrtMM');

    ScaleMM2.ScaleMMInstall;
    ExecuteTests('ScaleMM2');

    TopInstall.TopMMInstall;
    ExecuteTests('TopMM');

    Halt;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
