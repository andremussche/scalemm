program SimpleChallengeScaleMM3;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MMTests in 'MMTests.pas',
  ScaleMM3 in 'ScaleMM3.pas';

begin
  try
//    ExecuteTests('D2010');

//    TCmallocMM.Install;
//    ExecuteTests('TCmallocMM');

//    JemallocFFMM.Install;
//    ExecuteTests('JemallocFFMM');
//
//    JemallocMM.Install;
//    ExecuteTests('JemallocMM');
//
//    HoardMM.Install;
//    ExecuteTests('HoardMM');
//
//    msvcrtMM.Install;
//    ExecuteTests('msvcrtMM');

    ScaleMM3.ScaleMMInstall;
    ExecuteTests('ScaleMM3');
//
//    TopInstall.TopMMInstall;
//    ExecuteTests('TopMM');

    Halt;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
