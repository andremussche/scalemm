program SimpleChallengeMSvcrt;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MMTests in 'MMTests.pas',
//  JemallocFFMM in 'JemallocFFMM.pas',
//  JemallocMM in 'JemallocMM.pas',
//  HoardMM in 'HoardMM.pas',
  msvcrtMM in 'msvcrtMM.pas';
//  TopMemory in 'TopMemory.pas',
//  TopInstall in 'TopInstall.pas',
//  ScaleMM2 in 'smm2\ScaleMM2.pas';

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
    msvcrtMM.Install;
    ExecuteTests('msvcrtMM');
//
//    ScaleMM2.ScaleMMInstall;
//    ExecuteTests('ScaleMM2');
//
//    TopInstall.TopMMInstall;
//    ExecuteTests('TopMM');

    Halt;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
