program prgMultiLine;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FormMain},
  System.Android.Notification in 'System.Android.Notification.pas',
  Androidapi.JNI.Support in 'Androidapi.JNI.Support.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
