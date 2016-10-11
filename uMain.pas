unit uMain;

{
	author: ZuBy
	
	https://github.com/rzaripov1990/Multiline-Push-and-Local-Notifications
	rzaripov1990@gmail.com
	2016
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Notification,
  System.Android.Notification, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TFormMain = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure ShowNotification(MessageText: string; BadgeNumber: integer);
var
  NotifiCenter: TNotificationCenter;
  Notification: TNotification;
begin
  NotifiCenter := TNotificationCenter.Create(nil);
  Notification := NotifiCenter.CreateNotification;

  try
    if NotifiCenter.Supported then
    begin
      Notification.Title := 'Delphi [Fire-Monkey.ru]';
      Notification.AlertBody := MessageText;
      Notification.EnableSound := true;
      Notification.Number := BadgeNumber;
      NotifiCenter.ApplicationIconBadgeNumber := BadgeNumber;
      NotifiCenter.PresentNotification(Notification);
    end;
  finally
    NotifiCenter.Free;
    Notification.Free;
  end;
end;

procedure TFormMain.Button1Click(Sender: TObject);
begin
  ShowNotification('Теперь текст в локальных уведомлениях может быть мультилайн', 0);
end;

end.
