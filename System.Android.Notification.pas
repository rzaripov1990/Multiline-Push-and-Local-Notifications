{ ******************************************************* }
{ }
{ CodeGear Delphi Runtime Library }
{ }
{ Implementation Notification Center for Android }
{ }
{ Copyright(c) 2016 Embarcadero Technologies, Inc. }
{ All rights reserved }
{ }
{ ******************************************************* }

unit System.Android.Notification;

interface

{$SCOPEDENUMS ON}
{$IFDEF ANDROID}

uses
  System.Notification;

/// <summary>Common ancestor used to instantiate platform implementation</summary>
type
  TPlatformNotificationCenter = class(TBaseNotificationCenter)
  protected
    class function GetInstance: TBaseNotificationCenter; override;
  end;

{$ENDIF}

implementation

{$IFDEF ANDROID}

uses
  System.SysUtils, System.DateUtils, System.Classes, System.Messaging, System.TimeSpan,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.App,
  Androidapi.JNI.Support,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Media,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.Net,
  Androidapi.Helpers;

type
  { TNotificationCenterAndroid }

  TAndroidPreferenceAdapter = class
  strict private
    FPreference: JSharedPreferences;
    function FindNotification(const AName: string; out AIndex: Integer; out AID: Integer): Boolean;
    function ExtractName(const AStr: string): string;
    function ExtractID(const AStr: string): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveNotification(const ANotification: TNotification; const AID: Integer);
    procedure RemoveNotification(const AName: string);
    function IndexOf(const AName: string): Integer;
    function Find(const AName: string; out Index: Integer): Boolean;
    function GetID(const AName: string): Integer;
    function Contains(const AName: string): Boolean;
    function GetAllNotificationsNames: TStringList;
  end;

  TNotificationCenterAndroid = class(TPlatformNotificationCenter)
  private
    class var FNotificationCenterSingleton: TNotificationCenterAndroid;
  strict private
    FExternalStore: TAndroidPreferenceAdapter;
    FNotificationManager: JNotificationManager;
    function CreateNativeNotification(const ANotification: TNotification): JNotification;
    procedure SaveNotificationIntoIntent(var AIntent: JIntent; const ANotification: TNotification;
      const AID: Integer = -1);
    function LoadNotificationFromIntent(const AIntent: JIntent): TNotification;
    procedure CancelScheduledNotification(const AName: string);
    { Global FMX event }
    procedure DidFormsLoad;
    procedure DidReceiveNotification(const Sender: TObject; const M: TMessage);

    class function GetNotificationCenter: TNotificationCenterAndroid; static;
    class destructor Destroy;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoScheduleNotification(const ANotification: TNotification); override;
    procedure DoPresentNotification(const ANotification: TNotification); override;
    procedure DoCancelNotification(const AName: string); overload; override;
    procedure DoCancelNotification(const ANotification: TNotification); overload; override;
    procedure DoCancelAllNotifications; override;
    { Not supported }
    procedure DoSetIconBadgeNumber(const ACount: Integer); override;
    function DoGetIconBadgeNumber: Integer; override;
    procedure DoResetIconBadgeNumber; override;

    procedure DoLoaded; override;

    class property NotificationCenter: TNotificationCenterAndroid read GetNotificationCenter;
  end;

  TGeneratorUniqueID = class
  const
    SettingsNotificationUniquiID = 'SETTINGS_NOTIFICATION_UNIQUE_ID';
  strict private
    class var FNextUniqueID: Int64;
    class var FPreference: JSharedPreferences;
  public
    class constructor Create;
    class function GenerateID: Integer;
  end;

function GetNotificationService: JNotificationManager;
var
  NotificationServiceNative: JObject;
begin
  NotificationServiceNative := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE);
  Result := TJNotificationManager.Wrap((NotificationServiceNative as ILocalObject).GetObjectID);
end;

{$REGION 'TNotificationCenterAndroid'}

function DateTimeLocalToUnixMSecGMT(const ADateTime: TDateTime): Int64;
begin
  Result := DateTimeToUnix(ADateTime) * MSecsPerSec - Round(TTimeZone.Local.UtcOffset.TotalMilliseconds);
end;

function TNotificationCenterAndroid.CreateNativeNotification(const ANotification: TNotification): JNotification;

  function GetDefaultNotificationSound: Jnet_Uri;
  begin
    Result := TJRingtoneManager.JavaClass.getDefaultUri(TJRingtoneManager.JavaClass.TYPE_NOTIFICATION);
  end;

  function GetDefaultIconID: Integer;
  begin
    Result := TAndroidHelper.Context.getApplicationInfo.icon;
  end;

  function GetDefaultIcon: JBitmap;
  begin
    Result := TJBitmapFactory.JavaClass.decodeResource(TAndroidHelper.Context.getResources(), GetDefaultIconID);
  end;

  function GetContentTitle: JCharSequence;
  begin
    if ANotification.Title.IsEmpty then
      Result := StrToJCharSequence(TAndroidHelper.ApplicationTitle)
    else
      Result := StrToJCharSequence(ANotification.Title);
  end;

  function GetContentText: JCharSequence;
  begin
    Result := StrToJCharSequence(ANotification.AlertBody);
  end;

  function GetContentIntent: JPendingIntent;
  var
    Intent: JIntent;
  begin
    Intent := TAndroidHelper.Context.getPackageManager().getLaunchIntentForPackage
      (TAndroidHelper.Context.getPackageName());
    Intent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_SINGLE_TOP or TJIntent.JavaClass.FLAG_ACTIVITY_CLEAR_TOP);
    SaveNotificationIntoIntent(Intent, ANotification);
    Result := TJPendingIntent.JavaClass.getActivity(TAndroidHelper.Context, TGeneratorUniqueID.GenerateID, Intent,
      TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT);
  end;

var
  NotificationBuilder: JNotificationCompat_Builder;
  BigTextStyle: JNotificationCompat_BigTextStyle; // ZuBy
begin
  NotificationBuilder := TJNotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context);
  NotificationBuilder := NotificationBuilder.setDefaults(TJNotification.JavaClass.DEFAULT_LIGHTS);
  NotificationBuilder := NotificationBuilder.setSmallIcon(GetDefaultIconID);

  // ZuBy ***
  BigTextStyle := TJNotificationCompat_BigTextStyle.JavaClass.init();
  BigTextStyle.bigText(GetContentText);
  NotificationBuilder := NotificationBuilder.setStyle(BigTextStyle);
  // *** ZuBy

  NotificationBuilder := NotificationBuilder.setContentTitle(GetContentTitle);
  NotificationBuilder := NotificationBuilder.setContentText(GetContentText);
  NotificationBuilder := NotificationBuilder.setTicker(GetContentText);
  NotificationBuilder := NotificationBuilder.setContentIntent(GetContentIntent);
  NotificationBuilder := NotificationBuilder.setNumber(ANotification.Number);
  NotificationBuilder := NotificationBuilder.setAutoCancel(True);
  NotificationBuilder := NotificationBuilder.setWhen(TJDate.Create.getTime);
  if ANotification.EnableSound then
    if ANotification.SoundName.IsEmpty then
      NotificationBuilder := NotificationBuilder.setSound(GetDefaultNotificationSound)
    else
      NotificationBuilder := NotificationBuilder.setSound(StrToJURI(ANotification.SoundName));

  // Action buttons won't appear on platforms prior to Android 4.1!!!
  // http://developer.android.com/reference/android/support/v4/app/NotificationCompat.Builder.html#addAction
  Result := NotificationBuilder.Build;
end;

procedure TNotificationCenterAndroid.SaveNotificationIntoIntent(var AIntent: JIntent;
  const ANotification: TNotification; const AID: Integer);
var
  LaunchIntent: JIntent;
begin
  AIntent.setAction(TJNotificationInfo.JavaClass.ACTION_NOTIFICATION);
  AIntent.putExtra(TJNotificationInfo.JavaClass.EXTRA_UNIQUE_ID, AID);
  AIntent.putExtra(TJNotificationInfo.JavaClass.EXTRA_NAME, StringToJString(ANotification.Name));
  if ANotification.Title.IsEmpty then
    AIntent.putExtra(TJNotificationInfo.JavaClass.EXTRA_TITLE, StringToJString(TAndroidHelper.ApplicationTitle))
  else
    AIntent.putExtra(TJNotificationInfo.JavaClass.EXTRA_TITLE, StringToJString(ANotification.Title));
  AIntent.putExtra(TJNotificationInfo.JavaClass.EXTRA_ALERT_BODY, StringToJString(ANotification.AlertBody));
  AIntent.putExtra(TJNotificationInfo.JavaClass.EXTRA_ALERT_ACTION, StringToJString(ANotification.AlertAction));
  AIntent.putExtra(TJNotificationInfo.JavaClass.EXTRA_NUMBER, ANotification.Number);
  AIntent.putExtra(TJNotificationInfo.JavaClass.EXTRA_FIRE_DATE, ANotification.FireDate);
  AIntent.putExtra(TJNotificationInfo.JavaClass.EXTRA_FIRE_GMT_DATE,
    DateTimeLocalToUnixMSecGMT(ANotification.FireDate));
  AIntent.putExtra(TJNotificationInfo.JavaClass.EXTRA_REPEAT_INTERVAL, Integer(ANotification.RepeatInterval));
  AIntent.putExtra(TJNotificationInfo.JavaClass.EXTRA_ENABLE_SOUND, ANotification.EnableSound);
  AIntent.putExtra(TJNotificationInfo.JavaClass.EXTRA_SOUND_NAME, StringToJString(ANotification.SoundName));
  AIntent.putExtra(TJNotificationInfo.JavaClass.EXTRA_HAS_ACTION, ANotification.HasAction);
  LaunchIntent := TAndroidHelper.Context.getPackageManager().getLaunchIntentForPackage
    (TAndroidHelper.Context.getPackageName());
  AIntent.putExtra(TJNotificationInfo.JavaClass.EXTRA_ACTIVITY_CLASS_NAME, LaunchIntent.getComponent().getClassName());
end;

function TNotificationCenterAndroid.LoadNotificationFromIntent(const AIntent: JIntent): TNotification;
begin
  Result := TNotification.Create;
  Result.Name := JStringToString(AIntent.getStringExtra(TJNotificationInfo.JavaClass.EXTRA_NAME));
  Result.Title := JStringToString(AIntent.getStringExtra(TJNotificationInfo.JavaClass.EXTRA_TITLE));
  Result.AlertBody := JStringToString(AIntent.getStringExtra(TJNotificationInfo.JavaClass.EXTRA_ALERT_BODY));
  Result.AlertAction := JStringToString(AIntent.getStringExtra(TJNotificationInfo.JavaClass.EXTRA_ALERT_ACTION));
  Result.Number := AIntent.getIntExtra(TJNotificationInfo.JavaClass.EXTRA_NUMBER, 0);
  Result.FireDate := AIntent.getDoubleExtra(TJNotificationInfo.JavaClass.EXTRA_FIRE_DATE, Now);
  Result.RepeatInterval := TRepeatInterval(AIntent.getIntExtra(TJNotificationInfo.JavaClass.EXTRA_REPEAT_INTERVAL,
    Integer(TRepeatInterval.None)));
  Result.EnableSound := AIntent.getBooleanExtra(TJNotificationInfo.JavaClass.EXTRA_ENABLE_SOUND, True);
  Result.SoundName := JStringToString(AIntent.getStringExtra(TJNotificationInfo.JavaClass.EXTRA_SOUND_NAME));
  Result.HasAction := AIntent.getBooleanExtra(TJNotificationInfo.JavaClass.EXTRA_HAS_ACTION, True);
end;

class destructor TNotificationCenterAndroid.Destroy;
begin
  FNotificationCenterSingleton.Free;
end;

procedure TNotificationCenterAndroid.DidFormsLoad;

  function IsIntentWithNotification(const Intent: JIntent): Boolean;
  begin
    Result := (Intent <> nil) and (Intent.getAction <> nil) and
      Intent.getAction.equals(TJNotificationInfo.JavaClass.ACTION_NOTIFICATION);
  end;

var
  InputIntent: JIntent;
  Notification: TNotification;
begin
  if System.DelphiActivity <> nil then // This code will be executed if we have an activity
  begin
    InputIntent := TAndroidHelper.Activity.getIntent;
    if IsIntentWithNotification(InputIntent) then
    begin
      Notification := LoadNotificationFromIntent(InputIntent);
      try
        TMessageManager.DefaultManager.SendMessage(Self, TMessage<TNotification>.Create(Notification));
      finally
        Notification.DisposeOf;
      end;
    end;
  end;
end;

procedure TNotificationCenterAndroid.DidReceiveNotification(const Sender: TObject; const M: TMessage);

  function IsIntentWithNotification(Intent: JIntent): Boolean;
  begin
    Result := (Intent <> nil) and (Intent.getAction <> nil) and
      Intent.getAction.equals(TJNotificationInfo.JavaClass.ACTION_NOTIFICATION);
  end;

var
  InputIntent: JIntent;
  Notification: TNotification;
begin
  if M is TMessageReceivedNotification then
  begin
    InputIntent := (M as TMessageReceivedNotification).Value;
    if IsIntentWithNotification(InputIntent) then
    begin
      Notification := LoadNotificationFromIntent(InputIntent);
      try
        TMessageManager.DefaultManager.SendMessage(Self, TMessage<TNotification>.Create(Notification));
      finally
        Notification.DisposeOf;
      end;
    end;
  end;
end;

constructor TNotificationCenterAndroid.Create;
begin
  FExternalStore := TAndroidPreferenceAdapter.Create;
  FNotificationManager := GetNotificationService;
  { Subscription }
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, DidReceiveNotification);
end;

destructor TNotificationCenterAndroid.Destroy;
begin
  FExternalStore.DisposeOf;
  FNotificationManager := nil;
  { Unsibscribe }
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, DidReceiveNotification);
  inherited;
end;

procedure TNotificationCenterAndroid.DoPresentNotification(const ANotification: TNotification);
var
  NativeNotification: JNotification;
begin
  NativeNotification := CreateNativeNotification(ANotification);
  if ANotification.Name.IsEmpty then
    FNotificationManager.notify(TGeneratorUniqueID.GenerateID, NativeNotification)
  else
    FNotificationManager.notify(StringToJString(ANotification.Name), 0, NativeNotification);
  NativeNotification := nil;
end;

procedure TNotificationCenterAndroid.DoScheduleNotification(const ANotification: TNotification);

  function CreateNotificationAlarmIntent(const AID: Integer): JPendingIntent;
  var
    Intent: JIntent;
    Alarm: JNotificationAlarm;
  begin
    Alarm := TJNotificationAlarm.Create;
    Intent := TJIntent.Create;
    Intent.setClass(TAndroidHelper.Context, Alarm.getClass);
    Intent.setAction(TJNotificationInfo.JavaClass.ACTION_NOTIFICATION);
    SaveNotificationIntoIntent(Intent, ANotification, AID);
    Result := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, AID, Intent,
      TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT);
  end;

var
  PendingIntent: JPendingIntent;
  ID: Integer;
begin
  if not ANotification.Name.IsEmpty and FExternalStore.Contains(ANotification.Name) then
    CancelNotification(ANotification.Name);

  ID := TGeneratorUniqueID.GenerateID;
  PendingIntent := CreateNotificationAlarmIntent(ID);
  FExternalStore.SaveNotification(ANotification, ID);

  TAndroidHelper.AlarmManager.&set(TJAlarmManager.JavaClass.RTC_WAKEUP,
    DateTimeLocalToUnixMSecGMT(ANotification.FireDate), PendingIntent);
end;

procedure TNotificationCenterAndroid.DoCancelAllNotifications;
var
  Notifications: TStringList;
  NotificationName: string;
begin
  // Cancel all Presented notification
  FNotificationManager.cancelAll;

  // Cancel all scheduled notifications
  Notifications := FExternalStore.GetAllNotificationsNames;
  try
    for NotificationName in Notifications do
    begin
      CancelScheduledNotification(NotificationName);
      FExternalStore.RemoveNotification(NotificationName);
    end;
  finally
    Notifications.Free;
  end;
end;

procedure TNotificationCenterAndroid.DoCancelNotification(const ANotification: TNotification);
begin
  DoCancelNotification(ANotification.Name);
end;

procedure TNotificationCenterAndroid.CancelScheduledNotification(const AName: string);
var
  ID: Integer;
  Intent: JIntent;
  Alarm: JNotificationAlarm;
  PendingIntent: JPendingIntent;
begin
  if FExternalStore.Contains(AName) then
  begin
    ID := FExternalStore.GetID(AName);
    try
      Alarm := TJNotificationAlarm.Create;
      Intent := TJIntent.Create;
      Intent.setClass(TAndroidHelper.Context, Alarm.getClass);
      Intent.setAction(TJNotificationInfo.JavaClass.ACTION_NOTIFICATION);
      PendingIntent := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, ID, Intent,
        TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT);
      TAndroidHelper.AlarmManager.cancel(PendingIntent);
    finally
      FExternalStore.RemoveNotification(AName);
    end;
  end;
end;

procedure TNotificationCenterAndroid.DoCancelNotification(const AName: string);
begin
  FNotificationManager.cancel(StringToJString(AName), 0);
  CancelScheduledNotification(AName);
end;

procedure TNotificationCenterAndroid.DoSetIconBadgeNumber(const ACount: Integer);
begin
  // Android doesn't have Number icon on application Icon
end;

class function TNotificationCenterAndroid.GetNotificationCenter: TNotificationCenterAndroid;
begin
  if FNotificationCenterSingleton = nil then
    FNotificationCenterSingleton := TNotificationCenterAndroid.Create;
  Result := FNotificationCenterSingleton;
end;

function TNotificationCenterAndroid.DoGetIconBadgeNumber: Integer;
begin
  // Android doesn't have Number icon on application Icon
  Result := 0;
end;

procedure TNotificationCenterAndroid.DoLoaded;
begin
  inherited;
  DidFormsLoad;
end;

procedure TNotificationCenterAndroid.DoResetIconBadgeNumber;
begin
  // Android doesn't have Number icon on application Icon
end;

{$ENDREGION}
{ TGeneratorUniqueID }

class constructor TGeneratorUniqueID.Create;
begin
  FPreference := TAndroidHelper.Context.getSharedPreferences(TJNotificationAlarm.JavaClass.NOTIFICATION_CENTER,
    TJContext.JavaClass.MODE_PRIVATE);
  FNextUniqueID := FPreference.getInt(StringToJString(SettingsNotificationUniquiID), 0);
end;

class function TGeneratorUniqueID.GenerateID: Integer;
var
  PreferenceEditor: JSharedPreferences_Editor;
begin
  PreferenceEditor := FPreference.edit;
  try
    PreferenceEditor.putInt(StringToJString(SettingsNotificationUniquiID), FNextUniqueID);
  finally
    PreferenceEditor.commit;
  end;
  Result := FNextUniqueID;
  Inc(FNextUniqueID);
end;

{ TAndroidStorageAdapter }

function TAndroidPreferenceAdapter.FindNotification(const AName: string; out AIndex, AID: Integer): Boolean;
var
  Found: Boolean;
  Notifications: TStringList;
  NotificationPair: string;
  NotificationName: string;
  NotificationID: Integer;
  NotificationsStr: JString;
  I: Integer;
begin
  AIndex := -1;
  AID := -1;
  Notifications := TStringList.Create;
  try
    NotificationsStr := FPreference.getString(TJNotificationAlarm.JavaClass.SETTINGS_NOTIFICATION_IDS, nil);
    Notifications.Text := JStringToString(NotificationsStr);
    Found := False;
    I := 0;
    while (I < Notifications.Count) and not Found do
    begin
      NotificationPair := Notifications[I];
      NotificationName := ExtractName(NotificationPair);
      NotificationID := ExtractID(NotificationPair);
      if (NotificationID > -1) and (NotificationName = AName) then
      begin
        AIndex := I;
        Found := True;
      end;
      Inc(I);
    end;
    Result := Found;
  finally
    Notifications.DisposeOf;
  end;
end;

function TAndroidPreferenceAdapter.ExtractName(const AStr: string): string;
begin
  Result := AStr.Substring(0, AStr.LastIndexOf('='));
end;

function TAndroidPreferenceAdapter.ExtractID(const AStr: string): Integer;
var
  StrTmp: string;
begin
  StrTmp := AStr.Substring(AStr.LastIndexOf('=') + 1);
  if not TryStrToInt(StrTmp, Result) then
    Result := -1;
end;

constructor TAndroidPreferenceAdapter.Create;
begin
  FPreference := TAndroidHelper.Context.getSharedPreferences(TJNotificationAlarm.JavaClass.NOTIFICATION_CENTER,
    TJContext.JavaClass.MODE_PRIVATE);
end;

destructor TAndroidPreferenceAdapter.Destroy;
begin
  FPreference := nil;
  inherited;
end;

procedure TAndroidPreferenceAdapter.SaveNotification(const ANotification: TNotification; const AID: Integer);
var
  PreferenceEditor: JSharedPreferences_Editor;
  NotificationsList: TStringList;
  Index: Integer;
  Notifications: JString;
begin
  if not ANotification.Name.IsEmpty then
  begin
    Notifications := FPreference.getString(TJNotificationAlarm.JavaClass.SETTINGS_NOTIFICATION_IDS, nil);
    NotificationsList := TStringList.Create;
    try
      NotificationsList.Text := JStringToString(Notifications);
      if Find(ANotification.Name, Index) then
        NotificationsList.Delete(Index);
      NotificationsList.Add(ANotification.Name + '=' + AID.ToString);
      PreferenceEditor := FPreference.edit;
      try
        PreferenceEditor.putString(TJNotificationAlarm.JavaClass.SETTINGS_NOTIFICATION_IDS,
          StringToJString(NotificationsList.Text));
      finally
        PreferenceEditor.commit;
      end;
    finally
      NotificationsList.DisposeOf;
    end;
  end;
end;

procedure TAndroidPreferenceAdapter.RemoveNotification(const AName: string);
var
  NotificationsList: TStringList;
  Notifications: JString;
  I: Integer;
  Found: Boolean;
  PreferenceEditor: JSharedPreferences_Editor;
begin
  NotificationsList := TStringList.Create;
  try
    Notifications := FPreference.getString(TJNotificationAlarm.JavaClass.SETTINGS_NOTIFICATION_IDS, nil);
    NotificationsList.Text := JStringToString(Notifications);
    I := 0;
    Found := False;
    while not Found and (I < NotificationsList.Count) do
      if ExtractName(NotificationsList[I]) = AName then
        Found := True
      else
        Inc(I);

    if Found then
    begin
      PreferenceEditor := FPreference.edit;
      try
        NotificationsList.Delete(I);
        PreferenceEditor.putString(TJNotificationAlarm.JavaClass.SETTINGS_NOTIFICATION_IDS,
          StringToJString(NotificationsList.Text));
      finally
        PreferenceEditor.commit;
      end;
    end;
  finally
    NotificationsList.DisposeOf;
  end;
end;

function TAndroidPreferenceAdapter.IndexOf(const AName: string): Integer;
var
  ID: Integer;
begin
  FindNotification(AName, Result, ID);
end;

function TAndroidPreferenceAdapter.Find(const AName: string; out Index: Integer): Boolean;
var
  ID: Integer;
begin
  Result := FindNotification(AName, Index, ID);
end;

function TAndroidPreferenceAdapter.GetAllNotificationsNames: TStringList;
var
  Notifications: TStringList;
  NotificationsStr: JString;
  I: Integer;
begin
  Notifications := TStringList.Create;
  NotificationsStr := FPreference.getString(TJNotificationAlarm.JavaClass.SETTINGS_NOTIFICATION_IDS, nil);
  Notifications.Text := JStringToString(NotificationsStr);
  for I := 0 to Notifications.Count - 1 do
    Notifications[I] := ExtractName(Notifications[I]);

  Result := Notifications;
end;

function TAndroidPreferenceAdapter.GetID(const AName: string): Integer;
var
  NotificationsStr: JString;
  Notifications: TStringList;
  IDStr: string;
  Notification: string;
begin
  Notifications := TStringList.Create;
  try
    NotificationsStr := FPreference.getString(TJNotificationAlarm.JavaClass.SETTINGS_NOTIFICATION_IDS, nil);
    Notifications.Text := JStringToString(NotificationsStr);

    for Notification in Notifications do
    begin
      IDStr := Notification.Substring(Notification.IndexOf('=') + 1);
      if TryStrToInt(IDStr, Result) then
        Break
      else
        Result := -1;
    end;
  finally
    Notifications.DisposeOf;
  end;
end;

function TAndroidPreferenceAdapter.Contains(const AName: string): Boolean;
begin
  Result := IndexOf(AName) > -1;
end;

{ TPlatformNotificationCenter }

class function TPlatformNotificationCenter.GetInstance: TBaseNotificationCenter;
begin
  Result := TBaseNotificationCenter(TNotificationCenterAndroid.NotificationCenter)
end;
{$ENDIF}

end.
