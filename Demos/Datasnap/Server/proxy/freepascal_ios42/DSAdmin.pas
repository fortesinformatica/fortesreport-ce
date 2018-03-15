//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2012 Embarcadero Technologies, Inc.
//
//*******************************************************

unit DSAdmin;

interface
  uses
    Classes,
    DB,
    DSRESTParameterMetaData,
    DBXFPCJson,
    DBXValue,
    DBXFPCCommon,
    DSRESTConnection, DSRestTypes;

  type
    TDSAdmin = class
    private
      FConnection: TDSRESTConnection;
      FDSAdmin_GetPlatformName: TDSRESTParameterMetaDataArray;
      FDSAdmin_ClearResources: TDSRESTParameterMetaDataArray;
      FDSAdmin_FindPackages: TDSRESTParameterMetaDataArray;
      FDSAdmin_FindClasses: TDSRESTParameterMetaDataArray;
      FDSAdmin_FindMethods: TDSRESTParameterMetaDataArray;
      FDSAdmin_CreateServerClasses: TDSRESTParameterMetaDataArray;
      FDSAdmin_DropServerClasses: TDSRESTParameterMetaDataArray;
      FDSAdmin_CreateServerMethods: TDSRESTParameterMetaDataArray;
      FDSAdmin_DropServerMethods: TDSRESTParameterMetaDataArray;
      FDSAdmin_GetServerClasses: TDSRESTParameterMetaDataArray;
      FDSAdmin_ListClasses: TDSRESTParameterMetaDataArray;
      FDSAdmin_DescribeClass: TDSRESTParameterMetaDataArray;
      FDSAdmin_ListMethods: TDSRESTParameterMetaDataArray;
      FDSAdmin_DescribeMethod: TDSRESTParameterMetaDataArray;
      FDSAdmin_GetServerMethods: TDSRESTParameterMetaDataArray;
      FDSAdmin_GetServerMethodParameters: TDSRESTParameterMetaDataArray;
      FDSAdmin_GetDatabaseConnectionProperties: TDSRESTParameterMetaDataArray;
      FDSAdmin_GetDSServerName: TDSRESTParameterMetaDataArray;
      FDSAdmin_ConsumeClientChannel: TDSRESTParameterMetaDataArray;
      FDSAdmin_ConsumeClientChannelTimeout: TDSRESTParameterMetaDataArray;
      FDSAdmin_CloseClientChannel: TDSRESTParameterMetaDataArray;
      FDSAdmin_RegisterClientCallbackServer: TDSRESTParameterMetaDataArray;
      FDSAdmin_UnregisterClientCallback: TDSRESTParameterMetaDataArray;
      FDSAdmin_BroadcastToChannel: TDSRESTParameterMetaDataArray;
      FDSAdmin_BroadcastObjectToChannel: TDSRESTParameterMetaDataArray;
      FDSAdmin_NotifyCallback: TDSRESTParameterMetaDataArray;
      FDSAdmin_NotifyObject: TDSRESTParameterMetaDataArray;
    protected
      procedure DSAdmin_GetPlatformName;
      procedure DSAdmin_ClearResources;
      procedure DSAdmin_FindPackages;
      procedure DSAdmin_FindClasses;
      procedure DSAdmin_FindMethods;
      procedure DSAdmin_CreateServerClasses;
      procedure DSAdmin_DropServerClasses;
      procedure DSAdmin_CreateServerMethods;
      procedure DSAdmin_DropServerMethods;
      procedure DSAdmin_GetServerClasses;
      procedure DSAdmin_ListClasses;
      procedure DSAdmin_DescribeClass;
      procedure DSAdmin_ListMethods;
      procedure DSAdmin_DescribeMethod;
      procedure DSAdmin_GetServerMethods;
      procedure DSAdmin_GetServerMethodParameters;
      procedure DSAdmin_GetDatabaseConnectionProperties;
      procedure DSAdmin_GetDSServerName;
      procedure DSAdmin_ConsumeClientChannel;
      procedure DSAdmin_ConsumeClientChannelTimeout;
      procedure DSAdmin_CloseClientChannel;
      procedure DSAdmin_RegisterClientCallbackServer;
      procedure DSAdmin_UnregisterClientCallback;
      procedure DSAdmin_BroadcastToChannel;
      procedure DSAdmin_BroadcastObjectToChannel;
      procedure DSAdmin_NotifyCallback;
      procedure DSAdmin_NotifyObject;
    public
     destructor Destroy; override;
     constructor Create(aConnection: TDSRESTConnection); reintroduce; virtual;

      (*
       * @return result - Type on server: string
       *)
      function GetPlatformName():string;

      (*
       * @return result - Type on server: boolean
       *)
      function ClearResources():boolean;

      (*
       * @return result - Type on server: TDBXReader
       *)
      function FindPackages():TDBXReader;

      (*
       * @param PackageName [in] - Type on server: string
       * @param ClassPattern [in] - Type on server: string
       * @return result - Type on server: TDBXReader
       *)
      function FindClasses(PackageName: string;ClassPattern: string):TDBXReader;

      (*
       * @param PackageName [in] - Type on server: string
       * @param ClassPattern [in] - Type on server: string
       * @param MethodPattern [in] - Type on server: string
       * @return result - Type on server: TDBXReader
       *)
      function FindMethods(PackageName: string;ClassPattern: string;MethodPattern: string):TDBXReader;

      (*
       * @param ClassReader [in] - Type on server: TDBXReader
       *)
      procedure CreateServerClasses(ClassReader: TDBXReader);

      (*
       * @param ClassReader [in] - Type on server: TDBXReader
       *)
      procedure DropServerClasses(ClassReader: TDBXReader);

      (*
       * @param MethodReader [in] - Type on server: TDBXReader
       *)
      procedure CreateServerMethods(MethodReader: TDBXReader);

      (*
       * @param MethodReader [in] - Type on server: TDBXReader
       *)
      procedure DropServerMethods(MethodReader: TDBXReader);

      (*
       * @return result - Type on server: TDBXReader
       *)
      function GetServerClasses():TDBXReader;

      (*
       * @return result - Type on server: TJSONArray
       *)
      function ListClasses():TJSONArray;

      (*
       * @param ClassName [in] - Type on server: string
       * @return result - Type on server: TJSONObject
       *)
      function DescribeClass(_ClassName: string):TJSONObject;

      (*
       * @param ClassName [in] - Type on server: string
       * @return result - Type on server: TJSONArray
       *)
      function ListMethods(_ClassName: string):TJSONArray;

      (*
       * @param ServerMethodName [in] - Type on server: string
       * @return result - Type on server: TJSONObject
       *)
      function DescribeMethod(ServerMethodName: string):TJSONObject;

      (*
       * @return result - Type on server: TDBXReader
       *)
      function GetServerMethods():TDBXReader;

      (*
       * @return result - Type on server: TDBXReader
       *)
      function GetServerMethodParameters():TDBXReader;

      (*
       * @return result - Type on server: TDBXReader
       *)
      function GetDatabaseConnectionProperties():TDBXReader;

      (*
       * @return result - Type on server: string
       *)
      function GetDSServerName():string;

      (*
       * @param ChannelName [in] - Type on server: string
       * @param ClientManagerId [in] - Type on server: string
       * @param CallbackId [in] - Type on server: string
       * @param ChannelNames [in] - Type on server: string
       * @param SecurityToken [in] - Type on server: string
       * @param ResponseData [in] - Type on server: TJSONValue
       * @return result - Type on server: TJSONValue
       *)
      function ConsumeClientChannel(ChannelName: string;ClientManagerId: string;CallbackId: string;ChannelNames: string;SecurityToken: string;ResponseData: TJSONValue):TJSONValue;

      (*
       * @param ChannelName [in] - Type on server: string
       * @param ClientManagerId [in] - Type on server: string
       * @param CallbackId [in] - Type on server: string
       * @param ChannelNames [in] - Type on server: string
       * @param SecurityToken [in] - Type on server: string
       * @param Timeout [in] - Type on server: Int32
       * @param ResponseData [in] - Type on server: TJSONValue
       * @return result - Type on server: TJSONValue
       *)
      function ConsumeClientChannelTimeout(ChannelName: string;ClientManagerId: string;CallbackId: string;ChannelNames: string;SecurityToken: string;Timeout: Int32;ResponseData: TJSONValue):TJSONValue;

      (*
       * @param ChannelId [in] - Type on server: string
       * @param SecurityToken [in] - Type on server: string
       * @return result - Type on server: boolean
       *)
      function CloseClientChannel(ChannelId: string;SecurityToken: string):boolean;

      (*
       * @param ChannelId [in] - Type on server: string
       * @param CallbackId [in] - Type on server: string
       * @param ChannelNames [in] - Type on server: string
       * @param SecurityToken [in] - Type on server: string
       * @return result - Type on server: boolean
       *)
      function RegisterClientCallbackServer(ChannelId: string;CallbackId: string;ChannelNames: string;SecurityToken: string):boolean;

      (*
       * @param ChannelId [in] - Type on server: string
       * @param CallbackId [in] - Type on server: string
       * @param SecurityToken [in] - Type on server: string
       * @return result - Type on server: boolean
       *)
      function UnregisterClientCallback(ChannelId: string;CallbackId: string;SecurityToken: string):boolean;

      (*
       * @param ChannelName [in] - Type on server: string
       * @param Msg [in] - Type on server: TJSONValue
       * @return result - Type on server: boolean
       *)
      function BroadcastToChannel(ChannelName: string;Msg: TJSONValue):boolean;

      (*
       * @param ChannelName [in] - Type on server: string
       * @param Msg [in] - Type on server: TJSONObject
       * @return result - Type on server: boolean
       *)
      function BroadcastObjectToChannel(ChannelName: string;Msg: TJSONObject):boolean;

      (*
       * @param ClientId [in] - Type on server: string
       * @param CallbackId [in] - Type on server: string
       * @param Msg [in] - Type on server: TJSONValue
       * @param Response [out] - Type on server: TJSONValue
       * @return result - Type on server: boolean
       *)
      function NotifyCallback(ClientId: string;CallbackId: string;Msg: TJSONValue;out Response: TJSONValue):boolean;

      (*
       * @param ClientId [in] - Type on server: string
       * @param CallbackId [in] - Type on server: string
       * @param Msg [in] - Type on server: TJSONObject
       * @param Response [out] - Type on server: TJSONObject
       * @return result - Type on server: boolean
       *)
      function NotifyObject(ClientId: string;CallbackId: string;Msg: TJSONObject;out Response: TJSONObject):boolean;
      property Connection:TDSRESTConnection read FConnection;
    end;


implementation


{DSAdmin}
constructor TDSAdmin.Create(aConnection: TDSRESTConnection);
begin
  FConnection := aConnection;
end;


procedure TDSAdmin.DSAdmin_GetPlatformName;
begin
  if Length(FDSAdmin_GetPlatformName) = 0 then
  begin
    SetLength(FDSAdmin_GetPlatformName, 1);
    FDSAdmin_GetPlatformName[0] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,WideStringType,'string');
  end;
end;

procedure TDSAdmin.DSAdmin_ClearResources;
begin
  if Length(FDSAdmin_ClearResources) = 0 then
  begin
    SetLength(FDSAdmin_ClearResources, 1);
    FDSAdmin_ClearResources[0] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,BooleanType,'boolean');
  end;
end;

procedure TDSAdmin.DSAdmin_FindPackages;
begin
  if Length(FDSAdmin_FindPackages) = 0 then
  begin
    SetLength(FDSAdmin_FindPackages, 1);
    FDSAdmin_FindPackages[0] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,TableType,'TDBXReader');
  end;
end;

procedure TDSAdmin.DSAdmin_FindClasses;
begin
  if Length(FDSAdmin_FindClasses) = 0 then
  begin
    SetLength(FDSAdmin_FindClasses, 3);
    FDSAdmin_FindClasses[0] := TDSRESTParameterMetaData.CreateParam( 'PackageName',Input,WideStringType,'string');
    FDSAdmin_FindClasses[1] := TDSRESTParameterMetaData.CreateParam( 'ClassPattern',Input,WideStringType,'string');
    FDSAdmin_FindClasses[2] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,TableType,'TDBXReader');
  end;
end;

procedure TDSAdmin.DSAdmin_FindMethods;
begin
  if Length(FDSAdmin_FindMethods) = 0 then
  begin
    SetLength(FDSAdmin_FindMethods, 4);
    FDSAdmin_FindMethods[0] := TDSRESTParameterMetaData.CreateParam( 'PackageName',Input,WideStringType,'string');
    FDSAdmin_FindMethods[1] := TDSRESTParameterMetaData.CreateParam( 'ClassPattern',Input,WideStringType,'string');
    FDSAdmin_FindMethods[2] := TDSRESTParameterMetaData.CreateParam( 'MethodPattern',Input,WideStringType,'string');
    FDSAdmin_FindMethods[3] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,TableType,'TDBXReader');
  end;
end;

procedure TDSAdmin.DSAdmin_CreateServerClasses;
begin
  if Length(FDSAdmin_CreateServerClasses) = 0 then
  begin
    SetLength(FDSAdmin_CreateServerClasses, 1);
    FDSAdmin_CreateServerClasses[0] := TDSRESTParameterMetaData.CreateParam( 'ClassReader',Input,TableType,'TDBXReader');
  end;
end;

procedure TDSAdmin.DSAdmin_DropServerClasses;
begin
  if Length(FDSAdmin_DropServerClasses) = 0 then
  begin
    SetLength(FDSAdmin_DropServerClasses, 1);
    FDSAdmin_DropServerClasses[0] := TDSRESTParameterMetaData.CreateParam( 'ClassReader',Input,TableType,'TDBXReader');
  end;
end;

procedure TDSAdmin.DSAdmin_CreateServerMethods;
begin
  if Length(FDSAdmin_CreateServerMethods) = 0 then
  begin
    SetLength(FDSAdmin_CreateServerMethods, 1);
    FDSAdmin_CreateServerMethods[0] := TDSRESTParameterMetaData.CreateParam( 'MethodReader',Input,TableType,'TDBXReader');
  end;
end;

procedure TDSAdmin.DSAdmin_DropServerMethods;
begin
  if Length(FDSAdmin_DropServerMethods) = 0 then
  begin
    SetLength(FDSAdmin_DropServerMethods, 1);
    FDSAdmin_DropServerMethods[0] := TDSRESTParameterMetaData.CreateParam( 'MethodReader',Input,TableType,'TDBXReader');
  end;
end;

procedure TDSAdmin.DSAdmin_GetServerClasses;
begin
  if Length(FDSAdmin_GetServerClasses) = 0 then
  begin
    SetLength(FDSAdmin_GetServerClasses, 1);
    FDSAdmin_GetServerClasses[0] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,TableType,'TDBXReader');
  end;
end;

procedure TDSAdmin.DSAdmin_ListClasses;
begin
  if Length(FDSAdmin_ListClasses) = 0 then
  begin
    SetLength(FDSAdmin_ListClasses, 1);
    FDSAdmin_ListClasses[0] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,JsonValueType,'TJSONArray');
  end;
end;

procedure TDSAdmin.DSAdmin_DescribeClass;
begin
  if Length(FDSAdmin_DescribeClass) = 0 then
  begin
    SetLength(FDSAdmin_DescribeClass, 2);
    FDSAdmin_DescribeClass[0] := TDSRESTParameterMetaData.CreateParam( 'ClassName',Input,WideStringType,'string');
    FDSAdmin_DescribeClass[1] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,JsonValueType,'TJSONObject');
  end;
end;

procedure TDSAdmin.DSAdmin_ListMethods;
begin
  if Length(FDSAdmin_ListMethods) = 0 then
  begin
    SetLength(FDSAdmin_ListMethods, 2);
    FDSAdmin_ListMethods[0] := TDSRESTParameterMetaData.CreateParam( 'ClassName',Input,WideStringType,'string');
    FDSAdmin_ListMethods[1] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,JsonValueType,'TJSONArray');
  end;
end;

procedure TDSAdmin.DSAdmin_DescribeMethod;
begin
  if Length(FDSAdmin_DescribeMethod) = 0 then
  begin
    SetLength(FDSAdmin_DescribeMethod, 2);
    FDSAdmin_DescribeMethod[0] := TDSRESTParameterMetaData.CreateParam( 'ServerMethodName',Input,WideStringType,'string');
    FDSAdmin_DescribeMethod[1] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,JsonValueType,'TJSONObject');
  end;
end;

procedure TDSAdmin.DSAdmin_GetServerMethods;
begin
  if Length(FDSAdmin_GetServerMethods) = 0 then
  begin
    SetLength(FDSAdmin_GetServerMethods, 1);
    FDSAdmin_GetServerMethods[0] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,TableType,'TDBXReader');
  end;
end;

procedure TDSAdmin.DSAdmin_GetServerMethodParameters;
begin
  if Length(FDSAdmin_GetServerMethodParameters) = 0 then
  begin
    SetLength(FDSAdmin_GetServerMethodParameters, 1);
    FDSAdmin_GetServerMethodParameters[0] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,TableType,'TDBXReader');
  end;
end;

procedure TDSAdmin.DSAdmin_GetDatabaseConnectionProperties;
begin
  if Length(FDSAdmin_GetDatabaseConnectionProperties) = 0 then
  begin
    SetLength(FDSAdmin_GetDatabaseConnectionProperties, 1);
    FDSAdmin_GetDatabaseConnectionProperties[0] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,TableType,'TDBXReader');
  end;
end;

procedure TDSAdmin.DSAdmin_GetDSServerName;
begin
  if Length(FDSAdmin_GetDSServerName) = 0 then
  begin
    SetLength(FDSAdmin_GetDSServerName, 1);
    FDSAdmin_GetDSServerName[0] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,WideStringType,'string');
  end;
end;

procedure TDSAdmin.DSAdmin_ConsumeClientChannel;
begin
  if Length(FDSAdmin_ConsumeClientChannel) = 0 then
  begin
    SetLength(FDSAdmin_ConsumeClientChannel, 7);
    FDSAdmin_ConsumeClientChannel[0] := TDSRESTParameterMetaData.CreateParam( 'ChannelName',Input,WideStringType,'string');
    FDSAdmin_ConsumeClientChannel[1] := TDSRESTParameterMetaData.CreateParam( 'ClientManagerId',Input,WideStringType,'string');
    FDSAdmin_ConsumeClientChannel[2] := TDSRESTParameterMetaData.CreateParam( 'CallbackId',Input,WideStringType,'string');
    FDSAdmin_ConsumeClientChannel[3] := TDSRESTParameterMetaData.CreateParam( 'ChannelNames',Input,WideStringType,'string');
    FDSAdmin_ConsumeClientChannel[4] := TDSRESTParameterMetaData.CreateParam( 'SecurityToken',Input,WideStringType,'string');
    FDSAdmin_ConsumeClientChannel[5] := TDSRESTParameterMetaData.CreateParam( 'ResponseData',Input,JsonValueType,'TJSONValue');
    FDSAdmin_ConsumeClientChannel[6] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,JsonValueType,'TJSONValue');
  end;
end;

procedure TDSAdmin.DSAdmin_ConsumeClientChannelTimeout;
begin
  if Length(FDSAdmin_ConsumeClientChannelTimeout) = 0 then
  begin
    SetLength(FDSAdmin_ConsumeClientChannelTimeout, 8);
    FDSAdmin_ConsumeClientChannelTimeout[0] := TDSRESTParameterMetaData.CreateParam( 'ChannelName',Input,WideStringType,'string');
    FDSAdmin_ConsumeClientChannelTimeout[1] := TDSRESTParameterMetaData.CreateParam( 'ClientManagerId',Input,WideStringType,'string');
    FDSAdmin_ConsumeClientChannelTimeout[2] := TDSRESTParameterMetaData.CreateParam( 'CallbackId',Input,WideStringType,'string');
    FDSAdmin_ConsumeClientChannelTimeout[3] := TDSRESTParameterMetaData.CreateParam( 'ChannelNames',Input,WideStringType,'string');
    FDSAdmin_ConsumeClientChannelTimeout[4] := TDSRESTParameterMetaData.CreateParam( 'SecurityToken',Input,WideStringType,'string');
    FDSAdmin_ConsumeClientChannelTimeout[5] := TDSRESTParameterMetaData.CreateParam( 'Timeout',Input,Int32Type,'Int32');
    FDSAdmin_ConsumeClientChannelTimeout[6] := TDSRESTParameterMetaData.CreateParam( 'ResponseData',Input,JsonValueType,'TJSONValue');
    FDSAdmin_ConsumeClientChannelTimeout[7] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,JsonValueType,'TJSONValue');
  end;
end;

procedure TDSAdmin.DSAdmin_CloseClientChannel;
begin
  if Length(FDSAdmin_CloseClientChannel) = 0 then
  begin
    SetLength(FDSAdmin_CloseClientChannel, 3);
    FDSAdmin_CloseClientChannel[0] := TDSRESTParameterMetaData.CreateParam( 'ChannelId',Input,WideStringType,'string');
    FDSAdmin_CloseClientChannel[1] := TDSRESTParameterMetaData.CreateParam( 'SecurityToken',Input,WideStringType,'string');
    FDSAdmin_CloseClientChannel[2] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,BooleanType,'boolean');
  end;
end;

procedure TDSAdmin.DSAdmin_RegisterClientCallbackServer;
begin
  if Length(FDSAdmin_RegisterClientCallbackServer) = 0 then
  begin
    SetLength(FDSAdmin_RegisterClientCallbackServer, 5);
    FDSAdmin_RegisterClientCallbackServer[0] := TDSRESTParameterMetaData.CreateParam( 'ChannelId',Input,WideStringType,'string');
    FDSAdmin_RegisterClientCallbackServer[1] := TDSRESTParameterMetaData.CreateParam( 'CallbackId',Input,WideStringType,'string');
    FDSAdmin_RegisterClientCallbackServer[2] := TDSRESTParameterMetaData.CreateParam( 'ChannelNames',Input,WideStringType,'string');
    FDSAdmin_RegisterClientCallbackServer[3] := TDSRESTParameterMetaData.CreateParam( 'SecurityToken',Input,WideStringType,'string');
    FDSAdmin_RegisterClientCallbackServer[4] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,BooleanType,'boolean');
  end;
end;

procedure TDSAdmin.DSAdmin_UnregisterClientCallback;
begin
  if Length(FDSAdmin_UnregisterClientCallback) = 0 then
  begin
    SetLength(FDSAdmin_UnregisterClientCallback, 4);
    FDSAdmin_UnregisterClientCallback[0] := TDSRESTParameterMetaData.CreateParam( 'ChannelId',Input,WideStringType,'string');
    FDSAdmin_UnregisterClientCallback[1] := TDSRESTParameterMetaData.CreateParam( 'CallbackId',Input,WideStringType,'string');
    FDSAdmin_UnregisterClientCallback[2] := TDSRESTParameterMetaData.CreateParam( 'SecurityToken',Input,WideStringType,'string');
    FDSAdmin_UnregisterClientCallback[3] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,BooleanType,'boolean');
  end;
end;

procedure TDSAdmin.DSAdmin_BroadcastToChannel;
begin
  if Length(FDSAdmin_BroadcastToChannel) = 0 then
  begin
    SetLength(FDSAdmin_BroadcastToChannel, 3);
    FDSAdmin_BroadcastToChannel[0] := TDSRESTParameterMetaData.CreateParam( 'ChannelName',Input,WideStringType,'string');
    FDSAdmin_BroadcastToChannel[1] := TDSRESTParameterMetaData.CreateParam( 'Msg',Input,JsonValueType,'TJSONValue');
    FDSAdmin_BroadcastToChannel[2] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,BooleanType,'boolean');
  end;
end;

procedure TDSAdmin.DSAdmin_BroadcastObjectToChannel;
begin
  if Length(FDSAdmin_BroadcastObjectToChannel) = 0 then
  begin
    SetLength(FDSAdmin_BroadcastObjectToChannel, 3);
    FDSAdmin_BroadcastObjectToChannel[0] := TDSRESTParameterMetaData.CreateParam( 'ChannelName',Input,WideStringType,'string');
    FDSAdmin_BroadcastObjectToChannel[1] := TDSRESTParameterMetaData.CreateParam( 'Msg',Input,JsonValueType,'TJSONObject');
    FDSAdmin_BroadcastObjectToChannel[2] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,BooleanType,'boolean');
  end;
end;

procedure TDSAdmin.DSAdmin_NotifyCallback;
begin
  if Length(FDSAdmin_NotifyCallback) = 0 then
  begin
    SetLength(FDSAdmin_NotifyCallback, 5);
    FDSAdmin_NotifyCallback[0] := TDSRESTParameterMetaData.CreateParam( 'ClientId',Input,WideStringType,'string');
    FDSAdmin_NotifyCallback[1] := TDSRESTParameterMetaData.CreateParam( 'CallbackId',Input,WideStringType,'string');
    FDSAdmin_NotifyCallback[2] := TDSRESTParameterMetaData.CreateParam( 'Msg',Input,JsonValueType,'TJSONValue');
    FDSAdmin_NotifyCallback[3] := TDSRESTParameterMetaData.CreateParam( 'Response',Output,JsonValueType,'TJSONValue');
    FDSAdmin_NotifyCallback[4] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,BooleanType,'boolean');
  end;
end;

procedure TDSAdmin.DSAdmin_NotifyObject;
begin
  if Length(FDSAdmin_NotifyObject) = 0 then
  begin
    SetLength(FDSAdmin_NotifyObject, 5);
    FDSAdmin_NotifyObject[0] := TDSRESTParameterMetaData.CreateParam( 'ClientId',Input,WideStringType,'string');
    FDSAdmin_NotifyObject[1] := TDSRESTParameterMetaData.CreateParam( 'CallbackId',Input,WideStringType,'string');
    FDSAdmin_NotifyObject[2] := TDSRESTParameterMetaData.CreateParam( 'Msg',Input,JsonValueType,'TJSONObject');
    FDSAdmin_NotifyObject[3] := TDSRESTParameterMetaData.CreateParam( 'Response',Output,JsonValueType,'TJSONObject');
    FDSAdmin_NotifyObject[4] := TDSRESTParameterMetaData.CreateParam( '',ReturnValue,BooleanType,'boolean');
  end;
end;
destructor TDSAdmin.Destroy;
begin
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_GetPlatformName);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_ClearResources);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_FindPackages);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_FindClasses);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_FindMethods);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_CreateServerClasses);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_DropServerClasses);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_CreateServerMethods);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_DropServerMethods);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_GetServerClasses);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_ListClasses);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_DescribeClass);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_ListMethods);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_DescribeMethod);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_GetServerMethods);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_GetServerMethodParameters);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_GetDatabaseConnectionProperties);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_GetDSServerName);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_ConsumeClientChannel);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_ConsumeClientChannelTimeout);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_CloseClientChannel);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_RegisterClientCallbackServer);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_UnregisterClientCallback);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_BroadcastToChannel);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_BroadcastObjectToChannel);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_NotifyCallback);
   TDSRESTParameterMetaData.releaseArray(FDSAdmin_NotifyObject);
  inherited;
end;

(*
 * @return result - Type on server: string
 *)
function TDSAdmin.GetPlatformName():string;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.GetPlatformName';
    DSAdmin_GetPlatformName;
    cmd.prepare(FDSAdmin_GetPlatformName);
    Connection.execute(cmd);

    result:= cmd.parameters.items[0].Value.AsString;
  finally
    cmd.free;
  end;
end;

(*
 * @return result - Type on server: boolean
 *)
function TDSAdmin.ClearResources():boolean;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.ClearResources';
    DSAdmin_ClearResources;
    cmd.prepare(FDSAdmin_ClearResources);
    Connection.execute(cmd);

    result:= cmd.parameters.items[0].Value.AsBoolean;
  finally
    cmd.free;
  end;
end;

(*
 * @return result - Type on server: TDBXReader
 *)
function TDSAdmin.FindPackages():TDBXReader;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.FindPackages';
    DSAdmin_FindPackages;
    cmd.prepare(FDSAdmin_FindPackages);
    Connection.execute(cmd);

    result:= cmd.parameters.items[0].Value.AsTable as TDBXReader;
  finally
    cmd.free;
  end;
end;

(*
 * @param PackageName [in] - Type on server: string
 * @param ClassPattern [in] - Type on server: string
 * @return result - Type on server: TDBXReader
 *)
function TDSAdmin.FindClasses( packagename:string; classpattern:string):TDBXReader;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.FindClasses';
    DSAdmin_FindClasses;
    cmd.prepare(FDSAdmin_FindClasses);
    cmd.parameters.items[0].Value.AsString:=packagename;
    cmd.parameters.items[1].Value.AsString:=classpattern;
    Connection.execute(cmd);

    result:= cmd.parameters.items[2].Value.AsTable as TDBXReader;
  finally
    cmd.free;
  end;
end;

(*
 * @param PackageName [in] - Type on server: string
 * @param ClassPattern [in] - Type on server: string
 * @param MethodPattern [in] - Type on server: string
 * @return result - Type on server: TDBXReader
 *)
function TDSAdmin.FindMethods( packagename:string; classpattern:string; methodpattern:string):TDBXReader;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.FindMethods';
    DSAdmin_FindMethods;
    cmd.prepare(FDSAdmin_FindMethods);
    cmd.parameters.items[0].Value.AsString:=packagename;
    cmd.parameters.items[1].Value.AsString:=classpattern;
    cmd.parameters.items[2].Value.AsString:=methodpattern;
    Connection.execute(cmd);

    result:= cmd.parameters.items[3].Value.AsTable as TDBXReader;
  finally
    cmd.free;
  end;
end;

(*
 * @param ClassReader [in] - Type on server: TDBXReader
 *)
procedure TDSAdmin.CreateServerClasses( classreader:TDBXReader);
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := POST;
    cmd.text := 'DSAdmin.CreateServerClasses';
    DSAdmin_CreateServerClasses;
    cmd.prepare(FDSAdmin_CreateServerClasses);
    cmd.parameters.items[0].Value.AsTable:=classreader;
    Connection.execute(cmd);
  finally
    cmd.free;
  end;
end;

(*
 * @param ClassReader [in] - Type on server: TDBXReader
 *)
procedure TDSAdmin.DropServerClasses( classreader:TDBXReader);
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := POST;
    cmd.text := 'DSAdmin.DropServerClasses';
    DSAdmin_DropServerClasses;
    cmd.prepare(FDSAdmin_DropServerClasses);
    cmd.parameters.items[0].Value.AsTable:=classreader;
    Connection.execute(cmd);
  finally
    cmd.free;
  end;
end;

(*
 * @param MethodReader [in] - Type on server: TDBXReader
 *)
procedure TDSAdmin.CreateServerMethods( methodreader:TDBXReader);
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := POST;
    cmd.text := 'DSAdmin.CreateServerMethods';
    DSAdmin_CreateServerMethods;
    cmd.prepare(FDSAdmin_CreateServerMethods);
    cmd.parameters.items[0].Value.AsTable:=methodreader;
    Connection.execute(cmd);
  finally
    cmd.free;
  end;
end;

(*
 * @param MethodReader [in] - Type on server: TDBXReader
 *)
procedure TDSAdmin.DropServerMethods( methodreader:TDBXReader);
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := POST;
    cmd.text := 'DSAdmin.DropServerMethods';
    DSAdmin_DropServerMethods;
    cmd.prepare(FDSAdmin_DropServerMethods);
    cmd.parameters.items[0].Value.AsTable:=methodreader;
    Connection.execute(cmd);
  finally
    cmd.free;
  end;
end;

(*
 * @return result - Type on server: TDBXReader
 *)
function TDSAdmin.GetServerClasses():TDBXReader;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.GetServerClasses';
    DSAdmin_GetServerClasses;
    cmd.prepare(FDSAdmin_GetServerClasses);
    Connection.execute(cmd);

    result:= cmd.parameters.items[0].Value.AsTable as TDBXReader;
  finally
    cmd.free;
  end;
end;

(*
 * @return result - Type on server: TJSONArray
 *)
function TDSAdmin.ListClasses():TJSONArray;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.ListClasses';
    DSAdmin_ListClasses;
    cmd.prepare(FDSAdmin_ListClasses);
    Connection.execute(cmd);

    result:= cmd.parameters.items[0].Value.AsJSONValue as TJSONArray;
  finally
    cmd.free;
  end;
end;

(*
 * @param ClassName [in] - Type on server: string
 * @return result - Type on server: TJSONObject
 *)
function TDSAdmin.DescribeClass(_classname:string):TJSONObject;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.DescribeClass';
    DSAdmin_DescribeClass;
    cmd.prepare(FDSAdmin_DescribeClass);
    cmd.parameters.items[0].Value.AsString:=classname;
    Connection.execute(cmd);

    result:= cmd.parameters.items[1].Value.AsJSONValue as TJSONObject;
  finally
    cmd.free;
  end;
end;

(*
 * @param ClassName [in] - Type on server: string
 * @return result - Type on server: TJSONArray
 *)
function TDSAdmin.ListMethods(_classname:string):TJSONArray;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.ListMethods';
    DSAdmin_ListMethods;
    cmd.prepare(FDSAdmin_ListMethods);
    cmd.parameters.items[0].Value.AsString:=classname;
    Connection.execute(cmd);

    result:= cmd.parameters.items[1].Value.AsJSONValue as TJSONArray;
  finally
    cmd.free;
  end;
end;

(*
 * @param ServerMethodName [in] - Type on server: string
 * @return result - Type on server: TJSONObject
 *)
function TDSAdmin.DescribeMethod( servermethodname:string):TJSONObject;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.DescribeMethod';
    DSAdmin_DescribeMethod;
    cmd.prepare(FDSAdmin_DescribeMethod);
    cmd.parameters.items[0].Value.AsString:=servermethodname;
    Connection.execute(cmd);

    result:= cmd.parameters.items[1].Value.AsJSONValue as TJSONObject;
  finally
    cmd.free;
  end;
end;

(*
 * @return result - Type on server: TDBXReader
 *)
function TDSAdmin.GetServerMethods():TDBXReader;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.GetServerMethods';
    DSAdmin_GetServerMethods;
    cmd.prepare(FDSAdmin_GetServerMethods);
    Connection.execute(cmd);

    result:= cmd.parameters.items[0].Value.AsTable as TDBXReader;
  finally
    cmd.free;
  end;
end;

(*
 * @return result - Type on server: TDBXReader
 *)
function TDSAdmin.GetServerMethodParameters():TDBXReader;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.GetServerMethodParameters';
    DSAdmin_GetServerMethodParameters;
    cmd.prepare(FDSAdmin_GetServerMethodParameters);
    Connection.execute(cmd);

    result:= cmd.parameters.items[0].Value.AsTable as TDBXReader;
  finally
    cmd.free;
  end;
end;

(*
 * @return result - Type on server: TDBXReader
 *)
function TDSAdmin.GetDatabaseConnectionProperties():TDBXReader;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.GetDatabaseConnectionProperties';
    DSAdmin_GetDatabaseConnectionProperties;
    cmd.prepare(FDSAdmin_GetDatabaseConnectionProperties);
    Connection.execute(cmd);

    result:= cmd.parameters.items[0].Value.AsTable as TDBXReader;
  finally
    cmd.free;
  end;
end;

(*
 * @return result - Type on server: string
 *)
function TDSAdmin.GetDSServerName():string;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.GetDSServerName';
    DSAdmin_GetDSServerName;
    cmd.prepare(FDSAdmin_GetDSServerName);
    Connection.execute(cmd);

    result:= cmd.parameters.items[0].Value.AsString;
  finally
    cmd.free;
  end;
end;

(*
 * @param ChannelName [in] - Type on server: string
 * @param ClientManagerId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param ChannelNames [in] - Type on server: string
 * @param SecurityToken [in] - Type on server: string
 * @param ResponseData [in] - Type on server: TJSONValue
 * @return result - Type on server: TJSONValue
 *)
function TDSAdmin.ConsumeClientChannel( channelname:string; clientmanagerid:string; callbackid:string; channelnames:string; securitytoken:string; responsedata:TJSONValue):TJSONValue;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := POST;
    cmd.text := 'DSAdmin.ConsumeClientChannel';
    DSAdmin_ConsumeClientChannel;
    cmd.prepare(FDSAdmin_ConsumeClientChannel);
    cmd.parameters.items[0].Value.AsString:=channelname;
    cmd.parameters.items[1].Value.AsString:=clientmanagerid;
    cmd.parameters.items[2].Value.AsString:=callbackid;
    cmd.parameters.items[3].Value.AsString:=channelnames;
    cmd.parameters.items[4].Value.AsString:=securitytoken;
    cmd.parameters.items[5].Value.AsJSONValue:=responsedata;
    Connection.execute(cmd);

    result:= cmd.parameters.items[6].Value.AsJSONValue as TJSONValue;
  finally
    cmd.free;
  end;
end;

(*
 * @param ChannelName [in] - Type on server: string
 * @param ClientManagerId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param ChannelNames [in] - Type on server: string
 * @param SecurityToken [in] - Type on server: string
 * @param Timeout [in] - Type on server: Int32
 * @param ResponseData [in] - Type on server: TJSONValue
 * @return result - Type on server: TJSONValue
 *)
function TDSAdmin.ConsumeClientChannelTimeout( channelname:string; clientmanagerid:string; callbackid:string; channelnames:string; securitytoken:string; timeout:Int32; responsedata:TJSONValue):TJSONValue;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := POST;
    cmd.text := 'DSAdmin.ConsumeClientChannelTimeout';
    DSAdmin_ConsumeClientChannelTimeout;
    cmd.prepare(FDSAdmin_ConsumeClientChannelTimeout);
    cmd.parameters.items[0].Value.AsString:=channelname;
    cmd.parameters.items[1].Value.AsString:=clientmanagerid;
    cmd.parameters.items[2].Value.AsString:=callbackid;
    cmd.parameters.items[3].Value.AsString:=channelnames;
    cmd.parameters.items[4].Value.AsString:=securitytoken;
    cmd.parameters.items[5].Value.AsInt32:=timeout;
    cmd.parameters.items[6].Value.AsJSONValue:=responsedata;
    Connection.execute(cmd);

    result:= cmd.parameters.items[7].Value.AsJSONValue as TJSONValue;
  finally
    cmd.free;
  end;
end;

(*
 * @param ChannelId [in] - Type on server: string
 * @param SecurityToken [in] - Type on server: string
 * @return result - Type on server: boolean
 *)
function TDSAdmin.CloseClientChannel( channelid:string; securitytoken:string):boolean;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.CloseClientChannel';
    DSAdmin_CloseClientChannel;
    cmd.prepare(FDSAdmin_CloseClientChannel);
    cmd.parameters.items[0].Value.AsString:=channelid;
    cmd.parameters.items[1].Value.AsString:=securitytoken;
    Connection.execute(cmd);

    result:= cmd.parameters.items[2].Value.AsBoolean;
  finally
    cmd.free;
  end;
end;

(*
 * @param ChannelId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param ChannelNames [in] - Type on server: string
 * @param SecurityToken [in] - Type on server: string
 * @return result - Type on server: boolean
 *)
function TDSAdmin.RegisterClientCallbackServer( channelid:string; callbackid:string; channelnames:string; securitytoken:string):boolean;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.RegisterClientCallbackServer';
    DSAdmin_RegisterClientCallbackServer;
    cmd.prepare(FDSAdmin_RegisterClientCallbackServer);
    cmd.parameters.items[0].Value.AsString:=channelid;
    cmd.parameters.items[1].Value.AsString:=callbackid;
    cmd.parameters.items[2].Value.AsString:=channelnames;
    cmd.parameters.items[3].Value.AsString:=securitytoken;
    Connection.execute(cmd);

    result:= cmd.parameters.items[4].Value.AsBoolean;
  finally
    cmd.free;
  end;
end;

(*
 * @param ChannelId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param SecurityToken [in] - Type on server: string
 * @return result - Type on server: boolean
 *)
function TDSAdmin.UnregisterClientCallback( channelid:string; callbackid:string; securitytoken:string):boolean;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := GET;
    cmd.text := 'DSAdmin.UnregisterClientCallback';
    DSAdmin_UnregisterClientCallback;
    cmd.prepare(FDSAdmin_UnregisterClientCallback);
    cmd.parameters.items[0].Value.AsString:=channelid;
    cmd.parameters.items[1].Value.AsString:=callbackid;
    cmd.parameters.items[2].Value.AsString:=securitytoken;
    Connection.execute(cmd);

    result:= cmd.parameters.items[3].Value.AsBoolean;
  finally
    cmd.free;
  end;
end;

(*
 * @param ChannelName [in] - Type on server: string
 * @param Msg [in] - Type on server: TJSONValue
 * @return result - Type on server: boolean
 *)
function TDSAdmin.BroadcastToChannel( channelname:string; msg:TJSONValue):boolean;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := POST;
    cmd.text := 'DSAdmin.BroadcastToChannel';
    DSAdmin_BroadcastToChannel;
    cmd.prepare(FDSAdmin_BroadcastToChannel);
    cmd.parameters.items[0].Value.AsString:=channelname;
    cmd.parameters.items[1].Value.AsJSONValue:=msg;
    Connection.execute(cmd);

    result:= cmd.parameters.items[2].Value.AsBoolean;
  finally
    cmd.free;
  end;
end;

(*
 * @param ChannelName [in] - Type on server: string
 * @param Msg [in] - Type on server: TJSONObject
 * @return result - Type on server: boolean
 *)
function TDSAdmin.BroadcastObjectToChannel( channelname:string; msg:TJSONObject):boolean;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := POST;
    cmd.text := 'DSAdmin.BroadcastObjectToChannel';
    DSAdmin_BroadcastObjectToChannel;
    cmd.prepare(FDSAdmin_BroadcastObjectToChannel);
    cmd.parameters.items[0].Value.AsString:=channelname;
    cmd.parameters.items[1].Value.AsJSONValue:=msg;
    Connection.execute(cmd);

    result:= cmd.parameters.items[2].Value.AsBoolean;
  finally
    cmd.free;
  end;
end;

(*
 * @param ClientId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param Msg [in] - Type on server: TJSONValue
 * @param Response [out] - Type on server: TJSONValue
 * @return result - Type on server: boolean
 *)
function TDSAdmin.NotifyCallback( clientid:string; callbackid:string; msg:TJSONValue;out  response:TJSONValue):boolean;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := POST;
    cmd.text := 'DSAdmin.NotifyCallback';
    DSAdmin_NotifyCallback;
    cmd.prepare(FDSAdmin_NotifyCallback);
    cmd.parameters.items[0].Value.AsString:=clientid;
    cmd.parameters.items[1].Value.AsString:=callbackid;
    cmd.parameters.items[2].Value.AsJSONValue:=msg;
    Connection.execute(cmd);
    response :=   cmd.parameters.items[3].Value.AsJSONValue as TJSONValue;

    result:= cmd.parameters.items[4].Value.AsBoolean;
  finally
    cmd.free;
  end;
end;

(*
 * @param ClientId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param Msg [in] - Type on server: TJSONObject
 * @param Response [out] - Type on server: TJSONObject
 * @return result - Type on server: boolean
 *)
function TDSAdmin.NotifyObject( clientid:string; callbackid:string; msg:TJSONObject;out  response:TJSONObject):boolean;
var
  cmd: TDSRestCommand;
begin
  cmd := Connection.createCommand;
  try
    cmd.RequestType := POST;
    cmd.text := 'DSAdmin.NotifyObject';
    DSAdmin_NotifyObject;
    cmd.prepare(FDSAdmin_NotifyObject);
    cmd.parameters.items[0].Value.AsString:=clientid;
    cmd.parameters.items[1].Value.AsString:=callbackid;
    cmd.parameters.items[2].Value.AsJSONValue:=msg;
    Connection.execute(cmd);
    response :=   cmd.parameters.items[3].Value.AsJSONValue as TJSONObject;

    result:= cmd.parameters.items[4].Value.AsBoolean;
  finally
    cmd.free;
  end;
end;

end.

