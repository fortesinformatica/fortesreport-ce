//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "DSAdmin.h"
#import "DSRESTParameterMetaData.h"


@implementation DSAdmin
@synthesize Connection;
-(id) initWithConnection:(DSRESTConnection *) aConnection {
	self  = [self init];
	if (self) {
		
		self.Connection =aConnection ;
	}
	return self;
	
}


-(id) getDSAdmin_GetPlatformName {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:WideStringType withTypeName:@"string"],
			 nil];
}
-(id) getDSAdmin_ClearResources {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:BooleanType withTypeName:@"Boolean"],
			 nil];
}
-(id) getDSAdmin_FindPackages {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:TableType withTypeName:@"TDBXReader"],
			 nil];
}
-(id) getDSAdmin_FindClasses {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"PackageName" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"ClassPattern" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:TableType withTypeName:@"TDBXReader"],
			 nil];
}
-(id) getDSAdmin_FindMethods {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"PackageName" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"ClassPattern" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"MethodPattern" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:TableType withTypeName:@"TDBXReader"],
			 nil];
}
-(id) getDSAdmin_CreateServerClasses {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"ClassReader" withDirection:Input withDBXType:TableType withTypeName:@"TDBXReader"],
			 nil];
}
-(id) getDSAdmin_DropServerClasses {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"ClassReader" withDirection:Input withDBXType:TableType withTypeName:@"TDBXReader"],
			 nil];
}
-(id) getDSAdmin_CreateServerMethods {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"MethodReader" withDirection:Input withDBXType:TableType withTypeName:@"TDBXReader"],
			 nil];
}
-(id) getDSAdmin_DropServerMethods {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"MethodReader" withDirection:Input withDBXType:TableType withTypeName:@"TDBXReader"],
			 nil];
}
-(id) getDSAdmin_GetServerClasses {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:TableType withTypeName:@"TDBXReader"],
			 nil];
}
-(id) getDSAdmin_ListClasses {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:JsonValueType withTypeName:@"TJSONArray"],
			 nil];
}
-(id) getDSAdmin_DescribeClass {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"ClassName" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:JsonValueType withTypeName:@"TJSONObject"],
			 nil];
}
-(id) getDSAdmin_ListMethods {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"ClassName" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:JsonValueType withTypeName:@"TJSONArray"],
			 nil];
}
-(id) getDSAdmin_DescribeMethod {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"ServerMethodName" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:JsonValueType withTypeName:@"TJSONObject"],
			 nil];
}
-(id) getDSAdmin_GetServerMethods {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:TableType withTypeName:@"TDBXReader"],
			 nil];
}
-(id) getDSAdmin_GetServerMethodParameters {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:TableType withTypeName:@"TDBXReader"],
			 nil];
}
-(id) getDSAdmin_GetDatabaseConnectionProperties {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:TableType withTypeName:@"TDBXReader"],
			 nil];
}
-(id) getDSAdmin_GetDSServerName {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:WideStringType withTypeName:@"string"],
			 nil];
}
-(id) getDSAdmin_ConsumeClientChannel {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"ChannelName" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"ClientManagerId" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"CallbackId" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"ChannelNames" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"SecurityToken" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"ResponseData" withDirection:Input withDBXType:JsonValueType withTypeName:@"TJSONValue"],
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:JsonValueType withTypeName:@"TJSONValue"],
			 nil];
}
-(id) getDSAdmin_ConsumeClientChannelTimeout {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"ChannelName" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"ClientManagerId" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"CallbackId" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"ChannelNames" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"SecurityToken" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"Timeout" withDirection:Input withDBXType:Int32Type withTypeName:@"Integer"],
			 [DSRESTParameterMetaData parameterWithName: @"ResponseData" withDirection:Input withDBXType:JsonValueType withTypeName:@"TJSONValue"],
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:JsonValueType withTypeName:@"TJSONValue"],
			 nil];
}
-(id) getDSAdmin_CloseClientChannel {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"ChannelId" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"SecurityToken" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:BooleanType withTypeName:@"Boolean"],
			 nil];
}
-(id) getDSAdmin_RegisterClientCallbackServer {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"ChannelId" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"CallbackId" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"ChannelNames" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"SecurityToken" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:BooleanType withTypeName:@"Boolean"],
			 nil];
}
-(id) getDSAdmin_UnregisterClientCallback {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"ChannelId" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"CallbackId" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"SecurityToken" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:BooleanType withTypeName:@"Boolean"],
			 nil];
}
-(id) getDSAdmin_BroadcastToChannel {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"ChannelName" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"Msg" withDirection:Input withDBXType:JsonValueType withTypeName:@"TJSONValue"],
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:BooleanType withTypeName:@"Boolean"],
			 nil];
}
-(id) getDSAdmin_BroadcastObjectToChannel {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"ChannelName" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"Msg" withDirection:Input withDBXType:JsonValueType withTypeName:@"TObject"],
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:BooleanType withTypeName:@"Boolean"],
			 nil];
}
-(id) getDSAdmin_NotifyCallback {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"ClientId" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"CallbackId" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"Msg" withDirection:Input withDBXType:JsonValueType withTypeName:@"TJSONValue"],
			 [DSRESTParameterMetaData parameterWithName: @"Response" withDirection:Output withDBXType:JsonValueType withTypeName:@"TJSONValue"],
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:BooleanType withTypeName:@"Boolean"],
			 nil];
}
-(id) getDSAdmin_NotifyObject {
    return  [NSArray arrayWithObjects:
			 [DSRESTParameterMetaData parameterWithName: @"ClientId" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"CallbackId" withDirection:Input withDBXType:WideStringType withTypeName:@"string"],
			 [DSRESTParameterMetaData parameterWithName: @"Msg" withDirection:Input withDBXType:JsonValueType withTypeName:@"TObject"],
			 [DSRESTParameterMetaData parameterWithName: @"Response" withDirection:Output withDBXType:JsonValueType withTypeName:@"TObject"],
			 [DSRESTParameterMetaData parameterWithName: @"" withDirection:ReturnValue withDBXType:BooleanType withTypeName:@"Boolean"],
			 nil];
}

/**
 * @return result - Type on server: string
 */
-(NSString *) GetPlatformName{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.GetPlatformName";
    [cmd  prepare:[self getDSAdmin_GetPlatformName]];
    
    
    [Connection execute: cmd];
    
    return [[[cmd.parameters objectAtIndex:0]getValue]GetAsString];
}

/**
 * @return result - Type on server: Boolean
 */
-(bool) ClearResources{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.ClearResources";
    [cmd  prepare:[self getDSAdmin_ClearResources]];
    
    
    [Connection execute: cmd];
    
    return [[[cmd.parameters objectAtIndex:0]getValue]GetAsBoolean];
}

/**
 * @return result - Type on server: TDBXReader
 */
-(TDBXReader*) FindPackages{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.FindPackages";
    [cmd  prepare:[self getDSAdmin_FindPackages]];
    
    
    [Connection execute: cmd];
    
    return (TDBXReader*)[[[cmd.parameters objectAtIndex:0]getValue]GetAsTable];
}

/**
 * @param PackageName [in] - Type on server: string
 * @param ClassPattern [in] - Type on server: string
 * @return result - Type on server: TDBXReader
 */
-(TDBXReader*) FindClasses: (NSString *) packagename  withClassPattern: (NSString *) classpattern{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.FindClasses";
    [cmd  prepare:[self getDSAdmin_FindClasses]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsString:packagename];
    [[[cmd.parameters objectAtIndex:1]getValue]SetAsString:classpattern];
    
    [Connection execute: cmd];
    
    return (TDBXReader*)[[[cmd.parameters objectAtIndex:2]getValue]GetAsTable];
}

/**
 * @param PackageName [in] - Type on server: string
 * @param ClassPattern [in] - Type on server: string
 * @param MethodPattern [in] - Type on server: string
 * @return result - Type on server: TDBXReader
 */
-(TDBXReader*) FindMethods: (NSString *) packagename  withClassPattern: (NSString *) classpattern withMethodPattern: (NSString *) methodpattern{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.FindMethods";
    [cmd  prepare:[self getDSAdmin_FindMethods]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsString:packagename];
    [[[cmd.parameters objectAtIndex:1]getValue]SetAsString:classpattern];
    [[[cmd.parameters objectAtIndex:2]getValue]SetAsString:methodpattern];
    
    [Connection execute: cmd];
    
    return (TDBXReader*)[[[cmd.parameters objectAtIndex:3]getValue]GetAsTable];
}

/**
 * @param ClassReader [in] - Type on server: TDBXReader
 */
-(void) CreateServerClasses: (TDBXReader*) classreader {
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  POST;
    cmd.text= @"DSAdmin.CreateServerClasses";
    [cmd  prepare:[self getDSAdmin_CreateServerClasses]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsTable:classreader];
    
    [Connection execute: cmd];
}

/**
 * @param ClassReader [in] - Type on server: TDBXReader
 */
-(void) DropServerClasses: (TDBXReader*) classreader {
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  POST;
    cmd.text= @"DSAdmin.DropServerClasses";
    [cmd  prepare:[self getDSAdmin_DropServerClasses]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsTable:classreader];
    
    [Connection execute: cmd];
}

/**
 * @param MethodReader [in] - Type on server: TDBXReader
 */
-(void) CreateServerMethods: (TDBXReader*) methodreader {
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  POST;
    cmd.text= @"DSAdmin.CreateServerMethods";
    [cmd  prepare:[self getDSAdmin_CreateServerMethods]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsTable:methodreader];
    
    [Connection execute: cmd];
}

/**
 * @param MethodReader [in] - Type on server: TDBXReader
 */
-(void) DropServerMethods: (TDBXReader*) methodreader {
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  POST;
    cmd.text= @"DSAdmin.DropServerMethods";
    [cmd  prepare:[self getDSAdmin_DropServerMethods]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsTable:methodreader];
    
    [Connection execute: cmd];
}

/**
 * @return result - Type on server: TDBXReader
 */
-(TDBXReader*) GetServerClasses{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.GetServerClasses";
    [cmd  prepare:[self getDSAdmin_GetServerClasses]];
    
    
    [Connection execute: cmd];
    
    return (TDBXReader*)[[[cmd.parameters objectAtIndex:0]getValue]GetAsTable];
}

/**
 * @return result - Type on server: TJSONArray
 */
-(TJSONArray *) ListClasses{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.ListClasses";
    [cmd  prepare:[self getDSAdmin_ListClasses]];
    
    
    [Connection execute: cmd];
    
    return (TJSONArray *)[[[cmd.parameters objectAtIndex:0]getValue]GetAsJSONValue];
}

/**
 * @param ClassName [in] - Type on server: string
 * @return result - Type on server: TJSONObject
 */
-(TJSONObject *) DescribeClass: (NSString *) classname {
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.DescribeClass";
    [cmd  prepare:[self getDSAdmin_DescribeClass]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsString:classname];
    
    [Connection execute: cmd];
    
    return (TJSONObject *)[[[cmd.parameters objectAtIndex:1]getValue]GetAsJSONValue];
}

/**
 * @param ClassName [in] - Type on server: string
 * @return result - Type on server: TJSONArray
 */
-(TJSONArray *) ListMethods: (NSString *) classname {
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.ListMethods";
    [cmd  prepare:[self getDSAdmin_ListMethods]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsString:classname];
    
    [Connection execute: cmd];
    
    return (TJSONArray *)[[[cmd.parameters objectAtIndex:1]getValue]GetAsJSONValue];
}

/**
 * @param ServerMethodName [in] - Type on server: string
 * @return result - Type on server: TJSONObject
 */
-(TJSONObject *) DescribeMethod: (NSString *) servermethodname {
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.DescribeMethod";
    [cmd  prepare:[self getDSAdmin_DescribeMethod]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsString:servermethodname];
    
    [Connection execute: cmd];
    
    return (TJSONObject *)[[[cmd.parameters objectAtIndex:1]getValue]GetAsJSONValue];
}

/**
 * @return result - Type on server: TDBXReader
 */
-(TDBXReader*) GetServerMethods{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.GetServerMethods";
    [cmd  prepare:[self getDSAdmin_GetServerMethods]];
    
    
    [Connection execute: cmd];
    
    return (TDBXReader*)[[[cmd.parameters objectAtIndex:0]getValue]GetAsTable];
}

/**
 * @return result - Type on server: TDBXReader
 */
-(TDBXReader*) GetServerMethodParameters{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.GetServerMethodParameters";
    [cmd  prepare:[self getDSAdmin_GetServerMethodParameters]];
    
    
    [Connection execute: cmd];
    
    return (TDBXReader*)[[[cmd.parameters objectAtIndex:0]getValue]GetAsTable];
}

/**
 * @return result - Type on server: TDBXReader
 */
-(TDBXReader*) GetDatabaseConnectionProperties{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.GetDatabaseConnectionProperties";
    [cmd  prepare:[self getDSAdmin_GetDatabaseConnectionProperties]];
    
    
    [Connection execute: cmd];
    
    return (TDBXReader*)[[[cmd.parameters objectAtIndex:0]getValue]GetAsTable];
}

/**
 * @return result - Type on server: string
 */
-(NSString *) GetDSServerName{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.GetDSServerName";
    [cmd  prepare:[self getDSAdmin_GetDSServerName]];
    
    
    [Connection execute: cmd];
    
    return [[[cmd.parameters objectAtIndex:0]getValue]GetAsString];
}

/**
 * @param ChannelName [in] - Type on server: string
 * @param ClientManagerId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param ChannelNames [in] - Type on server: string
 * @param SecurityToken [in] - Type on server: string
 * @param ResponseData [in] - Type on server: TJSONValue
 * @return result - Type on server: TJSONValue
 */
-(TJSONValue *) ConsumeClientChannel: (NSString *) channelname  withClientManagerId: (NSString *) clientmanagerid withCallbackId: (NSString *) callbackid withChannelNames: (NSString *) channelnames withSecurityToken: (NSString *) securitytoken withResponseData: (TJSONValue *) responsedata{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  POST;
    cmd.text= @"DSAdmin.ConsumeClientChannel";
    [cmd  prepare:[self getDSAdmin_ConsumeClientChannel]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsString:channelname];
    [[[cmd.parameters objectAtIndex:1]getValue]SetAsString:clientmanagerid];
    [[[cmd.parameters objectAtIndex:2]getValue]SetAsString:callbackid];
    [[[cmd.parameters objectAtIndex:3]getValue]SetAsString:channelnames];
    [[[cmd.parameters objectAtIndex:4]getValue]SetAsString:securitytoken];
    [[[cmd.parameters objectAtIndex:5]getValue]SetAsJSONValue:responsedata];
    
    [Connection execute: cmd];
    
    return (TJSONValue *)[[[cmd.parameters objectAtIndex:6]getValue]GetAsJSONValue];
}

/**
 * @param ChannelName [in] - Type on server: string
 * @param ClientManagerId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param ChannelNames [in] - Type on server: string
 * @param SecurityToken [in] - Type on server: string
 * @param Timeout [in] - Type on server: Integer
 * @param ResponseData [in] - Type on server: TJSONValue
 * @return result - Type on server: TJSONValue
 */
-(TJSONValue *) ConsumeClientChannelTimeout: (NSString *) channelname  withClientManagerId: (NSString *) clientmanagerid withCallbackId: (NSString *) callbackid withChannelNames: (NSString *) channelnames withSecurityToken: (NSString *) securitytoken withTimeout: (long) timeout withResponseData: (TJSONValue *) responsedata{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  POST;
    cmd.text= @"DSAdmin.ConsumeClientChannelTimeout";
    [cmd  prepare:[self getDSAdmin_ConsumeClientChannelTimeout]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsString:channelname];
    [[[cmd.parameters objectAtIndex:1]getValue]SetAsString:clientmanagerid];
    [[[cmd.parameters objectAtIndex:2]getValue]SetAsString:callbackid];
    [[[cmd.parameters objectAtIndex:3]getValue]SetAsString:channelnames];
    [[[cmd.parameters objectAtIndex:4]getValue]SetAsString:securitytoken];
    [[[cmd.parameters objectAtIndex:5]getValue]SetAsInt32:timeout];
    [[[cmd.parameters objectAtIndex:6]getValue]SetAsJSONValue:responsedata];
    
    [Connection execute: cmd];
    
    return (TJSONValue *)[[[cmd.parameters objectAtIndex:7]getValue]GetAsJSONValue];
}

/**
 * @param ChannelId [in] - Type on server: string
 * @param SecurityToken [in] - Type on server: string
 * @return result - Type on server: Boolean
 */
-(bool) CloseClientChannel: (NSString *) channelid  withSecurityToken: (NSString *) securitytoken{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.CloseClientChannel";
    [cmd  prepare:[self getDSAdmin_CloseClientChannel]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsString:channelid];
    [[[cmd.parameters objectAtIndex:1]getValue]SetAsString:securitytoken];
    
    [Connection execute: cmd];
    
    return [[[cmd.parameters objectAtIndex:2]getValue]GetAsBoolean];
}

/**
 * @param ChannelId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param ChannelNames [in] - Type on server: string
 * @param SecurityToken [in] - Type on server: string
 * @return result - Type on server: Boolean
 */
-(bool) RegisterClientCallbackServer: (NSString *) channelid  withCallbackId: (NSString *) callbackid withChannelNames: (NSString *) channelnames withSecurityToken: (NSString *) securitytoken{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.RegisterClientCallbackServer";
    [cmd  prepare:[self getDSAdmin_RegisterClientCallbackServer]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsString:channelid];
    [[[cmd.parameters objectAtIndex:1]getValue]SetAsString:callbackid];
    [[[cmd.parameters objectAtIndex:2]getValue]SetAsString:channelnames];
    [[[cmd.parameters objectAtIndex:3]getValue]SetAsString:securitytoken];
    
    [Connection execute: cmd];
    
    return [[[cmd.parameters objectAtIndex:4]getValue]GetAsBoolean];
}

/**
 * @param ChannelId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param SecurityToken [in] - Type on server: string
 * @return result - Type on server: Boolean
 */
-(bool) UnregisterClientCallback: (NSString *) channelid  withCallbackId: (NSString *) callbackid withSecurityToken: (NSString *) securitytoken{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  GET;
    cmd.text= @"DSAdmin.UnregisterClientCallback";
    [cmd  prepare:[self getDSAdmin_UnregisterClientCallback]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsString:channelid];
    [[[cmd.parameters objectAtIndex:1]getValue]SetAsString:callbackid];
    [[[cmd.parameters objectAtIndex:2]getValue]SetAsString:securitytoken];
    
    [Connection execute: cmd];
    
    return [[[cmd.parameters objectAtIndex:3]getValue]GetAsBoolean];
}

/**
 * @param ChannelName [in] - Type on server: string
 * @param Msg [in] - Type on server: TJSONValue
 * @return result - Type on server: Boolean
 */
-(bool) BroadcastToChannel: (NSString *) channelname  withMsg: (TJSONValue *) msg{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  POST;
    cmd.text= @"DSAdmin.BroadcastToChannel";
    [cmd  prepare:[self getDSAdmin_BroadcastToChannel]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsString:channelname];
    [[[cmd.parameters objectAtIndex:1]getValue]SetAsJSONValue:msg];
    
    [Connection execute: cmd];
    
    return [[[cmd.parameters objectAtIndex:2]getValue]GetAsBoolean];
}

/**
 * @param ChannelName [in] - Type on server: string
 * @param Msg [in] - Type on server: TObject
 * @return result - Type on server: Boolean
 */
-(bool) BroadcastObjectToChannel: (NSString *) channelname  withMsg: (TJSONObject *) msg{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  POST;
    cmd.text= @"DSAdmin.BroadcastObjectToChannel";
    [cmd  prepare:[self getDSAdmin_BroadcastObjectToChannel]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsString:channelname];
    [[[cmd.parameters objectAtIndex:1]getValue]SetAsJSONValue:msg];
    
    [Connection execute: cmd];
    
    return [[[cmd.parameters objectAtIndex:2]getValue]GetAsBoolean];
}

/**
 * @param ClientId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param Msg [in] - Type on server: TJSONValue
 * @param Response [out] - Type on server: TJSONValue
 * @return result - Type on server: Boolean
 */
-(bool) NotifyCallback: (NSString *) clientid  withCallbackId: (NSString *) callbackid withMsg: (TJSONValue *) msg withResponse: (out TJSONValue **) response{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  POST;
    cmd.text= @"DSAdmin.NotifyCallback";
    [cmd  prepare:[self getDSAdmin_NotifyCallback]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsString:clientid];
    [[[cmd.parameters objectAtIndex:1]getValue]SetAsString:callbackid];
    [[[cmd.parameters objectAtIndex:2]getValue]SetAsJSONValue:msg];
    
    [Connection execute: cmd];
    *response =  (TJSONValue *) [[[cmd.parameters objectAtIndex:3]getValue]GetAsJSONValue];
    
    return [[[cmd.parameters objectAtIndex:4]getValue]GetAsBoolean];
}

/**
 * @param ClientId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param Msg [in] - Type on server: TObject
 * @param Response [out] - Type on server: TObject
 * @return result - Type on server: Boolean
 */
-(bool) NotifyObject: (NSString *) clientid  withCallbackId: (NSString *) callbackid withMsg: (TJSONObject *) msg withResponse: (out TJSONObject **) response{
    
    DSRESTCommand * cmd = [[self Connection ] CreateCommand];
    cmd.RequestType =  POST;
    cmd.text= @"DSAdmin.NotifyObject";
    [cmd  prepare:[self getDSAdmin_NotifyObject]];
    
    [[[cmd.parameters objectAtIndex:0]getValue]SetAsString:clientid];
    [[[cmd.parameters objectAtIndex:1]getValue]SetAsString:callbackid];
    [[[cmd.parameters objectAtIndex:2]getValue]SetAsJSONValue:msg];
    
    [Connection execute: cmd];
    *response =  (TJSONObject *) [[[cmd.parameters objectAtIndex:3]getValue]GetAsJSONValue];
    
    return [[[cmd.parameters objectAtIndex:4]getValue]GetAsBoolean];
}
@end
