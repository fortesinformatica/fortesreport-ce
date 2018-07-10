//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

/**
 *@brief Provides administrative built-in server methods. 

 */
#import <Foundation/Foundation.h>
#import "DSRESTConnection.h"
#import "DSRESTParameterMetaData.h"
#import "DBException.h"
#import "TParams.h"
#import "TDBXReader.h"
#import "TDataSet.h"
#import "TJSONNull.h"
#import "TJSONValue.h"
#import "TJSONObject.h"
#import "TJSONArray.h"
#import "TJSONTrue.h"
#import "TJSONFalse.h"
#import "DBXProtocols.h"
#import "TDBXAnsiStringValue.h"
#import "TDBXAnsiCharsValue.h"
#import "TDBXBooleanValue.h"
#import "TDBXDoubleValue.h"
#import "TDBXUInt8Value.h"
#import "TDBXInt8Value.h"
#import "TDBXUInt16Value.h"
#import "TDBXInt16Value.h"
#import "TDBXInt32Value.h"
#import "TDBXInt64Value.h"
#import "TDBXSingleValue.h"
#import "TDBXStringValue.h"
#import "TDBXTimeValue.h"
#import "TDBXTimeStampValue.h"
#import "TDBXWideStringValue.h"
#import "TDBXDateValue.h"
#import "TDBXReaderValue.h"
#import "TDBXStreamValue.h"
#import "TDBXBcdValue.h"

@interface DSAdmin:NSObject{
    DSRESTConnection * Connection;
	
} 

/**
 *Intialize the admin with a connection 
 *@param aConnection the connection to connect to te server 
 */
@property (nonatomic,retain) DSRESTConnection * Connection;  
-(id) initWithConnection:(DSRESTConnection *) aConnection ;

/**
 * @return result - Type on server: string
 */
-(NSString *) GetPlatformName;

/**
 * @return result - Type on server: Boolean
 */
-(bool) ClearResources;

/**
 * @return result - Type on server: TDBXReader
 */
-(TDBXReader*) FindPackages;

/**
 * @param PackageName [in] - Type on server: string
 * @param ClassPattern [in] - Type on server: string
 * @return result - Type on server: TDBXReader
 */
-(TDBXReader*) FindClasses: (NSString *) packagename  withClassPattern: (NSString *) classpattern;

/**
 * @param PackageName [in] - Type on server: string
 * @param ClassPattern [in] - Type on server: string
 * @param MethodPattern [in] - Type on server: string
 * @return result - Type on server: TDBXReader
 */
-(TDBXReader*) FindMethods: (NSString *) packagename  withClassPattern: (NSString *) classpattern withMethodPattern: (NSString *) methodpattern;

/**
 * @param ClassReader [in] - Type on server: TDBXReader
 */
-(void) CreateServerClasses: (TDBXReader*) classreader ;

/**
 * @param ClassReader [in] - Type on server: TDBXReader
 */
-(void) DropServerClasses: (TDBXReader*) classreader ;

/**
 * @param MethodReader [in] - Type on server: TDBXReader
 */
-(void) CreateServerMethods: (TDBXReader*) methodreader ;

/**
 * @param MethodReader [in] - Type on server: TDBXReader
 */
-(void) DropServerMethods: (TDBXReader*) methodreader ;

/**
 * @return result - Type on server: TDBXReader
 */
-(TDBXReader*) GetServerClasses;

/**
 * @return result - Type on server: TJSONArray
 */
-(TJSONArray *) ListClasses;

/**
 * @param ClassName [in] - Type on server: string
 * @return result - Type on server: TJSONObject
 */
-(TJSONObject *) DescribeClass: (NSString *) classname ;

/**
 * @param ClassName [in] - Type on server: string
 * @return result - Type on server: TJSONArray
 */
-(TJSONArray *) ListMethods: (NSString *) classname ;

/**
 * @param ServerMethodName [in] - Type on server: string
 * @return result - Type on server: TJSONObject
 */
-(TJSONObject *) DescribeMethod: (NSString *) servermethodname ;

/**
 * @return result - Type on server: TDBXReader
 */
-(TDBXReader*) GetServerMethods;

/**
 * @return result - Type on server: TDBXReader
 */
-(TDBXReader*) GetServerMethodParameters;

/**
 * @return result - Type on server: TDBXReader
 */
-(TDBXReader*) GetDatabaseConnectionProperties;

/**
 * @return result - Type on server: string
 */
-(NSString *) GetDSServerName;

/**
 * @param ChannelName [in] - Type on server: string
 * @param ClientManagerId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param ChannelNames [in] - Type on server: string
 * @param SecurityToken [in] - Type on server: string
 * @param ResponseData [in] - Type on server: TJSONValue
 * @return result - Type on server: TJSONValue
 */
-(TJSONValue *) ConsumeClientChannel: (NSString *) channelname  withClientManagerId: (NSString *) clientmanagerid withCallbackId: (NSString *) callbackid withChannelNames: (NSString *) channelnames withSecurityToken: (NSString *) securitytoken withResponseData: (TJSONValue *) responsedata;

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
-(TJSONValue *) ConsumeClientChannelTimeout: (NSString *) channelname  withClientManagerId: (NSString *) clientmanagerid withCallbackId: (NSString *) callbackid withChannelNames: (NSString *) channelnames withSecurityToken: (NSString *) securitytoken withTimeout: (long) timeout withResponseData: (TJSONValue *) responsedata;

/**
 * @param ChannelId [in] - Type on server: string
 * @param SecurityToken [in] - Type on server: string
 * @return result - Type on server: Boolean
 */
-(bool) CloseClientChannel: (NSString *) channelid  withSecurityToken: (NSString *) securitytoken;

/**
 * @param ChannelId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param ChannelNames [in] - Type on server: string
 * @param SecurityToken [in] - Type on server: string
 * @return result - Type on server: Boolean
 */
-(bool) RegisterClientCallbackServer: (NSString *) channelid  withCallbackId: (NSString *) callbackid withChannelNames: (NSString *) channelnames withSecurityToken: (NSString *) securitytoken;

/**
 * @param ChannelId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param SecurityToken [in] - Type on server: string
 * @return result - Type on server: Boolean
 */
-(bool) UnregisterClientCallback: (NSString *) channelid  withCallbackId: (NSString *) callbackid withSecurityToken: (NSString *) securitytoken;

/**
 * @param ChannelName [in] - Type on server: string
 * @param Msg [in] - Type on server: TJSONValue
 * @return result - Type on server: Boolean
 */
-(bool) BroadcastToChannel: (NSString *) channelname  withMsg: (TJSONValue *) msg;

/**
 * @param ChannelName [in] - Type on server: string
 * @param Msg [in] - Type on server: TObject
 * @return result - Type on server: Boolean
 */
-(bool) BroadcastObjectToChannel: (NSString *) channelname  withMsg: (TJSONObject *) msg;

/**
 * @param ClientId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param Msg [in] - Type on server: TJSONValue
 * @param Response [out] - Type on server: TJSONValue
 * @return result - Type on server: Boolean
 */
-(bool) NotifyCallback: (NSString *) clientid  withCallbackId: (NSString *) callbackid withMsg: (TJSONValue *) msg withResponse: (out TJSONValue **) response;

/**
 * @param ClientId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param Msg [in] - Type on server: TObject
 * @param Response [out] - Type on server: TObject
 * @return result - Type on server: Boolean
 */
-(bool) NotifyObject: (NSString *) clientid  withCallbackId: (NSString *) callbackid withMsg: (TJSONObject *) msg withResponse: (out TJSONObject **) response;

@end
