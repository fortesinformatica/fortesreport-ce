//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************


#import "DSRESTConnection.h"
#import "DBXJsonTools.h"
#import "DSRESTParameterMetaData.h"
#import "DBXDefaultFormatter.h"
#import "DSAdmin.h"
#import "DBXConnection.h"






 /**
 * @brief Allows you to prepare and execute a request of type REST.
 * 
 * Use the properties and methods of DSRESTCommand to: <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Set and get the request type. <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Set and get the specific method to call. <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Prepare the command from a metadata input
 * parameter. <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Get all the parameters contained. <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Get a parameter by index or name.
 * 
 */

@implementation DSRESTCommand

@synthesize Connection;
@synthesize RequestType;
@synthesize text = FullyQualifiedMethodName;
@synthesize parameters;
-(id) init {
	self = [super init];
	if (self) {
	 parameters =	[NSMutableArray arrayWithCapacity:1];
	}
	return self;
}


-(id) initWithConnection: (DSRESTConnection *) aconnection{
	self = [self init];
	if (self) {
		Connection = aconnection ;

		
	}
	return self;
}
/**
 *It prepares parameters list using information in an NSArray
 *@param metadatas An NSArray with metadatas to build the paramaters list 
 */

-(void) prepare: (NSArray *) metadatas{
	
	
	for (DSRESTParameterMetaData * param in metadatas)
	{
		DSRestParameter * p = [[DSRestParameter alloc]init];		
	
	
		p.direction = param.Direction;
		p.typeName = param.TypeName;
		p.DBXType = param.DBXType;
	    p.name = param.Name;
		[[p getValue]setDBXType:param.DBXType];
		[parameters addObject:p];
		
	
	}
	
		
}
/**
*  It prepares internal parameters array getting information from the server.
*/
-(void) prepare {
	NSString * LMethodName = [self text];
	DSAdmin * Admin = [[DSAdmin alloc]initWithConnection:Connection];
	@try{
		TDBXReader * MetaDatas = [Admin GetServerMethodParameters];
		bool Cicla = YES;
		bool IsEqual = NO;
		while ([MetaDatas next]) {
			if(Cicla && IsEqual) break;
			if ([LMethodName isEqualToString:[[MetaDatas getValueByName:@"MethodAlias"] GetAsString]]){
				
				IsEqual = YES;
				Cicla = NO;
				if ([[MetaDatas getValueByName:@"DBXType"] GetAsInt32] > 0){
				DSRestParameter* p = [[DSRestParameter alloc]init];
				if ([[MetaDatas getValueByName:@"Name"] isNull] ) continue;
				p.name = [[MetaDatas getValueByName:@"Name"] GetAsString];
				p.direction = [[MetaDatas getValueByName:@"DBXParameterDirection"] GetAsInt32];
				p.DBXType = [[MetaDatas getValueByName:@"DBXType"] GetAsInt32];
				p.typeName = [[MetaDatas getValueByName:@"ParameterTypeName"] GetAsString];
				[[p getValue] setDBXType:p.DBXType];
				[parameters addObject:p] ;
				}
			} else {
				Cicla = YES;
			}
		}
	}@finally {
	}
}


-(DSRestParameter *) getParameterByIndex:(int) index {
	return [parameters objectAtIndex:index] ; 
}
-(DSRestParameter *) getParameterByName:(NSString *) Name{
	for (DSRestParameter * p in parameters){
		if ([[p name]isEqualToString:Name]) {
			return p ;
		}
		
	}
	return nil;
}
/**
 * Execute the request from a specific {@link DSRESTCommand} input, that
 * will contain useful information to construct the URL as the type of
 * request, the method to execute and the parameters to be passed. This
 * information be added to those contained in this object as protocol,
 * target host, context... They form the complete request to execute. This
 * method is need to pass parameters correctly or under the parameter
 * direction, it will be append on the url string or written in the body of
 * the request. Upon receipt of the response will have to check the
 * correctness of the received parameters and set them in the
 * {@link DSRESTCommand}.
 * 
 * @param command
 *            the specific {@link DSRESTCommand}
 * 
 */
-(void) execute{
	[Connection execute:self];
}
@end


@implementation DSRESTConnection
@synthesize Port;
@synthesize UrlPath;
@synthesize Host;
@synthesize protocol;
@synthesize Context;
@synthesize SessionID;
@synthesize SessionIDExpires;
@synthesize UserName;
@synthesize Password;
@synthesize connectionTimeout;


-(void) closeSession{
	SessionID = nil;
	
	SessionIDExpires = -1;
}

-(id) initWithDelegate: (id) aDelegate{
	self= [self init];
	if (self) {
		internalDelegate = aDelegate;
	}
	return self;
}
-(id) init {
	self = [super init];
	if (self) {
		UrlPath = @"";
		Host = @"";
		protocol = @"";
		Context = @"";
		SessionID = @"";
		[self closeSession];
		connectionTimeout = 60;
	}
	return self;
}
-(void) setDelegate:(id) aDelegate{
	internalDelegate = aDelegate;

}
-(id)getDelegate{
	return internalDelegate;
}



-(NSString *) encodeURIComponent:(NSString *) value{
	

	 NSString * s =(NSString *)
	CFBridgingRelease(CFURLCreateStringByAddingPercentEscapes( NULL,
											(CFStringRef)value,
											NULL,
											
											(CFStringRef)@"!*'();:@&=+$,/?%#[]",
											kCFStringEncodingUTF8 ));
	
	NSString * r =[NSString stringWithString: s];
	return r ;
}
-(NSString *) encodeURIComponentWithParam:(DSRestParameter *) parameter {
	
	DBXValue* val =[parameter getValue];
	NSString * r =[self encodeURIComponent: [ val toString]];
	return r ;
	
}

-(DSRESTCommand *) CreateCommand{
	return[[DSRESTCommand alloc]initWithConnection:self];
	
}
-(void) SetUpSessionHeader:(NSMutableURLRequest*) request {
	
	if (SessionID){
		[request setValue:[NSString stringWithFormat:@"%@=%@",@"dssession",SessionID] forHTTPHeaderField:@"Pragma"];
	}


}

-(NSString *) buildRequestURL:(DSRESTCommand *) command{
	
	NSMutableString *LPathPrefix = [[NSMutableString alloc] initWithCapacity:0];
	if (![UrlPath isEqualToString:@""] ) {
	  [LPathPrefix appendString:[self UrlPath]];
		}
	
	int LPort = [self Port];
	
	NSMutableString * LMethodName = [[NSMutableString alloc] initWithString:command.text];
	
	NSString * LProtocol = [[NSString  alloc] initWithString:[self protocol]];
	
	if ([LProtocol isEqualToString:@""]){
		LProtocol = @"http";}
	
	NSString * LHost = [[NSString alloc] initWithString:   [self Host]];
	if ([LHost isEqualToString:@""]) {
		LHost = @"localhost";}
	if (![LPathPrefix isEqualToString:@""]){
		[ LPathPrefix insertString:@"/" atIndex:0];}
	NSString * LPortString = @"";
	if (LPort > 0){
		LPortString = [[NSString alloc] initWithFormat:@":%i",LPort];}
	if ((command.RequestType == GET)
		||( command.RequestType == DELETE)) {
		NSRange replaceRange = [LMethodName rangeOfString:@"."];
		if (replaceRange.location !=NSNotFound){
			[LMethodName replaceCharactersInRange:replaceRange withString:@"/"];  
		}
		NSRange replaceRange2 =[LMethodName rangeOfString:LMethodName];
		[LMethodName replaceOccurrencesOfString:@"\\" withString: @"\%22" options:NSCaseInsensitiveSearch range:replaceRange2];
		
	}
	else {
		NSRange replaceRange = [LMethodName rangeOfString:@"."];
		if (replaceRange.location !=NSNotFound){
			[LMethodName replaceCharactersInRange:replaceRange withString:@"/%22"];  
		}
		NSRange replaceRange2 =[LMethodName rangeOfString:LMethodName];
		[LMethodName replaceOccurrencesOfString:@"\\" withString: @"\%22" options:NSCaseInsensitiveSearch range:replaceRange2];
		[LMethodName appendString: @"%22"];
	}
	
	NSString * LContext = [[NSString alloc] initWithString:[self Context]];
	if ([LContext isEqualToString:@""]){
		LContext = @"datasnap/";}
	
	NSString * LUrl = [[NSString alloc] initWithFormat:@"%@://%@%@%@/%@rest/%@/",  LProtocol ,
	[self encodeURIComponent: LHost], LPortString , LPathPrefix , LContext , LMethodName];
	
	return LUrl;
 
 }


-(bool) isUrlParameter: (DSRestParameter *) parameter {
	return ((parameter.DBXType != JsonValueType)
			&& (parameter.DBXType != BinaryBlobType) && (parameter.DBXType != TableType));

}

-(NSMutableURLRequest*) BuildRequest:(requestTypes) arequestType withUrl: (NSString*) aurl
						   withJSon: (NSMutableArray* ) _parameters {
	
	NSMutableURLRequest * request =[NSMutableURLRequest requestWithURL:[NSURL 
																			URLWithString:aurl]];
	[request setTimeoutInterval: self.connectionTimeout];

	switch (arequestType) {
		case GET:
			[request setHTTPMethod: @"GET"];
			
			break;
		case DELETE:
			[request setHTTPMethod: @"DELETE"];
			
			break;
		case POST:
			[request setHTTPMethod: @"POST"];
			if (!_parameters) {
				@throw [DBXException exceptionWithName:@"ParametersNull" 
												reason:@"Parameters cannot be null in a POST request" userInfo:nil]; 
			}
			
			if ([_parameters count] > 0) {
				
				
			    
				NSDictionary* jbody = [NSDictionary dictionaryWithObject: _parameters forKey:@"_parameters"];
				@try {	
					TJSONObject * jo=   [[TJSONObject alloc]initWithJSONObject:jbody];
					NSString * body = [jo asJSONString];
					
					[request setHTTPBody: [body dataUsingEncoding:NSUTF8StringEncoding ]];
				}
				@finally {
				
				}
				
			
				
				
			}
			
			break;
		case PUT:
			[request setHTTPMethod: @"PUT"];
			if ([_parameters count] > 0) {
				NSString * body =@"body";
				[request setHTTPBody: [body dataUsingEncoding:NSUTF8StringEncoding ]];
				
			}
			
			break;

		default:
		
			break;
	}

	return request;
	

}

-(void) setUpAuthorizationHeader:(NSMutableURLRequest *) request{
	if (!UserName || [UserName isEqualToString:@""])
		[request addValue:@"Basic Og==" forHTTPHeaderField:@"Authorization"]; //no auth
	else {
	
		NSString* auth =[NSString stringWithFormat:@"%@:%@",UserName,Password];
		NSString* s= [NSString stringWithFormat:@"%@%@", @"Basic ", [[DBXDefaultFormatter getInstance] Base64Encode:auth]];
		[request addValue:s forHTTPHeaderField:@"Authorization"]; //auth
	}
}
-(void) setUpHeader:(NSMutableURLRequest *) request{
	
	[request addValue: @"Mon, 1 Oct 1990 05:00:00 GMT"forHTTPHeaderField:@"If-Modified-Since"];
	[request addValue:@"Keep-Alive"forHTTPHeaderField:@"Connection"];		
	[request addValue:@"text/plain;charset=UTF-8"forHTTPHeaderField:@"Content-Type"];		
	[request addValue:@"application/JSON"forHTTPHeaderField:@"Accept"];			
	[request addValue:@"identity"forHTTPHeaderField:@"Accept-Encoding"];	
	[request addValue:@"Mozilla/3.0 (compatible; Indy Library)"forHTTPHeaderField:@"User-Agent"];		
	if (!SessionID){
		[self setUpAuthorizationHeader:request];
	}else {
		[self SetUpSessionHeader:request];
	}


  }

-(NSMutableURLRequest *) CreateRequest: (DSRESTCommand *)command{
		
	NSMutableString * url = [[NSMutableString alloc]initWithCapacity:0];
	@try{
		[url appendString: [self buildRequestURL:command ]];
		NSMutableArray * parametersToSend = [NSMutableArray arrayWithCapacity: 
										 [command.parameters count]];

		NSMutableArray * _parameters = [NSMutableArray arrayWithCapacity:0];

		@try{		
			if ([command.parameters count] > 0 ) {
				for (DSRestParameter* p in command.parameters){
					if (((p.direction == Input) || (p.direction == InputOutput))) {
						[parametersToSend addObject:p];
					}
				}
			}
			if( (command.RequestType ==GET) ||
			   (command.RequestType ==DELETE)){
		
				for(DSRestParameter* p in parametersToSend ){
					[url appendString: [NSString stringWithFormat:@"%@/",
						[self encodeURIComponentWithParam:p]]];
				}
			}else//POST or PUT
			{
				bool CanAddParamsToUrl = true;
		
				for(DSRestParameter * p in parametersToSend){
					if (CanAddParamsToUrl && [self isUrlParameter:p]) {
						[url appendString: [NSString stringWithFormat:@"%@/",
									[self encodeURIComponentWithParam:p]]] ;	
						}else//add the json rappresentation in the body
						{
							CanAddParamsToUrl = false;
							[[p getValue] appendTo:_parameters]; 
						}
						 
				}
	
			}
					}
		@finally {

		}
		
		
		
		NSMutableURLRequest* req = [self BuildRequest:[command RequestType] 										  
											  withUrl:url withJSon:_parameters];	
		
		
		[self setUpHeader:req];
		return req;
	}
	@finally {
		
	}
}
/**
 * Clone the current connection. The session is not cloned, so the cloned
 * connection will not have the same session as its parent.
 * 
 * @return the new DSRESTConnection
 */
-(DSRESTConnection*) Clone:(bool) includeSession {
	DSRESTConnection *  connection =  [[DSRESTConnection alloc]init] ;
	connection.Host = self.Host;
	connection.Port = self.Port;
	connection.protocol = self.protocol;
	connection.UserName = self.UserName;
	connection.Password= self.Password;
	connection.UrlPath = self.UrlPath;
	if (includeSession) {
		connection.SessionID = self.SessionID;
		connection.SessionIDExpires = self.SessionIDExpires;
	}

	
	[connection setDelegate: [self getDelegate] ];
	
	return connection;
}
-(DSRESTConnection*) Clone{
	return [self Clone:NO];
}

/**
 * Throw an Exception inspecting the returned json object
 * 
 * @param json
 * @throws JSONException
 * @throws DBXException
 */
-(void) throwJsonExceptionIfNeed: (id) json jsonString:(NSString *) jsstring andThrowEx:(bool) athrowEx{
	if(	[json isKindOfClass:[TJSONObject class]]){
		TJSONObject * d = (TJSONObject *) json;
		if ( [d hasKey:@"error"]){
			NSString * msg = [d getStringForKey:@"error"];
			@throw [DBXException exceptionWithName:@"JsonError" 
											reason:msg  userInfo:nil]; 
		}
		if ( [d hasKey:@"SessionExpired"]){
			[self closeSession];
			NSString * msg = [d getStringForKey:@"SessionExpired"];
			@throw [DBXException exceptionWithName:@"SessionExpiredError" 
											reason:msg userInfo:nil]; 
		}
		
		   
	}else {
		if (athrowEx){
		@throw [DBXException exceptionWithName:@"InvalidJason" 
										reason:[NSString stringWithFormat:@"Invalid Jason Object : %@",jsstring] 
									  userInfo:nil]; 
		}
		
	}
}
-(void) throwResponseExceptionIfNeed: (NSHTTPURLResponse*) response withError: (NSError*) err{
	/*TO-DO*/
	if ([response isKindOfClass:[NSHTTPURLResponse class]]) {
	if ([response statusCode] != 200) {		
			@throw [DBXException exceptionWithName:@"InvalidReponse" 
											reason: [ NSString stringWithFormat: @"%i:%@",[response statusCode],
													 [NSHTTPURLResponse localizedStringForStatusCode:[response statusCode]]] 
										  userInfo:[err userInfo]]; 
			
			
		}

	}
	if(err){
		@throw [DBXException exceptionWithName:@"InvalidReponse" 
										reason:[err description] 
									  userInfo:[err userInfo]]; 
	}
	
	
	
}

-(bool) isOnlyOneParameterInOutput: (NSArray *)parameters{
    int Count = 0;
	for(DSRestParameter * param in parameters){
		if (((param.direction == ReturnValue)
			 || (param.direction == InputOutput) || (param.direction == Output))) {
			Count++;
		} // if
	}
	return Count ==1;
}

-(bool) isThereOnlyOneStreamInOutput:(NSArray *) parameters{
	if ([self isOnlyOneParameterInOutput: parameters]) {
		for (DSRestParameter *param in parameters) {
			if (((param.direction == ReturnValue)
				 || (param.direction == InputOutput) || (param.direction == Output))
				&& (param.DBXType == BinaryBlobType)) {
				return true;
			} // if
		} // for
	}
	return false;


}
-(void) setSessionIdentifier:(NSHTTPURLResponse*) response {
	bool found = false;
	NSDictionary * hd=  [response allHeaderFields ];
	

	// we have to parse the header
	// "dssession=962936.585703.822312,dssessionexpires=1200000"
	NSString * pragma = [hd objectForKey:@"Pragma"];
	if (pragma){
	  NSArray * ds= [pragma componentsSeparatedByString:@","];
		for (  NSString * e in ds){
			NSArray * er = [e componentsSeparatedByString:@"="];
			if([er count]>0){
				NSString * s=  (NSString *)[er objectAtIndex:0];
				if ([s isEqualToString:@"dssession"]) {
					SessionID = [er objectAtIndex:1]  ;
					found = YES;
				}else{
					if ([s isEqualToString:@"dssessionexpires"]) {
						SessionIDExpires =(long) [ er objectAtIndex:1];
					}
				}
			}
		
		}
	
		
	}
 

	if (!found){
		[self closeSession];};

}


/**
 * Execute the request from a specific {@link DSRESTCommand} input, that
 * will contain useful information to construct the URL as the type of
 * request, the method to execute and the parameters to be passed. This
 * information be added to those contained in this object as protocol,
 * target host, context... They form the complete request to execute. This
 * method is need to pass parameters correctly or under the parameter
 * direction, it will be append on the url string or written in the body of
 * the request. Upon receipt of the response will have to check the
 * correctness of the received parameters and set them in the
 * {@link DSRESTCommand}.
 * 
 * @param command
 *            the specific {@link DSRESTCommand}
 * @throws DBXException
 *             where the parameter type is not allowed or in case that the
 *             json response is an error message.
 */
-(void) execute: (DSRESTCommand *) command{
	@try {
		NSMutableURLRequest * request= [self CreateRequest:command];
	
		NSHTTPURLResponse * urlr = nil;
		NSError * urle = nil;
		@try{		
			NSData * response = [DBXConnection sendSynchronousRequest:request returningResponse:urlr error:urle
														usingDelegate: internalDelegate];
			[self setSessionIdentifier:urlr];
			
			NSString * json_string1 =[[NSString alloc] initWithData:response encoding:NSUTF8StringEncoding];
			id json1 = [[TJSONObject alloc] initWithJSONString:json_string1];
			[self throwJsonExceptionIfNeed:json1 jsonString:json_string1 andThrowEx:NO];
			[self throwResponseExceptionIfNeed:urlr withError:urle];
			
		    if ([ self isThereOnlyOneStreamInOutput:command.parameters]) {				
				for (DSRestParameter *param in command.parameters) {
					if ((param.direction == ReturnValue)
						|| (param.direction == InputOutput)
						|| (param.direction == Output)) {
						NSData *v = [NSData dataWithData:response];
						if ( [param.typeName hasPrefix:@"TDBX"]
							&& [param.typeName hasSuffix:@"Value"]) {
								[[[param getValue] GetAsDBXValue] SetAsStream:v];
						} else {
							[[param getValue] SetAsStream:v];}
						break;
					} // if
				} // for
			} else {
			
			NSString * json_string =[[NSString alloc] initWithData:response encoding:NSUTF8StringEncoding];
			@try{
				
				id json = [[TJSONObject alloc] initWithJSONString:json_string];
				[self throwJsonExceptionIfNeed:json jsonString:json_string andThrowEx:YES];
				   
				NSArray * results =	[[json getJSONArrayForKey:@"result" ] asJSONArray];
				int returnParIndex =0;	   
				DBXValue* v;
				for (DSRestParameter * param in [command parameters]){
					if ((param.direction == ReturnValue)
						|| (param.direction == InputOutput)
						|| (param.direction == Output)) {
						v = [param getValue];
						[ DBXJsonTools jsonToDBX:[results objectAtIndex:returnParIndex]
									withDbxValue:&v withDBXTypeName:[param typeName]];
						returnParIndex++;
					}		   
			
				}
			
			
			}
			@finally{
	
			}
				}	
		}
        @catch (NSException * e) {
           NSLog(@"Exception: %@", e);
        }
		@finally {

		}
			
	}
	@finally {

	}
		
	
}
 
 
 

@end

