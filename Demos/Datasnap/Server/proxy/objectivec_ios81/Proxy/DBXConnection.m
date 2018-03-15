//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "DBXConnection.h"




@implementation DBXConnection
@synthesize isSynchronous;


-(void) start{ 
    [dbxconn start];
	
	while (!complete && !cancelled ) {
		[[NSRunLoop currentRunLoop] runMode:NSDefaultRunLoopMode beforeDate:[NSDate distantFuture]];
		
	}
	
}


- (id)initWithRequest:(NSURLRequest *)request returningResponse:(NSURLResponse *)response error:(NSError *)error usingDelegate:(id) adelegete
{
	if (self = [super init])
	{
		internalDelegate = adelegete  ;
        complete = NO;
		cancelled = NO;
		urlerror = error;
		urlresponse = response;
		dbxconn = [NSURLConnection connectionWithRequest:request delegate:self];
		
		
	}
	
	return self;
}

- (void)dealloc
{
	[dbxconn cancel];
}

- (NSMutableData*) getReceivedData{
	return receivedData;
}

- (NSURLRequest *)connection:(NSURLConnection *)connection willSendRequest:(NSURLRequest *)request redirectResponse:(NSURLResponse *)response{

	if([internalDelegate respondsToSelector:@selector(connection:willSendRequest:redirectResponse:)]) {
		return [internalDelegate connection:connection willSendRequest:request redirectResponse:response];
	}
	return request;
	
}
- (NSInputStream *)connection:(NSURLConnection *)connection needNewBodyStream:(NSURLRequest *)request{
	if([internalDelegate respondsToSelector:@selector(connection:needNewBodyStream:)]) {
		return [internalDelegate connection:connection needNewBodyStream:request];
	}
	
	return nil;
}
// to deal with self-signed certificates
- (BOOL)connection:(NSURLConnection *)connection
canAuthenticateAgainstProtectionSpace:(NSURLProtectionSpace *)protectionSpace
{	
	if([internalDelegate respondsToSelector:@selector(connection:canAuthenticateAgainstProtectionSpace:)]) {
		return	[internalDelegate connection:connection canAuthenticateAgainstProtectionSpace:protectionSpace];}
	return NO;
	
}

- (void)connection:(NSURLConnection *)connection didReceiveAuthenticationChallenge:(NSURLAuthenticationChallenge *)challenge
{
	if([internalDelegate respondsToSelector:@selector(connection:didReceiveAuthenticationChallenge:)]) {
		[internalDelegate connection:connection  didReceiveAuthenticationChallenge:challenge];}
	
}

- (void)connection:(NSURLConnection *)connection didCancelAuthenticationChallenge:(NSURLAuthenticationChallenge *)challenge{
	if([internalDelegate respondsToSelector:@selector(connection:didCancelAuthenticationChallenge:)]) {
		[internalDelegate connection:connection  didCancelAuthenticationChallenge:challenge];}


}

- (BOOL)connectionShouldUseCredentialStorage:(NSURLConnection *)connection{
	if([internalDelegate respondsToSelector:@selector(connectionShouldUseCredentialStorage:)]) {
		return [internalDelegate connectionShouldUseCredentialStorage:connection];}
	return NO;
}

- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response
{
	if (isSynchronous) {
		
		// every response could mean a redirect
        receivedData = nil;
		urlresponse = response;
	}
	if([internalDelegate respondsToSelector:@selector(connection:didReceiveResponse:)]) {
		[internalDelegate connection:connection didReceiveResponse:response];
		
	}
}


- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data
{
	if (isSynchronous) {
		if (!receivedData)
		{
			// no store yet, make one
			receivedData = [[NSMutableData alloc] initWithData:data];
		}
		else
		{
			// append to previous chunks
			[receivedData appendData:data];
		}
	}
	if([internalDelegate respondsToSelector:@selector(connection:didReceiveData:)]) {
		[internalDelegate connection:connection didReceiveData:data];}
	
	
}

- (void)connection:(NSURLConnection *)connection didSendBodyData:(NSInteger)bytesWritten 
 totalBytesWritten:(NSInteger)totalBytesWritten totalBytesExpectedToWrite:(NSInteger)totalBytesExpectedToWrite{
	
	if([internalDelegate respondsToSelector:@selector(connection:didSendBodyData:totalBytesWritten:totalBytesExpectedToWrite:)]) {
		[internalDelegate connection:connection  didSendBodyData: bytesWritten totalBytesWritten:totalBytesWritten 
		   totalBytesExpectedToWrite: totalBytesExpectedToWrite];}



}



- (void)connectionDidFinishLoading:(NSURLConnection *)connection
{ 
	if (isSynchronous) {
		complete = YES;// this is for syncronous calls
	}
	if([internalDelegate respondsToSelector:@selector(connectionDidFinishLoading:)]) {
		[internalDelegate connectionDidFinishLoading:connection];}
}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error
{
		if (isSynchronous) {
			urlerror = error;
			cancelled = YES;
		}
		if([internalDelegate respondsToSelector:@selector(connection:didFailWithError:)]) {
			[internalDelegate connection:connection  didFailWithError:error];}
}

- (NSCachedURLResponse *)connection:(NSURLConnection *)connection willCacheResponse:(NSCachedURLResponse *)cachedResponse{
	if([internalDelegate respondsToSelector:@selector(connection:willCacheResponse:)]) {
		return [internalDelegate connection:connection  willCacheResponse:cachedResponse];}
	return nil;

}
@end

@implementation  DBXConnection (DBXConnectionSynchronousLoading)
+ (NSData *)sendSynchronousRequest:(NSURLRequest *)request returningResponse:(NSURLResponse *)response error:(NSError *)error
					 usingDelegate: (id) adelegate{

	DBXConnection * con =   [[DBXConnection alloc] initWithRequest:request returningResponse:response error:error usingDelegate:adelegate];
	@try{
        con.isSynchronous = YES;
        [con start];
	
        return [NSData dataWithData:[con getReceivedData]] ;
	}@finally{
	}

}
@end





