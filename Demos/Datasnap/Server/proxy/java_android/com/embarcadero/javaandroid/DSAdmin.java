//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

public class DSAdmin {
	private DSRESTConnection connection;

	public DSRESTConnection getConnection() {
		return connection;
	}

	public DSAdmin(DSRESTConnection Connection) {
		super();
		connection = Connection;
	}

	private DSRESTParameterMetaData[] DSAdmin_GetPlatformName_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_GetPlatformName_Metadata() {
		if (DSAdmin_GetPlatformName_Metadata == null) {
			DSAdmin_GetPlatformName_Metadata = new DSRESTParameterMetaData[] { new DSRESTParameterMetaData(
					"", DSRESTParamDirection.ReturnValue,
					DBXDataTypes.WideStringType, "string"), };
		}
		return DSAdmin_GetPlatformName_Metadata;
	}

	/**
	 * @return result - Type on server: string
	 */
	public String GetPlatformName() throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.GetPlatformName");
		cmd.prepare(get_DSAdmin_GetPlatformName_Metadata());
		getConnection().execute(cmd);
		return cmd.getParameter(0).getValue().GetAsString();
	}

	private DSRESTParameterMetaData[] DSAdmin_ClearResources_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_ClearResources_Metadata() {
		if (DSAdmin_ClearResources_Metadata == null) {
			DSAdmin_ClearResources_Metadata = new DSRESTParameterMetaData[] { new DSRESTParameterMetaData(
					"", DSRESTParamDirection.ReturnValue,
					DBXDataTypes.BooleanType, "Boolean"), };
		}
		return DSAdmin_ClearResources_Metadata;
	}

	/**
	 * @return result - Type on server: Boolean
	 */
	public boolean ClearResources() throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.ClearResources");
		cmd.prepare(get_DSAdmin_ClearResources_Metadata());
		getConnection().execute(cmd);
		return cmd.getParameter(0).getValue().GetAsBoolean();
	}

	private DSRESTParameterMetaData[] DSAdmin_FindPackages_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_FindPackages_Metadata() {
		if (DSAdmin_FindPackages_Metadata == null) {
			DSAdmin_FindPackages_Metadata = new DSRESTParameterMetaData[] { new DSRESTParameterMetaData(
					"", DSRESTParamDirection.ReturnValue,
					DBXDataTypes.TableType, "TDBXReader"), };
		}
		return DSAdmin_FindPackages_Metadata;
	}

	/**
	 * @return result - Type on server: TDBXReader
	 */
	public TDBXReader FindPackages() throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.FindPackages");
		cmd.prepare(get_DSAdmin_FindPackages_Metadata());
		getConnection().execute(cmd);
		return (TDBXReader) cmd.getParameter(0).getValue().GetAsTable();
	}

	private DSRESTParameterMetaData[] DSAdmin_FindClasses_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_FindClasses_Metadata() {
		if (DSAdmin_FindClasses_Metadata == null) {
			DSAdmin_FindClasses_Metadata = new DSRESTParameterMetaData[] {
					new DSRESTParameterMetaData("PackageName",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("ClassPattern",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("",
							DSRESTParamDirection.ReturnValue,
							DBXDataTypes.TableType, "TDBXReader"), };
		}
		return DSAdmin_FindClasses_Metadata;
	}

	/**
	 * @param PackageName
	 *            [in] - Type on server: string
	 * @param ClassPattern
	 *            [in] - Type on server: string
	 * @return result - Type on server: TDBXReader
	 */
	public TDBXReader FindClasses(String PackageName, String ClassPattern)
			throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.FindClasses");
		cmd.prepare(get_DSAdmin_FindClasses_Metadata());
		cmd.getParameter(0).getValue().SetAsString(PackageName);
		cmd.getParameter(1).getValue().SetAsString(ClassPattern);
		getConnection().execute(cmd);
		return (TDBXReader) cmd.getParameter(2).getValue().GetAsTable();
	}

	private DSRESTParameterMetaData[] DSAdmin_FindMethods_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_FindMethods_Metadata() {
		if (DSAdmin_FindMethods_Metadata == null) {
			DSAdmin_FindMethods_Metadata = new DSRESTParameterMetaData[] {
					new DSRESTParameterMetaData("PackageName",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("ClassPattern",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("MethodPattern",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("",
							DSRESTParamDirection.ReturnValue,
							DBXDataTypes.TableType, "TDBXReader"), };
		}
		return DSAdmin_FindMethods_Metadata;
	}

	/**
	 * @param PackageName
	 *            [in] - Type on server: string
	 * @param ClassPattern
	 *            [in] - Type on server: string
	 * @param MethodPattern
	 *            [in] - Type on server: string
	 * @return result - Type on server: TDBXReader
	 */
	public TDBXReader FindMethods(String PackageName, String ClassPattern,
			String MethodPattern) throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.FindMethods");
		cmd.prepare(get_DSAdmin_FindMethods_Metadata());
		cmd.getParameter(0).getValue().SetAsString(PackageName);
		cmd.getParameter(1).getValue().SetAsString(ClassPattern);
		cmd.getParameter(2).getValue().SetAsString(MethodPattern);
		getConnection().execute(cmd);
		return (TDBXReader) cmd.getParameter(3).getValue().GetAsTable();
	}

	private DSRESTParameterMetaData[] DSAdmin_CreateServerClasses_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_CreateServerClasses_Metadata() {
		if (DSAdmin_CreateServerClasses_Metadata == null) {
			DSAdmin_CreateServerClasses_Metadata = new DSRESTParameterMetaData[] { new DSRESTParameterMetaData(
					"ClassReader", DSRESTParamDirection.Input,
					DBXDataTypes.TableType, "TDBXReader"), };
		}
		return DSAdmin_CreateServerClasses_Metadata;
	}

	/**
	 * @param ClassReader
	 *            [in] - Type on server: TDBXReader
	 */
	public void CreateServerClasses(TDBXReader ClassReader) throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.POST);
		cmd.setText("DSAdmin.CreateServerClasses");
		cmd.prepare(get_DSAdmin_CreateServerClasses_Metadata());
		cmd.getParameter(0).getValue().SetAsTable(ClassReader);
		getConnection().execute(cmd);
		return;
	}

	private DSRESTParameterMetaData[] DSAdmin_DropServerClasses_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_DropServerClasses_Metadata() {
		if (DSAdmin_DropServerClasses_Metadata == null) {
			DSAdmin_DropServerClasses_Metadata = new DSRESTParameterMetaData[] { new DSRESTParameterMetaData(
					"ClassReader", DSRESTParamDirection.Input,
					DBXDataTypes.TableType, "TDBXReader"), };
		}
		return DSAdmin_DropServerClasses_Metadata;
	}

	/**
	 * @param ClassReader
	 *            [in] - Type on server: TDBXReader
	 */
	public void DropServerClasses(TDBXReader ClassReader) throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.POST);
		cmd.setText("DSAdmin.DropServerClasses");
		cmd.prepare(get_DSAdmin_DropServerClasses_Metadata());
		cmd.getParameter(0).getValue().SetAsTable(ClassReader);
		getConnection().execute(cmd);
		return;
	}

	private DSRESTParameterMetaData[] DSAdmin_CreateServerMethods_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_CreateServerMethods_Metadata() {
		if (DSAdmin_CreateServerMethods_Metadata == null) {
			DSAdmin_CreateServerMethods_Metadata = new DSRESTParameterMetaData[] { new DSRESTParameterMetaData(
					"MethodReader", DSRESTParamDirection.Input,
					DBXDataTypes.TableType, "TDBXReader"), };
		}
		return DSAdmin_CreateServerMethods_Metadata;
	}

	/**
	 * @param MethodReader
	 *            [in] - Type on server: TDBXReader
	 */
	public void CreateServerMethods(TDBXReader MethodReader)
			throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.POST);
		cmd.setText("DSAdmin.CreateServerMethods");
		cmd.prepare(get_DSAdmin_CreateServerMethods_Metadata());
		cmd.getParameter(0).getValue().SetAsTable(MethodReader);
		getConnection().execute(cmd);
		return;
	}

	private DSRESTParameterMetaData[] DSAdmin_DropServerMethods_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_DropServerMethods_Metadata() {
		if (DSAdmin_DropServerMethods_Metadata == null) {
			DSAdmin_DropServerMethods_Metadata = new DSRESTParameterMetaData[] { new DSRESTParameterMetaData(
					"MethodReader", DSRESTParamDirection.Input,
					DBXDataTypes.TableType, "TDBXReader"), };
		}
		return DSAdmin_DropServerMethods_Metadata;
	}

	/**
	 * @param MethodReader
	 *            [in] - Type on server: TDBXReader
	 */
	public void DropServerMethods(TDBXReader MethodReader) throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.POST);
		cmd.setText("DSAdmin.DropServerMethods");
		cmd.prepare(get_DSAdmin_DropServerMethods_Metadata());
		cmd.getParameter(0).getValue().SetAsTable(MethodReader);
		getConnection().execute(cmd);
		return;
	}

	private DSRESTParameterMetaData[] DSAdmin_GetServerClasses_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_GetServerClasses_Metadata() {
		if (DSAdmin_GetServerClasses_Metadata == null) {
			DSAdmin_GetServerClasses_Metadata = new DSRESTParameterMetaData[] { new DSRESTParameterMetaData(
					"", DSRESTParamDirection.ReturnValue,
					DBXDataTypes.TableType, "TDBXReader"), };
		}
		return DSAdmin_GetServerClasses_Metadata;
	}

	/**
	 * @return result - Type on server: TDBXReader
	 */
	public TDBXReader GetServerClasses() throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.GetServerClasses");
		cmd.prepare(get_DSAdmin_GetServerClasses_Metadata());
		getConnection().execute(cmd);
		return (TDBXReader) cmd.getParameter(0).getValue().GetAsTable();
	}

	private DSRESTParameterMetaData[] DSAdmin_ListClasses_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_ListClasses_Metadata() {
		if (DSAdmin_ListClasses_Metadata == null) {
			DSAdmin_ListClasses_Metadata = new DSRESTParameterMetaData[] { new DSRESTParameterMetaData(
					"", DSRESTParamDirection.ReturnValue,
					DBXDataTypes.JsonValueType, "TJSONArray"), };
		}
		return DSAdmin_ListClasses_Metadata;
	}

	/**
	 * @return result - Type on server: TJSONArray
	 */
	public TJSONArray ListClasses() throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.ListClasses");
		cmd.prepare(get_DSAdmin_ListClasses_Metadata());
		getConnection().execute(cmd);
		return (TJSONArray) cmd.getParameter(0).getValue().GetAsJSONValue();
	}

	private DSRESTParameterMetaData[] DSAdmin_DescribeClass_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_DescribeClass_Metadata() {
		if (DSAdmin_DescribeClass_Metadata == null) {
			DSAdmin_DescribeClass_Metadata = new DSRESTParameterMetaData[] {
					new DSRESTParameterMetaData("ClassName",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("",
							DSRESTParamDirection.ReturnValue,
							DBXDataTypes.JsonValueType, "TJSONObject"), };
		}
		return DSAdmin_DescribeClass_Metadata;
	}

	/**
	 * @param ClassName
	 *            [in] - Type on server: string
	 * @return result - Type on server: TJSONObject
	 */
	public TJSONObject DescribeClass(String ClassName) throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.DescribeClass");
		cmd.prepare(get_DSAdmin_DescribeClass_Metadata());
		cmd.getParameter(0).getValue().SetAsString(ClassName);
		getConnection().execute(cmd);
		return (TJSONObject) cmd.getParameter(1).getValue().GetAsJSONValue();
	}

	private DSRESTParameterMetaData[] DSAdmin_ListMethods_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_ListMethods_Metadata() {
		if (DSAdmin_ListMethods_Metadata == null) {
			DSAdmin_ListMethods_Metadata = new DSRESTParameterMetaData[] {
					new DSRESTParameterMetaData("ClassName",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("",
							DSRESTParamDirection.ReturnValue,
							DBXDataTypes.JsonValueType, "TJSONArray"), };
		}
		return DSAdmin_ListMethods_Metadata;
	}

	/**
	 * @param ClassName
	 *            [in] - Type on server: string
	 * @return result - Type on server: TJSONArray
	 */
	public TJSONArray ListMethods(String ClassName) throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.ListMethods");
		cmd.prepare(get_DSAdmin_ListMethods_Metadata());
		cmd.getParameter(0).getValue().SetAsString(ClassName);
		getConnection().execute(cmd);
		return (TJSONArray) cmd.getParameter(1).getValue().GetAsJSONValue();
	}

	private DSRESTParameterMetaData[] DSAdmin_DescribeMethod_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_DescribeMethod_Metadata() {
		if (DSAdmin_DescribeMethod_Metadata == null) {
			DSAdmin_DescribeMethod_Metadata = new DSRESTParameterMetaData[] {
					new DSRESTParameterMetaData("ServerMethodName",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("",
							DSRESTParamDirection.ReturnValue,
							DBXDataTypes.JsonValueType, "TJSONObject"), };
		}
		return DSAdmin_DescribeMethod_Metadata;
	}

	/**
	 * @param ServerMethodName
	 *            [in] - Type on server: string
	 * @return result - Type on server: TJSONObject
	 */
	public TJSONObject DescribeMethod(String ServerMethodName)
			throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.DescribeMethod");
		cmd.prepare(get_DSAdmin_DescribeMethod_Metadata());
		cmd.getParameter(0).getValue().SetAsString(ServerMethodName);
		getConnection().execute(cmd);
		return (TJSONObject) cmd.getParameter(1).getValue().GetAsJSONValue();
	}

	private DSRESTParameterMetaData[] DSAdmin_GetServerMethods_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_GetServerMethods_Metadata() {
		if (DSAdmin_GetServerMethods_Metadata == null) {
			DSAdmin_GetServerMethods_Metadata = new DSRESTParameterMetaData[] { new DSRESTParameterMetaData(
					"", DSRESTParamDirection.ReturnValue,
					DBXDataTypes.TableType, "TDBXReader"), };
		}
		return DSAdmin_GetServerMethods_Metadata;
	}

	/**
	 * @return result - Type on server: TDBXReader
	 */
	public TDBXReader GetServerMethods() throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.GetServerMethods");
		cmd.prepare(get_DSAdmin_GetServerMethods_Metadata());
		getConnection().execute(cmd);
		return (TDBXReader) cmd.getParameter(0).getValue().GetAsTable();
	}

	private DSRESTParameterMetaData[] DSAdmin_GetServerMethodParameters_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_GetServerMethodParameters_Metadata() {
		if (DSAdmin_GetServerMethodParameters_Metadata == null) {
			DSAdmin_GetServerMethodParameters_Metadata = new DSRESTParameterMetaData[] { new DSRESTParameterMetaData(
					"", DSRESTParamDirection.ReturnValue,
					DBXDataTypes.TableType, "TDBXReader"), };
		}
		return DSAdmin_GetServerMethodParameters_Metadata;
	}

	/**
	 * @return result - Type on server: TDBXReader
	 */
	public TDBXReader GetServerMethodParameters() throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.GetServerMethodParameters");
		cmd.prepare(get_DSAdmin_GetServerMethodParameters_Metadata());
		getConnection().execute(cmd);
		return (TDBXReader) cmd.getParameter(0).getValue().GetAsTable();
	}

	private DSRESTParameterMetaData[] DSAdmin_GetDatabaseConnectionProperties_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_GetDatabaseConnectionProperties_Metadata() {
		if (DSAdmin_GetDatabaseConnectionProperties_Metadata == null) {
			DSAdmin_GetDatabaseConnectionProperties_Metadata = new DSRESTParameterMetaData[] { new DSRESTParameterMetaData(
					"", DSRESTParamDirection.ReturnValue,
					DBXDataTypes.TableType, "TDBXReader"), };
		}
		return DSAdmin_GetDatabaseConnectionProperties_Metadata;
	}

	/**
	 * @return result - Type on server: TDBXReader
	 */
	public TDBXReader GetDatabaseConnectionProperties() throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.GetDatabaseConnectionProperties");
		cmd.prepare(get_DSAdmin_GetDatabaseConnectionProperties_Metadata());
		getConnection().execute(cmd);
		return (TDBXReader) cmd.getParameter(0).getValue().GetAsTable();
	}

	private DSRESTParameterMetaData[] DSAdmin_GetDSServerName_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_GetDSServerName_Metadata() {
		if (DSAdmin_GetDSServerName_Metadata == null) {
			DSAdmin_GetDSServerName_Metadata = new DSRESTParameterMetaData[] { new DSRESTParameterMetaData(
					"", DSRESTParamDirection.ReturnValue,
					DBXDataTypes.WideStringType, "string"), };
		}
		return DSAdmin_GetDSServerName_Metadata;
	}

	/**
	 * @return result - Type on server: string
	 */
	public String GetDSServerName() throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.GetDSServerName");
		cmd.prepare(get_DSAdmin_GetDSServerName_Metadata());
		getConnection().execute(cmd);
		return cmd.getParameter(0).getValue().GetAsString();
	}

	private DSRESTParameterMetaData[] DSAdmin_ConsumeClientChannel_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_ConsumeClientChannel_Metadata() {
		if (DSAdmin_ConsumeClientChannel_Metadata == null) {
			DSAdmin_ConsumeClientChannel_Metadata = new DSRESTParameterMetaData[] {
					new DSRESTParameterMetaData("ChannelName",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("ClientManagerId",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("CallbackId",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("ChannelNames",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("SecurityToken",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("ResponseData",
							DSRESTParamDirection.Input,
							DBXDataTypes.JsonValueType, "TJSONValue"),
					new DSRESTParameterMetaData("",
							DSRESTParamDirection.ReturnValue,
							DBXDataTypes.JsonValueType, "TJSONValue"), };
		}
		return DSAdmin_ConsumeClientChannel_Metadata;
	}

	/**
	 * @param ChannelName
	 *            [in] - Type on server: string
	 * @param ClientManagerId
	 *            [in] - Type on server: string
	 * @param CallbackId
	 *            [in] - Type on server: string
	 * @param ChannelNames
	 *            [in] - Type on server: string
	 * @param SecurityToken
	 *            [in] - Type on server: string
	 * @param ResponseData
	 *            [in] - Type on server: TJSONValue
	 * @return result - Type on server: TJSONValue
	 */
	public TJSONValue ConsumeClientChannel(String ChannelName,
			String ClientManagerId, String CallbackId, String ChannelNames,
			String SecurityToken, TJSONValue ResponseData) throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.POST);
		cmd.setText("DSAdmin.ConsumeClientChannel");
		cmd.prepare(get_DSAdmin_ConsumeClientChannel_Metadata());
		cmd.getParameter(0).getValue().SetAsString(ChannelName);
		cmd.getParameter(1).getValue().SetAsString(ClientManagerId);
		cmd.getParameter(2).getValue().SetAsString(CallbackId);
		cmd.getParameter(3).getValue().SetAsString(ChannelNames);
		cmd.getParameter(4).getValue().SetAsString(SecurityToken);
		cmd.getParameter(5).getValue().SetAsJSONValue(ResponseData);
		getConnection().execute(cmd);
		return (TJSONValue) cmd.getParameter(6).getValue().GetAsJSONValue();
	}

	private DSRESTParameterMetaData[] DSAdmin_ConsumeClientChannelTimeout_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_ConsumeClientChannelTimeout_Metadata() {
		if (DSAdmin_ConsumeClientChannelTimeout_Metadata == null) {
			DSAdmin_ConsumeClientChannelTimeout_Metadata = new DSRESTParameterMetaData[] {
					new DSRESTParameterMetaData("ChannelName",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("ClientManagerId",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("CallbackId",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("ChannelNames",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("SecurityToken",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("Timeout",
							DSRESTParamDirection.Input, DBXDataTypes.Int32Type,
							"Integer"),
					new DSRESTParameterMetaData("ResponseData",
							DSRESTParamDirection.Input,
							DBXDataTypes.JsonValueType, "TJSONValue"),
					new DSRESTParameterMetaData("",
							DSRESTParamDirection.ReturnValue,
							DBXDataTypes.JsonValueType, "TJSONValue"), };
		}
		return DSAdmin_ConsumeClientChannelTimeout_Metadata;
	}

	/**
	 * @param ChannelName
	 *            [in] - Type on server: string
	 * @param ClientManagerId
	 *            [in] - Type on server: string
	 * @param CallbackId
	 *            [in] - Type on server: string
	 * @param ChannelNames
	 *            [in] - Type on server: string
	 * @param SecurityToken
	 *            [in] - Type on server: string
	 * @param Timeout
	 *            [in] - Type on server: Integer
	 * @param ResponseData
	 *            [in] - Type on server: TJSONValue
	 * @return result - Type on server: TJSONValue
	 */
	public TJSONValue ConsumeClientChannelTimeout(String ChannelName,
			String ClientManagerId, String CallbackId, String ChannelNames,
			String SecurityToken, int Timeout, TJSONValue ResponseData)
			throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.POST);
		cmd.setText("DSAdmin.ConsumeClientChannelTimeout");
		cmd.prepare(get_DSAdmin_ConsumeClientChannelTimeout_Metadata());
		cmd.getParameter(0).getValue().SetAsString(ChannelName);
		cmd.getParameter(1).getValue().SetAsString(ClientManagerId);
		cmd.getParameter(2).getValue().SetAsString(CallbackId);
		cmd.getParameter(3).getValue().SetAsString(ChannelNames);
		cmd.getParameter(4).getValue().SetAsString(SecurityToken);
		cmd.getParameter(5).getValue().SetAsInt32(Timeout);
		cmd.getParameter(6).getValue().SetAsJSONValue(ResponseData);
		getConnection().execute(cmd);
		return (TJSONValue) cmd.getParameter(7).getValue().GetAsJSONValue();
	}

	private DSRESTParameterMetaData[] DSAdmin_CloseClientChannel_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_CloseClientChannel_Metadata() {
		if (DSAdmin_CloseClientChannel_Metadata == null) {
			DSAdmin_CloseClientChannel_Metadata = new DSRESTParameterMetaData[] {
					new DSRESTParameterMetaData("ChannelId",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("SecurityToken",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("",
							DSRESTParamDirection.ReturnValue,
							DBXDataTypes.BooleanType, "Boolean"), };
		}
		return DSAdmin_CloseClientChannel_Metadata;
	}

	/**
	 * @param ChannelId
	 *            [in] - Type on server: string
	 * @param SecurityToken
	 *            [in] - Type on server: string
	 * @return result - Type on server: Boolean
	 */
	public boolean CloseClientChannel(String ChannelId, String SecurityToken)
			throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.CloseClientChannel");
		cmd.prepare(get_DSAdmin_CloseClientChannel_Metadata());
		cmd.getParameter(0).getValue().SetAsString(ChannelId);
		cmd.getParameter(1).getValue().SetAsString(SecurityToken);
		getConnection().execute(cmd);
		return cmd.getParameter(2).getValue().GetAsBoolean();
	}

	private DSRESTParameterMetaData[] DSAdmin_RegisterClientCallbackServer_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_RegisterClientCallbackServer_Metadata() {
		if (DSAdmin_RegisterClientCallbackServer_Metadata == null) {
			DSAdmin_RegisterClientCallbackServer_Metadata = new DSRESTParameterMetaData[] {
					new DSRESTParameterMetaData("ChannelId",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("CallbackId",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("ChannelNames",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("SecurityToken",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("",
							DSRESTParamDirection.ReturnValue,
							DBXDataTypes.BooleanType, "Boolean"), };
		}
		return DSAdmin_RegisterClientCallbackServer_Metadata;
	}

	/**
	 * @param ChannelId
	 *            [in] - Type on server: string
	 * @param CallbackId
	 *            [in] - Type on server: string
	 * @param ChannelNames
	 *            [in] - Type on server: string
	 * @param SecurityToken
	 *            [in] - Type on server: string
	 * @return result - Type on server: Boolean
	 */
	public boolean RegisterClientCallbackServer(String ChannelId,
			String CallbackId, String ChannelNames, String SecurityToken)
			throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.RegisterClientCallbackServer");
		cmd.prepare(get_DSAdmin_RegisterClientCallbackServer_Metadata());
		cmd.getParameter(0).getValue().SetAsString(ChannelId);
		cmd.getParameter(1).getValue().SetAsString(CallbackId);
		cmd.getParameter(2).getValue().SetAsString(ChannelNames);
		cmd.getParameter(3).getValue().SetAsString(SecurityToken);
		getConnection().execute(cmd);
		return cmd.getParameter(4).getValue().GetAsBoolean();
	}

	private DSRESTParameterMetaData[] DSAdmin_UnregisterClientCallback_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_UnregisterClientCallback_Metadata() {
		if (DSAdmin_UnregisterClientCallback_Metadata == null) {
			DSAdmin_UnregisterClientCallback_Metadata = new DSRESTParameterMetaData[] {
					new DSRESTParameterMetaData("ChannelId",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("CallbackId",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("SecurityToken",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("",
							DSRESTParamDirection.ReturnValue,
							DBXDataTypes.BooleanType, "Boolean"), };
		}
		return DSAdmin_UnregisterClientCallback_Metadata;
	}

	/**
	 * @param ChannelId
	 *            [in] - Type on server: string
	 * @param CallbackId
	 *            [in] - Type on server: string
	 * @param SecurityToken
	 *            [in] - Type on server: string
	 * @return result - Type on server: Boolean
	 */
	public boolean UnregisterClientCallback(String ChannelId,
			String CallbackId, String SecurityToken) throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.GET);
		cmd.setText("DSAdmin.UnregisterClientCallback");
		cmd.prepare(get_DSAdmin_UnregisterClientCallback_Metadata());
		cmd.getParameter(0).getValue().SetAsString(ChannelId);
		cmd.getParameter(1).getValue().SetAsString(CallbackId);
		cmd.getParameter(2).getValue().SetAsString(SecurityToken);
		getConnection().execute(cmd);
		return cmd.getParameter(3).getValue().GetAsBoolean();
	}

	private DSRESTParameterMetaData[] DSAdmin_BroadcastToChannel_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_BroadcastToChannel_Metadata() {
		if (DSAdmin_BroadcastToChannel_Metadata == null) {
			DSAdmin_BroadcastToChannel_Metadata = new DSRESTParameterMetaData[] {
					new DSRESTParameterMetaData("ChannelName",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("Msg",
							DSRESTParamDirection.Input,
							DBXDataTypes.JsonValueType, "TJSONValue"),
					new DSRESTParameterMetaData("",
							DSRESTParamDirection.ReturnValue,
							DBXDataTypes.BooleanType, "Boolean"), };
		}
		return DSAdmin_BroadcastToChannel_Metadata;
	}

	/**
	 * @param ChannelName
	 *            [in] - Type on server: string
	 * @param Msg
	 *            [in] - Type on server: TJSONValue
	 * @return result - Type on server: Boolean
	 */
	public boolean BroadcastToChannel(String ChannelName, TJSONValue Msg)
			throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.POST);
		cmd.setText("DSAdmin.BroadcastToChannel");
		cmd.prepare(get_DSAdmin_BroadcastToChannel_Metadata());
		cmd.getParameter(0).getValue().SetAsString(ChannelName);
		cmd.getParameter(1).getValue().SetAsJSONValue(Msg);
		getConnection().execute(cmd);
		return cmd.getParameter(2).getValue().GetAsBoolean();
	}

	private DSRESTParameterMetaData[] DSAdmin_BroadcastObjectToChannel_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_BroadcastObjectToChannel_Metadata() {
		if (DSAdmin_BroadcastObjectToChannel_Metadata == null) {
			DSAdmin_BroadcastObjectToChannel_Metadata = new DSRESTParameterMetaData[] {
					new DSRESTParameterMetaData("ChannelName",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("Msg",
							DSRESTParamDirection.Input,
							DBXDataTypes.JsonValueType, "TObject"),
					new DSRESTParameterMetaData("",
							DSRESTParamDirection.ReturnValue,
							DBXDataTypes.BooleanType, "Boolean"), };
		}
		return DSAdmin_BroadcastObjectToChannel_Metadata;
	}

	/**
	 * @param ChannelName
	 *            [in] - Type on server: string
	 * @param Msg
	 *            [in] - Type on server: TObject
	 * @return result - Type on server: Boolean
	 */
	public boolean BroadcastObjectToChannel(String ChannelName, TJSONObject Msg)
			throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.POST);
		cmd.setText("DSAdmin.BroadcastObjectToChannel");
		cmd.prepare(get_DSAdmin_BroadcastObjectToChannel_Metadata());
		cmd.getParameter(0).getValue().SetAsString(ChannelName);
		cmd.getParameter(1).getValue().SetAsJSONValue(Msg);
		getConnection().execute(cmd);
		return cmd.getParameter(2).getValue().GetAsBoolean();
	}

	private DSRESTParameterMetaData[] DSAdmin_NotifyCallback_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_NotifyCallback_Metadata() {
		if (DSAdmin_NotifyCallback_Metadata == null) {
			DSAdmin_NotifyCallback_Metadata = new DSRESTParameterMetaData[] {
					new DSRESTParameterMetaData("ClientId",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("CallbackId",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("Msg",
							DSRESTParamDirection.Input,
							DBXDataTypes.JsonValueType, "TJSONValue"),
					new DSRESTParameterMetaData("Response",
							DSRESTParamDirection.Output,
							DBXDataTypes.JsonValueType, "TJSONValue"),
					new DSRESTParameterMetaData("",
							DSRESTParamDirection.ReturnValue,
							DBXDataTypes.BooleanType, "Boolean"), };
		}
		return DSAdmin_NotifyCallback_Metadata;
	}

	/**
	 * @param ClientId
	 *            [in] - Type on server: string
	 * @param CallbackId
	 *            [in] - Type on server: string
	 * @param Msg
	 *            [in] - Type on server: TJSONValue
	 * @param Response
	 *            [out] - Type on server: TJSONValue
	 * @return result - Type on server: Boolean
	 */
	public static class NotifyCallbackReturns {
		public TJSONValue Response;
		public boolean returnValue;
	}

	public NotifyCallbackReturns NotifyCallback(String ClientId,
			String CallbackId, TJSONValue Msg) throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.POST);
		cmd.setText("DSAdmin.NotifyCallback");
		cmd.prepare(get_DSAdmin_NotifyCallback_Metadata());
		cmd.getParameter(0).getValue().SetAsString(ClientId);
		cmd.getParameter(1).getValue().SetAsString(CallbackId);
		cmd.getParameter(2).getValue().SetAsJSONValue(Msg);
		getConnection().execute(cmd);
		NotifyCallbackReturns ret = new NotifyCallbackReturns();
		ret.Response = (TJSONValue) cmd.getParameter(3).getValue()
				.GetAsJSONValue();
		ret.returnValue = cmd.getParameter(4).getValue().GetAsBoolean();
		return ret;
	}

	private DSRESTParameterMetaData[] DSAdmin_NotifyObject_Metadata;

	private DSRESTParameterMetaData[] get_DSAdmin_NotifyObject_Metadata() {
		if (DSAdmin_NotifyObject_Metadata == null) {
			DSAdmin_NotifyObject_Metadata = new DSRESTParameterMetaData[] {
					new DSRESTParameterMetaData("ClientId",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("CallbackId",
							DSRESTParamDirection.Input,
							DBXDataTypes.WideStringType, "string"),
					new DSRESTParameterMetaData("Msg",
							DSRESTParamDirection.Input,
							DBXDataTypes.JsonValueType, "TObject"),
					new DSRESTParameterMetaData("Response",
							DSRESTParamDirection.Output,
							DBXDataTypes.JsonValueType, "TObject"),
					new DSRESTParameterMetaData("",
							DSRESTParamDirection.ReturnValue,
							DBXDataTypes.BooleanType, "Boolean"), };
		}
		return DSAdmin_NotifyObject_Metadata;
	}

	/**
	 * @param ClientId
	 *            [in] - Type on server: string
	 * @param CallbackId
	 *            [in] - Type on server: string
	 * @param Msg
	 *            [in] - Type on server: TObject
	 * @param Response
	 *            [out] - Type on server: TObject
	 * @return result - Type on server: Boolean
	 */
	public static class NotifyObjectReturns {
		public TJSONObject Response;
		public boolean returnValue;
	}

	public NotifyObjectReturns NotifyObject(String ClientId, String CallbackId,
			TJSONObject Msg) throws DBXException {
		DSRESTCommand cmd = getConnection().CreateCommand();
		cmd.setRequestType(DSHTTPRequestType.POST);
		cmd.setText("DSAdmin.NotifyObject");
		cmd.prepare(get_DSAdmin_NotifyObject_Metadata());
		cmd.getParameter(0).getValue().SetAsString(ClientId);
		cmd.getParameter(1).getValue().SetAsString(CallbackId);
		cmd.getParameter(2).getValue().SetAsJSONValue(Msg);
		getConnection().execute(cmd);
		NotifyObjectReturns ret = new NotifyObjectReturns();
		ret.Response = (TJSONObject) cmd.getParameter(3).getValue()
				.GetAsJSONValue();
		ret.returnValue = cmd.getParameter(4).getValue().GetAsBoolean();
		return ret;
	}
}
