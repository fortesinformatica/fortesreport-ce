//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

using System;

namespace Embarcadero.Datasnap.WindowsPhone7
{
    public class DSAdmin
    {
        public delegate void ExceptionCallback(Exception ex);
        public delegate void InternalConnectionDelegate();
        public ExceptionCallback BaseExCal;
        private DSRESTConnection connection;
        public DSRESTConnection getConnection()
        {
            return connection;
        }
        public DSAdmin(DSRESTConnection Connection, ExceptionCallback ExCal)
        {
            BaseExCal = ExCal;
            connection = Connection;
        }
       
        private DSRESTParameterMetaData[] DSAdmin_GetPlatformName_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_GetPlatformName_Metadata() {
        if (DSAdmin_GetPlatformName_Metadata == null) {
          DSAdmin_GetPlatformName_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.WideStringType, "string"),
          };
        }
        return DSAdmin_GetPlatformName_Metadata;
      }

        /**
       * @return result - Type on server: string
       */
      public delegate void GetPlatformNameCallback(String Result);
      private delegate void GetPlatformNameDelegate();

      public void GetPlatformName(GetPlatformNameCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.GetPlatformName");
        cmd.prepare(get_DSAdmin_GetPlatformName_Metadata());
        GetPlatformNameDelegate GetPlatformNameDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke(cmd.getParameter(0).getValue().GetAsString());
          }
        };
        getConnection().execute(cmd, this, GetPlatformNameDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_ClearResources_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_ClearResources_Metadata() {
        if (DSAdmin_ClearResources_Metadata == null) {
          DSAdmin_ClearResources_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.BooleanType, "Boolean"),
          };
        }
        return DSAdmin_ClearResources_Metadata;
      }

      /**
       * @return result - Type on server: Boolean
       */
      public delegate void ClearResourcesCallback(bool Result);
      private delegate void ClearResourcesDelegate();

      public void ClearResources(ClearResourcesCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.ClearResources");
        cmd.prepare(get_DSAdmin_ClearResources_Metadata());
        ClearResourcesDelegate ClearResourcesDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke(cmd.getParameter(0).getValue().GetAsBoolean());
          }
        };
        getConnection().execute(cmd, this, ClearResourcesDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_FindPackages_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_FindPackages_Metadata() {
        if (DSAdmin_FindPackages_Metadata == null) {
          DSAdmin_FindPackages_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.TableType, "TDBXReader"),
          };
        }
        return DSAdmin_FindPackages_Metadata;
      }

      /**
       * @return result - Type on server: TDBXReader
       */
      public delegate void FindPackagesCallback(TDBXReader Result);
      private delegate void FindPackagesDelegate();

      public void FindPackages(FindPackagesCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.FindPackages");
        cmd.prepare(get_DSAdmin_FindPackages_Metadata());
        FindPackagesDelegate FindPackagesDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke((TDBXReader)cmd.getParameter(0).getValue().GetAsTable());
          }
        };
        getConnection().execute(cmd, this, FindPackagesDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_FindClasses_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_FindClasses_Metadata() {
        if (DSAdmin_FindClasses_Metadata == null) {
          DSAdmin_FindClasses_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("PackageName", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("ClassPattern", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.TableType, "TDBXReader"),
          };
        }
        return DSAdmin_FindClasses_Metadata;
      }

      /**
       * @param PackageName [in] - Type on server: string
       * @param ClassPattern [in] - Type on server: string
       * @return result - Type on server: TDBXReader
       */
      public delegate void FindClassesCallback(TDBXReader Result);
      private delegate void FindClassesDelegate();

      public void FindClasses(String PackageName, String ClassPattern, FindClassesCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.FindClasses");
        cmd.prepare(get_DSAdmin_FindClasses_Metadata());
        FindClassesDelegate FindClassesDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke((TDBXReader)cmd.getParameter(2).getValue().GetAsTable());
          }
        };
        cmd.getParameter(0).getValue().SetAsString(PackageName);
        cmd.getParameter(1).getValue().SetAsString(ClassPattern);
        getConnection().execute(cmd, this, FindClassesDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_FindMethods_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_FindMethods_Metadata() {
        if (DSAdmin_FindMethods_Metadata == null) {
          DSAdmin_FindMethods_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("PackageName", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("ClassPattern", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("MethodPattern", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.TableType, "TDBXReader"),
          };
        }
        return DSAdmin_FindMethods_Metadata;
      }

      /**
       * @param PackageName [in] - Type on server: string
       * @param ClassPattern [in] - Type on server: string
       * @param MethodPattern [in] - Type on server: string
       * @return result - Type on server: TDBXReader
       */
      public delegate void FindMethodsCallback(TDBXReader Result);
      private delegate void FindMethodsDelegate();

      public void FindMethods(String PackageName, String ClassPattern, String MethodPattern, FindMethodsCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.FindMethods");
        cmd.prepare(get_DSAdmin_FindMethods_Metadata());
        FindMethodsDelegate FindMethodsDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke((TDBXReader)cmd.getParameter(3).getValue().GetAsTable());
          }
        };
        cmd.getParameter(0).getValue().SetAsString(PackageName);
        cmd.getParameter(1).getValue().SetAsString(ClassPattern);
        cmd.getParameter(2).getValue().SetAsString(MethodPattern);
        getConnection().execute(cmd, this, FindMethodsDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_CreateServerClasses_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_CreateServerClasses_Metadata() {
        if (DSAdmin_CreateServerClasses_Metadata == null) {
          DSAdmin_CreateServerClasses_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("ClassReader", DSRESTParamDirection.Input, DBXDataTypes.TableType, "TDBXReader"),
          };
        }
        return DSAdmin_CreateServerClasses_Metadata;
      }

      /**
       * @param ClassReader [in] - Type on server: TDBXReader
       */
      public delegate void CreateServerClassesCallback();
      private delegate void CreateServerClassesDelegate();

      public void CreateServerClasses(TDBXReader ClassReader, CreateServerClassesCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.POST);
        cmd.setText("DSAdmin.CreateServerClasses");
        cmd.prepare(get_DSAdmin_CreateServerClasses_Metadata());
        CreateServerClassesDelegate CreateServerClassesDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke();
          }
        };
        cmd.getParameter(0).getValue().SetAsTable(ClassReader);
        getConnection().execute(cmd, this, CreateServerClassesDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_DropServerClasses_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_DropServerClasses_Metadata() {
        if (DSAdmin_DropServerClasses_Metadata == null) {
          DSAdmin_DropServerClasses_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("ClassReader", DSRESTParamDirection.Input, DBXDataTypes.TableType, "TDBXReader"),
          };
        }
        return DSAdmin_DropServerClasses_Metadata;
      }

      /**
       * @param ClassReader [in] - Type on server: TDBXReader
       */
      public delegate void DropServerClassesCallback();
      private delegate void DropServerClassesDelegate();

      public void DropServerClasses(TDBXReader ClassReader, DropServerClassesCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.POST);
        cmd.setText("DSAdmin.DropServerClasses");
        cmd.prepare(get_DSAdmin_DropServerClasses_Metadata());
        DropServerClassesDelegate DropServerClassesDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke();
          }
        };
        cmd.getParameter(0).getValue().SetAsTable(ClassReader);
        getConnection().execute(cmd, this, DropServerClassesDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_CreateServerMethods_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_CreateServerMethods_Metadata() {
        if (DSAdmin_CreateServerMethods_Metadata == null) {
          DSAdmin_CreateServerMethods_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("MethodReader", DSRESTParamDirection.Input, DBXDataTypes.TableType, "TDBXReader"),
          };
        }
        return DSAdmin_CreateServerMethods_Metadata;
      }

      /**
       * @param MethodReader [in] - Type on server: TDBXReader
       */
      public delegate void CreateServerMethodsCallback();
      private delegate void CreateServerMethodsDelegate();

      public void CreateServerMethods(TDBXReader MethodReader, CreateServerMethodsCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.POST);
        cmd.setText("DSAdmin.CreateServerMethods");
        cmd.prepare(get_DSAdmin_CreateServerMethods_Metadata());
        CreateServerMethodsDelegate CreateServerMethodsDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke();
          }
        };
        cmd.getParameter(0).getValue().SetAsTable(MethodReader);
        getConnection().execute(cmd, this, CreateServerMethodsDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_DropServerMethods_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_DropServerMethods_Metadata() {
        if (DSAdmin_DropServerMethods_Metadata == null) {
          DSAdmin_DropServerMethods_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("MethodReader", DSRESTParamDirection.Input, DBXDataTypes.TableType, "TDBXReader"),
          };
        }
        return DSAdmin_DropServerMethods_Metadata;
      }

      /**
       * @param MethodReader [in] - Type on server: TDBXReader
       */
      public delegate void DropServerMethodsCallback();
      private delegate void DropServerMethodsDelegate();

      public void DropServerMethods(TDBXReader MethodReader, DropServerMethodsCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.POST);
        cmd.setText("DSAdmin.DropServerMethods");
        cmd.prepare(get_DSAdmin_DropServerMethods_Metadata());
        DropServerMethodsDelegate DropServerMethodsDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke();
          }
        };
        cmd.getParameter(0).getValue().SetAsTable(MethodReader);
        getConnection().execute(cmd, this, DropServerMethodsDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_GetServerClasses_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_GetServerClasses_Metadata() {
        if (DSAdmin_GetServerClasses_Metadata == null) {
          DSAdmin_GetServerClasses_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.TableType, "TDBXReader"),
          };
        }
        return DSAdmin_GetServerClasses_Metadata;
      }

      /**
       * @return result - Type on server: TDBXReader
       */
      public delegate void GetServerClassesCallback(TDBXReader Result);
      private delegate void GetServerClassesDelegate();

      public void GetServerClasses(GetServerClassesCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.GetServerClasses");
        cmd.prepare(get_DSAdmin_GetServerClasses_Metadata());
        GetServerClassesDelegate GetServerClassesDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke((TDBXReader)cmd.getParameter(0).getValue().GetAsTable());
          }
        };
        getConnection().execute(cmd, this, GetServerClassesDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_ListClasses_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_ListClasses_Metadata() {
        if (DSAdmin_ListClasses_Metadata == null) {
          DSAdmin_ListClasses_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.JsonValueType, "TJSONArray"),
          };
        }
        return DSAdmin_ListClasses_Metadata;
      }

      /**
       * @return result - Type on server: TJSONArray
       */
      public delegate void ListClassesCallback(TJSONArray Result);
      private delegate void ListClassesDelegate();

      public void ListClasses(ListClassesCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.ListClasses");
        cmd.prepare(get_DSAdmin_ListClasses_Metadata());
        ListClassesDelegate ListClassesDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke((TJSONArray)cmd.getParameter(0).getValue().GetAsJSONValue());
          }
        };
        getConnection().execute(cmd, this, ListClassesDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_DescribeClass_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_DescribeClass_Metadata() {
        if (DSAdmin_DescribeClass_Metadata == null) {
          DSAdmin_DescribeClass_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("ClassName", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.JsonValueType, "TJSONObject"),
          };
        }
        return DSAdmin_DescribeClass_Metadata;
      }

      /**
       * @param ClassName [in] - Type on server: string
       * @return result - Type on server: TJSONObject
       */
      public delegate void DescribeClassCallback(TJSONObject Result);
      private delegate void DescribeClassDelegate();

      public void DescribeClass(String ClassName, DescribeClassCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.DescribeClass");
        cmd.prepare(get_DSAdmin_DescribeClass_Metadata());
        DescribeClassDelegate DescribeClassDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke((TJSONObject)cmd.getParameter(1).getValue().GetAsJSONValue());
          }
        };
        cmd.getParameter(0).getValue().SetAsString(ClassName);
        getConnection().execute(cmd, this, DescribeClassDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_ListMethods_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_ListMethods_Metadata() {
        if (DSAdmin_ListMethods_Metadata == null) {
          DSAdmin_ListMethods_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("ClassName", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.JsonValueType, "TJSONArray"),
          };
        }
        return DSAdmin_ListMethods_Metadata;
      }

      /**
       * @param ClassName [in] - Type on server: string
       * @return result - Type on server: TJSONArray
       */
      public delegate void ListMethodsCallback(TJSONArray Result);
      private delegate void ListMethodsDelegate();

      public void ListMethods(String ClassName, ListMethodsCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.ListMethods");
        cmd.prepare(get_DSAdmin_ListMethods_Metadata());
        ListMethodsDelegate ListMethodsDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke((TJSONArray)cmd.getParameter(1).getValue().GetAsJSONValue());
          }
        };
        cmd.getParameter(0).getValue().SetAsString(ClassName);
        getConnection().execute(cmd, this, ListMethodsDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_DescribeMethod_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_DescribeMethod_Metadata() {
        if (DSAdmin_DescribeMethod_Metadata == null) {
          DSAdmin_DescribeMethod_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("ServerMethodName", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.JsonValueType, "TJSONObject"),
          };
        }
        return DSAdmin_DescribeMethod_Metadata;
      }

      /**
       * @param ServerMethodName [in] - Type on server: string
       * @return result - Type on server: TJSONObject
       */
      public delegate void DescribeMethodCallback(TJSONObject Result);
      private delegate void DescribeMethodDelegate();

      public void DescribeMethod(String ServerMethodName, DescribeMethodCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.DescribeMethod");
        cmd.prepare(get_DSAdmin_DescribeMethod_Metadata());
        DescribeMethodDelegate DescribeMethodDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke((TJSONObject)cmd.getParameter(1).getValue().GetAsJSONValue());
          }
        };
        cmd.getParameter(0).getValue().SetAsString(ServerMethodName);
        getConnection().execute(cmd, this, DescribeMethodDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_GetServerMethods_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_GetServerMethods_Metadata() {
        if (DSAdmin_GetServerMethods_Metadata == null) {
          DSAdmin_GetServerMethods_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.TableType, "TDBXReader"),
          };
        }
        return DSAdmin_GetServerMethods_Metadata;
      }

      /**
       * @return result - Type on server: TDBXReader
       */
      public delegate void GetServerMethodsCallback(TDBXReader Result);
      private delegate void GetServerMethodsDelegate();

      public void GetServerMethods(GetServerMethodsCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.GetServerMethods");
        cmd.prepare(get_DSAdmin_GetServerMethods_Metadata());
        GetServerMethodsDelegate GetServerMethodsDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke((TDBXReader)cmd.getParameter(0).getValue().GetAsTable());
          }
        };
        getConnection().execute(cmd, this, GetServerMethodsDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_GetServerMethodParameters_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_GetServerMethodParameters_Metadata() {
        if (DSAdmin_GetServerMethodParameters_Metadata == null) {
          DSAdmin_GetServerMethodParameters_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.TableType, "TDBXReader"),
          };
        }
        return DSAdmin_GetServerMethodParameters_Metadata;
      }

      /**
       * @return result - Type on server: TDBXReader
       */
      public delegate void GetServerMethodParametersCallback(TDBXReader Result);
      private delegate void GetServerMethodParametersDelegate();

      public void GetServerMethodParameters(GetServerMethodParametersCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.GetServerMethodParameters");
        cmd.prepare(get_DSAdmin_GetServerMethodParameters_Metadata());
        GetServerMethodParametersDelegate GetServerMethodParametersDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke((TDBXReader)cmd.getParameter(0).getValue().GetAsTable());
          }
        };
        getConnection().execute(cmd, this, GetServerMethodParametersDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_GetDatabaseConnectionProperties_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_GetDatabaseConnectionProperties_Metadata() {
        if (DSAdmin_GetDatabaseConnectionProperties_Metadata == null) {
          DSAdmin_GetDatabaseConnectionProperties_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.TableType, "TDBXReader"),
          };
        }
        return DSAdmin_GetDatabaseConnectionProperties_Metadata;
      }

      /**
       * @return result - Type on server: TDBXReader
       */
      public delegate void GetDatabaseConnectionPropertiesCallback(TDBXReader Result);
      private delegate void GetDatabaseConnectionPropertiesDelegate();

      public void GetDatabaseConnectionProperties(GetDatabaseConnectionPropertiesCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.GetDatabaseConnectionProperties");
        cmd.prepare(get_DSAdmin_GetDatabaseConnectionProperties_Metadata());
        GetDatabaseConnectionPropertiesDelegate GetDatabaseConnectionPropertiesDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke((TDBXReader)cmd.getParameter(0).getValue().GetAsTable());
          }
        };
        getConnection().execute(cmd, this, GetDatabaseConnectionPropertiesDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_GetDSServerName_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_GetDSServerName_Metadata() {
        if (DSAdmin_GetDSServerName_Metadata == null) {
          DSAdmin_GetDSServerName_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.WideStringType, "string"),
          };
        }
        return DSAdmin_GetDSServerName_Metadata;
      }

      /**
       * @return result - Type on server: string
       */
      public delegate void GetDSServerNameCallback(String Result);
      private delegate void GetDSServerNameDelegate();

      public void GetDSServerName(GetDSServerNameCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.GetDSServerName");
        cmd.prepare(get_DSAdmin_GetDSServerName_Metadata());
        GetDSServerNameDelegate GetDSServerNameDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke(cmd.getParameter(0).getValue().GetAsString());
          }
        };
        getConnection().execute(cmd, this, GetDSServerNameDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_ConsumeClientChannel_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_ConsumeClientChannel_Metadata() {
        if (DSAdmin_ConsumeClientChannel_Metadata == null) {
          DSAdmin_ConsumeClientChannel_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("ChannelName", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("ClientManagerId", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("CallbackId", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("ChannelNames", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("SecurityToken", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("ResponseData", DSRESTParamDirection.Input, DBXDataTypes.JsonValueType, "TJSONValue"),
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.JsonValueType, "TJSONValue"),
          };
        }
        return DSAdmin_ConsumeClientChannel_Metadata;
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
      public delegate void ConsumeClientChannelCallback(TJSONValue Result);
      private delegate void ConsumeClientChannelDelegate();

      public void ConsumeClientChannel(String ChannelName, String ClientManagerId, String CallbackId, String ChannelNames, String SecurityToken, TJSONValue ResponseData, ConsumeClientChannelCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.POST);
        cmd.setText("DSAdmin.ConsumeClientChannel");
        cmd.prepare(get_DSAdmin_ConsumeClientChannel_Metadata());
        ConsumeClientChannelDelegate ConsumeClientChannelDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke((TJSONValue)cmd.getParameter(6).getValue().GetAsJSONValue());
          }
        };
        cmd.getParameter(0).getValue().SetAsString(ChannelName);
        cmd.getParameter(1).getValue().SetAsString(ClientManagerId);
        cmd.getParameter(2).getValue().SetAsString(CallbackId);
        cmd.getParameter(3).getValue().SetAsString(ChannelNames);
        cmd.getParameter(4).getValue().SetAsString(SecurityToken);
        cmd.getParameter(5).getValue().SetAsJSONValue(ResponseData);
        getConnection().execute(cmd, this, ConsumeClientChannelDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_ConsumeClientChannelTimeout_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_ConsumeClientChannelTimeout_Metadata() {
        if (DSAdmin_ConsumeClientChannelTimeout_Metadata == null) {
          DSAdmin_ConsumeClientChannelTimeout_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("ChannelName", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("ClientManagerId", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("CallbackId", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("ChannelNames", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("SecurityToken", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("Timeout", DSRESTParamDirection.Input, DBXDataTypes.Int32Type, "Integer"),
            new DSRESTParameterMetaData("ResponseData", DSRESTParamDirection.Input, DBXDataTypes.JsonValueType, "TJSONValue"),
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.JsonValueType, "TJSONValue"),
          };
        }
        return DSAdmin_ConsumeClientChannelTimeout_Metadata;
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
      public delegate void ConsumeClientChannelTimeoutCallback(TJSONValue Result);
      private delegate void ConsumeClientChannelTimeoutDelegate();

      public void ConsumeClientChannelTimeout(String ChannelName, String ClientManagerId, String CallbackId, String ChannelNames, String SecurityToken, int Timeout, TJSONValue ResponseData, ConsumeClientChannelTimeoutCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.POST);
        cmd.setText("DSAdmin.ConsumeClientChannelTimeout");
        cmd.prepare(get_DSAdmin_ConsumeClientChannelTimeout_Metadata());
        ConsumeClientChannelTimeoutDelegate ConsumeClientChannelTimeoutDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke((TJSONValue)cmd.getParameter(7).getValue().GetAsJSONValue());
          }
        };
        cmd.getParameter(0).getValue().SetAsString(ChannelName);
        cmd.getParameter(1).getValue().SetAsString(ClientManagerId);
        cmd.getParameter(2).getValue().SetAsString(CallbackId);
        cmd.getParameter(3).getValue().SetAsString(ChannelNames);
        cmd.getParameter(4).getValue().SetAsString(SecurityToken);
        cmd.getParameter(5).getValue().SetAsInt32(Timeout);
        cmd.getParameter(6).getValue().SetAsJSONValue(ResponseData);
        getConnection().execute(cmd, this, ConsumeClientChannelTimeoutDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_CloseClientChannel_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_CloseClientChannel_Metadata() {
        if (DSAdmin_CloseClientChannel_Metadata == null) {
          DSAdmin_CloseClientChannel_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("ChannelId", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("SecurityToken", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.BooleanType, "Boolean"),
          };
        }
        return DSAdmin_CloseClientChannel_Metadata;
      }

      /**
       * @param ChannelId [in] - Type on server: string
       * @param SecurityToken [in] - Type on server: string
       * @return result - Type on server: Boolean
       */
      public delegate void CloseClientChannelCallback(bool Result);
      private delegate void CloseClientChannelDelegate();

      public void CloseClientChannel(String ChannelId, String SecurityToken, CloseClientChannelCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.CloseClientChannel");
        cmd.prepare(get_DSAdmin_CloseClientChannel_Metadata());
        CloseClientChannelDelegate CloseClientChannelDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke(cmd.getParameter(2).getValue().GetAsBoolean());
          }
        };
        cmd.getParameter(0).getValue().SetAsString(ChannelId);
        cmd.getParameter(1).getValue().SetAsString(SecurityToken);
        getConnection().execute(cmd, this, CloseClientChannelDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_RegisterClientCallbackServer_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_RegisterClientCallbackServer_Metadata() {
        if (DSAdmin_RegisterClientCallbackServer_Metadata == null) {
          DSAdmin_RegisterClientCallbackServer_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("ChannelId", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("CallbackId", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("ChannelNames", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("SecurityToken", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.BooleanType, "Boolean"),
          };
        }
        return DSAdmin_RegisterClientCallbackServer_Metadata;
      }

      /**
       * @param ChannelId [in] - Type on server: string
       * @param CallbackId [in] - Type on server: string
       * @param ChannelNames [in] - Type on server: string
       * @param SecurityToken [in] - Type on server: string
       * @return result - Type on server: Boolean
       */
      public delegate void RegisterClientCallbackServerCallback(bool Result);
      private delegate void RegisterClientCallbackServerDelegate();

      public void RegisterClientCallbackServer(String ChannelId, String CallbackId, String ChannelNames, String SecurityToken, RegisterClientCallbackServerCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.RegisterClientCallbackServer");
        cmd.prepare(get_DSAdmin_RegisterClientCallbackServer_Metadata());
        RegisterClientCallbackServerDelegate RegisterClientCallbackServerDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke(cmd.getParameter(4).getValue().GetAsBoolean());
          }
        };
        cmd.getParameter(0).getValue().SetAsString(ChannelId);
        cmd.getParameter(1).getValue().SetAsString(CallbackId);
        cmd.getParameter(2).getValue().SetAsString(ChannelNames);
        cmd.getParameter(3).getValue().SetAsString(SecurityToken);
        getConnection().execute(cmd, this, RegisterClientCallbackServerDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_UnregisterClientCallback_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_UnregisterClientCallback_Metadata() {
        if (DSAdmin_UnregisterClientCallback_Metadata == null) {
          DSAdmin_UnregisterClientCallback_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("ChannelId", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("CallbackId", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("SecurityToken", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.BooleanType, "Boolean"),
          };
        }
        return DSAdmin_UnregisterClientCallback_Metadata;
      }

      /**
       * @param ChannelId [in] - Type on server: string
       * @param CallbackId [in] - Type on server: string
       * @param SecurityToken [in] - Type on server: string
       * @return result - Type on server: Boolean
       */
      public delegate void UnregisterClientCallbackCallback(bool Result);
      private delegate void UnregisterClientCallbackDelegate();

      public void UnregisterClientCallback(String ChannelId, String CallbackId, String SecurityToken, UnregisterClientCallbackCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.GET);
        cmd.setText("DSAdmin.UnregisterClientCallback");
        cmd.prepare(get_DSAdmin_UnregisterClientCallback_Metadata());
        UnregisterClientCallbackDelegate UnregisterClientCallbackDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke(cmd.getParameter(3).getValue().GetAsBoolean());
          }
        };
        cmd.getParameter(0).getValue().SetAsString(ChannelId);
        cmd.getParameter(1).getValue().SetAsString(CallbackId);
        cmd.getParameter(2).getValue().SetAsString(SecurityToken);
        getConnection().execute(cmd, this, UnregisterClientCallbackDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_BroadcastToChannel_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_BroadcastToChannel_Metadata() {
        if (DSAdmin_BroadcastToChannel_Metadata == null) {
          DSAdmin_BroadcastToChannel_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("ChannelName", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("Msg", DSRESTParamDirection.Input, DBXDataTypes.JsonValueType, "TJSONValue"),
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.BooleanType, "Boolean"),
          };
        }
        return DSAdmin_BroadcastToChannel_Metadata;
      }

      /**
       * @param ChannelName [in] - Type on server: string
       * @param Msg [in] - Type on server: TJSONValue
       * @return result - Type on server: Boolean
       */
      public delegate void BroadcastToChannelCallback(bool Result);
      private delegate void BroadcastToChannelDelegate();

      public void BroadcastToChannel(String ChannelName, TJSONValue Msg, BroadcastToChannelCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.POST);
        cmd.setText("DSAdmin.BroadcastToChannel");
        cmd.prepare(get_DSAdmin_BroadcastToChannel_Metadata());
        BroadcastToChannelDelegate BroadcastToChannelDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke(cmd.getParameter(2).getValue().GetAsBoolean());
          }
        };
        cmd.getParameter(0).getValue().SetAsString(ChannelName);
        cmd.getParameter(1).getValue().SetAsJSONValue(Msg);
        getConnection().execute(cmd, this, BroadcastToChannelDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_BroadcastObjectToChannel_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_BroadcastObjectToChannel_Metadata() {
        if (DSAdmin_BroadcastObjectToChannel_Metadata == null) {
          DSAdmin_BroadcastObjectToChannel_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("ChannelName", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("Msg", DSRESTParamDirection.Input, DBXDataTypes.JsonValueType, "TObject"),
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.BooleanType, "Boolean"),
          };
        }
        return DSAdmin_BroadcastObjectToChannel_Metadata;
      }

      /**
       * @param ChannelName [in] - Type on server: string
       * @param Msg [in] - Type on server: TObject
       * @return result - Type on server: Boolean
       */
      public delegate void BroadcastObjectToChannelCallback(bool Result);
      private delegate void BroadcastObjectToChannelDelegate();

      public void BroadcastObjectToChannel(String ChannelName, TJSONObject Msg, BroadcastObjectToChannelCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.POST);
        cmd.setText("DSAdmin.BroadcastObjectToChannel");
        cmd.prepare(get_DSAdmin_BroadcastObjectToChannel_Metadata());
        BroadcastObjectToChannelDelegate BroadcastObjectToChannelDel = () => {
          if (callback != null)
          {
            callback.DynamicInvoke(cmd.getParameter(2).getValue().GetAsBoolean());
          }
        };
        cmd.getParameter(0).getValue().SetAsString(ChannelName);
        cmd.getParameter(1).getValue().SetAsJSONValue(Msg);
        getConnection().execute(cmd, this, BroadcastObjectToChannelDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_NotifyCallback_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_NotifyCallback_Metadata() {
        if (DSAdmin_NotifyCallback_Metadata == null) {
          DSAdmin_NotifyCallback_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("ClientId", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("CallbackId", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("Msg", DSRESTParamDirection.Input, DBXDataTypes.JsonValueType, "TJSONValue"),
            new DSRESTParameterMetaData("Response", DSRESTParamDirection.Output, DBXDataTypes.JsonValueType, "TJSONValue"),
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.BooleanType, "Boolean"),
          };
        }
        return DSAdmin_NotifyCallback_Metadata;
      }

      /**
       * @param ClientId [in] - Type on server: string
       * @param CallbackId [in] - Type on server: string
       * @param Msg [in] - Type on server: TJSONValue
       * @param Response [out] - Type on server: TJSONValue
       * @return result - Type on server: Boolean
       */
      public class NotifyCallbackReturns {
        public TJSONValue Response;
        public bool returnValue;
      }
      public delegate void NotifyCallbackCallback(NotifyCallbackReturns Returns);
      private delegate void NotifyCallbackDelegate();

      public void NotifyCallback(String ClientId, String CallbackId, TJSONValue Msg, NotifyCallbackCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.POST);
        cmd.setText("DSAdmin.NotifyCallback");
        cmd.prepare(get_DSAdmin_NotifyCallback_Metadata());
        NotifyCallbackDelegate NotifyCallbackDel = () => {
          if (callback != null)
          {
            NotifyCallbackReturns ret = new NotifyCallbackReturns();
            ret.Response = (TJSONValue)cmd.getParameter(3).getValue().GetAsJSONValue();
            ret.returnValue = cmd.getParameter(4).getValue().GetAsBoolean();
            callback.DynamicInvoke(ret);
          }
        };
        cmd.getParameter(0).getValue().SetAsString(ClientId);
        cmd.getParameter(1).getValue().SetAsString(CallbackId);
        cmd.getParameter(2).getValue().SetAsJSONValue(Msg);
        getConnection().execute(cmd, this, NotifyCallbackDel, ExCal);
      }
      private DSRESTParameterMetaData[] DSAdmin_NotifyObject_Metadata;
      private DSRESTParameterMetaData[] get_DSAdmin_NotifyObject_Metadata() {
        if (DSAdmin_NotifyObject_Metadata == null) {
          DSAdmin_NotifyObject_Metadata = new DSRESTParameterMetaData[]{
            new DSRESTParameterMetaData("ClientId", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("CallbackId", DSRESTParamDirection.Input, DBXDataTypes.WideStringType, "string"),
            new DSRESTParameterMetaData("Msg", DSRESTParamDirection.Input, DBXDataTypes.JsonValueType, "TObject"),
            new DSRESTParameterMetaData("Response", DSRESTParamDirection.Output, DBXDataTypes.JsonValueType, "TObject"),
            new DSRESTParameterMetaData("", DSRESTParamDirection.ReturnValue, DBXDataTypes.BooleanType, "Boolean"),
          };
        }
        return DSAdmin_NotifyObject_Metadata;
      }

      /**
       * @param ClientId [in] - Type on server: string
       * @param CallbackId [in] - Type on server: string
       * @param Msg [in] - Type on server: TObject
       * @param Response [out] - Type on server: TObject
       * @return result - Type on server: Boolean
       */
      public class NotifyObjectReturns {
        public TJSONObject Response;
        public bool returnValue;
      }
      public delegate void NotifyObjectCallback(NotifyObjectReturns Returns);
      private delegate void NotifyObjectDelegate();

      public void NotifyObject(String ClientId, String CallbackId, TJSONObject Msg, NotifyObjectCallback callback = null, ExceptionCallback ExCal = null)
      {
        DSRESTCommand cmd = getConnection().CreateCommand();
        cmd.setRequestType(DSHTTPRequestType.POST);
        cmd.setText("DSAdmin.NotifyObject");
        cmd.prepare(get_DSAdmin_NotifyObject_Metadata());
        NotifyObjectDelegate NotifyObjectDel = () => {
          if (callback != null)
          {
            NotifyObjectReturns ret = new NotifyObjectReturns();
            ret.Response = (TJSONObject)cmd.getParameter(3).getValue().GetAsJSONValue();
            ret.returnValue = cmd.getParameter(4).getValue().GetAsBoolean();
            callback.DynamicInvoke(ret);
          }
        };
        cmd.getParameter(0).getValue().SetAsString(ClientId);
        cmd.getParameter(1).getValue().SetAsString(CallbackId);
        cmd.getParameter(2).getValue().SetAsJSONValue(Msg);
        getConnection().execute(cmd, this, NotifyObjectDel, ExCal);
      }
    }
}
