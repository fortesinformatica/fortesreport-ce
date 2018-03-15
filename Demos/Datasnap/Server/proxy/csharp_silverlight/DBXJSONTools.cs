//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

using System;
using Newtonsoft.Json.Linq;

namespace Embarcadero.Datasnap.WindowsPhone7
{
    /**
     * Use the properties and methods of DBXJSONTools to:
     * <br> - Convert a JSON to a Value type and vice versa.
     * <br> - Convert a JSON to a Table type .
     * <br> - Create a JObject from a DBXParameter, DBXReader.
     * <br> - Convert a JArray to a TStream type.
     * <br> - Convert a JSON to a DBXValue.
     * <br> - Convert a JSON to TJSONValue.
     */
 
    public class DBXJSONTools
    {
       /**
    	 * create a DBXValueType from a JArray
    	 * @param json JArray with the values to set into DBXValuesType
    	 * @return a DBXValueType
    	 */
       
        public static DBXValueType JSONToValueType(TJSONArray json)
        {
            DBXValueType vt = new DBXValueType();
            JSONToValueType(json, vt);
            return vt;
        }

       /**
    	 * create a DBXValueType from a JArray 
    	 * @param json JArray with the values
    	 * @param vt the DBXValueType to set
    	 */
        public static void JSONToValueType(TJSONArray arr, DBXValueType vt)
        {
            JArray json = (JArray)arr.getInternalObject();
            vt.setName(json.Value<string>(0));
            vt.setDataType(json.Value<int>(1));
            vt.setOrdinal(json.Value<int>(2));
            vt.setSubType(json.Value<int>(3));
            vt.setScale(json.Value<int>(4));
            vt.setSize(json.Value<int>(5));
            vt.setPrecision(json.Value<int>(6));
            vt.setChildPosition(json.Value<int>(7));
            vt.setNullable(json.Value<bool>(8));
            vt.setHidden(json.Value<bool>(9));
            vt.setParameterDirection(json.Value<int>(10));
            vt.setValueParameter(json.Value<bool>(11));
            vt.setLiteral(json.Value<bool>(12));
        }

        /**
      	 * @param DataType the DBXValueType to set into the JArray
      	 * @return a JArray with the value of DBXValueType
      	 */

        public static TJSONArray ValueTypeToJSON(DBXValueType DataType)
        {
            JArray Meta = new JArray();
            Meta.Add(DataType.getName());
            Meta.Add(DataType.getDataType());
            Meta.Add(DataType.getOrdinal());
            Meta.Add(DataType.getSubType());
            Meta.Add(DataType.getScale());
            Meta.Add(DataType.getSize());
            Meta.Add(DataType.getPrecision());
            Meta.Add(DataType.getChildPosition());
            Meta.Add(DataType.getNullable());
            Meta.Add(DataType.getHidden());
            Meta.Add(DataType.getParameterDirection());
            Meta.Add(DataType.getValueParameter());
            Meta.Add(DataType.getLiteral());
            return new TJSONArray(Meta);
        }

        /** 
         * @param param is an InputOutput, Output or ReturnValue (JSONValueType)
         * @param value is a generic object readed from the json result array
         * @return an Object that rapresent the result JSON or the JSONObject itself
         */
        public static Object JSONToTableType(Object value, String DBXTypeName)
        {
            if (DBXTypeName.Equals("TParams"))
            {
                return TParams.CreateFrom(new TJSONObject((JObject)value));
            }
            else
            {
                if ((DBXTypeName.Equals("TDBXReader"))
                    || (DBXTypeName.Equals("TDBXReaderValue")))
                {
                    return TDBXReader.createFrom(new TJSONObject((JObject)value));
                }
                else
                {
                    if (DBXTypeName.Equals("TDataSet"))
                    {
                        return TDataSet.createFrom(new TJSONObject((JObject)value));
                    }
                }
            }
            throw new DBXException(DBXTypeName + " is not a table type");
        }

        /**
         * Create a JSONObject from a DBXParameters 
         * @param dbxParameters
         * @return JSONObject
         */
        public static TJSONObject DBXParametersToJSONObject(TParams dbxParameters)
        {
            TJSONObject json = new TJSONObject();
            TJSONObject jsonobj = new TJSONObject();

            for (int i = 0; i < dbxParameters.size(); i++)
            {
                TJSONArray arr3;
                try
                {
                    arr3 = new TJSONArray();
                    arr3.add(new TJSONString(dbxParameters.getParameter(i).getValue().ToString()));
                    json.addPairs(dbxParameters.getParameter(i).getName(), arr3);
                }
                catch (Exception)
                {

                }
            }

            TJSONArray arr2;
            arr2 = new TJSONArray();
            for (int i = 0; i < dbxParameters.size(); i++)
            {
                TJSONArray arr;
                arr = new TJSONArray();
                arr.add(new TJSONString(dbxParameters.getParameter(i).getName()));
                arr.add(new TJSONNumber(dbxParameters.getParameter(i).getDataType()));
                arr.add(new TJSONNumber(dbxParameters.getParameter(i).getOrdinal()));
                arr.add(new TJSONNumber(dbxParameters.getParameter(i).getSubType()));
                arr.add(new TJSONNumber(dbxParameters.getParameter(i).getScale()));
                arr.add(new TJSONNumber(dbxParameters.getParameter(i).getSize()));
                arr.add(new TJSONNumber(dbxParameters.getParameter(i).getPrecision()));
                arr.add(new TJSONNumber(dbxParameters.getParameter(i).getChildPosition()));
                if (dbxParameters.getParameter(i).getNullable()) arr.add(new TJSONTrue());
                else arr.add(new TJSONFalse());
                if (dbxParameters.getParameter(i).getHidden()) arr.add(new TJSONTrue());
                else arr.add(new TJSONFalse());
                arr.add(new TJSONNumber(dbxParameters.getParameter(i).getParameterDirection()));
                if (dbxParameters.getParameter(i).getValueParameter()) arr.add(new TJSONTrue());
                else arr.add(new TJSONFalse());
                if (dbxParameters.getParameter(i).getLiteral()) arr.add(new TJSONTrue());
                else arr.add(new TJSONFalse());
                arr2.add(arr);
            }
            try
            {
                json.addPairs("table", arr2);
            }
            catch (Exception)
            {

            }
            return json;
        }


        /**
      	 * Create a JSONObject from a DBXReader 
      	 * @param dbxReader
      	 * @return JSONObject
      	 */
        public static TJSONObject DBXReaderToJSONObject(TDBXReader dbxReader)
        {
            TJSONObject json = new TJSONObject();
            TJSONArray arr2;
            TParams columns = dbxReader.getColumns();
            try
            {
                arr2 = new TJSONArray();
                for (int i = 0; i < columns.size(); i++)
                {
                    arr2.add(columns.getParameter(i).tojson());
                    // Create the empty JArray for the data. Will be filled after
                    json.addPairs(columns.getParameter(i).getName(), new TJSONArray());
                }

                while (dbxReader.next())
                {
                    for (int c = 0; c < columns.size(); c++)
                        dbxReader.getColumns().getParameter(c).getValue().appendTo(
                                json.getJSONArray(columns.getParameter(c)
                                                .getName()));
                }
                json.addPairs("table", arr2);
            }
            catch (Exception)
            { 
            
            }
            return json;
        }


        /**
    	 * Create a TStream from a JArray 
    	 * @param value
    	 * @return TStream
    	 */
        public static TStream JSONToStream(TJSONArray value)
        {
            return TStream.CreateFrom(value);
        }


        private static Object JTokenToObject(Object o)
        {
            if ((o is JToken) && !(o is JArray) && !(o is JObject))
            {
                return ((JToken)o).Value<object>();
            }
            return o;
        }
  
  
      /**
    	 * Convert a Object to DBXValue 
    	 * @param o
    	 * @param value
    	 * @param DBXTypeName
    	 */
        public static void JSONtoDBX(Object o, DBXValue value, String DBXTypeName)
        {
            try
            {
                if (DBXTypeName.StartsWith("TDBX") && DBXTypeName.EndsWith("Value")
                    && !(DBXTypeName.IndexOf("Stream") > 0) && !(DBXTypeName.IndexOf("Reader") > 0))
                {
                    if (JTokenToObject(o) == null)
                    {
                        value.GetAsDBXValue().setNull();
                        return;
                    }
                }

                if (!((o == null) && (DBXTypeName.Equals(""))))
                {
                    switch (value.getDBXType())
                    {
                        case DBXDataTypes.Int8Type:
                            if (o is JToken)
                                o = ((JToken)o).Value<Object>();
                            if ((DBXTypeName.Equals("TDBXInt8Value")))
                            {
                                value.GetAsDBXValue()
                                        .SetAsInt8(Int16.Parse(o.ToString()));
                            }
                            else
                                value.SetAsInt8(Int16.Parse(o.ToString()));
                            break;

                        case DBXDataTypes.Int16Type:
                            if (o is JToken)
                                o = ((JToken)o).Value<Object>();
                            if ((DBXTypeName.Equals("TDBXInt16Value")))
                            {
                                value.GetAsDBXValue().SetAsInt16(
                                        (Int16.Parse(o.ToString())));
                            }
                            else
                                value.SetAsInt16(Int16.Parse(o.ToString()));
                            break;

                        case DBXDataTypes.Int32Type:
                            if (o is JToken)
                                o = ((JToken)o).Value<Object>();
                            if ((DBXTypeName.Equals("TDBXInt32Value")))
                            {
                                value.GetAsDBXValue().SetAsInt32(
                                        (Int32.Parse(o.ToString())));
                            }
                            else
                                value.SetAsInt32(Int32.Parse(o.ToString()));
                            break;

                        case DBXDataTypes.Int64Type:
                            if (o is JToken)
                                o = ((JToken)o).Value<Object>();
                            if ((DBXTypeName.Equals("TDBXInt64Value")))
                            {
                                value.GetAsDBXValue().SetAsInt64(
                                        (Int64.Parse(o.ToString())));
                            }
                            else
                                value.SetAsInt64(Int64.Parse(o.ToString()));
                            break;

                        case DBXDataTypes.BcdType:
                            if (o is JToken)
                                o = ((JToken)o).Value<Object>();
                            if ((DBXTypeName.Equals("TDBXBcdValue")))
                            {                                
                                value.GetAsDBXValue().SetAsBcd(
                                    DBXDefaultFormatter.getInstance().StringToDouble(o.ToString()));
                            }
                            else
                                value.SetAsBcd((DBXDefaultFormatter.getInstance()
                                    .StringToDouble((o.ToString()))));
                            break;

                        case DBXDataTypes.UInt8Type:
                            if (o is JToken)
                                o = ((JToken)o).Value<Object>();
                            if ((DBXTypeName.Equals("TDBXUInt8Value")))
                            {
                                value.GetAsDBXValue().SetAsUInt8(
                                        (UInt16.Parse(o.ToString())));
                            }
                            else
                                value.SetAsUInt8(UInt16.Parse(o.ToString()));
                            break;

                        case DBXDataTypes.UInt16Type:
                            if (o is JToken)
                                o = ((JToken)o).Value<Object>();
                            if ((DBXTypeName.Equals("TDBXUInt16Value")))
                            {
                                value.GetAsDBXValue().SetAsUInt16(
                                        (UInt16.Parse(o.ToString())));
                            }
                            else
                                value.SetAsUInt16(UInt16.Parse(o.ToString()));
                            break;

                        case DBXDataTypes.UInt32Type:
                            if (o is JToken)
                                o = ((JToken)o).Value<Object>();
                            if ((DBXTypeName.Equals("TDBXUInt32Value")))
                            {
                                value.GetAsDBXValue().SetAsUInt32(
                                        UInt32.Parse((o.ToString())));
                            }
                            else
                                value.SetAsUInt32(UInt32.Parse(o.ToString()));
                            break;

                        case DBXDataTypes.UInt64Type:
                            if (o is JToken)
                                o = ((JToken)o).Value<Object>();
                            if ((DBXTypeName.Equals("TDBXUInt64Value")))
                            {
                                value.GetAsDBXValue().SetAsUInt64(
                                        int.Parse(o.ToString()));
                            }
                            else
                                value.SetAsUInt64(int.Parse(o.ToString()));
                            break;
                        case DBXDataTypes.DoubleType:
                            {
                                if (o is JToken)
                                    o = ((JToken)o).Value<Object>();
                                if (DBXTypeName.Equals("TDBXDoubleValue"))
                                {
                                    value.GetAsDBXValue().SetAsDouble(
                                            (Convert.ToDouble(o.ToString())));
                                }
                                else
                                    value.SetAsDouble(Convert.ToDouble(o.ToString()));
                                break;
                            }
                        case DBXDataTypes.SingleType:
                            {
                                if (o is JToken)
                                    o = ((JToken)o).Value<Object>();
                                if (DBXTypeName.Equals("TDBXSingleValue"))
                                {
                                    value.GetAsDBXValue().SetAsSingle(
                                            Convert.ToSingle(o.ToString()));
                                }
                                else
                                    value.SetAsSingle(Convert.ToSingle(o.ToString()));
                                break;
                            }
                        case DBXDataTypes.CurrencyType:
                            {
                                if (o is JToken)
                                    o = ((JToken)o).Value<Object>();
                                value.SetAsCurrency(Convert.ToDouble(o));
                                break;
                            }
                        case DBXDataTypes.JsonValueType:
                            {
                                value.SetAsJSONValue((TJSONValue)DBXJSONTools
                                        .JSONToJSONValue(o));
                                break;
                            }
                        case DBXDataTypes.BinaryBlobType:
                            {
                                if (DBXTypeName.Equals("TDBXStreamValue"))
                                {
                                    value.GetAsDBXValue().SetAsStream(DBXJSONTools.JSONToStream(new TJSONArray((JArray)o)));
                                }
                                else
                                    value.SetAsStream(DBXJSONTools.JSONToStream(new TJSONArray((JArray)o)));
                                break;
                            }
                        case DBXDataTypes.BlobType:
                            {
                                value.SetAsBlob(DBXJSONTools.JSONToStream(new TJSONArray((JArray)o)));
                                break;
                            }
                        case DBXDataTypes.TimeStampType:
                            {
                                if (o is JToken)
                                    o = ((JToken)o).Value<Object>();
                                DateTime d = DBXDefaultFormatter.getInstance()
                                        .StringToDateTime((String)o);
                                if (d == null)
                                    throw new DBXException("Invalid date");
                                if (DBXTypeName.Equals("TDBXTimeStampValue"))
                                {
                                    value.GetAsDBXValue().SetAsTimeStamp(d);
                                }
                                else
                                    value.SetAsTimeStamp(d);
                                break;
                            }
                        case DBXDataTypes.AnsiStringType:
                            {
                                if (o is JToken)
                                    o = ((JToken)o).Value<Object>();
                                if ((DBXTypeName.Equals("TDBXAnsiStringValue"))
                                        || (DBXTypeName.Equals("TDBXStringValue"))
                                        || (DBXTypeName.Equals("TDBXAnsiCharsValue")))
                                {
                                    value.GetAsDBXValue().SetAsAnsiString((String)o);
                                }
                                else
                                    value.SetAsString((string)o);
                                break;
                            }

                        case DBXDataTypes.TableType:
                            {
                                if (DBXTypeName.Equals("TDBXReaderValue"))
                                {
                                    value.GetAsDBXValue().SetAsTable(
                                        (TableType)DBXJSONTools.JSONToTableType(o, DBXTypeName));
                                }
                                else
                                {
                                    value.SetAsTable((TableType)DBXJSONTools.JSONToTableType(
                                        o, DBXTypeName));
                                }
                                break;
                            }

                        case DBXDataTypes.WideStringType:
                            {
                                if (o is JToken)
                                    o = ((JToken)o).Value<Object>();
                                if ((DBXTypeName.Equals("TDBXAnsiStringValue"))
                                        || (DBXTypeName.Equals("TDBXStringValue"))
                                        || (DBXTypeName.Equals("TDBXAnsiCharsValue"))
                                        || (DBXTypeName.Equals("TDBXWideStringValue")))
                                {
                                    value.GetAsDBXValue().SetAsString((String)o);
                                }
                                else
                                    value.SetAsString((string)o);
                                break;
                            }

                        case DBXDataTypes.TimeType:
                            {
                                if (o is JToken)
                                    o = ((JToken)o).Value<Object>();
                                int t = DBXDefaultFormatter.getInstance().StringToTDBXTime(
                                        (String)o);
                                if (DBXTypeName.Equals("TDBXTimeValue"))
                                {
                                    value.GetAsDBXValue().SetAsTDBXTime(t);
                                }
                                else
                                    value.SetAsTDBXTime(t);
                                break;
                            }

                        case DBXDataTypes.DateType:
                            {
                                if (o is JToken)
                                    o = ((JToken)o).Value<Object>();
                                int t = DBXDefaultFormatter.getInstance().StringToTDBXDate(
                                        (String)o);
                                if (DBXTypeName.Equals("TDBXDateValue"))
                                {
                                    value.GetAsDBXValue().SetAsTDBXDate(t);
                                }
                                else
                                    value.SetAsTDBXDate(t);
                                break;
                            }

                        case DBXDataTypes.DateTimeType:
                            {
                                if (o is JToken)
                                    o = ((JToken)o).Value<Object>();
                                DateTime d = DBXDefaultFormatter.getInstance()
                                        .StringToDateTime((String)o);
                                if (d == null)
                                    throw new DBXException("Invalid date");
                                value.SetAsDateTime(d);
                                break;
                            }
                        case DBXDataTypes.BooleanType:
                            {
                                if (o is JToken)
                                    o = ((JToken)o).Value<Object>();
                                if (DBXTypeName.Equals("TDBXBooleanValue"))
                                {
                                    value.GetAsDBXValue().SetAsBoolean(Convert.ToBoolean(o.ToString()));
                                }
                                else
                                    value.SetAsBoolean(Convert.ToBoolean(o.ToString()));
                                break;
                            }

                        default:
                            throw new DBXException("Cannot convert datatype "
                                    + value.getDBXType().ToString());
                    } // switch
                }

            }
            catch (Exception ex)
            {
                throw new DBXException(ex.Message);
            }
        }

        
        /**
    	 * Convert an Object to TJSONValue 
    	 * @param o
    	 * @return TJSONValue
    	 */
	 
        private static TJSONValue JSONToJSONValue(Object o)
        {
            JToken jtk = JToken.FromObject(o);
            if (jtk == null)
                return new TJSONNull();
            else
            {
                switch (jtk.Type)
                {
                    case JTokenType.Null: { return new TJSONNull(); }
                    case JTokenType.String: { return new TJSONString(jtk.Value<string>()); }
                    case JTokenType.Float: { return new TJSONNumber(jtk.Value<float>()); }
                    case JTokenType.Integer: { return new TJSONNumber(jtk.Value<Int32>()); }
                    case JTokenType.Array: { return new TJSONArray(jtk.Value<JArray>()); }
                    case JTokenType.Object: { return new TJSONObject(jtk.Value<JObject>()); }
                    case JTokenType.Boolean: { if (jtk.Value<Boolean>()) return new TJSONTrue(); else return new TJSONFalse(); }
                }
            }
            throw new DBXException(o.GetType().FullName + " is not a valid JSONValue");
        }
    }
}
