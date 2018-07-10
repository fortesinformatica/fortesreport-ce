//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javaandroid;

import java.util.Date;

/**
 * Some utility functions to convert json object into String, {@link TJSONValue}
 * , {@link TDBXReader}, {@link TParams}.
 * 
 * Use the properties and methods of DBXJSONTools to: <br>
 * - Convert a JSON to a Value type and vice versa. <br>
 * - Convert a JSON to a Table type . <br>
 * - Create a JSONObject from a DBXParameter, DBXReader. <br>
 * - Convert a JSONArray to a TStream type. <br>
 * - Convert a JSON to a DBXValue. <br>
 * - Convert a JSON to TJSONValue.
 */

public class DBXJSONTools {

	/**
	 * Create a DBXValueType from a JSONArray
	 * 
	 * @param json
	 *            JSONArray with the values to set into DBXValuesType
	 * @return a DBXValueType
	 * @throws DBXException
	 */
	public static DBXValueType JSONToValueType(TJSONArray json)
			throws DBXException {
		DBXValueType vt = new DBXValueType();
		JSONToValueType(json, vt);
		return vt;
	}

	/**
	 * create a DBXValueType from a JSONArray
	 * 
	 * @param json
	 *            JSONArray with the values
	 * @param vt
	 *            the DBXValueType to set
	 * @throws DBXException
	 */
	public static void JSONToValueType(TJSONArray json, DBXValueType vt)
			throws DBXException {
		vt.setName(json.getString(0));
		vt.setDataType(json.getInt(1));
		vt.setOrdinal(json.getInt(2));
		vt.setSubType(json.getInt(3));
		vt.setScale(json.getInt(4));
		vt.setSize(json.getInt(5));
		vt.setPrecision(json.getInt(6));
		vt.setChildPosition(json.getInt(7));
		vt.setNullable(json.getBoolean(8));
		vt.setHidden(json.getBoolean(9));
		vt.setParameterDirection(json.getInt(10));
		vt.setValueParameter(json.getBoolean(11));
		vt.setLiteral(json.getBoolean(12));
	}

	/**
	 * Converts a DBXValueType to a json object
	 * 
	 * @param DataType
	 *            the DBXValueType to set into the JSONArray
	 * @return a JSONArray with the value of DBXValueType
	 * @throws DBXException
	 */
	public static TJSONArray ValueTypeToJSON(DBXValueType DataType)
			throws DBXException {
		TJSONArray Meta = new TJSONArray();
		Meta.add(DataType.getName());
		Meta.add(DataType.getDataType());
		Meta.add(DataType.getOrdinal());
		Meta.add(DataType.getSubType());
		Meta.add(DataType.getScale());
		Meta.add(DataType.getSize());
		Meta.add(DataType.getPrecision());
		Meta.add(DataType.getChildPosition());
		Meta.add(DataType.getNullable());
		Meta.add(DataType.getHidden());
		Meta.add(DataType.getParameterDirection());
		Meta.add(DataType.getValueParameter());
		Meta.add(DataType.getLiteral());
		return Meta;
	}

	/**
	 * 
	 * @param param
	 *            is an InputOutput, Output or ReturnValue (JSONValueType)
	 * @param value
	 *            is a generic object readed from the json result array
	 * @return an Object that rapresent the result JSON or the JSONObject itself
	 * @throws DBXException
	 */
	public static Object JSONToTableType(Object value, String DBXTypeName)
			throws DBXException {
		if (DBXTypeName.equals("TParams")) {
			return TParams.CreateFrom((TJSONObject) value);
		} else {
			if ((DBXTypeName.equals("TDBXReader"))
					|| (DBXTypeName.equals("TDBXReaderValue"))) {
				return TDBXReader.createFrom((TJSONObject) value);
			} else {
				if (DBXTypeName.equals("TDataSet")) {
					return TDataSet.createFrom((TJSONObject) value);
				}
			}
		}
		throw new DBXException(DBXTypeName + " is not a table type");
	}

	/**
	 * Create a JSONObject from a DBXParameters
	 * 
	 * @param dbxParameters
	 * @return TJSONObject
	 */
	public static TJSONObject DBXParametersToJSONObject(TParams dbxParameters) {
		TJSONObject json = new TJSONObject();

		TJSONArray arrParameters;
		arrParameters = new TJSONArray();
		for (int i = 0; i < dbxParameters.size(); i++) {
			TJSONArray arrParam;
			arrParam = new TJSONArray();
			arrParam.add(dbxParameters.getParameter(i).getName());
			arrParam.add(dbxParameters.getParameter(i).getDataType());
			arrParam.add(dbxParameters.getParameter(i).getOrdinal());
			arrParam.add(dbxParameters.getParameter(i).getSubType());
			arrParam.add(dbxParameters.getParameter(i).getScale());
			arrParam.add(dbxParameters.getParameter(i).getSize());
			arrParam.add(dbxParameters.getParameter(i).getPrecision());
			arrParam.add(dbxParameters.getParameter(i).getChildPosition());
			arrParam.add(dbxParameters.getParameter(i).getNullable());
			arrParam.add(dbxParameters.getParameter(i).getHidden());
			arrParam.add(dbxParameters.getParameter(i).getParameterDirection());
			arrParam.add(dbxParameters.getParameter(i).getValueParameter());
			arrParam.add(dbxParameters.getParameter(i).getLiteral());
			arrParameters.add(arrParam);
		}
		json.addPairs("table", arrParameters);

		for (int i = 0; i < dbxParameters.size(); i++) {
			TJSONArray arrParamsValue;
			arrParamsValue = new TJSONArray();
			arrParamsValue.add(dbxParameters.getParameter(i).getValue()
					.toString());
			json.addPairs(dbxParameters.getParameter(i).getName(), arrParamsValue);
		}
		return json;
	}

	/**
	 * Create a JSONObject from a DBXReader
	 * 
	 * @param dbxReader
	 * @return TJSONObject
	 * @throws TJSONException
	 */
	public static TJSONObject DBXReaderToJSONObject(TDBXReader dbxReader)
			throws DBXException {
		TJSONObject json = new TJSONObject();
		TJSONArray arrayParams;
		TParams columns = dbxReader.getColumns();
		arrayParams = new TJSONArray();
		for (int i = 0; i < columns.size(); i++) {
			arrayParams.add(columns.getParameter(i).tojson());
			// Create the empty JSONArray for the data. Will be filled after
			json.addPairs(columns.getParameter(i).getName(), new TJSONArray());
		}

		while (dbxReader.next()) {
			for (int c = 0; c < columns.size(); c++)
				dbxReader
						.getColumns()
						.getParameter(c)
						.getValue()
						.appendTo(
								json.getJSONArray(columns.getParameter(c)
										.getName()));
		}
		json.addPairs("table", arrayParams);

		return json;
	}

	/**
	 * Create a TStream from a JSONArray
	 * 
	 * @param value
	 * @return TStream
	 * @throws JSONException
	 * @throws DBXException
	 */
	public static TStream JSONToStream(TJSONArray value) throws DBXException {
		return TStream.CreateFrom(value);
	}

	/**
	 * Convert a Object to DBXValue
	 * 
	 * @param o
	 * @param value
	 * @param DBXTypeName
	 * @throws DBXException
	 */
	public static void JSONtoDBX(Object o, DBXValue value, String DBXTypeName)
			throws DBXException {
		try {
			if (DBXTypeName.startsWith("TDBX") && DBXTypeName.endsWith("Value")
					&& (o instanceof TJSONNull)) {
				value.GetAsDBXValue().setNull();
			} else {
				if (!((o instanceof TJSONNull) && (DBXTypeName.equals("")))) {
					switch (value.getDBXType()) {
					case DBXDataTypes.Int8Type:
						
						if ((DBXTypeName.equals("TDBXInt8Value"))) {
							value.GetAsDBXValue().SetAsInt8(
									((TJSONNumber) o).getValue().intValue());
						} else
							value.SetAsInt8(((TJSONNumber) o).getValue().intValue());
						break;
					case DBXDataTypes.Int16Type:
						if ((DBXTypeName.equals("TDBXInt16Value"))) {
							value.GetAsDBXValue().SetAsInt16(
									((TJSONNumber) o).getValue().intValue());
						} else
							value.SetAsInt16(
									((TJSONNumber) o).getValue().intValue());
						break;
					case DBXDataTypes.Int32Type:
						if ((DBXTypeName.equals("TDBXInt32Value"))) {
							value.GetAsDBXValue().SetAsInt32(
									((TJSONNumber) o).getValue().intValue());
						} else
							value.SetAsInt32(((TJSONNumber) o).getValue().intValue());
						break;
					case DBXDataTypes.Int64Type:
						if ((DBXTypeName.equals("TDBXInt64Value"))) {

							value.GetAsDBXValue().SetAsInt64(
									((TJSONNumber) o).getValue().longValue());
						} else
							value.SetAsInt64(((TJSONNumber) o).getValue().longValue());
						break;
					case DBXDataTypes.BcdType:
						if ((DBXTypeName.equals("TDBXBcdValue"))) {
							value.GetAsDBXValue().SetAsBcd(
									(DBXDefaultFormatter.getInstance()
											.StringToDouble(o.toString())));
						} else
							value.SetAsBcd((DBXDefaultFormatter.getInstance()
									.StringToDouble(o.toString())));
						break;
					case DBXDataTypes.UInt8Type:
						if ((DBXTypeName.equals("TDBXUInt8Value"))) {
							value.GetAsDBXValue().SetAsUInt8(
									((TJSONNumber) o).getValue().intValue());
						} else
							value.SetAsUInt8(((TJSONNumber) o).getValue().intValue());
						break;
					case DBXDataTypes.UInt16Type:
						if ((DBXTypeName.equals("TDBXUInt16Value"))) {
							value.GetAsDBXValue().SetAsUInt16(
									((TJSONNumber) o).getValue().intValue());
						} else
							value.SetAsUInt16(((TJSONNumber) o).getValue().intValue());
						break;
					case DBXDataTypes.UInt32Type:
						if ((DBXTypeName.equals("TDBXUInt32Value"))) {
							value.GetAsDBXValue().SetAsUInt32(
									((TJSONNumber) o).getValue().intValue());
						} else
							value.SetAsUInt32(((TJSONNumber) o).getValue().intValue());
						break;
					case DBXDataTypes.UInt64Type:
						if ((DBXTypeName.equals("TDBXUInt64Value"))) {
							value.GetAsDBXValue().SetAsUInt64(
									((TJSONNumber) o).getValue().longValue());
						} else
							value.SetAsUInt64(((TJSONNumber) o).getValue().longValue());
						break;
					case DBXDataTypes.DoubleType: {
						if (DBXTypeName.equals("TDBXDoubleValue")) {
							value.GetAsDBXValue().SetAsDouble(
									((TJSONNumber) o).getValue());
						} else
							value.SetAsDouble(((TJSONNumber) o).getValue());
						break;
					}
					case DBXDataTypes.SingleType: {
						if (DBXTypeName.equals("TDBXSingleValue")) {
							value.GetAsDBXValue().SetAsSingle(
									((TJSONNumber) o).getValue().floatValue());
						} else
							value.SetAsSingle(((TJSONNumber) o).getValue().floatValue());
						break;
					}
					case DBXDataTypes.CurrencyType: {
						value.SetAsCurrency(((TJSONNumber) o).getValue());
						break;
					}
					case DBXDataTypes.JsonValueType: {
						value.SetAsJSONValue((TJSONValue) DBXJSONTools
								.JSONToJSONValue(o));
						break;
					}
					case DBXDataTypes.BinaryBlobType: {
						if (DBXTypeName.equals("TDBXStreamValue")) {
							value.GetAsDBXValue().SetAsStream(
									DBXJSONTools.JSONToStream((TJSONArray) o));
						} else {
							value.SetAsStream(DBXJSONTools
									.JSONToStream((TJSONArray) o));
						}
						break;
					}
					case DBXDataTypes.BlobType: {
						value.SetAsBlob(DBXJSONTools
								.JSONToStream((TJSONArray) o));
						break;
					}
					case DBXDataTypes.TimeStampType: {
						Date d = DBXDefaultFormatter.getInstance()
								.StringToDateTime(((TJSONString) o).toString());
						if (d == null)
							throw new DBXException("Invalid date");
						if (DBXTypeName.equals("TDBXTimeStampValue")) {
							value.GetAsDBXValue().SetAsTimeStamp(d);
						} else
							value.SetAsTimeStamp(d);
						break;
					}
					case DBXDataTypes.AnsiStringType: {
						if ((DBXTypeName.equals("TDBXAnsiStringValue"))
								|| (DBXTypeName.equals("TDBXStringValue"))
								|| (DBXTypeName.equals("TDBXAnsiCharsValue"))) {
							value.GetAsDBXValue().SetAsAnsiString(((TJSONString) o).toString());
						} else
							value.SetAsAnsiString(((TJSONString) o).toString());
						break;
					}

					case DBXDataTypes.TableType: {
						if (DBXTypeName.equals("TDBXReaderValue")) {
							value.GetAsDBXValue().SetAsTable(
									(TableType) DBXJSONTools.JSONToTableType(o,
											DBXTypeName));
						} else {
							value.SetAsTable((TableType) DBXJSONTools
									.JSONToTableType(o, DBXTypeName));
						}

						break;
					}

					case DBXDataTypes.WideStringType: {
						if ((DBXTypeName.equals("TDBXAnsiStringValue"))
								|| (DBXTypeName.equals("TDBXStringValue"))
								|| (DBXTypeName.equals("TDBXAnsiCharsValue"))
								|| (DBXTypeName.equals("TDBXWideStringValue"))) {
							value.GetAsDBXValue().SetAsString(((TJSONString) o).toString());
						} else
							value.SetAsString(((TJSONString) o).toString());
						break;
					}

					case DBXDataTypes.TimeType: {
						int t = DBXDefaultFormatter.getInstance()
								.StringToTDBXTime(((TJSONString) o).toString());
						if (DBXTypeName.equals("TDBXTimeValue")) {
							value.GetAsDBXValue().SetAsTDBXTime(t);
						} else
							value.SetAsTDBXTime(t);
						break;
					}

					case DBXDataTypes.DateType: {
						int t = DBXDefaultFormatter.getInstance()
								.StringToTDBXDate(((TJSONString) o).toString());
						if (DBXTypeName.equals("TDBXDateValue")) {
							value.GetAsDBXValue().SetAsTDBXDate(t);
						} else
							value.SetAsTDBXDate(t);
						break;
					}

					case DBXDataTypes.DateTimeType: {
						Date d = DBXDefaultFormatter.getInstance()
								.StringToDateTime(((TJSONString) o).toString());
						if (d == null)
							throw new DBXException("Invalid date");
						value.SetAsDateTime(d);
						break;
					}
					case DBXDataTypes.BooleanType: {
						boolean val = ((Boolean)((TJSONValue) o).getInternalObject()).booleanValue();
						if (DBXTypeName.equals("TDBXBooleanValue")) {
							value.GetAsDBXValue().SetAsBoolean(val);
						} else
							value.SetAsBoolean(val);
						break;
					}

					default:
						throw new DBXException("Cannot convert datatype "
								+ String.valueOf(value.getDBXType()));
					} // switch
				} else {
					value.Clear();
				}
			}
		} catch (Exception ex) {
			throw new DBXException(ex.getMessage());
		}
	}

	/**
	 * Convert an Object to TJSONValue
	 * 
	 * @param o
	 * @return
	 * @throws DBXException
	 */
	private static TJSONValue JSONToJSONValue(Object o) throws DBXException {
		if (o instanceof TJSONNull) // TJSONNull
			return new TJSONNull();
		if (o instanceof TJSONObject) // TJSONObject
			return TJSONObject.Parse(o.toString());
		if (o instanceof TJSONArray) // TJSONArray
			return TJSONArray.Parse(o.toString());// return new
		if (o instanceof TJSONTrue) { // TJSONTrue
			return new TJSONTrue();
		}
		if (o instanceof TJSONFalse) { // TJSONFalse
			return new TJSONFalse();
		}
		throw new DBXException(o.getClass().toString()
				+ " is not a valid JSONValue");
	}
}
