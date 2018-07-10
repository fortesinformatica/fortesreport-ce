//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

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
	 *            The JSONArray that contains the information to create the
	 *            {@link DBXValueType}
	 * @return the DBXValueType created
	 * @throws DBXException
	 */

	public static DBXValueType TJSONToValueType(TJSONArray json)
			throws DBXException {
		DBXValueType vt = new DBXValueType();
		TJSONToValueType(json, vt);
		return vt;
	}

	/**
	 * Sets the passed {@link DBXValueType} with the information contained in
	 * the passed JSONArray.
	 * 
	 * @param json
	 * @param vt
	 * @throws DBXException
	 */
	public static void TJSONToValueType(TJSONArray json, DBXValueType vt)
			throws DBXException {
		vt.setName(json.getString(0));
		vt.setDataType(json.getInt(1).intValue());
		vt.setOrdinal(json.getInt(2).intValue());
		vt.setSubType(json.getInt(3).intValue());
		vt.setScale(json.getInt(4).intValue());
		vt.setSize(json.getInt(5).intValue());
		vt.setPrecision(json.getInt(6).intValue());
		vt.setChildPosition(json.getInt(7).intValue());
		vt.setNullable(json.getBoolean(8).booleanValue());
		vt.setHidden(json.getBoolean(9).booleanValue());
		vt.setParameterDirection(json.getInt(10).intValue());
		vt.setValueParameter(json.getBoolean(11).booleanValue());
		vt.setLiteral(json.getBoolean(12).booleanValue());
	}

	/**
	 * Converts a DBXValueType to a TJSONArray
	 * 
	 * @param DataType
	 * @return
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
			if (DBXTypeName.equals("TDBXReader")
					|| (DBXTypeName.equals("TDBXReaderValue"))) {
				return TDBXReader.createFrom((TJSONObject) value);
			} else if (DBXTypeName.equals("TDataSet")) {
				return TDataSet.CreateFrom((TJSONObject) value);
			}
		}
		throw new DBXException(DBXTypeName + " is not a table type");
	}

	/**
	 * Create a TJSONObject from a DBXParameters
	 * 
	 * @param dbxParameters
	 * @return JSONObject
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
			json.addPairs(dbxParameters.getParameter(i).getName(),
					arrParamsValue);
		}
		return json;
	}

	/**
	 * Returns a new JSONObject initialized to the value represented by the
	 * specified TDBXReader
	 * 
	 * @param dbxReader
	 * @return
	 * @throws DBXException
	 */

	public static TJSONObject DBXReaderToJSONObject(TDBXReader dbxReader)
			throws DBXException {
		TJSONObject json = new TJSONObject();
		TJSONArray arr2;
		TParams columns = dbxReader.getColumns();
		arr2 = new TJSONArray();
		for (int i = 0; i < columns.size(); i++) {
			arr2.add(columns.getParameter(i).tojson());
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
		json.addPairs("table", arr2);

		return json;
	}

	/**
	 * Convert a json in a Stream
	 * 
	 * @param value
	 * @return
	 * @throws DBXException
	 */

	public static TStream TJSONToStream(TJSONArray value) throws DBXException {
		return TStream.CreateFrom(value);
	}

	/**
	 * Insert a jsonObject into a DBXValue converting it to the right value
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
							value.GetAsDBXValue().SetAsInt8(((TJSONNumber) o).getValue().intValue());
						} else
							value.SetAsInt8(((TJSONNumber) o).getValue().intValue());
						break;
					case DBXDataTypes.Int16Type:
						if ((DBXTypeName.equals("TDBXInt16Value"))) {
							value.GetAsDBXValue().SetAsInt16(((TJSONNumber) o).getValue().intValue());
						} else
							value.SetAsInt16(((TJSONNumber) o).getValue().intValue());
						break;
					case DBXDataTypes.Int32Type:
						if ((DBXTypeName.equals("TDBXInt32Value"))) {
							value.GetAsDBXValue().SetAsInt32(((TJSONNumber) o).getValue().intValue());
							;
						} else {
							value.SetAsInt32(((TJSONNumber) o).getValue().intValue());
						}
						break;
					case DBXDataTypes.Int64Type:
						if ((DBXTypeName.equals("TDBXInt64Value"))) {
							value.GetAsDBXValue().SetAsInt64(((TJSONNumber) o).getValue().longValue());
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
							value.GetAsDBXValue().SetAsUInt8(((TJSONNumber) o).getValue().intValue());
						} else
							value.SetAsUInt8(((TJSONNumber) o).getValue().intValue());
						break;
					case DBXDataTypes.UInt16Type:
						if ((DBXTypeName.equals("TDBXUInt16Value"))) {
							value.GetAsDBXValue().SetAsUInt16(((TJSONNumber) o).getValue().intValue());
						} else
							value.SetAsUInt16(((TJSONNumber) o).getValue().intValue());
						break;
					case DBXDataTypes.UInt32Type:
						if ((DBXTypeName.equals("TDBXUInt32Value"))) {
							value.GetAsDBXValue().SetAsUInt32(((TJSONNumber) o).getValue().intValue());
						} else
							value.SetAsUInt32(((TJSONNumber) o).getValue().intValue());
						break;
					case DBXDataTypes.UInt64Type:
						if ((DBXTypeName.equals("TDBXUInt64Value"))) {
							value.GetAsDBXValue().SetAsUInt64(((TJSONNumber) o).getValue().longValue());
						} else
							value.SetAsUInt64(((TJSONNumber) o).getValue().longValue());
						break;
					case DBXDataTypes.DoubleType: {
						if (DBXTypeName.equals("TDBXDoubleValue")) {
							value.GetAsDBXValue().SetAsDouble(((TJSONNumber) o).getValue().doubleValue());
						} else
							value.SetAsDouble(((TJSONNumber) o).getValue().doubleValue());
						break;
					}
					case DBXDataTypes.SingleType: {
						if (DBXTypeName.equals("TDBXSingleValue")) {
							value.GetAsDBXValue().SetAsSingle(((TJSONNumber) o).getValue().floatValue());
						} else
							value.SetAsSingle(((TJSONNumber) o).getValue().floatValue());
						break;
					}
					case DBXDataTypes.CurrencyType: {
						value.SetAsCurrency(((TJSONNumber) o).getValue().doubleValue());
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
									DBXJSONTools.TJSONToStream(TJSONArray
											.Parse(o.toString())));
						} else {
							value.SetAsStream(DBXJSONTools
									.TJSONToStream(TJSONArray.Parse(o
											.toString())));
						}
						break;
					}
					case DBXDataTypes.BlobType: {
						value.SetAsBlob(DBXJSONTools.TJSONToStream(TJSONArray
								.Parse(o.toString())));
						break;
					}
					case DBXDataTypes.TimeStampType: {
						Date d = DBXDefaultFormatter.getInstance()
								.StringToDateTime(o.toString());
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
							value.GetAsDBXValue().SetAsAnsiString(o.toString());
						} else
							value.SetAsAnsiString(o.toString());
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
							value.GetAsDBXValue().SetAsString(o.toString());
						} else
							value.SetAsString(o.toString());
						break;
					}

					case DBXDataTypes.TimeType: {
						int t = DBXDefaultFormatter.getInstance()
								.StringToTDBXTime(o.toString());
						if (DBXTypeName.equals("TDBXTimeValue")) {
							value.GetAsDBXValue().SetAsTDBXTime(t);
						} else
							value.SetAsTDBXTime(t);
						break;
					}

					case DBXDataTypes.DateType: {
						int t = DBXDefaultFormatter.getInstance()
								.StringToTDBXDate(o.toString());
						if (DBXTypeName.equals("TDBXDateValue")) {
							value.GetAsDBXValue().SetAsTDBXDate(t);
						} else
							value.SetAsTDBXDate(t);
						break;
					}

					case DBXDataTypes.DateTimeType: {
						Date d = DBXDefaultFormatter.getInstance()
								.StringToDateTime(o.toString());
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
				} else
					value.Clear();
			}
		} catch (Exception ex) {
			ex.printStackTrace();
			throw new DBXException(ex.getMessage());
		}
	}

	/**
	 * Returns a new TJSONValue initialized to the value represented by the
	 * specified Object
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
			return TJSONArray.Parse(o.toString());
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
