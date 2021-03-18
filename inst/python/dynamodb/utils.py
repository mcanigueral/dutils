
# reticulate::use_python('/usr/bin/python3', required = T) # Restart R session to change the python env
from decimal import Decimal
from boto3.dynamodb.conditions import Key, Attr
from pandas import DataFrame


def query_table(dynamo_table, partition_key_name, partition_key_values, sort_key_name = None, sort_key_start = None, sort_key_end = None):
    
    if isinstance(partition_key_values, str): # If only one key value we put it in a list to iterate in the for loop
        partition_key_values = [partition_key_values]
  
    items = []
    if sort_key_name is not None:
        for value in partition_key_values:
            resp = dynamo_table.query(
                KeyConditionExpression = Key(partition_key_name).eq(value) & Key(sort_key_name).between(Decimal(sort_key_start), Decimal(sort_key_end))
            ) 
            items += resp["Items"] 
    else:
        for value in partition_key_values:
            resp = dynamo_table.query(
                KeyConditionExpression = Key(partition_key_name).eq(value)
            ) 
            items += resp["Items"] 
            
    if (len(items) == 0): return None
    
    items_df = DataFrame(items)
    items_df[partition_key_name] = [str(i) for i in items_df[partition_key_name]]
    if sort_key_name is not None:
        items_df[sort_key_name] = [Decimal(i) for i in items_df[sort_key_name]]
        
    return items_df
    
    
def scan_table(dynamo_table, attribute_name, attribute_start, attribute_end):
    resp = dynamo_table.scan(
        FilterExpression = Attr(attribute_name).between(Decimal(attribute_start), Decimal(attribute_end))
    )
    items = resp["Items"]
    
    if (len(items) == 0): return None
    
    items_df = DataFrame(items)
    return items_df


def query_data_table(table, user_id, start, end):
    items = table.query(KeyConditionExpression = Key("id").eq(user_id) & Key('timestamp').between(Decimal(start), Decimal(end)))['Items']
    if (len(items) == 0): return None
    items_df = DataFrame(items)
    if ('data' not in items_df): return None
    items_df['id'] = [str(i) for i in items_df['id']]
    items_df['timestamp'] = [int(i) for i in items_df['timestamp']]
    if items_df['data'].isnull().values.any():
        items_df = items_df.fillna(method='ffill')
    items_df['data'] = [dict(i) for i in items_df['data']]
    return items_df


def put_df(table, df):
    with table.batch_writer() as batch:
        for i in range(df.shape[0]):
            batch.put_item(
                Item={
                    'id': str(df['id'][i]),
                    'timestamp': int(df['timestamp'][i]),
                    'data': dict(df['data'][i])
                }
            )
        
        
def delete_df(table, df):
    with table.batch_writer() as batch:
        for i in range(df.shape[0]):
            batch.delete_item(
                Key={
                    'id': str(df['id'][i]),
                    'timestamp': int(df['timestamp'][i])
                }
            )
        
        
        
