
from decimal import Decimal
from boto3.dynamodb.conditions import Key, Attr
import pandas as pd

def query_table(table, user_id, start, end):
    items = table.query(KeyConditionExpression = Key("id").eq(user_id) & Key('timestamp').between(Decimal(start), Decimal(end)))['Items']
    if (len(items) == 0): return None
    items_df = pd.DataFrame(items)
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
        
        
        
