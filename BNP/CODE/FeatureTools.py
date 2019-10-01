#!/usr/bin/env python
# coding: utf-8

# In[3]:


import pandas as pd
import featuretools as ft
df = pd.DataFrame({'index': [1, 2, 3, 4, 5],
                    'location': ['main street',
                                 'main street',
                                 'main street',
                                 'arlington ave.',
                                 'arlington ave.'],
                    'pies sold': [3, 4, 5, 18, 1]})

df['date'] = pd.date_range('12/29/2017', periods=5, freq='D')
df


# In[4]:


es = ft.EntitySet('Transactions')

es.entity_from_dataframe(dataframe=df,
                         entity_id='log',
                         index='index')
es


# In[33]:


fm, features = ft.dfs(entityset=es, 
                      target_entity='log',
                      trans_primitives=['day', 'weekday', 'month'])

print((features))

fm


# In[34]:


#normalize your single entity by an interesting categorical. 

es.normalize_entity(base_entity_id='log',
                    new_entity_id='locations',
                    index='location')
print(es)

fm, features = ft.dfs(entityset=es, target_entity='log')
fm


# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[35]:


#using time index

df = pd.DataFrame({'index': [1, 2, 3, 4, 5],
                    'location': ['main street',
                                 'main street',
                                 'main street',
                                 'arlington ave.',
                                 'arlington ave.'],
                    'pies sold': [3, 4, 5, 18, 1]})

df['date'] = pd.date_range('12/29/2017', periods=5, freq='D')
es = ft.EntitySet('Transactions')

es.entity_from_dataframe(dataframe=df, entity_id='log', index='index', time_index='date')
es.normalize_entity(base_entity_id='log', new_entity_id='locations', index='location')
es


# In[36]:


fm, features = ft.dfs(entityset=es, target_entity='log')
fm


# In[ ]:





# In[ ]:





# In[37]:


ft.list_primitives()


# In[ ]:




####################################################################################
####################################################################################

#!/usr/bin/env python
# coding: utf-8

# In[ ]:


#Create two seperate dataframes
#dataframe_1


# In[1]:


import featuretools as ft
data = ft.demo.load_mock_customer()
transactions_df = data["transactions"].merge(data["sessions"]).merge(data["customers"])
transactions_df.head()


# In[ ]:


#dataframe_2


# In[2]:


products_df = data["products"]
products_df


# In[3]:


#Step - 1

#Create EntitySet

es = ft.EntitySet(id="customer_data")
es = es.entity_from_dataframe(entity_id="transactions",
                              dataframe=transactions_df,
                              index="transaction_id",
                              time_index="transaction_time",
                              variable_types={"product_id": ft.variable_types.Categorical,
                                              "zip_code": ft.variable_types.ZIPCode})
es


# In[5]:


es = es.entity_from_dataframe(entity_id="products",
                              dataframe=products_df,
                              index="product_id")
es


# In[ ]:





# In[6]:


#Adding a Relationship

#1. Each product has multiple transactions associated with it (Parent Entity)
#2. Transaction entity (Child Entity)
#3. When specifying relationships , list variables in Parent entity first
#4. ft.Relationship should denote one-to-many not one-to-one or many-many

new_relationship = ft.Relationship(es["products"]["product_id"],
                                   es["transactions"]["product_id"])

es = es.add_relationship(new_relationship)

es


# In[7]:


#Normalize Entity

es = es.normalize_entity(base_entity_id="transactions",
                         new_entity_id="sessions",
                         index="session_id",
                         make_time_index="session_start",
                         additional_variables=["device", "customer_id", "zip_code", "session_start", "join_date"])

es


# In[12]:


es['transactions'].df.head()


# In[13]:


es['products'].df


# In[17]:


es['sessions'].df.head()


# In[ ]:





# In[16]:


es = es.normalize_entity(base_entity_id="sessions",
                         new_entity_id="customers",
                         index="customer_id",
                         make_time_index="join_date",
                         additional_variables=["zip_code", "join_date"])
    
es


# In[ ]:





# In[ ]:


#Target Entity - Products


# In[18]:


feature_matrix, feature_defs = ft.dfs(entityset=es, target_entity="products")


# In[20]:


feature_matrix.shape


# In[19]:


feature_matrix


# In[ ]:





# In[ ]:


#Target Entity - Sessions


# In[21]:


feature_matrix, feature_defs = ft.dfs(entityset=es,
                                      target_entity="sessions",
                                      agg_primitives=["mean", "sum", "mode"],
                                      trans_primitives=["month", "hour"],
                                      max_depth=2)


# In[22]:


feature_matrix.shape


# In[ ]:




