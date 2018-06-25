# create function to compute logloss on validation set using ground truth----------------------------------

checkLogLoss <- function(model, data) {
  
  # LogLoss Function

  LogLoss <- function(actual, predicted, eps=0.00001) {

        predicted <- pmin(pmax(predicted, eps), 1-eps)
    
        -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
  
        }
  
  # create dummy predictions and compare with fitted model
  
    pred <- as.matrix(predict(model, newdata = data, type = 'prob'))
  
    LogLoss(truth, pred)

    }


##################################################################

# In[ ]:


f, ax = plt.subplots(1, 2 , figsize=(18,8))

train_df['Survived'].value_counts().plot.pie(explode=[0, 0.1], 
                                             autopct = '%1.1f%%',
                                            ax=ax[0],
                                            shadow=True)
ax[0].set_title('Survived')
ax[0].set_ylabel('')
sns.countplot('Survived', 
              data=train_df,
             ax=ax[1])
ax[1].set_title('Survived')
plt.show()


# In[ ]:


#categorical - Nominal
#Ordinal
#continous


# In[ ]:


train_df.groupby(['Sex', 'Survived'])['Survived'].count()


# In[ ]:


f, ax = plt.subplots(1, 2, figsize=(18,8))
train_df[['Sex', 'Survived']].groupby(['Sex']).mean().plot.bar(ax=ax[0])
ax[0].set_title('Survived vs Sex')
sns.countplot('Sex', hue='Survived', data=train_df, ax=ax[1])
ax[1].set_title('Sex:Survived vs Dead')
plt.show()


# > Female, Kids were first preferance 
# 
# > Age of females is less 
# 
# 
# age - kids - how many survived - 
# 
# female parents - how many survived - 
# 
# there's similar survival numbers in kids and parents  - then some pattern is present - add this as a new col

# ## Extend this to other columns - 

# In[ ]:


pd.crosstab([train_df.Sex, train_df.Survived], train_df.Pclass, margins=True).style.background_gradient(cmap='summer_r')


# In[ ]:


sns.factorplot('Pclass', 'Survived', hue='Sex', data=train_df)
plt.show()

#draw a seperate plot for each pclass 

#age - xaxis
#survived % - yaxis


# In[ ]:



> check why 3 females in pclass 1 did not survive

> possible reasons why females survived more in pclass 3 and males in pclass 1 (reason - age group)

> parents ratio in female and males in different Pclass 


# In[ ]:


train_df[['Pclass', 'Survived']].groupby(['Pclass'], as_index=False).mean().sort_values(by='Survived', ascending=False)


# In[ ]:


f,ax=plt.subplots(1,2,figsize=(18,8))
sns.violinplot("Pclass","Age", hue="Survived", data=train_df,split=True,ax=ax[0])
ax[0].set_title('Pclass and Age vs Survived')
ax[0].set_yticks(range(0,110,10))
sns.violinplot("Sex","Age", hue="Survived", data=train_df,split=True,ax=ax[1])
ax[1].set_title('Sex and Age vs Survived')
ax[1].set_yticks(range(0,110,10))
plt.show()


# In[ ]:


Reasons why certain age group have high survival rates


# In[ ]:


train_df[['SibSp', 'Survived']].groupby(['SibSp'], as_index=False).mean().sort_values(by='Survived', ascending=False)


# In[ ]:


#corelating numeric features
#corelating numerical and ordinal feature
#corelating b/w categorical


# In[ ]:


#corelating numeric features

g=sns.FacetGrid(train_df, col='Survived')
g.map(plt.hist, 'Age', bins=20)


# In[ ]:


#create segments of 5 of Age


# In[ ]:


#corelating numerical and ordinal feature

grid=sns.FacetGrid(train_df, col='Survived', row='Pclass', size=2.2, aspect=1.6)
grid.map(plt.hist, 'Age', alpha=0.5, bins=20)
grid.add_legend()


# In[ ]:


#corelating b/w categorical

grid=sns.FacetGrid(train_df, row='Embarked', size=2.2, aspect=1.6)
grid.map(sns.pointplot, 'Pclass', 'Survived', 'Sex', palette = 'deep')
grid.add_legend()


# In[ ]:


possible reasons why males from 'C' have almost 100% survival rate


