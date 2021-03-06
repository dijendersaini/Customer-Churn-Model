Company Information:
A telecom company called ‘Firm X’ is a leading telecommunications provider in the country. The company earns most of its revenue by providing internet services. Based on the past and current customer information, the company has maintained a database containing personal/demographic information, the services availed by a customer and the expense information related to each customer.
 
Problem Statement:
You are working for the telecom company ‘Firm X’. It has a customer base set across the country. In a city ‘Y’, which is a significant revenue base for the company, due to heavy marketing and promotion schemes by other companies, your company is losing customers i.e. the customers are churning. Whether a customer will churn or not will depend on data from the following three buckets:
Demographic Information
Services Availed by the customer
Overall Expenses
 
The data are provided in three separate data files given at the end of the page. The aim is to automate the process of predicting if a customer would churn or not and to find the factors affecting the churn. 

The goal of this case study: 
You are required to develop predictive models using each of the 4 models namely K-NN, Naive Bayes, Logistic Regression and SVM.

Note: Wherever required, set the VIF threshold to 2.  

Data dictionary:

S.No.	Variable Name 	Meaning

1.	CustomerID: 	The unique ID of each customer

2.	Gender: 	The gender of a person

3.	SeniorCitizen:	Whether a customer can be classified as a senior citizen.

4.	Partner: 	If a customer is married/ in a live-in relationship.

5.	Dependents:	If a customer has dependents (children/ retired parents)

6.	Tenure:	The time for which a customer has been using the service.

7.	PhoneService:  	Whether a customer has a landline phone service along with the internet service.

8.	MultipleLines:	Whether a customer has multiple lines of internet connectivity.

9.	InternetService: 	The type of internet services chosen by the customer.

10.	OnlineSecurity:	Specifies if a customer has online security.

11.	OnlineBackup: 	Specifies if a customer has online backup.

12.	DeviceProtection: 	Specifies if a customer has opted for device protection.

13.	TechSupport: 	Whether a customer has opted for tech support of not.

14.	StreamingTV:	Whether a customer has an option of TV streaming.

15.	StreamingMovies: 	Whether a customer has an option of Movie streaming.

16.	Contract:  	The type of contract a customer has chosen.

17.	PaperlessBilling:  	Whether a customer has opted for paperless billing.

18.	PaymentMethod: 	Specifies the method by which bills are paid.

19.	MonthlyCharges: 	Specifies the money paid by a customer each month.

20.	TotalCharges: 	The total money paid by the customer to the company.

21.	Churn: 	This is the target variable which specifies if a customer has churned or not.


