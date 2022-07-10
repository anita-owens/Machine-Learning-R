# Machine-Learning

## :dart: Machine Learning in R Introduction

#machine-learning

This repo contains projects related to marketing and business analytics problems using the R Programming language. Machine Learning can significantly improve marketing performance.

## **PROJECT 1:** 📊 [MARKETING CAMPAIGN OPTIMIZATION - Can we identify the right customers to target?](https://github.com/bonheurgirl/Machine-Learning-R/blob/master/1.Predict%20Bank%20Mktg%20Campaign%20Response-Logistic%20Regression%20%26%20Decision%20Tree%20Models.R)

The goal of the first project is to improve campaign marketing response rate for the bank. Marketing department needs to understand what is important. How can we allocate resources to those customers who are more likely to respond. This is a classic binary classification problem. We have customers who converted or did not convert.

With this project, I predict marketing response rates through logistic regression and decision trees. With the output of this model, marketing gets a profile of customers who are similar to those who successfully converted from past marketing campaigns.

Here is the profile I would send to marketing:

    Marketing should contact customers with these characteristics:
      1. marital status - single
      2. education -  tertiary
      3. had responded to a previous campaign and it was a positive outcome
      4. has account balance
      5. is retired or a student

    Marketing should not contact customers with these characteristics
      1. has a personal loan
      2. has mortgage loan
      
      
      KEY INSIGHT: The bank should limit the number of contacts it has with a customer. What's most likely                  happening is that customers may ignore communication from the bank if a customers gets too many. The bank should stop over-exposing customers to it's marketing communication.

------------------------------------------------------------------------

## **PROJECT 2:** :crystal_ball: [MARKETING CAMPAIGN OPTIMIZATION - What do customers want?](https://github.com/bonheurgirl/Machine-Learning-R/blob/master/2.Predict%20Magazine%20Subscription%20Behavior%20-%20Logistic%20Regression.R)

We have a dataset of subscribers who subscribed to a magazine or not. Can we predict which customers are more likely to subscribe? The goal is to determine which customers are most likely to respond positively to our offer. This is a simple dataset with few variables and is a good dataset to practice regression modeling. By using machine learning to identify quality customer characteristics, we can save costs and improve customer acquisition with increased ROI.

    Recommendations to marketing:

    1. We can safely say that the younger the customer (less than 33 years old),
    the more likely to subscribe (median age == 40).
    2. Women are more likely to subscribe then men. Let's take a look at the creatives to figure out why. Perhaps there is some cost savings by excluding older segments from marketing efforts in order to gain more subscribers.

------------------------------------------------------------------------

## **PROJECT 3:** :test_tube: [A/B TEST ANALYSIS - Was our promotion experiment successful?](https://github.com/bonheurgirl/Machine-Learning-R/blob/master/3B.IBM%20Watson%20Marketing%20AB%20Test%20Results%20Evaluation%20-%20ANOVA%20and%20post-hoc%20tests.Rmd)

The goal of project three is to analyze the results of an experiment. A chain of department stores that ran 3 promotions in different markets. The marketing team wants to know which promotion was the most effective in generating sales. The deliverable is to inform the marketing team on the results of the impact of the experiment.

    File structure:
    1.  File 3A is an R script with an ANOVA analysis of the experiment results.
    2.  File 3B is a Markdown document with an ANOVA analysis of the experiment results plus all post-hoc tests.

    Recommendations to marketing and sales team: 
    - Let's run again with just promotion 1 & 3 to see if we can get a significant result. The test should not take as long to run as we only have 2 groups to compare so we could see significant results quite fast. If this is not feasible, then we can inform marketing of the directional result of the experiment so that they can improve planning for the next promotion (e.g. a focus only on 1 or 2 promotions with a proper control group for comparison).

------------------------------------------------------------------------

## **PROJECT 4:** :🛒 🛍️ [PREDICT ONLINE SHOPPING BEHAVIOR - Who is most likely to convert?](https://github.com/bonheurgirl/Machine-Learning-R/blob/master/4.Predict%20Online%20Shoppers%20Intent%20-%20Logistic%20Regression.R)

The goal is to identify what characteristics makes a converting customer? What online shopping behaviors exist so we can improve marketing (targeting and customer acquisition) and product (how can we improve our offerings or the customer experience?)

      This analysis suggests that:
       - Visitor type is important (returning and new visitor segments)
       - The different pages a user visits is important and where they exit from the site
       - And whether they visited a product related page

      What's not important?
       - Special day doesn't appear to make much of a difference
       - We presumed weekend makes a difference (customers do more shopping on the weekend), but it doesn't.

      What can we do with this information? In other words, what do we tell product & sales teams?
      - Focus on optimizing product pages. Product pages usually have enough volume to do A/B testing. Here we can test product photos, copy, and USP's etc.)
      - Email marketing is important here. When we send emails to existing customers we should make sure the links point to the product page of interest. The email needs to keep its promise and the customer should land on the right product page.

------------------------------------------------------------------------

## **PROJECT 5:** :computer: [FACEBOOK PERFORMANCE - How do we engage with potential customers on Facebook?](https://github.com/bonheurgirl/Machine-Learning-R/blob/master/5.Facebook%20Performance%20-%20Clustering.R)

The project task is to analyze posts published to the Facebook page of a renowned cosmetics brand. The goal is to identify which posts led to the most customer engagement. Here, I use multiple cluster algorithms (k-means, hierarchical and Mclust) to gather insights into post performance.

This type of analysis can be used to answer the following questions:

1.  Are there obvious differences in group means? Hypothesis testing to help us understand the segments.

2.  Are the groups differentiated enough to tell us a story?

3.  Do we see immediately odd results (e.g. a mean equal to the value of one data level?)

Creating different tactics for different customer segments usually improves overall marketing ROI.

    The results of the data analysis and statistical modeling indicates that:
      1. Photos: Posts with photos offered the best ROI in terms of reach. It's an unpaid segment and indicates that posting photos goes a long way in engaging users with our Facebook page. There is no difference in page_likes between paid or unpaid segments
      2. Day of Week: Another important insight is posting on the right day of the week to optimize engagement goals. Page likes are consistent across days of week with more variability on days 5 & 6.
      3. Videos: Videos and status gets better page likes than links and photos.

------------------------------------------------------------------------

## **PROJECT 6:** :money_with_wings: [CUSTOMER CHURN - Why do customers churn and what can we do about it?](https://github.com/bonheurgirl/Machine-Learning-R/blob/master/6.Customer%20Churn%20Analysis%20-%20Logistic%20Regression%20%26%20Random%20Forest%20Models.Rmd)

**Churn definition: A measure of when a customer stops being a customer.**

It costs more money to acquire new customers than it is to keep the customers you already have. Successful companies know this and do what is needed to make sure new customer acquisition exceeds churn. A robust retention program is what it takes.

My personal favorite project and a common business task is to identify what factors lead customers to churn and how can we use this knowledge to improve customer retention.

With this project, I predict customer churn rates and identify major churn factors through logistic regression and random forest modeling. With the output of these models, I was able to identify core insights for business stakeholders.

    What causes churn?
         1. Most significant factor related to churn is the use of our key product (fiber optic).
          - Fiber optic service is a pain point for customers, but why? Difficult to set up? Quality
            concerns? Can’t stream Netflix? etc. Data suggests there is a significant problem with the                   service.
         2. Contract type is 2nd most significant factor leading to churn. 
          - This is a no-brainer. If you lock customers into a contract, they can’t churn.

    Economic impact of a bad product:     
      - The analysis revealed $2 million USD in lost revenue due to churned customers.

![\$2 4 million in lost revenue due to Fiber Optic internet service](https://user-images.githubusercontent.com/13710429/120935138-9c883900-c701-11eb-99f8-ac6c15bdd175.png)

    Next steps: Now that we know which factors lead to churn, we can take steps to reduce the lost revenue. For example, we can integrate the machine learning model with our marketing applications or customer support applications.

------------------------------------------------------------------------

## **PROJECT 7:** 💵: [Customer Lifetime Value - How much is each customer worth?](https://github.com/bonheurgirl/Machine-Learning-R/blob/master/7.%20Customer%20Lifetime%20Value%20Modeling%20with%20OLS%20and%20Bayesian%20Linear%20Regression.Rmd)

This project outlines how to use R to calculate customer lifetime value. In other words, how much is each customer worth?

| Statistical methods used:                     |
|-----------------------------------------------|
| Ordinary least squares (OLS) regression model |
| Bayesian linear regression                    |

------------------------------------------------------------------------

## **PROJECT 8:** 💰[Sales Forecast Model - Linear Regression](https://github.com/bonheurgirl/Machine-Learning-R/blob/master/8.Sales%20Forecast%20Model%20-%20Linear%20Regression.Rmd)

This project outlines how to create a forecast model based on sales data as published on Medium.

Medium article: [How to build a simple sales forecast model in R](https://medium.com/geekculture/how-to-build-a-sales-forecast-in-r-dafabb03a076)
