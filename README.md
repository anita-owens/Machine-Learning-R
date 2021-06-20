# Machine-Learning

This repo contains projects related to marketing and business analytics problems using the R Programming language.

PROJECT 1: MARKETING CAMPAIGN OPTIMIZATION - Can we identify the right customers to target?

The goal of the first project is to improve campaign marketing response rate for the bank.  Marketing department needs to understand what is important. How can we allocate resources to those customers who are more likely to respond. This is a classic binary classification problem. We have customers who converted or did not convert. With this project I predict marketing response rates through logistic regression and decision trees. With the output of this data, marketing gets a profile of customers who are similar to those who successfully converted from past marketing campaigns. Here is the profile I would send to marketing.

    Marketing should contact customers with these characteristics:
      1. marital status - single
      2. education -  tertiary
      3. had responded to a previous campaign and it was a positive outcome
      4. has account balance
      5. is retired or a student

    Marketing should not contact customers with these characteristics
      1. has a personal loan
      2. has mortgage loan
      
      
      KEY INSIGHT: The bank should limit the number of contacts it has with  a customer. What's most likely                      happening is that customers may ignore communication from the bank if a customers gets too many.


PROJECT 2: MARKETING CAMPAIGN OPTIMIZATION - What do customers want?

We have a dataset of subscribers who subscribed to a magazine or not. Can we predict which customers are more likely to subscribe? The goal of project two is to determine which customers are most likely to respond positively to our offer. This is a simple dataset with few variables and is a good dataset to practice modeling.

    Recommendations to marketing:
    
    1. We can safely say that the younger the customer (less than 33 years old),
    the more likely to subscribe (median age == 40).
    2. Women are more likely to subscribe then men. Let's take a look at the creatives to figure out why.          Perhaps there is some cost savings by excluding older segments from marketing efforts in order to gain       more subscribers.


PROJECT 3: A/B TEST ANALYSIS - Was our promotion experiment successful?

The goal of project three is to analyze the results of an experiment. A chain of department stores that ran 3 promotions in different markets. The marketing team wants to know which promotion was the most effective in generating sales. The deliverable is to inform the marketing team on the results of the impact of the experiment.

    File structure:
    1.  File 3A is an R script with an ANOVA analysis of the experiment results.
    2.  File 3B is a Markdown document with an ANOVA analysis of the experiment results plus all post-hoc            tests.
    
    Recommendations to marketing and sales team: 
    - Let's run again with just promotion 1 & 3 to see if we can get a significant result. The test should         not take as long to run as we only have 2 groups to compare so we could see significant results quite        fast.


PROJECT 4: PREDICT ONLINE SHOPPING BEHAVIOR - Who is most likely to convert?

The goal is to identify what characteristics makes a converting customer? What online shopping behaviors exist so we can improve marketing (targeting and customer acquisition) and product (how can we improve our offerings or the customer experience?)

      This analysis suggests that:
       - Visitor type is important (returnings and new visitor segments)
       - The different pages a user visits is important and where they exit from the site
       - And whether they visited a product related page

      What's not important?
       - Special day doesn't appear to make much of a difference
       - We presumed weekend makes a difference, but it doesn't.

      What can we do with this information? In other words, what do we tell marketing and product teams?
      - Focus on optimizing product pages

PROJECT 5: FACEBOOK PERFORMANCE - How do we engage with potential customers on Facebook?

The project task is to analyze posts published to the Facebook page of a renowned cosmetics brand. The goal is to identify which posts led to the most customer engagement. Here, I use multiple cluster algorithms to gather insights into post performance.

    The results of the data analysis and modeling indicates that:
      1. Photos: Posts with photos offered the best ROI in terms of reach. It's an unpaid segment and                 indicates that posting photos goes a long way in engaging users with our Facebook page.
      2. Day of Week: Another important insight is posting on the right day of the week to optimize                   engagement goals.


PROJECT 6: CUSTOMER CHURN - Why do customers churn and what can we do about it?

My personal favorite and a common business task is to identify what factors lead customers to churn and how can we use this data to improve customer retention. This project identified core insights for business stakeholders.

    What causes churn?
         1. Most significant factor related to churn is one of our product key products.
          - Fiber optic service is a pain point for customers, but why? Difficult to set up? Quality
            concerns? Can’t stream Netflix? etc. Data suggests there is a significant problem with the                   service.
         2. Contract type is 2nd most significant factor leading to churn. 
          - This is a no-brainer. If you lock customers into a contract, they can’t churn.

    Economic impact of a bad product:     
      - The analysis revealed $2 million USD in lost revenue due to churned customers.

![$2 4 million in lost revenue due to Fiber Optic internet service](https://user-images.githubusercontent.com/13710429/120935138-9c883900-c701-11eb-99f8-ac6c15bdd175.png)
