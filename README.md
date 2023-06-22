# trading-research
# Capstone project for WGU MSDA program.

**Capstone Project Name:** Artificial Neural Network For Predicting Stock Market Returns

**Project Topic:** Artificial Neural Network For Stock Market Returns

**This project does not involve human subjects research and is exempt from WGU IRB review.**

**Research Question:** Can an Artificial Neural Network (ANN) model increase stock market returns?

**Hypothesis:** 
  * H0: ANN model cannot improve stock market returns.
  * Ha: An ANN model can improve stock market returns.

**Context:** This study's contribution to the field of Data Analytics and the MSDA program is to use R to create a predictive ANN model. The Predictive ANN model will be used to estimate the returns of the Nasdaq Composite. The model will be used to determine if a single Nasdaq Composite index is worth purchasing. Given the price of the index, the model maximizes profit over time through three trading actions (Buy, Hold, Sell). This study is based on a case study provided by Luis Targo (Targo 2010). The predictive model will be used in a trading system to make decisions based on the model's information. Targo's case study evaluates the trading system results after applying the ANN model (profit or loss).  The evaluation is not the accuracy of the ANN model. Nasdaq composite daily data is freely available by Yahoo Finance.

Data: The Nasdaq Composite data is time series based. The raw dataset contains records contains 12,619 records. The data set is made available through Yahoo Finance.  The data set includes the date the market was open, the value of the index at the opening, the highest value of that day, the lowest value of that day, Adjusted Close, and the Volume representing the total number of transactions. The predictor variables are broken down as follows:
https://finance.yahoo.com/quote/%5EIXIC/history?p=%5EIXIC 

Field	Type
Date	Continuous
Open	Continuous
High	Continuous
Low	Continuous
Close	Continuous
Adjusted Close	Continuous
Volume	Continuous
<table>
    <thead>
        <tr>
            <td>Field</td>
            <td>Type</td>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Date</td>
            <td>Continuous</td>
        </tr>
        <tr>
            <td>Open</td>
            <td>Continuous</td>
        </tr>
        <tr>
            <td>High</td>
            <td>Continuous</td>
        </tr>
        <tr>
            <td>Low</td>
            <td>Continuous</td>
        </tr>
        <tr>
            <td>Close</td>
            <td>Continuous</td>
        </tr>
        <tr>
            <td>Adjusted Close</td>
            <td>Continuous</td>
        </tr>
        <tr>
            <td>Volume</td>
            <td>Continuous</td>
        </tr>
    </tbody>
</table>

The data is made public by Yahoo Finance. The accuracy and completeness of information limit the data set made publicly available by Yahoo Finance. The Nasdaq Composite launched in 1971; however, Yahoo finance does not contain information on Volume before 10/10/1984. Technical indicators needed to create the model require Volume data. Records will range from 10/10/1984 to 02/18/2021. To prevent a bias model, records with missing Volume data will not be used (Fischettin 2017).

**Data Gathering:** A CSV file will be downloaded from Yahoo Finance. Any missing data not be used in the creation of the model. R package xts is suited for handling time-dependent data. The sparsity of data is 27.4%. At 12,619 records, that leaves about 9,100 usable records.

**Data Analytics Tools and Techniques:** The Design to determine if ANN can predict stock market performance based on economic returns: 
1.	Define an indicator "T" that will summarize the price time series of the next couple of days. Select a set of technical indicators from the TTR package to define the value of "T".
2.	Use random forest to score the order of importance of the technical indicators to determine the value of "T".
3.	Set criteria for T that will signal to buy, sell, or hold using the error rate of the trading signals.
4.	Split the Nasdaq Composite data into a training, validation and testing group.
5.	Create the model using Artificial Neural Networks.
6.	Set the criteria for evaluating the trade-related economic performance.
7.	Run the ANN model through a trading simulator and analyze the results.
8.	Run the ANN model through a trading simulator using the valuation data and analyze the results.
9.	Determine if ANN can improve stock market returns.

Justification of Tools/Techniques: The case study provided by Luis Targo uses R to generate the model. R is free and uses many packages to create plots, such as quantmod to generate candlestick charts for technical analysis. ggplot package provides lots of customizable visualizations. R also has PerformanceAnalytics package for financial metric analysis of trading algorithms. (Larose 2015) R is also a free software environment. 

Project Outcomes: This project determines if ANN can be used to predict stock market returns. The ANN model can help develop a trading strategy to maximize returns. An ANN model might be able to project a margin of returns on a specific stock, but the margin will not be significantly different than that of a non-ANN model. The speed at which the market processes information makes it impossible to achieve greater returns on a specific stock over a diverse portfolio, this concept is known as “Efficient Market Hypothesis”. (Williams 2017)

Projected Project End Date: 2/28/2021
