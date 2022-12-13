Generator for Global Bike sample data
=====================================

# Intention

## Previous work
[1] The generator is based on the ideas for the dataset created in 2016 by
- Kristof Schneider (SAP)
- Stefan Weidner (SAP UCC Magdeburg)
- Ksenia Neumann (SAP UCC Magdeburg)
- Nitin Kale (University of Southern California)
- Michael Bliemel (Dalhousie University, Halifax, Canada)
- Klaus Freyburger (Hochschule Ludwigshafen)
- Tobias Hagen (Hochschule Offenburg)

This resulted in the `GBI_AnalyticsData.xlsx` dataset covering the years 2007-2016, with appr. 130,000 sales orders.
Various effects were incorporated as described in `GBI_Analytics model.pdf` (published in the course "Intro_BWonHANA_using_Global_Bike_v3.0_Rev001" available at the UCC website for members).

[2] Klaus Freyburger implemented a Python Pandas notebook `AnalyticModelV03.ipynb` with additional effects to be able to carry forward the data into subsequent years.

## Goal
We want to create sample data for SD sales orders in context of the Global Bike firm (GB) of a S/4HANA system provided by SAP University Competence Center (UCC). Instead of using a static dataset as a file for analytics, we like to import the data into a S/4HANA system and create proper sales orders. This will allow to do analytics from various DWH tools connecting to a "life" ERP source system. To be even more realistic, data will be imported on a daily basis, such that delta scenarios (only data up to the current date are available) become possible.

# Requirements
## Data characteristics
Based on the characteristics used in [1], the following characteristics will be implemented. Adoptions were necessary due to: 
-  Data will be imported to a S/4HANA system, which leads to different attributes (e.g. cost and revenue are computed automatically by the ERP system).
- We use existing customer and material master data of the Global Bike sample firm.
- We also want to be able to create data for the future, which makes it difficult to have very special effects like the Lehman crisis implemented.

| #  | Characteristic | Description
|----|----------------|------------
| C01 | Seasonality    | ➔ bicycles and accessories are more likely to be bought in spring and summer compared to fall and winter time ➔ however there is an exception (First Aid Kit) ➔  adoption: wholesale will likely buy after an autumn fair, so the maximum will be in winter
| C02 | "Small" vs. "big" customers    | ➔ pre-defined weights by customer ~~➔ customer Silicon Valley Bikes dropped ➔ customer Bavaria Bikes increases over time~~
| ~~C03~~ | ~~Simple discount model~~    | ~~customers get pre-defined discount rates dependent on their overall revenue~~
| C04 | More / Less popular products    | ➔ there are weights by product ➔ ~~E-Bike was introduced 2010 in DE and 2014 in US~~ ➔ ~~Hoverboard was introduced in 2015 and dropped shortly afterwards~~
| ~~C05~~ | ~~New attributes~~    | ~~➔ colors and components: A bike can be ordered in different colors (touring bikes) resp. with different components, e.g. Shimano or SRAM (race bikes and mountain bikes) ➔ Popularity of colors changes over time~~
| C06 | Regional effects    | There is a higher demand for mountain bikes in Denver, Munich, and Heidelberg compared to all other cities. This is implemented via C02: big customers are in preferred regions.
| C07 | Sales Prices    | ➔ Inflation: Sales prices increase per year by a certain percentage ➔ ~~Country-specific: sales prices are different in the US than in DE ➔ Currency: sales prices are in local currency (with the exchange rate beginning of the year) ➔ Daily exchange rate (at sales order date) is delivered for special analysis; suitable exchange rate type is implemented in BW~~
| ~~C08~~ | ~~Costs~~    | ~~Costs of Goods Manufactured are adjusted by a monthly cost index~~
| ~~C09~~ | ~~Time-dependent master data~~    | ~~➔ one customer moves to a different sales organization ➔ this is used to show the concept of "Slowly Changing Dimensions" (Kimball) (“Slowly Changing Dimensions”)~~
| ~~C10~~ | ~~Additional effects~~    | ~~➔ a slight increase in sales for some products during Summer Olympics in 2008, 2012 and 2016 ➔ major decline on overall revenue in US after Lehman crisis in 2008 and recovery afterwards~~

## Differences to previous data set
Despite modifying the characteristics as described above, we have:
- Less materials available in GB (e.g. "E-Bike Tailwind", "Hoverboard")
- More materials available in GB (e.g. "Deluxe GPS-Bike Computer Royal Blue")
- Different materials classes (product categories)

## Quantities
- We want to have approx. 50 sales orders per working day (as in [1]). Number of positions per order is 1-8. Sales quantity per position was 1-10.
- Data starts in 2020 and extends into the near future

## Problems
- We only have few customers (24). We would need at least one customer per state (in US and DE) to be able to do nice analysis with geo maps. We should add customer master data in ERP accordingly (just for group ### = 000).
- Financial inflation needs to be implemented via subsequent update of material prices

# Usage
`Generator.ipynb` creates the data.

`masterdata.xlsx` contains the master data and respective settings which drives the calculations.

The resulting sales order data is in folder `data/`.

In case you want to use the static data file for analytical purposes (not by accessing SAP ERP system),
the data is optionally extended with financial informations as this was done in previously [1,2].
