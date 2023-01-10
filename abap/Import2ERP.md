Import of generated sales orders into S/4HANA
================

# Good to know
## Language
All activities are done in language `EN`.

This is important since some field values depend on the language. The SalesOrderType is `TA` in German and `OR` in English.

## Order id

As the order id is created by the S4 system, we do need an additional internal id. The "Cust. Reference" (example value is "2020-01-01#000") serves this purpose. This helps to decide whether a planned order in our list is already created in S4 or not.

## Preparation Materials
The materials DGRB2000, DGRR2000, DGRW2000, GRBL2000, GRRL2000, GRWL2000, ORBC1000
are only available in VKORG/VTWEG = DS00/WH and UW00/WH.
We also need them in VKORGs DN00 and UE00.

To this end, we copied material master data for the above mentioned materials between
| Org unit | To1 | From1 | | To2 | From2 |
|----------|-----|-------|-|-----|-------|
| Plant    | HH00   | HD00  || MI00 | DL00
| Stor. Loc. | FG00 | FG00  || FG00 | FG00
| Sales Org. | DN00 | DS00  || UE00 | UW00
| Distr. Channel | WH | WH  || WH   | WH

and change "SalesOrg1" > "Delivering Plant" to HH00 or MI00, resp.
Also adapt "Sales: General/Plant Data" > "Trans.Group" to "On pallets", "Loading Grp." to "Hand lift". Otherwise, we will get a warning in the sales order because route determination will not find a shipping point. Check the settings using the SQL for materials in `GeneratorGB.iynb`.

Exception: ORBC1000 has no sales data at all. Therefore, I copied sales data from another material (GRWL2000 for org unit DN00/WH) to ORBC1000 for DN00/WH. Modify delivery plant and taxation as needed. Do this for all four sales orgs. On "Basic Data 1", change Division from 00 (cross-division) to BI. Also copy the data for all four plants / FG00 from another material and adapt fields as needed until using the material in a sales order is possible.

## Business partner id

To be able to use a given customer (we only know its name) as SoldToParty later, we need to find out the business partner ID in S4 (3 ways to do it):
- Table BUT000, Field BU_SORT1= '000', Field NAME_ORG1 = 'Olympic Protective Gear', then the id is in Field PARTNER.  
- Function BAPI_BUPA_SEARCH (fields CENTRALDATA > MC_NAM1 and SEARCHTERM1). {I found it via  BAPI Business Partner, looking at the related  Business Object BUS1006, and then navigate to Method BusinessPartner.Search}
- OData API_BUSINESS_PARTNER. Can be called from ABAP <https://blogs.sap.com/2014/11/09/calling-an-external-restful-service-from-abap-http-method-get/>

## Organization units
Sales organization (VKORG) controls calculation of conditions and hence currency / taxes.

The country is just an attribute of the customer and possibly different to the country of the VKORG. However, for Global Bike, customers are related to a VKORG in their own country.

## Dates
Available date fields are as follows (the list is not complete). 
"t" means: will be set to the planned date of our sales order (even if it is in the past).

| Field | DB field | will be set as follows |
|-------|----------|--------------------------------|
| Document Date | VBAK-AUDAT | t
| Customer Reference Date | VBKD-BSTDK | t
| Pricing Date | VBKD-PRSDT | t
| Billing Date | VBKD-FKDAT | t (implicitly set to t also)
| RequestedDeliveryDate | VBAK-VDATU | t
| LastChangeDate | VBAK-ERDAT | set by S4 to current date
| Ship-To Party's Customer Reference Date | VBKD-BSTDK_E | left empty

## Discount

For later analysis, it doesn't matter whether we apply discounts on item (RA00 condition) or on header (HA00 condition) level. We will do HA00.

## Order status

The order status after creationg is "open" and will not be further processed at the moment.


# Implementation
The following DDIC objects are created in Package `ZUCC_ANALYTICS`.

## Message class
Message class `ZUCC_ANALYTICS` with Transaction `SE91`. 
| No. | Text |
|-----|------|
| 000 | Error with code & (check sy-subrc in program) |
| 001 | Business partner & not found |

## Application Log
In Transaction `SLG0` the object `ZUCC_ANALYTICS` and subobject `ZUCC_ANALYTICS_SDGEN` are created.
**Workaround: Use `SACO` / `SACO_CHANGE_SA` instead.**

## Strcuture
Structure `ZUCC_ANALYTICS_SDGEN_READ` describes the row schema of the TSV input file.
| Component | Component type | |
|------------|----------------|---|
| BSTNK | 		BSTNK |
| POSNR | 		POSNR_VA |
| AUDAT | 		AUDAT | 
| VDATU |   EDATU_VBAK | 
| VKORG | 		VKORG |
| VTWEG | 		VTWEG |
| SPART | 		SPART |
| NAME_ORG1 |	BU_NAMEOR1 |
| DISCOUNTPCT | KBETR | Reference field: RV61A-KOEI1
| MATNR | 		MATNR |
| ZMENG | 		DZMENG | Reference field: ZUCC_ANALYTICS_SDGEN_READ-ZIEME
| ZIEME | 		DZIEME |

## Table
 Table `ZUCC_ANALY_SDGEN` "Generated Sales Orders for daily creation in SD"

| Field | Data element |   |
|-------|--------------|---|
| MANDT	| MANDT | key
| BSTNK	| BSTNK | key
| POSNR	| POSNR_VA | key
| AUDAT	| AUDAT | 
| VDATU |   EDATU_VBAK | 
| VKORG	| VKORG | 
| VTWEG	| VTWEG | 
| SPART	| SPART | 
| NAME_ORG1	| BU_NAMEOR1 | 
| DISCOUNTPCT | KBETR | Reference field: RV61A-KOEI1
| MATNR	| MATNR | 
| ZMENG	| DZMENG | Reference field: ZUCC_ANALY_SDGEN-ZIEME
| ZIEME	| DZIEME | 


## Program to define price list

`ImportPrices.ipynb` imports the prices via OData. See the documentation there. Prices have to be defined *before* we import and create sales orders.

## Program for data upload

`ZUCC_ANALYTICS_SDGEN_IMPORT` "Import SD data into DB table" imports the sales orders created by the generator from TSV-files into a DB table. The idea is to import all sales orders of the past and the future. The procedure may be repeated at any time (e.g. to modify sales orders to be created in the future). The actual creation of sales orders is done in the next step.

## Program for daily batch process
`ZUCC_ANALYTICS_SDGEN_DAILY` "Create new sales orders from generated data on daily basis" will create sales orders based on the contents of the DB table.

Parameter `p_all` "Check *all* past sales orders": Usually for the sake of performance only the data of the last month or so up to the current date is checked. Checking this option, we can extend this time range to the entire past.

Parameter `p_test` "Testrun only": when checked, will not create sales orders.

# Operation

Program `ZUCC_ANALYTICS_SDGEN_DAILY` with variant `???` (Parameters p_all and P_test are unchecked) is planned as a background job on a daily basis.

Global Bike configuration (at the point of writing this) is only valid up to the year 2030.

Check for success in the application log using Transaction `SLG1` and filtering for the object `ZUCC_ANALYTICS`.