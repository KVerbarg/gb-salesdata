Import of generated sales orders into S/4HANA
================

# Good to know
## Language
All activities are done in language `EN`.

This is important since some field values depend on the language. The SalesOrderType is `TA` in German and `OR` in English.

## Business partner id

To be able to use a given customer (we only know its name) as SoldToParty later, we need to find out the business partner ID in S4 (3 ways to do it):
- Table BUT000, Field BU_SORT1= '000', Field NAME_ORG1 = 'Olympic Protective Gear', then the id is in Field PARTNER.  
- Function BAPI_BUPA_SEARCH (fields CENTRALDATA > MC_NAM1 and SEARCHTERM1). {I found it via  BAPI Business Partner, looking at the related  Business Object BUS1006, and then navigate to Method BusinessPartner.Search}
- OData API_BUSINESS_PARTNER. Can be called from ABAP <https://blogs.sap.com/2014/11/09/calling-an-external-restful-service-from-abap-http-method-get/>

## Organization units
Sales organization (VKORG) controls calculation of conditions and hence currency / taxes.

The country is just an attribute of the customer and possibly different to the country of the VKORG. However, for Global Bike, customers are related to VKORG in their own country.

## Dates
Available date fields are as follows (the list is not complete). 
"t" means: will be set to the planned date of our sales order (even if it is in the past).

| Field | DB field | will be set as follows |
|-------|----------|--------------------------------|
| Document Date | VBAK-AUDAT | t
| Customer Reference Date | VBKD-BSTDK | t
| Pricing Date | VBKD-PRSDT | t
| Billing Date | VBKD-FKDAT | t (implicitly set to t also)
| RequestedDeliveryDate | KETDAT | t
| LastChangeDate | VBAK-ERDAT | set by S4 to current date
| Ship-To Party's Customer Reference Date | VBKD-BSTDK_E | left empty

## Discount

For later analysis, it doesn't matter whether we apply discounts on item (RA00 condition) or on header (HA00 condition) level.

## Field mappings

- Cust. Reference = External order id

## Order status

Wie ist der nach dem Anlegen? *************


--------------------------------------------------------
# Implementation
The following DDIC objects are created in Package `ZUCC_ANALYTICS`.

## Message class
Message class `ZUCC_ANALYTICS` with Transaction `SE91`. 
| No. | Text |
|-----|------|
| 000 | Error with code & (check sy-subrc in program) |
| 001 | Business partner & not found |

## Strcuture
Structure `ZUCC_ANALYTICS_SDGEN_READ` describes the row schema of the TSV input file.
| Component | Component type | |
|------------|----------------|---|
| BSTNK | 		BSTNK |
| POSNR | 		POSNR_VA |
| AUDAT | 		AUDAT |
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
| VKORG	| VKORG | 
| VTWEG	| VTWEG | 
| SPART	| SPART | 
| NAME_ORG1	| BU_NAMEOR1 | 
| DISCOUNTPCT | KBETR | Reference field: RV61A-KOEI1
| MATNR	| MATNR | 
| ZMENG	| DZMENG | Reference field: ZUCC_ANALY_SDGEN-ZIEME
| ZIEME	| DZIEME | 


## Program for upload

`ZUCC_ANALYTICS_SDGEN_IMPORT` "Import SD data into DB table" imports the sales orders created by the generator from TSV-files into a DB table. The idea is to import all sales orders of the past and the future. The procedure may be repeated at any time (e.g. to modify sales orders to be created in the future). The actual creation of sales orders is done in the next step.

## Program for daily batch process
`ZUCC_ANALYTICS_SDGEN_DAILY` "Create new sales orders from generated data on daily basis" will create sales orders based on the contents of the DB table.

Parameter `p_all` "Check *all* past sales orders": Usually for the sake of performance only the data of the last month or so up to the current date is checked. Checking this option, we can extend this time range to the entire past.

Parameter `p_test` "Testrun only": when checked, will not create sales orders.


## doku (delete this...)
https://codezentrale.de/tag/cl_gui_frontend_services/

https://help.sap.com/docs/ABAP_PLATFORM_NEW/5a005e044eef436f8b27bbd3f73a3cfc/1dac0155370648569fe843170e07c4da.html?locale=en-US&q=CL_GUI_FRONTEND_SERVICES