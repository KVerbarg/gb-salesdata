Import of generated sales orders into S/4HANA
================

# Good to know
## Language
All activities are done in language `EN`.

This is important since some field values depend on the language. The SalesOrderType is `OR` in German and `TA` in English.

## Business partner id

To be able to use a given customer (we only know its name) as SoldToParty later, we need to find out the business partner ID in S4 (3 ways to do it):
- Table BUT000, Field BU_SORT1= '000', Field NAME1_ORG = 'Olympic Protective Gear', then the id is in Field PARTNER.  
- Function BAPI_BUPA_SEARCH (fields CENTRALDATA > MC_NAM1 and SEARCHTERM1). {I found it via  BAPI Business Partner, looking at the related  Business Object BUS1006, and then navigate to Method BusinessPartner.Search}
- OData API_BUSINESS_PARTNER. Can be called from ABAP <https://blogs.sap.com/2014/11/09/calling-an-external-restful-service-from-abap-http-method-get/>

## Sales organization and company code
Wie wird das aus dem Kunden abgeleitet? - gar nicht, sondern übergeben!

``` Python
salesOrder_Data = {
    'SalesOrderType': 'TA',
    'SalesOrganization': 'UE00',
    'DistributionChannel': 'WH',
    'OrganizationDivision': 'BI',
    'SoldToParty': '5999',
    'PurchaseOrderByCustomer': 'Created via OData API (Pyodata)'
}

class A_SalesOrder:
def __init__(self, SalesOrderType, SalesOrganization, DistributionChannel, OrganizationDivision, SoldToParty, PurchaseOrderByCustomer, PricingDate, RequestedDeliveryDate, CustomerPurchaseOrderDate):
    self.SalesOrderType = SalesOrderType
    self.SalesOrganization = SalesOrganization
    self.DistributionChannel = DistributionChannel
    self.OrganizationDivision = OrganizationDivision
    self.SoldToParty = SoldToParty
    self.PurchaseOrderByCustomer = PurchaseOrderByCustomer
    self.PricingDate = PricingDate
    self.RequestedDeliveryDate = RequestedDeliveryDate
    self.CustomerPurchaseOrderDate = CustomerPurchaseOrderDate
    self.to_Item = []

def addtoItem(self, material, quantity):
    self.to_Item.append({'Material': material, 'RequestedQuantity': quantity})
```

## Price
Currency depends on...
Wo Listenpreis ändern?

## Dates
Available date fields are:
- SalesOrderDate
- CustomerPurchase-OrderDate
- Pricing Date
- RequestedDeliveryDate
- LastChangeDate (set by S4)
- LastChangeDateTime (set by S4)


They all can be set to values in the past.

## Order status

Wie ist der nach dem Anlegen?

## Field mappings

- Cust. Reference = External order id

## File access in ABAP

Ich kann mit ABAP (hoffe ich) auch von meinem Desktop eine Datei auf den Server laden.
Kommandos: CL_GUI_FRONTEND_SERVICES und OPEN DATASET..
Vielleicht muss man in der Transaktion FILE einen Pfad anlegen.
