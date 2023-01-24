@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consume C_SalesDocumentItemDEX'
@Metadata.ignorePropagatedAnnotations: false  -- inherit semantics annotations (UoM, currency)
@ObjectModel.usageType:{
    serviceQuality: #D,
    sizeCategory: #XL,
    dataClass: #TRANSACTIONAL
}
@VDM.viewType: #CONSUMPTION
@Analytics.dataCategory: #CUBE
define view entity ZUCC_C_SalesDocItem
  as select from C_SalesDocumentItemDEX
  association [0..1] to I_Material as _Material on $projection.Material = _Material.Material
  association [0..1] to I_Customer as _Customer on $projection.SoldToParty = _Customer.Customer
{
  key SalesDocument,
  key SalesDocumentItem,
      --SDDocumentCategory,
      --SalesDocumentType,
      SalesDocumentItemCategory,
      --CreationDate,
      --CreationTime,
      --LastChangeDate,
      SalesOrganization,
      DistributionChannel,
      Division,
      @ObjectModel.foreignKey.association: '_Material'
      Material,
      MaterialGroup,
      @ObjectModel.foreignKey.association: '_Customer'
      SoldToParty,
      --ShipToParty,
      --PayerParty,
      --BillToParty,
      SalesDocumentDate,
      @Aggregation.default: #SUM
      OrderQuantity,
      OrderQuantityUnit,
      --BaseUnit,
      @Aggregation.default: #SUM
      ItemGrossWeight,
      --@Aggregation.default: #SUM
      --ItemNetWeight,
      ItemWeightUnit,
      --PricingDate,
      --ExchangeRateDate,
      --Currency,
      TransactionCurrency,
      @Aggregation.default: #SUM
      NetAmount,
      @Aggregation.default: #SUM
      TaxAmount,
      @Aggregation.default: #SUM
      CostAmount,
      @Aggregation.default: #SUM
      Subtotal1Amount,
      --Subtotal2Amount,
      --Subtotal3Amount,
      --@Aggregation.default: #SUM
      --Subtotal1Amount - NetAmount AS Discount,  -- Datatype CURR is not (yet) supported in arithmetic expression ;-(
      --@Aggregation.default: #SUM
      --NetAmount + TaxAmount AS Revenue,

      --ShippingPoint,
      RequestedDeliveryDate,
      Plant,
      --Route,
      --IncotermsClassification,
      --IncotermsTransferLocation,
      --BusinessArea,
      --ControllingArea,
      --OverallSDProcessStatus,
      --OverallTotalDeliveryStatus,
      --OverallDeliveryStatus,
      --ItemGeneralIncompletionStatus,
      --ItemBillingIncompletionStatus,
      --PricingIncompletionStatus,
      --ItemDeliveryIncompletionStatus,
      --DeliveryConfirmationStatus,
      --SDProcessStatus,
      --TotalDeliveryStatus,
      --DeliveryStatus,

      -- public
      _Material,
      _Customer,
      _SalesDocument
}
where
  _SalesDocument.SDDocumentCategory = 'C' // Order
