*&---------------------------------------------------------------------*
*& Report ZUCC_ANALYTICS_SDGEN_DAILY
*&---------------------------------------------------------------------*
*& see https://github.com/KVerbarg/gb-salesdata
*&---------------------------------------------------------------------*
REPORT zucc_analytics_sdgen_daily.

CONSTANTS checked TYPE c VALUE 'X'.
PARAMETERS p_all  AS CHECKBOX DEFAULT checked. "space. ************************
PARAMETERS p_test AS CHECKBOX DEFAULT checked.

* The start date to process data
DATA ls_start TYPE d.
ls_start = '20150101'. " long ago
* Usually, we would only look at the data of the last month in order to improve performance
IF p_all EQ space.
  ls_start = sy-datum - 30.
ENDIF.
WRITE: / 'We start at date', ls_start.

* Read planned sales order data
* We have to do it in advance, since a DB COMMIT would break a SELECT loop
SELECT bstnk, audat, name_org1, vkorg, vtweg, spart, discountpct
  FROM zucc_analy_sdgen INTO TABLE @DATA(lt_order)
  WHERE audat BETWEEN @ls_start AND '20150101'       "@sy-datum   "*********** test ***************
  GROUP BY bstnk, audat, name_org1, vkorg, vtweg, spart, discountpct  "group positions to orders
  ORDER BY bstnk.

* Next sales order
DATA ls_order LIKE LINE OF lt_order.
DATA lv_so_processed TYPE i.
DATA lv_so_created TYPE i.
DATA lv_last_bstnk TYPE zucc_analy_sdgen-bstnk.
CLEAR lv_last_bstnk.
LOOP AT lt_order INTO ls_order.

  lv_so_processed = lv_so_processed + 1.
  ASSERT lv_last_bstnk <> ls_order-bstnk. " make sure we really group only by bstnk
  lv_last_bstnk = ls_order-bstnk.
  WRITE: / ls_order-bstnk.

* Check if sales order already exists (BSTNK is our secret primary key)?
  DATA lv_exists TYPE c.
  SELECT SINGLE 'X' FROM vbak INTO @lv_exists WHERE bstnk = @ls_order-bstnk.
  IF sy-subrc EQ 0 AND lv_exists EQ checked.
    WRITE: 'sales order already exists.'.
    CONTINUE.
  ENDIF.

* Find partner id for customer name
  DATA lv_partner TYPE vbak-kunnr.
  SELECT SINGLE partner FROM but000 INTO @lv_partner WHERE bu_sort1 = '000' AND name_org1 = @ls_order-name_org1.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZUCC_ANALYTICS' TYPE 'E' NUMBER 001 WITH ls_order-name_org1.
    CONTINUE.
  ENDIF.

* Create sales order
  DATA lv_salesdocument LIKE  bapivbeln-vbeln. " return sales order number
  DATA lt_return TYPE TABLE OF bapiret2.
  " header
  DATA ls_header TYPE bapisdhd1.
  DATA ls_headerx TYPE bapisdhd1x.
  CLEAR: ls_header, ls_headerx.
  ls_headerx-updateflag = 'I'.          "insert sales order
  ls_header-doc_type = 'TA'.  " must be German value for "standard order"
  ls_headerx-doc_type = checked.
  ls_header-sales_org = ls_order-vkorg.  "VKORG
  ls_headerx-sales_org = checked.
  ls_header-distr_chan = ls_order-vtweg. "VTWEG
  ls_headerx-distr_chan = checked.
  ls_header-division = ls_order-spart.   "SPART
  ls_headerx-division = checked.
  ls_header-doc_date = ls_order-audat. "Document Date
  ls_headerx-doc_date = checked.
  ls_header-purch_date = ls_order-audat. "Customer Reference Date
  ls_headerx-purch_date = checked.
  ls_header-price_date = ls_order-audat. "Date for Pricing and Exchange Rate
  ls_headerx-price_date = checked.
  ls_header-req_date_h = ls_order-audat. "Requested Delivery Date
  ls_headerx-req_date_h = checked.
  ls_header-purch_no_c = ls_order-bstnk. "customer order number (our unique key)
  ls_headerx-purch_no_c = checked.
  " business partners
  DATA ls_partner TYPE bapiparnr.
  DATA lt_partner TYPE TABLE OF bapiparnr.
  CLEAR: ls_partner, lt_partner.
  ls_partner-partn_role = 'AG'. " must be German value
  ls_partner-partn_numb = lv_partner.
  APPEND ls_partner TO lt_partner.
  ls_partner-partn_role = 'WE'. " must be German value
  ls_partner-partn_numb = lv_partner.
  APPEND ls_partner TO lt_partner.
  " items deep insert of 1:n relation to orders
  " plus a schedule line for each item
  DATA ls_item      TYPE bapisditm.
  DATA lt_item      TYPE TABLE OF bapisditm.
  DATA ls_itemx     TYPE bapisditmx.
  DATA lt_itemx     TYPE TABLE OF bapisditmx.
  DATA ls_schedule  TYPE bapischdl.
  DATA lt_schedule  TYPE TABLE OF bapischdl.
  DATA ls_schedulex TYPE bapischdlx.
  DATA lt_schedulex TYPE TABLE OF bapischdlx.
  CLEAR: ls_item, lt_item, ls_itemx, lt_itemx.
  CLEAR: ls_schedule, lt_schedule, ls_schedulex, lt_schedulex.
  " position data of the order
  SELECT posnr, matnr, zmeng, zieme
    FROM zucc_analy_sdgen
    INTO (@ls_item-itm_number, @ls_item-material, @ls_item-target_qty, @ls_item-target_qu)
    WHERE bstnk = @ls_order-bstnk
    AND matnr NOT IN ('DGRB2000','DGRR2000','DGRW2000','GRBL2000','GRRL2000','GRWL2000','ORBC1000')   "**************** workaround
    ORDER BY posnr.
    " item
    APPEND ls_item TO lt_item.
    " itemx
    ls_itemx-updateflag = 'I'.     " insert order item
    ls_itemx-itm_number = ls_item-itm_number.  " the same position
    ls_itemx-material = checked.
    ls_itemx-target_qty = checked.
    ls_itemx-target_qu = checked.
    APPEND ls_itemx TO lt_itemx.
    " schedule line
    ls_schedule-itm_number = ls_item-itm_number.  " the same position
    ls_schedule-sched_line = '0001'.
    ls_schedule-req_qty    = ls_item-target_qty.  " one-time delivery
    APPEND ls_schedule TO lt_schedule.
    " schedule flags
    ls_schedulex-updateflag  = 'I'.  " insert
    ls_schedulex-itm_number  = ls_item-itm_number.  " the same position
    ls_schedulex-sched_line  = '0001'.
    ls_schedulex-req_qty     = 'X'.
    APPEND ls_schedulex TO lt_schedulex.
  ENDSELECT.
  " Conditions
  DATA lt_cond TYPE TABLE OF bapicond.
  DATA ls_cond TYPE bapicond.
  DATA lt_condx TYPE TABLE OF bapicondx.
  DATA ls_condx TYPE bapicondx.
  CLEAR: ls_cond, lt_cond, ls_condx, lt_condx.
  ls_cond-cond_type = 'HA00'.   " discount per customer as condition in the order header
  ls_condx-cond_type = checked.
  ls_cond-cond_value = ls_order-discountpct.
  ls_condx-cond_value = checked.
  ls_condx-updateflag = 'I'.   " insert
  APPEND ls_cond TO lt_cond.
  APPEND ls_condx TO lt_condx.
  " Create
  CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
    EXPORTING
      order_header_in      = ls_header
      order_header_inx     = ls_headerx
      testrun              = p_test
    IMPORTING
      salesdocument        = lv_salesdocument
    TABLES
      return               = lt_return
      order_items_in       = lt_item
      order_items_inx      = lt_itemx
      order_partners       = lt_partner
      order_schedules_in   = lt_schedule
      order_schedules_inx  = lt_schedulex
      order_conditions_in  = lt_cond
      order_conditions_inx = lt_condx.
  ASSERT sy-subrc EQ 0.
  ASSERT ( p_test EQ space ) OR ( lv_salesdocument  IS INITIAL ). " lv_salesdocument is not set in test mode

* check the return table.
  DATA ls_return LIKE LINE OF lt_return.
  LOOP AT lt_return INTO ls_return WHERE type = 'W' OR type = 'E' OR type = 'A'.
    MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
    " message type E aborts anyway
    EXIT.
  ENDLOOP.

* commit
  IF NOT p_test EQ checked.
    CLEAR ls_return.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = checked "synchronous update
      IMPORTING
        return = ls_return.
    IF sy-subrc <> 0.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
    ELSE.
      lv_so_created = lv_so_created + 1.
      WRITE: 'Document ', lv_salesdocument, ' created'.
    ENDIF.
  ENDIF.

ENDLOOP.
ASSERT sy-subrc EQ 0.

WRITE: / '#sales orders processed', lv_so_processed.
WRITE: / '#sales orders created', lv_so_created.
WRITE: / 'Finished.'.
* application log ***********