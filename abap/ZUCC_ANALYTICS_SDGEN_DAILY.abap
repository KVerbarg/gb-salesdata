*&---------------------------------------------------------------------*
*& Report ZUCC_ANALYTICS_SDGEN_DAILY
*&---------------------------------------------------------------------*
*& see https://github.com/KVerbarg/gb-salesdata
*&---------------------------------------------------------------------*
REPORT zucc_analytics_sdgen_daily.

CONSTANTS checked TYPE c VALUE 'X'.
CONSTANTS l_version TYPE string VALUE 'Version 2022-12-30'.  " Update this when modifying the program
PARAMETERS p_all  AS CHECKBOX DEFAULT space.
PARAMETERS p_test AS CHECKBOX DEFAULT checked.

* Start application log
IF p_test EQ space.
  DATA ls_log       TYPE bal_s_log.
  DATA l_log_handle TYPE balloghndl.
  ls_log-extnumber  = l_version.
  ls_log-object     = 'SACO'.  " 'ZUCC_ANALYTICS'. ***********************
  ls_log-subobject  = 'SACO_CHANGE_SA'.  " 'ZUCC_ANALYTICS'. ***********************
  ls_log-aldate     = sy-datum.
  ls_log-altime     = sy-uzeit.
  ls_log-aluser     = sy-uname.
  ls_log-alprog     = sy-repid.
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = ls_log
    IMPORTING
      e_log_handle = l_log_handle.
ENDIF.

* The start and end date to process data
DATA ls_start TYPE d.
IF p_all EQ space.
  " Usually, we would only look at the data of the last month in order to improve performance
  ls_start = sy-datum - 30.
ELSE.
  ls_start = '20150101'. " long ago
ENDIF.
DATA ls_end TYPE d.
ls_end = sy-datum.

********************************** test. remove it ***********************
ls_start = '20150101'.
ls_end = '20150112'.

* logging: selected date range
IF p_test EQ space.
  DATA ls_msg TYPE bal_s_msg.
  CLEAR ls_msg.
  ls_msg-msgty     = 'S'.
  ls_msg-msgid     = 'ZUCC_ANALYTICS'.
  ls_msg-msgno     = 2.
  ls_msg-msgv1     = ls_start.
  ls_msg-msgv2     = ls_end.
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = l_log_handle
      i_s_msg      = ls_msg.
ENDIF.
WRITE: / 'Processing date range', ls_start, 'to', ls_end.

* Read planned sales order data
* We have to do it in advance, since a DB COMMIT would break a SELECT loop
SELECT bstnk, audat, vdatu, name_org1, vkorg, vtweg, spart, discountpct
  FROM zucc_analy_sdgen INTO TABLE @DATA(lt_order)
  WHERE audat BETWEEN @ls_start AND @ls_end
  GROUP BY bstnk, audat, vdatu, name_org1, vkorg, vtweg, spart, discountpct  "group positions to orders
  ORDER BY bstnk.

* Next sales order
DATA ls_order LIKE LINE OF lt_order.
DATA lv_so_created TYPE i.
DATA lv_last_bstnk TYPE zucc_analy_sdgen-bstnk.
CLEAR lv_last_bstnk.

LOOP AT lt_order INTO ls_order.
  DATA lv_nof_orders TYPE i.
  lv_nof_orders = sy-tfill.

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
  ls_header-req_date_h = ls_order-vdatu. "Requested Delivery Date
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
  ls_cond-cond_value = - ls_order-discountpct.  "HA00 discount should be negative
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
    IF p_test EQ space.
      CLEAR ls_msg.
      ls_msg-msgty     = 'E'.
      ls_msg-msgid     = 'ZUCC_ANALYTICS'.
      ls_msg-msgno     = 5.
      ls_msg-msgv1     = ls_order-bstnk.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = l_log_handle
          i_s_msg      = ls_msg.
      DATA ls_return2 LIKE LINE OF lt_return.
      LOOP AT lt_return INTO ls_return2.
        CLEAR ls_msg.
        ls_msg-msgty     = ls_return2-type.
        ls_msg-msgid     = ls_return2-id.
        ls_msg-msgno     = ls_return2-number.
        ls_msg-msgv1     = ls_return2-message_v1.
        ls_msg-msgv2     = ls_return2-message_v2.
        ls_msg-msgv3     = ls_return2-message_v3.
        ls_msg-msgv4     = ls_return2-message_v4.
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = l_log_handle
            i_s_msg      = ls_msg.
      ENDLOOP.
      " save it before the program terminates
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_save_all = 'X'.
    ENDIF.
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
      CLEAR ls_msg.
      ls_msg-msgty     = ls_return-type.
      ls_msg-msgid     = ls_return-id.
      ls_msg-msgno     = ls_return-number.
      ls_msg-msgv1     = ls_return-message_v1.
      ls_msg-msgv2     = ls_return-message_v2.
      ls_msg-msgv3     = ls_return-message_v3.
      ls_msg-msgv4     = ls_return-message_v4.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = l_log_handle
          i_s_msg      = ls_msg.
      " save it before the program terminates
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_save_all = 'X'.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
    ELSE.
      lv_so_created = lv_so_created + 1.
      ASSERT p_test EQ space.
      CLEAR ls_msg.
      ls_msg-msgty     = 'I'.
      ls_msg-msgid     = 'ZUCC_ANALYTICS'.
      ls_msg-msgno     = 3.
      ls_msg-msgv1     = lv_salesdocument.
      ls_msg-msgv2     = ls_order-bstnk.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = l_log_handle
          i_s_msg      = ls_msg.
      WRITE: 'Sales order', lv_salesdocument, 'for customer reference', ls_order-bstnk, 'created'.
    ENDIF.
  ENDIF.

   EXIT. "*********************************************

ENDLOOP.
ASSERT sy-subrc EQ 0.

IF p_test EQ space.
  CLEAR ls_msg.
  ls_msg-msgty     = 'S'.
  ls_msg-msgid     = 'ZUCC_ANALYTICS'.
  ls_msg-msgno     = 4.
  ls_msg-msgv1     = CONV string( lv_nof_orders ).
  ls_msg-msgv2     = CONV string( lv_so_created ).
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = l_log_handle
      i_s_msg      = ls_msg.
ENDIF.
WRITE: / '#sales orders processed', lv_nof_orders.
WRITE: / '#sales orders created', lv_so_created.
WRITE: / 'Finished.'.

* Close application log
IF p_test EQ space.
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_save_all = 'X'.
ENDIF.