FUNCTION zucc_analytics_sdgen_cost.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_DATE) TYPE  D DEFAULT SY-DATUM
*"     REFERENCE(I_LOG) TYPE  BALLOGHNDL
*"----------------------------------------------------------------------

  CONSTANTS checked TYPE c VALUE 'X'.
  DATA lv_year TYPE n LENGTH 4.
  lv_year = i_date(4).
  ASSERT lv_year IS NOT INITIAL.

* Check if we have cost deviation for the given year
  SELECT c~*
    FROM zucc_analy_costs AS c
    JOIN mbew AS m
    ON c~plant = m~bwkey AND c~matnr = m~matnr
    INTO @DATA(ls_data)
    WHERE c~so_year = @lv_year
    AND (
      ( m~vprsv = 'V' AND cost <> m~verpr )
      OR
      ( m~vprsv = 'S' AND cost <> m~stprs )
    ).

    " material (adjust data type for BAPI)
    DATA lv_material TYPE matnr18.
    CALL FUNCTION 'CONVERSION_EXIT_MATN5_INPUT'
      EXPORTING
        input        = ls_data-matnr
      IMPORTING
        output       = lv_material
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    ASSERT sy-subrc EQ 0.

    " valuation type
    DATA lv_val_type TYPE  bapi_matval_key-val_type.
    CLEAR lv_val_type.

    " pricedate
    DATA ls_pricedate TYPE bapi_matval_pricedate.
    CLEAR ls_pricedate.
    ls_pricedate-price_date = sy-datum.  " we cannot post in the past (material ledger closed)

    " price
    DATA ls_price TYPE bapi_matval_prices.
    CLEAR ls_price.
    ls_price-valuation_view = 0.  " Legal Valuation
    ls_price-curr_type = '10'.    " Company code currency
    ls_price-price = ls_data-cost.
    ls_price-currency_iso = ls_data-waers.
    ls_price-price_unit = 1.
    DATA lt_price TYPE TABLE OF bapi_matval_prices.
    CLEAR lt_price.
    APPEND ls_price TO lt_price.

    " return
    DATA lt_return TYPE TABLE OF bapiret2.
    CLEAR lt_return.
    DATA ls_doc TYPE bapi_pricechange_document.

    " change the price
    CALL FUNCTION 'BAPI_MATVAL_PRICE_CHANGE'
      EXPORTING
        material            = lv_material
        valuationarea       = ls_data-plant
        valuationtype       = lv_val_type
        pricedate           = ls_pricedate
*       MATERIAL_EVG        =
*       MATERIAL_LONG       =
        doc_header_text     = 'ZUCC_ANALYTICS_SDGEN_COST'
*       REF_DOC_NUMBER      =
*       REASON_CODE         =
      IMPORTING
        pricechangedocument = ls_doc
      TABLES
        prices              = lt_price
        return              = lt_return.
    ASSERT sy-subrc EQ 0.

    " application log of return and success
    IF i_log IS NOT INITIAL.
      DATA ls_return LIKE LINE OF lt_return.
      LOOP AT lt_return INTO ls_return.
        DATA ls_msg TYPE bal_s_msg.
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
            i_log_handle = i_log
            i_s_msg      = ls_msg.
      ENDLOOP.

      IF ls_doc-ml_doc_num IS NOT INITIAL.
        CLEAR ls_msg.
        ls_msg-msgty     = 'I'.
        ls_msg-msgid     = 'ZUCC_ANALYTICS'.
        ls_msg-msgno     = 7.
        ls_msg-msgv1     = lv_material.
        ls_msg-msgv2     = ls_data-plant.
        ls_msg-msgv3     = lv_year.
        ls_msg-msgv4     = ls_data-cost.
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = i_log
            i_s_msg      = ls_msg.
        CLEAR ls_msg.
        ls_msg-msgty     = 'I'.
        ls_msg-msgid     = 'ZUCC_ANALYTICS'.
        ls_msg-msgno     = 6.
        ls_msg-msgv1     = ls_doc-ml_doc_year.
        ls_msg-msgv2     = ls_doc-ml_doc_num.
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = i_log
            i_s_msg      = ls_msg.
      ENDIF.
    ENDIF.

    LOOP AT lt_return INTO ls_return WHERE type = 'W' OR type = 'E' OR type = 'A'.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
    ENDLOOP.
    WRITE: / 'Material', lv_material, 'Plant', ls_data-plant, 'Year', lv_year, 'Cost', ls_data-cost, 'Document', ls_doc-ml_doc_num.

  ENDSELECT.

  IF sy-subrc EQ 0.
    " commit
    CLEAR ls_return.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = checked "synchronous update
      IMPORTING
        return = ls_return.
    ASSERT sy-subrc EQ 0.
    WRITE: /.
  ELSE.
    ASSERT sy-subrc EQ 4.  " no costs changed
  ENDIF.

ENDFUNCTION.