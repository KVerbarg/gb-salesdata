*&---------------------------------------------------------------------*
*& Report ZUCC_ANALYTICS_SDGEN_IMPCOSTS
*&---------------------------------------------------------------------*
*& see https://github.com/KVerbarg/gb-salesdata
*&---------------------------------------------------------------------*
REPORT zucc_analytics_sdgen_impcosts.

*DELETE FROM zucc_analy_costs.

DATA lt_files TYPE filetable.
DATA ls_file LIKE LINE OF lt_files.
DATA lv_rc TYPE i.

* Select multiple files from local pc
CALL METHOD cl_gui_frontend_services=>file_open_dialog
  EXPORTING
    window_title            = 'Daten CSV'
    default_extension       = '.csv'
    default_filename        = 'costs.tsv'
*   file_filter             =
*   with_encoding           =
    initial_directory       = 'C:\Users\verbarg\Nextcloud\Forschung\2022 Reporting UCC\2022-12 GeneratorGB\gb-salesdata\data'
    multiselection          = 'X'
  CHANGING
    file_table              = lt_files
    rc                      = lv_rc
*   user_action             =
*   file_encoding           =
  EXCEPTIONS
    file_open_dialog_failed = 1
    cntl_error              = 2
    error_no_gui            = 3
    not_supported_by_gui    = 4
    OTHERS                  = 5.
IF sy-subrc <> 0.
  MESSAGE ID 'ZUCC_ANALYTICS' TYPE 'E' NUMBER 000 WITH sy-subrc.
ENDIF.

* Upload all these files
DATA lt_data_in TYPE TABLE OF zucc_analytics_costs_read.
LOOP AT lt_files INTO ls_file.

  DATA lv_file TYPE string.
  lv_file = ls_file-filename.

  WRITE: / 'Uploaing ', lv_file, '...'.

  " read file into itab
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_file
      filetype                = 'ASC'
      has_field_separator     = 'X'  " tabulator
*     header_length           = 0
*     read_by_line            = 'X'
*     dat_mode                = SPACE
*     codepage                = SPACE
*     ignore_cerr             = ABAP_TRUE
*     replacement             = '#'
*     virus_scan_profile      =
*  IMPORTING
*     filelength              =
*     header                  =
    CHANGING
      data_tab                = lt_data_in
*     isscanperformed         = SPACE
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZUCC_ANALYTICS' TYPE 'E' NUMBER 000 WITH sy-subrc.
  ENDIF.

  " Enhance data to comply with db table schema
  DATA ls_data_in LIKE LINE OF lt_data_in.
  DATA ls_data_out TYPE zucc_analy_costs.
  DATA lt_data_out TYPE TABLE OF zucc_analy_costs.
  CLEAR lt_data_out.
  LOOP AT lt_data_in INTO ls_data_in.
    MOVE-CORRESPONDING ls_data_in TO ls_data_out.
    ls_data_out-mandt = sy-mandt.
    APPEND ls_data_out TO lt_data_out.
  ENDLOOP.


  " Insert new data into db table and modify existig orders
  MODIFY zucc_analy_costs FROM TABLE lt_data_out.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZUCC_ANALYTICS' TYPE 'E' NUMBER 000 WITH sy-subrc.
  ENDIF.
  WRITE: / sy-dbcnt, 'rows processed.'.

ENDLOOP.


WRITE: / 'Finished.'.