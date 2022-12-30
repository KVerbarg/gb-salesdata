*&---------------------------------------------------------------------*
*& Report ZUCC_ANALYTICS_SDGEN_EXPORT
*&---------------------------------------------------------------------*
*& see https://github.com/KVerbarg/gb-salesdata
*&---------------------------------------------------------------------*
REPORT zucc_analytics_sdgen_export.

PARAMETERS p_year TYPE de_year DEFAULT '2015'.

* all days of the year
DATA l_from TYPE d.
DATA l_to TYPE d.
l_from = p_year && '0101'.
l_to = p_year && '1231'.

* read sales orders
SELECT
    salesdocument
    , salesdocumentitem
    , salesorganization
    , distributionchannel
    , division
    , material
    , soldtoparty
    , salesdocumentdate
    , orderquantity
    , orderquantityunit
    , itemgrossweight
    , itemweightunit
    , netamount
    , transactioncurrency
    , taxamount
    , costamount
    , subtotal1amount
    , plant
  FROM c_salesdocumentitemdex
  INTO TABLE @DATA(lt_data)
  WHERE salesdocumentdate BETWEEN @l_from AND @l_to
  AND sddocumentcategory = 'C'.

* Download directory
DATA l_upload TYPE string.
DATA l_download TYPE string.

CALL METHOD cl_gui_frontend_services=>get_upload_download_path
  CHANGING
    upload_path                 = l_upload
    download_path               = l_download
  EXCEPTIONS
    cntl_error                  = 1
    error_no_gui                = 2
    not_supported_by_gui        = 3
    gui_upload_download_path    = 4
    upload_download_path_failed = 5
    OTHERS                      = 6.
IF sy-subrc <> 0.
  MESSAGE ID 'ZUCC_ANALYTICS' TYPE 'E' NUMBER 000 WITH sy-subrc.
ENDIF.


* ask for filename
DATA l_filename TYPE string.
DATA l_path TYPE string.
DATA l_fullpath TYPE string.
DATA l_user_action TYPE i.

CALL METHOD cl_gui_frontend_services=>file_save_dialog
  EXPORTING
    window_title              = 'Save sales orders to...'
    default_extension         = 'tsv'
    default_file_name         = p_year && 'analytics.tsv'
*   with_encoding             =
    file_filter               = '*.tsv'
    initial_directory         = l_download
*   prompt_on_overwrite       = 'X'
  CHANGING
    filename                  = l_filename
    path                      = l_path
    fullpath                  = l_fullpath
    user_action               = l_user_action
*   file_encoding             =
  EXCEPTIONS
    cntl_error                = 1
    error_no_gui              = 2
    not_supported_by_gui      = 3
    invalid_default_file_name = 4
    OTHERS                    = 5.
IF sy-subrc <> 0.
  MESSAGE ID 'ZUCC_ANALYTICS' TYPE 'E' NUMBER 000 WITH sy-subrc.
ENDIF.

* User pressed cancel button
IF l_user_action EQ cl_gui_frontend_services=>action_cancel.
  EXIT.
ENDIF.

* download file
CALL METHOD cl_gui_frontend_services=>gui_download
  EXPORTING
*   bin_filesize            =
    filename                = l_fullpath
    filetype                = 'ASC'
*   append                  = SPACE
    write_field_separator   = ','
*   header                  = '00'
*   trunc_trailing_blanks   = SPACE
*   write_lf                = 'X'
*   col_select              = SPACE
*   col_select_mask         = SPACE
*   dat_mode                = SPACE
*   confirm_overwrite       = SPACE
*   no_auth_check           = SPACE
*   codepage                = SPACE
*   ignore_cerr             = ABAP_TRUE
*   replacement             = '#'
*   write_bom               = SPACE
*   trunc_trailing_blanks_eol = 'X'
*   wk1_n_format            = SPACE
*   wk1_n_size              = SPACE
*   wk1_t_format            = SPACE
*   wk1_t_size              = SPACE
*   show_transfer_status    = 'X'
*   fieldnames              =
*   write_lf_after_last_line  = 'X'
*   virus_scan_profile      = '/SCET/GUI_DOWNLOAD'
*  IMPORTING
*   filelength              =
  CHANGING
    data_tab                = lt_data
  EXCEPTIONS
    file_write_error        = 1
    no_batch                = 2
    gui_refuse_filetransfer = 3
    invalid_type            = 4
    no_authority            = 5
    unknown_error           = 6
    header_not_allowed      = 7
    separator_not_allowed   = 8
    filesize_not_allowed    = 9
    header_too_long         = 10
    dp_error_create         = 11
    dp_error_send           = 12
    dp_error_write          = 13
    unknown_dp_error        = 14
    access_denied           = 15
    dp_out_of_memory        = 16
    disk_full               = 17
    dp_timeout              = 18
    file_not_found          = 19
    dataprovider_exception  = 20
    control_flush_error     = 21
    not_supported_by_gui    = 22
    error_no_gui            = 23
    OTHERS                  = 24.
IF sy-subrc <> 0.
  MESSAGE ID 'ZUCC_ANALYTICS' TYPE 'E' NUMBER 000 WITH sy-subrc.
ENDIF.