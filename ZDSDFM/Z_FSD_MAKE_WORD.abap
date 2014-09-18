FUNCTION z_fsd_make_word.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(GUBUN) TYPE  C
*"  EXPORTING
*"     REFERENCE(RETURN) LIKE  SY-SUBRC
*"     VALUE(SAPWORKDIR) TYPE  SDOK_CHTRD
*"  TABLES
*"      FAX_H STRUCTURE  ZSSD_ACM_FAX_H OPTIONAL
*"      FAX_I STRUCTURE  ZSSD_ACM_FAX_I OPTIONAL
*"      REMI_H STRUCTURE  ZSSD_ACM_REMI_H OPTIONAL
*"      REMI_I STRUCTURE  ZSSD_ACM_REMI_I OPTIONAL
*"      WCI_H STRUCTURE  ZSSD_REC_NOTI_H OPTIONAL
*"      WCI_I STRUCTURE  ZSSD_REC_NOTI_I OPTIONAL
*"  EXCEPTIONS
*"      UPLOAD_FAIL
*"      DOWNLOAD_FAIL
*"----------------------------------------------------------------------

** Changed by Furong on 12/17/07
*CASE GUBUN.
*WHEN '1'. "FAX
*  FILENAME = 'c:\acm.rtf'.
*WHEN '2'. "REMI
*  FILENAME = 'c:\nor.rtf'.
*WHEN '3'. "WCI
*  FILENAME = 'c:\wci.rtf'.
*ENDCASE.

  CALL FUNCTION 'IW_C_GET_SAPWORKDIR'
    IMPORTING
      sapworkdir = sapworkdir.
*   ERROR_MSG        =
  .

  CASE gubun.
    WHEN '1'. "FAX
      CONCATENATE sapworkdir '\acm.rtf' INTO filename.
*  FILENAME = 'C:\Documents and Settings\acm.rtf'.
    WHEN '2'. "REMI
      CONCATENATE sapworkdir '\nor.rtf' INTO filename.
*  FILENAME = 'C:\Documents and Settings\nor.rtf'.
    WHEN '3'. "WCI
      CONCATENATE sapworkdir '\wci.rtf' INTO filename.
*  FILENAME = 'C:\Documents and Settings\wci.rtf'.
  ENDCASE.

*-<
*  DATA : l_filename TYPE string.
*  l_filename  =  filename.
*  CALL FUNCTION 'GUI_UPLOAD'
*    EXPORTING
*      filename                      = l_filename
*     filetype                      = 'ASC'
**   HAS_FIELD_SEPARATOR           = ' '
**   HEADER_LENGTH                 = 0
**   READ_BY_LINE                  = 'X'
**   DAT_MODE                      = ' '
**   CODEPAGE                      = ' '
**   IGNORE_CERR                   = ABAP_TRUE
**   REPLACEMENT                   = '#'
**   CHECK_BOM                     = ' '
**   VIRUS_SCAN_PROFILE            =
**   NO_AUTH_CHECK                 = ' '
** IMPORTING
**   FILELENGTH                    =
**   HEADER                        =
*    TABLES
*      data_tab                      = it_tab
*   EXCEPTIONS
*     file_open_error               = 1
*     file_read_error               = 2
*     no_batch                      = 3
*     gui_refuse_filetransfer       = 4
*     invalid_type                  = 5
*     no_authority                  = 6
*     unknown_error                 = 7
*     bad_data_format               = 8
*     header_not_allowed            = 9
*     separator_not_allowed         = 10
*     header_too_long               = 11
*     unknown_dp_error              = 12
*     access_denied                 = 13
*     dp_out_of_memory              = 14
*     disk_full                     = 15
*     dp_timeout                    = 16
*     OTHERS                        = 17
*            .
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.


*->

** End of change
  CALL FUNCTION 'UPLOAD'
   EXPORTING
*   CODEPAGE                      = ' '
     filename                      = filename
     filetype                      = 'ASC'
*   ITEM                          = ' '
*   FILEMASK_MASK                 = ' '
*   FILEMASK_TEXT                 = ' '
*   FILETYPE_NO_CHANGE            = ' '
*   FILEMASK_ALL                  = ' '
*   FILETYPE_NO_SHOW              = ' '
*   LINE_EXIT                     = ' '
*   USER_FORM                     = ' '
*   USER_PROG                     = ' '
*   SILENT                        = 'S'
* IMPORTING
*   FILESIZE                      =
*   CANCEL                        =
*   ACT_FILENAME                  =
*   ACT_FILETYPE                  =
    TABLES
      data_tab                      = it_tab
   EXCEPTIONS
     conversion_error              = 1
     invalid_table_width           = 2
     invalid_type                  = 3
     no_batch                      = 4
     unknown_error                 = 5
     gui_refuse_filetransfer       = 6
     OTHERS                        = 7
            .
*IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*
*CALL FUNCTION 'WS_UPLOAD'
*  EXPORTING
**   CODEPAGE                      = ' '
*    FILENAME                      = FILENAME
*    FILETYPE                      = 'ASC'
**   HEADLEN                       = ' '
**   LINE_EXIT                     = ' '
**   TRUNCLEN                      = ' '
**   USER_FORM                     = ' '
**   USER_PROG                     = ' '
**   DAT_D_FORMAT                  = ' '
** IMPORTING
**   FILELENGTH                    =
*  TABLES
*    DATA_TAB                      = IT_TAB
*  EXCEPTIONS
*    CONVERSION_ERROR              = 1
*    FILE_OPEN_ERROR               = 2
*    FILE_READ_ERROR               = 3
*    INVALID_TYPE                  = 4
*    NO_BATCH                      = 5
*    UNKNOWN_ERROR                 = 6
*    INVALID_TABLE_WIDTH           = 7
*    GUI_REFUSE_FILETRANSFER       = 8
*    CUSTOMER_ERROR                = 9
*    OTHERS                        = 10.
  IF sy-subrc <> 0.
    RAISE upload_fail.
  ENDIF.

  CASE gubun.
    WHEN '1'. "FAX
      PERFORM make_word_fax  TABLES fax_h fax_i.
    WHEN '2'. "REMI
      PERFORM make_word_remi TABLES remi_h remi_i.
    WHEN '3'. "WCI
      PERFORM make_word_wci TABLES wci_h wci_i.
  ENDCASE.


  CALL FUNCTION 'DOWNLOAD'
   EXPORTING
*   BIN_FILESIZE                  = ' '
*   CODEPAGE                      = ' '
     filename                      = filename
     filetype                      = 'ASC'
*   ITEM                          = ' '
*   MODE                          = ' '
*   WK1_N_FORMAT                  = ' '
*   WK1_N_SIZE                    = ' '
*   WK1_T_FORMAT                  = ' '
*   WK1_T_SIZE                    = ' '
*   FILEMASK_MASK                 = ' '
*   FILEMASK_TEXT                 = ' '
*   FILETYPE_NO_CHANGE            = ' '
*   FILEMASK_ALL                  = ' '
*   FILETYPE_NO_SHOW              = ' '
*   SILENT                        = 'S'
*   COL_SELECT                    = ' '
*   COL_SELECTMASK                = ' '
*   NO_AUTH_CHECK                 = ' '
* IMPORTING
*   ACT_FILENAME                  =
*   ACT_FILETYPE                  =
*   FILESIZE                      =
*   CANCEL                        =
    TABLES
      data_tab                      = it_tab
*   FIELDNAMES                    =
   EXCEPTIONS
     invalid_filesize              = 1
     invalid_table_width           = 2
     invalid_type                  = 3
     no_batch                      = 4
     unknown_error                 = 5
     gui_refuse_filetransfer       = 6
     customer_error                = 7
     OTHERS                        = 8
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  CALL FUNCTION 'WS_DOWNLOAD'
*    EXPORTING
**   BIN_FILESIZE                  = ' '
**   CODEPAGE                      = ' '
*      FILENAME                      = FILENAME
*      FILETYPE                      = 'ASC'
**   MODE                          = ' '
**   WK1_N_FORMAT                  = ' '
**   WK1_N_SIZE                    = ' '
**   WK1_T_FORMAT                  = ' '
**   WK1_T_SIZE                    = ' '
**   COL_SELECT                    = ' '
**   COL_SELECTMASK                = ' '
**   NO_AUTH_CHECK                 = ' '
** IMPORTING
**   FILELENGTH                    =
*    TABLES
*      DATA_TAB                      = IT_TAB
**   FIELDNAMES                    =
*    EXCEPTIONS
*      FILE_OPEN_ERROR               = 1
*      FILE_WRITE_ERROR              = 2
*      INVALID_FILESIZE              = 3
*      INVALID_TYPE                  = 4
*      NO_BATCH                      = 5
*      UNKNOWN_ERROR                 = 6
*      INVALID_TABLE_WIDTH           = 7
*      GUI_REFUSE_FILETRANSFER       = 8
*      CUSTOMER_ERROR                = 9
*      OTHERS                        = 10.
  IF sy-subrc <> 0.
    RAISE download_fail.
  ENDIF.
  return = sy-subrc.

ENDFUNCTION.



*---------------------------------------------------------------------*
*       FORM MAKE_WORD_FAX                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FAX_H                                                         *
*  -->  FAX_I                                                         *
*---------------------------------------------------------------------*
FORM make_word_fax TABLES fax_h STRUCTURE zssd_acm_fax_h
                          fax_i STRUCTURE zssd_acm_fax_i.
  DATA : w_tot1 LIKE fax_i-zacaa,
         w_tot2 LIKE fax_i-zacaa,
         w_tot3 LIKE fax_i-zacaa.
  DATA:  l_zcmno(11).

  READ TABLE fax_h INDEX 1.

  PERFORM get_email_address USING fax_h-zcdst.

  SELECT SINGLE * FROM kna1 WHERE kunnr = fax_h-zcdst.

  LOOP AT fax_i.
    w_tot1 = w_tot1 + fax_i-zscaa.
    w_tot2 = w_tot2 + fax_i-zacaa.
    w_tot3 = w_tot3 + fax_i-zcbaa.
  ENDLOOP.

  LOOP AT it_tab.
    SEARCH it_tab-line FOR 'h0101'. "REF
    IF sy-subrc EQ 0.
      REPLACE 'h0101' WITH fax_h-zcmno INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'h0201'. "TO
    IF sy-subrc EQ 0.
      REPLACE 'h0201' WITH kna1-name1+0(25) INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'h0301'. "ATTN
    IF sy-subrc EQ 0.
      READ TABLE mail_addr INDEX 1.
      IF sy-subrc = 0.
        CONCATENATE mail_addr-namev mail_addr-name1
                    INTO w_text SEPARATED BY space.
        REPLACE 'h0301' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'h0301' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'h0401'. "CC
    IF sy-subrc EQ 0.
      READ TABLE mail_addr INDEX 2.
      IF sy-subrc = 0.
        CONCATENATE mail_addr-namev mail_addr-name1
                    INTO w_text SEPARATED BY space.
        REPLACE 'h0401' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'h0401' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'h0501'. "FROM
    IF sy-subrc EQ 0.
      REPLACE 'h0501' WITH 'HMMA' INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'h0102'. "EMAIL
    IF sy-subrc EQ 0.
      READ TABLE mail_addr INDEX 1.
      IF sy-subrc = 0.
        REPLACE 'h0102' WITH mail_addr-smtp_addr+0(25)
                                INTO it_tab-line.
      ELSE.
        REPLACE 'h0102' WITH '' INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'h0202'. "DATE
    IF sy-subrc EQ 0.
      REPLACE 'h0202' WITH fax_h-zfxdt INTO it_tab-line.
    ENDIF.
*
    SEARCH it_tab-line FOR 'pycr'. "Unit
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 1.
      IF sy-subrc = 0.
        REPLACE 'pycr' WITH fax_i-zpycr INTO it_tab-line.
      ELSE.
        REPLACE 'pycr' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
*
    SEARCH it_tab-line FOR 'i0101'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 1.
      IF sy-subrc = 0.
        REPLACE 'i0101' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0101' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0102'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 1.
      IF sy-subrc = 0.
        REPLACE 'i0102' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0102' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0103'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 1.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i0103' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0103' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0104'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 1.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i0104' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0104' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0105'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 1.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i0105' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0105' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i0201'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 2.
      IF sy-subrc = 0.
        REPLACE 'i0201' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0201' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0202'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 2.
      IF sy-subrc = 0.
        REPLACE 'i0202' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0202' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0203'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 2.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i0203' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0203' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0204'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 2.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i0204' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0204' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0205'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 2.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i0205' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0205' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i0301'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 3.
      IF sy-subrc = 0.
        REPLACE 'i0301' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0301' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0302'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 3.
      IF sy-subrc = 0.
        REPLACE 'i0302' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0302' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0303'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 3.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i0303' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0303' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0304'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 3.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i0304' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0304' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0305'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 3.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i0305' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0305' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i0401'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 4.
      IF sy-subrc = 0.
        REPLACE 'i0401' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0401' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0402'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 4.
      IF sy-subrc = 0.
        REPLACE 'i0402' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0402' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0403'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 4.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i0403' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0403' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0404'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 4.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i0404' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0404' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0405'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 4.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i0405' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0405' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
*
    SEARCH it_tab-line FOR 'i0501'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 5.
      IF sy-subrc = 0.
        REPLACE 'i0501' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0501' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0502'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 5.
      IF sy-subrc = 0.
        REPLACE 'i0502' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0502' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0503'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 5.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i0503' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0503' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0504'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 5.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i0504' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0504' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0505'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 5.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i0505' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0505' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i0601'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 6.
      IF sy-subrc = 0.
        REPLACE 'i0601' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0601' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0602'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 6.
      IF sy-subrc = 0.
        REPLACE 'i0602' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0602' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0603'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 6.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i0603' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0603' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0604'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 6.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i0604' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0604' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0605'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 6.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i0605' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0605' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
*
    SEARCH it_tab-line FOR 'i0701'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 7.
      IF sy-subrc = 0.
        REPLACE 'i0701' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0701' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0702'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 7.
      IF sy-subrc = 0.
        REPLACE 'i0702' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0702' WITH ''          INTO it_tab-line
.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0703'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 7.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i0703' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0703' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0704'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 7.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i0704' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0704' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0705'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 7.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i0705' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0705' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i0801'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 8.
      IF sy-subrc = 0.
        REPLACE 'i0801' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0801' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0802'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 8.
      IF sy-subrc = 0.
        REPLACE 'i0802' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0802' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0803'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 8.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i0803' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0803' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0804'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 8.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i0804' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0804' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0805'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 8.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i0805' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0805' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i0901'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 9.
      IF sy-subrc = 0.
        REPLACE 'i0901' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0901' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0902'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 9.
      IF sy-subrc = 0.
        REPLACE 'i0902' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0902' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0903'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 9.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i0903' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0903' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0904'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 9.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i0904' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0904' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0905'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 9.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i0905' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0905' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i1001'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 10.
      IF sy-subrc = 0.
        REPLACE 'i1001' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i1001' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1002'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 10.
      IF sy-subrc = 0.
        REPLACE 'i1002' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i1002' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1003'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 10.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i1003' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1003' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1004'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 10.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i1004' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1004' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1005'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 10.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i1005' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1005' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i1101'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 11.
      IF sy-subrc = 0.
        REPLACE 'i1101' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i1101' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1102'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 11.
      IF sy-subrc = 0.
        REPLACE 'i1102' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i1102' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1103'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 11.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i1103' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1103' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1104'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 11.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i1104' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1104' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1105'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 11.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i1105' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1105' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i1201'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 12.
      IF sy-subrc = 0.
        REPLACE 'i1201' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i1201' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1202'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 12.
      IF sy-subrc = 0.
        REPLACE 'i1202' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i1202' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1203'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 12.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i1203' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1203' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1204'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 12.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i1204' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1204' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1205'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 12.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i1205' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1205' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i1301'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 13.
      IF sy-subrc = 0.
        REPLACE 'i1301' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i1301' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1302'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 13.
      IF sy-subrc = 0.
        REPLACE 'i1302' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i1302' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1303'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 13.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i1303' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1303' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1304'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 13.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i1304' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1304' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1305'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 13.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i1305' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1305' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i1401'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 14.
      IF sy-subrc = 0.
        REPLACE 'i1401' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i1401' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1402'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 14.
      IF sy-subrc = 0.
        REPLACE 'i1402' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i1402' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1403'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 14.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i1403' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1403' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1404'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 14.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i1404' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1404' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1405'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 14.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i1405' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1405' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i1501'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 15.
      IF sy-subrc = 0.
        REPLACE 'i1501' WITH fax_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i1501' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1502'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 15.
      IF sy-subrc = 0.
        REPLACE 'i1502' WITH fax_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i1502' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1503'. "SUBMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 15.
      IF sy-subrc = 0.
        WRITE fax_i-zscaa TO w_text.
        REPLACE 'i1503' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1503' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1504'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 15.
      IF sy-subrc = 0.
        WRITE fax_i-zacaa TO w_text.
        REPLACE 'i1504' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1504' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1505'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE fax_i INDEX 15.
      IF sy-subrc = 0.
        WRITE fax_i-zcbaa TO w_text.
        REPLACE 'i1505' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1505' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
*
    SEARCH it_tab-line FOR 'i1603'. "SUBMITTED AMOUNT TOTAL
    IF sy-subrc EQ 0.
      WRITE w_tot1 TO w_text.
      REPLACE 'i1603' WITH w_text INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'i1604'. "APPROVED AMOUNT TOTAL
    IF sy-subrc EQ 0.
      WRITE w_tot2 TO w_text.
      REPLACE 'i1604' WITH w_text INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'i1605'. "CHARGE BACK AMOUNT TOTAL
    IF sy-subrc EQ 0.
      WRITE w_tot3 TO w_text.
      REPLACE 'i1605' WITH w_text INTO it_tab-line.
    ENDIF.

    MODIFY it_tab.
  ENDLOOP.

  DATA: w_len TYPE i.
  w_len = strlen( filename ) - 4.

  CONCATENATE filename+0(w_len) '_' fax_h-zcmno filename+w_len(4)
              INTO filename.

*  CONCATENATE FILENAME+0(6) '_' FAX_H-ZCMNO FILENAME+6(4)
*              INTO FILENAME.
ENDFORM.                    "MAKE_WORD_FAX



*---------------------------------------------------------------------*
*       FORM MAKE_WORD_REMI                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  REMI_H                                                        *
*  -->  REMI_I                                                        *
*---------------------------------------------------------------------*
FORM make_word_remi TABLES remi_h STRUCTURE zssd_acm_remi_h
                           remi_i STRUCTURE zssd_acm_remi_i.
  DATA : w_tot1 LIKE remi_i-zacaa,
         w_tot2 LIKE remi_i-zacaa,
         w_tot3 LIKE remi_i-zacaa.

  DATA: l_zcmno(11).
  READ TABLE remi_h INDEX 1.

  PERFORM get_email_address USING remi_h-zcdst.

  SELECT SINGLE * FROM kna1 WHERE kunnr = remi_h-zcdst.

  LOOP AT remi_i.
    w_tot1 = w_tot1 + remi_i-zacaa.
    w_tot2 = w_tot2 + remi_i-zcbaa.
    w_tot3 = w_tot3 + remi_i-zremi.
  ENDLOOP.

  LOOP AT it_tab.
    SEARCH it_tab-line FOR 'h0101'. "REF
    IF sy-subrc EQ 0.
      REPLACE 'h0101' WITH remi_h-zcmno INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'h0201'. "TO
    IF sy-subrc EQ 0.
      REPLACE 'h0201' WITH kna1-name1+0(25) INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'h0301'. "ATTN
    IF sy-subrc EQ 0.
      READ TABLE mail_addr INDEX 1.
      IF sy-subrc = 0.
        CONCATENATE mail_addr-namev mail_addr-name1
                    INTO w_text SEPARATED BY space.
        REPLACE 'h0301' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'h0301' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'h0401'. "CC
    IF sy-subrc EQ 0.
      READ TABLE mail_addr INDEX 2.
      IF sy-subrc = 0.
        CONCATENATE mail_addr-namev mail_addr-name1
                    INTO w_text SEPARATED BY space.
        REPLACE 'h0401' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'h0401' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'h0501'. "FROM
    IF sy-subrc EQ 0.
      REPLACE 'h0501' WITH 'HMMA' INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'h0102'. "EMAIL
    IF sy-subrc EQ 0.
      READ TABLE mail_addr INDEX 1.
      IF sy-subrc = 0.
        REPLACE 'h0102' WITH mail_addr-smtp_addr+0(25)
                                INTO it_tab-line.
      ELSE.
        REPLACE 'h0102' WITH '' INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'h0202'. "DATE
    IF sy-subrc EQ 0.
      REPLACE 'h0202' WITH remi_h-zrtdt INTO it_tab-line.
    ENDIF.
*
    SEARCH it_tab-line FOR 'pycr'. "Unit
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 1.
      IF sy-subrc = 0.
        REPLACE 'pycr' WITH remi_i-zpycr INTO it_tab-line.
      ELSE.
        REPLACE 'pycr' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
*
    SEARCH it_tab-line FOR 'i0101'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 1.
      IF sy-subrc = 0.
        REPLACE 'i0101' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0101' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0102'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 1.
      IF sy-subrc = 0.
        REPLACE 'i0102' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0102' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0103'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 1.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i0103' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0103' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0104'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 1.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i0104' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0104' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0105'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 1.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i0105' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0105' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i0201'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 2.
      IF sy-subrc = 0.
        REPLACE 'i0201' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0201' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0202'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 2.
      IF sy-subrc = 0.
        REPLACE 'i0202' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0202' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0203'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 2.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i0203' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0203' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0204'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 2.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i0204' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0204' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0205'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 2.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i0205' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0205' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i0301'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 3.
      IF sy-subrc = 0.
        REPLACE 'i0301' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0301' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0302'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 3.
      IF sy-subrc = 0.
        REPLACE 'i0302' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0302' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0303'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 3.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i0303' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0303' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0304'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 3.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i0304' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0304' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0305'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 3.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i0305' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0305' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i0401'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 4.
      IF sy-subrc = 0.
        REPLACE 'i0401' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0401' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0402'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 4.
      IF sy-subrc = 0.
        REPLACE 'i0402' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0402' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0403'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 4.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i0403' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0403' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0404'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 4.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i0404' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0404' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0405'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 4.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i0405' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0405' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i0501'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 5.
      IF sy-subrc = 0.
        REPLACE 'i0501' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0501' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0502'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 5.
      IF sy-subrc = 0.
        REPLACE 'i0502' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0502' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0503'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 5.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i0503' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0503' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0504'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 5.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i0504' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0504' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0505'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 5.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i0505' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0505' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i0601'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 6.
      IF sy-subrc = 0.
        REPLACE 'i0601' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0601' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0602'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 6.
      IF sy-subrc = 0.
        REPLACE 'i0602' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0602' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0603'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 6.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i0603' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0603' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0604'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 6.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i0604' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0604' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0605'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 6.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i0605' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0605' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i0701'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 7.
      IF sy-subrc = 0.
        REPLACE 'i0701' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0701' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0702'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 7.
      IF sy-subrc = 0.
        REPLACE 'i0702' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0702' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0703'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 7.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i0703' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0703' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0704'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 7.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i0704' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0704' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0705'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 7.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i0705' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0705' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i0801'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 8.
      IF sy-subrc = 0.
        REPLACE 'i0801' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0801' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0802'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 8.
      IF sy-subrc = 0.
        REPLACE 'i0802' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0802' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0803'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 8.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i0803' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0803' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0804'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 8.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i0804' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0804' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0805'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 8.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i0805' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0805' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i0901'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 9.
      IF sy-subrc = 0.
        REPLACE 'i0901' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i0901' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0902'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 9.
      IF sy-subrc = 0.
        REPLACE 'i0902' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i0902' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0903'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 9.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i0903' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0903' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0904'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 9.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i0904' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0904' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0905'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 9.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i0905' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0905' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i1001'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 10.
      IF sy-subrc = 0.
        REPLACE 'i1001' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i1001' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1002'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 10.
      IF sy-subrc = 0.
        REPLACE 'i1002' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i1002' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1003'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 10.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i1003' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1003' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1004'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 10.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i1004' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1004' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1005'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 10.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i1005' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1005' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i1101'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 11.
      IF sy-subrc = 0.
        REPLACE 'i1101' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i1101' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1102'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 11.
      IF sy-subrc = 0.
        REPLACE 'i1102' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i1102' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1103'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 11.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i1103' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1103' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1104'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 11.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i1104' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1104' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1105'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 11.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i1105' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1105' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i1201'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 12.
      IF sy-subrc = 0.
        REPLACE 'i1201' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i1201' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1202'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 12.
      IF sy-subrc = 0.
        REPLACE 'i1202' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i1202' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1203'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 12.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i1203' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1203' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1204'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 12.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i1204' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1204' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1205'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 12.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i1205' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1205' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i1301'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 13.
      IF sy-subrc = 0.
        REPLACE 'i1301' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i1301' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1302'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 13.
      IF sy-subrc = 0.
        REPLACE 'i1302' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i1302' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1303'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 13.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i1303' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1303' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1304'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 13.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i1304' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1304' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1305'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 13.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i1305' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1305' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i1401'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 14.
      IF sy-subrc = 0.
        REPLACE 'i1401' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i1401' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1402'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 14.
      IF sy-subrc = 0.
        REPLACE 'i1402' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i1402' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1403'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 14.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i1403' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1403' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1404'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 14.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i1404' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1404' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1405'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 14.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i1405' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1405' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

    SEARCH it_tab-line FOR 'i1501'. "ACL NO
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 15.
      IF sy-subrc = 0.
        REPLACE 'i1501' WITH remi_i-zacln INTO it_tab-line.
      ELSE.
        REPLACE 'i1501' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1502'. "RECEIPT DATE
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 15.
      IF sy-subrc = 0.
        REPLACE 'i1502' WITH remi_i-zacdt INTO it_tab-line.
      ELSE.
        REPLACE 'i1502' WITH ''           INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1503'. "APPROVED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 15.
      IF sy-subrc = 0.
        WRITE remi_i-zacaa TO w_text.
        REPLACE 'i1503' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1503' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1504'. "CHARGE BACK AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 15.
      IF sy-subrc = 0.
        WRITE remi_i-zcbaa TO w_text.
        REPLACE 'i1504' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1504' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1505'. "REMITTED AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE remi_i INDEX 15.
      IF sy-subrc = 0.
        WRITE remi_i-zremi TO w_text.
        REPLACE 'i1505' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1505' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.

*-< Victor commented on 05.20.2014
** Changed by Furong on 09/10/09
*    SEARCH it_tab-line FOR 'i1601'. "ACL NO
*    IF sy-subrc EQ 0.
*      READ TABLE remi_i INDEX 16.
*      IF sy-subrc = 0.
*        REPLACE 'i1601' WITH remi_i-zacln INTO it_tab-line.
*      ELSE.
*        REPLACE 'i1601' WITH ''           INTO it_tab-line.
*      ENDIF.
*    ENDIF.
*    SEARCH it_tab-line FOR 'i1602'. "RECEIPT DATE
*    IF sy-subrc EQ 0.
*      READ TABLE remi_i INDEX 16.
*      IF sy-subrc = 0.
*        REPLACE 'i1602' WITH remi_i-zacdt INTO it_tab-line.
*      ELSE.
*        REPLACE 'i1602' WITH ''           INTO it_tab-line.
*      ENDIF.
*    ENDIF.
*    SEARCH it_tab-line FOR 'i1603'. "APPROVED AMOUNT
*    IF sy-subrc EQ 0.
*      READ TABLE remi_i INDEX 16.
*      IF sy-subrc = 0.
*        WRITE remi_i-zacaa TO w_text.
*        REPLACE 'i1603' WITH w_text INTO it_tab-line.
*      ELSE.
*        REPLACE 'i1603' WITH ''     INTO it_tab-line.
*      ENDIF.
*    ENDIF.
*    SEARCH it_tab-line FOR 'i1604'. "CHARGE BACK AMOUNT
*    IF sy-subrc EQ 0.
*      READ TABLE remi_i INDEX 15.
*      IF sy-subrc = 0.
*        WRITE remi_i-zcbaa TO w_text.
*        REPLACE 'i1604' WITH w_text INTO it_tab-line.
*      ELSE.
*        REPLACE 'i1604' WITH ''     INTO it_tab-line.
*      ENDIF.
*    ENDIF.
*    SEARCH it_tab-line FOR 'i1605'. "REMITTED AMOUNT
*    IF sy-subrc EQ 0.
*      READ TABLE remi_i INDEX 16.
*      IF sy-subrc = 0.
*        WRITE remi_i-zremi TO w_text.
*        REPLACE 'i1605' WITH w_text INTO it_tab-line.
*      ELSE.
*        REPLACE 'i1605' WITH ''     INTO it_tab-line.
*      ENDIF.
*    ENDIF.
*
*    SEARCH it_tab-line FOR 'i1701'. "ACL NO
*    IF sy-subrc EQ 0.
*      READ TABLE remi_i INDEX 17.
*      IF sy-subrc = 0.
*        REPLACE 'i1701' WITH remi_i-zacln INTO it_tab-line.
*      ELSE.
*        REPLACE 'i1701' WITH ''           INTO it_tab-line.
*      ENDIF.
*    ENDIF.
*    SEARCH it_tab-line FOR 'i1702'. "RECEIPT DATE
*    IF sy-subrc EQ 0.
*      READ TABLE remi_i INDEX 17.
*      IF sy-subrc = 0.
*        REPLACE 'i1702' WITH remi_i-zacdt INTO it_tab-line.
*      ELSE.
*        REPLACE 'i1702' WITH ''           INTO it_tab-line.
*      ENDIF.
*    ENDIF.
*    SEARCH it_tab-line FOR 'i1703'. "APPROVED AMOUNT
*    IF sy-subrc EQ 0.
*      READ TABLE remi_i INDEX 17.
*      IF sy-subrc = 0.
*        WRITE remi_i-zacaa TO w_text.
*        REPLACE 'i1703' WITH w_text INTO it_tab-line.
*      ELSE.
*        REPLACE 'i1703' WITH ''     INTO it_tab-line.
*      ENDIF.
*    ENDIF.
*    SEARCH it_tab-line FOR 'i1704'. "CHARGE BACK AMOUNT
*    IF sy-subrc EQ 0.
*      READ TABLE remi_i INDEX 17.
*      IF sy-subrc = 0.
*        WRITE remi_i-zcbaa TO w_text.
*        REPLACE 'i1704' WITH w_text INTO it_tab-line.
*      ELSE.
*        REPLACE 'i1704' WITH ''     INTO it_tab-line.
*      ENDIF.
*    ENDIF.
*    SEARCH it_tab-line FOR 'i1705'. "REMITTED AMOUNT
*    IF sy-subrc EQ 0.
*      READ TABLE remi_i INDEX 17.
*      IF sy-subrc = 0.
*        WRITE remi_i-zremi TO w_text.
*        REPLACE 'i1705' WITH w_text INTO it_tab-line.
*      ELSE.
*        REPLACE 'i1705' WITH ''     INTO it_tab-line.
*      ENDIF.
*    ENDIF.
*->

*    SEARCH IT_TAB-LINE FOR 'i1801'. "ACL NO
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 18.
*      IF SY-SUBRC = 0.
*        REPLACE 'i1801' WITH REMI_I-ZACLN INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i1801' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i1802'. "RECEIPT DATE
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 18.
*      IF SY-SUBRC = 0.
*        REPLACE 'i1802' WITH REMI_I-ZACDT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i1802' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i1803'. "APPROVED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 18.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZACAA TO W_TEXT.
*        REPLACE 'i1803' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i1803' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i1804'. "CHARGE BACK AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 18.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZCBAA TO W_TEXT.
*        REPLACE 'i1804' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i1804' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i1805'. "REMITTED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 18.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZREMI TO W_TEXT.
*        REPLACE 'i1805' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i1805' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*
*    SEARCH IT_TAB-LINE FOR 'i1901'. "ACL NO
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 19.
*      IF SY-SUBRC = 0.
*        REPLACE 'i1901' WITH REMI_I-ZACLN INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i1901' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i1902'. "RECEIPT DATE
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 19.
*      IF SY-SUBRC = 0.
*        REPLACE 'i1902' WITH REMI_I-ZACDT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i1902' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i1903'. "APPROVED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 19.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZACAA TO W_TEXT.
*        REPLACE 'i1903' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i1903' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i1904'. "CHARGE BACK AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 19.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZCBAA TO W_TEXT.
*        REPLACE 'i1904' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i1904' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i1905'. "REMITTED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 19.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZREMI TO W_TEXT.
*        REPLACE 'i1905' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i1905' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*
*    SEARCH IT_TAB-LINE FOR 'i2001'. "ACL NO
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 20.
*      IF SY-SUBRC = 0.
*        REPLACE 'i2001' WITH REMI_I-ZACLN INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2001' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2002'. "RECEIPT DATE
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 20.
*      IF SY-SUBRC = 0.
*        REPLACE 'i2002' WITH REMI_I-ZACDT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2002' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2003'. "APPROVED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 20.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZACAA TO W_TEXT.
*        REPLACE 'i2003' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2003' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2004'. "CHARGE BACK AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 20.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZCBAA TO W_TEXT.
*        REPLACE 'i2004' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2004' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2005'. "REMITTED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 20.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZREMI TO W_TEXT.
*        REPLACE 'i2005' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2005' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*
*    SEARCH IT_TAB-LINE FOR 'i2101'. "ACL NO
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 21.
*      IF SY-SUBRC = 0.
*        REPLACE 'i2101' WITH REMI_I-ZACLN INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2101' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2102'. "RECEIPT DATE
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 21.
*      IF SY-SUBRC = 0.
*        REPLACE 'i2102' WITH REMI_I-ZACDT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2102' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2103'. "APPROVED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 21.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZACAA TO W_TEXT.
*        REPLACE 'i2103' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2103' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2104'. "CHARGE BACK AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 21.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZCBAA TO W_TEXT.
*        REPLACE 'i2104' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2104' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2105'. "REMITTED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 21.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZREMI TO W_TEXT.
*        REPLACE 'i2105' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2105' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*
*    SEARCH IT_TAB-LINE FOR 'i2201'. "ACL NO
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 22.
*      IF SY-SUBRC = 0.
*        REPLACE 'i2201' WITH REMI_I-ZACLN INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2201' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2202'. "RECEIPT DATE
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 22.
*      IF SY-SUBRC = 0.
*        REPLACE 'i2202' WITH REMI_I-ZACDT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2202' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2203'. "APPROVED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 22.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZACAA TO W_TEXT.
*        REPLACE 'i2203' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2203' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2204'. "CHARGE BACK AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 22.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZCBAA TO W_TEXT.
*        REPLACE 'i2204' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2204' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2205'. "REMITTED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 22.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZREMI TO W_TEXT.
*        REPLACE 'i2205' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2205' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*
*
*    SEARCH IT_TAB-LINE FOR 'i2301'. "ACL NO
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 23.
*      IF SY-SUBRC = 0.
*        REPLACE 'i2301' WITH REMI_I-ZACLN INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2301' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2302'. "RECEIPT DATE
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 23.
*      IF SY-SUBRC = 0.
*        REPLACE 'i2302' WITH REMI_I-ZACDT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2302' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2303'. "APPROVED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 23.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZACAA TO W_TEXT.
*        REPLACE 'i2303' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2303' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2304'. "CHARGE BACK AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 23.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZCBAA TO W_TEXT.
*        REPLACE 'i2304' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2304' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2305'. "REMITTED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 23.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZREMI TO W_TEXT.
*        REPLACE 'i2305' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2305' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*
*    SEARCH IT_TAB-LINE FOR 'i2401'. "ACL NO
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 24.
*      IF SY-SUBRC = 0.
*        REPLACE 'i2401' WITH REMI_I-ZACLN INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2401' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2402'. "RECEIPT DATE
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 24.
*      IF SY-SUBRC = 0.
*        REPLACE 'i2402' WITH REMI_I-ZACDT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2402' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2403'. "APPROVED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 24.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZACAA TO W_TEXT.
*        REPLACE 'i2403' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2403' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2404'. "CHARGE BACK AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 24.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZCBAA TO W_TEXT.
*        REPLACE 'i2404' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2404' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2405'. "REMITTED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 24.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZREMI TO W_TEXT.
*        REPLACE 'i2405' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2405' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*
*    SEARCH IT_TAB-LINE FOR 'i2501'. "ACL NO
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 25.
*      IF SY-SUBRC = 0.
*        REPLACE 'i2501' WITH REMI_I-ZACLN INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2501' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2502'. "RECEIPT DATE
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 25.
*      IF SY-SUBRC = 0.
*        REPLACE 'i2502' WITH REMI_I-ZACDT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2502' WITH ''           INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2503'. "APPROVED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 25.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZACAA TO W_TEXT.
*        REPLACE 'i2503' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2503' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2504'. "CHARGE BACK AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 25.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZCBAA TO W_TEXT.
*        REPLACE 'i2504' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2504' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i2505'. "REMITTED AMOUNT
*    IF SY-SUBRC EQ 0.
*      READ TABLE REMI_I INDEX 25.
*      IF SY-SUBRC = 0.
*        WRITE REMI_I-ZREMI TO W_TEXT.
*        REPLACE 'i2505' WITH W_TEXT INTO IT_TAB-LINE.
*      ELSE.
*        REPLACE 'i2505' WITH ''     INTO IT_TAB-LINE.
*      ENDIF.
*    ENDIF.

    SEARCH it_tab-line FOR 'i1603'. "APPROVED AMOUNT TOTAL
    IF sy-subrc EQ 0.
      WRITE w_tot1 TO w_text.
      REPLACE 'i1603' WITH w_text INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'i1604'. "CHARGE BACK AMOUNT TOTAL
    IF sy-subrc EQ 0.
      WRITE w_tot2 TO w_text.
      REPLACE 'i1604' WITH w_text INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'i1605'. "REMITTED AMOUNT TOTAL
    IF sy-subrc EQ 0.
      WRITE w_tot3 TO w_text.
      REPLACE 'i1605' WITH w_text INTO it_tab-line.
    ENDIF.

*    SEARCH IT_TAB-LINE FOR 'i1603'. "APPROVED AMOUNT TOTAL
*    IF SY-SUBRC EQ 0.
*      WRITE W_TOT1 TO W_TEXT.
*      REPLACE 'i1603' WITH W_TEXT INTO IT_TAB-LINE.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i1604'. "CHARGE BACK AMOUNT TOTAL
*    IF SY-SUBRC EQ 0.
*      WRITE W_TOT2 TO W_TEXT.
*      REPLACE 'i1604' WITH W_TEXT INTO IT_TAB-LINE.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 'i1605'. "REMITTED AMOUNT TOTAL
*    IF SY-SUBRC EQ 0.
*      WRITE W_TOT3 TO W_TEXT.
*      REPLACE 'i1605' WITH W_TEXT INTO IT_TAB-LINE.
*    ENDIF.

** End of change


** Changed by Furong Wang on 02/01/2007
*    SEARCH IT_TAB-LINE FOR 't0101'. "
*    IF SY-SUBRC EQ 0.
*      REPLACE 't0101' WITH ''           INTO IT_TAB-LINE.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 't0201'. "
*    IF SY-SUBRC EQ 0.
*      REPLACE 't0201' WITH ''           INTO IT_TAB-LINE.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 't0301'. "
*    IF SY-SUBRC EQ 0.
*      REPLACE 't0301' WITH ''           INTO IT_TAB-LINE.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 't0401'. "
*    IF SY-SUBRC EQ 0.
*      REPLACE 't0401' WITH REMI_H-ZRTDT INTO IT_TAB-LINE.
*    ENDIF.

    SEARCH it_tab-line FOR 't0101'. "
    IF sy-subrc EQ 0.
      IF kna1-kunnr = 'B06AA'.
        REPLACE 't0101' WITH 'Bank of Nova Scotia'
             INTO it_tab-line.
      ELSEIF kna1-kunnr = 'B28AA'.
        REPLACE 't0101' WITH 'Bank of America'
             INTO it_tab-line.
      ELSE.
        REPLACE 't0101' WITH '' INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 't0201'. "
    IF sy-subrc EQ 0.
      IF kna1-kunnr = 'B06AA'.
        REPLACE 't0201' WITH 'Hyundai Auto Canada'
             INTO it_tab-line.
      ELSEIF kna1-kunnr = 'B28AA'.
        REPLACE 't0201' WITH 'Hyundai Motor America'
             INTO it_tab-line.
      ELSE.
        REPLACE 't0201' WITH '' INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 't0301'. "
    IF sy-subrc EQ 0.
      IF kna1-kunnr = 'B06AA'.
        REPLACE 't0301' WITH '1060-11'
             INTO it_tab-line.
      ELSEIF kna1-kunnr = 'B28AA'.
        REPLACE 't0301' WITH '7188811027'
             INTO it_tab-line.
      ELSE.
        REPLACE 't0301' WITH '' INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 't0401'. "
    IF sy-subrc EQ 0.
      REPLACE 't0401' WITH remi_h-zrtdt INTO it_tab-line.
    ENDIF.


** end of change

    MODIFY it_tab.
  ENDLOOP.

  DATA: w_len TYPE i.
  w_len = strlen( filename ) - 4.

  CONCATENATE filename+0(w_len) '_' remi_h-zcmno filename+w_len(4)
              INTO filename.

*  DATA: W_LEN TYPE I.
*  W_LEN = STRLEN( FILENAME ).
*  CONCATENATE FILENAME+0(W_LEN) '_' REMI_H-ZCMNO FILENAME+W_LEN(4)
*              INTO FILENAME.

*  CONCATENATE FILENAME+0(6) '_' REMI_H-ZCMNO FILENAME+6(4)
*              INTO FILENAME.
ENDFORM.                    "MAKE_WORD_REMI



*---------------------------------------------------------------------*
*       FORM MAKE_WORD_WCI                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  WCI_H                                                         *
*  -->  WCI_I                                                         *
*---------------------------------------------------------------------*
FORM make_word_wci TABLES wci_h STRUCTURE zssd_rec_noti_h
                          wci_i STRUCTURE zssd_rec_noti_i.
  DATA : w_tot1 LIKE wci_i-zrcqt,
         w_tot2 LIKE wci_i-zrctt.
  DATA: l_zcmno(11).

  READ TABLE wci_h INDEX 1.

  PERFORM get_email_address USING wci_h-zvend.

  SELECT SINGLE * FROM kna1 WHERE kunnr = wci_h-zvend.

  LOOP AT wci_i.
    w_tot1 = w_tot1 + wci_i-zrcqt.
    w_tot2 = w_tot2 + wci_i-zrctt.
  ENDLOOP.

  LOOP AT it_tab.
    SEARCH it_tab-line FOR 'h0101'. "REF
    IF sy-subrc EQ 0.
      REPLACE 'h0101' WITH wci_h-zdmno INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'h0201'. "TO
    IF sy-subrc EQ 0.
      REPLACE 'h0201' WITH kna1-name1+0(25) INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'h0301'. "ATTN
    IF sy-subrc EQ 0.
      READ TABLE mail_addr INDEX 1.
      IF sy-subrc = 0.
        CONCATENATE mail_addr-namev mail_addr-name1
                    INTO w_text SEPARATED BY space.
        REPLACE 'h0301' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'h0301' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'h0401'. "CC
    IF sy-subrc EQ 0.
      READ TABLE mail_addr INDEX 2.
      IF sy-subrc = 0.
        CONCATENATE mail_addr-namev mail_addr-name1
                    INTO w_text SEPARATED BY space.
        REPLACE 'h0401' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'h0401' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'h0501'. "FROM
    IF sy-subrc EQ 0.
      REPLACE 'h0501' WITH 'HMMA' INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'h0102'. "EMAIL
    IF sy-subrc EQ 0.
      READ TABLE mail_addr INDEX 1.
      IF sy-subrc = 0.
        REPLACE 'h0102' WITH mail_addr-smtp_addr+0(25)
                                INTO it_tab-line.
      ELSE.
        REPLACE 'h0102' WITH '' INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'h0202'. "DATE
    IF sy-subrc EQ 0.
      REPLACE 'h0202' WITH wci_h-zdate INTO it_tab-line.
    ENDIF.
*
    SEARCH it_tab-line FOR 'pycr'. "Unit
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 1.
      IF sy-subrc = 0.
        REPLACE 'pycr' WITH wci_i-zpycr INTO it_tab-line.
      ELSE.
        REPLACE 'pycr' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
*
    SEARCH it_tab-line FOR 'i0101'. "SEQ NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 1.
      IF sy-subrc = 0.
        WRITE wci_i-zseq TO wci_i-zseq NO-ZERO.
        REPLACE 'i0101' WITH wci_i-zseq INTO it_tab-line.
      ELSE.
        REPLACE 'i0101' WITH ''         INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0102'. "ISSUE NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 1.
      IF sy-subrc = 0.
        REPLACE 'i0102' WITH wci_i-zissn INTO it_tab-line.
      ELSE.
        REPLACE 'i0102' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0103'. "CLAIM QTY
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 1.
      IF sy-subrc = 0.
        WRITE wci_i-zrcqt TO wci_i-zrcqt NO-ZERO.
        REPLACE 'i0103' WITH wci_i-zrcqt INTO it_tab-line.
      ELSE.
        REPLACE 'i0103' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0104'. "CLAIM AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 1.
      IF sy-subrc = 0.
        WRITE wci_i-zrctt TO w_text.
        REPLACE 'i0104' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0104' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0105'. "
    IF sy-subrc EQ 0.
      REPLACE 'i0105' WITH ''     INTO it_tab-line.
    ENDIF.

    SEARCH it_tab-line FOR 'i0201'. "SEQ NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 2.
      IF sy-subrc = 0.
        WRITE wci_i-zseq TO wci_i-zseq NO-ZERO.
        REPLACE 'i0201' WITH wci_i-zseq INTO it_tab-line.
      ELSE.
        REPLACE 'i0201' WITH ''         INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0202'. "ISSUE NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 2.
      IF sy-subrc = 0.
        REPLACE 'i0202' WITH wci_i-zissn INTO it_tab-line.
      ELSE.
        REPLACE 'i0202' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0203'. "CLAIM QTY
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 2.
      IF sy-subrc = 0.
        WRITE wci_i-zrcqt TO wci_i-zrcqt NO-ZERO.
        REPLACE 'i0203' WITH wci_i-zrcqt INTO it_tab-line.
      ELSE.
        REPLACE 'i0203' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0204'. "CLAIM AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 2.
      IF sy-subrc = 0.
        WRITE wci_i-zrctt TO w_text.
        REPLACE 'i0204' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0204' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0205'. "
    IF sy-subrc EQ 0.
      REPLACE 'i0205' WITH ''     INTO it_tab-line.
    ENDIF.

    SEARCH it_tab-line FOR 'i0301'. "SEQ NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 3.
      IF sy-subrc = 0.
        WRITE wci_i-zseq TO wci_i-zseq NO-ZERO.
        REPLACE 'i0301' WITH wci_i-zseq INTO it_tab-line.
      ELSE.
        REPLACE 'i0301' WITH ''         INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0302'. "ISSUE NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 3.
      IF sy-subrc = 0.
        REPLACE 'i0302' WITH wci_i-zissn INTO it_tab-line.
      ELSE.
        REPLACE 'i0302' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0303'. "CLAIM QTY
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 3.
      IF sy-subrc = 0.
        WRITE wci_i-zrcqt TO wci_i-zrcqt NO-ZERO.
        REPLACE 'i0303' WITH wci_i-zrcqt INTO it_tab-line.
      ELSE.
        REPLACE 'i0303' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0304'. "CLAIM AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 3.
      IF sy-subrc = 0.
        WRITE wci_i-zrctt TO w_text.
        REPLACE 'i0304' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0304' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0305'. "
    IF sy-subrc EQ 0.
      REPLACE 'i0305' WITH ''     INTO it_tab-line.
    ENDIF.

    SEARCH it_tab-line FOR 'i0401'. "SEQ NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 4.
      IF sy-subrc = 0.
        WRITE wci_i-zseq TO wci_i-zseq NO-ZERO.
        REPLACE 'i0401' WITH wci_i-zseq INTO it_tab-line.
      ELSE.
        REPLACE 'i0401' WITH ''         INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0402'. "ISSUE NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 4.
      IF sy-subrc = 0.
        REPLACE 'i0402' WITH wci_i-zissn INTO it_tab-line.
      ELSE.
        REPLACE 'i0402' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0403'. "CLAIM QTY
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 4.
      IF sy-subrc = 0.
        WRITE wci_i-zrcqt TO wci_i-zrcqt NO-ZERO.
        REPLACE 'i0403' WITH wci_i-zrcqt INTO it_tab-line.
      ELSE.
        REPLACE 'i0403' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0404'. "CLAIM AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 4.
      IF sy-subrc = 0.
        WRITE wci_i-zrctt TO w_text.
        REPLACE 'i0404' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0404' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0405'. "
    IF sy-subrc EQ 0.
      REPLACE 'i0405' WITH ''     INTO it_tab-line.
    ENDIF.

    SEARCH it_tab-line FOR 'i0501'. "SEQ NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 5.
      IF sy-subrc = 0.
        WRITE wci_i-zseq TO wci_i-zseq NO-ZERO.
        REPLACE 'i0501' WITH wci_i-zseq INTO it_tab-line.
      ELSE.
        REPLACE 'i0501' WITH ''         INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0502'. "ISSUE NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 5.
      IF sy-subrc = 0.
        REPLACE 'i0502' WITH wci_i-zissn INTO it_tab-line.
      ELSE.
        REPLACE 'i0502' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0503'. "CLAIM QTY
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 5.
      IF sy-subrc = 0.
        WRITE wci_i-zrcqt TO wci_i-zrcqt NO-ZERO.
        REPLACE 'i0503' WITH wci_i-zrcqt INTO it_tab-line.
      ELSE.
        REPLACE 'i0503' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0504'. "CLAIM AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 5.
      IF sy-subrc = 0.
        WRITE wci_i-zrctt TO w_text.
        REPLACE 'i0504' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0504' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0505'. "
    IF sy-subrc EQ 0.
      REPLACE 'i0505' WITH ''     INTO it_tab-line.
    ENDIF.

    SEARCH it_tab-line FOR 'i0601'. "SEQ NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 6.
      IF sy-subrc = 0.
        WRITE wci_i-zseq TO wci_i-zseq NO-ZERO.
        REPLACE 'i0601' WITH wci_i-zseq INTO it_tab-line.
      ELSE.
        REPLACE 'i0601' WITH ''         INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0602'. "ISSUE NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 6.
      IF sy-subrc = 0.
        REPLACE 'i0602' WITH wci_i-zissn INTO it_tab-line.
      ELSE.
        REPLACE 'i0602' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0603'. "CLAIM QTY
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 6.
      IF sy-subrc = 0.
        WRITE wci_i-zrcqt TO wci_i-zrcqt NO-ZERO.
        REPLACE 'i0603' WITH wci_i-zrcqt INTO it_tab-line.
      ELSE.
        REPLACE 'i0603' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0604'. "CLAIM AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 6.
      IF sy-subrc = 0.
        WRITE wci_i-zrctt TO w_text.
        REPLACE 'i0604' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0604' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0605'. "
    IF sy-subrc EQ 0.
      REPLACE 'i0605' WITH ''     INTO it_tab-line.
    ENDIF.

    SEARCH it_tab-line FOR 'i0701'. "SEQ NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 7.
      IF sy-subrc = 0.
        WRITE wci_i-zseq TO wci_i-zseq NO-ZERO.
        REPLACE 'i0701' WITH wci_i-zseq INTO it_tab-line.
      ELSE.
        REPLACE 'i0701' WITH ''         INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0702'. "ISSUE NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 7.
      IF sy-subrc = 0.
        REPLACE 'i0702' WITH wci_i-zissn INTO it_tab-line.
      ELSE.
        REPLACE 'i0702' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0703'. "CLAIM QTY
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 7.
      IF sy-subrc = 0.
        WRITE wci_i-zrcqt TO wci_i-zrcqt NO-ZERO.
        REPLACE 'i0703' WITH wci_i-zrcqt INTO it_tab-line.
      ELSE.
        REPLACE 'i0703' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0704'. "CLAIM AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 7.
      IF sy-subrc = 0.
        WRITE wci_i-zrctt TO w_text.
        REPLACE 'i0704' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0704' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0705'. "
    IF sy-subrc EQ 0.
      REPLACE 'i0705' WITH ''     INTO it_tab-line.
    ENDIF.

    SEARCH it_tab-line FOR 'i0801'. "SEQ NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 8.
      IF sy-subrc = 0.
        WRITE wci_i-zseq TO wci_i-zseq NO-ZERO.
        REPLACE 'i0801' WITH wci_i-zseq INTO it_tab-line.
      ELSE.
        REPLACE 'i0801' WITH ''         INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0802'. "ISSUE NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 8.
      IF sy-subrc = 0.
        REPLACE 'i0802' WITH wci_i-zissn INTO it_tab-line.
      ELSE.
        REPLACE 'i0802' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0803'. "CLAIM QTY
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 8.
      IF sy-subrc = 0.
        WRITE wci_i-zrcqt TO wci_i-zrcqt NO-ZERO.
        REPLACE 'i0803' WITH wci_i-zrcqt INTO it_tab-line.
      ELSE.
        REPLACE 'i0803' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0804'. "CLAIM AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 8.
      IF sy-subrc = 0.
        WRITE wci_i-zrctt TO w_text.
        REPLACE 'i0804' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0804' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0805'. "
    IF sy-subrc EQ 0.
      REPLACE 'i0805' WITH ''     INTO it_tab-line.
    ENDIF.

    SEARCH it_tab-line FOR 'i0901'. "SEQ NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 9.
      IF sy-subrc = 0.
        WRITE wci_i-zseq TO wci_i-zseq NO-ZERO.
        REPLACE 'i0901' WITH wci_i-zseq INTO it_tab-line.
      ELSE.
        REPLACE 'i0901' WITH ''         INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0902'. "ISSUE NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 9.
      IF sy-subrc = 0.
        REPLACE 'i0902' WITH wci_i-zissn INTO it_tab-line.
      ELSE.
        REPLACE 'i0902' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0903'. "CLAIM QTY
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 9.
      IF sy-subrc = 0.
        WRITE wci_i-zrcqt TO wci_i-zrcqt NO-ZERO.
        REPLACE 'i0903' WITH wci_i-zrcqt INTO it_tab-line.
      ELSE.
        REPLACE 'i0903' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0904'. "CLAIM AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 9.
      IF sy-subrc = 0.
        WRITE wci_i-zrctt TO w_text.
        REPLACE 'i0904' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i0904' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i0905'. "
    IF sy-subrc EQ 0.
      REPLACE 'i0905' WITH ''     INTO it_tab-line.
    ENDIF.

    SEARCH it_tab-line FOR 'i1001'. "SEQ NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 10.
      IF sy-subrc = 0.
        WRITE wci_i-zseq TO wci_i-zseq NO-ZERO.
        REPLACE 'i1001' WITH wci_i-zseq INTO it_tab-line.
      ELSE.
        REPLACE 'i1001' WITH ''         INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1002'. "ISSUE NO
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 10.
      IF sy-subrc = 0.
        REPLACE 'i1002' WITH wci_i-zissn INTO it_tab-line.
      ELSE.
        REPLACE 'i1002' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1003'. "CLAIM QTY
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 10.
      IF sy-subrc = 0.
        WRITE wci_i-zrcqt TO wci_i-zrcqt NO-ZERO.
        REPLACE 'i1003' WITH wci_i-zrcqt INTO it_tab-line.
      ELSE.
        REPLACE 'i1003' WITH ''          INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1004'. "CLAIM AMOUNT
    IF sy-subrc EQ 0.
      READ TABLE wci_i INDEX 10.
      IF sy-subrc = 0.
        WRITE wci_i-zrctt TO w_text.
        REPLACE 'i1004' WITH w_text INTO it_tab-line.
      ELSE.
        REPLACE 'i1004' WITH ''     INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 'i1005'. "
    IF sy-subrc EQ 0.
      REPLACE 'i1005' WITH ''     INTO it_tab-line.
    ENDIF.

    SEARCH it_tab-line FOR 'i1103'. "CLAIM QTY TOTAL
    IF sy-subrc EQ 0.
      WRITE w_tot1 TO w_text.
      REPLACE 'i1103' WITH w_text INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'i1104'. "CLAIM AMOUNT TOTAL
    IF sy-subrc EQ 0.
      WRITE w_tot2 TO w_text.
      REPLACE 'i1104' WITH w_text INTO it_tab-line.
    ENDIF.
    SEARCH it_tab-line FOR 'i1105'. "
    IF sy-subrc EQ 0.
      REPLACE 'i1105' WITH ''     INTO it_tab-line.
    ENDIF.
*
** Changed by Furong Wang on 02/01/2007
*    SEARCH IT_TAB-LINE FOR 't0101'. "
*    IF SY-SUBRC EQ 0.
*      REPLACE 't0101' WITH ''           INTO IT_TAB-LINE.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 't0201'. "
*    IF SY-SUBRC EQ 0.
*      REPLACE 't0201' WITH ''           INTO IT_TAB-LINE.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 't0301'. "
*    IF SY-SUBRC EQ 0.
*      REPLACE 't0301' WITH ''           INTO IT_TAB-LINE.
*    ENDIF.
*    SEARCH IT_TAB-LINE FOR 't0401'. "
*    IF SY-SUBRC EQ 0.
*      REPLACE 't0401' WITH WCI_H-ZDATE INTO IT_TAB-LINE.
*    ENDIF.

    SEARCH it_tab-line FOR 't0101'. "
    IF sy-subrc EQ 0.
      IF kna1-kunnr = 'B06AA'.
        REPLACE 't0101' WITH 'Bank of Nova Scotia'
             INTO it_tab-line.
      ELSEIF kna1-kunnr = 'B28AA'.
        REPLACE 't0101' WITH 'Bank of America'
             INTO it_tab-line.
      ELSE.
        REPLACE 't0101' WITH '' INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 't0201'. "
    IF sy-subrc EQ 0.
      IF kna1-kunnr = 'B06AA'.
        REPLACE 't0201' WITH 'Hyundai Auto Canada'
             INTO it_tab-line.
      ELSEIF kna1-kunnr = 'B28AA'.
        REPLACE 't0201' WITH 'Hyundai Motor America'
             INTO it_tab-line.
      ELSE.
        REPLACE 't0201' WITH '' INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 't0301'. "
    IF sy-subrc EQ 0.
      IF kna1-kunnr = 'B06AA'.
        REPLACE 't0301' WITH '1060-11'
             INTO it_tab-line.
      ELSEIF kna1-kunnr = 'B28AA'.
        REPLACE 't0301' WITH '7188811027'
             INTO it_tab-line.
      ELSE.
        REPLACE 't0301' WITH '' INTO it_tab-line.
      ENDIF.
    ENDIF.
    SEARCH it_tab-line FOR 't0401'. "
    IF sy-subrc EQ 0.
      REPLACE 't0401' WITH wci_h-zdate INTO it_tab-line.
    ENDIF.
** End of change
    MODIFY it_tab.
  ENDLOOP.
  DATA: w_len TYPE i.
  w_len = strlen( filename ) - 4.
  CONCATENATE filename+0(w_len) '_' wci_h-zdmno filename+w_len(4)
             INTO filename.

*  CONCATENATE FILENAME+0(6) '_' WCI_H-ZDMNO FILENAME+6(4)
*              INTO FILENAME.
ENDFORM.                    "MAKE_WORD_WCI



*---------------------------------------------------------------------*
*       FORM GET_EMAIL_ADDRESS                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_KUNNR                                                       *
*---------------------------------------------------------------------*
FORM get_email_address USING p_kunnr.
  REFRESH mail_addr. CLEAR mail_addr.
  CALL FUNCTION 'Z_FSD_MAIL_ADDR'
    EXPORTING
      kunnr          = p_kunnr
    TABLES
      mail_addr      = mail_addr
    EXCEPTIONS
      not_found_kna1 = 1
      not_found_knvk = 2
      OTHERS         = 3.
ENDFORM.                    "GET_EMAIL_ADDRESS
