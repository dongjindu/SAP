*&---------------------------------------------------------------------*
*& Report  ZAPP724U_DOWNLOAD_M_RFC
*& Author                 : WSKIM
*& Creation Date          : 03/19/2004
*& Specification By       :
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description  : <Interface >GET DATA  FROM MOBIS To HMMA
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT  zapp724u_download_m_rfc  MESSAGE-ID zmbm.

TABLES : ztbm_mobis_log.

DATA : it_mobis_rfc LIKE ztbm_mobis_log,
       wa_subrc LIKE sy-subrc.
DATA : it_opt LIKE rfc_db_opt OCCURS 0 WITH HEADER LINE,
       it_field LIKE rfc_db_fld OCCURS 0 WITH HEADER LINE,
       it_data LIKE tab512 OCCURS 0 WITH HEADER LINE.
DATA : it_log LIKE ztbm_mobis_log OCCURS 0 WITH HEADER LINE.

PARAMETERS : p_date LIKE sy-datum OBLIGATORY DEFAULT sy-datum.

START-OF-SELECTION.
  PERFORM option.
  PERFORM filed.

  CALL FUNCTION 'RFC_READ_TABLE'
    DESTINATION 'MOBIS_BOM'
    EXPORTING
      query_table                = 'ZPPHLOG'
      delimiter                  =  'X'
      no_data                    = ' '
*   ROWSKIPS                   = 0
*   ROWCOUNT                   = 0
    TABLES
      options                    = it_opt
      fields                     = it_field
      data                       = it_data
   EXCEPTIONS
     table_not_available        = 1
     table_without_data         = 2
     option_not_valid           = 3
     field_not_valid            = 4
     not_authorized             = 5
     data_buffer_exceeded       = 6
     OTHERS                     = 7 .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    PERFORM update.
  ENDIF.

  CALL FUNCTION 'RFC_CONNECTION_CLOSE'
       EXPORTING
            destination          = 'MOBIS_BOM'
       EXCEPTIONS
            destination_not_open = 1
            OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  OPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM option.
  REFRESH it_opt.
  DATA : condition(100).
  CONCATENATE 'ZZIFDAT =' text-001 p_date text-002 INTO condition.
  it_opt-text = condition.
  APPEND it_opt.
ENDFORM.                    " OPTION
*&---------------------------------------------------------------------*
*&      Form  FILED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM filed.
  REFRESH it_field.
  it_field-fieldname = 'ZZIFIRC'.
  APPEND it_field.
  it_field-fieldname = 'ZZIFBRC'.
  APPEND it_field.
  it_field-fieldname = 'ZZIFCRC'.
  APPEND it_field.
  it_field-fieldname = 'ZZIFORC'.
  APPEND it_field.

ENDFORM.                    " FILED
*&---------------------------------------------------------------------*
*&      Form  UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update.
  DATA : wa_itm(10) TYPE c,
         wa_brc LIKE wa_itm,
         wa_crc LIKE wa_itm,
         wa_orc LIKE wa_itm.
  REFRESH it_log.CLEAR : wa_itm,wa_brc,wa_crc,wa_orc.
  READ TABLE it_data INDEX 1.
  IF sy-subrc = 0.
    CONDENSE it_data-wa NO-GAPS.
    SPLIT it_data-wa AT 'X' INTO wa_itm wa_brc wa_crc wa_orc.

*ITM
    CLEAR ztbm_mobis_log.
    MOVE : 'ZTBM_ABXITMDT'    TO it_log-ztable,
            'M'               TO it_log-gubun,
            p_date            TO it_log-infdate,
            wa_itm            TO it_log-zcount.
    APPEND it_log. CLEAR : it_log.
*STR(BRC)
    CLEAR ztbm_mobis_log.
    MOVE : 'ZTBM_ABXSTRDT'    TO it_log-ztable,
            'M'               TO it_log-gubun,
            p_date            TO it_log-infdate,
            wa_brc            TO it_log-zcount.
    APPEND it_log.  CLEAR : it_log.
*C23(CRC)
    CLEAR ztbm_mobis_log.
    MOVE : 'ZTBM_ABXC23DT'     TO it_log-ztable,
            'M'                TO it_log-gubun,
            p_date             TO it_log-infdate,
            wa_crc             TO it_log-zcount.
    APPEND it_log.CLEAR : it_log.
*OCN(ORC)
    CLEAR ztbm_mobis_log.
    MOVE : 'ZTBM_ABXOCNDT'    TO it_log-ztable,
            'M'               TO it_log-gubun,
            p_date            TO it_log-infdate,
            wa_orc            TO it_log-zcount.
    APPEND it_log.CLEAR : it_log.

    MODIFY ztbm_mobis_log FROM TABLE it_log.
    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE s000 WITH 'Successfully updated'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE s000 WITH 'Update error'.
    ENDIF.
    CLEAR : it_log.
  ELSE.
    WRITE : / text-003.
  ENDIF.

ENDFORM.                    " UPDATE
