*&---------------------------------------------------------------------*
*& Report  ZCFII91                                                     *
*& Author                 :  WSKIM
*& Creation Date          : 10/08/2004
*& Specification By       : YC, YOON, Andy Choi
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description  : AR  Upload
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------

REPORT zcfii90   NO STANDARD PAGE HEADING
                 LINE-SIZE 132
                 LINE-COUNT 65
                 MESSAGE-ID zmfi.
*Tables
TABLES : anla,ania,ankb,aufk.

*Internal tables
DATA : it_anla LIKE anla OCCURS 0 WITH HEADER LINE,
       BEGIN OF it_data OCCURS 0,
        anln1 LIKE anla-anln1,
        afasl LIKE anlb-afasl, " dep key
        ndjar LIKE anlb-ndjar, " usfl life
        afabg LIKE anlb-afabg, " o.dep.start
        check,
       END OF it_data.
DATA: it_msg                 LIKE TABLE OF bdcmsgcoll  WITH HEADER LINE,
      it_bdcdata             LIKE TABLE OF bdcdata     WITH HEADER LINE.
DATA: BEGIN OF iexcel OCCURS 0.
        INCLUDE STRUCTURE alsmex_tabline.
DATA: END OF iexcel.
* No of columns
DATA: BEGIN OF data_tab,
       value_0001(50),
       value_0002(50),
       value_0003(50),
       value_0004(50).
DATA: END OF data_tab.
DATA : BEGIN OF it_aufk OCCURS 0,
       aufnr LIKE  aufk-aufnr,
       anlkl LIKE ania-anlkl,
       aktiv LIKE ania-aktiv,
       akstl LIKE coas-akstl,
       mark,
       END OF it_aufk.
DATA: tind(4) TYPE n.
DATA: zwfeld(19).
FIELD-SYMBOLS: <fs1>.
* for excel upload - end
*Data
DATA : z_eaufn LIKE ania-objnr,
       z_ktogr LIKE ania-ktogr,
       w_int TYPE i.
DATA: external_date(10),
      internal_date TYPE d.
DATA original_date TYPE d.
DATA:  p_file LIKE rlgrap-filename.
DATA  :noheader TYPE c VALUE 'X'.
DATA: g_rc LIKE sy-subrc.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_afabe LIKE ankb-afabe DEFAULT '20'.

SELECTION-SCREEN END OF BLOCK b1.
PARAMETERS :  p_mode  TYPE c DEFAULT 'E'.

*AT SELECTION-SCREEN ON RADIOBUTTON GROUP rd1.

START-OF-SELECTION.
  PERFORM upload_pc_file USING g_rc.
  CHECK g_rc = 0.
  PERFORM bdc_update_process_order.

END-OF-SELECTION.
  PERFORM write.
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0195   text
*      -->P_0196   text
*      -->P_0197   text
*----------------------------------------------------------------------*
FORM bdc_dynpro_processing  USING    dy_begin  pg_name   sc_no.
  IF dy_begin = 'X'.
    CLEAR it_bdcdata.
    MOVE  pg_name  TO it_bdcdata-program.
    MOVE  sc_no    TO it_bdcdata-dynpro.
    MOVE  'X'      TO it_bdcdata-dynbegin.
    APPEND it_bdcdata.
  ELSE.
    CLEAR it_bdcdata.
    MOVE  pg_name  TO it_bdcdata-fnam.
    MOVE  sc_no    TO it_bdcdata-fval.
    APPEND it_bdcdata.
  ENDIF.
ENDFORM.                    " bdc_dynpro_processing
*&---------------------------------------------------------------------*
*&      Form  WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write.
  CLEAR w_int.
  DESCRIBE TABLE it_aufk LINES w_int.
  WRITE :/ 'Count', w_int.
  LOOP AT it_aufk.
    WRITE : / it_aufk-aufnr,it_aufk-anlkl,it_aufk-aktiv,it_aufk-mark.
  ENDLOOP.

  LOOP AT it_msg WHERE msgtyp EQ 'E'.
    WRITE : / it_msg-msgtyp,it_msg-msgid,it_msg-msgnr, it_msg-msgv1.
  ENDLOOP.
ENDFORM.                    " WRITE
*&---------------------------------------------------------------------*
*&      Form  date_conversion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA_AFABG  text
*      <--P_EXTERNAL_DATE  text
*----------------------------------------------------------------------*
FORM date_conversion USING    p_it_data_afabg
                     CHANGING p_external_date.

  original_date = p_it_data_afabg.

  CALL 'DATE_CONV_INT_TO_EXT'
  ID 'DATINT' FIELD original_date
  ID 'DATEXT' FIELD p_external_date.

ENDFORM.                    " date_conversion
*&---------------------------------------------------------------------*
*&      Form  upload_pc_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_pc_file USING f_rc LIKE sy-subrc.
  REFRESH :iexcel,it_aufk.
  CLEAR: f_rc.
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            mask             = ',*.xls.'
            mode             = 'O'
            title            = 'PC File'
       IMPORTING
            filename         = p_file
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.

  f_rc = sy-subrc.
  CHECK sy-subrc = 0.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            filename                = p_file
            i_begin_col             = 1
            i_begin_row             = 1
            i_end_col               = 100
            i_end_row               = 30000
       TABLES
            intern                  = iexcel
       EXCEPTIONS
            inconsistent_parameters = 1
            upload_ole              = 2
            OTHERS                  = 3.
  IF sy-subrc <> 0.
    WRITE: / 'EXCEL UPLOAD FAILED ', sy-subrc.
    f_rc = sy-subrc.
  ELSE.
    SORT iexcel BY row col.
    LOOP AT iexcel.
*      IF noheader = 'X' AND iexcel-row = 1.
*        CONTINUE.
*      ENDIF.
      tind = iexcel-col.
      CONCATENATE 'DATA_TAB-VALUE_' tind INTO zwfeld.
      ASSIGN (zwfeld) TO <fs1>.
      <fs1> = iexcel-value.
      AT END OF row.
        it_aufk-aufnr   = data_tab-value_0001.
        it_aufk-anlkl   = data_tab-value_0002.
        it_aufk-aktiv   = data_tab-value_0003.
        it_aufk-akstl   = data_tab-value_0004.
        APPEND it_aufk.
        CLEAR data_tab.
      ENDAT.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " upload_pc_file
*&---------------------------------------------------------------------*
*&      Form  BDC_UPDATE_PROCESS_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_update_process_order.
  REFRESH it_bdcdata.
  SORT it_aufk ASCENDING BY aufnr.
  LOOP AT it_aufk.
    REFRESH it_bdcdata.
    PERFORM bdc_dynpro_processing USING :
                           'X'  'SAPMKAUF'             '0110',
                           ' '  'BDC_OKCODE'           '/00' ,
                           ' '  'COAS-AUFNR'           it_aufk-aufnr,

                           'X'  'SAPMKAUF'             '0600',
                           ' '  'COAS-AKSTL'           it_aufk-akstl,
                           ' '  'BDC_OKCODE'           '=BUT5'.

    PERFORM date_conversion USING  it_aufk-aktiv
                         CHANGING external_date.


    PERFORM bdc_dynpro_processing USING :
                           'X'  'SAPMKAUF'             '0600',
                           ' '  'BDC_OKCODE'           '=SICH' ,
                           ' '  'ANIA-ANLKL'           it_aufk-anlkl,
                           ' '  'ANIA-AKTIV'           external_date.


    CALL TRANSACTION 'KO02'  USING it_bdcdata           MODE p_mode
                                MESSAGES INTO           it_msg .
    IF sy-subrc = 0.
      it_aufk-mark = 'S'.
      MODIFY it_aufk TRANSPORTING mark
       WHERE aufnr = it_aufk-aufnr.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " BDC_UPDATE_PROCESS_ORDER
