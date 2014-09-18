REPORT Z_IT_PU19_ADJUSTMENT_FED
       NO STANDARD PAGE HEADING LINE-SIZE 255.

TABLES: t100.
DATA: BEGIN OF it_data OCCURS 0 ,
        pernr LIKE p0000-pernr  ,
        amt1(17) type c,
        message(100) TYPE c     ,
      END OF it_data            .

DATA : it_intern  TYPE kcde_cells OCCURS 0 WITH HEADER LINE,
       gv_bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE,
       gv_messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA : gv_index TYPE i,
       gv_lines TYPE i,
       gv_mstring(480) TYPE c                     .

FIELD-SYMBOLS : <fs>.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS : p_local  LIKE rlgrap-filename             ,
             p_update LIKE ctu_params-updmode DEFAULT 'L',
             p_mode   LIKE ctu_params-dismode DEFAULT 'N'.

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_local.
  PERFORM f4_p_upfile USING p_local.

** Furong on 08/31/12 for deactivate the unused program
Initialization.
Leave program.
** End on 08/31/12

START-OF-SELECTION.
  PERFORM read_lcl_file.
  PERFORM update_pu19.
  PERFORM write_report.

*&---------------------------------------------------------------------*
*&      Form  read_lcl_file
*&---------------------------------------------------------------------*
*       Subroutine to read data from input file
*----------------------------------------------------------------------*
FORM read_lcl_file.
  DATA: l_tabix LIKE sy-tabix.
  CLEAR : it_data, it_data[].

  PERFORM excel_file_upload USING p_local.
  IF it_intern[] IS INITIAL.
    MESSAGE s000(zmco) WITH text-002.
    EXIT.
  ELSE.
    LOOP AT it_intern.
      gv_index = it_intern-col.
      ASSIGN COMPONENT gv_index OF STRUCTURE it_data TO <fs>.
      MOVE : it_intern-value TO <fs>.
      AT END OF row.
        APPEND it_data.
        CLEAR  it_data.
      ENDAT.
    ENDLOOP.
  ENDIF.
  DELETE it_data INDEX 1.
  DELETE it_data INDEX 1.
  IF it_data[] IS INITIAL.
    MESSAGE s000(zmco) WITH text-002.
    EXIT.
  ENDIF.
ENDFORM.                    " read_lcl_file
*&---------------------------------------------------------------------*
*&      Form  f4_p_upfile
*&---------------------------------------------------------------------*
*       Subroutine to provide F4 help for local file
*----------------------------------------------------------------------*
*      -->p_filename  text
*----------------------------------------------------------------------*
FORM f4_p_upfile USING    p_filename.
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_path         = p_filename  "* File Name
            mask             = ',*.XLS,*.XLS.'
            mode             = 'O'
       IMPORTING
            filename         = p_filename
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.

ENDFORM.                    " f4_p_upfile
*&---------------------------------------------------------------------*
*&      Form  excel_file_upload
*&---------------------------------------------------------------------*
*       Subroutine to download local excel file
*----------------------------------------------------------------------*
*      -->P_LOCAL  Local file name
*----------------------------------------------------------------------*
FORM excel_file_upload USING    p_local.
  DATA : l_begin_col TYPE i VALUE '1',
         l_begin_row TYPE i VALUE '1',
         l_end_col   TYPE i VALUE '256',
         l_end_row   TYPE i VALUE '65536'.

  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
       EXPORTING
            filename                = p_local
            i_begin_col             = l_begin_col
            i_begin_row             = l_begin_row
            i_end_col               = l_end_col
            i_end_row               = l_end_row
       TABLES
            intern                  = it_intern
       EXCEPTIONS
            inconsistent_parameters = 1
            upload_ole              = 2
            OTHERS                  = 3.

ENDFORM.                    " excel_file_upload
*&---------------------------------------------------------------------*
*&      Form  update_pu19
*&---------------------------------------------------------------------*
*       Subroutine to Updata PA30 record
*----------------------------------------------------------------------*
FORM update_pu19.
  DATA: l_tabix LIKE sy-tabix,
        l_extdate(12) TYPE c .
  LOOP AT it_data.
    CLEAR:   gv_bdcdata,
             gv_messtab.
    REFRESH: gv_bdcdata,
             gv_messtab.
    l_tabix = sy-tabix.

perform bdc_dynpro      using 'SAPMPU19' '2000'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'SUB-TAXCP'
                              'H201'.
perform bdc_field       using 'SUB-UDATE'
                              '12/31/2007'.
perform bdc_field       using 'SUB-ASOFD'
                              '12/31/2007'.
perform bdc_dynpro      using 'SAPMPU19' '2000'.
perform bdc_field       using 'BDC_OKCODE'
                              '=MANU'.
perform bdc_field       using 'SUB-TAXCP'
                              'H201'.
perform bdc_field       using 'SUB-UDATE'
                              '12/31/2007'.
perform bdc_field       using 'SUB-ASOFD'
                              '12/31/2007'.
perform bdc_dynpro      using 'SAPMPU19' '7000'.
perform bdc_field       using 'BDC_OKCODE'
                              '=NEW'.
perform bdc_field       using 'MAN_TXCMP'
                              'H201'.
perform bdc_field       using 'MAN_BEGDA'
                              '12/31/2007'.
perform bdc_field       using 'MAN_ENDDA'
                              '12/31/2007'.
perform bdc_field       using 'BDC_CURSOR'
                              'RNG_MAN_PERNR-LOW'.
perform bdc_field       using 'RNG_MAN_PERNR-LOW'
                              it_data-pernr.
perform bdc_dynpro      using 'SAPMPU19' '7000'.
perform bdc_field       using 'BDC_OKCODE'
                              '=SAVE'.
perform bdc_field       using 'MAN_TXCMP'
                              'H201'.
perform bdc_field       using 'MAN_BEGDA'
                              '12/31/2007'.
perform bdc_field       using 'MAN_ENDDA'
                              '12/31/2007'.
perform bdc_field       using 'BDC_CURSOR'
                              'QPU24-TAXBT(05)'.
perform bdc_field       using 'RNG_MAN_PERNR-LOW'
                              it_data-pernr.
perform bdc_field       using 'QPU24-TAXAU(01)'
                              'AL'.
perform bdc_field       using 'QPU24-WKSIT(01)'
                              '01'.
perform bdc_field       using 'QPU24-TBETR(01)'
                              it_data-amt1.
perform bdc_dynpro      using 'SAPLSPO1' '0100'.
perform bdc_field       using 'BDC_OKCODE'
                              '=YES'.
perform bdc_dynpro      using 'SAPMPU19' '7000'.
perform bdc_field       using 'BDC_OKCODE'
                              '/EBACK'.
perform bdc_field       using 'BDC_CURSOR'
                              'MAN_TXCMP'.
perform bdc_field       using 'MAN_TXCMP'
                              'H201'.
perform bdc_field       using 'MAN_BEGDA'
                              '12/31/2007'.
perform bdc_field       using 'MAN_ENDDA'
                              '12/31/2007'.
perform bdc_field       using 'RNG_MAN_PERNR-LOW'
                              it_data-pernr.
perform bdc_dynpro      using 'SAPMPU19' '2000'.
perform bdc_field       using 'BDC_OKCODE'
                              '/EBACK'.
perform bdc_field       using 'BDC_CURSOR'
                              'SUB-TAXCP'.

    CALL TRANSACTION 'PU19'  USING gv_bdcdata
                             MODE   p_mode
                             UPDATE p_update
                             MESSAGES INTO gv_messtab.

    IF sy-subrc = 0.
      it_data-message = 'Record Updated'.
      MODIFY it_data INDEX l_tabix TRANSPORTING message.
    ELSE.
      LOOP AT gv_messtab WHERE msgtyp = 'E' OR msgtyp = 'S'.
        SELECT SINGLE * FROM t100 WHERE sprsl = gv_messtab-msgspra
                                  AND   arbgb = gv_messtab-msgid
                                  AND   msgnr = gv_messtab-msgnr.
        IF sy-subrc = 0.
          CLEAR gv_mstring.
          gv_mstring = t100-text.
          IF gv_mstring CS '&1'.
            REPLACE '&1' WITH gv_messtab-msgv1 INTO gv_mstring.
            REPLACE '&2' WITH gv_messtab-msgv2 INTO gv_mstring.
            REPLACE '&3' WITH gv_messtab-msgv3 INTO gv_mstring.
            REPLACE '&4' WITH gv_messtab-msgv4 INTO gv_mstring.
          ELSE.
            REPLACE '&' WITH gv_messtab-msgv1 INTO gv_mstring.
            REPLACE '&' WITH gv_messtab-msgv2 INTO gv_mstring.
            REPLACE '&' WITH gv_messtab-msgv3 INTO gv_mstring.
            REPLACE '&' WITH gv_messtab-msgv4 INTO gv_mstring.
          ENDIF.
          CONDENSE gv_mstring.
          it_data-message = gv_mstring.
          MODIFY it_data INDEX l_tabix TRANSPORTING message.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " update_pu19
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       Subroutine to Start new screen
*----------------------------------------------------------------------*
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR gv_bdcdata.
  gv_bdcdata-program  = program.
  gv_bdcdata-dynpro   = dynpro.
  gv_bdcdata-dynbegin = 'X'.
  APPEND gv_bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       Subroutine to Insert field
*----------------------------------------------------------------------*
*      -->P_0330   text
*      -->P_0331   text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR gv_bdcdata.
  gv_bdcdata-fnam = fnam.
  gv_bdcdata-fval = fval.
  APPEND gv_bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  write_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM write_report.
  WRITE: /5 'Pernr', 15 'Message'.
  LOOP AT it_data.
    WRITE: /5 it_data-pernr, 15 it_data-message.
  ENDLOOP.
ENDFORM.                    " write_report
