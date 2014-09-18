REPORT z_it_0007_wsrule
       NO STANDARD PAGE HEADING LINE-SIZE 255.

TABLES: t100.
DATA: BEGIN OF it_data OCCURS 0 ,
        pernr LIKE p0000-pernr  ,
        choic LIKE rp50g-choic  ,
        schkz LIKE p0007-schkz  ,  " Work Schedule Rule
        date(10)     TYPE c     ,
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
  PERFORM update_pa30.
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
*&      Form  update_pa30
*&---------------------------------------------------------------------*
*       Subroutine to Updata PA30 record
*----------------------------------------------------------------------*
FORM update_pa30.
  DATA: l_tabix LIKE sy-tabix,
        l_extdate(12) TYPE c .
  LOOP AT it_data.
    CLEAR:   gv_bdcdata,
             gv_messtab.
    REFRESH: gv_bdcdata,
             gv_messtab.
    l_tabix = sy-tabix.
    PERFORM user_specific_date USING it_data-date
                             CHANGING l_extdate.

    PERFORM bdc_dynpro      USING 'SAPMP50A' '1000'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RP50G-PERNR'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'RP50G-PERNR'
                                  it_data-pernr.

    PERFORM bdc_dynpro      USING 'SAPMP50A' '1000'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'RP50G-CHOIC'
                                  it_data-choic.
    PERFORM bdc_dynpro      USING 'SAPMP50A' '1000'.

    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=INS'.
    PERFORM bdc_dynpro      USING 'MP000700' '2000'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'P0007-SCHKZ'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=UPD'.
    PERFORM bdc_field       USING 'P0007-BEGDA'
                                  l_extdate.
    PERFORM bdc_field       USING 'P0007-SCHKZ'
                                  it_data-schkz.

    CALL TRANSACTION 'PA30'  USING gv_bdcdata
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

ENDFORM.                    " update_pa30
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
*&---------------------------------------------------------------------*
*&      Form  user_specific_date
*&---------------------------------------------------------------------*
*       Subroutine to get user specific date format
*----------------------------------------------------------------------*
*      -->P_DATA     Date in generic form
*      <--P_EXTDATE  Date in User specific date format
*----------------------------------------------------------------------*
FORM user_specific_date USING    p_pdate
                        CHANGING p_extdate.
  DATA: w_datfm(1) TYPE c.
  CALL FUNCTION 'ITS_GET_USER_DEFAULTS'
       EXPORTING
            bname = sy-uname
       IMPORTING
            datfm = w_datfm.

  CASE w_datfm.
    WHEN 1.
      CONCATENATE p_pdate+3(2) p_pdate(2) p_pdate+6(4) INTO p_extdate
      SEPARATED BY '.'.

    WHEN 2.
      p_extdate = p_pdate.

    WHEN 3.
      CONCATENATE p_pdate(2) p_pdate+3(2) p_pdate+6(4) INTO p_extdate
      SEPARATED BY '-'.

    WHEN 4.
      CONCATENATE p_pdate+6(4) p_pdate(2) p_pdate+3(2) INTO p_extdate
      SEPARATED BY '.'.

    WHEN 5.
      CONCATENATE p_pdate+6(4) p_pdate(2) p_pdate+3(2) INTO p_extdate
      SEPARATED BY '/'.

    WHEN 6.
      CONCATENATE p_pdate+6(4) p_pdate(2) p_pdate+3(2) INTO p_extdate
      SEPARATED BY '-'.

  ENDCASE.
ENDFORM.                    " user_specific_date
