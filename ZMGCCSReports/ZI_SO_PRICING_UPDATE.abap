REPORT zi_so_pricing_update
       NO STANDARD PAGE HEADING LINE-SIZE 255.

TABLES: t100,vbfa,*vbfa,vbuk.

DATA: BEGIN OF it_data OCCURS 0 ,
*        s_no(4) TYPE c          ,
        vbeln   LIKE vbak-vbeln ,  " Sales Order No
        flag(1) TYPE c          ,
        message(100) TYPE c     ,
        result(1)    TYPE c     ,
      END OF it_data            ,

      BEGIN OF it_vbak OCCURS 0 ,
        vbeln LIKE vbak-vbeln   ,  " Sales Order No
      END OF it_vbak            ,

      BEGIN OF it_vbap OCCURS 0 ,
        vbeln LIKE vbap-vbeln   ,  " Sales Order No
        posnr LIKE vbap-posnr   ,  " Sales Order Item
      END OF it_vbap            .

DATA : it_intern  TYPE kcde_cells OCCURS 0 WITH HEADER LINE,
       gv_bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE,
       gv_messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA : gv_index TYPE i       ,
       gv_mstring(480) TYPE c,
       gv_total        TYPE i,
       gv_success      TYPE i,
       gv_fail         TYPE i.

* by ig.moon 7/23/2008 {
DEFINE __pick.
  perform bdc_dynpro      using 'SAPMV45A' '5003'.
  perform bdc_field       using 'BDC_OKCODE'
                                '=V69A_KONY'.

  perform bdc_dynpro      using 'SAPMSSY0' '0120'.
  perform bdc_field       using 'BDC_CURSOR'
                                '05/03'.
  perform bdc_field       using 'BDC_OKCODE'
                                '=PICK'.
END-OF-DEFINITION.
* }

FIELD-SYMBOLS : <fs>.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS : p_local  LIKE rlgrap-filename             ,
             p_update LIKE ctu_params-updmode DEFAULT 'L',
             p_mode   LIKE ctu_params-dismode DEFAULT 'N'.

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_local.
  PERFORM f4_p_upfile USING p_local.

START-OF-SELECTION.
  PERFORM read_lcl_file.
  PERFORM update_va02.
  PERFORM write_report.

*&---------------------------------------------------------------------*
*&      Form  read_lcl_file
*&---------------------------------------------------------------------*
*       Subroutine to read data from input file
*----------------------------------------------------------------------*
FORM read_lcl_file.
  DATA: l_tabix LIKE sy-tabix              ,
        l_10 LIKE vbap-posnr VALUE '000010',
        l_20 LIKE vbap-posnr VALUE '000020'.

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
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
             EXPORTING
                  input  = it_data-vbeln
             IMPORTING
                  output = it_data-vbeln.

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
  DESCRIBE TABLE it_data LINES gv_total.

  SELECT vbeln
         FROM vbak
         INTO TABLE it_vbak
         FOR ALL ENTRIES IN it_data
         WHERE   vbeln = it_data-vbeln.
  SORT it_vbak BY vbeln.

  IF NOT it_vbak[] IS INITIAL.
    SELECT vbeln posnr
           FROM vbap
           INTO TABLE it_vbap
           FOR ALL ENTRIES IN it_vbak
           WHERE   vbeln = it_vbak-vbeln AND
                   posnr IN (l_10, l_20).

    SORT it_vbap BY vbeln posnr.
  ENDIF.

  LOOP AT it_data.
    l_tabix = sy-tabix.
    READ TABLE it_vbak WITH KEY vbeln = it_data-vbeln BINARY SEARCH.
    IF sy-subrc <> 0.
      it_data-flag = '5'.
      MODIFY it_data INDEX l_tabix TRANSPORTING flag.
    ELSE.
      READ TABLE it_vbap WITH KEY vbeln = it_data-vbeln
                                  posnr = l_10.
      IF sy-subrc = 0.    " Item 10 is present
        READ TABLE it_vbap WITH KEY vbeln = it_data-vbeln
                                    posnr = l_20.
        IF sy-subrc = 0.  " Item 20 is also present
          it_data-flag = '2'.
          MODIFY it_data INDEX l_tabix TRANSPORTING flag.
        ELSE.             " Item 20 is not there
          it_data-flag = '1'.
          MODIFY it_data INDEX l_tabix TRANSPORTING flag.
        ENDIF.
      ELSE.
        READ TABLE it_vbap WITH KEY vbeln = it_data-vbeln
                                    posnr = l_20.
        IF sy-subrc = 0.  " Only Item 20 is present
          it_data-flag = '3'.
          MODIFY it_data INDEX l_tabix TRANSPORTING flag.
        ELSE.             " Item 10 and 20 both are not there
          it_data-flag = '4'.
          MODIFY it_data INDEX l_tabix TRANSPORTING flag.
        ENDIF.
      ENDIF.  " IF sy-subrc = 0.   " Item 10 is present
    ENDIF.
  ENDLOOP.
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
*&      Form  update_va02
*&---------------------------------------------------------------------*
*       Subroutine to Updata PA30 record
*----------------------------------------------------------------------*
FORM update_va02.
  DATA: l_tabix LIKE sy-tabix,
        l_extdate(12) TYPE c .
  DATA $flag_gi.
  LOOP AT it_data.
    CLEAR:   gv_bdcdata,
             gv_messtab.
    REFRESH: gv_bdcdata,
             gv_messtab.
    l_tabix = sy-tabix.

    PERFORM bdc_dynpro      USING 'SAPMV45A' '0102'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'VBAK-VBELN'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'VBAK-VBELN'
                                  it_data-vbeln.

    IF it_data-flag EQ '1' OR it_data-flag EQ '2'.

      CLEAR $flag_gi.

*      SELECT * INTO *vbfa FROM vbfa WHERE vbelv = it_data-vbeln
*                                    AND posnv = '000010'.
*        SELECT SINGLE * FROM vbuk
*                          WHERE vbeln EQ *vbfa-vbeln
*                             AND ( wbstk EQ 'B'
*                             OR wbstk EQ 'C' ) .
*        IF sy-subrc EQ 0.
*          $flag_gi = 'X'.
*        ENDIF.
*      ENDSELECT.

      IF it_data-flag = '1'.

        IF $flag_gi EQ 'X'.
          it_data-message = text-009.
          MODIFY it_data INDEX l_tabix TRANSPORTING message.
          CONTINUE.
        ELSE.

          PERFORM bdc_dynpro      USING 'SAPMV45A' '4001'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=PKO1'.

          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'VBAP-POSNR(01)'.
          PERFORM bdc_field       USING 'RV45A-VBAP_SELKZ(01)'
                                        'X'.

          __pick.

        ENDIF.

      ELSEIF it_data-flag = '2'.

        IF $flag_gi NE 'X'.

          PERFORM bdc_dynpro      USING 'SAPMV45A' '4001'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=PKO1'.

          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'VBAP-POSNR(01)'.
          PERFORM bdc_field       USING 'RV45A-VBAP_SELKZ(01)'
                                        'X'.

          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'VBAP-POSNR(02)'.
          PERFORM bdc_field       USING 'RV45A-VBAP_SELKZ(02)'
                                        'X'.
          __pick.

          PERFORM bdc_dynpro      USING 'SAPMV45A' '5003'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=BACK'.

          PERFORM bdc_dynpro      USING 'SAPMV45A' '4001'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=PKO1'.

          __pick.

        ELSE.

          PERFORM bdc_dynpro      USING 'SAPMV45A' '4001'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=PKO1'.

          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'VBAP-POSNR(02)'.
          PERFORM bdc_field       USING 'RV45A-VBAP_SELKZ(02)'
                                        'X'.
          __pick.

        ENDIF.
      ENDIF.


    ELSE.

      IF it_data-flag = '3'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'VBAP-POSNR(01)'.

        PERFORM bdc_field       USING 'RV45A-VBAP_SELKZ(01)'
                                      'X'.

        __pick.

      ELSEIF it_data-flag = '4'.
        it_data-message = text-003.
        MODIFY it_data INDEX l_tabix TRANSPORTING message.
        CONTINUE.
      ELSEIF it_data-flag = '5'.
        it_data-message = text-005.
        MODIFY it_data INDEX l_tabix TRANSPORTING message.
        CONTINUE.
      ENDIF.


    ENDIF.

    PERFORM bdc_dynpro      USING 'SAPMV45A' '5003'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=SICH'.

    CALL TRANSACTION 'VA02'  USING gv_bdcdata
                             MODE   p_mode
                             UPDATE p_update
                             MESSAGES INTO gv_messtab.

    IF sy-subrc = 0.
      it_data-message = 'Record Updated'.
      it_data-result = 'S'.
      MODIFY it_data INDEX l_tabix TRANSPORTING message result.
    ELSE.
      LOOP AT gv_messtab WHERE msgtyp = 'E'.
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

ENDFORM.                    " update_va02
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
  LOOP AT it_data WHERE result = 'S'.
    gv_success = gv_success + 1.
  ENDLOOP.
  gv_fail = gv_total - gv_success.
  WRITE: /5 text-004.
  SKIP 1.
  WRITE: /5 text-006 , gv_total.
  WRITE: /5 text-007 , gv_success.
  WRITE: /5 text-008 , gv_fail.
  SKIP 1.
  WRITE: /5 'Record #', 15 'Sales Order', 30 'Message'.
  LOOP AT it_data WHERE result <> 'S'.
*    WRITE: /5 it_data-s_no RIGHT-JUSTIFIED.
    WRITE: 15 it_data-vbeln, 30 it_data-message.
  ENDLOOP.
ENDFORM.                    " write_report
