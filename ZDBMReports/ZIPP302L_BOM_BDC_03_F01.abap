*----------------------------------------------------------------------*
*   INCLUDE ZIPP302L_BOM_BDC_03_F01                                    *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.
* BDC MODE, DEFAULT SIZE, UPDATE MODE
  wa_opt-defsize = 'X'.
  wa_opt-dismode = 'N'.
  wa_opt-updmode = 'S'.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM screen_modify.
  LOOP AT SCREEN.
*    IF SCREEN-NAME  EQ 'P_RDO1'.
    IF screen-name EQ 'P_FILETY' OR
       screen-name EQ '%_P_FILE_%_APP_%-TEXT'      OR
       screen-name EQ 'P_FILE'                    OR
       screen-name EQ '%_P_FILETY_%_APP_%-TEXT'.
      screen-active = 0.
    ELSEIF screen-name EQ  'P_TCODE'.
      screen-input = 0.
    ENDIF.
**    ELSEIF SCREEN-NAME  EQ 'P_RDO2'.  "EXCEL DATA
*    IF SCREEN-NAME EQ  'P_ZEDAT'  OR
*       SCREEN-NAME EQ '%_P_ZEDAT_%_APP_%-TEXT' OR
*       SCREEN-NAME EQ 'P_ZBTIM' OR
*       SCREEN-NAME EQ '%_P_ZBTIM_%_APP_%-TEXT'.
*      SCREEN-ACTIVE = 0.
*    ELSEIF SCREEN-NAME EQ 'P_FILETY' OR SCREEN-NAME EQ 'P_TCODE'.
*      SCREEN-INPUT = 0.
*    ENDIF.
    MODIFY SCREEN.
    CLEAR screen.
  ENDLOOP.
ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM at_sel_screen_on_value_request USING def_path LIKE rlgrap-filename
                                          mode     TYPE c.

  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: fieldln TYPE i.
  FIELD-SYMBOLS: <tmp_sym>.

* Build Filter for Fileselektor

*  IF GLOBAL_FILEMASK_MASK IS INITIAL.
  tmp_mask = ',*.*,*.*.'.
*  ELSE.
*    TMP_MASK = ','.
*    WRITE GLOBAL_FILEMASK_TEXT TO TMP_MASK+1.
*    WRITE ',' TO TMP_MASK+21.
*    WRITE GLOBAL_FILEMASK_MASK TO TMP_MASK+22.
*    WRITE '.' TO TMP_MASK+42.
*    CONDENSE TMP_MASK NO-GAPS.
*  ENDIF.

*  IF NOT GLOBAL_FILEMASK_ALL IS INITIAL.
*    TMP_MASK = GLOBAL_FILEMASK_ALL.
*  ENDIF.
*
  fieldln = strlen( def_path ) - 1.
  ASSIGN def_path+fieldln(1) TO <tmp_sym>.
  IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
    CLEAR <tmp_sym>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_filename     = p_file
            def_path         = def_path
*           MASK             = ',*.*,*.*.'
            mask             = tmp_mask
            mode             = mode
*           TITLE            = ' '
       IMPORTING
            filename         = tmp_filename
*         RC               =
       EXCEPTIONS
            inv_winsys       = 01
            no_batch         = 02
            selection_cancel = 03
            selection_error  = 04.

  IF sy-subrc = 0.
    p_file = tmp_filename.
  ELSE.
* IF SY-SUBRC = 01.    "// Does not work, why ???
*   MESSAGELINE = 'Not supported'.
* ENDIF.
  ENDIF.

ENDFORM.                               " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
FORM upload_process.

  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            codepage                = ' '
            filename                = p_file
            filetype                = p_filety
*           HEADLEN                 = ' '
*           LINE_EXIT               = ' '
*           TRUNCLEN                = ' '
*           USER_FORM               = ' '
*           USER_PROG               = ' '
*      IMPORTING
*           FILELENGTH              =
       TABLES
            data_tab                = it_excl
      EXCEPTIONS
           conversion_error        = 1
           file_open_error         = 2
           file_read_error         = 3
           invalid_table_width     = 4
           invalid_type            = 5
           no_batch                = 6
           unknown_error           = 7
           gui_refuse_filetransfer = 8
           customer_error          = 9
           OTHERS                  = 10
            .
  CASE sy-subrc.
    WHEN 0.
      DATA l_text(132).
      CONCATENATE p_file text-002
                  INTO l_text.
      WRITE: / l_text.
      SKIP.
    WHEN 2.
      MESSAGE e000 WITH text-003.
    WHEN 3.
      MESSAGE e000 WITH text-004.
    WHEN OTHERS.
      MESSAGE e000 WITH text-005.
  ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM bdc_process.
* NON COLOR PART BDC
  PERFORM non_color_part_bdc.
* COLOR PART BDC
  PERFORM color_part_bdc.
ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  NON_COL_END_ITEM_CREATE
*&---------------------------------------------------------------------*
FORM non_col_end_item_create.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'BDC_OKCODE'  '=FCBU'.
* CALL TRANSACTION
  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt
                           MESSAGES INTO it_mess.
ENDFORM.                    " NON_COL_END_ITEM_CREATE
*&---------------------------------------------------------------------*
*&      Form  NON_COL_CHANGE_HEADER_BOM
*&---------------------------------------------------------------------*
FORM non_col_change_header_bom USING  pa_ncol STRUCTURE wa_ncol.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' pa_ncol-matnr,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' pa_ncol-werks,   "PLANT
     ' ' 'RC29N-STLAN' pa_ncol-stlan,   "BOM usage
     ' ' 'RC29N-STLAL' pa_ncol-stlal,   "ALT BOM
     ' ' 'RC29N-AENNR' pa_ncol-aennr,   "Change number
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCNP'.

ENDFORM.                    " NON_COL_CHANGE_HEADER_BOM
*&---------------------------------------------------------------------*
*&      Form  NON_COL_BADY_ITEM_CREATE1
*&---------------------------------------------------------------------*
FORM   non_col_bady_item_create1 USING pa_ncol STRUCTURE wa_ncol.
  DATA: l_dbcnt TYPE sy-dbcnt.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'RC29P-AUSKZ(02)' 'X'   ,    "CHECK
     ' ' 'RC29P-POSNR(02)' pa_ncol-posnr,   "BOM item number
     ' ' 'RC29P-IDNRK(02)' pa_ncol-idnrk,   "BOM compenent
     ' ' 'RC29P-MENGE(02)' pa_ncol-menge,   "Compenent quantity
     ' ' 'RC29P-POSTP(02)' pa_ncol-postp,   "Item category
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0130',
     ' ' 'RC29P-ITSOB' pa_ncol-itsob,   "procurement type
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0131',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0138',
     ' ' 'ZEITM'       pa_ncol-zeitm,    "END ITEM TYPE
     ' ' 'ZSTGB'       pa_ncol-zstgb,    "STRUCTURE TYPE
     ' ' 'ZSUFF'       pa_ncol-zsuff,    "SUFFIX NO
     ' ' 'ZSEQU'       pa_ncol-zsequ,    "SEQUENCE NO
     ' ' 'ZUPGN'       pa_ncol-zupgn,    "UPG
     ' ' 'ZINFO'       pa_ncol-zseqc,    "COLOR SEQUENCE
     ' ' 'BDC_OKCODE'  '/00'.

  IF pa_ncol-clpt EQ 'C'.
    SELECT COUNT( * )
         FROM mast AS a INNER JOIN stpo AS b
                        ON a~stlnr EQ b~stlnr
         INTO l_dbcnt
         WHERE a~matnr EQ pa_ncol-matnr
         AND   a~werks EQ pa_ncol-werks
         AND   a~stlan EQ pa_ncol-stlan
         AND   a~stlal EQ pa_ncol-stlal.
    IF l_dbcnt GE '1'.
      PERFORM dynpro USING:
         'X' 'SAPLCSDI'    '0140',
         ' ' 'RC29P-AUSKZ(02)' 'X',                "
         ' ' 'BDC_OKCODE'  '=WIZU'.
    ELSE.
      PERFORM dynpro USING:
         'X' 'SAPLCSDI'    '0140',
         ' ' 'RC29P-AUSKZ(01)' 'X',                "
         ' ' 'BDC_OKCODE'  '=WIZU'.
    ENDIF.
    PERFORM dynpro USING:
     'X' 'SAPLCUKD'    '0130',
      ' ' 'RCUKD-KNNAM(01)' pa_ncol-dpid,      " O/J DEPENDENCY
      ' ' 'BDC_OKCODE'  '/00',

      'X' 'SAPLCUKD'    '0130',
      ' ' 'BDC_OKCODE'  '=BACK'.
  ENDIF.
  PERFORM dynpro USING:
       'X' 'SAPLCSDI'    '0140',
       ' ' 'BDC_OKCODE'  '=FCNP'.

ENDFORM.                    " NON_COL_BADY_ITEM_CREATE1
*&---------------------------------------------------------------------*
*&      Form  NON_COL_READ_MAST_STPO_STLKN
*&---------------------------------------------------------------------*
FORM non_col_read_mast_stpo_stlkn USING    pa_ncol STRUCTURE wa_ncol
                                  CHANGING p_mode
                                           p_stlkn.
  DATA: l_zsequ(4) TYPE n,
        l_stlnr TYPE mast-stlnr,
        l_cnt TYPE i.
  l_zsequ = pa_ncol-zsequ.
  l_zsequ = l_zsequ - 1.

  SELECT SINGLE a~stlnr
                b~stlkn
       FROM mast AS a INNER JOIN stpo AS b
                      ON    b~stlty EQ 'M'
                      AND   a~stlnr EQ b~stlnr
       INTO  (l_stlnr, p_stlkn)
       WHERE a~matnr EQ pa_ncol-matnr
       AND   a~werks EQ pa_ncol-werks
       AND   a~stlan EQ pa_ncol-stlan
       AND   a~stlal EQ pa_ncol-stlal
       AND   b~posnr EQ pa_ncol-posnr
       AND   b~idnrk EQ pa_ncol-idnrk
       AND   b~suff  EQ pa_ncol-zsuff
       AND   b~sequ  EQ l_zsequ.
  IF sy-subrc EQ 0.
    SELECT COUNT( DISTINCT stasz )
         FROM stas
         INTO l_cnt
         WHERE stlty EQ 'M'
         AND   stlnr EQ l_stlnr
         AND   stlal EQ pa_ncol-stlal
         AND   stlkn EQ p_stlkn.
    IF l_cnt GT 1.
      p_mode = '4'.
    ELSE.
      p_mode = '3'.
    ENDIF.
  ELSE.
    p_mode = '4'.
  ENDIF.
ENDFORM.                    " NON_COL_READ_MAST_STPO_STLKN
*&---------------------------------------------------------------------*
*&      Form  NON_COL_MAST_STPO_READ
*&---------------------------------------------------------------------*
FORM non_col_mast_stpo_read USING    pa_bmnc STRUCTURE wa_bmnc
                                  CHANGING p_mode
                                           p_stlkn.
  DATA: l_zsequ(4) TYPE n,
        l_stlnr TYPE mast-stlnr,
        l_cnt TYPE i.
*  l_zsequ = pa_bmnc-sequ.
*  l_zsequ = l_zsequ - 1.

  SELECT SINGLE MAX( sequ )
       FROM mast AS a INNER JOIN stpo AS b
                      ON    b~stlty EQ 'M'
                      AND   a~stlnr EQ b~stlnr
       INTO  (l_zsequ)
       WHERE a~matnr EQ pa_bmnc-mtno
       AND   a~werks EQ pa_bmnc-plnt
       AND   a~stlan EQ pa_bmnc-usag
       AND   a~stlal EQ pa_bmnc-altn
       AND   b~posnr EQ pa_bmnc-pref
       AND   b~idnrk EQ pa_bmnc-comp
       AND   b~suff  EQ pa_bmnc-suff.
  IF l_zsequ IS INITIAL.
    p_mode = '4'.
    EXIT.
  ENDIF.

  SELECT SINGLE a~stlnr
                b~stlkn
       FROM mast AS a INNER JOIN stpo AS b
                      ON    b~stlty EQ 'M'
                      AND   a~stlnr EQ b~stlnr
       INTO  (l_stlnr, p_stlkn)
       WHERE a~matnr EQ pa_bmnc-mtno
       AND   a~werks EQ pa_bmnc-plnt
       AND   a~stlan EQ pa_bmnc-usag
       AND   a~stlal EQ pa_bmnc-altn
       AND   b~posnr EQ pa_bmnc-pref
       AND   b~idnrk EQ pa_bmnc-comp
       AND   b~suff  EQ pa_bmnc-suff
       AND   b~sequ  EQ l_zsequ.
  IF sy-subrc EQ 0.
*    SELECT COUNT( DISTINCT stasz )
*         FROM stas
*         INTO l_cnt
*         WHERE stlty EQ 'M'
*         AND   stlnr EQ l_stlnr
*         AND   stlal EQ pa_bmnc-altn
*         AND   stlkn EQ p_stlkn.
*    IF l_cnt GT 1.
*      p_mode = '4'.
*    ELSE.
    p_mode = '3'.
*    ENDIF.
  ELSE.
    p_mode = '4'.
  ENDIF.
ENDFORM.                    " NON_COL_MAST_STPO_READ
*---------------------------------------------------------------------*
*       FORM DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM dynpro USING dynbegin name value.
  IF dynbegin = 'X'.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-program,
          value TO it_bdc-dynpro,
          dynbegin TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-fnam,
          value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
FORM rkc_msg_string CHANGING p_msg.
  DATA: lw_msg LIKE cfgnl-msglin.

  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       IMPORTING
            msg_lin = lw_msg
       EXCEPTIONS
            OTHERS  = 1.

  MOVE: lw_msg TO p_msg.
ENDFORM.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM data_process CHANGING pa_check.
  PERFORM data_check_marc CHANGING pa_check.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_CHECK_MARC
*&---------------------------------------------------------------------*
FORM data_check_marc CHANGING pa_check.
  DATA l_tabix TYPE sy-tabix.
  DATA: BEGIN OF lt_chck OCCURS 0,
          matnr TYPE marc-matnr,
          werks TYPE marc-werks,
        END OF lt_chck.
  DATA: BEGIN OF lt_marc OCCURS 0,
          matnr TYPE marc-matnr,
          werks TYPE marc-werks,
        END OF lt_marc.
  LOOP AT it_excl.
    lt_chck-matnr = it_excl-matnr.
    lt_chck-werks = it_excl-werks.
    COLLECT lt_chck.
    lt_chck-matnr = it_excl-idnrk.
    lt_chck-werks = it_excl-werks.
    COLLECT lt_chck.
    CLEAR: it_excl, lt_chck.
  ENDLOOP.

  IF NOT lt_chck[] IS INITIAL.
    SELECT matnr
           werks
         FROM marc
         INTO TABLE lt_marc
         FOR ALL ENTRIES IN lt_chck
         WHERE matnr EQ lt_chck-matnr
         AND   werks EQ lt_chck-werks.
    IF sy-subrc EQ 0.
      SORT lt_marc BY matnr werks.
      LOOP AT it_excl.
        l_tabix = sy-tabix.
        READ TABLE lt_marc WITH KEY matnr = it_excl-matnr
                                    werks = it_excl-werks
                           BINARY SEARCH.
        IF sy-subrc NE 0.
          pa_check = 'X'.
          it_excl-zresult = 'L'.
          CONCATENATE 'NENT MATERIAL : ' it_excl-matnr
                      '  Material number does not exist'
                 INTO it_excl-zmsg.
          MODIFY it_excl INDEX l_tabix TRANSPORTING zresult
                                                    zmsg.
        ENDIF.
        READ TABLE lt_marc WITH KEY matnr = it_excl-idnrk
                                    werks = it_excl-werks
                           BINARY SEARCH.
        IF sy-subrc NE 0.
          pa_check = 'X'.
          it_excl-zresult = 'L'.
          IF it_excl-zmsg IS INITIAL.
            CONCATENATE  'COMPORNENT : ' it_excl-idnrk
                         '  Material number does not exist'
                    INTO it_excl-zmsg.
          ELSE.
            CONCATENATE  it_excl-zmsg '  COMPORNENT : ' it_excl-idnrk
                         '  Material number does not exist'
                    INTO it_excl-zmsg.
          ENDIF.

          MODIFY it_excl INDEX l_tabix TRANSPORTING zresult
                                                    zmsg.
        ENDIF.
      ENDLOOP.
    ELSE.
      pa_check = 'X'.
      it_excl-zresult = 'L'.
      it_excl-zmsg = ' Material number does not exist'.
      MODIFY it_excl TRANSPORTING zmsg
                                  zresult
                               WHERE matnr GE space
                               AND   werks GE space.
    ENDIF.
  ENDIF.
  IF pa_check EQ 'X'.
    CLEAR wa_erro_idx.
    LOOP AT it_excl WHERE zresult EQ 'L'.
      wa_erro_idx = wa_erro_idx + 1.
    ENDLOOP.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / text-006, wa_erro_idx.
    FORMAT COLOR OFF.
    PERFORM error_write TABLES it_excl
                        USING  'L'.
  ENDIF.
ENDFORM.                    " DATA_CHECK_MARC
*&---------------------------------------------------------------------*
*&      Form  BOM_HEADER
*&---------------------------------------------------------------------*
FORM bom_header USING p_matnr
                      p_werks
                      p_stlan
                      p_stlal
                      p_bmeng
                      p_stlst.
  REFRESH: it_bdc, it_mess.
  CLEAR: it_bdc, it_mess.

  DATA: l_msg   LIKE cfgnl-msglin,
        l_msgty TYPE sy-msgty.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' p_matnr,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' p_werks,    "PLANT
     ' ' 'RC29N-STLAN' p_stlan,    "BOM STLANe
     ' ' 'RC29N-STLAL' p_stlal,    "ALT BOM
     ' ' 'RC29N-AENNR' '19000101-001',    "Change number
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0110',
     ' ' 'RC29K-BMENG' p_bmeng,    "Confirmed quantity
     ' ' 'RC29K-STLST' p_stlst,    "BOM STATUS
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0111',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0140',
     ' ' 'BDC_OKCODE'  '=FCBU'.
  CALL TRANSACTION 'CS01'  USING it_bdc
                           OPTIONS FROM wa_opt
                           MESSAGES INTO it_mess.
  l_msgty = sy-msgty.
  PERFORM rkc_msg_string CHANGING l_msg.

  REFRESH: it_bdc, it_mess.
ENDFORM.         " BOM_HEADER
*&---------------------------------------------------------------------*
*&      Form  ERROR_WRITE
*&---------------------------------------------------------------------*
FORM error_write TABLES   pt_excl STRUCTURE it_excl
                 USING    p_zresult.
  WRITE: /(18)  text-007,
          (05)  text-008,
          (05)  text-009,
          (06)  text-010,
          (12)  text-011,
          (07)  text-012,
          (08)  text-013,
          (06)  text-014,
          (07)  text-015,
          (03)  text-016,
          (18)  text-017,
          (10)  text-018,
          (08)  text-019,
          (08)  text-020,
          (08)  text-021,
          (08)  text-022,
          (10)  text-023,
          (30)  text-024,
          (12)  text-025,
          (08)  text-026,
                text-027.
  LOOP AT pt_excl WHERE zresult EQ p_zresult.
    WRITE: /(18) pt_excl-matnr,
            (05) pt_excl-werks,
            (05) pt_excl-stlan,
            (06) pt_excl-stlal,
            (12) pt_excl-aennr,
            (07) pt_excl-bmeng,
            (08) pt_excl-bmein,
            (06) pt_excl-stlst,
            (07) pt_excl-posnr,
            (03) pt_excl-postp,
            (18) pt_excl-idnrk,
            (10) pt_excl-menge,
            (08) pt_excl-meins,
            (08) pt_excl-itsob,
            (08) pt_excl-zeitm,
            (08) pt_excl-zsuff,
            (10) pt_excl-clpt,
            (30) pt_excl-dpid,
            (12) pt_excl-upct,
            (08) pt_excl-zresult,
                 pt_excl-zmsg.
  ENDLOOP.
ENDFORM.                    " ERROR_WRITE
*&---------------------------------------------------------------------*
*&      Form  BOM_EXPLODED
*&---------------------------------------------------------------------*
FORM bom_exploded TABLES   p_bom_exploded STRUCTURE it_bom_exploded.
*---/// START : Issue: PP-20041006-001
*---/// Changed on 2004.10.06, Changed by BSBAE, Request by IYCHOI
*  DATA: BEGIN OF lt_marc OCCURS 0,
*          matnr LIKE marc-matnr,  "MATERIAL
*          werks LIKE marc-werks,  "PLANT
*          mtart LIKE mara-mtart,  "MATERIAL TYPE
*        END   OF lt_marc.
*  DATA l_tabix TYPE sy-tabix.
** MATERIAL & COMPENENT COLLECT
*  LOOP AT it_colo WHERE upct NE '2'
*                  AND   zresult NE 'E'.
*    lt_marc-matnr = it_colo-matnr.
*    lt_marc-werks = it_colo-werks.
*    COLLECT lt_marc.
*    CLEAR: lt_marc.
*    lt_marc-matnr = it_colo-idnrk.
*    lt_marc-werks = it_colo-werks.
*    COLLECT lt_marc.
*    CLEAR: lt_marc, it_colo.
*  ENDLOOP.
*  IF NOT lt_marc[] IS INITIAL.
**   SELECTION MARA--->MODIFY LT_MARC-MTART(MATERIAL TYPE)
**                            LT_MARC-KZKFG(Configurable Material)
*    PERFORM read_mara TABLES lt_marc.
*
*    LOOP AT lt_marc WHERE mtart EQ 'FERT'.
*      l_tabix = sy-tabix.
*      p_bom_exploded-matnr = lt_marc-matnr.
*      p_bom_exploded-werks = lt_marc-werks.
*      APPEND p_bom_exploded.
*      DELETE lt_marc INDEX l_tabix.
*      CLEAR: p_bom_exploded, lt_marc.
*    ENDLOOP.
*
*    LOOP AT lt_marc.
*      SORT p_bom_exploded.
*      READ TABLE p_bom_exploded WITH KEY matnr = lt_marc-matnr
*                                         werks = lt_marc-werks
*                                BINARY SEARCH.
*      IF sy-subrc NE 0.
**       BOM EXPLODED.
*        PERFORM read_bom TABLES p_bom_exploded
*                        USING lt_marc-matnr
*                              lt_marc-werks
*                              sy-datum   "IDNRK WERKS DATUV
*                        CHANGING wa_last.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*---/// END : Issue: PP-20041006-001
ENDFORM.                    " BOM_EXPLODED
*&---------------------------------------------------------------------*
*&      Form  READ_MARA
*&---------------------------------------------------------------------*
FORM read_mara TABLES pt_marc.
  DATA: BEGIN OF lt_marc OCCURS 0,
          matnr LIKE marc-matnr,  "MATERIAL
          werks LIKE marc-werks,  "PLANT
          datuv LIKE rc29p-datuv,
          datub LIKE rc29p-datub,
          mtart LIKE mara-mtart,  "MATERIAL TYPE
        END   OF lt_marc.
  DATA: BEGIN OF lt_mara OCCURS 0,
          matnr LIKE mara-matnr, "MATERIAL
          mtart LIKE mara-mtart, "MATERIAL TYPE
        END   OF lt_mara.
  DATA: l_tabix TYPE sy-tabix.
* INTERNAL TABLE MOVE
  lt_marc[] = pt_marc[].
* REFRESH
  REFRESH pt_marc.
  SELECT matnr
         mtart
       FROM mara
       INTO TABLE lt_mara
       FOR ALL ENTRIES IN lt_marc
       WHERE matnr EQ lt_marc-matnr.
  IF sy-subrc EQ 0.
*   SORTING
    SORT lt_mara BY matnr mtart.
*   MODIFY MTART(Material type)
    LOOP AT lt_marc.
      l_tabix = sy-tabix.
      READ TABLE lt_mara WITH KEY matnr = lt_marc-matnr
                         BINARY SEARCH TRANSPORTING mtart.
      IF sy-subrc EQ 0.
        lt_marc-mtart = lt_mara-mtart.
        MODIFY lt_marc INDEX l_tabix TRANSPORTING mtart.
      ENDIF.
      CLEAR: lt_marc, lt_mara.
    ENDLOOP.
  ENDIF.
* INTERNAL TABLE MOVE
  pt_marc[] = lt_marc[].
ENDFORM.                    " READ_MARA
*&---------------------------------------------------------------------*
*&      Form  READ_BOM
*&---------------------------------------------------------------------*
FORM read_bom TABLES p_bom_exploded STRUCTURE it_bom_exploded
              USING p_idnrk
                    p_werks
                    p_datuv
                    p_datub
              CHANGING p_last.

  DATA : temp_istpov LIKE stpov OCCURS 0 WITH HEADER LINE.
* REFRESH: IMC29S, ISTPOV, ICSCEQUI, ICSCKND, ICSCMAT, ICSCSTD, ICSCTPL.
* CLEAR : IMC29S, ISTPOV, ICSCEQUI, ICSCKND, ICSCMAT, ICSCSTD, ICSCTPL.

  CALL FUNCTION 'CS_WHERE_USED_MAT'
       EXPORTING
            datub                      = p_datub
            datuv                      = p_datuv
            matnr                      = p_idnrk
            werks                      = p_werks
       IMPORTING
            topmat                     = it_mc29s
       TABLES
            wultb                      = it_stpov
            equicat                    = it_cscequi
            kndcat                     = it_cscknd
            matcat                     = it_cscmat
            stdcat                     = it_cscstd
            tplcat                     = it_csctpl
       EXCEPTIONS
            call_invalid               = 1
            material_not_found         = 2
            no_where_used_rec_found    = 3
            no_where_used_rec_selected = 4
            no_where_used_rec_valid    = 5
            OTHERS                     = 6.

  temp_istpov[] = it_stpov[].
  IF sy-subrc <> 0.
    p_last = 'X'.
  ELSE.
    LOOP AT temp_istpov.
      SORT p_bom_exploded.
      READ TABLE p_bom_exploded WITH KEY matnr = temp_istpov-matnr
                                         werks = temp_istpov-werks
                                BINARY SEARCH.
      IF sy-subrc NE 0.
        PERFORM read_bom TABLES p_bom_exploded
                         USING temp_istpov-matnr
                               temp_istpov-werks
                               p_datuv
                               '99991231'
                         CHANGING p_last.
        READ TABLE p_bom_exploded WITH KEY matnr = temp_istpov-matnr
                                           werks = temp_istpov-werks
                                  BINARY SEARCH.
        IF sy-subrc NE 0.
          p_bom_exploded-matnr = temp_istpov-matnr.
          p_bom_exploded-werks = temp_istpov-werks.
          APPEND p_bom_exploded. CLEAR p_bom_exploded.
          CLEAR p_last.
        ENDIF.
      ENDIF.
      CLEAR temp_istpov.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_BOM
*&---------------------------------------------------------------------*
*&      Form  MARA_CONFIGURABLE_MATERIAL
*&---------------------------------------------------------------------*
FORM mara_configurable_material TABLES   p_bom_exploded
                                              STRUCTURE it_bom_exploded.
  DATA: BEGIN OF lt_mara OCCURS 0,
          matnr TYPE mara-matnr,
          kzkfg TYPE mara-kzkfg,  "Configurable Material
        END   OF lt_mara.
  DATA: l_tabix TYPE sy-tabix.
  SELECT matnr
         kzkfg
       FROM mara
       INTO TABLE lt_mara
       FOR ALL ENTRIES IN p_bom_exploded
       WHERE matnr EQ p_bom_exploded-matnr.
  IF sy-subrc EQ 0.
*   SORTING
    SORT lt_mara BY matnr.
    LOOP AT p_bom_exploded.
      l_tabix = sy-tabix.
      READ TABLE lt_mara WITH KEY matnr = p_bom_exploded-matnr
                         BINARY SEARCH TRANSPORTING kzkfg.
      IF sy-subrc EQ 0.
        p_bom_exploded-kzkfg = lt_mara-kzkfg.
        MODIFY p_bom_exploded INDEX l_tabix TRANSPORTING kzkfg.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " MARA_CONFIGURABLE_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  MM02_CONFIGURABLE_MATERIAL_BDC
*&---------------------------------------------------------------------*
FORM mm02_configurable_material_bdc.

  DATA: l_tabix TYPE sy-tabix.
  REFRESH: it_bdc, it_mess.
  CLEAR  : it_bdc, it_mess.
  LOOP AT it_bom_exploded WHERE kzkfg NE 'X'.
    l_tabix = sy-tabix.
    PERFORM dynpro USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR' it_bom_exploded-matnr,   "
       ' ' 'BDC_OKCODE'  '=AUSW',

       'X' 'SAPLMGMM'    '0070',
       ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
       ' ' 'BDC_OKCODE'  '=BILD',

       'X' 'SAPLMGMM'    '5004',
       ' ' 'MARA-KZKFG'  'X',   "
       ' ' 'BDC_OKCODE'  '=BU'.

    CALL TRANSACTION 'MM02'  USING it_bdc
             OPTIONS FROM wa_opt
             MESSAGES INTO it_mess.
    PERFORM rkc_msg_string CHANGING it_exp_mess-msg.
    it_exp_mess = it_bom_exploded.
    APPEND it_exp_mess.
    CLEAR it_exp_mess.
    REFRESH: it_bdc, it_mess.
    CLEAR  : it_bdc, it_mess.
  ENDLOOP.
ENDFORM.                    " MM02_CONFIGURABLE_MATERIAL_BDC
*&---------------------------------------------------------------------*
*&      Form  NON_COL_BOM_ITEM_DELETE
*&---------------------------------------------------------------------*
FORM non_col_bom_item_delete USING    pa_ncol STRUCTURE wa_ncol
                              p_stlkn.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' pa_ncol-matnr,   "NEXT MATERIAL
     ' ' 'RC29N-WERKS' pa_ncol-werks,   "PLANT
     ' ' 'RC29N-STLAN' pa_ncol-stlan,   "BOM usage
     ' ' 'RC29N-STLAL' pa_ncol-stlal,   "ALT BOM
     ' ' 'RC29N-AENNR' pa_ncol-aennr,   "Change number
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=SETP',

     'X' 'SAPLCSDI'    '0708',
     ' ' 'RC29P-SELPI' p_stlkn,                "
     ' ' 'BDC_OKCODE'  '=CLWI',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',                "
     ' ' 'BDC_OKCODE'  '=FCDL',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCBU'.

* CALL TRANSACTION
  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt
                           MESSAGES INTO it_mess.
ENDFORM.                    " NON_COL_BOM_ITEM_DELETE
*&---------------------------------------------------------------------*
*&      Form  TABLE_NON_COL_BOM_DELETE
*&---------------------------------------------------------------------*
FORM table_non_col_bom_delete USING    pa_bmnc STRUCTURE wa_bmnc
                                       p_stlkn.

  CLEAR: it_bdc, it_bdc[].

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' pa_bmnc-mtno,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' pa_bmnc-plnt,    "PLANT
     ' ' 'RC29N-STLAN' pa_bmnc-usag,    "BOM usage
     ' ' 'RC29N-STLAL' pa_bmnc-altn,    "ALT BOM
     ' ' 'RC29N-AENNR' pa_bmnc-eono,    "Change number
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=SETP',

     'X' 'SAPLCSDI'    '0708',
     ' ' 'RC29P-SELPI' p_stlkn,                "
     ' ' 'BDC_OKCODE'  '=CLWI',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',                "
     ' ' 'BDC_OKCODE'  '=FCDL',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCBU'.

* CALL TRANSACTION
  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt
                           MESSAGES INTO it_mess.
ENDFORM.                    " TABLE_NON_COL_BOM_DELETE
*&---------------------------------------------------------------------*
*&      Form  DEPENDENCY_ECM_CHECK
*&---------------------------------------------------------------------*
FORM dependency_ecm_check CHANGING pa_check.
  DATA: lt_excl LIKE it_excl OCCURS 0 WITH HEADER LINE,
        l_knnam LIKE cukb-knnam.
  DATA: BEGIN OF lt_aenr OCCURS 0,
          aennr TYPE aenr-aennr,
        END OF lt_aenr.
  DATA: BEGIN OF mt_aenr OCCURS 0,
          aennr TYPE aenr-aennr,
        END OF mt_aenr.
  CLEAR: wa_erro_idx, wa_line_idx.
* DEPENDENCY CHECK
  LOOP AT it_excl WHERE clpt EQ 'C'
                  AND   upct EQ '1'.
    wa_line_idx = wa_line_idx + 1.
    SELECT SINGLE knnam
                FROM cukb
                INTO l_knnam
                WHERE knnam EQ it_excl-dpid.
    IF sy-subrc NE 0.
      lt_excl = it_excl.
      lt_excl-zresult = 'L'.
      lt_excl-zmsg = 'Dependency does not exist'.
      APPEND lt_excl.
      wa_erro_idx = wa_erro_idx + 1.
      pa_check = 'X'.
    ENDIF.
    CLEAR: it_excl, lt_excl.
  ENDLOOP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / text-028, wa_line_idx.
  WRITE: / text-029,
            wa_erro_idx.
  FORMAT COLOR OFF.
  IF wa_erro_idx GE '1'.
    PERFORM error_write TABLES lt_excl
                        USING  'L'.
  ENDIF.
  REFRESH lt_excl. CLEAR lt_excl.
* CHANGE NUMBER CHECK
  LOOP AT it_excl.
    it_subm-stlan = it_excl-stlan.
    it_subm-stlal = it_excl-stlal.
    it_subm-matnr = it_excl-matnr.
    it_subm-werks = it_excl-werks.
    COLLECT it_subm.
    lt_aenr-aennr = it_excl-aennr.
    COLLECT lt_aenr.
    CLEAR: it_excl, lt_aenr, it_subm.
  ENDLOOP.
  IF NOT lt_aenr[] IS INITIAL.
    SELECT aennr
         FROM aenr
         INTO TABLE mt_aenr
         FOR ALL ENTRIES IN lt_aenr
         WHERE aennr EQ lt_aenr-aennr.
    IF sy-subrc EQ 0.
      SORT mt_aenr.
      CLEAR: wa_erro_idx, wa_line_idx.
      LOOP AT it_excl.
        wa_line_idx = wa_line_idx + 1.
        READ TABLE mt_aenr WITH KEY aennr = it_excl-aennr
                           BINARY SEARCH.
        IF sy-subrc NE 0.
          wa_erro_idx = wa_erro_idx + 1.
          lt_excl = it_excl.
          lt_excl-zresult = 'L'.
          lt_excl-zmsg = 'Change No does not exist'.
          APPEND lt_excl.
          pa_check = 'X'.
        ENDIF.
      ENDLOOP.
      DESCRIBE TABLE it_excl LINES wa_line_idx.
      DESCRIBE TABLE lt_excl LINES wa_erro_idx.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE: / text-030, wa_line_idx.
      WRITE: / text-031,
                wa_erro_idx.
      FORMAT COLOR OFF.
      IF wa_erro_idx GE '1'.
        PERFORM error_write TABLES lt_excl
                            USING  'L'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " DEPENDENCY_ECM_CHECK
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_PARTITION
*&---------------------------------------------------------------------*
FORM color_part_partition.
  LOOP AT it_excl.
    IF it_excl-clpt EQ 'C'.
      MOVE-CORRESPONDING it_excl TO it_colo.
      APPEND it_colo. CLEAR it_colo.
    ELSE.
      MOVE-CORRESPONDING it_excl TO it_ncol.
      APPEND it_ncol. CLEAR it_ncol.
    ENDIF.
    CLEAR it_excl.
  ENDLOOP.
  REFRESH it_excl. CLEAR it_excl.
ENDFORM.                    " COLOR_PART_PARTITION
*&---------------------------------------------------------------------*
*&      Form  NON_COLOR_PART_BDC
*&---------------------------------------------------------------------*
FORM non_color_part_bdc.
  DATA: l_chk(1),
        l_tabix TYPE sy-tabix,
        l_tanew TYPE sy-tabix,
        l_taend TYPE sy-tabix,
        l_mode,
        l_stlkn TYPE stpo-stlkn,
        l_zresult LIKE it_excl-zresult,
        l_zmsg LIKE it_excl-zmsg.
* SORTING
  SORT it_ncol BY matnr werks stlan stlal posnr
                  idnrk zsuff zsequ upct  aennr.
* BDC PROCESS
  LOOP AT it_ncol.
    l_tabix = sy-tabix.
    wa_ncol = it_ncol.
    CLEAR: l_stlkn.
*   UPDATE CONTROL TYPE
    IF wa_ncol-upct EQ '2'.
*     BOM ITEM DELETE
      PERFORM non_col_read_mast_stpo_stlkn USING    wa_ncol
                                           CHANGING l_mode
                                                    l_stlkn.
      IF l_mode EQ '3'. "DELETE(ITEM DELETE)
        IF NOT l_stlkn IS INITIAL.
          PERFORM non_col_bom_item_delete USING    wa_ncol
                                        l_stlkn.      "Item node number

        ENDIF.
        PERFORM rkc_msg_string CHANGING l_zmsg.
        l_zresult = sy-msgty.
*       MODIFY IT_NCOL
        it_ncol-zresult = l_zresult.
        it_ncol-zmsg    = l_zmsg.
        MODIFY it_ncol INDEX l_tabix TRANSPORTING zresult
                                                  zmsg.
      ELSEIF l_mode EQ '4'.
*       MODIFY IT_NCOL
        it_ncol-zresult = 'E'.
        it_ncol-zmsg    = 'BOM ITEM to delete does not exist.'.
        MODIFY it_ncol INDEX l_tabix TRANSPORTING zresult
                                                  zmsg.
      ENDIF.
      CLEAR it_ncol.
      REFRESH: it_bdc, it_mess.
      CLEAR: it_bdc, it_mess.

*   BOM ITEM CREATE
    ELSEIF wa_ncol-upct EQ '1'.
*     AT NEW STLAL
      AT NEW stlal.
        l_tanew = l_tabix.
        CLEAR l_mode.
        CLEAR it_stpo. REFRESH it_stpo.
*       BOM CHECK & BOM HEADER CREATED
        PERFORM mast_stpo_check USING    wa_ncol
                                CHANGING l_mode.
        IF l_mode EQ '1' OR  l_mode EQ '2'.
          PERFORM non_col_change_header_bom USING wa_ncol.
          l_mode = '1'.
        ENDIF.
      ENDAT.

*     'UPDATE CONTROL MODE' different BDC
      IF l_mode EQ '1'.
*       BOM ITEM DATA already CHECK
        CLEAR l_chk.
        PERFORM check_it_stpo USING    wa_ncol
                              CHANGING l_chk.
        IF l_chk EQ 'X'.
          PERFORM non_col_bady_item_create1 USING wa_ncol.
        ELSE.
          it_ncol-zresult = 'E'.
          it_ncol-zmsg = 'BOM ITEM DATA already.'.
          MODIFY it_ncol INDEX l_tabix TRANSPORTING zresult
                                                    zmsg.
          CLEAR it_ncol.
        ENDIF.
      ENDIF.

*     AT END OF STLAL
      AT END OF stlal.
        l_taend = l_tabix.
        IF l_mode EQ '1'.
          PERFORM non_col_end_item_create.
          l_zresult = sy-msgty.
          PERFORM rkc_msg_string CHANGING l_zmsg.
*         MODIFY IT_NCOL

*---/// START : Issue: PP-20041006-001
*---/// Changed on 2004.10.06, Changed by BSBAE, Request by IYCHOI
          LOOP AT it_ncol FROM l_tanew TO l_taend
                         WHERE NOT ( zresult EQ 'E' OR
                                     zresult EQ 'A' OR
                                     zresult EQ 'X' ).
*---/// END : Issue: PP-20041006-001

            it_ncol-zresult = l_zresult.
            it_ncol-zmsg    = l_zmsg.
            MODIFY it_ncol INDEX sy-tabix TRANSPORTING zresult
                                                       zmsg.
            CLEAR it_ncol.
          ENDLOOP.
        ENDIF.

        REFRESH: it_bdc, it_mess.
        CLEAR: it_bdc, it_mess.
      ENDAT.
    ENDIF.
    CLEAR: it_ncol, wa_ncol.
  ENDLOOP.

* BOM ERROR WRITE.
  CLEAR: wa_line_idx, wa_erro_idx.
  REFRESH it_excl. CLEAR it_excl.
* TOTAL LINE COUNT
  DESCRIBE TABLE it_ncol LINES wa_line_idx.
* ERROR LINE COUNT
  LOOP AT it_ncol.

*---/// START : Issue: PP-20041006-001
*---/// Changed on 2004.10.06, Changed by BSBAE, Request by IYCHOI
    IF it_ncol-zresult EQ 'E' OR
       it_ncol-zresult EQ 'A' OR
       it_ncol-zresult EQ 'X'.
*---/// END : Issue: PP-20041006-001

      wa_erro_idx = wa_erro_idx + 1.
      MOVE-CORRESPONDING it_ncol TO it_excl.
      APPEND it_excl.
    ENDIF.
    CLEAR: it_ncol, it_excl.
  ENDLOOP.
  REFRESH it_ncol. CLEAR it_ncol.
  WRITE: / text-032, wa_line_idx.
  FORMAT COLOR 6 INTENSIFIED OFF.
  WRITE: / text-033, wa_erro_idx.
  FORMAT COLOR OFF.

* Error appears if is more than one case.
  IF wa_erro_idx GE '1'.
    PERFORM error_write TABLES it_excl
                        USING  'E'.
    REFRESH it_excl. CLEAR it_excl.
  ENDIF.
ENDFORM.                    " NON_COLOR_PART_BDC
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_BDC
*&---------------------------------------------------------------------*
FORM color_part_bdc.
  DATA: l_tabix TYPE sy-tabix,
        l_tanew TYPE sy-tabix,
        l_taend TYPE sy-tabix,
        l_mode,
        l_stlkn TYPE stpo-stlkn,
        l_zresult LIKE it_excl-zresult,
        l_zmsg LIKE it_excl-zmsg.

* SORTING
  SORT it_colo BY matnr werks stlan stlal posnr
                  idnrk zsuff zsequ upct aennr zseqc.
  LOOP AT it_colo.
    l_tabix = sy-tabix.
    wa_colo = it_colo.
    CASE wa_colo-upct.
*     BOM ITEM ADDITION & OBJECT DEPENDENCY CREATE
      WHEN '1'.
*       AT NEW ZSEQU
        AT NEW zsequ.
          l_tanew = l_tabix.
          CLEAR l_mode.
          CLEAR it_stpo. REFRESH it_stpo.
          PERFORM color_part_header USING    wa_colo
                                    CHANGING l_mode
                                             l_stlkn.
          IF l_mode EQ '1'. "CHANGE(ITEM CREATE)
            PERFORM change_header_bom USING     wa_colo.
            PERFORM change_bady_item USING     wa_colo.
*          ELSEIF L_MODE EQ '2'.
*            PERFORM APPEND_OBJECT_DEPENDENCY USING     WA_COLO
*                                                       L_STLKN.
          ENDIF.

        ENDAT.
*       AT END OF ZSEQU
        AT END OF zsequ.
          l_taend = l_tabix.
          IF l_mode EQ '1'. "CHANGE(ITEM CREATE)
            CLEAR l_mode.
            PERFORM color_part_call_transaction USING    l_tanew
                                                           l_taend
                                                  CHANGING l_zresult
                                                           l_zmsg
                                                           l_mode.
          ENDIF.
        ENDAT.
*       'UPDATE CONTROL MODE' different BDC
*        PERFORM COLOR_PART_BADY_1 USING    WA_COLO
*                                  CHANGING L_MODE.
*        IF L_MODE EQ '1'.
*          PERFORM CHANGE_BADY_ITEM_CREATE1 USING     WA_COLO.
*          L_MODE = '2'.
*        ELSEIF L_MODE EQ '2'.
*          PERFORM BADY_DEPENDENCY USING WA_COLO-DPID.
*        ENDIF.
**       AT END OF ZSEQU
*        AT END OF ZSEQU.
*          L_TAEND = L_TABIX.
*          PERFORM COLOR_PART_CALL_TRANSACTION USING    L_TANEW
*                                                       L_TAEND
*                                              CHANGING L_ZRESULT
*                                                       L_ZMSG
*                                                       L_MODE.
*        ENDAT.

*     BOM ITEM DELETE
      WHEN '2'.
        PERFORM color_part_item_delete USING    wa_colo
                                                l_tabix
                                       CHANGING l_mode
                                                l_stlkn
                                                l_zresult
                                                l_zmsg.
    ENDCASE.

  ENDLOOP.
* BOM ERROR WRITE.
  CLEAR: wa_line_idx, wa_erro_idx.
  REFRESH it_excl. CLEAR it_excl.
* TOTAL LINE COUNT
  DESCRIBE TABLE it_colo LINES wa_line_idx.
* ERROR LINE COUNT
  LOOP AT it_colo.
    l_tabix = sy-tabix.
    IF it_colo-zresult EQ'E'.

      wa_erro_idx = wa_erro_idx + 1.
      MOVE-CORRESPONDING it_colo TO it_excl.
      APPEND it_excl.
      DELETE it_colo INDEX l_tabix.
    ENDIF.
    CLEAR: it_colo, it_excl.
  ENDLOOP.
*  REFRESH IT_COLO. CLEAR IT_COLO.
  WRITE: / text-034, wa_line_idx.
  FORMAT COLOR 6 INTENSIFIED OFF.
  WRITE: / text-035, wa_erro_idx.
  FORMAT COLOR OFF.

* Error appears if is more than one case.
  IF wa_erro_idx GE '1'.
    PERFORM error_write TABLES it_excl
                        USING  'E'.
    REFRESH it_excl. CLEAR it_excl.
  ENDIF.
ENDFORM.                    " COLOR_PART_BDC
*&---------------------------------------------------------------------*
*&      Form  READ_MAST_STPO_STLKN
*&---------------------------------------------------------------------*
FORM read_mast_stpo_stlkn USING    p_matnr
                                   p_werks
                                   p_stlan
                                   p_stlal
                                   p_posnr
                                   p_idnrk
                                   p_zsuff
                                   p_zsequ
                          CHANGING p_mode
                                   p_stlkn.
  DATA: l_zsequ(4) TYPE n,
        l_stlnr TYPE mast-stlnr,
        l_cnt TYPE i.
  l_zsequ = p_zsequ.
  l_zsequ = l_zsequ - 1.

  SELECT SINGLE a~stlnr
                b~stlkn
       FROM mast AS a INNER JOIN stpo AS b
                      ON    b~stlty EQ 'M'
                      AND   a~stlnr EQ b~stlnr
       INTO  (l_stlnr, p_stlkn)
       WHERE a~matnr EQ p_matnr
       AND   a~werks EQ p_werks
       AND   a~stlan EQ p_stlan
       AND   a~stlal EQ p_stlal
       AND   b~posnr EQ p_posnr
       AND   b~idnrk EQ p_idnrk
       AND   b~suff  EQ p_zsuff
       AND   b~sequ  EQ l_zsequ.
  IF sy-subrc EQ 0.
    SELECT COUNT( DISTINCT stasz )
         FROM stas
         INTO l_cnt
         WHERE stlty EQ 'M'
         AND   stlnr EQ l_stlnr
         AND   stlal EQ p_stlal
         AND   stlkn EQ p_stlkn.
    IF l_cnt GT 1.
      p_mode = '4'.
    ELSE.
      p_mode = '3'.
    ENDIF.
  ELSE.
    p_mode = '4'.
  ENDIF.
ENDFORM.                    " READ_MAST_STPO_STLKN
*&---------------------------------------------------------------------*
*&      Form  BOM_ITEM_DELETE
*&---------------------------------------------------------------------*
FORM bom_item_delete USING    p_mtno
                              p_plnt
                              p_usag
                              p_altn
                              p_eono
                              p_stlkn.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' p_mtno,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' p_plnt,    "PLANT
     ' ' 'RC29N-STLAN' p_usag,    "BOM usage
     ' ' 'RC29N-STLAL' p_altn,    "ALT BOM
     ' ' 'RC29N-AENNR' p_eono,    "Change number
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=SETP',

     'X' 'SAPLCSDI'    '0708',
     ' ' 'RC29P-SELPI' p_stlkn,                "
     ' ' 'BDC_OKCODE'  '=CLWI',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',                "
     ' ' 'BDC_OKCODE'  '=FCDL',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCBU'.

* CALL TRANSACTION
  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt
                           MESSAGES INTO it_mess.
ENDFORM.                    " BOM_ITEM_DELETE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_HEADER_BOM
*&---------------------------------------------------------------------*
FORM change_header_bom USING   pa_colo STRUCTURE wa_colo.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' pa_colo-matnr,   "NEXT MATERIAL
     ' ' 'RC29N-WERKS' pa_colo-werks,   "PLANT
     ' ' 'RC29N-STLAN' pa_colo-stlan,   "BOM usage
     ' ' 'RC29N-STLAL' pa_colo-stlal,   "ALT BOM
     ' ' 'RC29N-AENNR' pa_colo-aennr,   "Change number
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCNP'.

ENDFORM.                    " CHANGE_HEADER_BOM
*&---------------------------------------------------------------------*
*&      Form  APPEND_OBJECT_DEPENDENCY
*&---------------------------------------------------------------------*
FORM append_object_dependency USING   pa_colo STRUCTURE wa_colo
                                      p_stlkn.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' pa_colo-matnr,   "NEXT MATERIAL
     ' ' 'RC29N-WERKS' pa_colo-werks,   "PLANT
     ' ' 'RC29N-STLAN' pa_colo-stlan,   "BOM usage
     ' ' 'RC29N-STLAL' pa_colo-stlal,   "ALT BOM
     ' ' 'RC29N-AENNR' pa_colo-aennr,   "Change number
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=SETP',

     'X' 'SAPLCSDI'    '0708',
     ' ' 'RC29P-SELPI' p_stlkn,    "
     ' ' 'BDC_OKCODE'  '=CLWI'.
ENDFORM.                    " APPEND_OBJECT_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  CHANGE_BADY_ITEM
*&---------------------------------------------------------------------*
FORM   change_bady_item USING    pa_colo STRUCTURE wa_colo.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'RC29P-AUSKZ(02)' 'X'   ,    "CHECK
     ' ' 'RC29P-POSNR(02)' pa_colo-posnr,   "BOM item number
     ' ' 'RC29P-IDNRK(02)' pa_colo-idnrk,   "BOM compenent
     ' ' 'RC29P-MENGE(02)' pa_colo-menge,   "Compenent quantity
     ' ' 'RC29P-POSTP(02)' pa_colo-postp,   "Item category
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0130',
     ' ' 'RC29P-ITSOB' pa_colo-itsob,   "procurement type
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0131',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0138',
     ' ' 'ZEITM'       pa_colo-zeitm,    "END ITEM TYPE
     ' ' 'ZSTGB'       pa_colo-zstgb,    "STRUCTURE TYPE
     ' ' 'ZSUFF'       pa_colo-zsuff,    "SUFFIX NO
     ' ' 'ZSEQU'       pa_colo-zsequ,    "SEQUENCE NO
     ' ' 'ZUPGN'       pa_colo-zupgn,    "UPG
     ' ' 'ZINFO'       pa_colo-zseqc,    "COLOR SEQUENCE
     ' ' 'BDC_OKCODE'  '/00'.

  PERFORM dynpro USING:
       'X' 'SAPLCSDI'    '0140',
       ' ' 'BDC_OKCODE'  '=FCNP'.

ENDFORM.                    " CHANGE_BADY_ITEM
*&---------------------------------------------------------------------*
*&      Form  BADY_DEPENDENCY
*&---------------------------------------------------------------------*
FORM bady_dependency USING p_dpid.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',    "
     ' ' 'BDC_OKCODE'  '=WIZU',

     'X' 'SAPLCUKD'    '0130',
     ' ' 'BDC_OKCODE'  '=NEWZ',

     'X' 'SAPLCUKD'    '0130',
     ' ' 'RCUKD-KNNAM(02)' p_dpid, "
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCUKD'    '0130',
     ' ' 'BDC_OKCODE'  '=BACK'.

ENDFORM.                    " BADY_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  CHANGE_END_ITEM_CREATE
*&---------------------------------------------------------------------*
FORM change_end_item_create.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'BDC_OKCODE'  '=FCBU'.
* CALL TRANSACTION
  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt
                           MESSAGES INTO it_mess.
ENDFORM.                    " CHANGE_END_ITEM_CREATE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_END_ITEM_CREATE1
*&---------------------------------------------------------------------*
FORM change_end_item_create1.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCBU'.
* CALL TRANSACTION
  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt
                           MESSAGES INTO it_mess.
ENDFORM.                    " CHANGE_END_ITEM_CREATE1
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM color_part_call_transaction USING    p_tanew
                                          p_taend
                                 CHANGING p_zresult
                                          p_zmsg
                                          p_mode.
  IF p_mode EQ '1' OR  p_mode EQ '2'.
    PERFORM change_end_item_create.
    p_zresult = sy-msgty.
    PERFORM rkc_msg_string CHANGING p_zmsg.
*   MODIFY IT_COLO
    LOOP AT it_colo FROM p_tanew TO p_taend.
      it_colo-zresult = p_zresult.
      it_colo-zmsg    = p_zmsg.
      MODIFY it_colo INDEX sy-tabix TRANSPORTING zresult
                                                 zmsg.
      CLEAR it_colo.
    ENDLOOP.
  ENDIF.
  REFRESH: it_bdc, it_mess.
  CLEAR: it_bdc, it_mess.
ENDFORM.                    " COLOR_PART_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_CALL_TRANSACTION1
*&---------------------------------------------------------------------*
FORM color_part_call_transaction1 USING    p_tanew
                                           p_taend
                                  CHANGING p_zresult
                                           p_zmsg
                                           p_mode.
  IF p_mode EQ '1' OR  p_mode EQ '2'.
    PERFORM change_end_item_create1.
    p_zresult = sy-msgty.
    PERFORM rkc_msg_string CHANGING p_zmsg.
*   MODIFY IT_COLO
    LOOP AT it_colo FROM p_tanew TO p_taend.
      it_colo-zresult = p_zresult.
      it_colo-zmsg    = p_zmsg.
      MODIFY it_colo INDEX sy-tabix TRANSPORTING zresult
                                                 zmsg.
      CLEAR it_colo.
    ENDLOOP.
  ENDIF.
  REFRESH: it_bdc, it_mess.
  CLEAR: it_bdc, it_mess.
ENDFORM.                    " COLOR_PART_CALL_TRANSACTION1
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_ITEM_DELETE
*&---------------------------------------------------------------------*
FORM color_part_item_delete USING    pa_colo STRUCTURE wa_colo
                                     p_tabix
                            CHANGING p_mode
                                     p_stlkn
                                     p_zresult
                                     p_zmsg.
  PERFORM read_mast_stpo_stlkn USING pa_colo-matnr "Material
                                     pa_colo-werks "Plant
                                     pa_colo-stlan "BOM usage
                                     pa_colo-stlal "Alternative
                                     pa_colo-posnr "BOM item
                                     pa_colo-idnrk "BOM
                                     pa_colo-zsuff "SUFFIX
                                     pa_colo-zsequ
                            CHANGING p_mode
                                     p_stlkn.
  IF p_mode EQ '3'. "DELETE(ITEM DELETE)
    IF NOT p_stlkn IS INITIAL.
      PERFORM bom_item_delete USING pa_colo-matnr "Material number
                                    pa_colo-werks "Plant
                                    pa_colo-stlan "BOM usage
                                    pa_colo-stlal "Alternative BOM
                                    pa_colo-aennr "Change number
                                    p_stlkn.      "Item node number

      PERFORM rkc_msg_string CHANGING p_zmsg.
      p_zresult = sy-msgty.
*     MODIFY IT_COLO
      it_colo-zresult = p_zresult.
      it_colo-zmsg    = p_zmsg.
      MODIFY it_colo INDEX p_tabix TRANSPORTING zresult
                                                zmsg.
    ELSE.
*     MODIFY IT_COLO
      it_colo-zresult = 'E'.
      it_colo-zmsg    = 'BOM ITEM to delete does not exist.'.
      MODIFY it_colo INDEX p_tabix TRANSPORTING zresult
                                                zmsg.
    ENDIF.
  ELSEIF p_mode EQ '4'.
*   MODIFY IT_COLO
    it_colo-zresult = 'E'.
    it_colo-zmsg    = 'BOM ITEM to delete does not exist.'.
    MODIFY it_colo INDEX p_tabix TRANSPORTING zresult
                                              zmsg.
  ENDIF.

  CLEAR it_colo.

  REFRESH: it_bdc, it_mess.
  CLEAR: it_bdc, it_mess.
ENDFORM.                    " COLOR_PART_ITEM_DELETE
*&---------------------------------------------------------------------*
*&      Form  MAST_STPO_CHECK
*&---------------------------------------------------------------------*
FORM mast_stpo_check USING    pa_ncol STRUCTURE wa_ncol
                     CHANGING p_mode.
  DATA: l_stlnr LIKE mast-stlnr,
        l_sequ(04) TYPE n.

  l_sequ = pa_ncol-zsequ.

  SELECT SINGLE stlnr
       FROM mast
       INTO l_stlnr
       WHERE matnr EQ pa_ncol-matnr
       AND   werks EQ pa_ncol-werks
       AND   stlan EQ pa_ncol-stlan
       AND   stlal EQ pa_ncol-stlal.
  IF sy-subrc EQ 0.
    p_mode = '1'.
    SELECT posnr
           idnrk
           suff
           sequ
           stlkn
         FROM stpo
         INTO TABLE it_stpo
         WHERE stlty EQ 'M'
         AND   stlnr EQ l_stlnr.
    IF sy-subrc EQ 0.
      SORT it_stpo BY posnr idnrk zsuff zsequ.
      READ TABLE it_stpo WITH KEY posnr  = pa_ncol-posnr
                                  idnrk  = pa_ncol-idnrk
                                  zsuff  = pa_ncol-zsuff
*                                  ZSEQU  = PA_NCOL-ZSEQU
                                  zsequ  = l_sequ
                         BINARY SEARCH.
      IF sy-subrc EQ 0.
        p_mode = '2'.
      ELSE.
        p_mode = '1'.
      ENDIF.
    ELSE.
      p_mode = '1'.
    ENDIF.
  ELSE.
*   BOM HEADER CREATED
    p_mode = '1'.
    PERFORM bom_header USING pa_ncol-matnr  "Material
                             pa_ncol-werks  "Plant
                             pa_ncol-stlan  "BOM usage
                             pa_ncol-stlal  "Alternat BOM
                             pa_ncol-bmeng  "Confirmed qua
                             pa_ncol-stlst. "BOM Status
  ENDIF.
ENDFORM.                    " MAST_STPO_CHECK
*&---------------------------------------------------------------------*
*&      Form  CHECK_IT_STPO
*&---------------------------------------------------------------------*
FORM check_it_stpo USING    pa_ncol STRUCTURE wa_ncol
                   CHANGING p_chk.
  DATA l_sequ(04) TYPE n.
  l_sequ = pa_ncol-zsequ.
  READ TABLE it_stpo WITH KEY posnr  = pa_ncol-posnr
                              idnrk  = pa_ncol-idnrk
                              zsuff  = pa_ncol-zsuff
                              zsequ  = l_sequ
*                              ZSEQU  = PA_NCOL-ZSEQU
                     BINARY SEARCH.
  IF sy-subrc EQ 0.
    p_chk = ' '.
  ELSE.
    p_chk = 'X'.
  ENDIF.
ENDFORM.                    " CHECK_IT_STPO
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_HEADER
*&---------------------------------------------------------------------*
FORM color_part_header USING    pa_colo STRUCTURE wa_colo
                       CHANGING p_mode
                                p_stlkn.

  DATA: l_stlnr LIKE mast-stlnr,
        l_sequ(04) TYPE n.

  l_sequ = pa_colo-zsequ.

  SELECT SINGLE stlnr
       FROM mast
       INTO l_stlnr
       WHERE matnr EQ pa_colo-matnr
       AND   werks EQ pa_colo-werks
       AND   stlan EQ pa_colo-stlan
       AND   stlal EQ pa_colo-stlal.
  IF sy-subrc EQ 0.
    p_mode = '1'.
    SELECT posnr
           idnrk
           suff
           sequ
           stlkn
         FROM stpo
         INTO TABLE it_stpo
         WHERE stlty EQ 'M'
         AND   stlnr EQ l_stlnr.
    IF sy-subrc EQ 0.
      SORT it_stpo BY posnr idnrk zsuff zsequ.
      READ TABLE it_stpo WITH KEY posnr  = pa_colo-posnr
                                  idnrk  = pa_colo-idnrk
                                  zsuff  = pa_colo-zsuff
                                  zsequ  = l_sequ
*                                  ZSEQU  = PA_COLO-ZSEQU
                         BINARY SEARCH.
      IF sy-subrc EQ 0.
        p_mode = '2'.
        p_stlkn = it_stpo-stlkn.
      ELSE.
        p_mode = '1'.
      ENDIF.
    ELSE.
      p_mode = '1'.
    ENDIF.
  ELSE.
*   BOM HEADER CREATED
    p_mode = '1'.
    PERFORM bom_header USING pa_colo-matnr  "Material
                             pa_colo-werks  "Plant
                             pa_colo-stlan  "BOM usage
                             pa_colo-stlal  "Alternat BOM
                             pa_colo-bmeng  "Confirmed qua
                             pa_colo-stlst. "BOM Status
  ENDIF.
ENDFORM.                    " COLOR_PART_HEADER
*&---------------------------------------------------------------------*
*&      Form  OBJECT_DEPENDENCY_APPENDING
*&---------------------------------------------------------------------*
FORM object_dependency_appending.
  DATA: l_stlkn TYPE stpo-stlkn,
        l_tabix TYPE sy-tabix,
        l_tanew TYPE sy-tabix,
        l_taend TYPE sy-tabix,
        l_zresult LIKE it_excl-zresult,
        l_zmsg LIKE it_excl-zmsg,
        l_mode.
  CLEAR: wa_line_idx, wa_erro_idx.
* MATERIAL & OBJECT DEPENDENCY APPENDING
  LOOP AT it_colo WHERE upct NE '2'
                  AND   zresult NE 'E'.
    l_tabix = sy-tabix.
    wa_line_idx = wa_line_idx + 1.

    wa_colo = it_colo.
*     BOM ITEM ADDITION & OBJECT DEPENDENCY CREATE
*       AT NEW ZSEQU
    AT NEW zseqc.
      l_mode = '1'.
      l_tanew = l_tabix.
      PERFORM mast_stpo_stlkn USING wa_colo
                              CHANGING l_stlkn.
      PERFORM append_object_dependency USING     wa_colo
                                                 l_stlkn.

    ENDAT.
*   'UPDATE CONTROL MODE' different BDC
    PERFORM bady_dependency USING wa_colo-dpid.
*       AT END OF ZSEQU
    AT END OF zseqc.
      l_taend = l_tabix.
      PERFORM color_part_call_transaction1 USING    l_tanew
                                                   l_taend
                                          CHANGING l_zresult
                                                   l_zmsg
                                                   l_mode.

    ENDAT.
    CLEAR:  it_colo.
  ENDLOOP.
* BOM ERROR WRITE.
  REFRESH it_excl. CLEAR it_excl.
* ERROR LINE COUNT
  LOOP AT it_colo.
    wa_line_idx = wa_line_idx + 1.
    IF it_colo-zresult EQ 'E'.
      wa_erro_idx = wa_erro_idx + 1.
      MOVE-CORRESPONDING it_colo TO it_excl.
      APPEND it_excl.
    ENDIF.
    CLEAR: it_colo, it_excl.
  ENDLOOP.
*  REFRESH IT_COLO. CLEAR IT_COLO.
  WRITE: / text-036,
            wa_line_idx.
  FORMAT COLOR 6 INTENSIFIED OFF.
  WRITE: / text-037,
            wa_erro_idx.
  FORMAT COLOR OFF.

* Error appears if is more than one case.
  IF wa_erro_idx GE '1'.
    PERFORM error_write TABLES it_excl
                        USING  'E'.
    REFRESH it_excl. CLEAR it_excl.
  ENDIF.
ENDFORM.                    " OBJECT_DEPENDENCY_APPENDING
*&---------------------------------------------------------------------*
*&      Form  MAST_STPO_STLKN
*&---------------------------------------------------------------------*
FORM mast_stpo_stlkn USING    pa_colo STRUCTURE wa_colo
                     CHANGING p_stlkn.
  DATA: l_sequ TYPE stpo-sequ.
  l_sequ = pa_colo-zsequ.
  SELECT SINGLE b~stlkn
       FROM mast AS a INNER JOIN stpo AS b
                      ON    b~stlty EQ 'M'
                      AND   a~stlnr EQ b~stlnr
       INTO   p_stlkn
       WHERE a~matnr EQ pa_colo-matnr
       AND   a~werks EQ pa_colo-werks
       AND   a~stlan EQ pa_colo-stlan
       AND   a~stlal EQ pa_colo-stlal
       AND   b~posnr EQ pa_colo-posnr
       AND   b~idnrk EQ pa_colo-idnrk
       AND   b~suff  EQ pa_colo-zsuff
       AND   b~sequ  EQ l_sequ.
ENDFORM.                    " MAST_STPO_STLKN
*&---------------------------------------------------------------------*
*&      Form  MAST_STPO_STLKN_1
*&---------------------------------------------------------------------*
FORM mast_stpo_stlkn_1 USING    pa_bmco STRUCTURE wa_bmco
                       CHANGING p_stlkn.
  DATA: l_sequ TYPE stpo-sequ.
*  L_SEQU = PA_BMCO-SEQC.
  l_sequ = pa_bmco-sequ.
  SELECT SINGLE b~stlkn
       FROM mast AS a INNER JOIN stpo AS b
                      ON    b~stlty EQ 'M'
                      AND   a~stlnr EQ b~stlnr
       INTO   p_stlkn
       WHERE a~matnr EQ pa_bmco-mtno
       AND   a~werks EQ pa_bmco-plnt
       AND   a~stlan EQ pa_bmco-usag
       AND   a~stlal EQ pa_bmco-altn
       AND   b~posnr EQ pa_bmco-pref
       AND   b~idnrk EQ pa_bmco-comp
       AND   b~suff  EQ pa_bmco-suff
       AND   b~sequ  EQ l_sequ.
ENDFORM.                    " MAST_STPO_STLKN_1
*&---------------------------------------------------------------------*
*&      Form  EXCEL_DATA_BDC_PROCESS
*&---------------------------------------------------------------------*
FORM excel_data_bdc_process.
*---/// START : Issue: PP-20041006-001
*---/// Changed on 2004.10.06, Changed by BSBAE, Request by IYCHOI
*  PERFORM upload_process.
*  PERFORM dependency_ecm_check CHANGING wa_check.
*  IF wa_check NE 'X'.
*    PERFORM data_process CHANGING wa_check.
*    IF wa_check NE 'X'.
**     COLOR PART & NON COLOR PART PARTITION
*      PERFORM color_part_partition.
**     BOM ITEM CREATE
*      PERFORM bdc_process.
**     BOM EXPLODED.
*      REFRESH it_bom_exploded. CLEAR it_bom_exploded.
*      PERFORM bom_exploded TABLES it_bom_exploded.
**     MARA CONFIGURABLE MATERIAL CHECK
*      IF NOT it_bom_exploded[] IS INITIAL.
*        PERFORM mara_configurable_material TABLES it_bom_exploded.
*        PERFORM mm02_configurable_material_bdc.
*      ENDIF.
*      PERFORM object_dependency_appending.
**      PERFORM JOB_PROCESS.
*    ENDIF.
*  ENDIF.
*---/// END : Issue: PP-20041006-001
ENDFORM.                    " EXCEL_DATA_BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  TABLE_SELECTION_DATA_BDC
*&---------------------------------------------------------------------*
FORM table_selection_data_bdc.
  CLEAR wa_check.
  PERFORM read_process.
  PERFORM dependency_change_no_check CHANGING wa_check.
  IF wa_check NE 'X'.
*   COLOR PART & NON COLOR PART PARTITION
    PERFORM table_color_part_partition.
*   BOM ITEM CREATE
    PERFORM table_selection_bdc_process.
*   BOM EXPLODED.
    REFRESH it_bom_exploded. CLEAR it_bom_exploded.
    PERFORM table_bom_exploded TABLES it_bom_exploded.
*   MARA CONFIGURABLE MATERIAL CHECK
    IF NOT it_bom_exploded[] IS INITIAL.
      PERFORM mara_configurable_material TABLES it_bom_exploded.
      PERFORM mm02_configurable_material_bdc.
    ENDIF.

*   PERFORM OBJECT_DEPENDENCY_APPENDING.

*/// START : Issue:20041115-003 Changed on 2004.11.24, Changed by BSBAE
*    PERFORM table_object_dependency.
    PERFORM manage_object_dependency.
*/// END   : Issue:20041115-003 Changed on 2004.11.24, Changed by BSBAE
*   ZTBM_ABXEBMDT TABLE UPDATE

    PERFORM update_job_process.
*   UPDATE & SUB MATERIAL & ENGINE BOM UPDATE
    PERFORM sub_engine_bom_update.
*   Material Master Update Data
    PERFORM material_master_update.

  ENDIF.
* MATERIAL CHANGED LIST
  PERFORM bom_exploded_write.
  PERFORM mm02_material_change_write.
ENDFORM.                    " TABLE_SELECTION_DATA_BDC
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM read_process.
  SELECT *
    FROM ztbm_abxebmdt
    INTO TABLE it_bmdt
   WHERE zedat EQ p_zedat
     AND zbtim EQ p_zbtim.
  IF sy-subrc EQ 0.
    DESCRIBE TABLE it_bmdt LINES wa_line_idx.
    WRITE: / 'BOM SELECTION TOTAL LINES : ' COLOR 4, wa_line_idx.
    it_bmdt-zresult = ' '.
    it_bmdt-zmsg = ' '.
*    LT_BMDT-ZBTIM = SY-UZEIT.
    MODIFY it_bmdt TRANSPORTING zresult
                                zmsg
                             WHERE mtno GE space
                             AND   plnt GE space
                             AND   usag GE space
                             AND   altn GE space
                             AND   pref GE space
                             AND   comp GE space
                             AND   suff GE space
                             AND   sequ GE space
                             AND   seqc GE space.
  ELSE.
    WRITE: / text-001 COLOR 4.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DEPENDENCY_CHANGE_NO_CHECK
*&---------------------------------------------------------------------*
FORM dependency_change_no_check CHANGING pa_check.
  DATA l_tabix TYPE sy-tabix.
  DATA: lt_bmdt LIKE it_bmdt OCCURS 0 WITH HEADER LINE,
        l_erro_dep LIKE wa_erro_idx,
        l_erro_ecm LIKE wa_erro_idx,
        l_erro_mat LIKE wa_erro_idx,
        l_knnam LIKE cukb-knnam.
  DATA: BEGIN OF lt_aenr OCCURS 0,
          aennr TYPE aenr-aennr,
        END OF lt_aenr.
  DATA: BEGIN OF mt_aenr OCCURS 0,
          aennr TYPE aenr-aennr,
        END OF mt_aenr.
  DATA: BEGIN OF lt_chck OCCURS 0,
          matnr TYPE marc-matnr,
          werks TYPE marc-werks,
        END OF lt_chck.
  DATA: BEGIN OF lt_marc OCCURS 0,
          matnr TYPE marc-matnr,
          werks TYPE marc-werks,
        END OF lt_marc.
  CLEAR: wa_erro_idx, wa_line_idx.

* DEPENDENCY CHECK
  LOOP AT it_bmdt WHERE clpt EQ 'C'
                  AND   upct EQ '1'.

    wa_line_idx = wa_line_idx + 1.
    SELECT SINGLE knnam
                FROM cukb
                INTO l_knnam
                WHERE knnam EQ it_bmdt-dpid.
    IF sy-subrc NE 0.
      lt_bmdt = it_bmdt.
      lt_bmdt-zresult = 'L'.
      lt_bmdt-zmsg    = 'Dependency does not exist'.
      APPEND lt_bmdt.
      l_erro_dep = l_erro_dep + 1.
    ENDIF.
    CLEAR: it_bmdt, lt_bmdt.
  ENDLOOP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / text-028, wa_line_idx.
  WRITE: / text-029,
            l_erro_dep.
  FORMAT COLOR OFF.

* CHANGE NUMBER CHECK
  DESCRIBE TABLE it_bmdt LINES wa_line_idx.

  LOOP AT it_bmdt.
    it_subm-stlan = it_bmdt-usag.
    it_subm-stlal = it_bmdt-altn.
    it_subm-matnr = lt_chck-matnr = it_bmdt-mtno.
    it_subm-werks = lt_chck-werks = it_bmdt-plnt.

    COLLECT it_subm.
    COLLECT lt_chck.
    lt_chck-matnr = it_bmdt-comp.
    lt_chck-werks = it_bmdt-plnt.
    COLLECT lt_chck.

    lt_aenr-aennr = it_bmdt-eono.
    COLLECT lt_aenr.
    CLEAR: it_bmdt, lt_chck, lt_aenr, it_subm.
  ENDLOOP.
  DESCRIBE TABLE lt_aenr LINES wa_line_idx.

  IF NOT lt_aenr[] IS INITIAL.
    SELECT aennr
         FROM aenr
         INTO TABLE mt_aenr
         FOR ALL ENTRIES IN lt_aenr
         WHERE aennr EQ lt_aenr-aennr.
    IF sy-subrc EQ 0.
      SORT mt_aenr.
      LOOP AT it_bmdt.
        READ TABLE mt_aenr WITH KEY aennr = it_bmdt-eono
                           BINARY SEARCH.
        IF sy-subrc NE 0.
          l_erro_ecm = l_erro_ecm + 1.
          lt_bmdt = it_bmdt.
          lt_bmdt-zresult = 'L'.
          lt_bmdt-zmsg = 'Change No does not exist'.
          APPEND lt_bmdt.
        ENDIF.
      ENDLOOP.

      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE: / text-030, wa_line_idx.
      WRITE: / text-031,
                l_erro_ecm.
      FORMAT COLOR OFF.
    ENDIF.
  ENDIF.

* MARC CHECK
  DESCRIBE TABLE lt_chck LINES wa_line_idx.
  IF NOT lt_chck[] IS INITIAL.
    SELECT matnr
           werks
         FROM marc
         INTO TABLE lt_marc
         FOR ALL ENTRIES IN lt_chck
         WHERE matnr EQ lt_chck-matnr
         AND   werks EQ lt_chck-werks.
    IF sy-subrc EQ 0.
      SORT lt_marc BY matnr werks.
      LOOP AT it_bmdt.
        l_tabix = sy-tabix.
*       NEXT MATERIAL CHECK
        READ TABLE lt_marc WITH KEY matnr = it_bmdt-mtno
                                    werks = it_bmdt-plnt
                           BINARY SEARCH.
        IF sy-subrc NE 0.
          it_bmdt-zresult = 'L'.
          CONCATENATE 'NENT MATERIAL : ' it_bmdt-mtno
                      '  Material number does not exist'
                 INTO it_bmdt-zmsg.
          MODIFY it_bmdt INDEX l_tabix TRANSPORTING zresult
                                                    zmsg.
        ENDIF.
*       COMPONENT CHECK
        READ TABLE lt_marc WITH KEY matnr = it_bmdt-comp
                                    werks = it_bmdt-plnt
                           BINARY SEARCH.
        IF sy-subrc NE 0.
          it_bmdt-zresult = 'L'.
          IF it_bmdt-zmsg IS INITIAL.
            CONCATENATE  'COMPORNENT : ' it_bmdt-comp
                         '  Material number does not exist'
                    INTO it_bmdt-zmsg.
          ELSE.
            CONCATENATE  it_bmdt-zmsg '  COMPORNENT : ' it_bmdt-comp
                         '  Material number does not exist'
                    INTO it_bmdt-zmsg.
          ENDIF.

          MODIFY it_bmdt INDEX l_tabix TRANSPORTING zresult
                                                    zmsg.
        ENDIF.
      ENDLOOP.
    ELSE.
      it_bmdt-zresult = 'L'.
      it_bmdt-zmsg = ' Material number does not exist'.
      MODIFY it_bmdt TRANSPORTING zmsg
                                  zresult
                               WHERE mtno GE space
                               AND   plnt GE space.
    ENDIF.
  ENDIF.
  LOOP AT it_bmdt WHERE zresult EQ 'L'.
    lt_bmdt = it_bmdt.
    l_erro_mat = l_erro_mat + 1.
    APPEND lt_bmdt.
    CLEAR: lt_bmdt, it_bmdt.
  ENDLOOP.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / text-038, wa_line_idx.
  WRITE: / text-006, l_erro_mat.
  FORMAT COLOR OFF.

  IF NOT lt_bmdt[] IS INITIAL.
*   ERROR LIST WRITE
    pa_check = 'X'.
    lt_bmdt-zbdat = sy-datum.
    lt_bmdt-zbnam = sy-uname.
*    LT_BMDT-ZBTIM = SY-UZEIT.
    MODIFY lt_bmdt TRANSPORTING zbdat
                                zbtim
                                zbnam
                             WHERE mtno GE space
                             AND   plnt GE space
                             AND   usag GE space
                             AND   altn GE space
                             AND   pref GE space
                             AND   comp GE space
                             AND   suff GE space
                             AND   sequ GE space
                             AND   seqc GE space.
    PERFORM error_data_write TABLES    lt_bmdt
                             USING     'L'.
    DELETE ADJACENT DUPLICATES FROM lt_bmdt COMPARING
                                    mtno plnt usag altn pref
                                    comp suff sequ seqc.
    PERFORM update_ztbm_abxebmdt TABLES    lt_bmdt.
  ENDIF.
ENDFORM.                    " DEPENDENCY_CHANGE_NO_CHECK
*&---------------------------------------------------------------------*
*&      Form  ERROR_DATA_WRITE
*&---------------------------------------------------------------------*
FORM error_data_write TABLES   pt_bmdt STRUCTURE it_bmdt
                      USING    p_zresult.
  WRITE: /(20)  text-039,
          (10)  text-040,
          (10)  text-041,
          (10)  text-042,
          (10)  text-043,
          (20)  text-044,
          (10)  text-045,
          (10)  text-046,
          (15)  text-047,
          (15)  text-048,
          (20)  text-049,
          (10)  text-050,
          (10)  text-051,
          (10)  text-052,
          (20)  text-053,
          (15)  text-054,
          (10)  text-055,
          (15)  text-056,
          (10)  text-057,
          (10)  text-058,
          (30)  text-059,
          (15)  text-060,
          (20)  text-061,
          (10)  text-062,
          (15)  text-063,
         (220)  text-064.
  LOOP AT pt_bmdt WHERE zresult EQ p_zresult.
    WRITE: /(20) pt_bmdt-mtno,
            (10) pt_bmdt-plnt,
            (10) pt_bmdt-usag,
            (10) pt_bmdt-altn,
            (10) pt_bmdt-pref,
            (20) pt_bmdt-comp,
            (10) pt_bmdt-suff,
            (10) pt_bmdt-sequ,
            (15) pt_bmdt-seqc,
            (15) pt_bmdt-eono,
            (20) pt_bmdt-bqty UNIT pt_bmdt-hunt,
            (10) pt_bmdt-hunt,
            (10) pt_bmdt-stat,
            (10) pt_bmdt-itca,
            (20) pt_bmdt-qnty UNIT pt_bmdt-unit,
            (15) pt_bmdt-stgb,
            (10) pt_bmdt-unit,
            (15) pt_bmdt-sppr,
            (10) pt_bmdt-eitm,
            (10) pt_bmdt-clpt,
            (30) pt_bmdt-dpid,
            (15) pt_bmdt-upct,
            (20) pt_bmdt-upgn,
            (10) pt_bmdt-zmode,
            (15) pt_bmdt-zresult,
            (220) pt_bmdt-zmsg.
  ENDLOOP.

ENDFORM.                    " ERROR_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTBM_ABXEBMDT
*&---------------------------------------------------------------------*
FORM update_ztbm_abxebmdt TABLES   pt_bmdt STRUCTURE it_bmdt.
  UPDATE ztbm_abxebmdt FROM TABLE pt_bmdt.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " UPDATE_ZTBM_ABXEBMDT
*&---------------------------------------------------------------------*
*&      Form  TABLE_COLOR_PART_PARTITION
*&---------------------------------------------------------------------*
FORM table_color_part_partition.
  LOOP AT it_bmdt.
    IF it_bmdt-clpt EQ 'C'.
      MOVE-CORRESPONDING it_bmdt TO it_bmco.
      APPEND it_bmco. CLEAR it_bmco.
    ELSE.
      IF it_bmdt-eitm EQ 'M' AND it_bmdt-dpid NE ' '.
        MOVE-CORRESPONDING it_bmdt TO it_bmco.
        APPEND it_bmco. CLEAR it_bmco.
      ELSE.
        MOVE-CORRESPONDING it_bmdt TO it_bmnc.
        APPEND it_bmnc. CLEAR it_bmnc.
      ENDIF.
    ENDIF.
    CLEAR it_bmdt.
  ENDLOOP.
  REFRESH it_bmdt. CLEAR it_bmdt.
ENDFORM.                    " TABLE_COLOR_PART_PARTITION
*&---------------------------------------------------------------------*
*&      Form  TABLE_SELECTION_BDC_PROCESS
*&---------------------------------------------------------------------*
FORM table_selection_bdc_process.
* NON COLOR PART BDC
  PERFORM table_non_color_part_bdc.
* COLOR PART BDC
  PERFORM table_color_part_bdc.
ENDFORM.                    " TABLE_SELECTION_BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  TABLE_NON_COLOR_PART_BDC
*&---------------------------------------------------------------------*
FORM table_non_color_part_bdc.
  DATA: l_chk(1),
        l_tabix TYPE sy-tabix,
        l_tanew TYPE sy-tabix,
        l_taend TYPE sy-tabix,
        l_mode,
        l_stlkn TYPE stpo-stlkn,
        l_zresult LIKE it_excl-zresult,
        l_zmsg LIKE it_excl-zmsg.
* SORTING
  SORT it_bmnc BY mtno plnt usag altn pref
                  comp suff sequ upct eono.
* BDC PROCESS
  LOOP AT it_bmnc.
    l_tabix = sy-tabix.
    wa_bmnc = it_bmnc.
    CLEAR: l_stlkn.
*   UPDATE CONTROL TYPE
    IF wa_bmnc-upct EQ '2'.
*     BOM ITEM DELETE
      PERFORM non_col_mast_stpo_read USING    wa_bmnc
                                     CHANGING l_mode
                                              l_stlkn.
      IF l_mode EQ '3'. "DELETE(ITEM DELETE)
        IF NOT l_stlkn IS INITIAL.
          PERFORM table_non_col_bom_delete USING   wa_bmnc
                                                   l_stlkn. "Item nodeNO
          IF sy-subrc EQ 0 AND sy-msgno EQ '031'.
            l_zresult = 'S'.
          ELSE.
            l_zresult = 'E'.
          ENDIF.
          PERFORM rkc_msg_string CHANGING l_zmsg.
*       MODIFY IT_BMNC
          it_bmnc-zmode = 'D'.
          it_bmnc-zresult = l_zresult.
          it_bmnc-zmsg    = l_zmsg.
          MODIFY it_bmnc INDEX l_tabix TRANSPORTING zmode
                                                    zresult
                                                    zmsg.
        ELSE.
          it_bmnc-zmode = 'D'.
          it_bmnc-zresult = 'E'.
          it_bmnc-zmsg    = 'BOM ITEM to delete does not exist.'.
          MODIFY it_bmnc INDEX l_tabix TRANSPORTING zmode
                                                    zresult
                                                    zmsg.
        ENDIF.
      ELSEIF l_mode EQ '4'.
*       MODIFY IT_BMNC
        it_bmnc-zmode = 'D'.
        it_bmnc-zresult = 'E'.
        it_bmnc-zmsg    = 'BOM ITEM to delete does not exist.'.
        MODIFY it_bmnc INDEX l_tabix TRANSPORTING zmode
                                                  zresult
                                                  zmsg.
      ENDIF.
      CLEAR it_bmnc.
      REFRESH: it_bdc, it_mess.
      CLEAR: it_bdc, it_mess.

*   BOM ITEM CREATE
    ELSEIF wa_bmnc-upct EQ '1'.
*     AT NEW ALTN
      AT NEW altn.
        l_tanew = l_tabix.
        CLEAR l_mode.
        CLEAR it_stpo. REFRESH it_stpo.
*       BOM CHECK & BOM HEADER CREATED
        PERFORM talbe_mast_stpo_check USING    wa_bmnc
                                      CHANGING l_mode.
        IF l_mode EQ '1' OR  l_mode EQ '2'.
          PERFORM table_non_col_change_bom USING wa_bmnc.
          l_mode = '1'.
        ENDIF.
      ENDAT.

*     'UPDATE CONTROL MODE' different BDC
      IF l_mode EQ '1'.
*       BOM ITEM DATA already CHECK
        CLEAR l_chk.
        PERFORM table_check_it_stpo USING    wa_bmnc
                                    CHANGING l_chk.
        IF l_chk EQ 'X'.
          PERFORM table_non_col_bady USING wa_bmnc.
        ELSE.
          it_bmnc-zmode   = 'C'.
          it_bmnc-zresult = 'E'.
          it_bmnc-zmsg = 'BOM ITEM DATA already.'.
          MODIFY it_bmnc INDEX l_tabix TRANSPORTING zmode
                                                    zresult
                                                    zmsg.
          CLEAR it_bmnc.
        ENDIF.
      ENDIF.

*     AT END OF ALTN
      AT END OF altn.
        l_taend = l_tabix.
        IF l_mode EQ '1'.
          PERFORM non_col_end_item_create.
          l_zresult = sy-msgty.
          PERFORM rkc_msg_string CHANGING l_zmsg.
*         MODIFY IT_BMNC
          LOOP AT it_bmnc FROM l_tanew TO l_taend.
*---/// START : Issue: PP-20041006-001
*---/// Changed on 2004.10.06, Changed by BSBAE, Request by IYCHOI
            IF l_zresult IS INITIAL OR
               l_zresult EQ 'A'     OR
               l_zresult EQ 'X'.
*---/// END : Issue: PP-20041006-001
              it_bmnc-zresult = 'E'.
            ELSE.
              it_bmnc-zresult = l_zresult.
            ENDIF.
            it_bmnc-zmode   = 'C'.
            it_bmnc-zmsg    = l_zmsg.
            MODIFY it_bmnc INDEX sy-tabix TRANSPORTING zmode
                                                       zresult
                                                       zmsg.
            CLEAR it_bmnc.
          ENDLOOP.
          CLEAR l_zresult.
        ENDIF.

        REFRESH: it_bdc, it_mess.
        CLEAR: it_bdc, it_mess.
      ENDAT.
    ENDIF.
    CLEAR: it_bmnc, wa_bmnc.
  ENDLOOP.

* BOM ERROR WRITE.
  CLEAR: wa_line_idx, wa_erro_idx.
  REFRESH it_bmdt. CLEAR it_bmdt.
* TOTAL LINE COUNT
  DESCRIBE TABLE it_bmnc LINES wa_line_idx.
* ERROR LINE COUNT
  LOOP AT it_bmnc.
    MOVE-CORRESPONDING it_bmnc TO it_bmdt.
    it_bmdt-zbdat = sy-datum.
    it_bmdt-zbnam = sy-uname.
    IF it_bmnc-zresult EQ'E'.
      wa_erro_idx = wa_erro_idx + 1.
    ELSE.
      it_bmdt-zbtim = sy-uzeit.
    ENDIF.
    APPEND it_bmdt.
    CLEAR: it_bmnc, it_bmdt.
  ENDLOOP.
  WRITE: / text-032, wa_line_idx.
  FORMAT COLOR 6 INTENSIFIED OFF.
  WRITE: / text-033, wa_erro_idx.
  FORMAT COLOR OFF.
  REFRESH it_bmnc. CLEAR it_bmnc.

* Error appears if is more than one case.
  IF wa_erro_idx GE '1'.
    PERFORM error_data_write TABLES it_bmdt
                             USING  'E'.
  ENDIF.
  it_abxebmdt[] = it_bmdt[].
  REFRESH it_bmdt. CLEAR it_bmdt.

ENDFORM.                    " TABLE_NON_COLOR_PART_BDC
*&---------------------------------------------------------------------*
*&      Form  TABLE_COLOR_PART_BDC
*&---------------------------------------------------------------------*
FORM table_color_part_bdc.
  DATA: l_tabix TYPE sy-tabix,
        l_tanew TYPE sy-tabix,
        l_taend TYPE sy-tabix,
        l_mode,
        l_chk,
        l_stlkn TYPE stpo-stlkn,
        l_zresult LIKE it_excl-zresult,
        l_zmsg LIKE it_excl-zmsg.

* SORTING
  SORT it_bmco BY mtno plnt usag altn pref
                  comp suff sequ upct eono seqc.
  LOOP AT it_bmco WHERE upct EQ '1'
                    AND sequ EQ '1'
                    AND zresult NE 'E'.

    l_tabix = sy-tabix.
    wa_bmco = it_bmco.


*/// START : Issue:20041115-003 Changed on 2004.11.24, Changed by BSBAE
*    CASE wa_bmco-upct.
**     BOM ITEM ADDITION & OBJECT DEPENDENCY CREATE
*      WHEN '1'.
**       AT NEW SEQU
*        AT NEW sequ.
*          l_tanew = l_tabix.
*          CLEAR l_mode.
*          CLEAR it_stpo. REFRESH it_stpo.
*          PERFORM table_color_part_header USING    wa_bmco
*                                          CHANGING l_mode
*                                                   l_stlkn.
*          IF l_mode EQ '1'. "CHANGE(ITEM CREATE)
*            PERFORM table_change_bom USING    wa_bmco.
*            PERFORM table_change_bady USING   wa_bmco.
*          ENDIF.
*        ENDAT.
*
**        IF L_MODE EQ '1'.
**          PERFORM TABLE_COL_CHECK_IT_STPO USING    WA_BMCO
**                                          CHANGING L_CHK.
**          IF L_CHK EQ 'X'.
**            PERFORM TABLE_CHANGE_BADY USING WA_BMCO.
**          ENDIF.
**        ENDIF.
*
**       AT END OF SEQU
*        AT END OF SEQU.
*
*          l_taend = l_tabix.
**          PERFORM TABLE_CHANGE_BADY USING WA_BMCO.
*          IF l_mode EQ '1'.
*
*            PERFORM color_part_transaction USING    l_tanew
*                                                    l_taend
*                                           CHANGING l_zresult
*                                                    l_zmsg
*                                                    l_mode.
*          ENDIF.
*          REFRESH: it_bdc, it_mess.
*          CLEAR: it_bdc, it_mess.
*        ENDAT.
*
*      WHEN '2'.
*        PERFORM table_color_part_delete USING    wa_bmco
*                                                  l_tabix
*                                         CHANGING l_mode
*                                                  l_stlkn
*                                                  l_zresult
*                                                  l_zmsg.
*    ENDCASE.
    ON CHANGE OF wa_bmco-mtno OR wa_bmco-plnt OR wa_bmco-usag OR
                 wa_bmco-altn OR wa_bmco-pref OR wa_bmco-comp OR
                 wa_bmco-suff OR wa_bmco-sequ.
*    AT NEW sequ.
      l_tanew = l_tabix.
      CLEAR l_mode.
      CLEAR it_stpo. REFRESH it_stpo.
      PERFORM table_color_part_header USING    wa_bmco
                                               l_mode
                                               l_stlkn.
*      IF l_mode EQ '1'. "CHANGE(ITEM CREATE)
      IF it_bmco-zresult EQ 'E'.
        MODIFY it_bmco TRANSPORTING zmode zresult zmsg
         WHERE mtno = wa_bmco-mtno
           AND plnt = wa_bmco-plnt
           AND usag = wa_bmco-usag
           AND altn = wa_bmco-altn
           AND pref = wa_bmco-pref
           AND comp = wa_bmco-comp
           AND suff = wa_bmco-suff
           AND sequ = wa_bmco-sequ.
        CONTINUE.
      ENDIF.

      PERFORM check_color_exist USING wa_bmco.
      IF wa_bmco-zresult EQ 'E'.
        MODIFY it_bmco TRANSPORTING zmode zresult zmsg
         WHERE mtno = wa_bmco-mtno
           AND plnt = wa_bmco-plnt
           AND usag = wa_bmco-usag
           AND altn = wa_bmco-altn
           AND pref = wa_bmco-pref
           AND comp = wa_bmco-comp
           AND suff = wa_bmco-suff
           AND sequ = wa_bmco-sequ.
        CONTINUE.
      ENDIF.

      PERFORM table_change_bom USING    wa_bmco.
      PERFORM table_change_bady USING   wa_bmco.
*      ENDIF.
      l_taend = l_tabix.
*      IF l_mode EQ '1'.
      PERFORM color_part_transaction USING    l_tanew
                                              l_taend
                                     CHANGING l_zresult
                                              l_zmsg
                                              l_mode.
*      ENDIF.
      COMMIT WORK AND WAIT.
      REFRESH: it_bdc, it_mess.
      CLEAR: it_bdc, it_mess.
    ENDON.


*    AT END OF sequ.
*      l_taend = l_tabix.
**      IF l_mode EQ '1'.
*      PERFORM color_part_transaction USING    l_tanew
*                                              l_taend
*                                     CHANGING l_zresult
*                                              l_zmsg
*                                              l_mode.
**      ENDIF.
*      REFRESH: it_bdc, it_mess.
*      CLEAR: it_bdc, it_mess.
*    ENDAT.
*/// END  : Issue:20041115-003 Changed on 2004.11.24, Changed by BSBAE

    CLEAR l_chk.
  ENDLOOP.
* BOM ERROR WRITE.
  CLEAR: wa_line_idx, wa_erro_idx.
* TOTAL LINE COUNT
  DESCRIBE TABLE it_bmco LINES wa_line_idx.
* ERROR LINE COUNT
  REFRESH: it_bmdt. CLEAR: it_bmdt.
  LOOP AT it_bmco.
    l_tabix = sy-tabix.
    MOVE-CORRESPONDING it_bmco TO it_bmdt.
    it_bmdt-zbdat = sy-datum.
    it_bmdt-zbnam = sy-uname.
    IF it_bmco-zresult EQ'E'.
      wa_erro_idx = wa_erro_idx + 1.
      DELETE it_bmco INDEX l_tabix.
    ELSE.
      it_bmdt-zbtim = sy-uzeit.
    ENDIF.
    APPEND it_bmdt.
    CLEAR: it_bmco, it_bmdt.
  ENDLOOP.
*  REFRESH IT_BMCO. CLEAR IT_BMCO.
  WRITE: / text-034, wa_line_idx.
  FORMAT COLOR 6 INTENSIFIED OFF.
  WRITE: / text-035, wa_erro_idx.
  FORMAT COLOR OFF.

ENDFORM.                    " TABLE_COLOR_PART_BDC
*&---------------------------------------------------------------------*
*&      Form  TALBE_MAST_STPO_CHECK
*&---------------------------------------------------------------------*
FORM talbe_mast_stpo_check USING    pa_bmnc STRUCTURE wa_bmnc
                           CHANGING p_mode.
  DATA: l_stlnr LIKE mast-stlnr,
        l_bqty(20),
        l_sequ  LIKE wa_bmnc-sequ.
  l_sequ = pa_bmnc-sequ.
  SELECT SINGLE stlnr
       FROM mast
       INTO l_stlnr
       WHERE matnr EQ pa_bmnc-mtno
       AND   werks EQ pa_bmnc-plnt
       AND   stlan EQ pa_bmnc-usag
       AND   stlal EQ pa_bmnc-altn.
  IF sy-subrc EQ 0.
    p_mode = '1'.

    DATA: lw_topmat LIKE cstmat.

    DATA: lt_stb LIKE stpox OCCURS 0 WITH HEADER LINE.

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
         EXPORTING
              aumng                 = 0
              capid                 = c_capid
              cuovs                 = '0'
              datuv                 = sy-datum
              mktls                 = 'X'
              cuobj                 = '999999999999'
              mtnrv                 = pa_bmnc-mtno
              stpst                 = 0
              stlan                 = pa_bmnc-usag
              stlal                 = pa_bmnc-altn
              svwvo                 = 'X'
              werks                 = pa_bmnc-plnt
              vrsvo                 = 'X'
         IMPORTING
              topmat                = lw_topmat
         TABLES
              stb                   = lt_stb
         EXCEPTIONS
              alt_not_found         = 1
              call_invalid          = 2
              material_not_found    = 3
              missing_authorization = 4
              no_bom_found          = 5
              no_plant_data         = 6
              no_suitable_bom_found = 7
              conversion_error      = 8
              OTHERS                = 9.

    CLEAR: it_stpo, it_stpo[].
    LOOP AT lt_stb.
      MOVE: lt_stb-posnr TO it_stpo-posnr,
            lt_stb-idnrk TO it_stpo-idnrk,
            lt_stb-suff  TO it_stpo-zsuff,
            lt_stb-sequ  TO it_stpo-zsequ,
            lt_stb-stlkn TO it_stpo-stlkn.
      APPEND it_stpo.
    ENDLOOP.

    SORT it_stpo BY posnr idnrk zsuff zsequ.
    READ TABLE it_stpo WITH KEY posnr  = pa_bmnc-pref
                                idnrk  = pa_bmnc-comp
                                zsuff  = pa_bmnc-suff
                                zsequ  = l_sequ
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      p_mode = '2'.
    ELSE.
      p_mode = '1'.
    ENDIF.

*    SELECT posnr
*           idnrk
*           suff
*           sequ
*           stlkn
*         FROM stpo
*         INTO TABLE it_stpo
*         WHERE stlty EQ 'M'
*         AND   stlnr EQ l_stlnr.
*    IF sy-subrc EQ 0.
*      SORT it_stpo BY posnr idnrk zsuff zsequ.
*      READ TABLE it_stpo WITH KEY posnr  = pa_bmnc-pref
*                                  idnrk  = pa_bmnc-comp
*                                  zsuff  = pa_bmnc-suff
*                                  zsequ  = l_sequ
*                         BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        p_mode = '2'.
*      ELSE.
*        p_mode = '1'.
*      ENDIF.
*    ELSE.
*      p_mode = '1'.
*    ENDIF.
  ELSE.
*   BOM HEADER CREATED
    WRITE: pa_bmnc-bqty TO l_bqty UNIT pa_bmnc-hunt LEFT-JUSTIFIED.
    p_mode = '1'.
    PERFORM bom_header USING pa_bmnc-mtno   "Material
                             pa_bmnc-plnt   "Plant
                             pa_bmnc-usag   "BOM usage
                             pa_bmnc-altn   "Alternat BOM
                             l_bqty         "Confirmed qua
                             pa_bmnc-stat . "BOM Status
  ENDIF.
ENDFORM.                    " TALBE_MAST_STPO_CHECK
*&---------------------------------------------------------------------*
*&      Form  TABLE_NON_COL_CHANGE_BOM
*&---------------------------------------------------------------------*
FORM table_non_col_change_bom USING    pa_bmnc STRUCTURE wa_bmnc.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' pa_bmnc-mtno,     "NEXT MATERIAL
     ' ' 'RC29N-WERKS' pa_bmnc-plnt,    "PLANT
     ' ' 'RC29N-STLAN' pa_bmnc-usag,    "BOM usage
     ' ' 'RC29N-STLAL' pa_bmnc-altn,    "ALT BOM
     ' ' 'RC29N-AENNR' pa_bmnc-eono,    "Change number
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCNP'.
ENDFORM.                    " TABLE_NON_COL_CHANGE_BOM
*&---------------------------------------------------------------------*
*&      Form  TABLE_CHECK_IT_STPO
*&---------------------------------------------------------------------*
FORM table_check_it_stpo USING    pa_bmnc STRUCTURE wa_bmnc
                         CHANGING p_chk.
  READ TABLE it_stpo WITH KEY posnr  = pa_bmnc-pref
                              idnrk  = pa_bmnc-comp
                              zsuff  = pa_bmnc-suff
                              zsequ  = pa_bmnc-sequ
                     BINARY SEARCH.
  IF sy-subrc EQ 0.
    p_chk = ' '.
  ELSE.
    p_chk = 'X'.
  ENDIF.
ENDFORM.                    " TABLE_CHECK_IT_STPO
*&---------------------------------------------------------------------*
*&      Form  TABLE_NON_COL_BADY
*&---------------------------------------------------------------------*
FORM table_non_col_bady USING    pa_bmnc STRUCTURE wa_bmnc.
  DATA: l_dbcnt TYPE sy-dbcnt,
        l_qnty(20).
  WRITE: pa_bmnc-qnty TO l_qnty UNIT pa_bmnc-unit LEFT-JUSTIFIED.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'RC29P-AUSKZ(02)' 'X'   ,    "CHECK
     ' ' 'RC29P-POSNR(02)' pa_bmnc-pref,    "BOM item number
     ' ' 'RC29P-IDNRK(02)' pa_bmnc-comp,    "BOM compenent
     ' ' 'RC29P-MENGE(02)' l_qnty,          "Compenent quantity
     ' ' 'RC29P-POSTP(02)' pa_bmnc-itca,    "Item category
*     ' ' 'RC29P-SORTF(02)' PA_BMNC-USR01,   "Sortstring
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0130',
     ' ' 'RC29P-ITSOB' pa_bmnc-sppr,    "procurement type
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0131',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0138',
     ' ' 'ZEITM'       pa_bmnc-eitm,    "END ITEM TYPE
     ' ' 'ZSTGB'       pa_bmnc-stgb,    "STRUCTURE TYPE
     ' ' 'ZSUFF'       pa_bmnc-suff,    "SUFFIX NO
     ' ' 'ZSEQU'       pa_bmnc-sequ,    "SEQUENCE NO
     ' ' 'ZUPGN'       pa_bmnc-upgn,    "UPG
*     ' ' 'ZINFO'       PA_BMNC-SEQC,    "COLOR SEQUENCE
     ' ' 'BDC_OKCODE'  '/00',
     'X' 'SAPLCSDI'    '0140',
     ' ' 'BDC_OKCODE'  '=FCNP'.

ENDFORM.                    " TABLE_NON_COL_BADY
*&---------------------------------------------------------------------*
*&      Form  TABLE_COLOR_PART_HEADER
*&---------------------------------------------------------------------*
FORM table_color_part_header USING    pa_bmco STRUCTURE wa_bmco
                                      p_mode
                                      p_stlkn.
  DATA: l_stlnr LIKE mast-stlnr,
        l_bqty(20),
        l_sequ(04) TYPE n.
*
**/// START : Issue:20041115-003 Changed on 2004.11.24, Changed by BSBAE
** l_sequ = pa_bmco-sequ.
*  l_sequ = 1.
**/// END   : Issue:20041115-003 Changed on 2004.11.24, Changed by BSBAE
*
*  SELECT SINGLE stlnr
*       FROM mast
*       INTO l_stlnr
*       WHERE matnr EQ pa_bmco-mtno
*       AND   werks EQ pa_bmco-plnt
*       AND   stlan EQ pa_bmco-usag
*       AND   stlal EQ pa_bmco-altn.
*  IF sy-subrc EQ 0.
*    p_mode = '1'.
*    SELECT posnr
*           idnrk
*           suff
*           sequ
*           stlkn
**           ZINFO
*         FROM stpo
*         INTO TABLE it_stpo
*         WHERE stlty EQ 'M'
*         AND   stlnr EQ l_stlnr.
*    IF sy-subrc EQ 0.
*      SORT it_stpo BY posnr idnrk zsuff zsequ.
*      READ TABLE it_stpo WITH KEY posnr  = pa_bmco-pref
*                                  idnrk  = pa_bmco-comp
*                                  zsuff  = pa_bmco-suff
*                                  zsequ  = l_sequ
**                                  ZINFO  = PA_BMCO-ZINFO
*                         BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        p_mode = '2'.
*        p_stlkn = it_stpo-stlkn.
*      ELSE.
*        p_mode = '1'.
*      ENDIF.
*    ELSE.
*      p_mode = '1'.
*    ENDIF.
*  ELSE.
**   BOM HEADER CREATED
*    WRITE: pa_bmco-bqty TO l_bqty UNIT pa_bmco-hunt LEFT-JUSTIFIED.
*    p_mode = '1'.
*    PERFORM bom_header USING pa_bmco-mtno  "Material
*                             pa_bmco-plnt  "Plant
*                             pa_bmco-usag  "BOM usage
*                             pa_bmco-altn  "Alternat BOM
*                             l_bqty        "Confirmed qua
*                             pa_bmco-stat. "BOM Status
*  ENDIF.


  DATA: lw_topmat LIKE cstmat.

  DATA: lt_stb LIKE stpox OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            aumng                 = 0
            capid                 = c_capid
            cuovs                 = '0'
            datuv                 = sy-datum
            mktls                 = 'X'
            cuobj                 = '999999999999'
            mtnrv                 = pa_bmco-mtno
            stpst                 = 0
            stlan                 = pa_bmco-usag
            stlal                 = pa_bmco-altn
            svwvo                 = 'X'
            werks                 = pa_bmco-plnt
            vrsvo                 = 'X'
       IMPORTING
            topmat                = lw_topmat
       TABLES
            stb                   = lt_stb
       EXCEPTIONS
            alt_not_found         = 1
            call_invalid          = 2
            material_not_found    = 3
            missing_authorization = 4
            no_bom_found          = 5
            no_plant_data         = 6
            no_suitable_bom_found = 7
            conversion_error      = 8
            OTHERS                = 9.
  IF sy-subrc NE 0 OR lw_topmat-stlal NE pa_bmco-altn.
    WRITE: pa_bmco-bqty TO l_bqty UNIT pa_bmco-hunt LEFT-JUSTIFIED.
    PERFORM bom_header USING pa_bmco-mtno  "Material
                             pa_bmco-plnt  "Plant
                             pa_bmco-usag  "BOM usage
                             pa_bmco-altn  "Alternat BOM
                             l_bqty        "Confirmed qua
                             pa_bmco-stat. "BOM Status
    EXIT.
  ENDIF.

  LOOP AT lt_stb WHERE idnrk EQ it_bmco-comp
                   AND suff  EQ it_bmco-suff.
    pa_bmco-zmode   = 'C'.
    pa_bmco-zresult = 'E'.
    pa_bmco-zmsg    = 'BOM ITEM DATA already.'.
    EXIT.
  ENDLOOP.
ENDFORM.                    " TABLE_COLOR_PART_HEADER
*&---------------------------------------------------------------------*
*&      Form  TABLE_CHANGE_BOM
*&---------------------------------------------------------------------*
FORM table_change_bom USING    pa_bmco STRUCTURE wa_bmco.
  REFRESH: it_bdc, it_mess.
  CLEAR: it_bdc, it_mess.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' pa_bmco-mtno,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' pa_bmco-plnt,    "PLANT
     ' ' 'RC29N-STLAN' pa_bmco-usag,    "BOM usage
     ' ' 'RC29N-STLAL' pa_bmco-altn,    "ALT BOM
     ' ' 'RC29N-AENNR' pa_bmco-eono,  "Change number
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCNP'.
ENDFORM.                    " TABLE_CHANGE_BOM
*&---------------------------------------------------------------------*
*&      Form  TABLE_CHANGE_BADY
*&---------------------------------------------------------------------*
FORM table_change_bady USING    pa_bmco STRUCTURE wa_bmco.
  DATA l_qnty(20).
  WRITE: pa_bmco-qnty TO l_qnty UNIT pa_bmco-unit LEFT-JUSTIFIED.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'RC29P-AUSKZ(02)' 'X'   ,    "CHECK
     ' ' 'RC29P-POSNR(02)' pa_bmco-pref,    "BOM item number
     ' ' 'RC29P-IDNRK(02)' pa_bmco-comp,    "BOM compenent
     ' ' 'RC29P-MENGE(02)' l_qnty,          "Compenent quantity
     ' ' 'RC29P-POSTP(02)' pa_bmco-itca,    "Item category
*     ' ' 'RC29P-SORTF(02)' PA_BMCO-USR01,   "Sortstring
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0130',
     ' ' 'RC29P-ITSOB' pa_bmco-sppr,    "procurement type
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0131',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0138',
     ' ' 'ZEITM'       pa_bmco-eitm,    "END ITEM TYPE
     ' ' 'ZSTGB'       pa_bmco-stgb,    "STRUCTURE TYPE
     ' ' 'ZSUFF'       pa_bmco-suff,    "SUFFIX NO
     ' ' 'ZSEQU'       pa_bmco-sequ,    "SEQUENCE NO
     ' ' 'ZUPGN'       pa_bmco-upgn,    "UPG
     ' ' 'ZINFO'       pa_bmco-seqc,    "COLOR SEQUENCE
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0140',
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=SETP',

     'X' 'SAPLCSDI'        '0708',
     ' ' 'RC29P-SELPO'     pa_bmco-pref,    "
     ' ' 'RC29P-SELID'     pa_bmco-comp,
     ' ' 'RC29P-SELPI'     ' ',
     ' ' 'BDC_OKCODE'      '=CLWI',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',    "
     ' ' 'BDC_OKCODE'      '=WIZU',

     'X' 'SAPLCUKD'        '0130',
     ' ' 'RCUKD-KNNAM(02)' 'Z_EMPTY_OD',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCUKD'        '0130',
     ' ' 'BDC_OKCODE'      '=BACK',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=FCBU'.
ENDFORM.                    " TABLE_CHANGE_BADY
*&---------------------------------------------------------------------*
*&      Form  TABLE_COL_CHECK_IT_STPO
*&---------------------------------------------------------------------*
FORM table_col_check_it_stpo USING    pa_bmco STRUCTURE wa_bmco
                             CHANGING p_chk.
  DATA l_sequ(04) TYPE n.
  l_sequ = pa_bmco-sequ.
  READ TABLE it_stpo WITH KEY posnr  = pa_bmco-pref
                              idnrk  = pa_bmco-comp
                              zsuff  = pa_bmco-suff
*                              ZSEQU  = PA_BMCO-SEQU
                              zsequ  = l_sequ
                     BINARY SEARCH.
  IF sy-subrc EQ 0.
    p_chk = ' '.
  ELSE.
    p_chk = 'X'.
  ENDIF.
ENDFORM.                    " TABLE_COL_CHECK_IT_STPO
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_TRANSACTION
*&---------------------------------------------------------------------*
FORM color_part_transaction USING    p_tanew
                                     p_taend
                            CHANGING p_zresult
                                     p_zmsg
                                     p_mode.
  DATA l_tabix TYPE sy-tabix.
*  IF p_mode EQ '1' OR  p_mode EQ '2'.
  PERFORM change_end_item_create.
  IF sy-subrc NE 0 OR sy-msgno NE '031'.
*    it_bmco-zresult = sy-msgty.
    PERFORM rkc_msg_string CHANGING it_bmco-zmsg.
*   MODIFY IT_BMCO
    it_bmco-zresult = 'E'.
    it_bmco-zmode   = 'C'.
    MODIFY it_bmco TRANSPORTING zmode zresult zmsg
     WHERE mtno = wa_bmco-mtno
       AND plnt = wa_bmco-plnt
       AND usag = wa_bmco-usag
       AND altn = wa_bmco-altn
       AND pref = wa_bmco-pref
       AND comp = wa_bmco-comp
       AND suff = wa_bmco-suff
       AND sequ = wa_bmco-sequ.
*  LOOP AT it_bmco FROM p_tanew TO p_taend.
*    l_tabix = sy-tabix.
*    IF p_zresult IS INITIAL.
*      it_bmco-zresult = 'E'.
*      it_bmco-zmode   = 'C'.
*      it_bmco-zmsg    = p_zmsg.
*    ELSE.
*      it_bmco-zresult = p_zresult.
*      it_bmco-zmode   = 'C'.
*      it_bmco-zmsg    = p_zmsg.
*    ENDIF.
*    CLEAR it_bmco.
*  ENDLOOP.
  ENDIF.
ENDFORM.                    " COLOR_PART_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_TRANSACTION1
*&---------------------------------------------------------------------*
FORM color_part_transaction1 USING    p_tanew
                                      p_taend
                             CHANGING p_zresult
                                      p_zmsg
                                      p_mode.
  DATA l_tabix TYPE sy-tabix.
  IF p_mode EQ '1' OR  p_mode EQ '2'.
    PERFORM change_end_item_create1.
    p_zresult = sy-msgty.
    PERFORM rkc_msg_string CHANGING p_zmsg.
*   MODIFY IT_BMCO
    LOOP AT it_bmco FROM p_tanew TO p_taend.
      l_tabix = sy-tabix.
      IF p_zresult EQ ' '     OR
         p_zresult EQ 'E'     OR
         p_zresult EQ 'A'     OR
         p_zresult EQ 'X'.
        it_bmco-zresult = 'E'.
        it_bmco-zmode   = 'C'.
        it_bmco-zmsg    = p_zmsg.
      ELSE.
        it_bmco-zresult = p_zresult.
        it_bmco-zmode   = 'C'.
        it_bmco-zmsg    = p_zmsg.
      ENDIF.
      MODIFY it_bmco INDEX l_tabix TRANSPORTING zmode
                                                zresult
                                                zmsg.
      CLEAR it_bmco.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " COLOR_PART_TRANSACTION1
*&---------------------------------------------------------------------*
*&      Form  TABLE_COLOR_PART_DELETE
*&---------------------------------------------------------------------*
FORM table_color_part_delete USING    pa_bmco STRUCTURE wa_bmco
                                      p_tabix
                             CHANGING p_mode
                                      p_stlkn
                                      p_zresult
                                      p_zmsg.
  PERFORM read_mast_stpo_stlkn USING pa_bmco-mtno  "Material
                                     pa_bmco-plnt  "Plant
                                     pa_bmco-usag  "BOM usage
                                     pa_bmco-altn  "Alternative
                                     pa_bmco-pref  "BOM item
                                     pa_bmco-comp  "BOM
                                     pa_bmco-suff  "SUFFIX
                                     pa_bmco-sequ
                            CHANGING p_mode
                                     p_stlkn.
  IF p_mode EQ '3'. "DELETE(ITEM DELETE)
    IF NOT p_stlkn IS INITIAL.
      PERFORM bom_item_delete USING pa_bmco-mtno  "Material number
                                    pa_bmco-plnt  "Plant
                                    pa_bmco-usag  "BOM usage
                                    pa_bmco-altn  "Alternative BOM
                                    pa_bmco-eono  "Change number
                                    p_stlkn.      "Item node number

      PERFORM rkc_msg_string CHANGING p_zmsg.
      p_zresult = sy-msgty.
*     MODIFY IT_COLO
      it_bmco-zmode   = 'D'.
      it_bmco-zbdat   = sy-datum.
      it_bmco-zbtim   = sy-uzeit.
      it_bmco-zbnam   = sy-uname.
      it_bmco-zresult = p_zresult.
      it_bmco-zmsg    = p_zmsg.
      MODIFY it_bmco INDEX p_tabix TRANSPORTING zmode
                                                zresult
                                                zmsg
                                                zbdat
                                                zbtim
                                                zbnam.
    ELSE.
*     MODIFY IT_BMCO
      it_bmco-zmode   = 'D'.
      it_bmco-zbdat   = sy-datum.
      it_bmco-zbnam   = sy-uname.
      it_bmco-zresult = 'E'.
      it_bmco-zmsg    = 'BOM ITEM to delete does not exist.'.
      MODIFY it_bmco INDEX p_tabix TRANSPORTING zmode
                                                zresult
                                                zmsg
                                                zbdat
                                                zbnam.
    ENDIF.
  ELSEIF p_mode EQ '4'.
*   MODIFY IT_BMCO
    it_bmco-zmode   = 'D'.
    it_bmco-zbdat   = sy-datum.
    it_bmco-zbnam   = sy-uname.
    it_bmco-zresult = 'E'.
    it_bmco-zmsg    = 'BOM ITEM to delete does not exist.'.
    MODIFY it_bmco INDEX p_tabix TRANSPORTING zmode
                                              zresult
                                              zmsg
                                              zbdat
                                              zbnam.
  ENDIF.

  CLEAR it_bmco.

  REFRESH: it_bdc, it_mess.
  CLEAR: it_bdc, it_mess.
ENDFORM.                    " TABLE_COLOR_PART_DELETE
*&---------------------------------------------------------------------*
*&      Form  TABLE_BOM_EXPLODED
*&---------------------------------------------------------------------*
FORM table_bom_exploded TABLES p_bom_exploded STRUCTURE it_bom_exploded.
  DATA: BEGIN OF lt_marc OCCURS 0,
          matnr LIKE marc-matnr,  "MATERIAL
          werks LIKE marc-werks,  "PLANT
          datuv LIKE rc29p-datuv,
          datub LIKE rc29p-datub,
          mtart LIKE mara-mtart,  "MATERIAL TYPE
        END   OF lt_marc.
  DATA l_tabix TYPE sy-tabix.
* MATERIAL & COMPENENT COLLECT

  SORT it_bmco BY mtno comp upct.
  LOOP AT it_bmco WHERE upct NE '2'
                  AND   zresult NE 'E'.
*---/// START : Issue: PP-20041006-001
*---/// Changed on 2004.10.06, Changed by BSBAE, Request by IYCHOI
    ON CHANGE OF it_bmco-mtno.
      CLEAR: lt_marc.
      lt_marc-matnr = it_bmco-mtno.
      lt_marc-werks = it_bmco-plnt.

      lt_marc-datub = '99991231'.
      lt_marc-datuv = sy-datum.
      COLLECT lt_marc.
    ENDON.
*---/// END : Issue: PP-20041006-001

    CLEAR: lt_marc.
    lt_marc-matnr = it_bmco-comp.
    lt_marc-werks = it_bmco-plnt.

*---/// START : Issue: PP-20041006-001
*---/// Changed on 2004.10.06, Changed by BSBAE, Request by IYCHOI
    lt_marc-datub = '99991231'.

    SELECT SINGLE datuv INTO lt_marc-datuv
      FROM aenr
     WHERE aennr = it_bmco-eono.
    IF sy-subrc NE 0.
      MOVE: '19000101' TO lt_marc-datuv.
    ENDIF.
*---/// END : Issue: PP-20041006-001

    COLLECT lt_marc.
    CLEAR: lt_marc, it_bmco.
  ENDLOOP.
  IF NOT lt_marc[] IS INITIAL.
*   SELECTION MARA--->MODIFY LT_MARC-MTART(MATERIAL TYPE)
*                            LT_MARC-KZKFG(Configurable Material)
    PERFORM read_mara TABLES lt_marc.

    LOOP AT lt_marc WHERE mtart EQ 'FERT'.
      l_tabix = sy-tabix.
      p_bom_exploded-matnr = lt_marc-matnr.
      p_bom_exploded-werks = lt_marc-werks.
      APPEND p_bom_exploded.
      DELETE lt_marc INDEX l_tabix.
      CLEAR: p_bom_exploded, lt_marc.
    ENDLOOP.

    LOOP AT lt_marc.
      SORT p_bom_exploded.
      READ TABLE p_bom_exploded WITH KEY matnr = lt_marc-matnr
                                         werks = lt_marc-werks
                                BINARY SEARCH.
      IF sy-subrc NE 0.
*       BOM EXPLODED.
*---/// START : Issue: PP-20041006-001
*---/// Changed on 2004.10.06, Changed by BSBAE, Request by IYCHOI
*        perform read_bom tables p_bom_exploded
*                        using lt_marc-matnr
*                              lt_marc-werks
*                              sy-datum   "IDNRK WERKS DATUV
*                        changing wa_last.

        PERFORM read_bom TABLES p_bom_exploded
                        USING lt_marc-matnr
                              lt_marc-werks
                              lt_marc-datuv   "IDNRK WERKS DATUV
                              lt_marc-datub   "IDNRK WERKS DATUB
                        CHANGING wa_last.
*---/// END : Issue: PP-20041006-001
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " TABLE_BOM_EXPLODED
*&---------------------------------------------------------------------*
*&      Form  TABLE_OBJECT_DEPENDENCY
*&---------------------------------------------------------------------*
FORM table_object_dependency.
  DATA: l_stlkn TYPE stpo-stlkn,
        l_tabix TYPE sy-tabix,
        l_tanew TYPE sy-tabix,
        l_taend TYPE sy-tabix,
        l_zresult LIKE it_excl-zresult,
        l_zmsg LIKE it_excl-zmsg,
        l_mode.
  CLEAR: wa_line_idx, wa_erro_idx, wa_bmco.
  REFRESH: it_bdc, it_mess.
  CLEAR: it_bdc, it_mess.
* MATERIAL & OBJECT DEPENDENCY APPENDING
  LOOP AT it_bmco WHERE upct EQ '2'
                  OR   zresult EQ 'E'.
    l_tabix = sy-tabix.
    MOVE-CORRESPONDING it_bmco TO it_bmdt.
    APPEND it_bmdt.
    DELETE it_bmco INDEX l_tabix.
    CLEAR: it_bmdt, it_bmco.
  ENDLOOP.
  CLEAR: l_tabix.
  LOOP AT it_bmco.
    l_tabix = sy-tabix.
    wa_line_idx = wa_line_idx + 1.

    wa_bmco = it_bmco.
*     BOM ITEM ADDITION & OBJECT DEPENDENCY CREATE
*   AT NEW SEQU
    AT NEW sequ.
      l_mode = '1'.
      l_tanew = l_tabix.

      PERFORM mast_stpo_stlkn_1 USING wa_bmco
                                CHANGING l_stlkn.
      PERFORM table_append_dependency USING     wa_bmco
                                                l_stlkn.

    ENDAT.
*   'UPDATE CONTROL MODE' different BDC
    PERFORM bady_dependency USING wa_bmco-dpid.
*   AT END OF ZSEQU
    AT END OF sequ.
      l_taend = l_tabix.
      PERFORM color_part_transaction1 USING    l_tanew
                                               l_taend
                                      CHANGING l_zresult
                                               l_zmsg
                                               l_mode.

      REFRESH: it_bdc, it_mess.
      CLEAR: it_bdc, it_mess.
    ENDAT.
    CLEAR:  it_bmco.
  ENDLOOP.
* ERROR LINE COUNT
  DESCRIBE TABLE it_bmco LINES wa_line_idx.
* ERROR LINE COUNT
  LOOP AT it_bmco.
    l_tabix = sy-tabix.
    MOVE-CORRESPONDING it_bmco TO it_bmdt.
    it_bmdt-zbdat = sy-datum.
    it_bmdt-zbnam   = sy-uname.

    IF it_bmco-zresult EQ'E'.
      wa_erro_idx = wa_erro_idx + 1.
*      DELETE IT_BMCO INDEX L_TABIX.
    ELSE.
      it_bmdt-zbtim = sy-uzeit.
    ENDIF.

    APPEND it_bmdt.
    CLEAR: it_bmco, it_bmdt.
  ENDLOOP.

*  REFRESH IT_BMCO. CLEAR IT_BMCO.
  WRITE: / text-036,
            wa_line_idx.
  FORMAT COLOR 6 INTENSIFIED OFF.
  WRITE: / text-037,
            wa_erro_idx.
  FORMAT COLOR OFF.

* Error appears if is more than one case.
  IF wa_erro_idx GE '1'.
    PERFORM error_data_write TABLES it_bmdt
                             USING  'E'.

  ENDIF.
  LOOP AT it_bmdt.
    MOVE-CORRESPONDING it_bmdt TO it_abxebmdt.
    APPEND it_abxebmdt.
    CLEAR: it_abxebmdt, it_bmdt.
  ENDLOOP.

ENDFORM.                    " TABLE_OBJECT_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  TABLE_APPEND_DEPENDENCY
*&---------------------------------------------------------------------*
FORM table_append_dependency USING    pa_bmco STRUCTURE wa_bmco
                                      p_stlkn.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' pa_bmco-mtno,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' pa_bmco-plnt,    "PLANT
     ' ' 'RC29N-STLAN' pa_bmco-usag,    "BOM usage
     ' ' 'RC29N-STLAL' pa_bmco-altn,    "ALT BOM
     ' ' 'RC29N-AENNR' pa_bmco-eono,    "Change number
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=SETP',

     'X' 'SAPLCSDI'    '0708',
     ' ' 'RC29P-SELPI' p_stlkn,    "
     ' ' 'BDC_OKCODE'  '=CLWI'.
ENDFORM.                    " TABLE_APPEND_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  BOM_EXPLODED_WRITE
*&---------------------------------------------------------------------*
FORM bom_exploded_write.
  IF NOT it_exp_mess[] IS INITIAL.
    CLEAR: wa_line_idx, wa_erro_idx.
    DESCRIBE TABLE it_exp_mess LINES wa_line_idx.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / text-080,  wa_line_idx.
    FORMAT COLOR OFF.
    WRITE: /(20) text-039,
            (10) text-040,
            (100) text-064.
    LOOP AT it_exp_mess.
      WRITE: /(20) it_exp_mess-matnr,
              (10) it_exp_mess-werks,
              (100) it_exp_mess-msg.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " BOM_EXPLODED_WRITE
*&---------------------------------------------------------------------*
*&      Form  JOB_PROCESS
*&---------------------------------------------------------------------*
FORM job_process.
* ENGINE
  PERFORM job_execution USING text-066
                              text-067.
* SUB MATERIAL
  PERFORM job_execution USING text-068
                              text-069.
ENDFORM.                    " JOB_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_OPEN
*&---------------------------------------------------------------------*
FORM call_job_open USING p_jobname p_jobcount.
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
*      DELANFREP              = ' '
*      JOBGROUP               = ' '
      jobname                = p_jobname
*      SDLSTRTDT              = NO_DATE
*      SDLSTRTTM              = NO_TIME
    IMPORTING
      jobcount               = p_jobcount
    EXCEPTIONS
      cant_create_job        = 1
      invalid_job_data       = 2
      jobname_missing        = 3
      OTHERS                 = 4.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
ENDFORM.                    " CALL_JOB_OPEN
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_SUBMIT
*&---------------------------------------------------------------------*
FORM call_job_submit USING p_jobname
                           p_report
                           p_jobcount.

  CALL FUNCTION 'JOB_SUBMIT'
    EXPORTING
*      ARCPARAMS                         =
      authcknam                         = sy-uname
*      COMMANDNAME                       = ' '
*      OPERATINGSYSTEM                   = ' '
*      EXTPGM_NAME                       = ' '
*      EXTPGM_PARAM                      = ' '
*      EXTPGM_SET_TRACE_ON               = ' '
*      EXTPGM_STDERR_IN_JOBLOG           = 'X'
*      EXTPGM_STDOUT_IN_JOBLOG           = 'X'
*      EXTPGM_SYSTEM                     = ' '
*      EXTPGM_RFCDEST                    = ' '
*      EXTPGM_WAIT_FOR_TERMINATION       = 'X'
      jobcount                          = p_jobcount
      jobname                           = p_jobname
*      LANGUAGE                          = SY-LANGU
*      PRIPARAMS                         = ' '
      report                            = p_report
*      VARIANT                           = ' '
*    IMPORTING
*      STEP_NUMBER                       =
      EXCEPTIONS
      bad_priparams                     = 1
      bad_xpgflags                      = 2
      invalid_jobdata                   = 3
      jobname_missing                   = 4
      job_notex                         = 5
      job_submit_failed                 = 6
      lock_failed                       = 7
      program_missing                   = 8
      prog_abap_and_extpg_set           = 9
      OTHERS                            = 10.

*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
ENDFORM.                    " CALL_JOB_SUBMIT
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_CLOSE
*&---------------------------------------------------------------------*
FORM call_job_close USING p_jobname p_jobcount.
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
*   AT_OPMODE                         = ' '
*   AT_OPMODE_PERIODIC                = ' '
*   CALENDAR_ID                       = ' '
*   EVENT_ID                          = ' '
*   EVENT_PARAM                       = ' '
*   EVENT_PERIODIC                    = ' '
      jobcount                          = p_jobcount
      jobname                           = p_jobname
*   LASTSTRTDT                        = NO_DATE
*   LASTSTRTTM                        = NO_TIME
*   PRDDAYS                           = 0
*   PRDHOURS                          = 0
*   PRDMINS                           = 0
*   PRDMONTHS                         = 0
*   PRDWEEKS                          = 0
*   PREDJOB_CHECKSTAT                 = ' '
*   PRED_JOBCOUNT                     = ' '
*   PRED_JOBNAME                      = ' '
*   SDLSTRTDT                         = NO_DATE
*   SDLSTRTTM                         = NO_TIME
*   STARTDATE_RESTRICTION             = BTC_PROCESS_ALWAYS
     strtimmed                         = 'X'  "IMMEDIATE
*   TARGETSYSTEM                      = ' '
*   START_ON_WORKDAY_NOT_BEFORE       = SY-DATUM
*   START_ON_WORKDAY_NR               = 0
*   WORKDAY_COUNT_DIRECTION           = 0
*   RECIPIENT_OBJ                     =
*   TARGETSERVER                      = ' '
*   DONT_RELEASE                      = ' '
* IMPORTING
*   JOB_WAS_RELEASED                  =
   EXCEPTIONS
     cant_start_immediate              = 1
     invalid_startdate                 = 2
     jobname_missing                   = 3
     job_close_failed                  = 4
     job_nosteps                       = 5
     job_notex                         = 6
     lock_failed                       = 7
     OTHERS                            = 8.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " CALL_JOB_CLOSE
*&---------------------------------------------------------------------*
*&      Form  JOB_EXECUTION
*&---------------------------------------------------------------------*
FORM job_execution USING    p_jobname p_ind.
  REFRESH r_datum. CLEAR r_datum.

  r_datum-low = p_zedat.
  r_datum-sign = 'I'.
  r_datum-option = 'EQ'.
  APPEND r_datum.

  wa_report = wa_jobname = p_jobname.
  CONCATENATE wa_jobname p_ind INTO wa_jobname.
*-----> JOB OPEN
  PERFORM call_job_open USING wa_jobname wa_jobcount.

*-----> JOB SUBMIT
  SUBMIT (wa_report) WITH p_zedat   EQ p_zedat
                     WITH p_zbtim   EQ p_zbtim
                     WITH p_submit  EQ p_submit
                     WITH it_subm   EQ it_subm
                    USER sy-uname
                    VIA JOB wa_jobname
                    NUMBER wa_jobcount
                    AND RETURN.

*-----> JOB CLOSE
  PERFORM call_job_close USING wa_jobname
                               wa_jobcount.

ENDFORM.                    " JOB_EXECUTION
*&---------------------------------------------------------------------*
*&      Form  READ_PLP0
*&---------------------------------------------------------------------*
FORM read_plp0 USING    p_vspvb
                        p_plnt
               CHANGING p_usr01.
  SELECT SINGLE usr01
              FROM plpo
              INTO p_usr01
              WHERE plnty EQ 'M'
              AND   plnnr EQ 'RP'
              AND   werks EQ p_plnt
              AND   usr00 EQ p_vspvb.
ENDFORM.                                                    " READ_PLP0
*&---------------------------------------------------------------------*
*&      Form  MATERIAL_MASTER_UPDATE
*&---------------------------------------------------------------------*
FORM material_master_update.
* READ DATA
  PERFORM read_ztbm_abxebmdt.
* DATA CHECK
  PERFORM mara_data_check.
* MATERIAL CHANGE BDC
  PERFORM mm02_material_change.
ENDFORM.                    " MATERIAL_MASTER_UPDATE
*&---------------------------------------------------------------------*
*&      Form  READ_ZTBM_ABXEBMDT
*&---------------------------------------------------------------------*
FORM read_ztbm_abxebmdt.
  SELECT comp plnt stgb sppr
       FROM ztbm_abxebmdt
       INTO TABLE it_mm02
       WHERE zedat EQ p_zedat
       AND   upct  EQ '1'
       GROUP by comp plnt stgb sppr.
  IF sy-subrc EQ 0.
    DELETE ADJACENT DUPLICATES FROM it_mm02 COMPARING ALL FIELDS.
  ENDIF.

ENDFORM.                    " READ_ZTBM_ABXEBMDT
*&---------------------------------------------------------------------*
*&      Form  MARA_DATA_CHECK
*&---------------------------------------------------------------------*
FORM mara_data_check.
  DATA: l_mtart LIKE mara-mtart,
        l_tabix LIKE sy-tabix.
  LOOP AT it_mm02.
    l_tabix = sy-tabix.
    it_mm02-normt = it_mm02-stgb.

    SELECT SINGLE mtart
         FROM mara
         INTO l_mtart
         WHERE matnr EQ it_mm02-comp.
    IF sy-subrc EQ 0.
      CASE l_mtart.
        WHEN 'HALB'.
          IF it_mm02-sppr EQ '50'.
            it_mm02-sobsl = it_mm02-sppr.
            it_mm02-ncost = 'X'.
          ENDIF.
        WHEN 'ROH'.
          CASE it_mm02-stgb.
            WHEN 'M'.
              it_mm02-dispo = 'M01'.
              it_mm02-beskz = 'X'.
            WHEN 'D'.
              it_mm02-dispo = 'M02'.

          ENDCASE.
      ENDCASE.
    ENDIF.
    MODIFY it_mm02 INDEX l_tabix.
    CLEAR it_mm02.
    CLEAR l_mtart.
  ENDLOOP.
ENDFORM.                    " MARA_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  MM02_MATERIAL_CHANGE
*&---------------------------------------------------------------------*
FORM mm02_material_change.

  LOOP AT it_mm02.
    PERFORM dynpro USING:
       'X' 'SAPLMGMM'             '0060',
       ' ' 'RMMG1-MATNR'          it_mm02-comp,
       ' ' 'BDC_OKCODE'           '=AUSW',

       'X' 'SAPLMGMM'             '0070',
       ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
       ' ' 'BDC_OKCODE'           '=ENTR'.

*   BASIC 2
    PERFORM dynpro USING:
                   'X' 'SAPLMGMM'    '5004'.
    IF NOT it_mm02-normt IS INITIAL.
      PERFORM dynpro USING:
                     ' ' 'MARA-NORMT'  it_mm02-normt.   "
    ENDIF.

    IF NOT it_mm02-dispo IS INITIAL.
*     MRP1 CHECK
      PERFORM dynpro USING:
                     ' ' 'BDC_OKCODE'  '=SP12'.
      PERFORM dynpro USING:
         'X' 'SAPLMGMM'             '0081',
         ' ' 'RMMG1-WERKS'          it_mm02-plnt,   "
         ' ' 'BDC_OKCODE'           '=ENTR'.
*     MRP 1
      PERFORM dynpro USING:
                     'X' 'SAPLMGMM'    '5000',
                     ' ' 'MARC-DISPO'  it_mm02-dispo.   "
      IF NOT it_mm02-sobsl IS INITIAL.
        PERFORM dynpro USING:
                            ' ' 'BDC_OKCODE'  '=SP13'.
*       MRP 2
        PERFORM dynpro USING:
                      'X' 'SAPLMGMM'    '5000',
                      ' ' 'MARC-SOBSL'  it_mm02-sobsl.   "
        IF NOT it_mm02-beskz IS INITIAL.
          PERFORM dynpro USING:
                      ' ' 'MARC-BESKZ'  it_mm02-beskz. "Procurement type
        ENDIF.
        PERFORM dynpro USING:
                       ' ' 'BDC_OKCODE'  '=SP26'.
*       COSTING 2
        PERFORM dynpro USING:
                       'X' 'SAPLMGMM'    '5000',
                       ' ' 'MARC-NCOST'  it_mm02-ncost.   "
        PERFORM dynpro USING:
                       ' ' 'BDC_OKCODE'  '=BU'.
      ELSE.
*       MRP 2
        IF NOT it_mm02-beskz IS INITIAL.
          PERFORM dynpro USING:
                              ' ' 'BDC_OKCODE'  '=SP13'.
          PERFORM dynpro USING:
                              'X' 'SAPLMGMM'    '5000'.
          PERFORM dynpro USING:
                    ' ' 'MARC-BESKZ'  it_mm02-beskz. "Procurement type
        ENDIF.
        PERFORM dynpro USING:
                            ' ' 'BDC_OKCODE'  '=BU'.
      ENDIF.
    ELSEIF NOT it_mm02-sobsl IS INITIAL.
*     MRP2 & COSTING1 CHECK
      PERFORM dynpro USING:
                     ' ' 'BDC_OKCODE'  '=SP13'.
      PERFORM dynpro USING:
         'X' 'SAPLMGMM'             '0081',
         ' ' 'RMMG1-WERKS'          it_mm02-plnt,   "
         ' ' 'BDC_OKCODE'           '=ENTR'.
*     MRP 2
      PERFORM dynpro USING:
                     'X' 'SAPLMGMM'    '5000',
                     ' ' 'MARC-SOBSL'  it_mm02-sobsl.   "
      IF NOT it_mm02-beskz IS INITIAL.
        PERFORM dynpro USING:
                    ' ' 'MARC-BESKZ'  it_mm02-beskz. "Procurement type
      ENDIF.
      PERFORM dynpro USING:
                     ' ' 'BDC_OKCODE'  '=SP26'.
*     COSTING 2
      PERFORM dynpro USING:
                     'X' 'SAPLMGMM'    '5000',
                     ' ' 'MARC-NCOST'  it_mm02-ncost.   "
      PERFORM dynpro USING:
                     ' ' 'BDC_OKCODE'  '=BU'.

    ELSE.
      PERFORM dynpro USING:
                          ' ' 'BDC_OKCODE'  '=BU'.
    ENDIF.

    CALL TRANSACTION 'MM02'  USING         it_bdc
                             OPTIONS  FROM wa_opt
                             MESSAGES INTO it_mess.

    MOVE-CORRESPONDING it_mm02 TO it_mass.

    PERFORM rkc_msg_string CHANGING it_mass-msg.

    APPEND it_mass.

    CLEAR: it_mm02, it_mass.
    REFRESH: it_bdc, it_mess.
    CLEAR  : it_bdc, it_mess.
  ENDLOOP.
ENDFORM.                    " MM02_MATERIAL_CHANGE
*&---------------------------------------------------------------------*
*&      Form  MM02_MATERIAL_CHANGE_WRITE
*&---------------------------------------------------------------------*
FORM mm02_material_change_write.
  IF NOT it_mass[] IS INITIAL.
    CLEAR: wa_line_idx, wa_erro_idx.
    DESCRIBE TABLE it_mass LINES wa_line_idx.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / text-065,  wa_line_idx.
    FORMAT COLOR OFF.
    WRITE: /(20) text-070,
            (10) text-071,
            (15) text-072,
            (20) text-073,
            (30) text-075,
            (26) text-076,
            (14) text-077,
            (18) text-078,
            (100) text-064.
    LOOP AT it_mass.
      WRITE: /(20) it_mass-comp,
              (10) it_mass-plnt,
              (15) it_mass-stgb,
              (20) it_mass-sppr,
              (30) it_mass-normt,
              (26) it_mass-sobsl,
              (14) it_mass-ncost,
              (18) it_mass-dispo,
             (100) it_mass-msg.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " MM02_MATERIAL_CHANGE_WRITE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_JOB_PROCESS
*&---------------------------------------------------------------------*
FORM update_job_process.
  DATA: l_tabix TYPE sy-tabix,
        l_uzeit TYPE sy-uzeit.
  l_uzeit = sy-uzeit.
  LOOP AT it_abxebmdt WHERE zresult NE 'E'.
    l_tabix = sy-tabix.
    IF it_abxebmdt-stgb EQ 'F'.
      p_submit = 'X'.
*     SUBMIT DATA CREATE
      it_subm-matnr = it_abxebmdt-mtno.
      it_subm-werks = it_abxebmdt-plnt.
      it_subm-stlan = it_abxebmdt-usag.
      it_subm-stlal = it_abxebmdt-altn.
      COLLECT it_subm.
      CLEAR: it_subm.
    ENDIF.
    it_abxebmdt-zbtim = l_uzeit.
    MODIFY it_abxebmdt INDEX l_tabix TRANSPORTING zbtim.
    CLEAR it_abxebmdt.
  ENDLOOP.
  PERFORM update_ztbm_abxebmdt TABLES   it_abxebmdt.
  REFRESH it_bmdt. CLEAR it_bmdt.
  REFRESH it_abxebmdt. CLEAR it_abxebmdt.
**** JOB PROCESS
***  IF P_SUBMIT EQ 'X'.
***    EXPORT IT_SUBM TO MEMORY ID 'BOM_SUB_DATA'.
****    SUBMIT ZEPP315U_ENG_BOM_FSC_ASSEM WITH P_ZEDAT EQ P_ZEDAT
****                                      WITH P_ZBTIM EQ P_ZBTIM
****                                      WITH P_SUBMIT EQ P_SUBMIT
****                      AND RETURN.
***    PERFORM ENGIN_BOM_PROCESS.
***
***
****    SUBMIT ZEPP308U_SUB_BOM_FSC_ASSIG WITH P_ZEDAT EQ P_ZEDAT
****                                      WITH P_ZBTIM EQ P_ZBTIM
****                                      WITH P_SUBMIT EQ P_SUBMIT
****                      AND RETURN.
***    PERFORM SUB_MATERIAL_BOM_PERCESS.
****    PERFORM JOB_PROCESS.
***  ENDIF.


ENDFORM.                    " UPDATE_JOB_PROCESS
*&---------------------------------------------------------------------*
*&      Form  ENGIN_BOM_PROCESS
*&---------------------------------------------------------------------*
FORM engin_bom_process USING p_value p_txt.
*  DATA: BEGIN OF LT_BMDT OCCURS 0,
*          MATNR TYPE ZTBM_ABXEBMDT-MTNO,
*          WERKS TYPE ZTBM_ABXEBMDT-PLNT,
*          STLAN TYPE ZTBM_ABXEBMDT-USAG,
*          STLAL TYPE ZTBM_ABXEBMDT-ALTN,
*          ZMODE,
*          ZMSG  LIKE CFGNL-MSGLIN,
*        END OF LT_BMDT.
  DATA: la_bmdt LIKE it_engb,
        l_idnrk LIKE mara-matnr,
        m_idnrk LIKE mara-matnr,
        l_zinfo LIKE stpo-zinfo,
        l_selid LIKE rc29p-selid,       "Component
        l_selpi LIKE rc29p-selpi,       "Item ID
        l_chk.
  DATA: l_tabix TYPE sy-tabix,
        l_worko(40),
        l_stlal(03).
  DATA: wa_line_tot TYPE i,
        wa_line_ero TYPE i.
  LOOP AT it_subm.
    SELECT SINGLE *
                FROM mara
                WHERE matnr EQ it_subm-matnr
                AND   mtart EQ 'FERT'.
    IF sy-subrc EQ 0.
      it_engb = it_subm.
      APPEND it_engb.
    ENDIF.
    CLEAR: it_subm, it_engb.
  ENDLOOP.

  LOOP AT it_engb.
    l_tabix = sy-tabix.
    wa_engb = it_engb.
    AT NEW stlal.
      CLEAR l_idnrk.
      PERFORM search_engine_material USING    wa_engb-matnr
                                              wa_engb-stlal
                                              p_value
                                     CHANGING l_idnrk.
    ENDAT.
    IF NOT l_idnrk IS INITIAL.
      CLEAR: l_chk.
      PERFORM check_component USING    wa_engb-matnr
                                       wa_engb-werks
                                       wa_engb-stlan
                                       wa_engb-stlal
                                       l_idnrk
                                       p_txt
                              CHANGING l_chk
                                       m_idnrk
                                       l_zinfo
                                       l_selpi.
*                    ENGINE
*     L_CHK EQ A ==> HC90  EQ HC90
*     L_CHK EQ B ==> HC89  NE HC90
*     L_CHK EQ C ==> SPACE
      CASE l_chk.
        WHEN 'A'.
*
          IF l_zinfo IS INITIAL.
*
*            PERFORM ENGINE_BOM_TYPE_A.
            PERFORM dynpro USING:
               'X' 'SAPLCSDI'       '0100',
               ' ' 'RC29N-MATNR'     wa_engb-matnr,   "NEXT MATERIAL
               ' ' 'RC29N-WERKS'     wa_engb-werks,   "PLANT
               ' ' 'RC29N-STLAN'     wa_engb-stlan,   "BOM usage
               ' ' 'RC29N-STLAL'     wa_engb-stlal,   "ALT BOM
               ' ' 'RC29N-AENNR'     '19000101-001',  "Change number
               ' ' 'BDC_OKCODE'      '=FCPU',

                  'X' 'SAPLCSDI'        '0150',
                 ' ' 'BDC_OKCODE'      '=SETP'.

            PERFORM dynpro USING:
               'X' 'SAPLCSDI'        '0708',
               ' ' 'RC29P-SELID'     l_idnrk,      "Component
               ' ' 'RC29P-SELPI'     l_selpi,       "Item ID
               ' ' 'BDC_OKCODE'      '=CLWI',

               'X' 'SAPLCSDI'        '0150',
               ' ' 'RC29P-AUSKZ(01)' 'X',            "procurement type
               ' ' 'BDC_OKCODE'      '=PALL',

               'X' 'SAPLCSDI'        '2130',
               ' ' 'BDC_OKCODE'      '=+002',

               'X' 'SAPLCSDI'        '2130',
              ' ' 'ZINFO'            p_txt,    "INFO
               ' ' 'BDC_OKCODE'      '=FCBU'.
*           CALL TRANSACTION
            CALL TRANSACTION 'CS02'  USING it_bdc
                                     OPTIONS FROM wa_opt
                                     MESSAGES INTO it_mess.
            it_engb-zmode = sy-msgty.
            PERFORM rkc_msg_string CHANGING it_engb-zmsg.
            IF it_engb-zmode EQ 'E'.
              wa_line_ero = wa_line_ero + 1.
            ELSE.
              it_engb-zmode = 'S'.
              CONCATENATE 'Next Material' wa_engb-matnr
                          'Compenent' l_idnrk 'already exists'
                                   INTO it_engb-zmsg SEPARATED BY space.

            ENDIF.
          ENDIF.
        WHEN 'B'.
          wa_line_ero = wa_line_ero + 1.
          it_engb-zmode = 'E'.
          CONCATENATE 'Next Material' wa_engb-matnr
                      'Compenent' m_idnrk 'Chaging Compenent' l_idnrk
                      'Unequal ' INTO it_engb-zmsg SEPARATED BY space.

        WHEN 'C'.
*       SUCCESS MESSAGE
          PERFORM dynpro USING:
             'X' 'SAPLCSDI'       '0100',
             ' ' 'RC29N-MATNR'     wa_engb-matnr,   "NEXT MATERIAL
             ' ' 'RC29N-WERKS'     wa_engb-werks,   "PLANT
             ' ' 'RC29N-STLAN'     wa_engb-stlan,   "BOM usage
             ' ' 'RC29N-STLAL'     wa_engb-stlal,   "ALT BOM
             ' ' 'RC29N-AENNR'     '19000101-001',  "Change number
             ' ' 'BDC_OKCODE'      '=FCPU',

                'X' 'SAPLCSDI'        '0150',
               ' ' 'BDC_OKCODE'      '=FCNP'.

          PERFORM dynpro USING:

             'X' 'SAPLCSDI'        '0140',
             ' ' 'RC29P-AUSKZ(02)' 'X'   ,        "CHECK
             ' ' 'RC29P-IDNRK(02)' l_idnrk,       "BOM compenent
             ' ' 'RC29P-MENGE(02)' '1',           "Compenent quantity
             ' ' 'RC29P-POSTP(02)' 'L'         ,  "Item category
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0130',
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0131',
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0138',
            ' ' 'ZSTGB'            'F'    ,     "STRUCTURE TYPE
            ' ' 'ZSUFF'            '0000',      "SUFFIX NO
            ' ' 'ZSEQU'            '0001',      "SEQUENCE NO
            ' ' 'ZINFO'            p_txt,    "INFO
            ' ' 'BDC_OKCODE'      '/00'.

          PERFORM dynpro USING:
            'X' 'SAPLCSDI'    '0140',
            ' ' 'BDC_OKCODE'  '=FCBU'.
*       CALL TRANSACTION
          CALL TRANSACTION 'CS02'  USING it_bdc
                                   OPTIONS FROM wa_opt
                                   MESSAGES INTO it_mess.
          it_engb-zmode = sy-msgty.
          PERFORM rkc_msg_string CHANGING it_engb-zmsg.
          IF it_engb-zmode EQ 'E'.
            wa_line_ero = wa_line_ero + 1.
          ELSE.
            it_engb-zmode = 'S'.
            CONCATENATE 'Next Material' wa_engb-matnr
                        'Compenent' l_idnrk 'Created'
                                   INTO it_engb-zmsg SEPARATED BY space.
          ENDIF.
          REFRESH: it_bdc, it_mess.
      ENDCASE.
    ELSE.
*     ERROR MESSAGE
      wa_line_ero = wa_line_ero + 1.
      it_engb-zmode = 'E'.
      it_engb-zmsg = 'Engine/TM Assembly  ALC code NO Item'.
    ENDIF.
    MODIFY it_engb INDEX l_tabix TRANSPORTING zmode
                                              zmsg.
    CLEAR: it_engb, wa_engb.
  ENDLOOP.
  SORT it_engb BY zmode matnr.
  DESCRIBE TABLE it_engb LINES wa_line_tot.

  DATA l_maktx LIKE makt-maktx.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / text-082.
  FORMAT COLOR OFF.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*  WA_TOP_PAGE = 'X'.
  LOOP AT it_engb WHERE zmode EQ 'E'.
    CLEAR l_maktx.
    SELECT SINGLE maktx
                FROM makt
                INTO l_maktx
                WHERE matnr EQ it_engb-matnr.

    WRITE: /(20) it_engb-matnr,
            (40) l_maktx,
            (06) it_engb-werks,
            (05) it_engb-stlan,
            (15) it_engb-stlal,
            (10) it_engb-zmode,
            (100) it_engb-zmsg.
  ENDLOOP.
*  WA_TOP_PAGE = ' '.
  FORMAT COLOR OFF.

ENDFORM.                    " ENGIN_BOM_PROCESS
*&---------------------------------------------------------------------*
*&      Form  TM_BOM_PROCESS
*&---------------------------------------------------------------------*
FORM tm_bom_process.
*  DATA: BEGIN OF LT_BMDT OCCURS 0,
*          MATNR TYPE ZTBM_ABXEBMDT-MTNO,
*          WERKS TYPE ZTBM_ABXEBMDT-PLNT,
*          STLAN TYPE ZTBM_ABXEBMDT-USAG,
*          STLAL TYPE ZTBM_ABXEBMDT-ALTN,
*          ZMODE,
*          ZMSG  LIKE CFGNL-MSGLIN,
*        END OF LT_BMDT.
  DATA: la_bmdt LIKE it_engb,
        l_idnrk LIKE mara-matnr,
        m_idnrk LIKE mara-matnr,
        l_zinfo LIKE stpo-zinfo,
        l_selid LIKE rc29p-selid,       "Component
        l_selpi LIKE rc29p-selpi,       "Item ID
        l_chk.
  DATA: l_tabix TYPE sy-tabix,
        l_worko(40),
        l_stlal(03).
  DATA: wa_line_tot TYPE i,
        wa_line_ero TYPE i.
  REFRESH it_engb. CLEAR it_engb.
  LOOP AT it_subm.
    SELECT SINGLE *
                FROM mara
                WHERE matnr EQ it_subm-matnr
                AND   mtart EQ 'FERT'.
    IF sy-subrc EQ 0.
      it_engb = it_subm.
      APPEND it_engb.
    ENDIF.
    CLEAR: it_subm, it_engb.
  ENDLOOP.

  LOOP AT it_engb.
    l_tabix = sy-tabix.
    wa_engb = it_engb.
    AT NEW stlal.
      CLEAR l_idnrk.
      PERFORM search_engine_material USING    wa_engb-matnr
                                              wa_engb-stlal
                                              'P_ALC_U_2'
                                     CHANGING l_idnrk.
    ENDAT.
    IF NOT l_idnrk IS INITIAL.
      CLEAR: l_chk.
      PERFORM check_component USING    wa_engb-matnr
                                       wa_engb-werks
                                       wa_engb-stlan
                                       wa_engb-stlal
                                       l_idnrk
                                       'TM'
                              CHANGING l_chk
                                       m_idnrk
                                       l_zinfo
                                       l_selpi.
*                    TM
*     L_CHK EQ A ==> U24T  EQ U02T
*     L_CHK EQ B ==> U24T  NE U24T
*     L_CHK EQ C ==> SPACE
      CASE l_chk.
        WHEN 'A'.
*
          IF l_zinfo IS INITIAL.
*
*            PERFORM ENGINE_BOM_TYPE_A.
            PERFORM dynpro USING:
               'X' 'SAPLCSDI'       '0100',
               ' ' 'RC29N-MATNR'     wa_engb-matnr,   "NEXT MATERIAL
               ' ' 'RC29N-WERKS'     wa_engb-werks,   "PLANT
               ' ' 'RC29N-STLAN'     wa_engb-stlan,   "BOM usage
               ' ' 'RC29N-STLAL'     wa_engb-stlal,   "ALT BOM
               ' ' 'RC29N-AENNR'     '19000101-001',  "Change number
               ' ' 'BDC_OKCODE'      '=FCPU',

                  'X' 'SAPLCSDI'        '0150',
                 ' ' 'BDC_OKCODE'      '=SETP'.

            PERFORM dynpro USING:
               'X' 'SAPLCSDI'        '0708',
               ' ' 'RC29P-SELID'     l_idnrk,      "Component
               ' ' 'RC29P-SELPI'     l_selpi,       "Item ID
               ' ' 'BDC_OKCODE'      '=CLWI',

               'X' 'SAPLCSDI'        '0150',
               ' ' 'RC29P-AUSKZ(01)' 'X',            "procurement type
               ' ' 'BDC_OKCODE'      '=PALL',

               'X' 'SAPLCSDI'        '2130',
               ' ' 'BDC_OKCODE'      '=+002',

               'X' 'SAPLCSDI'        '2130',
              ' ' 'ZINFO'            'TM',    "INFO
               ' ' 'BDC_OKCODE'      '=FCBU'.
*           CALL TRANSACTION
            CALL TRANSACTION 'CS02'  USING it_bdc
                                     OPTIONS FROM wa_opt
                                     MESSAGES INTO it_mess.
            it_engb-zmode = sy-msgty.
            PERFORM rkc_msg_string CHANGING it_engb-zmsg.
            IF it_engb-zmode EQ 'E'.
              wa_line_ero = wa_line_ero + 1.
            ELSE.
              it_engb-zmode = 'S'.
              CONCATENATE 'Next Material' wa_engb-matnr
                          'Compenent' l_idnrk 'already exists'
                                   INTO it_engb-zmsg SEPARATED BY space.

            ENDIF.
          ENDIF.
        WHEN 'B'.
          wa_line_ero = wa_line_ero + 1.
          it_engb-zmode = 'E'.
          CONCATENATE 'Next Material' wa_engb-matnr
                      'Compenent' m_idnrk 'Chaging Compenent' l_idnrk
                      'Unequal ' INTO it_engb-zmsg SEPARATED BY space.

        WHEN 'C'.
*       SUCCESS MESSAGE
          PERFORM dynpro USING:
             'X' 'SAPLCSDI'       '0100',
             ' ' 'RC29N-MATNR'     wa_engb-matnr,   "NEXT MATERIAL
             ' ' 'RC29N-WERKS'     wa_engb-werks,   "PLANT
             ' ' 'RC29N-STLAN'     wa_engb-stlan,   "BOM usage
             ' ' 'RC29N-STLAL'     wa_engb-stlal,   "ALT BOM
             ' ' 'RC29N-AENNR'     '19000101-001',  "Change number
             ' ' 'BDC_OKCODE'      '=FCPU',

                'X' 'SAPLCSDI'        '0150',
               ' ' 'BDC_OKCODE'      '=FCNP'.

          PERFORM dynpro USING:

             'X' 'SAPLCSDI'        '0140',
             ' ' 'RC29P-AUSKZ(02)' 'X'   ,        "CHECK
             ' ' 'RC29P-IDNRK(02)' l_idnrk,       "BOM compenent
             ' ' 'RC29P-MENGE(02)' '1',           "Compenent quantity
             ' ' 'RC29P-POSTP(02)' 'L'         ,  "Item category
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0130',
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0131',
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0138',
            ' ' 'ZSTGB'            'F'    ,     "STRUCTURE TYPE
            ' ' 'ZSUFF'            '0000',      "SUFFIX NO
            ' ' 'ZSEQU'            '0001',      "SEQUENCE NO
            ' ' 'ZINFO'               'TM',    "INFO
            ' ' 'BDC_OKCODE'      '/00'.

          PERFORM dynpro USING:
            'X' 'SAPLCSDI'    '0140',
            ' ' 'BDC_OKCODE'  '=FCBU'.
*       CALL TRANSACTION
          CALL TRANSACTION 'CS02'  USING it_bdc
                                   OPTIONS FROM wa_opt
                                   MESSAGES INTO it_mess.
          it_engb-zmode = sy-msgty.
          PERFORM rkc_msg_string CHANGING it_engb-zmsg.
          IF it_engb-zmode EQ 'E'.
            wa_line_ero = wa_line_ero + 1.
          ELSE.
            it_engb-zmode = 'S'.
            CONCATENATE 'Next Material' wa_engb-matnr
                        'Compenent' l_idnrk 'Created'
                                   INTO it_engb-zmsg SEPARATED BY space.
          ENDIF.
          REFRESH: it_bdc, it_mess.
      ENDCASE.
    ELSE.
*     ERROR MESSAGE
      wa_line_ero = wa_line_ero + 1.
      it_engb-zmode = 'E'.
      it_engb-zmsg = 'Transmission ALC code NO Item'.
    ENDIF.
    MODIFY it_engb INDEX l_tabix TRANSPORTING zmode
                                              zmsg.
    CLEAR: it_engb, wa_engb.
  ENDLOOP.
  SORT it_engb BY zmode matnr.
  DESCRIBE TABLE it_engb LINES wa_line_tot.

  DATA l_maktx LIKE makt-maktx.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / text-082.
  FORMAT COLOR OFF.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*  WA_TOP_PAGE = 'X'.
  LOOP AT it_engb WHERE zmode EQ 'E'.
    CLEAR l_maktx.
    SELECT SINGLE maktx
                FROM makt
                INTO l_maktx
                WHERE matnr EQ it_engb-matnr.

    WRITE: /(20) it_engb-matnr,
            (40) l_maktx,
            (06) it_engb-werks,
            (05) it_engb-stlan,
            (15) it_engb-stlal,
            (10) it_engb-zmode,
            (100) it_engb-zmsg.
  ENDLOOP.
*  WA_TOP_PAGE = ' '.
  FORMAT COLOR OFF.

ENDFORM.                    " TM_BOM_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SEARCH_ENGINE_MATERIAL
*&---------------------------------------------------------------------*
FORM search_engine_material USING    p_matnr
                                     p_stlal
                                     p_value
                            CHANGING p_idnrk.
  DATA: l_worko(40),
        l_stlal(03),
        l_matnr LIKE mara-matnr,
        l_atinn LIKE cabn-atinn,
        l_atwrt LIKE ausp-atwrt.
  CONCATENATE '0' p_stlal INTO l_stlal.
  CONCATENATE p_matnr l_stlal INTO l_worko SEPARATED BY space.

  SELECT SINGLE a~matnr
              FROM mara AS a INNER JOIN makt AS b
                             ON a~matnr EQ b~matnr
              INTO l_matnr
              WHERE a~mtart EQ 'WOHD'
              AND   b~maktx EQ l_worko
              AND   b~spras EQ sy-langu.
  IF sy-subrc EQ 0.
    PERFORM read_cabn USING    p_value
                      CHANGING l_atinn.

    SELECT SINGLE atwrt
                FROM ausp
                INTO l_atwrt
                WHERE objek EQ l_matnr
                AND   atinn EQ l_atinn.
    IF sy-subrc EQ 0.
      p_idnrk = l_atwrt.
    ELSE.
      CLEAR p_idnrk.
    ENDIF.
  ENDIF.

ENDFORM.                    " SEARCH_ENGINE_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  CHECK_COMPONENT
*&---------------------------------------------------------------------*
FORM check_component USING    p_matnr
                              p_werks
                              p_stlan
                              p_stlal
                              p_idnrk
                              p_value
                     CHANGING p_chk
                              q_idnrk
                              p_zinfo
                              p_selpi.
  SELECT SINGLE b~idnrk
                b~stlkn
                b~zinfo
              FROM mast AS a INNER JOIN stpo AS b
                             ON a~stlnr EQ b~stlnr
              INTO  (q_idnrk, p_selpi, p_zinfo)
              WHERE a~matnr EQ p_matnr
              AND   a~werks EQ p_werks
              AND   a~stlan EQ p_stlan
              AND   a~stlal EQ p_stlal
              AND   b~idnrk EQ p_idnrk.
  IF sy-subrc EQ 0.
    p_chk = 'A'.
  ELSE.
    SELECT SINGLE b~idnrk
                  b~zinfo
                FROM mast AS a INNER JOIN stpo AS b
                               ON a~stlnr EQ b~stlnr
                INTO  (q_idnrk, p_zinfo)
                WHERE a~matnr EQ p_matnr
                AND   a~werks EQ p_werks
                AND   a~stlan EQ p_stlan
                AND   a~stlal EQ p_stlal
                AND   b~zinfo EQ p_value.
    IF sy-subrc EQ 0.
      p_chk = 'B'.
    ELSE.
      p_chk = 'C'.

    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_COMPONENT
*&---------------------------------------------------------------------*
*&      Form  SUB_MATERIAL_BOM_PERCESS
*&---------------------------------------------------------------------*
FORM sub_material_bom_percess.
  DATA: BEGIN OF lt_bmdt OCCURS 0,
         matnr TYPE ztbm_abxebmdt-mtno,
         werks TYPE ztbm_abxebmdt-plnt,
         stlan TYPE ztbm_abxebmdt-usag,
         stlal TYPE ztbm_abxebmdt-altn,
         zmode,
         zmsg  LIKE cfgnl-msglin,
       END OF lt_bmdt.
  DATA: lt_vel LIKE it_vel OCCURS 0 WITH HEADER LINE.
  DATA: l_tabix TYPE sy-tabix.
  DATA: la_bmdt LIKE lt_bmdt.
  DATA: BEGIN OF lt_bom OCCURS 0,
          idnrk TYPE stpo-idnrk,
        END OF lt_bom.
  DATA: wa_line_tot TYPE i,
        wa_line_ero TYPE i.

  PERFORM read_ztbm_sub_bom_vel.

  LOOP AT it_subm.
    SELECT SINGLE *
                FROM mara
                WHERE matnr EQ it_subm-matnr
                AND   mtart EQ 'FERT'.
    IF sy-subrc EQ 0.
      lt_bmdt = it_subm.
      APPEND lt_bmdt.
    ENDIF.
    CLEAR: it_subm, lt_bmdt.
  ENDLOOP.



  DESCRIBE TABLE lt_bmdt LINES wa_line_tot.
  LOOP AT lt_bmdt.
    l_tabix = sy-tabix.
    la_bmdt = lt_bmdt.
    AT NEW matnr.
      REFRESH lt_vel. CLEAR lt_vel.
      REFRESH lt_bom. CLEAR lt_bom.
      PERFORM cs_bom_expl_mat_v2 TABLES   lt_bom
                                 USING    la_bmdt-matnr
                                          la_bmdt-werks
                                          la_bmdt-stlan
                                          la_bmdt-stlal.
      PERFORM read_it_vel TABLES lt_vel
                          USING la_bmdt-werks
                                la_bmdt-matnr.
    ENDAT.
    IF NOT lt_vel[] IS INITIAL.
      PERFORM dynpro USING:
         'X' 'SAPLCSDI'       '0100',
         ' ' 'RC29N-MATNR'     la_bmdt-matnr,    "NEXT MATERIAL
         ' ' 'RC29N-WERKS'     la_bmdt-werks,   "PLANT
         ' ' 'RC29N-STLAN'     la_bmdt-stlan,   "BOM usage
         ' ' 'RC29N-STLAL'     la_bmdt-stlal,   "ALT BOM
         ' ' 'RC29N-AENNR'     '19000101-001',  "Change number
         ' ' 'BDC_OKCODE'      '=FCPU',

            'X' 'SAPLCSDI'        '0150',
           ' ' 'BDC_OKCODE'      '=FCNP'.
      LOOP AT lt_vel.
        READ TABLE lt_bom WITH KEY idnrk = lt_vel-matnr
                          BINARY SEARCH TRANSPORTING NO FIELDS .
        IF sy-subrc EQ 0.
          CLEAR lt_vel.
          CONTINUE.
        ELSE.

          PERFORM dynpro USING:

             'X' 'SAPLCSDI'        '0140',
             ' ' 'RC29P-AUSKZ(02)' 'X'   ,        "CHECK
             ' ' 'RC29P-IDNRK(02)' lt_vel-matnr,    "BOM compenent
             ' ' 'RC29P-MENGE(02)' '1',          "Compenent quantity
             ' ' 'RC29P-POSTP(02)' 'L'         ,    "Item category
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0130',
             ' ' 'RC29P-ITSOB'     '50',            "procurement type
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0131',
             ' ' 'BDC_OKCODE'      '/00',

             'X' 'SAPLCSDI'        '0138',
            ' ' 'ZSTGB'            'F'    ,     "STRUCTURE TYPE
            ' ' 'ZSUFF'            '0000',      "SUFFIX NO
            ' ' 'ZSEQU'            '0001',      "SEQUENCE NO
             ' ' 'BDC_OKCODE'      '/00',
             'X' 'SAPLCSDI'        '0140',
             ' ' 'BDC_OKCODE'      '=FCNP'.
        ENDIF.
        CLEAR lt_vel.
      ENDLOOP.

      PERFORM dynpro USING:
              'X' 'SAPLCSDI'    '0140',
              ' ' 'BDC_OKCODE'  '=FCBU'.
*     CALL TRANSACTION
      CALL TRANSACTION 'CS02'  USING it_bdc
                               OPTIONS FROM wa_opt
                               MESSAGES INTO it_mess.
      lt_bmdt-zmode = sy-msgty.
      PERFORM rkc_msg_string CHANGING lt_bmdt-zmsg.
      IF lt_bmdt-zmode EQ 'E'.
        wa_line_ero = wa_line_ero + 1.
      ENDIF.
      REFRESH: it_bdc, it_mess.

    ELSE.
      wa_line_ero = wa_line_ero + 1.
      lt_bmdt-zmode = 'E'.

      lt_bmdt-zmsg = 'Sub material NO ITEM'.
    ENDIF.
    MODIFY lt_bmdt INDEX l_tabix TRANSPORTING zmode
                                              zmsg.
    CLEAR: lt_bmdt, la_bmdt.
  ENDLOOP.
  DATA l_maktx LIKE makt-maktx.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / text-082.
  FORMAT COLOR OFF.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  LOOP AT lt_bmdt WHERE zmode NE 'E'.
    CLEAR l_maktx.
    SELECT SINGLE maktx
                FROM makt
                INTO l_maktx
                WHERE matnr EQ lt_bmdt-matnr.
    WRITE: /(20) lt_bmdt-matnr,
            (40) l_maktx,
            (06) lt_bmdt-werks,
            (05) lt_bmdt-stlan,
            (15) lt_bmdt-stlal,
            (10) lt_bmdt-zmode,
            (100) lt_bmdt-zmsg.
  ENDLOOP.
  FORMAT COLOR OFF.
ENDFORM.                    " SUB_MATERIAL_BOM_PERCESS
*&---------------------------------------------------------------------*
*&      Form  CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
FORM cs_bom_expl_mat_v2 TABLES   pt_bom
                        USING    p_matnr
                                 p_werks
                                 p_stlan
                                 p_stlal.
  DATA: BEGIN OF lt_bom OCCURS 0,
          idnrk TYPE stpo-idnrk,
        END OF lt_bom.

  DATA: BEGIN OF selpool OCCURS 0.
          INCLUDE STRUCTURE cstmat.
  DATA: END OF selpool.
  DATA: lt_stb TYPE  stpox OCCURS 0 WITH HEADER LINE,
        lt_matcat TYPE  cscmat OCCURS 0 WITH HEADER LINE,
        lt_topmat LIKE  cstmat,
        l_cuobj   LIKE  marc-cuobj,
        l_stlnr   LIKE  mast-stlnr,
        lt_dstst  LIKE  csdata-xfeld.
  CLEAR: l_cuobj.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            aumng  = 0
            capid  = 'PP01'
*            CUOBJ  = L_CUOBJ
            cuovs  = '0'
            datuv  = '19000101' "P_ZEDAT
            emeng  = '1'
            mktls  = 'X'
*            MEHRS  = 'X'
            mtnrv  = p_matnr
            stpst  = 0
            stlan  = p_stlan
            stlal  = p_stlal
            svwvo  = 'X'
            werks  = p_werks
            vrsvo  = 'X'
* IMPORTING
*   TOPMAT                      = LT_TOPMAT
*   DSTST                       = LT_DSTST
 TABLES
            stb    = lt_stb
            matcat = selpool
 EXCEPTIONS
   alt_not_found               = 1
   call_invalid                = 2
   material_not_found          = 3
   missing_authorization       = 4
   no_bom_found                = 5
   no_plant_data               = 6
   no_suitable_bom_found       = 7
   conversion_error            = 8
   OTHERS                      = 9
          .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_stb.
    lt_bom-idnrk = lt_stb-idnrk.
    APPEND lt_bom.
    CLEAR lt_bom.
  ENDLOOP.
  SORT lt_bom BY idnrk.
  pt_bom[] = lt_bom[].
ENDFORM.                    " CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
*&      Form  READ_IT_VEL
*&---------------------------------------------------------------------*
FORM read_it_vel TABLES pt_vel STRUCTURE it_vel
                 USING  p_werks
                        p_matnr.
  DATA: mt_vel LIKE it_vel OCCURS 0 WITH HEADER LINE.
  DATA: l_zvalue TYPE ztbm_model_val-zvalue,
        l_z_nation TYPE ztbm_sub_bom_vel-z_nation.

  REFRESH mt_vel. CLEAR mt_vel.
  LOOP AT it_vel WHERE werks EQ p_werks
                 AND  ( ( ( z_nation EQ p_matnr+1(5) OR
                            z_nation EQ '*'           ) AND
** Changed By Tonkey on 05/19/2004.
** 'XX' : Body In White, 'XY' : Body In Paint
                          ( z_nation+03(02) <> 'XX'  AND
                            z_nation+03(02) <> 'XY'    )
                        )  OR
*
                        ( z_nation+03(02) = p_matnr+04(02) AND
                          ( z_nation+03(02) = 'XX' OR
                            z_nation+03(02) = 'XY'  )
                        )
                      ).
    IF ( p_matnr+04(02) = 'XX' OR p_matnr+04(02) = 'XY' ) AND
       ( it_vel-z_nation = p_matnr+01(05) OR
         it_vel-z_nation = '*'             ).
      CONTINUE.
    ENDIF.
**
    pt_vel = it_vel.
    APPEND pt_vel.
    CLEAR: pt_vel, it_vel.
  ENDLOOP.

  REFRESH mt_vel. CLEAR mt_vel.
  LOOP AT pt_vel WHERE z_car EQ p_matnr+6(2)
                      OR z_car EQ '*'.
    mt_vel = pt_vel.
    APPEND mt_vel.
    CLEAR: it_vel, pt_vel.
  ENDLOOP.

  REFRESH pt_vel. CLEAR pt_vel.
  pt_vel[] = mt_vel[].
  REFRESH mt_vel. CLEAR mt_vel.

  LOOP AT pt_vel WHERE z_year EQ p_matnr(1)
                 OR    z_year EQ '*'.
    mt_vel = pt_vel.
    APPEND mt_vel.
    CLEAR: mt_vel, pt_vel.
  ENDLOOP.

  REFRESH pt_vel. CLEAR pt_vel.
  pt_vel[] = mt_vel[].
  REFRESH mt_vel. CLEAR mt_vel.

**  SELECT SINGLE ZVALUE
**              FROM ZTBM_MODEL_VAL
**              INTO L_ZVALUE
**              WHERE ZFIELD EQ '03'
**              AND   ZVALNM EQ P_MATNR+1(3).
**  IF SY-SUBRC EQ 0.
**    CLEAR L_Z_NATION.
**    L_Z_NATION = L_ZVALUE.
**  ENDIF.
*  LOOP AT PT_VEL WHERE Z_NATION EQ P_MATNR+1(5)
*                 OR    Z_NATION EQ '*'.
*    MT_VEL = PT_VEL.
*    APPEND MT_VEL.
*    CLEAR: MT_VEL, PT_VEL.
*  ENDLOOP.
*  REFRESH PT_VEL. CLEAR PT_VEL.
*  PT_VEL[] = MT_VEL[].
*  REFRESH MT_VEL. CLEAR MT_VEL.

  LOOP AT pt_vel WHERE z_bt EQ p_matnr+8(1)
                 OR    z_bt EQ '*'.
    mt_vel = pt_vel.
    APPEND mt_vel.
    CLEAR: mt_vel, pt_vel.
  ENDLOOP.
  REFRESH pt_vel. CLEAR pt_vel.
  pt_vel[] = mt_vel[].
  REFRESH mt_vel. CLEAR mt_vel.

  LOOP AT pt_vel WHERE z_tl EQ p_matnr+9(1)
                 OR    z_tl EQ '*'.
    mt_vel = pt_vel.
    APPEND mt_vel.
    CLEAR: mt_vel, pt_vel.
  ENDLOOP.
  REFRESH pt_vel. CLEAR pt_vel.
  pt_vel[] = mt_vel[].
  REFRESH mt_vel. CLEAR mt_vel.

  LOOP AT pt_vel WHERE z_ec EQ p_matnr+10(1)
                 OR    z_ec EQ '*'.
    mt_vel = pt_vel.
    APPEND mt_vel.
    CLEAR: mt_vel, pt_vel.
  ENDLOOP.
  REFRESH pt_vel. CLEAR pt_vel.
  pt_vel[] = mt_vel[].
  REFRESH mt_vel. CLEAR mt_vel.

  LOOP AT pt_vel WHERE z_ft EQ p_matnr+11(1)
                 OR    z_ft EQ '*'.
    mt_vel = pt_vel.
    APPEND mt_vel.
    CLEAR: mt_vel, pt_vel.
  ENDLOOP.
  REFRESH pt_vel. CLEAR pt_vel.
  pt_vel[] = mt_vel[].
  REFRESH mt_vel. CLEAR mt_vel.

  LOOP AT pt_vel WHERE z_tm EQ p_matnr+12(1)
                 OR    z_tm EQ '*'.
    mt_vel = pt_vel.
    APPEND mt_vel.
    CLEAR: mt_vel, pt_vel.
  ENDLOOP.
  REFRESH pt_vel. CLEAR pt_vel.
  pt_vel[] = mt_vel[].
ENDFORM.                    " READ_IT_VEL
*&---------------------------------------------------------------------*
*&      Form  READ_ZTBM_SUB_BOM_VEL
*&---------------------------------------------------------------------*
FORM read_ztbm_sub_bom_vel.

  SELECT *
       FROM ztbm_sub_bom_vel
       INTO TABLE it_vel.
  IF sy-subrc EQ 0.
    SORT it_vel BY matnr
                   werks
                   z_nation
                   sequ  DESCENDING.
  ENDIF.


ENDFORM.                    " READ_ZTBM_SUB_BOM_VEL
*&---------------------------------------------------------------------*
*&      Form  SUB_ENGINE_BOM_UPDATE
*&---------------------------------------------------------------------*
FORM sub_engine_bom_update.
  IF p_submit EQ 'X'.

    PERFORM engin_bom_process USING 'P_ALC_U_1' 'ENG'.

    PERFORM engin_bom_process USING 'P_ALC_U_2' 'TM'.

    PERFORM sub_material_bom_percess.

  ENDIF.

ENDFORM.                    " SUB_ENGINE_BOM_UPDATE
*&---------------------------------------------------------------------*
*&      Form  READ_CABN
*&---------------------------------------------------------------------*
FORM read_cabn USING    p_value
               CHANGING p_atinn.
  SELECT SINGLE atinn
            FROM cabn
            INTO p_atinn
            WHERE atnam EQ p_value.

ENDFORM.                    " READ_CABN
*&---------------------------------------------------------------------*
*&      Form  TABLE_NON_COLOR_BOM_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_BMNR  text
*      -->P_L_STLKN  text
*----------------------------------------------------------------------*
FORM table_non_color_bom_change USING  pa_bmnc STRUCTURE wa_bmnc
                                       p_stlkn.
  REFRESH: it_bdc, it_mess.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' pa_bmnc-mtno,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' pa_bmnc-plnt,    "PLANT
     ' ' 'RC29N-STLAN' pa_bmnc-usag,    "BOM usage
     ' ' 'RC29N-STLAL' pa_bmnc-altn,    "ALT BOM
     ' ' 'RC29N-AENNR' pa_bmnc-eono,    "Change number
     ' ' 'BDC_OKCODE'  '=KALL',

     'X' 'SAPLCSDI'    '2110',
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=SETP',

     'X' 'SAPLCSDI'    '0708',
     ' ' 'RC29P-SELPI' p_stlkn,
     ' ' 'BDC_OKCODE'  '=CLWI',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',
     ' ' 'BDC_OKCODE'  '=PALL',

     'X' 'SAPLCSDI'    '02130',
     ' ' 'BDC_OKCODE'  '=+002',

     'X' 'SAPLCSDI'    '02130',
     ' ' 'ZSEQU'       pa_bmnc-sequ,
     ' ' 'BDC_OKCODE'  '=FCBU'.

  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt
                           MESSAGES INTO it_mess.
ENDFORM.                    " TABLE_NON_COLOR_BOM_CHANGE
*&---------------------------------------------------------------------*
*&      Form  manage_object_dependency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM manage_object_dependency.
  DATA: lw_stlkn TYPE stpo-stlkn.

  DATA: lw_knobj LIKE stpo-knobj.

  DATA: lt_stb TYPE  stpox OCCURS 0 WITH HEADER LINE.

  LOOP AT it_bmco WHERE zresult EQ 'E'.
    MOVE-CORRESPONDING it_bmco TO it_bmdt.
    APPEND it_bmdt.
    DELETE it_bmco.
    CLEAR: it_bmdt, it_bmco.
  ENDLOOP.

  SORT it_bmco BY mtno plnt usag altn pref comp suff sequ eono seqc.
  LOOP AT it_bmco WHERE zresult NE 'E'.
    CLEAR: lw_knobj, lw_stlkn.

    PERFORM read_od_allocation_no USING lw_knobj lw_stlkn.

    CHECK it_bmco-zresult NE 'E'.

    PERFORM read_object_dependency USING it_bmco-dpid lw_knobj.

    CHECK it_bmco-zresult NE 'E'.

*    PERFORM get_stlkn_for_od USING lw_stlkn.
*
*    CHECK it_bmco-zresult NE 'E'.

    PERFORM generate_bdc_for_od USING lw_stlkn.

    PERFORM call_transaction_cs02_for_od.
  ENDLOOP.

* COLOR LINE COUNT
  DESCRIBE TABLE it_bmco LINES wa_line_idx.
* ERROR LINE COUNT
  LOOP AT it_bmco.
    MOVE-CORRESPONDING it_bmco TO it_bmdt.
    it_bmdt-zbdat = sy-datum.
    it_bmdt-zbnam   = sy-uname.

    IF it_bmco-zresult EQ'E'.
      wa_erro_idx = wa_erro_idx + 1.
*      DELETE IT_BMCO INDEX L_TABIX.
    ELSE.
      it_bmdt-zbtim = sy-uzeit.
    ENDIF.

    APPEND it_bmdt.
    CLEAR: it_bmco, it_bmdt.
  ENDLOOP.

*  REFRESH IT_BMCO. CLEAR IT_BMCO.
  WRITE: / text-036,
            wa_line_idx.
  FORMAT COLOR 6 INTENSIFIED OFF.
  WRITE: / text-037,
            wa_erro_idx.
  FORMAT COLOR OFF.

* Error appears if is more than one case.
  IF wa_erro_idx GE '1'.
    PERFORM error_data_write TABLES it_bmdt
                             USING  'E'.

  ENDIF.
  LOOP AT it_bmdt.
    MOVE-CORRESPONDING it_bmdt TO it_abxebmdt.
    APPEND it_abxebmdt.
    CLEAR: it_abxebmdt, it_bmdt.
  ENDLOOP.
ENDFORM.                    " manage_object_dependency
*&---------------------------------------------------------------------*
*&      Form  read_object_dependency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB_KNOBJ  text
*      -->P_LW_ODIDX  text
*----------------------------------------------------------------------*
FORM read_object_dependency USING p_knnam p_knobj.
  DATA: lt_cuob LIKE cuob OCCURS 0 WITH HEADER LINE.

  CLEAR: it_knnam, it_knnam[].

  SELECT * INTO TABLE lt_cuob
    FROM cuob
   WHERE kntab =  'STPO'
     AND knobj =  p_knobj
     AND datuv <= sy-datum.
  IF sy-subrc NE 0.
    MOVE: '9999999999' TO lt_cuob-knnum.
    APPEND lt_cuob.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_knnam
    FROM cukb
     FOR ALL ENTRIES IN lt_cuob
   WHERE knnum =  lt_cuob-knnum
     AND adzhl =  lt_cuob-adzhl
     AND datuv <= sy-datum.


  CASE it_bmco-upct.
    WHEN '1'.
      READ TABLE it_knnam WITH KEY knnam = p_knnam.
      IF sy-subrc EQ 0.
        MOVE: text-085 TO it_bmco-zmsg,
              'E'      TO it_bmco-zresult,
              'C'      TO it_bmco-zmode.
        MODIFY it_bmco.
        EXIT.
      ENDIF.
      MOVE: p_knnam TO it_knnam-knnam.
      APPEND it_knnam.
    WHEN '2'.
      READ TABLE it_knnam WITH KEY knnam = p_knnam.
      IF sy-subrc NE 0.
*        LOOP AT it_bmco TRANSPORTING NO FIELDS
*                        WHERE mtno = it_bmco-mtno
*                          AND plnt = it_bmco-plnt
*                          AND usag = it_bmco-usag
*                          AND altn = it_bmco-altn
*                          AND pref = it_bmco-pref
*                          AND comp = it_bmco-comp
*                          AND suff = it_bmco-suff
*                          AND seqc = it_bmco-seqc
*                          AND sequ < it_bmco-sequ
*                          AND upct = '1'.
*        ENDLOOP.
*        IF sy-subrc NE 0.
        MOVE: text-084 TO it_bmco-zmsg,
              'E'      TO it_bmco-zresult,
              'C'      TO it_bmco-zmode.
        MODIFY it_bmco.
        EXIT.
*        ENDIF.
      ENDIF.

      DELETE it_knnam WHERE knnam = p_knnam.
  ENDCASE.
ENDFORM.                    " read_object_dependency
*&---------------------------------------------------------------------*
*&      Form  bdc_od_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_od_create.
*  PERFORM dynpro USING:
*     'X' 'SAPLCUKD'        '0130',
*     ' ' 'BDC_OKCODE'      '=MPKA',
*
*     'X' 'SAPLCUKD'        '0130',
*     ' ' 'BDC_OKCODE'      '=DELZ'.
*
*  LOOP AT it_cukb.
*    PERFORM dynpro USING:
*       'X' 'SAPLCUKD'        '0130',
*       ' ' 'BDC_OKCODE'      '=NEWZ',
*
*       'X' 'SAPLCUKD'        '0130',
*       ' ' 'RCUKD-KNNAM(02)' it_cukb-knnam, "
*       ' ' 'BDC_OKCODE'      '/00'.
*  ENDLOOP.
*
*
*  PERFORM dynpro USING:
*     'X' 'SAPLCUKD'        '0130',
*     ' ' 'BDC_OKCODE'      '=NEWZ',
*
*     'X' 'SAPLCUKD'        '0130',
*     ' ' 'RCUKD-KNNAM(02)' it_bmco-dpid, "
*     ' ' 'BDC_OKCODE'      '/00',
*
*     'X' 'SAPLCUKD'        '0130',
*     ' ' 'BDC_OKCODE'      '=BACK',
*
*     'X' 'SAPLCSDI'        '0150',
*     ' ' 'BDC_OKCODE'      '=FCBU',
*
*     'X' 'SAPLCUKD'        '0130',
*     ' ' 'BDC_OKCODE'      '=MPKA',
*
*     'X' 'SAPLCUKD'        '0130',
*     ' ' 'BDC_OKCODE'      '=DELZ'.
*
*  LOOP AT it_knnam.
*    PERFORM dynpro USING:
*       'X' 'SAPLCUKD'        '0130',
*       ' ' 'BDC_OKCODE'      '=NEWZ',
*
*       'X' 'SAPLCUKD'        '0130',
*       ' ' 'RCUKD-KNNAM(02)' it_knnam-knnam, "
*       ' ' 'BDC_OKCODE'      '/00'.
*  ENDLOOP.
*
*
*  PERFORM dynpro USING:
*     'X' 'SAPLCUKD'        '0130',
*     ' ' 'BDC_OKCODE'      '=BACK',
*
*     'X' 'SAPLCSDI'        '0150',
*     ' ' 'BDC_OKCODE'      '=FCBU'.
ENDFORM.                    " bdc_od_create
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_for_od
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB_STLKN  text
*----------------------------------------------------------------------*
FORM generate_bdc_for_od USING p_stlkn.
  CLEAR: it_bdc, it_bdc[].

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0100',
     ' ' 'RC29N-MATNR'     it_bmco-mtno,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS'     it_bmco-plnt,    "PLANT
     ' ' 'RC29N-STLAN'     it_bmco-usag,    "BOM usage
     ' ' 'RC29N-STLAL'     it_bmco-altn,    "ALT BOM
     ' ' 'RC29N-AENNR'     it_bmco-eono,    "Change number
     ' ' 'BDC_OKCODE'      '=FCPU',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=SETP',

     'X' 'SAPLCSDI'        '0708',
     ' ' 'RC29P-SELPI'     p_stlkn,    "
     ' ' 'BDC_OKCODE'      '=CLWI',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',    "
     ' ' 'BDC_OKCODE'      '=WIZU',

     'X' 'SAPLCUKD'        '0130',
     ' ' 'BDC_OKCODE'      '=MRKA',

     'X' 'SAPLCUKD'        '0130',
     ' ' 'BDC_OKCODE'      '=DELZ'.

  LOOP AT it_knnam.
    PERFORM dynpro USING:
       'X' 'SAPLCUKD'        '0130',
       ' ' 'BDC_OKCODE'      '=NEWZ',

       'X' 'SAPLCUKD'        '0130',
       ' ' 'RCUKD-KNNAM(02)' it_knnam-knnam,
       ' ' 'BDC_OKCODE'      '/00'.
  ENDLOOP.

  PERFORM dynpro USING:
     'X' 'SAPLCUKD'        '0130',
     ' ' 'BDC_OKCODE'      '=BACK',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=FCBU'.
ENDFORM.                    " generate_bdc_for_od
*&---------------------------------------------------------------------*
*&      Form  call_transaction_cs02_for_od
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_transaction_cs02_for_od.
  CLEAR: it_mess, it_mess[].

  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt
                           MESSAGES INTO it_mess.
  IF sy-subrc NE 0 OR sy-msgno NE '031'.
    it_bmco-zresult = 'E'.
  ELSE.
    it_bmco-zresult = 'S'.
  ENDIF.

  it_bmco-zmode = 'C'.

  PERFORM rkc_msg_string CHANGING it_bmco-zmsg.

  MODIFY it_bmco.
ENDFORM.                    " call_transaction_cs02_for_od
*&---------------------------------------------------------------------*
*&      Form  read_bom_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_od_allocation_no USING pw_knobj pw_stlkn.
  DATA: l_zsequ(4) TYPE n,
        l_stlnr TYPE mast-stlnr,
        l_cnt TYPE i.

  DATA: lw_topmat LIKE cstmat.

  CLEAR: pw_knobj.

*  SELECT SINGLE b~knobj INTO pw_knobj
*       FROM mast AS a INNER JOIN stpo AS b
*                      ON    b~stlty EQ 'M'
*                      AND   a~stlnr EQ b~stlnr
*       WHERE a~matnr EQ it_bmco-mtno
*       AND   a~werks EQ it_bmco-plnt
*       AND   a~stlan EQ it_bmco-usag
*       AND   a~stlal EQ it_bmco-altn
*       AND   b~posnr EQ it_bmco-pref
*       AND   b~idnrk EQ it_bmco-comp
*       AND   b~suff  EQ it_bmco-suff.
*
*
*
  DATA: lt_stb LIKE stpox OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
*            aumng                 = 0
            capid                 = c_capid
*            cuovs                 = '0'
            datuv                 = sy-datum
*            mktls                 = 'X'
            cuobj                 = '999999999999999999'
            mtnrv                 = it_bmco-mtno
*            stpst                 = 0
            stlan                 = it_bmco-usag
            stlal                 = it_bmco-altn
*            svwvo                 = 'X'
            werks                 = it_bmco-plnt
*            vrsvo                 = 'X'
            mmory = '0'
       IMPORTING
            topmat                = lw_topmat
       TABLES
            stb                   = lt_stb
       EXCEPTIONS
            alt_not_found         = 1
            call_invalid          = 2
            material_not_found    = 3
            missing_authorization = 4
            no_bom_found          = 5
            no_plant_data         = 6
            no_suitable_bom_found = 7
            conversion_error      = 8
            OTHERS                = 9.
  IF sy-subrc NE 0 OR lw_topmat-stlal NE it_bmco-altn.
    MOVE: text-083 TO it_bmco-zmsg,
          'E'      TO it_bmco-zresult,
          'C'      TO it_bmco-zmode.
    MODIFY it_bmco.
    EXIT.
  ENDIF.

  LOOP AT lt_stb WHERE idnrk EQ it_bmco-comp
                   AND suff  EQ it_bmco-suff.
    MOVE: lt_stb-knobj TO pw_knobj,
          lt_stb-stvkn TO pw_stlkn.
  ENDLOOP.
  IF sy-subrc NE 0 OR lw_topmat-stlal NE it_bmco-altn.
    MOVE: text-086 TO it_bmco-zmsg,
          'E'      TO it_bmco-zresult,
          'C'      TO it_bmco-zmode.
    MODIFY it_bmco.
  ENDIF.
ENDFORM.                    " read_bom_info
*&---------------------------------------------------------------------*
*&      Form  get_stlkn_for_od
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_STLKN  text
*----------------------------------------------------------------------*
FORM get_stlkn_for_od USING pw_stlkn.
  DATA: l_zsequ(4) TYPE n.

  SELECT SINGLE MAX( sequ )
       FROM mast AS a INNER JOIN stpo AS b
                      ON    b~stlty EQ 'M'
                      AND   a~stlnr EQ b~stlnr
       INTO  (l_zsequ)
       WHERE a~matnr EQ it_bmco-mtno
       AND   a~werks EQ it_bmco-plnt
       AND   a~stlan EQ it_bmco-usag
       AND   a~stlal EQ it_bmco-altn
       AND   b~posnr EQ it_bmco-pref
       AND   b~idnrk EQ it_bmco-comp
       AND   b~suff  EQ it_bmco-suff.
  IF l_zsequ IS INITIAL.
    MOVE: text-086 TO it_bmco-zmsg,
          'E'      TO it_bmco-zresult,
          'C'      TO it_bmco-zmode.
    MODIFY it_bmco.
    EXIT.
  ENDIF.

  SELECT SINGLE b~stlkn
       FROM mast AS a INNER JOIN stpo AS b
                      ON    b~stlty EQ 'M'
                      AND   a~stlnr EQ b~stlnr
       INTO  (pw_stlkn)
       WHERE a~matnr EQ it_bmco-mtno
       AND   a~werks EQ it_bmco-plnt
       AND   a~stlan EQ it_bmco-usag
       AND   a~stlal EQ it_bmco-altn
       AND   b~posnr EQ it_bmco-pref
       AND   b~idnrk EQ it_bmco-comp
       AND   b~suff  EQ it_bmco-suff
       AND   b~sequ  EQ l_zsequ.
  IF sy-subrc NE 0.
    MOVE: text-086 TO it_bmco-zmsg,
          'E'      TO it_bmco-zresult,
          'C'      TO it_bmco-zmode.
    MODIFY it_bmco.
    EXIT.
  ENDIF.
ENDFORM.                    " get_stlkn_for_od
