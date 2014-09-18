*----------------------------------------------------------------------*
*   INCLUDE ZIPP305L_FSC_219_DATA_BDC_F01                              *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.
  REFRESH: it_bdc, it_mess.
  CLEAR:   it_bdc, it_mess.
ENDFORM.                               " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM screen_modify.
  LOOP AT SCREEN.
*    IF SCREEN-NAME  EQ 'P_RDO1'.
    IF p_rdo1 EQ 'X'.
      CASE screen-name.
        WHEN 'P_FILETY' OR '%_P_FILE_%_APP_%-TEXT'
          OR 'P_FILE'   OR '%_P_FILETY_%_APP_%-TEXT'.
          screen-active = 0.
        WHEN 'P_TCODE'.
          screen-input = 0.
      ENDCASE.
*    ELSEIF SCREEN-NAME  EQ 'P_RDO2'.  "EXCEL DATA
    ELSEIF p_rdo2 EQ 'X'.  "EXCEL DATA
      CASE screen-name.
        WHEN 'P_ZEDAT' OR '%_P_ZEDAT_%_APP_%-TEXT'
          OR 'P_ZBTIM' OR '%_P_ZBTIM_%_APP_%-TEXT'.
          screen-active = 0.
        WHEN 'P_FILETY' OR 'P_TCODE'.
          screen-input = 0.
      ENDCASE.
    ENDIF.
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
      CONCATENATE p_file text-001
                  INTO l_text.
      WRITE: / l_text.
      SKIP.
    WHEN 2.
      MESSAGE e000 WITH text-002.
    WHEN 3.
      MESSAGE e000 WITH text-003.
    WHEN OTHERS.
      MESSAGE e000 WITH text-004.
  ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM bdc_process.
  DATA: l_tabix TYPE sy-tabix,
        l_bmeng(20),
        l_datum(10).
  data: l_ver(3) type n.
  DATA l_msg  LIKE cfgnl-msglin.
  DATA: BEGIN OF la_opt OCCURS 0.
          INCLUDE STRUCTURE ctu_params.
  DATA: END OF la_opt.
  la_opt-defsize = 'X'.
  la_opt-dismode = 'N'.
  la_opt-updmode = 'S'.
* DATA TOTAL LINES
  DESCRIBE TABLE it_aalc LINES wa_line_idx.
  DATA l_len TYPE i.
  LOOP AT it_aalc.
    l_tabix = sy-tabix.
*    L_LEN = STRLEN( IT_AALC-ALCD ).
*    WRITE: / L_LEN COLOR 5.
*    PERFORM REPLACE_CA_CHANGE CHANGING IT_AALC-ALCD.
    WRITE: it_aalc-bqty TO l_bmeng,
           c_datum      TO l_datum.
    SHIFT l_bmeng LEFT DELETING LEADING space.
    PERFORM dynpro USING:
       'X' 'SAPLCSDI'    '0100',
       ' ' 'RC29N-MATNR' it_aalc-mtno,   "
       ' ' 'RC29N-WERKS' it_aalc-plnt,   "
       ' ' 'RC29N-STLAN' it_aalc-usag,   "
       ' ' 'RC29N-STLAL' it_aalc-altn,   "
       ' ' 'RC29N-DATUV' l_datum,        "
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLCSDI'    '0150',
       ' ' 'BDC_OKCODE'  '=KALL',

       'X' 'SAPLCSDI'    '2110',
       ' ' 'RC29K-BMENG' l_bmeng,   "
       ' ' 'RC29K-STLST' it_aalc-stat,
       ' ' 'BDC_OKCODE'  '=LTKO',

       'X' 'SAPLSTXX'    '1100'.
    IF NOT it_aalc-alcd IS INITIAL.
      PERFORM do_text_change.
    ENDIF.
    PERFORM dynpro USING:
       ' ' 'BDC_OKCODE'  '=TXBA',

       'X' 'SAPLCSDI'    '2110',
       ' ' 'BDC_OKCODE'  '=FCBU'.


    CALL TRANSACTION 'CS02'  USING it_bdc
                    OPTIONS FROM la_opt
                    MESSAGES INTO it_mess.
    it_aalc-zresult = sy-msgty.
    it_aalc-zmode = 'C'.
    PERFORM rkc_msg_string CHANGING l_msg.
    it_aalc-zmsg = l_msg.
*   MODIFY IT_AALC
    PERFORM it_aalc_modify USING l_msg
                                 sy-msgty
                                 l_tabix.
    REFRESH: it_bdc, it_mess.
*-----start wskim 02/11/2005
*insert or update AT CBO Table ZTPP_PMT03BB
    DATA : field(20),
           c_num(3) TYPE n,
           f_num(3) TYPE n.
    CLEAR :c_num,f_num.
    FIELD-SYMBOLS : <value> TYPE ANY.
    MOVE : it_aalc-mtno+6(2)  TO ztpp_pmt03bb-mdlg,
           it_aalc-mtno+1(5)  TO ztpp_pmt03bb-dist,
           it_aalc-mtno+6(7)  TO ztpp_pmt03bb-bmdl,
           it_aalc-mtno+14(4) TO ztpp_pmt03bb-ocnn,
           it_aalc-altn       TO l_ver ,
           l_ver              to ztpp_pmt03bb-vers,
           sy-datum           TO ztpp_pmt03bb-rdat.
    MOVE : it_aalc-alcd(1)    TO ztpp_pmt03bb-ospc001.
    f_num = 1.
    DO 218 TIMES.
      c_num = c_num + 1.f_num = f_num + 1.
      CONCATENATE 'ZTPP_PMT03BB-OSPC' f_num INTO field.
      ASSIGN (field) TO <value>.
      MOVE :  it_aalc-alcd+c_num(1) TO <value>.
    ENDDO.
    MOVE : it_aalc-mtno(1)  TO ztpp_pmt03bb-flag.

*   add by chris requested by MR. choi
*   confirmed by Kevin Able
    data: l_MDLG  like ztpp_pmt03bb-MDLG.
    select single MDLG into l_MDLG
      from ztpp_pmt03bb
      where MDLG  = ztpp_pmt03bb-MDLG
       and  dist  = ztpp_pmt03bb-dist
       and  bmdl  = ztpp_pmt03bb-bmdl
       and  ocnn  = ztpp_pmt03bb-ocnn
       and  vers  = ztpp_pmt03bb-vers.

    if sy-subrc ne 0.
      MODIFY ztpp_pmt03bb FROM  ztpp_pmt03bb.
    endif.
*-----end
  ENDLOOP.
ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  IT_AALC_MODIFY
*&---------------------------------------------------------------------*
FORM it_aalc_modify USING p_msg
                          p_msgty
                          p_tabix.

  CASE p_msgty.
    WHEN 'S'.
      it_aalc-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
      it_aalc-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
      it_aalc-zbnam = sy-uname.  "BDC User ID
*     C: Create U: Update D: Delete
      it_aalc-zmode = 'C'.       "C:CREATE
    WHEN 'E'.
      it_aalc-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
*      IT_AALC-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      it_aalc-zbnam = sy-uname.  "BDC User ID
      it_aalc-zmode = 'C'.       "C:CREATE
    WHEN OTHERS.
      it_aalc-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
      it_aalc-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
      it_aalc-zbnam = sy-uname.  "BDC User ID
      it_aalc-zmode = 'C'.       "C:CREATE
  ENDCASE.
* IT_AALC MODIFY
  MODIFY it_aalc INDEX p_tabix TRANSPORTING zbdat
                                            zbtim
                                            zbnam
                                            zmode
                                            zresult
                                            zmsg.
ENDFORM.                    " IT_AALC_MODIFY
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS1
*&---------------------------------------------------------------------*
FORM bdc_process1.
  DATA: l_tabix TYPE sy-tabix,
        l_datum(10).
  DATA: BEGIN OF la_opt OCCURS 0.
          INCLUDE STRUCTURE ctu_params.
  DATA: END OF la_opt.
  la_opt-defsize = 'X'.
  la_opt-dismode = 'N'.
  la_opt-updmode = 'S'.
* DATA TOTAL LINES
  DESCRIBE TABLE it_excl LINES wa_line_idx.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / 'FSC 219 CODE Upload Total : ', wa_line_idx.
  FORMAT COLOR OFF.
  LOOP AT it_excl.
    l_tabix = sy-tabix.
    WRITE: c_datum TO l_datum.
    PERFORM dynpro USING:
       'X' 'SAPLCSDI'    '0100',
       ' ' 'RC29N-MATNR' it_excl-matnr,   "
       ' ' 'RC29N-WERKS' it_excl-werks,  "
       ' ' 'RC29N-STLAN' it_excl-stlan,  "
       ' ' 'RC29N-STLAL' it_excl-stlal,  "
       ' ' 'RC29N-DATUV' l_datum,        "
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLCSDI'    '0150',
       ' ' 'BDC_OKCODE'  '=KALL',

       'X' 'SAPLCSDI'    '2110',
       ' ' 'RC29K-BMENG' it_excl-bmeng,
       ' ' 'RC29K-STLST' it_excl-stlst,
       ' ' 'BDC_OKCODE'  '=LTKO',

       'X' 'SAPLSTXX'    '1100',
       ' ' 'RSTXT-TXLINE(02)' it_excl-zalcd(40),
       ' ' 'RSTXT-TXLINE(03)' it_excl-zalcd+40(40),
       ' ' 'RSTXT-TXLINE(04)' it_excl-zalcd+80(40),
       ' ' 'RSTXT-TXLINE(05)' it_excl-zalcd+120(40),
       ' ' 'RSTXT-TXLINE(06)' it_excl-zalcd+160(40),
       ' ' 'RSTXT-TXLINE(07)' it_excl-zalcd+200(19),
       ' ' 'BDC_OKCODE'  '=TXBA',

       'X' 'SAPLCSDI'    '2110',
       ' ' 'BDC_OKCODE'  '=FCBU'.


    CALL TRANSACTION 'CS02'  USING it_bdc
                    OPTIONS FROM la_opt
                    MESSAGES INTO it_mess.
    it_excl-msgty = sy-msgty.
    PERFORM rkc_msg_string CHANGING it_excl-messg.
*     MODIFY IT_EXCL
    MODIFY it_excl INDEX l_tabix TRANSPORTING msgty
                                              messg.
    REFRESH: it_bdc, it_mess.
  ENDLOOP.
ENDFORM.                    " BDC_PROCESS1
*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
FORM rkc_msg_string CHANGING p_msg.
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
            msg_lin = p_msg
       EXCEPTIONS
            OTHERS  = 1.
ENDFORM.                    " RKC_MSG_STRING
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
ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM write_process.
  DATA l_index TYPE sy-index.

  CLEAR: wa_erro_idx, wa_line_idx.
  DESCRIBE TABLE it_aalc LINES wa_line_idx.
  LOOP AT it_aalc WHERE zresult EQ 'E'.
    wa_erro_idx = wa_erro_idx + 1.
  ENDLOOP.
  wa_line_idx = wa_line_idx - wa_erro_idx.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / text-005, wa_line_idx.
  WRITE: / text-006, wa_erro_idx.
  FORMAT COLOR OFF.
  IF wa_erro_idx GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / text-006.
    FORMAT COLOR OFF.
    WRITE: /(18)  text-007,
            (05)  text-008,
            (10)  text-009,
            (10)  text-010,
            (12)  text-011,
            (15)  text-012 RIGHT-JUSTIFIED,
            (04)  text-013,
            (06)  text-014,
            (219)  text-015,
            (40)  text-016,
            (15)  text-017,
            (10)  text-018,
                  text-019.
    LOOP AT it_aalc WHERE zresult EQ 'E'.
      WRITE: /(18) it_aalc-mtno,
              (05) it_aalc-plnt,
              (10) it_aalc-usag,
              (10) it_aalc-altn,
              (12) it_aalc-eono,
              (15) it_aalc-bqty,
              (04) it_aalc-hunt,
              (06) it_aalc-stat,
              (219) it_aalc-alcd,
              (40) it_aalc-zdesc,
              (15) it_aalc-upct,
              (10) it_aalc-zresult,
                   it_aalc-zmsg.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS1
*&---------------------------------------------------------------------*
FORM write_process1.
  DATA l_index TYPE sy-index.

  CLEAR: wa_erro_idx, wa_line_idx.
  DESCRIBE TABLE it_excl LINES wa_line_idx.
  LOOP AT it_excl WHERE msgty EQ 'E'.
    wa_erro_idx = wa_erro_idx + 1.
  ENDLOOP.
  wa_line_idx = wa_line_idx - wa_erro_idx.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / text-005, wa_line_idx.
  WRITE: / text-006, wa_erro_idx.
  FORMAT COLOR OFF.
  IF wa_erro_idx GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / text-006.
    FORMAT COLOR OFF.
*    WRITE: /(18) 'MATERIAL NUMBER',
*            (05) 'PLANT',
*            (10) 'BOM USAGE',
*            (10) 'ALTER BOM',
*            (12) 'CHANGE NO',
*            (15) 'BASE QUANTITY' RIGHT-JUSTIFIED,
*            (04) 'UNIT',
*            (06) 'STATUS',
*            (219) '219 CODE',
*            (40) 'ALTERNATIVE BOM TEXT',
*            (15) 'CONTROL TYPE',
*            (10) 'ERROR TYPE',
*                 'MESSAGE TEXT'.
    WRITE: /(18)  text-007,
            (05)  text-008,
            (10)  text-009,
            (10)  text-010,
            (12)  text-011,
            (15)  text-012 RIGHT-JUSTIFIED,
            (04)  text-013,
            (06)  text-014,
            (219)  text-015,
            (40)  text-016,
            (15)  text-017,
            (10)  text-018,
                  text-019.
    LOOP AT it_excl WHERE msgty EQ 'E'.
      WRITE: /(18) it_excl-matnr,
              (05) it_excl-werks,
              (10) it_excl-stlan,
              (10) it_excl-stlal,
              (12) it_excl-aennr,
              (15) it_excl-bmeng,
              (04) it_excl-bmein,
              (06) it_excl-stlst,
              (219) it_excl-zalcd,
              (40) it_excl-zalcd,
              (15) it_excl-zupct,
              (10) it_excl-msgty,
                   it_excl-messg.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS1
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM read_process.
*  DATA: L_DATUM TYPE SY-DATUM,
*        L_ZBTIM TYPE SY-UZEIT.
*  L_DATUM = SY-DATUM.
  SELECT *
       FROM ztbm_abxalcdt
       INTO TABLE it_aalc
       WHERE zedat EQ p_zedat
       AND   zbtim EQ p_zbtim.
  IF sy-subrc NE 0.
    WRITE: / text-020.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM data_process.
  DATA: BEGIN OF lt_marc OCCURS 0,
          matnr TYPE marc-matnr,
          werks TYPE marc-werks,
        END OF lt_marc,
        mt_marc LIKE lt_marc OCCURS 0 WITH HEADER LINE,
        lt_aalc LIKE it_aalc OCCURS 0 WITH HEADER LINE,
        l_tabix TYPE sy-tabix.
  DESCRIBE TABLE it_aalc LINES wa_line_idx.

  LOOP AT it_aalc.
    lt_marc-matnr = it_aalc-mtno.
    lt_marc-werks = it_aalc-plnt.
    COLLECT lt_marc.
    CLEAR: it_aalc, lt_marc.
  ENDLOOP.
  IF NOT lt_marc[] IS INITIAL.
    SELECT matnr
           werks
         FROM marc
         INTO TABLE mt_marc
         FOR ALL ENTRIES IN lt_marc
         WHERE matnr EQ lt_marc-matnr
         AND   werks EQ lt_marc-werks.
    IF sy-subrc EQ 0.
*     SORTING
      SORT mt_marc BY matnr werks.
      LOOP AT it_aalc.
        l_tabix = sy-tabix.
        READ TABLE mt_marc WITH KEY matnr = it_aalc-mtno
                                    werks = it_aalc-plnt
                           BINARY SEARCH.
        IF sy-subrc NE 0.
          lt_aalc = it_aalc.
          lt_aalc-zbdat = sy-datum.
          lt_aalc-zbnam = sy-uname.
          lt_aalc-zresult = 'L'.
          lt_aalc-zmsg = 'Material number does not exist'.
          DELETE it_aalc INDEX l_tabix.
          APPEND lt_aalc.
        ENDIF.
        CLEAR: it_aalc, lt_aalc, mt_marc.
      ENDLOOP.
    ELSE.
*     ALL ERROR
      LOOP AT it_aalc.
        l_tabix = sy-tabix.
        lt_aalc = it_aalc.
        lt_aalc-zbdat = sy-datum.
        lt_aalc-zbnam = sy-uname.
        lt_aalc-zresult = 'L'.
        lt_aalc-zmsg = 'Material number does not exist'.
        DELETE it_aalc INDEX l_tabix.
        APPEND lt_aalc.
      ENDLOOP.
    ENDIF.

  ENDIF.
  DESCRIBE TABLE lt_aalc LINES wa_erro_idx.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / text-021, wa_line_idx.
  WRITE: / text-022, wa_erro_idx.
  FORMAT COLOR OFF.
* Material number does not exist WRITE
  IF NOT lt_aalc[] IS INITIAL.
    WRITE: / text-022.
    PERFORM not_exist_matnrial_write TABLES lt_aalc.
    UPDATE ztbm_abxalcdt FROM TABLE lt_aalc.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS1
*&---------------------------------------------------------------------*
FORM data_process1.
  DATA: BEGIN OF lt_marc OCCURS 0,
          matnr TYPE marc-matnr,
          werks TYPE marc-werks,
        END OF lt_marc,
        mt_marc LIKE lt_marc OCCURS 0 WITH HEADER LINE,
        lt_excl LIKE it_excl OCCURS 0 WITH HEADER LINE,
        l_tabix TYPE sy-tabix.
  DESCRIBE TABLE it_excl LINES wa_line_idx.

  LOOP AT it_excl.
    lt_marc-matnr = it_excl-matnr.
    lt_marc-werks = it_excl-werks.
    COLLECT lt_marc.
    CLEAR: it_excl, lt_marc.
  ENDLOOP.
  IF NOT lt_marc[] IS INITIAL.
    SELECT matnr
           werks
         FROM marc
         INTO TABLE mt_marc
         FOR ALL ENTRIES IN lt_marc
         WHERE matnr EQ lt_marc-matnr
         AND   werks EQ lt_marc-werks.
    IF sy-subrc EQ 0.
*     SORTING
      SORT mt_marc BY matnr werks.
      LOOP AT it_excl.
        l_tabix = sy-tabix.
        READ TABLE mt_marc WITH KEY matnr = it_excl-matnr
                                    werks = it_excl-werks
                           BINARY SEARCH.
        IF sy-subrc NE 0.
          lt_excl = it_excl.
          lt_excl-msgty = 'L'.
          lt_excl-messg = 'Material number does not exist'.
          DELETE it_excl INDEX l_tabix.
          APPEND lt_excl.
        ENDIF.
        CLEAR: it_excl, lt_excl, mt_marc.
      ENDLOOP.
    ELSE.
*     ALL ERROR
      LOOP AT it_excl.
        l_tabix = sy-tabix.
        lt_excl = it_excl.
        lt_excl-msgty = 'L'.
        lt_excl-messg = 'Material number does not exist'.
        DELETE it_excl INDEX l_tabix.
        APPEND lt_excl.
      ENDLOOP.
    ENDIF.

  ENDIF.
  DESCRIBE TABLE it_excl LINES wa_erro_idx.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / text-023, wa_line_idx.
  WRITE: / text-022, wa_erro_idx.
  FORMAT COLOR OFF.
* Material number does not exist WRITE
  IF NOT lt_excl[] IS INITIAL.

    WRITE: / text-022.
    PERFORM not_exist_matnrial_write1 TABLES lt_excl.
  ENDIF.
ENDFORM.                    " DATA_PROCESS1
*&---------------------------------------------------------------------*
*&      Form  NOT_EXIST_MATNRIAL_WRITE
*&---------------------------------------------------------------------*
FORM not_exist_matnrial_write TABLES pt_aalc STRUCTURE it_aalc.
  WRITE: /(18)  text-007,
          (05)  text-008,
          (10)  text-009,
          (10)  text-010,
          (12)  text-011,
          (15)  text-012 RIGHT-JUSTIFIED,
          (04)  text-013,
          (06)  text-014,
          (219)  text-015,
          (40)  text-016,
          (15)  text-017,
          (10)  text-018,
                text-019.
  LOOP AT pt_aalc WHERE zresult EQ 'E'.
    WRITE: /(18) pt_aalc-mtno,
            (05) pt_aalc-plnt,
            (10) pt_aalc-usag,
            (10) pt_aalc-altn,
            (12) pt_aalc-eono,
            (15) pt_aalc-bqty,
            (04) pt_aalc-hunt,
            (06) pt_aalc-stat,
            (219) pt_aalc-alcd,
            (40) pt_aalc-zdesc,
            (15) pt_aalc-upct,
            (10) pt_aalc-zresult,
                 pt_aalc-zmsg.
  ENDLOOP.
ENDFORM.                    " NOT_EXIST_MATNRIAL_WRITE
*&---------------------------------------------------------------------*
*&      Form  NOT_EXIST_MATNRIAL_WRITE1
*&---------------------------------------------------------------------*
FORM not_exist_matnrial_write1 TABLES pt_excl STRUCTURE it_excl.
  WRITE: /(18)  text-007,
          (05)  text-008,
          (10)  text-009,
          (10)  text-010,
          (12)  text-011,
          (15)  text-012 RIGHT-JUSTIFIED,
          (04)  text-013,
          (06)  text-014,
          (219)  text-015,
          (40)  text-016,
          (15)  text-017,
          (10)  text-018,
                text-019.
  LOOP AT pt_excl.
    WRITE: /(18) pt_excl-matnr,
            (05) pt_excl-werks,
            (10) pt_excl-stlan,
            (10) pt_excl-stlal,
            (12) pt_excl-aennr,
            (15) pt_excl-bmeng,
            (04) pt_excl-bmein,
            (06) pt_excl-stlst,
            (219) pt_excl-zalcd,
            (40) pt_excl-zdesc,
            (15) pt_excl-zupct,
            (10) pt_excl-msgty,
                 pt_excl-messg.
  ENDLOOP.
ENDFORM.                    " NOT_EXIST_MATNRIAL_WRITE1
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PROCESS
*&---------------------------------------------------------------------*
FORM update_process.
  UPDATE ztbm_abxalcdt FROM TABLE it_aalc.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " UPDATE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  REPLACE_CA_CHANGE
*&---------------------------------------------------------------------*
FORM replace_ca_change CHANGING p_alcd.
  DATA: l_change VALUE '-',
        l_search VALUE space.
  DO.
    IF p_alcd CA ' '.
      REPLACE l_search WITH l_change
                      INTO p_alcd.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " REPLACE_CA_CHANGE
*&---------------------------------------------------------------------*
*&      Form  DO_TEXT_CHANGE
*&---------------------------------------------------------------------*
FORM do_text_change.
  DATA: l_text(40),
        l_len TYPE i,
        m_len TYPE i,
        n_len TYPE i,
        l_line(02) TYPE n,
        m_line(02) TYPE n,
        l_strlen TYPE i,
        l_field(30),
        l_fiel1(30).
  DO.
    l_line = sy-index.
    m_line = l_line + 1.
    IF sy-index EQ 1.
      l_text = it_aalc-alcd(40).
      l_strlen = strlen( l_text ).
      IF l_strlen EQ 0.
        l_strlen = 40.
      ENDIF.
      m_len = l_strlen + m_len.
      l_len = m_len.
      PERFORM concatenate_bdc_count_field USING    'RSTXT-TXLINE'
                                                   'RSTXT-TXPARGRAPH'
                                                   m_line
                                          CHANGING l_field
                                                   l_fiel1.
      PERFORM dynpro USING:
                  ' ' l_field l_text,
                  ' ' l_fiel1 '*'.
    ELSE.
      IF l_len GT 179.
        n_len = 219 - l_len.
        l_text = it_aalc-alcd+l_len(n_len).
        l_strlen = strlen( l_text ).
        m_len = l_strlen + m_len.
        PERFORM concatenate_bdc_count_field USING    'RSTXT-TXLINE'
                                                     'RSTXT-TXPARGRAPH'
                                                     m_line
                                            CHANGING l_field
                                                     l_fiel1.
        PERFORM dynpro USING:
                    ' ' l_field l_text,
                    ' ' l_fiel1 '='.
        EXIT.
      ELSE.
        l_text = it_aalc-alcd+l_len(40).
        l_strlen = strlen( l_text ).
        IF l_strlen EQ 0.
          l_strlen = 40.
        ENDIF.
        l_text = it_aalc-alcd+l_len(l_strlen).
        m_len = l_strlen + m_len.
        PERFORM concatenate_bdc_count_field USING    'RSTXT-TXLINE'
                                                     'RSTXT-TXPARGRAPH'
                                                     m_line
                                            CHANGING l_field
                                                     l_fiel1.
        l_len = m_len.
        PERFORM dynpro USING:
                    ' ' l_field l_text,
                    ' ' l_fiel1 '='.
      ENDIF.
    ENDIF.
  ENDDO.
ENDFORM.                    " DO_TEXT_CHANGE
*&---------------------------------------------------------------------*
*&      Form  CONCATENATE_BDC_COUNT_FIELD
*&---------------------------------------------------------------------*
FORM concatenate_bdc_count_field USING    p_text
                                          p_tline
                                          p_line
                                 CHANGING p_field
                                          p_fiel1.
  CONCATENATE: p_text '(' p_line ')' INTO p_field,
               p_tline '(' p_line ')' INTO p_fiel1.
ENDFORM.                    " CONCATENATE_BDC_COUNT_FIELD
