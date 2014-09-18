*----------------------------------------------------------------------*
*   INCLUDE ZEPP317L_MAT_INIT_BDC_01_F01                               *
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
        WHEN 'P_FILETY' OR 'P_TCODE' OR 'P_FILE' OR
        '%B002004_BLOCK_1000' OR '%_P_FILE_%_APP_%-TEXT' OR
        '%_P_FILETY_%_APP_%-TEXT' OR '%_P_TCODE_%_APP_%-TEXT'.
          screen-active = 0.
      ENDCASE.
*    ELSEIF SCREEN-NAME  EQ 'P_RDO2'.  "EXCEL DATA
    ELSEIF p_rdo2 EQ 'X'.  "EXCEL DATA
      CASE screen-name.
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
      CONCATENATE p_file ' DATA UPLOAD SUCCESS!!'
                  INTO l_text.
      WRITE: / l_text.
      SKIP.
    WHEN 2.
      MESSAGE e000 WITH 'FILE OPEN ERROR, FILE NO FOUND!'.
    WHEN 3.
      MESSAGE e000 WITH 'FILE READ ERROR'.
    WHEN OTHERS.
      MESSAGE e000 WITH 'FILE UPLOAD ERROR, CHECK YOUR FILE!.'.
  ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM bdc_process.
  DATA: l_tabix TYPE sy-tabix,
        l_mtart TYPE mara-mtart.
  wa_opt-defsize = 'X'.
  wa_opt-dismode = 'N'.
  wa_opt-updmode = 'S'.
* DATA TOTAL LINES
  DESCRIBE TABLE it_excl LINES wa_line_idx.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / 'Material Master Upload Total : ', wa_line_idx.
  FORMAT COLOR OFF.
  LOOP AT it_excl.
    l_tabix = sy-tabix.
    CLEAR l_mtart.
    SELECT SINGLE mtart
            FROM mara
            INTO l_mtart
            WHERE matnr EQ it_excl-matnr.
    IF sy-subrc EQ 0.
      SELECT SINGLE *
                  FROM marc
                  WHERE matnr EQ it_excl-matnr
                  AND   werks EQ it_excl-werks.
      IF sy-subrc EQ 0.
*       Material already maintained for this transaction/event
        PERFORM error_log_modify USING l_tabix.
      ELSE.
*   Create material that there does not exist to 'MARC' being to 'MARA'.
        IF l_mtart EQ it_excl-mtart.
          PERFORM marc_does_not_exist_material.
          PERFORM call_transaction_mm01 USING l_tabix.
        ELSE.
          PERFORM error_log_mat_type USING l_tabix.
        ENDIF.
      ENDIF.
    ELSE.
*     Create material that there does not exist to MARA.
      PERFORM mara_does_not_exist_material.
      PERFORM call_transaction_mm01 USING l_tabix.
    ENDIF.
  ENDLOOP.

  REFRESH: it_bdc, it_mess.
  LOOP AT it_excl WHERE msgty NE 'E'
                  AND   kzkfg EQ 'X'.
    PERFORM dynpro USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR'  it_excl-matnr,
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLMGMM'    '0070',
       ' ' 'MSICHTAUSW-KZSEL(02)'  'X',
       ' ' 'BDC_OKCODE'  '=ENTR',

       'X' 'SAPLMGMM'    '5004',
       ' ' 'MARA-KZKFG'  it_excl-kzkfg,
       ' ' 'BDC_OKCODE'  '=BU'.
    CALL TRANSACTION 'MM02'   USING it_bdc
                     OPTIONS  FROM wa_opt
                     MESSAGES INTO it_mess.
    REFRESH: it_bdc, it_mess.
  ENDLOOP.
ENDFORM.                    " BDC_PROCESS
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
*&      Form  FERT_SELECTION_VIEW
*&---------------------------------------------------------------------*
FORM fert_selection_view.
  DATA: l_vkorg      TYPE rmmg1-vkorg,  "Sales organization
        l_prodh      TYPE t179t-prodh,  "Product hierarchy
        l_mtpos      TYPE mvke-mtpos,   "Item category group from M/M
        l_natn_c     TYPE ztbm_abxtrtdt-natn_c,
        l_delr_c     TYPE ztbm_abxtrtdt-delr_c,
        l_blnk_cloc4 TYPE ztbm_abxtrtdt-blnk_cloc4.

  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'BDC_OKCODE'  '=SELA',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(03)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' ' ',   "
     ' ' 'BDC_OKCODE'  '/00'.

* ORGANIZATIONAL LEVELS

** Changed and Added by Tonkey on 05/20/2004.
  MOVE: it_excl-matnr+01(03) TO l_natn_c,
        it_excl-matnr+04(02) TO l_delr_c.
*
  SELECT SINGLE blnk_cloc4
    INTO l_blnk_cloc4
    FROM ztbm_abxtrtdt
    WHERE natn_c = l_natn_c AND  "Nation Code
          delr_c = l_delr_c   .  "Dealer
*
  CASE l_blnk_cloc4.
    WHEN 'D'.
      l_vkorg = 'D100'.
      l_mtpos = '0002'.
    WHEN 'E'.
      l_vkorg = 'E100'.
      l_mtpos = 'Z002'.
  ENDCASE.
*  case it_excl-matnr+1(3).
*    when 'B28'.
*      l_vkorg = 'D100'.
*    when 'B06'.
*      l_vkorg = 'E100'.
*  endcase.

**

  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_excl-werks,
     ' ' 'RMMG1-LGORT' it_excl-lgort,
     ' ' 'RMMG1-VKORG' l_vkorg,
     ' ' 'RMMG1-VTWEG' '10',
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Basic Data 1
  PERFORM read_t179t USING    it_excl-mtart
                              it_excl-matnr+6(2)
                     CHANGING l_prodh.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'        '5004',
     ' ' 'MAKT-MAKTX'      it_excl-maktx,
     ' ' 'MARA-MEINS'      it_excl-meins,
     ' ' 'MARA-MSTAE'      it_excl-mstae, "Xplant matl status
     ' ' 'MARA-SPART'      it_excl-spart, "Division
     ' ' 'MARA-MTPOS_MARA' it_excl-mtpos_mara, " EXCEL DATA
     ' ' 'MARA-BRGEW'      it_excl-brgew,
     ' ' 'MARA-GEWEI'      'KG', "it_excl-gewei,
     ' ' 'MARA-NTGEW'      it_excl-ntgew,
     ' ' 'MARA-VOLUM'      it_excl-volum,
     ' ' 'MARA-VOLEH'      it_excl-voleh,
     ' ' 'MARA-GROES'      it_excl-groes,
     ' ' 'MARA-PRDHA'      l_prodh,
     ' ' 'BDC_OKCODE'      '/00'.

  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'BDC_OKCODE'      '=SP02'.

* Basic Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARA-PROFL'  it_excl-profl,
     ' ' 'BDC_OKCODE'  '=SP04'.
* Classification
* Sales: Sales Org. Data 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARA-SPART' it_excl-spart,
     ' ' 'MVKE-DWERK' it_excl-dwerk,
*     ' ' 'BDC_OKCODE'  '=SP06', "?
     ' ' 'BDC_OKCODE'  '/00', "?

     'X' 'SAPLMGMM'    '4200',
     ' ' 'MG03STEUER-TAXKM(01)' it_excl-taxkm,   " EXCEL DATA
     ' ' 'MG03STEUER-TAXKM(02)' it_excl-taxkm,   " EXCEL DATA
     ' ' 'MG03STEUER-TAXKM(03)' it_excl-taxkm,   " EXCEL DATA
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP05'.
* Sales: Sales Org. Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MVKE-VERSG'      it_excl-versg,
     ' ' 'MVKE-KTGRM'      it_excl-ktgrm,
*     ' ' 'MARA-MTPOS_MARA' it_excl-mtpos_mara,

** Changed By Tonkey on 05/20/2004.
     ' ' 'MVKE-MTPOS'      l_mtpos,
*     ' ' 'MVKE-MTPOS'      it_excl-mtpos,  " EXCEL DATA
**
     ' ' 'MVKE-MVGR3'      it_excl-matnr+12(01),
     ' ' 'MVKE-MVGR4'      it_excl-matnr+10(01),
     ' ' 'MVKE-MVGR5'      it_excl-matnr+09(01),
     ' ' 'MVKE-PRODH'      l_prodh, "
     ' ' 'BDC_OKCODE'  '=SP06'  .
* Sales: General/Plant Data
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_excl-mtvfp,
     ' ' 'MARA-TRAGR'  it_excl-tragr,
     ' ' 'MARC-LADGR'  it_excl-ladgr,
     ' ' 'BDC_OKCODE'  '=SP12'.
* Foreign Trade: Export Data
* Sales Text
* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MARC-MEINS'  it_excl-meins1,
     ' ' 'MARC-DISMM'  it_excl-dismm,
     ' ' 'MARC-DISGR'  it_excl-disgr,
     ' ' 'MARC-DISPO'  it_excl-dispo,
     ' ' 'MARC-DISLS'  it_excl-disls,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_excl-beskz,
     ' ' 'MARC-LGPRO'  it_excl-lgpro,
     ' ' 'MARC-FHORI'  it_excl-fhori,
     ' ' 'MARC-WEBAZ'  it_excl-webaz,
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-STRGR'  it_excl-strgr,
     ' ' 'MARC-VRMOD'  it_excl-vrmod,
     ' ' 'MARC-MTVFP'  it_excl-mtvfp,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_excl-altsl,
     ' ' 'MARC-SBDKZ'  it_excl-sbdkz,
     ' ' 'MARC-SAUFT'  it_excl-sauft,
     ' ' 'MARC-SFEPR'  it_excl-sfepr,
     ' ' 'MARC-PROFIL' it_excl-profil,
     ' ' 'BDC_OKCODE'  '=SP17'.
* Forecasting
* Work Scheduling
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  it_excl-fevor,
     ' ' 'MARC-UEETO'  it_excl-ueeto,
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Warehouse Management 1
* Warehouse Management 2
* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  '7920',
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MBEW-STPRS'  '1',
     ' ' 'BDC_OKCODE'  '=BU'.

ENDFORM.                    " FERT_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  FERT_SELECTION_VIEW1
*&---------------------------------------------------------------------*
FORM fert_selection_view1.
  DATA: l_vkorg      TYPE rmmg1-vkorg,
        l_prodh      TYPE t179t-prodh,
        l_mtpos      TYPE mvke-mtpos,   "Item category group from M/M
        l_natn_c     TYPE ztbm_abxtrtdt-natn_c,
        l_delr_c     TYPE ztbm_abxtrtdt-delr_c,
        l_blnk_cloc4 TYPE ztbm_abxtrtdt-blnk_cloc4.

  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'BDC_OKCODE'  '=SELA',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(03)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' ' ',   "
     ' ' 'BDC_OKCODE'  '/00'.

* ORGANIZATIONAL LEVELS
** Changed and Added by Tonkey on 05/20/2004.
  MOVE: it_excl-matnr+01(03) TO l_natn_c,
        it_excl-matnr+04(02) TO l_delr_c.
*
  SELECT SINGLE blnk_cloc4
    INTO l_blnk_cloc4
    FROM ztbm_abxtrtdt
    WHERE natn_c = l_natn_c AND  "Nation
          delr_c = l_delr_c   .  "Dealer
*
  CASE l_blnk_cloc4.
    WHEN 'D'.
      l_vkorg = 'D100'.
      l_mtpos = '0002'.
    WHEN 'E'.
      l_vkorg = 'E100'.
      l_mtpos = 'Z002'.
  ENDCASE.
*  case it_excl-matnr+1(3).
*    when 'B28'.
*      l_vkorg = 'D100'.
*    when 'B06'.
*      l_vkorg = 'E100'.
*  endcase.

**

  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_excl-werks,
     ' ' 'RMMG1-VKORG' l_vkorg,
     ' ' 'RMMG1-VTWEG' '10',
     ' ' 'BDC_OKCODE'  '=ENTR'.
** Basic Data 1
*  PERFORM DYNPRO USING:
*     'X' 'SAPLMGMM'    '4004',
*     ' ' 'MAKT-MAKTX' IT_EXCL-MAKTX,
*     ' ' 'MARA-MEINS' IT_EXCL-MEINS,
*     ' ' 'MARA-MTPOS_MARA' IT_EXCL-MTPOS,
*     ' ' 'BDC_OKCODE'  '=SP02'.
** Basic Data 2
*  PERFORM DYNPRO USING:
*     'X' 'SAPLMGMM'    '4004',
**     ' ' 'MARA-NORMT' IT_EXCL-NORMT,
*     ' ' 'MARA-PROFL' IT_EXCL-NORMT,
*     ' ' 'BDC_OKCODE'  '=SP04'.
* Classification
* Sales: Sales Org. Data 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MAKT-MAKTX'  it_excl-maktx,
     ' ' 'MARA-SPART'  it_excl-spart,
     ' ' 'MVKE-DWERK'  it_excl-dwerk,
     ' ' 'BDC_OKCODE'  '=SP05',

     'X' 'SAPLMGMM'    '4200',
     ' ' 'MG03STEUER-TAXKM(01)' it_excl-taxkm,
     ' ' 'MG03STEUER-TAXKM(02)' it_excl-taxkm,
     ' ' 'MG03STEUER-TAXKM(03)' it_excl-taxkm,
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Sales: Sales Org. Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MVKE-VERSG'      it_excl-versg,
     ' ' 'MVKE-KTGRM'      it_excl-ktgrm,
** Changed By Tonkey on 05/20/2004.
     ' ' 'MARA-MTPOS_MARA' l_mtpos,
*     ' ' 'MARA-MTPOS_MARA' it_excl-mtpos,
     ' ' 'MVKE-MTPOS'      l_mtpos,
*     ' ' 'MVKE-MTPOS'      it_excl-mtpos,
**
     ' ' 'MVKE-MVGR3'      it_excl-matnr+12(01),
     ' ' 'MVKE-MVGR4'      it_excl-matnr+10(01),
     ' ' 'MVKE-MVGR5'      it_excl-matnr+09(01),
*     ' ' 'MVKE-PRODH'      L_PRODH, "
     ' ' 'BDC_OKCODE'  '=SP06'.
* Sales: General/Plant Data
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_excl-mtvfp,
     ' ' 'MARA-TRAGR'  it_excl-tragr,
     ' ' 'MARC-LADGR'  it_excl-ladgr,
     ' ' 'BDC_OKCODE'  '=SP12'.
* Foreign Trade: Export Data
* Sales Text
* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  it_excl-dismm,
     ' ' 'MARC-DISGR'  it_excl-disgr,
     ' ' 'MARC-DISPO'  it_excl-dispo,
     ' ' 'MARC-DISLS'  it_excl-disls,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_excl-beskz,
     ' ' 'MARC-LGPRO'  it_excl-lgpro,
     ' ' 'MARC-FHORI'  it_excl-fhori,
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-STRGR'  it_excl-strgr,
     ' ' 'MARC-VRMOD'  it_excl-vrmod,
     ' ' 'MARC-MTVFP'  it_excl-mtvfp,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_excl-altsl,
     ' ' 'MARC-SBDKZ'  it_excl-sbdkz,
     ' ' 'MARC-SAUFT'  it_excl-sauft,
     ' ' 'MARC-SFEPR'  it_excl-sfepr,
     ' ' 'BDC_OKCODE'  '=SP17'.
* Forecasting
* Work Scheduling
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  it_excl-fevor,
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Warehouse Management 1
* Warehouse Management 2
* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  '7920',
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MBEW-STPRS'  '1',
     ' ' 'BDC_OKCODE'  '=BU'.


ENDFORM.                    " FERT_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  HALB_SELECTION_VIEW
*&---------------------------------------------------------------------*
FORM halb_selection_view.
  DATA: l_prodh TYPE t179t-prodh.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "

     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_excl-werks,
     ' ' 'RMMG1-LGORT' it_excl-lgort,
     ' ' 'RMMG1-VKORG' it_excl-vkorg,
     ' ' 'RMMG1-VTWEG' it_excl-vtweg,
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Basic Data 1
  PERFORM read_t179t USING    it_excl-mtart
                              it_excl-matnr+6(2)
                     CHANGING l_prodh.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'        '5004',
     ' ' 'MAKT-MAKTX'      it_excl-maktx,
     ' ' 'MARA-MEINS'      it_excl-meins,
     ' ' 'MARA-MATKL'      it_excl-matkl,
     ' ' 'MARA-MSTAE'      it_excl-mstae, "Xplant matl status
     ' ' 'MARA-SPART'      it_excl-spart, "Division
     ' ' 'MARA-MTPOS_MARA' it_excl-mtpos_mara,
     ' ' 'MARA-BRGEW'      it_excl-brgew,
     ' ' 'MARA-GEWEI'      it_excl-gewei,
     ' ' 'MARA-NTGEW'      it_excl-ntgew,
     ' ' 'MARA-VOLUM'      it_excl-volum,
     ' ' 'MARA-VOLEH'      it_excl-voleh,
     ' ' 'MARA-GROES'      it_excl-groes,
     ' ' 'MARA-PRDHA'      l_prodh,
     ' ' 'BDC_OKCODE'      '=ENTR'.
*ADDITIONAL DATA
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'        '5004',
     ' ' 'BDC_OKCODE'      '=ZU02'.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'        '5300',
     ' ' 'SMEINH-UMREN(02)' it_excl-umren,
     ' ' 'SMEINH-MEINH(02)' it_excl-meinh,
     ' ' 'SMEINH-UMREZ(02)' it_excl-umrez,
     ' ' 'BDC_OKCODE'      '=SP02'.
* Basic Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
*     ' ' 'MARA-NORMT' IT_EXCL-NORMT,
     ' ' 'MARA-PROFL' it_excl-profl,
     ' ' 'BDC_OKCODE'  '=SP04'.

** Classification
* Sales: Sales Org. Data 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARA-SPART' it_excl-spart,
     ' ' 'MVKE-DWERK' it_excl-dwerk,
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLMGMM'    '4200',
     ' ' 'MG03STEUER-TAXKM(01)' it_excl-taxkm,   "
     ' ' 'MG03STEUER-TAXKM(02)' it_excl-taxkm,   "
     ' ' 'MG03STEUER-TAXKM(03)' it_excl-taxkm,   "
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP05'.
* Sales: Sales Org. Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MVKE-VERSG'      it_excl-versg,
     ' ' 'MVKE-KTGRM'      it_excl-ktgrm,
     ' ' 'MARA-MTPOS_MARA' it_excl-mtpos,
     ' ' 'MVKE-MTPOS'      it_excl-mtpos,  " EXCEL DATA
*     ' ' 'MVKE-MVGR3'      IT_EXCL-MATNR+12(01),
*     ' ' 'MVKE-MVGR4'      IT_EXCL-MATNR+10(01),
*     ' ' 'MVKE-MVGR5'      IT_EXCL-MATNR+09(01),
     ' ' 'MVKE-PRODH'      l_prodh, "
     ' ' 'BDC_OKCODE'  '=SP06'.
* Sales: General/Plant Data
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_excl-mtvfp,
     ' ' 'MARA-TRAGR'  it_excl-tragr,
     ' ' 'MARC-LADGR'  it_excl-ladgr,
     ' ' 'BDC_OKCODE'  '=SP12'.
* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MARC-MEINS'  it_excl-meins1,
     ' ' 'MARC-DISGR'  it_excl-disgr,
     ' ' 'MARC-DISMM'  it_excl-dismm,
     ' ' 'MARC-DISPO'  it_excl-dispo,
     ' ' 'MARC-DISLS'  it_excl-disls,
     ' ' 'MARC-BSTMI'  it_excl-bstmi,
     ' ' 'MARC-BSTMA'  it_excl-bstma,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_excl-beskz,
     ' ' 'MARC-LGPRO'  it_excl-lgpro,
     ' ' 'MARC-FHORI'  it_excl-fhori,
*    HALB
     ' ' 'MARC-SOBSL'  it_excl-sobsl,
     ' ' 'MARC-VSPVB'  it_excl-vspvb,
     ' ' 'MARC-RGEKZ'  it_excl-rgekz,
     ' ' 'MARC-EISBE'  it_excl-eisbe,
     ' ' 'MARC-WEBAZ'  it_excl-webaz,
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_excl-mtvfp,
     ' ' 'MARC-STRGR'  it_excl-strgr,
     ' ' 'MARC-VRMOD'  it_excl-vrmod,
*    HALB
     ' ' 'MARC-VINT2'  it_excl-vint2,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_excl-altsl,
     ' ' 'MARC-SBDKZ'  it_excl-sbdkz,
*     ' ' 'MARC-SAUFT'  IT_EXCL-SAUFT,
*     ' ' 'MARC-SFEPR'  IT_EXCL-SFEPR,
*     ' ' 'MARC-PROFIL' it_excl-profil,
     ' ' 'BDC_OKCODE'  '=SP17'.
* Work Scheduling
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  it_excl-fevor,
     ' ' 'MARC-UEETO'  it_excl-ueeto,
     ' ' 'MARC-UNETO'  it_excl-uneto,
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  '7900',
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'MARC-NCOST'  'X',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MBEW-STPRS'  '1',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " HALB_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  HALB_SELECTION_VIEW1
*&---------------------------------------------------------------------*
FORM halb_selection_view1.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
*     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_excl-werks,
     ' ' 'RMMG1-LGORT' it_excl-lgort,
     ' ' 'RMMG1-VKORG' it_excl-vkorg,
     ' ' 'RMMG1-VTWEG' it_excl-vtweg,
     ' ' 'BDC_OKCODE'  '=ENTR'.
** Basic Data 1
*  PERFORM DYNPRO USING:
*     'X' 'SAPLMGMM'    '4004',
*     ' ' 'MAKT-MAKTX' IT_EXCL-MAKTX,
*     ' ' 'MARA-MEINS' IT_EXCL-MEINS,
*     ' ' 'MARA-MTPOS_MARA' IT_EXCL-MTPOS,
*     ' ' 'BDC_OKCODE'  '=SP02'.
** Basic Data 2
*  PERFORM DYNPRO USING:
*     'X' 'SAPLMGMM'    '4004',
**     ' ' 'MARA-NORMT' IT_EXCL-NORMT,
*     ' ' 'MARA-PROFL' IT_EXCL-NORMT,
*     ' ' 'BDC_OKCODE'  '=SP03'.
** Classification
*  PERFORM DYNPRO USING:
*     'X' 'SAPLCLCA'    '0602',
**     ' ' 'RMCLF-KLART'  IT_EXCL-KLART,
*     ' ' 'RMCLF-KLART'  '001',
*     ' ' 'BDC_OKCODE'  '=ENTE',
*
*          'X' 'SAPLCLFM'    '0500',
**     ' ' 'RMCLF-CLASS(01)'  IT_EXCL-CLASS,
**     ' ' 'RMCLF-CLASS(01)'  'HPC_MASTER',  "CLIENT '130'
*     ' ' 'RMCLF-CLASS(01)'  'P_HPC',       "CLIENT '140'
*     ' ' 'BDC_OKCODE'  '=WEI1'.
* Sales: Sales Org. Data 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MARA-SPART' it_excl-spart,
     ' ' 'MVKE-DWERK' it_excl-dwerk,
     ' ' 'BDC_OKCODE'  '=SP05',

     'X' 'SAPLMGMM'    '4200',
     ' ' 'MG03STEUER-TAXKM(01)' it_excl-taxkm,   "
     ' ' 'MG03STEUER-TAXKM(02)' it_excl-taxkm,   "
     ' ' 'MG03STEUER-TAXKM(03)' it_excl-taxkm,   "
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP05'.
* Sales: Sales Org. Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MVKE-VERSG'      it_excl-versg,
     ' ' 'MVKE-KTGRM'      it_excl-ktgrm,
*     ' ' 'MARA-MTPOS_MARA' it_excl-mtpos,
     ' ' 'MVKE-MTPOS'      it_excl-mtpos,  " EXCEL DATA
*     ' ' 'MVKE-MVGR3'      IT_EXCL-MATNR+12(01),
*     ' ' 'MVKE-MVGR4'      IT_EXCL-MATNR+10(01),
*     ' ' 'MVKE-MVGR5'      IT_EXCL-MATNR+09(01),
*     ' ' 'MVKE-PRODH'      L_PRODH, "
     ' ' 'BDC_OKCODE'  '=SP06'.
* Sales: General/Plant Data
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_excl-mtvfp,
     ' ' 'MARA-TRAGR'  it_excl-tragr,
     ' ' 'MARC-LADGR'  it_excl-ladgr,
     ' ' 'BDC_OKCODE'  '=SP12'.
* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MAKT-MAKTX'  it_excl-maktx,
     ' ' 'MARC-DISGR'  it_excl-disgr,
     ' ' 'MARC-DISMM'  it_excl-dismm,
     ' ' 'MARC-DISPO'  it_excl-dispo,
     ' ' 'MARC-DISLS'  it_excl-disls,
     ' ' 'MARC-BSTMI'  it_excl-bstmi,
     ' ' 'MARC-BSTMA'  it_excl-bstma,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_excl-beskz,
     ' ' 'MARC-LGPRO'  it_excl-lgpro,
     ' ' 'MARC-FHORI'  it_excl-fhori,
*    HALB
     ' ' 'MARC-RGEKZ'  it_excl-rgekz,
     ' ' 'MARC-EISBE'  it_excl-eisbe,
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_excl-mtvfp,
     ' ' 'MARC-STRGR'  it_excl-strgr,
     ' ' 'MARC-VRMOD'  it_excl-vrmod,
*    HALB
     ' ' 'MARC-VINT2'  it_excl-vint2,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_excl-altsl,
     ' ' 'MARC-SBDKZ'  it_excl-sbdkz,
     ' ' 'MARC-SAUFT'  it_excl-sauft,
     ' ' 'MARC-SFEPR'  it_excl-sfepr,
     ' ' 'BDC_OKCODE'  '=SP17'.
* Work Scheduling
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  it_excl-fevor,
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  '7900',
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'MARC-NCOST'  'X',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MBEW-STPRS'  '1',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " HALB_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  HALB_PHAN_SELECTION_VIEW
*&---------------------------------------------------------------------*
FORM halb_phan_selection_view.
  DATA l_prodh TYPE t179t-prodh.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_excl-werks,
     ' ' 'RMMG1-LGORT' it_excl-lgort,
     ' ' 'BDC_OKCODE'  '=ENTR'.

* Basic Data 1
  PERFORM read_t179t USING    it_excl-mtart
                              it_excl-matnr+6(2)
                     CHANGING l_prodh.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MAKT-MAKTX'      it_excl-maktx,
     ' ' 'MARA-MEINS'      it_excl-meins,
     ' ' 'MARA-MATKL'      it_excl-matkl,
     ' ' 'MARA-MSTAE'      it_excl-mstae, "Xplant matl status
     ' ' 'MARA-SPART'      it_excl-spart1, "Division
     ' ' 'MARA-MTPOS_MARA' it_excl-mtpos_mara,
     ' ' 'MARA-BRGEW'      it_excl-brgew,
     ' ' 'MARA-GEWEI'      it_excl-gewei,
     ' ' 'MARA-NTGEW'      it_excl-ntgew,
     ' ' 'MARA-VOLUM'      it_excl-volum,
     ' ' 'MARA-VOLEH'      it_excl-voleh,
     ' ' 'MARA-GROES'      it_excl-groes,
     ' ' 'MARA-PRDHA'      l_prodh,
     ' ' 'BDC_OKCODE'      '=ENTR'.
*ADDITIONAL DATA
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'        '5004',
     ' ' 'BDC_OKCODE'      '=ZU02'.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'        '5300',
     ' ' 'SMEINH-UMREN(02)' it_excl-umren,
     ' ' 'SMEINH-MEINH(02)' it_excl-meinh,
     ' ' 'SMEINH-UMREZ(02)' it_excl-umrez,
     ' ' 'BDC_OKCODE'      '=SP02'.
* Basic Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
*     ' ' 'MARA-NORMT' IT_EXCL-NORMT,
     ' ' 'MARA-PROFL' it_excl-profl,
     ' ' 'BDC_OKCODE'  '=SP12'.
* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MARC-MEINS'  it_excl-meins1,
     ' ' 'MARC-DISGR'  it_excl-disgr,
     ' ' 'MARC-DISMM'  it_excl-dismm,
     ' ' 'MARC-DISPO'  it_excl-dispo,
     ' ' 'MARC-DISLS'  it_excl-disls,
     ' ' 'MARC-BSTMI'  it_excl-bstmi,
     ' ' 'MARC-BSTMA'  it_excl-bstma,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_excl-beskz,
     ' ' 'MARC-LGPRO'  it_excl-lgpro,
     ' ' 'MARC-FHORI'  it_excl-fhori,
*    HALB
     ' ' 'MARC-SOBSL'  it_excl-sobsl,
     ' ' 'MARC-VSPVB'  it_excl-vspvb,
     ' ' 'MARC-RGEKZ'  it_excl-rgekz,
     ' ' 'MARC-EISBE'  it_excl-eisbe,
     ' ' 'MARC-WEBAZ'  it_excl-webaz,
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_excl-mtvfp,
     ' ' 'MARC-STRGR'  it_excl-strgr,
     ' ' 'MARC-VRMOD'  it_excl-vrmod,
*    HALB
     ' ' 'MARC-VINT2'  it_excl-vint2,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_excl-altsl,
     ' ' 'MARC-SBDKZ'  it_excl-sbdkz,
     ' ' 'MARC-SAUFT'  it_excl-sauft,
     ' ' 'MARC-SFEPR'  it_excl-sfepr,
*     ' ' 'MARC-PROFIL' it_excl-profil,
     ' ' 'BDC_OKCODE'  '=SP17'.
* Work Scheduling
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  it_excl-fevor,
     ' ' 'MARC-UEETO'  it_excl-ueeto,
     ' ' 'MARC-UNETO'  it_excl-uneto,
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  '7900',
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'MARC-NCOST'  'X',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MBEW-STPRS'  '1',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " HALB_PHAN_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  HALB_PHAN_SELECTION_VIEW1
*&---------------------------------------------------------------------*
FORM halb_phan_selection_view1.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
*     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_excl-werks,
     ' ' 'RMMG1-LGORT' it_excl-lgort,
     ' ' 'BDC_OKCODE'  '=ENTR'.

** Basic Data 1
*  PERFORM DYNPRO USING:
*     'X' 'SAPLMGMM'    '4004',
*     ' ' 'MAKT-MAKTX' IT_EXCL-MAKTX,
*     ' ' 'MARA-MEINS' IT_EXCL-MEINS,
*     ' ' 'MARA-MTPOS_MARA' IT_EXCL-MTPOS,
*     ' ' 'BDC_OKCODE'  '=SP02'.
** Basic Data 2
*  PERFORM DYNPRO USING:
*     'X' 'SAPLMGMM'    '4004',
**     ' ' 'MARA-NORMT' IT_EXCL-NORMT,
*     ' ' 'MARA-PROFL' IT_EXCL-NORMT,
*     ' ' 'BDC_OKCODE'  '=SP03'.
** Classification
*  PERFORM DYNPRO USING:
*     'X' 'SAPLCLCA'    '0602',
**     ' ' 'RMCLF-KLART'  IT_EXCL-KLART,
*     ' ' 'RMCLF-KLART'  '001',
*     ' ' 'BDC_OKCODE'  '=ENTE',
*
*          'X' 'SAPLCLFM'    '0500',
**     ' ' 'RMCLF-CLASS(01)'  IT_EXCL-CLASS,
**     ' ' 'RMCLF-CLASS(01)'  'HPC_MASTER',  "CLIENT '130'
*     ' ' 'RMCLF-CLASS(01)'  'P_HPC',       "CLIENT '140'
*     ' ' 'BDC_OKCODE'  '=WEI1'.
* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MAKT-MAKTX'  it_excl-maktx,
     ' ' 'MARC-DISGR'  it_excl-disgr,
     ' ' 'MARC-DISMM'  it_excl-dismm,
     ' ' 'MARC-DISPO'  it_excl-dispo,
     ' ' 'MARC-DISLS'  it_excl-disls,
     ' ' 'MARC-BSTMI'  it_excl-bstmi,
     ' ' 'MARC-BSTMA'  it_excl-bstma,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_excl-beskz,
     ' ' 'MARC-LGPRO'  it_excl-lgpro,
     ' ' 'MARC-FHORI'  it_excl-fhori,
*    HALB
     ' ' 'MARC-RGEKZ'  it_excl-rgekz,
     ' ' 'MARC-EISBE'  it_excl-eisbe,
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_excl-mtvfp,
     ' ' 'MARC-STRGR'  it_excl-strgr,
     ' ' 'MARC-VRMOD'  it_excl-vrmod,
*    HALB
     ' ' 'MARC-VINT2'  it_excl-vint2,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_excl-altsl,
     ' ' 'MARC-SBDKZ'  it_excl-sbdkz,
     ' ' 'MARC-SAUFT'  it_excl-sauft,
     ' ' 'MARC-SFEPR'  it_excl-sfepr,
     ' ' 'MARC-PROFIL' it_excl-profil,
*     ' ' 'BDC_OKCODE'  '/00',
*     'X' 'SAPLMGMM'    '5004',
     ' ' 'BDC_OKCODE'  '=SP17'.
**Production version
*    PERFORM dynpro USING :
*      'X' 'SAPLMDIA'    '0100',
*      ' ' 'BDC_OKCODE'  '=DETA',
*      ' ' 'BDC_CURSOR'  'MKAL-VERID(01)',
*      ' ' 'MKAL-VERID(01)'        it_excl-verid,
*      ' ' 'MKAL-TEXT1(01)'        it_excl-text1,
*      ' ' 'MKAL-ADATU(01)'        '01/01/1900',
*      ' ' 'MKAL-BDATU(01)'        '12/31/9999',
*
*      'X' 'SAPLMDIA'    '0200',
*      ' ' 'MKAL-PLNTY'  it_excl-plnty, "Detailed planning
*      ' ' 'MKAL-PLTYG'  it_excl-pltyg, "Rate-based planning	
*      ' ' 'MKAL-PLNNG'  it_excl-plnng,  "Group	"
*      ' ' 'MKAL-ALNAG'  it_excl-alnag, "Group Counter"	
*      ' ' 'MKAL-STLAL'  it_excl-stlal,  "Alternative BOM	"
*      ' ' 'MKAL-STLAN'  it_excl-stlan,  "BOM usage"	"
*      ' ' 'MKAL-SERKZ'  it_excl-serkz,  "REM Allowed"	"
*      ' ' 'MKAL-MDV01'  it_excl-mdv01, "Production  line"	
*      ' ' 'MKAL-ELPRO'  it_excl-elpro, "Issue stor. location
*      ' ' 'MKAL-ALORT'  it_excl-alort,
*      ' ' 'BDC_OKCODE'  '=ENTR'.
*
*    PERFORM dynpro USING :
*      'X' 'SAPLMDIA'    '0100',
*      ' ' 'BDC_OKCODE'  '=ENTR',
*      ' ' 'BDC_CURSOR'  'RM03T-AKTZL',
*
*      'X' 'SAPLMGMM'    '5004',
*      ' ' 'BDC_OKCODE'  '=SP18'.
* Work Scheduling
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  it_excl-fevor,
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  '7900',
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'MARC-NCOST'  'X',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MBEW-STPRS'  '1',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " HALB_PHAN_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  ROH_SELECTION_VIEW
*&---------------------------------------------------------------------*
FORM roh_selection_view.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(09)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(10)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(11)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.

* ORGANIZATIONAL LEVELS
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_excl-werks,
     ' ' 'RMMG1-LGORT' it_excl-lgort,
     ' ' 'RMMG1-LGNUM' it_excl-lgnum,
     ' ' 'RMMG1-LGTYP' it_excl-lgtyp,
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Basic Data 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MAKT-MAKTX'      it_excl-maktx,
     ' ' 'MARA-MEINS'      it_excl-meins,
     ' ' 'MARA-MATKL'      it_excl-matkl,
     ' ' 'MARA-MSTAE'      it_excl-mstae, "Xplant matl status
     ' ' 'MARA-SPART'      it_excl-spart, "Division
*     ' ' 'MARA-MATKL' 'A01',  "CLIENT '130'
*     ' ' 'MARA-MATKL' 'P01',  "CLIENT '140'
     ' ' 'MARA-MTPOS_MARA' it_excl-mtpos_mara,
     ' ' 'MARA-BRGEW'      it_excl-brgew,
     ' ' 'MARA-GEWEI'      it_excl-gewei,
     ' ' 'MARA-NTGEW'      it_excl-ntgew,
     ' ' 'MARA-VOLUM'      it_excl-volum,
     ' ' 'MARA-VOLEH'      it_excl-voleh,
     ' ' 'MARA-GROES'      it_excl-groes,
     ' ' 'BDC_OKCODE'      '=ENTR'.
*Additional data
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'        '5004',
     ' ' 'BDC_OKCODE'      '=ZU02'.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'        '5300',
     ' ' 'SMEINH-UMREN(02)' it_excl-umren,
     ' ' 'SMEINH-MEINH(02)' it_excl-meinh,
     ' ' 'SMEINH-UMREZ(02)' it_excl-umrez,
     ' ' 'BDC_OKCODE'      '=SP02'.
* Basic Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
*     ' ' 'MARA-NORMT' IT_EXCL-NORMT,
     ' ' 'MARA-PROFL'      it_excl-profl,
     ' ' 'BDC_OKCODE'      '=SP09'.
** Classification
*  PERFORM DYNPRO USING:
*     'X' 'SAPLCLCA'    '0602',
**     ' ' 'RMCLF-KLART'  IT_EXCL-KLART,
*     ' ' 'RMCLF-KLART'  '001',
*     ' ' 'BDC_OKCODE'  '=ENTE',
*
*          'X' 'SAPLCLFM'    '0500',
**     ' ' 'RMCLF-CLASS(01)'  IT_EXCL-CLASS,
**     ' ' 'RMCLF-CLASS(01)'  'HPC_MASTER',  "CLIENT '130'
*     ' ' 'RMCLF-CLASS(01)'  'P_HPC',       "CLIENT '140'
*     ' ' 'BDC_OKCODE'  '=WEI1'.
* Purchasing View
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-EKGRP'  it_excl-ekgrp, "Purchasing group
     ' ' 'MARC-KAUTB'  it_excl-kautb, "Autom.PO
     ' ' 'MARA-BSTME'  it_excl-bstme,
     ' ' 'MARC-KORDB'  it_excl-kordb, "Source list
     ' ' 'MARC-FABKZ'  it_excl-fabkz, "JIT sched.
     ' ' 'MARC-MMSTA'  it_excl-mmsta,
     ' ' 'BDC_OKCODE'  '=SP10'.
* Foreign trade View
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-STAWN'  it_excl-stawn, "
     ' ' 'MARC-HERKL'  it_excl-herkl, "Autom.PO
     ' ' 'BDC_OKCODE'  '=SP12'.
* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MARC-MEINS'  it_excl-meins1,
     ' ' 'MARC-DISGR'  it_excl-disgr,
     ' ' 'MARC-DISMM'  it_excl-dismm,
     ' ' 'MARC-DISPO'  it_excl-dispo,
     ' ' 'MARC-DISLS'  it_excl-disls,
     ' ' 'MARC-BSTMI'  it_excl-bstmi,
     ' ' 'MARC-BSTMA'  it_excl-bstma,
     ' ' 'MARC-BSTRF'  it_excl-bstrf,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_excl-beskz,
     ' ' 'MARC-LGPRO'  it_excl-lgpro,
     ' ' 'MARC-FHORI'  it_excl-fhori,
     ' ' 'MARC-SOBSL'  it_excl-sobsl,
*
     ' ' 'MARC-RGEKZ'  it_excl-rgekz,
     ' ' 'MARC-EISBE'  it_excl-eisbe,
     ' ' 'MARC-VSPVB'  it_excl-vspvb,
     ' ' 'MARC-PLIFZ'  it_excl-plifz,
     ' ' 'MARC-WEBAZ'  it_excl-webaz,
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_excl-mtvfp,
     ' ' 'MARC-STRGR'  it_excl-strgr,
     ' ' 'MARC-VRMOD'  it_excl-vrmod,
*    HALB
     ' ' 'MARC-VINT2'  it_excl-vint2,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_excl-altsl,
     ' ' 'MARC-SBDKZ'  it_excl-sbdkz,
     ' ' 'MARC-SAUFT'  it_excl-sauft,
     ' ' 'MARC-SFEPR'  it_excl-sfepr,
*     ' ' 'MARC-PROFIL' it_excl-profil,
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARA-RAUBE'  it_excl-raube,
     ' ' 'MARA-TEMPB'  it_excl-tempb,
     ' ' 'MARC-ABCIN'  it_excl-abcin,
     ' ' 'MARC-CCFIX'  it_excl-ccfix,
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-XMCNG'  it_excl-xmcng,
     ' ' 'BDC_OKCODE'  '=SP21'.

* Werehouse mgmt 1
  IF it_excl-werks = 'P001' AND it_excl-lgort = 'P400'.
    PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MLGN-LTKZA'  it_excl-ltkza,
     ' ' 'MLGN-LTKZE'  it_excl-ltkze,
     ' ' 'BDC_OKCODE'  '=SP22'.
  ELSE.
    PERFORM dynpro USING:
      'X' 'SAPLMGMM'    '5000',
*    ' ' 'MLGN-LTKZA'  it_excl-ltkza,
*    ' ' 'MLGN-LTKZE'  it_excl-ltkze,
    ' ' 'BDC_OKCODE'  '=SP22'.
  ENDIF.
* Werehouse mgmt 2
  IF it_excl-werks = 'P001' AND it_excl-lgort = 'P400'.
    PERFORM dynpro USING:
      'X' 'SAPLMGMM'    '5000',
      ' ' 'MLGT-LGPLA'  it_excl-lgpla,
      ' ' 'MLGT-LPMIN'  it_excl-lpmin,
      ' ' 'MLGT-LPMIN'  it_excl-lpmin,
      ' ' 'MLGT-RDMNG'  it_excl-rdmng,
      ' ' 'BDC_OKCODE'  '=SP23'.
  ELSE.
    PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MLGT-LGPLA'  it_excl-lgpla,
*     ' ' 'MLGT-LPMIN'  it_excl-lpmin,
*     ' ' 'MLGT-LPMIN'  it_excl-lpmin,
*     ' ' 'MLGT-RDMNG'  it_excl-rdmng,
     ' ' 'BDC_OKCODE'  '=SP23'.
  ENDIF.
* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  it_excl-bklas,
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  DATA: l_zpld1 TYPE sy-datum,
        m_zpld1(10),
        l_zpld3 TYPE sy-datum,
        m_zpld3(10).

  WRITE: it_excl-zpld1 TO l_zpld1,
         l_zpld1 TO m_zpld1,
         it_excl-zpld3 TO l_zpld3,
         l_zpld3 TO m_zpld3.

  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MBEW-ZPLP1'  it_excl-zplp1,
*     ' ' 'MBEW-ZPLD1'  m_zpld1,
*     ' ' 'MBEW-ZPLP3'  it_excl-zplp3,
*     ' ' 'MBEW-ZPLD3'  m_zpld3,
     ' ' 'BDC_OKCODE'  '=BU'.


ENDFORM.                    " ROH_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  ROH_SELECTION_VIEW1
*&---------------------------------------------------------------------*
FORM roh_selection_view1.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
*     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(09)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(10)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
*     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.

* ORGANIZATIONAL LEVELS
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_excl-werks,
     ' ' 'RMMG1-LGORT' it_excl-lgort,
     ' ' 'RMMG1-LGNUM' it_excl-lgnum,
     ' ' 'RMMG1-LGTYP' it_excl-lgtyp,
     ' ' 'BDC_OKCODE'  '=ENTR'.
** Basic Data 1
*  PERFORM DYNPRO USING:
*     'X' 'SAPLMGMM'    '4004',
*     ' ' 'MAKT-MAKTX' IT_EXCL-MAKTX,
*     ' ' 'MARA-MEINS' IT_EXCL-MEINS,
**     ' ' 'MARA-MATKL' 'A01',  "CLIENT '130'
*     ' ' 'MARA-MATKL' 'P01',  "CLIENT '140'
*     ' ' 'MARA-MTPOS_MARA' IT_EXCL-MTPOS,
*     ' ' 'BDC_OKCODE'  '=SP02'.
** Basic Data 2
*  PERFORM DYNPRO USING:
*     'X' 'SAPLMGMM'    '4004',
**     ' ' 'MARA-NORMT' IT_EXCL-NORMT,
*     ' ' 'MARA-PROFL' IT_EXCL-NORMT,
*     ' ' 'BDC_OKCODE'  '=SP03'.
** Classification
*  PERFORM DYNPRO USING:
*     'X' 'SAPLCLCA'    '0602',
**     ' ' 'RMCLF-KLART'  IT_EXCL-KLART,
*     ' ' 'RMCLF-KLART'  '001',
*     ' ' 'BDC_OKCODE'  '=ENTE',
*
*          'X' 'SAPLCLFM'    '0500',
**     ' ' 'RMCLF-CLASS(01)'  IT_EXCL-CLASS,
**     ' ' 'RMCLF-CLASS(01)'  'HPC_MASTER',  "CLIENT '130'
*     ' ' 'RMCLF-CLASS(01)'  'P_HPC',       "CLIENT '140'
*     ' ' 'BDC_OKCODE'  '=WEI1'.
* Purchasing View
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-EKGRP'  it_excl-ekgrp,
     ' ' 'MARC-KAUTB'  it_excl-kautb,
     ' ' 'MARC-KORDB'  it_excl-kordb,
     ' ' 'MARC-FABKZ'  it_excl-fabkz,
     ' ' 'MARC-MMSTA'  it_excl-mmsta,
     ' ' 'BDC_OKCODE'  '=SP10'.
* Foreign trade View
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-STAWN'  it_excl-stawn, "
     ' ' 'MARC-HERKL'  it_excl-herkl, "Autom.PO
     ' ' 'BDC_OKCODE'  '=SP12'.
* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MAKT-MAKTX'  it_excl-maktx,
     ' ' 'MARC-DISGR'  it_excl-disgr,
     ' ' 'MARC-DISMM'  it_excl-dismm,
     ' ' 'MARC-DISPO'  it_excl-dispo,
     ' ' 'MARC-DISLS'  it_excl-disls,
     ' ' 'MARC-BSTMI'  it_excl-bstmi,
     ' ' 'MARC-BSTMA'  it_excl-bstma,
     ' ' 'MARC-BSTRF'  it_excl-bstrf,
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_excl-beskz,
     ' ' 'MARC-LGPRO'  it_excl-lgpro,
     ' ' 'MARC-FHORI'  it_excl-fhori,
     ' ' 'MARC-SOBSL'  it_excl-sobsl,
*    HALB
     ' ' 'MARC-RGEKZ'  it_excl-rgekz,
     ' ' 'MARC-EISBE'  it_excl-eisbe,
     ' ' 'MARC-VSPVB'  it_excl-vspvb,
     ' ' 'MARC-PLIFZ'  it_excl-plifz,
     ' ' 'MARC-WEBAZ'  it_excl-webaz,
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_excl-mtvfp,
     ' ' 'MARC-STRGR'  it_excl-strgr,
     ' ' 'MARC-VRMOD'  it_excl-vrmod,
*    HALB
     ' ' 'MARC-VINT2'  it_excl-vint2,
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_excl-altsl,
     ' ' 'MARC-SBDKZ'  it_excl-sbdkz,
     ' ' 'MARC-SAUFT'  it_excl-sauft,
     ' ' 'MARC-SFEPR'  it_excl-sfepr,
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MARD-LGPBE'  it_excl-lgpbe,
*     ' ' 'MARA-RAUBE'  it_excl-raube,
*     ' ' 'MARA-TEMPB'  it_excl-tempb,
     ' ' 'MARC-ABCIN'  it_excl-abcin,
     ' ' 'MARC-CCFIX'  it_excl-ccfix,
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-XMCNG'  it_excl-xmcng,
     ' ' 'BDC_OKCODE'  '=SP23'.
** Werehouse mgmt 1
*  PERFORM dynpro USING:
*     'X' 'SAPLMGMM'    '5000',
*     ' ' 'BDC_OKCODE'  '=SP23'.
*
** Werehouse mgmt 2
*  PERFORM dynpro USING:
*     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MLGT-LGPLA'  it_excl-lgpla,
*     ' ' 'MLGT-LPMIN'  it_excl-lpmin,
*     ' ' 'MLGT-LPMIN'  it_excl-lpmin,
*     ' ' 'MLGT-RDMNG'  it_excl-rdmng,
*     ' ' 'BDC_OKCODE'  '=SP24'.
* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  it_excl-bklas,
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  '1',
*     ' ' 'MBEW-VPRSV'  'S',
*     ' ' 'MBEW-PEINH'  '1',
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  'X',
     ' ' 'MARC-LOSGR'  '1',
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  DATA: l_zpld1 TYPE sy-datum,
        m_zpld1(10),
        l_zpld3 TYPE sy-datum,
        m_zpld3(10).

  WRITE: it_excl-zpld1 TO l_zpld1,
         l_zpld1 TO m_zpld1,
         it_excl-zpld3 TO l_zpld3,
         l_zpld3 TO m_zpld3.

  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MBEW-ZPLP1'  it_excl-zplp1,
*     ' ' 'MBEW-ZPLD1'  m_zpld1,
*     ' ' 'MBEW-ZPLP3'  it_excl-zplp3,
*     ' ' 'MBEW-ZPLD3'  m_zpld3,
     ' ' 'BDC_OKCODE'  '=BU'.

ENDFORM.                    " ROH_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM write_process.
  DATA: BEGIN OF lt_marc OCCURS 0,
          matnr TYPE marc-matnr,
          werks TYPE marc-werks,
        END OF lt_marc.

  DATA: l_index TYPE sy-index,
        l_tabix TYPE sy-tabix.

  CLEAR: wa_erro_idx, wa_line_idx.
  DESCRIBE TABLE it_excl LINES wa_line_idx.
  IF NOT it_excl[] IS INITIAL.
    SELECT matnr
           werks
         FROM marc
         INTO TABLE lt_marc
         FOR ALL ENTRIES IN it_excl
         WHERE matnr EQ it_excl-matnr
         AND   werks EQ it_excl-werks.
*     SORTING
    SORT lt_marc BY matnr werks.

    LOOP AT it_excl.
      l_tabix = sy-tabix.
      IF it_excl-msgty EQ 'E'.
        wa_erro_idx = wa_erro_idx + 1.
      ELSE.
        READ TABLE lt_marc WITH KEY matnr = it_excl-matnr
                                    werks = it_excl-werks
                           BINARY SEARCH.
        IF sy-subrc NE 0.
          wa_erro_idx = wa_erro_idx + 1.
          it_excl-msgty = 'E'.
          MODIFY it_excl INDEX l_tabix TRANSPORTING msgty.
        ENDIF.
      ENDIF.
      CLEAR: it_excl, lt_marc.
    ENDLOOP.
  ENDIF.

  wa_line_idx = wa_line_idx - wa_erro_idx.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / 'MATERIAL MASTER EXCEL BDC SUCCESS :  ', wa_line_idx.
  WRITE: / 'MATERIAL MASTER EXCEL BDC ERROR :  ', wa_erro_idx.
  FORMAT COLOR OFF.

  IF wa_erro_idx GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / 'MATERIAL MASTER EXCEL BDC ERROR'.
    FORMAT COLOR OFF.
    WRITE: /(18) 'MATERIAL',
            (08) 'INDUSTRY',
            (04) 'TYPE',
            (06) 'PLANT',
            (40) 'DESCRIPTION',
            (03) 'U/M',
            (12) 'GENERAL ITEM',
            (18) 'SOURCE',
            (10) 'MAT/CONFIG',
            (07) 'MRPTYPE',
            (07) 'MRPCTRL',
            (07) 'LOTSIZE',
            (10) 'PROCU/TYPE',
            (10) 'SCHMARGINE',
            (11) 'AVALL.CHECK',
            (18) 'CONFIG.MATERIAL',
            (06) 'METHOD',
            (07) 'COLLECT',
            (07) 'VER.IND',
            (10) 'CLASS.TYPE',
            (10) 'CLASS.CLAS',
            (99) 'ERROR MESSAGE'.
    LOOP AT it_excl WHERE msgty EQ 'E'.
      WRITE: /(18) it_excl-matnr,
              (08) it_excl-mbrsh,
              (04) it_excl-mtart,
              (06) it_excl-werks,
              (40) it_excl-maktx,
              (03) it_excl-meins,
              (12) it_excl-mtpos,
              (18) it_excl-normt,
              (10) it_excl-kzkfg,
              (07) it_excl-dismm,
              (07) it_excl-dispo,
              (07) it_excl-disls,
              (10) it_excl-beskz,
              (10) it_excl-fhori,
              (11) it_excl-mtvfp,
*              (18) IT_EXCL-SATNR,
              (06) it_excl-altsl,
              (07) it_excl-sbdkz,
              (07) it_excl-verkz,
*              (10) IT_EXCL-KLART,
*              (10) IT_EXCL-CLASS,
              (99) it_excl-messg.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  MARA_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
FORM mara_does_not_exist_material.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0060',
     ' ' 'RMMG1-MATNR' it_excl-matnr,   "
     ' ' 'RMMG1-MBRSH' it_excl-mbrsh,   "
     ' ' 'RMMG1-MTART' it_excl-mtart,   "
     ' ' 'BDC_OKCODE'  '/00'.
* MATERIAL TYPE
  CASE it_excl-mtart.
    WHEN 'FERT'.
*     Industry Standard Description
      PERFORM fert_selection_view.
    WHEN 'HALB'.
      IF NOT it_excl-vkorg IS INITIAL.
*       SELECTION VIEW( Sales VIEW CHOICE )
        PERFORM halb_selection_view.
*       PHANTOM
      ELSE.
*       SELECTION VIEW( Sales VIEW NOT CHOICE )
        PERFORM halb_phan_selection_view.
      ENDIF.

    WHEN 'ROH'.
*     SELECTION VIEW( ROH : BDC TAB )
      PERFORM roh_selection_view.
  ENDCASE.
ENDFORM.                    " MARA_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  ERROR_LOG_MODIFY
*&---------------------------------------------------------------------*
FORM error_log_modify USING p_tabix.
  it_excl-msgty = 'E'.
  it_excl-messg =
  'Material already maintained for this transaction/event'.
* MODIFY IT_EXCL
  MODIFY it_excl INDEX p_tabix TRANSPORTING msgty
                                            messg.
ENDFORM.                    " ERROR_LOG_MODIFY
*&---------------------------------------------------------------------*
*&      Form  ERROR_LOG_MAT_TYPE
*&---------------------------------------------------------------------*
FORM error_log_mat_type USING p_tabix.
  it_excl-msgty = 'E'.
  it_excl-messg = 'MATERIAL TYPE is Mismatch'.
* MODIFY IT_EXCL
  MODIFY it_excl INDEX p_tabix TRANSPORTING msgty
                                            messg.
ENDFORM.                    " ERROR_LOG_MAT_TYPE
*&---------------------------------------------------------------------*
*&      Form  MARC_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
FORM marc_does_not_exist_material.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0060',
     ' ' 'RMMG1-MATNR' it_excl-matnr,   "
     ' ' 'RMMG1-MBRSH' it_excl-mbrsh,   "
     ' ' 'RMMG1-MTART' it_excl-mtart,   "
     ' ' 'BDC_OKCODE'  '/00'.
* MATERIAL TYPE
  CASE it_excl-mtart.
    WHEN 'FERT'.
*     Industry Standard Description
      PERFORM fert_selection_view1.
    WHEN 'HALB'.
*     Sales org.
      IF NOT it_excl-vkorg IS INITIAL.
*       SELECTION VIEW( Sales VIEW  CHOICE )
        PERFORM halb_selection_view1.
*       PHANTOM
      ELSE.
*       SELECTION VIEW( NOT Sales VIEW CHOICE )
        PERFORM halb_phan_selection_view1.
      ENDIF.

    WHEN 'ROH'.
*     SELECTION VIEW( ROH : BDC TAB )
      PERFORM roh_selection_view1.
  ENDCASE.
ENDFORM.                    " MARC_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_MM01
*&---------------------------------------------------------------------*
FORM call_transaction_mm01 USING    p_tabix.
  CALL TRANSACTION 'MM01'  USING it_bdc
                  OPTIONS FROM wa_opt
                  MESSAGES INTO it_mess.
  it_excl-msgty = sy-msgty.
  PERFORM rkc_msg_string CHANGING it_excl-messg.
*     MODIFY IT_EXCL
  MODIFY it_excl INDEX p_tabix TRANSPORTING msgty
                                            messg.
  REFRESH: it_bdc, it_mess.
ENDFORM.                    " CALL_TRANSACTION_MM01
*&---------------------------------------------------------------------*
*&      Form  READ_T179T
*&---------------------------------------------------------------------*
FORM read_t179t USING    p_mtart
                         p_carty
                CHANGING p_prodh.
  DATA: l_prodh TYPE t179t-prodh,
        l_vtext TYPE t179t-vtext.
  CASE p_mtart.
    WHEN 'FERT'.
      CONCATENATE p_mtart '%' INTO l_vtext.
      SELECT SINGLE prodh
                  FROM t179t
                  INTO l_prodh
                  WHERE vtext LIKE l_vtext.
      CLEAR l_vtext.

      CONCATENATE l_prodh '%' INTO l_prodh.
      CONCATENATE p_carty '%' INTO l_vtext.
      SELECT SINGLE prodh
           FROM t179t
           INTO p_prodh
           WHERE prodh LIKE l_prodh
           AND   vtext LIKE l_vtext.
    WHEN 'HALB'.
      CONCATENATE p_mtart '%' INTO l_vtext.
      SELECT SINGLE prodh
                  FROM t179t
                  INTO p_prodh
                  WHERE vtext LIKE l_vtext.
      CLEAR l_vtext.
  ENDCASE.
ENDFORM.                    " READ_T179T
