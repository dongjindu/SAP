************************************************************************
* Program Name      : ZEPP311E_COSTING_BOM
* Author            : Bongsoo, Kim
* Creation Date     : 2004.01.27.
* Specifications By : Bongsoo, Kim
*                     Andy Choi
* Development Request No : UD1K906456
* Addl Documentation:
* Description       : Costing BOM for annual planning
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT zepp311e_costing_bom_01
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID zmbm.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: pbim,
        ztco_ebusplanbom,
        mara,
        marc,
        mast,
        aenr,
        stpo.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: BEGIN OF it_cbom OCCURS 0,
        gjahr     TYPE ztco_ebusplanbom-gjahr,    "Fiscal year
        bomtype   TYPE ztco_ebusplanbom-bomtype,  "Plan Type
        matnr     TYPE ztco_ebusplanbom-matnr,    "Material number
        werks     TYPE ztco_ebusplanbom-werks,    "Plant
        datuv     TYPE ztco_ebusplanbom-datuv,    "Valid-from date
        matnr_chg TYPE ztco_ebusplanbom-matnr_chg, "Material number
        chk_exist TYPE ztco_ebusplanbom-chk_exist, "Check for Existance
     END OF it_cbom.
DATA: BEGIN OF it_bdcl OCCURS 0,
        matnr     TYPE ztco_ebusplanbom-matnr,    "Material number
        werks     TYPE ztco_ebusplanbom-werks,    "Plant
        matnr_chg TYPE ztco_ebusplanbom-matnr_chg, "Material number
        datuv     TYPE ztco_ebusplanbom-datuv,    "Valid-from date
        fdatu TYPE sy-datum,
        tdatu TYPE sy-datum,
        stlan TYPE mast-stlan,
        stlal TYPE mast-stlal,
        statu,
        msgty TYPE sy-msgty,
        zmsg  LIKE cfgnl-msglin,
      END OF it_bdcl.
DATA: BEGIN OF it_list OCCURS 0,
        zmode(02),
        matnr TYPE pbim-matnr,
        edatu TYPE sy-datum,
        werks TYPE pbim-werks,
        cmatn TYPE mara-matnr,
        nmatn TYPE mara-matnr,
        fdatu TYPE sy-datum,
        tdatu TYPE sy-datum,
        cwerk TYPE t001w-werks,
        stlan TYPE mast-stlan,
        stlal TYPE mast-stlal,
        statu,
      END OF it_list.
DATA: BEGIN OF it_boml OCCURS 0,
        matnr     TYPE ztco_ebusplanbom-matnr,    "Material number
        werks     TYPE ztco_ebusplanbom-werks,    "Plant
        matnr_chg TYPE ztco_ebusplanbom-matnr_chg, "Material number
        datuv     TYPE ztco_ebusplanbom-datuv,    "Valid-from date
        fdatu TYPE sy-datum,
        tdatu TYPE sy-datum,
        stlan TYPE mast-stlan,
        stlal TYPE mast-stlal,
        statu,
        msgty TYPE sy-msgty,
        zmsg  LIKE cfgnl-msglin,
      END OF it_boml.

*----------------------------------------------------------------------*
* DATAS
*----------------------------------------------------------------------*
DATA: wa_aennr_sta TYPE aenr-aennr,
      wa_aennr_end TYPE aenr-aennr,
      wa_chk,
      wa_lines TYPE i,
      wa_error TYPE i,
      wa_succe TYPE i,
      wa_stla6 TYPE mast-stlan VALUE '6',
      wa_stla1 TYPE mast-stlan VALUE '1'.
*----------------------------------------------------------------------*
* BDC-DATA
*----------------------------------------------------------------------*
DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_mess.

DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
PARAMETERS: p_rdo1 RADIOBUTTON GROUP r1 DEFAULT 'X'
                                   USER-COMMAND ucom,
            p_rdo2 RADIOBUTTON GROUP r1.
PARAMETERS: p_gjahr LIKE ztco_ebusplanbom-gjahr.
PARAMETERS: p_btype LIKE ztco_ebusplanbom-bomtype DEFAULT 'P'.
*PARAMETERS: P_DATUV LIKE ZTCO_EBUSPLANBOM-DATUV DEFAULT SY-DATUM.
SELECT-OPTIONS: s_matnr FOR ztco_ebusplanbom-matnr.
SELECTION-SCREEN END   OF BLOCK b2.

* TABLE SELECTION
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS: p_tcode LIKE tstc-tcode DEFAULT 'CS01'.
SELECTION-SCREEN END   OF BLOCK b1.
PARAMETERS: p_mod      TYPE ctu_mode DEFAULT 'N',
            p_donly    AS CHECKBOX,
            p_log      as checkbox.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.
*
*AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION_SCREEN.
*
START-OF-SELECTION.
  wa_opt-dismode = p_mod.

  PERFORM change_number_concatenate CHANGING wa_chk.
  PERFORM chcek_mast_stpo_usage CHANGING wa_chk.
  PERFORM read_process.
  PERFORM data_process.
  IF p_rdo1 EQ 'X'.
*   DISPLAY MODE
    PERFORM write_process.
  ELSE.
    IF wa_chk EQ 'X'.
      WRITE: / text-011 COLOR 6.
    ELSE.
*   COSTING BOM BDC
      PERFORM bom_usage_6_delete.
      IF p_donly = space.
        PERFORM bom_process.
        PERFORM bom_write_process.
      ENDIF.
    ENDIF.
  ENDIF.

END-OF-SELECTION.

TOP-OF-PAGE.
  PERFORM top_of_page.

END-OF-PAGE.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.
* BDC MODE, DEFAULT SIZE, UPDATE MODE
  wa_opt-defsize = 'X'.
  wa_opt-dismode = 'N'.
  wa_opt-updmode = 'S'.
  p_gjahr = sy-datum(04) + 1.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM screen_modify.
  LOOP AT SCREEN.
*    IF SCREEN-NAME  EQ 'P_RDO1'.
    IF screen-name EQ 'P_TCODE' OR screen-name EQ 'P_BTYPE'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
    CLEAR screen.
  ENDLOOP.
ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM read_process.
  PERFORM read_ztco_ebusplanbom.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_ZTCO_EBUSPLANBOM
*&---------------------------------------------------------------------*
FORM read_ztco_ebusplanbom.
  SELECT gjahr
         bomtype
         matnr
         werks
         datuv
         matnr_chg
         chk_exist
       FROM ztco_ebusplanbom
       INTO TABLE it_cbom
       WHERE  gjahr      EQ p_gjahr
       AND    bomtype    EQ p_btype
       AND    matnr      IN s_matnr.
*       AND    DATUV      EQ P_DATUV.
  IF sy-subrc NE 0.
    WRITE: / text-003.
  ENDIF.

ENDFORM.                    " READ_ZTCO_EBUSPLANBOM
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM data_process.
* IT_CBOM-MATNR_CHG INITIAL <== IT_CBOM-MATNR
  PERFORM matnr_chg_initial_check.

  PERFORM data_check_mast_stpo.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  MATNR_CHG_INITIAL_CHECK
*&---------------------------------------------------------------------*
FORM matnr_chg_initial_check.
  DATA: l_tabix TYPE sy-tabix.
  LOOP AT it_cbom.
    l_tabix = sy-tabix.
    IF it_cbom-matnr_chg IS INITIAL.
      DELETE it_cbom INDEX l_tabix.
*      IT_CBOM-MATNR_CHG = IT_CBOM-MATNR.
*      MODIFY IT_CBOM INDEX L_TABIX TRANSPORTING MATNR_CHG.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MATNR_CHG_INITIAL_CHECK
*&---------------------------------------------------------------------*
*&      Form  DATA_CHECK_MAST_STPO
*&---------------------------------------------------------------------*
FORM data_check_mast_stpo.
  DATA: l_stlnr TYPE mast-stlnr,
        l_stlal TYPE mast-stlal,
        l_stlkn TYPE stpo-stlkn,
        l_datuv TYPE stpo-datuv,
        l_tabix TYPE sy-tabix.
  REFRESH it_list. CLEAR it_list.

  LOOP AT it_cbom.
    it_bdcl-matnr     = it_cbom-matnr.
    it_bdcl-werks     = it_cbom-werks.
    it_bdcl-matnr_chg = it_cbom-matnr_chg.
    it_bdcl-datuv     = it_cbom-datuv.
    it_bdcl-fdatu     = wa_aennr_sta(8).
    it_bdcl-tdatu     = wa_aennr_end(8).

    SELECT SINGLE      stlnr
                  MAX( stlal )
                FROM mast
                INTO (l_stlnr, l_stlal)
                WHERE matnr EQ it_bdcl-matnr
                AND   werks EQ it_bdcl-werks
                AND   stlan EQ wa_stla1
                GROUP by stlnr.
    IF sy-subrc EQ 0.
      SELECT SINGLE a~stlkn
           FROM stpo AS a INNER JOIN stas AS b
                          ON a~stlnr EQ b~stlnr
           INTO l_stlkn
           WHERE a~stlnr EQ l_stlnr
           AND   b~stlal EQ l_stlal.
*           AND   A~DATUV EQ P_FDATU.
      IF sy-subrc EQ 0.
        it_bdcl-stlan = wa_stla6.
        it_bdcl-stlal = l_stlal.
      ENDIF.
    ENDIF.
    PERFORM read_mast USING    l_datuv
                               l_stlnr
                               l_stlal
                      CHANGING it_bdcl-statu.
    APPEND it_bdcl.
    CLEAR: it_bdcl, l_stlnr, l_stlal.
  ENDLOOP.


ENDFORM.                    " DATA_CHECK_MAST_STPO
*&---------------------------------------------------------------------*
*&      Form  READ_MAST
*&---------------------------------------------------------------------*
FORM read_mast USING    p_gjahr
                        p_stlnr
                        p_stlal
               CHANGING p_statu.
  DATA l_datuv TYPE stpo-datuv.
  SELECT SINGLE datuv
          FROM mast AS a INNER JOIN stpo AS b
                         ON a~stlnr EQ b~stlnr
          INTO l_datuv
          WHERE a~matnr EQ it_bdcl-matnr
          AND   a~werks EQ it_bdcl-werks
*          AND   A~STLNR EQ P_STLNR
          AND   a~stlan EQ wa_stla6
          AND   a~stlal EQ p_stlal
          AND   b~aennr EQ wa_aennr_sta.
  IF sy-subrc EQ 0.
    p_statu = 'Y'.
  ELSE.
    p_statu = 'N'.
  ENDIF.

ENDFORM.                    " READ_MAST
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM write_process.
  SORT it_bdcl BY matnr.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  DESCRIBE TABLE it_bdcl LINES wa_lines.

  LOOP AT it_bdcl.
    WRITE: /(24) it_bdcl-matnr COLOR COL_GROUP,
            (06) it_bdcl-werks COLOR COL_GROUP,
            (20) it_bdcl-datuv,
            (20) it_bdcl-matnr_chg,
            (12) it_bdcl-fdatu,
            (12) it_bdcl-tdatu,
            (06) it_bdcl-stlan,
            (06) it_bdcl-statu.
  ENDLOOP.
  FORMAT COLOR OFF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BOM_WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM bom_write_process.
  SORT it_bdcl BY matnr.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  CLEAR: wa_error, wa_lines.
  DESCRIBE TABLE it_bdcl LINES wa_lines.
  LOOP AT it_bdcl WHERE msgty EQ 'E'.
    wa_error = wa_error + 1.
  ENDLOOP.
  wa_succe = wa_lines - wa_error.
  LOOP AT it_bdcl.
    WRITE: /(24) it_bdcl-matnr COLOR COL_GROUP,
            (06) it_bdcl-werks COLOR COL_GROUP,
            (20) it_bdcl-datuv,
            (20) it_bdcl-matnr_chg,
            (12) it_bdcl-fdatu,
            (12) it_bdcl-tdatu,
            (06) it_bdcl-stlan,
            (06) it_bdcl-statu,
            (15) it_bdcl-msgty,
            (100) it_bdcl-zmsg.
  ENDLOOP.
*  LOOP AT IT_BOML.
*    WRITE: /(24) IT_BOML-MATNR COLOR COL_GROUP,
*            (06) IT_BOML-WERKS COLOR COL_GROUP,
*            (20) IT_BOML-DATUV,
*            (20) IT_BOML-MATNR_CHG,
*            (12) IT_BOML-FDATU,
*            (12) IT_BOML-TDATU,
*            (06) IT_BOML-STLAN,
*            (06) IT_BOML-STATU,
*            (15) IT_BOML-MSGTY,
*            (100) IT_BOML-ZMSG.
*  ENDLOOP.
  FORMAT COLOR OFF.
ENDFORM.                    " BOM_WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  IF p_rdo1 EQ 'X'.
    WRITE: / text-022 COLOR 3, p_gjahr COLOR 3.
    WRITE: / text-024 , wa_lines COLOR 3.
    WRITE: /(142) sy-uline.
    WRITE: /(24)  text-012,
            (06)  text-013,
            (20)  text-014,
            (20)  text-015,
            (12)  text-016,
            (12)  text-017,
            (06)  text-018,
            (06)  text-019.
    WRITE: /(142) sy-uline.
  ELSE.
    WRITE: / text-023 COLOR 3, p_gjahr COLOR 3.
    WRITE: / text-025 , wa_lines COLOR 3.
    WRITE: / text-026 , wa_succe COLOR 3.
    WRITE: / text-027 , wa_error COLOR 3.
    WRITE: /(142) sy-uline.
    WRITE: /(24)  text-012,
            (06)  text-013,
            (20)  text-014,
            (20)  text-015,
            (12)  text-016,
            (12)  text-017,
            (06)  text-018,
            (06)  text-019,
            (15)  text-020,
            (100)  text-021.
    WRITE: /(142) sy-uline.
  ENDIF.
  FORMAT COLOR OFF.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  BOM_PROCESS
*&---------------------------------------------------------------------*
FORM bom_process.
  DATA: l_aennr(12),
        l_tabix TYPE sy-tabix,
        l_zmsg  LIKE cfgnl-msglin,
        l_msgty TYPE sy-msgty,
        l_lines TYPE i,
        l_check.
*  CONCATENATE P_GJAHR '0101-001' INTO L_AENNR.
  LOOP AT it_bdcl.
    l_tabix = sy-tabix.
    CLEAR l_check.
    PERFORM read_mast_bom_check USING   it_bdcl-matnr
                                        it_bdcl-werks
                                        it_bdcl-stlan
                                        it_bdcl-stlal
                                        wa_aennr_sta
                                CHANGING l_check.
    IF l_check EQ 'X'.
      it_bdcl-msgty = 'S'.
      CONCATENATE 'S030 BOM created for material '
                   it_bdcl-matnr INTO it_bdcl-zmsg SEPARATED BY space.
      APPEND it_boml. CLEAR it_boml.
      MODIFY it_bdcl INDEX l_tabix TRANSPORTING msgty zmsg.
    ELSE.
      REFRESH: it_bdc. CLEAR: it_bdc.
      PERFORM bom_copy_create USING    it_bdcl-matnr
                                       it_bdcl-werks
                                       it_bdcl-stlan
                                       it_bdcl-stlal
                                       it_bdcl-matnr_chg
                                       wa_aennr_sta
                                       it_bdcl-datuv.
*     CALL TRANSACTION
      CALL TRANSACTION 'CS01'  USING it_bdc
                               OPTIONS FROM wa_opt
                               MESSAGES INTO it_mess.
      l_msgty = sy-msgty.
      PERFORM rkc_msg_string CHANGING l_zmsg.
      MOVE-CORRESPONDING it_bdcl TO it_boml.
      it_bdcl-msgty = it_boml-msgty = l_msgty.
      it_bdcl-zmsg  = it_boml-zmsg  = l_zmsg.
      APPEND it_boml. CLEAR it_boml.
      MODIFY it_bdcl INDEX l_tabix TRANSPORTING msgty zmsg.
      REFRESH: it_bdc, it_mess. CLEAR: it_bdc, it_mess.

      PERFORM cs_bom_expl_mat_v2 USING    it_bdcl-matnr
                                          it_bdcl-werks
                                          wa_stla1
                                          it_bdcl-stlal
                                          it_bdcl-datuv
                                 CHANGING l_lines.

      PERFORM bom_out_point_of_time USING    it_bdcl-matnr
                                             it_bdcl-werks
                                             it_bdcl-stlal
                                             wa_aennr_end
                                             l_lines.
*     CALL TRANSACTION
      CALL TRANSACTION 'CS02'  USING it_bdc
                               OPTIONS FROM wa_opt
                               MESSAGES INTO it_mess.
      REFRESH: it_bdc, it_mess. CLEAR: it_bdc, it_mess.
    ENDIF.
    CLEAR it_bdcl.
  ENDLOOP.
ENDFORM.                    " BOM_PROCESS
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
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BOM_COPY_CREATE
*&---------------------------------------------------------------------*
FORM bom_copy_create USING    p_matnr
                              p_werks
                              p_stlan
                              p_stlal
                              p_cmatn
                              p_aennr
                              p_datuv.
  DATA l_datuv(10).
  WRITE: p_datuv TO l_datuv.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' p_matnr,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' p_werks,    "PLANT
     ' ' 'RC29N-STLAN' wa_stla6,   "BOM usage
     ' ' 'RC29N-STLAL' p_stlal,    "ALT BOM
     ' ' 'RC29N-AENNR' p_aennr,    "Change number
     ' ' 'BDC_OKCODE'  '=VOKO'.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0105',
     ' ' 'RC29N-MATNR' p_cmatn,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' p_werks,    "PLANT
     ' ' 'RC29N-STLAN' wa_stla1,   "BOM usage
     ' ' 'RC29N-STLAL' p_stlal,    "ALT BOM
     ' ' 'RC29N-DATUV' l_datuv,    "Valid-from date
     ' ' 'BDC_OKCODE'  '=CLWI'.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0156',
     ' ' 'BDC_OKCODE'  '=MALL'.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0156',
     ' ' 'BDC_OKCODE'  '=FCUE'.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCBU'.

ENDFORM.                    " BOM_COPY_CREATE
*&---------------------------------------------------------------------*
*&      Form  CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
FORM cs_bom_expl_mat_v2 USING    p_matnr
                                 p_werks
                                 p_stlan
                                 p_stlal
                                 p_datuv
                        CHANGING p_lines.
  DATA: lt_stb    LIKE stpox  OCCURS 0 WITH HEADER LINE,
        lt_matcat LIKE cscmat OCCURS 0 WITH HEADER LINE.
  DATA: l_exist,
        l_chk,
        l_zmsg  LIKE cfgnl-msglin,
        l_msgty TYPE sy-msgty,
        l_lines TYPE i,
        l_datuv(10).
  WRITE: p_datuv TO l_datuv.
  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            aumng                 = 0
            capid                 = 'PP01'
            cuobj                 = 000000000000000
            cuovs                 = 0
            cuols                 = ' '
            datuv                 = p_datuv
            emeng                 = 1
            mktls                 = 'X'
            mehrs                 = ' '
            mtnrv                 = p_matnr
            stlal                 = p_stlal
            stlan                 = p_stlan
            svwvo                 = 'X'
            werks                 = p_werks
            vrsvo                 = 'X'
*       IMPORTING
*            TOPMAT                =
*            DSTST                 =
       TABLES
            stb                   = lt_stb
            matcat                = lt_matcat
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
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  DESCRIBE TABLE lt_stb LINES p_lines.
  LOOP AT lt_stb. " WHERE XISDT EQ 'X'.
    CLEAR l_chk.
    IF lt_stb-xisdt EQ 'X'.
      PERFORM check_mast_stpo USING    lt_stb-idnrk
                                       lt_stb-werks
                                       lt_stb-xtlty
                                       lt_stb-xtlnr
                                       lt_stb-xtlal
                              CHANGING l_chk.
      IF l_chk EQ space.
        PERFORM bom_copy_create USING    lt_stb-idnrk
                                         lt_stb-werks
                                         lt_stb-xtlan
                                         lt_stb-xtlal
                                         lt_stb-idnrk
                                         wa_aennr_sta
                                         p_datuv.
*       CALL TRANSACTION
        CALL TRANSACTION 'CS01'  USING it_bdc
                                 OPTIONS FROM wa_opt
                                 MESSAGES INTO it_mess.
        l_msgty = sy-msgty.
        PERFORM rkc_msg_string CHANGING l_zmsg.

        it_boml-matnr = lt_stb-idnrk.
        it_boml-werks = lt_stb-werks.
        it_boml-stlan = lt_stb-xtlan.
        it_boml-stlal = lt_stb-xtlal.
        it_boml-fdatu = wa_aennr_sta(8).
        it_boml-tdatu = wa_aennr_end(8).
        it_boml-msgty = l_msgty.
        it_boml-zmsg  = l_zmsg.
        APPEND it_boml. CLEAR it_boml.

        REFRESH: it_bdc, it_mess. CLEAR: it_bdc, it_mess.
        PERFORM cs_bom_expl_mat_v2 USING    lt_stb-idnrk
                                            lt_stb-werks
                                            lt_stb-xtlan
                                            lt_stb-xtlal
                                            p_datuv
                                   CHANGING l_lines.


        PERFORM bom_out_point_of_time USING    lt_stb-idnrk
                                               lt_stb-werks
                                               lt_stb-xtlal
                                               wa_aennr_end
                                               l_lines.
*       CALL TRANSACTION
        CALL TRANSACTION 'CS02'  USING it_bdc
                                 OPTIONS FROM wa_opt
                                 MESSAGES INTO it_mess.
        REFRESH: it_bdc, it_mess. CLEAR: it_bdc, it_mess.
      ENDIF.
    ELSE.
      IF lt_stb-zinfo EQ 'ENG'.
        lt_stb-werks = 'E001'.
        PERFORM check_mast_stpo USING    lt_stb-idnrk
                                         lt_stb-werks
                                         lt_stb-xtlty
                                         lt_stb-xtlnr
                                         lt_stb-xtlal
                                CHANGING l_chk.
        IF l_chk EQ space.
*--------- Update Request by IY Choi at 2004-04-06.---------------------
*--------- Check the Engine's BOM. if Not exist, Skip the process.------
          PERFORM check_exist_bom USING    lt_stb-idnrk
                                           lt_stb-werks
                                           lt_stb-xtlty
                                           lt_stb-xtlnr
                                           lt_stb-xtlal
                                  CHANGING l_exist  .
          IF l_exist = space .
            CONTINUE        .
          ENDIF.
*--------- End of Update Request by IY Choi at 2004-04-06.--------------
          PERFORM bom_copy_create USING    lt_stb-idnrk
                                           lt_stb-werks
                                           lt_stb-xtlan
                                           lt_stb-xtlal
                                           lt_stb-idnrk
                                           wa_aennr_sta
                                           p_datuv.
*         CALL TRANSACTION
          CALL TRANSACTION 'CS01'  USING it_bdc
                                   OPTIONS FROM wa_opt
                                   MESSAGES INTO it_mess.
          l_msgty = sy-msgty.
          PERFORM rkc_msg_string CHANGING l_zmsg.

          it_boml-matnr = lt_stb-idnrk.
          it_boml-werks = lt_stb-werks.
          it_boml-stlan = lt_stb-xtlan.
          it_boml-stlal = lt_stb-xtlal.
          it_boml-fdatu = wa_aennr_sta(8).
          it_boml-tdatu = wa_aennr_end(8).
          it_boml-msgty = l_msgty.
          it_boml-zmsg  = l_zmsg.
          APPEND it_boml. CLEAR it_boml.

          REFRESH: it_bdc, it_mess. CLEAR: it_bdc, it_mess.
          PERFORM cs_bom_expl_mat_v2 USING    lt_stb-idnrk
                                              lt_stb-werks
                                              lt_stb-xtlan
                                              lt_stb-xtlal
                                              p_datuv
                                     CHANGING l_lines.


          PERFORM bom_out_point_of_time USING    lt_stb-idnrk
                                                 lt_stb-werks
                                                 lt_stb-xtlal
                                                 wa_aennr_end
                                                 l_lines.
*         CALL TRANSACTION
          CALL TRANSACTION 'CS02'  USING it_bdc
                                   OPTIONS FROM wa_opt
                                   MESSAGES INTO it_mess.
          REFRESH: it_bdc, it_mess. CLEAR: it_bdc, it_mess.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
*&      Form  CHANGE_NUMBER_CONCATENATE
*&---------------------------------------------------------------------*
FORM change_number_concatenate CHANGING p_chk.
  RANGES rt_aennr FOR aenr-aennr.
  DATA: BEGIN OF lt_aenr OCCURS 0,
          aennr TYPE aenr-aennr,
        END   OF lt_aenr.

  CONCATENATE p_gjahr '0101-001' INTO wa_aennr_sta.
  CONCATENATE p_gjahr '1231-001' INTO wa_aennr_end.

  rt_aennr-low = wa_aennr_sta.
  rt_aennr-sign = 'I'.
  rt_aennr-option = 'EQ'.
  APPEND rt_aennr.

  rt_aennr-low = wa_aennr_end.
  rt_aennr-sign = 'I'.
  rt_aennr-option = 'EQ'.
  APPEND rt_aennr.

  SELECT aennr
         FROM aenr
         INTO TABLE lt_aenr
         WHERE aennr IN rt_aennr.
  IF sy-subrc EQ 0.
    SORT lt_aenr BY aennr.
    READ TABLE lt_aenr WITH KEY aennr = wa_aennr_sta
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      p_chk = 'X'.
    ENDIF.

    READ TABLE lt_aenr WITH KEY aennr = wa_aennr_end
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      p_chk = 'X'.
    ENDIF.
  ELSE.

    p_chk = 'X'.

  ENDIF.
ENDFORM.                    " CHANGE_NUMBER_CONCATENATE
*&---------------------------------------------------------------------*
*&      Form  CHCEK_MAST_STPO_USAGE
*&---------------------------------------------------------------------*
FORM chcek_mast_stpo_usage CHANGING p_chk.
  DATA: BEGIN OF lt_mast OCCURS 0,
          matnr TYPE mast-matnr,
          werks TYPE mast-werks,
          stlan TYPE mast-stlan,
          stlal TYPE mast-stlal,
          datuv TYPE stpo-datuv,
          aennr TYPE stpo-aennr,
        END   OF lt_mast.
  SELECT  a~matnr
          a~werks
          a~stlan
          a~stlal
          b~datuv
          b~aennr
          FROM mast AS a INNER JOIN stpo AS b
                         ON a~stlnr EQ b~stlnr
          INTO  TABLE lt_mast
          WHERE a~stlan EQ wa_stla6
          AND   b~aennr EQ wa_aennr_sta.
  IF sy-subrc EQ 0.
*    P_CHK = 'X'.
*    FORMAT RESET.
*    SORT LT_MAST BY MATNR WERKS STLAN STLAL.
*    WRITE: / TEXT-004.
*    WRITE: /(20)  TEXT-005,
*            (10)  TEXT-006,
*            (10)  TEXT-007,
*            (10)  TEXT-008,
*            (10)  TEXT-009,
*            (12)  TEXT-010.
*    LOOP AT LT_MAST.
*      WRITE: /(20) LT_MAST-MATNR,
*              (10) LT_MAST-WERKS,
*              (10) LT_MAST-STLAN,
*              (10) LT_MAST-STLAL,
*              (10) LT_MAST-DATUV,
*              (12) LT_MAST-AENNR.
*    ENDLOOP.
  ENDIF.
ENDFORM.                    " CHCEK_MAST_STPO_USAGE
*&---------------------------------------------------------------------*
*&      Form  CHECK_MAST_STPO
*&---------------------------------------------------------------------*
FORM check_mast_stpo USING    p_idnrk
                              p_werks
                              p_xtlty
                              p_xtlnr
                              p_xtlal
                     CHANGING p_chk.
  DATA l_matnr LIKE mast-matnr.
  SELECT SINGLE a~matnr
*          A~WERKS
*          A~STLAN
*          A~STLAL
*          B~DATUV
          FROM mast AS a INNER JOIN stpo AS b
                         ON a~stlnr EQ b~stlnr
          INTO  l_matnr
          WHERE a~matnr EQ p_idnrk
          AND   a~werks EQ p_werks
          AND   a~stlan EQ wa_stla6
          AND   a~stlal EQ p_xtlal
          AND   b~stlty EQ p_xtlty
          AND   b~datuv EQ wa_aennr_sta.
  IF sy-subrc EQ 0.
    p_chk = 'X'.
  ENDIF.
ENDFORM.                    " CHECK_MAST_STPO
*&---------------------------------------------------------------------*
*&      Form  BOM_OUT_POINT_OF_TIME
*&---------------------------------------------------------------------*
FORM bom_out_point_of_time USING    p_idnrk
                                    p_werks
                                    p_xtlal
                                    p_aennr
                                    p_lines.
  DATA: l_times TYPE i,
        l_count TYPE i.
  l_times = p_lines DIV 5.
  l_count = p_lines MOD 5.
  l_times = l_times + 1.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' p_idnrk,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' p_werks,    "PLANT
     ' ' 'RC29N-STLAN' wa_stla6,   "BOM usage
     ' ' 'RC29N-STLAL' p_xtlal,    "ALT BOM
     ' ' 'RC29N-AENNR' p_aennr,    "Change number
     ' ' 'BDC_OKCODE'  '=FCPU'.

  DO l_times TIMES.
    IF l_times EQ sy-index.
      CASE l_count.
        WHEN '0'.
          PERFORM dynpro USING:
             'X' 'SAPLCSDI'        '0150',
             ' ' 'RC29P-AUSKZ(01)' 'X',    "
             ' ' 'RC29P-AUSKZ(02)' 'X',    "
             ' ' 'RC29P-AUSKZ(03)' 'X',    "
             ' ' 'RC29P-AUSKZ(04)' 'X',    "
             ' ' 'RC29P-AUSKZ(05)' 'X',    "
             ' ' 'BDC_OKCODE'  '=FCDL'.
        WHEN '1'.
          PERFORM dynpro USING:
             'X' 'SAPLCSDI'        '0150',
             ' ' 'RC29P-AUSKZ(01)' 'X',    "
             ' ' 'BDC_OKCODE'  '=FCDL'.
        WHEN '2'.
          PERFORM dynpro USING:
             'X' 'SAPLCSDI'        '0150',
             ' ' 'RC29P-AUSKZ(01)' 'X',    "
             ' ' 'RC29P-AUSKZ(02)' 'X',    "
             ' ' 'BDC_OKCODE'  '=FCDL'.
        WHEN '3'.
          PERFORM dynpro USING:
             'X' 'SAPLCSDI'        '0150',
             ' ' 'RC29P-AUSKZ(01)' 'X',    "
             ' ' 'RC29P-AUSKZ(02)' 'X',    "
             ' ' 'RC29P-AUSKZ(03)' 'X',    "
             ' ' 'BDC_OKCODE'  '=FCDL'.
        WHEN '4'.
          PERFORM dynpro USING:
             'X' 'SAPLCSDI'        '0150',
             ' ' 'RC29P-AUSKZ(01)' 'X',    "
             ' ' 'RC29P-AUSKZ(02)' 'X',    "
             ' ' 'RC29P-AUSKZ(03)' 'X',    "
             ' ' 'RC29P-AUSKZ(04)' 'X',    "
             ' ' 'BDC_OKCODE'  '=FCDL'.
      ENDCASE.
    ELSE.
      PERFORM dynpro USING:
         'X' 'SAPLCSDI'        '0150',
         ' ' 'RC29P-AUSKZ(01)' 'X',    "
         ' ' 'RC29P-AUSKZ(02)' 'X',    "
         ' ' 'RC29P-AUSKZ(03)' 'X',    "
         ' ' 'RC29P-AUSKZ(04)' 'X',    "
         ' ' 'RC29P-AUSKZ(05)' 'X',    "
         ' ' 'BDC_OKCODE'  '=FCDL'.
    ENDIF.
  ENDDO.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCBU'.
ENDFORM.                    " BOM_OUT_POINT_OF_TIME
*&---------------------------------------------------------------------*
*&      Form  BOM_USAGE_6_DELETE
*&---------------------------------------------------------------------*
FORM bom_usage_6_delete.
  DATA: BEGIN OF lt_mast OCCURS 0,
          matnr TYPE mast-matnr,
          werks TYPE mast-werks,
          stlan TYPE mast-stlan,
          stlal TYPE mast-stlal,
          stlnr TYPE mast-stlnr,
        END OF lt_mast.
  DATA l_datum(10).

  SELECT a~matnr
         a~werks
         a~stlan
         a~stlal
         a~stlnr
       FROM mast AS a INNER JOIN stpo AS b
                      ON a~stlnr EQ b~stlnr
       INTO TABLE lt_mast
       WHERE a~stlan EQ wa_stla6
*        AND   b~aennr EQ wa_aennr_sta
         AND   b~stlty EQ 'M'.

  IF sy-subrc EQ 0.
    REFRESH: it_bdc, it_mess. CLEAR: it_bdc, it_mess.
    SORT lt_mast BY matnr werks stlan stlal.
    DELETE ADJACENT DUPLICATES FROM lt_mast COMPARING ALL FIELDS.

    write:/ '*** Usage 6 Costing is being deleted.'.

    LOOP AT lt_mast.
      SELECT SINGLE * FROM stpo WHERE stlty = 'M'
                                  AND stlnr = lt_mast-stlnr.

      WRITE: stpo-datuv TO l_datum.

      PERFORM dynpro USING:
         'X' 'SAPLCSDI'    '0100',
         ' ' 'RC29N-MATNR' lt_mast-matnr,  "NEXT MATERIAL
         ' ' 'RC29N-WERKS' lt_mast-werks,  "PLANT
         ' ' 'RC29N-STLAN' lt_mast-stlan,  "BOM usage
         ' ' 'RC29N-STLAL' lt_mast-stlal,  "ALT BOM
         ' ' 'RC29N-DATUV' l_datum,        "Change number
         ' ' 'BDC_OKCODE'  '/00',

         'X' 'SAPLCSDI'    '0150',
         ' ' 'BDC_OKCODE'  '=FCLO'.

      CALL TRANSACTION 'CS02'  USING it_bdc
                               OPTIONS FROM wa_opt
                               MESSAGES INTO it_mess.
      if p_log = 'X'.
        write:/ lt_mast-matnr, '...deleted'.
      endif.
      REFRESH: it_bdc, it_mess. CLEAR: it_bdc, it_mess.
      CLEAR lt_mast.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " BOM_USAGE_6_DELETE
*&---------------------------------------------------------------------*
*&      Form  READ_MAST_BOM_CHECK
*&---------------------------------------------------------------------*
FORM read_mast_bom_check USING    p_matnr
                                  p_werks
                                  p_stlan
                                  p_stlal
                                  p_aennr
                         CHANGING p_check.
  DATA l_matnr TYPE mast-matnr.

  SELECT SINGLE a~matnr
       FROM mast AS a INNER JOIN stpo AS b
                      ON a~stlnr EQ b~stlnr
       INTO  l_matnr
       WHERE a~matnr EQ p_matnr
       AND   a~werks EQ p_werks
       AND   a~stlan EQ p_stlan
       AND   a~stlal EQ p_stlal
       AND   b~aennr EQ p_aennr
       AND   b~stlty EQ 'M'.
  IF sy-subrc EQ 0.
    CLEAR l_matnr.
    p_check = 'X'.
  ENDIF.
ENDFORM.                    " READ_MAST_BOM_CHECK

*&---------------------------------------------------------------------*
*&      Form  CHECK_EXIST_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_EXIST  text
*----------------------------------------------------------------------*
FORM check_exist_bom USING    pa_matnr  pa_werks  pa_xtlty
                              pa_xtlnr  pa_xtlal
                     CHANGING pa_exist  .
  DATA l_matnr LIKE mast-matnr.

  CLEAR: pa_exist.
  SELECT SINGLE a~matnr
          FROM mast AS a INNER JOIN stpo AS b
                         ON a~stlnr EQ b~stlnr
          INTO  l_matnr
          WHERE a~matnr EQ pa_matnr
          AND   a~werks EQ pa_werks
          AND   a~stlan EQ wa_stla1
          AND   a~stlal EQ pa_xtlal
          AND   b~stlty EQ pa_xtlty
          AND   b~datuv EQ wa_aennr_sta.
  IF sy-subrc EQ 0.
    pa_exist = 'X'.
  ENDIF.
ENDFORM.                    " CHECK_EXIST_BOM
