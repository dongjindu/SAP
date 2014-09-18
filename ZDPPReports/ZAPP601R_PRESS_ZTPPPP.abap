************************************************************************
* Program Name      : ZAPP601R_PRESS_ZTPPPP
* Author            : JongOh, Kim
* Creation Date     : 2004.02.11
* Specifications By : JongOh, Kim
* Pattern           : 5.2.2
* Development Request No : UD1K907281
* Addl Documentation:
* Description       : Panel Sequencing ( Include IPP601 Program )
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zapp601r_press_ztpppp NO STANDARD PAGE HEADING
                               MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : mara,        "General Material Data
         marc,        "Plant Data for Material
         mard,        "Storage Location Data for Material
         plaf,        "Planned order
         mkal,        "Production Versions of Material
         ztpppp.      "Press Production Planning From PP(MIP) -> MES
TABLES: zspp_app601,
        zspp_app601b.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA : it_plaf        LIKE TABLE OF plaf       WITH HEADER LINE.
DATA : it_ztpppp      LIKE TABLE OF ztpppp     WITH HEADER LINE,
       it_zspppp      LIKE TABLE OF zspppp_rfc WITH HEADER LINE.
DATA : BEGIN OF it_screen OCCURS 0,
        chk.
        INCLUDE STRUCTURE zspp_app601 .
DATA : zas ,                " AS: 'X' , OTHER: ' '
       zmatnr(5) ,          " Material 5 character
       lhrh  ,              "LH/RH
       prior          LIKE   zspp_app601-maxqt,  " priority value
       p110           LIKE mard-labst,
       mark  ,              " Plan Order's Capacity Check Result Flag.
       END OF it_screen.

DATA : it_screen_blank  LIKE TABLE OF it_screen      WITH HEADER LINE.
DATA : it_backup        LIKE TABLE OF it_screen      WITH HEADER LINE.
DATA : it_backup_blank  LIKE TABLE OF it_screen      WITH HEADER LINE.
DATA : it_prs_master    LIKE TABLE OF zspp_vin_value WITH HEADER LINE.
DATA : it_blk_master    LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA: wa_objekt         TYPE   mara-matnr ,
      wa_objekt_pnl     TYPE   mara-matnr .

DATA: wa_screen_ds      LIKE   it_screen  ,
      wa_matnr          LIKE   mara-matnr .

DATA: wa_ix             TYPE   sy-tabix .

*-----> SET/GET CURSOR HANDLING VARIABLE
DATA: wa_txt9000(40)    TYPE  c,         "SET/GET CURSOR FIELD
      wa_line9000       TYPE  sy-index.  "SET/GET CURSOR LINE

DATA: ok_code           TYPE  sy-ucomm,
      save_ok_code      TYPE  sy-ucomm.
DATA: chk,
      chk1.

DATA : wa_iferror_ix        LIKE  sy-index      ,
       wa_ifsuccess_ix      LIKE  sy-index      ,
       wa_iftotal_ix        LIKE  sy-index      ,

       wa_prdord_panel_ix   LIKE  sy-index      ,
       wa_bdc_panel_success LIKE  sy-index      ,
       wa_bdc_panel_error   LIKE  sy-index      ,

       wa_prdord_blank_ix   LIKE  sy-index      ,
       wa_bdc_blank_success LIKE  sy-index      ,
       wa_bdc_blank_error   LIKE  sy-index      .

DATA : wa_subrc         LIKE  sy-subrc      ,
       it_msg           LIKE TABLE OF bdcmsgcoll WITH HEADER LINE.

*-----> TABLE CONTROL
CONTROLS : tc_9000p     TYPE TABLEVIEW USING SCREEN 9000,
           tc_9000b     TYPE TABLEVIEW USING SCREEN 9000.

*-----> RANGES
RANGES: r_datum         FOR  sy-datum         .

RANGES: r_aufnr         FOR  ztpppp-aufnr     .
*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION (BDC DATA)
*----------------------------------------------------------------------*
DATA : it_bdcdata     LIKE TABLE OF bdcdata  WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION(BDC)
*----------------------------------------------------------------------*
*------> BDC MODE ('A' DISPLAY SCREEN, 'N' NO DISPLAY)
DATA: wa_option_ds   LIKE ctu_params.   "BDC OPTION

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: c_mark    VALUE  'X',
           c_mode    VALUE  'N',         "BDC MODE
           c_dest(10) VALUE 'WMPP01'.   "Outbound Interface Destination

CONSTANTS: c_spp     TYPE   marc-fevor   VALUE  'SPP',
           c_spb     TYPE   marc-fevor   VALUE  'SPB',
           c_p121    TYPE   mard-lgort   VALUE  'P121',
           c_p128    TYPE   mard-lgort   VALUE  'P128',
           c_p125    TYPE   mard-lgort   VALUE  'P125',
           c_p110    TYPE   mard-lgort   VALUE  'P110',
           c_p230    TYPE   mard-lgort   VALUE  'P230'.

CONSTANTS: c_plan_initial(2)    VALUE  'PL'.
*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS : p_werks      LIKE   t001w-werks OBLIGATORY MEMORY ID wrk,
             p_panel      LIKE   t024f-fevor OBLIGATORY MEMORY ID cfv,
             p_blank      LIKE   t024f-fevor OBLIGATORY MEMORY ID zpb.
SELECT-OPTIONS : s_datum   FOR   sy-datum  OBLIGATORY NO-EXTENSION,
                 s_matnr   FOR   mara-matnr .
SELECTION-SCREEN SKIP 1.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT  (20) TEXT-100.
*PARAMETERS: P_SEQ RADIOBUTTON GROUP R .
*SELECTION-SCREEN COMMENT  (25) TEXT-101 FOR FIELD P_SEQ.   "SEQ LOGIC
*PARAMETERS: P_MRP RADIOBUTTON GROUP R .
*SELECTION-SCREEN COMMENT  (25) TEXT-102 FOR FIELD P_MRP.   "MRP LOGIC
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK B1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (20) text-100.
PARAMETERS: p_seq RADIOBUTTON GROUP r .
SELECTION-SCREEN COMMENT  (15) text-101 FOR FIELD p_seq.   "SEQ LOGIC
PARAMETERS: p_check  AS CHECKBOX .
SELECTION-SCREEN COMMENT  (25) text-103 FOR FIELD p_check. "Allowance
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (20) text-104.
PARAMETERS: p_mrp RADIOBUTTON GROUP r .
SELECTION-SCREEN COMMENT  (25) text-102 FOR FIELD p_mrp.   "MRP LOGIC
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-005.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS      p_ir RADIOBUTTON GROUP rad1 DEFAULT 'X'.   "IR
SELECTION-SCREEN COMMENT (25) text-006 FOR FIELD p_ir.
PARAMETERS      p_rp RADIOBUTTON GROUP rad1.               "RP
SELECTION-SCREEN COMMENT (25) text-007 FOR FIELD p_rp.
PARAMETERS      p_dl RADIOBUTTON GROUP rad1.               "DL
SELECTION-SCREEN COMMENT (20) text-008 FOR FIELD p_dl.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
  PERFORM initialization.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM execute_process.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  CALL SCREEN 9000.

*  PERFORM LIST_PROCESS.
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM bdc_dynpro USING  p_program
                       p_dynpro.
  CLEAR it_bdcdata.
  it_bdcdata-program  = p_program.
  it_bdcdata-dynpro   = p_dynpro.
  it_bdcdata-dynbegin = 'X'.
  APPEND it_bdcdata.

ENDFORM.                    " BDC_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM bdc_field USING    p_fnam
                        p_fval.
*  IF P_FVAL <> Nodata.
  CLEAR it_bdcdata.
  it_bdcdata-fnam = p_fnam.
  it_bdcdata-fval = p_fval.
  APPEND it_bdcdata.
*  ENDIF.

ENDFORM.                    " BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.
  p_werks  = 'P001'   .
  p_panel  = c_spp    .   "PANEL
  p_blank  = c_spb    .   "BLANK

  s_datum-sign   = 'I'.
  s_datum-option = 'EQ'.
  s_datum-low    = sy-datum .
  APPEND s_datum.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM execute_process.

  CASE c_mark.
    WHEN p_seq .
*----> Seq Logic Process
      PERFORM seq_logic_process.
    WHEN p_mrp .
*----> MRP Logic Process
      PERFORM mrp_logic_process.
  ENDCASE.
ENDFORM.                    " EXECUTE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SELECT_PLAF
*&---------------------------------------------------------------------*
FORM select_plaf USING    pa_prgrp.
  DATA   l_rework     LIKE   zspp_app601-maxqt  .

  CLEAR : it_plaf  , it_plaf[].
  SELECT *
        INTO TABLE it_plaf
        FROM plaf
        WHERE plwrk EQ p_werks       " Plant
          AND psttr IN r_datum       " Production Plan Date
          AND matnr IN s_matnr       " Material No
          AND plgrp EQ pa_prgrp      " Production Scheduler(PANEL)
          AND plscn EQ space  .      " Scenario

  LOOP AT it_plaf.
    MOVE : it_plaf-psttr   TO  it_screen-gstrp ,    " Plan date
           it_plaf-plnum   TO  it_screen-plnum ,    " Plan Order
           it_plaf-matnr   TO  it_screen-matnr ,    " Material Code
           it_plaf-gsmng   TO  it_screen-gamng ,    " Order Qty
           pa_prgrp+2(1)   TO  it_screen-fevor .    " SPP, SPB

    CLEAR : l_rework .
*-----> Safety Stock
    SELECT SINGLE eisbe
            INTO it_screen-eisbe
            FROM marc
            WHERE werks EQ p_werks
              AND matnr EQ it_screen-matnr.

*-----> Current Stock = Available Stock + Rework Stock
*--> Available Stock
    CLEAR mkal.
    SELECT SINGLE *
            FROM mkal
            WHERE werks EQ p_werks
              AND matnr EQ it_screen-matnr
              AND verid EQ '01'     .

    CLEAR: it_screen-cstok, l_rework, it_screen-cstok1, it_screen-p110.
    SELECT SINGLE labst
            INTO it_screen-cstok
            FROM mard
            WHERE werks EQ p_werks
              AND matnr EQ it_screen-matnr
              AND lgort EQ mkal-alort.
*--> P121 Stock
    SELECT SINGLE  labst
           INTO l_rework
           FROM mard
           WHERE werks EQ p_werks
             AND matnr EQ it_screen-matnr
             AND lgort EQ c_p121.
    it_screen-cstok = it_screen-cstok + l_rework.
*--> Rework Stock
    CLEAR: l_rework.
    SELECT SINGLE  labst
           INTO l_rework
           FROM mard
           WHERE werks EQ p_werks
             AND matnr EQ it_screen-matnr
             AND lgort EQ c_p128.
    it_screen-cstok = it_screen-cstok + l_rework.

*--> P230 Stock
    SELECT SINGLE  labst
           INTO it_screen-cstok1
           FROM mard
           WHERE werks EQ p_werks
             AND matnr EQ it_screen-matnr
             AND lgort EQ c_p230.

*--> P110 Stock
    DATA: l_matnr    LIKE mara-matnr.
    CONCATENATE 'B' it_screen-matnr  INTO l_matnr.
    SELECT SINGLE  labst
           INTO it_screen-p110
           FROM mard
           WHERE werks EQ p_werks
             AND matnr EQ l_matnr
             AND lgort EQ c_p110.

*------> Material Description
    SELECT SINGLE maktx
           INTO it_screen-maktx
           FROM makt
           WHERE matnr EQ it_screen-matnr
             AND spras EQ sy-langu .
*Issue number :20041105-004 Add colummn(Reqmnts)
*Requested by Moon,changed by WSKIM,On 11.10.2004
*Add column
*-----Start
    PERFORM read_routing USING it_plaf
                         CHANGING it_screen-reqmnts.
*-----End

    APPEND it_screen.      CLEAR it_screen.
  ENDLOOP.
ENDFORM.                    " SELECT_PLAF

*&---------------------------------------------------------------------*
*&      Form  SELECT_PLAF_BLANK
*&---------------------------------------------------------------------*
FORM select_plaf_blank.
  DATA   l_rework     LIKE   zspp_app601-maxqt  .
  DATA   l_lgpro      TYPE   marc-lgpro         .

  CLEAR : it_plaf  , it_plaf[].
  SELECT *
        INTO TABLE it_plaf
        FROM plaf
        WHERE plwrk EQ p_werks       " Plant
          AND psttr IN r_datum       " Production Plan Date
          AND matnr IN s_matnr       " Material No
          AND plgrp EQ p_blank       " Production Scheduler(BLANK)
          AND plscn EQ space  .      " Scenario

  LOOP AT it_plaf.
    MOVE : it_plaf-psttr   TO  it_screen_blank-gstrp ,   " Plan date
           it_plaf-plnum   TO  it_screen_blank-plnum ,   " Plan Order
           it_plaf-matnr   TO  it_screen_blank-matnr ,   " Material Code
           it_plaf-gsmng   TO  it_screen_blank-gamng ,   " Order Qty
           p_blank+2(1)    TO  it_screen_blank-fevor .   " SPP, SPB

*-----> Safety Stock
    CLEAR l_lgpro.
    SELECT SINGLE eisbe lgpro
            INTO (it_screen_blank-eisbe, l_lgpro)
            FROM marc
            WHERE werks EQ p_werks
              AND matnr EQ it_screen_blank-matnr.

*-----> Current Stock = Available Stock + Rework Stock
*--> Available Stock
*    CLEAR MKAL.
*    SELECT SINGLE *
*            FROM MKAL
*            WHERE WERKS EQ P_WERKS
*              AND MATNR EQ IT_SCREEN_BLANK-MATNR
*              AND VERID EQ '01'     .

    SELECT SINGLE labst
            INTO it_screen_blank-cstok
            FROM mard
            WHERE werks EQ p_werks
              AND matnr EQ it_screen_blank-matnr
              AND lgort EQ l_lgpro.

*--> Rework Stock
    CLEAR l_rework.
    SELECT SINGLE  labst
           INTO l_rework
           FROM mard
           WHERE werks EQ p_werks
             AND matnr EQ it_screen_blank-matnr
             AND lgort EQ c_p128.
    it_screen_blank-cstok = it_screen_blank-cstok + l_rework.

**-----> EA/PAL UoM
*    SELECT SINGLE UMREZ
*            INTO IT_SCREEN_BLANK-UMREZ
*            FROM MARM
*            WHERE MATNR EQ IT_SCREEN_BLANK-MATNR
*              AND MEINH EQ 'PAL'.

*------> Material Description
    SELECT SINGLE maktx
           INTO it_screen_blank-maktx
           FROM makt
           WHERE matnr EQ it_screen_blank-matnr
             AND spras EQ sy-langu .
    APPEND it_screen_blank.      CLEAR it_screen_blank.
  ENDLOOP.

ENDFORM.                    " SELECT_PLAF_BLANK

*&---------------------------------------------------------------------*
*&      Form  INFORM_PALLET_PANEL
*&---------------------------------------------------------------------*
FORM inform_pallet_panel.
*-----> Available Pallet Qtty
  DATA : l_num        TYPE   i                 .
  DATA : l_pallet     LIKE   zspp_app601-pallt,
         l_maxqt      LIKE   zspp_app601-maxqt,
         l_as         LIKE   zspp_app601-maxqt.

  CLEAR it_prs_master.
  READ TABLE it_prs_master WITH KEY atnam = 'PRS_PNL_OHQ' .
  it_screen-umrez = it_prs_master-atwrt .

  CLEAR it_prs_master .
*  READ TABLE IT_PRS_MASTER WITH KEY ATNAM = 'PRS_PNL_OHQ'.
  READ TABLE it_prs_master WITH KEY atnam = 'PRS_PNL_CQP'.
  l_num = it_prs_master-atwrt.
*  IF L_NUM  GT 0.
  it_screen-pallt = it_prs_master-atwrt.
  it_screen-maxqt = it_screen-pallt * it_screen-umrez .
*  ELSE.
*    CLEAR IT_PRS_MASTER.
*    READ TABLE IT_PRS_MASTER WITH KEY ATNAM = 'PRS_PNL_TQP'.
*
*    IF SY-SUBRC EQ 0.
*      L_PALLET = IT_PRS_MASTER-ATWRT.
*
**--> A/S STOCK
*      SELECT SINGLE  LABST
*             INTO L_AS
*             FROM MARD
*             WHERE WERKS EQ P_WERKS
*               AND LGORT EQ C_P125.
*
**--> Total Pallet * MARM-UMREZ - STOCK (Available+Rework+A/S)
*      IT_SCREEN-MAXQT = ( L_PALLET * IT_SCREEN-UMREZ )
*                      - ( IT_SCREEN-CSTOK + L_AS ) .
*      IF IT_SCREEN-UMREZ NE 0.
*        IT_SCREEN-PALLT = IT_SCREEN-MAXQT / IT_SCREEN-UMREZ   .
*        CALL FUNCTION 'FIMA_NUMERICAL_VALUE_ROUND'
*             EXPORTING
*                  I_RTYPE     = '+'
*                  I_RUNIT     = '1'
*                  I_VALUE     = IT_SCREEN-MAXQT
*             IMPORTING
*                  E_VALUE_RND = IT_SCREEN-MAXQT.
*      ENDIF.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " INFORM_PALLET_PANEL

*&---------------------------------------------------------------------*
*&      Form  INFORM_PALLET_BLANK
*&---------------------------------------------------------------------*
FORM inform_pallet_blank.
**-----> Available Pallet Qtty
*  DATA : L_NUM        TYPE   I                 .
*  DATA : L_PALLET     LIKE   ZSPP_APP601-PALLT,
*         L_MAXQT      LIKE   ZSPP_APP601-MAXQT,
*         L_AS         LIKE   ZSPP_APP601-MAXQT.
*
*  CLEAR IT_PRS_MASTER.
*  READ TABLE IT_PRS_MASTER WITH KEY ATNAM = 'PRS_PNL_OHQ'.
*  L_NUM = IT_PRS_MASTER-ATWRT.
*  IF L_NUM  GT 0.
*    IT_SCREEN-PALLT = IT_PRS_MASTER-ATWRT.
*    IT_SCREEN-MAXQT = IT_SCREEN-PALLT * IT_SCREEN-UMREZ .
*  ELSE.
*    CLEAR IT_PRS_MASTER.
*    READ TABLE IT_PRS_MASTER WITH KEY ATNAM = 'PRS_PNL_TQP'.
*
*    IF SY-SUBRC EQ 0.
*      L_PALLET = IT_PRS_MASTER-ATWRT.
*
**--> A/S STOCK
*      SELECT SINGLE  LABST
*             INTO L_AS
*             FROM MARD
*             WHERE WERKS EQ P_WERKS
*               AND LGORT EQ C_P125.
*
**--> Total Pallet * MARM-UMREZ - STOCK (Available+Rework+A/S)
*      IT_SCREEN-MAXQT = ( L_PALLET * IT_SCREEN-UMREZ )
*                      - ( IT_SCREEN-CSTOK + L_AS ) .
*      IF IT_SCREEN-UMREZ NE 0.
*        IT_SCREEN-PALLT = IT_SCREEN-MAXQT / IT_SCREEN-UMREZ   .
*        CALL FUNCTION 'FIMA_NUMERICAL_VALUE_ROUND'
*             EXPORTING
*                  I_RTYPE     = '+'
*                  I_RUNIT     = '1'
*                  I_VALUE     = IT_SCREEN-MAXQT
*             IMPORTING
*                  E_VALUE_RND = IT_SCREEN-MAXQT.
*      ENDIF.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " INFORM_PALLET_BLANK

*&---------------------------------------------------------------------*
*&      Form  PROCESS_SPP
*&---------------------------------------------------------------------*
FORM process_spp.
  DATA : BEGIN OF lt_plnmg OCCURS 0,
          plnmg    LIKE   pbed-plnmg.
  DATA : END OF lt_plnmg.
*-----> Division of LH & RH
  CLEAR it_prs_master.
  READ TABLE it_prs_master WITH KEY atnam = 'PRS_PNL_DTYP'.
  IF sy-subrc EQ 0.
    IF it_prs_master-atwrt EQ 'D'.
      IF it_screen-matnr+3(1) EQ '2'.
        it_screen-lhrh = 'R'.
      ELSEIF it_screen-matnr+3(1) EQ '1'.
        it_screen-lhrh = 'L'.
      ENDIF.
    ENDIF.
  ENDIF.
  CLEAR it_prs_master.

*-----> Work Center (PANEL)
  READ TABLE it_prs_master WITH KEY atnam = 'PRS_PNL_MWC'.
  MOVE  it_prs_master-atwrt   TO  it_screen-arbpl  .
  CLEAR it_prs_master.

  IF sy-dynnr NE '9000'.
*-----> Information of Pallet
    PERFORM inform_pallet_panel.
  ENDIF.
  SELECT b~plnmg
        INTO TABLE lt_plnmg
        FROM pbim AS a INNER JOIN pbed AS b
          ON a~bdzei EQ b~bdzei
          WHERE a~matnr EQ it_screen-matnr   "Material
            AND a~werks EQ p_werks           "Plant
            AND a~versb EQ 'AS'              "Version
            AND b~pdatu EQ it_screen-gstrp.  "order finish date

  LOOP AT lt_plnmg.
*    IT_SCREEN-PRIOR = IT_SCREEN-PRIOR + LT_PLNMG-PLNMG.
    AT LAST.
      SUM.
      it_screen-plnmg = lt_plnmg-plnmg.
    ENDAT.
  ENDLOOP.

*--> PRIOR = PIR + Safety Stock - Stock Available
  it_screen-prior = it_screen-plnmg + it_screen-eisbe
                  - it_screen-cstok .

ENDFORM.                    " PROCESS_SPP

*&---------------------------------------------------------------------*
*&      Form  PROCESS_SPB
*&---------------------------------------------------------------------*
FORM process_spb.

*-----> Work Center (BLANK)
  READ TABLE it_prs_master WITH KEY atnam = 'PRS_BLK_MWC'.
  MOVE  it_prs_master-atwrt   TO  it_screen-arbpl  .
  CLEAR it_prs_master.

*-----> Information of Pallet
*  PERFORM INFORM_PALLET_BLANK.

ENDFORM.                    " PROCESS_SPB
*&---------------------------------------------------------------------*
*&      Form  APPEND_SCREEN
*&---------------------------------------------------------------------*
FORM append_screen USING pa_panel.
  DATA  l_tabix     LIKE   sy-tabix.
  LOOP AT it_screen WHERE fevor EQ pa_panel+2(1).
    l_tabix   = sy-tabix.
    wa_objekt = it_screen-matnr.
    CLEAR : it_prs_master, it_prs_master[].
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_objekt
              mode         = 'R'
              ctype        = '001'
         TABLES
              val_table    = it_prs_master
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    IF sy-subrc EQ 0.
      CASE pa_panel.
        WHEN c_spp.
          PERFORM process_spp.      " PANEL
*        WHEN C_SPB.
*          PERFORM PROCESS_SPB.      " BLANK
      ENDCASE.
    ENDIF.
    MODIFY it_screen INDEX  l_tabix.
  ENDLOOP.
ENDFORM.                    " APPEND_SCREEN

*&---------------------------------------------------------------------*
*&      Form  APPEND_SCREEN_BLANK
*&---------------------------------------------------------------------*
FORM append_screen_blank.
  DATA  l_tabix     LIKE   sy-tabix.
  DATA : BEGIN OF lt_plnmg OCCURS 0,
          plnmg    LIKE   pbed-plnmg.
  DATA : END OF lt_plnmg.

  LOOP AT it_screen_blank WHERE fevor EQ p_blank+2(1).
    l_tabix   = sy-tabix.
    CLEAR : it_blk_master, it_blk_master[].
    wa_objekt = it_screen_blank-matnr.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_objekt
              mode         = 'R'
              ctype        = '001'
         TABLES
              val_table    = it_blk_master
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    IF sy-subrc EQ 0.
*-----> Division of LH & RH
      wa_objekt_pnl = wa_objekt+1(17).
      IF wa_objekt_pnl+3(1) EQ '1'.
        CLEAR : it_prs_master, it_prs_master[].
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
             EXPORTING
                  object       = wa_objekt_pnl
                  mode         = 'R'
                  ctype        = '001'
             TABLES
                  val_table    = it_prs_master
             EXCEPTIONS
                  no_data      = 1
                  error_mode   = 2
                  error_object = 3
                  error_value  = 4
                  OTHERS       = 5.

        CLEAR it_prs_master.
        READ TABLE it_prs_master WITH KEY atnam = 'PRS_PNL_DTYP'.
        IF sy-subrc EQ 0.
          IF it_prs_master-atwrt EQ 'D'.
            IF it_screen_blank-matnr+4(1) EQ '2'.
              it_screen_blank-lhrh = 'R'.
            ELSEIF  it_screen_blank-matnr+4(1) EQ '1'.
              it_screen_blank-lhrh = 'L'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR it_blk_master.
*-----> Work Center (BLANK)
      READ TABLE it_blk_master WITH KEY atnam = 'PRS_BLK_MWC'.
      MOVE  it_blk_master-atwrt   TO  it_screen_blank-arbpl  .
      CLEAR it_blk_master.

**-----> Information of Pallet
*      PERFORM INFORM_PALLET_BLANK.

*-----> PIR
      SELECT b~plnmg
            INTO TABLE lt_plnmg
            FROM pbim AS a INNER JOIN pbed AS b
              ON a~bdzei EQ b~bdzei
              WHERE a~matnr EQ it_screen_blank-matnr  "Material
                AND a~werks EQ p_werks                "Plant
                AND a~versb EQ 'AS'                   "Version
                AND b~pdatu EQ it_screen_blank-gstrp. "Order finish date

      LOOP AT lt_plnmg.
        AT LAST.
          SUM.
          it_screen_blank-plnmg = lt_plnmg-plnmg.
        ENDAT.
      ENDLOOP.

    ENDIF.
*--> PRIOR = PIR + Safety Stock - Stock Available
    it_screen_blank-prior = it_screen_blank-plnmg
                          + it_screen_blank-eisbe
                          - it_screen_blank-cstok .
    MODIFY it_screen_blank INDEX  l_tabix.
  ENDLOOP.


ENDFORM.                    " APPEND_SCREEN_BLANK

*&---------------------------------------------------------------------*
*&      Form  INITIAL_SEQUENCING
*&---------------------------------------------------------------------*
FORM initial_sequencing.
*-> Sort by Plan date , Work Center , Priority
  DATA: l_seq           LIKE   ztpppp-cy_seqnr ,
        l_tabix         TYPE   sy-tabix        .
  DATA: l_temp_tabix    TYPE   sy-tabix        .
  DATA: l_low           TYPE   sy-datum        ,
        l_high          TYPE   sy-datum        .
  DATA: lt_temp         LIKE TABLE OF it_screen      WITH HEADER LINE.

  IF s_datum-high IS INITIAL.
    l_low   = s_datum-low.
    l_high  = s_datum-low.
  ELSE.
    l_low   = s_datum-low.
    l_high  = s_datum-high.
  ENDIF.

  SORT it_screen BY gstrp ASCENDING   " Plan date
                    arbpl ASCENDING   " Work Center
                    fevor DESCENDING  " SPP , SPB
                    prior DESCENDING  " Priority
                    lhrh  ASCENDING . " LH = 'L' , RH = 'R'

  LOOP AT it_screen WHERE gstrp GE l_low AND gstrp LE l_high.
    l_tabix  =  sy-tabix.
    AT NEW arbpl.
      CLEAR l_seq.
    ENDAT.

    IF it_screen-fevor NE 'B'     .
      IF it_screen-lhrh EQ 'R'.
        wa_matnr = it_screen-matnr.
        wa_matnr+3(1) = '1'.
        CLEAR wa_screen_ds.
        READ TABLE lt_temp INTO wa_screen_ds
               WITH KEY gstrp = it_screen-gstrp "Plan date
                        arbpl = it_screen-arbpl "W/C
                        fevor = it_screen-fevor "SPP
                        lhrh  =  'L'            "LH
                        matnr = wa_matnr.       "Material
        IF sy-subrc EQ 0.
          l_temp_tabix = sy-tabix.
          it_screen-cy_seqnr = wa_screen_ds-cy_seqnr.
          IF it_screen-gamng GT wa_screen_ds-gamng.
            wa_screen_ds-gamng = it_screen-gamng.
            MODIFY it_screen FROM wa_screen_ds INDEX l_temp_tabix.
          ELSE.
            it_screen-gamng = wa_screen_ds-gamng.
          ENDIF.
        ELSE.
          l_seq = l_seq + 1           .
          it_screen-cy_seqnr = l_seq  .
        ENDIF.
      ELSEIF it_screen-lhrh EQ 'L' .
        wa_matnr = it_screen-matnr.
        wa_matnr+3(1) = '2'.
        CLEAR wa_screen_ds.
        READ TABLE lt_temp INTO wa_screen_ds
               WITH KEY gstrp = it_screen-gstrp "Plan date
                        arbpl = it_screen-arbpl "W/C
                        fevor = it_screen-fevor "SPP
                        lhrh  =  'R'            "RH
                        matnr = wa_matnr.       "Material
        IF sy-subrc EQ 0.
          l_temp_tabix = sy-tabix.
          it_screen-cy_seqnr = wa_screen_ds-cy_seqnr.
          IF it_screen-gamng GT wa_screen_ds-gamng.
            wa_screen_ds-gamng = it_screen-gamng.
            MODIFY it_screen FROM wa_screen_ds INDEX l_temp_tabix.
          ELSE.
            it_screen-gamng = wa_screen_ds-gamng.
          ENDIF.
        ELSE.
          l_seq = l_seq + 1           .
          it_screen-cy_seqnr = l_seq  .
        ENDIF.
      ELSE.
        l_seq = l_seq + 1           .
        it_screen-cy_seqnr = l_seq  .
      ENDIF.
      MODIFY it_screen  INDEX  l_tabix.
      APPEND it_screen  TO lt_temp  .
      CLEAR : it_screen, wa_screen_ds.
    ENDIF.
  ENDLOOP.

  SORT it_screen BY gstrp ASCENDING    " Plan date
                    arbpl ASCENDING    " Work Center
                    fevor DESCENDING   " SPP , SPB
                    cy_seqnr ASCENDING " SEQ
                    matnr ASCENDING .  " Material

  DESCRIBE TABLE it_screen  LINES  tc_9000p-lines.
ENDFORM.                    " INITIAL_SEQUENCING
*&---------------------------------------------------------------------*
*&      Form  INITIAL_SEQUENCING_FOR_MRP
*&---------------------------------------------------------------------*
FORM initial_sequencing_for_mrp.
*-> Sort by Plan date , Work Center , Priority
  DATA: l_seq           LIKE   ztpppp-cy_seqnr ,
         l_tabix        TYPE   sy-tabix        .
  DATA: l_temp_tabix    TYPE   sy-tabix        .
  DATA: l_low           TYPE   sy-datum        ,
        l_high          TYPE   sy-datum        .
  DATA: lt_temp         LIKE TABLE OF it_screen      WITH HEADER LINE.

  IF s_datum-high IS INITIAL.
    l_low   = s_datum-low.
    l_high  = s_datum-low.
  ELSE.
    l_low   = s_datum-low.
    l_high  = s_datum-high.
  ENDIF.

  SORT it_screen BY gstrp ASCENDING   " Plan date
                    arbpl ASCENDING   " Work Center
                    fevor DESCENDING  " SPP , SPB
                    prior DESCENDING  " Priority
                    lhrh  ASCENDING . " LH = 'L' , RH = 'R'

  LOOP AT it_screen WHERE gstrp GE l_low AND gstrp LE l_high.
    l_tabix  =  sy-tabix.
    AT NEW arbpl.
      CLEAR l_seq.
    ENDAT.

    IF it_screen-fevor NE 'B'     .
      IF it_screen-lhrh EQ 'R'.
        wa_matnr = it_screen-matnr.
        wa_matnr+3(1) = '1'.
        CLEAR wa_screen_ds.
        READ TABLE lt_temp INTO wa_screen_ds
               WITH KEY gstrp = it_screen-gstrp "Plan date
                        arbpl = it_screen-arbpl "W/C
                        fevor = it_screen-fevor "SPP
                        lhrh  =  'L'            "LH
                        matnr = wa_matnr.       "Material
        IF sy-subrc EQ 0.
          l_temp_tabix = sy-tabix.
          it_screen-cy_seqnr = wa_screen_ds-cy_seqnr.
          IF it_screen-gamng GT wa_screen_ds-gamng.    "
            wa_screen_ds-gamng = it_screen-gamng.
            MODIFY it_screen FROM wa_screen_ds INDEX l_temp_tabix.
          ELSE.
            it_screen-gamng = wa_screen_ds-gamng     .
          ENDIF.
          it_screen-cy_seqnr = wa_screen_ds-cy_seqnr .
        ELSE.
          l_seq = l_seq + 1           .
          it_screen-cy_seqnr = l_seq  .
        ENDIF.
      ELSEIF it_screen-lhrh EQ 'L' .
        wa_matnr = it_screen-matnr.
        wa_matnr+3(1) = '2'.
        CLEAR wa_screen_ds.
        READ TABLE lt_temp INTO wa_screen_ds
               WITH KEY gstrp = it_screen-gstrp "Plan date
                        arbpl = it_screen-arbpl "W/C
                        fevor = it_screen-fevor "SPP
                        lhrh  =  'R'            "RH
                        matnr = wa_matnr.       "Material
        IF sy-subrc EQ 0.
          l_temp_tabix = sy-tabix.
          it_screen-cy_seqnr = wa_screen_ds-cy_seqnr.
          IF it_screen-gamng GT wa_screen_ds-gamng.
            wa_screen_ds-gamng = it_screen-gamng.
            MODIFY it_screen FROM wa_screen_ds INDEX l_temp_tabix.
          ELSE.
            it_screen-gamng = wa_screen_ds-gamng.
          ENDIF.
        ELSE.
          l_seq = l_seq + 1           .
          it_screen-cy_seqnr = l_seq  .
        ENDIF.

      ELSE.
        l_seq = l_seq + 1           .
        it_screen-cy_seqnr = l_seq  .
      ENDIF.
      MODIFY it_screen  INDEX  l_tabix.
      CLEAR : it_screen, wa_screen_ds.
    ENDIF.
  ENDLOOP.

  SORT it_screen BY gstrp ASCENDING    " Plan date
                    arbpl ASCENDING    " Work Center
                    fevor DESCENDING   " SPP , SPB
                    cy_seqnr ASCENDING " SEQ
                    matnr ASCENDING .  " material

  DESCRIBE TABLE it_screen  LINES  tc_9000p-lines.
ENDFORM.                    " INITIAL_SEQUENCING_FOR_MRP
*&---------------------------------------------------------------------*
*&      Form  INITIAL_SEQUENCING_BLANK
*&---------------------------------------------------------------------*
FORM initial_sequencing_blank.
  DATA : l_temp_tabix     LIKE   sy-tabix  ,
         l_tabix          LIKE   sy-tabix  ,
         l_seq            LIKE   ztpppp-cy_seqnr .
  DATA : l_low            LIKE   sy-datum  ,
         l_high           LIKE   sy-datum  .
  DATA: lt_temp           LIKE TABLE OF it_screen      WITH HEADER LINE.
  IF s_datum-high IS INITIAL.
    l_low  = s_datum-low .
    l_high = s_datum-low .
  ELSE.
    l_low  = s_datum-low .
    l_high = s_datum-high .
  ENDIF.

*  SORT IT_SCREEN_BLANK BY GSTRP ASCENDING   " Plan date
*                          ARBPL ASCENDING   " Work Center
*                          FEVOR DESCENDING  " SPP , SPB
*                          MATNR ASCENDING   " Material
*                          LHRH  ASCENDING . " LH = 'L' , RH = 'R'
  SORT it_screen_blank BY gstrp ASCENDING   " Plan date
                          arbpl ASCENDING   " Work Center
                          fevor DESCENDING  " SPP , SPB
                          prior DESCENDING  " Priority
                          lhrh  ASCENDING . " LH = 'L' , RH = 'R'
  LOOP AT it_screen_blank WHERE gstrp GE l_low AND gstrp LE l_high.
    l_tabix = sy-tabix.
    AT NEW arbpl.
      CLEAR l_seq .
    ENDAT.

    IF it_screen_blank-lhrh EQ 'R'.
      wa_matnr = it_screen_blank-matnr.
      wa_matnr+4(1) = '1'.
      CLEAR wa_screen_ds.
      READ TABLE it_screen_blank INTO wa_screen_ds
             WITH KEY gstrp = it_screen_blank-gstrp "Plan date
                      arbpl = it_screen_blank-arbpl "W/C
                      fevor = it_screen_blank-fevor "SPB
                      lhrh  = 'L'                   "LH
                      matnr = wa_matnr.             "Material
      IF sy-subrc EQ 0.
        l_temp_tabix = sy-tabix.
        IF it_screen_blank-gamng GT wa_screen_ds-gamng.
          wa_screen_ds-gamng = it_screen_blank-gamng.
          MODIFY it_screen_blank FROM wa_screen_ds INDEX l_temp_tabix.
        ELSE.
          it_screen_blank-gamng = wa_screen_ds-gamng.
        ENDIF.
        it_screen_blank-cy_seqnr = wa_screen_ds-cy_seqnr.
      ELSE.
        IF it_screen_blank-gstrp GE l_low  AND
           it_screen_blank-gstrp LE l_high .
          l_seq = l_seq + 1.
          it_screen_blank-cy_seqnr = l_seq .
        ENDIF.
      ENDIF.

    ELSEIF it_screen_blank-lhrh EQ 'L' .
      wa_matnr = it_screen_blank-matnr.
      wa_matnr+4(1) = '2'.
      CLEAR wa_screen_ds.
      READ TABLE lt_temp INTO wa_screen_ds
             WITH KEY gstrp = it_screen_blank-gstrp "Plan date
                      arbpl = it_screen_blank-arbpl "W/C
                      fevor = it_screen_blank-fevor "SPP
                      lhrh  = 'R'             "RH
                      matnr = wa_matnr.       "Material
      IF sy-subrc EQ 0.
        l_temp_tabix = sy-tabix.
        it_screen_blank-cy_seqnr = wa_screen_ds-cy_seqnr.
        IF it_screen_blank-gamng GT wa_screen_ds-gamng.
          wa_screen_ds-gamng = it_screen_blank-gamng.
          MODIFY it_screen_blank FROM wa_screen_ds INDEX l_temp_tabix.
        ELSE.
          it_screen_blank-gamng = wa_screen_ds-gamng.
        ENDIF.
      ELSE.
        l_seq = l_seq + 1           .
        it_screen_blank-cy_seqnr = l_seq  .
      ENDIF.

    ELSE.
      IF it_screen_blank-gstrp GE l_low  AND
         it_screen_blank-gstrp LE l_high .
        l_seq = l_seq + 1.
        it_screen_blank-cy_seqnr = l_seq .
      ENDIF.
    ENDIF.

    MODIFY it_screen_blank  INDEX l_tabix.
  ENDLOOP.

  SORT it_screen_blank BY gstrp ASCENDING    " Plan date
                          arbpl ASCENDING    " Work Center
                          fevor DESCENDING   " SPP , SPB
                          cy_seqnr ASCENDING " SEQ No
                          matnr ASCENDING .  " Material

  DESCRIBE TABLE it_screen_blank  LINES  tc_9000b-lines.
ENDFORM.                    " INITIAL_SEQUENCING_BLANK

*&---------------------------------------------------------------------*
*&      Module  SET_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_status OUTPUT.
  IF it_screen[] IS INITIAL AND it_screen_blank[] IS INITIAL.
    SET PF-STATUS '9000' EXCLUDING 'ZTRAN'.
  ELSE.
    SET PF-STATUS '9000'.
  ENDIF.

  CASE c_mark.
    WHEN p_seq .
      SET TITLEBAR '9000' WITH text-200.
    WHEN p_mrp .
      SET TITLEBAR '9000' WITH text-201.
  ENDCASE.
ENDMODULE.                 " SET_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_cursor_field OUTPUT.
  SET CURSOR FIELD wa_txt9000 LINE wa_line9000.
ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_DATA_PANEL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_data_panel OUTPUT.
  CLEAR zspp_app601 .
  MOVE : 'EA'                 TO    zspp_app601-gmein.
  MOVE : it_screen-chk        TO    chk                ,
         it_screen-gstrp      TO    zspp_app601-gstrp  ,  " PLAN DATE
         it_screen-arbpl      TO    zspp_app601-arbpl  ,  " W/C
         it_screen-matnr      TO    zspp_app601-matnr  ,  " Material
         it_screen-plnum      TO    zspp_app601-plnum  ,  " Planned No
         it_screen-eisbe      TO    zspp_app601-eisbe  ,  " Safety Stock
         it_screen-plnmg      TO    zspp_app601-plnmg  ,  " PIR
         it_screen-cstok      TO    zspp_app601-cstok  ,    " P120/p121
         it_screen-cstok1     TO    zspp_app601-cstok1 ,  " P230 Stock
         it_screen-pallt      TO    zspp_app601-pallt  ,  " Avail Pal Q
         it_screen-maxqt      TO    zspp_app601-maxqt  ,  " Max Qty
         it_screen-gamng      TO    zspp_app601-gamng  ,  " Planned Qty
*Issue number :20041105-004 Add colummn(Reqmnts)
*Requested by Moon,changed by WSKIM,On 11.10.2004
*Add column : Requirements : CRP Data
*-----Start
         it_screen-reqmnts    TO    zspp_app601-reqmnts ,
*-----End
         it_screen-aufnr      TO    zspp_app601-aufnr  ,  " Prd Ord
         it_screen-umrez      TO    zspp_app601-umrez  ,  " Pnl/Pal
         it_screen-cy_seqnr   TO    zspp_app601-cy_seqnr, " SEQ No
         it_screen-p110       TO    zspp_app601-bstok  ,  " Blank CStock
         it_screen-maktx      TO    zspp_app601-maktx  .  " Description
ENDMODULE.                 " DISPLAY_DATA_PANEL  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  DISPLAY_DATA_BLANK  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_data_blank OUTPUT.
  CLEAR : zspp_app601b .
  MOVE : 'EA'                       TO    zspp_app601b-gmein.
  MOVE : it_screen_blank-chk        TO    chk1                ,
         it_screen_blank-gstrp      TO    zspp_app601b-gstrp  ,
         it_screen_blank-arbpl      TO    zspp_app601b-arbpl  ,
         it_screen_blank-matnr      TO    zspp_app601b-matnr  ,
         it_screen_blank-plnum      TO    zspp_app601b-plnum  ,
         it_screen_blank-eisbe      TO    zspp_app601b-eisbe  ,
         it_screen_blank-plnmg      TO    zspp_app601b-plnmg  ,
         it_screen_blank-cstok      TO    zspp_app601b-cstok  ,
         it_screen_blank-pallt      TO    zspp_app601b-pallt  ,
         it_screen_blank-maxqt      TO    zspp_app601b-maxqt  ,
         it_screen_blank-gamng      TO    zspp_app601b-gamng  ,
         it_screen_blank-aufnr      TO    zspp_app601b-aufnr  ,
         it_screen_blank-umrez      TO    zspp_app601b-umrez  ,
         it_screen_blank-cy_seqnr   TO    zspp_app601b-cy_seqnr,
         it_screen_blank-maktx      TO    zspp_app601b-maktx  .
ENDMODULE.                 " DISPLAY_DATA_BLANK  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CHANGE_DISPLAY_DATA_PANEL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_display_data_panel OUTPUT.
  PERFORM change_display_data_panel.

ENDMODULE.                 " CHANGE_DISPLAY_DATA_PANEL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHANGE_DISPLAY_DATA_BLANK  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_display_data_blank OUTPUT.
  PERFORM change_display_data_blank.

ENDMODULE.                 " CHANGE_DISPLAY_DATA_BLANK  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
*      LEAVE  PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_PANEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen_panel INPUT.
  PERFORM modify_screen_panel.

ENDMODULE.                 " MODIFY_SCREEN_PANEL  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_BLANK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen_blank INPUT.
  PERFORM modify_screen_blank.

ENDMODULE.                 " MODIFY_SCREEN_BLANK  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_cursor_field INPUT.
  CLEAR: wa_txt9000, wa_line9000.
  GET CURSOR FIELD wa_txt9000 LINE wa_line9000.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CASE save_ok_code.
    WHEN 'ZTRAN'.
      PERFORM release_process.
    WHEN 'PALL' OR 'BALL' OR 'PDLL' OR 'BDLL' .
      PERFORM all_selection.
    WHEN 'PINS' OR 'BINS' OR 'PDEL' OR 'BDEL' .
      PERFORM line_insert.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DISPLAY_DATA_PANEL
*&---------------------------------------------------------------------*
FORM change_display_data_panel.
  DATA : l_total      LIKE  zspp_app601-gamng,
         l_low        TYPE  sy-datum,
         l_high       TYPE  sy-datum.

  IF NOT s_datum-high IS INITIAL.
    l_low   = s_datum-low .
    l_high  = s_datum-high.
  ELSE.
    l_low   = s_datum-low .
    l_high  = s_datum-low .
  ENDIF.

*--------> Sequence Logic.
  CASE c_mark .
    WHEN p_seq .
      LOOP AT SCREEN.
*----> All fields for All FORECAST is inactive.
       IF ( it_screen-gstrp GE l_low AND it_screen-gstrp LE l_high ) OR
                                             it_screen-gstrp IS INITIAL.

*----> Planned Qty > Max Qty  warning message
          IF zspp_app601-gamng GT zspp_app601-maxqt .
            IF zspp_app601-umrez NE 1 .
              IF screen-name EQ 'ZSPP_APP601-GAMNG'.
                screen-intensified  = '1'.
                MODIFY SCREEN .
              ENDIF.
            ENDIF.
          ENDIF.

          IF screen-group2 EQ 'FOR' .
*----> Material fields for Insert Line is active
            IF NOT it_screen-plnum IS INITIAL. "Insert Line
              IF screen-group1 EQ 'MAT'.
                screen-input = 0.
                MODIFY SCREEN.
              ENDIF.
              IF screen-group1 EQ 'DAT'.
                screen-input = 0.
                MODIFY SCREEN.
              ENDIF.
            ELSE.
              IF screen-group1 EQ 'MAT'.
                IF zspp_app601-aufnr IS INITIAL.
                  screen-input = 1.
                ELSE.
                  screen-input = 0.
                ENDIF.
                MODIFY SCREEN.
              ENDIF.
              IF screen-group1 EQ 'DAT'.
                IF zspp_app601-aufnr IS INITIAL.
                  screen-input = 1.
                ELSE.
                  screen-input = 0.
                ENDIF.
                MODIFY SCREEN.
              ENDIF.
            ENDIF.

*----> SEQ cannot be updated, If 'RH'
*           IF IT_SCREEN-LHRH EQ 'R'.    "SEQ cannot be updated, If 'RH'
*              IF SCREEN-GROUP1 EQ 'PRH' .
*                SCREEN-INPUT = 0        .
*                MODIFY SCREEN.
*              ENDIF.
*            ELSE.
*              IF SCREEN-GROUP1 EQ 'PRH' .
*                IF ZSPP_APP601-AUFNR IS INITIAL.
*                  SCREEN-INPUT = 1.
*                ELSE.
*                  SCREEN-INPUT = 0.
*                ENDIF.
*                MODIFY SCREEN.
*              ENDIF.
*            ENDIF.
          ENDIF.

*----> PLAN QTY
          IF screen-group1 EQ 'PLN'.
            IF zspp_app601-aufnr IS INITIAL.
              screen-input = 1.
            ELSE.
              screen-input = 0.
            ENDIF.
            MODIFY SCREEN.
          ENDIF.

*----> BLK C/S
          IF screen-group1 EQ 'BST'.
            CLEAR: l_total.
            l_total = zspp_app601-cstok + zspp_app601-cstok1.
            IF zspp_app601-gamng > l_total .
              screen-intensified  = '1'.
            ELSE.
              screen-intensified  = 0  .
            ENDIF.
            MODIFY SCREEN.
          ENDIF.

*----> AVA PALLET QTY
          IF screen-group1 EQ 'PAL'.
            IF zspp_app601-aufnr IS INITIAL.
              screen-input = 1.
            ELSE.
              screen-input = 0.
            ENDIF.
            MODIFY SCREEN.
          ENDIF.

        ELSE.
          IF screen-group2 EQ 'FOR'.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
      ENDLOOP.

*---------> MRP Logic
    WHEN p_mrp .
      LOOP AT SCREEN.
*----> All fields for All FORECAST is inactive.
       IF ( it_screen-gstrp GE l_low AND it_screen-gstrp LE l_high ) OR
                                            it_screen-gstrp IS INITIAL .
*----> Planned Qty > Max Qty  warning message
          IF zspp_app601-gamng GT zspp_app601-maxqt .
            IF screen-name EQ 'ZSPP_APP601-GAMNG'.
              screen-intensified  = '1'.
              MODIFY SCREEN .
            ENDIF.
          ENDIF.

          IF screen-group2 EQ 'FOR'.
*----> Material fields for Insert Line is active
            IF NOT it_screen-plnum IS INITIAL. "Insert Line
              IF screen-group1 EQ 'MAT'.
                screen-input = 0.
                MODIFY SCREEN.
              ENDIF.
              IF screen-group1 EQ 'DAT'.
                screen-input = 0.
                MODIFY SCREEN.
              ENDIF.
            ELSE.
              IF screen-group1 EQ 'MAT'.
                IF zspp_app601-aufnr IS INITIAL.
                  screen-input = 1.
                ELSE.
                  screen-input = 0.
                ENDIF.
                MODIFY SCREEN.
              ENDIF.
              IF screen-group1 EQ 'DAT'.
                IF zspp_app601-aufnr IS INITIAL.
                  screen-input = 1.
                ELSE.
                  screen-input = 0.
                ENDIF.
                MODIFY SCREEN.
              ENDIF.
            ENDIF.

**----> SEQ cannot be updated, If 'RH'
*            IF IT_SCREEN-LHRH EQ 'R'.   "SEQ cannot be updated, If 'RH'
*              IF SCREEN-GROUP1 EQ 'PRH'.
*                SCREEN-INPUT = 0       .
*                MODIFY SCREEN.
*              ENDIF.
*            ELSE.
*              IF SCREEN-GROUP1 EQ 'PRH'.
*                SCREEN-INPUT = 1.
*                MODIFY SCREEN.
*              ENDIF.
*            ENDIF.

*----> PAL cannot be update, IF MRP Logic
            IF screen-group1 EQ 'PAL' .
              screen-input = 0.
              MODIFY SCREEN .
            ENDIF.

*----> SEQ cannot be update, IF MRP Logic
            IF screen-group1 EQ 'PRH' .
              screen-input = 0.
              MODIFY SCREEN .
            ENDIF.
          ENDIF.

        ELSE.
          IF screen-group2 EQ 'FOR'.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
      ENDLOOP.

  ENDCASE.
ENDFORM.                    " CHANGE_DISPLAY_DATA_PANEL
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DISPLAY_DATA_BLANK
*&---------------------------------------------------------------------*
FORM change_display_data_blank.
  DATA : l_low        TYPE  sy-datum,
         l_high       TYPE  sy-datum.

  IF NOT s_datum-high IS INITIAL.
    l_low   = s_datum-low.
    l_high  = s_datum-high.
  ELSE.
    l_low   = s_datum-low.
    l_high  = s_datum-low.
  ENDIF.
  LOOP AT SCREEN.

*----> All fields for All FORECAST is inactive.
    IF ( it_screen_blank-gstrp GE l_low AND
       it_screen_blank-gstrp LE l_high ) OR
       it_screen_blank-gstrp IS INITIAL.
*----> Material fields for Insert Line is active
      IF NOT it_screen_blank-plnum IS INITIAL. "Insert Line
        IF screen-group1 EQ 'BAT'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1 EQ 'BLN'.
          IF zspp_app601b-aufnr IS INITIAL.
            screen-input = 1.
          ELSE.
            screen-input = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1 EQ 'DAT'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-group1 EQ 'BAT'.
          IF zspp_app601b-aufnr IS INITIAL.
            screen-input = 1.
          ELSE.
            screen-input = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1 EQ 'BLN'.
          IF zspp_app601b-aufnr IS INITIAL.
            screen-input = 1.
          ELSE.
            screen-input = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1 EQ 'DAT'.
          IF zspp_app601b-aufnr IS INITIAL.
            screen-input = 1.
          ELSE.
            screen-input = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ELSE.
      IF screen-group2 EQ 'BOR'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHANGE_DISPLAY_DATA_BLANK
*&---------------------------------------------------------------------*
*&      Form  ALL_SELECTION
*&---------------------------------------------------------------------*
FORM all_selection.
  CASE  save_ok_code.
    WHEN 'PALL'.
      LOOP AT it_screen.
        it_screen-chk  =  c_mark.
        MODIFY it_screen.
      ENDLOOP.
    WHEN 'BALL'.
      LOOP AT it_screen_blank.
        it_screen_blank-chk = c_mark.
        MODIFY it_screen_blank.
      ENDLOOP.
    WHEN 'PDLL'.
      LOOP AT it_screen.
        CLEAR it_screen-chk.
        MODIFY it_screen.
      ENDLOOP.
    WHEN 'BDLL'.
      LOOP AT it_screen_blank.
        CLEAR it_screen_blank-chk.
        MODIFY it_screen_blank.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    " ALL_SELECTION
*&---------------------------------------------------------------------*
*&      Form  LINE_INSERT
*&---------------------------------------------------------------------*
FORM line_insert.
  DATA: l_line       TYPE    sy-tabix,
        l_low        TYPE    sy-datum,
        l_high       TYPE    sy-datum.

  IF s_datum-high IS INITIAL.
    l_low   = s_datum-low.
    l_high  = s_datum-low.
  ELSE.
    l_low   = s_datum-low.
    l_high  = s_datum-high.
  ENDIF.

  CASE save_ok_code.
    WHEN 'PINS' .
      l_line = tc_9000p-top_line + wa_line9000 - 1.
      CLEAR wa_screen_ds.
*      READ TABLE IT_SCREEN INTO WA_SCREEN_DS INDEX L_LINE
*                                 TRANSPORTING GSTRP.
      INSERT wa_screen_ds INTO it_screen INDEX l_line.
      DESCRIBE TABLE it_screen         LINES  tc_9000p-lines.
    WHEN 'BINS' .
      l_line = tc_9000b-top_line + wa_line9000 - 1.
      CLEAR wa_screen_ds.
*      READ TABLE IT_SCREEN_BLANK INTO WA_SCREEN_DS INDEX L_LINE
*                                 TRANSPORTING GSTRP .
      INSERT wa_screen_ds INTO it_screen_blank INDEX l_line.
      DESCRIBE TABLE it_screen_blank   LINES  tc_9000b-lines.
    WHEN 'PDEL'.
      CLEAR : it_backup , it_backup[].
      LOOP AT it_screen WHERE chk EQ c_mark.
        MOVE-CORRESPONDING it_screen TO it_backup.
        APPEND it_backup.   CLEAR it_backup.
      ENDLOOP.
      DELETE it_screen       WHERE chk EQ c_mark.
      DESCRIBE TABLE it_screen         LINES  tc_9000p-lines.
    WHEN 'BDEL'.
      CLEAR : it_backup_blank,  it_backup_blank[].
      LOOP AT it_screen_blank WHERE chk EQ c_mark.
        MOVE-CORRESPONDING it_screen TO it_backup_blank.
        APPEND it_backup_blank. CLEAR it_backup_blank.
      ENDLOOP.
      DELETE it_screen_blank WHERE chk EQ c_mark.
      DESCRIBE TABLE it_screen_blank   LINES  tc_9000b-lines.
    WHEN 'PREC'.
    WHEN 'BREC'.
  ENDCASE.
ENDFORM.                    " LINE_INSERT

*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN_PANEL
*&---------------------------------------------------------------------*
FORM modify_screen_panel.
  MOVE : chk                   TO    it_screen-chk     ,
         zspp_app601-matnr     TO    it_screen-matnr   ,
         zspp_app601-pallt     TO    it_screen-pallt   ,
         zspp_app601-gamng     TO    it_screen-gamng   .

*----> MAX Quantity
  it_screen-maxqt
            = it_screen-pallt * it_screen-umrez.

*----> Check plan date
  PERFORM check_plan_date.

*----> Check Material
  PERFORM check_material.

**----> CHECK DUPLICATE, IF MRP Logic ,
  IF p_seq EQ c_mark.
    PERFORM check_duplicate.
  ENDIF.

*----> STOCK & PIR
  PERFORM move_stock_pir.

*----> Description
  SELECT SINGLE maktx
         INTO it_screen-maktx
         FROM makt
         WHERE matnr EQ zspp_app601-matnr
           AND spras EQ sy-langu .
  MODIFY it_screen   INDEX  tc_9000p-current_line.

ENDFORM.                    " MODIFY_SCREEN_PANEL
*&---------------------------------------------------------------------*
*&      Form  CHECK_PLAN_DATE
*&---------------------------------------------------------------------*
FORM check_plan_date .
  DATA: l_line       TYPE    sy-tabix,
        l_low        TYPE    sy-datum,
        l_high       TYPE    sy-datum.

  IF s_datum-high IS INITIAL.
    l_low   = s_datum-low.
    l_high  = s_datum-low.
  ELSE.
    l_low   = s_datum-low.
    l_high  = s_datum-high.
  ENDIF.

  IF it_screen-plnum IS INITIAL.
    IF zspp_app601-gstrp GE l_low AND
       zspp_app601-gstrp LE l_high.
      MOVE zspp_app601-gstrp TO it_screen-gstrp .
    ELSE.
      IF ok_code EQ 'PDEL' OR ok_code EQ 'BDEL'.
      ELSE.
        MESSAGE e001 WITH text-303.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_PLAN_DATE
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL
*&---------------------------------------------------------------------*
FORM check_material.
  IF NOT zspp_app601-matnr IS INITIAL.
    SELECT SINGLE *
              FROM marc
              WHERE werks EQ p_werks
                AND matnr EQ zspp_app601-matnr
                AND fevor EQ p_panel.
    IF sy-subrc NE 0.
      SET CURSOR FIELD 'ZSPP_APP601-MATNR' LINE tc_9000p-current_line.
      MESSAGE e001 WITH text-301.
    ELSE.
      MOVE zspp_app601-matnr   TO  it_screen-matnr.
    ENDIF.
  ELSE.
    MOVE zspp_app601-matnr    TO  it_screen-matnr.
  ENDIF.

ENDFORM.                    " CHECK_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  MOVE_STOCK_PIR
*&---------------------------------------------------------------------*
FORM move_stock_pir.
  DATA : l_rework     TYPE    mard-labst.
  IF NOT zspp_app601-matnr IS INITIAL AND
     zspp_app601-plnum IS INITIAL.
*-----> Safety Stock
    SELECT SINGLE eisbe
            INTO it_screen-eisbe
            FROM marc
            WHERE werks EQ p_werks
              AND matnr EQ zspp_app601-matnr.

*-----> Current Stock = Available Stock + Rework Stock
*--> Available Stock
    CLEAR mkal.
    SELECT SINGLE *
            FROM mkal
            WHERE werks EQ p_werks
              AND matnr EQ zspp_app601-matnr
              AND verid EQ '01'     .

    SELECT SINGLE labst
            INTO it_screen-cstok
            FROM mard
            WHERE werks EQ p_werks
              AND matnr EQ zspp_app601-matnr
              AND lgort EQ mkal-alort.

*--> Rework Stock
    SELECT SINGLE  labst
           INTO l_rework
           FROM mard
           WHERE werks EQ p_werks
             AND matnr EQ zspp_app601-matnr
             AND lgort EQ c_p128.
    it_screen-cstok = it_screen-cstok + l_rework.

**-----> EA/PAL UoM
*    SELECT SINGLE UMREZ
*            INTO IT_SCREEN-UMREZ
*            FROM MARM
*            WHERE MATNR EQ ZSPP_APP601-MATNR
*              AND MEINH EQ 'PAL'.

*-----> PRESS CLASSIFICATION
    wa_objekt = zspp_app601-matnr.
    CLEAR : it_prs_master, it_prs_master[].
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_objekt
              mode         = 'R'
              ctype        = '001'
         TABLES
              val_table    = it_prs_master
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    PERFORM module_process_spp.      " PANEL
  ENDIF.
ENDFORM.                    " MOVE_STOCK_PIR

*&---------------------------------------------------------------------*
*&      Form  CHECK_DUPLICATE
*&---------------------------------------------------------------------*
FORM check_duplicate.
  DATA : lt_screen      LIKE TABLE OF it_screen WITH HEADER LINE.
  DATA : l_low          TYPE sy-datum,
         l_high         TYPE sy-datum.
  DATA : l_matnr        TYPE mara-matnr ,
         l_tabix_end    TYPE sy-tabix,
         l_tabix_std    TYPE sy-tabix.

  IF s_datum-high IS INITIAL.
    l_low   = s_datum-low.
    l_high  = s_datum-low.
  ELSE.
    l_low   = s_datum-low.
    l_high  = s_datum-high.
  ENDIF.

  CLEAR : lt_screen[] , lt_screen.
  lt_screen[] = it_screen[].

  IF NOT zspp_app601-matnr IS INITIAL .
*---> RH
    IF it_screen-lhrh EQ 'R' .
*-> For Sequencing
      IF zspp_app601-gstrp GE l_low   AND
         zspp_app601-gstrp LE l_high.
        l_matnr = zspp_app601-matnr .
        l_matnr+3(1) = '1'.
        READ TABLE lt_screen WITH KEY gstrp    = zspp_app601-gstrp
                                      arbpl    = zspp_app601-arbpl
                                      matnr    = l_matnr
*                                      CY_SEQNR = ZSPP_APP601-CY_SEQNR
                                      lhrh     =  'L' .
        IF sy-subrc EQ 0 .
          it_screen-cy_seqnr = lt_screen-cy_seqnr .
        ENDIF.
      ENDIF.
*---> LH or others
    ELSE.
*-> For Sequencing
      IF zspp_app601-gstrp GE l_low   AND
         zspp_app601-gstrp LE l_high.
        l_tabix_std = tc_9000p-current_line .
        LOOP AT lt_screen .
          l_tabix_end = sy-tabix .
          IF l_tabix_end EQ l_tabix_std .
            EXIT.
          ELSE.
            IF lt_screen-lhrh NE 'R'
               AND lt_screen-gstrp    EQ zspp_app601-gstrp
               AND lt_screen-arbpl    EQ zspp_app601-arbpl
               AND lt_screen-cy_seqnr EQ zspp_app601-cy_seqnr .
              MESSAGE e001  WITH  text-302.
            ENDIF.
          ENDIF.
        ENDLOOP.
*        READ TABLE LT_SCREEN WITH KEY GSTRP    = ZSPP_APP601-GSTRP
*                                      ARBPL    = ZSPP_APP601-ARBPL
*                                      CY_SEQNR = ZSPP_APP601-CY_SEQNR
*                                      LHRH     = IT_SCREEN-LHRH .
*        IF SY-SUBRC EQ 0.
*
*          MESSAGE E001  WITH  TEXT-302.
*        ELSE.
        it_screen-cy_seqnr = zspp_app601-cy_seqnr .
*        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_DUPLICATE
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN_BLANK
*&---------------------------------------------------------------------*
FORM modify_screen_blank.
  MOVE : chk1                   TO    it_screen_blank-chk     ,
         zspp_app601b-matnr     TO    it_screen_blank-matnr   ,
         zspp_app601b-gamng     TO    it_screen_blank-gamng   .

*----> Check plan date
  PERFORM check_plan_date_blank.

*-----> Check Material Blank
  PERFORM check_material_blank.

*----->
  PERFORM move_stock_pir_blank.

*----> Description
  SELECT SINGLE maktx
         INTO it_screen_blank-maktx
         FROM makt
         WHERE matnr EQ zspp_app601b-matnr
           AND spras EQ sy-langu .
  MODIFY it_screen_blank   INDEX  tc_9000b-current_line.
ENDFORM.                    " MODIFY_SCREEN_BLANK
*&---------------------------------------------------------------------*
*&      Form  CHECK_PLAN_DATE_BLANK
*&---------------------------------------------------------------------*
FORM check_plan_date_blank.
  DATA: l_line       TYPE    sy-tabix,
        l_low        TYPE    sy-datum,
        l_high       TYPE    sy-datum.

  IF s_datum-high IS INITIAL.
    l_low   = s_datum-low.
    l_high  = s_datum-low.
  ELSE.
    l_low   = s_datum-low.
    l_high  = s_datum-high.
  ENDIF.

  IF it_screen_blank-gstrp GE l_low AND
     it_screen_blank-gstrp LE l_high .
*    IT_SCREEN_BLANK-CY_SEQNR = TC_9000B-CURRENT_LINE.
  ENDIF.

  IF it_screen_blank-plnum IS INITIAL .
    IF zspp_app601b-gstrp GE l_low AND
       zspp_app601b-gstrp LE l_high.
      MOVE zspp_app601b-gstrp TO it_screen_blank-gstrp .
    ELSE.
      IF ok_code EQ 'PDEL' OR ok_code EQ 'BDEL'.
      ELSE.
        MESSAGE e001 WITH text-303.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_PLAN_DATE_BLANK
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL_BLANK
*----------------------------------------------------------------------*
FORM check_material_blank.
  IF NOT zspp_app601b-matnr IS INITIAL.
    SELECT SINGLE *
              FROM marc
              WHERE werks EQ p_werks
                AND matnr EQ zspp_app601b-matnr
                AND fevor EQ p_blank.
    IF sy-subrc NE 0.
      SET CURSOR FIELD 'ZSPP_APP601B-MATNR' LINE tc_9000b-current_line.
      MESSAGE e001 WITH text-301.
    ELSE.
      MOVE zspp_app601b-matnr   TO  it_screen_blank-matnr.
    ENDIF.
  ELSE.
    MOVE zspp_app601b-matnr    TO  it_screen_blank-matnr.
  ENDIF.

ENDFORM.                    " CHECK_MATERIAL_BLANK
*&---------------------------------------------------------------------*
*&      Form  MOVE_STOCK_PIR_BLANK
*&---------------------------------------------------------------------*
FORM move_stock_pir_blank.
  DATA : l_lgpro      TYPE       marc-lgpro,
         l_rework     TYPE       mard-labst,
         l_cstok      TYPE       mard-labst.
  DATA : BEGIN OF lt_plnmg OCCURS 0,
          plnmg    LIKE   pbed-plnmg.
  DATA : END OF lt_plnmg.
*-----> Safety Stock
  SELECT SINGLE eisbe lgpro
          INTO (it_screen_blank-eisbe, l_lgpro)
          FROM marc
          WHERE werks EQ p_werks
            AND matnr EQ it_screen_blank-matnr.

*-----> Current Stock = Available Stock + Rework Stock
  SELECT SINGLE labst
          INTO l_cstok    "IT_SCREEN_BLANK-CSTOK
          FROM mard
          WHERE werks EQ p_werks
            AND matnr EQ it_screen_blank-matnr
            AND lgort EQ l_lgpro.

*--> Rework Stock
  SELECT SINGLE  labst
         INTO l_rework
         FROM mard
         WHERE werks EQ p_werks
           AND matnr EQ it_screen_blank-matnr
           AND lgort EQ c_p128.
*  IT_SCREEN_BLANK-CSTOK = IT_SCREEN_BLANK-CSTOK + L_REWORK.
  it_screen_blank-cstok = l_cstok + l_rework.

*-----> PIR
  SELECT b~plnmg
        INTO TABLE lt_plnmg
        FROM pbim AS a INNER JOIN pbed AS b
          ON a~bdzei EQ b~bdzei
          WHERE a~matnr EQ it_screen_blank-matnr  "Material
            AND a~werks EQ p_werks                "Plant
            AND a~versb EQ 'AS'                   "Version
            AND b~pdatu EQ it_screen_blank-gstrp. "Order finish date

  LOOP AT lt_plnmg.
    AT LAST.
      SUM.
      it_screen_blank-plnmg = lt_plnmg-plnmg.
    ENDAT.
  ENDLOOP.

*-----> BLANK CLASSIFICATION
  CLEAR : it_blk_master, it_blk_master[].
  wa_objekt = it_screen_blank-matnr.
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object             = wa_objekt
      mode               = 'R'
      ctype              = '001'
*        DISPLAY            = 'D'
    TABLES
      val_table          = it_blk_master
      EXCEPTIONS
        no_data            = 1
        error_mode         = 2
        error_object       = 3
        error_value        = 4
        OTHERS             = 5 .

  CLEAR it_blk_master.
*-----> Work Center (BLANK)
  READ TABLE it_blk_master WITH KEY atnam = 'PRS_BLK_MWC'.
  MOVE  it_blk_master-atwrt   TO  it_screen_blank-arbpl  .
  CLEAR it_blk_master.

ENDFORM.                    " MOVE_STOCK_PIR_BLANK
*&---------------------------------------------------------------------*
*&      Form  MODULE_PROCESS_SPP
*&---------------------------------------------------------------------*
FORM module_process_spp.
  DATA : BEGIN OF lt_plnmg OCCURS 0,
          plnmg    LIKE   pbed-plnmg.
  DATA : END OF lt_plnmg.
**-----> Division of LH & RH
*  CLEAR IT_PRS_MASTER.
*  READ TABLE IT_PRS_MASTER WITH KEY ATNAM = 'PRS_PNL_DTYP'.
*  IF SY-SUBRC EQ 0.
*    IF IT_PRS_MASTER-ATWRT EQ 'D'.
*      IF IT_SCREEN-MATNR+3(1) NE '1'.
*        IT_SCREEN-LHRH = 'R'.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*  CLEAR IT_PRS_MASTER.

*----> CHECK DUPLICATE
  PERFORM check_duplicate.

*-----> Work Center (PANEL)
  READ TABLE it_prs_master WITH KEY atnam = 'PRS_PNL_MWC' .
  MOVE  it_prs_master-atwrt   TO  it_screen-arbpl  .
  CLEAR it_prs_master.

*-----> EA/PAL UoM
*    SELECT SINGLE UMREZ
*            INTO IT_SCREEN-UMREZ
*            FROM MARM
*            WHERE MATNR EQ ZSPP_APP601-MATNR
*              AND MEINH EQ 'PAL'.
  READ TABLE it_prs_master WITH KEY atnam = 'PRS_PNL_OHQ' .
  MOVE it_prs_master-atwrt    TO  it_screen-umrez  .
  CLEAR it_prs_master.

*  IF SY-DYNNR NE '9000'.
**-----> Information of Pallet
*    PERFORM INFORM_PALLET_PANEL.
*  ENDIF.
  SELECT b~plnmg
        INTO TABLE lt_plnmg
        FROM pbim AS a INNER JOIN pbed AS b
          ON a~bdzei EQ b~bdzei
          WHERE a~matnr EQ it_screen-matnr   "Material
            AND a~werks EQ p_werks           "Plant
            AND a~versb EQ 'AS'              "Version
            AND b~pdatu EQ it_screen-gstrp.  "order finish date

  LOOP AT lt_plnmg.
    it_screen-prior = it_screen-prior + lt_plnmg-plnmg.
    AT LAST.
      SUM.
      it_screen-plnmg = lt_plnmg-plnmg.
    ENDAT.
  ENDLOOP.

*--> PRIOR = PIR + Safety Stock + Stock Available
  it_screen-prior = it_screen-plnmg + it_screen-eisbe
                  - it_screen-cstok .

ENDFORM.                    " MODULE_PROCESS_SPP
*&---------------------------------------------------------------------*
*&      Form  RELEASE_PROCESS
*&---------------------------------------------------------------------*
FORM release_process.
  PERFORM set_mode.
*---> Convert PLANORDER to PRODORDER & Create PRODORDER .
  PERFORM convert_planorder .

*---> Transfer PP to MES
  PERFORM transfer_to_mes   .

ENDFORM.                    " RELEASE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  CHECK_PCC_PRCESS
*&---------------------------------------------------------------------*
FORM check_pcc_prcess USING p_matnr .
  DATA: l_verid   TYPE   mkal-verid ,
        l_flag    ,
        l_text    LIKE   makt-maktx .

  CLEAR: wa_subrc, it_msg, it_msg[].

*----> Function Call for the PCC check...
* Read Material's Production Version
  SELECT SINGLE verid
             INTO l_verid
             FROM mkal
             WHERE matnr EQ p_matnr
               AND werks EQ p_werks .
* Check PCC
  PERFORM check_pcc_function   USING  p_matnr l_verid l_flag.
  IF l_flag = space OR l_flag = 'X' .
    CLEAR: l_text .
    CONCATENATE p_matnr l_verid      INTO l_text .
* Generate PCC
    PERFORM create_pcc USING p_matnr   p_werks
                             l_text   l_verid  l_flag .
  ENDIF.
ENDFORM.                    " CHECK_PCC_PRCESS

*&---------------------------------------------------------------------*
*&      Form  check_pcc_function
*&---------------------------------------------------------------------*
FORM check_pcc_function USING    pa_matnr  pa_verid  pa_flag .
  DATA: lp_procnr        LIKE aufk-procnr,
        lp_verid         LIKE afpo-verid ,
        lp_stlan         LIKE mkal-stlan ,
        lp_stlal         LIKE mkal-stlal ,
        lp_plnty         LIKE plko-plnty ,
        lp_plnnr         LIKE plko-plnnr ,
        lp_plnal         LIKE plko-plnal ,
        lp_aufnr         LIKE aufk-aufnr ,
        lt_vkks0         LIKE TABLE OF vkks0           WITH HEADER LINE,
        lt_pkosa         LIKE TABLE OF pkosa           WITH HEADER LINE.

  pa_flag = 'X'.
  CALL FUNCTION 'KK_F_PKOSA_FIND'
       EXPORTING
            i_matnr               = pa_matnr
            i_werks               = p_werks
            i_pwerk               = p_werks
            i_verid               = pa_verid
       IMPORTING
            e_procnr              = lp_procnr
            e_verid               = lp_verid
            e_stlan               = lp_stlan
            e_stlal               = lp_stlal
            e_plnty               = lp_plnty
            e_plnnr               = lp_plnnr
            e_plnal               = lp_plnal
            e_aufnr               = lp_aufnr
       TABLES
            e_vkks0               = lt_vkks0
            e_pkosa               = lt_pkosa
       EXCEPTIONS
            none_found            = 1
            wrong_input           = 2
            none_picked           = 3
            wrong_rule            = 4
            rsh_not_valid         = 5
            wrong_characteristics = 6
            no_rule               = 7
            version_not_valid     = 8
            OTHERS                = 9.

  CASE sy-subrc  .
    WHEN 0.
      pa_flag = 'S' .     " Can not call the PCC Function..
    WHEN 1.
      SELECT SINGLE aufnr INTO lp_aufnr
        FROM afko
       WHERE plnbez = pa_matnr .

      IF sy-subrc = 0.
        SELECT SINGLE aufnr INTO lp_aufnr
          FROM aufk
         WHERE aufnr = lp_aufnr
           AND auart = 'RM01'
           AND werks = 'P001'  .
        IF sy-subrc  = 0       .
          CLEAR: pa_flag.
        ELSE.
          CLEAR: pa_flag.
        ENDIF                  .
      ELSE.
        pa_flag = 'X'.
      ENDIF.
    WHEN 8.
      pa_flag = 'E' .     " Error - Un-Respected Scenario.
    WHEN OTHERS.
      pa_flag = 'E' .     " Error - Un-Respected Scenario.
  ENDCASE.
ENDFORM.                    " check_pcc_function

*&---------------------------------------------------------------------*
*&      Form  CREATE_PCC
*&---------------------------------------------------------------------*
FORM create_pcc USING pa_matnr  pa_werks  pa_text  pa_verid  pa_flag .
  DATA: l_matnr  LIKE bdcdata-fval,
        l_werks  LIKE bdcdata-fval,
        l_ktext  LIKE bdcdata-fval,
        l_verid  LIKE bdcdata-fval,
        l_flag   LIKE bdcdata-fval.

  l_matnr = pa_matnr.   l_werks = pa_werks.
  l_ktext = pa_text .   l_verid = pa_verid.

  CALL FUNCTION 'Z_FCO_PCC_ORDER_CRE_WITH_PDV'
       EXPORTING
            matnr_001 = l_matnr
            werks_002 = l_werks
            ktext_004 = l_ktext
            verid_007 = l_verid
            p_first   = pa_flag
       IMPORTING
            subrc     = wa_subrc
       TABLES
            messtab   = it_msg.


ENDFORM.                    " CREATE_PCC
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA_CO01
*&---------------------------------------------------------------------*
FORM generate_bdc_data_co01  USING pa_ind.
  DATA : l_date_tx(10).
  DATA : l_gamng(14).

  CASE pa_ind .
    WHEN c_spp.
      WRITE: it_screen-gamng TO l_gamng LEFT-JUSTIFIED.
      WRITE: it_screen-gstrp TO l_date_tx.
      PERFORM bdc_dynpro  USING 'SAPLCOKO1'	'0100'    .
      PERFORM bdc_field   USING :
  			'BDC_CURSOR'	    'CAUFVD-MATNR'     ,
  			'BDC_OKCODE'	    '/00'              ,
  			'CAUFVD-MATNR'	    it_screen-matnr    ,
  			'CAUFVD-WERKS'	    p_werks            ,
  			'AUFPAR-PP_AUFART'    'PP01'             .

      PERFORM bdc_dynpro  USING 'SAPLCOKO1'	'0115'	.
      PERFORM bdc_field   USING :
  			'BDC_OKCODE'	    '=FREI'            ,
  			'BDC_CURSOR'	    'CAUFVD-GAMNG'     ,
  			'CAUFVD-GAMNG'	    l_gamng            ,
  			'CAUFVD-GLTRP'	    l_date_tx          ,
  			'CAUFVD-GSTRP'	    l_date_tx          ,
  			'CAUFVD-TERKZ'	    '3'                .

      PERFORM bdc_dynpro  USING 'SAPLCOKO1'	'0115'	.
      PERFORM bdc_field   USING :
  			'BDC_OKCODE'	'=BU'                  ,
  			'BDC_CURSOR'	'CAUFVD-GAMNG'         .
    WHEN c_spb.
      WRITE: it_screen_blank-gamng TO l_gamng LEFT-JUSTIFIED.
      WRITE: it_screen_blank-gstrp TO l_date_tx.
      PERFORM bdc_dynpro  USING 'SAPLCOKO1'	'0100'    .
      PERFORM bdc_field   USING :
  			'BDC_CURSOR'	    'CAUFVD-MATNR'     ,
  			'BDC_OKCODE'	    '/00'              ,
  			'CAUFVD-MATNR'	    it_screen_blank-matnr    ,
  			'CAUFVD-WERKS'	    p_werks            ,
  			'AUFPAR-PP_AUFART'    'PP01'             .

      PERFORM bdc_dynpro  USING 'SAPLCOKO1'	'0115'	.
      PERFORM bdc_field   USING :
  			'BDC_OKCODE'	    '=FREI'            ,
  			'BDC_CURSOR'	    'CAUFVD-GAMNG'     ,
  			'CAUFVD-GAMNG'	    l_gamng            ,
  			'CAUFVD-GLTRP'	    l_date_tx          ,
  			'CAUFVD-GSTRP'	    l_date_tx          ,
  			'CAUFVD-TERKZ'	    '3'                .

      PERFORM bdc_dynpro  USING 'SAPLCOKO1'	'0115'	.
      PERFORM bdc_field   USING :
  			'BDC_OKCODE'	'=BU'                  ,
  			'BDC_CURSOR'	'CAUFVD-GAMNG'         .
  ENDCASE.


ENDFORM.                    " GENERATE_BDC_DATA_CO01
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA_CO40
*&---------------------------------------------------------------------*
FORM generate_bdc_data_co40 USING pa_ind.

  DATA : l_date_tx(10).
  DATA : l_gamng(14).


  IF pa_ind EQ c_spp.
    WRITE: it_screen-gamng TO l_gamng LEFT-JUSTIFIED.

    PERFORM bdc_dynpro  USING 'SAPLCOKO1'   '0150'.
    PERFORM bdc_field   USING :
      'BDC_CURSOR'        'AUFPAR-PP_AUFART' ,
      'BDC_OKCODE'	      '/00' ,
      'AFPOD-PLNUM'       it_screen-plnum,
      'AUFPAR-PP_AUFART'	'PP01' .

    WRITE it_screen-gstrp    TO    l_date_tx.

*----> Release process.
    PERFORM bdc_dynpro  USING 'SAPLCOKO1'	'0115' .
    PERFORM bdc_field   USING :
  			'BDC_OKCODE'	'=FREI'           ,
  			'BDC_CURSOR'	'CAUFVD-GAMNG'    ,
  			'CAUFVD-GAMNG'	l_gamng           ,
  			'CAUFVD-TERKZ'	'3'               .

*----> Convert Planned Order to Production Order
    PERFORM bdc_dynpro  USING 'SAPLCOKO1'	'0115' .
    PERFORM bdc_field   USING :
  			'BDC_OKCODE'	'=BU'             ,
  			'BDC_CURSOR'	'CAUFVD-GAMNG'    .
  ELSE.
    WRITE: it_screen_blank-gamng TO l_gamng LEFT-JUSTIFIED.
    PERFORM bdc_dynpro  USING 'SAPLCOKO1'   '0150'.
    PERFORM bdc_field   USING :
      'BDC_CURSOR'        'AUFPAR-PP_AUFART' ,
      'BDC_OKCODE'	      '/00' ,
      'AFPOD-PLNUM'       it_screen_blank-plnum,
      'AUFPAR-PP_AUFART'	'PP01' .

    WRITE it_screen_blank-gstrp    TO    l_date_tx.

*----> Release process.
    PERFORM bdc_dynpro  USING 'SAPLCOKO1'	'0115' .
    PERFORM bdc_field   USING :
  			'BDC_OKCODE'	'=FREI'                 ,
  			'BDC_CURSOR'	'CAUFVD-GAMNG'          ,
  			'CAUFVD-GAMNG'	l_gamng                 ,
  			'CAUFVD-TERKZ'	'3'               .

*----> Convert Planned Order to Production Order
    PERFORM bdc_dynpro  USING 'SAPLCOKO1'	'0115' .
    PERFORM bdc_field   USING :
  			'BDC_OKCODE'	'=BU'             ,
  			'BDC_CURSOR'	'CAUFVD-GAMNG'    .
  ENDIF.
ENDFORM.                    " GENERATE_BDC_DATA_CO40
*&---------------------------------------------------------------------*
*&      Form  CALL_TANSACTION_CO01
*&---------------------------------------------------------------------*
FORM call_tansaction_co01 USING pa_ind.
  CALL TRANSACTION 'CO01' USING it_bdcdata
                          OPTIONS FROM wa_option_ds.
  PERFORM error_text USING pa_ind.
  CLEAR it_bdcdata.
  REFRESH it_bdcdata.
ENDFORM.                    " CALL_TANSACTION_CO01

*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_CO40
*&---------------------------------------------------------------------*
FORM call_transaction_co40 USING pa_ind.
  CALL TRANSACTION 'CO40' USING it_bdcdata
                          OPTIONS FROM wa_option_ds.
  PERFORM error_text USING pa_ind.
  CLEAR it_bdcdata.
  REFRESH it_bdcdata.
ENDFORM.                    " CALL_TRANSACTION_CO40
*&---------------------------------------------------------------------*
*&      Form  ERROR_TEXT
*&---------------------------------------------------------------------*
FORM error_text USING    pa_ind.
  DATA : l_msg    LIKE cfgnl-msglin ,
         l_aufnr  LIKE afko-aufnr   .
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
            msg_lin = l_msg
       EXCEPTIONS
            OTHERS  = 1.
  CASE sy-msgty.
    WHEN 'E' OR 'A' OR 'X' OR 'W'.
      CASE pa_ind .
        WHEN c_spp.
          MOVE-CORRESPONDING it_screen       TO it_ztpppp.
          CONCATENATE c_plan_initial it_screen-plnum
                                     INTO it_ztpppp-aufnr .
          wa_bdc_panel_error = wa_bdc_panel_error + 1.
        WHEN c_spb.
          MOVE-CORRESPONDING it_screen_blank TO it_ztpppp.
          CONCATENATE c_plan_initial it_screen_blank-plnum
                                     INTO it_ztpppp-aufnr .
          wa_bdc_blank_error = wa_bdc_blank_error + 1.
      ENDCASE.

      MOVE:   p_werks      TO    it_ztpppp-pwerk    ,
              sy-mandt     TO    it_ztpppp-mandt    ,
*              'E'          TO    IT_ZTPPPP-FLAG     ,
              'B'          TO    it_ztpppp-zresult  ,
              l_msg        TO    it_ztpppp-zmsg     .

      APPEND it_ztpppp.   CLEAR it_ztpppp.

    WHEN OTHERS.            " 'I', 'S' :SUCCESS
*      L_AUFNR = SY-MSGV1.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
           EXPORTING
                input  = sy-msgv1
           IMPORTING
                output = l_aufnr.
      CASE pa_ind .
        WHEN c_spp.
          MOVE l_aufnr                       TO it_screen-aufnr.
          MOVE-CORRESPONDING it_screen       TO it_zspppp.
          MODIFY it_screen       INDEX wa_ix.
          wa_bdc_panel_success = wa_bdc_panel_success + 1.
        WHEN c_spb.
          MOVE l_aufnr                       TO it_screen_blank-aufnr.
          MOVE-CORRESPONDING it_screen_blank TO it_zspppp.
          MODIFY it_screen_blank INDEX wa_ix.
          wa_bdc_blank_success = wa_bdc_blank_success + 1.

      ENDCASE.
      MOVE: p_werks       TO    it_zspppp-pwerk  ,
            sy-mandt      TO    it_zspppp-mandt  .
      APPEND it_zspppp.   CLEAR it_zspppp.
  ENDCASE.


ENDFORM.                    " ERROR_TEXT
*&---------------------------------------------------------------------*
*&      Form  CONVERT_PLANORDER
*&---------------------------------------------------------------------*
FORM convert_planorder.
  DATA : l_low       TYPE    sy-datum,
         l_high      TYPE    sy-datum.

  IF s_datum-high IS INITIAL.
    l_low   =  s_datum-low.
    l_high  =  s_datum-low.
  ELSE.
    l_low   =  s_datum-low.
    l_high  =  s_datum-high.
  ENDIF.

  CLEAR : wa_iferror_ix ,
          wa_ifsuccess_ix      , wa_iftotal_ix       ,
          wa_prdord_panel_ix   , wa_prdord_blank_ix  ,
          wa_bdc_panel_error   , wa_bdc_blank_error  ,
          wa_bdc_panel_success , wa_bdc_blank_success,
          it_zspppp    , it_zspppp[]   ,
          it_ztpppp    , it_ztpppp[]   .

  DELETE it_screen WHERE gstrp IS initial
                      OR matnr IS initial
                      OR gamng IS initial .

  LOOP AT it_screen WHERE gstrp  GE l_low
                      AND gstrp  LE l_high
                      AND chk    EQ c_mark .
    wa_ix = sy-tabix.
    wa_prdord_panel_ix = wa_prdord_panel_ix + 1.

*----> Check PCC and Create PCC
    PERFORM check_pcc_prcess USING it_screen-matnr.
    READ TABLE it_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc NE 0.
      IF it_screen-aufnr EQ space.
        IF it_screen-plnum IS INITIAL.
          PERFORM generate_bdc_data_co01 USING p_panel .
          PERFORM call_tansaction_co01   USING p_panel .
        ELSE.
          PERFORM generate_bdc_data_co40 USING p_panel .
          PERFORM call_transaction_co40  USING p_panel .
        ENDIF.
      ELSE.
        wa_bdc_panel_success = wa_bdc_panel_success + 1 .
        MOVE-CORRESPONDING it_screen TO it_zspppp.
        MOVE: p_werks       TO    it_zspppp-pwerk  ,
             sy-mandt       TO    it_zspppp-mandt  .
        APPEND it_zspppp.   CLEAR it_zspppp.
      ENDIF.
    ELSE.
      wa_bdc_panel_error = wa_bdc_panel_error + 1 .
      MOVE-CORRESPONDING   it_screen    TO it_ztpppp.
      MOVE:   p_werks      TO    it_ztpppp-pwerk    ,
              sy-mandt     TO    it_ztpppp-mandt    ,
              'B'          TO    it_ztpppp-zresult  ,
              it_msg-msgv1 TO    it_ztpppp-zmsg     .

      APPEND it_ztpppp.   CLEAR it_ztpppp.
    ENDIF.
  ENDLOOP.

  LOOP AT it_screen_blank WHERE gstrp GE l_low
                            AND gstrp LE l_high
                            AND chk   EQ c_mark .
    wa_ix = sy-tabix.
    wa_prdord_blank_ix = wa_prdord_blank_ix + 1.

*----> Check PCC and Create PCC
    PERFORM check_pcc_prcess USING it_screen_blank-matnr.
    READ TABLE it_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc NE 0.
      IF it_screen_blank-aufnr EQ space .
        IF it_screen_blank-plnum IS INITIAL.
          PERFORM generate_bdc_data_co01 USING p_blank .
          PERFORM call_tansaction_co01 USING   p_blank .
        ELSE.
          PERFORM generate_bdc_data_co40 USING p_blank .
          PERFORM call_transaction_co40  USING p_blank .
        ENDIF.
      ELSE.
        wa_bdc_blank_success = wa_bdc_blank_success + 1 .
        MOVE-CORRESPONDING it_screen_blank TO it_zspppp.
        MOVE: p_werks       TO    it_zspppp-pwerk  ,
              sy-mandt      TO    it_zspppp-mandt  .
        APPEND it_zspppp.   CLEAR it_zspppp.
      ENDIF.
    ELSE.
      wa_bdc_blank_error = wa_bdc_blank_error + 1 .
      MOVE-CORRESPONDING   it_screen_blank  TO it_ztpppp.
      MOVE:   p_werks      TO    it_ztpppp-pwerk    ,
              sy-mandt     TO    it_ztpppp-mandt    ,
              'B'          TO    it_ztpppp-zresult  ,
              it_msg-msgv1 TO    it_ztpppp-zmsg     .

      APPEND it_ztpppp.   CLEAR it_ztpppp.
    ENDIF.
  ENDLOOP.

  LOOP AT it_screen WHERE gstrp  GE l_low
                      AND gstrp  LE l_high
                      AND chk    NE c_mark .
    MOVE-CORRESPONDING  it_screen         TO   it_zspppp       .
    MOVE : p_werks                        TO   it_zspppp-pwerk ,
           sy-mandt                       TO   it_zspppp-mandt .
    CONCATENATE c_plan_initial it_screen-plnum
                INTO it_zspppp-aufnr .
    APPEND it_zspppp.   CLEAR it_zspppp.
  ENDLOOP.

  LOOP AT it_screen WHERE gstrp GT l_high.
    MOVE-CORRESPONDING  it_screen         TO   it_zspppp       .
    MOVE : p_werks                        TO   it_zspppp-pwerk ,
           sy-mandt                       TO   it_zspppp-mandt .
    CONCATENATE c_plan_initial it_screen-plnum
                INTO it_zspppp-aufnr .
    APPEND it_zspppp.   CLEAR it_zspppp.
  ENDLOOP.

  LOOP AT it_screen_blank WHERE gstrp GT l_high.
    MOVE-CORRESPONDING  it_screen_blank   TO   it_zspppp       .
    MOVE : p_werks                        TO   it_zspppp-pwerk ,
           sy-mandt                       TO   it_zspppp-mandt .
    CONCATENATE c_plan_initial it_screen_blank-plnum
                INTO it_zspppp-aufnr .
    APPEND it_zspppp.   CLEAR it_zspppp.
  ENDLOOP.

ENDFORM.                    " CONVERT_PLANORDER
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_TO_MES
*&---------------------------------------------------------------------*
FORM transfer_to_mes.
  DATA: l_msgtxt(100),
        l_tabix         LIKE     sy-tabix.
  DATA: l_zuser    LIKE  ztpppp-zuser ,
        l_zsdat    LIKE  ztpppp-zsdat ,
        l_zstim    LIKE  ztpppp-zstim .

  l_zuser  =  sy-uname .
  l_zsdat  =  sy-datum .
  l_zstim  =  sy-uzeit .

  CASE c_mark .
    WHEN p_ir .
      it_zspppp-eflag = 'IR' .
    WHEN p_rp .
      it_zspppp-eflag = 'RP' .
    WHEN p_dl .
      it_zspppp-eflag = 'DL' .
  ENDCASE .

  it_zspppp-zuser  =  l_zuser .
  it_zspppp-zsdat  =  l_zsdat .
  it_zspppp-zstim  =  l_zstim .

  SORT it_zspppp BY pwerk gstrp arbpl cy_seqnr matnr .
  MODIFY it_zspppp  TRANSPORTING zuser zsdat zstim eflag
                    WHERE mandt EQ sy-mandt.

*----> TOTAL COUNT FOR INTERFACE
  DESCRIBE TABLE it_zspppp LINES wa_iftotal_ix.

  CALL FUNCTION 'Z_FPP_PRESS_PP'
    DESTINATION  c_dest
    TABLES
      t_ztpppp       = it_zspppp
  EXCEPTIONS
    communication_failure = 1  MESSAGE l_msgtxt
    system_failure        = 2  MESSAGE l_msgtxt.

*  IF SY-SUBRC NE 0.
*    WA_IFERROR_IX = WA_IFTOTAL_IX .
*    IT_ZSPPPP-ZZRET   = 'E'       .
**    IT_ZSPPPP-FLAG    = IT_ZSPPPP-ZZRET.
*    IT_ZSPPPP-ZRESULT = 'E'       .
*    IT_ZSPPPP-ZUSER   = SY-UNAME  .
*    IT_ZSPPPP-ZSDAT   = SY-DATUM  .
*    IT_ZSPPPP-ZSTIM   = SY-UZEIT  .
*    IT_ZSPPPP-ZMODE   = 'C'       .
*    IT_ZSPPPP-ZMSG    = L_MSGTXT  .
*    MODIFY IT_ZSPPPP TRANSPORTING ZZRET ZRESULT ZUSER ZEDAT ZETIM ZMSG
*                                  WHERE MANDT EQ SY-MANDT .
*    MESSAGE I001 WITH L_MSGTXT.
*  ELSE.
  LOOP AT it_zspppp    .
    l_tabix = sy-tabix .
    IF it_zspppp-zzret = 'E'    .
      wa_iferror_ix    = wa_iferror_ix + 1.
      it_zspppp-zzret  = 'E'     .
*        IT_ZSPPPP-FLAG  = IT_ZSPPPP-ZZRET.
      it_zspppp-zresult = 'E'   .
      it_zspppp-zuser = l_zuser .
      it_zspppp-zsdat = l_zsdat .
      it_zspppp-zstim = l_zstim .
      it_zspppp-zmode = 'C'.
      it_zspppp-zresult = 'E'.
      MODIFY it_zspppp INDEX l_tabix.
    ELSE.
      wa_ifsuccess_ix = wa_ifsuccess_ix + 1.
*        IT_ZSPPPP-FLAG  = IT_ZSPPPP-ZZRET.
      it_zspppp-zuser = l_zuser .
      it_zspppp-zsdat = l_zsdat .
      it_zspppp-zstim = l_zstim .
      it_zspppp-zmode = 'C'.
      it_zspppp-zresult = 'S' .
      MODIFY it_zspppp INDEX l_tabix.
    ENDIF.

    MOVE-CORRESPONDING it_zspppp TO  it_ztpppp.
    APPEND it_ztpppp.           CLEAR it_ztpppp.
  ENDLOOP.
*  ENDIF.

*----> TOTAL UPLOAD COUNT
  it_ztpppp-zuser  =  l_zuser .
  it_ztpppp-zsdat  =  l_zsdat .
  it_ztpppp-zstim  =  l_zstim .
  MODIFY it_ztpppp TRANSPORTING zuser zsdat zstim
                   WHERE mandt EQ sy-mandt .
*  DESCRIBE TABLE IT_ZTPPPP LINES WA_TOTAL_IX.
*----> DELETE Previous PLAN ORDER
  CLEAR : r_aufnr, r_aufnr[].
  r_aufnr-sign   = 'I'.
  r_aufnr-option = 'CP'.
  r_aufnr-low    = 'PL*'.
  APPEND r_aufnr .

  DELETE FROM ztpppp WHERE aufnr IN r_aufnr .
  LOOP AT it_ztpppp.
    MODIFY ztpppp FROM it_ztpppp.
  ENDLOOP.

  CALL SCREEN 9005 STARTING AT 20 10.
ENDFORM.                    " TRANSFER_TO_MES
*&---------------------------------------------------------------------*
*&      Form  SET_MODE
*&---------------------------------------------------------------------*
FORM set_mode.
  wa_option_ds-dismode = c_mode.
  wa_option_ds-defsize = 'X'.
  wa_option_ds-updmode = 'S'.
ENDFORM.                    " SET_MODE
*&---------------------------------------------------------------------*
*&      Form  SEQ_LOGIC_PROCESS
*&---------------------------------------------------------------------*
FORM seq_logic_process.
  CLEAR : it_screen      , it_screen[],
          it_screen_blank, it_screen_blank[],
          r_datum        , r_datum[].

  r_datum-sign   = 'I' .
  r_datum-option = 'BT'.
  r_datum-low    = s_datum-low.
  r_datum-high   = r_datum-low + 30 .
  APPEND r_datum.

*----> PANEL
  PERFORM select_plaf   USING p_panel.
  PERFORM append_screen USING p_panel.
  PERFORM initial_sequencing.

*----> BLANK.
*  PERFORM SELECT_PLAF   USING P_BLANK.
*  PERFORM APPEND_SCREEN USING P_BLANK.
  PERFORM select_plaf_blank.
  PERFORM append_screen_blank.
  PERFORM initial_sequencing_blank.

ENDFORM.                    " SEQ_LOGIC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  MRP_LOGIC_PROCESS
*&---------------------------------------------------------------------*
FORM mrp_logic_process.
  CLEAR : it_screen      , it_screen[],
          it_screen_blank, it_screen_blank[],
          r_datum        , r_datum[].

  r_datum-sign   = 'I'.
  r_datum-option = 'BT'.
  r_datum-low    = s_datum-low.
  r_datum-high   = r_datum-low + 30 .
  APPEND r_datum.

*----> PANEL
  PERFORM select_plaf   USING p_panel.
  PERFORM append_screen USING p_panel.
*  PERFORM INITIAL_SEQUENCING.
  PERFORM initial_sequencing_for_mrp.
*----> BLANK.
*  PERFORM SELECT_PLAF   USING P_BLANK.
*  PERFORM APPEND_SCREEN USING P_BLANK.
  PERFORM select_plaf_blank.
  PERFORM append_screen_blank.
  PERFORM initial_sequencing_blank.

ENDFORM.                    " MRP_LOGIC_PROCESS
*&---------------------------------------------------------------------*
*&      Module  STATUS_9005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9005 OUTPUT.
  SET PF-STATUS '9005'.
  SET TITLEBAR '9005'.

ENDMODULE.                 " STATUS_9005  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9005 INPUT.
  save_ok_code = ok_code.
  CASE save_ok_code.
    WHEN 'ENTE' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9005  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_Capacity  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_capacity INPUT.
  IF it_screen-mark = space.
    IF it_screen-plnum = space.    EXIT.     ENDIF.
    PERFORM read_reqmnts_planorder  USING    it_screen-plnum.
    it_screen-mark  = 'X'  .
    MODIFY it_screen       INDEX tc_9000p-current_line .
  ENDIF.
ENDMODULE.                 " check_Capacity  INPUT

*&---------------------------------------------------------------------*
*&      Form  read_reqmnts_planOrder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SCREEN_PLNUM  text
*----------------------------------------------------------------------*
FORM read_reqmnts_planorder USING    pa_plnum.

ENDFORM.                    " read_reqmnts_planOrder
*&---------------------------------------------------------------------*
*&      Form  read_routing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PLAF  text
*      <--P_IT_SCREEN_REQMNTS  text
*----------------------------------------------------------------------*
FORM read_routing USING    p_plaf LIKE it_plaf
                  CHANGING p_reqmnts.
  DATA : l_plnnr LIKE mapl-plnnr,
         l_plnal LIKE mapl-plnal.

  DATA: BEGIN OF it_routing OCCURS 0,
          matnr TYPE mara-matnr,
          dispo TYPE marc-dispo,
          plnnr TYPE plko-plnnr,
          plnal TYPE plko-plnal,
          vornr TYPE plpo-vornr,
*          arbpl TYPE crhd-arbpl,
*          sortb TYPE crhd-sortb,
          bmsch LIKE plpo-bmsch,
          lar02 TYPE plpo-lar02,  "Activity type
          vgw02 TYPE plpo-vgw02,  "Standard value
          vge02 TYPE plpo-vge02,  "Unit of measure for the standard
          lar03 TYPE plpo-lar03,  "Activity Type
          vgw03 TYPE plpo-vgw03,  "Standard value
          vge03 TYPE plpo-vge03,  "Unit of measure for the standard
        END OF it_routing.

  DATA : l_matnr LIKE plaf-matnr ,
         p_plant LIKE ztpp_bfst-plant,
         l_verid LIKE plaf-verid,
         l_dispo LIKE plaf-dispo,
         l_plnng LIKE mkal-plnng,
         l_alnag LIKE mkal-alnag,
         g_sortb(9).
  DATA : lpp_bfst-plan_ord LIKE plaf-plnum ."VALUE '1000245'.
  CLEAR : it_routing[],l_matnr,l_verid,l_dispo,l_plnnr,l_plnal.

  SELECT SINGLE  matnr verid dispo INTO (l_matnr,l_verid,l_dispo)
       FROM plaf
        WHERE plnum EQ p_plaf-plnum.

  SELECT SINGLE  plnnr alnal INTO (l_plnnr,l_plnal)
       FROM mkal
        WHERE matnr EQ l_matnr
          AND verid EQ l_verid
          AND werks EQ p_plaf-plwrk.

  SELECT   ma~matnr  mr~dispo pk~plnnr
           pk~plnal  pp~vornr pp~bmsch pp~lar02 pp~vgw02 pp~vge02
           pp~lar03  pp~vgw03 pp~vge03   "ch~arbpl ch~sortb
    INTO CORRESPONDING FIELDS OF TABLE it_routing
      FROM ( ( ( ( ( mara AS ma
        INNER JOIN marc AS mr ON ma~matnr = mr~matnr )
        INNER JOIN mapl AS mp ON mr~matnr = mp~matnr )
        INNER JOIN plko AS pk ON mp~plnnr = pk~plnnr AND
                                 mp~plnal = pk~plnal )
        INNER JOIN plas AS pa ON mp~plnty = pa~plnty AND
                                 mp~plnnr = pa~plnnr AND
                                 mp~plnal = pa~plnal )
        INNER JOIN plpo AS pp ON pk~plnnr = pp~plnnr AND
                                 pa~plnty = pp~plnty AND
                                 pa~plnnr = pp~plnnr AND
                                 pa~plnkn = pp~plnkn AND
                                 pa~zaehl = pp~zaehl )
*      INNER JOIN crhd AS ch ON pp~arbid = ch~objid )

      WHERE mp~plnty = 'N' AND
            mp~loekz = ' ' AND
            pa~loekz = ' ' AND
            ma~matnr EQ l_matnr  AND
            mr~werks EQ p_plaf-plwrk  AND
            mr~dispo EQ l_dispo  AND
            pk~plnnr EQ l_plnnr  AND
            pk~plnal EQ l_plnal  AND
            pa~plnfl EQ '000000'." AND
*          ch~objty EQ 'A' AND
*          ch~sortb EQ g_sortb.

  SORT it_routing BY matnr  ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_routing
           COMPARING matnr  vornr.
  READ TABLE it_routing INDEX 1.
  CASE it_routing-vge02.
    WHEN 'S'.
      p_reqmnts =
      ( ( it_routing-vgw02 * p_plaf-gsmng )  / it_routing-bmsch ) / 60.
    WHEN 'MIN'.
      p_reqmnts =
       ( it_routing-vgw02 * p_plaf-gsmng )  / it_routing-bmsch .
    WHEN OTHERS.
      p_reqmnts = ' ' .
  ENDCASE.
ENDFORM.                    " read_routing
