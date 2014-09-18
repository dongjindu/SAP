*&--------------------------------------------------------------------
*& Author                 : HS.Jeong
*& Creation Date          : 06/12/2003
*& Specification By       : hs.jeong
*& Pattern                : Report 1-2
*& Development Request No : UD1K904474
*& Addl documentation     :
*& Description  : Create Settlement Rule for PM/QM(Work Order)
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT ZCFII15 MESSAGE-ID  ZMFI.
TYPE-POOLS: SLIS, VRM.
INCLUDE <ICON>.
INCLUDE <SYMBOL>.
CLASS CL_GUI_RESOURCES DEFINITION LOAD.

CONSTANTS:
  C_F2CODE               LIKE SY-UCOMM                    VALUE '&ETA'.

DEFINE __CLS.                          " clear & refresh
  CLEAR &1.REFRESH &1.
END-OF-DEFINITION.

DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GS_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      GT_SP_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GT_EVENTS   TYPE SLIS_T_EVENT,
      GT_SORT     TYPE SLIS_T_SORTINFO_ALV,
      GS_PRNT     TYPE SLIS_PRINT_ALV,
      GT_LISTHEADER      TYPE SLIS_T_LISTHEADER,
      GS_VARIANT             TYPE DISVARIANT,
      GS_FIELDCAT            TYPE SLIS_FIELDCAT_ALV,
      GS_SORT                TYPE SLIS_SORTINFO_ALV.

DATA  G_ERROR.
DATA: GV_DEFAULT(1)  TYPE C,
      GV_REPID    LIKE SY-REPID.
DATA : G_PROGRAM              LIKE SY-REPID,
       G_EXIT_CAUSED_BY_CALLER,
       G_EXIT,
       G_SAVE                 VALUE 'A',
       G_VARIANT              TYPE DISVARIANT.

DATA: WA_REPID LIKE SY-REPID,
      WA_VAR_SAVE(1) TYPE C             VALUE  'A',
      WA_DEFAULT(1)  TYPE C,
      WA_EXIT(1) TYPE C,
      WA_CNT     TYPE I,
      WA_VARIANT LIKE DISVARIANT,
      WA_VAR LIKE DISVARIANT,
      WA_ALV_FUNCTION_NAME(30) TYPE C VALUE 'REUSE_ALV_LIST_LIST',
      WA_ALV_GET_INFO_NAME(40) TYPE C.

DATA : W_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
       W_EVENTCAT TYPE SLIS_T_EVENT WITH HEADER LINE,
       W_SELFIELD TYPE SLIS_SELFIELD,
       W_SORTCAT  TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
       W_COL_POS  TYPE I,
       W_PROGRAM  LIKE SY-REPID,
       W_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
       W_LINE1 TYPE SLIS_LISTHEADER.


*DATA: gt_list_top_of_page TYPE slis_t_listheader.

*----------------------------------------------------------------------
*
* define tables and internal structure
*
*----------------------------------------------------------------------
*
TABLES: AUFK, IMPR, FMFCTR, RIPASW, CODIA, BKPF, COAS.

DATA : IT_JEST    LIKE JEST    OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF IT_COSP OCCURS 0,
         AUFNR TYPE AUFNR,
* by IG.MOON 10/2007 {
         KTEXT TYPE KTEXT,
        $KOSTL TYPE KOSTL, " CC for Settlement
* }
         AUART TYPE AUFART,
         AUTYP TYPE AUFTYP,
         KOSTL TYPE KOSTL,
         BALAN LIKE COSP-WKG001.
        INCLUDE STRUCTURE COSP.
DATA :  END OF IT_COSP.

* comment outted by IG.MOON 10/2007 {
*investment order master
*DATA : BEGIN OF IT_AUFK OCCURS 0,
*         AUFNR TYPE AUFNR,
*         KOSTL TYPE KOSTL,
*         OBJNR TYPE J_OBJNR,
*         POSNR  TYPE POSNR,
*         POSID  TYPE POSID,
*       END OF IT_AUFK.
*DATA : it_vafiloa LIKE vafiloa OCCURS 0 WITH HEADER LINE.
*DATA : it_anla    LIKE anla    OCCURS 0 WITH HEADER LINE.
*DATA : it_cosp    LIKE aufk    OCCURS 0 WITH HEADER LINE.
*DATA : begin of it_imzo occurs 0,
*         objnr  type j_objnr,
*         posnr  type posnr,
*         posid  type posid,
*       end of it_imzo.
*DATA : IT_V_EQUI  LIKE V_EQUI  OCCURS 0 WITH HEADER LINE.
* }

DATA : BEGIN OF IT_VAFILOA OCCURS 0,
         AUFNR  TYPE AUFNR,
         EQUNR  TYPE EQUNR,
         TPLNR  TYPE TPLNR,
         KOSTL  TYPE KOSTL,
         ANLNR  TYPE ANLN1,
       END OF IT_VAFILOA.

DATA : BEGIN OF IT_V_EQUI OCCURS 0,
         EQUNR  TYPE EQUNR,
         EQART  TYPE EQART,
         TPLNR  TYPE TPLNR,
         KOSTL  TYPE KOSTL,
         ANLNR  TYPE ANLN1,
       END OF IT_V_EQUI.
* }

DATA : IT_COBRB   LIKE COBRB   OCCURS 0 WITH HEADER LINE.

*DATA : BEGIN OF IT_ANLC OCCURS 0,
*         ANLN1  TYPE ANLN1,
*         ANLN2  TYPE ANLN2,
*         ANSWL  TYPE ANSWL,
*         EAUFN  TYPE EAUFN,
*       END OF IT_ANLC.

*====FOR BDC
DATA : IT_BDC      LIKE BDCDATA OCCURS 0 WITH HEADER LINE.
DATA:  IT_MESSTAB  LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA : TCODE LIKE  TSTC-TCODE.
*--ASSET NUMBER
DATA : BEGIN OF IT_ITOB OCCURS 0,
         ERDAT LIKE ITOB-ERDAT,
         EQUNR LIKE ITOB-EQUNR,
         ANLNR LIKE ITOB-ANLNR,
         KOSTL LIKE ITOB-KOSTL,
         TPLNR LIKE ITOB-TPLNR,
       END OF IT_ITOB.
*

DATA: BEGIN OF IT_IS_SET OCCURS 0,
       OBJNR TYPE J_OBJNR,
       KOSTL TYPE KOSTL,
      END OF IT_IS_SET.


DATA: BEGIN OF GT_OUT OCCURS 0,
        AUFNR        LIKE  AUFK-AUFNR,
        AUART        LIKE  AUFK-AUART,
        OBJNR        LIKE  AUFK-OBJNR,
        KTEXT        LIKE  AUFK-KTEXT,
        KOSTV        LIKE  AUFK-KOSTV,
        BALAN        LIKE  ANLC-ANSWL,     "order balance
        TPLNR        LIKE  VAFILOA-TPLNR,
        EQUNR        LIKE  VAFILOA-EQUNR,
        ANLNR        LIKE  ITOB-ANLNR,     "asset
        KOSTL        LIKE  ITOB-KOSTL,
        ANSWL        LIKE  ANLC-ANSWL,     "asset value
        RATE(5) TYPE P DECIMALS 2,
*        EAUFN        LIKE  ANLA-EAUFN,
*        POSID(20), "        LIKE  impr-posid,
        IO(2),
        ST(2),
       $KOSTL        LIKE  ITOB-KOSTL,
        CHKBOX       TYPE C,
        ICON         TYPE ICON_D,
       $MSG(80)      TYPE C,
       $FLAG         TYPE C,
        TABCOLOR     TYPE SLIS_T_SPECIALCOL_ALV,
      END OF GT_OUT.

* by IG.MOON 10/2007 {
DATA GT_OUT1 LIKE GT_OUT OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF GT_COBRB OCCURS 0,
        OBJNR LIKE COBRB-OBJNR,
        BUREG LIKE COBRB-BUREG,
        LFDNR LIKE COBRB-LFDNR,
        ERSJA LIKE COBRB-ERSJA,
        LETJA LIKE COBRB-LETJA,
        EXTNR LIKE COBRB-EXTNR,
       END OF GT_COBRB.


*DATA: BEGIN OF GT_OUT1 OCCURS 0,
*        AUFNR        LIKE  AUFK-AUFNR,
*        OBJNR        LIKE  AUFK-OBJNR,
*        KTEXT        LIKE  AUFK-KTEXT,
*        KOSTV        LIKE  AUFK-KOSTV,
*        BALAN        LIKE  ANLC-ANSWL,     "order balance
*        TPLNR        LIKE  VAFILOA-TPLNR,
*        EQUNR        LIKE  VAFILOA-EQUNR,
*        ANLNR        LIKE  ITOB-ANLNR,     "asset
*        KOSTL        LIKE  ITOB-KOSTL,
*        ANSWL        LIKE  ANLC-ANSWL,     "asset value
*        RATE(5) TYPE P DECIMALS 2,
*        EAUFN        LIKE  ANLA-EAUFN,
*        POSID        LIKE  IMPR-POSID,
*        IO(2),
*        ST(2),
*        CHKBOX       TYPE C,
*        LIGHT        TYPE C,
*        TABCOLOR     TYPE SLIS_T_SPECIALCOL_ALV,
*      END OF GT_OUT1.
* }

DATA: BEGIN OF GT_TEMP OCCURS 0,
        AUFNR        LIKE  AUFK-AUFNR,
        KTEXT        LIKE  AUFK-KTEXT,
        KOSTV        LIKE  AUFK-KOSTV,
        TPLNR        LIKE  VAFILOA-TPLNR,
        EQUNR        LIKE  VAFILOA-EQUNR,
        ANLNR        LIKE  ITOB-ANLNR,     "asset
        KOSTL        LIKE  ITOB-KOSTL,
        EAUFN        LIKE  ANLA-EAUFN,
        POSID        LIKE  IMPR-POSID,
        CHKBOX       TYPE C,
        LIGHT        TYPE C,
        TABCOLOR     TYPE SLIS_T_SPECIALCOL_ALV,
      END OF GT_TEMP.
*----io sun   check rate

*DATA: BEGIN OF GT_SUM OCCURS 0,
*        AUFNR        LIKE  AUFK-AUFNR,
*        ANSWL        LIKE  ANLC-ANSWL,     "asset value
*        CNT          TYPE I,
*      END OF GT_SUM.

DATA : IT_DUMP LIKE GT_OUT  OCCURS 0 WITH HEADER LINE.
*----for combox
DATA: IT_VAL TYPE VRM_VALUES,
      W_LINE LIKE LINE OF IT_VAL.
*---WORK AREA
DATA : WA_T_CNT TYPE I,
       WA_CONFIRM,
*       wa_rate(6),
       WA_RATE(6) TYPE N, " DECIMALS 2, " LIKE cobrb-prozs,
*       wa_rate1(6) TYPE n, " DECIMALs 2, " LIKE cobrb-prozs,
       WA_RATE1(10), "  LIKE cobrb-prozs,
       WA_CHK,
       WA_SUBRC LIKE SY-SUBRC,
       WA_D_CNT TYPE I.

DATA : AUFNR        LIKE  AUFK-AUFNR,
       OBJNR        LIKE  AUFK-OBJNR,
       KTEXT        LIKE  AUFK-KTEXT,
       KOSTV        LIKE  AUFK-KOSTV,
       TPLNR        LIKE  VAFILOA-TPLNR,
       EQUNR        LIKE  VAFILOA-EQUNR,
       ANLNR        LIKE  ITOB-ANLNR,     "asset
       KOSTL        LIKE  ITOB-KOSTL,
      $KOSTL        LIKE  ITOB-KOSTL,     "SettlementCC
       ANSWL        LIKE  ANLC-ANSWL,     "asset value
       RATE(5) TYPE P DECIMALS 2,
       EAUFN        LIKE  ANLA-EAUFN,
       POSID        LIKE  IMPR-POSID,
       IO(2),
       ST(2).

*---Ranges
RANGES : R_AUART FOR AUFK-AUART.
DATA : CTU_PARAMS LIKE CTU_PARAMS.
*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK S1 WITH FRAME TITLE C010.
PARAMETERS :   P_BUKRS LIKE ANLA-BUKRS MEMORY ID BUK,
               P_GJAHR LIKE IMPR-GJAHR DEFAULT
                           SY-DATUM+0(4) OBLIGATORY,
               P_MONTH LIKE BKPF-MONAT DEFAULT
                           SY-DATUM+4(2) OBLIGATORY.
SELECTION-SCREEN END OF BLOCK S1.

SELECTION-SCREEN BEGIN OF BLOCK S2 WITH FRAME TITLE C020.
*--2004/02/12
*sELECT-OPTIONS: s_aufnr FOR aufk-aufnr.
SELECT-OPTIONS:  S_AUART FOR AUFK-AUART, "OBLIGATORY,
                 S_AUFNR FOR  COAS-AUFNR MATCHCODE OBJECT ORDE,
                 S_AUTYP FOR AUFK-AUTYP DEFAULT '30'.
SELECTION-SCREEN END OF BLOCK S2.
PARAMETERS : P_CHK1 AS CHECKBOX DEFAULT 'X',
             P_CHK2 AS CHECKBOX DEFAULT 'X'.
*SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME.
*PARAMETERS : p_test AS CHECKBOX MODIF ID bl2.        " Test Run
*SELECTION-SCREEN END OF BLOCK bl2.
*
PARAMETERS : P_MODE LIKE  CTU_PARAMS-DISMODE DEFAULT 'N' NO-DISPLAY.
"FP_MODE.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
PARAMETER P_VARI TYPE SLIS_VARI.
SELECTION-SCREEN END OF BLOCK B2.

*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = 'P_ACT'
            VALUES = IT_VAL.
*----------------------------------------------------------------------
*
* AT SELECTION-SCREEN ON VALUE-REQUEST
*
*----------------------------------------------------------------------
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM F4_VARIANT CHANGING P_VARI.
*----------------------------------------------------------------------
*
* INITIALIZATION
*
*----------------------------------------------------------------------
*
INITIALIZATION.
* ==> Change Variant saving type
  WA_VAR_SAVE = 'A'.
* ==> Change first mode   GRID or LIST
  WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
*  wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : GT_FIELDCAT.
  CLEAR   : GS_LAYOUT.
*--title set
  C010 = 'Select option'.
  WA_REPID = SY-REPID.
*--Ranges
*  CLEAR : r_auart, r_auart[].
*  MOVE : 'I'    TO   r_auart-sign,
*         'BT'   TO   r_auart-option,
*         'PM01' TO   r_auart-low,
*         'PM04' TO   r_auart-high.
*  APPEND r_auart.
*  CLEAR  r_auart.
*  MOVE : 'I'    TO   r_auart-sign,
*         'EQ'   TO   r_auart-option,
*         'QM10' TO   r_auart-low,
*         'QM10' TO   r_auart-high.
*  APPEND r_auart.
*  CLEAR  r_auart.
*  MOVE : 'I'    TO   r_auart-sign,
*         'EQ'   TO   r_auart-option,
*         'QM20' TO   r_auart-low,
*         'QM20' TO   r_auart-high.
*  APPEND r_auart.
*  CLEAR  r_auart.
*---------------------------------------------------------------------
*    M   A   I   N
*---------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM SELECT_DATA.

  IF GT_OUT[] IS INITIAL.
    MESSAGE S000(ZMFI) WITH 'No found data '.
    G_ERROR = 'X'.
    EXIT.
  ENDIF.

END-OF-SELECTION.

  CHECK G_ERROR EQ SPACE .
  PERFORM SET_OUTPUT .

****  PERFORM BUILD_FIELD_CATEGORY
****  USING :
****   'AUFNR'  'Order#'        '12' 'X' 'L'  ' '  ' '  '  ' '  ' '',
****   'BALAN'  'Order balance' '12' ' ' 'R'  ' '  ' '  '  ' '  ' '',
****   'KTEXT'  'Description'   '16' ' ' 'L'  ' '  ' '  '  ' '  ' '',
****   'TPLNR'  'Func locat'    '10' ' ' 'L'  ' '  ' '  '  ' '  ' '',
****   'EQUNR'  'Equipment'     '10' ' ' 'L'  ' '  ' '  '  ' '  ' '',
****   'KOSTV'  'CCtr'          '06' ' ' 'L'  ' '  ' '  '  ' '  ' '',
****   'ANLNR'  'Asset Number'  '10' ' ' 'L'  ' '  ' '  '  ' '  ' '',
****   'KOSTL'  'Setl.CC'       '06' ' ' 'L'  ' '  ' '  '  ' '  ' '',
****   'ANSWL'  'Asset Value'   '12' ' ' 'R'  ' '  ' '  '  ' '  ' '',
****   'RATE'   'Rate'          '5'  ' ' 'R'  ' '  ' '  '  ' '  ' '',
****   '$KOSTL' 'S.CC'          '6'  ' ' 'L'  ' '  ' '  '  ' '  ' '',
****   'ICON'   'F'             '2'  ' ' 'L'  ' '  ' '  '  ' '  ' ''.
****
****  REFRESH GT_SORTS.
****  PERFORM BUILD_SORT_TABLE
****    USING :
****       '1'    'AUFNR'   'X'   'X'   '*'.
****
****  PERFORM SET_VARIANT CHANGING WA_VAR.
****  PERFORM SET_LAYOUT CHANGING GS_LAYOUT.
****  PERFORM SET_EVENTS CHANGING GT_EVENTS.
****  PERFORM SET_BUILD_EVENT.
****  PERFORM COMMENT_BUILD USING  W_TOP_OF_PAGE[].
****
****  CALL FUNCTION WA_ALV_FUNCTION_NAME
****       EXPORTING
****            I_CALLBACK_PROGRAM       = WA_REPID
****            I_CALLBACK_PF_STATUS_SET = 'ALV_EVENT_PF_STATUS_SET'
****            I_CALLBACK_USER_COMMAND  = 'ALV_EVENT_USER_COMMAND'
****            IS_LAYOUT                = GS_LAYOUT
****            IT_FIELDCAT              = GT_FIELDCAT[]
****            IT_SPECIAL_GROUPS        = GT_SP_GROUP[]
****            IT_SORT                  = GT_SORTS[]
****            I_DEFAULT                = WA_DEFAULT
****            I_SAVE                   = WA_VAR_SAVE
****            IS_VARIANT               = WA_VAR
****            IT_EVENTS                = W_EVENTCAT[]
****            IS_PRINT                 = GS_PRNT
****       TABLES
****            T_OUTTAB                 = GT_OUT.
***********************************************************************
*

*&---------------------------------------------------------------------
*&      Form  select_data
*&---------------------------------------------------------------------
FORM SELECT_DATA.
  CLEAR G_ERROR.
  PERFORM SELECT_ORDER_BALANCE.

  DESCRIBE TABLE IT_COSP LINES WA_T_CNT.
  CHECK WA_T_CNT > 0.

*----2004/03/23
**-----STATUS CHECK
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_jest
*    FROM jest
*    FOR ALL ENTRIES IN it_cosp
*    WHERE objnr = it_cosp-objnr
*    AND   inact <> 'X'
*    AND   stat  = 'I0045'.       " Status text = 'teco'  complete

* by IG.MOON 10/2007 - delete logic for setl.rule{
*  perform read_existing_settle_rule.
* }

*PLEASE FIX IT - TOO SLOW....
  PERFORM SELECT_PM_MASTERS.

  PERFORM GET_ASSET_VALUE.
  PERFORM GET_INVESTMENT_ORDER.

  PERFORM MAKE_ITAB.

*---sum by work order    n
*  REFRESH : GT_SUM.
*  CLEAR   : GT_SUM.
  DATA : WA_ZERO.
  CLEAR : WA_ZERO.

*------rate

*  SORT GT_SUM BY AUFNR.

*  SELECT COUNT(*) INTO WA_CNT
*  FROM COBRB
*  WHERE OBJNR = WA_OBJNR
*  AND   GABJA = P_GJAHR.

* by IG.MOON 10/2007 {

*  CLEAR : IT_IS_SET[], IT_IS_SET.
*
*  LOOP AT GT_OUT.
*    $ORDER-AUFNR = GT_OUT-AUFNR.
*    COLLECT $ORDER.
*  ENDLOOP.
*
*  SORT $ORDER.
*  DELETE ADJACENT DUPLICATES FROM $ORDER.
*
*  LOOP AT $ORDER.
*    CALL FUNCTION 'QRP_APO_PKOSA_AUFNR_TO_OBJNR'
*         EXPORTING
*              IF_PKOSA_AUFNR = $ORDER-AUFNR
*         IMPORTING
*              EF_PKOSA_OBJNR = $ORDER-OBJNR.
*    MODIFY $ORDER.
*  ENDLOOP.
*
*  SELECT COUNT(*) INTO WA_CNT
*  FROM COBRB
*  WHERE OBJNR = WA_OBJNR
*  AND   GABJA = P_GJAHR.
*
* }

*  SELECT COUNT(*) INTO WA_CNT
*  FROM COBRB
*  WHERE OBJNR = WA_OBJNR
*  AND   GABJA = P_GJAHR.

  LOOP AT GT_OUT.

    CLEAR WA_CNT.
*    PERFORM chk_settlement USING wa_cnt gt_out-aufnr gt_out-posid
*                                 gt_out-kostv.
    IF WA_CNT > 0.
      MOVE 'O' TO GT_OUT-ST.
    ELSE.
      MOVE ' ' TO GT_OUT-ST.
    ENDIF.
    IF GT_OUT-AUART = 'PM05'.
      MOVE 'X' TO GT_OUT-ST.
    ENDIF.
*---2004/03/16
    IF P_CHK1 = 'X' AND P_CHK2 = ' '.
      IF GT_OUT-ST = ' '.
        DELETE GT_OUT INDEX SY-TABIX.
        CONTINUE.
      ENDIF.
    ELSEIF P_CHK2 = 'X' AND P_CHK1 = ' '.
      IF GT_OUT-ST = 'O'.
        DELETE GT_OUT INDEX SY-TABIX.
        CONTINUE.
      ENDIF.
    ENDIF.

*    READ TABLE GT_SUM WITH KEY AUFNR = GT_OUT-AUFNR BINARY SEARCH.
*    IF SY-SUBRC = 0.
**====2004/03/15
*      IF GT_SUM-CNT = 1.
*        GT_OUT-RATE = 100.
*        MODIFY GT_OUT.
*        CONTINUE.
*      ENDIF.
*
*      IF GT_SUM-ANSWL = 0.
*        WA_ZERO = 'X'.
*        GT_OUT-RATE = 0.
*      ELSE.
*        WA_ZERO = ' '.
*        GT_OUT-RATE = GT_OUT-ANSWL / GT_SUM-ANSWL * 100.
*      ENDIF.
*    ENDIF.

    IF GT_OUT-$KOSTL IS INITIAL. " Create Rule
      GT_OUT-$FLAG = 'N'.
    ELSE.
      IF GT_OUT-$KOSTL EQ GT_OUT-KOSTL. " Do not need to process BDC
        GT_OUT-$FLAG = 'O'.
      ELSE.                   " Change
        GT_OUT-$FLAG = 'N'.
      ENDIF.
    ENDIF.

    IF GT_OUT-KOSTL IS INITIAL. " Create Rule
        GT_OUT-$FLAG = 'E'.
    ENDIF.

    CASE GT_OUT-$FLAG.
      WHEN 'N'.
        GT_OUT-ICON = ICON_LED_YELLOW.
      WHEN 'O'.
        GT_OUT-ICON = ICON_LED_GREEN.
      WHEN 'E'.
        GT_OUT-ICON = ICON_LED_RED.
    ENDCASE.

    MODIFY GT_OUT.

    AUFNR = GT_OUT-AUFNR.
    OBJNR = GT_OUT-OBJNR.
    KTEXT = GT_OUT-KTEXT.
    KOSTV = GT_OUT-KOSTV.
    TPLNR = GT_OUT-TPLNR.
    EQUNR = GT_OUT-EQUNR.
    ANLNR = GT_OUT-ANLNR.     "asset
    KOSTL = GT_OUT-KOSTL.
    ANSWL = GT_OUT-ANSWL.     "asset value
*        rate(5) TYPE p DECIMALS 2,
*    eaufn = gt_out-eaufn.
*    posid = gt_out-posid.
*    io    = gt_out-io.
*    st    = gt_out-st.

    AT END OF AUFNR.
      IF WA_ZERO = 'X'.
        GT_OUT-AUFNR = AUFNR.
        GT_OUT-OBJNR = OBJNR.
        GT_OUT-KTEXT = KTEXT.
        GT_OUT-KOSTV = KOSTV.
        GT_OUT-TPLNR = TPLNR.
        GT_OUT-RATE = 100.
        GT_OUT-EQUNR = EQUNR.
        GT_OUT-ANLNR = ANLNR.     "asset
        GT_OUT-KOSTL = KOSTL.
        GT_OUT-ANSWL = ANSWL.     "asset value

        MODIFY GT_OUT.
      ENDIF.
    ENDAT.

  ENDLOOP.

  SORT GT_OUT BY AUFNR TPLNR EQUNR.

ENDFORM.                    " select_data

*---------------------------------------------------------------------*
*  FORM alv_event_pf_status_set
*---------------------------------------------------------------------*
FORM PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  CLEAR: RT_EXTAB. REFRESH RT_EXTAB.
                                                            "#EC *
  IF WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
    SET PF-STATUS 'STANDARD' EXCLUDING RT_EXTAB.
  ELSE.
    SET PF-STATUS 'STANDARD' EXCLUDING RT_EXTAB.
  ENDIF.
  SET TITLEBAR  'STANDARD'.

ENDFORM.                    "alv_event_pf_status_set

*---------------------------------------------------------------------*
*  FORM alv_event_user_command
*---------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM     LIKE SY-UCOMM
                                      RS_SELFIELD TYPE SLIS_SELFIELD.
                                                            "#EC *

  DATA $FLAG(1).

  CASE R_UCOMM.
*   ---------------------------------- processing on double click.
    WHEN '&IC1'.
      READ TABLE GT_OUT INDEX RS_SELFIELD-TABINDEX.
      CASE RS_SELFIELD-FIELDNAME.
        WHEN 'AUFNR'.
          SET PARAMETER ID 'ANR' FIELD GT_OUT-AUFNR.
          CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN.
        WHEN 'TPLNR'.
          SET PARAMETER ID 'IFL' FIELD GT_OUT-TPLNR.
          CALL TRANSACTION 'IL03' AND SKIP FIRST SCREEN.
        WHEN 'EQUNR'.
          SET PARAMETER ID 'EQN' FIELD GT_OUT-EQUNR.
          CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN.
        WHEN 'ANLNR'.
          SET PARAMETER ID 'AN1' FIELD GT_OUT-ANLNR.
          SET PARAMETER ID 'BUK' FIELD P_BUKRS.
          CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
      ENDCASE.
    WHEN '&SETT'.
      PERFORM POPUP_SCREEN USING WA_CONFIRM.
      IF WA_CONFIRM = 'J'.

        CLEAR IT_DUMP[].
        LOOP AT GT_OUT WHERE CHKBOX = 'X'.
          IF GT_OUT-RATE = 0.
            CONTINUE.
          ENDIF.
          MOVE-CORRESPONDING GT_OUT TO IT_DUMP.
          APPEND IT_DUMP.
          CLEAR  IT_DUMP.
        ENDLOOP.

        REFRESH : GT_OUT1.

        LOOP AT GT_OUT WHERE CHKBOX = 'X'.
          MOVE-CORRESPONDING GT_OUT TO  GT_OUT1.
          APPEND GT_OUT1.
        ENDLOOP.

        DATA L_MSG  LIKE CFGNL-MSGLIN.
        DATA $IX LIKE SY-TABIX.

* Total Doc. Count to be created.
        DATA  : TOTAL_DOC_CNT TYPE I,
                CURRENT_DOC_CNT TYPE I.
        DATA : PERCENTAGE TYPE P,$MOD TYPE I,
              $PROG_TEXT(50),$CURRENT_CNT(10),$TOTAL_CNT(10),$TEXT(30) .

        DESCRIBE TABLE GT_OUT1 LINES TOTAL_DOC_CNT.
        $TOTAL_CNT = TOTAL_DOC_CNT.
        CLEAR CURRENT_DOC_CNT.

        LOOP AT GT_OUT1.
          CHECK GT_OUT1-$FLAG NE : 'E','O'.

          $IX = SY-TABIX.
          AUFNR = GT_OUT1-AUFNR.
          OBJNR = GT_OUT1-OBJNR.
          KOSTL = GT_OUT1-KOSTL.
          $KOSTL = GT_OUT1-$KOSTL.
          RATE  = GT_OUT1-RATE.
* // progress bar
          ADD 1 TO CURRENT_DOC_CNT.
          $CURRENT_CNT = CURRENT_DOC_CNT.
          CONCATENATE $CURRENT_CNT '/' $TOTAL_CNT
          INTO $TEXT.
          CONDENSE $TEXT.
          CONCATENATE 'posting...' $TEXT INTO $PROG_TEXT.
          PERCENTAGE = CURRENT_DOC_CNT / TOTAL_DOC_CNT * 100.
          PERFORM SHOW_PROGRESS USING $PROG_TEXT PERCENTAGE.
* //
          CLEAR $FLAG.
          PERFORM CREATE_BDC_SETT USING AUFNR
                                        POSID
                                        OBJNR
                                        KOSTL
                                       $KOSTL
                                        RATE
                              CHANGING $FLAG.

          CHECK $FLAG EQ SPACE.
          CALL FUNCTION 'RKC_MSG_STRING'
               EXPORTING
                    ID      = SY-MSGID
                    MTYPE   = SY-MSGTY
                    NUMBER  = SY-MSGNO
                    PAR1    = SY-MSGV1
                    PAR2    = SY-MSGV2
                    PAR3    = SY-MSGV3
                    PAR4    = SY-MSGV4
               IMPORTING
                    MSG_LIN = L_MSG
               EXCEPTIONS
                    OTHERS  = 1.

          READ TABLE IT_MESSTAB WITH KEY MSGTYP = 'S'
                                         MSGID = 'IW'
                                         MSGNR = '085'.
          IF SY-SUBRC = 0.
            MOVE 'O' TO GT_OUT1-ST.
            MOVE GT_OUT1-KOSTL TO GT_OUT1-$KOSTL.
            GT_OUT1-ICON = ICON_LED_GREEN..
            GT_OUT1-$MSG = L_MSG. "'ok'.
          ELSE.
            MOVE 'X' TO GT_OUT1-ST.
            GT_OUT1-ICON = ICON_LED_RED.
            GT_OUT1-$MSG = L_MSG.
          ENDIF.

          MODIFY  GT_OUT1 INDEX $IX.
          RS_SELFIELD-REFRESH = 'X'.

        ENDLOOP.

        LOOP AT GT_OUT1.
          READ TABLE GT_OUT WITH KEY AUFNR = GT_OUT1-AUFNR.
          IF SY-SUBRC EQ 0.
            GT_OUT = GT_OUT1.
            MODIFY GT_OUT INDEX SY-TABIX.
          ENDIF.
        ENDLOOP.

      ENDIF.
    WHEN 'LIST' OR 'GRID'.
      PERFORM SWITCH_LIST_OR_GRID USING R_UCOMM.
  ENDCASE.

  CHECK R_UCOMM EQ 'LIST' OR
        R_UCOMM EQ 'GRID'.

* by IG.MOON 10/2007 {
*  RS_SELFIELD-EXIT = 'X'.
*  RS_SELFIELD-COL_STABLE = 'X'.
*  RS_SELFIELD-ROW_STABLE = 'X'.
*
  CALL FUNCTION 'REUSE_ALV_GRID_LAYOUT_INFO_SET'
       EXPORTING
            IS_LAYOUT   = GS_LAYOUT
            IT_FIELDCAT = GT_FIELDCAT
            IT_SORT     = GT_SORT.

  RS_SELFIELD-REFRESH    = 'X'.
  RS_SELFIELD-ROW_STABLE = 'X'.
  RS_SELFIELD-COL_STABLE = 'X'.
* }

ENDFORM.                    "alv_event_user_command
*&---------------------------------------------------------------------
*&      Form  set_variant
*&---------------------------------------------------------------------
FORM SET_VARIANT CHANGING CS_VARI TYPE DISVARIANT.

  CHECK P_VARI NE SPACE.

  CS_VARI-REPORT      = SY-REPID.
  CS_VARI-HANDLE      = SPACE.
  CS_VARI-LOG_GROUP   = SPACE.
  CS_VARI-USERNAME    = SPACE.
  CS_VARI-VARIANT     = P_VARI.
  CS_VARI-TEXT        = SPACE.
  CS_VARI-DEPENDVARS  = SPACE.

ENDFORM.                    " set_variant

*&---------------------------------------------------------------------
*&      Form  set_events
*&---------------------------------------------------------------------
FORM SET_EVENTS CHANGING CT_EVENTS TYPE SLIS_T_EVENT.

  FIELD-SYMBOLS: <LS_EVENT> TYPE SLIS_ALV_EVENT.

  DATA: L_EVENT TYPE LVC_FNAME.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE     = 0
       IMPORTING
            ET_EVENTS       = CT_EVENTS
       EXCEPTIONS
            LIST_TYPE_WRONG = 1
            OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    DELETE CT_EVENTS WHERE NAME NE 'END_OF_PAGE'
                       AND NAME NE 'TOP_OF_PAGE'
                       AND NAME NE 'TOP_OF_LIST'
                       AND NAME NE 'END_OF_LIST'.
    LOOP AT CT_EVENTS ASSIGNING <LS_EVENT>.
      CONCATENATE 'ALV_EVENT_'
                  <LS_EVENT>-NAME
                  INTO <LS_EVENT>-FORM.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " f01_set_evts


*&---------------------------------------------------------------------
*&      Form  set_layout
*&---------------------------------------------------------------------
FORM SET_LAYOUT CHANGING CS_LAYO TYPE SLIS_LAYOUT_ALV.

*... Display options
  CS_LAYO-COLWIDTH_OPTIMIZE      = 'X'.
*****  "?????
  CS_LAYO-NO_COLHEAD             = SPACE.
  CS_LAYO-NO_HOTSPOT             = SPACE.
  CS_LAYO-ZEBRA                  = ' '.
  CS_LAYO-NO_VLINE               = SPACE.
  CS_LAYO-CELL_MERGE             = SPACE.
  CS_LAYO-NO_MIN_LINESIZE        = SPACE.
  CS_LAYO-MIN_LINESIZE           = SPACE.
  CS_LAYO-MAX_LINESIZE           = SPACE.
  CS_LAYO-WINDOW_TITLEBAR        = SPACE.
  CS_LAYO-NO_ULINE_HS            = SPACE.
*... Edit
  CS_LAYO-EDIT                   = ' '."space.
  CS_LAYO-EDIT_MODE              = ' '."space.

*... Exceptions
*  CS_LAYO-LIGHTS_FIELDNAME       = 'LIGHT'.
*  CS_LAYO-LIGHTS_TABNAME         = SPACE.
*  CS_LAYO-LIGHTS_ROLLNAME        = SPACE.
*  CS_LAYO-LIGHTS_CONDENSE        = SPACE.

*... Sums
  CS_LAYO-NO_SUMCHOICE           = SPACE.
  CS_LAYO-NO_TOTALLINE           = SPACE.
  CS_LAYO-TOTALS_BEFORE_ITEMS    = SPACE.
  CS_LAYO-TOTALS_ONLY            = SPACE.
  CS_LAYO-TOTALS_TEXT            = SPACE.
  CS_LAYO-NO_SUBCHOICE           = SPACE.
  CS_LAYO-NO_SUBTOTALS           = SPACE.
  CS_LAYO-SUBTOTALS_TEXT         = SPACE.
  CS_LAYO-NUMC_SUM               = 'X'.
  CS_LAYO-NO_UNIT_SPLITTING      = SPACE.
*... Interaction
  CS_LAYO-BOX_FIELDNAME          = 'CHKBOX'.
  CS_LAYO-BOX_TABNAME            = SPACE.
  CS_LAYO-BOX_ROLLNAME           = SPACE.
  CS_LAYO-EXPAND_FIELDNAME       = SPACE.
  CS_LAYO-HOTSPOT_FIELDNAME      = SPACE.
  CS_LAYO-NO_INPUT               = ' '.
  CS_LAYO-F2CODE                 = SPACE.
  CS_LAYO-CONFIRMATION_PROMPT    = SPACE.
  CS_LAYO-KEY_HOTSPOT            = SPACE.
  CS_LAYO-FLEXIBLE_KEY           = SPACE.
  CS_LAYO-REPREP                 = SPACE.
  CS_LAYO-GROUP_BUTTONS          = 'X'.
  CS_LAYO-NO_KEYFIX              = SPACE.
  CS_LAYO-GET_SELINFOS           = SPACE.
  CS_LAYO-GROUP_CHANGE_EDIT      = 'X'.
  CS_LAYO-NO_SCROLLING           = SPACE.
  CS_LAYO-EXPAND_ALL             = SPACE.
  CS_LAYO-NO_AUTHOR              = SPACE.
*... Detailed screen
  CS_LAYO-DETAIL_POPUP           = 'X'.
  CS_LAYO-DETAIL_INITIAL_LINES   = SPACE.
  CS_LAYO-DETAIL_TITLEBAR        = SPACE.
*... PF-status
  CS_LAYO-DEF_STATUS             = SPACE.
*... Display variants
  CS_LAYO-HEADER_TEXT            = SPACE.
  CS_LAYO-ITEM_TEXT              = SPACE.
  CS_LAYO-DEFAULT_ITEM           = SPACE.
*... colour
  CS_LAYO-INFO_FIELDNAME         = SPACE.
  CS_LAYO-COLTAB_FIELDNAME       = 'TABCOLOR'.
*... others
  CS_LAYO-LIST_APPEND            = SPACE.

ENDFORM.                    " set_layout


*---------------------------------------------------------------------*
*  FORM f01_alv_event_top_of_page
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE.                                           "#EC CALLED
*  WRITE : /(10) 'nvestment Program' , p_prnam.
*          /(10) 'BBBBBBB',  BKPF-BUKRS INVERSE COLOR 1 INPUT ON,
*           (20) 'CCCCCCC',  BKPF-BELNR INPUT ON.
ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*       FORM alv_event_top_of_LIST                                    *
*---------------------------------------------------------------------*
FORM ALV_EVENT_TOP_OF_LIST.                                 "#EC CALLED


ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_page
*---------------------------------------------------------------------*
FORM ALV_EVENT_END_OF_PAGE.
*  NEW-LINE.
*  ULINE.
*  DATA: l_page(10).
*  WRITE : sy-pagno TO l_page.
*  WRITE: /(120) l_page CENTERED.
*
ENDFORM.                    "alv_event_end_of_page

*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_list
*---------------------------------------------------------------------*
FORM ALV_EVENT_END_OF_LIST.


ENDFORM.                    "alv_event_end_of_list
*&---------------------------------------------------------------------*
*&      Form  dispaly_heager
*----------------------------------------------------------------------*
FORM DISPLAY_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
         EXPORTING
*           i_logo             = 'Z_HYUNDAI_LOGO'
*           i_logo             = 'ENJOYSAP_LOGO'
              IT_LIST_COMMENTARY = W_TOP_OF_PAGE.
ENDFORM.                    " top_of_page

*&---------------------------------------------------------------------
*&      Form  switch_list_or_grid
*&---------------------------------------------------------------------
FORM SWITCH_LIST_OR_GRID USING R_UCOMM.

  DATA: LS_VARI      TYPE DISVARIANT,
       LS_SLIS_LAYO TYPE SLIS_LAYOUT_ALV,
       LT_SLIS_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       LT_SLIS_SORT TYPE SLIS_T_SORTINFO_ALV,
       LT_SLIS_FILT TYPE SLIS_T_FILTER_ALV,
       LS_SLIS_PRNT TYPE SLIS_PRINT_ALV.


  IF R_UCOMM = 'LIST' AND
     WA_ALV_FUNCTION_NAME = 'REUSE_ALV_LIST_DISPLY'.
    EXIT.
  ENDIF.
  IF R_UCOMM = 'GRID' AND
     WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
    EXIT.
  ENDIF.
  CASE WA_ALV_FUNCTION_NAME.
    WHEN 'REUSE_ALV_LIST_DISPLAY'.
      WA_ALV_GET_INFO_NAME = 'REUSE_ALV_LIST_LAYOUT_INFO_GET'.
    WHEN 'REUSE_ALV_GRID_DISPLAY'.
      WA_ALV_GET_INFO_NAME = 'REUSE_ALV_GRID_LAYOUT_INFO_GET'.

  ENDCASE.

  CALL FUNCTION WA_ALV_GET_INFO_NAME
       IMPORTING
            ES_LAYOUT     = LS_SLIS_LAYO
            ET_FIELDCAT   = LT_SLIS_FCAT
            ET_SORT       = LT_SLIS_SORT
            ET_FILTER     = LT_SLIS_FILT
            ES_VARIANT    = LS_VARI
       EXCEPTIONS
            NO_INFOS      = 1
            PROGRAM_ERROR = 2
            OTHERS        = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  IF R_UCOMM = 'LIST'.
    WA_ALV_FUNCTION_NAME = 'REUSE_ALV_LIST_DISPLAY'.
    CALL FUNCTION WA_ALV_FUNCTION_NAME
         EXPORTING
              I_CALLBACK_PROGRAM       = WA_REPID
              I_CALLBACK_PF_STATUS_SET = 'ALV_EVENT_PF_STATUS_SET'
              I_CALLBACK_USER_COMMAND  = 'ALV_EVENT_USER_COMMAND'
              IS_LAYOUT                = LS_SLIS_LAYO
              IT_FIELDCAT              = LT_SLIS_FCAT
              IT_SORT                  = LT_SLIS_SORT
              IT_FILTER                = LT_SLIS_FILT
              I_DEFAULT                = ' '  "gs_test-vari_default
              I_SAVE                   = WA_VAR_SAVE
              IS_VARIANT               = LS_VARI
              IS_PRINT                 = LS_SLIS_PRNT
              IT_EVENTS                = GT_EVENTS[]
         TABLES
              T_OUTTAB                 = GT_OUT
         EXCEPTIONS
              PROGRAM_ERROR            = 1
              OTHERS                   = 2.
  ENDIF.
  IF R_UCOMM = 'GRID'.
    WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
    CALL FUNCTION WA_ALV_FUNCTION_NAME
         EXPORTING
              I_CALLBACK_PROGRAM       = WA_REPID
              I_CALLBACK_PF_STATUS_SET = 'ALV_EVENT_PF_STATUS_SET'
              I_CALLBACK_USER_COMMAND  = 'ALV_EVENT_USER_COMMAND'
              IS_LAYOUT                = LS_SLIS_LAYO
              IT_FIELDCAT              = LT_SLIS_FCAT
              IT_SORT                  = LT_SLIS_SORT
              IT_FILTER                = LT_SLIS_FILT
              I_DEFAULT                = ' '  "gs_test-vari_default
              I_SAVE                   = WA_VAR_SAVE
              IS_VARIANT               = LS_VARI
              IS_PRINT                 = LS_SLIS_PRNT
*                it_events               = gt_events[]
         TABLES
              T_OUTTAB                 = GT_OUT
         EXCEPTIONS
              PROGRAM_ERROR            = 1
              OTHERS                   = 2.

  ENDIF.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " switch_list_or_grid
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM BUILD_FIELD_CATEGORY USING
                                  P_FIELDNAME       " field name
                                  P_TITLE           " field title
                                  P_OUTPUTLEN       " length
                                  P_KEY             "
                                  P_JUST            "
                                  P_NOOUT           "
                                  P_EDIT            "
                                  P_CFIELD          " currency field nam
                                  P_QFIELD          " quantity field nam
                                  P_DATA_TYPE
                                  .

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = P_FIELDNAME.
  LS_FIELDCAT-SELTEXT_L = P_TITLE.
  LS_FIELDCAT-OUTPUTLEN = P_OUTPUTLEN.
  LS_FIELDCAT-KEY       = P_KEY.
  LS_FIELDCAT-JUST      = P_JUST.
  LS_FIELDCAT-EDIT      = P_EDIT.
  LS_FIELDCAT-NO_OUT     = P_NOOUT.
  LS_FIELDCAT-CFIELDNAME = P_CFIELD.
  LS_FIELDCAT-QFIELDNAME = P_QFIELD.

  APPEND LS_FIELDCAT TO GT_FIELDCAT.

ENDFORM.                    " fill_field_category

*&---------------------------------------------------------------------*
*&      Form  f4_variant
*&---------------------------------------------------------------------*
FORM F4_VARIANT CHANGING C_VARIANT TYPE DISVARIANT-VARIANT.

  DATA: LS_VARIANT TYPE DISVARIANT,
        L_EXIT     TYPE CHAR1.

  LS_VARIANT-REPORT = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT          = LS_VARIANT
            I_SAVE              = 'A'
*           it_default_fieldcat =
       IMPORTING
            E_EXIT              = L_EXIT
            ES_VARIANT          = LS_VARIANT
       EXCEPTIONS
            NOT_FOUND = 2.
  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF L_EXIT EQ SPACE.
      C_VARIANT = LS_VARIANT-VARIANT.
    ENDIF.
  ENDIF.

ENDFORM.                    " f4_variant
***&--------------------------------------------------------------------
*-
***
***&      Form  build_sort_table
***&--------------------------------------------------------------------
*-
***
**FORM BUILD_SORT_TABLE USING  P_SPOS
**                             P_FIELDNAME
**                             P_UP
**                             P_SUBTOT
**                             P_GROUP.
**  DATA: LS_SORT TYPE SLIS_SORTINFO_ALV.
**
**  LS_SORT-SPOS      = P_SPOS.
**  LS_SORT-FIELDNAME = P_FIELDNAME.
**  LS_SORT-UP        = P_UP.
**  LS_SORT-SUBTOT    = P_SUBTOT.
**  LS_SORT-GROUP     = P_GROUP.
**  APPEND LS_SORT TO GT_SORTS.
**ENDFORM.                    " build_sort_table
***&--------------------------------------------------------------------
*-
***
***&      Form  set_line_color
***&--------------------------------------------------------------------
*-
***
**FORM SET_LINE_COLOR USING    P_COLOR.
**  DATA: LS_FIELDCAT   TYPE SLIS_FIELDCAT_ALV,
**        LT_COLOR      TYPE SLIS_T_SPECIALCOL_ALV,
**        LS_COLOR      TYPE SLIS_SPECIALCOL_ALV.
**
**  REFRESH LT_COLOR.
**  CLEAR   LT_COLOR.
**  LOOP AT GT_FIELDCAT INTO LS_FIELDCAT.
**    LS_COLOR-FIELDNAME = LS_FIELDCAT-FIELDNAME.
**    LS_COLOR-COLOR-COL = P_COLOR.
***    "cl_gui_resources=>list_col_positive.
**    LS_COLOR-COLOR-INT = CL_GUI_RESOURCES=>LIST_INTENSIFIED.
**    LS_COLOR-COLOR-INV = 0.
**    LS_COLOR-NOKEYCOL  = 'X'.
**    APPEND LS_COLOR TO LT_COLOR.
**    GT_OUT-TABCOLOR = LT_COLOR.
**  ENDLOOP.
**
**ENDFORM.                    " set_line_color
*&---------------------------------------------------------------------*
*&      Form  build_field_category1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0634   text
*      -->P_0635   text
*      -->P_0636   text
*      -->P_0637   text
*      -->P_0638   text
*      -->P_0639   text
*      -->P_0640   text
*      -->P_0641   text
*      -->P_0642   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATEGORY1 USING
                                  P_FIELDNAME       " field name
                                  P_TITLE           " field title
                                  P_OUTPUTLEN       " length
                                  P_KEY             "
                                  P_JUST            "
                                  P_NOOUT           "
                                  P_EDIT            "
                                  P_CFIELD          " currency field nam
                                  P_QFIELD          " quantity field nam
                                  .

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = P_FIELDNAME.
  LS_FIELDCAT-SELTEXT_L = P_TITLE.
  LS_FIELDCAT-OUTPUTLEN = P_OUTPUTLEN.
  LS_FIELDCAT-KEY       = P_KEY.
  LS_FIELDCAT-JUST      = P_JUST.
  LS_FIELDCAT-EDIT      = P_EDIT.
  LS_FIELDCAT-NO_OUT     = P_NOOUT.
  LS_FIELDCAT-CFIELDNAME = P_CFIELD.
  LS_FIELDCAT-QFIELDNAME = P_QFIELD.
*  if p_fieldname = 'KUNNR'.
*    ls_fieldcat-emphasize = 'C100'.
*  endif.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.
ENDFORM.                    " build_field_category1
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM COMMENT_BUILD USING  LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: LS_LINE TYPE SLIS_LISTHEADER,
          L_MANAGER(50),
          L_DATE(50),
          L_LIST(50),
          L_DSNAM LIKE T024D-DSNAM,
          L_H_DSNAM LIKE T024D-DSNAM,
          L_LDATE(10),
          L_HDATE(10).

*-------------- HEADER
*  CLEAR ls_line.
*  ls_line-typ  = 'H'.
*  ls_line-info = text-h01.     "HEADER TITLE (H001)
*  APPEND ls_line TO lt_top_of_page.
*
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Investment program : '.
*  ls_line-info = s_prnam-low.
*  APPEND ls_line TO lt_top_of_page.
**--
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Approval Year : '.
*  ls_line-info = p_ayear.
*  APPEND ls_line TO lt_top_of_page.
**--
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Position ID : '.
*  CONCATENATE   s_posid-low  ' ~'  s_posid-high INTO l_list.
*  ls_line-info = l_list.
*  APPEND ls_line TO lt_top_of_page.
*
*
ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  set_build_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_BUILD_EVENT.
  W_EVENTCAT-NAME = 'TOP_OF_PAGE'.
  W_EVENTCAT-FORM = 'DISPLAY_HEADER'.
  APPEND W_EVENTCAT.
ENDFORM.                    " set_build_event
*&---------------------------------------------------------------------*
*&      Form  make_out
*&---------------------------------------------------------------------*
FORM MAKE_OUT.

* comment outted by IG.MOON 10/08/2007 {
*

** what is this????
*
**---get asset number
*  IF IT_V_EQUI-EQUNR <> ' '.
*    READ TABLE IT_ITOB WITH KEY EQUNR = IT_V_EQUI-EQUNR
*                                TPLNR = IT_V_EQUI-TPLNR
*                                BINARY SEARCH  .
*    IF SY-SUBRC = 0.
*      MOVE IT_ITOB-ANLNR TO GT_OUT-ANLNR.
*      MOVE IT_ITOB-KOSTL TO GT_OUT-KOSTL.
**---asset value
*      READ TABLE IT_ANLC WITH KEY ANLN1 = GT_OUT-ANLNR.
*      IF SY-SUBRC = 0.
*        GT_OUT-ANSWL = IT_ANLC-ANSWL.
**        gt_out-eaufn = it_anlc-eaufn.
**---I/O
*        READ TABLE IT_AUFK WITH KEY AUFNR = IT_ANLC-EAUFN.
*        IF SY-SUBRC = 0.
**          gt_out-posid = it_aufk-posid.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*  ENDIF.
*
*  MOVE-CORRESPONDING IT_COSP TO GT_OUT.
*
**--ok
*  MOVE '3'   TO GT_OUT-LIGHT.
**---error
*  IF IT_VAFILOA-EQUNR = ' ' AND
*     IT_VAFILOA-TPLNR = ' '.
*    MOVE '2' TO GT_OUT-LIGHT.
*  ENDIF.
**--overhead order missing
*  IF GT_OUT-EAUFN = ' '.
*    MOVE '2' TO GT_OUT-LIGHT.
*  ENDIF.
**---pi missing
*  IF GT_OUT-POSID = ' '.
*    MOVE '1' TO GT_OUT-LIGHT.
*  ENDIF.
*  APPEND GT_OUT.
*  CLEAR : GT_OUT.

* }

ENDFORM.                    " make_out
*&---------------------------------------------------------------------*
*&      Form  popup_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CONFIRM  text
*----------------------------------------------------------------------*
FORM POPUP_SCREEN USING    U_CONFIRM.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
*   DEFAULTOPTION        = 'Y'
           TEXTLINE1            = 'Do you really want to process?'
*   TEXTLINE2            = ' '
           TITEL                = 'Yes/No'
*   START_COLUMN         = 25
*   START_ROW            = 6
*   CANCEL_DISPLAY       = 'X'
        IMPORTING
           ANSWER               = U_CONFIRM.

ENDFORM.                    " popup_screen
*&---------------------------------------------------------------------*
*&      Form  make_bdc_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2405   text
*      -->P_2406   text
*      -->P_2407   text
*----------------------------------------------------------------------*
FORM MAKE_BDC_RTN USING   DYNBEGIN PROGRAM DYNPRO.
  CLEAR IT_BDC.

  IF DYNBEGIN = 'X'.
    IT_BDC-PROGRAM  = PROGRAM.
    IT_BDC-DYNPRO   = DYNPRO.
    IT_BDC-DYNBEGIN = 'X'.
  ELSE.
    IT_BDC-FNAM     = PROGRAM.
    IT_BDC-FVAL     = DYNPRO.
  ENDIF.

  APPEND IT_BDC.

ENDFORM.                    " make_bdc_rtn
*&---------------------------------------------------------------------*
*&      Form  create_bdc_Sett
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_AUFNR  text
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
FORM CREATE_BDC_SETT USING    U_AUFNR
                              U_POSID
                              U_OBJNR
                              U_KOSTL
                              U_$KOSTL
                              U_RATE
                   CHANGING   P_FLAG.

  DATA : $BDC_TYPE(1),
         $USED TYPE I,         " Count for used setl. rules
         $ALL_RULES TYPE I.    " Count for not used setl. rules

  IF U_$KOSTL IS INITIAL.
    $BDC_TYPE = 'C'. " Create
  ELSE.
    IF U_$KOSTL EQ U_KOSTL.
      P_FLAG = 'X'.
      EXIT.
    ELSE.
      $BDC_TYPE = 'M'. " Modify
    ENDIF.
  ENDIF.

  CTU_PARAMS-RACOMMIT = ' '.
*
  CTU_PARAMS-DISMODE  = P_MODE. "'E'. "FP_MODE.
  CTU_PARAMS-UPDMODE  = 'S'. "FP_UPDATE.
  CTU_PARAMS-CATTMODE = ' '.
  CTU_PARAMS-DEFSIZE  = ' '.
  CTU_PARAMS-NOBINPT  = 'X'.
  CTU_PARAMS-NOBIEND  = 'X'.

  WRITE U_RATE TO WA_RATE NO-GAP.
  TCODE = 'IW32'.
  REFRESH : IT_BDC.
  CLEAR   : IT_BDC.
  PERFORM MAKE_BDC_RTN USING :
                     'X'  'SAPLCOIH'        '0101',
                     ' '  'CAUFVD-AUFNR'    U_AUFNR,
                     ' '  'BDC_OKCODE'      '/00'.
  PERFORM MAKE_BDC_RTN USING :
                     'X'  'SAPLCOIH'        '3000',
                     ' '  'BDC_OKCODE'      '=KOBK'.

  IF U_$KOSTL IS INITIAL. " Create Rule
    PERFORM MAKE_BDC_RTN USING :
                   'X'  'SAPLSPO2'        '0100',
                   ' '  'BDC_OKCODE'      '=OPT2'.
  ELSE.

    IF U_$KOSTL EQ U_KOSTL. " Do not need to process BDC
      P_FLAG = 'X'.
      EXIT.
    ELSE.                   " Change

      PERFORM CHK_SETTLEMENT USING U_OBJNR
                          CHANGING $USED
                                   $ALL_RULES.
      IF $USED EQ 0.
        DO $ALL_RULES TIMES.
          PERFORM MAKE_BDC_RTN USING :
                             'X'  'SAPLKOBS'        '0130',
                             ' '  'BDC_OKCODE'      '=DELL'.
        ENDDO.
      ELSE.
        PERFORM MAKE_RULE_ITEM USING $ALL_RULES.
      ENDIF.
    ENDIF.
  ENDIF.

*----2004/02/03  JHS mdify
  CLEAR :  WA_RATE1.
  LOOP AT IT_DUMP WHERE AUFNR = GT_OUT1-AUFNR.

    WRITE IT_DUMP-RATE TO WA_RATE NO-GAP.
    PERFORM MAKE_BDC_RTN USING :
                       'X'  'SAPLKOBS'        '0130',
                       ' '  'BDC_OKCODE'      '=NEUR'.
*====2004/03/15 jhs modify
*    IF IT_DUMP-POSID = ' '.
*---2004/06/25

    IF IT_DUMP-ANLNR = ' '.
      PERFORM MAKE_BDC_RTN USING :
                         'X'  'SAPLKOBS'        '0100',
                         ' '  'COBRB-GABPE'     P_MONTH,
                         ' '  'COBRB-GBISP'     ' ',
                         ' '  'COBRB-GABJA'     P_GJAHR,
                         ' '  'COBRB-PERBZ'     'CTR',
                         ' '  'COBRB-PROZS'     WA_RATE, "u_rate,
                         ' '  'COBL-KOSTL'      IT_DUMP-KOSTL,
                         ' '  'BDC_OKCODE'      '=BACK'.
    ELSE.
      PERFORM MAKE_BDC_RTN USING :
                         'X'  'SAPLKOBS'        '0100',
                         ' '  'COBRB-GABPE'     P_MONTH,
                         ' '  'COBRB-GBISP'     ' ',
                         ' '  'COBRB-GABJA'     P_GJAHR,
                         ' '  'COBRB-PERBZ'     'PER',
                         ' '  'COBRB-PROZS'     WA_RATE, "u_rate,
                         ' '  'COBL-KOSTL'      IT_DUMP-KOSTL,
                         ' '  'BDC_OKCODE'      '=BACK'.
    ENDIF.

*    ELSE.
**---2004/06/25
*      IF IT_DUMP-ANLNR = ' '.
*        PERFORM MAKE_BDC_RTN USING :
*                           'X'  'SAPLKOBS'        '0100',
*                           ' '  'COBRB-GABPE'     P_MONTH,
*                           ' '  'COBRB-GBISP'     ' ',
*                           ' '  'COBRB-GABJA'     P_GJAHR,
*                           ' '  'COBRB-PERBZ'     'CTR',
*                           ' '  'COBRB-PROZS'     WA_RATE, "u_rate,
*                           ' '  'COBL-KOSTL'      IT_DUMP-KOSTL,
*                           ' '  'BDC_OKCODE'      '=BACK'.
*      ELSE.
*        PERFORM MAKE_BDC_RTN USING :
*                           'X'  'SAPLKOBS'        '0100',
*                           ' '  'COBRB-GABPE'     P_MONTH,
*                           ' '  'COBRB-GBISP'     ' ',
*                           ' '  'COBRB-GABJA'     P_GJAHR,
*                           ' '  'COBRB-PERBZ'     'PER',
*                           ' '  'COBRB-PROZS'     WA_RATE, "u_rate,
*                           ' '  'COBL-AUFNR'      IT_DUMP-POSID,
*                           ' '  'BDC_OKCODE'      '=BACK'.
*      ENDIF.
*    ENDIF.

  ENDLOOP.

*------------------*
  PERFORM MAKE_BDC_RTN USING :
                     'X'  'SAPLKOBS'        '0130',
                     ' '  'BDC_OKCODE'      '=BACK'.
  PERFORM MAKE_BDC_RTN USING :
                     'X'  'SAPLCOIH'        '3000',
                     ' '  'BDC_OKCODE'      '=BU'.
  CALL TRANSACTION TCODE USING   IT_BDC
                            OPTIONS FROM CTU_PARAMS
                            MESSAGES INTO IT_MESSTAB.

ENDFORM.                    " create_bdc_Sett
*&---------------------------------------------------------------------*
*&      Form  CHECK_COBRB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_COBRB USING U_OBJNR.
  CLEAR : WA_D_CNT.
  SELECT SINGLE COUNT(*) INTO WA_D_CNT
  FROM COBRB
  WHERE OBJNR = U_OBJNR.

ENDFORM.                    " CHECK_COBRB
*&---------------------------------------------------------------------*
*&      Form  chk_settlement
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CNT  text
*----------------------------------------------------------------------*
FORM CHK_SETTLEMENT USING U_OBJNR
                 CHANGING P_USED
                          P_ALL_RULES .

  CLEAR : P_USED, P_ALL_RULES.

  __CLS GT_COBRB.

  SELECT  OBJNR BUREG LFDNR ERSJA LETJA EXTNR
          INTO TABLE GT_COBRB
  FROM COBRB
  WHERE OBJNR = U_OBJNR.

  LOOP AT GT_COBRB.
    ADD 1 TO P_ALL_RULES.
    IF NOT GT_COBRB-ERSJA IS INITIAL OR
       NOT GT_COBRB-LETJA IS INITIAL.
      ADD 1 TO P_USED.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " chk_settlement
*&---------------------------------------------------------------------*
*&      Form  select_order_balance
*&---------------------------------------------------------------------*
FORM SELECT_ORDER_BALANCE.

  __CLS : IT_COSP.

  " // External

  SELECT A~AUFNR A~AUART A~AUTYP A~KOSTL
         B~OBJNR A~KTEXT
         SUM( B~WKG001 ) AS WKG001
         SUM( B~WKG002 ) AS WKG002
         SUM( B~WKG003 ) AS WKG003
         SUM( B~WKG004 ) AS WKG004
         SUM( B~WKG005 ) AS WKG005
         SUM( B~WKG006 ) AS WKG006
         SUM( B~WKG007 ) AS WKG007
         SUM( B~WKG008 ) AS WKG008
         SUM( B~WKG009 ) AS WKG009
         SUM( B~WKG010 ) AS WKG010
         SUM( B~WKG011 ) AS WKG011
         SUM( B~WKG012 ) AS WKG012
  INTO CORRESPONDING FIELDS OF TABLE IT_COSP
     FROM AUFK AS A
     INNER JOIN COSP AS B
        ON A~OBJNR = B~OBJNR
  WHERE A~AUFNR IN S_AUFNR
    AND A~AUART IN S_AUART
    AND A~AUTYP IN S_AUTYP
    AND B~LEDNR = '00'
    AND B~VERSN = '000'
    AND B~WRTTP = '04'
  GROUP BY A~AUFNR A~AUART A~AUTYP A~KOSTL
           B~OBJNR A~KTEXT.

  " // Internal

  SELECT A~AUFNR A~AUART A~AUTYP A~KOSTL
         B~OBJNR A~KTEXT
         SUM( B~WKG001 ) AS WKG001
         SUM( B~WKG002 ) AS WKG002
         SUM( B~WKG003 ) AS WKG003
         SUM( B~WKG004 ) AS WKG004
         SUM( B~WKG005 ) AS WKG005
         SUM( B~WKG006 ) AS WKG006
         SUM( B~WKG007 ) AS WKG007
         SUM( B~WKG008 ) AS WKG008
         SUM( B~WKG009 ) AS WKG009
         SUM( B~WKG010 ) AS WKG010
         SUM( B~WKG011 ) AS WKG011
         SUM( B~WKG012 ) AS WKG012
  APPENDING CORRESPONDING FIELDS OF TABLE IT_COSP
     FROM AUFK AS A
     INNER JOIN COSS AS B
        ON A~OBJNR = B~OBJNR
  WHERE A~AUFNR IN S_AUFNR
    AND A~AUART IN S_AUART
    AND A~AUTYP IN S_AUTYP
    AND B~LEDNR = '00'
    AND B~VERSN = '000'
    AND B~WRTTP = '04'
  GROUP BY A~AUFNR A~AUART A~AUTYP A~KOSTL
           B~OBJNR A~KTEXT.

  LOOP AT IT_COSP.
    ADD IT_COSP-WKG001 THEN IT_COSP-WKG002
        UNTIL IT_COSP-WKG012
        TO    IT_COSP-BALAN.
    MODIFY IT_COSP.
  ENDLOOP.

  DATA $IT_COSP LIKE IT_COSP OCCURS 0 WITH HEADER LINE.

  LOOP AT IT_COSP.
    $IT_COSP = IT_COSP.
    COLLECT $IT_COSP.
  ENDLOOP.

  __CLS IT_COSP.

  IT_COSP[] = $IT_COSP[].
  DELETE IT_COSP WHERE BALAN = 0.

  READ TABLE IT_COSP INDEX 1.
  CHECK SY-SUBRC EQ 0.

  DATA: BEGIN OF $COBRB OCCURS 0,
         OBJNR LIKE COBRB-OBJNR,
         KOSTL LIKE COBRB-KOSTL,
         GABJA LIKE COBRB-GABJA,
         GABPE LIKE COBRB-GABPE,
         GBISJ LIKE COBRB-GBISJ,
         GBISP LIKE COBRB-GBISP,
        $VAL_F(6) TYPE N,
        $VAL_T(6) TYPE N,
        END OF $COBRB.
  DATA $IX LIKE SY-TABIX.

  SELECT OBJNR KOSTL GABJA GABPE GBISJ GBISP
    INTO TABLE $COBRB
    FROM COBRB
    FOR ALL ENTRIES IN IT_COSP
    WHERE OBJNR EQ IT_COSP-OBJNR.

  LOOP AT $COBRB.
    CONCATENATE $COBRB-GABJA $COBRB-GABPE INTO $COBRB-$VAL_F.
    MODIFY $COBRB INDEX SY-TABIX TRANSPORTING $VAL_F.
  ENDLOOP.

  SORT $COBRB BY OBJNR ASCENDING $VAL_F DESCENDING.

  LOOP AT IT_COSP.
    $IX = SY-TABIX.
    CLEAR IT_COSP-$KOSTL.
    READ TABLE $COBRB WITH KEY OBJNR = IT_COSP-OBJNR
                            BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_COSP-$KOSTL = $COBRB-KOSTL.
      MODIFY IT_COSP INDEX $IX TRANSPORTING $KOSTL.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " select_order_balance
*&---------------------------------------------------------------------*
*&      Form  select_pm_masters
*&---------------------------------------------------------------------*
FORM SELECT_PM_MASTERS.

* by IG.MOON 10/2007 {

*---Functional location + equipment
*  SELECT * INTO TABLE IT_VAFILOA
*    FROM VAFILOA
*    FOR ALL ENTRIES IN IT_COSP
*    WHERE AUFNR = IT_COSP-AUFNR.

  SELECT A~AUFNR A~EQUNR B~TPLNR B~KOSTL B~ANLNR
  INTO TABLE IT_VAFILOA
  FROM AFIH AS A
  JOIN ILOA AS B
  ON B~ILOAN EQ A~ILOAN
  FOR ALL ENTRIES IN IT_COSP
  WHERE A~AUFNR = IT_COSP-AUFNR
  %_HINTS ORACLE 'FIRST_ROWS(10)'.

* }

*---Functional location tree suructure equipment
  DESCRIBE TABLE IT_VAFILOA LINES WA_T_CNT.

  IF WA_T_CNT > 0.

* by IG.MOON 10/2007 {

*    SELECT * INTO TABLE IT_V_EQUI
*     FROM V_EQUI
*     FOR ALL ENTRIES IN IT_VAFILOA
*     WHERE TPLNR = IT_VAFILOA-TPLNR   "location
*       AND KOSTL = IT_VAFILOA-KOSTL   "cost center
*       AND TPLNR <> ' '
*       AND DATAB <= SY-DATUM.

*    SELECT C~EQUNR C~EQART A~TPLNR A~KOSTL  A~ANLNR
*     INTO TABLE IT_V_EQUI
*     FROM ILOA AS A
*     JOIN EQUZ AS B
*     ON B~ILOAN EQ A~ILOAN
*     JOIN EQUI AS C
*     ON C~EQUNR EQ B~EQUNR
*     FOR ALL ENTRIES IN IT_VAFILOA
*     WHERE
*           A~KOSTL = IT_VAFILOA-KOSTL   "cost center
*       AND A~TPLNR = IT_VAFILOA-TPLNR   "location
*       AND A~TPLNR <> ' '
*       AND B~DATAB <= SY-DATUM
*       %_HINTS ORACLE 'FIRST_ROWS(10) index("ILOA" "ILOA~C")'.


*    DATA: BEGIN OF $KOSTL OCCURS 0,
*            KOSTL  TYPE KOSTL,
*            TPLNR  TYPE TPLNR,
*          END OF $KOSTL.
*
*    LOOP AT IT_VAFILOA.
*      $KOSTL-KOSTL = IT_VAFILOA-KOSTL.
*      $KOSTL-TPLNR = IT_VAFILOA-TPLNR.
*      COLLECT $KOSTL.
*    ENDLOOP.
*
*    SORT $KOSTL.
*    DELETE ADJACENT DUPLICATES FROM $KOSTL.
*
*    SELECT
*      C~EQUNR C~EQART
*      A~TPLNR A~KOSTL  A~ANLNR
*     INTO CORRESPONDING FIELDS OF TABLE IT_V_EQUI
*     FROM ILOA AS A
*     JOIN EQUZ AS B
*     ON B~ILOAN EQ A~ILOAN
*     JOIN EQUI AS C
*     ON C~EQUNR EQ B~EQUNR
*     FOR ALL ENTRIES IN $KOSTL
*     WHERE
*           A~TPLNR = $KOSTL-TPLNR   "location
*       AND A~TPLNR <> ' '
*       AND A~KOSTL = $KOSTL-KOSTL   "cost center
*       AND B~DATAB <= SY-DATUM
*       %_HINTS ORACLE 'FIRST_ROWS(3)'.
*

* }

  ENDIF.

**--asset number
*  CLEAR : WA_T_CNT.
*  DESCRIBE TABLE IT_V_EQUI LINES WA_T_CNT.
*
*  IF WA_T_CNT > 0.
*
** by IG.MOON 10/2007 {
**    SELECT DISTINCT EQUNR ANLNR KOSTL TPLNR
**      INTO CORRESPONDING FIELDS OF TABLE IT_ITOB
**      FROM ITOB
**      FOR ALL ENTRIES IN IT_V_EQUI
**      WHERE EQUNR = IT_V_EQUI-EQUNR
**        AND EQART = IT_V_EQUI-EQART
**        AND TPLNR = IT_V_EQUI-TPLNR.
**
**    SELECT DISTINCT EQUNR ANLNR KOSTL TPLNR
**      APPENDING CORRESPONDING FIELDS OF TABLE IT_ITOB
**      FROM ITOB
**      FOR ALL ENTRIES IN IT_VAFILOA
**      WHERE EQUNR = IT_VAFILOA-EQUNR.
*
*    SELECT DISTINCT A~EQUNR C~ANLNR C~KOSTL C~TPLNR
*     INTO CORRESPONDING FIELDS OF TABLE IT_ITOB
*     FROM EQUI AS A
*     JOIN EQUZ AS B
*     ON B~EQUNR EQ A~EQUNR
*     JOIN ILOA AS C
*     ON C~ILOAN EQ B~ILOAN
*     FOR ALL ENTRIES IN IT_V_EQUI
*     WHERE A~EQUNR EQ IT_V_EQUI-EQUNR
*       AND A~EQART EQ IT_V_EQUI-EQART
*       AND C~TPLNR EQ IT_V_EQUI-TPLNR
*       %_HINTS ORACLE 'FIRST_ROWS(3)'.
*
*    SELECT DISTINCT A~EQUNR C~ANLNR C~KOSTL C~TPLNR
*     APPENDING CORRESPONDING FIELDS OF TABLE IT_ITOB
*     FROM EQUI AS A
*     JOIN EQUZ AS B
*     ON B~EQUNR EQ A~EQUNR
*     JOIN ILOA AS C
*     ON C~ILOAN EQ B~ILOAN
*     FOR ALL ENTRIES IN IT_VAFILOA
*     WHERE A~EQUNR = IT_VAFILOA-EQUNR.
*
** }
*
*  ENDIF.

ENDFORM.                    " select_pm_masters
*&---------------------------------------------------------------------*
*&      Form  get_asset_value
*&---------------------------------------------------------------------*
FORM GET_ASSET_VALUE.

* Has been comment outted by IG.MOON after discuss with Andy. {
* 10/08/2007

***  CLEAR : WA_T_CNT.
***  DESCRIBE TABLE IT_ITOB LINES WA_T_CNT.
***  CHECK WA_T_CNT > 0.
****---assert value, default order#
***
***  SELECT A~ANLN1 A~ANLN2 B~EAUFN
***  INTO CORRESPONDING FIELDS OF TABLE IT_ANLC
***  FROM ANLC AS A
***  INNER JOIN ANLA AS B
***    ON  A~BUKRS = B~BUKRS
***    AND A~ANLN1 = B~ANLN1
***    AND A~ANLN2 = B~ANLN2
***  FOR ALL ENTRIES IN IT_ITOB
***    WHERE A~BUKRS = P_BUKRS
***      AND A~ANLN1 = IT_ITOB-ANLNR
***      AND A~AFABE = '01'.

* }

**---I/O
*    SELECT * INTO TABLE it_anla
*    FROM anla
*    FOR ALL ENTRIES IN it_itob
*    WHERE bukrs = p_bukrs
*    AND  anln1  = it_itob-anlnr.
*    CLEAR : wa_t_cnt.
*
*    DESCRIBE TABLE it_anla LINES wa_t_cnt.


ENDFORM.                    " get_asset_value
*&---------------------------------------------------------------------*
*&      Form  get_investment_order
*&---------------------------------------------------------------------*
FORM GET_INVESTMENT_ORDER.

* Has been comment outted by IG.MOON after discuss with Andy. {
* 10/08/2007

*  DESCRIBE TABLE IT_ANLC LINES WA_T_CNT.
*
*  CHECK WA_T_CNT > 0.
*
*  SELECT A~AUFNR A~KOSTL A~OBJNR B~POSNR C~POSID
*    INTO CORRESPONDING FIELDS OF TABLE IT_AUFK
*     FROM AUFK AS A
*      INNER JOIN IMZO AS B
*         ON A~OBJNR = B~OBJNR
*      INNER JOIN IMPR AS C
*         ON B~POSNR = C~POSNR
*     FOR ALL ENTRIES IN IT_ANLC
*     WHERE A~AUFNR = IT_ANLC-EAUFN.
*
*  CLEAR : wa_t_cnt.
*  DESCRIBE TABLE it_aufk LINES sy-tabix.

* }

**---get imzo
*  IF sy-tabix > 0.
*    SELECT * INTO corresponding fields of TABLE it_imzo
*      FROM imzo as a
*      inner join impr as b
*         on a~posnr = b~posnr
*      FOR ALL ENTRIES IN it_aufk
*      WHERE a~objnr = it_cosp-objnr.
*
**    CLEAR : wa_t_cnt.
**    DESCRIBE TABLE it_cosp LINES wa_t_cnt.
***---get impr  position id
**    IF wa_t_cnt > 0.
**      SELECT * INTO TABLE it_impr
**      FROM impr
**      FOR ALL ENTRIES IN it_imzo
**      WHERE posnr = it_imzo-posnr.
**    ENDIF.
*  ENDIF.

ENDFORM.                    " get_investment_order
*&---------------------------------------------------------------------*
*&      Form  make_itab
*&---------------------------------------------------------------------*
FORM MAKE_ITAB.

  SORT : IT_VAFILOA BY AUFNR,
         IT_ITOB BY EQUNR TPLNR.

  LOOP AT IT_COSP.
    MOVE-CORRESPONDING IT_COSP TO GT_OUT.

**--get status = TECO -> ending
*    READ TABLE it_jest WITH KEY objnr = it_cosp-objnr.
*    IF sy-subrc = 0.
*      CONTINUE.
*    ENDIF.

*--functional location + equipment
    READ TABLE IT_VAFILOA WITH KEY AUFNR = IT_COSP-AUFNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      MOVE IT_VAFILOA-TPLNR TO GT_OUT-TPLNR.
      MOVE IT_VAFILOA-EQUNR TO GT_OUT-EQUNR.
      MOVE IT_VAFILOA-KOSTL TO GT_OUT-KOSTV.
      MOVE IT_VAFILOA-ANLNR TO GT_OUT-ANLNR.
      MOVE 100 TO GT_OUT-RATE.
    ENDIF.

*-----functional location <>  ' ' and equipment is ' ' ==> structure
*    IF  IT_VAFILOA-TPLNR <> ' ' AND IT_VAFILOA-EQUNR = ' '.
*      LOOP AT IT_V_EQUI WHERE TPLNR = IT_VAFILOA-TPLNR
*                       AND     KOSTL = IT_COSP-KOSTL.
*        MOVE IT_V_EQUI-EQUNR  TO GT_OUT-EQUNR.
*        MOVE IT_VAFILOA-TPLNR TO GT_OUT-TPLNR.
*        PERFORM MAKE_OUT.
*      ENDLOOP.
*      CONTINUE.
*    ENDIF.

*---get asset number
    IF IT_VAFILOA-EQUNR <> ' '.
      READ TABLE IT_ITOB WITH KEY EQUNR = IT_VAFILOA-EQUNR
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        MOVE IT_ITOB-ANLNR TO GT_OUT-ANLNR.
*        MOVE it_itob-kostl TO gt_out-kostl.

*---asset value
***        READ TABLE IT_ANLC WITH KEY ANLN1 = GT_OUT-ANLNR.
***        IF SY-SUBRC = 0.
***          MOVE IT_ANLC-ANSWL TO GT_OUT-ANSWL.
***          MOVE IT_ANLC-EAUFN TO GT_OUT-EAUFN.

* { <-------------- delete
*--get pi
**          READ TABLE IT_AUFK WITH KEY AUFNR = IT_ANLC-EAUFN.
**          IF SY-SUBRC = 0.
**            GT_OUT-POSID = IT_AUFK-POSID.
**          ENDIF.
***        ENDIF.
      ENDIF.
    ENDIF.

****-- status for order settlement
****--ok
***    MOVE '3'   TO GT_OUT-LIGHT.
****---error
***    IF IT_VAFILOA-EQUNR = ' ' AND
***       IT_VAFILOA-TPLNR = ' '.
***      MOVE '2' TO GT_OUT-LIGHT.
***    ENDIF.
****--overhead order missing
***    IF GT_OUT-EAUFN = ' '.
***      MOVE '2' TO GT_OUT-LIGHT.
***    ENDIF.
****--pi missing
***    IF GT_OUT-POSID = ' '.
***      MOVE '1' TO GT_OUT-LIGHT.
***    ENDIF.
****---convert posid.
***    CALL FUNCTION 'CONVERSION_EXIT_POSID_INPUT'
***         EXPORTING
***              INPUT  = GT_OUT-POSID
***         IMPORTING
***              OUTPUT = GT_OUT-POSID.
****--2004/06/25
***    IF GT_OUT-ANLNR = ' '.
***      GT_OUT-LIGHT = '3'.
***    ENDIF.
***
    APPEND GT_OUT.
    CLEAR  GT_OUT.
  ENDLOOP.

ENDFORM.                    " make_itab
*&---------------------------------------------------------------------*
*&      Form  read_existing_settle_rule
*&---------------------------------------------------------------------*
FORM READ_EXISTING_SETTLE_RULE.

*-get existing settlement rule
  SELECT * INTO TABLE IT_COBRB
    FROM COBRB
    FOR ALL ENTRIES IN IT_COSP
    WHERE OBJNR = IT_COSP-OBJNR.

ENDFORM.                    " read_existing_settle_rule
*&---------------------------------------------------------------------*
*&      Form  MAKE_RULE_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_RULE_ITEM USING P_ALL_RULES.

  DATA : $STRING(30),
         $RULES TYPE I,
         $PAGE TYPE I,
         $MONTH(2) TYPE N,
         $YEAR(4) TYPE N,
         $STR_RULES(10).
  IF P_MONTH EQ 1.
    $MONTH = 12.
    $YEAR = P_GJAHR - 1.
  ELSE.
    $MONTH = P_MONTH - 1.
    $YEAR = P_GJAHR.
  ENDIF.

  PERFORM MAKE_BDC_RTN USING 'X'  'SAPLKOBS'        '0130'.

  IF P_ALL_RULES >= 14.
    $RULES = P_ALL_RULES MOD 14.
    $PAGE = P_ALL_RULES / 14.
    IF $PAGE EQ 0.
      $RULES = 14.
    ENDIF.
    DO $PAGE TIMES.
      PERFORM MAKE_BDC_RTN USING ' '  'BDC_OKCODE' '=P+'.
      PERFORM MAKE_BDC_RTN USING 'X'  'SAPLKOBS' '0130'.
    ENDDO.
  ELSE.
    $RULES = P_ALL_RULES.
  ENDIF.

  $STR_RULES = $RULES.
  CONDENSE $STR_RULES.

  CONCATENATE 'COBRB-GBISP(' $STR_RULES ')' INTO $STRING.

  PERFORM MAKE_BDC_RTN USING ' '  'BDC_CURSOR' $STRING.
  PERFORM MAKE_BDC_RTN USING ' '  $STRING $MONTH.

  CONCATENATE 'COBRB-GBISJ(' $STR_RULES ')' INTO $STRING.
  PERFORM MAKE_BDC_RTN USING ' '  $STRING $YEAR.

ENDFORM.                    " MAKE_RULE_ITEM
*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_OUTPUT.
  PERFORM INIT_ALV_PARM.

***   Initialization fieldcatalog   ***
  PERFORM FIELDCAT_INIT     USING GT_FIELDCAT[].
  PERFORM SORT_BUILD        USING GT_SORT[].

  PERFORM ALV_EVENTS_GET    USING:  'P'.
  PERFORM ALV_GRID_DISPLAY  TABLES  GT_OUT USING ''.

ENDFORM.                    " SET_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_PARM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_ALV_PARM.

  __CLS   :  GT_FIELDCAT, GT_SORT, GT_EVENTS, GT_LISTHEADER,
             GT_SP_GROUP.

  CLEAR   :  GS_LAYOUT.

  PERFORM SET_LAYOUT CHANGING GS_LAYOUT.

*  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
*
*   Set variant
  GV_REPID = GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-VARIANT = P_VARI.


ENDFORM.                    " INIT_ALV_PARM
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT USING FT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV .

  DATA: L_POS       TYPE I.

  __CLS FT_FIELDCAT.

  DEFINE __CATALOG.
    L_POS = L_POS + 1.
    CLEAR GS_FIELDCAT.
    GS_FIELDCAT-COL_POS       = L_POS.
    GS_FIELDCAT-KEY           = &1.
    GS_FIELDCAT-FIELDNAME     = &2.
    GS_FIELDCAT-SELTEXT_M     = &3.        " Column heading
    GS_FIELDCAT-OUTPUTLEN     = &4.        " Column width
    GS_FIELDCAT-DATATYPE      = &5.        " Data type
    GS_FIELDCAT-EMPHASIZE     = &6.
    GS_FIELDCAT-CFIELDNAME    = &7.
    APPEND GS_FIELDCAT TO  FT_FIELDCAT.
  END-OF-DEFINITION.

  __CATALOG :
    'X'  'AUFNR'    'Order#'           12  'CHAR' '' '',
    ' '  'BALAN'    'Balance'          12  'QUAN' '' '',
    ' '  'KTEXT'    'Description'      16  'CHAR' '' '',
    ' '  'TPLNR'    'Func loc.'        10  'CHAR' '' '',
    ' '  'EQUNR'    'Equipment'        10  'CHAR' '' '',
    ' '  'KOSTV'    'CC.'               6  'CHAR' '' '',
    ' '  'ANLNR'    'Asset #'          10  'CHAR' '' '',
    ' '  'KOSTL'    'Setl.CC'           6  'CHAR' '' '',
    ' '  'ANSWL'    'Asset Val.'       12  'CHAR' '' '',
    ' '  'RATE'     'Rate(%)'           5  'CHAR' '' '',
    ' '  '$KOSTL'   'S.CC'              6  'CHAR' '' '',
    ' '  'ICON'     'Inf.'              3  'ICON' '' '',
    ' '  '$MSG'     'Remarks'          80  'CHAR' '' ''.

  PERFORM CHANGE_FIELDCAT USING FT_FIELDCAT[] .

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM CHANGE_FIELDCAT USING    PT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.


  LOOP AT PT_FIELDCAT INTO GS_FIELDCAT.
    CASE GS_FIELDCAT-FIELDNAME.
      WHEN 'BALAN' OR 'ANSWL' OR  'RATE'.
        GS_FIELDCAT-JUST = 'R'.
        MODIFY PT_FIELDCAT FROM GS_FIELDCAT.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM SORT_BUILD USING    FT_SORT TYPE SLIS_T_SORTINFO_ALV.

  DEFINE SORT_TAB.
    CLEAR GS_SORT.
    GS_SORT-FIELDNAME = &1.
    GS_SORT-SPOS      = &2.
    GS_SORT-UP        = &3.
    GS_SORT-GROUP     = &4.
    GS_SORT-COMP      = &5.
    APPEND GS_SORT TO FT_SORT.
  END-OF-DEFINITION.

  SORT_TAB :
             'AUFNR'        ' ' 'X' 'X' 'X'.

ENDFORM.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  ALV_EVENTS_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3420   text
*----------------------------------------------------------------------*
FORM ALV_EVENTS_GET  USING VALUE(FP_EVENT).
  DATA LS_EVENT TYPE SLIS_ALV_EVENT.

  CASE FP_EVENT.
    WHEN 'P'.
      LS_EVENT-NAME = SLIS_EV_PF_STATUS_SET.
      LS_EVENT-FORM = 'PF_STATUS_SET'.
    WHEN 'U'.
      LS_EVENT-NAME = SLIS_EV_USER_COMMAND.
      LS_EVENT-FORM = 'USER_COMMAND'.
    WHEN 'T'.
      LS_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
      LS_EVENT-FORM = 'TOP_OF_PAGE'.
    WHEN 'D'.
      LS_EVENT-NAME = SLIS_EV_DATA_CHANGED.
      LS_EVENT-FORM = 'DATA_CHANGED'.
  ENDCASE.

  IF GT_EVENTS[] IS INITIAL.
    CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
         EXPORTING
              I_LIST_TYPE = 0  "Simple List
         IMPORTING
              ET_EVENTS   = GT_EVENTS.
  ENDIF.

  READ TABLE  GT_EVENTS TRANSPORTING NO FIELDS
                        WITH KEY  NAME = LS_EVENT-NAME.
  IF SY-SUBRC = 0.
    MODIFY GT_EVENTS FROM LS_EVENT INDEX SY-TABIX.
  ELSE.
    APPEND LS_EVENT  TO GT_EVENTS.
  ENDIF.
ENDFORM.                    " ALV_EVENTS_GET
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT  text
*      -->P_3430   text
*----------------------------------------------------------------------*
FORM ALV_GRID_DISPLAY TABLES FT_OUTTAB
                      USING  PF_EDIT_SET.

  G_PROGRAM = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = G_PROGRAM
            I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_SET'
            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
            IS_LAYOUT                = GS_LAYOUT
            IT_FIELDCAT              = GT_FIELDCAT
            IT_SPECIAL_GROUPS        = GT_SP_GROUP
            IT_SORT                  = GT_SORT
            I_SAVE                   = G_SAVE
            IS_VARIANT               = GS_VARIANT
            IT_EVENTS                = GT_EVENTS
*       IMPORTING
*            E_EXIT_CAUSED_BY_CALLER  = G_EXIT_CAUSED_BY_CALLER
*            ES_EXIT_CAUSED_BY_USER   = GS_EXIT_CAUSED_BY_USER
       TABLES
            T_OUTTAB                 = FT_OUTTAB
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.


ENDFORM.                    " ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$PROG_TEXT  text
*      -->P_PERCENTAGE  text
*----------------------------------------------------------------------*
FORM SHOW_PROGRESS USING    PF_TEXT
                            VALUE(PF_VAL).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = PF_VAL
            TEXT       = PF_TEXT.

ENDFORM.                    " SHOW_PROGRESS
