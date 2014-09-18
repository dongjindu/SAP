*&---------------------------------------------------------------------*
*& Report  ZMMI_VAATZ_IF003                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT ZMMI_VAATZ_IF003 MESSAGE-ID ZMM_IF
                  NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
** COPY FROM ZMMR_IF003
*----------------------------------------------------------------------*


************************************************************************
* SAP Tables                                                           *
************************************************************************
TABLES : EKPO,
         EKBE,
         MARA,
         ADRP,
         SSCRFIELDS,
         ZTMM_VAZ_IF018.
************************************************************************
* Include                                                              *
************************************************************************
.
INCLUDE <ICON>.

************************************************************************
* TYPES and TYPE-POOLS                                                 *
************************************************************************
TYPE-POOLS: SLIS,   "> Globale Type(ALV)
            VRM .
TYPES:
*-- Single Value in Value Set
       BEGIN OF VRM_VALUE,
         KEY(40) TYPE C,
         TEXT(80) TYPE C,
       END OF VRM_VALUE,
*-- Table of Values
       VRM_VALUES TYPE VRM_VALUE OCCURS 0,
*-- Id of Value Set
       VRM_ID TYPE VRM_VALUE-TEXT,
*-- table of Ids of Value Set
       VRM_IDS TYPE VRM_ID OCCURS 0,
*-- QueueRow
       BEGIN OF VRM_QUEUEROW,
         TAG,
         VALUE TYPE VRM_VALUE,
       END   OF VRM_QUEUEROW,
*-- Queue
       VRM_QUEUE TYPE VRM_QUEUEROW OCCURS 0.

************************************************************************
*  CONTROL                                                             *
************************************************************************
DATA: GS_LAYOUT TYPE LVC_S_LAYO,
      GS_FDCAT  TYPE LVC_S_FCAT,
      GT_FDCAT  TYPE LVC_T_FCAT.

************************************************************************
* Internal Tables                                                      *
************************************************************************
DATA: BEGIN OF IT_TAB OCCURS 0.
        INCLUDE STRUCTURE ZSMM_IF016.
DATA: GJAHR LIKE EKBE-GJAHR,
      ELIKZ LIKE EKPO-ELIKZ.
DATA: END OF IT_TAB.

DATA: BEGIN OF IT_ITEM OCCURS 0.
        INCLUDE STRUCTURE ZSMM_VAZ_IF016.
DATA: END OF IT_ITEM.

DATA: WA_USER LIKE USADDRESS.
DATA: IT_HEAD_LOG LIKE TABLE OF ZTMM_VAZ_IF018 WITH HEADER LINE.
*DATA: IT_RETURN   LIKE TABLE OF ZTMM_IF019 WITH HEADER LINE.

*---- LIST BOX DATA
DATA: WA_FLD_NAME  TYPE VRM_ID,
      IT_LIST_BOX  TYPE VRM_VALUES,
      WA_VALUE LIKE LINE OF IT_LIST_BOX.

*DATA : BEGIN OF IT_COMP OCCURS 10,                          "UD1K922836
*       BUKRS TYPE T001-BUKRS,
*       RCOMP TYPE T001-RCOMP,
*       END OF IT_COMP.


************************************************************************
* RANGES                                                               *
************************************************************************

*---// Number range
DATA NUMBER LIKE ZTMM_VAZ_IF018-SERNO.

*---// Screen Range internal table
RANGES: RA_ZZTYPE FOR EKPO-ZZTYPE,
        RA_EBELN  FOR EKPO-EBELN.

*------------RANGE_MACRO----------------*
DEFINE RANGE_MACRO.
  IF   &2 NE '' AND &3 NE ''
    OR &2 EQ '' AND &3 NE ''.
    MOVE: 'I'    TO  &1-SIGN,
          'BT'   TO  &1-OPTION,
           &2    TO  &1-LOW,
           &3    TO  &1-HIGH.
    APPEND &1.
  ELSEIF &2 NE '' AND &3 EQ ''.
    MOVE : 'I'    TO  &1-SIGN,
           'EQ'   TO  &1-OPTION,
            &2    TO  &1-LOW.
    APPEND &1.
  ENDIF.
END-OF-DEFINITION.
*--------END OF RANGE_MACRO-------------*

DATA: GF_EBELN  TYPE EKPO-EBELN,
      GT_EBELN  TYPE EKPO-EBELN.

************************************************************************
* CONSTANTS                                                            *
************************************************************************
CONSTANTS : GT_ZZTYPE TYPE EKPO-ZZTYPE VALUE 'Z'.

************************************************************************
* VARIANTS                                                             *
************************************************************************
*---// Move index
DATA G_TABIX LIKE SY-TABIX.
*---// Select data checking field
DATA C_FLAG   TYPE C.
*---// item count check
DATA: CUNT   LIKE ZTMM_VAZ_IF018-CUNT,
      L_ZCNT LIKE ZTMM_VAZ_IF018-ZCNT.

************************************************************************
* RANGES                                                               *
************************************************************************
*---// Ranges internal table
RANGES: R_EBELN  FOR ZTMM_VAZ_IF018-EBELN.
RANGES: R_EBELP  FOR ZTMM_VAZ_IF018-EBELP.
RANGES: R_NUMBER FOR ZTMM_VAZ_IF018-SERNO.
*---// Number Ranges variables
DATA:   V_NUMBER LIKE ZTMM_VAZ_IF018-SERNO.

************************************************************************
* SELECT-OPTIONS / PARAMETERS                                          *
************************************************************************
*Search condition
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:   S_EBELN FOR ZTMM_VAZ_IF018-EBELN MODIF ID IOG,
                  S_EBELP FOR ZTMM_VAZ_IF018-EBELP MODIF ID IOG,
                  S_SERNO FOR ZTMM_VAZ_IF018-SERNO,
                  S_CPUDT FOR EKBE-CPUDT DEFAULT SY-DATUM,
                  S_BELNR FOR EKBE-BELNR.
SELECTION-SCREEN SKIP 1.
PARAMETERS :  P_CHK AS CHECKBOX.                            "UD1K922961
SELECTION-SCREEN END   OF BLOCK BOX1.

*Division
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-004.
PARAMETERS: P_BATCH     RADIOBUTTON GROUP AB3 DEFAULT 'X',
            P_MANUAL    RADIOBUTTON GROUP AB3.

SELECTION-SCREEN END OF BLOCK BOX2.

************************************************************************
* AT SELECTION-SCREEN                                                  *
************************************************************************
AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM = 'FC01'.
    CALL SCREEN 0100 STARTING AT 1  1
                     ENDING   AT 48 5.
  ENDIF.

************************************************************************
* INITIALIZATION                                                       *
************************************************************************
INITIALIZATION.
  SSCRFIELDS-FUNCTXT_01 = ICON_LOCKED.
  SELECTION-SCREEN FUNCTION KEY 1.

************************************************************************
* START-OF-SELECTION                                                   *
************************************************************************
START-OF-SELECTION.

*---// Create number range
  PERFORM NUMBER_CREATE.

*---// Table inner join selection
  PERFORM GET_DATA_TABLE.
  IF C_FLAG = 'X'.
    MESSAGE S001.
    EXIT.
  ENDIF.

*---// Transfer data to EAI
  PERFORM GET_EAI_TRANSER.

*---// G/R Data search Report program call 'ZMMR_IF005'
  CHECK P_MANUAL EQ 'X'.
  PERFORM SUBMIT_DATA.

************************************************************************
* END-OF-SELECTION                                                   *
************************************************************************



*&---------------------------------------------------------------------*
*&      Form  GET_DATA_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_TABLE .
  DATA L_LGORT LIKE MSEG-LGORT.

  CLEAR:   IT_TAB, IT_ITEM, IT_HEAD_LOG, G_TABIX.
  REFRESH: IT_TAB, IT_ITEM, IT_HEAD_LOG.

*---// A~ EKKO
*---// B~ EKPO
*---// C~ EKBE
  IF P_CHK IS INITIAL.                                      "UD1K922961
    SELECT A~LIFNR A~WAERS A~BUKRS
           B~EBELN B~EBELP B~MATNR B~ELIKZ
           B~MEINS B~LGORT B~TXZ01 B~WERKS
           B~NETPR AS NET_PRICE
           B~MENGE AS MENGE C~MENGE AS ZMENGE
           C~BWART C~DMBTR C~BELNR C~BUZEI C~GJAHR
           C~BPMNG C~SHKZG                                  "UD1K922788
           C~BUDAT C~CPUDT C~CPUTM                          "UD1K922961
           C~ERNAM AS USNAM                                 "UD1K922961
         FROM EKKO AS A INNER JOIN EKPO AS B
           ON A~EBELN = B~EBELN
                       INNER JOIN EKBE AS C
           ON B~EBELN = C~EBELN
          AND B~EBELP = C~EBELP
         INTO CORRESPONDING FIELDS OF TABLE IT_TAB
          WHERE A~BSART  EQ 'ZB'
          AND B~ZZTYPE NOT IN ('F', 'Z')
          AND B~EBELN  IN S_EBELN
          AND B~EBELP  IN S_EBELP
          AND C~BELNR  IN S_BELNR
          AND C~BEWTP  EQ 'E'
          AND C~CPUDT IN S_CPUDT.
  ELSE.
* Begin of changes -  UD1K922961
    SELECT A~LIFNR A~WAERS A~BUKRS
          B~EBELN B~EBELP B~MATNR B~ELIKZ
          B~MEINS B~LGORT B~TXZ01 B~WERKS
          B~NETPR AS NET_PRICE
          B~MENGE AS MENGE C~MENGE AS ZMENGE
          C~BWART C~DMBTR C~BELNR C~BUZEI C~GJAHR
          C~BPMNG C~SHKZG                                   "UD1K922788
          C~BUDAT C~CPUDT C~CPUTM                           "UD1K922961
          C~ERNAM AS USNAM                                  "UD1K922961
       FROM EKKO AS A INNER JOIN EKPO AS B
          ON A~EBELN = B~EBELN
                      INNER JOIN EKBE AS C
          ON B~EBELN = C~EBELN
         AND B~EBELP = C~EBELP
        INTO CORRESPONDING FIELDS OF TABLE IT_TAB
         WHERE A~BSART  EQ 'ZB'
         AND B~EBELN  IN S_EBELN
         AND B~EBELP  IN S_EBELP
         AND C~BELNR  IN S_BELNR
         AND C~BEWTP  EQ 'E'
         AND C~CPUDT IN S_CPUDT.
* End of changes - UD1K922961
  ENDIF.

  IF IT_TAB[] IS INITIAL AND P_MANUAL  IS INITIAL.
    MESSAGE I000 WITH  'No data found'.                     "UD1K923157
    EXIT.
  ENDIF.
* Select Company code - UD1K922836
*  SELECT BUKRS RCOMP INTO TABLE IT_COMP                     "UD1K922836
*         FROM T001                                          "UD1K922836
*         WHERE RCOMP NE SPACE.                              "UD1K922836
*

  IF NOT IT_TAB[] IS INITIAL.

    DATA: LV_KONTS LIKE T030-KONTS.

    LOOP AT IT_TAB.
      G_TABIX = SY-TABIX.

*---// MARA table checking for 'ROH1'
* In Prod. since none of the materials have
* basic material desc. so the below code
* is okay otherwise it needs to be modified
*      SELECT SINGLE WRKST
*               FROM MARA
*               INTO IT_ITEM-WRKST
*              WHERE MATNR =  IT_TAB-MATNR
*                AND MTART =  'ROH1'.
*      IF SY-SUBRC EQ 0.
*        CONTINUE.
*      ELSE.
*---< GR Outbound additional transfer data implementation start
*---// Changed by YWYANG
      CLEAR: LV_KONTS.

      SELECT SINGLE ISOCODE INTO IT_TAB-ZMEINS
                            FROM T006
                            WHERE MSEHI = IT_TAB-MEINS.

* Begin of changes - UD1K922961
*        SELECT SINGLE budat FROM mkpf       " Posting date
*                            INTO it_tab-budat
*                            WHERE mblnr = it_tab-belnr
*                            AND   mjahr = it_tab-gjahr.
* End of changes - UD1K922961


      PERFORM GET_ASSET_ACCOUNT CHANGING LV_KONTS.

      IF IT_TAB-DMBTR IS INITIAL.                           "UD1K922961
        PERFORM GET_AMOUNT_IN_LC.
      ENDIF.

      MOVE: LV_KONTS TO IT_TAB-KONTS.

      MOVE: IT_TAB-ZMENGE TO IT_TAB-ZGRQTY.
*---> 2006/02/16 - end of change

      MOVE-CORRESPONDING IT_TAB TO IT_ITEM.

      IT_ITEM-BUKRS = 'HMMA'.
      CONCATENATE 'HMMA' IT_TAB-GJAHR IT_TAB-BELNR
        INTO IT_ITEM-GR_NO.
      IF IT_TAB-BWART = '101'.
        IT_ITEM-GR_COUNT = '1'.
      ELSE.
        IT_ITEM-GR_COUNT = '2'.
** Send negtive QTY on 04/15/13
        IT_TAB-ZMENGE = IT_TAB-ZMENGE * -1.
        IT_ITEM-ZGRQTY = IT_ITEM-ZGRQTY * -1.
        IT_ITEM-DMBTR = IT_ITEM-DMBTR * -1.
** End on 04/15/13
      ENDIF.
      IT_ITEM-P_FLAG = 'P'.
      IT_ITEM-S_STATUS = 'C'.

      IT_ITEM-ADD_TIME = SY-UZEIT.
      IT_ITEM-CHANGE_DATE = SY-DATUM.
      IT_ITEM-CHANGE_TIME = IT_ITEM-ADD_TIME.
      IT_ITEM-DOC_TYPE = 'PO'.
      IT_ITEM-ARRIVAL_QTY = IT_ITEM-ZGRQTY.
      IT_ITEM-DOM_EXP_FLAG = 'DO'.
      IT_ITEM-EXCHGE_RATE = '1'.

*LOCAL_CUR
*LOCAL_PRICE
*LOCAL_AMT
*EXCH_CUR
*EXCH_PRICE
*EXCH_AMT
*
*---< Move GR Qty to transfer data
*        MOVE: it_tab-zmenge TO it_item-zmenge,
      MOVE  IT_TAB-ZMENGE TO IT_ITEM-ZGRQTY.

**--// Net Price   - UD1K922961
*        SELECT SINGLE netpr
*        FROM ekpo
*        INTO it_item-net_price
*        WHERE ebeln = it_tab-ebeln
*        AND ebelp   = it_tab-ebelp.
* - UD1K922961

*---// MBEW table checking
*--// Standard Price.
*        SELECT SINGLE STPRS
*                 FROM MBEW
*                 INTO IT_ITEM-STPRS
*                WHERE MATNR = IT_TAB-MATNR
*                  AND BWKEY = IT_TAB-WERKS
*                  AND VPRSV = 'S'.

*---// MARD table checking
*--// Storage Bin
      IF IT_TAB-LGORT IS INITIAL.              " Storage location

        CLEAR IT_TAB-LGORT.
        SELECT SINGLE LGORT
                 INTO IT_ITEM-LGORT
                 FROM MSEG
                WHERE MBLNR = IT_TAB-BELNR
                  AND MJAHR = IT_TAB-GJAHR
                  AND ZEILE = IT_TAB-BUZEI.
      ENDIF.

*        SELECT SINGLE lgpbe
*                 FROM mard
*                 INTO it_item-lgpbe
*                WHERE matnr = it_tab-matnr
*                  AND werks = it_tab-werks
*                  AND lgort = it_tab-lgort.


*---// MKPF table checking  _ begin of changes - UD1K922961
*        SELECT SINGLE cpudt cputm usnam
*                 FROM mkpf
*                 INTO (it_item-cpudt,it_item-cputm,it_item-usnam)
*                WHERE mblnr = it_tab-belnr
*                  AND mjahr = it_tab-gjahr.
* End of changes - UD1K922961

*---// HR User name select call function
      PERFORM NAME_SELECT.
*      ENDIF.

* Company Code.
*      READ TABLE IT_COMP WITH KEY BUKRS = IT_TAB-BUKRS.     "UD1K922836
*      IF SY-SUBRC EQ 0.                                     "UD1K922836
*        IT_ITEM-RCOMP = IT_COMP-RCOMP.                      "UD1K922836
*      ENDIF.                                                "UD1K922836
*

*---// Log data save

      ADD 1 TO CUNT.
      MOVE-CORRESPONDING IT_ITEM TO IT_HEAD_LOG.
      MOVE: NUMBER               TO IT_HEAD_LOG-SERNO,
            CUNT                 TO IT_HEAD_LOG-CUNT,
            SY-DATUM             TO IT_HEAD_LOG-TRAN_DATE,
            SY-UZEIT             TO IT_HEAD_LOG-TRAN_TIME,
            SY-UNAME             TO IT_HEAD_LOG-TRAN_NAME.

      L_ZCNT = L_ZCNT + 1.
      IT_ITEM-ZCNT = L_ZCNT.
*-- Currency = 'USD'
*      it_item-waers = 'USD'.

      APPEND: IT_HEAD_LOG, IT_ITEM.
      CLEAR:  IT_HEAD_LOG, IT_ITEM.

      AT END OF EBELP.
        CLEAR L_ZCNT.
      ENDAT.

    ENDLOOP.
  ELSE.
*---// Log data save not found
    C_FLAG = 'X'.
    EXIT.
  ENDIF.

ENDFORM.                    " GET_DATA_TABLE
*&---------------------------------------------------------------------*
*&      Form  GET_EAI_TRANSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_EAI_TRANSER.

  DATA: L_RESULT(1),
          L_MESS(255),
          L_BELNR LIKE IT_ITEM-BELNR.
  DATA: LT_EAI LIKE TABLE OF IT_ITEM WITH HEADER LINE.


  CLEAR:   IT_HEAD_LOG, IT_ITEM, CUNT,
           G_TABIX,     ZTMM_VAZ_IF018.
  DATA:  L_ERRTEXT(50) TYPE C.

  SORT IT_ITEM BY BELNR EBELN EBELP.
  LOOP AT IT_ITEM.

    AT NEW BELNR.
      REFRESH LT_EAI.
      CLEAR: LT_EAI.
      CLEAR: L_RESULT, L_MESS.
      L_BELNR = IT_ITEM-BELNR.
    ENDAT.

    LT_EAI = IT_ITEM.
    APPEND LT_EAI.

    AT END OF BELNR.

      CALL FUNCTION 'ZMMF_VAZ_IF_GR_OUTBOUND' DESTINATION 'WMHR01'
         IMPORTING
              E_IFRESULT = L_RESULT
              E_IFFAILMSG = L_MESS
        TABLES
          IT_OUTBOUND = LT_EAI
        EXCEPTIONS
       COMMUNICATION_FAILURE = 1
       SYSTEM_FAILURE        = 2.

      CASE SY-SUBRC.
        WHEN 1.
          L_RESULT = 'E'.
          L_ERRTEXT = 'Communication failure'.
        WHEN 2.
          L_RESULT = 'E'.
          L_ERRTEXT = 'System failure '.
      ENDCASE.

      IF SY-SUBRC NE 0.
        LOOP AT IT_HEAD_LOG WHERE BELNR = L_BELNR.
          MOVE: 'E'                     TO IT_HEAD_LOG-TYPE,
                L_ERRTEXT               TO IT_HEAD_LOG-MESSAGE.
          MODIFY IT_HEAD_LOG INDEX SY-TABIX
                 TRANSPORTING TYPE MESSAGE.
          CLEAR: IT_HEAD_LOG.
        ENDLOOP.
      ELSE.
        CHECK   P_CHK IS INITIAL AND
                S_BELNR IS INITIAL.

        ADD 1 TO CUNT.

        LOOP AT IT_HEAD_LOG WHERE BELNR = L_BELNR.

          MOVE: L_RESULT    TO IT_HEAD_LOG-TYPE,
                L_MESS      TO IT_HEAD_LOG-MESSAGE.
          MODIFY IT_HEAD_LOG INDEX SY-TABIX
                 TRANSPORTING TYPE MESSAGE.
          CLEAR: IT_HEAD_LOG.
        ENDLOOP.

        IF L_RESULT = 'S'.
          LOOP AT LT_EAI.

            READ TABLE IT_TAB WITH KEY EBELN = LT_EAI-EBELN
                                       EBELP = LT_EAI-EBELP
                                       ELIKZ = 'X'.
*---// ELIKZ value 'X' Transmission is finish
            IF SY-SUBRC = 0.
              UPDATE EKPO   SET   ZZTYPE = 'F'
                          WHERE   EBELN = LT_EAI-EBELN
                            AND   EBELP = LT_EAI-EBELP.
              COMMIT WORK AND WAIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDAT.
  ENDLOOP.
  INSERT ZTMM_VAZ_IF018 FROM TABLE IT_HEAD_LOG.
  COMMIT WORK AND WAIT.
ENDFORM.                    " GET_EAI_TRANSER
*&---------------------------------------------------------------------*
*&      Form  NUMBER_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM NUMBER_CREATE.

  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            NR_RANGE_NR             = '05'
            OBJECT                  = 'ZSERNO4'
            QUANTITY                = '1'
       IMPORTING
            NUMBER                  = NUMBER
       EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            OTHERS                  = 7.

ENDFORM.                    " NUMBER_CREATE

*&---------------------------------------------------------------------*
*&      Form  SUBMIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUBMIT_DATA .
  CLEAR:   R_NUMBER, R_EBELN, R_EBELP.
  REFRESH: R_NUMBER, R_EBELN, R_EBELP.

  MOVE: 'I'      TO R_NUMBER-SIGN,
        'EQ'     TO R_NUMBER-OPTION,
        S_SERNO  TO R_NUMBER-LOW,
        SPACE    TO R_NUMBER-HIGH.

  MOVE: 'I'      TO R_EBELN-SIGN,
        'EQ'     TO R_EBELN-OPTION,
        S_EBELN  TO R_EBELN-LOW,
        SPACE    TO R_EBELN-HIGH.

  MOVE: 'I'      TO R_EBELP-SIGN,
        'EQ'     TO R_EBELP-OPTION,
        S_EBELP  TO R_EBELP-LOW,
        SPACE    TO R_EBELP-HIGH.


  APPEND:  R_NUMBER, R_EBELN, R_EBELP.
  CLEAR:   R_NUMBER, R_EBELN, R_EBELP.

  SUBMIT ZMMR_IF005 AND RETURN
         WITH S_EBELN IN R_EBELN
         WITH S_EBELP IN R_EBELP
         WITH S_SERNO IN R_NUMBER.
*         WITH P_ALL   EQ 'X'.
ENDFORM.                    " SUBMIT_DATA
*&---------------------------------------------------------------------*
*&      Form  NAME_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM NAME_SELECT .
  CLEAR WA_USER.
  CALL FUNCTION 'SUSR_USER_READ'
    EXPORTING
      USER_NAME                  = IT_ITEM-USNAM
*     WITH_TEXT                  =
*     READ_UCLASS                = ' '
   IMPORTING
*     USER_LOGONDATA             =
*     USER_DEFAULTS              =
      USER_ADDRESS               = WA_USER
*     REF_USER                   =
*     ALIAS                      =
*     UCLASS                     =
*   TABLES
*     USER_PARAMETERS            = IT_PARA.
*     UCLASSSYS                  =
   EXCEPTIONS
     USER_NAME_NOT_EXISTS       = 1
     INTERNAL_ERROR             = 2
     OTHERS                     = 3.

*  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF

*  SELECT SINGLE NAME_LAST
*           FROM ADRP
*           INTO IT_ITEM-USNAM
*          WHERE PERSNUMBER = WA_USER-PERSNUMBER
*            AND DATE_FROM  = '00010101'
*            AND NATION     = SPACE.

ENDFORM.                    " NAME_SELECT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  IF SY-UCOMM = 'CANC'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXEC'.
      PERFORM UPDATE_TYPE.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_TYPE.
  PERFORM CALL_RANGE.

  UPDATE EKPO SET ZZTYPE = GT_ZZTYPE
            WHERE EBELN IN RA_EBELN.
  COMMIT WORK AND WAIT.
ENDFORM.                    " UPDATE_TYPE
*&---------------------------------------------------------------------*
*&      Form  CALL_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_RANGE.

*-- Transfer attribute type
  REFRESH RA_ZZTYPE.
  IF NOT GT_ZZTYPE IS INITIAL.
    RA_ZZTYPE-SIGN   = 'I'.
    RA_ZZTYPE-OPTION = 'EQ'.
    RA_ZZTYPE-LOW    = GT_ZZTYPE.
    APPEND RA_ZZTYPE.
  ENDIF.

*-- P/R number condition
  REFRESH RA_EBELN.
  RANGE_MACRO RA_EBELN GF_EBELN GT_EBELN.
  CLEAR: GF_EBELN, GT_EBELN.
ENDFORM.                    " CALL_RANGE
*&---------------------------------------------------------------------*
*&      Form  get_amount_local_currency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_WAERS  text
*      <--P_LV_LAMOUNT  text
*----------------------------------------------------------------------*
FORM GET_AMOUNT_LOCAL_CURRENCY USING    P_LV_WAERS
                               CHANGING P_LV_LAMOUNT.
*  DATA: lv_budat1 LIKE sy-datum,
*        lv_budat2(10).

  DATA: BUDAT_OLD LIKE MKPF-BUDAT,
        BUDAT_NEW LIKE MKPF-BUDAT.

*  MOVE: it_tab-budat TO lv_budat1.
*  WRITE: lv_budat1 TO lv_budat2.

*  DO 20 TIMES.
  CLEAR: P_LV_LAMOUNT.

  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
    EXPORTING
*     CLIENT                  = SY-MANDT
      DATE                    = IT_TAB-BUDAT
      FOREIGN_AMOUNT          = IT_TAB-NET_PRICE
      FOREIGN_CURRENCY        = IT_TAB-WAERS
      LOCAL_CURRENCY          = P_LV_WAERS
      RATE                    = 0
      TYPE_OF_RATE            = 'M'
      READ_TCURR              = 'X'
   IMPORTING
*     exchange_rate           =
*     FOREIGN_FACTOR          =
      LOCAL_AMOUNT            = P_LV_LAMOUNT
*     LOCAL_FACTOR            =
*     EXCHANGE_RATEX          =
*     FIXED_RATE              =
*     DERIVED_RATE_TYPE       =
   EXCEPTIONS
     NO_RATE_FOUND           = 1
     OVERFLOW                = 2
     NO_FACTORS_FOUND        = 3
     NO_SPREAD_FOUND         = 4
     DERIVED_2_TIMES         = 5
     OTHERS                  = 6
            .
  IF SY-SUBRC EQ 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ELSE.
    CLEAR P_LV_LAMOUNT.
*      CLEAR: budat_old, budat_new.
*
*      MOVE: it_tab-budat TO budat_old.
*
*      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*        EXPORTING
*          date            = budat_old
*          days            = '01'
*          months          = '00'
*          signum          = '-'
*          years           = '00'
*        IMPORTING
*          calc_date       = budat_new
*                .
*      IF NOT budat_new IS INITIAL.
*        MOVE: budat_new TO it_tab-budat.
*      ENDIF.
  ENDIF.
*  ENDDO.
ENDFORM.                    " get_amount_local_currency
*&---------------------------------------------------------------------*
*&      Form  get_asset_account
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_KONTS  text
*----------------------------------------------------------------------*
FORM GET_ASSET_ACCOUNT CHANGING P_LV_KONTS.

  DATA: LV_BKLAS LIKE MBEW-BKLAS,
        LV_KTOPL LIKE T001-KTOPL.

  CLEAR: LV_BKLAS, LV_KTOPL.
  CLEAR: P_LV_KONTS.

  SELECT SINGLE BKLAS FROM MBEW            " Valuation class
                      INTO LV_BKLAS
                      WHERE MATNR = IT_TAB-MATNR
                      AND   BWKEY = IT_TAB-WERKS.

  SELECT SINGLE KTOPL FROM T001            " Chart of account
                      INTO LV_KTOPL
                      WHERE BUKRS = IT_TAB-BUKRS.

  SELECT SINGLE KONTS FROM T030            " Asset account
                      INTO P_LV_KONTS
                      WHERE BKLAS = LV_BKLAS
                      AND   KTOPL = LV_KTOPL
                      AND   KTOSL = 'BSX'.

ENDFORM.                    " get_asset_account
*&---------------------------------------------------------------------*
*&      Form  get_amount_in_lc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_AMOUNT_IN_LC.
  DATA: LV_MENGE   LIKE MSEG-MENGE,
        LV_WAERS   LIKE T001-WAERS,
        LV_LAMOUNT LIKE MSEG-MENGE.

  CLEAR: LV_MENGE, LV_WAERS, LV_LAMOUNT.

*        SELECT SINGLE netpr FROM ekpo          " Net price
*                            INTO lv_netpr
*                            WHERE ebeln = it_tab-ebeln
*                            AND   ebelp = it_tab-ebelp.

  SELECT SINGLE MENGE FROM MSEG               " GR Qty
                      INTO LV_MENGE
                      WHERE MBLNR = IT_TAB-BELNR
                        AND MJAHR = IT_TAB-GJAHR
                        AND ZEILE = IT_TAB-BUZEI.

  SELECT SINGLE WAERS FROM T001               " Currency unit
                      INTO LV_WAERS
                      WHERE BUKRS = IT_TAB-BUKRS.

  IF IT_TAB-WAERS NE LV_WAERS.
    PERFORM GET_AMOUNT_LOCAL_CURRENCY USING    LV_WAERS
                                      CHANGING LV_LAMOUNT.
    IF LV_LAMOUNT IS INITIAL.
      CLEAR IT_TAB-DMBTR.
    ELSE.
      IT_TAB-DMBTR = LV_LAMOUNT * LV_MENGE.
    ENDIF.
  ELSE.
    IT_TAB-DMBTR = IT_TAB-NET_PRICE * LV_MENGE.
  ENDIF.

ENDFORM.                    " get_amount_in_lc
