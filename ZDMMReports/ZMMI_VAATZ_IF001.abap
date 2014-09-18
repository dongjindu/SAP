*&---------------------------------------------------------------------*
*& Report  ZMMI_VAATZ_IF001
*& Copy from ZMMR_IF002
*&                                                                     *
*&---------------------------------------------------------------------*
*& Date        Developer      Request           Description
*& 08/29/06    Manju          UD1K921930        Send  User Name / Dept.
*&                                              details of the person
*&                                              who created PR to VAATZ
*& 09/01/06    Manju          UD1K921983        Bug fix to avoid those
*&                                              extra records into log
*&                                              table incase of RFC
*&                                              Failure.
*& 09/13/06    Manju          UD1K922124        Remove Prefix from Name
*& 10/31/06    Manju          UD1K922828        Add message field to *
*                                               capture return error
*                                               message from Vaatz
* 10/31/06     Manju          UD1K922836        Pass HMMA as company
*                                               instead of H201
* 11/14/06     Manju          UD1K923009        Add re-sending Option
* 11/12/07     Rakesh         UD1K942140        Replace field RCOMP with
*              Gandhi                           SORT1 of table ADRC
*&---------------------------------------------------------------------*

REPORT ZMMI_VAATZ_IF001 MESSAGE-ID ZMM_IF
                        NO STANDARD PAGE HEADING.
*------------RANGE----------------*
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
************************************************************************
* SAP Tables                                                           *
************************************************************************
TABLES : EBAN,
         EBKN,
         SSCRFIELDS,
         ZTMM_VAZ_IF001.
*         ZTMM_IF006.
.
INCLUDE <ICON>.

************************************************************************
* TYPES and TYPE-POOLS                                                 *
************************************************************************
TYPE-POOLS: SLIS,   "> Globale Type(ALV)
            VRM .
TYPES:
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
*---// EBAN & EBKN join internal table
DATA : BEGIN OF IT_TAB OCCURS 0.
        INCLUDE STRUCTURE ZSMM_VAZ_IF001.
DATA:   FRGKZ  LIKE EBAN-FRGKZ,     "< Release indicator.
       ZZTYPE LIKE EBAN-ZZTYPE.    "< I/F Message type
DATA : END OF IT_TAB.

DATA IT_EBKN  LIKE TABLE OF EBKN WITH HEADER LINE.

DATA : BEGIN OF  IT_ST_EBAN OCCURS 0,
        BANFN LIKE EBAN-BANFN,
       END OF IT_ST_EBAN.

DATA : BEGIN OF  IT_ST_EBAN1 OCCURS 0,
        BANFN LIKE EBAN-BANFN,
        BNFPO LIKE EBAN-BNFPO,
        ZZTYPE1 LIKE EBAN-ZZTYPE1,
        ZZTYPE LIKE EBAN-ZZTYPE,
       END OF IT_ST_EBAN1.

*---// P/R outbound transfer internal table
DATA : IT_OUTBOUND LIKE TABLE OF ZSMM_VAZ_IF001 WITH HEADER LINE.

*---// P/R outbound Log data save internal table
DATA : IT_OUT_LOG  LIKE TABLE OF ZTMM_VAZ_IF001 WITH HEADER LINE.

*---// RETURN message internal table
DATA : IT_RETURN         LIKE TABLE OF ZSMM_VAZ_IF012 WITH HEADER LINE.
* DATA : IT_RELEASE_RETURN LIKE TABLE OF BAPIRETURN WITH HEADER LINE.

*---- LIST BOX DATA
DATA: WA_FLD_NAME  TYPE VRM_ID,
      IT_LIST_BOX  TYPE VRM_VALUES,
      WA_VALUE LIKE LINE OF IT_LIST_BOX.

*---- RECIPIENT
DATA: LV_ERNAM TYPE ZSMM_VAZ_IF001-ERNAM.

DATA : BEGIN OF IT_ADRC OCCURS 10,
         ADDRNUMBER TYPE ADRC-ADDRNUMBER,
         SORT1 TYPE ADRC-SORT1,
       END OF IT_ADRC.


************************************************************************
* RANGES                                                               *
************************************************************************
*---// Number Ranges variables
DATA:   V_NUMBER LIKE ZTMM_VAZ_IF001-SERNO.

*---// Ranges internal table
RANGES: R_NUMBER FOR ZTMM_VAZ_IF001-SERNO.

*---// Screen Range internal table
RANGES: RA_ZZTYPE FOR EBAN-ZZTYPE,
        RA_BANFN  FOR EBAN-BANFN.

DATA: GF_BANFN  TYPE EBAN-BANFN,
      GT_BANFN  TYPE EBAN-BANFN.

************************************************************************
* CONSTANTS                                                            *
************************************************************************
*---// P/R outbound flag.
CONSTANTS : V_OUTBOUND_FLAG TYPE ZTMM_VAZ_IF001-FLAG VALUE '3',
            GT_ZZTYPE TYPE EBAN-ZZTYPE VALUE 'Z'.


************************************************************************
* VARIANTS                                                             *
************************************************************************
*---// Table index check.
DATA L_TABIX  LIKE SY-TABIX.
*---// Count field
DATA COUNT    LIKE ZTMM_VAZ_IF001-CUNT.
*---// Select data checking field
DATA C_FLAG   TYPE C.
*---// Log Item sequential no
*DATA: L_SERNOI  LIKE ZTMM_IF006-SERNOI.

************************************************************************
* SELECT-OPTIONS / PARAMETERS                                          *
************************************************************************
*Search condition
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:   S_WERKS FOR ZTMM_VAZ_IF001-WERKS  "Plant code.
                          NO-EXTENSION NO INTERVALS,
                  S_BANFN FOR EBAN-BANFN,
                  S_ERDAT FOR EBAN-ERDAT OBLIGATORY,
                  S_frgdt FOR EBAN-frgdt.
SELECTION-SCREEN END OF BLOCK BOX1.

*Division
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-006.

PARAMETERS: P_BATCH     RADIOBUTTON GROUP AB3 DEFAULT 'X',
            P_MANUAL    RADIOBUTTON GROUP AB3.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-002.
PARAMETERS  : P_RSEND AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BOX3.

************************************************************************
* AT SELECTION-SCREEN                                                  *
************************************************************************
AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM = 'FC01'.
    CALL SCREEN 0100 STARTING AT 1  1
                     ENDING   AT 40 5     .
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
    if P_RSEND = 'X'.
      LOOP AT SCREEN.
         IF SCREEN-NAME = 'P_BATCH'.
            CLEAR: P_BATCH.
            SCREEN-INPUT = 0.
            MODIFY SCREEN.
         ENDIF.
      ENDLOOP.
    ENDIF.
************************************************************************
* INITIALIZATION                                                       *
************************************************************************
INITIALIZATION.
  SSCRFIELDS-FUNCTXT_01 = ICON_LOCKED.
  SELECTION-SCREEN FUNCTION KEY 1.
  S_ERDAT-low = sy-datum.
  APPEND S_ERDAT.
************************************************************************
* START-OF-SELECTION                                                   *
************************************************************************
START-OF-SELECTION.
*---// Selection table for condition EBAN & EBKN
  PERFORM GET_DATA_FOR_TABLE.

  IF C_FLAG = 'X'.
    EXIT.
  ELSE.

*---// Transfer data to EAI
    PERFORM GET_EAI_TRANSER.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  get_data_for_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_FOR_TABLE.

  DATA: L_CUNT LIKE ZTMM_IF005-CUNT,
       L_BUKRS LIKE PA0001-BUKRS.

  DATA: L_TABIX LIKE SY-TABIX.      " +UD1K942140

  DATA :  ST TYPE I,
          ST1 TYPE I,
          L_ANSWER(1).

  FIELD-SYMBOLS: <FS>.


  CLEAR C_FLAG.
  CLEAR:   IT_TAB, IT_OUTBOUND, IT_OUT_LOG, IT_EBKN.
  REFRESH: IT_TAB, IT_OUTBOUND, IT_OUT_LOG, IT_EBKN.

  IF P_RSEND  EQ 'X'.
    IF sy-batch = 'X'.
      MESSAGE E000 WITH 'Resend cannot run in Background'.
      C_FLAG = 'X'.
      EXIT.
    else.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR                    = ' '
*         DIAGNOSE_OBJECT             = ' '
          TEXT_QUESTION               = 'Are you sure you want to resend?'
         TEXT_BUTTON_1               = 'Yes'
*         ICON_BUTTON_1               = ' '
         TEXT_BUTTON_2               = 'NO'
*         ICON_BUTTON_2               = ' '
*         DEFAULT_BUTTON              = '1'
*         DISPLAY_CANCEL_BUTTON       = 'X'
*         USERDEFINED_F1_HELP         = ' '
*         START_COLUMN                = 25
*         START_ROW                   = 6
*         POPUP_TYPE                  =
*         IV_QUICKINFO_BUTTON_1       = ' '
*         IV_QUICKINFO_BUTTON_2       = ' '
       IMPORTING
         ANSWER                      = L_ANSWER
*       TABLES
*         PARAMETER                   =
*       EXCEPTIONS
*         TEXT_NOT_FOUND              = 1
*         OTHERS                      = 2
                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      IF L_ANSWER <> '1'.
         C_FLAG = 'X'.
         exit.
      endif.
    ENDIF.

    SELECT BANFN  BNFPO BSART LOEKZ
          KNTTP  PSTYP MATNR TXZ01
          WERKS  LGORT MATKL MENGE
          MEINS  LPEIN LFDAT PREIS
          PEINH  EKGRP EKORG FLIEF
          DISPO  AFNAM BEDNR LIFNR
          FRGKZ  FRGZU FRGST WAERS
          ZZTYPE GEBER ERNAM ESTKZ
          INTO CORRESPONDING FIELDS OF TABLE IT_TAB
          FROM EBAN
        WHERE BSART  IN ('NB', 'PM', 'ZB')
           AND MATKL  NE 'AM'
           AND ERDAT IN S_ERDAT
*         AND zztype IN (space, 'E', 'W')
           AND WERKS  IN S_WERKS
           AND BANFN  IN S_BANFN
           AND LOEKZ  NE 'X'.
  ELSE.
    SELECT BANFN  BNFPO BSART LOEKZ
          ERDAT
          KNTTP  PSTYP MATNR TXZ01
          WERKS  LGORT MATKL MENGE
          MEINS  LPEIN LFDAT PREIS
          PEINH  EKGRP EKORG FLIEF
          DISPO  AFNAM BEDNR LIFNR
          FRGKZ  FRGZU FRGST WAERS
          ZZTYPE GEBER ERNAM ESTKZ
          INTO CORRESPONDING FIELDS OF TABLE IT_TAB
          FROM EBAN
        WHERE BSART  IN ('NB', 'PM', 'ZB')
           AND MATKL  NE 'AM'
**  for temp fix the error of zztype1
           AND frgdt IN S_frgdt
           and ebakz = ' '
** End
*          AND zztype  IN (space, 'E', 'W')
           AND ZZTYPE1 IN (SPACE, 'E', 'W')
           AND WERKS  IN S_WERKS
           AND BANFN  IN S_BANFN
           AND LOEKZ  NE 'X'.
  ENDIF.

  PERFORM CHECK_RELEASE_STATUS.

  IF NOT IT_TAB[] IS INITIAL.
    SELECT BANFN BNFPO ANLN1 GEBER
           AUFNR SAKTO KOKRS KOSTL
           FROM  EBKN
           INTO CORRESPONDING FIELDS OF TABLE IT_EBKN
           FOR ALL ENTRIES IN IT_TAB
*---// release condition index check
           WHERE BANFN = IT_TAB-BANFN
             AND BNFPO = IT_TAB-BNFPO.
  ENDIF.

  IF NOT IT_TAB[] IS INITIAL.

    PERFORM NUMBER_RANGE.

    CLEAR L_CUNT.
    LOOP AT IT_TAB.

      IT_TAB-EKORG = 'HMMA'.
      IT_TAB-P_FLAG = 'P'.
      IT_TAB-S_STATUS = 'C'.
      IF IT_TAB-PEINH > 1.
        IT_TAB-PREIS = IT_TAB-PREIS / IT_TAB-PEINH.
      ENDIF.
      IT_TAB-PR_AMT = IT_TAB-PREIS * IT_TAB-MENGE.
      IT_TAB-ADD_TIME = SY-UZEIT.
      IT_TAB-CHANGE_TIME = IT_TAB-ADD_TIME.
      IT_TAB-SUBJECT = 'GM Materials'.
      IT_TAB-OPERATING_CODE = 'ALL'.
      IT_TAB-DOM_EXP_FLAG = 'DO'.
      IT_TAB-SHIPPER_TYPE = 'D'.
*      IT_TAB-DELY_TO_ADDRESS = IT_TAB-LGORT.
*      IT_TAB-STR_CODE_NAME = IT_TAB-LGORT.

      IT_TAB-AUTO_PO_FLAG = 'N'.
*IT_TAB-LOCAL_CUR
*IT_TAB-LOCAL_PRICE
*IT_TAB-LOCAL_AMT
*IT_TAB-EXCH_CUR
*IT_TAB-EXCH_PRICE
*IT_TAB-EXCH_AMT

      CLEAR LV_ERNAM.
*---------------------------------------------------------------

*--Old statement-----------------------------------------------
      IF IT_TAB-ZZTYPE NE 'W'.
        IF IT_TAB-FRGKZ = '5'.
*--------------------------------------------------------------
          MOVE-CORRESPONDING IT_TAB TO IT_OUTBOUND.
          MOVE-CORRESPONDING IT_TAB TO IT_OUT_LOG.

** Changed by Furong on 05/19/10
          IF IT_TAB-KNTTP = 'A'.
            IT_OUTBOUND-KNTTP = 'F'.
          ENDIF.
** End of change

*---// RECIPIENT ID NUMBER MOVE
          IF NOT LV_ERNAM IS INITIAL.
            MOVE: LV_ERNAM TO IT_OUTBOUND-ERNAM,
                  LV_ERNAM TO IT_OUT_LOG-ERNAM.
          ENDIF.

*2006.02.14 modification by sgcho
*          IF IT_TAB-KNTTP EQ 'F'.
          READ TABLE IT_EBKN WITH KEY BANFN = IT_TAB-BANFN
                                      BNFPO = IT_TAB-BNFPO.
          IF SY-SUBRC = 0.
            IT_OUTBOUND-AUFNR = IT_EBKN-AUFNR.
            IT_OUTBOUND-SAKTO = IT_EBKN-SAKTO.
** Furong on 07/18/12
            IF IT_TAB-KNTTP = 'K'.
              IT_TAB-DELY_TO_ADDRESS = IT_EBKN-KOSTL.
              IT_TAB-STR_CODE_NAME = IT_EBKN-KOSTL.
            ELSE.
              IT_TAB-DELY_TO_ADDRESS = IT_TAB-LGORT.
              IT_TAB-STR_CODE_NAME = IT_TAB-LGORT.
            ENDIF.
            IT_OUTBOUND-DELY_TO_ADDRESS = IT_TAB-DELY_TO_ADDRESS.
            IT_OUTBOUND-STR_CODE_NAME = IT_TAB-STR_CODE_NAME.
            IT_OUT_LOG-DELY_TO_ADDRESS = IT_TAB-DELY_TO_ADDRESS.
            IT_OUT_LOG-STR_CODE_NAME = IT_TAB-STR_CODE_NAME.

** End on 07/18/12

*            READ TABLE it_comp WITH KEY bukrs =  it_ebkn-kokrs."
*            IF sy-subrc EQ 0.                               "UD1K922836
**              it_outbound-rcomp = it_comp-rcomp.
*"UD1K922836
*              it_outbound-rcomp = it_comp-sort1.         " + UD1K942140
*            ENDIF.                                          "UD1K922836
*            it_outbound-kokrs = it_ebkn-kokrs.
*            it_outbound-kostl = it_ebkn-kostl.
*            it_outbound-anln1 = it_ebkn-anln1.
*            it_outbound-geber = it_ebkn-geber.
*--2006.01.16 Modification-------------------------------------
*            it_outbound-fistl = it_ebkn-fistl.
*            it_outbound-fipos = it_ebkn-fipos.
*--------------------------------------------------------------
            IT_OUT_LOG-AUFNR = IT_EBKN-AUFNR.
            IT_OUT_LOG-SAKTO = IT_EBKN-SAKTO.
*            it_out_log-kokrs = it_ebkn-kokrs.
            IT_OUT_LOG-KOSTL = IT_EBKN-KOSTL.
*            it_out_log-anln1 = it_ebkn-anln1.
*            it_out_log-geber = it_ebkn-geber.
*--2006.01.16 Modification-------------------------------------
*            it_out_log-fistl = it_ebkn-fistl.
*            it_out_log-fipos = it_ebkn-fipos.
*--------------------------------------------------------------
          ENDIF.
* For Temporary Employee's NO HR data is maintained
* So Default Values for uname, udept and dname.
          IF IT_OUTBOUND-UNAME IS INITIAL.
            IT_OUTBOUND-UNAME = 'N/A'.
            IT_OUT_LOG-UNAME = IT_OUTBOUND-UNAME.
          ENDIF.
          IF IT_OUTBOUND-UDEPT IS INITIAL.
            IT_OUTBOUND-UDEPT = '11111'.
            IT_OUT_LOG-UDEPT = IT_OUTBOUND-UDEPT.
          ENDIF.
          IF IT_OUTBOUND-DNAME IS INITIAL.
            IT_OUTBOUND-DNAME = 'ASK to Buyer'.
            IT_OUT_LOG-DNAME =  IT_OUTBOUND-DNAME.
          ENDIF.

          APPEND IT_OUTBOUND.
          CLEAR  IT_OUTBOUND.

          IT_OUT_LOG-SERNO = V_NUMBER.

          ADD 1 TO L_CUNT.
          IT_OUT_LOG-CUNT = L_CUNT.
          APPEND IT_OUT_LOG. CLEAR IT_OUT_LOG.
        ENDIF.

      ELSE.
        IT_OUT_LOG-BANFN = IT_TAB-BANFN.
        IT_OUT_LOG-BNFPO = IT_TAB-BNFPO.
*---// RECIPIENT ID NUMBER MOVE
        IF NOT LV_ERNAM IS INITIAL.
          MOVE: LV_ERNAM TO IT_OUT_LOG-ERNAM.
        ENDIF.

* Transfer success and realase fale.
*---Modification 2006.01.20----------------------
        IT_OUT_LOG-TYPE  = 'W'.
*------------------------------------------------

        IT_OUT_LOG-SERNO = V_NUMBER.
        ADD 1 TO L_CUNT.
        IT_OUT_LOG-CUNT = L_CUNT.
        APPEND IT_OUT_LOG. CLEAR IT_OUT_LOG.
      ENDIF.
    ENDLOOP.

  ELSE.
    C_FLAG = 'X'.
    EXIT.
  ENDIF.

ENDFORM.                    " get_data_for_table

*&---------------------------------------------------------------------*
*&      Form  GET_EAI_TRANSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_EAI_TRANSER .
  DATA: L_RESULT(1),
        L_MESS(255),
        L_TOT_PR_AMT LIKE IT_OUTBOUND-PR_TOT_AMT,
        L_BANFN LIKE IT_OUTBOUND-BANFN.
  DATA: LT_EAI LIKE TABLE OF IT_OUTBOUND WITH HEADER LINE,
        WS_HD LIKE ZSMM_VAZ_IF001.
  DATA:  L_SERVICE TYPE I ,
         L_WITH_PART TYPE I.
  CLEAR:   IT_RETURN,
           IT_OUT_LOG.

  REFRESH: IT_RETURN.

  IF NOT IT_OUTBOUND[] IS INITIAL.
    LOOP AT IT_OUTBOUND.

      AT NEW BANFN.
        REFRESH LT_EAI.
        CLEAR: LT_EAI, L_TOT_PR_AMT.
        CLEAR: L_RESULT, L_MESS.
        L_BANFN = IT_OUTBOUND-BANFN.
      ENDAT.

      LT_EAI = IT_OUTBOUND.

*      IF LT_EAI-PEINH > 1.
*        LT_EAI-PREIS = LT_EAI-PREIS / LT_EAI-PEINH.
*      ENDIF.

      L_TOT_PR_AMT = L_TOT_PR_AMT +  LT_EAI-PR_AMT.

      APPEND LT_EAI.

      AT END OF BANFN.

        CLEAR: L_SERVICE, L_WITH_PART.
        LOOP AT LT_EAI.
          LT_EAI-PR_TOT_AMT = L_TOT_PR_AMT.
          MODIFY LT_EAI TRANSPORTING PR_TOT_AMT.
          IF LT_EAI-MATNR IS INITIAL.
            L_SERVICE = L_SERVICE + 1.
          ELSE.
            L_WITH_PART =  L_WITH_PART + 1.
          ENDIF.
        ENDLOOP.
        IF  L_WITH_PART = 0.
          LOOP AT LT_EAI.
            LT_EAI-MATNR = 'SS8530S9999999999'.
            MODIFY LT_EAI TRANSPORTING MATNR.
          ENDLOOP.
        ELSE.
          LOOP AT LT_EAI.
            IF  LT_EAI-MATNR IS INITIAL.
              LT_EAI-MATNR = 'SS8540M9999999999'.
              MODIFY LT_EAI TRANSPORTING MATNR.
            ENDIF.
          ENDLOOP.
        ENDIF.

        WS_HD = LT_EAI.

        CALL FUNCTION 'ZMMF_VAZ_IF_PR_OUTBOUND'
          DESTINATION 'WMHR01'
          EXPORTING
            I_HEADER              = WS_HD
          IMPORTING
            E_IFRESULT            = L_RESULT
            E_IFFAILMSG           = L_MESS
          TABLES
            IT_ZSMM_IF001         = LT_EAI
          EXCEPTIONS
            COMMUNICATION_FAILURE = 1
            SYSTEM_FAILURE        = 2.

** On 03/04/13
*        IF SY-SUBRC = 0.
*        ELSE.
*          L_RESULT = 'E'.
*          L_MESS = SY-MSGV1.
*          CONCATENATE L_MESS '(EAI connection error)' INTO L_MESS
*           SEPARATED BY SPACE.
*          IF SY-BATCH EQ 'X'.
*            PERFORM SEND_EMAIL USING L_BANFN.
*          ENDIF.
*        ENDIF.

        IF SY-SUBRC = 0.
          if L_RESULT is INITIAL.
             L_RESULT = 'E'.
             L_MESS = SY-MSGV1.
             CONCATENATE L_MESS 'Retunn flag is empty' INTO L_MESS
           SEPARATED BY SPACE.
          endif.
        ELSE.
          L_RESULT = 'E'.
          L_MESS = SY-MSGV1.
          CONCATENATE L_MESS '(EAI connection error)' INTO L_MESS
           SEPARATED BY SPACE.
          IF SY-BATCH EQ 'X'.
            PERFORM SEND_EMAIL USING L_BANFN.
          ENDIF.
        ENDIF.
** End on 03/04/13

        UPDATE EBAN SET ZZTYPE1 = L_RESULT
                  WHERE  BANFN = L_BANFN.
*                AND  BNFPO = IT_RETURN-BNFPO.
        COMMIT WORK AND WAIT.

        LOOP AT LT_EAI.

          READ TABLE IT_OUT_LOG WITH KEY BANFN = LT_EAI-BANFN
                                         BNFPO = LT_EAI-BNFPO.
          IF SY-SUBRC = 0.
            MOVE: V_OUTBOUND_FLAG   TO IT_OUT_LOG-FLAG,
                  L_RESULT          TO IT_OUT_LOG-TYPE,
                  SY-DATUM          TO IT_OUT_LOG-TRAN_DATE,
                  SY-UZEIT          TO IT_OUT_LOG-TRAN_TIME,
               L_MESS            TO IT_OUT_LOG-MESSAGE.     "UD1K922828
            MODIFY IT_OUT_LOG INDEX SY-TABIX.
            CLEAR  IT_OUT_LOG.
          ENDIF.
        ENDLOOP.
      ENDAT.

    ENDLOOP.

    INSERT ZTMM_VAZ_IF001 FROM TABLE IT_OUT_LOG.
    COMMIT WORK AND WAIT.

 MESSAGE S999(ZMMM) WITH 'Log table entries created with Serial number'
                                                               V_NUMBER.


* if the problem occurs due to delayed update's ( Asynchoronous update)
* then we might have to put wait statement to overcome inconstiency.

* if partial line items of PR are interfaced to Vaatz i.e if some of
*line items PR are marked for deletion or not released, set flag of
*line  items  which are not interfaced to VAATZ as Z.
*    DELETE IT_RETURN WHERE TYPE NE 'S'.
*    IT_ST_EBAN[] = IT_RETURN[].
    LOOP AT IT_OUT_LOG.
      IF IT_OUT_LOG-TYPE = 'S'.
        IT_ST_EBAN-BANFN = IT_OUT_LOG-BANFN.
        APPEND IT_ST_EBAN.
      ENDIF.
    ENDLOOP.
    DELETE ADJACENT DUPLICATES FROM IT_ST_EBAN.
    IF NOT IT_ST_EBAN[] IS INITIAL.
      SELECT BANFN  BNFPO
              ZZTYPE1 ZZTYPE
              INTO  TABLE IT_ST_EBAN1
              FROM EBAN FOR ALL ENTRIES IN IT_ST_EBAN
            WHERE BANFN = IT_ST_EBAN-BANFN AND
                BSART  IN ('NB', 'PM', 'ZB')
                AND ZZTYPE1 NE 'S' .
    ENDIF.
    LOOP AT IT_ST_EBAN1.
      UPDATE EBAN SET ZZTYPE1 = 'Z'
                WHERE  BANFN = IT_ST_EBAN1-BANFN
                  AND  BNFPO = IT_ST_EBAN1-BNFPO.
    ENDLOOP.
    COMMIT WORK AND WAIT.

    CHECK P_MANUAL EQ 'X'.
    PERFORM SUBMIT_DATA.
  ENDIF.
ENDFORM.                    " GET_EAI_TRANSER
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUBMIT_DATA .

  CLEAR   R_NUMBER.
  REFRESH R_NUMBER.

  MOVE: 'I'      TO R_NUMBER-SIGN,
        'EQ'     TO R_NUMBER-OPTION,
        V_NUMBER TO R_NUMBER-LOW,
        SPACE    TO R_NUMBER-HIGH.

  APPEND R_NUMBER.
  CLEAR  R_NUMBER.

  SUBMIT ZMMR_VAATZ_IF001_LOG AND RETURN
         WITH S_SERNO       IN R_NUMBER
         WITH P_OUTBND      EQ 'X'
         WITH P_ALL         EQ ' '.

ENDFORM.                    " SUBMIT_DATA
*&---------------------------------------------------------------------*
*&      Form  NUMBER_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM NUMBER_RANGE.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = '04'
      OBJECT                  = 'ZSERNO3'
      QUANTITY                = '1'
    IMPORTING
      NUMBER                  = V_NUMBER
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      OTHERS                  = 7.

ENDFORM.                    " NUMBER_RANGE
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
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LAYOUT.
  CLEAR GS_LAYOUT.
  GS_LAYOUT-CWIDTH_OPT = 'X'.  "??? ???
  GS_LAYOUT-ZEBRA      = 'X'.
ENDFORM.                    " LAYOUT
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

  IF NOT RA_BANFN IS INITIAL.
    UPDATE EBAN SET ZZTYPE = GT_ZZTYPE
              WHERE BANFN IN RA_BANFN.
    COMMIT WORK AND WAIT.
  ENDIF.
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

*-- P/R number condition
  REFRESH RA_BANFN.
  RANGE_MACRO RA_BANFN GF_BANFN GT_BANFN.

ENDFORM.                    " CALL_RANGE
*&---------------------------------------------------------------------*
*&      Form  get_recipient_id
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_RECIPIENT_ID.
  CLEAR: LV_ERNAM.
ENDFORM.                    " get_recipient_id

*---------------------------------------------------------------------*
*       FORM send_email                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SEND_EMAIL USING P_BANFN.
  DATA:     IT_PACKING_LIST LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
            IT_CONTENTS LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
            IT_RECEIVERS LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE,
            IT_ATTACHMENT LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
            GD_CNT TYPE I,
            GD_SENT_ALL(1) TYPE C,
            GD_DOC_DATA LIKE SODOCCHGI1,
            GD_ERROR TYPE SY-SUBRC,
            PSUBJECT(40) TYPE C VALUE  'PR OUTBOUND Error ',
            P_EMAIL(40)   TYPE C VALUE 'pshrewsbury@hmmausa.com' .
*           p_email(40)   type c value 'RF_GR_ERROR' .

  DATA:   IT_MESSAGE TYPE STANDARD TABLE OF SOLISTI1 INITIAL SIZE 0
                  WITH HEADER LINE.

* Populate Message Body .

  APPEND 'PR Outbound Data Transfer to VAATZ System Failed'
                      TO IT_MESSAGE.

  APPEND '------------------------------------------' TO IT_MESSAGE.
  APPEND  'RFC Failure ' TO IT_MESSAGE.
  APPEND '------------------------------------------' TO IT_MESSAGE.
*  loop at it_error.
  MOVE  'PR:' TO IT_MESSAGE(10).
  MOVE  '/'            TO IT_MESSAGE+11(1).
  MOVE  P_BANFN TO IT_MESSAGE+13(20).
  MOVE   ''   TO IT_MESSAGE+35(80).
  APPEND IT_MESSAGE. CLEAR IT_MESSAGE.
*  endloop.

* Fill the document data.
  GD_DOC_DATA-DOC_SIZE = 1.

* Populate the subject/generic message attributes
  GD_DOC_DATA-OBJ_LANGU = SY-LANGU.
  GD_DOC_DATA-OBJ_NAME  = 'SAPRPT'.
  GD_DOC_DATA-OBJ_DESCR = PSUBJECT.
  GD_DOC_DATA-SENSITIVTY = 'F'.

* Describe the body of the message
  CLEAR IT_PACKING_LIST.
  REFRESH IT_PACKING_LIST.
  IT_PACKING_LIST-TRANSF_BIN = SPACE.
  IT_PACKING_LIST-HEAD_START = 1.
  IT_PACKING_LIST-HEAD_NUM = 0.
  IT_PACKING_LIST-BODY_START = 1.
  DESCRIBE TABLE IT_MESSAGE LINES IT_PACKING_LIST-BODY_NUM.
  IT_PACKING_LIST-DOC_TYPE = 'RAW'.
  APPEND IT_PACKING_LIST.

* Add the recipients email address
  CLEAR IT_RECEIVERS.
  REFRESH IT_RECEIVERS.
*  it_receivers-receiver = p_email.
*  it_receivers-rec_type = 'U'.
*  it_receivers-com_type = 'INT'.
*  it_receivers-notif_del = 'X'.
*  it_receivers-notif_ndel = 'X'.

  IT_RECEIVERS-RECEIVER = P_EMAIL.
  IT_RECEIVERS-REC_TYPE = 'C'.
  IT_RECEIVERS-COM_TYPE = ''.

  APPEND IT_RECEIVERS.

* Call the FM to send the message to SAPMAIL
* asynchronously
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
*  STARTING NEW TASK  'T1'
       EXPORTING
            DOCUMENT_DATA              = GD_DOC_DATA
*            PUT_IN_OUTBOX              = 'X'
            COMMIT_WORK                = 'X'
*       IMPORTING
*            sent_to_all                = gd_sent_all
       TABLES
            PACKING_LIST               = IT_PACKING_LIST
            CONTENTS_TXT               = IT_MESSAGE
            RECEIVERS                  = IT_RECEIVERS
       EXCEPTIONS
            TOO_MANY_RECEIVERS         = 1
            DOCUMENT_NOT_SENT          = 2
            DOCUMENT_TYPE_NOT_EXIST    = 3
            OPERATION_NO_AUTHORIZATION = 4
            PARAMETER_ERROR            = 5
            X_ERROR                    = 6
            ENQUEUE_ERROR              = 7
            OTHERS                     = 8.
  IF SY-SUBRC = 0.
    SUBMIT RSCONN01           "Start the sending process
          WITH MODE   = 'INT'
          WITH OUTPUT = ' '
          AND RETURN.

  ENDIF.

ENDFORM.                    " send_email
*&---------------------------------------------------------------------*
*&      Form  check_release_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_RELEASE_STATUS.
  DATA: L_BANFN LIKE EBAN-BANFN,
        L_TOT_CN TYPE I,
        L_REL_CN TYPE I.

  SORT IT_TAB BY BANFN.
  LOOP AT IT_TAB.
    AT NEW BANFN.
      CLEAR: L_TOT_CN, L_REL_CN.
      L_BANFN = IT_TAB-BANFN.
    ENDAT.
    L_TOT_CN = L_TOT_CN + 1.
    IF IT_TAB-FRGKZ = '5'.
      L_REL_CN = L_REL_CN + 1.
    ENDIF.
    AT END OF BANFN.
      IF L_TOT_CN <> L_REL_CN.
        DELETE IT_TAB WHERE BANFN = L_BANFN.
      ENDIF.
    ENDAT.
  ENDLOOP.
  IF IT_TAB[] IS INITIAL.
    C_FLAG = 'X'.
  ENDIF.
ENDFORM.                    " check_release_status
