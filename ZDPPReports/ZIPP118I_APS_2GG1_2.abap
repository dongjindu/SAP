REPORT ZIPP118I_APS_2GG1_2 NO STANDARD PAGE HEADING
                          MESSAGE-ID ZMPP.
************************************************************************
* Program Name      : ZIPP118I_APS_2GG1_2
* Developer         : Furong Wang
* Creation Date     : 10/09/08
* Specifications By :
* Addl Documentation: APS II - Daily Input Plan
* Description       :
*
************************************************************************
* Modification Logs
* Date        Developer    RequestNo    Description
*
************************************************************************

TABLES: ZTPP_PSS02GG.

DATA: IT_PSS02GG LIKE TABLE OF ZTPP_PSS02GG WITH HEADER LINE.

CONSTANTS: C_DEST(10) VALUE 'WMPP01'.   "Outbound Interface Destination

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME.
*PARAMETERS: P_DATE LIKE SY-DATUM.
**SELECT-OPTIONS: S_DATE FOR ZTPP_pss02gg-BDAT.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_EAI TYPE C AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT (55) TEXT-100 FOR FIELD P_EAI.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK B1.

START-OF-SELECTION.
  PERFORM DATA_SELECT.
  IF IT_PSS02GG[] IS INITIAL.
    MESSAGE I001 WITH TEXT-001.
  ELSE.
    IF P_EAI = 'X'.
      PERFORM SEND_DATA.
    ENDIF.
    PERFORM DATA_UPDATE.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
FORM DATA_SELECT.
  DATA: L_CDATE LIKE SY-DATUM,
        L_CTIME LIKE SY-UZEIT.

  DATA: L_TOTAL LIKE ZTPP_PSS02GG-BQTY.

  DATA:  LT_INPUT LIKE TABLE OF ZTPP_INPUT_PLAN
                  WITH HEADER LINE.

  DATA: BEGIN OF LT_WORDER OCCURS 0,
        WORK_ORDER LIKE ZTPP_INPUT_PLAN-WORK_ORDER,
        EXTC LIKE ZTPP_INPUT_PLAN-EXTC,
        INTC LIKE ZTPP_INPUT_PLAN-INTC,
        RD01 LIKE ZTPP_INPUT_PLAN-RD01,
        END OF LT_WORDER.

  CLEAR: IT_PSS02GG, IT_PSS02GG[].
  L_CDATE = SY-DATUM.
  L_CTIME = SY-UZEIT.

  SELECT * INTO TABLE LT_INPUT
   FROM ZTPP_INPUT_PLAN
    WHERE RD01 <> '00000000'.

  SORT LT_INPUT BY WORK_ORDER EXTC INTC RD01.
  LOOP AT LT_INPUT.
    MOVE-CORRESPONDING LT_INPUT TO LT_WORDER.
    COLLECT LT_WORDER.
  ENDLOOP.

  LOOP AT LT_WORDER.
    IT_PSS02GG-BDAT = LT_WORDER-RD01.
    IT_PSS02GG-EXTC = LT_WORDER-EXTC.
    IT_PSS02GG-INTC = LT_WORDER-INTC.
    IT_PSS02GG-ORDR = LT_WORDER-WORK_ORDER+0(9).
    IT_PSS02GG-DIST = LT_WORDER-WORK_ORDER+9(5).
    CLEAR: L_TOTAL.
    LOOP AT LT_INPUT WHERE WORK_ORDER = LT_WORDER-WORK_ORDER
                     AND EXTC = LT_WORDER-EXTC
                     AND INTC = LT_WORDER-INTC
                     AND RD01 = LT_WORDER-RD01.
      L_TOTAL = L_TOTAL + 1.
      IT_PSS02GG-MODL = LT_INPUT-MODL.
      IT_PSS02GG-BMDL = LT_INPUT-MI.
      IT_PSS02GG-OCNN = LT_INPUT-OCNN.
      IT_PSS02GG-VERS = LT_INPUT-VERS.
    ENDLOOP.
    IT_PSS02GG-BQTY = L_TOTAL.
    IT_PSS02GG-PLNT = '1'.
    IT_PSS02GG-STDT = L_CDATE.
    IT_PSS02GG-STTM = L_CDATE.
    IT_PSS02GG-ERZET = L_CTIME.
    APPEND IT_PSS02GG.
    CLEAR: IT_PSS02GG.
  ENDLOOP.
ENDFORM.                    " DATA_SELECT

*---------------------------------------------------------------------*
*       FORM read_normal_class                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VMNO                                                        *
*  -->  P_CHAR                                                        *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM READ_NORMAL_CLASS USING P_VMNO P_CHAR
                             CHANGING P_VALUE.
  SELECT SINGLE AU~ATWRT
    INTO P_VALUE
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_VMNO      AND
          KLART = '002'       AND
          CA~ATNAM = P_CHAR  .
ENDFORM.                    " read_normal_classification

*&---------------------------------------------------------------------*
*&      Form  DATA_UPDATE
*&---------------------------------------------------------------------*
FORM DATA_UPDATE.
  DATA: L_TEXT(60) TYPE C,
        L_INT TYPE I.

  DELETE FROM ZTPP_PSS02GG CLIENT SPECIFIED WHERE MANDT = SY-MANDT.

  INSERT ZTPP_PSS02GG FROM TABLE IT_PSS02GG.

  IF SY-SUBRC = 0.
    DESCRIBE TABLE IT_PSS02GG LINES L_INT.
    WRITE L_INT TO L_TEXT LEFT-JUSTIFIED.
    COMMIT WORK.
    CONCATENATE 'Created Record count :' L_TEXT
      INTO L_TEXT.
    MESSAGE  S001 WITH L_TEXT.
    MESSAGE  S001 WITH TEXT-002.
  ELSE.
    ROLLBACK WORK.
    MESSAGE  W001 WITH TEXT-003.
  ENDIF.
ENDFORM.                    " DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  SEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_DATA.
  DATA : L_MSGTXT(100),
         L_RESULT(1).

  CALL FUNCTION 'Z_FPP_SET_PSS02GG_2'
    DESTINATION C_DEST
    IMPORTING
      FLAG          = L_RESULT
    TABLES
      I_PSS02GG     = IT_PSS02GG
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

  IF L_RESULT = 'S'.
    MESSAGE I001 WITH 'Successfully sent out'.
  ELSE.
    IF L_RESULT IS INITIAL.
      L_RESULT = 'E'.
    ENDIF.
    MESSAGE I001 WITH L_MSGTXT.
  ENDIF.
  LOOP AT IT_PSS02GG.
    IT_PSS02GG-INT_FLAG = L_RESULT.
    MODIFY IT_PSS02GG TRANSPORTING INT_FLAG.
  ENDLOOP.
ENDFORM.                    " SEND_DATA
