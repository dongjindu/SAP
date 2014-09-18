************************************************************************
* Program Name      : ZIPP_BACKORDER_TO_GCC
* Author            : Furong Wang
* Creation Date     : 11/2010
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : Send Backorder to GCC
* Modification Logs
* Date       Developer    RequestNo    Description
*
*********************************************************************

REPORT ZIPP_BACKORDER_TO_GCC NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

TABLES: ZTPPVR.

DATA : L_MSGTXT(100),
       L_RESULT(1).

DATA: W_DEST(20),   "Interface Destination.
      W_FLAG(1).

DATA: BEGIN OF IT_DATA OCCURS 0.
        INCLUDE STRUCTURE ZTPP_BORDER_GCC.
DATA: END OF IT_DATA.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.

PARAMETERS: P_DATUM LIKE SY-DATUM OBLIGATORY.
*PARAMETERS: P_OPT1 RADIOBUTTON GROUP GRP1,
*            P_OPT2 RADIOBUTTON GROUP GRP1.

PARAMETERS: P_BUKRS LIKE T001-BUKRS DEFAULT 'H201' OBLIGATORY.

SELECTION-SCREEN SKIP 1.
PARAMETERS: P_SEND AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
  PERFORM INIT_DATA.

AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN.

START-OF-SELECTION.
  PERFORM GET_DATA.
  IF IT_DATA[] IS INITIAL.
    MESSAGE I000 WITH 'No Data'.
  ELSE.
    PERFORM SAVE_SEND_DATA.
  ENDIF.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_DATA.
  DATA: L_FR_DATE LIKE SY-DATUM,
        L_DATE LIKE SY-DATUM,
        L_BACK_DATE LIKE SY-DATUM,
        L_NUM(8) TYPE N,
        L_MATNR LIKE MARA-MATNR,
        L_MATNR_HD1 LIKE MARA-MATNR,
        L_MATNR_HD2 LIKE MARA-MATNR,
        L_CURR_MONTH(4),
        L_ST_MONTH(4),
        L_BMDL LIKE ZTPP_MODEL_CONV-BMDL,
        L_MODEL LIKE ZTPP_MODEL_CONV-MODEL,
        L_CURR_WONO LIKE ZTPP_WOSUM-WO_SER,
        L_FR_CURR_DATE LIKE SY-DATUM,
        L_TO_CURR_DATE LIKE SY-DATUM,
        L_FR_PRE_DATE LIKE SY-DATUM,
        L_TO_PRE_DATE LIKE SY-DATUM,
        L_OBJEK LIKE MARA-MATNR,
        L_FR_WO LIKE ZTPP_WOSUM-WO_SER,
        L_TO_WO LIKE ZTPP_WOSUM-WO_SER,
        L_QTY LIKE ZTPP_PLAN_DAY-QTY_PLAN,
        L_QTY_BODY LIKE ZTPP_PROD_ACTUAL-QTY_BODY,
        L_QTY_TRIM LIKE ZTPP_PROD_ACTUAL-QTY_BODY,
        L_QTY_SOFF LIKE ZTPP_PROD_ACTUAL-QTY_BODY,
        L_QTY_SHIP LIKE ZTPP_PROD_ACTUAL-QTY_BODY,
        L_BACKMONTHS TYPE NUMC3,
        L_DATUM LIKE SY-DATUM,
        L_UZEIT LIKE SY-UZEIT,
        L_MI(14),
        L_LC_NO(1),
        L_FIRST_D.

  DATA: L_VARIABLE LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE,
        LT_ITAB LIKE TABLE OF ZTPP_BORDER_GCC WITH HEADER LINE.

  DATA: BEGIN OF LT_WOSUM OCCURS 0.
          INCLUDE STRUCTURE ZTPP_WOSUM.
  DATA: MI(14),
        P_LC_NO(1),
        PRE_BODY LIKE ZTPP_WOSUM-MODQTY,
        PRE_TRIM LIKE ZTPP_WOSUM-MODQTY,
        PRE_SOFF LIKE ZTPP_WOSUM-MODQTY,
        PRE_SHIP LIKE ZTPP_WOSUM-MODQTY,
        END OF LT_WOSUM.

  DATA: BEGIN OF LT_OBJEK OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
        END OF LT_OBJEK.

  RANGES: R_NATION FOR ZTPP_PROD_ACTUAL-NATN.

  L_FR_DATE = P_DATUM - 183.

  L_CURR_MONTH = P_DATUM+2(4).
  L_ST_MONTH = L_FR_DATE+2(4).

  CONCATENATE P_DATUM+0(6) '01' INTO L_FR_CURR_DATE.

  L_BACKMONTHS = 1.
  CALL FUNCTION 'CCM_GO_BACK_MONTHS'
       EXPORTING
            CURRDATE   = L_FR_CURR_DATE
            BACKMONTHS = L_BACKMONTHS
       IMPORTING
            NEWDATE    = L_FR_PRE_DATE.

  CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = L_FR_PRE_DATE
       IMPORTING
            LAST_DAY_OF_MONTH = L_TO_PRE_DATE
       EXCEPTIONS
            DAY_IN_NO_DATE    = 1
            OTHERS            = 2.


  L_BACKMONTHS = 6.
  CALL FUNCTION 'CCM_GO_BACK_MONTHS'
       EXPORTING
            CURRDATE   = L_FR_CURR_DATE
            BACKMONTHS = L_BACKMONTHS
       IMPORTING
            NEWDATE    = L_BACK_DATE.
  .

  CONCATENATE 'E' L_BACK_DATE+4(4) '0000' INTO L_FR_WO.

*  SELECT A~WO_SER A~NATION A~DEALER A~EXTC A~INTC A~INITQTY A~MODQTY
*         A~SEQQTY A~PLANQTY A~FORECASTQTY A~MITUQTY A~WOCREDATE
*         A~WOMODDATE A~FSC A~VERSION A~SALES A~RP01TQ A~RP02TQ A~RP03TQ
*         A~RP04TQ A~RP05TQ A~RP06TQ A~RP07TQ A~RP08TQ A~RP09TQ A~RP10TQ
*         A~RP11TQ A~RP12TQ A~RP13TQ A~RP14TQ A~RP15TQ A~RP16TQ
*    INTO CORRESPONDING FIELDS OF TABLE LT_WOSUM
*    FROM ZTPP_WOSUM AS A
*    INNER JOIN ZTPP_WOSUM AS B
*    ON A~WO_SER = B~WO_SER
*    AND A~NATION = B~NATION
*    AND A~DEALER = B~DEALER
*    AND A~EXTC = B~EXTC
*    AND A~INTC = B~INTC
*     WHERE A~MODQTY > B~RP15TQ
*      AND A~NATION <> 'B28'.

  SELECT WO_SER NATION DEALER EXTC INTC INITQTY MODQTY
           SEQQTY PLANQTY FORECASTQTY MITUQTY WOCREDATE
           WOMODDATE FSC VERSION SALES RP01TQ RP02TQ RP03TQ
           RP04TQ RP05TQ RP06TQ RP07TQ RP08TQ RP09TQ RP10TQ
           RP11TQ RP12TQ RP13TQ RP14TQ RP15TQ RP16TQ
      INTO CORRESPONDING FIELDS OF TABLE LT_WOSUM
      FROM ZTPP_WOSUM AS A
      WHERE WO_SER >= L_FR_WO
          AND A~NATION <> 'B28'.

  IF SY-SUBRC <> 0.
    W_FLAG = 'X'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = P_DATUM
       IMPORTING
            LAST_DAY_OF_MONTH = L_TO_CURR_DATE
       EXCEPTIONS
            DAY_IN_NO_DATE    = 1
            OTHERS            = 2.

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  if p_datum+6(2) = '01'.
    L_FIRST_D = 'X'.
  ELSE.
    CLEAR: L_FIRST_D.
  ENDIF.

  L_DATUM = SY-DATUM.
  L_UZEIT = SY-UZEIT.

  SORT LT_WOSUM BY WO_SER NATION DEALER.

  LOOP AT LT_WOSUM.

    IF LT_WOSUM-DEALER+0(1) <> 'A' AND LT_WOSUM-DEALER+0(1) <> 'B'.
      DELETE LT_WOSUM.
      CONTINUE.
    ENDIF.

    REFRESH: L_VARIABLE.
    CLEAR: L_VARIABLE.
    L_VARIABLE-ATNAM = 'P_SALES_ORDER'.
    APPEND L_VARIABLE .

    CONCATENATE LT_WOSUM-WO_SER LT_WOSUM-NATION LT_WOSUM-DEALER
      LT_WOSUM-EXTC LT_WOSUM-INTC INTO L_MATNR.

    CALL FUNCTION 'Z_ALL_CLASS_CHARC'   "'Z_FPP_HANDLING_MASTER'
           EXPORTING
                OBJECT       = L_MATNR
*                  MODE         = 'R'
                CTYPE        = '001'
           TABLES
                VAL_TABLE    = L_VARIABLE
           EXCEPTIONS
                NO_DATA      = 1
                ERROR_MODE   = 2
                ERROR_OBJECT = 3.

    READ TABLE L_VARIABLE INDEX 1.
    IF L_VARIABLE-ATWRT IS INITIAL.
      DELETE LT_WOSUM.
      CONTINUE.
    ENDIF.

    CONCATENATE LT_WOSUM-WO_SER LT_WOSUM-NATION LT_WOSUM-DEALER
     INTO L_MATNR_HD1.
    IF L_MATNR_HD1 <> L_MATNR_HD2.
      L_MATNR_HD2 = L_MATNR_HD1.

      REFRESH: L_VARIABLE.
      CLEAR: L_VARIABLE.

      L_VARIABLE-ATNAM = 'P_MI'.
      APPEND L_VARIABLE .
      L_VARIABLE-ATNAM = 'P_LC_NO'.
      APPEND L_VARIABLE .

      CALL FUNCTION 'Z_ALL_CLASS_CHARC'   "'Z_FPP_HANDLING_MASTER'
             EXPORTING
                  OBJECT       = L_MATNR_HD1
*                  MODE         = 'R'
                  CTYPE        = '001'
             TABLES
                  VAL_TABLE    = L_VARIABLE
             EXCEPTIONS
                  NO_DATA      = 1
                  ERROR_MODE   = 2
                  ERROR_OBJECT = 3.

      READ TABLE L_VARIABLE INDEX 1.
      L_MI = L_VARIABLE-ATWRT.
      READ TABLE L_VARIABLE INDEX 2.
      IF L_VARIABLE-ATWRT IS INITIAL.
        CLEAR: L_LC_NO.
      ELSE.
        L_LC_NO = 'X'.
      ENDIF.
*    ENDIF.
      IF LT_WOSUM-WO_SER+1(4) >= L_CURR_MONTH AND L_FIRST_D = 'X'.
        CONCATENATE LT_WOSUM-WO_SER LT_WOSUM-NATION LT_WOSUM-DEALER
        INTO L_MATNR.

*      CONCATENATE LT_WOSUM-WO_SER LT_WOSUM-NATION LT_WOSUM-DEALER
*      LT_WOSUM-EXTC LT_WOSUM-INTC INTO L_MATNR.

        REFRESH: LT_OBJEK.

        SELECT OBJEK INTO TABLE LT_OBJEK
        FROM AUSP AS AU
          INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
        WHERE KLART = '002'       AND
              CA~ATNAM = 'P_WORK_ORDER' AND
              AU~ATWRT = L_MATNR.

        LOOP AT LT_OBJEK.
          L_OBJEK =  LT_OBJEK-OBJEK.

          REFRESH: L_VARIABLE.
          CLEAR: L_VARIABLE.
          L_VARIABLE-ATNAM = 'P_RP01_SHOP_DATE'.
          APPEND L_VARIABLE .
          L_VARIABLE-ATNAM = 'P_RP06_SHOP_DATE'.
          APPEND L_VARIABLE .
          L_VARIABLE-ATNAM = 'P_RP18_SHOP_DATE'.
          APPEND L_VARIABLE .
          L_VARIABLE-ATNAM = 'P_RP25_SHOP_DATE'.
          APPEND L_VARIABLE .
          L_VARIABLE-ATNAM = 'P_RP27_SHOP_DATE'.
          APPEND L_VARIABLE .

          CALL FUNCTION 'Z_ALL_CLASS_CHARC'   "'Z_FPP_HANDLING_MASTER'
                   EXPORTING
                        OBJECT       = L_OBJEK
                        CTYPE        = '002'
                   TABLES
                        VAL_TABLE    = L_VARIABLE
                   EXCEPTIONS
                        NO_DATA      = 1
                        ERROR_MODE   = 2
                        ERROR_OBJECT = 3.
          READ TABLE L_VARIABLE INDEX 1.
          CONCATENATE L_VARIABLE-ATWRT+6(6) L_VARIABLE-ATWRT+0(2)
                      L_VARIABLE-ATWRT+3(2) INTO L_NUM.
          L_DATE = L_NUM.
          IF NOT L_VARIABLE-ATWRT IS INITIAL AND
             L_DATE BETWEEN L_FR_PRE_DATE AND L_TO_PRE_DATE.
            LT_WOSUM-PRE_BODY = LT_WOSUM-PRE_BODY + 1.
          ENDIF.
          READ TABLE L_VARIABLE INDEX 2.
          CONCATENATE L_VARIABLE-ATWRT+6(6) L_VARIABLE-ATWRT+0(2)
                        L_VARIABLE-ATWRT+3(2) INTO L_NUM.
          L_DATE = L_NUM.
          IF NOT L_VARIABLE-ATWRT IS INITIAL AND
             L_DATE BETWEEN L_FR_PRE_DATE AND L_TO_PRE_DATE.
            LT_WOSUM-PRE_TRIM = LT_WOSUM-PRE_TRIM + 1.
          ENDIF.
          READ TABLE L_VARIABLE INDEX 3.
          CONCATENATE L_VARIABLE-ATWRT+6(6) L_VARIABLE-ATWRT+0(2)
                        L_VARIABLE-ATWRT+3(2) INTO L_NUM.
          L_DATE = L_NUM.
          IF NOT L_VARIABLE-ATWRT IS INITIAL AND
             L_DATE BETWEEN L_FR_PRE_DATE AND L_TO_PRE_DATE.
            LT_WOSUM-PRE_SOFF = LT_WOSUM-PRE_SOFF + 1.
          ENDIF.
          READ TABLE L_VARIABLE INDEX 4.
          CONCATENATE L_VARIABLE-ATWRT+6(6) L_VARIABLE-ATWRT+0(2)
                       L_VARIABLE-ATWRT+3(2) INTO L_NUM.
          L_DATE = L_NUM.
          IF NOT L_VARIABLE-ATWRT IS INITIAL AND
             L_DATE BETWEEN L_FR_PRE_DATE AND L_TO_PRE_DATE.
            LT_WOSUM-PRE_SHIP = LT_WOSUM-PRE_SHIP + 1.
          ENDIF.
          READ TABLE L_VARIABLE INDEX 5.
          CONCATENATE L_VARIABLE-ATWRT+6(6) L_VARIABLE-ATWRT+0(2)
                      L_VARIABLE-ATWRT+3(2) INTO L_NUM.
          L_DATE = L_NUM.
          IF NOT L_VARIABLE-ATWRT IS INITIAL AND
             L_DATE BETWEEN L_FR_PRE_DATE AND L_TO_PRE_DATE.
            LT_WOSUM-PRE_SHIP = LT_WOSUM-PRE_SHIP + 1.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    LT_WOSUM-MI = L_MI.
    LT_WOSUM-P_LC_NO = L_LC_NO.
    MODIFY LT_WOSUM.

    LT_ITAB-CRTN_DT = P_DATUM.
    LT_ITAB-CO_SCN_CD = 'H'.
    LT_ITAB-PRDP_CD = 'A'.
    IF P_BUKRS = 'H201'.
      LT_ITAB-PLNT_CD = '5N'.
    ELSE.
    ENDIF.

    LT_ITAB-DE_SCN_CD = 'E'.
    LT_ITAB-VEHL_CD = LT_WOSUM-MI.
    LT_ITAB-NAT_CD =  LT_WOSUM-NATION.
    IF L_FIRST_D = 'X'.
    LT_ITAB-BEFO_PROD_BODY =  LT_WOSUM-PRE_BODY.
    LT_ITAB-BEFO_PROD_TRIM =  LT_WOSUM-PRE_TRIM.
    LT_ITAB-BEFO_PROD_SOFF =  LT_WOSUM-PRE_SOFF.
    LT_ITAB-BEFO_PROD_SHIP =  LT_WOSUM-PRE_SHIP.
    ENDIF.

    COLLECT LT_ITAB.
    CLEAR: LT_ITAB, LT_WOSUM.

*    ELSE.
*      DELETE LT_WOSUM.
*    ENDIF.
  ENDLOOP.

  SORT LT_WOSUM BY NATION MI.
  LOOP AT LT_ITAB.
    L_BMDL = LT_ITAB-VEHL_CD+0(2).
    SELECT SINGLE MODEL INTO L_MODEL
      FROM ZTPP_MODEL_CONV
      WHERE BMDL = L_BMDL.

    LOOP AT LT_WOSUM WHERE NATION = LT_ITAB-NAT_CD
                       AND MI = LT_ITAB-VEHL_CD.
      IF LT_WOSUM-WO_SER+1(4) = L_CURR_MONTH.
        LT_ITAB-TSMN_ORD = LT_ITAB-TSMN_ORD + LT_WOSUM-MODQTY.
      ENDIF.

      IF LT_WOSUM-WO_SER+1(4) > L_CURR_MONTH.
        LT_ITAB-NXMN_ORD = LT_ITAB-NXMN_ORD + LT_WOSUM-MODQTY.
      ENDIF.

      IF LT_WOSUM-WO_SER+1(4) < L_CURR_MONTH AND
         LT_WOSUM-P_LC_NO IS INITIAL.
        LT_ITAB-JW_WAIT_QTY = LT_ITAB-JW_WAIT_QTY + LT_WOSUM-MODQTY.
      ENDIF.
      IF LT_WOSUM-WO_SER+1(4) = L_CURR_MONTH AND
         LT_WOSUM-P_LC_NO IS INITIAL.
        LT_ITAB-DW_WAIT_QTY = LT_ITAB-DW_WAIT_QTY + LT_WOSUM-MODQTY.
      ENDIF.
      IF LT_WOSUM-WO_SER+1(4) > L_CURR_MONTH AND
         LT_WOSUM-P_LC_NO IS INITIAL.
        LT_ITAB-CW_WAIT_QTY = LT_ITAB-CW_WAIT_QTY + LT_WOSUM-MODQTY.
      ENDIF.

      IF LT_WOSUM-WO_SER+1(4) < L_CURR_MONTH.
        LT_ITAB-REMN_ORDR_BODY = LT_ITAB-REMN_ORDR_BODY +
                    LT_WOSUM-MODQTY -  LT_WOSUM-RP01TQ.
      ENDIF.
      IF LT_WOSUM-WO_SER+1(4) < L_CURR_MONTH.
        LT_ITAB-REMN_ORDR_TRIM = LT_ITAB-REMN_ORDR_TRIM +
                    LT_WOSUM-MODQTY -  LT_WOSUM-RP06TQ.
      ENDIF.
      IF LT_WOSUM-WO_SER+1(4) < L_CURR_MONTH.
        LT_ITAB-REMN_ORDR_SOFF = LT_ITAB-REMN_ORDR_SOFF +
                    LT_WOSUM-MODQTY -  LT_WOSUM-RP08TQ.
      ENDIF.
      IF LT_WOSUM-WO_SER+1(4) < L_CURR_MONTH.
        LT_ITAB-REMN_ORDR_SHIP = LT_ITAB-REMN_ORDR_SHIP +
                    LT_WOSUM-MODQTY -  LT_WOSUM-RP15TQ.
      ENDIF.
    ENDLOOP.

*    SELECT SUM( QTY_PLAN ) INTO L_QTY
*    FROM ZTPP_PLAN_DAY
*    WHERE PRDT_DATE BETWEEN L_FR_CURR_DATE AND L_TO_CURR_DATE
*     AND MODEL = L_MODEL.
*    IF SY-SUBRC = 0.
*      LT_ITAB-TSMN_PRDN_PLN = L_QTY.
*    ENDIF.

    REFRESH R_NATION.

    R_NATION-SIGN = 'I'.
    R_NATION-OPTION = 'BT'.

    CONCATENATE LT_WOSUM-NATION '00' INTO R_NATION-LOW.
    CONCATENATE LT_WOSUM-NATION 'ZZ' INTO R_NATION-HIGH.
    APPEND R_NATION.

    SELECT SUM( QTY_BODY ) SUM( QTY_TRIM )
           SUM( QTY_SIGNOFF ) SUM( QTY_SHIPOUT )
     INTO (L_QTY_BODY, L_QTY_TRIM, L_QTY_SOFF, L_QTY_SHIP)
     FROM ZTPP_PROD_ACTUAL
     WHERE PRDT_DATE BETWEEN L_FR_CURR_DATE AND L_TO_CURR_DATE
       AND NATN IN R_NATION
       AND BMDL = LT_WOSUM-MI.

    IF SY-SUBRC = 0.
      LT_ITAB-CURR_PROD_BODY = L_QTY_BODY.
      LT_ITAB-CURR_PROD_TRIM = L_QTY_TRIM.
      LT_ITAB-CURR_PROD_SOFF =  L_QTY_SOFF.
      LT_ITAB-CURR_PROD_SHIP =  L_QTY_SHIP.
    ENDIF.

   IF L_FIRST_D IS INITIAL.
      SELECT SINGLE BEFO_PROD_BODY BEFO_PROD_TRIM
                    BEFO_PROD_SOFF BEFO_PROD_SHIP
       INTO (LT_ITAB-BEFO_PROD_BODY, LT_ITAB-BEFO_PROD_TRIM,
             LT_ITAB-BEFO_PROD_SOFF, LT_ITAB-BEFO_PROD_SHIP)
       FROM ZTPP_BORDER_GCC
       WHERE CRTN_DT = L_FR_CURR_DATE
         AND CO_SCN_CD = LT_ITAB-CO_SCN_CD
         AND PRDP_CD = LT_ITAB-PRDP_CD
         AND PLNT_CD = LT_ITAB-PLNT_CD
         AND DE_SCN_CD = LT_ITAB-DE_SCN_CD
         AND VEHL_CD = LT_ITAB-VEHL_CD
         AND NAT_CD = LT_ITAB-NAT_CD.
      IF SY-SUBRC = 0.
      ELSE.
        CLEAR: LT_ITAB-BEFO_PROD_BODY, LT_ITAB-BEFO_PROD_TRIM,
             LT_ITAB-BEFO_PROD_SOFF, LT_ITAB-BEFO_PROD_SHIP.
      ENDIF.
   ENDIF.

    LT_ITAB-ZSDAT = L_DATUM.
    LT_ITAB-ZSTIM = L_UZEIT.

    MODIFY LT_ITAB.
    CLEAR: LT_ITAB.
    CLEAR: L_QTY, L_QTY_BODY, L_QTY_TRIM, L_QTY_SOFF, L_QTY_SHIP.

  ENDLOOP.
  SORT LT_ITAB BY VEHL_CD NAT_CD.

  IT_DATA[] = LT_ITAB[].
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SAVE_SEND_DATA                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SAVE_SEND_DATA.
  DATA: IT_SAVE LIKE TABLE OF ZTPP_BORDER_GCC WITH HEADER LINE,
        LT_SAVE_LOG LIKE TABLE OF ZTPP_BORDER_GCC WITH HEADER LINE,
        L_FLAG(1),
        L_NUM(08) TYPE N.

  IF IT_DATA[] IS INITIAL.
    EXIT.
  ENDIF.

  IF P_BUKRS = 'H201'.
    W_DEST = 'WMHR01'.
  ELSEIF P_BUKRS = 'K201'.
** For KMMG
*    SELECT SINGLE DEST INTO (W_DEST)
*      FROM ZDEST
*     WHERE SY_SYSID = SY-SYSID
*       AND SY_MANDT = SY-MANDT.
  ENDIF.

  IF P_SEND IS INITIAL.
    LOOP AT IT_DATA.
      MOVE-CORRESPONDING IT_DATA TO IT_SAVE.
      APPEND IT_SAVE.
      CLEAR: IT_SAVE.
    ENDLOOP.

  ELSE.
    CALL FUNCTION 'Z_FPP_BACKORDER_GCC'
     DESTINATION W_DEST
     IMPORTING
       FLAG            = L_FLAG
     TABLES
       I_DATA       = IT_DATA
     EXCEPTIONS
       COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
       SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

    IF SY-SUBRC = 0.
      IF L_FLAG = 'E'.
        MESSAGE I001 WITH 'Sending to EAI failed'.
      ELSE.
        L_FLAG = 'S'.
        MESSAGE I001 WITH 'Data was sent successfully'.
      ENDIF.
    ELSE.
      L_FLAG = 'E'.
      MESSAGE I001 WITH L_MSGTXT.
    ENDIF.

    LOOP AT IT_DATA.
      MOVE-CORRESPONDING IT_DATA TO IT_SAVE.
*      IT_SAVE-ZMSG = L_MSGTXT.
      IT_SAVE-ZEDAT = SY-DATUM.
      IT_SAVE-ZETIM = SY-UZEIT.
      APPEND IT_SAVE.
      CLEAR: IT_SAVE.
    ENDLOOP.

  ENDIF.

  DELETE FROM ZTPP_BORDER_GCC
           WHERE CRTN_DT = P_DATUM.

  INSERT ZTPP_BORDER_GCC FROM TABLE IT_SAVE.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE I000 WITH 'Error: Log Table Updating'.
  ENDIF.
** End of change

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  read_normal_class
*&---------------------------------------------------------------------*
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
*&      Form  READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OBJEK_OBJEK  text
*      -->P_0461   text
*      <--P_L_ATFLV_TEMP  text
*----------------------------------------------------------------------*
FORM READ_NORMAL_CLASS_ATFLV USING P_VMNO P_CHAR
                             CHANGING P_VALUE.
  SELECT SINGLE AU~ATFLV
      INTO P_VALUE
      FROM AUSP AS AU
        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
      WHERE OBJEK = P_VMNO      AND
            KLART = '002'       AND
            CA~ATNAM = P_CHAR  .
ENDFORM.                    " READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*&      Form  control_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN.
*  LOOP AT SCREEN.
*    IF P_OPT2 = 'X'.
*      IF SCREEN-GROUP1 = 'G11'.
*        SCREEN-INVISIBLE = 1.
*        SCREEN-INPUT = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " control_screen
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_DATA.
  P_DATUM = SY-DATUM - 1.
ENDFORM.                    " init_data
