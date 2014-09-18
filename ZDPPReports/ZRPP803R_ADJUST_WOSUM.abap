************************************************************************
* Program Name      : ZRPP803R_ADJUST_WOSUM
* Author            : Bobby
* Creation Date     : 2004.01.11.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K905549
* Addl Documentation:
* Description       : 'Adjust ZTPP_WOSUM from Vehicle Master
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZRPP803R_ADJUST_WOSUM      NO STANDARD PAGE HEADING
                                  LINE-SIZE  1023  LINE-COUNT 65
                                  MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ausp.

*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: it_AUSP                LIKE TABLE OF AUSP        WITH HEADER LINE,
      it_OLD                 LIKE TABLE OF ZTPP_WOSUM  WITH HEADER LINE,
      it_WOSUM               LIKE TABLE OF ZTPP_WOSUM  WITH HEADER LINE.

*----------------------------------------------------------------------*
* Working AREA
*----------------------------------------------------------------------*
DATA: wa_01                    LIKE ausp-atinn,
      wa_02                    LIKE ausp-atinn,
      wa_03                    LIKE ausp-atinn,
      wa_04                    LIKE ausp-atinn,
      wa_05                    LIKE ausp-atinn,
      wa_06                    LIKE ausp-atinn,
      wa_17                    LIKE ausp-atinn,
      wa_18                    LIKE ausp-atinn,
      wa_19                    LIKE ausp-atinn,
      wa_20                    LIKE ausp-atinn,
      wa_21                    LIKE ausp-atinn,
      wa_22                    LIKE ausp-atinn,
      wa_23                    LIKE ausp-atinn,
      wa_24                    LIKE ausp-atinn,
      wa_25                    LIKE ausp-atinn,
      wa_26                    LIKE ausp-atinn,
      wa_27                    LIKE ausp-atinn,
      wa_28                    LIKE ausp-atinn.

RANGES: R_OBJEK                FOR AUSP-OBJEK ,
        R_ATINN                FOR AUSP-ATINN .


*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS:
  p_run                type c   default 'X'.
SELECTION-SCREEN END   OF BLOCK b1.

START-OF-SELECTION.
  PERFORM SET_ATINN            .
  PERFORM READ_WOSUM           .
  PERFORM update_process       .
  PERFORM DISPLAY_PROCESS      .

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  SET_ATINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATINN      .
  CLEAR: R_ATINN, R_ATINN[].
  R_ATINN = 'IEQ'          .
  PERFORM READ_ATINN   USING  'P_RP01_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_01 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP02_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_02 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP03_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_03 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP04_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_04 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP05_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_05 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP06_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_06 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP17_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_17 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP18_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_18 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP19_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_19 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP20_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_20 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP21_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_21 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP22_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_22 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP23_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_23 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP24_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_24 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP25_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_25 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP26_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_26 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP27_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_27 = R_ATINN-LOW.
  PERFORM READ_ATINN   USING  'P_RP28_SHOP_DATE'  R_ATINN-LOW.
  APPEND R_ATINN           .   WA_28 = R_ATINN-LOW.
ENDFORM.                       " SET_ATINN

*&---------------------------------------------------------------------*
*&      Form  READ_WOSUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_WOSUM     .
  DATA: L_WORD           LIKE AUSP-OBJEK,
        L_COUNT          TYPE I         ,
        L_atflv          LIKE AUSP-ATFLV,
        L_WO             LIKE AUSP-ATINN,
        L_EXTC           LIKE AUSP-ATINN,
        L_INTC           LIKE AUSP-ATINN.

  SELECT * INTO TABLE IT_WOSUM
    FROM ZTPP_WOSUM    .

  IT_OLD[] = IT_WOSUM[].  CLEAR: L_ATFLV.

  PERFORM READ_ATINN USING 'P_WORK_ORDER'  L_WO .
  PERFORM READ_ATINN USING 'P_EXT_COLOR'   L_EXTC.
  PERFORM READ_ATINN USING 'P_INT_COLOR'   L_INTC.

  LOOP AT IT_WOSUM     .
    CLEAR: IT_AUSP, IT_AUSP[].
    PERFORM CLEAR_FIELD_WOSUM.
    CONCATENATE IT_WOSUM-WO_SER IT_WOSUM-NATION IT_WOSUM-DEALER
           INTO L_WORD .
    SELECT *  INTO TABLE  it_ausp
      FROM ausp
     WHERE objek IN ( select OBJEK
                        FROM AUSP
                       WHERE OBJEK IN ( SELECT OBJEK
                                          FROM AUSP
                                         WHERE ATINN = L_WO
                                           AND klart = '002'
                                           AND ATWRT = L_WORD )
                         AND ATINN = L_INTC
                         AND klart = '002'
                         AND ATWRT = IT_WOSUM-INTC )
       AND ATINN = L_EXTC
       AND klart = '002'
       AND ATWRT = IT_WOSUM-EXTC .

    IF SY-SUBRC NE 0 .
      MODIFY IT_WOSUM.
      CONTINUE.
    ENDIF.
    CLEAR: R_OBJEK, R_OBJEK[], L_COUNT.

    LOOP AT IT_AUSP.
      CONCATENATE 'IEQ' IT_AUSP-OBJEK INTO R_OBJEK.
      APPEND R_OBJEK.
    ENDLOOP.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_01
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP01TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_02
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP02TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_03
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP03TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_04
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP04TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_05
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP05TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_06
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP06TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_17
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP07TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_18
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP08TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_19
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP09TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_20
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP10TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_21
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP11TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_22
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP12TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_23
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP13TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_24
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP14TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_26
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP14TQ = IT_WOSUM-RP14TQ + L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_25
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP15TQ = L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_27
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP15TQ = IT_WOSUM-RP15TQ + L_COUNT .   CLEAR: L_COUNT.

    SELECT COUNT( * ) INTO L_COUNT
      FROM AUSP
     WHERE OBJEK IN R_OBJEK
       AND ATINN = WA_28
       AND KLART = '002'
       AND ATFLV > L_ATFLV  .

    IT_WOSUM-RP16TQ = L_COUNT .   CLEAR: L_COUNT.

    MODIFY IT_WOSUM.
  ENDLOOP.
ENDFORM.                    " READ_WOSUM

*&---------------------------------------------------------------------*
*&      Form  UPDATE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_process.
  DELETE FROM ZTPP_WOSUM CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
  MODIFY ZTPP_WOSUM FROM TABLE IT_WOSUM.
ENDFORM.                    " UPDATE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_PROCESS.
  LOOP AT IT_WOSUM.
    WRITE: / IT_WOSUM.
  ENDLOOP.
ENDFORM.                    " DISPLAY_PROCESS

*&---------------------------------------------------------------------*
*&      Form  READ_ATINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0113   text
*      -->P_L_WO  text
*----------------------------------------------------------------------*
FORM READ_ATINN USING    PA_CHAR  PA_ATINN .
  SELECT SINGLE ATINN  INTO PA_ATINN
    FROM CABN
   WHERE ATNAM = PA_CHAR.
ENDFORM.                    " READ_ATINN

*&---------------------------------------------------------------------*
*&      Form  CLEAR_FIELD_WOSUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_FIELD_WOSUM.
  CLEAR: IT_WOSUM-RP01TQ, IT_WOSUM-RP02TQ, IT_WOSUM-RP03TQ,
         IT_WOSUM-RP04TQ, IT_WOSUM-RP05TQ, IT_WOSUM-RP06TQ,
         IT_WOSUM-RP07TQ, IT_WOSUM-RP08TQ, IT_WOSUM-RP09TQ,
         IT_WOSUM-RP10TQ, IT_WOSUM-RP11TQ, IT_WOSUM-RP12TQ,
         IT_WOSUM-RP13TQ, IT_WOSUM-RP14TQ, IT_WOSUM-RP15TQ,
         IT_WOSUM-RP16TQ.
ENDFORM.                    " CLEAR_FIELD_WOSUM
