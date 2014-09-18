************************************************************************
* Program Name      : ZPPI101_VIN_219_GQMS
* Author            : Furong Wang
* Creation Date     : 11/29/2007
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : GQMS (Vin info & 219)
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT ZPPI101_VIN_219_GQMS NO STANDARD PAGE HEADING LINE-SIZE 255
                       MESSAGE-ID ZMPP.


DATA: IT_AUSP LIKE TABLE OF AUSP WITH HEADER LINE.
DATA: IT_VIN_219 LIKE TABLE OF ZTPP_VIN_219 WITH HEADER LINE.

DATA  C_COUNT TYPE I.

CONSTANTS: C_DEST(10) VALUE 'WMHR01'.   "Interface Destination.

SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.
SELECT-OPTIONS: S_DATUM FOR SY-DATUM OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS: P_SEND(1) DEFAULT  'X'.
SELECTION-SCREEN END OF BLOCK BL1.
SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-T02.
PARAMETERS: P_DAYS TYPE I.
SELECTION-SCREEN END OF BLOCK BL2.

START-OF-SELECTION.
  PERFORM READ_DATA.
** Changed by Furong on 12/04/08
  IF IT_VIN_219[] IS INITIAL OR IT_AUSP[] IS INITIAL.
  ELSE.
** End of change on 12/04/08
    PERFORM SAVE_DATA.
    IF P_SEND IS INITIAL.
    ELSE.
      PERFORM SEND_RTN.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
  DATA: L_ATFLV LIKE AUSP-ATFLV,
        L_NO(03) TYPE N,
        L_MATNR LIKE MARA-MATNR,
        L_ATINN LIKE AUSP-ATINN,
        L_NAME(30) TYPE C,
        L_NUM(08),
        L_NUM_D(08) TYPE N,
        L_DATE LIKE SY-DATUM,
        L_BIT(1),
        L_DEALER(1).

  DATA: L_RP_SHOP_DATE LIKE CABN-ATNAM.

  DATA: LT_AUSP_219 LIKE TABLE OF AUSP WITH HEADER LINE,
        LT_CABN LIKE TABLE OF CABN WITH HEADER LINE,
        L_CONF LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

  DATA: BEGIN OF LT_VIN OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
        ATWRT LIKE AUSP-ATWRT,
        END OF LT_VIN.
  DATA:LT_NATN LIKE TABLE OF LT_VIN WITH HEADER LINE,
       LT_ENG LIKE TABLE OF LT_VIN WITH HEADER LINE,
       LT_TM LIKE TABLE OF LT_VIN WITH HEADER LINE,
       LT_EXTC LIKE TABLE OF LT_VIN WITH HEADER LINE,
       LT_INTC LIKE TABLE OF LT_VIN WITH HEADER LINE,
       LT_ACTUAL LIKE TABLE OF LT_VIN WITH HEADER LINE,
****Begin of Change, Haseeb on request of Daniel 08-18-2009
       LT_MI LIKE TABLE OF LT_VIN WITH HEADER LINE,
       LT_OCN LIKE TABLE OF LT_VIN WITH HEADER LINE,
       LT_VERSION LIKE TABLE OF LT_VIN WITH HEADER LINE.
****End of Change Haseeb 08-18-2009
*  data: begin of lt_worder occurs 0,
*        objek like ausp-objek,
*        atwrt like ausp-atwrt,
*        end of lt_worder.

  DATA: BEGIN OF LT_AUSP_219VAL OCCURS 0,
         OBJEK LIKE AUSP-OBJEK,
         ATNAM LIKE CABN-ATNAM,
         ATINN LIKE AUSP-ATINN,
         ATWRT LIKE AUSP-ATWRT,
         END OF LT_AUSP_219VAL.

  DATA: BEGIN OF LT_AUSP OCCURS 0,
         ATNAM LIKE CABN-ATNAM,
         ATINN LIKE AUSP-ATINN,
         END OF LT_AUSP.

  RANGES: S_ATFLV FOR AUSP-ATFLV,
          S_219 FOR CABN-ATNAM.

  FIELD-SYMBOLS: <FS>.
  L_RP_SHOP_DATE = 'P_RP18_SHOP_DATE'.

  L_NUM = S_DATUM-LOW.
  S_ATFLV-LOW = L_NUM.
  L_NUM = S_DATUM-HIGH.
  S_ATFLV-HIGH = L_NUM.
  S_ATFLV-SIGN = S_DATUM-SIGN.
  S_ATFLV-OPTION = S_DATUM-OPTION.
  APPEND S_ATFLV.

  SELECT * INTO TABLE LT_CABN
   FROM CABN
   CLIENT SPECIFIED.

  SORT LT_CABN BY ATNAM.

  READ TABLE LT_CABN WITH KEY ATNAM = L_RP_SHOP_DATE.

  SELECT DISTINCT * INTO TABLE IT_AUSP
    FROM AUSP
    WHERE ATINN = LT_CABN-ATINN
     AND ATFLV IN S_ATFLV
     AND KLART = '002'.

** Changed by Furong on 12/04/08
  IF IT_AUSP[] IS INITIAL.
    EXIT.
  ENDIF.
** End of change on 12/04/08

*  READ TABLE lt_cabn WITH KEY atnam = 'P_WORK_ORDER'.
*  l_atinn = lt_cabn-atinn.
*
*  SELECT objek atwrt into table lt_worder
*      FROM ausp
*      for all entries in it_ausp
*      WHERE objek = it_ausp-objek
*        and atinn = l_atinn
*        AND klart = '002'.

*  READ TABLE LT_CABN WITH KEY ATNAM = 'P_NATN_CODE'.
*  L_ATINN = LT_CABN-ATINN.
*
*  SELECT OBJEK ATWRT INTO TABLE LT_NATN
*      FROM AUSP
*      FOR ALL ENTRIES IN IT_AUSP
*      WHERE OBJEK = IT_AUSP-OBJEK
*        AND ATINN = L_ATINN
*        AND KLART = '002'.

  READ TABLE LT_CABN WITH KEY ATNAM = 'P_DESTINATION_CODE'.
  L_ATINN = LT_CABN-ATINN.

  SELECT OBJEK ATWRT INTO TABLE LT_NATN
      FROM AUSP
      FOR ALL ENTRIES IN IT_AUSP
      WHERE OBJEK = IT_AUSP-OBJEK
        AND ATINN = L_ATINN
        AND KLART = '002'.

  READ TABLE LT_CABN WITH KEY ATNAM = 'P_ENGINE_NO'.
  L_ATINN = LT_CABN-ATINN.

  SELECT OBJEK ATWRT INTO TABLE LT_ENG
      FROM AUSP
      FOR ALL ENTRIES IN IT_AUSP
      WHERE OBJEK = IT_AUSP-OBJEK
        AND ATINN = L_ATINN
        AND KLART = '002'.

  READ TABLE LT_CABN WITH KEY ATNAM = 'P_TM_NO'.
  L_ATINN = LT_CABN-ATINN.

  SELECT OBJEK ATWRT INTO TABLE LT_TM
      FROM AUSP
      FOR ALL ENTRIES IN IT_AUSP
      WHERE OBJEK = IT_AUSP-OBJEK
        AND ATINN = L_ATINN
        AND KLART = '002'.

  READ TABLE LT_CABN WITH KEY ATNAM = 'P_EXT_COLOR'.
  L_ATINN = LT_CABN-ATINN.

  SELECT OBJEK ATWRT INTO TABLE LT_EXTC
      FROM AUSP
      FOR ALL ENTRIES IN IT_AUSP
      WHERE OBJEK = IT_AUSP-OBJEK
        AND ATINN = L_ATINN
        AND KLART = '002'.

  READ TABLE LT_CABN WITH KEY ATNAM = 'P_INT_COLOR'.
  L_ATINN = LT_CABN-ATINN.

  SELECT OBJEK ATWRT INTO TABLE LT_INTC
      FROM AUSP
      FOR ALL ENTRIES IN IT_AUSP
      WHERE OBJEK = IT_AUSP-OBJEK
        AND ATINN = L_ATINN
        AND KLART = '002'.


  READ TABLE LT_CABN WITH KEY ATNAM = 'P_VIN'.
  L_ATINN = LT_CABN-ATINN.

  SELECT OBJEK ATWRT INTO TABLE LT_VIN
      FROM AUSP
      FOR ALL ENTRIES IN IT_AUSP
      WHERE OBJEK = IT_AUSP-OBJEK
        AND ATINN = L_ATINN
        AND KLART = '002'.

  READ TABLE LT_CABN WITH KEY ATNAM = 'P_RP18_ACTUAL_DATE'.
  L_ATINN = LT_CABN-ATINN.

  SELECT OBJEK ATWRT INTO TABLE LT_ACTUAL
      FROM AUSP
      FOR ALL ENTRIES IN IT_AUSP
      WHERE OBJEK = IT_AUSP-OBJEK
        AND ATINN = L_ATINN
        AND KLART = '002'.
****Begin of Change, Haseeb on request of Daniel 08-18-2009
  READ TABLE LT_CABN WITH KEY ATNAM = 'P_MI'.
  L_ATINN = LT_CABN-ATINN.

  SELECT OBJEK ATWRT INTO TABLE LT_MI
      FROM AUSP
      FOR ALL ENTRIES IN IT_AUSP
      WHERE OBJEK = IT_AUSP-OBJEK
        AND ATINN = L_ATINN
        AND KLART = '002'.

  READ TABLE LT_CABN WITH KEY ATNAM = 'P_OCN'.
  L_ATINN = LT_CABN-ATINN.

  SELECT OBJEK ATWRT INTO TABLE LT_OCN
      FROM AUSP
      FOR ALL ENTRIES IN IT_AUSP
      WHERE OBJEK = IT_AUSP-OBJEK
        AND ATINN = L_ATINN
        AND KLART = '002'.

  READ TABLE LT_CABN WITH KEY ATNAM = 'P_VERSION'.
  L_ATINN = LT_CABN-ATINN.

  SELECT OBJEK ATWRT INTO TABLE LT_VERSION
      FROM AUSP
      FOR ALL ENTRIES IN IT_AUSP
      WHERE OBJEK = IT_AUSP-OBJEK
        AND ATINN = L_ATINN
        AND KLART = '002'.

****End of Change Haseeb 08-18-2009
  CLEAR L_NO.
  S_219-SIGN = 'I'.
  S_219-OPTION = 'EQ'.

  DO  9 TIMES.
    CLEAR: L_BIT.
    L_NO = L_NO + 1.
    CONCATENATE 'P_219_' L_NO+2(1)   INTO S_219-LOW.
    APPEND S_219.
  ENDDO.

  DO 90 TIMES.
    CLEAR: L_BIT.
    L_NO = L_NO + 1.
    CONCATENATE 'P_219_' L_NO+1(2)   INTO  S_219-LOW.
    APPEND S_219.
  ENDDO.

  DO 120 TIMES.
    CLEAR: L_BIT.
    L_NO = L_NO + 1.
    CONCATENATE 'P_219_' L_NO   INTO  S_219-LOW.
    APPEND S_219.
  ENDDO.

*  DO  9 TIMES.
*    CLEAR: L_BIT.
*    L_NO = L_NO + 1.
*    CONCATENATE 'P_219_' L_NO+2(1)   INTO LT_AUSP-ATNAM.
*
*    APPEND LT_AUSP.
*    CLEAR: LT_AUSP.
*  ENDDO.
*
*  DO 90 TIMES.
*    CLEAR: L_BIT.
*    L_NO = L_NO + 1.
*    CONCATENATE 'P_219_' L_NO+1(2)   INTO LT_AUSP-ATNAM.
*
*    APPEND LT_AUSP.
*    CLEAR: LT_AUSP.
*
*  ENDDO.
*
*  DO 120 TIMES.
*    CLEAR: L_BIT.
*    L_NO = L_NO + 1.
*    CONCATENATE 'P_219_' L_NO   INTO LT_AUSP-ATNAM.
*    APPEND LT_AUSP.
*    CLEAR: LT_AUSP.
*  ENDDO.

  SELECT OBJEK ATNAM A~ATINN ATWRT INTO TABLE LT_AUSP_219VAL
    FROM AUSP AS A
     INNER JOIN CABN AS B
     ON A~ATINN = B~ATINN
     FOR ALL ENTRIES IN IT_AUSP
     WHERE OBJEK = IT_AUSP-OBJEK
       AND KLART = '002'
       AND B~ATNAM IN S_219.

  SORT LT_AUSP_219VAL BY OBJEK ATNAM.

  LOOP AT IT_AUSP.
    CLEAR: L_MATNR,L_NO,L_NAME, L_DEALER.
    L_DATE = L_NUM_D = IT_AUSP-ATFLV.

    IT_VIN_219-PROD_DATE = L_DATE.

    READ TABLE LT_NATN WITH KEY OBJEK = IT_AUSP-OBJEK.
    IT_VIN_219-NATN_CODE = LT_NATN-ATWRT+0(3).
    L_DEALER = LT_NATN-ATWRT+3(1).
    IF L_DEALER = 'A'.

      READ TABLE LT_VIN WITH KEY OBJEK = IT_AUSP-OBJEK.
      IT_VIN_219-VIN = LT_VIN-ATWRT.

      READ TABLE LT_ENG WITH KEY OBJEK = IT_AUSP-OBJEK.
      IT_VIN_219-ENGINE_NO = LT_ENG-ATWRT.

      READ TABLE LT_TM WITH KEY OBJEK = IT_AUSP-OBJEK.
      IT_VIN_219-TM_NO = LT_TM-ATWRT.

      READ TABLE LT_EXTC WITH KEY OBJEK = IT_AUSP-OBJEK.
      IT_VIN_219-EXT_COL = LT_EXTC-ATWRT.

      READ TABLE LT_INTC WITH KEY OBJEK = IT_AUSP-OBJEK.
      IT_VIN_219-INT_COL = LT_INTC-ATWRT.

      READ TABLE LT_ACTUAL WITH KEY OBJEK = IT_AUSP-OBJEK.
      IT_VIN_219-ACT_DATE = LT_ACTUAL-ATWRT.
****Begin of Change, Haseeb on request of Daniel 08-18-2009
      READ TABLE LT_MI WITH KEY OBJEK = IT_AUSP-OBJEK.
      IT_VIN_219-MI = LT_MI-ATWRT.

      READ TABLE LT_OCN WITH KEY OBJEK = IT_AUSP-OBJEK.
      IT_VIN_219-OCN = LT_OCN-ATWRT.

      READ TABLE LT_VERSION WITH KEY OBJEK = IT_AUSP-OBJEK.
      IT_VIN_219-VERSION = LT_VERSION-ATWRT.
****End of Change Haseeb 08-18-2009
      IT_VIN_219-MODEL = IT_AUSP-OBJEK+0(3).
      IT_VIN_219-BODYNO = IT_AUSP-OBJEK+3(6).

*    SELECT OBJEK ATNAM A~ATINN ATWRT INTO TABLE LT_AUSP_219VAL
*    FROM AUSP AS A
*     INNER JOIN CABN AS B
*     ON A~ATINN = B~ATINN
*     FOR ALL ENTRIES IN LT_AUSP
*     WHERE OBJEK = IT_AUSP-OBJEK
*       AND KLART = '002'
*       AND B~ATNAM = LT_AUSP-ATNAM.

      DO  9 TIMES.
        CLEAR: L_BIT.
        L_NO = L_NO + 1.
        CONCATENATE 'P_219_' L_NO+2(1)   INTO L_NAME.
        READ TABLE LT_AUSP_219VAL WITH KEY OBJEK = IT_AUSP-OBJEK
                                           ATNAM = L_NAME
                                           BINARY SEARCH.
        CONCATENATE 'IT_VIN_219-C219_' L_NO+2(1)   INTO L_NAME.
        ASSIGN (L_NAME) TO <FS>.
        IF SY-SUBRC = 0.
          IF LT_AUSP_219VAL-ATWRT = '-'.
          ELSE.
            <FS> = LT_AUSP_219VAL-ATWRT.
          ENDIF.
        ENDIF.
      ENDDO.

      DO 90 TIMES.
        CLEAR: L_BIT.
        L_NO = L_NO + 1.
        CONCATENATE 'P_219_' L_NO+1(2)   INTO L_NAME.
        READ TABLE LT_AUSP_219VAL WITH KEY OBJEK = IT_AUSP-OBJEK
                                           ATNAM = L_NAME
                                           BINARY SEARCH.
        CONCATENATE 'IT_VIN_219-C219_' L_NO+1(2)  INTO L_NAME.
        ASSIGN (L_NAME) TO <FS>.
        IF SY-SUBRC = 0.
          IF LT_AUSP_219VAL-ATWRT = '-'.
          ELSE.
            <FS> = LT_AUSP_219VAL-ATWRT.
          ENDIF.
        ENDIF.
      ENDDO.

      DO 120 TIMES.
        CLEAR: L_BIT.
        L_NO = L_NO + 1.
        CONCATENATE 'P_219_' L_NO   INTO L_NAME.
        READ TABLE LT_AUSP_219VAL WITH KEY OBJEK = IT_AUSP-OBJEK
                                           ATNAM = L_NAME
                                           BINARY SEARCH.

        CONCATENATE 'IT_VIN_219-C219_' L_NO   INTO L_NAME.
        ASSIGN (L_NAME) TO <FS>.
        IF SY-SUBRC = 0.
          IF LT_AUSP_219VAL-ATWRT = '-'.
          ELSE.
            <FS> = LT_AUSP_219VAL-ATWRT.
          ENDIF.
        ENDIF.
      ENDDO.
      IT_VIN_219-CREATION_DATE = SY-DATUM.
      IT_VIN_219-CREATION_TIME = SY-UZEIT.
      IT_VIN_219-CRUSER = SY-UNAME.
      APPEND IT_VIN_219.
    ENDIF.
    CLEAR: IT_VIN_219, IT_AUSP, LT_VIN, LT_NATN, LT_ENG, LT_TM,
           LT_EXTC, LT_INTC, LT_ACTUAL.
  ENDLOOP.

ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  send_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_RTN.
  DATA : L_MSGTXT(100),
         L_RESULT(1),
         L_DATE LIKE SY-DATUM.

  CALL FUNCTION 'Z_FPP_VIN_219'
       DESTINATION C_DEST
   IMPORTING
     RESULT        = L_RESULT
    TABLES
      VIN_219       = IT_VIN_219
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

  IF SY-SUBRC = 0.
    MESSAGE I001 WITH 'Data successfully sent out'.
  ELSE.
    MESSAGE I001 WITH L_MSGTXT.
  ENDIF.

ENDFORM.                    " send_rtn
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA.
  DATA: L_DATE LIKE SY-DATUM.
  DELETE FROM ZTPP_VIN_219 WHERE PROD_DATE IN S_DATUM.
  IF P_DAYS IS INITIAL.
  ELSE.
    L_DATE = SY-DATUM - P_DAYS.
    DELETE FROM ZTPP_VIN_219 WHERE CREATION_DATE < L_DATE.
  ENDIF.

  MODIFY ZTPP_VIN_219 FROM TABLE IT_VIN_219.
*    MODIFY ZTPP_PLAN_HMC FROM TABLE IT_PLAN_DAY.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE I000 WITH 'Table update error'.
  ENDIF.
ENDFORM.                    " save_data
