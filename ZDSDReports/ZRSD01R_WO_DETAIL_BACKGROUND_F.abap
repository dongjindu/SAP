*----------------------------------------------------------------------*
*   INCLUDE ZRSD01R_WO_DETAIL_BACKGROUND_F                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.

  DATA: L_TABIX LIKE SY-TABIX,
        L_MON(6),
        L_WORK(14).

  DATA: BEGIN OF LTAB2 OCCURS 0,
           OBJEK LIKE AUSP-OBJEK,
           ATINN LIKE AUSP-ATINN,
           ATWRT LIKE AUSP-ATWRT,
           atflv like ausp-atflv,
        END OF LTAB2.

  data: w_n_8(8) type n.

  CONCATENATE 'E' P_MON '%' INTO L_MON.

  SELECT WO_SER  NATION  DEALER  EXTC
         INTC    FSC     MODQTY  SALES
         RP14DQ  RP16DQ
  INTO CORRESPONDING FIELDS OF TABLE LTAB1
  FROM ZTPP_WOSUM
  WHERE WO_SER LIKE L_MON
  AND   SEQQTY NE 0.

  CLEAR: LTAB1, L_TABIX.
  LOOP AT LTAB1.
     L_TABIX = SY-TABIX.
     IF LTAB1-MODQTY < LTAB1-RP14DQ
     OR LTAB1-MODQTY < LTAB1-RP16DQ.
        DELETE LTAB1 INDEX L_TABIX.
        CLEAR: L_TABIX.
        CONTINUE.
     ENDIF.

     CLEAR: L_TABIX.
  ENDLOOP.

  SORT LTAB1 BY WO_SER NATION DEALER EXTC INTC FSC SALES.
  DELETE ADJACENT DUPLICATES FROM LTAB1
         COMPARING WO_SER NATION DEALER EXTC INTC FSC SALES.

  PERFORM MAKE_CHAR_VALUE.

  CLEAR: LTAB1, SC, SO.
  REFRESH: SC, SO.
  LOOP AT LTAB1.

    CONCATENATE LTAB1-WO_SER LTAB1-NATION LTAB1-DEALER INTO L_WORK.

    SC-NAME_CHAR  = 'P_WORK_ORDER'.
    SC-CHAR_VALUE = L_WORK.
    APPEND SC.

    SC-NAME_CHAR  = 'P_EXT_COLOR'.
    SC-CHAR_VALUE = LTAB1-EXTC.
    APPEND SC.

    SC-NAME_CHAR  = 'P_INT_COLOR'.
    SC-CHAR_VALUE = LTAB1-INTC.
    APPEND SC.

    SC-NAME_CHAR  = 'P_SALES_ORDER'.
    SC-CHAR_VALUE = LTAB1-SALES.
    APPEND SC.

**    SC-NAME_CHAR  = 'P_FSC'.
**    SC-CHAR_VALUE = LTAB1-FSC.
**    APPEND SC.

    CALL FUNCTION 'BAPI_CLASS_SELECT_OBJECTS'
      EXPORTING
        CLASSTYPE                  = '002'
        CLASSNUM                   = 'P_VEHICLE_MASTER'
*       LANGUISO                   =
*       LANGUINT                   =
        KEYDATE                    = SY-DATUM
*       MAXHITS                    = 100
*       I_STATUS_LOCKED            = ' '
*       I_STATUS_INCOMPLETE        = ' '
*     IMPORTING
*       RETURN                     =
      TABLES
        SELECTIONCRITERIONS        = SC
        SELECTEDOBJECTS            = SO
*       OBJECTCLASSIFICATION       =
              .

    CLEAR: SO, LTAB2.
    REFRESH: LTAB2.
    LOOP AT SO.

      CLEAR: LTAB2.
      REFRESH: LTAB2.

      SELECT OBJEK ATINN ATWRT atflv
      INTO   TABLE LTAB2
      FROM   AUSP
      WHERE  ATINN IN S_ATINN
      AND    OBJEK EQ SO-OBJECT.

      MOVE: LTAB1-WO_SER  TO LTAB3-WO_SER,
            LTAB1-NATION  TO LTAB3-NATION,
            LTAB1-DEALER  TO LTAB3-DEALER,
            LTAB1-EXTC    TO LTAB3-EXTC,
            LTAB1-INTC    TO LTAB3-INTC,
            LTAB1-FSC     TO LTAB3-FSC,
            LTAB1-MODQTY  TO LTAB3-MODQTY,
            LTAB1-SALES   TO LTAB3-SALES.

      CLEAR: LTAB2.
      LOOP AT LTAB2.

        READ TABLE LTAB4 WITH KEY ATNAM = 'P_VIN'.
        IF SY-SUBRC = 0.
          READ TABLE LTAB2 WITH KEY ATINN = LTAB4-ATINN.
          IF SY-SUBRC = 0.
            MOVE: LTAB2-ATWRT TO LTAB3-ZFVIN.
            CLEAR: LTAB2.
*            CONTINUE.
          ENDIF.
        ENDIF.

        READ TABLE LTAB4 WITH KEY ATNAM = 'P_SEQUENCE_DATE'.
        IF SY-SUBRC = 0.
          READ TABLE LTAB2 WITH KEY ATINN = LTAB4-ATINN.
          IF SY-SUBRC = 0.
            w_n_8 = ltab2-atflv.
            ltab3-zdate1 = w_n_8.
*****       MOVE: LTAB2-ATWRT TO LTAB3-ZDATE1.
            CLEAR: LTAB2.
*            CONTINUE.
          ENDIF.
        ENDIF.

        READ TABLE LTAB4 WITH KEY ATNAM = 'P_RP07_SHOP_DATE'.
        IF SY-SUBRC = 0.
          READ TABLE LTAB2 WITH KEY ATINN = LTAB4-ATINN.
          IF SY-SUBRC = 0.
            w_n_8 = ltab2-atflv.
            ltab3-zdate2 = w_n_8.
*****       MOVE: LTAB2-ATWRT TO LTAB3-ZDATE2.
            CLEAR: LTAB2.
*            CONTINUE.
          ENDIF.
        ENDIF.

*        READ TABLE LTAB4 WITH KEY ATNAM = 'P_RP17_SHOP_DATE'.
*        IF SY-SUBRC = 0.
*          READ TABLE LTAB2 WITH KEY ATINN = LTAB4-ATINN.
*          IF SY-SUBRC = 0.
*            w_n_8 = ltab2-atflv.
*            ltab3-zdate3 = w_n_8.
******       MOVE: LTAB2-ATWRT TO LTAB3-ZDATE3.
*            CLEAR: LTAB2.
*          ENDIF.
*        ENDIF.

        READ TABLE LTAB4 WITH KEY ATNAM = 'P_RP18_SHOP_DATE'.
        IF SY-SUBRC = 0.
          READ TABLE LTAB2 WITH KEY ATINN = LTAB4-ATINN.
          IF SY-SUBRC = 0.
            w_n_8 = ltab2-atflv.
            ltab3-zdate3 = w_n_8.
*****       MOVE: LTAB2-ATWRT TO LTAB3-ZDATE3.
            CLEAR: LTAB2.
*            CONTINUE.
          ENDIF.
        ENDIF.

        READ TABLE LTAB4 WITH KEY ATNAM = 'P_RP19_SHOP_DATE'.
        IF SY-SUBRC = 0.
          READ TABLE LTAB2 WITH KEY ATINN = LTAB4-ATINN.
          IF SY-SUBRC = 0.
            w_n_8 = ltab2-atflv.
            ltab3-zdate4 = w_n_8.
*****       MOVE: LTAB2-ATWRT TO LTAB3-ZDATE4.
            CLEAR: LTAB2.
          ENDIF.
        ENDIF.

        READ TABLE LTAB4 WITH KEY ATNAM = 'P_RP21_SHOP_DATE'.
        IF SY-SUBRC = 0.
          READ TABLE LTAB2 WITH KEY ATINN = LTAB4-ATINN.
          IF SY-SUBRC = 0.
            w_n_8 = ltab2-atflv.
            ltab3-zdate5 = w_n_8.
*****       MOVE: LTAB2-ATWRT TO LTAB3-ZDATE5.
            CLEAR: LTAB2.
*            CONTINUE.
          ENDIF.
        ENDIF.

        READ TABLE LTAB4 WITH KEY ATNAM = 'P_RP27_SHOP_DATE'.
        IF SY-SUBRC = 0.
          READ TABLE LTAB2 WITH KEY ATINN = LTAB4-ATINN.
          IF SY-SUBRC = 0.
            w_n_8 = ltab2-atflv.
            ltab3-zdate6 = w_n_8.
*****       MOVE: LTAB2-ATWRT TO LTAB3-ZDATE6.
            CLEAR: LTAB2.
*            CONTINUE.
          ENDIF.
        ENDIF.

        READ TABLE LTAB4 WITH KEY ATNAM = 'P_RP28_SHOP_DATE'.
        IF SY-SUBRC = 0.
          READ TABLE LTAB2 WITH KEY ATINN = LTAB4-ATINN.
          IF SY-SUBRC = 0.
            w_n_8 = ltab2-atflv.
            ltab3-zdate7 = w_n_8.
*****       MOVE: LTAB2-ATWRT TO LTAB3-ZDATE7.
            CLEAR: LTAB2.
*            CONTINUE.
          ENDIF.
        ENDIF.

      ENDLOOP.
      APPEND LTAB3.
      CLEAR: LTAB3.
    ENDLOOP.

    CLEAR: LTAB1, SC, SO.
    REFRESH: SC, SO.
  ENDLOOP.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  MAKE_CHAR_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_CHAR_VALUE.

  DATA: BEGIN OF L_ATNAM OCCURS 0,
           ATNAM LIKE CABN-ATNAM,
        END OF L_ATNAM.

  L_ATNAM-ATNAM = 'P_VIN'.
  APPEND L_ATNAM.

  L_ATNAM-ATNAM = 'P_SEQUENCE_DATE'.
  APPEND L_ATNAM.

  L_ATNAM-ATNAM = 'P_RP07_SHOP_DATE'.
  APPEND L_ATNAM.

  L_ATNAM-ATNAM = 'P_RP17_SHOP_DATE'.
  APPEND L_ATNAM.

  L_ATNAM-ATNAM = 'P_RP18_SHOP_DATE'.
  APPEND L_ATNAM.

  L_ATNAM-ATNAM = 'P_RP19_SHOP_DATE'.
  APPEND L_ATNAM.

  L_ATNAM-ATNAM = 'P_RP21_SHOP_DATE'.
  APPEND L_ATNAM.

  L_ATNAM-ATNAM = 'P_RP27_SHOP_DATE'.
  APPEND L_ATNAM.

  L_ATNAM-ATNAM = 'P_RP28_SHOP_DATE'.
  APPEND L_ATNAM.

  LOOP AT L_ATNAM.

    SELECT SINGLE ATINN
    INTO  S_ATINN-LOW
    FROM  CABN
    WHERE ATNAM EQ L_ATNAM-ATNAM.

    IF SY-SUBRC = 0.
      S_ATINN-SIGN = 'I'.
      S_ATINN-OPTION = 'EQ'.
      APPEND S_ATINN.

      LTAB4-ATNAM = L_ATNAM-ATNAM.
      LTAB4-ATINN = S_ATINN-LOW.
      APPEND LTAB4.
    ENDIF.
    CLEAR: L_ATNAM, S_ATINN, LTAB4.
  ENDLOOP.

ENDFORM.                    " MAKE_CHAR_VALUE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA.

  DATA: BEGIN OF LT1 OCCURS 0,
          WO_SER  LIKE ZTPP_WOSUM-WO_SER,
          NATION  LIKE ZTPP_WOSUM-NATION,
          DEALER  LIKE ZTPP_WOSUM-DEALER,
          EXTC    LIKE ZTPP_WOSUM-EXTC,
          INTC    LIKE ZTPP_WOSUM-INTC,
       END OF LT1.

  DATA: BEGIN OF LTAB5 OCCURS 0.
          INCLUDE STRUCTURE ZSSD_WO_DETAIL_BACKGROUND.
  DATA: END OF LTAB5.

  DATA: L_NUM1 TYPE I,
        L_NUM2 TYPE I,
        L_NUM3 TYPE I,
        L_NUM4 TYPE I,
        L_NUM5 TYPE I,
        L_NUM6 TYPE I,
        L_NUM7 TYPE I.

*  CLEAR: LTAB3.
*  LOOP AT LTAB3 INTO T_ITAB1.
*
*    MOVE-CORRESPONDING T_ITAB1 TO LT1.
*    COLLECT LT1.
*  ENDLOOP.
  DATA: L_TABIX LIKE SY-TABIX.

  CLEAR: LTAB3.
  LOOP AT LTAB3 .
    L_TABIX = SY-TABIX.

**    IF NOT LTAB3-ZDATE7 IS INITIAL.
**       LTAB3-ZDATE1 = ''.
**       LTAB3-ZDATE2 = ''.
**       LTAB3-ZDATE3 = ''.
**       LTAB3-ZDATE4 = ''.
**       LTAB3-ZDATE5 = ''.
**       LTAB3-ZDATE6 = ''.
**    ENDIF.
**
**    IF NOT LTAB3-ZDATE6 IS INITIAL.
**       LTAB3-ZDATE1 = ''.
**       LTAB3-ZDATE2 = ''.
**       LTAB3-ZDATE3 = ''.
**       LTAB3-ZDATE4 = ''.
**       LTAB3-ZDATE5 = ''.
**    ENDIF.
**
**    IF NOT LTAB3-ZDATE5 IS INITIAL.
**       LTAB3-ZDATE1 = ''.
**       LTAB3-ZDATE2 = ''.
**       LTAB3-ZDATE3 = ''.
**       LTAB3-ZDATE4 = ''.
**    ENDIF.
**
**    IF NOT LTAB3-ZDATE4 IS INITIAL.
**       LTAB3-ZDATE1 = ''.
**       LTAB3-ZDATE2 = ''.
**       LTAB3-ZDATE3 = ''.
**    ENDIF.
**
**    IF NOT LTAB3-ZDATE3 IS INITIAL.
**       LTAB3-ZDATE1 = ''.
**       LTAB3-ZDATE2 = ''.
**    ENDIF.
**
**    IF NOT LTAB3-ZDATE2 IS INITIAL.
**       LTAB3-ZDATE1 = ''.
**    ENDIF.

    IF NOT LTAB3-ZDATE1 IS INITIAL.
    CONCATENATE LTAB3-ZDATE1+4(2) '/' LTAB3-ZDATE1+6(2) '/'
                LTAB3-ZDATE1+0(4)  INTO LTAB3-ZDATE1.
    ENDIF.
    IF NOT LTAB3-ZDATE2 IS INITIAL.
    CONCATENATE LTAB3-ZDATE2+4(2) '/' LTAB3-ZDATE2+6(2) '/'
                LTAB3-ZDATE2+0(4)  INTO LTAB3-ZDATE2.
    ENDIF.
    IF NOT LTAB3-ZDATE3 IS INITIAL.
    CONCATENATE LTAB3-ZDATE3+4(2) '/' LTAB3-ZDATE3+6(2) '/'
                LTAB3-ZDATE3+0(4)  INTO LTAB3-ZDATE3.
    ENDIF.
    IF NOT LTAB3-ZDATE4 IS INITIAL.
    CONCATENATE LTAB3-ZDATE4+4(2) '/' LTAB3-ZDATE4+6(2) '/'
                LTAB3-ZDATE4+0(4)  INTO LTAB3-ZDATE4.
    ENDIF.
    IF NOT LTAB3-ZDATE5 IS INITIAL.
    CONCATENATE LTAB3-ZDATE5+4(2) '/' LTAB3-ZDATE5+6(2) '/'
                LTAB3-ZDATE5+0(4)  INTO LTAB3-ZDATE5.
    ENDIF.
    IF NOT LTAB3-ZDATE6 IS INITIAL.
    CONCATENATE LTAB3-ZDATE6+4(2) '/' LTAB3-ZDATE6+6(2) '/'
                LTAB3-ZDATE6+0(4)  INTO LTAB3-ZDATE6.
    ENDIF.
    IF NOT LTAB3-ZDATE7 IS INITIAL.
    CONCATENATE LTAB3-ZDATE7+4(2) '/' LTAB3-ZDATE7+6(2) '/'
                LTAB3-ZDATE7+0(4)  INTO LTAB3-ZDATE7.
    ENDIF.

    MODIFY LTAB3 INDEX L_TABIX.

    MOVE-CORRESPONDING LTAB3 TO LT1.
    COLLECT LT1.
    CLEAR: L_TABIX, LTAB3.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM LTAB1
         COMPARING WO_SER NATION DEALER EXTC INTC.

  CLEAR: LT1.
  LOOP AT LT1.
    CLEAR: LTAB3,  L_NUM1, L_NUM2, L_NUM3,
           L_NUM4, L_NUM5, L_NUM6, L_NUM7.

    LOOP AT LTAB3  WHERE WO_SER = LT1-WO_SER
                   AND   NATION = LT1-NATION
                   AND   DEALER = LT1-DEALER
                   AND   EXTC   = LT1-EXTC
                   AND   INTC   = LT1-INTC.

      IF NOT LTAB3-ZDATE1 IS INITIAL.
        L_NUM1 = L_NUM1 + 1.
      ENDIF.

      IF NOT LTAB3-ZDATE2 IS INITIAL.
        L_NUM2 = L_NUM2 + 1.
      ENDIF.

      IF NOT LTAB3-ZDATE3 IS INITIAL.
        L_NUM3 = L_NUM3 + 1.
      ENDIF.

      IF NOT LTAB3-ZDATE4 IS INITIAL.
        L_NUM4 = L_NUM4 + 1.
      ENDIF.

      IF NOT LTAB3-ZDATE5 IS INITIAL.
        L_NUM5 = L_NUM5 + 1.
      ENDIF.

      IF NOT LTAB3-ZDATE6 IS INITIAL.
        L_NUM6 = L_NUM6 + 1.
      ENDIF.

      IF NOT LTAB3-ZDATE7 IS INITIAL.
        L_NUM7 = L_NUM7 + 1.
      ENDIF.
    ENDLOOP.

    MOVE-CORRESPONDING LT1 TO LTAB5.

    MOVE: L_NUM1 TO LTAB5-ZDATE1,
          L_NUM2 TO LTAB5-ZDATE2,
          L_NUM3 TO LTAB5-ZDATE3,
          L_NUM4 TO LTAB5-ZDATE4,
          L_NUM5 TO LTAB5-ZDATE5,
          L_NUM6 TO LTAB5-ZDATE6,
          L_NUM7 TO LTAB5-ZDATE7.
    MOVE: LTAB3-MODQTY TO LTAB5-MODQTY.
    APPEND LTAB5.
    CLEAR: LT1.
  ENDLOOP.

  LOOP AT LTAB5.
    LTAB5-BAKQTY = LTAB5-MODQTY - LTAB5-ZDATE5
                   - LTAB5-ZDATE6 .

    MOVE-CORRESPONDING LTAB5 TO LTAB3.
    APPEND LTAB3.
    CLEAR: LTAB5, LTAB3.
  ENDLOOP.

**  SORT LTAB3 BY  WO_SER NATION DEALER EXTC INTC FSC SALES ZDATE1
**                 ZDATE2 ZDATE3 ZDATE4 ZDATE5 ZDATE6 ZDATE7 .
  SORT LTAB3 BY  WO_SER NATION DEALER EXTC INTC FSC SALES ZFVIN.

  DELETE ADJACENT DUPLICATES FROM LTAB3
         COMPARING WO_SER NATION DEALER EXTC INTC FSC SALES ZFVIN.

  CLEAR: LTAB3.
  LOOP AT LTAB3 INTO T_ITAB1.
    IF  NOT T_ITAB1-FSC   IS INITIAL
    AND NOT T_ITAB1-SALES IS INITIAL
    AND NOT T_ITAB1-ZFVIN IS INITIAL.
        T_ITAB1-MODQTY = 0.
    ENDIF.
    APPEND T_ITAB1 TO ITAB1.
    CLEAR: T_ITAB1.
  ENDLOOP.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_SCREEN.

  DESCRIBE TABLE ITAB1 LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
  ELSE.
    CALL SCREEN 9000.
  ENDIF.

ENDFORM.                    " CALL_SCREEN
