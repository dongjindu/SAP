*----------------------------------------------------------------------*
***INCLUDE MZPP_APPLICATIONF06 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MAKE_DATA_SUM01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DATA_SUM01.
  DATA: LT_SUM01         LIKE TABLE OF ZTPP_SEQ_SUM01  WITH HEADER LINE,
        LT_COLLECT       LIKE TABLE OF ZTPP_SEQ_SUM01  WITH HEADER LINE,
        L_NO(3)          TYPE N.

* Reporting Point, Column, Model, Code .
  SELECT * INTO TABLE LT_SUM01
    FROM ZTPP_SEQ_SUM01       .

  SORT LT_SUM01 BY RP ALC_CODE MODEL ALC_VALS .
  LOOP AT LT_SUM01.
    CLEAR LT_COLLECT.
    MOVE-CORRESPONDING LT_SUM01 TO LT_COLLECT.
*   Code(Code Number conversion)
    LT_COLLECT-ALC_CODE = L_NO = LT_SUM01-ALC_CODE+1(3) .
    CONCATENATE LT_SUM01-ALC_CODE(1)  L_NO  INTO  LT_COLLECT-ALC_CODE .
    COLLECT LT_COLLECT.
  ENDLOOP.

*  loop at lt_collect.
*    lt_collect-t01 = lt_collect-h02 + lt_collect-h04 + lt_collect-h06 +
*    lt_collect-h08 + lt_collect-h10 + lt_collect-h12 + lt_collect-h14 +
*                     lt_collect-h16 + lt_collect-h18 + lt_collect-h20 .
*    lt_collect-t02 = lt_collect-h22 + lt_collect-h24 + lt_collect-h26 +
*    lt_collect-h28 + lt_collect-h30 + lt_collect-h32 + lt_collect-h34 +
*                     lt_collect-h36 + lt_collect-h38 + lt_collect-h40 .
*    lt_collect-t03 = lt_collect-h42 + lt_collect-h44 + lt_collect-h46 +
*    lt_collect-h48 + lt_collect-h50 + lt_collect-h52 + lt_collect-h54 +
*                     lt_collect-h56 + lt_collect-h58 + lt_collect-h60 .
*    lt_collect-gtot = lt_collect-STOT + lt_collect-mitu.
*    modify lt_collect         .
*    move-corresponding lt_collect to it_data.
*    append it_data.
*  endloop.
ENDFORM.                    " MAKE_DATA_SUM01
*&---------------------------------------------------------------------*
*&      Form  excel_down_8081
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCEL_DOWN_8081.
  CLEAR IT_EXCEL_8081.
  REFRESH IT_EXCEL_8081.
  MOVE 'Nation' TO IT_EXCEL_8081-COL01.
  MOVE 'Order No' TO IT_EXCEL_8081-COL02.
  MOVE 'FSC' TO IT_EXCEL_8081-COL24.
  MOVE 'Extc' TO IT_EXCEL_8081-COL03.
  MOVE 'Intc' TO IT_EXCEL_8081-COL04.
  MOVE 'Mod Qty' TO IT_EXCEL_8081-COL05.
  MOVE 'Seq Qty' TO IT_EXCEL_8081-COL06.
  MOVE 'Body In' TO IT_EXCEL_8081-COL07.
  MOVE 'Trim In' TO IT_EXCEL_8081-COL08.
  MOVE 'S Off' TO IT_EXCEL_8081-COL09.
  MOVE 'C/Gate' TO IT_EXCEL_8081-COL10.
  MOVE 'Ship In' TO IT_EXCEL_8081-COL11.
  MOVE TL_8081_OUTPUT-T_C TO IT_EXCEL_8081-COL12.
  MOVE TL_8081_OUTPUT-T_01 TO IT_EXCEL_8081-COL13.
  MOVE TL_8081_OUTPUT-T_02 TO IT_EXCEL_8081-COL14.
  MOVE TL_8081_OUTPUT-T_03 TO IT_EXCEL_8081-COL15.
  MOVE TL_8081_OUTPUT-T_04 TO IT_EXCEL_8081-COL16.
  MOVE TL_8081_OUTPUT-T_05 TO IT_EXCEL_8081-COL17.
  MOVE TL_8081_OUTPUT-T_06 TO IT_EXCEL_8081-COL18.
  MOVE TL_8081_OUTPUT-T_07 TO IT_EXCEL_8081-COL19.
  MOVE TL_8081_OUTPUT-T_08 TO IT_EXCEL_8081-COL20.
  MOVE TL_8081_OUTPUT-T_09 TO IT_EXCEL_8081-COL21.
  MOVE TL_8081_OUTPUT-T_10 TO IT_EXCEL_8081-COL22.
  MOVE TL_8081_OUTPUT-T_11 TO IT_EXCEL_8081-COL23.
  APPEND IT_EXCEL_8081.
*
  LOOP AT IT_8081.
    CLEAR IT_EXCEL_8081.
    MOVE IT_8081-NATION TO IT_EXCEL_8081-COL01.
    MOVE IT_8081-ORDER TO IT_EXCEL_8081-COL02.
    MOVE IT_8081-FSC TO IT_EXCEL_8081-COL24.
    MOVE IT_8081-EXTC TO IT_EXCEL_8081-COL03.
    MOVE IT_8081-INTC TO IT_EXCEL_8081-COL04.
    MOVE IT_8081-MODQTY TO IT_EXCEL_8081-COL05.
    MOVE IT_8081-SEQ TO IT_EXCEL_8081-COL06.
    MOVE IT_8081-BIN TO IT_EXCEL_8081-COL07.
    MOVE IT_8081-TIN TO IT_EXCEL_8081-COL08.
    MOVE IT_8081-SOFF TO IT_EXCEL_8081-COL09.
    MOVE IT_8081-CGATE TO IT_EXCEL_8081-COL10.
*    MOVE it_8081-shipin TO it_excel_8081-col11.
    MOVE IT_8081-SHIPOUT TO IT_EXCEL_8081-COL11.
    MOVE IT_8081-NEWQTYC TO IT_EXCEL_8081-COL12.
    MOVE IT_8081-NEWQTY01 TO IT_EXCEL_8081-COL13.
    MOVE IT_8081-NEWQTY02 TO IT_EXCEL_8081-COL14.
    MOVE IT_8081-NEWQTY03 TO IT_EXCEL_8081-COL15.
    MOVE IT_8081-NEWQTY04 TO IT_EXCEL_8081-COL16.
    MOVE IT_8081-NEWQTY05 TO IT_EXCEL_8081-COL17.
    MOVE IT_8081-NEWQTY06 TO IT_EXCEL_8081-COL18.
    MOVE IT_8081-NEWQTY07 TO IT_EXCEL_8081-COL19.
    MOVE IT_8081-NEWQTY08 TO IT_EXCEL_8081-COL20.
    MOVE IT_8081-NEWQTY09 TO IT_EXCEL_8081-COL21.
    MOVE IT_8081-NEWQTY10 TO IT_EXCEL_8081-COL22.
    MOVE IT_8081-NEWQTY11 TO IT_EXCEL_8081-COL23.
    APPEND IT_EXCEL_8081.
  ENDLOOP.
*
  CALL FUNCTION 'DOWNLOAD'
   EXPORTING
     FILENAME                      = 'HMA_ORDER.XLS'
     FILETYPE                      = 'DAT'
     ITEM                          = ' '
*     FILETYPE_NO_CHANGE            = ' '
*     FILETYPE_NO_SHOW              = ' '
*     SILENT                        = 'S'
*     COL_SELECT                    = ' '
*     COL_SELECTMASK                = ' '
*     NO_AUTH_CHECK                 = ' '
*   IMPORTING
*     ACT_FILENAME                  =
*     ACT_FILETYPE                  =
*     FILESIZE                      =
*     CANCEL                        =
    TABLES
      DATA_TAB                      = IT_EXCEL_8081
*   EXCEPTIONS
*     INVALID_FILESIZE              = 1
*     INVALID_TABLE_WIDTH           = 2
*     INVALID_TYPE                  = 3
*     NO_BATCH                      = 4
*     UNKNOWN_ERROR                 = 5
*     GUI_REFUSE_FILETRANSFER       = 6
*     CUSTOMER_ERROR                = 7
*     OTHERS                        = 8
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " excel_down_8081
*&---------------------------------------------------------------------*
*&      Form  data_select_8081
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_SELECT_8081.
  DATA : BEGIN OF IT_WO OCCURS 0,
          WO_SER  LIKE ZTPP_WOSUM-WO_SER,    " WORK ORDER
          NATION  LIKE ZTPP_WOSUM-NATION,    " Nation
          DEALER  LIKE ZTPP_WOSUM-DEALER,    " Dealer
          FSC     LIKE ZTPP_WOSUM-FSC,
          EXTC    LIKE ZTPP_WOSUM-EXTC,
          INTC    LIKE ZTPP_WOSUM-INTC,
          MODQTY  LIKE ZTPP_WOSUM-MODQTY,    " Modify quntity
          SEQQTY  LIKE ZTPP_WOSUM-SEQQTY,
          RP01TQ  LIKE ZTPP_WOSUM-RP01TQ,   " B/IN
          RP07TQ  LIKE ZTPP_WOSUM-RP07TQ,    " T/IN
          RP08TQ  LIKE ZTPP_WOSUM-RP08TQ,    " S/OFF
          RP09TQ  LIKE ZTPP_WOSUM-RP09TQ,    " C/Gate
*          rp14tq  LIKE ztpp_wosum-rp14tq,   " ship/in
          RP15TQ  LIKE ZTPP_WOSUM-RP15TQ,   " ship/out
          OBJEK   LIKE AUSP-OBJEK,
         END OF IT_WO.

  DATA : BEGIN OF IT_WO_NO OCCURS 0,
            WO_SER  LIKE ZTPP_WOSUM-WO_SER,    " WORK ORDER
            NATION  LIKE ZTPP_WOSUM-NATION,    " Nation
            DEALER  LIKE ZTPP_WOSUM-DEALER,    " Dealer
            FSC     LIKE ZTPP_WOSUM-FSC,
            MODQTY  LIKE ZTPP_WOSUM-MODQTY,    " Modify quntity
            SEQQTY  LIKE ZTPP_WOSUM-SEQQTY,
            RP01TQ  LIKE ZTPP_WOSUM-RP01TQ,   " B/IN
            RP07TQ  LIKE ZTPP_WOSUM-RP07TQ,    " T/IN
            RP08TQ  LIKE ZTPP_WOSUM-RP08TQ,    " S/OFF
            RP09TQ  LIKE ZTPP_WOSUM-RP09TQ,    " C/Gate
** Changed by Furong on 04/11/07  >> Help desk no: 73VA39452A
**                                >> Transport UD1K940295
*            rp14tq  LIKE ztpp_wosum-rp14tq,   " ship/in
            RP15TQ  LIKE ZTPP_WOSUM-RP15TQ,   " ship/out
** end of change
            OBJEK   LIKE AUSP-OBJEK,
           END OF IT_WO_NO.

  DATA: WA_8081 LIKE LINE OF IT_8081.

  DATA : B_WO        LIKE ZTPP_WOSUM-WO_SER   ,
         E_WO        LIKE ZTPP_WOSUM-WO_SER   ,
         OBJEK       LIKE AUSP-OBJEK  ,
         L_PACK(4)   TYPE C,
         L_NAT(3)    TYPE C,
         L_DAT       TYPE D,
         L_VALS(8)   TYPE N,
         L_DEL(2)    TYPE C,
         CHECK       TYPE C,
         L_RECNO(2)  TYPE N,
         L_NEWQTY    LIKE ZTSD_SODATA-NEWQTY,
         L_YEAR(4)   TYPE N,
         L_MONTH(2)  TYPE N,
         L_BYEAR(4)   TYPE N,
         L_BMONTH(2)  TYPE N,
         L_TEXT(25).

  FIELD-SYMBOLS: <FIELD>, <FIELD_TL>.

  RANGES : R_NAT FOR  ZTPP_WOSUM-NATION,    " Nation
           R_DEL FOR  ZTPP_WOSUM-DEALER.

  RANGES: DDATE FOR SY-DATUM.

  IF ST_8081_INPUT-USE = 'D'.
    DDATE-SIGN = 'I'.
    DDATE-OPTION = 'BT'.
    CONCATENATE '20' ST_8081_INPUT-MONTY '01'
                 INTO DDATE-LOW.
    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        DAY_IN                  = DDATE-LOW
     IMPORTING
       LAST_DAY_OF_MONTH       = DDATE-HIGH
*          EXCEPTIONS
*            DAY_IN_NO_DATE          = 1
*            OTHERS                  = 2
    .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    APPEND DDATE.
  ENDIF.
  CLEAR: TL_8081_OUTPUT,TL_8081_OUTPUT[].


  CONCATENATE '20' ST_8081_INPUT-MONTY+(2) INTO L_BYEAR.
  L_BMONTH = ST_8081_INPUT-MONTY+2(2).
  CONCATENATE L_BMONTH '/' L_BYEAR INTO TL_8081_OUTPUT-T_C.
  L_RECNO = '00'.
  L_YEAR = L_BYEAR.
  L_MONTH = L_BMONTH.

  DO 11 TIMES.
    L_RECNO = L_RECNO + 1.
    CONCATENATE 'TL_8081_OUTPUT-T_' L_RECNO INTO L_TEXT.
    ASSIGN (L_TEXT) TO <FIELD_TL>.
    IF L_MONTH = '12'.
      L_MONTH = '01'.
      L_YEAR = L_YEAR + 1.
    ELSE.
      L_MONTH = L_MONTH + 1.
    ENDIF.
    CONCATENATE  L_MONTH '/'  L_YEAR INTO <FIELD_TL>.
  ENDDO.

  IF ST_8081_INPUT-NATION <> ''.
    CLEAR : R_NAT , R_NAT[].
    R_NAT-OPTION = 'EQ'.
    R_NAT-SIGN  = 'I'.
    R_NAT-LOW    = ST_8081_INPUT-NATION(3).
    APPEND R_NAT. CLEAR R_NAT.
  ENDIF.

  CLEAR : IT_8081, IT_8081[], IT_AUSP, IT_AUSP[], ST_8081_INPUT-QTY.

  IF ST_8081_INPUT-COLOR = 'Y'.
    SELECT WO_SER NATION DEALER FSC EXTC INTC SUM( MODQTY )
           SUM( SEQQTY ) SUM( RP01TQ ) SUM( RP07TQ )
** Changed by Furong on 04/11/07  >> Help desk no: 73VA39452A
**                                >> Transport UD1K940295
*           SUM( rp08tq ) SUM( rp09tq ) SUM( rp14tq )
           SUM( RP08TQ ) SUM( RP09TQ ) SUM( RP15TQ )
** end of change
          INTO TABLE IT_WO
          FROM  ZTPP_WOSUM
          WHERE  NATION IN R_NAT
          GROUP BY  WO_SER NATION DEALER FSC EXTC INTC.
  ELSE.
    SELECT WO_SER NATION DEALER FSC SUM( MODQTY )
           SUM( SEQQTY ) SUM( RP01TQ ) SUM( RP07TQ )
** Changed by Furong on 04/11/07  >> Help desk no: 73VA39452A
**                                >> Transport UD1K940295
*           SUM( rp08tq ) SUM( rp09tq ) SUM( rp14tq )
           SUM( RP08TQ ) SUM( RP09TQ ) SUM( RP15TQ )
** end of change
          INTO TABLE IT_WO_NO
          FROM  ZTPP_WOSUM
          WHERE  NATION IN R_NAT
          GROUP BY WO_SER NATION DEALER FSC.
    LOOP AT IT_WO_NO.
      MOVE-CORRESPONDING IT_WO_NO TO IT_WO.
      APPEND IT_WO.
      CLEAR: IT_WO.
    ENDLOOP.
    CLEAR: IT_WO_NO, IT_WO_NO[].
  ENDIF.
* objeck number concatenate.
  IF IT_WO[] IS INITIAL.
    MESSAGE S001  WITH  TEXT-100.
  ENDIF.

  LOOP AT IT_WO.
    CONCATENATE IT_WO-WO_SER IT_WO-NATION IT_WO-DEALER INTO IT_WO-OBJEK.
    MODIFY IT_WO.
  ENDLOOP.

  PERFORM RANGE_INSERT_8081.  " range value
  PERFORM RANGE_INSERT_8081_219. " 219 range value.

  LOOP AT IT_WO.
    CLEAR : L_NAT, L_DEL, CHECK.

    SELECT  OBJEK ATINN ATZHL MAFID KLART ADZHL ATWRT ATFLV ATNAM
        INTO CORRESPONDING FIELDS OF TABLE IT_AUSP
     FROM ZVPP_CHA
     WHERE OBJEK = IT_WO-OBJEK
       AND KLART = '001'
       AND ATNAM IN R_ATNAM
       AND LKENZ = ' ' .

    LOOP AT IT_AUSP.
      CASE  IT_AUSP-ATNAM.
        WHEN 'P_MODEL' .
          IF IT_AUSP-ATWRT  NE WA_MODEL        .
            CHECK = 'X' .
          ENDIF.
        WHEN 'P_219_19'.
          " Check the Input Option...
          MOVE : IT_AUSP-ATWRT TO IT_8081-219_19 .
          CHECK ST_8081_INPUT-USE NE SPACE     .

** Changed by Furong on 04/18/08
*          IF ST_8081_INPUT-USE NE IT_8081-219_19 .
*            CHECK = 'X' .
*          ENDIF.
          IF ST_8081_INPUT-USE = 'E'.

            ELSE.
              IF ST_8081_INPUT-USE NE IT_8081-219_19 .
                CHECK = 'X' .
              ENDIF.
            ENDIF.
          WHEN 'P_NATION'.
            IF ST_8081_INPUT-USE = 'E' AND IT_AUSP-ATWRT+0(3) = 'B28'.
              CHECK = 'X' .
            ENDIF.
** End of change

        WHEN 'P_WO_SER'.
          IF IT_AUSP-ATWRT+1(4) NE ST_8081_INPUT-MONTY.
            CHECK = 'X'.
          ENDIF.
      ENDCASE.

      CASE IT_AUSP-ATNAM+6(3).
        WHEN ''.

        WHEN ST_8081_INPUT-219_1.
          IF ST_8081_INPUT-219_1_V <> '' AND
             ST_8081_INPUT-219_1_V <> IT_AUSP-ATWRT.
            CHECK = 'X'.
          ENDIF.

*          MOVE : it_ausp-atwrt TO it_8081-219_1.

        WHEN ST_8081_INPUT-219_2.
          IF ST_8081_INPUT-219_2_V <> '' AND
             ST_8081_INPUT-219_2_V <> IT_AUSP-ATWRT.
            CHECK = 'X'.
          ENDIF.

*          MOVE : it_ausp-atwrt TO it_8081-219_2.

        WHEN ST_8081_INPUT-219_3.
          IF ST_8081_INPUT-219_3_V <> '' AND
             ST_8081_INPUT-219_3_V <> IT_AUSP-ATWRT.
            CHECK = 'X'.
          ENDIF.

*          MOVE : it_ausp-atwrt TO it_8081-219_3.

        WHEN ST_8081_INPUT-219_4.
          IF ST_8081_INPUT-219_4_V <> '' AND
             ST_8081_INPUT-219_4_V <> IT_AUSP-ATWRT.
            CHECK = 'X'.
          ENDIF.

*          MOVE : it_ausp-atwrt TO it_8081-219_4.

        WHEN ST_8081_INPUT-219_5.
          IF ST_8081_INPUT-219_5_V <> '' AND
             ST_8081_INPUT-219_5_V <> IT_AUSP-ATWRT.
            CHECK = 'X'.
          ENDIF.

*          MOVE : it_ausp-atwrt TO it_8081-219_5.

        WHEN ST_8081_INPUT-219_6.
          IF ST_8081_INPUT-219_6_V <> '' AND
             ST_8081_INPUT-219_6_V <> IT_AUSP-ATWRT.
            CHECK = 'X'.
          ENDIF.

*          MOVE : it_ausp-atwrt TO it_8081-219_6.

        WHEN ST_8081_INPUT-219_7.
          IF ST_8081_INPUT-219_7_V <> '' AND
             ST_8081_INPUT-219_7_V <> IT_AUSP-ATWRT.
            CHECK = 'X'.
          ENDIF.

*          MOVE : it_ausp-atwrt TO it_8081-219_7.

        WHEN ST_8081_INPUT-219_8.
          IF ST_8081_INPUT-219_8_V <> '' AND
             ST_8081_INPUT-219_8_V <> IT_AUSP-ATWRT.
            CHECK = 'X'.
          ENDIF.

*          MOVE : it_ausp-atwrt TO it_8081-219_8.

        WHEN ST_8081_INPUT-219_9.
          IF ST_8081_INPUT-219_9_V <> '' AND
             ST_8081_INPUT-219_9_V <> IT_AUSP-ATWRT.
            CHECK = 'X'.
          ENDIF.

*          MOVE : it_ausp-atwrt TO it_8081-219_9.

        WHEN ST_8081_INPUT-219_10.
          IF ST_8081_INPUT-219_10_V <> '' AND
             ST_8081_INPUT-219_10_V <> IT_AUSP-ATWRT.
            CHECK = 'X'.
          ENDIF.

*          MOVE : it_ausp-atwrt TO it_8081-219_10.
      ENDCASE.

    ENDLOOP.

    MOVE : IT_WO-WO_SER TO IT_8081-ORDER,
           IT_WO-FSC TO IT_8081-FSC,
           IT_WO-MODQTY TO IT_8081-MODQTY,
           IT_WO-EXTC TO IT_8081-EXTC,
           IT_WO-INTC TO IT_8081-INTC,
           IT_WO-SEQQTY TO IT_8081-SEQ,
           IT_WO-RP01TQ TO IT_8081-BIN,
           IT_WO-RP07TQ TO IT_8081-TIN,
           IT_WO-RP08TQ TO IT_8081-SOFF,
           IT_WO-RP09TQ TO IT_8081-CGATE,
** Changed by Furong on 04/11/07  >> Help desk no: 73VA39452A
**                                >> Transport UD1K940295
*           it_wo-rp14tq TO it_8081-shipin.
           IT_WO-RP15TQ TO IT_8081-SHIPOUT.
** end of change
    CONCATENATE IT_WO-NATION IT_WO-DEALER INTO IT_8081-NATION.

    IF CHECK = ''.
      ST_8081_INPUT-QTY = IT_8081-MODQTY + ST_8081_INPUT-QTY.

      IF ST_8081_INPUT-USE = 'D'.
        L_YEAR = L_BYEAR.
        L_MONTH = L_BMONTH.

        IF ST_8081_INPUT-COLOR = 'Y'.
          SELECT SINGLE SUM( NEWQTY ) INTO IT_8081-NEWQTYC
            FROM ZTSD_SODATA
            WHERE WO_SER = IT_WO-WO_SER
              AND NATION = IT_WO-NATION
              AND DEALER = IT_WO-DEALER
              AND EXTC = IT_WO-EXTC
              AND INTC = IT_WO-INTC
              AND POYEAR = L_YEAR
              AND POMONTH = L_MONTH
          GROUP BY WO_SER NATION DEALER EXTC INTC.
        ELSE.
          SELECT SINGLE SUM( NEWQTY ) INTO IT_8081-NEWQTYC
          FROM ZTSD_SODATA
          WHERE WO_SER = IT_WO-WO_SER
            AND NATION = IT_WO-NATION
            AND DEALER = IT_WO-DEALER
            AND POYEAR = L_YEAR
             AND POMONTH = L_MONTH
          GROUP BY WO_SER NATION DEALER.
        ENDIF.
        L_RECNO = '00'.
        DO 11 TIMES.
          L_RECNO = L_RECNO + 1.
          CONCATENATE 'IT_8081-NEWQTY' L_RECNO INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FIELD>.
          IF L_MONTH = '12'.
            L_MONTH = '01'.
            L_YEAR = L_YEAR + 1.
          ELSE.
            L_MONTH = L_MONTH + 1.
          ENDIF.
          IF ST_8081_INPUT-COLOR = 'Y'.
            SELECT SINGLE SUM( NEWQTY ) INTO L_NEWQTY
              FROM ZTSD_SODATA
              WHERE WO_SER = IT_WO-WO_SER
                AND NATION = IT_WO-NATION
                AND DEALER = IT_WO-DEALER
                AND EXTC = IT_WO-EXTC
                AND INTC = IT_WO-INTC
                AND POYEAR = L_YEAR
                AND POMONTH = L_MONTH
              GROUP BY WO_SER NATION DEALER EXTC INTC.
          ELSE.
            SELECT SINGLE SUM( NEWQTY ) INTO L_NEWQTY
            FROM ZTSD_SODATA
            WHERE WO_SER = IT_WO-WO_SER
              AND NATION = IT_WO-NATION
              AND DEALER = IT_WO-DEALER
              AND POYEAR = L_YEAR
              AND POMONTH = L_MONTH
            GROUP BY WO_SER NATION DEALER.
          ENDIF.
          MOVE: L_NEWQTY TO <FIELD>.
          CLEAR: L_NEWQTY.
        ENDDO.
      ENDIF.
      APPEND IT_8081. CLEAR IT_8081.
    ENDIF.
  ENDLOOP.
  LOOP AT IT_8081.
    AT LAST.
      SUM.
      WA_8081-MODQTY = IT_8081-MODQTY.
      WA_8081-SEQ = IT_8081-SEQ.
      WA_8081-BIN = IT_8081-BIN.
      WA_8081-TIN = IT_8081-TIN.
      WA_8081-SOFF = IT_8081-SOFF.
      WA_8081-CGATE = IT_8081-CGATE.
*      wa_8081-shipin = it_8081-shipin.
      WA_8081-SHIPOUT = IT_8081-SHIPOUT.
      WA_8081-NEWQTYC = IT_8081-NEWQTYC.
      WA_8081-NEWQTY01 = IT_8081-NEWQTY01.
      WA_8081-NEWQTY02 = IT_8081-NEWQTY02.
      WA_8081-NEWQTY03 = IT_8081-NEWQTY03.
      WA_8081-NEWQTY04 = IT_8081-NEWQTY04.
      WA_8081-NEWQTY01 = IT_8081-NEWQTY01.
      WA_8081-NEWQTY05 = IT_8081-NEWQTY05.
      WA_8081-NEWQTY06 = IT_8081-NEWQTY06.
      WA_8081-NEWQTY07 = IT_8081-NEWQTY07.
      WA_8081-NEWQTY08 = IT_8081-NEWQTY08.
      WA_8081-NEWQTY09 = IT_8081-NEWQTY09.
      WA_8081-NEWQTY10 = IT_8081-NEWQTY10.
      WA_8081-NEWQTY11 = IT_8081-NEWQTY11.
      WA_8081-NATION = 'Total'.
    ENDAT.
  ENDLOOP.
  APPEND WA_8081 TO IT_8081.

  CLEAR: WA_8081.
ENDFORM.                    " data_select_8081
*&---------------------------------------------------------------------*
*&      Form  range_insert_8081
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RANGE_INSERT_8081.
  CLEAR R_ATNAM[].

* Model
  R_ATNAM-SIGN  = 'I'.
  R_ATNAM-OPTION = 'EQ'.
  R_ATNAM-LOW    = 'P_MODEL' .
  APPEND R_ATNAM. CLEAR R_ATNAM.

* NATION
  R_ATNAM-SIGN  = 'I'.
  R_ATNAM-OPTION = 'EQ'.
  R_ATNAM-LOW    = 'P_NATION'.
  APPEND R_ATNAM. CLEAR R_ATNAM.

* WORK ORDER SERIAL NUMBER
  R_ATNAM-SIGN  = 'I'.
  R_ATNAM-OPTION = 'EQ'.
  R_ATNAM-LOW    = 'P_WO_SER'.
  APPEND R_ATNAM. CLEAR R_ATNAM.

** MOD_DATE
*  r_atnam-sign  = 'I'.
*  r_atnam-option = 'EQ'.
*  r_atnam-low    = 'P_MOD_DATE'.
*  APPEND r_atnam. CLEAR r_atnam.
*
* P_219_19 (Check Usage).
  R_ATNAM-SIGN   = 'I'.
  R_ATNAM-OPTION = 'EQ'.
  R_ATNAM-LOW    = 'P_219_19'.
  APPEND R_ATNAM. CLEAR R_ATNAM.

ENDFORM.                    " range_insert_8081
*&---------------------------------------------------------------------*
*&      Form  range_insert_8081_219
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RANGE_INSERT_8081_219.
  DATA: L_INT          TYPE I.

  IF ST_8081_INPUT-219_1 <> ''.
* 219_1
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8081_INPUT-219_1 = L_INT = ST_8081_INPUT-219_1.
    CONDENSE ST_8081_INPUT-219_1.
    CONCATENATE 'P_219_'  ST_8081_INPUT-219_1 INTO   R_ATNAM-LOW.
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8081_INPUT-219_2 <> ''.
* 219_2
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8081_INPUT-219_2 = L_INT = ST_8081_INPUT-219_2.
    CONDENSE ST_8081_INPUT-219_2.
    CONCATENATE 'P_219_'  ST_8081_INPUT-219_2 INTO   R_ATNAM-LOW  .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8081_INPUT-219_3 <> ''.
* 219_3
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8081_INPUT-219_3 = L_INT = ST_8081_INPUT-219_3.
    CONDENSE ST_8081_INPUT-219_3.
    CONCATENATE 'P_219_'  ST_8081_INPUT-219_3 INTO   R_ATNAM-LOW  .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8081_INPUT-219_4 <> ''.
* 219_4
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8081_INPUT-219_4 = L_INT = ST_8081_INPUT-219_4.
    CONDENSE ST_8081_INPUT-219_4.
    CONCATENATE 'P_219_'  ST_8081_INPUT-219_4 INTO   R_ATNAM-LOW .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8081_INPUT-219_5 <> ''.
* 219_5
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8081_INPUT-219_5 = L_INT = ST_8081_INPUT-219_5.
    CONDENSE ST_8081_INPUT-219_5.
    CONCATENATE 'P_219_'  ST_8081_INPUT-219_5 INTO   R_ATNAM-LOW  .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8081_INPUT-219_6 <> ''.
* 219_6
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8081_INPUT-219_6 = L_INT = ST_8081_INPUT-219_6.
    CONDENSE ST_8081_INPUT-219_6.
    CONCATENATE 'P_219_'  ST_8081_INPUT-219_6 INTO   R_ATNAM-LOW  .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8081_INPUT-219_7 <> ''.
* 219_7
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8081_INPUT-219_7 = L_INT = ST_8081_INPUT-219_7.
    CONDENSE ST_8081_INPUT-219_7.
    CONCATENATE 'P_219_'  ST_8081_INPUT-219_7 INTO   R_ATNAM-LOW  .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8081_INPUT-219_8 <> ''.
* 219_8
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8081_INPUT-219_8 = L_INT = ST_8081_INPUT-219_8.
    CONDENSE ST_8081_INPUT-219_8.
    CONCATENATE 'P_219_'  ST_8081_INPUT-219_8 INTO   R_ATNAM-LOW  .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8081_INPUT-219_9 <> ''.
* 219_9
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8081_INPUT-219_9 = L_INT = ST_8081_INPUT-219_9.
    CONDENSE ST_8081_INPUT-219_9.
    CONCATENATE 'P_219_'  ST_8081_INPUT-219_9 INTO   R_ATNAM-LOW  .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8081_INPUT-219_10 <> ''.
* 219_10
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8081_INPUT-219_10 = L_INT = ST_8081_INPUT-219_10.
    CONDENSE ST_8081_INPUT-219_10.
    CONCATENATE 'P_219_'  ST_8081_INPUT-219_10 INTO   R_ATNAM-LOW .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

ENDFORM.                    " range_insert_8081_219
*&---------------------------------------------------------------------*
*&      Form  data_select_8082
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_SELECT_8082.
  DATA : BEGIN OF IT_WO OCCURS 0,
         WO_SER  LIKE ZTPP_WOSUM-WO_SER,    " WORK ORDER
         NATION  LIKE ZTPP_WOSUM-NATION,    " Nation
         DEALER  LIKE ZTPP_WOSUM-DEALER,    " Dealer
         FSC     LIKE ZTPP_WOSUM-FSC,
         EXTC    LIKE ZTPP_WOSUM-EXTC,
         INTC    LIKE ZTPP_WOSUM-INTC,
         MODQTY  LIKE ZTPP_WOSUM-MODQTY,    " Modify quntity
         SEQQTY  LIKE ZTPP_WOSUM-SEQQTY,
         PLANQTY LIKE ZTPP_WOSUM-PLANQTY,
         FORECASTQTY  LIKE ZTPP_WOSUM-FORECASTQTY,
         RP01TQ  LIKE ZTPP_WOSUM-RP01TQ,   " B/IN
         RP07TQ  LIKE ZTPP_WOSUM-RP07TQ,    " T/IN
         RP08TQ  LIKE ZTPP_WOSUM-RP08TQ,    " S/OFF
         RP09TQ  LIKE ZTPP_WOSUM-RP09TQ,    " C/Gate
*          rp14tq  LIKE ztpp_wosum-rp14tq,   " ship/in
         RP15TQ  LIKE ZTPP_WOSUM-RP15TQ,   " ship/out
         OBJEK   LIKE AUSP-OBJEK,
        END OF IT_WO.

  DATA : BEGIN OF IT_WO_NO OCCURS 0,
            WO_SER  LIKE ZTPP_WOSUM-WO_SER,    " WORK ORDER
            NATION  LIKE ZTPP_WOSUM-NATION,    " Nation
            DEALER  LIKE ZTPP_WOSUM-DEALER,    " Dealer
            FSC     LIKE ZTPP_WOSUM-FSC,
*            EXTC LIKE ZTPP_WOSUM-EXTC,
*            INTC LIKE ZTPP_WOSUM-INTC,
            MODQTY  LIKE ZTPP_WOSUM-MODQTY,    " Modify quntity
            SEQQTY  LIKE ZTPP_WOSUM-SEQQTY,
            PLANQTY LIKE ZTPP_WOSUM-PLANQTY,
            FORECASTQTY  LIKE ZTPP_WOSUM-FORECASTQTY,
            RP01TQ  LIKE ZTPP_WOSUM-RP01TQ,   " B/IN
            RP07TQ  LIKE ZTPP_WOSUM-RP07TQ,    " T/IN
            RP08TQ  LIKE ZTPP_WOSUM-RP08TQ,    " S/OFF
            RP09TQ  LIKE ZTPP_WOSUM-RP09TQ,    " C/Gate
** Changed by Furong on 04/11/07  >> Help desk no: 73VA39452A
**                                >> Transport UD1K940295
*            rp14tq  LIKE ztpp_wosum-rp14tq,   " ship/in
            RP15TQ  LIKE ZTPP_WOSUM-RP15TQ,   " ship/out
** end of change
            OBJEK   LIKE AUSP-OBJEK,
           END OF IT_WO_NO.

  DATA: WA_8082 LIKE LINE OF IT_8082.

  DATA : B_WO        LIKE ZTPP_WOSUM-WO_SER   ,
         E_WO        LIKE ZTPP_WOSUM-WO_SER   ,
         OBJEK       LIKE AUSP-OBJEK  ,
         L_PACK(4)   TYPE C,
         L_NAT(3)    TYPE C,
         L_DAT       TYPE D,
         L_VALS(8)   TYPE N,
         L_DEL(2)    TYPE C,
         CHECK       TYPE C,
         L_RECNO(2)  TYPE N,
         L_NEWQTY    LIKE ZTSD_SODATA-NEWQTY,
         L_YEAR(4)   TYPE N,
         L_MONTH(2)  TYPE N,
         L_BYEAR(4)   TYPE N,
         L_EMONTH(2)  TYPE N,
         L_BMONTH(2)  TYPE N,
         L_FR_YYMM(6) TYPE N,
         L_TO_YYMM(6) TYPE N,
         L_WO_YYMM(6) TYPE N,
         L_YYMM(6) TYPE N,
         L_TEXT(25).



  DATA: BEGIN OF IT_SODATA OCCURS 0,
         WO_SER LIKE ZTSD_SODATA-WO_SER,
         NATION LIKE ZTSD_SODATA-NATION,
         DEALER LIKE ZTSD_SODATA-DEALER,
         EXTC LIKE ZTSD_SODATA-EXTC,
         INTC LIKE ZTSD_SODATA-INTC,
         POYEAR LIKE ZTSD_SODATA-POYEAR,
         POMONTH LIKE ZTSD_SODATA-POMONTH,
         NEWQTY LIKE ZTSD_SODATA-NEWQTY,
         END OF IT_SODATA.

  DATA: LT_SODATA_TEMP LIKE TABLE OF IT_SODATA WITH HEADER LINE,
        LT_WO_TEMP LIKE TABLE OF IT_WO WITH HEADER LINE.

  FIELD-SYMBOLS: <FIELD>, <FIELD_TL>.

  RANGES : R_NAT FOR  ZTPP_WOSUM-NATION,    " Nation
           R_DEL FOR  ZTPP_WOSUM-DEALER.

*  RANGES: ddate FOR sy-datum.

*  IF st_8082_input-use = 'D'.
*    ddate-sign = 'I'.
*    ddate-option = 'BT'.
*    CONCATENATE st_8082_input-fyear '01'
*                 INTO ddate-low.
*    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
*      EXPORTING
*        day_in                  = ddate-low
*     IMPORTING
*       last_day_of_month       = ddate-high
**          EXCEPTIONS
**            DAY_IN_NO_DATE          = 1
**            OTHERS                  = 2
*    .
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*    APPEND ddate.
*  ENDIF.
  CLEAR: TL_8082_OUTPUT,TL_8082_OUTPUT[].

*  CONCATENATE '20' st_8081_input-monty+(2) INTO l_byear.
*  l_bmonth = st_8081_input-monty+2(2).


  L_BYEAR = ST_8082_INPUT-FYEAR.
  L_YEAR = L_BYEAR.
  L_BMONTH = ST_8082_INPUT-FMONTH.
  L_MONTH = ST_8082_INPUT-FMONTH.
  L_EMONTH = ST_8082_INPUT-TMONTH.

  CONCATENATE L_BMONTH '/' L_BYEAR INTO
              TL_8082_OUTPUT-T_C.

  CONCATENATE L_YEAR L_BMONTH INTO
              L_FR_YYMM.

  CONCATENATE L_YEAR L_EMONTH INTO
              L_TO_YYMM.

  L_RECNO = '00'.
  L_YYMM = L_FR_YYMM.
  WHILE L_YYMM < L_TO_YYMM.
    L_RECNO = L_RECNO + 1.
    CONCATENATE 'TL_8082_OUTPUT-T_' L_RECNO INTO L_TEXT.
    ASSIGN (L_TEXT) TO <FIELD_TL>.
    IF L_MONTH = '12'.
      L_MONTH = '01'.
      L_YEAR = L_YEAR + 1.
    ELSE.
      L_MONTH = L_MONTH + 1.
    ENDIF.
    CONCATENATE  L_MONTH '/'  L_YEAR INTO <FIELD_TL>.
    CONCATENATE L_YEAR L_MONTH INTO L_YYMM.
  ENDWHILE.

*  IF st_8082_input-nation <> ''.
*    CLEAR : r_nat , r_nat[].
*    r_nat-option = 'EQ'.
*    r_nat-sign  = 'I'.
*    r_nat-low    = st_8081_input-nation(3).
*    APPEND r_nat. CLEAR r_nat.
*  ENDIF.

  CLEAR : IT_8082, IT_8082[], IT_AUSP, IT_AUSP[], ST_8082_INPUT-QTY.
  CLEAR: IT_SODATA.

  IF ST_8082_INPUT-USE = 'D'.

    PERFORM RANGE_INSERT_8081.  " range value
    PERFORM RANGE_INSERT_8082_219. " 219 range value.

** Changed by Furong on 03/18/08
*    SELECT WO_SER NATION DEALER EXTC INTC POYEAR POMONTH
*                        SUM( NEWQTY )
*      INTO TABLE IT_SODATA
*      FROM ZTSD_SODATA
*      WHERE POYEAR = ST_8082_INPUT-FYEAR
*        AND ( POMONTH BETWEEN ST_8082_INPUT-FMONTH
*            AND ST_8082_INPUT-TMONTH )
*      GROUP BY WO_SER NATION DEALER EXTC INTC POYEAR POMONTH.

    IF ST_8082_INPUT-COLOR = 'Y'.
      IF WA_EXTC IS INITIAL AND WA_INTC IS INITIAL.
        SELECT WO_SER NATION DEALER EXTC INTC POYEAR POMONTH
                           SUM( NEWQTY )
         INTO TABLE IT_SODATA
         FROM ZTSD_SODATA
         WHERE POYEAR = ST_8082_INPUT-FYEAR
           AND ( POMONTH BETWEEN ST_8082_INPUT-FMONTH
               AND ST_8082_INPUT-TMONTH )
           AND ( DEALER = 'AA' OR DEALER = 'AB' )
         GROUP BY WO_SER NATION DEALER EXTC INTC POYEAR POMONTH.
      ELSE.
        IF WA_EXTC <> ' ' AND WA_INTC <> ' '.
          SELECT WO_SER NATION DEALER EXTC INTC POYEAR POMONTH
                             SUM( NEWQTY )
           INTO TABLE IT_SODATA
           FROM ZTSD_SODATA
           WHERE POYEAR = ST_8082_INPUT-FYEAR
             AND EXTC = WA_EXTC
             AND INTC = WA_INTC
             AND ( POMONTH BETWEEN ST_8082_INPUT-FMONTH
                 AND ST_8082_INPUT-TMONTH )
             AND ( DEALER = 'AA' OR DEALER = 'AB' )
           GROUP BY WO_SER NATION DEALER EXTC INTC POYEAR POMONTH.
        ELSE.
          IF WA_EXTC <> ' '.
            SELECT WO_SER NATION DEALER EXTC INTC POYEAR POMONTH
                                 SUM( NEWQTY )
               INTO TABLE IT_SODATA
               FROM ZTSD_SODATA
               WHERE POYEAR = ST_8082_INPUT-FYEAR
                 AND EXTC = WA_EXTC
                 AND ( POMONTH BETWEEN ST_8082_INPUT-FMONTH
                     AND ST_8082_INPUT-TMONTH )
                 AND ( DEALER = 'AA' OR DEALER = 'AB' )
               GROUP BY WO_SER NATION DEALER EXTC INTC POYEAR POMONTH.
          ELSE.
            SELECT WO_SER NATION DEALER EXTC INTC POYEAR POMONTH
                                  SUM( NEWQTY )
                INTO TABLE IT_SODATA
                FROM ZTSD_SODATA
                WHERE POYEAR = ST_8082_INPUT-FYEAR
                  AND INTC = WA_INTC
                  AND ( POMONTH BETWEEN ST_8082_INPUT-FMONTH
                      AND ST_8082_INPUT-TMONTH )
            AND ( DEALER = 'AA' OR DEALER = 'AB' )
                GROUP BY WO_SER NATION DEALER EXTC INTC POYEAR POMONTH.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
*      SELECT WO_SER NATION DEALER POYEAR POMONTH
*                        SUM( NEWQTY ) AS NEWQTY
*      INTO CORRESPONDING FIELDS OF TABLE IT_SODATA
*      FROM ZTSD_SODATA
*      WHERE POYEAR = ST_8082_INPUT-FYEAR
*        AND ( POMONTH BETWEEN ST_8082_INPUT-FMONTH
*            AND ST_8082_INPUT-TMONTH )
*       AND ( DEALER = 'AA' OR DEALER = 'AB' )
*      GROUP BY WO_SER NATION DEALER POYEAR POMONTH.
      SELECT WO_SER NATION DEALER EXTC INTC POYEAR POMONTH
                           SUM( NEWQTY )
         INTO TABLE LT_SODATA_TEMP
         FROM ZTSD_SODATA
         WHERE POYEAR = ST_8082_INPUT-FYEAR
           AND ( POMONTH BETWEEN ST_8082_INPUT-FMONTH
               AND ST_8082_INPUT-TMONTH )
           AND ( DEALER = 'AA' OR DEALER = 'AB' )
         GROUP BY WO_SER NATION DEALER EXTC INTC POYEAR POMONTH.
      CLEAR:  IT_SODATA[], IT_SODATA .
      LOOP AT LT_SODATA_TEMP.
        MOVE-CORRESPONDING LT_SODATA_TEMP TO IT_SODATA.
        CLEAR: IT_SODATA-EXTC, IT_SODATA-INTC.
        COLLECT IT_SODATA.
        CLEAR: IT_SODATA.
      ENDLOOP.

    ENDIF.
** End of change on 03/18/08

*   SELECT WO_SER NATION DEALER EXTC INTC FSC SUM( MODQTY )
*         SUM( SEQQTY ) SUM( RP01TQ ) SUM( RP07TQ )
*         SUM( RP08TQ ) SUM( RP09TQ ) SUM( RP15TQ )
*        INTO TABLE IT_WO_NO
*        FROM  ZTPP_WOSUM
*        GROUP BY WO_SER NATION DEALER EXTC INTC FSC.

    IF IT_SODATA[] IS INITIAL.
      MESSAGE S001 WITH  TEXT-100.
      EXIT.
    ENDIF.


*    SELECT WO_SER NATION DEALER FSC EXTC INTC MODQTY
*            SEQQTY RP01TQ RP07TQ RP08TQ RP09TQ RP15TQ
*           INTO TABLE IT_WO_NO
*           FROM  ZTPP_WOSUM
*           FOR ALL ENTRIES IN IT_SODATA
*           WHERE WO_SER = IT_SODATA-WO_SER
*             AND NATION = IT_SODATA-NATION
*             AND DEALER = IT_SODATA-DEALER
*             AND EXTC = IT_SODATA-EXTC
*             AND INTC = IT_SODATA-INTC.
*
    IF ST_8082_INPUT-COLOR = 'Y'.
      SELECT WO_SER NATION DEALER FSC EXTC INTC MODQTY
             SEQQTY PLANQTY FORECASTQTY RP01TQ RP07TQ
             RP08TQ RP09TQ RP15TQ
            INTO TABLE IT_WO
            FROM  ZTPP_WOSUM
            FOR ALL ENTRIES IN IT_SODATA
            WHERE WO_SER = IT_SODATA-WO_SER
              AND NATION = IT_SODATA-NATION
              AND DEALER = IT_SODATA-DEALER
              AND EXTC = IT_SODATA-EXTC
              AND INTC = IT_SODATA-INTC.
    ELSE.
*      SELECT WO_SER NATION DEALER FSC MODQTY
*             SEQQTY PLANQTY FORECASTQTY
*             RP01TQ RP07TQ RP08TQ RP09TQ RP15TQ
*            INTO TABLE IT_WO_NO
*            FROM  ZTPP_WOSUM
*            FOR ALL ENTRIES IN IT_SODATA
*            WHERE WO_SER = IT_SODATA-WO_SER
*              AND NATION = IT_SODATA-NATION
*              AND DEALER = IT_SODATA-DEALER.
*      LOOP AT IT_WO_NO.
*        MOVE-CORRESPONDING IT_WO_NO TO IT_WO.
*        COLLECT IT_WO.
*        CLEAR: IT_WO.
*      ENDLOOP.
      SELECT WO_SER NATION DEALER FSC EXTC INTC MODQTY
                 SEQQTY PLANQTY FORECASTQTY RP01TQ RP07TQ
                 RP08TQ RP09TQ RP15TQ
                INTO TABLE LT_WO_TEMP
                FROM  ZTPP_WOSUM
                FOR ALL ENTRIES IN LT_SODATA_TEMP
                WHERE WO_SER = LT_SODATA_TEMP-WO_SER
                  AND NATION = LT_SODATA_TEMP-NATION
                  AND DEALER = LT_SODATA_TEMP-DEALER
                  AND EXTC = LT_SODATA_TEMP-EXTC
                  AND INTC = LT_SODATA_TEMP-INTC.

      LOOP AT LT_WO_TEMP.
        MOVE-CORRESPONDING LT_WO_TEMP TO IT_WO.
        CLEAR: IT_WO-EXTC, IT_WO-INTC.
        COLLECT IT_WO.
        CLEAR: IT_WO.
      ENDLOOP.

    ENDIF.
*  SORT IT_WO_NO BY WO_SER NATION DEALER FSC EXTC INTC.


    CLEAR: IT_WO_NO, IT_WO_NO[].

* objeck number concatenate.

    LOOP AT IT_WO.
    CONCATENATE IT_WO-WO_SER IT_WO-NATION IT_WO-DEALER INTO IT_WO-OBJEK.
      MODIFY IT_WO.
    ENDLOOP.
    CLEAR: IT_WO_NO, IT_WO_NO[].

* objeck number concatenate.

    LOOP AT IT_WO.

      CLEAR : L_NAT, L_DEL, CHECK.

      SELECT  OBJEK ATINN ATZHL MAFID KLART ADZHL ATWRT ATFLV ATNAM
          INTO CORRESPONDING FIELDS OF TABLE IT_AUSP
       FROM ZVPP_CHA
       WHERE OBJEK = IT_WO-OBJEK
         AND KLART = '001'
         AND ATNAM IN R_ATNAM
         AND LKENZ = ' ' .

      LOOP AT IT_AUSP.
        CASE  IT_AUSP-ATNAM.
          WHEN 'P_MODEL' .
            IF IT_AUSP-ATWRT  NE WA_MODEL        .
              CHECK = 'X' .
            ENDIF.
          WHEN 'P_219_19'.
            " Check the Input Option...
            MOVE : IT_AUSP-ATWRT TO IT_8082-219_19 .
            CHECK ST_8082_INPUT-USE NE SPACE     .
** Changed by Furong on 04/18/08
*            IF ST_8082_INPUT-USE NE IT_8082-219_19 .
*              CHECK = 'X' .
*            ENDIF.
            IF ST_8082_INPUT-USE = 'E'.

            ELSE.
              IF ST_8082_INPUT-USE NE IT_8082-219_19 .
                CHECK = 'X' .
              ENDIF.
            ENDIF.
          WHEN 'P_NATION'.
            IF ST_8082_INPUT-USE = 'E' AND IT_AUSP-ATWRT+0(3) = 'B28'.
              CHECK = 'X' .
            ENDIF.
** END OF CHANGE

*        WHEN 'P_WO_SER'.
*          IF it_ausp-atwrt+1(4) NE st_8082_input-monty.
*            check = 'X'.
*          ENDIF.
        ENDCASE.

        CASE IT_AUSP-ATNAM+6(3).
          WHEN ''.

          WHEN ST_8082_INPUT-219_1.
            IF ST_8082_INPUT-219_1_V <> '' AND
               ST_8082_INPUT-219_1_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_2.
            IF ST_8082_INPUT-219_2_V <> '' AND
               ST_8082_INPUT-219_2_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_3.
            IF ST_8082_INPUT-219_3_V <> '' AND
               ST_8082_INPUT-219_3_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_4.
            IF ST_8082_INPUT-219_4_V <> '' AND
               ST_8082_INPUT-219_4_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_5.
            IF ST_8082_INPUT-219_5_V <> '' AND
               ST_8082_INPUT-219_5_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_6.
            IF ST_8082_INPUT-219_6_V <> '' AND
               ST_8082_INPUT-219_6_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_7.
            IF ST_8082_INPUT-219_7_V <> '' AND
               ST_8082_INPUT-219_7_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_8.
            IF ST_8082_INPUT-219_8_V <> '' AND
               ST_8082_INPUT-219_8_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_9.
            IF ST_8082_INPUT-219_9_V <> '' AND
               ST_8082_INPUT-219_9_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_10.
            IF ST_8082_INPUT-219_10_V <> '' AND
               ST_8082_INPUT-219_10_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.
        ENDCASE.
      ENDLOOP.

      MOVE : IT_WO-WO_SER TO IT_8082-ORDER,
             IT_WO-FSC TO IT_8082-FSC,
             IT_WO-MODQTY TO IT_8082-MODQTY,
             IT_WO-PLANQTY TO IT_8082-PLANQTY,
             IT_WO-FORECASTQTY TO IT_8082-FORECASTQTY,
             IT_WO-EXTC TO IT_8082-EXTC,
             IT_WO-INTC TO IT_8082-INTC,
             IT_WO-SEQQTY TO IT_8082-SEQ,
             IT_WO-RP01TQ TO IT_8082-BIN,
             IT_WO-RP07TQ TO IT_8082-TIN,
             IT_WO-RP08TQ TO IT_8082-SOFF,
             IT_WO-RP09TQ TO IT_8082-CGATE,
             IT_WO-RP15TQ TO IT_8082-SHIPOUT.
      CONCATENATE IT_WO-NATION IT_WO-DEALER INTO IT_8082-NATION.

      IF CHECK = ''.
*      st_8082_input-qty = it_8082-modqty + st_8082_input-qty.

        L_YEAR = ST_8082_INPUT-FYEAR.
        L_MONTH = ST_8082_INPUT-FMONTH.
        READ TABLE IT_SODATA WITH KEY WO_SER = IT_WO-WO_SER
                                      NATION = IT_WO-NATION
                                      DEALER = IT_WO-DEALER
                                      EXTC = IT_WO-EXTC
                                      INTC = IT_WO-INTC
                                      POYEAR = L_YEAR
                                      POMONTH = L_MONTH.
        IF SY-SUBRC = 0.
          IT_8082-NEWQTYC = IT_SODATA-NEWQTY.
        ENDIF.
        L_RECNO = '00'.
*          CONCATENATE ST_8082_INPUT-FYEAR ST_8082_INPUT-FMONTH
*                 INTO L_FR_YYMM.
*
        L_YYMM = L_FR_YYMM.
        WHILE L_YYMM < L_TO_YYMM.
          L_RECNO = L_RECNO + 1.
          CONCATENATE 'IT_8082-NEWQTY' L_RECNO INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FIELD>.
          IF L_MONTH = '12'.
            L_MONTH = '01'.
            L_YEAR = L_YEAR + 1.
          ELSE.
            L_MONTH = L_MONTH + 1.
          ENDIF.
          CONCATENATE L_YEAR L_MONTH
                  INTO L_YYMM.

          READ TABLE IT_SODATA WITH KEY WO_SER = IT_WO-WO_SER
                                         NATION = IT_WO-NATION
                                         DEALER = IT_WO-DEALER
                                         EXTC = IT_WO-EXTC
                                         INTC = IT_WO-INTC
                                         POYEAR = L_YEAR
                                         POMONTH = L_MONTH.
          IF SY-SUBRC = 0.
            L_NEWQTY = IT_SODATA-NEWQTY.
            MOVE: L_NEWQTY TO <FIELD>.
            CLEAR: L_NEWQTY.
          ENDIF.
        ENDWHILE.
        APPEND IT_8082. CLEAR IT_8082.
      ENDIF.
    ENDLOOP.

  ELSE.
    DATA: L_WO_F(9),
          L_WO_T(9).
    RANGES: R_WO FOR ZTPP_WOSUM-WO_SER.

    CONCATENATE 'E' L_YEAR+2(2) L_BMONTH
                '0000' INTO L_WO_F.
    CONCATENATE 'E' L_YEAR+2(2) L_MONTH
                'ZZZZ' INTO L_WO_T.

    R_WO-SIGN = 'I'.
    R_WO-OPTION = 'BT'.
    R_WO-LOW = L_WO_F.
    R_WO-HIGH = L_WO_T.
    APPEND R_WO.

    IF ST_8082_INPUT-COLOR = 'Y'.
      IF WA_EXTC IS INITIAL AND WA_INTC IS INITIAL.
        SELECT WO_SER NATION DEALER FSC EXTC INTC SUM( MODQTY )
            SUM( SEQQTY ) SUM( PLANQTY ) SUM( FORECASTQTY )
            SUM( RP01TQ ) SUM( RP07TQ )
            SUM( RP08TQ ) SUM( RP09TQ ) SUM( RP15TQ )
           INTO TABLE IT_WO
           FROM  ZTPP_WOSUM
             WHERE  WO_SER IN R_WO
               AND ( DEALER = 'AA' OR DEALER = 'AB' )
           GROUP BY WO_SER NATION DEALER FSC EXTC INTC.
      ELSE.
        IF WA_EXTC <> ' ' AND WA_INTC <> ' '.
          SELECT WO_SER NATION DEALER FSC EXTC INTC SUM( MODQTY )
                   SUM( SEQQTY ) SUM( PLANQTY ) SUM( FORECASTQTY )
                   SUM( RP01TQ ) SUM( RP07TQ )
                   SUM( RP08TQ ) SUM( RP09TQ ) SUM( RP15TQ )
                  INTO TABLE IT_WO
                  FROM  ZTPP_WOSUM
                    WHERE  WO_SER IN R_WO
                      AND ( DEALER = 'AA' OR DEALER = 'AB' )
                      AND  EXTC = WA_EXTC
                      AND  INTC = WA_INTC
                  GROUP BY WO_SER NATION DEALER FSC EXTC INTC.
        ELSE.
          IF WA_EXTC <> ' '.
            SELECT WO_SER NATION DEALER FSC EXTC INTC SUM( MODQTY )
       SUM( SEQQTY ) SUM( PLANQTY ) SUM( FORECASTQTY )
       SUM( RP01TQ ) SUM( RP07TQ )
       SUM( RP08TQ ) SUM( RP09TQ ) SUM( RP15TQ )
      INTO TABLE IT_WO
      FROM  ZTPP_WOSUM
        WHERE  WO_SER IN R_WO
          AND ( DEALER = 'AA' OR DEALER = 'AB' )
          AND  EXTC = WA_EXTC
      GROUP BY WO_SER NATION DEALER FSC EXTC INTC.

          ELSE.
            SELECT WO_SER NATION DEALER FSC EXTC INTC SUM( MODQTY )
                     SUM( SEQQTY ) SUM( PLANQTY ) SUM( FORECASTQTY )
                     SUM( RP01TQ ) SUM( RP07TQ )
                     SUM( RP08TQ ) SUM( RP09TQ ) SUM( RP15TQ )
                    INTO TABLE IT_WO
                    FROM  ZTPP_WOSUM
                      WHERE  WO_SER IN R_WO
                         AND ( DEALER = 'AA' OR DEALER = 'AB' )
                         AND  INTC = WA_INTC
                    GROUP BY WO_SER NATION DEALER FSC EXTC INTC.
          ENDIF.
        ENDIF.

      ENDIF.
    ELSE.
      SELECT WO_SER NATION DEALER FSC SUM( MODQTY )
        SUM( SEQQTY )  SUM( PLANQTY ) SUM( FORECASTQTY )
        SUM( RP01TQ ) SUM( RP07TQ )
        SUM( RP08TQ ) SUM( RP09TQ ) SUM( RP15TQ )
       INTO  TABLE IT_WO_NO
       FROM  ZTPP_WOSUM
         WHERE  WO_SER IN R_WO
           AND ( DEALER = 'AA' OR DEALER = 'AB' )
       GROUP BY WO_SER NATION DEALER FSC.
      LOOP AT IT_WO_NO.
        MOVE-CORRESPONDING IT_WO_NO TO IT_WO.
        APPEND IT_WO.
        CLEAR: IT_WO.
      ENDLOOP.

    ENDIF.
    CLEAR: IT_WO_NO, IT_WO_NO[].

* objeck number concatenate.
    IF IT_WO[] IS INITIAL.
      MESSAGE S001  WITH  TEXT-100.
    ENDIF.

    LOOP AT IT_WO.
    CONCATENATE IT_WO-WO_SER IT_WO-NATION IT_WO-DEALER INTO IT_WO-OBJEK.
      MODIFY IT_WO.
    ENDLOOP.

    PERFORM RANGE_INSERT_8081.  " range value
    PERFORM RANGE_INSERT_8082_219. " 219 range value.


    LOOP AT IT_WO.

      CLEAR : L_NAT, L_DEL, CHECK.

      SELECT  OBJEK ATINN ATZHL MAFID KLART ADZHL ATWRT ATFLV ATNAM
          INTO CORRESPONDING FIELDS OF TABLE IT_AUSP
       FROM ZVPP_CHA
       WHERE OBJEK = IT_WO-OBJEK
         AND KLART = '001'
         AND ATNAM IN R_ATNAM
         AND LKENZ = ' ' .

      LOOP AT IT_AUSP.
        CASE  IT_AUSP-ATNAM.
          WHEN 'P_MODEL' .
            IF IT_AUSP-ATWRT  NE WA_MODEL        .
              CHECK = 'X' .
            ENDIF.
          WHEN 'P_219_19'.
            " Check the Input Option...
            MOVE : IT_AUSP-ATWRT TO IT_8082-219_19 .
            CHECK ST_8082_INPUT-USE NE SPACE     .
** Changed by Furong on 04/18/08
*            IF ST_8082_INPUT-USE NE IT_8082-219_19 .
*              CHECK = 'X' .
*            ENDIF.
            IF ST_8082_INPUT-USE = 'E'.

            ELSE.
              IF ST_8082_INPUT-USE NE IT_8082-219_19 .
                CHECK = 'X' .
              ENDIF.
            ENDIF.
          WHEN 'P_NATION'.
            IF ST_8082_INPUT-USE = 'E' AND IT_AUSP-ATWRT+0(3) = 'B28'.
              CHECK = 'X' .
            ENDIF.
** End of change

*
*        WHEN 'P_WO_SER'.
*          IF it_ausp-atwrt+1(4) NE st_8082_input-monty.
*            check = 'X'.
*          ENDIF.
        ENDCASE.

        CASE IT_AUSP-ATNAM+6(3).
          WHEN ''.

          WHEN ST_8082_INPUT-219_1.
            IF ST_8082_INPUT-219_1_V <> '' AND
               ST_8082_INPUT-219_1_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_2.
            IF ST_8082_INPUT-219_2_V <> '' AND
               ST_8082_INPUT-219_2_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_3.
            IF ST_8082_INPUT-219_3_V <> '' AND
               ST_8082_INPUT-219_3_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_4.
            IF ST_8082_INPUT-219_4_V <> '' AND
               ST_8082_INPUT-219_4_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_5.
            IF ST_8082_INPUT-219_5_V <> '' AND
               ST_8082_INPUT-219_5_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_6.
            IF ST_8082_INPUT-219_6_V <> '' AND
               ST_8082_INPUT-219_6_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_7.
            IF ST_8082_INPUT-219_7_V <> '' AND
               ST_8082_INPUT-219_7_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_8.
            IF ST_8082_INPUT-219_8_V <> '' AND
               ST_8082_INPUT-219_8_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_9.
            IF ST_8082_INPUT-219_9_V <> '' AND
               ST_8082_INPUT-219_9_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.

          WHEN ST_8082_INPUT-219_10.
            IF ST_8082_INPUT-219_10_V <> '' AND
               ST_8082_INPUT-219_10_V <> IT_AUSP-ATWRT.
              CHECK = 'X'.
            ENDIF.
        ENDCASE.
      ENDLOOP.

      MOVE : IT_WO-WO_SER TO IT_8082-ORDER,
             IT_WO-FSC TO IT_8082-FSC,
             IT_WO-MODQTY TO IT_8082-MODQTY,
             IT_WO-PLANQTY TO IT_8082-PLANQTY,
             IT_WO-FORECASTQTY TO IT_8082-FORECASTQTY,
             IT_WO-EXTC TO IT_8082-EXTC,
             IT_WO-INTC TO IT_8082-INTC,
             IT_WO-SEQQTY TO IT_8082-SEQ,
             IT_WO-RP01TQ TO IT_8082-BIN,
             IT_WO-RP07TQ TO IT_8082-TIN,
             IT_WO-RP08TQ TO IT_8082-SOFF,
             IT_WO-RP09TQ TO IT_8082-CGATE,
             IT_WO-RP15TQ TO IT_8082-SHIPOUT.
      CONCATENATE IT_WO-NATION IT_WO-DEALER INTO IT_8082-NATION.

      IF CHECK = ''.
*      st_8082_input-qty = it_8082-modqty + st_8082_input-qty.
        L_RECNO = IT_WO-WO_SER+3(2) - L_BMONTH.
        IF L_RECNO = '00'.
          L_TEXT = 'IT_8082-NEWQTYC'.
        ELSE.
          CONCATENATE 'IT_8082-NEWQTY' L_RECNO INTO L_TEXT.
        ENDIF.
        ASSIGN (L_TEXT) TO <FIELD>.
        MOVE IT_WO-MODQTY  TO <FIELD>.
        APPEND IT_8082. CLEAR IT_8082.
      ENDIF.
    ENDLOOP.

  ENDIF.

  LOOP AT IT_8082.
    AT LAST.
      SUM.
      WA_8082-MODQTY = IT_8082-MODQTY.
      WA_8082-PLANQTY = IT_8082-PLANQTY.
      WA_8082-FORECASTQTY = IT_8082-FORECASTQTY.
      WA_8082-SEQ = IT_8082-SEQ.
      WA_8082-BIN = IT_8082-BIN.
      WA_8082-TIN = IT_8082-TIN.
      WA_8082-SOFF = IT_8082-SOFF.
      WA_8082-CGATE = IT_8082-CGATE.
      WA_8082-SHIPOUT = IT_8082-SHIPOUT.
      WA_8082-NEWQTYC = IT_8082-NEWQTYC.
      WA_8082-NEWQTY01 = IT_8082-NEWQTY01.
      WA_8082-NEWQTY02 = IT_8082-NEWQTY02.
      WA_8082-NEWQTY03 = IT_8082-NEWQTY03.
      WA_8082-NEWQTY04 = IT_8082-NEWQTY04.
      WA_8082-NEWQTY01 = IT_8082-NEWQTY01.
      WA_8082-NEWQTY05 = IT_8082-NEWQTY05.
      WA_8082-NEWQTY06 = IT_8082-NEWQTY06.
      WA_8082-NEWQTY07 = IT_8082-NEWQTY07.
      WA_8082-NEWQTY08 = IT_8082-NEWQTY08.
      WA_8082-NEWQTY09 = IT_8082-NEWQTY09.
      WA_8082-NEWQTY10 = IT_8082-NEWQTY10.
      WA_8082-NEWQTY11 = IT_8082-NEWQTY11.
      WA_8082-NATION = 'Total'.
    ENDAT.
  ENDLOOP.
  APPEND WA_8082 TO IT_8082.
  CLEAR: WA_8082.
ENDFORM.                    " data_select_8082
*&---------------------------------------------------------------------*
*&      Form  range_insert_8082_219
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RANGE_INSERT_8082_219.
  DATA: L_INT          TYPE I.

  IF ST_8082_INPUT-219_1 <> ''.
* 219_1
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8082_INPUT-219_1 = L_INT = ST_8082_INPUT-219_1.
    CONDENSE ST_8082_INPUT-219_1.
    CONCATENATE 'P_219_'  ST_8082_INPUT-219_1 INTO   R_ATNAM-LOW.
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8082_INPUT-219_2 <> ''.
* 219_2
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8082_INPUT-219_2 = L_INT = ST_8082_INPUT-219_2.
    CONDENSE ST_8082_INPUT-219_2.
    CONCATENATE 'P_219_'  ST_8082_INPUT-219_2 INTO   R_ATNAM-LOW  .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8082_INPUT-219_3 <> ''.
* 219_3
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8082_INPUT-219_3 = L_INT = ST_8082_INPUT-219_3.
    CONDENSE ST_8082_INPUT-219_3.
    CONCATENATE 'P_219_'  ST_8082_INPUT-219_3 INTO   R_ATNAM-LOW  .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8082_INPUT-219_4 <> ''.
* 219_4
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8082_INPUT-219_4 = L_INT = ST_8082_INPUT-219_4.
    CONDENSE ST_8082_INPUT-219_4.
    CONCATENATE 'P_219_'  ST_8082_INPUT-219_4 INTO   R_ATNAM-LOW .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8082_INPUT-219_5 <> ''.
* 219_5
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8082_INPUT-219_5 = L_INT = ST_8082_INPUT-219_5.
    CONDENSE ST_8082_INPUT-219_5.
    CONCATENATE 'P_219_'  ST_8082_INPUT-219_5 INTO   R_ATNAM-LOW  .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8082_INPUT-219_6 <> ''.
* 219_6
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8082_INPUT-219_6 = L_INT = ST_8082_INPUT-219_6.
    CONDENSE ST_8082_INPUT-219_6.
    CONCATENATE 'P_219_'  ST_8082_INPUT-219_6 INTO   R_ATNAM-LOW  .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8082_INPUT-219_7 <> ''.
* 219_7
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8082_INPUT-219_7 = L_INT = ST_8082_INPUT-219_7.
    CONDENSE ST_8082_INPUT-219_7.
    CONCATENATE 'P_219_'  ST_8082_INPUT-219_7 INTO   R_ATNAM-LOW  .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8082_INPUT-219_8 <> ''.
* 219_8
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8082_INPUT-219_8 = L_INT = ST_8082_INPUT-219_8.
    CONDENSE ST_8082_INPUT-219_8.
    CONCATENATE 'P_219_'  ST_8082_INPUT-219_8 INTO   R_ATNAM-LOW  .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8082_INPUT-219_9 <> ''.
* 219_9
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8082_INPUT-219_9 = L_INT = ST_8082_INPUT-219_9.
    CONDENSE ST_8082_INPUT-219_9.
    CONCATENATE 'P_219_'  ST_8082_INPUT-219_9 INTO   R_ATNAM-LOW  .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.

  IF ST_8082_INPUT-219_10 <> ''.
* 219_10
    R_ATNAM-SIGN  = 'I'.
    R_ATNAM-OPTION = 'EQ'.
    ST_8082_INPUT-219_10 = L_INT = ST_8082_INPUT-219_10.
    CONDENSE ST_8082_INPUT-219_10.
    CONCATENATE 'P_219_'  ST_8082_INPUT-219_10 INTO   R_ATNAM-LOW .
    APPEND R_ATNAM. CLEAR R_ATNAM.
  ENDIF.
ENDFORM.                    " range_insert_8082_219
*&---------------------------------------------------------------------*
*&      Form  excel_down_8082
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCEL_DOWN_8082.
  CLEAR IT_EXCEL_8082.
  REFRESH IT_EXCEL_8082.
  MOVE 'Nation' TO IT_EXCEL_8082-COL01.
  MOVE 'Order No' TO IT_EXCEL_8082-COL02.
  MOVE 'FSC' TO IT_EXCEL_8082-COL24.
  MOVE 'Extc' TO IT_EXCEL_8082-COL03.
  MOVE 'Intc' TO IT_EXCEL_8082-COL04.
  MOVE 'Mod Qty' TO IT_EXCEL_8082-COL05.
  MOVE 'Seq Qty' TO IT_EXCEL_8082-COL06.
  MOVE 'Body In' TO IT_EXCEL_8082-COL07.
  MOVE 'Trim In' TO IT_EXCEL_8082-COL08.
  MOVE 'S Off' TO IT_EXCEL_8082-COL09.
  MOVE 'C/Gate' TO IT_EXCEL_8082-COL10.
  MOVE 'Ship In' TO IT_EXCEL_8082-COL11.
  MOVE TL_8082_OUTPUT-T_C TO IT_EXCEL_8082-COL12.
  MOVE TL_8082_OUTPUT-T_01 TO IT_EXCEL_8082-COL13.
  MOVE TL_8082_OUTPUT-T_02 TO IT_EXCEL_8082-COL14.
  MOVE TL_8082_OUTPUT-T_03 TO IT_EXCEL_8082-COL15.
  MOVE TL_8082_OUTPUT-T_04 TO IT_EXCEL_8082-COL16.
  MOVE TL_8082_OUTPUT-T_05 TO IT_EXCEL_8082-COL17.
  MOVE TL_8082_OUTPUT-T_06 TO IT_EXCEL_8082-COL18.
  MOVE TL_8082_OUTPUT-T_07 TO IT_EXCEL_8082-COL19.
  MOVE TL_8082_OUTPUT-T_08 TO IT_EXCEL_8082-COL20.
  MOVE TL_8082_OUTPUT-T_09 TO IT_EXCEL_8082-COL21.
  MOVE TL_8082_OUTPUT-T_10 TO IT_EXCEL_8082-COL22.
  MOVE TL_8082_OUTPUT-T_11 TO IT_EXCEL_8082-COL23.
  APPEND IT_EXCEL_8082.
*
  LOOP AT IT_8082.
    CLEAR IT_EXCEL_8082.
    MOVE IT_8082-NATION TO IT_EXCEL_8082-COL01.
    MOVE IT_8082-ORDER TO IT_EXCEL_8082-COL02.
    MOVE IT_8082-FSC TO IT_EXCEL_8082-COL24.
    MOVE IT_8082-EXTC TO IT_EXCEL_8082-COL03.
    MOVE IT_8082-INTC TO IT_EXCEL_8082-COL04.
    MOVE IT_8082-MODQTY TO IT_EXCEL_8082-COL05.
    MOVE IT_8082-SEQ TO IT_EXCEL_8082-COL06.
    MOVE IT_8082-BIN TO IT_EXCEL_8082-COL07.
    MOVE IT_8082-TIN TO IT_EXCEL_8082-COL08.
    MOVE IT_8082-SOFF TO IT_EXCEL_8082-COL09.
    MOVE IT_8082-CGATE TO IT_EXCEL_8082-COL10.
*    MOVE it_8081-shipin TO it_excel_8081-col11.
    MOVE IT_8082-SHIPOUT TO IT_EXCEL_8082-COL11.
    MOVE IT_8082-NEWQTYC TO IT_EXCEL_8082-COL12.
    MOVE IT_8082-NEWQTY01 TO IT_EXCEL_8082-COL13.
    MOVE IT_8082-NEWQTY02 TO IT_EXCEL_8082-COL14.
    MOVE IT_8082-NEWQTY03 TO IT_EXCEL_8082-COL15.
    MOVE IT_8082-NEWQTY04 TO IT_EXCEL_8082-COL16.
    MOVE IT_8082-NEWQTY05 TO IT_EXCEL_8082-COL17.
    MOVE IT_8082-NEWQTY06 TO IT_EXCEL_8082-COL18.
    MOVE IT_8082-NEWQTY07 TO IT_EXCEL_8082-COL19.
    MOVE IT_8082-NEWQTY08 TO IT_EXCEL_8082-COL20.
    MOVE IT_8082-NEWQTY09 TO IT_EXCEL_8082-COL21.
    MOVE IT_8082-NEWQTY10 TO IT_EXCEL_8082-COL22.
    MOVE IT_8082-NEWQTY11 TO IT_EXCEL_8082-COL23.
    APPEND IT_EXCEL_8082.
  ENDLOOP.
*
  CALL FUNCTION 'DOWNLOAD'
   EXPORTING
     FILENAME                      = 'HMA_ORDER.XLS'
     FILETYPE                      = 'DAT'
     ITEM                          = ' '
*     FILETYPE_NO_CHANGE            = ' '
*     FILETYPE_NO_SHOW              = ' '
*     SILENT                        = 'S'
*     COL_SELECT                    = ' '
*     COL_SELECTMASK                = ' '
*     NO_AUTH_CHECK                 = ' '
*   IMPORTING
*     ACT_FILENAME                  =
*     ACT_FILETYPE                  =
*     FILESIZE                      =
*     CANCEL                        =
    TABLES
      DATA_TAB                      = IT_EXCEL_8082
*   EXCEPTIONS
*     INVALID_FILESIZE              = 1
*     INVALID_TABLE_WIDTH           = 2
*     INVALID_TYPE                  = 3
*     NO_BATCH                      = 4
*     UNKNOWN_ERROR                 = 5
*     GUI_REFUSE_FILETRANSFER       = 6
*     CUSTOMER_ERROR                = 7
*     OTHERS                        = 8
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " excel_down_8082
*&---------------------------------------------------------------------*
*&      Form  PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OK_CODE  text
*----------------------------------------------------------------------*
FORM PAGE USING  CODE TB STRUCTURE TB_8082.
  DATA: I TYPE I,
        J TYPE I.

  CASE CODE.
    WHEN 'P--'. TB-TOP_LINE = 1.
    WHEN 'P-'.
      TB-TOP_LINE = TB-TOP_LINE - LINE_COUNT.
      IF TB-TOP_LINE LE 0.
        TB-TOP_LINE = 1.
      ENDIF.
    WHEN 'P+'.
      I = TB-TOP_LINE + LINE_COUNT.
      J = TB-LINES - LINE_COUNT + 1.
      IF J LE 0.
        J = 1.
      ENDIF.
      IF I LE J.
        TB-TOP_LINE = I.
      ELSE.
        TB-TOP_LINE = J.
      ENDIF.
    WHEN 'P++'.
      TB-TOP_LINE = TB-LINES - LINE_COUNT + 1.
      IF TB-TOP_LINE LE 0.
        TB-TOP_LINE = 1.
      ENDIF.
  ENDCASE.
ENDFORM.                    " PAGE
*&---------------------------------------------------------------------*
*&      Form  call_trans_8088
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_TRANS_8088.
  DATA: L_TYPE LIKE SY-REPID .
  CASE 'X'.
    WHEN P_01_8088.  " ENG PROD PALN
      IF SY-TCODE = 'ZPPA9999'.
        CALL TRANSACTION 'ZAPP_ENG_PLAN' . " AND SKIP FIRST SCREEN .
      ELSE.
        CALL TRANSACTION 'ZAPP_ENG_PIR' . " AND SKIP FIRST SCREEN .
      ENDIF.
    WHEN P_02_8088.  " ENG REPORT (Daily)
      CALL TRANSACTION 'ZRPP_ENG_MIP_D' . " AND SKIP FIRST SCREEN .
    WHEN P_03_8088.  " ENG REPORT (Weekly)
      CALL TRANSACTION 'ZRPP_ENG_MIP_M' . " AND SKIP FIRST SCREEN .
    WHEN P_04_8088.  " VEH PROD PLAN/ACT D
      IF SY-TCODE = 'ZPPA9999'.
        CALL TRANSACTION 'ZRPP_PROD_PLAN_D' . " AND SKIP FIRST SCREEN .
      ELSE.
        CALL TRANSACTION 'ZAPP_PROD_PLAN_D' . " AND SKIP FIRST SCREEN .
      ENDIF.
    WHEN P_05_8088.  " VEH PROD PLAN/ACT M
      IF SY-TCODE = 'ZPPA9999'.
        CALL TRANSACTION 'ZRPP_PROD_PLAN_M' . " AND SKIP FIRST SCREEN .
      ELSE.
        CALL TRANSACTION 'ZAPP_PROD_PLAN_M' . " AND SKIP FIRST SCREEN .
      ENDIF.
    WHEN P_06_8088.  " Monthly plan APS II
      CALL TRANSACTION 'ZRPP113I_2AB' . " AND SKIP FIRST SCREEN
    WHEN P_11_8088.  " Monthly plan APS II
      CALL TRANSACTION 'ZQPP_DEL_HMC'.
    WHEN P_12_8088.  " Monthly plan APS II
      CALL TRANSACTION 'ZQPP_ACT_HMC'.
    WHEN P_13_8088.  " Monthly plan APS II
      CALL TRANSACTION 'ZQPP_DTS_HMC'.
    WHEN P_14_8088.  " Monthly plan APS II
      CALL TRANSACTION 'ZRPP_VPC_HMA_NEW'.
    WHEN P_15_8088.  " VPC Stock: HMA
      CALL TRANSACTION 'ZPPR00001'.
  ENDCASE.

ENDFORM.                    " call_trans_8088
