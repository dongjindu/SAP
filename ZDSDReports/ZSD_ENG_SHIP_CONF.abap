************************************************************************
* Program Name      : ZSD_ENG_SHIP_CONF
* Author            : Furong Wang
* Creation Date     : 08/2009
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************

REPORT ZSD_ENG_SHIP_CONF NO STANDARD PAGE HEADING
                          LINE-SIZE 132
                          LINE-COUNT 64(1)
                          MESSAGE-ID ZMPP.
TYPE-POOLS: SLIS .

DATA: BEGIN OF IT_ITAB1 OCCURS 0,
      SEL(1),
      SEQ_NO(3) TYPE N,
      TRUCK_NO(12),
      TRUCK_DATE LIKE SY-DATUM,
      ASSYID(12),
      ITEM_NO LIKE JITMA-MATNR,
      RACK_NO(12),
      JIT_CALL_NO LIKE JITHD-PRODN,
      STOCK_DOC LIKE MKPF-MBLNR,
      ODNO LIKE LIKP-VBELN,
*      CRCH TYPE CHAR1,
*      APPROVE TYPE CHAR1,
      QTY_NO TYPE I,
      FIRST(1),
      MSG(255),
*      CELLTAB TYPE LVC_T_STYL,
      END OF IT_ITAB1.

DATA: WA_ITAB1 LIKE IT_ITAB1.

DATA: IT_VMASTER  LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

DATA:  CTUMODE LIKE CTU_PARAMS-DISMODE VALUE 'N',
       CUPDATE LIKE CTU_PARAMS-UPDMODE VALUE 'A',
       BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE,
       MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       IT_TLINE TYPE TLINE OCCURS 0 WITH HEADER LINE.

DATA: W_ODNO LIKE LIKP-VBELN,
      W_MSG(255),
      W_INDEX LIKE SY-TABIX.

DATA: W_TRUCK_FR LIKE AUSP-ATWRT,
      W_TRUCK_TO LIKE AUSP-ATWRT,
      W_VBELN_300 TYPE LIKP-VBELN,
      W_VBELN_300_TO LIKE LIKP-VBELN,
      W_DATE_FR TYPE SY-DATUM,
      W_DATE_TO TYPE SY-DATUM.

** ALV
DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
*       IT_FIELDCAT_FI  TYPE LVC_T_FCAT WITH HEADER LINE,
*       IT_FIELDCAT_CO  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.
*       IT_FIELDCAT_DET TYPE LVC_T_FCAT WITH HEADER LINE. "/Detail

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: BEGIN OF IT_TAB OCCURS 5,
       FCODE LIKE RSMPE-FUNC,
      END OF IT_TAB.

DATA: OK_CODE LIKE SY-UCOMM,
      W_CODE LIKE SY-UCOMM,
      W_OLD_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT   TYPE   I.

* -------------------------------------------------------------
* EVent class
*-----------------------------------------------------------
* local class to handle semantic checks
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA: G_EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

*************************************************************
* LOCAL CLASS Definition
**************************************************************
*§4.Define and implement event handler to handle event DATA_CHANGED.
*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS:
      HANDLE_DATA_CHANGED
         FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
              IMPORTING ER_DATA_CHANGED,

      HANDLE_LEFT_CLICK_RUN
         FOR EVENT LEFT_CLICK_RUN OF CL_GUI_ALV_GRID.

    DATA: ERROR_IN_DATA TYPE C.


ENDCLASS.
DATA :IT_LVC  LIKE LVC_S_ROW.
*************************************************************
* LOCAL CLASS IMPLEMENTATION
**************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_DATA_CHANGED.

    DATA: LS_GOOD TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
*          W_QTY(13),
*          W_QTY type p decimals 0,
          LVC_T_ROW TYPE LVC_T_ROW.

    ERROR_IN_DATA = SPACE.
    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LS_GOOD.
      CASE LS_GOOD-FIELDNAME.
* check if column Name1 of this row was changed
        WHEN 'SEL'.
          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
                     EXPORTING
                        I_ROW_ID  = LS_GOOD-ROW_ID
                        I_FIELDNAME = LS_GOOD-FIELDNAME
                     IMPORTING
                        E_VALUE =   LV_VALUE.
          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
                  EXPORTING
                       I_ROW_ID = LS_GOOD-ROW_ID
                       I_FIELDNAME = LS_GOOD-FIELDNAME
                       I_VALUE     = LV_VALUE.

      ENDCASE.
    ENDLOOP.

*§7.Display application log if an error has occured.
    IF ERROR_IN_DATA EQ 'X'.
      CALL METHOD ER_DATA_CHANGED->DISPLAY_PROTOCOL.
    ENDIF.
  ENDMETHOD.

  METHOD HANDLE_LEFT_CLICK_RUN.

  ENDMETHOD.

ENDCLASS.

*SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
**SELECT-OPTIONS : S_QMNUM FOR QMEL-QMNUM,
**                 S_ERDAT FOR QMEL-ERDAT,
*SELECTION-SCREEN END OF BLOCK BLOCK1.
*
*INITIALIZATION.
**  PERFORM INIT_DATA.
*
*START-OF-SELECTION.
*    CALL SCREEN 0200.



*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.

  DATA: BEGIN OF LT_DATA OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
*        POSID LIKE JITIT-POSID,
        END OF LT_DATA.
  DATA: BEGIN OF LT_PRODN OCCURS 0,
        MATNR LIKE JITMA-MATNR,
        GRPIN LIKE JITIT-GRPIN,
        FLAG(1),
*        PRODN LIKE JITHD-PRODN,
        END OF LT_PRODN.

  DATA: L_OD_NO LIKE AUSP-ATWRT,
        L_ATINN LIKE CABN-ATINN,
        L_VALS(8) TYPE N,
        L_ATFLV LIKE AUSP-ATFLV,
        L_DATE LIKE SY-DATUM,
        L_NEW(1),
        L_SEQ_NO TYPE I,
*        L_RECNO TYPE I,
        L_ITEM_NO LIKE IT_ITAB1-ITEM_NO,
          L_TRUCK_NO  LIKE IT_ITAB1-TRUCK_NO,
        L_RACK LIKE IT_ITAB1-RACK_NO,
        L_JIT_CALL_NO LIKE LT_PRODN-GRPIN,
        L_INDEX LIKE SY-TABIX.

  REFRESH IT_ITAB1.
  CLEAR: IT_ITAB1.

  SELECT SINGLE ATINN INTO L_ATINN
     FROM CABN
    WHERE ATNAM = 'EN_TRUCK_DATE' .

** Changed on 03/01/11
*  CLEAR: L_DATE.
  L_DATE = SY-DATUM - 3.
** End of change
  L_ATFLV  = L_VALS  = L_DATE.
  SELECT OBJEK INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    FROM AUSP
   WHERE KLART = '002'
     AND ATINN = L_ATINN
     AND ATFLV > L_ATFLV
     AND LKENZ = ' ' .

  LOOP AT LT_DATA.
    SELECT SINGLE ATWRT INTO L_OD_NO
      FROM AUSP AS A
      INNER JOIN CABN AS B
      ON A~ATINN = B~ATINN
      WHERE KLART = '002'
        AND OBJEK = LT_DATA-OBJEK
        AND ATNAM = 'EN_OUTBOUND_NO'.

    IF L_OD_NO IS INITIAL.
      PERFORM READ_NORMAL_CLASS USING LT_DATA-OBJEK 'EN_TRUCK_NO'
                               CHANGING IT_ITAB1-TRUCK_NO.

      PERFORM READ_NORMAL_CLASS USING LT_DATA-OBJEK 'EN_RACK_NO'
                              CHANGING IT_ITAB1-RACK_NO.

      PERFORM READ_NORMAL_CLASS USING LT_DATA-OBJEK 'EN_ITEM_CODE'
                                CHANGING IT_ITAB1-ITEM_NO.


      SELECT SINGLE AU~ATFLV INTO L_ATFLV
      FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
      WHERE OBJEK = LT_DATA-OBJEK
        AND KLART = '002'
        AND CA~ATNAM = 'EN_TRUCK_DATE'.

      L_VALS = L_ATFLV.
      IT_ITAB1-TRUCK_DATE = L_VALS.
      IT_ITAB1-QTY_NO = 1.
      IT_ITAB1-ASSYID = LT_DATA-OBJEK.
      APPEND IT_ITAB1.
      CLEAR: IT_ITAB1.
    ELSE.
      CLEAR: L_OD_NO.
    ENDIF.
  ENDLOOP.

  SORT IT_ITAB1 BY ITEM_NO TRUCK_NO RACK_NO ASSYID.
  IF IT_ITAB1[] IS INITIAL.
    MESSAGE I009 WITH 'No data'.
    EXIT.
  ENDIF.

  L_SEQ_NO = 1.
  L_TRUCK_NO = '*'.
  LOOP AT IT_ITAB1.
    IF IT_ITAB1-TRUCK_NO <> L_TRUCK_NO.
      L_SEQ_NO = 1.
      L_TRUCK_NO = IT_ITAB1-TRUCK_NO.
    ENDIF.
    IT_ITAB1-SEQ_NO = L_SEQ_NO.
    L_SEQ_NO =  L_SEQ_NO + 1.
    MODIFY IT_ITAB1.
  ENDLOOP.

  SELECT MATNR GRPIN INTO TABLE LT_PRODN
    FROM JITMA AS A
   INNER JOIN JITCO AS B
   ON A~MATID = B~MATID
    INNER JOIN JITIT AS D
    ON B~POSID = D~POSID
*     INNER JOIN JITHD AS C
*       ON C~JINUM = D~JINUM
    FOR ALL ENTRIES IN IT_ITAB1
    WHERE A~MATNR = IT_ITAB1-ITEM_NO
      AND D~INTST = '0000'.

  SORT LT_PRODN BY MATNR GRPIN.

  L_ITEM_NO = '*'.
  L_TRUCK_NO = '*'.
*  LOOP AT IT_ITAB1.
*    IF L_ITEM_NO <> IT_ITAB1-ITEM_NO
*       OR  L_TRUCK_NO <> IT_ITAB1-TRUCK_NO .
*      L_ITEM_NO = IT_ITAB1-ITEM_NO.
*      L_TRUCK_NO = IT_ITAB1-TRUCK_NO.
*      REFRESH LT_PRODN.
*      L_RECNO = 1.
*      L_RACK = 7.
*
*      SELECT GRPIN INTO TABLE LT_PRODN
*     FROM JITMA AS A
*    INNER JOIN JITCO AS B
*    ON A~MATID = B~MATID
*     INNER JOIN JITIT AS D
*     ON B~POSID = D~POSID
**     INNER JOIN JITHD AS C
**       ON C~JINUM = D~JINUM
*     WHERE A~MATNR = IT_ITAB1-ITEM_NO
*       AND D~INTST = '0000'.
*      SORT LT_PRODN BY GRPIN.
*
*    ENDIF.
*    IF L_RACK > 6.
*      READ TABLE LT_PRODN INDEX L_RECNO.
*      IF SY-SUBRC = 0.
*        L_RECNO = L_RECNO + 1.
*        L_JIT_CALL_NO = LT_PRODN-GRPIN.
*
*      ELSE.
*        CLEAR: L_JIT_CALL_NO.
*      ENDIF.
*      L_RACK = 1.
*    ENDIF.
*    L_RACK  = L_RACK  + 1.
*    IT_ITAB1-JIT_CALL_NO = L_JIT_CALL_NO.
*    MODIFY IT_ITAB1.
*  ENDLOOP.

  LOOP AT IT_ITAB1.
    IF L_ITEM_NO <> IT_ITAB1-ITEM_NO
       OR  L_TRUCK_NO <> IT_ITAB1-TRUCK_NO
       OR L_RACK > 6.
      L_ITEM_NO = IT_ITAB1-ITEM_NO.
      L_TRUCK_NO = IT_ITAB1-TRUCK_NO.
      L_RACK = 1.

      READ TABLE LT_PRODN WITH KEY MATNR = IT_ITAB1-ITEM_NO
                                   FLAG = ' '.
      IF SY-SUBRC = 0.
        L_INDEX = SY-TABIX.
        LT_PRODN-FLAG = 'X'.
        MODIFY LT_PRODN INDEX L_INDEX TRANSPORTING FLAG..
        L_JIT_CALL_NO = LT_PRODN-GRPIN.
      ELSE.
        CLEAR: L_JIT_CALL_NO.
      ENDIF.
    ENDIF.
    L_RACK  = L_RACK  + 1.
    IT_ITAB1-JIT_CALL_NO = L_JIT_CALL_NO.
    MODIFY IT_ITAB1.
  ENDLOOP.

  SORT IT_ITAB1 BY TRUCK_NO RACK_NO ITEM_NO ASSYID .
  LOOP AT IT_ITAB1.
    AT NEW TRUCK_NO.
      L_NEW = 'X'.
    ENDAT.
    IF L_NEW IS INITIAL.
      CLEAR: IT_ITAB1-FIRST.
    ELSE.
      CLEAR: L_NEW.
      IT_ITAB1-FIRST = 'X'.
    ENDIF.
    MODIFY IT_ITAB1.
  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'ST9000'.
  SET TITLEBAR 'ST9000'.
*  if it_itab1[] is initial.
*   perform make_dropdown_list.
*  endif.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  DATA: L_COMM LIKE SY-UCOMM.
  L_COMM = SY-UCOMM.
  CASE L_COMM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR: L_COMM.
ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&spwizard: declaration of tablecontrol 'TC_9000' itself
CONTROLS: TC_9000 TYPE TABLEVIEW USING SCREEN 9000.

*&spwizard: output module for tc 'TC_9000'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
MODULE TC_9000_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_ITAB1 LINES TC_9000-LINES.
ENDMODULE.

*&spwizard: input module for tc 'TC_9000'. do not change this line!
*&spwizard: modify table
MODULE TC_9000_MODIFY INPUT.
  MODIFY IT_ITAB1
    INDEX TC_9000-CURRENT_LINE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_DATA.
ENDFORM.                    " INIT_DATA
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING    PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                    " bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0305   text
*      -->P_0306   text
*----------------------------------------------------------------------*
FORM BDC_FIELD USING    FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                    " bdc_field

*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0315   text
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION TABLES  P_IT_TLINE STRUCTURE TLINE
                      USING TCODE P_FLAG.

  DATA: L_SUBRC LIKE SY-SUBRC,
        MSG(255).

* call transaction using

  REFRESH: MESSTAB.

  CALL TRANSACTION TCODE USING BDCDATA
                   MODE   CTUMODE
                   UPDATE CUPDATE
                   MESSAGES INTO MESSTAB.
  L_SUBRC = SY-SUBRC.

  READ TABLE MESSTAB WITH KEY MSGTYP = 'E'.

  IF SY-SUBRC = 0.
*    LOOP AT MESSTAB.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH MESSTAB-MSGV1 MESSTAB-MSGV2 MESSTAB-MSGV3
*                 MESSTAB-MSGV4.
*
*      CONCATENATE MSG MESSTAB-MSGV1 INTO MSG.
*    ENDLOOP.
*    CONCATENATE 'System Status' MSG INTO P_IT_TLINE-TDLINE
*              SEPARATED BY SPACE.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              MSGID               = SY-MSGID
              MSGNR               = SY-MSGNO
              MSGV1               = SY-MSGV1
              MSGV2               = SY-MSGV2
              MSGV3               = SY-MSGV3
              MSGV4               = SY-MSGV4
         IMPORTING
              MESSAGE_TEXT_OUTPUT = MSG.

    REFRESH BDCDATA.
    P_FLAG = 'E'.
    CONCATENATE '* Error:' TCODE  MSG INTO W_MSG
       SEPARATED BY SPACE.
    P_IT_TLINE-TDFORMAT = 'E' .
    P_IT_TLINE-TDLINE = MSG.
    MESSAGE I000 WITH 'Error:' MSG.
  ELSE.
    READ TABLE MESSTAB WITH KEY MSGTYP = 'A'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                MSGID               = SY-MSGID
                MSGNR               = SY-MSGNO
                MSGV1               = SY-MSGV1
                MSGV2               = SY-MSGV2
                MSGV3               = SY-MSGV3
                MSGV4               = SY-MSGV4
           IMPORTING
                MESSAGE_TEXT_OUTPUT = MSG.

      REFRESH BDCDATA.
      P_FLAG = 'E'.
      CONCATENATE '* Error:' TCODE  MSG INTO W_MSG
       SEPARATED BY SPACE.
      P_IT_TLINE-TDFORMAT = 'E' .
      P_IT_TLINE-TDLINE = MSG.
      MESSAGE I000 WITH 'Error:' MSG.
    ELSE.
      P_FLAG = 'S'.
*      COMMIT WORK.
      MSG =  TEXT-M04.
      W_MSG =  TEXT-M04.
      REFRESH BDCDATA.
      P_IT_TLINE-TDFORMAT = 'S' .
      P_IT_TLINE-TDLINE = MSG.
      MESSAGE S000 WITH  MSG.
    ENDIF.
  ENDIF.
  APPEND P_IT_TLINE.
  CLEAR: P_IT_TLINE, MSG.
  REFRESH: MESSTAB.
ENDFORM.                    " bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_DATA.
  CLEAR: IT_ITAB1[], BDCDATA[], MESSTAB[], IT_TLINE[].
  CLEAR: WA_ITAB1 .
*  clear:  P_FEGRP,  P_FECOD.
ENDFORM.                    " CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Module  tc_9000_change_field_attr  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_9000_CHANGE_FIELD_ATTR OUTPUT.
* if IT_ITAB1-hmma = 'X'.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'IT_ITAB1-APPROVE'.
      IF IT_ITAB1-TRUCK_NO IS INITIAL.
        SCREEN-INPUT = 0.
      ELSE.
        SCREEN-INPUT = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " tc_9000_change_field_attr  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  READ_NORMAL_CLASS_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_APP245_OBJEK  text
*      -->P_0276   text
*      <--P_IT_TEMP_APP245_SUMINF  text
*----------------------------------------------------------------------*
FORM READ_NORMAL_CLASS USING P_OBJEK P_CHAR
                              CHANGING P_VALUE.
  SELECT SINGLE AU~ATWRT
    INTO P_VALUE
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_OBJEK      AND
          KLART = '002'       AND
          CA~ATNAM = P_CHAR  .
ENDFORM.                    " read_normal_classification
*&---------------------------------------------------------------------*
*&      Form  PROCESS_JITM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_JITM USING PA_TRUCK.
  DATA: BEGIN OF LT_JIT_CALL OCCURS 0,
        JIT_CALL_NO LIKE JITHDDAT-PRODN,
        END OF LT_JIT_CALL.
  DATA: L_COUNT(2) TYPE N,
        L_LIST_CN(2) TYPE N,
        L_TEXT(28),
        L_FLAG(1).
  DATA: L_KUNNR LIKE jitcusel-kunnr.

*  FIELD-SYMBOLS : <FS01>.

  REFRESH: BDCDATA, IT_TLINE.

** Changed by Furogn on 10/22/12 for clear memory
  CLEAR: L_KUNNR.
  SET PARAMETER ID 'VAG' FIELD  L_KUNNR.
** End
  LOOP AT IT_ITAB1 WHERE TRUCK_NO = PA_TRUCK.
    IF IT_ITAB1-ODNO IS INITIAL AND IT_ITAB1-JIT_CALL_NO <> ' '.
      LT_JIT_CALL-JIT_CALL_NO = IT_ITAB1-JIT_CALL_NO.
      COLLECT: LT_JIT_CALL.
      CLEAR: LT_JIT_CALL.
    ELSE.
      IF IT_ITAB1-JIT_CALL_NO = ' '.
        MESSAGE E000 WITH 'No JIT Call for IT_ITAB1-assyid' .
      ELSE.
        MESSAGE E000 WITH 'Cannot re-create OD'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM BDC_DYNPRO      USING 'RJITMON001' '1000'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'KUNNR_PT-LOW'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=JIT_C'.
  PERFORM BDC_FIELD       USING 'FBX_PV'
                                'X'.
  PERFORM BDC_FIELD       USING 'RBX_PV'
                                'X'.
  PERFORM BDC_FIELD       USING 'BBX_PV'
                                'X'.
  PERFORM BDC_FIELD       USING 'CBX_PV'
                                'X'.
  PERFORM BDC_FIELD       USING 'ABX_PV'
                                'X'.

*  PERFORM BDC_DYNPRO      USING 'RJITMON001' '1000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'KUNNR_PT-LOW'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=JITTAB2'.

  PERFORM BDC_DYNPRO      USING 'RJITMON001' '1000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=JITTAB3'.

  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'INTST_PT-LOW'.

*  PERFORM BDC_DYNPRO      USING 'RJITMON001' '1000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'KUNNR_PT-LOW'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=%00410200000259787'.

  PERFORM BDC_DYNPRO      USING 'RJITMON001' '1000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=%00810300000259787'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'GRPIN_PT-LOW'.

*  PERFORM BDC_DYNPRO      USING 'SAPLALDB' '3000'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '/00'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                              'RSCSEL-SLOW_I(02)'.
*
*  L_COUNT = '01'.
*  LOOP AT LT_JIT_CALL.
*    IF L_COUNT > 9.
*      L_COUNT = '01'.
*      PERFORM BDC_DYNPRO      USING 'SAPLALDB' '3000'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                    '=P+'.
*      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                    'RSCSEL-SLOW_I(09)'.
*
*    ENDIF.
*    CONCATENATE 'RSCSEL-SLOW_I(' L_COUNT ')' INTO L_TEXT.
**    ASSIGN (L_TEXT) TO <FS01>.
*    PERFORM BDC_FIELD  USING L_TEXT
*                       LT_JIT_CALL-JIT_CALL_NO.
*
*    L_COUNT = L_COUNT + 1.
*  ENDLOOP.
*
*  PERFORM BDC_DYNPRO      USING 'SAPLALDB' '3000'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=ACPT'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'RSCSEL-SLOW_I(02)'.



  PERFORM BDC_DYNPRO      USING 'SAPLALDB' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=P+'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'RSCSEL-SLOW_I(01)'.
                                'RSCSEL_255-SLOW_I(01)'.


  L_COUNT = '01'.
  L_LIST_CN = '01'.
*  LOOP AT LT_JIT_CALL.
*    IF L_LIST_CN > 8.
*      L_COUNT = '02'.
*      L_LIST_CN = '01'.
*      PERFORM BDC_DYNPRO      USING 'SAPLALDB' '3000'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                    '=P+'.
*      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
**                                    'RSCSEL-SLOW_I(02)'.
*                                    'RSCSEL_255-SLOW_I(02)'.
*    ENDIF.

 LOOP AT LT_JIT_CALL.
    IF L_LIST_CN > 8.
      L_COUNT = '02'.
      L_LIST_CN = '02'.
      PERFORM BDC_DYNPRO      USING 'SAPLALDB' '3000'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=P+'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                    'RSCSEL-SLOW_I(02)'.
                                    'RSCSEL_255-SLOW_I(02)'.
    ENDIF.


*    CONCATENATE 'RSCSEL-SLOW_I(' L_COUNT ')' INTO L_TEXT.
        CONCATENATE 'RSCSEL_255-SLOW_I(' L_COUNT ')' INTO L_TEXT.
*    ASSIGN (L_TEXT) TO <FS01>.
    PERFORM BDC_FIELD  USING L_TEXT
                       LT_JIT_CALL-JIT_CALL_NO.

    L_COUNT = L_COUNT + 1.
    L_LIST_CN = L_LIST_CN + 1.
  ENDLOOP.

  PERFORM BDC_DYNPRO      USING 'SAPLALDB' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=ACPT'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'RSCSEL-SLOW_I(02)'.
'RSCSEL_255-SLOW_I(02)'.

  PERFORM BDC_DYNPRO      USING 'RJITMON001' '1000'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'KUNNR_PT-LOW'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=ONLI'.

  PERFORM BDC_FIELD       USING 'WERKS_PT-LOW'
                                 ' '.
*  PERFORM BDC_FIELD       USING 'TBX_PV'
*                                'X'.
*  PERFORM BDC_FIELD       USING 'NBX_PV'
*                                'X'.

  PERFORM BDC_DYNPRO      USING 'SAPMSSY0' '0120'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                '06/03'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=&ALL'.
  PERFORM BDC_DYNPRO      USING 'SAPMSSY0' '0120'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                '06/03'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=ACTN'.
  PERFORM BDC_DYNPRO      USING 'SAPLJIT05' '0400'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'IO400_GS-CNRTL(01)'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=EXEC'.
  PERFORM BDC_FIELD       USING 'IO400_GS-ASEL(01)'
                                'X'.
*                              record-ASEL_01_074.
  PERFORM BDC_TRANSACTION TABLES IT_TLINE USING 'JITM' L_FLAG .


ENDFORM.                    " PROCESS_JITM
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'ST200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_alv  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_ITAB1'.
    PERFORM ASSIGN_ITAB_TO_ALV.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.
ENDMODULE.                 " display_alv  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  W_CODE = OK_CODE.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'POST'.   " OR 'OD' OR 'OD&TR'.
      PERFORM POST_ALL.
*    WHEN 'TRANSFER'.
*      PERFORM POST_STOCK_TRANSFER.
    WHEN 'PRINT' OR 'SEND'.
      PERFORM CALL_VL71 USING '1'.
    WHEN 'CAL300'.
      PERFORM CLEAR_DATA_300.
      CALL SCREEN '300'.
      LEAVE PROGRAM.
    WHEN 'REFRESH'.
      PERFORM REFRESH_DATA.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
               I_DEFAULT        = SPACE
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_ITAB1[]
               IT_SORT          = IT_SORT[].

** ENTER
  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
                EXPORTING
                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

* Cursor----
  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
                EXPORTING
                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CREATE OBJECT G_EVENT_RECEIVER.
  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
  SET HANDLER G_EVENT_RECEIVER->HANDLE_LEFT_CLICK_RUN FOR ALV_GRID.

  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID.

ENDFORM.                    " assign_itab1_to_alv
*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
*  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
*  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
*  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*  WA_IS_LAYOUT-BOX_FNAME = 'SEL'.
*  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.
ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.

  IT_SORT-SPOS           = 1.
  IT_SORT-FIELDNAME      = 'TRUCK_NO'.
  IT_SORT-UP             = 'X'.
  IT_SORT-SUBTOT         = 'X'.
  APPEND IT_SORT.

ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_ITAB.

  DATA: LW_ITAB TYPE SLIS_TABNAME.
*        lw_waers LIKE t001-waers,

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

*                                 'S' 'SEL'    ' ',
*                                  ' ' 'COLTEXT'     'Check',
*                                  ' ' 'CHECKBOX'    'X',
**                                  ' ' 'INPUT'    'X',
**                                   ' ' 'EDIT'    'X',
*                                  'E' 'OUTPUTLEN'   '5',


                                  'S' 'SEQ_NO'    ' ',
*                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Seq No',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'TRUCK_NO'    ' ',
*                                 ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Truck No',
                                  'E' 'OUTPUTLEN'   '18',


                                  'S' 'TRUCK_DATE'    ' ',
                                  ' ' 'COLTEXT'     'Truck Date',
                                  'E' 'OUTPUTLEN'   '10',


                                  'S' 'RACK_NO'     ' ',
                                  ' ' 'COLTEXT'     'Rack No',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'ITEM_NO'       ' ',
                                  ' ' 'COLTEXT'     'Item No',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'ASSYID'       ' ',
                                  ' ' 'COLTEXT'     'ASSY ID',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'JIT_CALL_NO'        ' ',
                                  ' ' 'COLTEXT'     'JIT Call No',
                                  'E' 'OUTPUTLEN'   '18',


                                  'S' 'ODNO'        ' ',
                                  ' ' 'COLTEXT'     'O/D Number',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'STOCK_DOC'        ' ',
                                  ' ' 'COLTEXT'     'Stock Transfer #',
                                  'E' 'OUTPUTLEN'   '18',


                                  'S' 'QTY_NO'     ' ',
                                  ' ' 'COLTEXT'     'Qty No',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S'  'MSG'      ' ',
                                  ' ' 'COLTEXT'     'Message',
                                  'E' 'OUTPUTLEN'   '100'
                   .


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  CLEAR: W_REPID.
  CREATE OBJECT GRID_CONTAINER
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL
          EXCEPTIONS
           CNTL_ERROR = 1
           CNTL_SYSTEM_ERROR = 2
           CREATE_ERROR = 3
           LIFETIME_ERROR = 4
           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT ALV_GRID
         EXPORTING I_PARENT = GRID_CONTAINER
                   I_APPL_EVENTS = 'X'.
ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog'.
    ENDIF.

    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    ADD 1 TO W_CNT.
    P_FIELDCAT-COL_POS = W_CNT.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*&---------------------------------------------------------------------*
*&      Form  SELECT_EDIT_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_EDIT_LINE.
*
*  DATA: LT_CELLTAB TYPE LVC_T_STYL,
*         W_CELLTAB TYPE LVC_S_STYL,
*         L_INDEX TYPE I,
*         L_MODE TYPE RAW4.
*
*  L_MODE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*  LOOP AT IT_ITAB1.
*    L_INDEX = SY-TABIX.
*    REFRESH LT_CELLTAB.
*    IF IT_ITAB1-FIRST = 'X'.
*      L_MODE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*    ELSE.
*      L_MODE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    ENDIF.
*
*    W_CELLTAB-FIELDNAME = 'SEL'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*
*    W_CELLTAB-FIELDNAME = 'TRUCK_NO'.
*    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*
*    W_CELLTAB-FIELDNAME = 'RACK_NO'.
*    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*
*    W_CELLTAB-FIELDNAME = 'ITEM_NO'.
*    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*
*    W_CELLTAB-FIELDNAME = 'ASSYID'.
*    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*
*    W_CELLTAB-FIELDNAME = 'JIT_CALL_NO'.
*    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*
*    INSERT LINES OF LT_CELLTAB INTO TABLE IT_ITAB1-CELLTAB.
*    MODIFY IT_ITAB1 INDEX L_INDEX.
*  ENDLOOP.

ENDFORM.                    " SELECT_EDIT_LINE
*&---------------------------------------------------------------------*
*&      Form  PERFORM_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_ALL.

  DATA: L_TRUCK LIKE IT_ITAB1-TRUCK_NO,
        L_JIT_CALL_NO LIKE IT_ITAB1-JIT_CALL_NO.

  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
          LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I,
        L_TEXT01(80),
        L_TEXT02(80),
       L_TEXT03(80),
       L_TEXT04(80),
       L_LABST LIKE MARD-LABST,
       L_LABST_CHAR(10),
       L_NO(2) TYPE N,
       L_TEXT_FS(20).

  DATA: BEGIN OF LT_MATNR OCCURS 0,
        MATNR LIKE IT_ITAB1-ITEM_NO,
        QTY LIKE MARD-LABST,
        END OF LT_MATNR.

  DATA: BEGIN OF  LT_POPUP OCCURS 10,
        MESSAGE(80),
        END OF  LT_POPUP.

  FIELD-SYMBOLS: <FS>.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error Found During Flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M05.
  ENDIF.
  READ TABLE IT_ITAB1 INDEX LT_ROWS-INDEX.

  IF SY-SUBRC = 0.

** Changed by Furong on 12/03/09
    LOOP AT IT_ITAB1 WHERE TRUCK_NO = IT_ITAB1-TRUCK_NO.
      LT_MATNR-MATNR = IT_ITAB1-ITEM_NO.
      LT_MATNR-QTY = 1.
      COLLECT LT_MATNR.
      CLEAR: LT_MATNR.
    ENDLOOP.
    LOOP AT LT_MATNR.
      CLEAR: L_LABST.
      SELECT SINGLE LABST INTO L_LABST
        FROM MARD
        WHERE MATNR = LT_MATNR-MATNR
         AND WERKS = 'E001'
         AND  LGORT = 'E301'.
      IF L_LABST < LT_MATNR-QTY.
        L_LABST_CHAR = L_LABST.
        CONCATENATE LT_MATNR-MATNR 'has' L_LABST_CHAR 'in E301'
               INTO LT_POPUP-MESSAGE SEPARATED BY SPACE.
        APPEND LT_POPUP.
        CLEAR:  LT_POPUP.
      ENDIF.
    ENDLOOP.
    IF NOT LT_POPUP[] IS INITIAL.
      L_NO = '01'.
      LOOP AT LT_POPUP FROM 1 TO 4.
        CONCATENATE 'L_TEXT' L_NO INTO L_TEXT_FS.
        ASSIGN (L_TEXT_FS) TO <FS>.
        <FS> = LT_POPUP-MESSAGE.
        L_NO =  L_NO + 1.
      ENDLOOP.
      CALL FUNCTION 'POPUP_TO_INFORM'
           EXPORTING
                TITEL = 'Insufficient Stock'
                TXT1  = 'Please check your available stock in E301'
                TXT2  = L_TEXT01
                TXT3  = L_TEXT02
                TXT4  = L_TEXT03.
      EXIT.
    ENDIF.
** End of change
    W_INDEX = LT_ROWS-INDEX.
*** Create O/D
    CLEAR: W_ODNO, L_JIT_CALL_NO, W_MSG.

    L_JIT_CALL_NO = IT_ITAB1-JIT_CALL_NO.
    PERFORM PROCESS_JITM USING IT_ITAB1-TRUCK_NO.

    WAIT UP TO 3 SECONDS.

    SELECT SINGLE VBELN_VL INTO W_ODNO
       FROM JITIT AS A
       WHERE GRPIN = L_JIT_CALL_NO.

    IF W_ODNO IS INITIAL.

** LOG THE ERROR
      CONCATENATE W_MSG '** No OD document was created' INTO W_MSG
        SEPARATED BY SPACE.
      MESSAGE E000 WITH 'No OD was created'.
    ELSE.

* assigne Serial Number & Truck No
      PERFORM UPDATE_ODNO USING IT_ITAB1-TRUCK_NO.
      PERFORM ASSIGN_SERIAL_TRUCK_NO USING IT_ITAB1-TRUCK_NO.
      CLEAR: W_MSG.

* update engine master
      PERFORM UPDATE_ENGINE_MASTER USING IT_ITAB1-TRUCK_NO.
      CASE W_CODE.
        WHEN 'POST'.
* Stock transfer posting
          PERFORM PROCESS_MB1B USING IT_ITAB1-TRUCK_NO.
** Print OD
*          PERFORM PRINT_OD USING W_ODNO '1'.
** Send ASN
*          PERFORM SEND_ASN  USING W_ODNO '1'.
* Print list
*          PERFORM PRINT_LIST USING IT_ITAB1-TRUCK_NO.
        WHEN  'OD&TR'.
          PERFORM PROCESS_MB1B USING IT_ITAB1-TRUCK_NO.
      ENDCASE.

*      PERFORM PROCESS_VL71 USING W_ODNO.
    ENDIF.
  ENDIF.

  CLEAR: L_TRUCK.

ENDFORM.                    " PERFORM_POST
*&---------------------------------------------------------------------*
*&      Form  refresh_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_DATA.
  PERFORM CHECK_STATUS.
  PERFORM GET_DATA.
ENDFORM.                    " refresh_data
*&---------------------------------------------------------------------*
*&      Form  PROCESS_MB1B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_MB1B USING PA_TRUCK.

  DATA: GOODSMVT_HEADER   LIKE  BAPI2017_GM_HEAD_01,
    GOODSMVT_CODE     LIKE  BAPI2017_GM_CODE,
    IT_GOODSMVT_ITEM
    LIKE TABLE OF BAPI2017_GM_ITEM_CREATE WITH HEADER LINE,
    IT_RETURN  LIKE TABLE OF BAPIRET2 WITH HEADER LINE.

  DATA: L_MBLNR    TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC,
        L_MJAHR    TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR,
        MSG(255),
        L_OBJECT LIKE MARA-MATNR.

  DATA: BEGIN OF LT_MATNR OCCURS 0,
        MATNR LIKE IT_ITAB1-ITEM_NO,
        QTY LIKE IT_GOODSMVT_ITEM-ENTRY_QNT,
        END OF LT_MATNR.

  REFRESH IT_TLINE.

  LOOP AT IT_ITAB1 WHERE TRUCK_NO = PA_TRUCK.
    IF IT_ITAB1-JIT_CALL_NO IS INITIAL.
      CONTINUE.
    ENDIF.

    LT_MATNR-MATNR = IT_ITAB1-ITEM_NO.
    LT_MATNR-QTY = 1.
    COLLECT LT_MATNR.
    CLEAR: LT_MATNR.
  ENDLOOP.

*  LOOP AT IT_ITAB1 WHERE TRUCK_NO = PA_TRUCK.

  CLEAR: GOODSMVT_HEADER, GOODSMVT_CODE, L_MBLNR, L_MJAHR.
  REFRESH:  IT_GOODSMVT_ITEM, IT_RETURN.

  GOODSMVT_HEADER-PSTNG_DATE = SY-DATUM.
  GOODSMVT_HEADER-DOC_DATE   = SY-DATUM.
  GOODSMVT_HEADER-HEADER_TXT = IT_ITAB1-ASSYID.
*  GM_Code 01: Goods receipt for purchase order
*  GM_Code 02: Goods receipt for production order
*  GM_Code 03: Goods issue
*  GM_Code 04: Transfer posting
*  GM_Code 05: Other goods receipts
*  GM_Code 06: Reversal of goods movements
  GOODSMVT_CODE = '04'.
*    IT_GOODSMVT_ITEM-MATERIAL  = IT_ITAB1-ITEM_NO.   "MATERIAL
  IT_GOODSMVT_ITEM-ENTRY_QNT  = 1.
  IT_GOODSMVT_ITEM-PLANT     = 'E001' . "Engine PLANT
  IT_GOODSMVT_ITEM-STGE_LOC   = 'E301'.      "FROM
  IT_GOODSMVT_ITEM-MOVE_TYPE  = '311' . "MVT type
*    IT_GOODSMVT_ITEM-ENTRY_UOM  = IT_ZTPPERM-MEINS.
  IT_GOODSMVT_ITEM-MOVE_STLOC = 'E302'.  "TO Stock

  LOOP AT LT_MATNR.
    IT_GOODSMVT_ITEM-MATERIAL  = LT_MATNR-MATNR.   "MATERIAL
    IT_GOODSMVT_ITEM-ENTRY_QNT  = LT_MATNR-QTY.
    APPEND IT_GOODSMVT_ITEM.
  ENDLOOP.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      GOODSMVT_HEADER             = GOODSMVT_HEADER
      GOODSMVT_CODE               = GOODSMVT_CODE
    IMPORTING
      MATERIALDOCUMENT            = L_MBLNR
      MATDOCUMENTYEAR             = L_MJAHR
    TABLES
      GOODSMVT_ITEM               = IT_GOODSMVT_ITEM
*      GOODSMVT_SERIALNUMBER       =
      RETURN                      = IT_RETURN.

  IF SY-SUBRC = 0 AND L_MBLNR <> '  '.
*    IT_ITAB1-STOCK_DOC = L_MBLNR.
    IT_TLINE-TDFORMAT = 'S' .
    CONCATENATE IT_ITAB1-STOCK_DOC 'was created sucessfully' INTO MSG
     SEPARATED BY SPACE.
    IT_TLINE-TDLINE = MSG.

** Update  Engine master , transfer document
    CLEAR: IT_VMASTER,
           IT_VMASTER[].
    PERFORM APPEND_VMASTER USING 'EN_TRANSFER_DOC'  L_MBLNR.

    LOOP AT IT_ITAB1 WHERE TRUCK_NO = PA_TRUCK.

      L_OBJECT =  IT_ITAB1-ASSYID.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                OBJECT     = L_OBJECT
                MODE       = 'W'
                CMODE      = '002'
           TABLES
                VAL_TABLE  = IT_VMASTER
           EXCEPTIONS
                NO_DATA    = 1
                ERROR_MODE = 2
                OTHERS     = 3.

      IF SY-SUBRC = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ELSE.
        REFRESH BDCDATA.
        W_MSG =  'Error in updating engine master'.
        CONCATENATE IT_ITAB1-MSG W_MSG INTO IT_ITAB1-MSG
        SEPARATED BY SPACE.
        IT_TLINE-TDFORMAT = 'E' .
        IT_TLINE-TDLINE = W_MSG.
        APPEND IT_TLINE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
    ENDLOOP.
  ELSE.
** Error log
    READ TABLE IT_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
           EXPORTING
                LANGU = SY-LANGU
                MSGID = IT_RETURN-ID
                MSGNO = IT_RETURN-NUMBER
                MSGV1 = IT_RETURN-MESSAGE_V1+0(50)
                MSGV2 = IT_RETURN-MESSAGE_V2+0(50)
                MSGV3 = IT_RETURN-MESSAGE_V3+0(50)
                MSGV4 = IT_RETURN-MESSAGE_V4+0(50)
           IMPORTING
                TEXT  = MSG.
    ELSE.
      READ TABLE IT_RETURN WITH KEY TYPE = 'A'.
      IF SY-SUBRC = 0.
        CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
             EXPORTING
                  LANGU = SY-LANGU
                  MSGID = IT_RETURN-ID
                  MSGNO = IT_RETURN-NUMBER
                  MSGV1 = IT_RETURN-MESSAGE_V1+0(50)
                  MSGV2 = IT_RETURN-MESSAGE_V2+0(50)
                  MSGV3 = IT_RETURN-MESSAGE_V3+0(50)
                  MSGV4 = IT_RETURN-MESSAGE_V4+0(50)
             IMPORTING
                  TEXT  = MSG.
      ELSE.
        READ TABLE IT_RETURN INDEX 1.
        IF SY-SUBRC = 0.
          CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
               EXPORTING
                    LANGU = SY-LANGU
                    MSGID = IT_RETURN-ID
                    MSGNO = IT_RETURN-NUMBER
                    MSGV1 = IT_RETURN-MESSAGE_V1+0(50)
                    MSGV2 = IT_RETURN-MESSAGE_V2+0(50)
                    MSGV3 = IT_RETURN-MESSAGE_V3+0(50)
                    MSGV4 = IT_RETURN-MESSAGE_V4+0(50)
               IMPORTING
                    TEXT  = MSG.
        ENDIF.
        W_MSG = 'Error in transfer posting'.
        CONCATENATE IT_ITAB1-MSG W_MSG INTO IT_ITAB1-MSG
        SEPARATED BY SPACE.
*          IT_ITAB1-STOCK_DOC = 'Posting Error'.
        IT_TLINE-TDFORMAT = 'E' .
        IT_TLINE-TDLINE = MSG.
*          MESSAGE I999 WITH 'Error:' MSG.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
*        CLEAR: GOODSMVT_HEADER, GOODSMVT_CODE,  L_MBLNR, L_MJAHR.
*        REFRESH: IT_GOODSMVT_ITEM, IT_RETURN.
    ENDIF.
  ENDIF.

  LOOP AT IT_ITAB1 WHERE TRUCK_NO = PA_TRUCK.
    IT_ITAB1-MSG  = W_MSG.
    IT_ITAB1-STOCK_DOC = L_MBLNR.
    MODIFY IT_ITAB1  TRANSPORTING STOCK_DOC MSG.
  ENDLOOP.
*  ENDLOOP.

ENDFORM.                    " PROCESS_MB1B
*&---------------------------------------------------------------------*
*&      Form  update_engine_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB1_TRUCK_NO  text
*----------------------------------------------------------------------*
FORM UPDATE_ENGINE_MASTER USING PA_TRUCK.

  DATA: L_DATE(8),
        L_TIME(8),
        MSG(255),
        L_OBJECT LIKE MARA-MATNR.

  L_DATE = SY-DATUM.
  L_TIME = SY-UZEIT.
  REFRESH: IT_TLINE.

  LOOP AT IT_ITAB1 WHERE TRUCK_NO = PA_TRUCK.
    IF IT_ITAB1-JIT_CALL_NO IS INITIAL.
      CONTINUE.
    ENDIF.
    L_OBJECT =  IT_ITAB1-ASSYID.
    CLEAR : IT_VMASTER,  IT_VMASTER[] , W_MSG.
    PERFORM APPEND_VMASTER USING 'EN_OUTBOUND_NO' W_ODNO.
    PERFORM APPEND_VMASTER USING 'EN_JITCALL_NO' IT_ITAB1-JIT_CALL_NO.
    PERFORM APPEND_VMASTER USING 'EN_OUTBOUND_DATE' L_DATE.
    PERFORM APPEND_VMASTER USING 'EN_OUTBOUND_TIME' L_TIME.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT     = L_OBJECT
              MODE       = 'W'
              CTYPE      = '002'
         TABLES
              VAL_TABLE  = IT_VMASTER
         EXCEPTIONS
              NO_DATA    = 1
              ERROR_MODE = 2
              OTHERS     = 3.
    IF SY-SUBRC NE 0.
** ERROR LOG

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                MSGID               = SY-MSGID
                MSGNR               = SY-MSGNO
                MSGV1               = SY-MSGV1
                MSGV2               = SY-MSGV2
                MSGV3               = SY-MSGV3
                MSGV4               = SY-MSGV4
           IMPORTING
                MESSAGE_TEXT_OUTPUT = MSG.

      REFRESH BDCDATA.
      CONCATENATE 'Error update engime master:'  MSG INTO W_MSG
         SEPARATED BY SPACE.
      CONCATENATE IT_ITAB1-MSG W_MSG INTO IT_ITAB1-MSG
      SEPARATED BY SPACE.
      MODIFY IT_ITAB1.
      IT_TLINE-TDFORMAT = 'E' .
      IT_TLINE-TDLINE = MSG.
      APPEND IT_TLINE.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " update_engine_master

*&---------------------------------------------------------------------*
*&      Form  APPEND_VMASTER
*&---------------------------------------------------------------------*
FORM APPEND_VMASTER USING P_ATNAM  P_ATWRT .
  CLEAR IT_VMASTER .
  IT_VMASTER-ATNAM = P_ATNAM    .
  IT_VMASTER-ATWRT = P_ATWRT    .
  APPEND IT_VMASTER.
ENDFORM.                    " APPEND_VMASTER
*&---------------------------------------------------------------------*
*&      Form  send_asn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_ASN  USING P_ODNO P_MODE.

  REFRESH: BDCDATA, IT_TLINE.

  PERFORM BDC_DYNPRO      USING 'SD70AV2A' '1000'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RG_VBELN-LOW'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=ONLI'.
  PERFORM BDC_FIELD       USING 'RG_KSCHL-LOW'
                                'LALE'.
*perform bdc_field       using 'PM_NSORT'
*                              record-PM_NSORT_002.
  PERFORM BDC_FIELD       USING 'PM_VERMO'
                                P_MODE.
  PERFORM BDC_FIELD       USING 'RG_VBELN-LOW'
                                P_ODNO.
  PERFORM BDC_DYNPRO      USING 'SAPMSSY0' '0120'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                '04/03'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=&ALL'.
  PERFORM BDC_DYNPRO      USING 'SAPMSSY0' '0120'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                '04/03'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=PROC'.

  PERFORM BDC_TRANSACTION_1 TABLES IT_TLINE
                            USING 'VL71' 'S'.

ENDFORM.                    " send_asn
*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE  text
*      -->P_2821   text
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION_1 TABLES  P_IT_TLINE STRUCTURE TLINE
                      USING TCODE MODE.

  DATA: L_SUBRC LIKE SY-SUBRC,
        MSG(255).

  CALL TRANSACTION TCODE USING BDCDATA
                   MODE   CTUMODE
                   UPDATE CUPDATE
                   MESSAGES INTO MESSTAB.

  L_SUBRC = SY-SUBRC.

  IF L_SUBRC = 0.
    IF MODE = 'P'.
      MSG =  'Successfuly Printed DCR'.
    ELSE.
      MSG =  'Successfully Sent out ASN'.
    ENDIF.

    REFRESH BDCDATA.
    P_IT_TLINE-TDFORMAT = 'S' .
    P_IT_TLINE-TDLINE = MSG.
    MESSAGE S000 WITH  MSG.
  ELSE.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              MSGID               = SY-MSGID
              MSGNR               = SY-MSGNO
              MSGV1               = SY-MSGV1
              MSGV2               = SY-MSGV2
              MSGV3               = SY-MSGV3
              MSGV4               = SY-MSGV4
         IMPORTING
              MESSAGE_TEXT_OUTPUT = MSG.
    IF MODE = 'P'.
      W_MSG =  'Error in print DCR'.
    ELSE.
      W_MSG =  'Error in Sending ASN'.
    ENDIF.
    CONCATENATE IT_ITAB1-MSG W_MSG INTO IT_ITAB1-MSG
    SEPARATED BY SPACE.
    MODIFY IT_ITAB1 INDEX W_INDEX TRANSPORTING MSG.
    P_IT_TLINE-TDLINE = MSG.
    REFRESH BDCDATA.
    P_IT_TLINE-TDFORMAT = 'E' .
    P_IT_TLINE-TDLINE = MSG.
    MESSAGE I000 WITH 'Error:' MSG.
  ENDIF.

  APPEND P_IT_TLINE.
  CLEAR: P_IT_TLINE, MSG.
  REFRESH: MESSTAB.

ENDFORM.                    " BDC_TRANSACTION_1
*&---------------------------------------------------------------------*
*&      Form  SEND_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_OD USING P_ODNO P_MODE .
  REFRESH: BDCDATA, IT_TLINE.

  PERFORM BDC_DYNPRO      USING 'SD70AV2A' '1000'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RG_KSCHL-LOW'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=ONLI'.
  PERFORM BDC_FIELD       USING 'RG_KSCHL-LOW'
                                'ZE00'.
*perform bdc_field       using 'PM_NSORT'
*                              record-PM_NSORT_002.
  PERFORM BDC_FIELD       USING 'PM_VERMO'
                                P_MODE.
  PERFORM BDC_FIELD       USING 'RG_VBELN-LOW'
                                P_ODNO.

  PERFORM BDC_DYNPRO      USING 'SAPMSSY0' '0120'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                '04/03'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=&ALL'.

  PERFORM BDC_DYNPRO      USING 'SAPMSSY0' '0120'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                '04/03'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=PROC'.

  PERFORM BDC_DYNPRO      USING 'SAPLSTXBC' '0100'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'SSFPP-TDDEST'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=PRNT'.

  PERFORM BDC_DYNPRO      USING 'SAPLSTXBC' '0100'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'SSFPP-TDDEST'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=PRNT'.

  PERFORM BDC_TRANSACTION_1 TABLES IT_TLINE USING 'VL71' 'P'.

ENDFORM.                    " SEND_PRINT
*&---------------------------------------------------------------------*
*&      Module  CLEAR_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CLEAR_DATA OUTPUT.
  W_OLD_CODE = W_CODE.
  CLEAR: W_CODE.
ENDMODULE.                 " CLEAR_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  POST_STOCK_TRANSFER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_STOCK_TRANSFER.
  DATA: L_TRUCK LIKE IT_ITAB1-TRUCK_NO,
        L_JIT_CALL_NO LIKE IT_ITAB1-JIT_CALL_NO.

  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
          LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.


  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M05.
  ENDIF.
  READ TABLE IT_ITAB1 INDEX LT_ROWS-INDEX.

  IF SY-SUBRC = 0.
    W_INDEX = LT_ROWS-INDEX.
    IF IT_ITAB1-ODNO IS INITIAL.
      MESSAGE E000(ZZ) WITH TEXT-M06.
    ENDIF.
    W_ODNO = IT_ITAB1-ODNO.
    PERFORM PROCESS_MB1B USING IT_ITAB1-TRUCK_NO.
  ENDIF.
*  CLEAR: W_CODE.
ENDFORM.                    " POST_STOCK_TRANSFER
*&---------------------------------------------------------------------*
*&      Form  update_odno
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ODNO  text
*----------------------------------------------------------------------*
FORM UPDATE_ODNO USING P_TRUCK.
  LOOP AT IT_ITAB1 WHERE TRUCK_NO = P_TRUCK.
    IT_ITAB1-ODNO = W_ODNO.
    IT_ITAB1-MSG = W_MSG.
    MODIFY IT_ITAB1 TRANSPORTING ODNO MSG.
  ENDLOOP.
ENDFORM.                    " update_odno
*&---------------------------------------------------------------------*
*&      Form  PRINT_OD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_VL71 USING P_MODE.
  DATA: L_TRUCK LIKE IT_ITAB1-TRUCK_NO,
        L_JIT_CALL_NO LIKE IT_ITAB1-JIT_CALL_NO.

  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
          LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.


  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M05.
  ENDIF.
  READ TABLE IT_ITAB1 INDEX LT_ROWS-INDEX.

  IF SY-SUBRC = 0.
    W_INDEX = LT_ROWS-INDEX.
    IF IT_ITAB1-ODNO IS INITIAL.
      MESSAGE E000(ZZ) WITH TEXT-M06.
    ENDIF.
** clear message
    W_ODNO = IT_ITAB1-ODNO.
    L_TRUCK = IT_ITAB1-TRUCK_NO.
    LOOP AT IT_ITAB1 WHERE TRUCK_NO = L_TRUCK.
      CLEAR: IT_ITAB1-MSG.
      MODIFY IT_ITAB1.
    ENDLOOP.

    IF W_CODE = 'PRINT'.
*      IF P_MODE = '1'.
      PERFORM PRINT_LIST USING L_TRUCK.
*      ENDIF.
      PERFORM PRINT_OD USING W_ODNO P_MODE.
    ELSE.
      PERFORM CHECK_SERIAL_NUMBER USING L_TRUCK.
      PERFORM SEND_ASN  USING W_ODNO P_MODE.
    ENDIF.
  ENDIF.
*  CLEAR: W_CODE.
ENDFORM.                    " PRINT_OD
*&---------------------------------------------------------------------*
*&      Form  check_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_STATUS.
  IF W_OLD_CODE = 'OD' OR W_OLD_CODE = 'TRANSFER'.
    MESSAGE E000 WITH 'Cannot use "REFRESH", process not completed'.
  ENDIF.
ENDFORM.                    " check_status
*&---------------------------------------------------------------------*
*&      Form  assign_serial_truck_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_SERIAL_TRUCK_NO USING P_TRUCK.
  DATA: BEGIN OF LT_TEMP OCCURS 50,
        ASSYID(12),
        MATNR LIKE JITMA-MATNR,
        IMNO(2) TYPE N,
        END OF LT_TEMP.

  DATA: L_TEXT(20),
        L_LINE(2) TYPE N,
        L_LIST_CN(2) TYPE N,
        L_IMNO LIKE LIPS-POSNR,
        L_INDEX LIKE SY-TABIX,
        L_TOT_PAGE TYPE I,
        L_PAGE TYPE I,
        L_TOT_COUNT TYPE I,
        L_COUNT TYPE I,
        L_FLAG(1),
        L_TIME TYPE I,
        L_LOCK(1).

  DATA: BEGIN OF LT_VL02N OCCURS 50,
        ASSYID(12),
        END OF LT_VL02N.
  DATA: LT_18 LIKE TABLE OF LT_VL02N WITH HEADER LINE.

  FIELD-SYMBOLS <FS>.

  LOOP AT IT_ITAB1 WHERE TRUCK_NO = P_TRUCK.
    LT_TEMP-MATNR  =  IT_ITAB1-ITEM_NO.
    COLLECT LT_TEMP.
  ENDLOOP.

  LOOP AT LT_TEMP.
    SELECT SINGLE POSNR INTO L_IMNO
      FROM LIPS
      WHERE VBELN = W_ODNO
        AND MATNR = LT_TEMP-MATNR.

    CASE L_IMNO.
      WHEN '10'.
        LT_TEMP-IMNO = '01'.
      WHEN '20'.
        LT_TEMP-IMNO = '02'.
      WHEN '30'.
        LT_TEMP-IMNO = '03'.
      WHEN '40'.
        LT_TEMP-IMNO = '04'.

* by ig.moon 10/19/12 {
      WHEN '50'.
        LT_TEMP-IMNO = '05'.
      WHEN '60'.
        LT_TEMP-IMNO = '06'.
      WHEN '70'.
        LT_TEMP-IMNO = '07'.
      WHEN '80'.
        LT_TEMP-IMNO = '08'.
      WHEN '90'.
        LT_TEMP-IMNO = '09'.
*  }
    ENDCASE.
    MODIFY LT_TEMP.
  ENDLOOP.


  LOOP AT LT_TEMP.
    REFRESH: BDCDATA, LT_VL02N.
    CLEAR: L_TOT_COUNT.

    LOOP AT IT_ITAB1 WHERE TRUCK_NO = P_TRUCK
                        AND ITEM_NO = LT_TEMP-MATNR.
      L_TOT_COUNT = L_TOT_COUNT + 1.
      LT_VL02N-ASSYID = IT_ITAB1-ASSYID.
      APPEND LT_VL02N.
    ENDLOOP.

*    L_TOT_PAGE = L_TOT_COUNT DIV 19.

*    PERFORM BDC_DYNPRO      USING 'SAPMV50A' '1000'.
*    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                  '=PSER_T'.
*
*    CONCATENATE 'LIPS-POSNR(' LT_TEMP-IMNO ')' INTO L_TEXT.
*    ASSIGN (L_TEXT) TO <FS>.
*
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                   L_TEXT.
*
*    CONCATENATE 'RV50A-LIPS_SELKZ(' LT_TEMP-IMNO ')' INTO L_TEXT.
*    ASSIGN (L_TEXT) TO <FS>.
**    IF SY-SUBRC = 0.
**      PERFORM BDC_FIELD       USING 'RV50A-LIPS_SELKZ(01)'
**                                    RECORD-LIPS_SELKZ_01_007.
*    PERFORM BDC_FIELD       USING L_TEXT
*    'X'.
*
** Changed on 09/24/09

*    PERFORM BDC_DYNPRO      USING 'SAPLIPW1' '0200'.
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                  'RIPW0-SERNR(06)'.
*    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                  '=RWS'.
*    UNASSIGN <FS>.

*    L_LINE  = '01'.
*    LOOP AT IT_ITAB1 WHERE TRUCK_NO = P_TRUCK
*                       AND ITEM_NO = LT_TEMP-MATNR.
*      CONCATENATE 'RIPW0-SERNR(' L_LINE ')' INTO L_TEXT.
*      ASSIGN (L_TEXT) TO <FS>.
**        IF SY-SUBRC = 0.
*      PERFORM BDC_FIELD  USING L_TEXT
*                          IT_ITAB1-ASSYID.
*      L_LINE  = L_LINE + 1.

    L_PAGE = 1.

    IF L_TOT_COUNT <= 20.

      REFRESH: BDCDATA.


*      PERFORM BDC_DYNPRO      USING 'SAPMV50A' '4004'.
*      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                    'LIKP-VBELN'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                    '/00'.
*      PERFORM BDC_FIELD       USING 'LIKP-VBELN'
*                                    W_ODNO.
*
*      PERFORM BDC_DYNPRO      USING 'SAPMV50A' '1000'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                    '=PSER_T'.
*
*      CONCATENATE 'LIPS-POSNR(' LT_TEMP-IMNO ')' INTO L_TEXT.
*      ASSIGN (L_TEXT) TO <FS>.
*
*      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                     L_TEXT.
*
*      CONCATENATE 'RV50A-LIPS_SELKZ(' LT_TEMP-IMNO ')' INTO L_TEXT.
*      ASSIGN (L_TEXT) TO <FS>.
**    IF SY-SUBRC = 0.
**      PERFORM BDC_FIELD       USING 'RV50A-LIPS_SELKZ(01)'
**                                    RECORD-LIPS_SELKZ_01_007.
*      PERFORM BDC_FIELD       USING L_TEXT
*      'X'.
*
*      PERFORM BDC_DYNPRO      USING 'SAPLIPW1' '0200'.
*      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                             'RIPW0-SERNR(02)'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                             '=RWS'.
*
*      L_LINE = '01'.
*      LOOP AT IT_ITAB1 WHERE TRUCK_NO = P_TRUCK
*                         AND ITEM_NO = LT_TEMP-MATNR.
*        CONCATENATE 'RIPW0-SERNR(' L_LINE ')' INTO L_TEXT.
*        ASSIGN (L_TEXT) TO <FS>.
**        IF SY-SUBRC = 0.
*        PERFORM BDC_FIELD  USING L_TEXT
*                            IT_ITAB1-ASSYID.
*        L_LINE  = L_LINE + 1.
*      ENDLOOP.
*
*      PERFORM BDC_DYNPRO      USING 'SAPMV50A' '1000'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                    '=HDET_T'.
*       CONCATENATE 'RV50A-LIPS_SELKZ(' LT_TEMP-IMNO ')' INTO L_TEXT.
*      ASSIGN (L_TEXT) TO <FS>.
*      PERFORM BDC_FIELD       USING L_TEXT
*                                    'X'.
*
*      PERFORM BDC_DYNPRO      USING 'SAPMV50A' '2000'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                    '=T\07'.
*      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                    'LIKP-KODAT'.
*
*      PERFORM BDC_DYNPRO      USING 'SAPMV50A' '2000'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                    '/00'.
*      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                    'LIKP-LIFEX'.
**
*      PERFORM BDC_DYNPRO      USING 'SAPMV50A' '2000'.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                    '=SICH_T'.
*      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                    'LIKP-LIFEX'.
*      PERFORM BDC_FIELD       USING 'LIKP-LIFEX'
*                                     P_TRUCK.


      PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0101'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'LIKP-VBELN'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM BDC_FIELD       USING 'LIKP-VBELN'
                                      W_ODNO.

      PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0220'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'LIPS-POSNR(01)'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=KALL'.
*  PERFORM BDC_FIELD       USING 'RV50A-LIPS_SELKZ(01)'
*                                'X'.
      CONCATENATE 'RV50A-LIPS_SELKZ(' LT_TEMP-IMNO ')' INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS>.
      PERFORM BDC_FIELD       USING L_TEXT
      'X'.

      PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0300'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'LIKP-LIFEX'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM BDC_FIELD       USING 'LIKP-LIFEX'
                                    P_TRUCK.

      PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0300'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'LIKP-LIFEX'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=BACK'.

      PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0220'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'LIPS-MATNR(02)'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=PSER'.

**** 300
      PERFORM BDC_DYNPRO      USING 'SAPLIPW1' '0300'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'RIPW0-SERNR(03)'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=RWS'.

      L_LINE = '01'.
      LOOP AT IT_ITAB1 WHERE TRUCK_NO = P_TRUCK
                         AND ITEM_NO = LT_TEMP-MATNR.
        CONCATENATE 'RIPW0-SERNR(' L_LINE ')' INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS>.
*        IF SY-SUBRC = 0.
        PERFORM BDC_FIELD  USING L_TEXT
                            IT_ITAB1-ASSYID.
        L_LINE  = L_LINE + 1.
      ENDLOOP.

      PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0220'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'LIPS-MATNR(02)'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=SICH'.

      L_TIME = 10.
      WHILE L_TIME > 0.
        CLEAR: L_LOCK.
        PERFORM CHECK_LOCK USING L_LOCK.
        IF L_LOCK = 'X'.
          WAIT UP TO 10 SECONDS.
          L_TIME = L_TIME - 1.
        ELSE.
          EXIT.
        ENDIF.
      ENDWHILE.
      IF L_TIME <= 0.
        MESSAGE I000 WITH 'Serial No Update Error: Transaction Lock'.
      ELSE.
        PERFORM BDC_TRANSACTION TABLES IT_TLINE USING 'VL02' L_FLAG.
        IF L_FLAG = 'E'.
          READ TABLE IT_ITAB1 WITH KEY TRUCK_NO = P_TRUCK.
          IF SY-SUBRC = 0.
            L_INDEX = SY-TABIX.
            IT_ITAB1-MSG = W_MSG.
            MODIFY IT_ITAB1 INDEX L_INDEX.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.

      L_PAGE  = 1.
      L_COUNT = 1.

      LOOP AT LT_VL02N.

        IF L_COUNT <= 20.
          L_COUNT = L_COUNT + 1.
          LT_18 = LT_VL02N.
          APPEND LT_18.
        ELSE.

          REFRESH: BDCDATA.

*          PERFORM BDC_DYNPRO      USING 'SAPMV50A' '4004'.
*          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                        'LIKP-VBELN'.
*          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                        '/00'.
*          PERFORM BDC_FIELD       USING 'LIKP-VBELN'
*                                        W_ODNO.
*
*          PERFORM BDC_DYNPRO      USING 'SAPMV50A' '1000'.
*          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                        '=PSER_T'.
*
*
*          CONCATENATE 'RV50A-LIPS_SELKZ(' LT_TEMP-IMNO ')' INTO L_TEXT.
*          ASSIGN (L_TEXT) TO <FS>.
*          PERFORM BDC_FIELD       USING L_TEXT
*          'X'.

          PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0101'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'LIKP-VBELN'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '/00'.
          PERFORM BDC_FIELD       USING 'LIKP-VBELN'
                                          W_ODNO.

          PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0220'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'LIPS-POSNR(01)'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=KALL'.

          CONCATENATE 'RV50A-LIPS_SELKZ(' LT_TEMP-IMNO ')' INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FS>.
          PERFORM BDC_FIELD       USING L_TEXT
          'X'.

          PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0300'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'LIKP-LIFEX'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '/00'.
          PERFORM BDC_FIELD       USING 'LIKP-LIFEX'
                                        P_TRUCK.

          PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0300'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'LIKP-LIFEX'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=BACK'.

          PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0220'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'LIPS-MATNR(02)'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=PSER'.

          DO L_PAGE TIMES.
*            PERFORM BDC_DYNPRO      USING 'SAPLIPW1' '0200'.
*            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                      'RIPW0-SERNR(02)'.
*            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                     '=PNPG'.

** 300
            PERFORM BDC_DYNPRO      USING 'SAPLIPW1' '0300'.
            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                          'RIPW0-SERNR(02)'.
            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                          '=PNPG'.
          ENDDO.
*          PERFORM BDC_DYNPRO      USING 'SAPLIPW1' '0200'.
*          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                 'RIPW0-SERNR(02)'.
*          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                 '=RWS'.

**** 300
          PERFORM BDC_DYNPRO      USING 'SAPLIPW1' '0300'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'RIPW0-SERNR(03)'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=RWS'.

*          IF L_PAGE = 1.
*            L_LINE = '01'.
*          ELSE.
*            L_LINE = '02'.
*          ENDIF.
*
          L_LINE = '01'.

          LOOP AT LT_18.  " WHERE TRUCK_NO = P_TRUCK
            "   AND ITEM_NO = LT_TEMP-MATNR.
            CONCATENATE 'RIPW0-SERNR(' L_LINE ')' INTO L_TEXT.
            ASSIGN (L_TEXT) TO <FS>.
*        IF SY-SUBRC = 0.
            PERFORM BDC_FIELD  USING L_TEXT
                                LT_18-ASSYID.
            L_LINE  = L_LINE + 1.
          ENDLOOP.

*          PERFORM BDC_DYNPRO      USING 'SAPMV50A' '1000'.
*          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                        '=HDET_T'.
*          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                        'LIPS-MATNR(02)'.
*
*          PERFORM BDC_DYNPRO      USING 'SAPMV50A' '2000'.
*          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                        '=SICH_T'.
*          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                        'LIKP-LIFEX'.
*          PERFORM BDC_FIELD       USING 'LIKP-LIFEX'
*                                              P_TRUCK.
          PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0220'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'LIPS-MATNR(02)'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=SICH'.
          L_TIME = 10.
          WHILE L_TIME > 0.
            CLEAR: L_LOCK.
            PERFORM CHECK_LOCK USING L_LOCK.
            IF L_LOCK = 'X'.
              WAIT UP TO 10 SECONDS.
              L_TIME = L_TIME - 1.
            ELSE.
              EXIT.
            ENDIF.
          ENDWHILE.
          IF L_TIME <= 0.
           MESSAGE I000 WITH 'Serial No Update Error: Transaction Lock'.
          ELSE.
            PERFORM BDC_TRANSACTION TABLES IT_TLINE USING 'VL02' L_FLAG.
*          WAIT UP TO 10 SECONDS.
            IF L_FLAG = 'E'.
              READ TABLE IT_ITAB1 WITH KEY TRUCK_NO = P_TRUCK.
              IF SY-SUBRC = 0.
                L_INDEX = SY-TABIX.
                IT_ITAB1-MSG = W_MSG.
                MODIFY IT_ITAB1 INDEX L_INDEX.
              ENDIF.
            ENDIF.
          ENDIF.
          REFRESH LT_18.

          L_COUNT = L_COUNT + 1.
          LT_18 = LT_VL02N.
          APPEND LT_18.
          L_COUNT = 2.
          L_PAGE = L_PAGE + 1.
        ENDIF.
      ENDLOOP.

      IF NOT LT_18[] IS INITIAL.

        REFRESH: BDCDATA.

        PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0101'.
        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                      'LIKP-VBELN'.
        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      '/00'.
        PERFORM BDC_FIELD       USING 'LIKP-VBELN'
                                        W_ODNO.

        PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0220'.
        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                      'LIPS-POSNR(01)'.
        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      '=KALL'.

        CONCATENATE 'RV50A-LIPS_SELKZ(' LT_TEMP-IMNO ')' INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS>.
        PERFORM BDC_FIELD       USING L_TEXT
        'X'.

        PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0300'.
        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                      'LIKP-LIFEX'.
        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      '/00'.
        PERFORM BDC_FIELD       USING 'LIKP-LIFEX'
                                      P_TRUCK.

        PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0300'.
        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                      'LIKP-LIFEX'.
        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      '=BACK'.

        PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0220'.
        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                      'LIPS-MATNR(02)'.
        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      '=PSER'.

        DO L_PAGE TIMES.
*            PERFORM BDC_DYNPRO      USING 'SAPLIPW1' '0200'.
*            PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                      'RIPW0-SERNR(02)'.
*            PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                     '=PNPG'.

** 300
          PERFORM BDC_DYNPRO      USING 'SAPLIPW1' '0300'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'RIPW0-SERNR(02)'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=PNPG'.
        ENDDO.
*          PERFORM BDC_DYNPRO      USING 'SAPLIPW1' '0200'.
*          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                 'RIPW0-SERNR(02)'.
*          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                 '=RWS'.

** 300
        PERFORM BDC_DYNPRO      USING 'SAPLIPW1' '0300'.
        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                      'RIPW0-SERNR(03)'.
        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      '=RWS'.
*
*        IF L_PAGE = 1.
*          L_LINE = '01'.
*        ELSE.
*          L_LINE = '02'.
*        ENDIF.
         L_LINE = '01'.

        LOOP AT LT_18.  " WHERE TRUCK_NO = P_TRUCK
          "   AND ITEM_NO = LT_TEMP-MATNR.
          CONCATENATE 'RIPW0-SERNR(' L_LINE ')' INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FS>.
*        IF SY-SUBRC = 0.
          PERFORM BDC_FIELD  USING L_TEXT
                              LT_18-ASSYID.
          L_LINE  = L_LINE + 1.
        ENDLOOP.

*          PERFORM BDC_DYNPRO      USING 'SAPMV50A' '1000'.
*          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                        '=HDET_T'.
*          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                        'LIPS-MATNR(02)'.
*
*          PERFORM BDC_DYNPRO      USING 'SAPMV50A' '2000'.
*          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                        '=SICH_T'.
*          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                        'LIKP-LIFEX'.
*          PERFORM BDC_FIELD       USING 'LIKP-LIFEX'
*                                              P_TRUCK.
        PERFORM BDC_DYNPRO      USING 'SAPMV50A' '0220'.
        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                      'LIPS-MATNR(02)'.
        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      '=SICH'.

        L_TIME = 10.
        WHILE L_TIME > 0.
          CLEAR: L_LOCK.
          PERFORM CHECK_LOCK USING L_LOCK.
          IF L_LOCK = 'X'.
            WAIT UP TO 10 SECONDS.
            L_TIME = L_TIME - 1.
          ELSE.
            EXIT.
          ENDIF.
        ENDWHILE.
        IF L_TIME <= 0.
          MESSAGE I000 WITH 'Serial No Update Error: Transaction Lock'.
        ELSE.
          PERFORM BDC_TRANSACTION TABLES IT_TLINE USING 'VL02' L_FLAG.
*          WAIT UP TO 10 SECONDS.
          IF L_FLAG = 'E'.
            READ TABLE IT_ITAB1 WITH KEY TRUCK_NO = P_TRUCK.
            IF SY-SUBRC = 0.
              L_INDEX = SY-TABIX.
              IT_ITAB1-MSG = W_MSG.
              MODIFY IT_ITAB1 INDEX L_INDEX.
            ENDIF.
          ENDIF.
        ENDIF.
*        PERFORM BDC_DYNPRO      USING 'SAPMV50A' '4004'.
*        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                      'LIKP-VBELN'.
*        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                      '/00'.
*        PERFORM BDC_FIELD       USING 'LIKP-VBELN'
*                                      W_ODNO.
*
*        PERFORM BDC_DYNPRO      USING 'SAPMV50A' '1000'.
*        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                      '=PSER_T'.
*
*        CONCATENATE 'LIPS-POSNR(' LT_TEMP-IMNO ')' INTO L_TEXT.
*        ASSIGN (L_TEXT) TO <FS>.
*
*        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                       L_TEXT.
*
*        CONCATENATE 'RV50A-LIPS_SELKZ(' LT_TEMP-IMNO ')' INTO L_TEXT.
*        ASSIGN (L_TEXT) TO <FS>.
*        PERFORM BDC_FIELD       USING L_TEXT
*        'X'.
*
*        DO L_PAGE TIMES.
*          PERFORM BDC_DYNPRO      USING 'SAPLIPW1' '0200'.
*          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                    'RIPW0-SERNR(02)'.
*          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                   '=PNPG'.
*        ENDDO.
*        PERFORM BDC_DYNPRO      USING 'SAPLIPW1' '0200'.
*        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                               'RIPW0-SERNR(02)'.
*        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                               '=RWS'.
*
*        L_LINE = '02'.
*        LOOP AT LT_18.  " WHERE TRUCK_NO = P_TRUCK
*          "   AND ITEM_NO = LT_TEMP-MATNR.
*          CONCATENATE 'RIPW0-SERNR(' L_LINE ')' INTO L_TEXT.
*          ASSIGN (L_TEXT) TO <FS>.
**        IF SY-SUBRC = 0.
*          PERFORM BDC_FIELD  USING L_TEXT
*                              LT_18-ASSYID.
*          L_LINE  = L_LINE + 1.
*        ENDLOOP.
*
*        PERFORM BDC_DYNPRO      USING 'SAPMV50A' '1000'.
*        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                      '=HDET_T'.
*        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                      'LIPS-MATNR(02)'.
*
*        PERFORM BDC_DYNPRO      USING 'SAPMV50A' '2000'.
*        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                      '=SICH_T'.
*        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                      'LIKP-LIFEX'.
*        PERFORM BDC_FIELD       USING 'LIKP-LIFEX'
*                                            P_TRUCK.
*
*
*
*        PERFORM BDC_TRANSACTION TABLES IT_TLINE USING 'VL02N' L_FLAG.
*        IF L_FLAG = 'E'.
*          READ TABLE IT_ITAB1 WITH KEY TRUCK_NO = P_TRUCK.
*          IF SY-SUBRC = 0.
*            L_INDEX = SY-TABIX.
*            IT_ITAB1-MSG = W_MSG.
*            MODIFY IT_ITAB1 INDEX L_INDEX.
*          ENDIF.
*        ENDIF.


      ENDIF.

      REFRESH LT_18.
      L_PAGE = L_PAGE + 1.
*    ENDIF.
    ENDIF.

  ENDLOOP.




ENDFORM.                    " assign_serial_truck_no
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  PERFORM SET_EXCLUDE_TAB.
  SET PF-STATUS 'ST300' EXCLUDING IT_TAB.
  SET TITLEBAR 'ST300'.
ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.
  W_CODE = OK_CODE.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SEARCH'.    "'DISP'.
      PERFORM GET_DATA_300.
*    WHEN 'OD' OR 'OD&TR'.
*      PERFORM POST_ALL.
    WHEN 'REVE'.
      PERFORM POST_REVERSAL.
    WHEN 'PRINT' OR 'SEND'.
      PERFORM CALL_VL71 USING '2'.
    WHEN 'LIST'.
      PERFORM PRINT_LIST_300.
*    WHEN 'REFRESH'.
*      PERFORM REFRESH_DATA.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  init_data  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INIT_DATA OUTPUT.
  IF IT_ITAB1[] IS INITIAL.
    PERFORM GET_DATA.
  ENDIF.
  IF IT_ITAB1[] IS INITIAL.
    MESSAGE S009 WITH TEXT-M01.
  ENDIF.
ENDMODULE.                 " init_data  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  clear_data_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_DATA_300.
  REFRESH: IT_ITAB1, IT_VMASTER, BDCDATA, MESSTAB, IT_TLINE,
           IT_FIELDCAT, IT_SORT, IT_FIELDNAME.
  CLEAR: WA_ITAB1, IT_VMASTER, W_ODNO, W_MSG, W_INDEX, W_FIELDNAME,
         WA_VARIANT, OK_CODE, W_REPID,W_CNT.
ENDFORM.                    " clear_data_300
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_300.
  DATA: BEGIN OF LT_DATA OCCURS 0,
         OBJEK LIKE AUSP-OBJEK,
*        POSID LIKE JITIT-POSID,
         END OF LT_DATA.

  DATA: BEGIN OF LT_PRODN OCCURS 0,
        PRODN LIKE JITHD-PRODN,
        END OF LT_PRODN.

  DATA: L_OD_NO LIKE AUSP-ATWRT,
        L_ATINN LIKE CABN-ATINN,
        L_VALS(8) TYPE N,
*        L_ATFLV_FR LIKE AUSP-ATFLV,
*        L_ATFLV_TO LIKE AUSP-ATFLV,
        L_DATE LIKE SY-DATUM,
        L_NEW(1),
        L_SEQ_NO TYPE I,
        L_ATFLV LIKE AUSP-ATFLV,
        L_RECNO TYPE I,
        L_ITEM_NO LIKE IT_ITAB1-ITEM_NO,
        L_TRUCK_NO  LIKE IT_ITAB1-TRUCK_NO,
        L_RACK LIKE IT_ITAB1-RACK_NO,
        L_JIT_CALL_NO LIKE LT_PRODN-PRODN,
        L_VBELN LIKE LIKP-VBELN,
        L_TRUCK(12).

  RANGES: S_DATE FOR AUSP-ATFLV,
          S_TRUCK FOR AUSP-ATWRT,
          S_VBLEN FOR AUSP-ATWRT.

  REFRESH IT_ITAB1.
  CLEAR: IT_ITAB1.


  IF W_DATE_TO < W_DATE_FR AND NOT W_DATE_TO IS INITIAL.
    EXIT.
  ENDIF.

  IF W_DATE_FR IS INITIAL.
    IF W_DATE_TO IS INITIAL.
    ELSE.
      S_DATE-OPTION = 'BT'.
      S_DATE-SIGN = 'I'.
      S_DATE-HIGH  = L_VALS  = W_DATE_TO.
      APPEND S_DATE.
    ENDIF.
  ELSE.
    IF W_DATE_TO IS INITIAL.
      S_DATE-OPTION = 'EQ'.
      S_DATE-SIGN = 'I'.
      S_DATE-LOW  = L_VALS  = W_DATE_FR.
      APPEND S_DATE.
    ELSE.
      S_DATE-OPTION = 'BT'.
      S_DATE-SIGN = 'I'.
      S_DATE-LOW  = L_VALS  = W_DATE_FR.
      S_DATE-HIGH  = L_VALS  = W_DATE_TO.
      APPEND S_DATE.
    ENDIF.
  ENDIF.

  IF W_TRUCK_FR IS INITIAL.
    IF W_TRUCK_TO IS INITIAL.
    ELSE.
      S_TRUCK-OPTION = 'BT'.
      S_TRUCK-SIGN = 'I'.
      S_TRUCK-HIGH  = W_TRUCK_TO.
      APPEND S_TRUCK.
    ENDIF.
  ELSE.
    IF W_TRUCK_TO IS INITIAL.
      S_TRUCK-OPTION = 'EQ'.
      S_TRUCK-SIGN = 'I'.
      S_TRUCK-LOW  = W_TRUCK_FR.
      APPEND S_TRUCK.
    ELSE.
      S_TRUCK-OPTION = 'BT'.
      S_TRUCK-SIGN = 'I'.
      S_TRUCK-LOW  = W_TRUCK_FR.
      S_TRUCK-HIGH  = W_TRUCK_TO.
      APPEND S_TRUCK.
    ENDIF.
  ENDIF.

*  IF W_TRUCK_FR IS INITIAL.
*    IF W_TRUCK_TO IS INITIAL.
*      W_TRUCK_TO = 'ZZZZ'.
*    ENDIF.
*  ELSE.
*    IF W_TRUCK_TO IS INITIAL.
*      W_TRUCK_TO = W_TRUCK_FR.
*    ENDIF.
*  ENDIF.
*
*  IF W_TRUCK_TO < W_TRUCK_FR.
*    EXIT.
*  ENDIF.

  IF W_VBELN_300 IS INITIAL.
    IF W_VBELN_300_TO IS INITIAL.
      W_VBELN_300_TO = 'ZZZZZZZZZZ'.
    ENDIF.
  ELSE.
    IF W_VBELN_300_TO IS INITIAL.
      W_VBELN_300_TO = W_VBELN_300.
    ENDIF.
  ENDIF.

  SELECT SINGLE ATINN INTO L_ATINN
     FROM CABN
    WHERE ATNAM = 'EN_TRUCK_DATE' .

  CLEAR: L_DATE.

  SELECT OBJEK INTO CORRESPONDING FIELDS OF TABLE LT_DATA
   FROM AUSP
  WHERE KLART = '002'
    AND ATINN = L_ATINN
*     AND ATFLV BETWEEN L_ATFLV_FR AND  L_ATFLV_TO
    AND ATFLV IN S_DATE
    AND LKENZ = ' ' .


*  SELECT SINGLE ATINN INTO L_ATINN
*      FROM CABN
*     WHERE ATNAM = 'EN_TRUCK_NO' .
*
*  SELECT OBJEK INTO CORRESPONDING FIELDS OF TABLE LT_DATA
*    FROM AUSP
*   WHERE KLART = '002'
*     AND ATINN = L_ATINN
*     AND ATWRT BETWEEN W_TRUCK_FR AND W_TRUCK_TO
*     AND LKENZ = ' ' .

  IF NOT W_VBELN_300 IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              INPUT  = W_VBELN_300
         IMPORTING
              OUTPUT = W_VBELN_300.

  ENDIF.
  IF NOT W_VBELN_300_TO IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              INPUT  = W_VBELN_300_TO
         IMPORTING
              OUTPUT = W_VBELN_300_TO.

  ENDIF.

  IF W_VBELN_300_TO < W_VBELN_300.
    EXIT.
  ENDIF.

  LOOP AT LT_DATA.
*    CLEAR: L_OD_NO.
*    SELECT SINGLE ATWRT INTO L_OD_NO
*      FROM AUSP AS A
*      INNER JOIN CABN AS B
*      ON A~ATINN = B~ATINN
*      WHERE KLART = '002'
*        AND OBJEK = LT_DATA-OBJEK
*        AND ATNAM = 'EN_OUTBOUND_NO'.
*
*    L_VBELN = L_OD_NO.
*    IF L_VBELN >= W_VBELN_300 AND L_VBELN <= W_VBELN_300_TO.

    CLEAR: L_TRUCK.
    SELECT SINGLE ATWRT INTO L_TRUCK
      FROM AUSP AS A
      INNER JOIN CABN AS B
      ON A~ATINN = B~ATINN
      WHERE KLART = '002'
        AND OBJEK = LT_DATA-OBJEK
        AND ATNAM = 'EN_TRUCK_NO'.

*    IF L_TRUCK >= W_TRUCK_FR AND L_TRUCK <= W_TRUCK_TO.
    IF L_TRUCK IN S_TRUCK.
      IT_ITAB1-ODNO = L_OD_NO.
      IT_ITAB1-TRUCK_NO = L_TRUCK.

*      PERFORM READ_NORMAL_CLASS USING LT_DATA-OBJEK 'EN_TRUCK_NO'
*                               CHANGING IT_ITAB1-TRUCK_NO.

      PERFORM READ_NORMAL_CLASS USING LT_DATA-OBJEK 'EN_OUTBOUND_NO'
                              CHANGING IT_ITAB1-ODNO.

      PERFORM READ_NORMAL_CLASS USING LT_DATA-OBJEK 'EN_RACK_NO'
                              CHANGING IT_ITAB1-RACK_NO.

      PERFORM READ_NORMAL_CLASS USING LT_DATA-OBJEK 'EN_ITEM_CODE'
                                CHANGING IT_ITAB1-ITEM_NO.

      PERFORM READ_NORMAL_CLASS USING LT_DATA-OBJEK 'EN_TRANSFER_DOC'
                                CHANGING IT_ITAB1-STOCK_DOC.

      PERFORM READ_NORMAL_CLASS USING LT_DATA-OBJEK 'EN_JITCALL_NO'
                                CHANGING IT_ITAB1-JIT_CALL_NO.

      IT_ITAB1-ASSYID = LT_DATA-OBJEK.

      SELECT SINGLE AU~ATFLV INTO L_ATFLV
      FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
      WHERE OBJEK = LT_DATA-OBJEK
        AND KLART = '002'
        AND CA~ATNAM = 'EN_TRUCK_DATE'.

      L_VALS = L_ATFLV.
      IT_ITAB1-TRUCK_DATE = L_VALS.
      IT_ITAB1-QTY_NO = 1.

      APPEND IT_ITAB1.
      CLEAR: IT_ITAB1.
    ENDIF.
  ENDLOOP.

  SORT IT_ITAB1 BY TRUCK_NO RACK_NO ITEM_NO ASSYID .
  LOOP AT IT_ITAB1.
    AT NEW TRUCK_NO.
      L_NEW = 'X'.
    ENDAT.
    IF L_NEW IS INITIAL.
      CLEAR: IT_ITAB1-FIRST.
    ELSE.
      CLEAR: L_NEW.
      IT_ITAB1-FIRST = 'X'.
    ENDIF.
    MODIFY IT_ITAB1.
  ENDLOOP.

  L_SEQ_NO = 1.
  L_TRUCK_NO = '*'.
  LOOP AT IT_ITAB1.
    IF IT_ITAB1-TRUCK_NO <> L_TRUCK_NO.
      L_SEQ_NO = 1.
      L_TRUCK_NO = IT_ITAB1-TRUCK_NO.
    ENDIF.
    IT_ITAB1-SEQ_NO = L_SEQ_NO.
    L_SEQ_NO =  L_SEQ_NO + 1.
    MODIFY IT_ITAB1.
  ENDLOOP.

ENDFORM.                    " GET_DATA_300
*&---------------------------------------------------------------------*
*&      Form  POST_REVERSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_REVERSAL.
  DATA: L_TRUCK LIKE IT_ITAB1-TRUCK_NO,
        L_OBJECT LIKE MARA-MATNR,
        L_JIT_CALL_NO LIKE IT_ITAB1-JIT_CALL_NO,
        L_STOCK_DOC LIKE IT_ITAB1-STOCK_DOC.

  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
          LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows

  DATA: L_LINE TYPE I,
        L_FLAG(1).


  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M05.
  ENDIF.
  READ TABLE IT_ITAB1 INDEX LT_ROWS-INDEX.

  IF SY-SUBRC = 0.
    W_INDEX = LT_ROWS-INDEX.
    IF IT_ITAB1-ODNO IS INITIAL.
      MESSAGE E000(ZZ) WITH TEXT-M06.
    ENDIF.
    W_ODNO = IT_ITAB1-ODNO.
    L_STOCK_DOC = IT_ITAB1-STOCK_DOC.

** clear message
    L_TRUCK = IT_ITAB1-TRUCK_NO.
*    LOOP AT IT_ITAB1 WHERE TRUCK_NO = L_TRUCK.
*      CLEAR: IT_ITAB1-MSG.
*      MODIFY IT_ITAB1.
*    ENDLOOP.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              INPUT  = W_ODNO
         IMPORTING
              OUTPUT = W_ODNO.

    PERFORM BDC_DYNPRO      USING 'SAPMV50A' '4004'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'LIKP-VBELN'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'LIKP-VBELN'
                                  W_ODNO.
    PERFORM BDC_DYNPRO      USING 'SAPMV50A' '1000'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/ELOES_T'.

    PERFORM BDC_TRANSACTION TABLES IT_TLINE USING 'VL02N' L_FLAG.

    IF L_FLAG = 'E'.
      READ TABLE IT_ITAB1 INDEX W_INDEX.
      IF SY-SUBRC = 0.
        IT_ITAB1-MSG = W_MSG.
        MODIFY IT_ITAB1 INDEX W_INDEX.
      ENDIF.
    ELSE.

      PERFORM BDC_DYNPRO      USING 'SAPMM07M' '0460'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'XFULL'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '/00'.

      PERFORM BDC_FIELD       USING 'RM07M-MBLNR'
                                    L_STOCK_DOC.

      PERFORM BDC_FIELD       USING 'XFULL'
                                     ' '.
      PERFORM BDC_FIELD       USING 'RM07M-WVERS2'
                                    'X'.

      PERFORM BDC_DYNPRO      USING 'SAPMM07M' '0421'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'RM07M-XSELK(01)'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=BU'.

      CLEAR: L_FLAG.
      PERFORM BDC_TRANSACTION TABLES IT_TLINE USING 'MBST' L_FLAG.

      IF L_FLAG = 'E'.
        READ TABLE IT_ITAB1 INDEX W_INDEX.
        IF SY-SUBRC = 0.
          IT_ITAB1-MSG = W_MSG.
          MODIFY IT_ITAB1 INDEX W_INDEX.
        ENDIF.
      ENDIF.

      CLEAR: IT_VMASTER,
             IT_VMASTER[].

      PERFORM APPEND_VMASTER USING 'EN_OUTBOUND_NO'  ' '.
      PERFORM APPEND_VMASTER USING 'EN_JITCALL_NO'  ' '.
      PERFORM APPEND_VMASTER USING 'EN_OUTBOUND_DATE' ' '.
      PERFORM APPEND_VMASTER USING 'EN_OUTBOUND_TIME' ' '.
      PERFORM APPEND_VMASTER USING 'EN_TRANSFER_DOC'  ' '.

      LOOP AT IT_ITAB1 WHERE TRUCK_NO = L_TRUCK.

        L_OBJECT =  IT_ITAB1-ASSYID.
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
             EXPORTING
                  OBJECT     = L_OBJECT
                  MODE       = 'W'
                  CMODE      = '002'
             TABLES
                  VAL_TABLE  = IT_VMASTER
             EXCEPTIONS
                  NO_DATA    = 1
                  ERROR_MODE = 2
                  OTHERS     = 3.

        IF SY-SUBRC = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ELSE.
          REFRESH BDCDATA.
          W_MSG =  'Error in updating engine master'.
          CONCATENATE IT_ITAB1-MSG W_MSG INTO IT_ITAB1-MSG
          SEPARATED BY SPACE.
          IT_TLINE-TDFORMAT = 'E' .
          IT_TLINE-TDLINE = W_MSG.
          APPEND IT_TLINE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " POST_REVERSAL
*&---------------------------------------------------------------------*
*&      Form  set_exclude_tab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_EXCLUDE_TAB.
  CLEAR IT_TAB.
  IT_TAB-FCODE = 'OD&TR'.
  APPEND IT_TAB.
ENDFORM.                    " set_exclude_tab
*&---------------------------------------------------------------------*
*&      Form  print_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB1_TRUCK_NO  text
*----------------------------------------------------------------------*
FORM PRINT_LIST USING P_TRUCK.
  NEW-PAGE PRINT ON NO DIALOG .
  DATA: S_TST  TYPE TIMESTAMP.

  SET COUNTRY 'US'.

  GET TIME STAMP FIELD S_TST.

  WRITE : /(40) 'Engine Packing List'.
  WRITE : /(5) 'Date:',
          (20)  S_TST TIME ZONE 'UTC+12'.
  SKIP.
  WRITE : / SY-ULINE.
  WRITE : /(5) 'S/No',
           (7) 'Truck',
           (10) 'Tr Date',
           (7) 'Rack',
           (8) 'Item No',
           (13) 'ASSY ID',
           (11) 'JIT Call No',
           (10) 'O/D Number'.
*           (90) 'Stock Transfer #'.

  WRITE : / SY-ULINE.

  LOOP AT IT_ITAB1 WHERE TRUCK_NO = P_TRUCK.
    WRITE : /(5) IT_ITAB1-SEQ_NO,
              (7) IT_ITAB1-TRUCK_NO,
              (10) IT_ITAB1-TRUCK_DATE,
              (7) IT_ITAB1-RACK_NO,
              (8) IT_ITAB1-ITEM_NO,
              (13) IT_ITAB1-ASSYID,
              (11) IT_ITAB1-JIT_CALL_NO,
              (10) IT_ITAB1-ODNO.
    WRITE : / SY-ULINE.
  ENDLOOP.
  MESSAGE S009 WITH 'Printing Sucessfully Finished'.
  NEW-PAGE PRINT OFF.
ENDFORM.                    " print_list
*&---------------------------------------------------------------------*
*&      Form  PRINT_LIST_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_LIST_300.
  DATA: L_TRUCK LIKE IT_ITAB1-TRUCK_NO.

  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
          LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows

  DATA: L_LINE TYPE I,
        L_FLAG(1).

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error Found During Flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M05.
  ENDIF.
  READ TABLE IT_ITAB1 INDEX LT_ROWS-INDEX.

  IF SY-SUBRC = 0.
    W_INDEX = LT_ROWS-INDEX.
    IF IT_ITAB1-ODNO IS INITIAL.
      MESSAGE E000(ZZ) WITH TEXT-M06.
    ENDIF.

    L_TRUCK = IT_ITAB1-TRUCK_NO.
    PERFORM PRINT_LIST USING L_TRUCK.
  ENDIF.
ENDFORM.                    " PRINT_LIST_300
*&---------------------------------------------------------------------*
*&      Form  CHECK_SERIAL_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TRUCK  text
*----------------------------------------------------------------------*
FORM CHECK_SERIAL_NUMBER USING P_TRUCK.
  DATA: L_EQUNR LIKE EQUI_EQUNR.
  LOOP AT IT_ITAB1 WHERE TRUCK_NO = P_TRUCK.
    SELECT SINGLE EQUNR INTO L_EQUNR
      FROM EQUI
      WHERE EQTYP = 'S'
        AND MATNR = IT_ITAB1-ITEM_NO
        AND SERNR = IT_ITAB1-ASSYID.
    IF SY-SUBRC = 0.
    ELSE.
      MESSAGE E000 WITH TEXT-M07.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHECK_SERIAL_NUMBER
*&---------------------------------------------------------------------*
*&      Form  CHECK_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_LOCK  text
*----------------------------------------------------------------------*
FORM CHECK_LOCK USING P_LOCK.

  DATA: LT_ENQ LIKE TABLE OF SEQG3 WITH HEADER LINE.

  CALL FUNCTION 'ENQUE_READ'
   EXPORTING
     GCLIENT       = SY-MANDT
     GNAME         = ' '
     GARG          = ' '
     GUNAME        = SY-UNAME
* IMPORTING
*   NUMBER        =
*   SUBRC         =
    TABLES
      ENQ           = LT_ENQ   .

  READ TABLE LT_ENQ WITH KEY GNAME = 'LIKP'
                             GTCODE = 'VL02'.
  IF SY-SUBRC = 0.
    IF LT_ENQ-GTARG CS W_ODNO.
      P_LOCK = 'X'.
    ELSE.
      CLEAR: P_LOCK.
    ENDIF.
  ELSE.
    CLEAR: P_LOCK.
  ENDIF.

ENDFORM.                    " CHECK_LOCK
