************************************************************************
* Program Name      : ZAPP888_RESB_PORDER_VIN_NAFTA
* Creation Date     : 01/15/08
* Developer         : Furong Wang
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZAPP888_RESB_PORDER_VIN_NAFTA NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.
TABLES: ZTPP_RESB_B06.
TYPE-POOLS: SLIS, VRM.

DATA: IT_TAB LIKE TABLE OF ZTPP_RESB_B06 WITH HEADER LINE.

DATA: OK_CODE      LIKE SY-UCOMM,
      W_REPID  LIKE SY-REPID,
      W_CNT       TYPE   I,
      W_NO_DATA(1),
      W_FLAG(1).
DATA: W_PRGRP LIKE PGMI-PRGRP.

DATA: XNAME    TYPE VRM_ID,
      XLIST    TYPE VRM_VALUES,
      XVALUE   LIKE LINE OF XLIST.

DATA:  L_KALID LIKE KAKO-KALID.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_NATN(3) TYPE C OBLIGATORY.
PARAMETERS: P_DATE LIKE SY-DATUM OBLIGATORY.
PARAMETERS: P_RP(2) TYPE C AS LISTBOX VISIBLE LENGTH 25 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.

*at selection-screen on value-request for p_prdt.
AT SELECTION-SCREEN OUTPUT.
  PERFORM MAKE_DROPDOWN_LIST_BOX.

INITIALIZATION.
  PERFORM SET_INIT_DATA.

START-OF-SELECTION.

  PERFORM READ_DATA.

  IF IT_TAB[] IS INITIAL.
    MESSAGE I001 WITH TEXT-004.
    EXIT.
  ENDIF.
  PERFORM SAVE_DATA.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM get_req_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM READ_DATA.
  DATA: L_ATNAM    TYPE CABN-ATNAM,
        L_ATNAM1   TYPE CABN-ATNAM,
        L_ATWRT    TYPE AUSP-ATWRT,
        L_ATFLV    TYPE AUSP-ATFLV,
        L_TEMP(06),
        L_DATUM    TYPE SY-DATUM,
        L_ATFLV_EN TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N.

  DATA: BEGIN OF LT_AUSP OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
        ATNAM LIKE CABN-ATNAM,
        ATINN LIKE AUSP-ATINN,
        ATWRT LIKE AUSP-ATWRT,
        ATFLV LIKE AUSP-ATFLV,
        END OF LT_AUSP.

  DATA: BEGIN OF LT_OBJEK OCCURS 0,
          OBJEK   LIKE AUSP-OBJEK,
        END OF LT_OBJEK.

  DATA: IT_OBJEK LIKE TABLE OF LT_OBJEK WITH HEADER LINE.
  DATA: W_PLNUM LIKE PLAF-PLNUM,
        W_MODYR TYPE C,
        W_DEST(5) TYPE C,
        W_MI(9) TYPE C,
        W_OCN(4) TYPE C,
        W_FSC LIKE MARA-MATNR,
        L_LEN TYPE I,
        L_DEALER_OLD(2),
        L_DEALER_NEW(1).

  DATA: L_MI TYPE CABN-ATNAM,
        L_MODEL_YEAR TYPE CABN-ATNAM,
        L_OCN TYPE CABN-ATNAM,
        L_PLAN_ORDER TYPE CABN-ATNAM,
        L_VIN TYPE CABN-ATNAM,
        L_DEST TYPE CABN-ATNAM.

  DATA: BEGIN OF LT_RESB OCCURS 0,
        RSPOS LIKE RESB-RSPOS,
        MATNR LIKE RESB-MATNR,
        WERKS LIKE RESB-WERKS,
        LGORT LIKE RESB-LGORT,
        PRVBE LIKE RESB-PRVBE,
        BDMNG LIKE RESB-BDMNG,
        MEINS LIKE RESB-MEINS,
        SORTF LIKE RESB-SORTF,
        BAUGR LIKE RESB-BAUGR,
        END OF LT_RESB.

  DATA: BEGIN OF LT_BKLAS OCCURS 0,
        MATNR LIKE MBEW-MATNR,
        BWKEY LIKE MBEW-BWKEY,
        BKLAS LIKE MBEW-BKLAS,
        END OF LT_BKLAS.

  DATA: BEGIN OF LT_EORD OCCURS 0,
        MATNR LIKE EORD-MATNR,
        WERKS LIKE EORD-WERKS,
        LIFNR LIKE EORD-LIFNR,
        END OF LT_EORD.

  CLEAR : IT_TAB, IT_TAB[].

  L_MI = 'P_MI'.
  L_MODEL_YEAR = 'P_MODEL_YEAR'.
  L_OCN = 'P_OCN'.
  L_PLAN_ORDER = 'P_PLAN_ORDER'.
  L_VIN = 'P_VIN'.
  L_DEST = 'P_DESTINATION_CODE'.

  IF P_RP BETWEEN '01' AND '27'.
    CONCATENATE 'P_RP' P_RP '_SHOP_DATE'  INTO L_ATNAM .
    L_ATFLV = L_NUM = P_DATE.
    SELECT A~OBJEK
    INTO TABLE LT_OBJEK
    FROM AUSP AS A
      INNER JOIN CABN AS B ON A~ATINN = B~ATINN
    WHERE KLART = '002'
      AND A~ATFLV = L_ATFLV
      AND B~ATNAM = L_ATNAM.
    DESCRIBE TABLE LT_OBJEK LINES L_LEN.
    IF L_LEN > 0.
      SELECT A~OBJEK INTO TABLE IT_OBJEK
        FROM AUSP AS A INNER JOIN CABN AS B
           ON A~ATINN = B~ATINN
         FOR ALL ENTRIES IN LT_OBJEK
           WHERE KLART = '002'
             AND OBJEK = LT_OBJEK-OBJEK
             AND ATNAM = 'P_NATN_CODE'
             AND ATWRT = P_NATN.
      DESCRIBE TABLE IT_OBJEK LINES L_LEN.
      IF L_LEN > 0.
        SELECT OBJEK B~ATNAM A~ATINN ATWRT ATFLV INTO TABLE LT_AUSP
           FROM AUSP AS A INNER JOIN CABN AS B
           ON A~ATINN = B~ATINN
           FOR ALL ENTRIES IN IT_OBJEK
           WHERE KLART = '002'
             AND OBJEK = IT_OBJEK-OBJEK
             AND ( ATNAM = L_MI
                OR  ATNAM = L_MODEL_YEAR
                OR  ATNAM = L_OCN
                OR  ATNAM = L_VIN
                OR  ATNAM = L_PLAN_ORDER
                OR ATNAM = 'P_DESTINATION_CODE' ).
      ENDIF.
    ENDIF.
    SORT LT_AUSP BY OBJEK.
    LOOP AT LT_AUSP.
      CASE LT_AUSP-ATNAM.
        WHEN L_MI.
          W_MI = LT_AUSP-ATWRT.
        WHEN L_OCN.
          W_OCN = LT_AUSP-ATWRT.
        WHEN L_MODEL_YEAR.
          W_MODYR = LT_AUSP-ATWRT.
        WHEN L_VIN.
          IT_TAB-VIN = LT_AUSP-ATWRT.
        WHEN L_PLAN_ORDER.
          W_PLNUM = LT_AUSP-ATWRT.
        WHEN L_DEST.
          W_DEST = LT_AUSP-ATWRT.
      ENDCASE.
      AT END OF OBJEK.
        L_LEN = STRLEN( W_MI ).
        IF L_LEN = 7.
          CONCATENATE W_MODYR W_DEST  W_MI INTO W_FSC.
          CONCATENATE W_FSC W_OCN INTO W_FSC SEPARATED BY SPACE.
        ELSE.
          L_DEALER_OLD = W_DEST+3(2).
          CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
               EXPORTING
                    OLD_DEALER = L_DEALER_OLD
               IMPORTING
                    NEW_DEALER = L_DEALER_NEW.

          CONCATENATE W_MODYR W_DEST+0(3) L_DEALER_NEW W_MI W_OCN
                      INTO W_FSC.
        ENDIF.
        IT_TAB-FSC = W_FSC.
        IT_TAB-BODY = LT_AUSP-OBJEK.
        IT_TAB-RP_SHOP_DATE = P_DATE.
        SELECT SINGLE VERID PLWRK PEDTR INTO (IT_TAB-VERSION,
              IT_TAB-PLANT, IT_TAB-ORD_FIN_DATE)
           FROM PLAF
           WHERE PLNUM = W_PLNUM.
        IT_TAB-PLNUM = W_PLNUM.
        IT_TAB-CR_DATE = SY-DATUM.
        IT_TAB-CR_TIME = SY-UZEIT.

        SELECT RSPOS MATNR WERKS LGORT PRVBE BDMNG MEINS SORTF BAUGR
               INTO TABLE LT_RESB
               FROM RESB
               WHERE PLNUM = W_PLNUM.
        DESCRIBE TABLE LT_RESB LINES L_LEN.

        IF L_LEN > 0.
          SELECT MATNR BWKEY BKLAS INTO TABLE LT_BKLAS
             FROM MBEW
             FOR ALL ENTRIES IN LT_RESB
          WHERE MATNR = LT_RESB-MATNR
            AND BWKEY = LT_RESB-WERKS.

          SELECT MATNR WERKS LIFNR INTO TABLE LT_EORD
             FROM EORD
             FOR ALL ENTRIES IN LT_RESB
          WHERE MATNR = LT_RESB-MATNR
            AND WERKS = LT_RESB-WERKS
            AND VDATU <= SY-DATUM
            AND BDATU >= SY-DATUM.
        ENDIF.

        LOOP AT LT_RESB.
          IT_TAB-RSPOS = LT_RESB-RSPOS.
          IT_TAB-MATNR = LT_RESB-MATNR.
          IT_TAB-LGORT = LT_RESB-LGORT.
          IT_TAB-PRVBE = LT_RESB-PRVBE.
          IT_TAB-QTY = LT_RESB-BDMNG.
          IT_TAB-MEINS = LT_RESB-MEINS.
          IT_TAB-SORTF = LT_RESB-SORTF.
          IT_TAB-BAUGR = LT_RESB-BAUGR.
          READ TABLE LT_BKLAS WITH KEY MATNR = LT_RESB-MATNR
                                       BWKEY = LT_RESB-WERKS.
          IF SY-SUBRC = 0.
            IT_TAB-BKLAS = LT_BKLAS-BKLAS.
          ENDIF.
          READ TABLE LT_EORD WITH KEY MATNR = LT_RESB-MATNR
                                       WERKS = LT_RESB-WERKS.
          IF SY-SUBRC = 0.
            IT_TAB-LIFNR = LT_EORD-LIFNR.
          ENDIF.
          APPEND IT_TAB.
*          collect IT_TAB.
        ENDLOOP.

        CLEAR: LT_RESB, LT_BKLAS,LT_EORD.
        CLEAR: LT_RESB[], LT_BKLAS[],LT_EORD[], IT_TAB, L_LEN.
      ENDAT.
    ENDLOOP.
  ELSE.
    MESSAGE I001 WITH TEXT-M11.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MAKE_DROPDOWN_LIST_BOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DROPDOWN_LIST_BOX.
  CLEAR : XLIST[] , XVALUE.

  XVALUE-TEXT = 'Control Gate - 19'.
  XVALUE-KEY  = '19'.
  APPEND XVALUE TO XLIST .
*
  XVALUE-TEXT = 'Ship out - 25'.
  XVALUE-KEY  = '25'.
  APPEND XVALUE TO XLIST .

  PERFORM LIST_BOX_FUNCTION USING 'P_RP'.
*  READ TABLE XLIST INTO XVALUE  INDEX 1.
*  P_PRDT = XVALUE-KEY.

ENDFORM.                    " MAKE_DROPDOWN_LIST_BOX

*---------------------------------------------------------------------*
*       FORM LIST_BOX_FUNCTION                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_LIST_NAME                                                   *
*---------------------------------------------------------------------*
FORM LIST_BOX_FUNCTION USING   P_LIST_NAME .
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID              = P_LIST_NAME  " list box
            VALUES          = XLIST
       EXCEPTIONS
            ID_ILLEGAL_NAME = 1
            OTHERS          = 2.
ENDFORM.                    " list_box_function
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA.
  MODIFY ZTPP_RESB_B06 FROM TABLE IT_TAB.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  set_init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_INIT_DATA.
  P_NATN = 'B06'.
  P_RP = '19'.
  P_DATE = SY-DATUM - 1.
ENDFORM.                    " set_init_data
