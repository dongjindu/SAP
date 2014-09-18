*----------------------------------------------------------------------*
***INCLUDE ZXM06F01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN USING    VALUE(P_DYNNR).
  CASE P_DYNNR.
    WHEN 0111.
* gl_aktyp : A:Display, V:Change, H:Create
      LOOP AT SCREEN.
        CHECK GL_AKTYP = 'A'.
        SCREEN-INPUT = 0.   " Display mode
        MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    " modify_screen

*&---------------------------------------------------------------------*
*&      Form  get_mrp_qty
*&---------------------------------------------------------------------*
*        Get MRP Qty to pass as exact Qty in IDoc.
*----------------------------------------------------------------------*
*      -->P_MDSU   planning values
*      -->P_PLSCN  planning version
*      -->P_MATNR  material number
*      -->P_WERKS  plant
*      -->P_INPER  in period
*----------------------------------------------------------------------*
FORM GET_MRP_QTY TABLES P_MDSU STRUCTURE MDSU
                 USING  P_PLSCN
                        P_MATNR
                        P_WERKS
                        P_INPER.

  CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
    EXPORTING
      PLSCN                          = P_PLSCN
      MATNR                          = P_MATNR
      WERKS                          = P_WERKS
*   BERID                          =
*   ERGBZ                          =
*   AFIBZ                          =
      INPER                          = P_INPER
*   DISPLAY_LIST_MDPSX             =
*   DISPLAY_LIST_MDEZX             =
*   DISPLAY_LIST_MDSUX             =
* IMPORTING
*   E_MT61D                        =
*   E_MDKP                         =
   TABLES
*   MDPSX                          =
*   MDEZX                          =
      MDSUX                          = P_MDSU
   EXCEPTIONS
     MATERIAL_PLANT_NOT_FOUND       = 1
     PLANT_NOT_FOUND                = 2
     OTHERS                         = 3 .

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " get_mrp_qty
*&---------------------------------------------------------------------*
*&      Form  replace_qty_mrpqty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DINT_EDIDD  text
*----------------------------------------------------------------------*
FORM REPLACE_QTY_MRPQTY TABLES P_EDIDD STRUCTURE EDIDD
                        USING  P_ABART.

  DATA: WA_MDSU  LIKE MDSU,
        WA_EDIDD_P10 LIKE EDIDD,
        WA_EDIDD_P16 LIKE EDIDD,
        WA_E1EDP10 LIKE E1EDP10,
        WA_E1EDP16 LIKE E1EDP16.


  DATA: T_MDSU LIKE TABLE OF WA_MDSU.

  DATA: W_MATNR     LIKE MARA-MATNR,
        W_WERKS     LIKE MARC-WERKS,
        W_21_MON    LIKE SY-DATUM,
        W_EDATUB    LIKE SY-DATUM,
        W_MENGE     LIKE WA_MDSU-MNG01,
        W_CUM_QTY   LIKE WA_MDSU-MNG01,
        W_PLSCN     LIKE PLSC-PLSCN,
        W_PLSCN1    LIKE PLSC-PLSCN,
        W_INPER     LIKE MDST-INPER,
        W_NO_LINES  TYPE I,
        W_CHAR_QTY(15) TYPE C,
        W_CUR_MON LIKE SY-DATUM.

  DELETE P_EDIDD WHERE SEGNAM = 'E1EDP16'.

  READ TABLE P_EDIDD INTO WA_EDIDD_P10 WITH KEY SEGNAM = 'E1EDP10'.
  WA_E1EDP10 = WA_EDIDD_P10-SDATA.
  W_MATNR = WA_E1EDP10-IDNKD.
  W_WERKS = WA_E1EDP10-KWERK.
*To get planning data.
  CASE P_ABART.
    WHEN 'JIT'.
      W_PLSCN = '000'.
      W_INPER = 'T'.
    WHEN 'FOR'.
      W_PLSCN = '900'.
      W_INPER = 'W'.
  ENDCASE.
*Check whether the Planning scenario in long-term planning, exists.
  SELECT SINGLE PLSCN FROM PLSC INTO W_PLSCN1 WHERE PLSCN = W_PLSCN.
  IF SY-SUBRC NE 0.
    RAISE DATA_NOT_RELEVANT_FOR_SENDING.
  ENDIF.
*Get MRP Qty.
  PERFORM GET_MRP_QTY TABLES T_MDSU
                      USING W_PLSCN W_MATNR W_WERKS W_INPER.
  DESCRIBE TABLE T_MDSU LINES W_NO_LINES.
  CHECK W_NO_LINES NE 0.
  SORT T_MDSU BY DAT00.
  CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
       EXPORTING
            DATE   = SY-DATUM
       IMPORTING
            MONDAY = W_CUR_MON.
*&--------------------------------------------------------------------&*
*& 21 weeks including current week. (21 * 7 = 147. 147 - 7 = 140).
*& If requirement = 0, no need to send forecast info. for that week.
*&--------------------------------------------------------------------&*
  W_21_MON = W_CUR_MON + 140.

  LOOP AT T_MDSU INTO WA_MDSU
                 WHERE DAT00 BETWEEN W_CUR_MON AND W_21_MON
                 AND   PERKZ EQ W_INPER.
    CLEAR: W_MENGE, WA_EDIDD_P16, W_EDATUB, WA_E1EDP16, W_CHAR_QTY.

    W_MENGE = ABS( WA_MDSU-MNG01 ) + ABS( WA_MDSU-MNG02 ).
    IF W_MENGE EQ 0.
      CONTINUE.
    ENDIF.
    W_CUM_QTY = W_CUM_QTY + W_MENGE.
    WA_EDIDD_P16-SEGNAM = 'E1EDP16'.
    WA_E1EDP16-PRGRS = 'W'.
    WA_E1EDP16-EDATUV = WA_MDSU-DAT00.
    W_EDATUB = WA_MDSU-DAT00 + 6.
    WA_E1EDP16-EDATUB = W_EDATUB.
    W_CHAR_QTY = W_MENGE.
    SHIFT W_CHAR_QTY LEFT DELETING LEADING SPACE.
    WA_E1EDP16-WMENG = W_CHAR_QTY.
    CLEAR W_CHAR_QTY.
    W_CHAR_QTY = W_CUM_QTY.
    SHIFT W_CHAR_QTY LEFT DELETING LEADING SPACE.
    WA_E1EDP16-FZABR = W_CHAR_QTY.
    WA_EDIDD_P16-SDATA = WA_E1EDP16.
    CLEAR W_CHAR_QTY.
    APPEND WA_EDIDD_P16 TO P_EDIDD.
  ENDLOOP.
*&--------------------------------------------------------------------&*
*   Check whether we have atleast one valid forecast information
*   segment, else don't send the IDoc.
*&--------------------------------------------------------------------&*
  READ TABLE P_EDIDD WITH KEY SEGNAM = 'E1EDP16' TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0.
    RAISE DATA_NOT_RELEVANT_FOR_SENDING.
  ENDIF.
ENDFORM.                    " replace_qty_mrpqty
*&---------------------------------------------------------------------*
*&      Form  modify_screen_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN_0101 USING   VALUE(P_SY_DYNNR).
* Begin of changes - UD1K922802
* Make KD PO field editable during change mode.
  CASE P_SY_DYNNR.
    WHEN 0101.
      IF NOT ( SY-TCODE EQ 'ME22N' OR
               SY-TCODE EQ 'ME22' ).
        LOOP AT SCREEN.
          SCREEN-INPUT = 0.   " Display mode
          MODIFY SCREEN.
        ENDLOOP.
      ENDIF.
  ENDCASE.
* End of changes - UD1K922802
ENDFORM.                    " modify_screen_0101
