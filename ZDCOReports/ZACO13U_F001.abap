*----------------------------------------------------------------------*
*   INCLUDE ZACO13R_F001                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_FR_ZTCO_SHOPCOST
*&---------------------------------------------------------------------*
*       Read data from ZTCO_SHOPCOST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FR_ZTCO_SHOPCOST.

  CLEAR : IT_DATACON, IT_DATACON[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE
           IT_DATACON
           FROM ZTCO_SHOPCOST
          WHERE KOKRS = P_KOKRS
            AND BDATJ = P_BDATJ
            AND POPER = P_POPER
            AND RECORD_TYPE = 'S'. "STD

  CLEAR IT_DATACON.

  IF IT_DATACON[] IS INITIAL .
    MESSAGE E000 WITH TEXT-101.
  ENDIF.

  SORT IT_DATACON BY
       KOKRS
       BDATJ
       POPER
       WERKS
       SHOP
       FSC_MATNR
       LLV_MATNR
       TYPPS
       KSTAR
       ELEMT
       KOSTL
       LSTAR
       MTART.

* ->  IT_ZTCO_SHOPVAR
  CLEAR ZSCA_TIME_STAMP.

  LOOP AT IT_DATACON.
    MOVE-CORRESPONDING IT_DATACON TO IT_ZTCO_SHOPVAR.
*    CLEAR LOG.
    MOVE-CORRESPONDING ZSCA_TIME_STAMP TO IT_ZTCO_SHOPVAR.

** Royalty data can not have MEINS value : SET Meins from MARA
*    if IT_ZTCO_SHOPVAR-MEEHT eq space.
*    endif.

** Unit Conv (-> HR)
    SELECT SINGLE  * FROM T006
                    WHERE MSEHI = IT_ZTCO_SHOPVAR-MEEHT
                      AND DIMID = 'TIME'.
    IF SY-SUBRC = 0 AND IT_ZTCO_SHOPVAR-MEEHT NE 'STD'.
      IT_ZTCO_SHOPVAR-MEEHT = 'STD'.
    ENDIF.

    COLLECT IT_ZTCO_SHOPVAR.
    CLEAR : IT_ZTCO_SHOPVAR, IT_DATACON.
  ENDLOOP.

  CLEAR IT_ZTCO_SHOPVAR.

* Data without SHOP information (Cat. "V") should be remained
* Do not rid off them

* Free Itab
  FREE IT_DATACON.
  CLEAR : IT_DATACON, IT_DATACON[].

ENDFORM.                    " READ_FR_ZTCO_SHOPCOST

*&---------------------------------------------------------------------*
*&      Form  Read_TKA01
*&---------------------------------------------------------------------*
*       Controlling Area Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_TKA01.
  CLEAR TKA01.
  SELECT SINGLE * FROM TKA01
                 WHERE KOKRS = P_KOKRS.
  IF SY-SUBRC <> 0.
    MESSAGE E038 WITH P_KOKRS.
  ENDIF.
ENDFORM.                    " Read_TKA01

*&---------------------------------------------------------------------*
*&      Form  READ_FR_ZTCO_SHOPCOST_AT
*&---------------------------------------------------------------------*
*       Read data from ZTCO_SHOPCOST_AT (Actual)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FR_ZTCO_SHOPCOST_AT.

  CLEAR : IT_DATACON, IT_DATACON[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE
           IT_DATACON
           FROM ZTCO_SHOPCOST_AT
          WHERE KOKRS = P_KOKRS
            AND BDATJ = P_BDATJ
            AND POPER = P_POPER
            AND RECORD_TYPE = 'A'. "Actual

  CLEAR IT_DATACON.

  IF IT_DATACON[] IS INITIAL .
    MESSAGE E000 WITH TEXT-102.
  ENDIF.

  SORT IT_DATACON BY
       KOKRS
       BDATJ
       POPER
       WERKS
       SHOP
       FSC_MATNR
       LLV_MATNR
       TYPPS
       KSTAR
       ELEMT
       KOSTL
       LSTAR
       MTART.

* ->  IT_ZTCO_SHOPVAR
  CLEAR ZSCA_TIME_STAMP.
  LOOP AT IT_DATACON.
    MOVE-CORRESPONDING IT_DATACON TO IT_ZTCO_SHOPVAR.
*    CLEAR LOG.
    MOVE-CORRESPONDING ZSCA_TIME_STAMP TO IT_ZTCO_SHOPVAR.

** Royalty data can not have MEINS value : SET Meins from MARA
*    if IT_ZTCO_SHOPVAR-MEEHT eq space.
*    endif.

** Unit Conv (-> HR)
    SELECT SINGLE  * FROM T006
                    WHERE MSEHI = IT_ZTCO_SHOPVAR-MEEHT
                      AND DIMID = 'TIME'.
    IF SY-SUBRC = 0 .
      PERFORM UNI_CONV_STD.
      IT_ZTCO_SHOPVAR-MEEHT = 'STD'.
    ENDIF.

    COLLECT IT_ZTCO_SHOPVAR.
    CLEAR : IT_ZTCO_SHOPVAR, IT_DATACON.
  ENDLOOP.

** -> BR
  CLEAR IT_ZTCO_SHOPVAR.

* Data without SHOP information (Cat. "V") should be remained
* Do not rid off them

* Free Itab
  FREE IT_DATACON.
  CLEAR : IT_DATACON, IT_DATACON[].



* Read Previous WIP data
  DATA : BEGIN OF IT_L_PRE_WIP OCCURS 1000.
          INCLUDE STRUCTURE ZSCO_SHOPVAR_KEY.
  DATA :  PR_WIP_QUANTITY  LIKE ZTCO_SHOPVAR-WIP_QUANTITY,
          PR_WIP_AMT       LIKE ZTCO_SHOPVAR-WIP_AMT.
  DATA :  WIP_QUANTITY     LIKE ZTCO_SHOPVAR-WIP_QUANTITY,
          WIP_AMT          LIKE ZTCO_SHOPVAR-WIP_AMT.
  DATA : END OF  IT_L_PRE_WIP .
  DATA : LV_PR_POPER LIKE CKMLPP-POPER,
         LV_PR_BDATJ LIKE CKMLPP-BDATJ.

* Read Previous Period
  CALL FUNCTION 'CKML_F_GET_PREVIOUS_PERIOD'
       EXPORTING
            INPUT_PERIOD    = P_POPER
            INPUT_YEAR      = P_BDATJ
            INPUT_PERIV     = TKA01-LMONA
       IMPORTING
            PREVIOUS_PERIOD = LV_PR_POPER
            PREVIOUS_YEAR   = LV_PR_BDATJ.

*
  SELECT * INTO CORRESPONDING FIELDS OF TABLE
           IT_L_PRE_WIP
           FROM ZTCO_SHOPCOST_AT
          WHERE KOKRS = P_KOKRS
            AND BDATJ = LV_PR_BDATJ
            AND POPER = LV_PR_POPER
            AND RECORD_TYPE = 'A'   "Actual
            AND  (  WIP_QUANTITY NE SPACE
                 OR WIP_AMT      NE SPACE ).

* ->  IT_ZTCO_SHOPVAR
  LOOP AT IT_L_PRE_WIP.
    MOVE-CORRESPONDING IT_L_PRE_WIP TO IT_ZTCO_SHOPVAR.
*  WIP data
    IT_ZTCO_SHOPVAR-PR_WIP_QUANTITY = IT_ZTCO_SHOPVAR-WIP_QUANTITY.
    IT_ZTCO_SHOPVAR-PR_WIP_AMT      = IT_ZTCO_SHOPVAR-WIP_AMT.
    CLEAR : IT_ZTCO_SHOPVAR-WIP_QUANTITY,
            IT_ZTCO_SHOPVAR-WIP_AMT.
*  Change Period
    IT_ZTCO_SHOPVAR-BDATJ = P_BDATJ.
    IT_ZTCO_SHOPVAR-POPER = P_POPER.

    COLLECT IT_ZTCO_SHOPVAR.
    CLEAR : IT_ZTCO_SHOPVAR, IT_L_PRE_WIP.
  ENDLOOP.

** -> BR
  CLEAR IT_ZTCO_SHOPVAR.

* Free
  FREE : IT_L_PRE_WIP.
  CLEAR : IT_L_PRE_WIP, IT_L_PRE_WIP[].

ENDFORM.                    " READ_FR_ZTCO_SHOPCOST_AT

*&---------------------------------------------------------------------*
*&      Form  CAL_DATA
*&---------------------------------------------------------------------*
*       Calculate Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_DATA.
* Read ML data : Only Receipt ('ZU') - Production ('BF')
  PERFORM READ_ML_DATA_ZU_BF.

* Cal. Data (For Frozen, Current and Actual)
  PERFORM CAL_FR_CUR_ACT_VALUES.

* Applying Variance Formula
  PERFORM APPLY_VARIANCE_FORMULA.

ENDFORM.                    " CAL_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_ML_DATA_ZU_BF
*&---------------------------------------------------------------------*
*       READ MLCD data (CKM3)
*       Only read data for 'Receipt - Production'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ML_DATA_ZU_BF.

* Read CKM3 <- ML
* MLCD : 'ZU' 'BF'
* KALNR <- CKMLHD / Should be collected Because of BVALT


* Local Data Definition
  DATA : IT_L_MLCD LIKE IT_MLCD_EX OCCURS 0 WITH HEADER LINE .

* Read data
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_L_MLCD
           FROM MLCD
          WHERE BDATJ = P_BDATJ
            AND POPER = P_POPER
            AND CURTP = '10' "controlling Area Curr.
            AND CATEG = 'ZU' "Receipt
            AND PTYP  = 'BF'.

* Collect data
* If there is no ML-Closing, There might be no data on ML Tables
* or it is not possible to get rolled-up data.

  CLEAR : IT_MLCD_EX, IT_MLCD_EX[].
  LOOP AT IT_L_MLCD.
    MOVE-CORRESPONDING IT_L_MLCD TO IT_MLCD_EX.
    COLLECT IT_MLCD_EX.
    CLEAR IT_MLCD_EX.
    CLEAR IT_L_MLCD.
  ENDLOOP.

* Read Material Info.
  LOOP AT IT_MLCD_EX.
*  Clear CKMLHD.
    SELECT SINGLE MATNR
                  BWKEY
                  BWTAR
             INTO (IT_MLCD_EX-MATNR,
                   IT_MLCD_EX-BWKEY,
                   IT_MLCD_EX-BWTAR)
             FROM CKMLHD
            WHERE KALNR = IT_MLCD_EX-KALNR.
    MODIFY IT_MLCD_EX.
    CLEAR IT_MLCD_EX.
  ENDLOOP.

  CLEAR IT_MLCD_EX.

ENDFORM.                    " READ_ML_DATA_ZU_BF

*&---------------------------------------------------------------------*
*&      Form  CAL_FR_CUR_ACT_VALUES
*&---------------------------------------------------------------------*
*       Cal. Data (For Frozen, Current and Actual)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_FR_CUR_ACT_VALUES.

  CLEAR IT_MLCD_EX.
  CLEAR IT_ZTCO_SHOPVAR.

* Sort
  SORT IT_MLCD_EX BY MATNR BWKEY BWTAR.

* The valuation information of some materials in IT_ZTCO_SHOPVAR can be
* different from that of MBEW because the plant can be switch up from
* 'P001' to 'E001' to get the cost component data at the manufacturing
* plant if the child materials of a FSC are made in E001.

  LOOP AT IT_ZTCO_SHOPVAR.
** -- test Only For CAT. 'M'. ???
**     WHERE TYPPS = 'M'.
** --> Read Production Qty (CKM3: From ML)
    CLEAR IT_MLCD_EX.
    READ TABLE IT_MLCD_EX WITH KEY
                          MATNR = IT_ZTCO_SHOPVAR-FSC_MATNR
                          BWKEY = IT_ZTCO_SHOPVAR-BWKEY
                          BWTAR = IT_ZTCO_SHOPVAR-BWTAR.
    IF SY-SUBRC <> 0.
      CLEAR IT_MLCD_EX.
      READ TABLE IT_MLCD_EX WITH KEY
                            MATNR = IT_ZTCO_SHOPVAR-FSC_MATNR.
*                           BWKEY = IT_ZTCO_SHOPVAR-BWKEY
*                           BWTAR = IT_ZTCO_SHOPVAR-BWTAR.
    ENDIF.

** --> Frozen / How if Activity Qty??????

    IT_ZTCO_SHOPVAR-FROZEN_QTY = IT_MLCD_EX-LBKUM.
**  Save meins For FSC_MATNR
    IT_ZTCO_SHOPVAR-MEINS = IT_MLCD_EX-MEINS.
    IT_ZTCO_SHOPVAR-FROZEN_AMT =
               (
               ( IT_ZTCO_SHOPVAR-WERTN * IT_ZTCO_SHOPVAR-FROZEN_QTY )
                 + IT_ZTCO_SHOPVAR-WIP_AMT
               ) - IT_ZTCO_SHOPVAR-PR_WIP_AMT.

** --> Current

    IT_ZTCO_SHOPVAR-CURR_QTY = IT_ZTCO_SHOPVAR-MBGBTR.
    IT_ZTCO_SHOPVAR-CURR_AMT = IT_ZTCO_SHOPVAR-WKGBTR.

** --> Actual

    IT_ZTCO_SHOPVAR-ACTUAL_QTY = IT_ZTCO_SHOPVAR-MBGBTR
                               + IT_ZTCO_SHOPVAR-ADD_MBGBTR.
    IT_ZTCO_SHOPVAR-ACTUAL_AMT = IT_ZTCO_SHOPVAR-WKGBTR
                               + IT_ZTCO_SHOPVAR-ADD_WKGBTR.

* Modify
    MODIFY IT_ZTCO_SHOPVAR.
    CLEAR  IT_ZTCO_SHOPVAR.
  ENDLOOP.

  CLEAR  IT_ZTCO_SHOPVAR.

ENDFORM.                    " CAL_FR_CUR_ACT_VALUES

*&---------------------------------------------------------------------*
*&      Form  APPLY_VARIANCE_FORMULA
*&---------------------------------------------------------------------*
*       Applying Variance Formula
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPLY_VARIANCE_FORMULA.

  LOOP AT IT_ZTCO_SHOPVAR.

** --> Spec Var.
    IT_ZTCO_SHOPVAR-SPCVAR_QTY =  ( IT_ZTCO_SHOPVAR-CURR_QTY
                                  - IT_ZTCO_SHOPVAR-ACTUAL_SCRAP )
                                  - IT_ZTCO_SHOPVAR-FROZEN_QTY.
    IT_ZTCO_SHOPVAR-SPCVAR_AMT =  ( IT_ZTCO_SHOPVAR-CURR_AMT
                                  - IT_ZTCO_SHOPVAR-SCRAP_AMT )
                                  - IT_ZTCO_SHOPVAR-FROZEN_AMT.
** --> Qty Var.
    IT_ZTCO_SHOPVAR-QTYVAR_QTY = IT_ZTCO_SHOPVAR-ACTUAL_QTY
                               - IT_ZTCO_SHOPVAR-CURR_QTY.
    IT_ZTCO_SHOPVAR-QTYVAR_AMT = IT_ZTCO_SHOPVAR-ACTUAL_AMT
                               - IT_ZTCO_SHOPVAR-CURR_AMT.

** --> Scrap Var.
* Already Applied

** --> Price Difference
    IT_ZTCO_SHOPVAR-PRDIF = IT_ZTCO_SHOPVAR-ML_ACT_PREIS
                          - IT_ZTCO_SHOPVAR-CURR_AMT.

* Modify
    MODIFY  IT_ZTCO_SHOPVAR.
    CLEAR   IT_ZTCO_SHOPVAR.
  ENDLOOP.

ENDFORM.                    " APPLY_VARIANCE_FORMULA

*&---------------------------------------------------------------------*
*&      Form  DEL_DATA_FR_ZTCO_SHOPVAR
*&---------------------------------------------------------------------*
*       - > Always deletion -> Refresh data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEL_DATA_FR_ZTCO_SHOPVAR.

  DELETE FROM ZTCO_SHOPVAR
        WHERE
              KOKRS        =  P_KOKRS
          AND BDATJ        =  P_BDATJ
          AND POPER        =  P_POPER.
  IF SY-SUBRC = 0.
  ENDIF.
* No- Error Check In Deletion Phase
  COMMIT WORK AND WAIT.

ENDFORM.                    " DEL_DATA_FR_ZTCO_SHOPVAR

*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTCO_SHOPVAR
*&---------------------------------------------------------------------*
*       Update/Insert
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_ZTCO_SHOPVAR.
* Always Insertion
  CLEAR IT_ZTCO_SHOPVAR.

* Sort By Key.
  SORT IT_ZTCO_SHOPVAR
    BY
                        KOKRS
                        BDATJ
                        POPER
                        WERKS
                        SHOP
                        FSC_MATNR
                        LLV_MATNR
                        TYPPS
                        KSTAR
                        ELEMT
                        KOSTL
                        LSTAR.

  LOOP AT IT_ZTCO_SHOPVAR.
* LOG
    IT_ZTCO_SHOPVAR-ERDAT = SY-DATUM.
    IT_ZTCO_SHOPVAR-ERZET = SY-UZEIT.
    IT_ZTCO_SHOPVAR-ERNAM = SY-UNAME.
* CURKY
    IF IT_ZTCO_SHOPVAR-HWAER EQ SPACE .
      IT_ZTCO_SHOPVAR-HWAER = TKA01-WAERS.
    ENDIF.
    CLEAR ZTCO_SHOPVAR.
    MOVE-CORRESPONDING  IT_ZTCO_SHOPVAR TO ZTCO_SHOPVAR.
    INSERT ZTCO_SHOPVAR .
    IF SY-SUBRC <> 0.
*      WRITE : / ZTCO_SHOPCOST .
      MESSAGE E044.
    ENDIF.
    CLEAR IT_ZTCO_SHOPVAR.
  ENDLOOP.

ENDFORM.                    " UPDATE_ZTCO_SHOPVAR

*&---------------------------------------------------------------------*
*&      Form  UP_INS_LOG
*&---------------------------------------------------------------------*
*       Update / Insert Log
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UP_INS_LOG.
* LOG
  DESCRIBE TABLE IT_ZTCO_SHOPVAR LINES SY-TFILL.
  WRITE : / 'No. of Created Records : ' , SY-TFILL.
  WRITE : / 'Created date           : ' , SY-DATUM.
  WRITE : / 'Created By             : ' , SY-UNAME.
  SKIP 1.
  WRITE : / TEXT-190.

* Success
  MESSAGE S009 WITH 'Data Creation'.

ENDFORM.                    " UP_INS_LOG

*&---------------------------------------------------------------------*
*&      Form  UNI_CONV_STD
*&---------------------------------------------------------------------*
*       Unit Conv. -> STD
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UNI_CONV_STD.

* WIP_QUANTITY
* ACTUAL_SCRAP
* MBGBTR
* ADD_MBGBTR
* PR_WIP_QUANTITY

  CHECK IT_ZTCO_SHOPVAR-MEEHT NE 'STD'.

  PERFORM UNIT_CONVERION USING IT_ZTCO_SHOPVAR-WIP_QUANTITY
                               IT_ZTCO_SHOPVAR-WIP_QUANTITY
                               IT_ZTCO_SHOPVAR-MEEHT
                               'STD'.

  PERFORM UNIT_CONVERION USING IT_ZTCO_SHOPVAR-ACTUAL_SCRAP
                               IT_ZTCO_SHOPVAR-ACTUAL_SCRAP
                               IT_ZTCO_SHOPVAR-MEEHT
                               'STD'.

  PERFORM UNIT_CONVERION USING IT_ZTCO_SHOPVAR-MBGBTR
                               IT_ZTCO_SHOPVAR-MBGBTR
                               IT_ZTCO_SHOPVAR-MEEHT
                               'STD'.

  PERFORM UNIT_CONVERION USING IT_ZTCO_SHOPVAR-ADD_MBGBTR
                               IT_ZTCO_SHOPVAR-ADD_MBGBTR
                               IT_ZTCO_SHOPVAR-MEEHT
                               'STD'.

  PERFORM UNIT_CONVERION USING IT_ZTCO_SHOPVAR-PR_WIP_QUANTITY
                               IT_ZTCO_SHOPVAR-PR_WIP_QUANTITY
                               IT_ZTCO_SHOPVAR-MEEHT
                               'STD'.


ENDFORM.                    " UNI_CONV_STD

*&---------------------------------------------------------------------*
*&      Form  UNIT_CONVERION
*&---------------------------------------------------------------------*
*       Unit Conversion
*----------------------------------------------------------------------*
*      -->P_INPUT     Inout
*      -->P_OUTPUT    OutPut
*      -->P_UNIT_IN   unit In
*      -->P_UNIT_OUT  Unit Out
*----------------------------------------------------------------------*
FORM UNIT_CONVERION USING    P_INPUT
                             P_OUTPUT
                             P_UNIT_IN
                             P_UNIT_OUT.

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      INPUT                      = P_INPUT
*     NO_TYPE_CHECK              = ' '
*     ROUND_SIGN                 = ' '
      UNIT_IN                    = P_UNIT_IN
      UNIT_OUT                   = P_UNIT_OUT
    IMPORTING
*     ADD_CONST                  = '1'
*     DECIMALS                   =
*     DENOMINATOR                =
*     NUMERATOR                  =
      OUTPUT                     = P_OUTPUT
   EXCEPTIONS
     CONVERSION_NOT_FOUND       = 1
     DIVISION_BY_ZERO           = 2
     INPUT_INVALID              = 3
     OUTPUT_INVALID             = 4
     OVERFLOW                   = 5
     TYPE_INVALID               = 6
     UNITS_MISSING              = 7
     UNIT_IN_NOT_FOUND          = 8
     UNIT_OUT_NOT_FOUND         = 9
     OTHERS                     = 10
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " UNIT_CONVERION
