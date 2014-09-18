****************************************************************
* Program Name      : ZEMMPM29E_CD_CHANGE_SA
* Author            : Min-su Park
* Creation Date     : 2003.10.27.
* Specifications By : Min-su Park
* Development Request No : UD1K901873
* Addl Documentation:
* Description       : Condition change in Schedule Agreement
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.19.     Min-su Park    UD1K901873     Initial Coding
***************************************************************
*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM29E_CD_CHANGE_SAF01                                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
* Read Current scheduling agreement Data.
  SELECT * FROM ZVMM_PO_DETAIL1
           INTO CORRESPONDING FIELDS OF TABLE IT_PO_DETAIL
          WHERE EBELN IN S_EBELN.

* Read Condition of Current Info record Record Data
  SELECT * FROM A017
           INTO CORRESPONDING FIELDS OF TABLE IT_A017
          WHERE DATAB <= SY-DATUM
            AND DATBI >= SY-DATUM.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CONDITION_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONDITION_CHANGE.
  SORT IT_PO_DETAIL BY EBELN EBELP.
  LOOP AT IT_PO_DETAIL.
    MOVE-CORRESPONDING IT_PO_DETAIL TO WA_TMP_PO_DETAIL.
    AT NEW EBELN.
      PERFORM FIRST_SCREEN_ME32L USING WA_TMP_PO_DETAIL.
    ENDAT.
    PERFORM REMAIND_ME32L USING WA_TMP_PO_DETAIL.

    AT END OF EBELN.
      PERFORM BDC_PASS USING:
        'X' 'SAPMM06E'      '0220'          ,
        ' ' 'BDC_OKCODE'    '=BU'           .
      PERFORM BDC_PASS USING:
        'X' 'SAPLSPO1'      '0300'          ,
        ' ' 'BDC_OKCODE'    '=YES'          .
      CALL TRANSACTION 'ME32L'
            USING IT_BDC
             MODE W_STATUS
           UPDATE'S'
         MESSAGES INTO IT_MESSAGE.
    ENDAT.
    CLEAR WA_TMP_PO_DETAIL.
  ENDLOOP.
ENDFORM.                    " CONDITION_CHANGE
*&---------------------------------------------------------------------*
*&      Form  FIRST_SCREEN_ME32L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_TMP_PO_DETAIL  text
*----------------------------------------------------------------------*
FORM FIRST_SCREEN_ME32L USING P_PO_DETAIL STRUCTURE ZVMM_PO_DETAIL1.
  CLEAR : IT_BDC, IT_MESSAGE.
  REFRESH : IT_BDC, IT_MESSAGE.
  PERFORM BDC_PASS USING:
          'X' 'SAPMM06E'    '0205',
          ' ' 'RM06E-EVRTN' P_PO_DETAIL-EBELN,
          ' ' 'BDC_OKCODE'  '=AB'.

ENDFORM.                    " FIRST_SCREEN_ME32L
*&---------------------------------------------------------------------*
*&      Form  BDC_PASS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0010   text
*      -->P_0011   text
*      -->P_0012   text
*----------------------------------------------------------------------*
FORM BDC_PASS USING PAR1 PAR2 PAR3.
  CLEAR IT_BDC.
  IF PAR1 = 'X'.
    IT_BDC-DYNBEGIN = 'X'.
    IT_BDC-PROGRAM  = PAR2.
    IT_BDC-DYNPRO   = PAR3.
    APPEND IT_BDC.
  ELSE.
    IT_BDC-FNAM = PAR2.
    IT_BDC-FVAL = PAR3.
    APPEND IT_BDC.
  ENDIF.
ENDFORM.                    " BDC_PASS
*&---------------------------------------------------------------------*
*&      Form  REMAIND_ME32L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_TMP_PO_DETAIL  text
*----------------------------------------------------------------------*
FORM REMAIND_ME32L USING P_PO_DETAIL STRUCTURE ZVMM_PO_DETAIL1.
  DATA   : DATE_CHK ,
           CONDITION_INDEX(02) TYPE N                 ,
           IT_A016 LIKE A016 OCCURS 0 WITH HEADER LINE.
  CHECK P_PO_DETAIL-LOEKZ1 <> 'L'.
*Check valid date between inforecord and pocondition.
* [ 1 ] Get Inforecord Condition
  READ TABLE IT_A017 WITH KEY KAPPL = 'M'
                              KSCHL = 'PB00'
                              LIFNR = P_PO_DETAIL-LIFNR
                              MATNR = P_PO_DETAIL-MATNR
                              EKORG = P_PO_DETAIL-EKORG
                              WERKS = P_PO_DETAIL-WERKS
                              ESOKZ = '0'.
  CHECK SY-SUBRC = 0.
* [ 2 ] Get PO condition
  SELECT * FROM A016
           INTO CORRESPONDING FIELDS OF TABLE IT_A016
          WHERE KAPPL = 'M'
            AND KSCHL = 'PB00'
            AND EVRTN = P_PO_DETAIL-EBELN
            AND EVRTP = P_PO_DETAIL-EBELP.
* [ 3 ] Compare Inforecord Condition and PO condition
  CLEAR CONDITION_INDEX.
  LOOP AT IT_A016.
    IF IT_A017-DATBI = IT_A016-DATBI AND
       IT_A017-DATAB = IT_A016-DATAB.
      DATE_CHK = 'X'.      "Valid date is exist.
      CONDITION_INDEX = SY-TABIX.
      EXIT.
    ENDIF.
  ENDLOOP.
* Perform BDC
  PERFORM CONDITON_UPDATE USING DATE_CHK        "valid / not valid date
                                CONDITION_INDEX "position condition day
                                P_PO_DETAIL     "po
                                IT_A017         "condition record no.
                                IT_A016       . "po condition record.
ENDFORM.                    " REMAIND_ME32L
*&---------------------------------------------------------------------*
*&      Form  CONDITON_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATE_CHK  text
*      -->P_CONDITION_INDEX  text
*----------------------------------------------------------------------*
FORM CONDITON_UPDATE USING    P_CHK
                              P_INDEX
                              P_PO_DETAIL STRUCTURE ZVMM_PO_DETAIL1
                              P_A017      STRUCTURE A017
                              P_A016      STRUCTURE A016.

  DATA : FIELD(20), KZUST LIKE KONH-KZUST,
         CNT(02) TYPE N                  ,
         FIELD1(20)                      ,
         FIELD2(20)                      ,
         DATES(10)                       ,
         DATEE(10)                       ,
         KBETR(13)                       ,
         W_LINES TYPE I                  .

  DATA : BEGIN OF IT_KONP_INFO OCCURS 0.
          INCLUDE STRUCTURE KONP.
  DATA : END OF IT_KONP_INFO.

  DATA : BEGIN OF IT_KONP_PO OCCURS 0,
          KNUMH LIKE KONP-KNUMH      ,
          KSCHL LIKE KONP-KSCHL      , "Condition type
          KOPOS LIKE KONP-KOPOS      , "Sequential number of the
                                       "condition
          KBETR LIKE KONP-KBETR      ,
          KONWA LIKE KONP-KONWA      ,
          LOEVM_KO LIKE KONP-LOEVM_KO, "Delete flag
          NEW_FLG                    ,
         END OF IT_KONP_PO.

* Get Reason Code
  SELECT SINGLE KZUST
           INTO KZUST
           FROM KONH
          WHERE KNUMH = P_A017-KNUMH.

  SELECT * FROM KONP
           INTO CORRESPONDING FIELDS OF TABLE IT_KONP_INFO
          WHERE KNUMH = P_A017-KNUMH.
  SELECT * FROM KONP
           INTO CORRESPONDING FIELDS OF TABLE IT_KONP_PO
          WHERE KNUMH = P_A016-KNUMH.
      WRITE : P_A017-DATAB TO DATES,
              P_A017-DATBI TO DATEE.

  PERFORM BDC_PASS USING:
       'X' 'SAPMM06E'     '0220'           ,
       ' ' 'RM06E-EBELP'  P_PO_DETAIL-EBELP,
       ' ' 'BDC_OKCODE'   '/00'            .

  PERFORM BDC_PASS USING:
       'X' 'SAPMM06E'             '0220'   ,
       ' ' 'RM06E-TCSELFLAG(01)'  'X'      ,
       ' ' 'BDC_OKCODE'           '=KO'    .

  CASE P_CHK.
************************************************************************
* Change Condition
************************************************************************
    WHEN 'X'.     "Condition exist       .
* Adjust IT_KONP_PO.
      SORT IT_KONP_PO BY KNUMH KSCHL KOPOS.
      LOOP AT IT_KONP_INFO WHERE LOEVM_KO <> 'X'.
        CLEAR IT_KONP_PO.
        READ TABLE IT_KONP_PO WITH KEY KSCHL    = IT_KONP_INFO-KSCHL
                                       LOEVM_KO = SPACE            .
        IF SY-SUBRC = 0.
          IT_KONP_PO-KBETR = IT_KONP_INFO-KBETR.
          MODIFY IT_KONP_PO TRANSPORTING KBETR
                                   WHERE KNUMH = IT_KONP_PO-KNUMH
                                     AND KOPOS = IT_KONP_PO-KOPOS.
        ELSE.
          IT_KONP_PO-KSCHL   = IT_KONP_INFO-KSCHL.
          IT_KONP_PO-KBETR   = IT_KONP_INFO-KBETR.
          IT_KONP_PO-NEW_FLG = 'X'.
          APPEND IT_KONP_PO.
        ENDIF.
      ENDLOOP.
* make Field
      CONCATENATE 'VAKE-DATAB(' P_INDEX ')' INTO FIELD.
* Select Date
      PERFORM BDC_PASS USING:
        'X' 'SAPLV14A'      '0102'          ,
        ' ' 'BDC_CURSOR'    FIELD           ,
        ' ' 'BDC_OKCODE'    '=PICK'         .
* Input Condition
      PERFORM BDC_PASS USING:
        'X' 'SAPMV13A'      '0201'          ,
        ' ' 'RV13A-DATAB'   DATES           ,
        ' ' 'RV13A-DATBI'   DATEE           .
      LOOP AT IT_KONP_PO.
        CASE IT_KONP_PO-KONWA.
         WHEN '%'.
             WRITE : IT_KONP_PO-KBETR TO KBETR.
         WHEN OTHERS.
             WRITE :
               IT_KONP_PO-KBETR TO KBETR CURRENCY IT_KONP_PO-KONWA.
        ENDCASE.
        IF IT_KONP_PO-LOEVM_KO <> 'X'
           AND IT_KONP_PO-NEW_FLG <> 'X'.
          CNT = SY-TABIX.
          CONCATENATE 'KONP-KBETR(' CNT ')' INTO FIELD2. "Rate.
          PERFORM BDC_PASS USING:
            ' ' FIELD2          KBETR.
        ELSEIF IT_KONP_PO-LOEVM_KO <> 'X'
           AND IT_KONP_PO-NEW_FLG = 'X'.
          CNT = SY-TABIX.
         CONCATENATE 'KONP-KSCHL(' CNT ')' INTO FIELD1. "Condition Type.
          CONCATENATE 'KONP-KBETR(' CNT ')' INTO FIELD2. "Rate.
          PERFORM BDC_PASS USING:
            ' ' FIELD1          IT_KONP_PO-KSCHL,
            ' ' FIELD2          KBETR.
        ENDIF.
      ENDLOOP.
      PERFORM BDC_PASS USING:
        ' ' 'BDC_OKCODE'    '=KDAT'         .
* Input Reason Code
      PERFORM BDC_PASS USING:
        'X' 'SAPMV13A'      '0200'          ,
        ' ' 'KONH-KZUST'    KZUST           ,
        ' ' 'BDC_OKCODE'    '=BACK'          .
*      PERFORM BDC_PASS USING:
*        'X' 'SAPLSPO1'      '0300'          ,
*        ' ' 'BDC_OKCODE'    '=YES'          .

************************************************************************
* Input New Condition
************************************************************************
    WHEN OTHERS.  "Condition dose'nt exist.
      PERFORM BDC_PASS USING:
        'X' 'SAPLV14A'      '0102'          ,
        ' ' 'BDC_OKCODE'    '=NEWD'         .
* Input Condition
      PERFORM BDC_PASS USING:
        'X' 'SAPMV13A'      '0201'          ,
        ' ' 'RV13A-DATAB'   DATES           ,
        ' ' 'RV13A-DATBI'   DATEE           .
      LOOP AT IT_KONP_INFO WHERE LOEVM_KO <> 'X'.
        CASE IT_KONP_INFO-KONWA.
         WHEN '%'.
             WRITE : IT_KONP_INFO-KBETR TO KBETR.
         WHEN OTHERS.
             WRITE :
               IT_KONP_INFO-KBETR TO KBETR CURRENCY IT_KONP_INFO-KONWA.
        ENDCASE.
        CNT = SY-TABIX.
        CONCATENATE 'KONP-KSCHL(' CNT ')' INTO FIELD1. "Condition Type.
        CONCATENATE 'KONP-KBETR(' CNT ')' INTO FIELD2. "Rate.
        PERFORM BDC_PASS USING:
          ' ' FIELD1          IT_KONP_INFO-KSCHL,
          ' ' FIELD2          KBETR.
      ENDLOOP.
      PERFORM BDC_PASS USING:
        ' ' 'BDC_OKCODE'    '=KDAT'         .
* Input Reason Code
      PERFORM BDC_PASS USING:
        'X' 'SAPMV13A'      '0200'          ,
        ' ' 'KONH-KZUST'    KZUST           ,
        ' ' 'BDC_OKCODE'    '=BACK'          .
*      PERFORM BDC_PASS USING:
*        'X' 'SAPLSPO1'      '0300'          ,
*        ' ' 'BDC_OKCODE'    '=YES'          .
  ENDCASE.
ENDFORM.                    " CONDITON_UPDATE
