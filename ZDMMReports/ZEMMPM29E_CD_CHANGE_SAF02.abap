****************************************************************
* Program Name  : ZEMMPM29E_CD_CHANGE_SA
* Created  by   : Min-su Park
* Creation on   : 2003.10.27.
* Pattern       :
* Description   : Condition change in Schedule Agreement
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.19.     Min-su Park    UD1K901873     Initial Coding
*&---------------------------------------------------------------------*
* Date            Developer        RequestNo      Description
* 2004.02.04.     Jaesung Lee    UD1K906915     Changed condition logic
* Condition logic changed: plant leavel =>  Purchasing group
*&---------------------------------------------------------------------*


************************************************************************

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
FORM get_data.

*&<<<<-----------------------------------------------------------------*
* insert by jaesung lee on 04/02/2004
*&<<<<-----------------------------------------------------------------*

  CHECK sy-ucomm = 'ONLI'.

*select ko~EBELN po~EBELP ko~LOEKZ po~MATNR
*       po~WERKS po~LGORT ko~LIFNR po~ETFZ1
*       ko~BSTYP ko~BUKRS ko~BSART ko~EKORG
*       ko~EKGRP po~LOEKZ as LOEKZ1
*
*    into corresponding fields of table it_po_detail
*
*    from ekko  as ko inner join ekpo as po
*                on ko~ebeln = po~ebeln
*
*    where ko~ebeln in s_ebeln
*      and ko~BSTYP = 'L'        " Purchasing document category
*                                " schedule agreement 'L'
*      and ko~LOEKZ = ''
*      and po~LOEKZ = ''  .
*
*   if sy-subrc ne 0.
*      message e013(zmmm) with 'DATA'.
*   endif.

*&<<<<-----------------------------------------------------------------*
* end of insert.
*&<<<<-----------------------------------------------------------------*

*--- Read Current scheduling agreement Data.
  SELECT * FROM zvmm_po_detail1
           INTO CORRESPONDING FIELDS OF TABLE it_po_detail
          WHERE ebeln IN s_ebeln
            AND bstyp EQ 'L'
            AND loekz1 EQ space
            AND loekz EQ space
            AND elikz EQ space
            AND ( kdatb LE p_datum
              AND kdate GE p_datum ).

* Read Condition of Current Info record Record Data
*  SELECT * FROM A017
*           INTO CORRESPONDING FIELDS OF TABLE IT_A017
*          WHERE DATAB <= SY-DATUM
*            AND DATBI >= SY-DATUM.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CONDITION_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM condition_change.
**---
  SORT it_po_detail BY ebeln ebelp.

  LOOP AT it_po_detail.
    MOVE-CORRESPONDING it_po_detail TO wa_tmp_po_detail.
    PERFORM remaind_me32l USING wa_tmp_po_detail.
    CLEAR : wa_tmp_po_detail.
  ENDLOOP.
ENDFORM.                    " CONDITION_CHANGE
*&---------------------------------------------------------------------*
*&      Form  FIRST_SCREEN_ME32L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_TMP_PO_DETAIL  text
*----------------------------------------------------------------------*
FORM first_screen_me32l USING p_po_detail STRUCTURE zvmm_po_detail1.
**---
  CLEAR : it_bdc, it_message.
  REFRESH : it_bdc, it_message.

**---
  PERFORM bdc_pass USING :
          'X' 'SAPMM06E'    '0205',
          ' ' 'RM06E-EVRTN' p_po_detail-ebeln,
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
FORM bdc_pass USING par1 par2 par3.
  CLEAR it_bdc.
  IF par1 = 'X'.
    it_bdc-dynbegin = 'X'.
    it_bdc-program  = par2.
    it_bdc-dynpro   = par3.
    APPEND it_bdc.
  ELSE.
    it_bdc-fnam = par2.
    it_bdc-fval = par3.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " BDC_PASS
*&---------------------------------------------------------------------*
*&      Form  REMAIND_ME32L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_TMP_PO_DETAIL  text
*----------------------------------------------------------------------*
FORM remaind_me32l USING p_po_detail STRUCTURE zvmm_po_detail1.
**---
  DATA : date_chk,
         condition_index(02) TYPE n.

  DATA : BEGIN OF it_a016 OCCURS 0.
          INCLUDE STRUCTURE a016.
  DATA :   status,
         END OF it_a016.

  DATA : wa_a016 LIKE a016.

  DATA : sy_indx(02) TYPE n.

*-- Check valid date between inforecord and po condition.

*--- [ 1 ] Get Inforecord Condition

*&<<<<-----------------------------------------------------------------*
* insert by jaesung lee on 04/02/2004
*  Material Info Record strategic change a017 table => a018 table
*&<<<<-----------------------------------------------------------------*

  CLEAR : it_a017, it_a017[].

  SELECT * FROM a018
           INTO CORRESPONDING FIELDS OF TABLE it_a017
          WHERE kappl = 'M'
            AND kschl = 'PB00'
            AND lifnr = p_po_detail-lifnr
            AND matnr = p_po_detail-matnr
            AND ekorg = p_po_detail-ekorg
            AND esokz = '0'
            AND ( datab LE p_datum
              AND datbi GE p_datum ).

*&<<<<-----------------------------------------------------------------*
* delete by jaesung lee on 04/02/2004
*&<<<<-----------------------------------------------------------------
*CLEAR : it_a017, it_a017[].
*  SELECT * FROM a017
*           INTO CORRESPONDING FIELDS OF TABLE it_a017
*          WHERE kappl = 'M'
*            AND kschl = 'PB00'
*            AND lifnr = p_po_detail-lifnr
*            AND matnr = p_po_detail-matnr
*            AND ekorg = p_po_detail-ekorg
*            AND werks = p_po_detail-werks
*            AND esokz = '0'.
*&<<<<-----------------------------------------------------------------*
* end of delete
*&<<<<-----------------------------------------------------------------

  CHECK sy-subrc = 0.

*--- [ 2 ] Get PO condition
  SELECT * FROM a016
           INTO CORRESPONDING FIELDS OF TABLE it_a016
          WHERE kappl = 'M'
            AND kschl = 'PB00'
            AND evrtn = p_po_detail-ebeln
            AND evrtp = p_po_detail-ebelp
            AND ( datab LE p_datum
              AND datbi GE p_datum ).

  SORT : it_a017 BY datab,
         it_a016 BY datab.

*--- [ 3 ] Status Decision.
*--- 'S' = same ,'O' = overlapping AND New.
  LOOP AT it_a017.
    sy_indx = sy-tabix.
    CLEAR : wa_a016.
    PERFORM first_screen_me32l USING p_po_detail.
    READ TABLE it_a016 INDEX sy_indx.
    CASE sy-subrc.
      WHEN 0.
        PERFORM conditon_update
                          USING 'O'            "valid / not valid date
                               sy_indx         "position condition day
                               p_po_detail     "po
                               it_a017         "condition record no.
                               it_a016       . "po condition record.
      WHEN OTHERS. "New
        PERFORM conditon_update
                          USING 'N'             "valid / not valid date
                                sy_indx         "position condition day
                                p_po_detail     "po
                                it_a017         "condition record no.
                                it_a016       . "po condition record.
    ENDCASE.

    PERFORM bdc_pass USING :
            'X' 'SAPMM06E'      '0220',
            ' ' 'BDC_OKCODE'    '=BU'.

    PERFORM bdc_pass USING :
            'X' 'SAPLSPO1'      '0300',
            ' ' 'BDC_OKCODE'    '=YES'.

    CALL TRANSACTION 'ME32L' USING it_bdc
                              MODE w_status
                            UPDATE 'S'
                          MESSAGES INTO it_message.
  ENDLOOP.

** [ 4 ] Compare Inforecord Condition and PO condition
*  CLEAR CONDITION_INDEX.
*  LOOP AT IT_A016.
*    IF IT_A017-DATBI = IT_A016-DATBI AND
*       IT_A017-DATAB = IT_A016-DATAB.
*      DATE_CHK = 'X'.      "Valid date is exist.
*      CONDITION_INDEX = SY-TABIX.
**     EXIT.
*    ELSE.
*      CLEAR DATE_CHK.
*    ENDIF.
*   PERFORM FIRST_SCREEN_ME32L USING P_PO_DETAIL.
*   PERFORM CONDITON_UPDATE USING DATE_CHK        "valid / not valid
*date
*                                 CONDITION_INDEX "position condition
*day
*                                 P_PO_DETAIL     "po
*                                 IT_A017         "condition record no.
*                                 IT_A016       . "po condition record.
*      PERFORM BDC_PASS USING:
*        'X' 'SAPMM06E'      '0220'          ,
*        ' ' 'BDC_OKCODE'    '=BU'           .
*      PERFORM BDC_PASS USING:
*        'X' 'SAPLSPO1'      '0300'          ,
*        ' ' 'BDC_OKCODE'    '=YES'          .
*      CALL TRANSACTION 'ME32L'
*            USING IT_BDC
*             MODE W_STATUS
*           UPDATE'S'
*         MESSAGES INTO IT_MESSAGE.
*
*  ENDLOOP.
** Perform BDC
**  PERFORM CONDITON_UPDATE USING DATE_CHK        "valid / not valid
*date
**                                CONDITION_INDEX "position condition
*day
**                                P_PO_DETAIL     "po
**                                IT_A017         "condition record no.
**                                IT_A016       . "po condition record.
ENDFORM.                    " REMAIND_ME32L
*&---------------------------------------------------------------------*
*&      Form  CONDITON_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATE_CHK  text
*      -->P_CONDITION_INDEX  text
*----------------------------------------------------------------------*
FORM conditon_update USING    p_chk
                              p_index
                              p_po_detail STRUCTURE zvmm_po_detail1
                              p_a017      STRUCTURE a017
                              p_a016      STRUCTURE a016.

**---
  DATA : field(20),
         kzust LIKE konh-kzust,
         cnt(02) TYPE n,
         field1(20),
         field2(20),
         dates(10),
         datee(10),
         kbetr(13),
         w_lines TYPE i.

  DATA : BEGIN OF it_konp_info OCCURS 0.
          INCLUDE STRUCTURE konp.
  DATA : END OF it_konp_info.

  DATA : BEGIN OF it_konp_po OCCURS 0,
          knumh LIKE konp-knumh,
          kschl LIKE konp-kschl, "Condition type
          kopos LIKE konp-kopos, "Sequential number of the condition
          kbetr LIKE konp-kbetr,
          konwa LIKE konp-konwa,
          loevm_ko LIKE konp-loevm_ko, "Delete flag
          new_flg,
         END OF it_konp_po.

*--- Get Reason Code
  SELECT SINGLE kzust INTO kzust
                      FROM konh
                     WHERE knumh = p_a017-knumh.

*---
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_konp_info
           FROM konp
          WHERE knumh = p_a017-knumh.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_konp_po
           FROM konp
          WHERE knumh = p_a016-knumh.

  WRITE : p_a017-datab TO dates,
          p_a017-datbi TO datee.

  PERFORM bdc_pass USING :
          'X' 'SAPMM06E'     '0220',
          ' ' 'RM06E-EBELP'  p_po_detail-ebelp,
          ' ' 'BDC_OKCODE'   '/00'.

  PERFORM bdc_pass USING :
          'X' 'SAPMM06E'             '0220',
          ' ' 'RM06E-TCSELFLAG(01)'  'X',
          ' ' 'BDC_OKCODE'           '=KO'.

  CASE p_chk.
************************************************************************
* Input New Condition
************************************************************************
    WHEN 'N'.  " Condition does not exist.
      PERFORM bdc_pass USING :
              'X' 'SAPLV14A'      '0102',
              ' ' 'BDC_OKCODE'    '=NEWD'.

*--- Input Condition
      PERFORM bdc_pass USING :
              'X' 'SAPMV13A'      '0201',
              ' ' 'RV13A-DATAB'   dates,
              ' ' 'RV13A-DATBI'   datee.

      LOOP AT it_konp_info WHERE loevm_ko <> 'X'.
        CASE it_konp_info-konwa.
          WHEN '%'.
*            WRITE : IT_KONP_INFO-KBETR TO KBETR.
            MOVE : it_konp_info-kbetr TO kbetr.
          WHEN OTHERS.
            WRITE :
              it_konp_info-kbetr TO kbetr CURRENCY it_konp_info-konwa.
        ENDCASE.
        cnt = sy-tabix.
        IF cnt > 1.
         CONCATENATE 'KONP-KSCHL(' cnt ')' INTO field1. "Condition Type.
        ENDIF.
        CONCATENATE 'KONP-KBETR(' cnt ')' INTO field2. "Rate.
        PERFORM bdc_pass USING :
                ' ' field1          it_konp_info-kschl,
                ' ' field2          kbetr.
      ENDLOOP.

      PERFORM bdc_pass USING :
              ' ' 'BDC_OKCODE'    '=KDAT'.

*--- Input Reason Code
      PERFORM bdc_pass USING :
              'X' 'SAPMV13A'      '0200',
              ' ' 'KONH-KZUST'    kzust,
              ' ' 'BDC_OKCODE'    '=BACK'.

*      PERFORM BDC_PASS USING:
*        'X' 'SAPLSPO1'      '0300'          ,
*        ' ' 'BDC_OKCODE'    '=YES'          .

************************************************************************
* Change Condition
************************************************************************
    WHEN OTHERS.     "Condition exist
*--- Adjust IT_KONP_PO.
      SORT it_konp_po BY knumh kschl kopos.

      LOOP AT it_konp_info WHERE loevm_ko <> 'X'.
        CLEAR : it_konp_po.
        READ TABLE it_konp_po WITH KEY kschl    = it_konp_info-kschl
                                       loevm_ko = space.
        IF sy-subrc = 0.
          it_konp_po-kbetr = it_konp_info-kbetr.
          MODIFY it_konp_po TRANSPORTING kbetr
                                   WHERE knumh = it_konp_po-knumh
                                     AND kopos = it_konp_po-kopos.
        ELSE.
          it_konp_po-kschl   = it_konp_info-kschl.
          it_konp_po-kbetr   = it_konp_info-kbetr.
          it_konp_po-new_flg = 'X'.
          APPEND it_konp_po.
        ENDIF.
      ENDLOOP.

*--- make Field
      CONCATENATE 'VAKE-DATAB(' p_index ')' INTO field.

*--- Select Date
      PERFORM bdc_pass USING :
              'X' 'SAPLV14A'      '0102',
              ' ' 'BDC_CURSOR'    field,
              ' ' 'BDC_OKCODE'    '=PICK'.

*--- Input Condition
      PERFORM bdc_pass USING :
              'X' 'SAPMV13A'      '0201',
              ' ' 'RV13A-DATAB'   dates,
              ' ' 'RV13A-DATBI'   datee.

      LOOP AT it_konp_po.
        CASE it_konp_po-konwa.
          WHEN '%'.
*            WRITE : it_konp_po-kbetr TO kbetr.
            MOVE : it_konp_po-kbetr TO kbetr.
          WHEN OTHERS.
            WRITE :
              it_konp_po-kbetr TO kbetr CURRENCY it_konp_po-konwa.
        ENDCASE.

        IF it_konp_po-loevm_ko <> 'X' AND it_konp_po-new_flg <> 'X'.
          cnt = sy-tabix.
          CONCATENATE 'KONP-KBETR(' cnt ')' INTO field2. "Rate.
          PERFORM bdc_pass USING :
                  ' ' field2          kbetr.
        ELSEIF it_konp_po-loevm_ko <> 'X' AND it_konp_po-new_flg = 'X'.
          cnt = sy-tabix.
          CONCATENATE 'KONP-KSCHL(' cnt ')' INTO field1. "Condition Type
          CONCATENATE 'KONP-KBETR(' cnt ')' INTO field2. "Rate.
          PERFORM bdc_pass USING :
                  ' ' field1          it_konp_po-kschl,
                  ' ' field2          kbetr.
        ENDIF.
      ENDLOOP.

      PERFORM bdc_pass USING :
              ' ' 'BDC_OKCODE'    '=KDAT'.

*--- Input Reason Code
      PERFORM bdc_pass USING :
              'X' 'SAPMV13A'      '0200',
              ' ' 'KONH-KZUST'    kzust,
              ' ' 'BDC_OKCODE'    '=BACK'.

*      PERFORM BDC_PASS USING:
*        'X' 'SAPLSPO1'      '0300'          ,
*        ' ' 'BDC_OKCODE'    '=YES'          .
  ENDCASE.
ENDFORM.                    " CONDITON_UPDATE

*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_data.

ENDFORM.                    " write_data
