*----------------------------------------------------------------------*
*   INCLUDE ZXCKAU01
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
INCLUDE zxckau00_data.
DATA: w_werks LIKE keko-werks.

IF syst-tcode(4) = 'CK13'.
**Check data saved
*  IF f_ckiuser-sobes = '7'. "Stock Trf.
*    w_werks = f_ckiuser-sowrk.
*  ELSE.
*    w_werks = f_ckiuser-werks.
*  ENDIF.
*
*  SELECT SINGLE * INTO corresponding fields of f_ckiuser
*     FROM keko
*     WHERE matnr = f_ckiuser-matnr
*       AND werks = w_werks
*       AND klvar = f_ckiuser-klvar
*       AND kadky = f_ckiuser-kadky.  "costing from
*  move-corresponding w_keko to f_ckiuser.
  INCLUDE zxckau00_read_db.
ELSE.
  INCLUDE zxckau00_read.
ENDIF.

*Module(Color) filtering
INCLUDE zxckau00_module_color.

* /////////////////////////////////////
* get vendor when if costing type is 'UN'.
* { ///////////////////////////////////
IF f_ckiuser-kalka EQ 'UN'.

  DATA: BEGIN OF it_lifnr OCCURS 21,
          lifnr TYPE lifnr,
          land1 TYPE land1,
        END   OF it_lifnr.
  DATA: l_lifnr TYPE lifnr,
        l_land1 TYPE land1.
  DATA $ix LIKE sy-tabix.
  LOOP AT itab.
    $ix = sy-tabix.
    CLEAR : it_lifnr, it_lifnr[], l_lifnr, l_land1.

    SELECT a~lifnr b~land1 INTO TABLE it_lifnr
                 FROM ztcou137 AS a
                 INNER JOIN lfa1 AS b
                    ON a~lifnr = b~lifnr
                WHERE a~bukrs EQ f_ckiuser-bukrs
                  AND a~matnr EQ itab-compn
                  AND a~zdtfr <= f_ckiuser-bwdat
                  AND a~zdtto >= f_ckiuser-bwdat .

*---multiple vendor - take KD vendor
    READ TABLE it_lifnr INDEX 2.
    IF sy-subrc EQ 0.
      LOOP AT it_lifnr.
        IF it_lifnr-land1 <> 'US'.
          l_lifnr = it_lifnr-lifnr.
          l_land1 = it_lifnr-land1.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF l_lifnr EQ space.
      READ TABLE it_lifnr INDEX 1.
      IF sy-subrc EQ 0.
        l_lifnr = it_lifnr-lifnr.
        l_land1 = it_lifnr-land1.
      ENDIF.
    ENDIF.

    itab-lifnr = l_lifnr.
    itab-land1 = l_land1.
    MODIFY itab INDEX $ix TRANSPORTING lifnr land1.

  ENDLOOP.


ENDIF.
* ///////////////////////////////////////
* }
* ///////////////////////////////////////

*----------------------------------------------------------------------*

DATA: l_answer(1) TYPE c.
TABLES: ztco_ck11.
CALL FUNCTION 'POPUP_TO_CONFIRM'
     EXPORTING
          text_question = 'Do you want to save?'
          text_button_1 = 'Yes'
          text_button_2 = 'No'
     IMPORTING
          answer        = l_answer.

IF l_answer = '1'.
*save to table
  IF f_ckiuser-klvar = 'ZUNF'.
    DELETE FROM ztco_ck11 WHERE
         kokrs  = f_ckiuser-kokrs
     AND klvar  = f_ckiuser-klvar
     AND bdatj  = f_ckiuser-bdatj
     AND poper  = f_ckiuser-poper
     AND artnr  = f_ckiuser-matnr
     AND verid  = f_ckiuser-verid.
  ELSE.
    DELETE FROM ztco_ck11 WHERE
         kokrs  = f_ckiuser-kokrs
     AND klvar  = f_ckiuser-klvar
     AND bdatj  = f_ckiuser-bdatj
     AND poper  = f_ckiuser-poper
     AND artnr  = f_ckiuser-matnr
     AND verid  = f_ckiuser-verid
     AND refdt  = f_ckiuser-bwdat.
  ENDIF.

  IF sy-binpt IS INITIAL.
    COMMIT WORK.
  endif.

  LOOP AT itab.
    MOVE-CORRESPONDING itab TO ztco_ck11.

    ztco_ck11-klvar = f_ckiuser-klvar.
    IF ztco_ck11-klvar = 'ZUNF'.
    ELSE.
      ztco_ck11-refdt = f_ckiuser-bwdat.
    ENDIF.

    ztco_ck11-indx  = itab-index.

* determine PM indicator... (costing type is 'UN'???)
* FIXME
*    if ztco_ck11-STRAT = space.
*      ztco_ck11-STKKZ = 'X'.
*    endif.

    ztco_ck11-hwaer = f_ckiuser-hwaer.
*...costing lot-size
    ztco_ck11-losgr = f_ckiuser-losgr.
    ztco_ck11-meins = f_ckiuser-meins.

    ztco_ck11-aedat = sy-datum.
    ztco_ck11-aenam = sy-uname.
    INSERT ztco_ck11.
  ENDLOOP.
ENDIF.
