*&---------------------------------------------------------------------*
* ZCKML_RESET_STATUS_OF_PERIOD
* ============================
* Allows the status to be reset to 'Period opened' (10) under the
* following conditions:
*
* o  Goods movements are completely reversed
*
* o  The collected price differences and exchange rate differences are
*    zero in all currencies. Generally, this is also possible by
*    reversing all transactions that have been carried out.
*
* o  The status of the previous period is not 'Closing entry
*    performed'.
*
* o  The material is not locked by another user.
*
* o  The relevant period is the current period. After resetting the
*    status, it is possible to carry out price changes in the period
*    again.
*
* Passcode : HMMA
*&---------------------------------------------------------------------*
REPORT z_ml_ckmlpp_status_set.

TABLES : ckmlpp, zckmlpp, mara.

DATA : it_ckmlpp LIKE STANDARD TABLE OF ckmlpp
                 WITH HEADER LINE .

DATA : it_zckmlpp LIKE STANDARD TABLE OF zckmlpp
                 WITH HEADER LINE .
DATA : it_ckmlhd LIKE STANDARD TABLE OF ckmlhd
                 WITH HEADER LINE .

PARAMETERS : p_bdatj LIKE ckmlpp-bdatj OBLIGATORY,
             p_poper LIKE ckmlpp-poper OBLIGATORY.

SELECTION-SCREEN SKIP.

SELECT-OPTIONS : s_matnr FOR mara-matnr OBLIGATORY.
PARAMETERS : p_1030 RADIOBUTTON GROUP ra01,
             p_3010 RADIOBUTTON GROUP ra01.

SELECTION-SCREEN SKIP.
PARAMETERS : p_chk(4) OBLIGATORY.

START-OF-SELECTION.

  CLEAR : it_ckmlhd, it_ckmlhd[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ckmlhd
           FROM ckmlhd
           WHERE matnr IN s_matnr .

  IF it_ckmlhd[] IS INITIAL.
    MESSAGE e000(zmco) WITH text-010.
  ENDIF.

  IF    p_chk <> 'HMMA'.
    MESSAGE e000(zmco) WITH text-011.
  ENDIF.

  CLEAR : it_ckmlpp,  it_ckmlpp[].
  CLEAR : it_zckmlpp, it_zckmlpp[].


  DATA : lv_bf_sta LIKE ckmlpp-status.
  DATA : lv_af_sta LIKE ckmlpp-status.

  CASE 'X'.
    WHEN p_3010 . lv_bf_sta = '30'. lv_af_sta = '10'.
    WHEN p_1030 . lv_bf_sta = '10'. lv_af_sta = '30'.
  ENDCASE.

*

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ckmlpp
           FROM ckmlpp
           FOR ALL ENTRIES IN  it_ckmlhd
          WHERE bdatj = p_bdatj
            AND poper = p_poper
            AND kalnr = it_ckmlhd-kalnr
            AND status = lv_bf_sta.


  LOOP AT it_ckmlpp.
    CLEAR ckmlpp.
    MOVE-CORRESPONDING it_ckmlpp TO ckmlpp.
    ckmlpp-status = lv_af_sta.

    UPDATE ckmlpp.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE e000(zmco) WITH 'Update Failure'.
    ELSE.
* Log to table
      DELETE  FROM zckmlpp
                WHERE bdatj = p_bdatj
                  AND poper = p_poper
                  AND kalnr = it_ckmlpp-kalnr.
      INSERT  zckmlpp FROM it_ckmlpp.

      IF sy-subrc <> 0.
        ROLLBACK WORK.
        MESSAGE e000(zmco) WITH 'Update Failure'.
      ELSE.
        WRITE:/ '*** Status is changed to', lv_af_sta.
        WRITE:/ '*** Data is saved to ZCKMLPP'.
      ENDIF.

    ENDIF.
    CLEAR  it_ckmlpp.
  ENDLOOP.

*  it_zckmlpp[] = it_ckmlpp[].
*
*  DELETE  FROM zckmlpp
*            WHERE bdatj = p_bdatj
*              AND poper = p_poper.
*  INSERT  zckmlpp FROM TABLE it_zckmlpp.
