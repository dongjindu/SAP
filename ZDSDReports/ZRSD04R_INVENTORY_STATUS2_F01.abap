*----------------------------------------------------------------------*
***INCLUDE ZRSD04R_INVENTORY_STATUS2_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM read_data.
  PERFORM get_atinn USING 'P_RP_STATUS'.

  SELECT *
         INTO TABLE it_ausp
         FROM ausp
        WHERE objek IN ( select OBJEK
                                from AUSP
                               WHERE atinn = w_atinn
                                 AND klart = '002'
                                 AND atwrt IN ('19',   "C/GATE
                                               '20',   "UNDER BODY COA
                                               '21',   "DEALER ALLO
                                               '22',   "VPC IN
                                               '23',   "VPC OUT
                                               '24',   "TRUCKING IN
                                               '26',
                                               '27') ) "RAILING IN
          and ATINN in R_ATINN
          AND klart = '002'.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  INIT_CABN
*&---------------------------------------------------------------------*
FORM init_cabn.
  REFRESH : it_cabn, r_atinn.
  CLEAR   : it_cabn, r_atinn.

  r_atinn-sign = 'I'.
  r_atinn-option = 'EQ'.

  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_RP_STATUS'.
  APPEND it_cabn.
* R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_RP19_ACTUAL_DATE'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_RP20_ACTUAL_DATE'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_RP22_ACTUAL_DATE'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_RP23_ACTUAL_DATE'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_RP24_ACTUAL_DATE'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_RP26_ACTUAL_DATE'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_MODEL'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_BODY_SERIAL'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_WORK_ORDER'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_EXT_COLOR'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_INT_COLOR'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_MODEL_YEAR'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_MI'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_OCN'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_VIN'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_DEALER_NO'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_SALES_ORDER'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
ENDFORM.                    " INIT_CABN
*&---------------------------------------------------------------------*
*&      Form  GET_ATINN
*&---------------------------------------------------------------------*
FORM get_atinn USING p_atnam.
  READ TABLE it_cabn WITH KEY atnam = p_atnam.
  IF sy-subrc = 0.
    w_atinn = it_cabn-atinn.
  ENDIF.
ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  GET_ATNAM
*&---------------------------------------------------------------------*
FORM get_atnam USING p_atinn.
  READ TABLE it_cabn WITH KEY atinn = p_atinn.
  IF sy-subrc = 0.
    w_atnam = it_cabn-atnam.
  ENDIF.
ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data.
  DATA : w_objek LIKE ausp-objek.

  REFRESH it_inv_st2. CLEAR it_inv_st2.

  READ TABLE it_ausp INDEX 1.
  w_objek = it_ausp-objek.

  LOOP AT it_ausp.
    IF w_objek <> it_ausp-objek.
      APPEND t_inv_st2 TO it_inv_st2. CLEAR t_inv_st2.
      w_objek = it_ausp-objek.
    ENDIF.

    PERFORM get_atnam USING it_ausp-atinn.

    IF w_atnam = 'P_RP26_ACTUAL_DATE'.
      IF t_inv_st2-rp24_actual_date IS INITIAL.
        t_inv_st2-rp24_actual_date = it_ausp-atwrt.
      ENDIF.
    ELSE.
      CONCATENATE 'T_INV_ST2-' w_atnam+2(20) INTO field.
      ASSIGN (field) TO <fs>.
      <fs> = it_ausp-atwrt.
    ENDIF.
  ENDLOOP.
  IF sy-subrc = 0.
    APPEND t_inv_st2 TO it_inv_st2. CLEAR t_inv_st2.
  ENDIF.

  DELETE it_inv_st2 WHERE rp19_actual_date > sy-datum.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
FORM call_screen.
  SORT it_inv_st2 BY dealer_no.
  DESCRIBE TABLE it_inv_st2 LINES w_cnt.
  IF w_cnt = 0.
    MESSAGE i000 WITH text-m01.
  ELSE.
    CLEAR: w_shipped_cnt.
    LOOP AT it_inv_st2 INTO t_inv_st2
                      WHERE rp27_actual_date > '00000000'.
      w_shipped_cnt = w_shipped_cnt + 1.
    ENDLOOP.

    w_tot_cnt = w_cnt - w_shipped_cnt.
    CALL SCREEN 9000.
  ENDIF.
ENDFORM.                    " CALL_SCREEN
*&---------------------------------------------------------------------*
*&      Form  choice_collect_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM choice_collect_data.
  DATA: lw_inv_st2 LIKE zssd_inv_st2.

  DATA: lt_inv_st2 LIKE zssd_inv_st2 OCCURS 0 WITH HEADER LINE.

  lt_inv_st2[] = it_inv_st2[].

  CLEAR: it_inv_st2, it_inv_st2[].

  LOOP AT lt_inv_st2 WHERE rp19_actual_date IN s_date.
    READ TABLE s_kunnr INDEX 1.
    IF sy-subrc EQ 0.
      SELECT SINGLE * FROM vbak WHERE vbeln = lt_inv_st2-sales_order.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      IF vbak-kunnr IN s_kunnr.
        APPEND lt_inv_st2 TO it_inv_st2.
      ELSE.
        CONTINUE.
      ENDIF.
    ELSE.
      APPEND lt_inv_st2 TO it_inv_st2.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " choice_collect_data
