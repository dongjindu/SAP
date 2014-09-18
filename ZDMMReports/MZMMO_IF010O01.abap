*----------------------------------------------------------------------*
*   INCLUDE MZMMO_IF010O01                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
*  SET PF-STATUS '100'.
*  SET TITLEBAR '100'.

  IF gv_first IS INITIAL.
    CLEAR it_sub.
    REFRESH it_sub.

    SELECT bklas FROM t025
                 INTO CORRESPONDING FIELDS OF TABLE it_sub.
    LOOP AT it_sub.
      SELECT kd_lp_ind FROM ztmm_if024
                       INTO it_sub-kd_lp_ind
                       WHERE bklas = it_sub-bklas.
      ENDSELECT.
      MODIFY it_sub.
    ENDLOOP.
    gv_first = 'X'.
  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  change_attribute  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_attribute OUTPUT.
  IF v_btn_flag = 'X'.
    LOOP AT SCREEN.
      IF screen-name  = 'IT_SUB-KD_LP_IND'.
         screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    SET PF-STATUS '100' EXCLUDING 'CHG'.
    SET TITLEBAR '200'.
  ELSE.
    SET PF-STATUS '100' EXCLUDING 'SAVE'.
    SET TITLEBAR '100'.
  ENDIF.
  CLEAR v_btn_flag.
ENDMODULE.                 " change_attribute  OUTPUT
