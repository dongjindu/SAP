*&---------------------------------------------------------------------*
*&  Include           ZMMRCOMMON
*&---------------------------------------------------------------------*
* Working Time
DATA:
  xwtime TYPE TABLE OF zmms0401 WITH HEADER LINE.
*&---------------------------------------------------------------------*
*&      Form  get_working_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_XWTIME  text
*      -->P_SY_DATUM  text
*      -->P_0219   text
*      -->P_PA_WERKS  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
*FORM get_working_time  TABLES   pt_xwtime STRUCTURE zmms0401
*                       USING    p_datum
*                                p_day
*                                p_werks
*                                p_arbpl.
*  DATA: lf_fabkl TYPE t001w-fabkl,
*        lf_shift TYPE sa_shift,
*        lf_shop  TYPE crhd-arbpl,
*        lf_uph   TYPE i,
*        lf_date  TYPE sy-datum.
**        lt_wtime TYPE TABLE OF zspp0013 WITH HEADER LINE.
*  DATA:
*    BEGIN OF ls_shop,
*      shop1 TYPE crhd-arbpl VALUE 'B',
*      shop2 TYPE crhd-arbpl VALUE 'P',
*      shop3 TYPE crhd-arbpl VALUE 'T',
*    END OF ls_shop.
*
*  REFRESH pt_xwtime.
*  lf_date = p_datum.
*  PERFORM get_last_working_date USING    p_werks
*                                CHANGING lf_date.
*
*  IF p_arbpl IS INITIAL.
*    DO 3 TIMES VARYING lf_shop FROM ls_shop-shop1
*                               NEXT ls_shop-shop2.
*      REFRESH lt_wtime.
*      CALL FUNCTION 'Z_PP_GET_WORKING_TIME'
*        EXPORTING
*          i_datum                     = lf_date
*          i_day                       = p_day
*          i_werks                     = p_werks
*          i_arbpl                     = lf_shop
*        TABLES
*          t_working_time              = lt_wtime
*        EXCEPTIONS
*          cannot_read_dayname         = 1
*          incorrect_shift_info        = 2
*          incorrect_capa_info         = 3
*          incorrect_plant_value       = 4
*          incorrect_work_center_value = 5
*          OTHERS                      = 6.
*
*      IF sy-subrc <> 0.
*        MESSAGE e053(h1).
*        LEAVE PROGRAM.
*      ENDIF.
*
*
*      LOOP AT lt_wtime.
*        CLEAR pt_xwtime.
*        MOVE-CORRESPONDING lt_wtime TO pt_xwtime.
*        pt_xwtime-arbpl = lf_shop.
*        APPEND pt_xwtime.
*      ENDLOOP.
*    ENDDO.
*  ELSE.
*    REFRESH lt_wtime.
*    CALL FUNCTION 'Z_PP_GET_WORKING_TIME'
*      EXPORTING
*        i_datum                     = lf_date
*        i_day                       = p_day
*        i_werks                     = p_werks
*        i_arbpl                     = p_arbpl
*      TABLES
*        t_working_time              = lt_wtime
*      EXCEPTIONS
*        cannot_read_dayname         = 1
*        incorrect_shift_info        = 2
*        incorrect_capa_info         = 3
*        incorrect_plant_value       = 4
*        incorrect_work_center_value = 5
*        OTHERS                      = 6.
*
*    IF sy-subrc <> 0.
*      MESSAGE e053(h1).
*      LEAVE PROGRAM.
*    ENDIF.
*
*    LOOP AT lt_wtime.
*      CLEAR pt_xwtime.
*      MOVE-CORRESPONDING lt_wtime TO pt_xwtime.
*      pt_xwtime-arbpl = p_arbpl.
*      APPEND pt_xwtime.
*    ENDLOOP.
*  ENDIF.
*  SORT pt_xwtime BY arbpl datum tprog shidx index.
*  LOOP AT pt_xwtime.
*    AT NEW datum.
*      CLEAR lf_shift.
*    ENDAT.
*    AT NEW tprog.
*      lf_shift = lf_shift + 1.
*    ENDAT.
*    pt_xwtime-shidx = lf_shift.
*    MODIFY pt_xwtime.
*  ENDLOOP.

*ENDFORM.                    " get_working_time
