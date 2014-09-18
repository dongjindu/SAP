*----------------------------------------------------------------------*
*   INCLUDE ZTSD_HMA_MN_OUTBND_F01                                     *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  P1000_START_PROGRESSBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p1000_start_progressbar USING percent.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = percent
      text       = text-001
    EXCEPTIONS
      OTHERS     = 1.


ENDFORM.                    " P1000_START_PROGRESSBAR

*&---------------------------------------------------------------------*
*&      Form  P2000_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p2000_get_data.

  DATA : lt_return TYPE TABLE OF bapireturn.

  CASE 'X'.
    WHEN r_f1. " SIGNOFF
      PERFORM p2100_proc_delivery TABLES lt_return
                                  USING 'Z_FSD_HMA_SIGNOFF'.
    WHEN r_f2. " GI
      PERFORM p2100_proc_delivery TABLES lt_return
                                  USING 'Z_FSD_HMA_DELIVERY'.
    WHEN r_f3. " INVOICE
      PERFORM p2200_proc_invoice TABLES lt_return.

    WHEN r_f4. " GLOVIS

      PERFORM p2100_proc_delivery TABLES lt_return
                                  USING 'Z_FSD_GLOVIS_PREP'.
  ENDCASE.
  PERFORM p3000_write TABLES lt_return.
ENDFORM.                    "P2000_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  P2100_PROC_DELIVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0039   text
*----------------------------------------------------------------------*
FORM p2100_proc_delivery TABLES   p_return  STRUCTURE bapireturn
                         USING    fname.
  DATA : lv_objek LIKE ausp-objek,
         lv_return TYPE bapireturn.

** furong on 08/29/12
*  IF R_BATCH EQ 'X'.
  IF r_batch EQ 'X' AND r_f4 EQ 'X'.
** End on 08/29/12

    RANGES : r_status FOR ztppvr-p_status    ,
*             r_dest   FOR ztppvr-p_dest_code ,
             r_date   FOR ztppvr-zbdat ,
             r_time   FOR ztppvr-zbtim .
    DATA :  lt_ppvr LIKE TABLE OF ztppvr WITH HEADER LINE.

    DATA   : lv_date LIKE sy-datum ,
             lv_time  TYPE t,
             lv_difft TYPE t.

    r_status-sign =  r_date-sign = r_time-sign = 'I'.
    r_status-option = 'EQ'.
* by Daniel on 04/21/11 {
*    R_STATUS-LOW    = 'T01'. APPEND R_STATUS.
*    R_STATUS-LOW    = 'T02'. APPEND R_STATUS.
    r_status-low    = 'T03'. APPEND r_status.
    r_status-low    = 'T06'. APPEND r_status.
* }

*    r_dest-option = 'CP'.
*    r_dest-low    = 'B28A*' . APPEND r_dest.
*
*** Fuorng on 08/21/12 for HACC
*    r_dest-option = 'CP'.
*    r_dest-low    = 'B06A*' . APPEND r_dest.
*** End on 08/21/12

    lv_time  = sy-uzeit .
    lv_date  = sy-datum .
    lv_difft = '010000'.
    PERFORM p3200_add_time USING lv_time
                                 lv_date
                                 lv_difft .

    r_date-option = 'EQ'.
    r_date-low    =  lv_date. APPEND r_date.

    r_time-option = 'BT'.
    CONCATENATE lv_time+0(2) '00' '00' INTO r_time-low.
    CONCATENATE lv_time+0(2) '59' '59' INTO r_time-high.
    APPEND r_time.

    SELECT p_model
           p_body_serial
      INTO CORRESPONDING FIELDS OF TABLE lt_ppvr
      FROM ztppvr
        WHERE flag = 'LT'
          AND p_status    IN r_status
          AND p_dest_code IN s_dest
          AND zbdat       IN r_date
          AND zbtim       IN r_time
          AND zresult = 'S'.

    IF NOT lt_ppvr[] IS INITIAL.
      CLEAR : s_vbeln[].

      LOOP AT lt_ppvr .
        s_vbeln-sign = 'I'.
        s_vbeln-option = 'EQ'.
        CONCATENATE lt_ppvr-p_model lt_ppvr-p_body_serial
        INTO s_vbeln-low.
        COLLECT s_vbeln.
      ENDLOOP.
    ENDIF.

  ENDIF.

  SORT s_vbeln.
  DELETE ADJACENT  DUPLICATES FROM s_vbeln.
  IF r_f4 EQ 'X'.
    DATA : lt_input LIKE TABLE OF zssd_glov_input WITH HEADER LINE.
    LOOP AT s_vbeln.
      lt_input-objek = s_vbeln-low.
      APPEND lt_input.
    ENDLOOP.

    CALL FUNCTION fname
      IMPORTING
        return   = lv_return
      TABLES
        t_bodyno = lt_input[].

  ELSE.
    LOOP AT s_vbeln .
      lv_objek = s_vbeln-low.

      CALL FUNCTION fname
        EXPORTING
          bodyno = lv_objek
        IMPORTING
          return = lv_return.

      APPEND lv_return TO p_return.

    ENDLOOP.
  ENDIF.
ENDFORM.                    " P2100_PROC_DELIVERY
*&---------------------------------------------------------------------*
*&      Form  P2200_PROC_INVOICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_RETURN  text
*----------------------------------------------------------------------*
FORM p2200_proc_invoice TABLES    p_return  STRUCTURE bapireturn.

  DATA : lv_vbeln LIKE vbrk-vbeln,
         lv_return TYPE bapireturn.

  LOOP AT s_vbeln .
    lv_vbeln = s_vbeln-low.

    CALL FUNCTION 'Z_FSD_HMA_INVOICE'
      EXPORTING
        vbeln  = lv_vbeln
      IMPORTING
        return = lv_return.

    APPEND lv_return TO p_return.


  ENDLOOP.

ENDFORM.                    " P2200_PROC_INVOICE
*&---------------------------------------------------------------------*
*&      Form  P3000_POPUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_RETURN  text
*----------------------------------------------------------------------*
FORM p3000_write TABLES  p_return  STRUCTURE bapireturn.

  DATA : lv_message(200), line TYPE i,
         c_line(5),
         lv_return TYPE bapireturn.

  DESCRIBE TABLE p_return LINES line.
  WRITE line TO c_line .

** On 03/13/12 - only write error message
  IF line > 0.
** End
*# WRITE SCREEN
    WRITE AT: /001(014) 'Total Count :',
               016(010) c_line.

    WRITE AT: /.
    LOOP AT p_return INTO lv_return.
      PERFORM p3100_conversion_output USING :
        lv_return-message , lv_return-message_v1.

      WRITE AT: /001(002) '[',
                 003(005) sy-tabix,
                 008(002) ']' ,
                 018(020) lv_return-message,
                 040(020) lv_return-message_v1.


    ENDLOOP.
  ELSE.
    WRITE AT: /001(030) 'Successfully Sent'.
  ENDIF.

*  MESSAGE S001 WITH LV_MESSAGE.

ENDFORM.                    " P3000_POPUP


*---------------------------------------------------------------------*
*       FORM P3100_CONVERSION_OUTPUT                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM p3100_conversion_output USING p_value.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_value
    IMPORTING
      output = p_value.

ENDFORM.                    "P3100_CONVERSION_OUTPUT


*---------------------------------------------------------------------*
*       FORM P3200_ADD_TIME                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  I_STARTTIME                                                   *
*  -->  TYPE  T*
*  -->  I_STARTDATE                                                   *
*  -->  TYPE  D
*  -->  I_ADDTIME                                                     *
*  -->  TYPE  T
*---------------------------------------------------------------------*
FORM p3200_add_time USING i_starttime TYPE  t
                          i_startdate TYPE  d
                          i_addtime   TYPE  t.

  DATA : lv_time TYPE t .
  lv_time = i_starttime - i_addtime.

  IF i_starttime+0(2) EQ '00'.
    i_startdate = i_startdate - 1.
  ENDIF.

  i_starttime = lv_time.

ENDFORM.                    "P3200_ADD_TIME
