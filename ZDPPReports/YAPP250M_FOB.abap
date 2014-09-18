*----------------------------------------------------------------------*
***INCLUDE YAPP250M_FOB .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  status_data_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM status_data_create.
  IF NOT  sav  IS INITIAL.
    IF key-model  EQ sav-model  AND  key-stats  EQ  sav-stats  AND
       key-fdate  EQ sav-fdate  AND  key-tdate  EQ  sav-tdate  AND
       key-bodyno EQ sav-bodyno.
      EXIT.
    ENDIF.
  ENDIF.

  PERFORM   range_date_made    USING  key-fdate  key-tdate.
  PERFORM   range_brp_made     USING  key-stats.
  PERFORM   range_bodyno_made  USING  key-bodyno.

  PERFORM   change_data_select.

  MOVE-CORRESPONDING  key   TO   sav.

ENDFORM.                    " status_data_create
*&---------------------------------------------------------------------*
*&      Form  range_brp_made
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_KEY_STATS  text
*----------------------------------------------------------------------*
FORM range_brp_made USING    p_stats.
  REFRESH  r_brp.  CLEAR  r_brp.

  IF p_stats  IS INITIAL.
    EXIT.
  ENDIF.

  MOVE   'I'      TO   r_brp-sign.
  MOVE   'EQ'     TO   r_brp-option.
  MOVE   p_stats  TO   r_brp-low.
  APPEND  r_brp.

ENDFORM.                    " range_brp_made
*&---------------------------------------------------------------------*
*&      Form  range_date_made
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_KEY_FDATE  text
*      -->P_KEY_TDATE  text
*----------------------------------------------------------------------*
FORM range_date_made USING    p_fdate
                              p_tdate.

  REFRESH  r_cdate.  CLEAR  r_cdate.

  IF p_fdate IS INITIAL  AND  p_tdate IS INITIAL.
    EXIT.
  ENDIF.

  IF NOT p_fdate IS INITIAL  AND  NOT p_tdate IS INITIAL.
    MOVE   'I'      TO   r_cdate-sign.
    MOVE   'BT'     TO   r_cdate-option.
    MOVE   p_fdate  TO   r_cdate-low.
    MOVE   p_tdate  TO   r_cdate-high.
  ENDIF.
  IF NOT p_fdate IS INITIAL  AND  p_tdate IS INITIAL.
    MOVE   'I'      TO   r_cdate-sign.
    MOVE   'GE'     TO   r_cdate-option.
    MOVE   p_fdate  TO   r_cdate-low.
  ENDIF.
  IF p_fdate IS INITIAL      AND  NOT p_tdate IS INITIAL.
    MOVE   'I'      TO   r_cdate-sign.
    MOVE   'LE'     TO   r_cdate-option.
    MOVE   p_tdate  TO   r_cdate-high.
  ENDIF.

  APPEND  r_cdate.

ENDFORM.                    " range_date_made
*&---------------------------------------------------------------------*
*&      Form  range_BODYNO_made
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_KEY_BODYNO  text
*----------------------------------------------------------------------*
FORM range_bodyno_made USING    p_bodyno.

  REFRESH  r_bodyno.  CLEAR  r_bodyno.

  IF p_bodyno  IS INITIAL.
    EXIT.
  ENDIF.

  MOVE   'I'      TO   r_bodyno-sign.
  MOVE   'EQ'     TO   r_bodyno-option.
  MOVE   p_bodyno TO   r_bodyno-low.
  APPEND  r_bodyno.

ENDFORM.                    " range_BODYNO_made
*&---------------------------------------------------------------------*
*&      Form  change_data_select
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_data_select.

  REFRESH  it_chg.  CLEAR  it_chg.

  SELECT * FROM  ztpp_change
   INTO CORRESPONDING  FIELDS OF TABLE   it_chg
   WHERE cdate  IN  r_cdate
     AND bodyno IN  r_bodyno
     AND b_rp   IN  r_brp.

  DESCRIBE TABLE  it_chg  LINES  tc100-lines.
  IF tc100-lines IS INITIAL.
    MESSAGE s001 WITH 'Data not found !'.
    EXIT.
  ENDIF.
  IF NOT key-model  IS INITIAL.
    DELETE it_chg  WHERE  bodyno(3) NE  key-model.
  ENDIF.
ENDFORM.                    " change_data_select
