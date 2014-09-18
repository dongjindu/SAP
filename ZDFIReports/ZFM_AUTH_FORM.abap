*&---------------------------------------------------------------------*
*&  Include           ZFM_AUTH_FORM
*&---------------------------------------------------------------------*
DATA : g_auth_check(1). "auth check
*  AUTHORITY-CHECK OBJECT 'F_FICA_CTR'
*           ID 'FM_AUTHACT' FIELD '03'
*           ID 'FM_FIKRS' FIELD p_fikrs
*           ID 'FM_FICTR' FIELD P_FICTR.
*AUTHORITY-CHECK OBJECT 'F_FICA_FSG'
*         ID 'FM_AUTHACT' FIELD '__________'
*         ID 'FM_FIKRS' FIELD '__________'
*         ID 'FM_AUTHGRC' FIELD '__________'.

*&---------------------------------------------------------------------*
*&      Form  user_auth_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_FICTR[]  text
*      -->P_P_FIKRS  text
*----------------------------------------------------------------------*
FORM user_auth_check  TABLES   p_s_fictr
                      USING    p_p_fikrs.
  DATA : lt_fictr LIKE fmfctr OCCURS 0 WITH HEADER LINE.
  DATA : l_kostl LIKE usr21-kostl.

  CLEAR : g_auth_check.
  SELECT * FROM fmfctr
           INTO TABLE lt_fictr
           WHERE fikrs = p_p_fikrs
             AND fictr IN p_s_fictr.
  LOOP AT lt_fictr .

*    AUTHORITY-CHECK OBJECT 'F_FICA_CTR'
*             ID 'FM_AUTHACT' FIELD '03'
*             ID 'FM_FIKRS' FIELD p_p_fikrs
*             ID 'FM_FICTR' FIELD lt_fictr-fictr.
*    IF sy-subrc NE 0.
    CALL FUNCTION 'SUSR_USER_KOSTL_GET'
      EXPORTING
        user_name           = sy-uname
      IMPORTING
        user_kostl          = l_kostl
      EXCEPTIONS
        user_name_not_exist = 1
        OTHERS              = 2.
    IF l_kostl <> lt_fictr-fictr AND l_kostl NE space. "sy-subrc NE 0.
      MESSAGE s019(zmfi) WITH lt_fictr-fictr .
      g_auth_check = 'E'.
      EXIT.
    ENDIF.
  ENDLOOP.
*  AUTHORITY-CHECK OBJECT 'F_FICA_CTR'
*           ID 'FM_AUTHACT' FIELD '03'
*           ID 'FM_FIKRS' FIELD p_fikrs
*           ID 'FM_FICTR' FIELD P_FICTR.

ENDFORM.                    " user_auth_check
*&---------------------------------------------------------------------*
*&      Form  user_auth_check_geber
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_GEBER[]  text
*      -->P_S_FICTR[]  text
*      -->P_P_FIKRS  text
*----------------------------------------------------------------------*
FORM user_auth_check_geber  TABLES   p_s_geber
                                     p_s_fictr
                            USING    p_p_fikrs.
  DATA : lt_fictr LIKE fmfctr OCCURS 0 WITH HEADER LINE.
  DATA : l_kostl LIKE usr21-kostl.
  CLEAR : g_auth_check.

*  FMFINCODE
  DATA : lt_fmfincode LIKE fmfincode OCCURS 0 WITH HEADER LINE.
  SELECT * FROM fmfincode
           INTO TABLE lt_fmfincode
           WHERE fikrs = p_p_fikrs
             AND fincode IN p_s_geber.
  DATA : l_fictr TYPE fistl.
  DATA : l_char(2) TYPE c.
  LOOP AT  lt_fmfincode.
    l_char = lt_fmfincode-fincode+4(2).
    CONCATENATE 'G' l_char '00' INTO l_fictr.
    IF l_fictr = 'G9990' OR
       l_fictr = 'G9999'.
      CONTINUE.
    ELSE.
      AUTHORITY-CHECK OBJECT 'F_FICA_CTR'
               ID 'FM_AUTHACT' FIELD '03'
               ID 'FM_FIKRS' FIELD p_p_fikrs
               ID 'FM_FICTR' FIELD l_fictr.

      IF sy-subrc <> 0.
        MESSAGE s019(zmfi) WITH l_fictr.
        g_auth_check = 'E'. EXIT.
      ENDIF.

*      CALL FUNCTION 'SUSR_USER_KOSTL_GET'
*        EXPORTING
*          user_name           = sy-uname
*        IMPORTING
*          user_kostl          = l_kostl
*        EXCEPTIONS
*          user_name_not_exist = 1
*          OTHERS              = 2.

*
*      IF l_kostl <> l_fictr AND l_kostl NE space. "sy-subrc NE 0.
*        MESSAGE s019(zmfi) WITH l_fictr.
*        g_auth_check = 'E'.
*        EXIT.
*      ENDIF.
    ENDIF.
  ENDLOOP.

  SELECT * FROM fmfctr
           INTO TABLE lt_fictr
           WHERE fikrs = p_p_fikrs
             AND fictr IN p_s_fictr.
  LOOP AT lt_fictr .
    IF lt_fictr-fictr = 'G9990' OR
       lt_fictr-fictr = 'G9999'.
      CONTINUE.
    ENDIF.
    AUTHORITY-CHECK OBJECT 'F_FICA_CTR'
             ID 'FM_AUTHACT' FIELD '03'
             ID 'FM_FIKRS' FIELD p_p_fikrs
             ID 'FM_FICTR' FIELD lt_fictr-fictr.

    IF sy-subrc NE 0.
      MESSAGE s019(zmfi) WITH lt_fictr-fictr .
      g_auth_check = 'E'.
      EXIT.
    ENDIF.

*    CALL FUNCTION 'SUSR_USER_KOSTL_GET'
*      EXPORTING
*        user_name           = sy-uname
*      IMPORTING
*        user_kostl          = l_kostl
*      EXCEPTIONS
*        user_name_not_exist = 1
*        OTHERS              = 2.
*    IF l_kostl <> lt_fictr-fictr AND l_kostl NE space.
*      MESSAGE s019(zmfi) WITH lt_fictr-fictr .
*      g_auth_check = 'E'.
*      EXIT.
*    ENDIF.
  ENDLOOP.

ENDFORM.                    " user_auth_check_geber
*&---------------------------------------------------------------------*
*&      Form  user_auth_check_geber_im
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_COSP_AUFNR  text
*----------------------------------------------------------------------*
FORM user_auth_check_geber_im  USING    p_aufnr
                                        p_fikrs
                                        p_auth.
  DATA : l_fictr TYPE fistl.
  DATA : l_char(2) TYPE c.
  DATA : l_kostl LIKE usr21-kostl.

  l_char = p_aufnr+4(2).
  CONCATENATE 'G' l_char '00' INTO l_fictr.
  CALL FUNCTION 'SUSR_USER_KOSTL_GET'
    EXPORTING
      user_name           = sy-uname
    IMPORTING
      user_kostl          = l_kostl
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.
  IF l_kostl <> l_fictr AND l_kostl NE space.
*    MESSAGE s019(zmfi) WITH lt_fictr-fictr .
*    g_auth_check = 'E'.
*    EXIT.
    p_auth = 'X'.
  ELSE.
    p_auth = ''.
  ENDIF.

*  AUTHORITY-CHECK OBJECT 'F_FICA_CTR'
*           ID 'FM_AUTHACT' FIELD '03'
*           ID 'FM_FIKRS' FIELD p_fikrs
*           ID 'FM_FICTR' FIELD l_fictr.
*  IF sy-subrc NE 0.
*    p_auth = 'X'.
*  ELSE.
*    p_auth = ''.
*  ENDIF.

ENDFORM.                    " user_auth_check_geber_im
