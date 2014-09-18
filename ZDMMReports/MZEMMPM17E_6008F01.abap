*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM17E_6008F01                                         *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_rearcharacters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LEIN_LENUM  text
*      -->P_10     text
*      <--P_LV_LENUM  text
*----------------------------------------------------------------------*
FORM get_rearcharacters USING    value(p_f)
                                 value(p_rearcharacters_no)
                        CHANGING value(p_rearcharacters).
* By Hakchin Kim
  DATA l_offset TYPE i.
  l_offset = strlen( p_f ) - p_rearcharacters_no.
  MOVE p_f+l_offset(p_rearcharacters_no) TO p_rearcharacters.
*  WRITE:/ p_rearcharacters.
ENDFORM.                    "get_rearcharacters
*&---------------------------------------------------------------------*
*&      Form  check_lenum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LEIN_LENUM  text
*----------------------------------------------------------------------*
FORM check_lenum USING value(p_lenum).
  DATA: ls_lein LIKE lein.
  SELECT SINGLE * INTO ls_lein
    FROM lein
    WHERE lenum = p_lenum.
ENDFORM.                    " check_lenum
