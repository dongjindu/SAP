*----------------------------------------------------------------------*
***INCLUDE ZXQPLF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_TEXT_OF_CODE_QPCT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_KATART_OC  text
*      -->P_CODEGRP_OC  text
*      -->P_CODE_OC  text
*      <--P_KURZTEXT_OC  text
*----------------------------------------------------------------------*
FORM GET_TEXT_OF_CODE_QPCT USING    P_KATART
                                    P_CODEGRP
                                    P_CODE
                           CHANGING P_KURZTEXT.

   SELECT SINGLE KURZTEXT_C INTO P_KURZTEXT
      FROM ZVQM_VEHICLE
        WHERE KATALOGART  = P_KATART
          AND CODEGRUPPE  = P_CODEGRP
          AND CODE        = P_CODE.

ENDFORM.                    " GET_TEXT_OF_CODE_QPCT
