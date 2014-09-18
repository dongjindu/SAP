*----------------------------------------------------------------------*
*   INCLUDE ZXQQMU07                                                   *
*----------------------------------------------------------------------*
*Tables : ZSQM_CI_QMEL. "/Screen Layout Str. for User Data for CI_QMEL
MOVE-CORRESPONDING : VIQMEL TO ZSQM_CI_QMEL.
MOVE :
       'Q' TO ZSQM_CI_QMEL-KATART_VH, "/Vehicle/Engine type
*       'Q' TO ZSQM_CI_QMEL-KATART_OC, "/Occurrence Location
       'R' TO ZSQM_CI_QMEL-KATART_AT, "/Activity Type
*       'S' TO ZSQM_CI_QMEL-KATART_DT. "/Disposal Type
** Changed by Furong on 01/26/09
       '$' TO ZSQM_CI_QMEL-KATART_is. "/Issue Type
** End of change
*Move: qmel-lndwntm to zsqm_cI_QMEL-LNDWNTM.
