************************************************************************
* Program Name      : ZEMMPM35E_SPE_MANAGE
* Created by        : Min-su Park
* Created on        : 2003.10.21.
* Pattern           :
* Description       :  Manage Error Standard Price for Purchase Material
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.22.     Min-su Park    UD1K901873     Initial Coding
************************************************************************
*----------------------------------------------------------------------*
*   INCLUDE ZMINSUEVT                                                  *
*----------------------------------------------------------------------*
INITIALIZATION.
*Setting s_werks as 'P001'.
S_WERKS-LOW = 'P001'.
APPEND S_WERKS.

AT SELECTION-SCREEN.
*Get Error Data from ZTMM_SPE
SELECT * FROM ZTMM_SPE
         INTO CORRESPONDING FIELDS OF TABLE IT_ZTMM_SPE
        WHERE EKORG IN S_EKORG
          AND WERKS IN S_WERKS.

START-OF-SELECTION.
CALL SCREEN 100.
