************************************************************************
* Program Name      : ZIMMPM_KD_AASN_DEL_INV
* Author            : Furong Wang
* Creation Date     : 06/09/2005.
* Specifications By :
* Pattern           :
* Development Request No : UD1K916406
* Addl Documentation:
* Description       : Deletion of invoice of KD-ASN
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zimmpm_kd_aasn_del_inv NO STANDARD PAGE HEADING
                      LINE-SIZE 180
                      LINE-COUNT 58
                      MESSAGE-ID zmmm.

**---
DATA : it_ztmm_kd_asn LIKE ztmm_kd_asn OCCURS 0 WITH HEADER LINE,
       it_ztmm_kd_asn_main LIKE ztmm_kd_asn_main
                                       OCCURS 0 WITH HEADER LINE.


DATA : w_werks LIKE ekpo-werks VALUE 'P001'.

**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-002.
SELECT-OPTIONS : s_zinv FOR it_ztmm_kd_asn-zinvoice OBLIGATORY.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETER: p_rd1 RADIOBUTTON GROUP grp1.
SELECTION-SCREEN COMMENT 5(20) text-101.
*parameter: p_asn type tabname default 'ZTMM_KD_ASN'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETER: p_rd2 RADIOBUTTON GROUP grp1.
SELECTION-SCREEN COMMENT 5(20) text-102.
*parameter: p_main type tabname default 'ZTMM_KD_ASN_MAIN'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK block1.

**---
START-OF-SELECTION.
  PERFORM process_data.
**---
END-OF-SELECTION.
*---

*** FORM ***

FORM process_data.
  DATA: w_totrec TYPE i,
        w_answer(1).
  IF p_rd1 IS INITIAL.
    SELECT * INTO TABLE it_ztmm_kd_asn_main FROM ztmm_kd_asn_main
                                    WHERE zinvoice IN s_zinv.
    DESCRIBE TABLE it_ztmm_kd_asn_main LINES w_totrec.
  ELSE.
    SELECT * INTO TABLE it_ztmm_kd_asn FROM ztmm_kd_asn
                                    WHERE zinvoice IN s_zinv.
    DESCRIBE TABLE it_ztmm_kd_asn LINES w_totrec.
  ENDIF.
  IF w_totrec EQ 0.
    MESSAGE i999 WITH text-001.
    EXIT.
  ENDIF.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
            text_question  = 'Are you sure to process the data?'
       IMPORTING
            answer         = w_answer
       EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.
  IF w_answer = '1'.
    IF p_rd1 IS INITIAL.
      DELETE ztmm_kd_asn_main FROM TABLE it_ztmm_kd_asn_main.
    ELSE.
      DELETE ztmm_kd_asn FROM TABLE it_ztmm_kd_asn.
    ENDIF.
    IF sy-subrc EQ 0.
      MESSAGE s999 WITH 'Data has been deleted'.
*        CALL FUNCTION 'POPUP_TO_INFORM'
*       EXPORTING
*         TITEL = 'Information?'
*         TXT1 = ' '
*         TXT2 = 'Data has been changed'.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
ENDFORM.
