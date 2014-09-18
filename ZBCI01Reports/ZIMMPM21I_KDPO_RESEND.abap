************************************************************************
* Program Name      : ZIMMPM21I_KDPO_RESEND
* Author            : Furong Wang
* Creation Date     : 04/11/2005.
* Specifications By :
* Pattern           :
* Development Request No : UD1K902659
* Addl Documentation:
* Description       : Change KD-PO send indicator
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZIMMPM21I_KDPO_RESEND NO STANDARD PAGE HEADING
                      LINE-SIZE 180
                      LINE-COUNT 58
                      MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl,
          <icon>,
          <symbol>.


**---
DATA : it_send LIKE zsmm_kd_po OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_itab OCCURS 0,
         check(1),
         ebeln LIKE ekko-ebeln,
         lifnr LIKE ekko-lifnr,
         name1 LIKE lfa1-name1,
         ekgrp LIKE ekko-ekgrp,
         eknam LIKE t024-eknam,
         werks LIKE ekpo-werks,
         aedat LIKE ekko-aedat,
         flag  LIKE ztmm_kd_po-flag,
         zmsg(15),
       END OF it_itab.

DATA : it_result LIKE ztmm_kd_po OCCURS 0 WITH HEADER LINE.

Data : w_werks like ekpo-werks value 'P001'.

**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_ebeln FOR ekko-ebeln.
SELECTION-SCREEN END OF BLOCK block1.

**---
START-OF-SELECTION.
  PERFORM CHECK_DATA.

  PERFORM PROCESS_DATA.
**---
END-OF-SELECTION.
*---

*** FORM ***

FORM CHECK_DATA.
  DATA: W_TOTREC TYPE I,
        W_ANSWER(1),
        W_TEXT(50) VALUE ' has been sent. Do you want to resent?'.

  SELECT * INTO TABLE IT_RESULT FROM ZTMM_KD_PO WHERE EBELN IN S_EBELN.
  DESCRIBE TABLE IT_RESULT LINES W_TOTREC.
  IF W_TOTREC EQ 0.
     MESSAGE E999 WITH TEXT-001.
  ENDIF.
  LOOP AT IT_RESULT.
    IF IT_RESULT-FLAG = 'S'.
       CONCATENATE IT_RESULT-EBELN W_TEXT INTO W_TEXT.
       CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
         TEXT_QUESTION = W_TEXT
       IMPORTING
         ANSWER = W_ANSWER
       EXCEPTIONS
         TEXT_NOT_FOUND = 1
         OTHERS = 2.
       IF W_ANSWER = '2'.
          DELETE IT_RESULT.
       ENDIF.
       CLEAR W_TEXT.
    ENDIF.
   ENDLOOP.

ENDFORM.

FORM PROCESS_DATA.
  DATA W_ANSWER(1).
  CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
         TEXT_QUESTION = 'Are you sure to process the data?'
       IMPORTING
         ANSWER = W_ANSWER
       EXCEPTIONS
         TEXT_NOT_FOUND = 1
         OTHERS = 2.
  IF W_ANSWER = '1'.
     LOOP AT IT_RESULT.
*       IT_RESULT-BFLAG = IT_RESULT-FLAG.
       CLEAR IT_RESULT-FLAG.
       MODIFY IT_RESULT.
       CLEAR IT_RESULT.
     ENDLOOP.
     MODIFY ZTMM_KD_PO FROM TABLE IT_RESULT.
     if sy-subrc eq 0.
        MESSAGE S999 WITH 'Data has been changed'.
*        CALL FUNCTION 'POPUP_TO_INFORM'
*       EXPORTING
*         TITEL = 'Information?'
*         TXT1 = ' '
*         TXT2 = 'Data has been changed'.
      endif.
     COMMIT WORK.
  ENDIF.
ENDFORM.
