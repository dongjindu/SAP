************************************************************************
* Program Name      : ZRDA_QA_DOC_ARCH
* Creation Date     : 04/21/2014
* Developer         : Hyunok Pak
* Development Request No :
* Addl Documentation:
* Description       : QA Archiving data input processing
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zrda_qa_doc_arch NO STANDARD PAGE HEADING
                        LINE-SIZE 132
                        LINE-COUNT 64(1)
                        MESSAGE-ID zmmm.

INCLUDE zrda_qa_doc_arch_top.
INCLUDE zrda_qa_doc_arch_o01.
INCLUDE zrda_qa_doc_arch_i01.
INCLUDE zrda_qa_doc_arch_f01.





SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
PARAMETERS: p_buybak  RADIOBUTTON GROUP gr1 DEFAULT 'X',
            p_legal   RADIOBUTTON GROUP gr1,
            p_claim   RADIOBUTTON GROUP gr1,
            p_rclaim  RADIOBUTTON GROUP gr1,
            p_vendor  RADIOBUTTON GROUP gr1.

SELECTION-SCREEN END OF BLOCK block1.



INITIALIZATION.
  PERFORM init_data.

*AT SELECTION-SCREEN OUTPUT.
*
*AT SELECTION-SCREEN.

START-OF-SELECTION.

  CLEAR: g_date, g_date_tm.
  g_date    = sy-datum.
  g_date_tm = sy-uzeit.

  CASE 'X'.
    WHEN p_buybak.
      g_proc = 'B'.
      g_tid = '00001'.        CALL SCREEN 0210.
    WHEN p_legal.
      g_proc = 'L'.
      g_tid = '00002'.        CALL SCREEN 0220.
    WHEN p_claim.
      g_proc = 'C'.
      g_tid = '00003'.        CALL SCREEN 0230.
    WHEN p_rclaim.
      g_proc = 'R'.
      g_tid = '00004'.        CALL SCREEN 0230.
    WHEN p_vendor.
      g_proc = 'V'.
      g_tid = '00005'.        CALL SCREEN 0240.
  ENDCASE.

END-OF-SELECTION.


**AT LINE-SELECTION.
**AT USER-COMMAND
