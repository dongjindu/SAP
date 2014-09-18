*&--------------------------------------------------------------------&*
* Program Name      : ZHRI_MONITOR_TASKDATE
* Author            : Valerian Utama
* Creation Date     : 11/15/2010
* Specifications By : Grace Li
* Pattern           :
* Addl Documentation:
* Description       : The program send email remainder to specified
*                     recipient when 401 suspension expiring date is
*                     approaching.
*
*&--------------------------------------------------------------------&*
* Modification Logs
* Date       Developer  Request ID  Description
* 11/15/2010 VALERIAN   UD1K950214  Initial Program Development
* 03/30/2011 VALERIAN   UD1K951251  Fix logic to retrieve TM's name
* 12/16/2011 VALERIAN   UD1K953584  Get only 'Active' or 'Inactive'
*                                   TMs in data selections
*&--------------------------------------------------------------------&*

REPORT zhri_monitor_taskdate NO STANDARD PAGE HEADING
       MESSAGE-ID zmhr.

* Data Declarations
TABLES: pa0019.

DATA: lt_mailsubject     TYPE sodocchgi1.
DATA: lt_mailrecipients  TYPE STANDARD TABLE OF somlreci1
                         WITH HEADER LINE.
DATA: lt_mailtxt         TYPE STANDARD TABLE OF solisti1
                              WITH HEADER LINE.
DATA: g_datefr(10)       TYPE c,
      g_dateto(10)       TYPE c,
      g_interval(1)      TYPE c,
      g_tmtxt            TYPE tmtxt.

DATA: BEGIN OF t_pa0019 OCCURS 0,
        pernr TYPE pa0019-pernr,
        nachn TYPE pa0002-nachn,
        vorna TYPE pa0002-vorna,
        termn TYPE pa0019-termn,
        mndat TYPE pa0019-mndat,
      END OF t_pa0019.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text-s01.
SELECT-OPTIONS: s_mndat FOR pa0019-mndat DEFAULT sy-datum OBLIGATORY.
PARAMETERS:     p_tmart TYPE pa0019-tmart DEFAULT 'ZH' OBLIGATORY.

PARAMETERS: p_dstlst TYPE so_recname OBLIGATORY.
PARAMETERS: p_test TYPE check NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK sel.

* Start of selection
START-OF-SELECTION.
  PERFORM get_data.
  PERFORM prepare_mail.
  PERFORM send_mail.


*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       Get 401K Suspension Data
*----------------------------------------------------------------------*
FORM get_data.
  SELECT pa0019~pernr
         pa0002~nachn
         pa0002~vorna
         pa0019~termn
         pa0019~mndat

    INTO CORRESPONDING FIELDS OF TABLE t_pa0019
    FROM pa0019 JOIN pa0002
                  ON pa0002~pernr = pa0019~pernr
                JOIN pa0000                                 "UD1K953584
                  ON pa0000~pernr = pa0019~pernr            "UD1K953584

   WHERE pa0019~mndat IN s_mndat
     AND pa0019~tmart = p_tmart
     AND pa0002~endda = '99991231'                          "UD1K951251
     AND pa0000~endda = '99991231'                          "UD1K953584
     AND ( pa0000~STAT2 = '1' OR pa0000~STAT2 = '3' ).      "UD1K953584

  SORT t_pa0019 BY pernr ASCENDING
                   mndat DESCENDING.
  DELETE ADJACENT DUPLICATES FROM t_pa0019 COMPARING pernr.

  IF t_pa0019[] IS INITIAL.
    MESSAGE s001 WITH text-m01.
  ENDIF.

  SELECT SINGLE tmtxt INTO g_tmtxt
    FROM t531s
   WHERE sprsl = sy-langu
     AND tmart = p_tmart.

  LOOP AT s_mndat.
    WRITE s_mndat-low TO g_datefr.
    IF NOT s_mndat-high IS INITIAL.
      WRITE s_mndat-high TO g_dateto.
    ELSE.
      WRITE s_mndat-low TO g_dateto.
    ENDIF.
    g_interval = '-'.
  ENDLOOP.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  prepare_mail
*&---------------------------------------------------------------------*
*       Prepare mail with data that need to be sent
*----------------------------------------------------------------------*
FORM prepare_mail.
* Recipients
  lt_mailrecipients-rec_type  = 'C'.
  lt_mailrecipients-receiver = p_dstlst.
  lt_mailrecipients-express = 'X'.

  APPEND lt_mailrecipients.
  CLEAR lt_mailrecipients.

* Subject.
  lt_mailsubject-obj_name = 'SUBJECT'.
  lt_mailsubject-obj_langu = sy-langu.
  CONCATENATE text-h02 g_tmtxt g_datefr g_interval g_dateto
         INTO lt_mailsubject-obj_descr
         SEPARATED BY space.

* Mail Contents
  lt_mailtxt = text-l01.
  APPEND lt_mailtxt. CLEAR lt_mailtxt.

  CONCATENATE text-l02 g_tmtxt text-l04 INTO lt_mailtxt
              SEPARATED BY space.
  APPEND lt_mailtxt. CLEAR lt_mailtxt.
  APPEND lt_mailtxt. CLEAR lt_mailtxt.

  WRITE sy-uline TO lt_mailtxt(103).
  APPEND lt_mailtxt. CLEAR lt_mailtxt.

  lt_mailtxt = text-l03.
  APPEND lt_mailtxt. CLEAR lt_mailtxt.

  WRITE sy-uline TO lt_mailtxt(103).
  APPEND lt_mailtxt. CLEAR lt_mailtxt.

  LOOP AT t_pa0019.
    WRITE: t_pa0019-pernr TO lt_mailtxt(8),
           t_pa0019-nachn TO lt_mailtxt+9(40),
           t_pa0019-vorna TO lt_mailtxt+50(40),
           t_pa0019-termn TO lt_mailtxt+91(10).
    APPEND lt_mailtxt. CLEAR lt_mailtxt.
  ENDLOOP.
ENDFORM.                    " prepare_mail
*&---------------------------------------------------------------------*
*&      Form  send_mail
*&---------------------------------------------------------------------*
*       Send Mail
*----------------------------------------------------------------------*
FORM send_mail.
  IF p_test IS INITIAL.

* Send Mail
    CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
      EXPORTING
        document_data              = lt_mailsubject
        COMMIT_WORK                = 'X'
      TABLES
        object_content             = lt_mailtxt
        receivers                  = lt_mailrecipients
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.
    IF sy-subrc EQ 0.
      COMMIT WORK.

*   Push mail out from SAP outbox
      SUBMIT rsconn01 WITH mode = 'INT' AND RETURN.
    ENDIF.

  ELSE.

* Display data to screen
    SET BLANK LINES ON.
    WRITE: / text-h01, lt_mailsubject-obj_descr.
    SKIP.
    LOOP AT lt_mailtxt.
      WRITE lt_mailtxt.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " send_mail
