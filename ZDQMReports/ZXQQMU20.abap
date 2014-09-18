*<<<<<<<< Start of EQM01 - ZQMEX_02 QM-Notification Enhancement >>>>>>

************************************************************************
* Program Name      : ZQMEX_02
* Author            : SeungLyong, Lee
* Creation Date     : 2003.08.08.
* Specifications By : SeungLyong, Lee
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Notification Enhancement
*
*
* Related Table List:
*   QMEL - Quality Notification
*
* Modification Logs
* Date       Developer    RequestNo      Description
* 05/19/2005  Shiva       UD1K916098    Save the text created using
*                                       customer screen exit when the
*                                       docuemnt is created.
* 01/09       Furong      UD1K942078    Send email by system/user status
*
************************************************************************
*----------------------------------------------------------------------*
*   INCLUDE ZXQQMU20                                                   *
*----------------------------------------------------------------------*

IF i_viqmel-qmart = 'Q2' OR  i_viqmel-qmart = 'Q4'.
  IF sy-tcode = 'QM01'.
    IF i_viqmel-ausvn > sy-datum.
      MESSAGE e000(zmqm) WITH
      'Malfunction Start Date must not be a future date'.
    ELSEIF  i_viqmel-ausvn = sy-datum.
  MESSAGE w000(zmqm) WITH 'Malfunction date same as notification date?'
.
    ENDIF.
  ELSEIF sy-tcode = 'QM02'.
    IF i_viqmel-ausvn > sy-datum.
      MESSAGE e000(zmqm) WITH
      'Malfunction Start Date must not be a future date'.
    ENDIF.
  ENDIF.
ENDIF.

** Furong on 07/15-14 for adding location in screen 103
IF i_viqmel-qmart = 'Q3' OR i_viqmel-qmart = 'Q4'.
  IF  zsqm_ci_qmel-qcodegrp_loc IS INITIAL .
    MESSAGE e006(zmqm).
  ENDIF.
ENDIF.
IF  i_viqmel-qmart = 'Q4' AND zsqm_ci_qmel-code_vh IS INITIAL.
  MESSAGE e007(zmqm).
ENDIF.
** )

*%%% QM Notification : Qx.
*-- 1. just for QM Notification.
*   2. Restrict authorization for responsible dept users to update text
*      editor
CHECK i_viqmel-qmart+0(1) = 'Q' AND "/
      sy-tcode NE 'IQS21' AND       "/
      sy-tcode NE 'IQS22'.


wi_qmnum = i_viqmel-qmnum.
*&---                                       added by shiva.

PERFORM save_text.   " ON COMMIT.
*&---                                       end shiva.

**-- check Material Code and fill System Type code.
IF NOT i_viqmel-matnr IS INITIAL.

  PERFORM get_external_mat_grp_code   USING  i_viqmel-matnr
                                   CHANGING  i_viqmel-extwg.
  MOVE i_viqmel-extwg TO e_viqmel-extwg.
ENDIF.

**-- Check Vehicle/Engine type and Activity Code required in Improvement
**-- tab
* check for Q2 Notification only -100565
IF i_viqmel-qmart = 'Q2'.
  IF  e_viqmel-code_vh    IS INITIAL OR
      e_viqmel-codegrp_vh IS INITIAL       .
    MESSAGE e000(zmqm)
      WITH 'Vehicle/Engine type is required in Improvement tab.'(e12).
  ENDIF.
  IF  e_viqmel-code_is IS INITIAL.
    MESSAGE e000(zmqm)
        WITH 'Issue type is required in Improvement tab.'.
  ENDIF.
ENDIF.

** Changed by Furong on 12/14/09
*CASE ZSQM_CI_QMEL-PANEL6.
*  WHEN '1' or 'Yes'.
*    ZSQM_CI_QMEL-PANEL6 = 'Yes'.
*  WHEN '2' or 'No'.
*    ZSQM_CI_QMEL-PANEL6 = 'No'.
*  WHEN OTHERS.
IF zsqm_ci_qmel-panel6 IS INITIAL AND i_viqmel-qmart = 'Q2'.
  MESSAGE e000(zmqm) WITH 'Please enter 6 Panel C/M'.
ENDIF.
*ENDCASE.
** end of change

*-- Remark - Start : 03/24/2004 - sllee : 20040319 FCR by DoKim
*  IF     E_VIQMEL-CODE_AT    IS INITIAL OR
*         E_VIQMEL-CODEGRP_AT IS INITIAL.
*    MESSAGE E000(ZMQM)
*        WITH 'Activity is required in Improvement tab.'(E13).
*  ENDIF.
*-- Remark - end

*/// Chec Notification Status Exit for Sending Date - 2003/09/25-SLLEE
*-- Import Notification Satus value Using Field Exit.
* commented and moved code 08/30/06 100565
*CHECK I_VIQMEL-SENDDAT IS INITIAL.


DATA: lw_sttxt LIKE riwo00-sttxt.
DATA: scr_fields LIKE dynpread OCCURS 1 WITH HEADER LINE.


TYPES: BEGIN OF itab_type,
        word(20),
      END   OF itab_type.

DATA: it_status TYPE STANDARD TABLE OF itab_type WITH
                   NON-UNIQUE DEFAULT KEY INITIAL SIZE 3
                   WITH HEADER LINE.

*  import LW_STTXT from MEMORY ID 'Z_STATUS'.

CLEAR : scr_fields, scr_fields[].
scr_fields-fieldname = 'RIWO00-STTXT'.
APPEND scr_fields.

* Get Screen Value of Noti. Status field from Screen
CALL FUNCTION 'DYNP_VALUES_READ'
  EXPORTING
    dyname               = 'SAPLIQS0'
    dynumb               = '1070'  "SY-DYNNR
  TABLES
    dynpfields           = scr_fields
  EXCEPTIONS
    invalid_abapworkarea = 1
    invalid_dynprofield  = 2
    invalid_dynproname   = 3
    invalid_dynpronummer = 4
    invalid_request      = 5
    no_fielddescription  = 6
    invalid_parameter    = 7
    undefind_error       = 8
    double_conversion    = 9
    OTHERS               = 10.

IF sy-subrc = 0.
  CLEAR : lw_sttxt.
  READ TABLE scr_fields INDEX 1.

  IF sy-subrc = 0.
    MOVE scr_fields-fieldvalue  TO  lw_sttxt.

  ENDIF.
ELSE.
  EXIT.
ENDIF.


* added code to check the system status
* cannot complete Notification if field RESPONSIVENESS is empty
*if  LW_STTXT cs 'NOCO'.
IF i_viqmel-qmart = 'Q2'.
  IF NOT ( i_viqmel-qmdab IS INITIAL ).
    IF  zsqm_ci_qmel-responsive IS INITIAL .
      MESSAGE e003(zmqm).
    ENDIF.
  ENDIF.
ENDIF.

** Changed by Furong on 01/29/09; sending email to vendor

IF i_viqmel-qmart = 'Q2' AND sy-tcode = 'QM02'.
  DATA: v_mess1(255),
        v_mess2(255),
        v_email1(80),
        v_email2(80).

  DATA: BEGIN OF lt_jest OCCURS 0,
          stat LIKE jest-stat,
          txt04 LIKE tj02t-txt04,
        END OF lt_jest.
  DATA: l_stat LIKE jest-stat,
        l_date(10),
        l_date_c(10),
        l_date_d LIKE sy-datum,
        l_nopr_new(1),
        l_noco_new(1),
        l_plandat LIKE i_viqmel-plandat,
        l_success  LIKE i_viqmel-success,
        l_completed LIKE i_viqmel-completed.

  DATA: BEGIN OF lt_mess OCCURS 10,
        mess(132),
        END OF lt_mess.

  DATA: l_i0072 LIKE jest-stat VALUE 'I0072'.

  CONSTANTS: c_no_email(40) VALUE ' No Email Maintained'.

CONCATENATE 'Please forward this email to your team members who should'
                                       'be informed abouth this issue.'
                                  INTO  lt_mess-mess SEPARATED BY space.

  APPEND lt_mess.
  CLEAR: lt_mess.

  APPEND lt_mess.
  CLEAR: lt_mess.


  CONCATENATE 'Disclaimer: This mail is automated  and unattended,'
             'therefore please  DO NOT REPLY  to this message  and'
             INTO  lt_mess-mess SEPARATED BY space.

  APPEND lt_mess.
  CLEAR: lt_mess.

  CONCATENATE ' contact the responsible person in HMMA Quality'
              'Team for any question regarding this Notification.'
              INTO  lt_mess-mess SEPARATED BY space.
  APPEND lt_mess.
  CLEAR: lt_mess.

  APPEND lt_mess.
  CLEAR: lt_mess.

  CONCATENATE 'The information in this email and any attachments are'
            'for the sole use of the intended recipient'
            'and may contain'
               INTO  lt_mess-mess SEPARATED BY space.
  APPEND lt_mess.
  CLEAR: lt_mess.

 CONCATENATE 'privileged  and confidential information. If you are not'
            'the intended  recipient, any use, disclosure, copying  or'
                                  INTO  lt_mess-mess SEPARATED BY space.
  APPEND lt_mess.
  CLEAR: lt_mess.

  CONCATENATE 'distribution of this message or attachment is strictly'
          'prohibited. We have taken precautions to minimize the risk'
               INTO  lt_mess-mess SEPARATED BY space.
  APPEND lt_mess.
  CLEAR: lt_mess.

  CONCATENATE 'of  transmitting software viruses, but we advise you to'
              'carry out your own virus checks  on any attachment this'
                 INTO  lt_mess-mess SEPARATED BY space.
  APPEND lt_mess.
  CLEAR: lt_mess.

  CONCATENATE 'message. We  cannot accept liability for any loss or'
          'damage causedby software viruses.If you believe  that you'
               INTO  lt_mess-mess SEPARATED BY space.
  APPEND lt_mess.
  CLEAR: lt_mess.

  CONCATENATE 'have  received  this email in error,  please contact'
              'the sender immediately  and delete  the email and all'
               'of its'
                 INTO  lt_mess-mess SEPARATED BY space.
  APPEND lt_mess.
  CLEAR: lt_mess.

  CONCATENATE  'attachments.'
               ''
                INTO  lt_mess-mess SEPARATED BY space.
  APPEND lt_mess.
  CLEAR: lt_mess.


  SELECT stat txt04 INTO TABLE lt_jest
    FROM jest AS a
    INNER JOIN tj02t AS b
    ON a~stat = b~istat
    WHERE objnr = i_viqmel-objnr
      AND inact = ' '
      AND spras = 'E'.

  SELECT SINGLE plandat completed success
         INTO (l_plandat, l_completed, l_success)
          FROM qmel
          WHERE qmnum = i_viqmel-qmnum.

  SPLIT lw_sttxt AT ' ' INTO TABLE it_status.

** Changed by Furong on 09/22/09
  CALL FUNCTION 'STATUS_CHECK'
    EXPORTING
*     BYPASS_BUFFER     = ' '
*     CLIENT            = SY-MANDT
      objnr             = i_viqmel-objnr
      status            = l_i0072
    EXCEPTIONS
      object_not_found  = 1
      status_not_active = 2.
  IF sy-subrc = 0.
    READ TABLE lt_jest WITH KEY txt04 = 'NOPR'.
    IF sy-subrc = 0.
      AUTHORITY-CHECK OBJECT 'ZQM_REV' ID 'ZQM_REV' FIELD 'X'.
      IF sy-subrc <> 0.
 MESSAGE e000(zmqm) WITH 'No Authorization to set COMPLETION indicator'.
      ENDIF.
    ENDIF.

  ENDIF.
** Changed by Furong on 09/15/09

*  READ TABLE IT_STATUS WITH KEY WORD = 'NOCO'.
*  IF SY-SUBRC = 0.
*    READ TABLE LT_JEST WITH KEY TXT04 = 'NOPR'.
*    IF SY-SUBRC = 0.
*      AUTHORITY-CHECK OBJECT 'ZQM_REV' ID 'ZQM_REV' FIELD 'X'.
*      IF SY-SUBRC <> 0.
* MESSAGE E000(ZMQM) WITH 'No Authorization to set COMPLETION indicator'
*.
*      ENDIF.
*    ENDIF.
*  ENDIF.
** End of change
** End of change on 09/22/09

  READ TABLE it_status WITH KEY word = 'NOPR'. "Release Status :'NOPR'

*    READ TABLE LT_JEST WITH KEY TXT04 = 'NOPR'.

*** first layer
  IF sy-subrc = 0.

    READ TABLE lt_jest WITH KEY txt04 = 'NOPR'.
*** second layer
    IF sy-subrc = 0.           " System status no change

      IF i_viqmel-plandat IS INITIAL AND
           i_viqmel-completed IS INITIAL.          "TASK CREATED

        IF ( i_viqmel-plandat <> l_plandat )
               OR ( i_viqmel-completed <> l_completed ).

          IF i_viqmel-plandat_old IS INITIAL.

            WRITE: i_viqmel-erdat TO l_date MM/DD/YYYY.

            SELECT SINGLE ltrmn INTO l_date_d
               FROM qmel
               WHERE qmnum = i_viqmel-qmnum.

            WRITE: l_date_d TO l_date_c MM/DD/YYYY.

            CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                        'is created on' l_date
                        INTO v_mess1 SEPARATED BY space.
            CONCATENATE v_mess1 'with required end date of' l_date_c
                       ', please'
                         INTO v_mess1 SEPARATED BY space.

            CLEAR:  v_mess2.
            CONCATENATE 'log on the “Supplier Portal” to view'
                  'more details about this notification.'
                  INTO v_mess2 SEPARATED BY space.

*          V_MESS1 = '11111'.
*          V_MESS2 = '11111'.
          ELSE.
            CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                        'submitted was rejected by HMMA QC,'
                        'Please take the appropriate action to'
                        INTO v_mess1 SEPARATED BY space.

            CONCATENATE 'address the problem.'
                        'Please log on to the “Supplier Portal” to'
                        'view more details about this notification.'
                        INTO v_mess2 SEPARATED BY space.
*
*          V_MESS1 = '4A4A'.
*          V_MESS2 = '4A4A'.
          ENDIF.


          SELECT SINGLE email1 INTO v_email1
                 FROM ztqm_vend_email
                 WHERE lifnr = i_viqmel-lifnum.
          IF v_email1 IS INITIAL.
            SELECT SINGLE smtp_addr INTO v_email1
              FROM adr6 AS a
              INNER JOIN usr21 AS b
              ON a~persnumber = b~persnumber
              AND a~addrnumber = b~addrnumber
              WHERE bname =  i_viqmel-ernam.

            CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                      'email did not get sent to the Vendor'
                       i_viqmel-lifnum
                      'Please inform the vendor to'
                      INTO v_mess1 SEPARATED BY space.

            CONCATENATE 'log on to the “Supplier Portal” to'
                      'update email address and view'
                      'more details about this notification.'
                      INTO v_mess2 SEPARATED BY space.
            DELETE lt_mess INDEX 1.
          ENDIF.
          PERFORM send_email TABLES lt_mess
                             USING v_email1 v_email2  i_viqmel-qmnum
                                   v_mess1 v_mess2.
        ENDIF.
      ENDIF.

*      READ TABLE LT_JEST WITH KEY TXT04 = 'TSRL'.
*      IF SY-SUBRC = 0.
      IF NOT i_viqmel-plandat IS INITIAL AND
         i_viqmel-completed IS INITIAL.         "TASK RELEASED

*        V_MESS1 = '2222'.
*        V_MESS2 = '2222'.

        IF ( i_viqmel-plandat <> l_plandat )
               OR ( i_viqmel-completed <> l_completed ).

          CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                    'was released with “Planned Date of Completion.”'
                      'Please log on to the “Supplier'
                      INTO v_mess1 SEPARATED BY space.


          CONCATENATE 'Portal” to view more details'
                    'about this notification.'
                    INTO v_mess2 SEPARATED BY space.

          SELECT SINGLE email1 INTO v_email1
          FROM ztqm_vend_email
          WHERE lifnr = i_viqmel-lifnum.

          IF v_email1 IS INITIAL.
            SELECT SINGLE smtp_addr INTO v_email1
             FROM adr6 AS a
             INNER JOIN usr21 AS b
             ON a~persnumber = b~persnumber
             AND a~addrnumber = b~addrnumber
             WHERE bname = i_viqmel-ernam.

            CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                      'email did not get sent to the Vendor'
                       i_viqmel-lifnum
                      'Please inform the vendor to'
                      INTO v_mess1 SEPARATED BY space.

            CONCATENATE 'log on to the “Supplier Portal” to'
                      'update email address and view'
                      'more details about this notification.'
                      INTO v_mess2 SEPARATED BY space.
            DELETE lt_mess INDEX 1.

            PERFORM send_email TABLES lt_mess
                             USING v_email1 v_email2  i_viqmel-qmnum
                                 v_mess1 v_mess2.
          ELSE.
            SELECT SINGLE smtp_addr INTO v_email2
              FROM adr6 AS a
              INNER JOIN usr21 AS b
              ON a~persnumber = b~persnumber
              AND a~addrnumber = b~addrnumber
              WHERE bname = i_viqmel-ernam.
            PERFORM send_email TABLES lt_mess
                               USING v_email1 v_email2 i_viqmel-qmnum
                               v_mess1 v_mess2.
            CLEAR: v_email2.

          ENDIF.
        ENDIF.
      ENDIF.
*      READ TABLE LT_JEST WITH KEY TXT04 = 'TSCO'.
*      IF SY-SUBRC = 0.
      IF i_viqmel-completed = 'X'
             AND i_viqmel-success = ' '.               "TASK COMPLETED

        IF ( i_viqmel-success <> l_success )
               OR ( i_viqmel-completed <> l_completed ).

          CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                      'which is completed is now with HMMA'
                      'QC for further review. Please log on'
                      INTO v_mess1 SEPARATED BY space.

          CONCATENATE 'to the “Supplier Portal” to view more details '
                          'about this notification.'
                         INTO v_mess2 SEPARATED BY space.
*
*        V_MESS1 = '3333'.
*        V_MESS2 = '3333'.

          SELECT SINGLE email1 INTO v_email1
          FROM ztqm_vend_email
          WHERE lifnr = i_viqmel-lifnum.
          IF v_email1 IS INITIAL.
            SELECT SINGLE smtp_addr INTO v_email1
            FROM adr6 AS a
            INNER JOIN usr21 AS b
            ON a~persnumber = b~persnumber
            AND a~addrnumber = b~addrnumber
            WHERE bname = i_viqmel-ernam.

            CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                      'email did not get sent to the Vendor'
                       i_viqmel-lifnum
                      'Please inform the vendor to'
                      INTO v_mess1 SEPARATED BY space.

            CONCATENATE 'log on to the “Supplier Portal” to'
                      'update email address and view'
                      'more details about this notification.'
                      INTO v_mess2 SEPARATED BY space.
            DELETE lt_mess INDEX 1.
            PERFORM send_email TABLES lt_mess
                              USING v_email1 v_email2  i_viqmel-qmnum
                               v_mess1 v_mess2.
          ELSE.
            SELECT SINGLE smtp_addr INTO v_email2
         FROM adr6 AS a
        INNER JOIN usr21 AS b
         ON a~persnumber = b~persnumber
         AND a~addrnumber = b~addrnumber
         WHERE bname = i_viqmel-ernam.
            PERFORM send_email TABLES lt_mess
                               USING v_email1 v_email2 i_viqmel-qmnum
                                v_mess1 v_mess2.
            CLEAR: v_email2.
          ENDIF.
        ENDIF.
      ENDIF.
*      READ TABLE LT_JEST WITH KEY TXT04 = 'TSSC'.
*      IF SY-SUBRC = 0.
      IF i_viqmel-success = 'X' AND i_viqmel-success <> l_success.
**                                             "TASK SUCCESSFUL

        CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                     'submitted was accepted by HMMA QC,'
                     'Please log on to the “Supplier Portal”'
                     INTO v_mess1 SEPARATED BY space.

        CONCATENATE 'to view more details about this notification.'
                    '  '
                    INTO v_mess2 SEPARATED BY space.

*        V_MESS1 = '4B4B'.
*        V_MESS2 = '4B4B'.

        SELECT SINGLE email1 INTO v_email1
        FROM ztqm_vend_email
        WHERE lifnr = i_viqmel-lifnum.
        IF v_email1 IS INITIAL.
          SELECT SINGLE smtp_addr INTO v_email1
            FROM adr6 AS a
           INNER JOIN usr21 AS b
            ON a~persnumber = b~persnumber
            AND a~addrnumber = b~addrnumber
            WHERE bname = i_viqmel-ernam.

          CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                    'email did not get sent to the Vendor'
                     i_viqmel-lifnum
                    'Please inform the vendor to'
                    INTO v_mess1 SEPARATED BY space.

          CONCATENATE 'log on to the “Supplier Portal” to'
                    'update email address and view'
                    'more details about this notification.'
                    INTO v_mess2 SEPARATED BY space.
          DELETE lt_mess INDEX 1.

        ENDIF.
        PERFORM send_email TABLES lt_mess
                           USING v_email1 v_email2 i_viqmel-qmnum
                           v_mess1 v_mess2.
      ENDIF.

*** second layer
    ELSE.                            " sysyem status changed

*      READ TABLE LT_JEST WITH KEY TXT04 = 'TSOS'.
      IF i_viqmel-plandat IS INITIAL AND
         i_viqmel-completed IS INITIAL.          "TASK CREATED

        IF i_viqmel-plandat_old IS INITIAL.
          WRITE: i_viqmel-erdat TO l_date MM/DD/YYYY.


          WRITE: i_viqmel-erdat TO l_date MM/DD/YYYY.
          SELECT SINGLE ltrmn INTO l_date_d
             FROM qmel
             WHERE qmnum = i_viqmel-qmnum.

          WRITE: l_date_d TO l_date_c MM/DD/YYYY.

          CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                      'is created on' l_date
                      INTO v_mess1 SEPARATED BY space.
          CONCATENATE v_mess1 'with required end date of' l_date_c
                     ', please'
                       INTO v_mess1 SEPARATED BY space.

          CLEAR:  v_mess2.
          CONCATENATE 'log on the “Supplier Portal” to view'
                'more details about this notification.'
                INTO v_mess2 SEPARATED BY space.

*          CONCATENATE 'Quality Notification Task;'  I_VIQMEL-QMNUM
*                         'is created on' L_DATE
*                   ', please log on the “Supplier Portal” to view '
*                         INTO V_MESS1 SEPARATED BY SPACE.
*          CLEAR:  V_MESS2.
*          V_MESS2 = 'more details about this notification.'.

*          V_MESS1 = '11111'.
*          V_MESS2 = '11111'.
        ELSE.
          CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                      'submitted was rejected by HMMA QC,'
                      'Please take the appropriate action to'
                      INTO v_mess1 SEPARATED BY space.

          CONCATENATE 'address the problem.'
                      'Please log on to the “Supplier Portal” to'
                      'view more details about this notification.'
                      INTO v_mess2 SEPARATED BY space.

*          V_MESS1 = '4A4A'.
*          V_MESS2 = '4A4A'.
        ENDIF.
*        ENDIF.

        SELECT SINGLE email1 INTO v_email1
               FROM ztqm_vend_email
               WHERE lifnr = i_viqmel-lifnum.
        IF v_email1 IS INITIAL.
          SELECT SINGLE smtp_addr INTO v_email1
            FROM adr6 AS a
           INNER JOIN usr21 AS b
            ON a~persnumber = b~persnumber
            AND a~addrnumber = b~addrnumber
            WHERE bname = i_viqmel-ernam.

          CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                    'email did not get sent to the Vendor'
                     i_viqmel-lifnum
                    'Please inform the vendor to'
                    INTO v_mess1 SEPARATED BY space.

          CONCATENATE 'log on to the “Supplier Portal” to'
                    'update email address and view'
                    'more details about this notification.'
                    INTO v_mess2 SEPARATED BY space.
          DELETE lt_mess FROM 1 TO 2.

        ENDIF.
        PERFORM send_email TABLES lt_mess
                           USING v_email1 v_email2 i_viqmel-qmnum
                           v_mess1 v_mess2.
      ENDIF.

*      READ TABLE LT_JEST WITH KEY TXT04 = 'TSRL'.
*      IF SY-SUBRC = 0.
      IF NOT i_viqmel-plandat IS INITIAL AND
         i_viqmel-completed IS INITIAL.         "TASK RELEASED


        CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                  'was released with “Planned Date of Completion.”'
                    'Please log on to the “Supplier'
                    INTO v_mess1 SEPARATED BY space.


        CONCATENATE 'Portal” to view more details'
                  'about this notification.'
                  INTO v_mess2 SEPARATED BY space.

*        V_MESS1 = '2222'.
*        V_MESS2 = '2222'.

        SELECT SINGLE email1 INTO v_email1
        FROM ztqm_vend_email
        WHERE lifnr = i_viqmel-lifnum.
        IF v_email1 IS INITIAL.
          SELECT SINGLE smtp_addr INTO v_email1
            FROM adr6 AS a
           INNER JOIN usr21 AS b
            ON a~persnumber = b~persnumber
            AND a~addrnumber = b~addrnumber
            WHERE bname = i_viqmel-ernam.

          CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                    'email did not get sent to the Vendor'
                     i_viqmel-lifnum
                    'Please inform the vendor to'
                    INTO v_mess1 SEPARATED BY space.

          CONCATENATE 'log on to the “Supplier Portal” to'
                    'update email address and view'
                    'more details about this notification.'
                    INTO v_mess2 SEPARATED BY space.
          DELETE lt_mess INDEX 1.

          PERFORM send_email TABLES lt_mess
                             USING v_email1 v_email2 i_viqmel-qmnum
                             v_mess1 v_mess2.
        ELSE.

          SELECT SINGLE smtp_addr INTO v_email2
                FROM adr6 AS a
               INNER JOIN usr21 AS b
                ON a~persnumber = b~persnumber
                AND a~addrnumber = b~addrnumber
                WHERE bname = i_viqmel-ernam.
          PERFORM send_email TABLES lt_mess
                              USING v_email1 v_email2 i_viqmel-qmnum
                              v_mess1 v_mess2.
          CLEAR: v_email2.
        ENDIF.
      ENDIF.
*      READ TABLE LT_JEST WITH KEY TXT04 = 'TSCO'.
*      IF SY-SUBRC = 0.
      IF i_viqmel-completed = 'X'
           AND i_viqmel-success = ' '.               "TASK COMPLETED

        CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                   'which is completed is now with HMMA'
                   'QC for further review. Please log on'
                   INTO v_mess1 SEPARATED BY space.

        CONCATENATE 'to the “Supplier Portal” to view more details '
                      'about this notification.'
                     INTO v_mess2 SEPARATED BY space.

*        V_MESS1 = '3333'.
*        V_MESS2 = '3333'.

        SELECT SINGLE email1 INTO v_email1
        FROM ztqm_vend_email
        WHERE lifnr = i_viqmel-lifnum.
        IF v_email1 IS INITIAL.
          SELECT SINGLE smtp_addr INTO v_email1
            FROM adr6 AS a
           INNER JOIN usr21 AS b
            ON a~persnumber = b~persnumber
            AND a~addrnumber = b~addrnumber
            WHERE bname = i_viqmel-ernam.

          CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                    'email did not get sent to the Vendor'
                     i_viqmel-lifnum
                    'Please inform the vendor to'
                    INTO v_mess1 SEPARATED BY space.

          CONCATENATE 'log on to the “Supplier Portal” to'
                    'update email address and view'
                    'more details about this notification.'
                    INTO v_mess2 SEPARATED BY space.
          DELETE lt_mess INDEX 1.

          PERFORM send_email TABLES lt_mess
                             USING v_email1 v_email2 i_viqmel-qmnum
                             v_mess1 v_mess2.
        ELSE.
          SELECT SINGLE smtp_addr INTO v_email2
                  FROM adr6 AS a
                 INNER JOIN usr21 AS b
                  ON a~persnumber = b~persnumber
                  AND a~addrnumber = b~addrnumber
                  WHERE bname = i_viqmel-ernam.
          PERFORM send_email TABLES lt_mess
                              USING v_email1 v_email2 i_viqmel-qmnum
                              v_mess1 v_mess2.
          CLEAR: v_email2.
        ENDIF.
      ENDIF.
*      READ TABLE LT_JEST WITH KEY TXT04 = 'TSSC'.
*      IF SY-SUBRC = 0.
      IF i_viqmel-success = 'X'.               "TASK SUCCESSFUL

        CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                     'submitted was accepted by HMMA QC,'
                     'Please log on to the “Supplier Portal”'
                     INTO v_mess1 SEPARATED BY space.

        CONCATENATE 'to view more details about this notification.'
                    '  '
                    INTO v_mess2 SEPARATED BY space.

*        V_MESS1 = '4B4B'.
*        V_MESS2 = '4B4B'.

        SELECT SINGLE email1 INTO v_email1
        FROM ztqm_vend_email
        WHERE lifnr = i_viqmel-lifnum.
        IF v_email1 IS INITIAL.
          SELECT SINGLE smtp_addr INTO v_email1
            FROM adr6 AS a
           INNER JOIN usr21 AS b
            ON a~persnumber = b~persnumber
            AND a~addrnumber = b~addrnumber
            WHERE bname = i_viqmel-ernam.

          CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                    'email did not get sent to the Vendor'
                     i_viqmel-lifnum
                    'Please inform the vendor to'
                    INTO v_mess1 SEPARATED BY space.

          CONCATENATE 'log on to the “Supplier Portal” to'
                    'update email address and view'
                    'more details about this notification.'
                    INTO v_mess2 SEPARATED BY space.
          DELETE lt_mess INDEX 1.

        ENDIF.
        PERFORM send_email TABLES lt_mess
                           USING v_email1 v_email2 i_viqmel-qmnum
                           v_mess1 v_mess2.
      ENDIF.
*** second layer
    ENDIF.
*** first layer
  ELSE.

*    READ TABLE IT_STATUS WITH KEY WORD = 'NOCO'.
    CALL FUNCTION 'STATUS_CHECK'
      EXPORTING
*       BYPASS_BUFFER     = ' '
*       CLIENT            = SY-MANDT
        objnr             = i_viqmel-objnr
        status            = l_i0072
      EXCEPTIONS
        object_not_found  = 1
        status_not_active = 2.
*  IF SY-SUBRC = 0.

    IF sy-subrc = 0 AND i_viqmel-success = 'X'.   "TASK SUCCESSFUL
      READ TABLE lt_jest WITH KEY txt04 = 'NOCO'.
      IF sy-subrc = 0.                  " system status no change
        IF i_viqmel-success = l_success.
          EXIT.
        ENDIF.
      ENDIF.
      CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
               'is now completed and closed. Please log on to the '
                 '“Supplier Portal” to view more details about '
                  INTO v_mess1 SEPARATED BY space.
      v_mess2 = 'this notification.'.

*          V_MESS1 = '55555'.
*          V_MESS2 = '55555'.
      SELECT SINGLE email1 INTO v_email1
        FROM ztqm_vend_email
       WHERE lifnr = i_viqmel-lifnum.
      IF v_email1 IS INITIAL.
        SELECT SINGLE smtp_addr INTO v_email1
          FROM adr6 AS a
         INNER JOIN usr21 AS b
          ON a~persnumber = b~persnumber
          AND a~addrnumber = b~addrnumber
          WHERE bname = i_viqmel-ernam.

        CONCATENATE 'Quality Notification Task;'  i_viqmel-qmnum
                  'email did not get sent to the Vendor'
                   i_viqmel-lifnum
                  'Please inform the vendor to'
                  INTO v_mess1 SEPARATED BY space.

        CONCATENATE 'log on to the “Supplier Portal” to'
                  'update email address and view'
                  'more details about this notification.'
                  INTO v_mess2 SEPARATED BY space.
        DELETE lt_mess INDEX 1.

      ENDIF.
      PERFORM send_email TABLES lt_mess
                          USING v_email1 v_email2 i_viqmel-qmnum
                          v_mess1 v_mess2.
    ENDIF.
  ENDIF.
ENDIF.
** End of change on 01/29/09

* moved code from above 08/30/06 100565
CHECK i_viqmel-senddat IS INITIAL.
* end move
*--  Check Status of Notification.
CHECK NOT lw_sttxt IS INITIAL.

SPLIT lw_sttxt AT ' ' INTO TABLE it_status.

CHECK sy-subrc = 0.
READ TABLE it_status WITH KEY word = 'NOPR'. "Release Status :'NOPR'

CHECK sy-subrc = 0.

e_viqmel-senddat = sy-datum.

*<<<<<<<< End of EQM01 - ZQMEX_02 QM-Notification Enhancement >>>>>>
