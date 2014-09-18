* Program Name      : ZAHR_ATTBOUNUS
* Author            : Chris Li
* Creation Date     : 2005.02.25.
* Specifications By : Chris Li
* Pattern           :
* Development Request No :UD1K914669
* Addl Documentation:
* Description       : check the attendance bonus eligibility and
*                     upload the bonus payment by using PA30
* 05/14/12        t-code was deleted with recommended by APM system
*&--------------------------------------------------------------------&*
* Modification Logs
* Date       Developer    RequestNo    Description
* 10/11/2005  Shiva       UD1K917906   added new att./absence code 2000.
* 10/13/2005  Shiva       UD1K917936  1.copied the program ZAHR_ATTBONUS
*                                     2.changed the infotype for tcode
*                                      'PA30' from 'P0267' to 'P0015'.
* 10/14/2005  Shiva       UD1K917966   HR decided to give att.bonus for
*                                      military leave, so commented the
*                                      logic for adding abs.type '1033'
*                                      to exclude from attendence bonus.
* 01/09/2006 Hassan        UD1K918830  Changed selection text for
*                                      P_PDATE to date of Origin.
* 03/15/2006 Hassan        UD1K919747  Add 1018 and 1019 Absence Type
*                                      to the list to deny Empl with
*                                      these codes Attendance Bonus.
* 03/16/2006 Hassan        UD1K919747  Add logic to deny Att Bonus for
*                                      Emp with action (ZX,ZW,ZY) in
*                                      PA0000 table.
* 05/04/2006 Hassan        UDIK920507  Add logic to allow Att Bonus for
*                                      Emp with action 'ZC'(FMLA/STD)
*                                      in PA0000 table.
*01/05/2007 Hassan         UD1K930220  Block New Abs code 1057 & 1058.
*03/14/2007 Furong         UD1K940076 Block 1046 attendence program
*&--------------------------------------------------------------------&*
REPORT zahr_attbonus MESSAGE-ID zmhr.

*****************************************************************
*GLOBAL DATA
*****************************************************************
TABLES: pa0001, pa0002, t554s,  t554t, pa0000.

CONSTANTS: c_active(1) TYPE   c VALUE 1.

CONTROLS: tc1 TYPE TABLEVIEW USING SCREEN 100.
DATA: ok_code      LIKE sy-ucomm.
DATA: save_okcode  LIKE sy-ucomm.
DATA: high(8),
      low(8),
      pdate(10).
DATA: total(5),
      etotal(5),
      suc_tot(5).
****************************************************************
*INTERNAL TABLES
****************************************************************
DATA: BEGIN OF it_dis OCCURS 0,
*  pernr       LIKE pa0002-pernr,
  pernr       like pernr-pernr,
  massn       like pa0000-massn,
  massg       like pa0000-massg,
  nachn       LIKE pa0002-nachn,
  vorna       LIKE pa0002-vorna,
  begda       LIKE pa0002-begda,
  persg       LIKE pa0001-persg,
  stell       LIKE pa0001-stell,
  stltx       LIKE t513s-stltx,
*       subty       LIKE t554s-subty,
  amunt       LIKE pa0008-bet08,
       eligi(3)    TYPE c,
       updat(10),
       messg(80) TYPE c,
   chkda      LIKE pa0002-begda,
*       payid      LIKE pa0267-payid,
       mark(1),
      END OF it_dis.
DATA: BEGIN OF IT_FILE OCCURS 0,
       pernr(10),
       nachn(15),
       vorna(15),
       begda(10),
       persg(03),
       stltx(20),
       amunt(08),
       eligi(03) ,
       updat(10),
       messg(80),
*       payid(01) ,
      END OF IT_FILE.
DATA: BEGIN OF it_colnames OCCURS 10,
            name(20),
           END OF it_colnames.

*FOR BDC
DATA: BEGIN OF bdcdata OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

DATA: BEGIN OF it_message OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_message.
DATA: BEGIN OF it_mess OCCURS 0,
        msgty LIKE sy-msgty,
        msgtx(120) TYPE c,
      END OF it_mess.
DATA  g_des(20).
DATA: l_period(02).
DATA: l_incentive_beg  LIKE sy-datum.


*****************************************************************
*END OF  DATA DECLARATION
*****************************************************************


*****************************************************************
*SELECTION-SCREEN
*****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
SELECT-OPTIONS:  r_jobkey FOR pa0001-stell NO-DISPLAY  ,
                 r_abstyp FOR t554s-subty  NO-DISPLAY  ,
                 r_subty1 FOR t554s-subty  NO-DISPLAY  ,
                 r_mastyp for pa0000-massn NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:  p_pernr     FOR  pa0001-pernr.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS:  p_bperd     FOR  sy-datum     OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS:      p_pdate     LIKE sy-datum     OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS:      p_amunt     LIKE pa0008-bet08 OBLIGATORY DEFAULT 100.
*SELECTION-SCREEN SKIP.
*PARAMETERS:      p_payid     LIKE pa0267-payid .
SELECTION-SCREEN END OF BLOCK b1.

*****************************************************************
*END OF SELECTION SCREEN
*****************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM initial.

AT SELECTION-SCREEN.
  PERFORM check_input.

START-OF-SELECTION.

  PERFORM get_employee.
  PERFORM check_eligible.

END-OF-SELECTION.
  PERFORM display_list.


*****************************************************************
*               FORMS
*****************************************************************
*&---------------------------------------------------------------------*
*&      Form  check_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input.
  DATA: l_days TYPE i.
  l_days = p_bperd-high - p_bperd-low + 1.
  IF p_bperd-high IS INITIAL.
    MESSAGE e001 WITH 'Please enter the period end date!'.
  ELSEIF p_bperd-high GT sy-datum.
    MESSAGE e001 WITH 'The period can not be future date!'.
  ELSEIF l_days NE 28.
    MESSAGE e001 WITH 'The period is not four weeks.'.
  ENDIF.


  high = p_bperd-high.
  low  = p_bperd-low.

* CONVERT THE DATE FOR EXTERNAL DATE FORMAT
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
       EXPORTING
            date_internal            = p_pdate
       IMPORTING
            date_external            = pdate
       EXCEPTIONS
            date_internal_is_invalid = 1.

* MAKE THE ASSIGN DESCRIPTION
  g_des(8)   = p_bperd-low.
  g_des+8(1) = '-'.
  g_des+9(8) = p_bperd-high.

  suc_tot = 0.



ENDFORM.                    " check_input
*&---------------------------------------------------------------------*
*&      Form  check_eligible
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_eligible.
  DATA: BEGIN OF lt_p0001 OCCURS 0,
          pernr  LIKE pernr-pernr,
          persg  LIKE pa0001-persg,
          stell  LIKE pa0001-stell,
          begda  LIKE pa0001-begda,
          endda  like pa0001-endda,
          messg(80) TYPE c,
         END OF lt_p0001.

  data: begin of lt_p0000 occurs 0,
        pernr like pernr-pernr,
        massn like pa0000-massn,
        massg like pa0000-massg,
        begda  LIKE pa0000-begda,
        endda  like pa0000-endda,
       end of lt_P0000.

  DATA: l_days TYPE i.
  DATA: lt_513s LIKE t513s OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF lt_p2001 OCCURS 0,
         pernr   LIKE pa2001-pernr,
         awart   LIKE pa2001-awart,
          atext   LIKE t554t-atext,
           END OF lt_p2001.
  DATA: lt_abstx  LIKE t554t OCCURS 0 WITH HEADER LINE.
* check if employee terminated
*  select pernr persg stell begda endda
*               into table lt_p0001
*               from pa0001
*             FOR ALL ENTRIES IN it_dis
*      WHERE pernr  = it_dis-pernr  AND
*           begda LE p_bperd-high   AND
*           endda GE p_bperd-high   AND
*            sprps ne 'X'           AND
*               persg = c_active       .
*  LOOP AT it_dis.
*    CLEAR: lt_p0001.
*    READ TABLE lt_p0001 WITH KEY pernr = it_dis-pernr.
*    IF sy-subrc NE 0 or lt_p0001-persg eq '5' .
*      DELETE it_dis.
*    ENDIF.
*  ENDLOOP.
***************************************
* check if employee terminated
  select pernr persg stell begda endda
               into table lt_p0001
               from pa0001
             FOR ALL ENTRIES IN it_dis
      WHERE pernr  = it_dis-pernr  AND
           begda LE p_bperd-high   AND
           endda GE p_bperd-high   AND
            sprps ne 'X'           AND
               persg = c_active       .
  LOOP AT it_dis.
    CLEAR: lt_p0001.
    READ TABLE lt_p0001 WITH KEY pernr = it_dis-pernr.
    IF sy-subrc NE 0 or lt_p0001-persg eq '5' .
      DELETE it_dis.
    ENDIF.
  ENDLOOP.

*******
**********************************************

  clear: lt_p0001, lt_p0001[].

*& Do not select emp with action type zw,zx,zy from PA0000.

  select pernr massn into table lt_p0000
    from pa0000
    for all entries in it_dis
     where pernr   = it_dis-pernr AND
         endda >= sy-datum        and
         begda <= sy-datum        and
                                  not
        massn in r_mastyp.

  LOOP AT it_dis.
    CLEAR: lt_p0000.
    READ TABLE lt_p0000 WITH KEY pernr = it_dis-pernr.
    IF sy-subrc NE 0

        or lt_p0000-massn eq 'zx'
        or lt_p0000-massn eq 'zy'
        or lt_p0000-massn eq 'zw'.

      DELETE it_dis.

    ENDIF.

  ENDLOOP.
***************************
  clear: lt_p0001, lt_p0001[].

*& Do not select emp with action type zc and sub action type:
*('02','03','05','06','07','08','09','10','11') from PA0000.

  select: pernr massg massn into table lt_p0000
     from pa0000
     for all entries in it_dis
      where pernr   = it_dis-pernr and
          endda >= sy-datum        and
          begda <= sy-datum        and
          massn = 'ZC'             and
          massg in ('02','03','05','06','07','08','09','10','11') .

  LOOP AT it_dis.


    CLEAR: lt_p0000.
    READ TABLE lt_p0000 WITH KEY pernr = it_dis-pernr.
    IF sy-subrc eq 0.

      delete it_dis.
    ELSE.

      modify it_dis.

    ENDIF.
  ENDLOOP.

****************************************
  clear: lt_p0001, lt_p0001[].
* CHECK EMPLOYEE EFFECTIVE
  SELECT pernr persg stell begda endda
               INTO  TABLE lt_p0001
               FROM pa0001
             FOR ALL ENTRIES IN it_dis
      WHERE pernr  = it_dis-pernr  AND
            begda LE p_bperd-low   AND
            endda GE p_bperd-low   AND
            sprps ne 'X'           AND
        persg = c_active       AND NOT
                     stell IN r_jobkey.
* DELETE THE NON ELIGIBLE EE
  LOOP AT it_dis.
    CLEAR: lt_p0001.
    READ TABLE lt_p0001 WITH KEY pernr = it_dis-pernr.
    IF sy-subrc NE 0 or lt_p0001-persg eq '5'.

      DELETE it_dis.
    ELSE.
      it_dis-persg = lt_p0001-persg.
      it_dis-stell = lt_p0001-stell.
      MODIFY it_dis.
    ENDIF.
  ENDLOOP.
* GET THE JOB TITLE
  SELECT * INTO TABLE lt_513s
    FROM t513s
    FOR ALL ENTRIES IN it_dis
    WHERE stell = it_dis-stell.
  LOOP AT it_dis.
    CLEAR: lt_513s.
    READ TABLE lt_513s WITH KEY stell = it_dis-stell.
    IF sy-subrc = 0.
      it_dis-stltx = lt_513s-stltx.
      MODIFY it_dis.
    ENDIF.
  ENDLOOP.

*CHECK THE EE HIRE DAYS >90 DAYS
  LOOP AT it_dis.
    l_days = p_bperd-low - it_dis-begda.
    IF l_days LT 90.
      it_dis-eligi = 'NO'.
      it_dis-messg  = 'Hire less than 90 days'.
      MODIFY it_dis.
    ELSEIF l_days GE 90 AND
l_days LT 118.  "it's the first period
*           "requested by Naveen, changed by chris
      it_dis-chkda = p_bperd-low .   "it_dis-begda.
      MODIFY it_dis.
    ELSEIF l_days GE 118. "it's not the first period
      it_dis-chkda = p_bperd-low.
      MODIFY it_dis.
    ENDIF.
  ENDLOOP.

* CHECK THE EE ABSENCE STATUS.
  CLEAR: lt_p0001, lt_p0001[].
  SELECT pernr awart
    INTO TABLE lt_p2001
    FROM pa2001
    FOR ALL ENTRIES IN it_dis
WHERE  pernr   = it_dis-pernr       AND
    ( ( begda   GE it_dis-chkda
 AND  begda   LE p_bperd-high )
   OR ( endda   GE it_dis-chkda
AND endda   LE p_bperd-high ) )  AND
          awart  IN r_abstyp .
*&----select from PA2002 Attendance also.
  select pernr awart
    appending table lt_p2001
    from pa2002
    for all entries in it_dis
WHERE  pernr   = it_dis-pernr       AND
    ( ( begda   GE it_dis-chkda
 AND  begda   LE p_bperd-high )
   OR ( endda   GE it_dis-chkda
AND endda   LE p_bperd-high ) )  AND
          awart  IN r_subty1 .

* get the absence text
  SELECT * INTO TABLE lt_abstx
    FROM t554t
    FOR ALL ENTRIES IN lt_p2001
WHERE awart = lt_p2001-awart AND
          sprsl = 'EN'.
  LOOP AT lt_p2001.
    CLEAR: lt_abstx.
    READ TABLE lt_abstx WITH KEY awart = lt_p2001-awart.
    IF sy-subrc EQ 0.
      lt_p2001-atext = lt_abstx-atext.
      MODIFY lt_p2001.
    ENDIF.
  ENDLOOP.

* CHECK THE NON-ELIGIBLE EE WHO HAS ABSENCE
  LOOP AT it_dis.
    CLEAR: lt_p2001.
    READ TABLE lt_p2001 WITH KEY pernr = it_dis-pernr.
    IF sy-subrc EQ 0.
      IF it_dis-eligi NE 'NO'.
        it_dis-eligi = 'NO'.
        it_dis-messg = lt_p2001-atext.
        MODIFY it_dis.
      ENDIF.
    ELSE.
      IF it_dis-eligi NE 'NO'.
        it_dis-eligi   = 'YES'.
        it_dis-amunt  = p_amunt.
        etotal = etotal + 1.
        MODIFY it_dis.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_dis LINES total.



ENDFORM.                    " check_eligible
*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_list.
  CALL SCREEN 100.
ENDFORM.                    " display_list
*&---------------------------------------------------------------------*
*&      Form  GET_EMPLOYEE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_employee.
  DATA: lt_name LIKE it_dis OCCURS 0 WITH HEADER LINE.

* select the EE and hire date
  SELECT pernr dat01 AS begda
INTO CORRESPONDING FIELDS OF TABLE it_dis
   FROM pa0041
   WHERE sprps = space AND
         pernr IN p_pernr.

  SORT it_dis BY pernr begda.
  DELETE ADJACENT DUPLICATES FROM it_dis
                   COMPARING pernr begda.
* get the name of ee
  SELECT pernr nachn vorna
INTO CORRESPONDING FIELDS OF TABLE lt_name
   FROM pa0002
   FOR ALL ENTRIES IN it_dis
 WHERE pernr = it_dis-pernr AND
         sprps = space.
  LOOP AT it_dis.
    IF it_dis-begda IS INITIAL.
      MESSAGE e001 WITH 'No hire date for :' it_dis-pernr.
    ENDIF.
    READ TABLE lt_name WITH KEY pernr = it_dis-pernr.
    it_dis-nachn = lt_name-nachn.
    it_dis-vorna = lt_name-vorna.
*    it_dis-payid = p_payid.
    MODIFY it_dis.
  ENDLOOP.


ENDFORM.                    " GET_EMPLOYEE
*&---------------------------------------------------------------------*
*&      Form  INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial.
* MAKE THE RANGES
  r_jobkey-sign     = 'I'.
  r_jobkey-option   = 'EQ'.

  r_jobkey-low      = '10001144'.
  APPEND r_jobkey.
  r_jobkey-low      = '10001145'.
  APPEND r_jobkey.
  r_jobkey-low      = '90000040'.
  APPEND r_jobkey.
  r_jobkey-low      = '90000269'.
  APPEND r_jobkey.
  r_jobkey-low      = '90000657'.
  APPEND r_jobkey.
  r_jobkey-low      = '90000658'.
  APPEND r_jobkey.
  r_jobkey-low      = '90000659'.
  APPEND r_jobkey.
  r_jobkey-low      = '90000661'.
  APPEND r_jobkey.
  r_jobkey-low      = '90002147'.
  APPEND r_jobkey.
************************************
*Do notpay these Abs types*********
  r_abstyp-sign     = 'I'.
  r_abstyp-option   = 'EQ'.
  r_abstyp-low      = '1018'.
  APPEND r_abstyp.
*  r_abstyp-low      = '1019'.
*  APPEND r_abstyp.
  r_abstyp-low      = '1020'.
  APPEND r_abstyp.
  r_abstyp-low      = '1021'.
  APPEND r_abstyp.
  r_abstyp-low      = '1025'.
  APPEND r_abstyp.
*  r_abstyp-low      = '1033'.
*  APPEND r_abstyp.
  r_abstyp-low      = '1034'.
  APPEND r_abstyp.
  r_abstyp-low      = '1035'.
  APPEND r_abstyp.
  r_abstyp-low      = '1044'.
  APPEND r_abstyp.
  r_abstyp-low      = '1045'.
  APPEND r_abstyp.
** changed by Furong >>> UD1K940076
  r_abstyp-low      = '1046'.
  APPEND r_abstyp.
** end of change
  r_abstyp-low      = '1050'.
  APPEND r_abstyp.
  r_abstyp-low      = '1053'.
  APPEND r_abstyp.

  r_abstyp-low      = '1057'.
  APPEND r_abstyp.

  r_abstyp-low      = '1058'.
  APPEND r_abstyp.

  r_abstyp-low      = '2000'.
  APPEND r_abstyp.
*******************************
*Do not pay these Attendance types
** pulling from pa2002
  r_subty1-sign   = 'I'.
  r_subty1-option = 'EQ'.
  r_subty1-low    = '1019'.
  append r_subty1.
  r_subty1-low    = '2000'.
  append r_subty1.

** Changed  Furong 03/14/07 >> UD1K940076  ticket no: 73EC174278
  r_subty1-low    = '1046'.
  append r_subty1.
** end of change
**changed by Furong  >> UD1K930984
  r_subty1-low    = '1058'.
  append r_subty1.
** end of change
******************************
*Do not pay these action types
  r_mastyp-sign   = 'I'.
  r_mastyp-option = 'EQ'.
*  r_mastyp-low    = 'ZC'.
*  append r_mastyp.
  r_mastyp-low    = 'ZW'.
  append r_mastyp.
  r_mastyp-low    = 'ZX'.
  append r_mastyp.
  r_mastyp-low    = 'ZY'.
  append r_mastyp.
*****************************


*******************************


ENDFORM.                    " INITIAL
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS100'.
  SET TITLEBAR 'TITLE_100'.
  DESCRIBE TABLE it_dis LINES tc1-lines.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  save_okcode = ok_code.
  CLEAR ok_code.
  CASE save_okcode.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'UPDAT'.
      PERFORM save_pay.
    WHEN 'DISE'.
      PERFORM dis_eligible.
    WHEN 'ELIG'.
      PERFORM eligible.
    WHEN 'DSEL'.
      PERFORM dis_select_all.
    WHEN 'SELA'.
      PERFORM select_all.
    WHEN 'DNLD'.
      PERFORM DOWNLOAD_FILE.
    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_PAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_pay.
  DATA: i_total TYPE i.
  DATA: i_percent TYPE i.
  DATA: i_processed TYPE i.
  DATA: answer.

  IF total = 0.
    EXIT.
  ENDIF.

* CHECK DUPLICATE UPLOAD
  PERFORM check_duplicate_pay.

* CHECK IF THE DATA HAS BEEN SAVED
*  OR SOME DATA HAVE BEEN SAVED.
  READ TABLE it_dis WITH KEY updat = 'SUCCESS'.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
                                        EXPORTING
                 titel = 'make selection ;;'(b01)
diagnosetext1 = 'ALL OR SOME DATA HAS BEEN SAVED. ;;->B03'(b02)
diagnosetext2 = 'ONLY UNSAVED DATA CAN BE SAVED AGAIN. '(b03)
*      DIAGNOSETEXT3 = 'selection criteria. ;;'(B04)
textline1 = 'ARE YOU SURE YOU WANT TO SAVE ?? ;;->B06'(b05)
*      TEXTLINE2 = 'new planning alternative? ;;'(B06)
                        IMPORTING answer = answer.

    IF answer NE 'J'.
      EXIT.
    ENDIF.
  ELSE.
    CLEAR answer.

*   CONFIRM UPDATE
    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
                                        EXPORTING
                 titel = 'make selection ;;'(b01)
diagnosetext1 = 'THE DATA WILL BE SAVED TO DATABASE. ;;->B03'(b02)
*      DIAGNOSETEXT2 = 'alternative for the specified ;;->B04'(B03)
*      DIAGNOSETEXT3 = 'selection criteria. ;;'(B04)
textline1 = 'ARE YOU SURE YOU WANT TO SAVE?? ;;->B06'(b05)
*      TEXTLINE2 = 'new planning alternative? ;;'(B06)
                        IMPORTING answer = answer.

    IF answer NE 'J'.
      EXIT.
    ENDIF.
  ENDIF.
*
* UPDATE THE DATABASE
  DESCRIBE TABLE it_dis LINES i_total.
  IF i_total NE 0.
    i_processed = 0.
    LOOP AT it_dis.
      CLEAR: bdcdata, bdcdata[].
*     CHECK IF THIS RECORD HAS BEEN SAVED.
*     OR NON-ELIGIBLE OR ALREADY UPLOAD
      IF it_dis-updat = 'SUCCESS' OR
         it_dis-updat = 'EXIST'   OR
              it_dis-eligi NE 'YES'.
        CONTINUE.
      ENDIF.

      PERFORM do_bdc USING it_dis.

      i_processed = i_processed + 1.
      i_percent = ( i_processed * 100 ) / i_total .
      PERFORM progress_indicator USING i_percent.
    ENDLOOP.
  ENDIF.

  suc_tot = '0'.
  LOOP AT it_dis WHERE updat = 'SUCCESS'.
    suc_tot = suc_tot + 1.
  ENDLOOP.

ENDFORM.                    " SAVE_PAY

*&      Form  DO_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_bdc USING p_person LIKE it_dis.
  DATA: l_update TYPE c.

* FILL BDC DATA
  PERFORM fill_bdcdata USING p_person .
* CALL TRANSACTION
  PERFORM call_transaction USING 'PA30' l_update.
  IF l_update = 'S'.
    p_person-updat = 'SUCCESS'.
    MODIFY it_dis FROM p_person TRANSPORTING updat messg.
  ELSE.
    p_person-updat = 'FAIL'.
    MODIFY it_dis FROM p_person TRANSPORTING updat messg.
  ENDIF.

ENDFORM.                    " DO_BDC
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0194   text
*----------------------------------------------------------------------*
FORM call_transaction USING p_tcode p_update.
  DATA: l_msgstr(100) TYPE c.

  CALL TRANSACTION p_tcode
           USING bdcdata
           MODE 'N'
           UPDATE 'S'
       MESSAGES INTO it_message.
* ckeck the message
  IF sy-subrc = 0.
    p_update = 'S'.
    CLEAR: it_dis-messg.
  ELSE.
    p_update = 'F'.

    PERFORM rkc_msg_string USING l_msgstr.

    APPEND it_mess.
    it_dis-messg = l_msgstr.
  ENDIF.

ENDFORM.                    " CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
*       SERACH THE MESSAGE OF BDC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM rkc_msg_string CHANGING p_msg.
  DATA: lw_msg LIKE cfgnl-msglin.

  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       IMPORTING
            msg_lin = lw_msg
       EXCEPTIONS
            OTHERS  = 1.
*  CONCATENATE 'Update failed for' it_person-pernr
*    INTO lw_msg SEPARATED BY space.
  CONCATENATE it_dis-pernr lw_msg INTO lw_msg
                           SEPARATED BY space.
  MOVE: lw_msg TO p_msg.
ENDFORM.                    " RKC_MSG_STRING

*&---------------------------------------------------------------------*
*&      Form  FILL_BDCDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_bdcdata USING p_person LIKE it_dis.
  DATA: l_amount(11) TYPE c.

  WRITE it_dis-amunt TO l_amount .

  PERFORM bdc_dynpro      USING 'SAPMP50A' '1000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                       '/00'.
  PERFORM bdc_field       USING 'RP50G-PERNR'
                               p_person-pernr.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                               'RP50G-CHOIC'.
  PERFORM bdc_field       USING 'RP50G-CHOIC'
                                       '0015'.
  PERFORM bdc_dynpro      USING 'SAPMP50A' '1000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=INS'.
  PERFORM bdc_field       USING 'RP50G-PERNR'
                               p_person-pernr.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                               'RP50G-ENDDA'.
  PERFORM bdc_field       USING 'RP50G-TIMR6'
                                          'X'.
  PERFORM bdc_field       USING 'RP50G-BEGDA'
                                        pdate.
  PERFORM bdc_field       USING 'RP50G-ENDDA'
                                        pdate.
  PERFORM bdc_field       USING 'RP50G-CHOIC'
                                       '0015'.
  PERFORM bdc_field       USING 'RP50G-SUBTY'
                                       '0318'.
  PERFORM bdc_dynpro      USING 'MP001500' '2000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                               'P0015-ZUORD'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=UPD'.
  PERFORM bdc_field       USING 'P0015-LGART'
                                       '0318'.
  PERFORM bdc_field       USING 'Q0015-BETRG'
                                     l_amount.
  PERFORM bdc_field       USING 'P0015-WAERS'
                                        'USD'.
  PERFORM bdc_field       USING 'P0015-BEGDA'
                                        pdate.
  PERFORM bdc_field       USING 'P0015-ZUORD'
                                        g_des.
  PERFORM bdc_dynpro      USING 'MP001500' '2000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                       '/00'.

ENDFORM.                    " FILL_BDCDATA
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.
*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_PERCENT  text
*----------------------------------------------------------------------*
FORM progress_indicator USING    p_percent.
  DATA: l_text(40).
  DATA: i_mod TYPE i.

  l_text = p_percent.
  CONDENSE l_text.
  i_mod = p_percent MOD 5.
  IF i_mod = 0.
    CONCATENATE l_text '% PROCESSED' INTO l_text.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              text = l_text.
  ENDIF.
ENDFORM.                    " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Module  MODIFY_DIS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_dis INPUT.
  MODIFY it_dis INDEX tc1-current_line
         TRANSPORTING amunt eligi mark.
ENDMODULE.                 " MODIFY_DIS  INPUT
*&---------------------------------------------------------------------*
*&      Form  DIS_ELIGIBLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dis_eligible.
  LOOP AT it_dis WHERE mark = 'X'.
    it_dis-eligi = 'NO'.
    MODIFY it_dis.
  ENDLOOP.
ENDFORM.                    " DIS_ELIGIBLE
*&---------------------------------------------------------------------*
*&      Form  ELIGIBLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM eligible.
  LOOP AT it_dis WHERE mark = 'X'.
    it_dis-eligi = 'YES'.
    MODIFY it_dis.
  ENDLOOP.

ENDFORM.                    " ELIGIBLE
*&---------------------------------------------------------------------*
*&      Form  DIS_SELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dis_select_all.
  it_dis-mark = space.
  MODIFY it_dis TRANSPORTING mark WHERE mark NE space.

ENDFORM.                    " DIS_SELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  SELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_all.
  it_dis-mark = 'X'.
  MODIFY it_dis TRANSPORTING mark WHERE mark NE 'X'.
ENDFORM.                    " SELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  check_duplicate_pay
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_duplicate_pay.
*  DATA: BEGIN OF lt_p0267 OCCURS 0,
*         pernr  LIKE pa0267-pernr,
*         begda  LIKE pa0267-begda,
*         endda  LIKE pa0267-endda,
*         zuord  LIKE pa0267-zuord,
*         lgart  LIKE pa0267-lgart,
*         betrg  LIKE pa0267-betrg,
*         payid  LIKE pa0267-payid,
*        END OF lt_p0267.
  DATA: BEGIN OF lt_p0015 OCCURS 0,
          pernr  LIKE pa0015-pernr,
          begda  LIKE pa0015-begda,
          endda  LIKE pa0015-endda,
          zuord  LIKE pa0015-zuord,
          lgart  LIKE pa0015-lgart,
          betrg  LIKE pa0015-betrg,
           END OF lt_p0015.

  DATA: l_begin LIKE sy-datum.
  DATA: l_end   LIKE sy-datum.
  DATA: l_period(20).
  CONCATENATE p_pdate(4) '0101' INTO l_begin.
  CONCATENATE p_pdate(4) '1231' INTO l_end.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_p0015
                                           FROM pa0015
                             FOR ALL ENTRIES IN it_dis
                       WHERE pernr = it_dis-pernr  AND
                             lgart = '0318'        AND
                             betrg NE 0            AND
                             endda GE l_begin      AND
                                        endda LE l_end.
  CHECK sy-subrc EQ 0.
  LOOP AT it_dis WHERE eligi = 'YES'.

    CLEAR: lt_p0015.
    READ TABLE lt_p0015 WITH KEY pernr = it_dis-pernr
                                      begda = p_pdate.
    IF sy-subrc EQ 0.
      it_dis-updat = 'EXIST'.
      it_dis-messg = 'Payment already exist for this period'.
      MODIFY it_dis.
      continue.
    endif.

    CLEAR: lt_p0015.
    READ TABLE lt_p0015 WITH KEY pernr = it_dis-pernr
                                        zuord = g_des.
    IF sy-subrc EQ 0.
      it_dis-updat = 'EXIST'.
      it_dis-messg = 'Payment already exist for this period'.
      MODIFY it_dis.
    ENDIF.


  ENDLOOP.


ENDFORM.                    " check_duplicate_pay
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form DOWNLOAD_FILE.
  DATA : FILENAME LIKE  RLGRAP-FILENAME.
  clear: it_file[].
  loop at it_dis.
    clear: it_file.
    move-corresponding it_dis to it_file.
    append it_file.
  endloop.
  perform make_colname.
  PERFORM GET_FILENAME USING FILENAME.
  PERFORM DOWNLOAD     USING FILENAME.

endform.                    " DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILENAME  text
*----------------------------------------------------------------------*
form GET_FILENAME using    p_file  LIKE rlgrap-filename.
  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: FNAME     LIKE RLGRAP-FILENAME.
  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.

* Build Filter for Fileselektor
*  FNAME = 'C:/MY DOCUMENTS/   .TXT'.
  tmp_mask = ',*.*,*.*.'.
  CALL FUNCTION 'WS_FILENAME_GET'
         EXPORTING
         def_filename     = FNAME
         def_path         = FNAME
      mask             = tmp_mask
           mode             = 'S'
**           TITLE            = ' '
         IMPORTING
  filename         = tmp_filename
*         RC               =
         EXCEPTIONS
            inv_winsys       = 01
            no_batch         = 02
            selection_cancel = 03
            selection_error  = 04.

  IF sy-subrc = 0.
    P_FILE = tmp_filename.
  ENDIF.
endform.                    " GET_FILENAME
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILENAME  text
*----------------------------------------------------------------------*
form DOWNLOAD using    p_filename.
  DATA:l_text(30).

  IF P_FILENAME IS INITIAL.
    EXIT.
  ENDIF.
  CALL FUNCTION 'WS_DOWNLOAD'
      EXPORTING
filename                = p_filename
filetype                = 'DAT'
*           col_select              = 'X'
      TABLES
data_tab                = it_file
fieldnames              = it_colnames
    EXCEPTIONS
    file_open_error         = 1
    file_write_error        = 2
    invalid_filesize        = 3
    invalid_table_width     = 4
    invalid_type            = 5
    no_batch                = 6
    unknown_error           = 7
*         GUI_REFUSE_FILETRANSFER = 8
    OTHERS                  = 9
       .
  IF sy-subrc <> 0.
    l_text = 'File Download Not Success'.
  ELSE.
    l_text = 'File Download Success '.
  ENDIF.
  MESSAGE S001 WITH L_TEXT.
endform.                    " DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  make_colname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_colname.
* MAKE COLIMN HEADER
  clear: it_colnames, it_colnames[].
  it_colnames-name = 'PERNR'.
  APPEND it_colnames.
  it_colnames-name = 'LAST NAME'.
  APPEND it_colnames.
  it_colnames-name = 'FIRST NAME'.
  APPEND it_colnames.
  it_colnames-name = 'HIRE DATE'.
  APPEND it_colnames.
  it_colnames-name = 'GROUP'.
  APPEND it_colnames.
  it_colnames-name = 'JOB TITLE'.
  APPEND it_colnames.
  it_colnames-name = 'AMOUNT'.
  APPEND it_colnames.
  it_colnames-name = 'ELEIGIBLE'.
  APPEND it_colnames.
  it_colnames-name = 'UPDATE'.
  APPEND it_colnames.
  it_colnames-name = 'MESSAGE'.
  APPEND it_colnames.
*  it_colnames-name = 'PAYID'.
*  APPEND it_colnames.

endform.                    " make_colname
