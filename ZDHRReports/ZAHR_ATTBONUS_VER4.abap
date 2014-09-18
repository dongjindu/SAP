* Program Name      : ZAHR_ATTBONUS_VER3
* Author            : ig.moon ( copied from ZAHR_ATTBONUS_NEW )
* Creation Date     : 2008.04.22.
* Specifications By : Imtiaz Ahmad
* Pattern           : BDC program
* Addl Documentation:
* Description       : check the attendance bonus eligibility and
*                     upload the bonus payment by using PA30
*&--------------------------------------------------------------------&*
* Modification Logs
* Date       Developer    Description
* 04/22/2008  ig          copied from ZAHR_ATTBONUS and fully revised
* 02/28/2010 VALERIAN     Remove A/A type: 1019, 1027, 1058 from
*            HIS20094     occurrences list
*                         For Action type: ZC and Reason for action:
*                         02 or 05 or 06, count the days as occurrences
*
* 04/26/2010 VALERIAN     UD1K951488 Various fixes.
* 11/30/2011 VALERIAN     UD1K953480 Add new absence type in the check
*                                    logic.
* 05/23/2012 VALERIAN     UD1K954831 Add A/A type 3300 and 3303 as
*                                    unexcused absence
*&--------------------------------------------------------------------&*
REPORT zahr_attbonus_ver3 MESSAGE-ID zmhr.

*****************************************************************
*GLOBAL DATA
*****************************************************************
TABLES: pa0001, pa0002, t554s,  t554t, pa0000, p0001, pc261.
INCLUDE zacoui00.

CONSTANTS: c_active(1) TYPE   c VALUE 1.
CONTROLS: tc1 TYPE TABLEVIEW USING SCREEN 200.

DATA: save_okcode  LIKE sy-ucomm.
DATA: high(8),
      low(8),
      pdate(10).
DATA: total(5),
      etotal(5),
      suc_tot(5).

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __focus.
  call method cl_gui_control=>set_focus
      exporting
        control = &1 .
END-OF-DEFINITION.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DATA  : flag_data_changed,
        info(80).
DATA: BEGIN OF ftab OCCURS 10,
        fcode(6),
      END OF ftab.

DEFINE __define_not_important.
* { not important
* Total Doc. Count to be created.
  data  : total_doc_cnt type i,
          current_doc_cnt type i.
  data : percentage type p,$mod type i,
         $current_cnt(10),$total_cnt(10),$text(100) .
  clear : total_doc_cnt,current_doc_cnt.
* }
END-OF-DEFINITION.

****************************************************************
*INTERNAL TABLES
****************************************************************

DATA: BEGIN OF $it_dis OCCURS 0,
  pernr       LIKE pernr-pernr,
  massn       LIKE pa0000-massn,
  massg       LIKE pa0000-massg,
  nachn       LIKE pa0002-nachn,
  vorna       LIKE pa0002-vorna,
  begda       LIKE pa0002-begda,
  persg       LIKE pa0001-persg,
  stell       LIKE pa0001-stell,
  stltx       LIKE t513s-stltx,
  amunt       LIKE pa0008-bet08,
  eligi(3)    TYPE c,
  updat(10),
  messg(80) TYPE c,
  chkda      LIKE pa0002-begda,
  mark(1),
      END OF $it_dis.

TYPES: BEGIN OF ty_out.
        INCLUDE STRUCTURE zshr_attbonus_alv.
TYPES   abcnt1   TYPE zabcnt.                               "HIS20094
TYPES   celltab  TYPE lvc_t_styl.
TYPES   tabcolor TYPE slis_t_specialcol_alv.
TYPES: END OF ty_out.

DATA  it_dis   TYPE TABLE OF ty_out  WITH HEADER LINE.
DATA  $gt_out LIKE it_dis OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_file OCCURS 0,
       pernr(10),
       nachn(15),
       vorna(15),
       begda(10),
       persg(03),
       stltx(20),
       text1(10),
       pamunt(12),
       amunt(10),
       eligi(03) ,
       updat(10),
       abcnt(10),
       abcnt1(10),                                          "UD1K951488
       messg(80),
      END OF it_file.
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

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ztcou131_k,
              co_area   TYPE kokrs,
              fisc_year TYPE gjahr,
              version   TYPE versn,
              kostl     TYPE kostl,
              kstar     TYPE kstar,
           END OF ztcou131_k.

    TYPES: ztcou131_key   TYPE STANDARD TABLE OF ztcou131_k,
           ztcou131_table TYPE STANDARD TABLE OF ztcou131.

    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed,
                       get_deleted_rows
             EXPORTING
                       deleted_rows TYPE ztcou131_table,

      refresh_delta_tables.

  PRIVATE SECTION.
    DATA deleted_rows TYPE STANDARD TABLE OF ztcou131.

* This flag is set if any error occured in one of the
* following methods:
    DATA: error_in_data TYPE c.
    METHODS:
      update_delta_tables
         IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

* Setting for Change data
  METHOD handle_data_changed.

** remember deleted lines for saving
*    CALL METHOD update_delta_tables( er_data_changed ).
*
*    PERFORM data_changed USING er_data_changed.
*
*    __set_refresh_mode true.
*    CALL METHOD g_grid->refresh_table_display
*         EXPORTING is_stable = stable.

  ENDMETHOD.                    " handle_data_changed

  METHOD get_deleted_rows.
    deleted_rows = me->deleted_rows.
  ENDMETHOD.

  METHOD refresh_delta_tables.
    CLEAR me->deleted_rows[].
  ENDMETHOD.

  METHOD update_delta_tables.
*    DATA: l_del_row TYPE lvc_s_moce,
*          ls_ztcou131 TYPE ztcou131,
*          ls_outtab LIKE LINE OF it_dis.
*
*    LOOP AT pr_data_changed->mt_deleted_rows INTO l_del_row.
*      READ TABLE it_dis INTO ls_outtab INDEX l_del_row-row_id.
*      IF sy-subrc NE 0.
*        MESSAGE i000(0k) WITH text-e01. "Internal error
*      ELSE.
*        MOVE-CORRESPONDING ls_outtab TO ls_ztcou131.
*        APPEND ls_ztcou131 TO deleted_rows.
*      ENDIF.
*    ENDLOOP.
  ENDMETHOD.

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

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
                 r_mastyp FOR pa0000-massn NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:  p_pernr     FOR  pa0001-pernr.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS:  p_bperd     FOR  sy-datum     OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS:      p_pdate     LIKE sy-datum     OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS:      p_amunt     LIKE pa0008-bet08 NO-DISPLAY.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
PARAMETERS :   p_abkrs LIKE  pa0001-abkrs OBLIGATORY DEFAULT '11'.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN END OF BLOCK b1.

* Layout
*selection-screen begin of block b4 with frame title text-01s.
PARAMETER p_vari TYPE slis_vari NO-DISPLAY .
*selection-screen end of block b4.

*****************************************************************
*END OF SELECTION SCREEN
*****************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM initial.

AT SELECTION-SCREEN.
  PERFORM check_input.

START-OF-SELECTION.

  PERFORM show_progress     USING 'Initializing...' '5'.
  PERFORM get_employee.
  PERFORM check_eligible.

  info = text-015.

  PERFORM show_progress     USING 'Preparing screen...' '95'.

*  PERFORM display_list.

END-OF-SELECTION.
  CALL SCREEN 200.

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
*  ELSEIF l_days NE 28.
*    MESSAGE e001 WITH 'The period is not four weeks.'.
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
          endda  LIKE pa0001-endda,
          messg(80) TYPE c,
          persk  LIKE pa0001-persk,
          categ  TYPE zhrcatg,
         END OF lt_p0001.

  DATA: BEGIN OF lt_p0000 OCCURS 0,
        pernr LIKE pernr-pernr,
        massn LIKE pa0000-massn,
        massg LIKE pa0000-massg,
        begda LIKE pa0000-begda,
        endda LIKE pa0000-endda,
        stat2 LIKE pa0000-stat2,
       END OF lt_p0000.

* Begin of HIS20094
  DATA: loa_date TYPE i,
        loa_date_tot TYPE i,
        loa_tot(5) TYPE c.

  DATA: BEGIN OF lt_loa OCCURS 0,
        begda LIKE pa0000-begda,
        endda LIKE pa0000-endda,
        pernr LIKE pernr-pernr,
       END OF lt_loa.

  DATA: lt_loa_pernr LIKE lt_loa OCCURS 0 WITH HEADER LINE.
* End of HIS20094

  DATA: l_days TYPE i.
  DATA: lt_513s LIKE t513s OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF lt_p2001 OCCURS 0,
         pernr   LIKE pa2001-pernr,
         awart   LIKE pa2001-awart,
         atext   LIKE t554t-atext,
         begda   LIKE pa2001-begda,
         endda   LIKE pa2001-endda,
        END OF lt_p2001.

  DATA: lt_abstx  LIKE t554t OCCURS 0 WITH HEADER LINE.

  SELECT pernr persg stell begda endda persk
               INTO CORRESPONDING FIELDS OF TABLE lt_p0001
               FROM pa0001
             FOR ALL ENTRIES IN it_dis
      WHERE pernr  = it_dis-pernr  AND
           begda LE p_bperd-high   AND
           endda GE p_bperd-high   AND
            sprps NE 'X'           AND
               persg = c_active       .

  SORT lt_p0001 BY pernr.
  DATA $ix LIKE sy-tabix.

  PERFORM show_progress     USING 'Gather data...' '15'.

  IF NOT it_dis[]  IS INITIAL.
    __cls lt_p0000.
    SELECT pernr massn stat2
  INTO CORRESPONDING FIELDS OF TABLE lt_p0000
      FROM pa0000
      FOR ALL ENTRIES IN it_dis
       WHERE pernr   = it_dis-pernr
         AND endda >= sy-datum
         AND begda <= sy-datum
         AND NOT massn IN r_mastyp.

    SORT lt_p0000 BY pernr.
  ENDIF.

  LOOP AT it_dis.
    $ix = sy-tabix.
    CLEAR lt_p0001.
    READ TABLE lt_p0001 WITH KEY pernr = it_dis-pernr
                      BINARY SEARCH.
    IF sy-subrc NE 0 OR lt_p0001-persg EQ '5' .
      DELETE it_dis INDEX $ix .
      CONTINUE.
    ENDIF.

*    IF lt_p0001-persk EQ 'U0'.
*      READ TABLE lt_p0000 WITH KEY pernr = it_dis-pernr BINARY SEARCH.
*      IF sy-subrc EQ 0 AND lt_p0000-stat2 NE '3'.
*        DELETE it_dis INDEX $ix.
*        CONTINUE.
*      ENDIF.
*    ENDIF.

    READ TABLE lt_p0000 WITH KEY pernr = it_dis-pernr BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_dis-stat2 = lt_p0000-stat2.
      CASE it_dis-stat2.
        WHEN '0'.
          it_dis-text1 = 'Withdrawn'.
        WHEN '1'.
          it_dis-text1 = 'Inactive'.
        WHEN '2'.
          it_dis-text1 = 'Retire'.
        WHEN '3'.
          it_dis-text1 = 'Active'.
        WHEN OTHERS.
      ENDCASE.
      MODIFY it_dis INDEX $ix TRANSPORTING stat2 text1.
    ENDIF.

    PERFORM get_emp_categ  USING    lt_p0001-persg lt_p0001-persk
                           CHANGING lt_p0001-categ.

    IF lt_p0001-categ NE 'B'.
      DELETE it_dis INDEX $ix .
    ENDIF.

  ENDLOOP.


**********************************************
  CHECK NOT it_dis[] IS INITIAL.

*& Do not select emp with action type zw,zx,zy from PA0000.

  LOOP AT it_dis.
    $ix = sy-tabix.
    CLEAR lt_p0000.
    READ TABLE lt_p0000 WITH KEY pernr = it_dis-pernr BINARY SEARCH.
    IF sy-subrc NE 0
      OR lt_p0000-massn EQ 'zx'
      OR lt_p0000-massn EQ 'zy'
      OR lt_p0000-massn EQ 'zw'
      OR lt_p0000-stat2 EQ '0'.                             "UD1K951488
      DELETE it_dis INDEX $ix.
    ENDIF.
  ENDLOOP.

***************************

  PERFORM show_progress     USING 'Gather data...' '25'.

  __cls lt_p0000.
*& Do not select emp with action type zc and sub action type:
*('02','03','05','06','07','08','09','10','11') from PA0000.

  SELECT: pernr massg massn INTO TABLE lt_p0000
     FROM pa0000
     FOR ALL ENTRIES IN it_dis
      WHERE pernr   = it_dis-pernr
        AND endda >= p_pdate
        AND begda <= p_pdate
        AND massn = 'ZC'
*        AND massg IN ('02','03','05','06','07','08','09','10','11') .
        AND massg IN ('08','09','10','13') .

  SORT lt_p0000 BY pernr.

  LOOP AT it_dis.
    $ix = sy-tabix.
    CLEAR lt_p0000.
    READ TABLE lt_p0000 WITH KEY pernr = it_dis-pernr BINARY SEARCH.
    IF sy-subrc EQ 0.
      DELETE it_dis INDEX $ix .
    ENDIF.
  ENDLOOP.

* Begin of HIS20094
* Include this Action type 'ZC' and reason code '02', '05', '06'.
  SELECT: begda endda pernr INTO TABLE lt_loa
     FROM pa0000
     FOR ALL ENTRIES IN it_dis
      WHERE pernr = it_dis-pernr
        AND massn = 'ZC'
        AND massg IN ('02','05','06').

  SORT lt_loa BY pernr.
* End of HIS20094

  CHECK NOT it_dis[] IS INITIAL.

  PERFORM show_progress     USING 'Gather data...' '35'.

****************************************
  __cls lt_p0001.

* CHECK EMPLOYEE EFFECTIVE
  SELECT pernr persg stell begda endda persk
               INTO  CORRESPONDING FIELDS OF TABLE lt_p0001
               FROM pa0001
             FOR ALL ENTRIES IN it_dis
      WHERE pernr  = it_dis-pernr
        AND begda LE p_bperd-low
        AND endda GE p_bperd-low
        AND sprps NE 'X'
        AND persg = c_active
        AND NOT stell IN r_jobkey.

  SORT lt_p0001 BY pernr.

* DELETE THE NON ELIGIBLE EE
  LOOP AT it_dis.

    $ix = sy-tabix.
    CLEAR lt_p0001.
    READ TABLE lt_p0001 WITH KEY pernr = it_dis-pernr BINARY SEARCH.

    IF sy-subrc NE 0 OR lt_p0001-persg EQ '5'.
      DELETE it_dis INDEX $ix.
      CONTINUE.
    ENDIF.

    it_dis-persg = lt_p0001-persg.
    it_dis-stell = lt_p0001-stell.
    MODIFY it_dis INDEX $ix TRANSPORTING persg stell.

  ENDLOOP.

  PERFORM show_progress     USING 'Gather data...' '45'.

  CHECK NOT it_dis[] IS INITIAL.

* GET THE JOB TITLE
  SELECT * INTO TABLE lt_513s
    FROM t513s
    FOR ALL ENTRIES IN it_dis
    WHERE stell = it_dis-stell.

  SORT lt_513s BY stell.

  LOOP AT it_dis.
    $ix = sy-tabix.
    CLEAR lt_513s.
    READ TABLE lt_513s WITH KEY stell = it_dis-stell BINARY SEARCH.
    IF sy-subrc = 0.
      it_dis-stltx = lt_513s-stltx.
      MODIFY it_dis INDEX $ix TRANSPORTING stltx.
    ENDIF.
  ENDLOOP.

*CHECK THE EE HIRE DAYS >90 DAYS OR INACTIVE
  LOOP AT it_dis.
    $ix = sy-tabix.
*    l_days = p_bperd-low - it_dis-begda.
    l_days = p_bperd-high - it_dis-begda.
    IF l_days LT 90.
      it_dis-eligi = 'NO'.
      it_dis-messg  = 'Hire less than 90 days'.
      MODIFY it_dis  INDEX $ix TRANSPORTING eligi messg.
    ELSEIF l_days GE 90 AND l_days LT 118.
      "it's the first period
*           "requested by Naveen, changed by chris
      it_dis-chkda = p_bperd-low .   "it_dis-begda.
      MODIFY it_dis  INDEX $ix TRANSPORTING chkda.
    ELSEIF l_days GE 118. "it's not the first period
      it_dis-chkda = p_bperd-low.
      MODIFY it_dis  INDEX $ix TRANSPORTING chkda.
    ENDIF.
* BEGIN OF UD1K951488
    IF it_dis-stat2 = '1'.
      it_dis-eligi = 'NO'.
      MODIFY it_dis  INDEX $ix TRANSPORTING eligi.
    ENDIF.
* END OF UD1K951488
  ENDLOOP.

  PERFORM show_progress     USING 'Gather data...' '55'.

* CHECK THE EE ABSENCE STATUS.
  __cls lt_p2001.
  SELECT pernr awart begda endda
    INTO CORRESPONDING FIELDS OF TABLE lt_p2001
    FROM pa2001
    FOR ALL ENTRIES IN it_dis
        WHERE  pernr   = it_dis-pernr
*          AND  ( ( begda GE it_dis-chkda AND begda LE p_bperd-high )
*              OR ( endda GE _dis-chkda AND endda LE p_bperd-high ) )
          AND ( begda IN p_bperd OR endda IN p_bperd )
          and AWART  in R_ABSTYP .

*&----select from PA2002 Attendance also.

  SELECT pernr awart  begda endda
    APPENDING CORRESPONDING FIELDS OF TABLE lt_p2001
    FROM pa2002
    FOR ALL ENTRIES IN it_dis
            WHERE  pernr   = it_dis-pernr
*           AND ( ( begda GE it_dis-chkda  AND begda LE p_bperd-high )
*              OR ( endda GE it_dis-chkda  AND endda LE p_bperd-high ) )
           AND ( begda IN p_bperd OR endda IN p_bperd )
           and AWART  in R_SUBTY1 .

  PERFORM show_progress     USING 'Gather data...' '65'.

  READ TABLE lt_p2001 INDEX 1.
  IF sy-subrc EQ 0.
* get the absence text
    SELECT * INTO TABLE lt_abstx
      FROM t554t
      FOR ALL ENTRIES IN lt_p2001
      WHERE awart = lt_p2001-awart
        AND sprsl = 'EN'.
  ENDIF.

  SORT lt_abstx BY awart.
  LOOP AT lt_p2001.
    $ix = sy-tabix.
    CLEAR lt_abstx.
    READ TABLE lt_abstx WITH KEY awart = lt_p2001-awart BINARY SEARCH.
    IF sy-subrc EQ 0.
      lt_p2001-atext = lt_abstx-atext.
      MODIFY lt_p2001 INDEX $ix TRANSPORTING atext.
    ENDIF.
  ENDLOOP.

  PERFORM show_progress     USING 'Gather data...' '75'.

* CHECK THE NON-ELIGIBLE EE WHO HAS ABSENCE

* by ig.moon 10/23 {
*  SORT lt_p2001 BY pernr.

  SORT lt_p2001 BY pernr begda.
  DELETE ADJACENT DUPLICATES FROM lt_p2001 COMPARING pernr begda.

* }

  DATA : $cnt TYPE i,
         $atext LIKE lt_p2001-atext,
         $tcnt(5).

  __define_not_important.
  DESCRIBE TABLE it_dis LINES total_doc_cnt.
  $total_cnt = total_doc_cnt.

  LOOP AT it_dis.

    ADD 1 TO current_doc_cnt.
    $current_cnt = current_doc_cnt.
*    CONCATENATE it_dis-pernr ':' $current_cnt '/' $total_cnt
*    INTO $text.
    CONCATENATE 'Calculating...' $current_cnt '/' $total_cnt
    INTO $text.
    CONDENSE $text.
*    percentage = current_doc_cnt / total_doc_cnt * 100.
    percentage = current_doc_cnt MOD 10.
*    IF percentage EQ 0.
    PERFORM show_progress USING $text 0.
*    ENDIF.
    $ix = sy-tabix.
    CLEAR lt_p2001.
    CLEAR : $cnt, $atext.

    READ TABLE lt_p2001 WITH KEY pernr = it_dis-pernr BINARY SEARCH.
    IF sy-subrc EQ 0.
      LOOP AT lt_p2001 FROM sy-tabix.
        IF lt_p2001-pernr NE it_dis-pernr.
          EXIT.
        ENDIF.
        $atext = lt_p2001-atext.
        ADD 1 TO $cnt.
      ENDLOOP.
    ENDIF.

* Begin of HIS20094 - Calculate LOA Date
    __cls lt_loa_pernr.
    LOOP AT lt_loa WHERE pernr = it_dis-pernr.
      lt_loa_pernr = lt_loa.
      APPEND lt_loa_pernr.
    ENDLOOP.

    CLEAR loa_date_tot.
    PROVIDE * FROM lt_loa_pernr BETWEEN p_bperd-low AND p_bperd-high.
      loa_date = lt_loa_pernr-endda - lt_loa_pernr-begda + 1.
      loa_date_tot = loa_date_tot + loa_date.
    ENDPROVIDE.

    it_dis-abcnt1 = loa_date_tot.
    loa_tot       = loa_date_tot.

* End of HIS20094

    CLEAR p_amunt.

    IF $cnt > 2.
*     IF it_dis-eligi NE 'YES'.                             "UD1K951488
*     IF it_dis-eligi NE 'NO'.                              "UD1K951488
      it_dis-eligi = 'NO'.
      $tcnt = $cnt.

* Begin of HIS20094 - Set Absence Descriptions
      IF NOT loa_date_tot IS INITIAL.
        CONCATENATE $tcnt ' Absence(s)(' $atext '..)+' loa_tot
                          'Occurrence(s) LOA'
        INTO it_dis-messg SEPARATED BY space.
      ELSE.

        CONCATENATE $tcnt ' Absence(s)(' $atext '..)' INTO it_dis-messg
                                      SEPARATED BY space.

      ENDIF.

      CONDENSE it_dis-messg.
* End of HIS20094

      it_dis-abcnt = $cnt.

      PERFORM read_payroll_data USING it_dis-pernr
                                CHANGING p_amunt.

      it_dis-pamunt = p_amunt.
      MODIFY it_dis INDEX $ix TRANSPORTING eligi pamunt messg abcnt
                                           abcnt1.          "HIS20094
*     ENDIF.                                                "UD1K951488
    ELSE.

      PERFORM read_payroll_data USING it_dis-pernr          "UD1K951488
                                CHANGING p_amunt.           "UD1K951488

      IF it_dis-eligi NE 'NO'.
        it_dis-eligi   = 'YES'.
*       PERFORM read_payroll_data USING it_dis-pernr        "UD1K951488
*                                 CHANGING p_amunt.         "UD1K951488
        IF $cnt EQ 0.
          it_dis-amunt  = p_amunt * '0.025'.
        ENDIF.
        IF $cnt EQ 1.
          it_dis-amunt  = p_amunt * '0.0125'.
        ENDIF.
        IF $cnt EQ 2.
          it_dis-amunt  = p_amunt * '0.006'.
        ENDIF.

* BEGIN OF UD1K951488
*        etotal = etotal + 1.
*        it_dis-abcnt = $cnt.
*        it_dis-pamunt = p_amunt.
*       MODIFY it_dis INDEX $ix TRANSPORTING eligi pamunt amunt abcnt
*                                            abcnt1.        "HIS20094
* END OF UD1K951488

      ENDIF.

* BEGIN OF UD1K951488
      etotal = etotal + 1.
      it_dis-abcnt = $cnt.
      it_dis-pamunt = p_amunt.

      MODIFY it_dis INDEX $ix TRANSPORTING eligi pamunt amunt abcnt
                                             abcnt1.
* END OF UD1K951488
    ENDIF.

  ENDLOOP.

  DESCRIBE TABLE it_dis LINES total.
**S> 08/04/11 Paul
  PERFORM show_progress     USING 'Gather data...' '85'.
**E<
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
  DATA: lt_name LIKE $it_dis OCCURS 0 WITH HEADER LINE.

  __cls : $it_dis, it_dis.

* select the EE and hire date
  SELECT pernr dat01 AS begda
INTO CORRESPONDING FIELDS OF TABLE $it_dis
   FROM pa0041
   WHERE sprps = space AND
         pernr IN p_pernr.

  SORT $it_dis BY pernr begda.
  DELETE ADJACENT DUPLICATES FROM $it_dis
                   COMPARING pernr begda.
* get the name of ee
  SELECT pernr nachn vorna
INTO CORRESPONDING FIELDS OF TABLE lt_name
   FROM pa0002
   FOR ALL ENTRIES IN $it_dis
 WHERE pernr = $it_dis-pernr
         AND begda <= p_pdate
         AND endda >= p_pdate
         AND sprps = space.

  LOOP AT $it_dis.
    IF $it_dis-begda IS INITIAL.
      MESSAGE e001 WITH 'No hire date for :' $it_dis-pernr.
    ENDIF.
    READ TABLE lt_name WITH KEY pernr = $it_dis-pernr.
    $it_dis-nachn = lt_name-nachn.
    $it_dis-vorna = lt_name-vorna.
    MODIFY $it_dis.
  ENDLOOP.

  LOOP AT $it_dis.
    MOVE-CORRESPONDING $it_dis TO it_dis.
    APPEND it_dis.
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
  r_abstyp-low      = '1017'.
  APPEND r_abstyp.
  r_abstyp-low      = '3300'.                               "UD1K954831
  APPEND r_abstyp.                                          "UD1K954831

* by ig.moon 2/3/2009 {
*  r_abstyp-low      = '1018'.
*  APPEND r_abstyp.
* }

*  r_abstyp-low      = '1019'. "HIS20094
*  APPEND r_abstyp.            "HIS20094
  r_abstyp-low      = '1025'.
  APPEND r_abstyp.
  r_abstyp-low      = '1034'.
  APPEND r_abstyp.
  r_abstyp-low      = '1035'.
  APPEND r_abstyp.
  r_abstyp-low      = '1044'.
  APPEND r_abstyp.
  r_abstyp-low      = '1045'.
  APPEND r_abstyp.
  r_abstyp-low      = '1046'.
  APPEND r_abstyp.
  r_abstyp-low      = '1050'.
  APPEND r_abstyp.
  r_abstyp-low      = '1053'.
  APPEND r_abstyp.
  r_abstyp-low      = '1057'.
  APPEND r_abstyp.
*  r_abstyp-low      = '1058'. "HIS20094
*  APPEND r_abstyp.            "HIS20094
  r_abstyp-low      = '2000'.
  APPEND r_abstyp.
*  r_abstyp-low      = '1027'. "HIS20094
*  APPEND r_abstyp.            "HIS20094
  r_abstyp-low      = '1074'.  "UD1K953480
  APPEND r_abstyp.             "UD1K953480


*******************************
*Do not pay these Attendance types
** pulling from pa2002
  r_subty1-sign   = 'I'.
  r_subty1-option = 'EQ'.

*  r_subty1-low    = '1019'.   "HIS20094
*  APPEND r_subty1.            "HIS20094
  r_subty1-low    = '2000'.
  APPEND r_subty1.
  r_subty1-low    = '3303'.                                 "UD1K954831
  APPEND r_subty1.                                          "UD1K954831
  r_subty1-low    = '1046'.
  APPEND r_subty1.
*  r_subty1-low    = '1058'.   "HIS20094
*  APPEND r_subty1.            "HIS20094
** end of change
******************************
*Do not pay these action types
  r_mastyp-sign   = 'I'.
  r_mastyp-option = 'EQ'.
*  r_mastyp-low    = 'ZC'.
*  append r_mastyp.
  r_mastyp-low    = 'ZW'.
  APPEND r_mastyp.
  r_mastyp-low    = 'ZX'.
  APPEND r_mastyp.
  r_mastyp-low    = 'ZY'.
  APPEND r_mastyp.
*****************************

ENDFORM.                    " INITIAL
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET TITLEBAR 'TITLE_100'.
  PERFORM exclude_functions.

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
      PERFORM save_pay_100.
      __focus g_grid.
    WHEN 'DISE'.
      PERFORM dis_eligible_100.
      __focus g_grid.
    WHEN 'ELIG'.
      PERFORM eligible_100.
      __focus g_grid.
    WHEN 'DSEL'.
      PERFORM dis_select_all.
    WHEN 'SELA'.
      PERFORM select_all.
    WHEN 'DNLD'.
      PERFORM download_file.
    WHEN 'SWITCH'.
      IF sy-dynnr EQ '0100'.
        PERFORM switch_edit_mode.
      ENDIF.
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
FORM save_pay_100.

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
  READ TABLE it_dis WITH KEY updat = 'SUCCESS'
                             mark = true.
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
**S> 08/04/11 Paul
diagnosetext1 = 'THE DATA WILL BE SAVED TO DATABASE.;;->b03'(b02)
**E<
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
*  DESCRIBE TABLE it_dis LINES i_total.

  LOOP AT it_dis WHERE mark EQ true.
    ADD 1 TO i_total.
  ENDLOOP.

  IF i_total NE 0.
    i_processed = 0.
    LOOP AT it_dis WHERE mark EQ true.
      __cls bdcdata.
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
  LOOP AT it_dis WHERE updat = 'SUCCESS' AND mark EQ true.
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
FORM do_bdc USING p_person STRUCTURE it_dis.
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
    CLEAR it_dis-messg.
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
FORM fill_bdcdata USING p_person STRUCTURE it_dis.
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
*&      Form  DIS_ELIGIBLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dis_eligible_100.

  LOOP AT it_dis WHERE mark = 'X'.
    it_dis-eligi = 'NO'.
    MODIFY it_dis.
  ENDLOOP.

  it_dis-mark = false .
  MODIFY it_dis TRANSPORTING mark WHERE mark EQ true.

ENDFORM.                    " DIS_ELIGIBLE
*&---------------------------------------------------------------------*
*&      Form  ELIGIBLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM eligible_100.
  LOOP AT it_dis WHERE mark = 'X'.
    it_dis-eligi = 'YES'.
    MODIFY it_dis.
  ENDLOOP.

  it_dis-mark = false .
  MODIFY it_dis TRANSPORTING mark WHERE mark EQ true.

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
  LOOP AT it_dis WHERE eligi = 'YES' AND mark EQ true.

    CLEAR lt_p0015.
    READ TABLE lt_p0015 WITH KEY pernr = it_dis-pernr
                                      begda = p_pdate.
    IF sy-subrc EQ 0.
      it_dis-updat = 'EXIST'.
      it_dis-messg = 'Payment already exist for this period'.
      MODIFY it_dis.
      CONTINUE.
    ENDIF.

    CLEAR lt_p0015.
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
FORM download_file.
  DATA : filename LIKE  rlgrap-filename.
  __cls it_file.
  LOOP AT it_dis.
    CLEAR it_file.
    MOVE-CORRESPONDING it_dis TO it_file.
    APPEND it_file.
  ENDLOOP.
  PERFORM make_colname.
  PERFORM get_filename USING filename.
  PERFORM download     USING filename.

ENDFORM.                    " DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILENAME  text
*----------------------------------------------------------------------*
FORM get_filename USING    p_file  LIKE rlgrap-filename.
  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: fname     LIKE rlgrap-filename.
  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.

  tmp_mask = '*,*.*.'.
  CALL FUNCTION 'WS_FILENAME_GET'
         EXPORTING
         def_filename     = fname
         def_path         = fname
         mask             = tmp_mask
         mode             = 'O'
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
    p_file = tmp_filename.
  ENDIF.

ENDFORM.                    " GET_FILENAME
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILENAME  text
*----------------------------------------------------------------------*
FORM download USING    p_filename.
  DATA:l_text(30).

  IF p_filename IS INITIAL.
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
  MESSAGE s001 WITH l_text.
ENDFORM.                    " DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  make_colname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_colname.
* MAKE COLIMN HEADER
  __cls it_colnames.
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
  it_colnames-name = 'STATUS'.
  APPEND it_colnames.
  it_colnames-name = '/101'.
  APPEND it_colnames.
  it_colnames-name = 'AMOUNT'.
  APPEND it_colnames.
  it_colnames-name = 'ELEIGIBLE'.
  APPEND it_colnames.
  it_colnames-name = 'UPDATE'.
  APPEND it_colnames.
  it_colnames-name = 'ABCNT'.
  APPEND it_colnames.
  it_colnames-name = 'LOA.CNT'.                             "UD1K951488
  APPEND it_colnames.                                       "UD1K951488
  it_colnames-name = 'MESSAGE'.
  APPEND it_colnames.
*  it_colnames-name = 'PAYID'.
*  APPEND it_colnames.

ENDFORM.                    " make_colname
*&---------------------------------------------------------------------*
*&      Form  exclude_functions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_functions.

  PERFORM append_exclude_functions
           TABLES gt_exclude[]
           USING: cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
                  cl_gui_alv_grid=>mc_fc_loc_append_row,
                  cl_gui_alv_grid=>mc_fc_loc_cut,
                  cl_gui_alv_grid=>mc_fc_loc_insert_row,
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.

ENDFORM.                    " exclude_functions
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_100 OUTPUT.

  IF g_custom_container IS INITIAL.
    PERFORM create_and_init_alv.
*   Display alv grid
    CALL METHOD g_grid->set_table_for_first_display
         EXPORTING is_layout            = gs_layo
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
                   is_variant           = gs_variant
         CHANGING  it_outtab            = it_dis[]
                   it_fieldcatalog      = gt_fcat[]
                   it_sort              = gt_sort[].
  ELSE.
    CALL METHOD g_grid->refresh_table_display.
  ENDIF.
  __focus g_grid.
  PERFORM user_status.

ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_and_init_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_alv.

*   Create object
  PERFORM create_object.

*  Create Object to verify input values.
  CREATE OBJECT g_event_receiver.
  SET HANDLER : g_event_receiver->handle_data_changed FOR g_grid.

*   Create field category
  PERFORM create_field_category USING false.

  CALL METHOD g_grid->register_edit_event
       EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->set_ready_for_input
     EXPORTING
            i_ready_for_input = 0.

  PERFORM sort_build USING gt_sort[].

*   Setting for layout
  PERFORM set_lvc_layout.

*   Set colors
  PERFORM set_color.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

*   Define cell attribute
  PERFORM build_cell_attr.
ENDFORM.                    " create_and_init_alv
*&---------------------------------------------------------------------*
*&      Form  create_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM create_field_category USING mode_edit.
  DATA: l_pos       TYPE i.
  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fcat.
    gs_fcat-col_pos       = l_pos.
    gs_fcat-key           = &1.
    gs_fcat-fieldname     = &2.
    gs_fcat-coltext       = &3.     " Column heading
    gs_fcat-outputlen     = &4.     " Column width
    gs_fcat-datatype      = &5.     " Data type
    gs_fcat-emphasize     = &6.
    append gs_fcat to gt_fcat.
  END-OF-DEFINITION.

  __catalog :
    'X'  'PERNR'             'Pernr'             8 'CHAR' '',
    'X'  'NACHN'             'Last Name'        15 'CHAR' '',
    'X'  'VORNA'             'First Name'       15 'CHAR' '',
    ' '  'BEGDA'             'Hire Date'        10 'DATS' '',
    ' '  'PERSG'             'GR'                5 'CHAR' '',
    ' '  'STLTX'             'Job Title'        20 'CHAR' '',
    ' '  'AMUNT'             'Amount'           15 'CURR' '',
    ' '  'ELIGI'             'Elig.'
                                                 4 'CHAR' '',
    ' '  'UPDAT'             'Update'           10 'CHAR' '',
    ' '  'MESSG'             'Message'          80
'CHAR' ''.

  LOOP AT gt_fcat INTO gs_fcat.
    gs_fcat-ref_table = 'ZSHR_ATTBONUS_ALV'.
    gs_fcat-ref_field = gs_fieldcat-fieldname.
    MODIFY gt_fcat FROM gs_fcat.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING ft_sort TYPE lvc_t_sort.
  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-subtot    = &5.
    gs_sort-comp      = &6.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
      'PERNR'             ' ' 'X' '' 'X' '',
      'NACHN'             ' ' 'X' '' 'X' '',
      'VORNA'             ' ' 'X' '' 'X' ''.
ENDFORM.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  set_lvc_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_lvc_layout.
  CLEAR gs_layo.
  gs_layo-edit       = 'X'.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
  gs_layo-stylefname = 'CELLTAB'.

ENDFORM.                    " set_lvc_layout
*&---------------------------------------------------------------------*
*&      Form  set_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_color.
  CLEAR: gs_specialcol, gt_specialcol[], it_dis-tabcolor[].

  DEFINE __color.
    gs_specialcol-fieldname = &1 .
    gs_specialcol-color-col = &2 .
    gs_specialcol-color-int = &3 .
    append gs_specialcol to gt_specialcol .
  END-OF-DEFINITION.

  __color :
           'PERNR'  '1' 0,
           'NACHN'  '1' 0,
           'VORNA'  '1' 0,
           'BEGDA'  '2' 0,
           'PERSG'  '2' 0,
           'STLTX'  '2' 0,
           'AMUNT'  '2' 0,
           'ELIGI'  '2' 0,
           'UPDAT'  '2' 0,
           'MESSG'  '2' 0.

  it_dis-tabcolor[] = gt_specialcol[].
  MODIFY it_dis TRANSPORTING tabcolor WHERE tabcolor IS initial.

ENDFORM.                    " set_color
*&---------------------------------------------------------------------*
*&      Form  build_cell_attr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_cell_attr.

  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl.

  CLEAR lt_celltab.
  REFRESH lt_celltab.

  CLEAR gs_fcat.

  LOOP AT gt_fcat INTO gs_fcat.
    ls_celltab-fieldname = gs_fcat-fieldname.
    IF ls_celltab-fieldname EQ 'BEGDA'
      OR ls_celltab-fieldname EQ 'PERSG'
      OR ls_celltab-fieldname EQ 'STLTX'
      OR ls_celltab-fieldname EQ 'BET08'
      OR ls_celltab-fieldname EQ 'ELIGI'
      OR ls_celltab-fieldname EQ 'UPDAT'
      OR ls_celltab-fieldname EQ 'MESSG'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    INSERT ls_celltab INTO TABLE lt_celltab.
  ENDLOOP.

  CLEAR it_dis-celltab.
  INSERT LINES OF lt_celltab INTO TABLE it_dis-celltab.
  MODIFY it_dis TRANSPORTING celltab WHERE celltab IS initial.

ENDFORM.                    " build_cell_attr
*&---------------------------------------------------------------------*
*&      Form  user_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_status.

  __cls ftab.

  IF g_grid->is_ready_for_input( ) EQ 1.
    ftab-fcode = 'SAVE'.
    APPEND ftab.
  ENDIF.

  SET PF-STATUS '100' EXCLUDING ftab.

ENDFORM.                    " user_status
*&---------------------------------------------------------------------*
*&      Form  data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_data_changed  text
*----------------------------------------------------------------------*
FORM data_changed USING    p_er_data_changed.

ENDFORM.                    " data_changed
*&---------------------------------------------------------------------*
*&      Form  switch_edit_mode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM switch_edit_mode.
  DATA answer.
  IF g_grid->is_ready_for_input( ) EQ 0.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 1.

    PERFORM info_text_set USING true.
  ELSE.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 0.
    PERFORM info_text_set USING false.
  ENDIF.

  PERFORM build_cell_attr.

ENDFORM.                    " switch_edit_mode
*&---------------------------------------------------------------------*
*&      Form  info_text_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRUE  text
*----------------------------------------------------------------------*
FORM info_text_set USING    p_true.
  IF p_true EQ true.
    info = text-015.
  ELSE.
    info = text-015.
  ENDIF.

ENDFORM.                    " INFO_TEXT_SET
*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1894   text
*      -->P_1895   text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '100' EXCLUDING 'SWITCH'.
  SET TITLEBAR 'TITLE_100'.
  DESCRIBE TABLE it_dis LINES tc1-lines.
ENDMODULE.                 " STATUS_0200  OUTPUT
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
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
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
      PERFORM download_file.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  save_pay
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

  READ TABLE it_dis WITH KEY mark = true.
  IF sy-subrc NE 0.
    it_dis-mark = true.
    MODIFY it_dis TRANSPORTING mark WHERE mark EQ false.
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

ENDFORM.                    " save_pay
*&---------------------------------------------------------------------*
*&      Form  dis_eligible
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

ENDFORM.                    " dis_eligible
*&---------------------------------------------------------------------*
*&      Form  eligible
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

ENDFORM.                    " eligible
*&---------------------------------------------------------------------*
*&      Form  read_payroll_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DIS_PERNR  text
*----------------------------------------------------------------------*
FORM read_payroll_data USING p_pernr CHANGING p_p_amount.
  CLEAR p_p_amount.

  DATA: in_rgdir LIKE pc261 OCCURS 0 WITH HEADER LINE,
        wa_rt LIKE pc207 OCCURS 0 WITH HEADER LINE,
        seqnr LIKE pc261-seqnr,
        ws_401k TYPE   pc207-betrg,
        ws_gsal TYPE   pc207-betrg,
        ws_8387 TYPE   pc207-betrg,
        ws_8337 TYPE  pc207-betrg,
        ws_bc31 TYPE  pc207-betrg,
        l_limit  TYPE pc207-betrg,
        l_ee% TYPE pa0169-eepct,
        ws_pernr TYPE p0000-pernr,
        result TYPE pay99_result.

  DATA : ws_rgdir LIKE LINE OF in_rgdir,
         l_relid LIKE  pcl2-relid,
         l_pernr LIKE pc200-pernr,
         l_code LIKE p0041-dar01,
         l_code1 LIKE p0041-dar01,
         t_date LIKE p0041-dat01,
         hire_date LIKE  p0041-dat01,
         l_edate  LIKE pa0169-begda,
         l_edt1  LIKE pa0169-begda,
         l_edate1(10) TYPE c,
         l_edate2(10) TYPE c .

  DATA : flag(1) TYPE c,
         prev_fpper TYPE  pc261-fpper,
         lv_molga TYPE molga,
         lw_month  LIKE  t5a4a-dlydy,
         $l_edate LIKE l_edate.


  DATA: w_permo  LIKE t549a-permo,   " Period parameters
        w_abkrt  LIKE t549t-atext,   " Payroll Area Text
        w_begda  TYPE begda,
        w_endda  TYPE endda,
        w_abrjr  LIKE t549q-pabrj,
        w_abrpr  LIKE t549q-pabrp,
        $ix LIKE sy-tabix.

* Read Payroll Control Records
  CLEAR lv_molga.
  CALL FUNCTION 'CU_READ_RGDIR'
       EXPORTING
            persnr          = p_pernr
       IMPORTING
            molga           = lv_molga
       TABLES
            in_rgdir        = in_rgdir
       EXCEPTIONS
            no_record_found = 1
            OTHERS          = 2.

  ws_pernr = p_pernr.

* Delete payroll control records based on selection input
  IF NOT p_bperd[] IS INITIAL.
*    DELETE in_rgdir WHERE NOT paydt IN p_bperd.

    DELETE in_rgdir WHERE NOT fpbeg IN p_bperd.

*    LOOP AT in_rgdir.
*      $ix = sy-tabix.
*      w_abrpr = in_rgdir-fpper+4(2).
*      w_abrjr = in_rgdir-fpper(4).
*
*      PERFORM get_payroll_period USING p_abkrs
*                              CHANGING w_permo w_begda w_endda
*                                       w_abkrt w_abrpr w_abrjr.
*
*      IF w_begda IN p_bperd AND w_endda IN p_bperd.
*      ELSE.
*        DELETE in_rgdir INDEX $ix.
*      ENDIF.
*
*    ENDLOOP.


  ENDIF.


** Delete payroll control records based on selection input
*  IF  NOT  s_fpper[]  IS INITIAL.
*    DELETE in_rgdir WHERE NOT fpper IN s_fpper. "Payroll Period
*  ENDIF.
* Delete payroll control records where payroll period is 000000
*  DELETE in_rgdir WHERE fpper EQ '000000'.

** Delete voided payroll data.
  DELETE in_rgdir WHERE voidr NE space.

  DELETE in_rgdir WHERE srtza NE 'A'. "Active

* Cluster id for US
* Personnel Country Grouping
  CLEAR l_relid.

  SELECT SINGLE relid INTO l_relid
                FROM t500l
                WHERE molga = lv_molga.
  IF   l_relid IS INITIAL.
    l_relid = 'RU'.
  ENDIF.

  LOOP AT in_rgdir INTO ws_rgdir.
    seqnr = ws_rgdir-seqnr.
    l_pernr = ws_pernr.

* Read Payroll cluster Data for each payroll control record
    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
         EXPORTING
              clusterid                    = l_relid
              employeenumber               = l_pernr
              sequencenumber               = seqnr
              read_only_international      = 'X'
         CHANGING
              payroll_result               = result
         EXCEPTIONS
              illegal_isocode_or_clusterid = 1
              error_generating_import      = 2
              import_mismatch_error        = 3
              subpool_dir_full             = 4
              no_read_authority            = 5
              no_record_found              = 6
              versions_do_not_match        = 7
              error_reading_archive        = 8
              error_reading_relid          = 9
              OTHERS                       = 10.

    CLEAR : ws_8387, ws_401k,ws_8337, ws_bc31, ws_gsal .

    LOOP AT result-inter-rt INTO wa_rt
    WHERE ( lgart = '/101' ).
      p_p_amount = p_p_amount + wa_rt-betrg.
    ENDLOOP.


  ENDLOOP.

ENDFORM.                    " read_payroll_data
*&---------------------------------------------------------------------*
*&      Form  get_emp_categ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_P0001_PERSG  text
*      -->P_LT_P0001_PERSK  text
*      <--P_LT_P0001_CATEG  text
*----------------------------------------------------------------------*
FORM get_emp_categ USING    f_persg
                            f_persk
                   CHANGING f_categ.

  CONSTANTS:
   c_eg1(1)   TYPE c VALUE   'A',"US-Salary
   c_eg2(1)   TYPE c VALUE   'B',"US-Wage
   c_eg3(1)   TYPE c VALUE   'K'."KR-Salary

  IF f_persg = '9' AND f_persk = 'U2'.
    f_categ = c_eg3.
  ELSEIF ( ( f_persg = '1' AND f_persk = 'U2' ) OR
           ( f_persg = '1' AND f_persk = 'U3' ) ).
    f_categ = c_eg1.
  ELSE.
    f_categ = c_eg2.
  ENDIF.

ENDFORM.                    " get_emp_categ
*&---------------------------------------------------------------------*
*&      Form  get_payroll_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ABKRS  text
*      <--P_W_PERMO  text
*      <--P_W_BEGDA  text
*      <--P_W_ENDDA  text
*      <--P_W_ABKRT  text
*      <--P_P_ABRPR  text
*      <--P_W_ABRJR  text
*----------------------------------------------------------------------*
FORM get_payroll_period USING v_abkrs
                     CHANGING v_permo v_begda v_endda
                              v_abkrt v_pabrp v_pabrj.

  CALL FUNCTION 'PA03_PERIODDATES_GET'
       EXPORTING
            f_abkrs               = v_abkrs
       IMPORTING
            f_permo               = v_permo
            f_current_begda       = v_begda
            f_current_endda       = v_endda
            f_abkrs_text          = v_abkrt
       CHANGING
            f_current_period      = v_pabrp
            f_current_year        = v_pabrj
       EXCEPTIONS
            pcr_does_not_exist    = 1
            abkrs_does_not_exist  = 2
            period_does_not_exist = 3
            OTHERS                = 4.
  IF sy-subrc <> 0.
*      message e003 with v_pabrp v_pabrj.
  ENDIF.


ENDFORM.                    " get_payroll_period
