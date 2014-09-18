*&----------------------------------------------------------------------
*& Development ID : ZAHRU01D
*& Program ID     : ZAHRU01D
*& Program Name   : [HR] Global HR ID Integration
*& Created by     : Valerian Utama
*& Created on     : 05/17/2010
*& Reference Pgm  : N/A
*&
*& Modification Log
*& Date        Developer Issue No  Description
*&======================================================================
*& 05/17/10    Valerian UD1K949027 Initial Program Development
*              HIS20094
*
*&----------------------------------------------------------------------

REPORT  zhrr05000t.

TYPE-POOLS: slis.

TABLES: pa0006.

*- File Catalog
DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
       gs_fieldcat TYPE slis_fieldcat_alv.

*- Output Structure
DATA: BEGIN OF it_out OCCURS 0,
        empno(20)              TYPE c,      "Employee Number
        corp_code(5)           TYPE c,      "Corporation Code
        company_code(10)       TYPE c,      "Company Code
        first_name(50)         TYPE c,      "First Name
        last_name(50)          TYPE c,      "Last Name
        local_name(100)        TYPE c,      "Full Name
        english_name(100)      TYPE c,      "English Name
        birth_day(18)          TYPE c,      "Birth Day
        officephone(50)        TYPE c,      "Phone Number
        mobile(50)             TYPE c,      "Mobile Number
        email(100)             TYPE c,      "E-Mail Address
        parent_dept_code(20)   TYPE c,      "Parent Dept. Code
        parent_dept_name(100)  TYPE c,      "Parent Dept. Name
        dept_code(20)          TYPE c,      "Department Code
        dept_name(100)         TYPE c,      "Department Name
        team_code(20)          TYPE c,      "Team Code
        team_name(100)         TYPE c,      "Team Name
        old_dept_code(20)      TYPE c,      "Old Dept. Code
        old_dept_name(100)     TYPE c,      "Old Dept. Name
        duty_code(20)          TYPE c,      "Duty Code
        duty_name(100)         TYPE c,      "Duty Name
        position_code(20)      TYPE c,      "Position Code
        position_name(100)     TYPE c,      "Position Name
        location_code(20)      TYPE c,      "Work Area Code
        location_name(100)     TYPE c,      "Work Area Name
        rcnt_offc_ord_date(50) TYPE c,      "Office Order Date
        language_code(10)      TYPE c,      "Language Code
        emp_type(20)           TYPE c,      "Employee Type
        hiredate(50)           TYPE c,      "Hire Date
        retire_date(50)        TYPE c,      "Retirement Data
        retire_status(20)      TYPE c,      "Retirement Status
        createdate(8)          TYPE c,      "Create Date
        modifydate(8)          TYPE c,      "Update Date
      END OF it_out.

DATA : BEGIN OF it_data OCCURS 0,
         pernr                 TYPE pa0000-pernr,
         corp_code(5)          TYPE c,
         company_code(10)      TYPE c,
         vorna                 TYPE pa0002-vorna,
         nachn                 TYPE pa0002-nachn,
         ename                 TYPE pa0001-ename,
         ename1                TYPE pa0001-ename,
         birth_day(18)         TYPE c,
         num02                 TYPE pa0006-num02,
         mobile(50)            TYPE c,
         usrid1                TYPE pa0105-usrid,
         parent_dept_code(20)  TYPE c,
         parent_dept_name(100) TYPE c,
         dept_code(20)         TYPE c,
         dept_name(100)        TYPE c,
         team_code(20)         TYPE c,
         team_name(100)        TYPE c,
         old_dept_code(20)     TYPE c,
         old_dept_name(100)    TYPE c,
         stell                 TYPE pa0001-stell,
         stext                 TYPE hrp1000-stext,
         plans                 TYPE pa0001-plans,
         stext1                TYPE hrp1000-stext,
         werks                 TYPE pa0001-werks,
         name1                 TYPE t500p-name1,
         rcnt                  TYPE pa0000-begda,
         language_code(10)     TYPE c,
         emp_type(20)          TYPE c,
         hiredate              TYPE sy-datum,
         retire_date           TYPE sy-datum,
         retire_status(20)     TYPE c,
         createdate            TYPE sy-datum,
         modifydate            TYPE pa0001-aedtm,
         persg                 TYPE pa0001-persg,
         stat2                 TYPE pa0000-stat2,
         massn                 TYPE pa0000-massn,
         begda                 TYPE pa0000-begda,
         orgeh                 TYPE pa0001-orgeh,
       END OF it_data.

DATA: BEGIN OF it_change OCCURS 0,
        aedtm TYPE pa0000-aedtm,
      END OF it_change.

DATA: v_sdate TYPE sy-datum.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_pernr FOR it_data-pernr.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X' USER-COMMAND chck,
            p_actv AS CHECKBOX,
            p_kdate TYPE datum DEFAULT sy-datum OBLIGATORY.

SELECT-OPTIONS: s_cdate FOR it_data-modifydate
        DEFAULT v_sdate TO sy-datum OPTION BT SIGN I.

SELECTION-SCREEN ULINE MODIF ID chk.
PARAMETERS: p_rfcdes TYPE rfcdest
                     MATCHCODE OBJECT vers_rfcdest_sh
                     MODIF ID chk.
SELECTION-SCREEN END OF BLOCK blk1.

INITIALIZATION.
  v_sdate = sy-datum - 6.
  READ TABLE s_cdate INDEX 1.
  IF sy-subrc EQ 0.
    s_cdate-low = v_sdate.
    MODIFY s_cdate INDEX sy-tabix.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'CHK'.
      IF NOT p_test IS INITIAL.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  CHECK sy-ucomm NE 'CHCK'.
  IF p_test IS INITIAL AND
     p_rfcdes IS INITIAL.
    MESSAGE e208(00) WITH 'Please Enter RFC Destination'.
  ENDIF.

START-OF-SELECTION.
  PERFORM select_data.
  PERFORM modify_data.
  CHECK NOT it_data[] IS INITIAL.

  IF p_test IS INITIAL.
    PERFORM transfer_data.
  ELSE.
    PERFORM display_data.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_grid_display TABLES ft_outtab.

  DATA : gs_layout TYPE slis_layout_alv,
         l_repid TYPE sy-repid.

  l_repid = sy-repid.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name         = l_repid
            i_structure_name       = 'ZHR_GLOBAL_HR_ID'
       CHANGING
            ct_fieldcat            = gt_fieldcat
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Set key field
  READ TABLE gt_fieldcat INTO gs_fieldcat
                         WITH KEY fieldname = 'EMPNO'.
  IF sy-subrc = 0.
    gs_fieldcat-key = 'X'.
    MODIFY gt_fieldcat FROM gs_fieldcat INDEX sy-tabix
                       TRANSPORTING key.
  ENDIF.

  PERFORM rename USING:
        'EMPNO' 'Employee Number',
        'CORP_CODE' 'Corporation Code',
        'COMPANY_CODE' 'Company Code',
        'FIRST_NAME' 'First Name',
        'LAST_NAME' 'Last Name',
        'LOCAL_NAME' 'Full Name',
        'ENGLISH_NAME' 'English Name',
        'BIRTH_DAY' 'Birth Day',
        'OFFICEPHONE' 'Phone Number',
        'MOBILE' 'Mobile Number',
        'EMAIL' 'E-Mail Address',
        'PARENT_DEPT_CODE' 'Parent Dept. Code',
        'PARENT_DEPT_NAME' 'Parent Dept. Name',
        'DEPT_CODE' 'Department Code',
        'DEPT_NAME' 'Department Name',
        'TEAM_CODE' 'Team Code',
        'TEAM_NAME' 'Team Name',
        'OLD_DEPT_CODE' 'Old Dept. Code',
        'OLD_DEPT_NAME' 'Old Dept. Name',
        'DUTY_CODE' 'Duty Code',
        'DUTY_NAME' 'Duty Name',
        'POSITION_CODE' 'Position Code',
        'POSITION_NAME' 'Position Name',
        'LOCATION_CODE' 'Work Area Code',
        'LOCATION_NAME' 'Work Area Name',
        'RCNT_OFFC_ORD_DATE' 'Office Order Date',
        'LANGUAGE_CODE' 'Language Code',
        'EMP_TYPE' 'Employee Type',
        'HIREDATE' 'Hire Date',
        'RETIRE_DATE' 'Retirement Date',
        'RETIRE_STATUS' 'Retirement Status',
        'CREATEDATE' 'Create Date',
        'MODIFYDATE' 'Update Date'.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = l_repid
            is_layout          = gs_layout
            it_fieldcat        = gt_fieldcat
       TABLES
            t_outtab           = ft_outtab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  RENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FNAME  text
*      -->P_FVALUE  text
*----------------------------------------------------------------------*
FORM rename USING p_fname  TYPE c
                  p_fvalue TYPE c.

  READ TABLE gt_fieldcat INTO gs_fieldcat
                         WITH KEY fieldname = p_fname.
  IF sy-subrc = 0.
    gs_fieldcat-seltext_l = p_fvalue.
    MODIFY gt_fieldcat FROM gs_fieldcat INDEX sy-tabix
               TRANSPORTING seltext_l.
  ENDIF.

ENDFORM.                    " RENAME
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data .
  SELECT  a~ename a~ename AS ename1 a~stell a~plans a~werks a~orgeh
          b~vorna b~nachn c~pernr a~persg c~stat2 c~massn c~begda

  INTO CORRESPONDING FIELDS OF TABLE it_data
  FROM pa0001 AS a
      INNER JOIN pa0002 AS b
          ON a~pernr  =  b~pernr
      INNER JOIN pa0000 AS c
          ON a~pernr  =  c~pernr

  WHERE  a~endda GE p_kdate
    AND  a~begda LE p_kdate
    AND  b~endda GE p_kdate
    AND  b~begda LE p_kdate
    AND  c~endda GE p_kdate
    AND  c~begda LE p_kdate
    AND  a~pernr IN s_pernr.

* Check if only active employee has been selected
  IF NOT p_actv IS INITIAL.
    DELETE it_data WHERE stat2 = '0'.
  ENDIF.

  SORT it_data BY pernr.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_data .
  DATA: l_rec TYPE i,
        l_com TYPE pa0006-com01,
        l_num TYPE pa0006-num01.


  DESCRIBE TABLE it_data LINES l_rec.

  LOOP AT it_data.
* Show progress indicator.
    PERFORM show_progress USING 'Processing Data...' l_rec.

* Get Corportation Code
    it_data-corp_code = 'A1'.

* Get Company Code
    it_data-company_code = 'A1'.

* Remove Name Title.
    SHIFT it_data-ename LEFT DELETING LEADING space.

    SEARCH it_data-ename FOR 'MRS' STARTING AT 1 ENDING AT 3.
    IF sy-subrc EQ 0.
      it_data-ename+sy-fdpos(3) = space.
      SHIFT it_data-ename LEFT DELETING LEADING space.
    ELSE.
      SEARCH it_data-ename FOR 'MR' STARTING AT 1 ENDING AT 2.
      IF sy-subrc EQ 0.
        it_data-ename+sy-fdpos(2) = space.
        SHIFT it_data-ename LEFT DELETING LEADING space.
      ELSE.
        SEARCH it_data-ename FOR 'MS' STARTING AT 1 ENDING AT 2.
        IF sy-subrc EQ 0.
          it_data-ename+sy-fdpos(2) = space.
          SHIFT it_data-ename LEFT DELETING LEADING space.
        ELSE.
          SEARCH it_data-ename FOR 'MISS' STARTING AT 1 ENDING AT 4.
          IF sy-subrc EQ 0.
            it_data-ename+sy-fdpos(4) = space.
            SHIFT it_data-ename LEFT DELETING LEADING space.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    it_data-ename1 = it_data-ename.

* Get Telephone Number
    SELECT *
      FROM pa0006
     UP TO 1 ROWS
     WHERE pernr = it_data-pernr
       AND subty = '1'
       AND endda GE p_kdate
       AND begda LE p_kdate.
    ENDSELECT.

    CLEAR: l_com, l_num.
    DO 6 TIMES VARYING l_com FROM pa0006-com01
                             NEXT pa0006-com02
               VARYING l_num FROM pa0006-num01
                             NEXT pa0006-num02.
      IF l_com = 'WORK'.
        it_data-num02 = l_num.
        EXIT.
      ENDIF.
    ENDDO.

* Get E-Mail Address
    SELECT usrid_long INTO it_data-usrid1
      FROM pa0105
     UP TO 1 ROWS
     WHERE pernr = it_data-pernr
       AND endda GE p_kdate
       AND begda LE p_kdate
       AND usrty = '0010'.
    ENDSELECT.

* Get Departement Info
    PERFORM get_dept_info.

* Get Duty Name
    SELECT stext INTO it_data-stext
      FROM hrp1000
     UP TO 1 ROWS
     WHERE plvar = '01'
       AND otype = 'C'
       AND objid = it_data-stell
       AND begda LE p_kdate
       AND endda GE p_kdate.
    ENDSELECT.

* Get Position Name
    SELECT stext INTO it_data-stext1
      FROM hrp1000
     UP TO 1 ROWS
     WHERE plvar = '01'
       AND otype = 'S'
       AND objid = it_data-plans
       AND begda LE p_kdate
       AND endda GE p_kdate.
    ENDSELECT.

* Get Work Area Name
    SELECT SINGLE name1 INTO it_data-name1
      FROM t500p
     WHERE persa = it_data-werks.

* Get Office Order Date
    SELECT begda INTO it_data-rcnt
      FROM pa0000
      UP TO 1 ROWS
     WHERE pernr = it_data-pernr
       AND endda = '99991231'
       AND ( massn = 'Z2'
        OR   massn = 'Z3' ).
    ENDSELECT.

* Get Language Code
    it_data-language_code = 'EN'.

* Get Employee Type
    IF it_data-persg = '9'.
      it_data-emp_type = '2'.
    ELSE.
      it_data-emp_type = '1'.
    ENDIF.

* Get Hire Date
    SELECT MIN( DISTINCT begda ) INTO it_data-hiredate
    FROM pa0000
   WHERE pernr = it_data-pernr
     AND ( massn = 'Z0'
      OR   massn = 'Z6'
      OR   massn = 'Z9' ).

* Get Termination Date
    IF it_data-stat2 = '0' AND
      ( it_data-massn = 'Z5' OR it_data-massn = 'ZE' OR
        it_data-massn = 'ZW' OR it_data-massn = 'ZX' OR
        it_data-massn = 'ZY' ).
      it_data-retire_date  =  it_data-begda.
    ENDIF.

* Get Retirement Status
    IF it_data-stat2 = '0'.
      it_data-retire_status = 'Y'.
    ELSE.
      it_data-retire_status = 'N'.
    ENDIF.

* Get Create Date
    SELECT MIN( DISTINCT begda ) INTO it_data-createdate
    FROM pa0000
   WHERE pernr = it_data-pernr
     AND ( massn = 'Z0'
      OR   massn = 'Z6'
      OR   massn = 'Z9' ).

* Get Update Date
    SELECT MAX( DISTINCT aedtm ) INTO TABLE it_change
    FROM pa0000
   WHERE pernr = it_data-pernr
     AND endda GE p_kdate
     AND begda LE p_kdate
     AND ( massn = 'Z0'
      OR   massn = 'Z6'
      OR   massn = 'Z9'
      OR   massn = 'Z3'
      OR   massn = 'Z4'
      OR   massn = 'Z5'
      OR   massn = 'ZE'
      OR   massn = 'ZW'
      OR   massn = 'ZX'
      OR   massn = 'ZY' ).

    SELECT MAX( DISTINCT aedtm ) APPENDING TABLE it_change
    FROM pa0001
   WHERE pernr = it_data-pernr
     AND endda GE p_kdate
     AND begda LE p_kdate.

    SELECT MAX( DISTINCT aedtm ) APPENDING TABLE it_change
    FROM pa0002
   WHERE pernr = it_data-pernr
     AND endda GE p_kdate
     AND begda LE p_kdate.

    SELECT MAX( DISTINCT aedtm ) APPENDING TABLE it_change
    FROM pa0006
   WHERE pernr = it_data-pernr
     AND subty = '1'
     AND endda GE p_kdate
     AND begda LE p_kdate.

    SELECT MAX( DISTINCT aedtm ) APPENDING TABLE it_change
    FROM pa0105
   WHERE pernr = it_data-pernr
     AND endda GE p_kdate
     AND begda LE p_kdate
     AND usrty = '0010'.

    SELECT MAX( DISTINCT begda ) APPENDING TABLE it_change
    FROM pa0000
   WHERE pernr = it_data-pernr
     AND endda GE p_kdate
     AND begda LE p_kdate
     AND ( massn = 'Z0'
      OR   massn = 'Z6'
      OR   massn = 'Z9'
      OR   massn = 'Z3'
      OR   massn = 'Z4'
      OR   massn = 'Z5'
      OR   massn = 'ZE'
      OR   massn = 'ZW'
      OR   massn = 'ZX'
      OR   massn = 'ZY' ).

    SELECT MAX( DISTINCT begda ) APPENDING TABLE it_change
    FROM pa0001
   WHERE pernr = it_data-pernr
     AND endda GE p_kdate
     AND begda LE p_kdate.

    SELECT MAX( DISTINCT begda ) APPENDING TABLE it_change
    FROM pa0002
   WHERE pernr = it_data-pernr
     AND endda GE p_kdate
     AND begda LE p_kdate.

    SELECT MAX( DISTINCT begda ) APPENDING TABLE it_change
    FROM pa0006
   WHERE pernr = it_data-pernr
     AND subty = '1'
     AND endda GE p_kdate
     AND begda LE p_kdate.

    SELECT MAX( DISTINCT begda ) APPENDING TABLE it_change
    FROM pa0105
   WHERE pernr = it_data-pernr
     AND endda GE p_kdate
     AND begda LE p_kdate
     AND usrty = '0010'.

    SORT it_change BY aedtm DESCENDING.
    READ TABLE it_change INDEX 1.
    IF sy-subrc EQ 0.
      it_data-modifydate = it_change-aedtm.
    ENDIF.

    MODIFY it_data.
  ENDLOOP.

* Get only data which is changed on selection criteria
  DELETE it_data WHERE NOT modifydate IN s_cdate.

* Move data to output structure
  SET LOCALE LANGUAGE sy-langu.

  LOOP AT it_data.
    it_out-empno = it_data-pernr.
    it_out-corp_code = it_data-corp_code.
    it_out-company_code = it_data-company_code.
    it_out-first_name = it_data-vorna.
    it_out-last_name = it_data-nachn.
    it_out-local_name = it_data-ename.
    it_out-english_name = it_data-ename1.
    it_out-birth_day = it_data-birth_day.
    it_out-officephone = it_data-num02.
    it_out-mobile = it_data-mobile.
    it_out-email = it_data-usrid1.
    it_out-parent_dept_code = it_data-parent_dept_code.
    it_out-parent_dept_name = it_data-parent_dept_name.
    it_out-dept_code = it_data-dept_code.
    it_out-dept_name = it_data-dept_name.
    it_out-team_code = it_data-team_code.
    it_out-team_name = it_data-team_name.
    it_out-old_dept_code = it_data-old_dept_code.
    it_out-old_dept_name = it_data-old_dept_name.

    IF NOT it_data-stell IS INITIAL.
      it_out-duty_code     = it_data-stell.
      it_out-duty_name     = it_data-stext.
    ENDIF.

    it_out-position_code = it_data-plans.
    it_out-position_name = it_data-stext1.
    it_out-location_code = it_data-werks.
    it_out-location_name = it_data-name1.
    it_out-language_code = it_data-language_code.
    it_out-emp_type = it_data-emp_type.

* Remove Employee Number leading zeros.
    SHIFT it_out-empno LEFT DELETING LEADING '0'.

    IF NOT it_data-rcnt IS INITIAL.
      it_out-rcnt_offc_ord_date = it_data-rcnt.
    ENDIF.

    IF NOT it_data-hiredate IS INITIAL.
      it_out-hiredate = it_data-hiredate.
    ENDIF.

    IF NOT it_data-retire_date IS INITIAL.
      it_out-retire_date = it_data-retire_date.
    ENDIF.

    it_out-retire_status = it_data-retire_status.

    IF NOT it_data-createdate IS INITIAL.
      it_out-createdate = it_data-createdate.
    ENDIF.

    IF NOT it_data-modifydate IS INITIAL.
      it_out-modifydate = it_data-modifydate.
    ENDIF.

    TRANSLATE it_out TO UPPER CASE.

    APPEND it_out. CLEAR it_out.
  ENDLOOP.

  DESCRIBE TABLE it_data LINES l_rec.
  MESSAGE s368(00) WITH 'Total record(s) extracted:' l_rec.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  PERFORM alv_grid_display  TABLES it_out.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transfer_data.

  CALL FUNCTION 'Z_HR_GLOBAL_ID_INTEGRATION'
    DESTINATION p_rfcdes
    TABLES
      it_data                        = it_out
    EXCEPTIONS
      call_function_destination_no_t = 1
      call_function_no_dest          = 2
      call_function_remote_error     = 3
      rfc_no_authority               = 4
      OTHERS                         = 5.
  IF sy-subrc <> 0.
    MESSAGE e368(00) WITH 'Error when calling RFC destination:'
                           p_rfcdes.
  ELSE.
    MESSAGE s368(00) WITH 'Data has been sent to:' p_rfcdes.
  ENDIF.

ENDFORM.                    " TRANSFER_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DEPT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dept_info.
  DATA: it_objec TYPE objec OCCURS 0 WITH HEADER LINE,
        it_struc TYPE struc OCCURS 0 WITH HEADER LINE,
        l_level  TYPE struc-level.

  DATA: BEGIN OF it_dept OCCURS 0,
          pernr TYPE pa0001-pernr,
          endda TYPE pa0001-endda,
          begda TYPE pa0001-begda,
          orgeh TYPE pa0001-orgeh,
        END OF it_dept.

  CHECK NOT it_data-orgeh IS INITIAL.

  CALL FUNCTION 'RH_STRUC_GET'
    EXPORTING
      act_otype              = 'O'
      act_objid              = it_data-orgeh
      act_wegid              = 'O-O'
*     ACT_INT_FLAG           =
*     act_plvar              = '01'
      act_begda              = p_kdate
      act_endda              = p_kdate
*     ACT_TDEPTH             = 0
*     ACT_TFLAG              = 'X'
*     ACT_VFLAG              = 'X'
*     AUTHORITY_CHECK        = 'X'
*     TEXT_BUFFER_FILL       =
*     BUFFER_MODE            =
*   IMPORTING
*     ACT_PLVAR              =
    TABLES
*     RESULT_TAB             =
      result_objec           = it_objec
      result_struc           = it_struc
    EXCEPTIONS
      no_plvar_found         = 1
      no_entry_found         = 2
      OTHERS                 = 3
            .
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Get Team code or Department code
  CLEAR l_level.
  LOOP AT it_struc WHERE vpriox NE space.
    IF it_struc-vpriox GE '40'.
      it_data-team_code = it_data-orgeh.
      READ TABLE it_objec WITH KEY objid = it_data-orgeh.
      IF sy-subrc EQ 0.
        it_data-team_name = it_objec-stext.
      ENDIF.

    ELSEIF it_struc-vpriox LT '40'
        OR it_struc-vpriox GE 'A '.
      it_data-dept_code = it_data-orgeh.
      l_level = it_struc-level - 1.
      READ TABLE it_objec WITH KEY objid = it_data-orgeh.
      IF sy-subrc EQ 0.
        it_data-dept_name = it_objec-stext.
      ENDIF.
    ENDIF.
    EXIT.
  ENDLOOP.

* Get Department Code for Team Code
  IF sy-subrc EQ 0.
    IF it_data-dept_code IS INITIAL.
      LOOP AT it_struc WHERE vpriox NE space.
        CHECK it_struc-vpriox LE '40'
           OR it_struc-vpriox GE 'A '.
        it_data-dept_code = it_struc-objid.
        l_level = it_struc-level.
        READ TABLE it_objec WITH KEY objid = it_struc-objid.
        IF sy-subrc EQ 0.
          it_data-dept_name = it_objec-stext.
        ENDIF.
        EXIT.
      ENDLOOP.
    ENDIF.

* Get the highest level
  ELSE.
    it_data-dept_code = it_data-orgeh.
    l_level = 1.
    READ TABLE it_objec WITH KEY objid = it_data-orgeh.
    IF sy-subrc EQ 0.
      it_data-dept_name = it_objec-stext.
    ENDIF.
  ENDIF.

* Get Parent Department Code
  l_level = l_level + 1.
  READ TABLE it_struc WITH KEY level = l_level.
  IF sy-subrc EQ 0.
    it_data-parent_dept_code = it_struc-objid.
    READ TABLE it_objec WITH KEY objid = it_struc-objid.
    IF sy-subrc EQ 0.
      it_data-parent_dept_name = it_objec-stext.
    ENDIF.
  ENDIF.

* Get Old Department Code.
  SELECT pernr endda begda orgeh INTO TABLE it_dept
    FROM pa0001
   WHERE pernr = it_data-pernr.

  SORT it_dept BY pernr ASCENDING endda DESCENDING.
  LOOP AT it_dept WHERE endda LT p_kdate.
    EXIT.
  ENDLOOP.

  CHECK sy-subrc EQ 0.

  CHECK NOT it_dept-orgeh IS INITIAL.

  REFRESH: it_objec, it_struc.

  IF NOT it_dept-endda IS INITIAL.
    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype              = 'O'
        act_objid              = it_dept-orgeh
        act_wegid              = 'O-O'
*       ACT_INT_FLAG           =
*       act_plvar              = '01'
        act_begda              = it_dept-endda
        act_endda              = it_dept-endda
*       ACT_TDEPTH             = 0
*       ACT_TFLAG              = 'X'
*       ACT_VFLAG              = 'X'
*       AUTHORITY_CHECK        = 'X'
*       TEXT_BUFFER_FILL       =
*       BUFFER_MODE            =
*    IMPORTING
*       ACT_PLVAR              =
     TABLES
*       RESULT_TAB             =
        result_objec           = it_objec
        result_struc           = it_struc
     EXCEPTIONS
        no_plvar_found         = 1
        no_entry_found         = 2
        OTHERS                 = 3.

* Get Old Department code
    LOOP AT it_struc WHERE vpriox NE space.
      IF it_struc-vpriox LT '40'
      OR it_struc-vpriox GE 'A '.
        it_data-old_dept_code = it_dept-orgeh.
        READ TABLE it_objec WITH KEY objid = it_dept-orgeh.
        IF sy-subrc EQ 0.
          it_data-old_dept_name = it_objec-stext.
        ENDIF.
      ENDIF.
      EXIT.
    ENDLOOP.

* Get Old Department Code for Team Code
    IF sy-subrc EQ 0.
      IF it_data-old_dept_code IS INITIAL.
        LOOP AT it_struc WHERE vpriox NE space.
          CHECK it_struc-vpriox LE '40'
             OR it_struc-vpriox GE 'A '.
          it_data-old_dept_code = it_struc-objid.
          READ TABLE it_objec WITH KEY objid = it_struc-objid.
          IF sy-subrc EQ 0.
            it_data-old_dept_name = it_objec-stext.
          ENDIF.
          EXIT.
        ENDLOOP.
      ENDIF.

* Get the highest level
    ELSE.
      it_data-old_dept_code = it_dept-orgeh.
      READ TABLE it_objec WITH KEY objid = it_dept-orgeh.
      IF sy-subrc EQ 0.
        it_data-old_dept_name = it_objec-stext.
      ENDIF.
    ENDIF.

* Clear the old dept code if equal with current one
    IF it_data-dept_code = it_data-old_dept_code.
      CLEAR: it_data-old_dept_code,
             it_data-old_dept_name.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_DEPT_INFO

*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_&1  text
*      -->P_&2  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            pf_val.

  STATICS: l_text(50) TYPE c,
           l_baseval  TYPE i,
           l_percent  TYPE i,
           l_counter  TYPE i.

  IF l_text NE pf_text.
    l_text = pf_text.
    CLEAR: l_baseval,
           l_percent,
           l_counter.
  ENDIF.

  IF NOT l_baseval IS INITIAL.
    l_counter = l_counter - 1.
    CHECK l_counter LE 0.
    l_percent = l_percent + 10.
    CHECK l_percent LE 100.
    l_counter = l_baseval.
  ELSE.
    l_baseval = pf_val DIV 10.
    l_counter = l_baseval.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = l_percent
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
