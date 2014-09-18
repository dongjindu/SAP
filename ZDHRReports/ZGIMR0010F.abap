*&---------------------------------------------------------------------*
*&  Include           ZGIM0010F
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_initialize .

*>> Initial Value
  "  CLEAR : it_t001, it_t001[]."

*>> Company Code & Text
  SELECT * FROM t001 INTO TABLE it_t001.

ENDFORM.                    "" SET_INITIALIZE"
*&---------------------------------------------------------------------*
*&      Form  GET_MASTER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_master_data .

  DATA : lt_0001 LIKE pa0001 OCCURS 0 WITH HEADER LINE,"
         ls_pinfo TYPE zgimt0001,"
         ls_before TYPE zgimt0001,"
         l_retdt TYPE sy-datum,"
         l_stat2 TYPE stat2.

  CONSTANTS: c_bl(6) VALUE '(B/L)' ,
             c_exempt(8) VALUE 'Exempt',
             c_non_exempt(12) VALUE 'Non Exempt',
             c_dash(1) VALUE '-'.
* Transfer data
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_0001
           FROM pa0001 AS a INNER JOIN  pa0000 AS b
             ON   a~pernr = b~pernr
          WHERE ( a~pernr IN s_pernr
            AND   a~begda <= p_datum
            AND   a~endda >= p_datum
            AND   b~begda <= p_datum
            AND   b~endda >= p_datum
            AND  (  b~stat2  = '1' OR  b~stat2 = '3') )
             OR ( a~begda <= p_datum
            AND   a~endda >= p_datum
            AND   b~aedtm  = p_datum
"            AND   b~massn  IN ('Z5', 'Z7', 'ZX', 'ZY', 'ZW')"
            AND   b~begda <= p_datum ).

  IF sy-subrc <> 0.
    MESSAGE s001(zmhr) WITH 'No Data found'.
    EXIT.
  ENDIF.
  LOOP AT lt_0001.
    CLEAR ls_pinfo.
    MOVE-CORRESPONDING lt_0001 TO ls_pinfo.

    "*>> First, Middle, Last Name, Laguage"
    PERFORM get_name USING ls_pinfo-pernr
                  CHANGING ls_pinfo-nachn ls_pinfo-midnm ls_pinfo-vorna
                           ls_pinfo-language.

    PERFORM modify_fullname CHANGING ls_pinfo-ename.

    ls_pinfo-zmnmen = ls_pinfo-ename.

*>> Communication Info.
*>> Hot line did not managed in SAP HR.
    "*>> If later on the managed in SAP HR, Please add the logic"
    PERFORM get_communication USING ls_pinfo-pernr
                  CHANGING ls_pinfo-zchline ls_pinfo-zncell.

*>> Company Name
    READ TABLE it_t001 WITH KEY bukrs = ls_pinfo-bukrs BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_pinfo-butxt = it_t001-butxt.
    ENDIF.

*>> Org.Unit Mother Tongue & English Stext
    PERFORM get_object_info USING 'O' ls_pinfo-orgeh
                         CHANGING ls_pinfo-zmorgnm ls_pinfo-zeorgnm.

*>> Position Mother Tongue & English Stext
    PERFORM get_object_info USING 'S' ls_pinfo-plans
                         CHANGING ls_pinfo-zmjikwi ls_pinfo-zejikwi.

** Furong on 08/01/14 delele B/L in the job description (
    REPLACE ALL OCCURRENCES OF SUBSTRING c_bl
            IN ls_pinfo-zmjikwi WITH ' '.
    CONDENSE ls_pinfo-zmjikwi.

    REPLACE ALL OCCURRENCES OF SUBSTRING c_bl
          IN ls_pinfo-zejikwi WITH ' '.
    CONDENSE ls_pinfo-zejikwi.

    REPLACE ALL OCCURRENCES OF SUBSTRING '(B/L'
           IN ls_pinfo-zmjikwi WITH ' '.
    CONDENSE ls_pinfo-zmjikwi.

    REPLACE ALL OCCURRENCES OF SUBSTRING '(B/L'
          IN ls_pinfo-zejikwi WITH ' '.
    CONDENSE ls_pinfo-zejikwi.

** ) end on 08/01/14

*>> Job Mother Tongue & English Stext
    PERFORM get_object_info USING 'C' ls_pinfo-stell
                         CHANGING ls_pinfo-zmjikmu ls_pinfo-zejikmu.

    REPLACE ALL OCCURRENCES OF SUBSTRING c_non_exempt
               IN ls_pinfo-zejikmu WITH ' '.
    CONDENSE ls_pinfo-zejikmu.

    REPLACE ALL OCCURRENCES OF SUBSTRING c_exempt
           IN ls_pinfo-zejikmu WITH ' '.
    CONDENSE ls_pinfo-zejikmu.

    REPLACE ALL OCCURRENCES OF SUBSTRING c_dash
           IN ls_pinfo-zejikmu WITH ' '.
    CONDENSE ls_pinfo-zejikmu.


    REPLACE ALL OCCURRENCES OF SUBSTRING c_non_exempt
                IN ls_pinfo-zmjikmu WITH ' '.
    CONDENSE ls_pinfo-zmjikmu.
    REPLACE ALL OCCURRENCES OF SUBSTRING c_exempt
              IN ls_pinfo-zmjikmu WITH ' '.
    CONDENSE ls_pinfo-zmjikmu.
    REPLACE ALL OCCURRENCES OF SUBSTRING c_dash
            IN ls_pinfo-zmjikmu WITH ' '.
    CONDENSE ls_pinfo-zmjikmu.

*>> Job Duty
** Changed on 04/16/14 - for HMMA only
**  Add logic (Job Duty = Position)
    ls_pinfo-zmjikck = ls_pinfo-zmjikwi.
*    IF lt_0001-ztrfgr >= 'G3'. ""Manager"
*      PERFORM get_job_duty USING lt_0001-bukrs lt_0001-ztrfst
*                        CHANGING ls_pinfo-zmjikck.
*    ENDIF.
*** End

*>> Check the retirement
    CLEAR : l_stat2, l_retdt."
    SELECT SINGLE begda stat2 FROM pa0000 INTO (l_retdt, l_stat2)"
                             WHERE pernr = lt_0001-pernr
                               AND begda <= p_datum
                               AND endda >= p_datum.
    IF l_stat2 = '0' OR l_stat2 = '2'.
      ls_pinfo-zfret = '*'.  ""Retirement"
      ls_pinfo-retdt = l_retdt.
    ELSE.
      "      CLEAR : ls_pinfo-zfret, ls_pinfo-retdt.  ""Active"
    ENDIF.

*<< Employee Type
    IF lt_0001-persg EQ '9'. ""Expatriate(From Korea)"
      ls_pinfo-emp_type = '1'.
    ELSE.
      ls_pinfo-emp_type = '2'.
    ENDIF.

*<< E-mail & System ID
    PERFORM get_0105 USING ls_pinfo-pernr
                  CHANGING ls_pinfo-msg_id ls_pinfo-zeemail.

*<< Check the messenger useage employee
*<< Sales subsiduaries are all use the messenger
    "*<< but, Manufacture use only employee in the office."
*<<      (Used only the regular employee --> Excluse product line employee).
** On 04/17/14 for HMMA only (
*    ls_pinfo-zclins = 'Y'.

    IF lt_0001-persk EQ 'U0'.  "hourly
      ls_pinfo-zclins = 'N'.
    ELSE.
      ls_pinfo-zclins = 'Y'.
    ENDIF.
** )

    SELECT SINGLE * FROM zgimt0001 INTO ls_before
                   WHERE pernr = lt_0001-pernr.

    IF sy-subrc EQ 0.
*>>   Compare before and now
      ls_pinfo-zcmdgn = ls_before-zcmdgn.
      ls_pinfo-aedtm = ls_before-aedtm.

      IF ls_pinfo <> ls_before.
        IF NOT ls_pinfo-retdt IS INITIAL.
          ls_pinfo-zcmdgn = 'D'.   "Retirement
        ELSE.
          IF NOT ls_before-retdt IS INITIAL. "If Re-Hire employee
            ls_pinfo-zcmdgn = 'I'.
          ELSE.
            ls_pinfo-zcmdgn = 'U'.
          ENDIF.
        ENDIF.

        ls_pinfo-aedtm = sy-datum.

        MOVE-CORRESPONDING ls_pinfo TO zgimt0001.
        MODIFY zgimt0001.
      ENDIF.
    ELSE.   ""New employee"
      IF NOT ls_pinfo-retdt IS INITIAL.
        ls_pinfo-zcmdgn = 'D'.   ""Retirement"
      ELSE.
        ls_pinfo-zcmdgn = 'I'.
      ENDIF.
      ls_pinfo-aedtm = sy-datum.

      MOVE-CORRESPONDING ls_pinfo TO zgimt0001.
      MODIFY zgimt0001.
    ENDIF.
  ENDLOOP.

  MESSAGE s001(zmhr) WITH 'Data was successfully saved'.
ENDFORM.                    "" GET_MASTER_DATA"
*&---------------------------------------------------------------------*
*&      Form  GET_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_PINFO_PERNR  text
*      <--P_LS_PINFO_NACHN  text
*      <--P_LS_PINFO_MIDNM  text
*      <--P_LS_PINFO_VORNA  text
*      <--P_LS_PINFO_ZMNMEN  text
*----------------------------------------------------------------------*
FORM get_name  USING    p_pernr
               CHANGING p_nachn
                        p_midnm
                        p_vorna
                        p_language.

  "*>> First, Middle, Last Name"
  SELECT SINGLE nachn midnm vorna sprsl FROM pa0002
           INTO (p_nachn, p_midnm, p_vorna,  p_language)"
                                WHERE pernr  = p_pernr
                                  AND begda <= p_datum
                                  AND endda >= p_datum.

*>> Language
  SELECT SINGLE laiso FROM t002 INTO p_language
                     WHERE spras = p_language.
ENDFORM.                    "" GET_NAME"
*&---------------------------------------------------------------------*
*&      Form  GET_COMMUNICATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_PINFO_PERNR  text
*      <--P_LS_PINFO_ZCHLINE  text
*      <--P_LS_PINFO_ZNCELL  text
*----------------------------------------------------------------------*
FORM get_communication  USING    p_pernr
                        CHANGING p_zchline
                                 p_zncell.

  DATA: BEGIN OF lt_mobile,"
          comky TYPE comky,"
          comnr TYPE comnr,"
        END   OF lt_mobile.

  DATA: st_p0006 TYPE pa0006.

  SELECT SINGLE * FROM pa0006 INTO st_p0006
          WHERE pernr = p_pernr
            AND subty = '1' ""Home Address"
            AND begda <= p_datum
            AND endda >= p_datum.

  IF sy-subrc = 0.
    DO 6 TIMES VARYING lt_mobile-comky FROM st_p0006-com01 NEXT st_p0006-com02
               VARYING lt_mobile-comnr FROM st_p0006-num01 NEXT st_p0006-num02.
      CASE lt_mobile-comky.
        WHEN 'WORK'. ""Office tel no."
          p_zchline = lt_mobile-comnr.
        WHEN 'WCEL'. ""Company mobile no."
          p_zncell  = lt_mobile-comnr.
      ENDCASE.
    ENDDO.
  ENDIF.
ENDFORM.                    "" GET_COMMUNICATION"
*&---------------------------------------------------------------------*
*&      Form  GET_OBJECT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0194   text
*      -->P_LS_PINFO_ORGEH  text
*      <--P_LS_PINFO_ZMORGNM  text
*      <--P_LS_PINFO_ZEORGNM  text
*----------------------------------------------------------------------*
FORM get_object_info  USING    p_otype
                               p_objid
                      CHANGING p_stext
                               p_eng_name.
  DATA : l_objid TYPE plog-objid,"
         l_stext LIKE p1000-stext.

  l_objid = p_objid.

  CLEAR l_stext.
*<< Mother Tongue Stext
  CALL FUNCTION 'RH_READ_OBJECT'
    EXPORTING
      plvar     = '01'
      otype     = p_otype
      objid     = l_objid
      istat     = '1'
      begda     = p_datum
      endda     = p_datum
      langu     = sy-langu
      read_db   = 'X'
    IMPORTING
      stext     = l_stext
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  p_stext = l_stext.

  CLEAR l_stext.
*<< English Stext
  CALL FUNCTION 'RH_READ_OBJECT'
    EXPORTING
      plvar     = '01'
      otype     = p_otype
      objid     = l_objid
      istat     = '1'
      begda     = p_datum
      endda     = p_datum
      langu     = 'E'
      read_db   = 'X'
    IMPORTING
      stext     = l_stext
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  p_eng_name = l_stext.
ENDFORM.                    "" GET_OBJECT_INFO"
*&---------------------------------------------------------------------*
*&      Form  GET_JOB_DUTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_0001_ZTRFST  text
*      <--P_LS_PINFO_ZMJIKCK  text
*----------------------------------------------------------------------*
*FORM get_job_duty  USING    p_bukrs p_ztrfst
*                   CHANGING p_zmjikck.
*
*  SELECT SINGLE zlctyt FROM zhrt0540 INTO p_zmjikck
*                      WHERE bukrs  = p_bukrs
*                        AND ztrfst = p_ztrfst
*                        AND begda <= p_datum
*                        AND endda >= p_datum.
*ENDFORM.                    "" GET_JOB_DUTY"
*&---------------------------------------------------------------------*
*&      Form  GET_0105
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_PINFO_PERNR  text
*      <--P_LS_PINFO_MSG_ID  text
*      <--P_LS_PINFO_ZEEMAIL  text
*----------------------------------------------------------------------*
FORM get_0105  USING    p_pernr
               CHANGING p_sys_id
                        p_mail.

*<< Messenger Login ID (=System ID)
  SELECT SINGLE usrid FROM pa0105 INTO p_sys_id
                     WHERE pernr = p_pernr
                       AND subty = '0001'
                       AND begda <= p_datum
                       AND endda >= p_datum.
*<< e-mail
  SELECT SINGLE usrid_long FROM pa0105 INTO p_mail
                          WHERE pernr = p_pernr
                            AND subty = '0010'
                            AND begda <= p_datum
                            AND endda >= p_datum.

ENDFORM.                    "" GET_0105"
*&---------------------------------------------------------------------*
*&      Form  MODIFY_FULLNAME
*&---------------------------------------------------------------------*
FORM modify_fullname  CHANGING p_ename.

  REPLACE FIRST OCCURRENCE OF 'Mrs' IN p_ename WITH ''.
  IF sy-subrc <> 0.
    REPLACE FIRST OCCURRENCE OF 'Ms' IN p_ename WITH ''.
    IF sy-subrc <> 0.
      REPLACE FIRST OCCURRENCE OF 'Miss' IN p_ename WITH ''.
      IF sy-subrc <> 0.
        REPLACE FIRST OCCURRENCE OF 'Mr' IN p_ename WITH ''.
      ENDIF.
    ENDIF.
  ENDIF.
  CONDENSE p_ename.

ENDFORM.                    " MODIFY_FULLNAME
