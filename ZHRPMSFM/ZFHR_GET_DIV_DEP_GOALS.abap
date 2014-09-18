FUNCTION zfhr_get_div_dep_goals.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_PERNR) TYPE  PERSNO
*"     REFERENCE(I_BEGDA) TYPE  HAP_AP_START_DATE OPTIONAL
*"     REFERENCE(I_ENDDA) TYPE  HAP_AP_END_DATE OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_GOALS) TYPE  ZTYHR_GOALS
*"     REFERENCE(E_KEYDT) TYPE  SY-DATUM
*"----------------------------------------------------------------------


  DATA: lt_tab        TYPE TABLE OF swhactor,
        lt_objec      TYPE TABLE OF objec,
        lt_struc      TYPE TABLE OF struc,
        lt_addps      TYPE TABLE OF zthr_addps,
        lt_i1002      TYPE TABLE OF p1002.

  DATA: ls_objec      LIKE LINE OF lt_objec,
        ls_struc      LIKE LINE OF lt_struc,
        ls_parent     LIKE LINE OF lt_struc,
        ls_addps      LIKE LINE OF lt_addps,
        ls_i1002      TYPE p1002,
        ls_goals      LIKE LINE OF et_goals.

  DATA: BEGIN OF ls_orgeh,
          orgeh       TYPE orgeh,
        END OF ls_orgeh,
        lt_orgeh      LIKE TABLE OF ls_orgeh.

  DATA: l_orgeh       TYPE orgeh,
        l_objid       TYPE hrobjid,
        l_stext       TYPE stext,
        l_division    TYPE string,
        l_department  TYPE string,
        l_keydt       type sy-datum.

  DATA: rt_objid      TYPE RANGE OF hrobjid WITH HEADER LINE.

  DATA: BEGIN OF ls_orglv,
          objid   TYPE hrobjid,
          sclas   TYPE sclas,
          sobid   TYPE sobid,
        END OF ls_orglv,
        lt_orglv  LIKE TABLE OF ls_orglv.


  CHECK i_pernr IS NOT INITIAL.

  CLEAR et_goals.


************************************************
*   get data
************************************************
  " get orgeh
*  CLEAR: l_orgeh, lt_orgeh.
*  SELECT SINGLE orgeh FROM pa0001
*    INTO l_orgeh
*    WHERE pernr = i_pernr
*      AND endda >= i_begda
*      AND begda <= i_begda.
  CLEAR: l_orgeh, lt_orgeh.
  CALL FUNCTION 'ZFHR_GET_JOB_POSITION'
    EXPORTING
      i_pernr = i_pernr
      i_begda = i_begda
      i_endda = i_endda
    IMPORTING
      e_orgeh = l_orgeh
      e_keydt = l_keydt.

  ls_orgeh = l_orgeh.
  APPEND ls_orgeh TO lt_orgeh.CLEAR ls_orgeh.

  " get additional position
  CLEAR lt_addps.
  SELECT * FROM zthr_addps
    INTO TABLE lt_addps
    WHERE pernr = i_pernr
      AND begda <= i_begda
      AND endda >= i_begda.

  IF lines( lt_addps ) > 0.
    LOOP AT lt_addps INTO ls_addps.
      ls_orgeh = ls_addps-orgeh.
      APPEND ls_orgeh TO lt_orgeh.CLEAR ls_orgeh.
    ENDLOOP.
  ENDIF.

  CHECK lines( lt_orgeh ) > 0.

  LOOP AT lt_orgeh INTO ls_orgeh.
    CLEAR ls_goals.
    ls_goals-orgeh = ls_orgeh.

    " get org text
    SELECT SINGLE stext FROM hrp1000
      INTO ls_goals-orgtx
      WHERE plvar = '01'
        AND otype = 'O'
        AND objid = ls_orgeh
        AND istat = '1'
        AND begda <= l_keydt
        AND endda >= l_keydt
        AND langu = sy-langu.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    " get struc org
    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype      = 'O'
        act_objid      = ls_orgeh
        act_wegid      = 'O-O'
        act_plvar      = '01'
        act_begda      = l_keydt
        act_endda      = l_keydt
      TABLES
        result_tab     = lt_tab
        result_objec   = lt_objec
        result_struc   = lt_struc
      EXCEPTIONS
        no_plvar_found = 1
        no_entry_found = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.

    " set range data
    CLEAR: rt_objid, rt_objid[].
    LOOP AT lt_objec INTO ls_objec.
      rt_objid-sign = 'I'.
      rt_objid-option = 'EQ'.
      rt_objid-low = ls_objec-objid.
      APPEND rt_objid.CLEAR: ls_objec, rt_objid.
    ENDLOOP.

    " get org level
    CLEAR lt_orglv.
    SELECT objid sclas sobid FROM hrp1001
      INTO TABLE lt_orglv
      WHERE otype = 'O'
        AND objid IN rt_objid
        AND plvar = '01'
        AND rsign = 'A'
        AND relat = 'Z03'
        AND istat = '1'
        AND begda <= l_keydt
        AND endda >= l_keydt.

    SORT lt_orglv BY objid.
    SORT lt_objec BY objid.

*** get department description
    CLEAR: ls_orglv, ls_i1002, lt_i1002.
    READ TABLE lt_orglv INTO ls_orglv
                        WITH KEY objid = ls_orgeh
                        BINARY SEARCH.
    IF '40' CO ls_orglv-sobid.
      " read object info
      CLEAR ls_objec.
      READ TABLE lt_objec INTO ls_objec
                          WITH KEY objid = ls_orgeh
                          BINARY SEARCH.
      IF sy-subrc = 0.
        " get department description
        PERFORM get_goal_description USING    ls_objec
                                              l_keydt
                                              ls_i1002.
        IF ls_i1002 IS NOT INITIAL.
          " append department
*          APPEND ls_i1002 TO lt_i1002.
          ls_goals-department = ls_i1002.
        ENDIF.
      ENDIF.
    ENDIF.

    IF ls_goals-department IS INITIAL.
      LOOP AT lt_struc INTO ls_struc.
        " read parent org
        READ TABLE lt_struc INTO ls_parent
                            WITH KEY seqnr = ls_struc-pdown.
        IF sy-subrc = 0.
          " read org level of parent org
          READ TABLE lt_orglv INTO ls_orglv
                              WITH KEY objid = ls_parent-objid
                              BINARY SEARCH.
          IF '40' CO ls_orglv-sobid.
            " read object info
            READ TABLE lt_objec INTO ls_objec
                                WITH KEY objid = ls_parent-objid
                                BINARY SEARCH.
            IF sy-subrc = 0.
              " get department description
              PERFORM get_goal_description USING ls_objec
                                                 l_keydt
                                                 ls_i1002.
              IF ls_i1002 IS NOT INITIAL.
                " append department
                ls_goals-department = ls_i1002.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR: ls_struc, ls_parent, ls_orglv, ls_objec.
      ENDLOOP.
    ENDIF.

*** get division description
    " read org level
    CLEAR: ls_orglv, ls_i1002, lt_i1002.
    READ TABLE lt_orglv INTO ls_orglv
                        WITH KEY objid = ls_orgeh
                        BINARY SEARCH.
    IF '20' CO ls_orglv-sobid.
      " read object info
      CLEAR ls_objec.
      READ TABLE lt_objec INTO ls_objec
                          WITH KEY objid = ls_orgeh
                          BINARY SEARCH.
      IF sy-subrc = 0.
        " get division description
        PERFORM get_goal_description USING ls_objec
                                           l_keydt
                                           ls_i1002.
        IF ls_i1002 IS NOT INITIAL.
          " append division
          ls_goals-division = ls_i1002.
        ENDIF.
      ENDIF.
    ENDIF.

    IF ls_goals-division IS INITIAL.
      LOOP AT lt_struc INTO ls_struc.
        " read parent org
        READ TABLE lt_struc INTO ls_parent
                            WITH KEY seqnr = ls_struc-pdown.
        IF sy-subrc = 0.
          " read org level of parent org
          READ TABLE lt_orglv INTO ls_orglv
                              WITH KEY objid = ls_parent-objid
                              BINARY SEARCH.
          IF '20' CO ls_orglv-sobid.
            " read object info
            READ TABLE lt_objec INTO ls_objec
                                WITH KEY objid = ls_parent-objid
                                BINARY SEARCH.
            IF sy-subrc = 0.
              " get division description
              PERFORM get_goal_description USING ls_objec
                                                 l_keydt
                                                 ls_i1002.
              IF ls_i1002 IS NOT INITIAL.
                " append division
                ls_goals-division = ls_i1002.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR: ls_struc, ls_parent, ls_orglv, ls_objec.
      ENDLOOP.
    ENDIF.

    IF ls_goals-division IS INITIAL.
      LOOP AT lt_struc INTO ls_struc.
        " read parent org
        READ TABLE lt_struc INTO ls_parent
                            WITH KEY seqnr = ls_struc-pdown.
        IF sy-subrc = 0.
          " read org level of parent org
          READ TABLE lt_orglv INTO ls_orglv
                              WITH KEY objid = ls_parent-objid
                              BINARY SEARCH.
          IF '25' CO ls_orglv-sobid.
            " read object info
            READ TABLE lt_objec INTO ls_objec
                                WITH KEY objid = ls_parent-objid
                                BINARY SEARCH.
            IF sy-subrc = 0.
              " get division description
              PERFORM get_goal_description USING ls_objec
                                                 l_keydt
                                                 ls_i1002.
              IF ls_i1002 IS NOT INITIAL.
                " append division
                ls_goals-division = ls_i1002.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR: ls_struc, ls_parent, ls_orglv, ls_objec.
      ENDLOOP.
    ENDIF.

    " append export
    APPEND ls_goals TO et_goals.

    CLEAR: ls_orgeh, lt_tab, lt_objec, lt_struc.
  ENDLOOP.

  " export key date
  e_keydt = l_keydt.

ENDFUNCTION.
