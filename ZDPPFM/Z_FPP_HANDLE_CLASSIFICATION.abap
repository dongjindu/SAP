FUNCTION Z_FPP_HANDLE_CLASSIFICATION.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(MATNR) LIKE  MARA-MATNR
*"     REFERENCE(MTYPE) TYPE  ZMTYPE
*"  EXPORTING
*"     VALUE(RETURN_FLAG) TYPE  ZRESULT
*"  TABLES
*"      VAL_TABLE STRUCTURE  ZSPP_VIN_VALUE OPTIONAL
*"----------------------------------------------------------------------

  DATA: L_NO(3)             TYPE N         ,
        L_FNAME(30)         TYPE C         ,
        L_CHECK             TYPE C         ,
        L_CONF_OUT          LIKE TABLE OF CONF_OUT     WITH HEADER LINE,
        L_VTABLE            LIKE TABLE OF VAL_TABLE    WITH HEADER LINE,
        L_INSTANCE          LIKE INOB-CUOBJ.
  DATA: L_VIN_SPEC(11),
        L_MI(10),
        L_OCN(10).
  DATA: L_SUBRC LIKE SY-SUBRC.

** Changed by Furong on 06/19/09

*  SELECT SINGLE cuobf INTO l_instance
*    FROM mara
*   WHERE matnr = matnr .
*
*  CALL FUNCTION 'VC_I_GET_CONFIGURATION_IBASE'
*       EXPORTING
*            instance           = l_instance
*       TABLES
*            configuration      = l_conf_out
*       EXCEPTIONS
*            instance_not_found = 1.
*
*  IF sy-subrc = 1.
*    return_flag = 'L' .
*    EXIT   .
*  ENDIF.

  DO 10 TIMES.
    SELECT SINGLE CUOBF INTO L_INSTANCE
       FROM MARA
      WHERE MATNR = MATNR .

    CALL FUNCTION 'VC_I_GET_CONFIGURATION_IBASE'
         EXPORTING
              INSTANCE           = L_INSTANCE
         TABLES
              CONFIGURATION      = L_CONF_OUT
         EXCEPTIONS
              INSTANCE_NOT_FOUND = 1.

    L_SUBRC = SY-SUBRC.
    IF SY-SUBRC = 0.
      CLEAR: RETURN_FLAG.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
      CLEAR: L_CONF_OUT, L_CONF_OUT[].
    ENDIF.

  ENDDO.
  IF L_SUBRC = 1.
    RETURN_FLAG = 'L' .
    EXIT   .
  ENDIF.
** end of change

  CLEAR: L_VTABLE, L_VTABLE[], L_NO, L_FNAME.
  LOOP AT L_CONF_OUT.
    MOVE-CORRESPONDING L_CONF_OUT  TO L_VTABLE.  APPEND L_VTABLE.
  ENDLOOP.

  IF MTYPE = 'H' .      " ALC Unique.
    READ TABLE L_VTABLE WITH KEY ATNAM = 'P_VIN_123'.
    IF SY-SUBRC = 0.
      CONCATENATE L_VIN_SPEC L_VTABLE-ATWRT INTO L_VIN_SPEC.
    ELSE.
      RETURN_FLAG = 'V'.
    ENDIF.
    READ TABLE L_VTABLE WITH KEY ATNAM = 'P_VIN_4'.
    IF SY-SUBRC = 0.
      CONCATENATE L_VIN_SPEC L_VTABLE-ATWRT INTO L_VIN_SPEC.
    ELSE.
      RETURN_FLAG = 'V'.
    ENDIF.
    READ TABLE L_VTABLE WITH KEY ATNAM = 'P_VIN_5'.
    IF SY-SUBRC = 0.
      CONCATENATE L_VIN_SPEC L_VTABLE-ATWRT INTO L_VIN_SPEC.
    ELSE.
      RETURN_FLAG = 'V'.
    ENDIF.
    READ TABLE L_VTABLE WITH KEY ATNAM = 'P_VIN_6'.
    IF SY-SUBRC = 0.
      CONCATENATE L_VIN_SPEC L_VTABLE-ATWRT INTO L_VIN_SPEC.
    ELSE.
      RETURN_FLAG = 'V'.
    ENDIF.
    READ TABLE L_VTABLE WITH KEY ATNAM = 'P_VIN_7'.
    IF SY-SUBRC = 0.
      CONCATENATE L_VIN_SPEC L_VTABLE-ATWRT INTO L_VIN_SPEC.
    ELSE.
      RETURN_FLAG = 'V'.
    ENDIF.
    READ TABLE L_VTABLE WITH KEY ATNAM = 'P_VIN_8'.
    IF SY-SUBRC = 0.
      CONCATENATE L_VIN_SPEC L_VTABLE-ATWRT INTO L_VIN_SPEC.
    ELSE.
      RETURN_FLAG = 'V'.
    ENDIF.
    READ TABLE L_VTABLE WITH KEY ATNAM = 'P_VIN_9'.
    IF SY-SUBRC = 0.
      CONCATENATE L_VIN_SPEC L_VTABLE-ATWRT INTO L_VIN_SPEC.
    ELSE.
      RETURN_FLAG = 'V'.
    ENDIF.
    READ TABLE L_VTABLE WITH KEY ATNAM = 'P_VIN_10'.
    IF SY-SUBRC = 0.
      CONCATENATE L_VIN_SPEC L_VTABLE-ATWRT INTO L_VIN_SPEC.
    ELSE.
      RETURN_FLAG = 'V'.
    ENDIF.
    READ TABLE L_VTABLE WITH KEY ATNAM = 'P_VIN_11'.
    IF SY-SUBRC = 0.
      CONCATENATE L_VIN_SPEC L_VTABLE-ATWRT INTO L_VIN_SPEC.
    ELSE.
      RETURN_FLAG = 'V'.
    ENDIF.

    " Delete P_VINXXX Characteristics...
    DELETE L_VTABLE WHERE ATNAM = 'P_VIN_123'.
    DELETE L_VTABLE WHERE ATNAM = 'P_VIN_4'  .
    DELETE L_VTABLE WHERE ATNAM = 'P_VIN_5'  .
    DELETE L_VTABLE WHERE ATNAM = 'P_VIN_6'  .
    DELETE L_VTABLE WHERE ATNAM = 'P_VIN_7'  .
    DELETE L_VTABLE WHERE ATNAM = 'P_VIN_8'  .
    DELETE L_VTABLE WHERE ATNAM = 'P_VIN_9'  .
    DELETE L_VTABLE WHERE ATNAM = 'P_VIN_10' .
    DELETE L_VTABLE WHERE ATNAM = 'P_VIN_11' .

    CLEAR: L_VTABLE.
    L_VTABLE-ATNAM = 'P_VIN_SPEC'.
    L_VTABLE-ATWRT = L_VIN_SPEC.
    APPEND L_VTABLE.

    READ TABLE L_VTABLE WITH KEY ATNAM = 'P_MI'.
    L_MI = L_VTABLE-ATWRT.
    READ TABLE L_VTABLE WITH KEY ATNAM = 'P_OCN'.
    L_OCN = L_VTABLE-ATWRT.

    DATA: L_TEXT(50).
    CONCATENATE MATNR L_VIN_SPEC L_MI L_OCN INTO L_TEXT.
    MESSAGE S001(ZMPP) WITH L_TEXT.
*    write: / matnr, l_vin_spec, L_MI, ' ', L_OCN .

    DO 9 TIMES.
      CLEAR: L_FNAME, L_VTABLE, VAL_TABLE .
      L_NO = L_NO + 1.
      CONCATENATE 'P_ALC_U_' L_NO+2(1) INTO L_FNAME.
      READ TABLE L_CONF_OUT WITH KEY ATNAM = L_FNAME.
      IF SY-SUBRC = 0.
        IF L_CONF_OUT-ATWRT = '-' OR L_CONF_OUT-ATWRT = 'not specified'.
          L_CHECK = 'X' .
*       ELSE.
*         MOVE-CORRESPONDING l_conf_out  TO l_vtable.  APPEND l_vtable.
        ENDIF.
      ENDIF.
    ENDDO.

    DO 90 TIMES.
      CLEAR: L_FNAME, L_VTABLE, VAL_TABLE .
      L_NO = L_NO + 1.
      CONCATENATE 'P_ALC_U_' L_NO+1(2) INTO L_FNAME.
      READ TABLE L_CONF_OUT WITH KEY ATNAM = L_FNAME.
      IF SY-SUBRC = 0.
        IF L_CONF_OUT-ATWRT = '-' OR L_CONF_OUT-ATWRT = 'not specified'.
          L_CHECK = 'X' .
*       ELSE.
*         MOVE-CORRESPONDING l_conf_out  TO l_vtable.  APPEND l_vtable.
        ENDIF.
      ENDIF.
    ENDDO.

    DO 101 TIMES.
      CLEAR: L_FNAME, L_VTABLE, VAL_TABLE .
      L_NO = L_NO + 1.
      CONCATENATE 'P_ALC_U_' L_NO      INTO L_FNAME.
      READ TABLE L_CONF_OUT WITH KEY ATNAM = L_FNAME.
      IF SY-SUBRC = 0.
        IF L_CONF_OUT-ATWRT = '-' OR L_CONF_OUT-ATWRT = 'not specified'.
          L_CHECK = 'X' .
*       ELSE.
*         MOVE-CORRESPONDING l_conf_out  TO l_vtable.  APPEND l_vtable.
        ENDIF.
      ENDIF.
    ENDDO.
  ELSE.                " ALC Color...
    DO 9 TIMES.
      CLEAR: L_FNAME, L_VTABLE, VAL_TABLE .
      L_NO = L_NO + 1.
      CONCATENATE 'P_ALC_C_' L_NO+2(1) INTO L_FNAME.
      READ TABLE L_CONF_OUT WITH KEY ATNAM = L_FNAME.
      IF SY-SUBRC = 0.
        IF L_CONF_OUT-ATWRT = '-' OR L_CONF_OUT-ATWRT = 'not specified'.
          L_CHECK = 'X' .
*       ELSE.
*         MOVE-CORRESPONDING l_conf_out  TO l_vtable.  APPEND l_vtable.
        ENDIF.
      ENDIF.
    ENDDO.

    DO 41 TIMES.
      CLEAR: L_FNAME, L_VTABLE, VAL_TABLE .
      L_NO = L_NO + 1.
      CONCATENATE 'P_ALC_C_' L_NO+1(2) INTO L_FNAME.
      READ TABLE L_CONF_OUT WITH KEY ATNAM = L_FNAME.
      IF SY-SUBRC = 0.
        IF L_CONF_OUT-ATWRT = '-' OR L_CONF_OUT-ATWRT = 'not specified'.
          L_CHECK = 'X' .
*       ELSE.
*         MOVE-CORRESPONDING l_conf_out  TO l_vtable.  APPEND l_vtable.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
  L_VTABLE-ATNAM = 'P_PERF_YN'.
  IF L_CHECK = 'X'.
*---START wskim 03/07/2005 requested ny hur
*do'nt need chkeck logic, first of all must initial
*    l_vtable-atwrt = 'N'        .
    L_VTABLE-ATWRT = ' '        .
*---end
  ELSE.
    L_VTABLE-ATWRT = ' '        .
  ENDIF.
  APPEND L_VTABLE.

  " Check the Existance of the Classification View

  " If it is, Call the function for the change the Characteristics..
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = MATNR
            MODE         = 'W'
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = L_VTABLE
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC =  0.
    RETURN_FLAG = 'S' .
    CLEAR: VAL_TABLE, VAL_TABLE[].
    LOOP AT L_CONF_OUT .
      MOVE-CORRESPONDING L_CONF_OUT TO VAL_TABLE.  APPEND VAL_TABLE .
      APPEND VAL_TABLE .
    ENDLOOP.
  ELSE.
    RETURN_FLAG = 'E' .
  ENDIF.
ENDFUNCTION.
