FUNCTION Z_FPM_GET_BOM_OF_FUNC.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_TPLNR) LIKE  IFLOT-TPLNR
*"  EXPORTING
*"     VALUE(RETURN) LIKE  BAPIRETURN STRUCTURE  BAPIRETURN
*"  TABLES
*"      T_EQUI_LIST STRUCTURE  ZSPM_EQUI_BY_FUNC
*"  EXCEPTIONS
*"      INVALID_TPLNR
*"      NOT_FOUND_EQUI
*"----------------------------------------------------------------------

  DATA: LV_STATUS.

  CLEAR RETURN.

  IF I_TPLNR IS INITIAL.
    PERFORM ERROR_MESSAGE USING TEXT-M02 '' '' '' RETURN.
    EXIT.
  ENDIF.

  CLEAR RETURN.
  LV_STATUS = 'X'.
  PERFORM CHECK_STATUS_OF_TPLNR USING I_TPLNR CHANGING LV_STATUS.

  IF LV_STATUS EQ ' '.
    PERFORM ERROR_MESSAGE USING TEXT-M03 '' '' '' RETURN.
    EXIT.
  ENDIF.

  CLEAR IT_EQUI_LIST.    REFRESH IT_EQUI_LIST.

  DATA : LV_WERKS LIKE T001W-WERKS.
  DATA : LT_STB    LIKE STPOX OCCURS 0
                              WITH HEADER LINE.
  DATA : LT_MATCAT LIKE CSCMAT OCCURS 0
                               WITH HEADER LINE.
  DATA : LST_TOPTPL LIKE CSTTPL.
  DATA : LV_MENGE(15).

  SELECT SINGLE SWERK INTO LV_WERKS FROM IFLO
   WHERE TPLNR = I_TPLNR
     AND SPRAS = SY-LANGU.

  CALL FUNCTION 'CS_BOM_EXPL_TPL_V1'
       EXPORTING
            CAPID                         = 'INST'
            DATUV                         = SY-DATUM
            TPNRV                         = I_TPLNR
            WERKS                         = LV_WERKS
       IMPORTING
            TOPTPL                        = LST_TOPTPL
       TABLES
            STB                           = LT_STB
            MATCAT                        = LT_MATCAT
       EXCEPTIONS
            ALT_NOT_FOUND                 = 1
            CALL_INVALID                  = 2
            FUNCTIONAL_LOCATION_NOT_FOUND = 3
            MISSING_AUTHORIZATION         = 4
            NO_BOM_FOUND                  = 5
            NO_PLANT_DATA                 = 6
            NO_SUITABLE_BOM_FOUND         = 7
            OTHERS                        = 8.
  IF SY-SUBRC <> 0.
    PERFORM ERROR_MESSAGE USING  SY-MSGV1 SY-MSGV2
                                 SY-MSGV3 SY-MSGV4 RETURN.
  ENDIF.

  CLEAR IT_EQUI_LIST.
  DATA : LV_TEMP(12).

  LOOP AT LT_STB.
    MOVE : LST_TOPTPL-TPLNR TO IT_EQUI_LIST-TPLNR,
           LT_STB-IDNRK     TO IT_EQUI_LIST-EQUNR.
    MOVE : LT_STB-OJTXP     TO IT_EQUI_LIST-SHTXT.
    IF LT_STB-MENGE NE 0.
      WRITE : LT_STB-MENGE TO LV_MENGE
                        LEFT-JUSTIFIED UNIT LT_STB-MEINS.
      SPLIT LV_MENGE AT '.' INTO LV_MENGE LV_TEMP.
      CONCATENATE LV_MENGE '(' LT_STB-MEINS ')' INTO LV_MENGE.
      MOVE :  LV_MENGE TO IT_EQUI_LIST-ZMENGE .
    ELSE.
      MOVE : LT_STB-OJTXP     TO IT_EQUI_LIST-SHTXT.
    ENDIF.
    APPEND IT_EQUI_LIST.   CLEAR IT_EQUI_LIST.
  ENDLOOP.

  T_EQUI_LIST[] = IT_EQUI_LIST[].
ENDFUNCTION.
