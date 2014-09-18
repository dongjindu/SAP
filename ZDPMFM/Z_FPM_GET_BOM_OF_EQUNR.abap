FUNCTION Z_FPM_GET_BOM_OF_EQUNR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_EQUNR) LIKE  EQUI-EQUNR
*"     VALUE(I_WERKS) LIKE  T001W-WERKS
*"     VALUE(I_TPLNR) LIKE  ITOB-TPLNR
*"  EXPORTING
*"     VALUE(RETURN) LIKE  BAPIRETURN STRUCTURE  BAPIRETURN
*"     VALUE(E_SUBMT) LIKE  ITOB-SUBMT
*"  TABLES
*"      T_BOM_HIRACHY STRUCTURE  ZSPM_BOM_HIRACHY OPTIONAL
*"      T_STB STRUCTURE  STPOX OPTIONAL
*"----------------------------------------------------------------------
  CONSTANTS: LC_STLAN  VALUE '4'.              "BOM Usage

  DATA: LIT_BOM TYPE TABLE OF ZSPM_BOM_HIRACHY WITH HEADER LINE.
  DATA: LT_STB LIKE STPOX OCCURS 0 WITH HEADER LINE.

  DATA: LV_STLNR LIKE EQST-STLNR.

  DATA : LS_ITOB LIKE ITOB.
  DATA : LV_EEQUI LIKE EQUI.

  CALL FUNCTION 'CHECK_EQUI_BOM'
       EXPORTING
            DATUM          = SY-DATUM
            EQUNR          = I_EQUNR
            FLG_LVORM_EXC  = 'X'
            EEQUI          = LV_EEQUI
       EXCEPTIONS
            EQUI_NOT_FOUND = 1
            EQUI_TO_DELETE = 2
            OTHERS         = 3.

  CLEAR RETURN.
  IF SY-SUBRC <> 0.
    PERFORM ERROR_MESSAGE USING TEXT-M04 '' '' '' RETURN.
  ENDIF.

  CLEAR LIT_BOM.    REFRESH LIT_BOM.

  CLEAR WA_WERKS.
  WA_WERKS = I_WERKS.

  SELECT SINGLE * INTO CORRESPONDING FIELDS OF LS_ITOB
               FROM ITOB
               WHERE EQUNR = I_EQUNR
                AND  TPLNR = I_TPLNR
               AND  SUBMT NE SPACE.

  CLEAR RETURN.
  IF SY-SUBRC <> 0.
    PERFORM ERROR_MESSAGE USING TEXT-M05 '' '' '' RETURN.
  ENDIF.

  PERFORM GET_RECURSIVE_BOM_NEW TABLES LIT_BOM
                                       LT_STB
                                USING  I_EQUNR
                                       LS_ITOB-SUBMT
                                       I_WERKS
                                       RETURN.

  T_BOM_HIRACHY[] = LIT_BOM[].
  T_STB[]         = LT_STB[].
  E_SUBMT         = LS_ITOB-SUBMT.

ENDFUNCTION.
