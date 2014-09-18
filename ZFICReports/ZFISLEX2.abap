REPORT RGIVS000 MESSAGE-ID GI.

TABLES: GLU1.

*Add
INCLUDE FGLIND04.
TABLES: BKPF, BSEG, COBK.
TABLES: ACCIT, ACCIT_GLX, ACCHD.

TABLES: SKA1, AUFK.
*----------------------------------------------------------------------*
*        E01_MVC  USING FROM_FIELD TO_FIELD
*----------------------------------------------------------------------*
*        This exit is an example (commented out) that shows you how
*        to manipulate the account number while posting data
*
*----------------------------------------------------------------------*

FORM E01_MVC  USING FROM_FIELD TO_FIELD.
  DATA: HELP_FIELD TYPE RACCT.

* store the account number in the help field
  HELP_FIELD = FROM_FIELD.
* replace the last digit of the account number
  IF HELP_FIELD+9(1) = '0'.
    HELP_FIELD+9(1) = '9'.
  ENDIF.
* give the new value back to the calling program
  TO_FIELD = HELP_FIELD.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM E91_MVC                                                  *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM E91_MVC  USING FROM_FIELD TO_FIELD.
  DATA:
  LT_ACCHD  LIKE  ACCHD OCCURS 1 WITH HEADER LINE,
  LT_ACCIT  LIKE  ACCIT OCCURS 1 WITH HEADER LINE.

  IF ( ACCIT_GLX-VORGN <> 'RFRK' ).

*    Fill structure ACCIT to pass it to the substitution:
    MOVE-CORRESPONDING ACCIT_GLX TO ACCHD.
    MOVE-CORRESPONDING ACCIT_GLX TO ACCIT.

*    Set ACCIT-FKBER to initial value:
    CLEAR ACCIT-FKBER.

    APPEND ACCIT TO LT_ACCIT.
    APPEND ACCHD TO LT_ACCHD.

    CALL FUNCTION 'AC_DOCUMENT_FAREA_SET'
         TABLES
              T_ACCHD = LT_ACCHD
              T_ACCIT = LT_ACCIT. "functional area is filled

    READ TABLE LT_ACCIT INDEX 1.
    MOVE LT_ACCIT-FKBER TO ACCIT_GLX-FKBER.

  ENDIF.


  MOVE ACCIT_GLX-FKBER TO TO_FIELD.

**  Before, clear Func.Area Data
*  if  ACCIT_GLX-BUKRS = 'H201' AND ACCIT_GLX-BUDAT <= '20040531'
*  AND ACCIT_GLX-HKONT >= '0000600000'.
** Chg
*    if from_field = '0004'.
*      to_field = '0002'.
*    endif.
** AuC
*    if ACCIT_GLX-hkont > '0000901000'.
*      to_field = '0001'.
*    endif.
*
** normal process
*  else.
*    MOVE FROM_FIELD TO TO_FIELD.
*  endif.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM E93_MVC                                                  *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM E93_MVC  USING FROM_FIELD TO_FIELD.
  tables: AFPO.
  data: l_aufnr like afpo-aufnr.

  check accit_glx-BSCHL >= '80'.

  IF ACCIT_GLX-matnr is initial.
* look order
    if not ACCIT_GLX-aufnr is initial.
      select single * from afpo
         where aufnr = ACCIT_GLX-aufnr.
      if sy-subrc = 0.
        MOVE AFPO-matnr TO TO_FIELD.
      endif.
* look assignment
    elseif not accit_glx-zuonr is initial.
*    ( acchd-GLVOR(2) = 'RM' or acchd-GLVOR(2) = 'SD' ).

      l_aufnr = accit_glx-zuonr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
           EXPORTING
                INPUT  = l_aufnr  "accit_glx-zuonr
           IMPORTING
                OUTPUT = l_aufnr.

      select single * from afpo
        where aufnr = l_aufnr.
      if sy-subrc = 0.
        MOVE AFPO-matnr TO TO_FIELD.
      endif.
    endif.
* material
  else.
    MOVE ACCIT_GLX-matnr TO TO_FIELD.
  endif.

ENDFORM.
