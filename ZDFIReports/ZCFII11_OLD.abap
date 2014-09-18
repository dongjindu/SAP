*&--------------------------------------------------------------------
*& Author                 : Andy Choi
*& Creation Date          : 09/20/2003
*& Specification By       : Andy Choi
*& Addl documentation     :
*& Description  : Update Depreciation Data to Appr.Req Variants
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT  ZCFII11 NO STANDARD PAGE HEADING
                LINE-SIZE 132
                LINE-COUNT 65
                MESSAGE-ID ZMFI.


TABLES: IMAK, IMAV, ANIA, ANIB, IMAVZ.

DATA GV_CHK.

SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-010.
PARAMETERS: P_GJAHR LIKE IMAVZ-GJAHR DEFAULT SY-DATUM(4) OBLIGATORY,
            P_VERSI LIKE IMAVZ-VERSI DEFAULT 'ID' OBLIGATORY,
            P_RESET AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B0.

SELECT-OPTIONS:
  S_IVART   FOR   IMAK-IVART,
  S_POSNR   FOR   IMAK-POSNR,
  S_VKOSTL  FOR   IMAK-VKOSTL,
* AR approval year
  S_GJAHR   FOR   IMAK-GJAHR,
  S_USR03   for   imak-USR03,
  S_WDATU   for   imak-WDATU,
  S_USR09   for   imak-USR09.

DATA: C_FLG_CHANGE  LIKE  T020-AKTYP.

DATA: I_IMAK   LIKE IMAK OCCURS 0 WITH HEADER LINE.
DATA: I_IMAKPA LIKE IMAKPA OCCURS 0 WITH HEADER LINE.

DATA:
    LT_ANIA  LIKE RANIA OCCURS 0 WITH HEADER LINE,
    LT_ANIB  LIKE STANDARD TABLE OF RANIB
                  INITIAL SIZE 10 WITH HEADER LINE,
    LT_ANIB1 LIKE STANDARD TABLE OF RANIB
                  INITIAL SIZE 10 WITH HEADER LINE.

*&--------------------------------------------------------------------

PERFORM GET_AR_MASTER.

LOOP AT I_IMAK.
  SELECT SINGLE * FROM IMAVZ
      WHERE GJAHR = P_GJAHR
        AND POSNR = I_IMAK-POSNR
        AND VERSI = P_VERSI.

  CHECK SY-SUBRC = 0.

* read AR variant
  SELECT SINGLE * FROM IMAV
     WHERE POSNR = I_IMAK-POSNR
       AND VARNT = IMAVZ-VARNT.

* << Start of modification on 10.17.2006 by Michelle
*  IF P_RESET = 'X'.
*    PERFORM RESET_DEPR USING IMAV-OBJNR.
*  ELSE.
*    PERFORM SIMULATE_DEPR.
*  ENDIF.

* End of modification on 10.17.2006 by Michelle >>
  IF P_RESET = 'X'.
    PERFORM RESET_DEPR USING IMAV-OBJNR.

    CALL FUNCTION 'DEPREC_SIMUL_MODIFIKATION'
         TABLES
              T_ANIA = LT_ANIA
              T_ANIB = LT_ANIB.
    IF SY-SUBRC = 0.
      WRITE:/ I_IMAK-POSNR, ' is updated!'.
    ENDIF.

  ELSE.
    PERFORM SIMULATE_DEPR.

    CALL FUNCTION 'DEPREC_SIMUL_MODIFIKATION'
         TABLES
              T_ANIA = LT_ANIA
              T_ANIB = LT_ANIB1.

    IF SY-SUBRC = 0.
      WRITE:/ I_IMAK-POSNR, ' is updated!'.
    ENDIF.
  ENDIF.

* ANIA, ANIB tables...
*  CALL FUNCTION 'DEPREC_SIMUL_MODIFIKATION'
*       TABLES
*            T_ANIA = LT_ANIA
*            T_ANIB = LT_ANIB.
*  IF SY-SUBRC = 0.
*    WRITE:/ I_IMAK-POSNR, ' is updated!'.
*  ENDIF.
ENDLOOP.

*&---------------------------------------------------------------------*
*&      Form  simulate_depr
*&---------------------------------------------------------------------*
FORM SIMULATE_DEPR.
  CLEAR GV_CHK.

  DATA:
    LS_RANIA LIKE RANIA,
    LS_ANIB  LIKE RANIB,
    L_SUBRC  LIKE SY-SUBRC,
    L_NODEP  LIKE SY-SUBRC.

  DATA:
        I_OBJNR LIKE  ANIA-OBJNR,
        I_ANLKL LIKE  ANIA-ANLKL,
        I_AKTIV LIKE  ANIA-AKTIV,
        I_BUKRS LIKE  T001-BUKRS,
        I_KOKRS LIKE  TKA01-KOKRS,
        I_IVPRO LIKE  TAIF1-IVPRO,
        I_REPID LIKE  SY-REPID,
        I_DYNNR LIKE  SY-DYNNR,
        I_AKTYP LIKE  T020-AKTYP.

  DATA: IT_AFASPLIT LIKE AFASPLIT OCCURS 0.

  I_BUKRS = I_IMAK-VBUKRS.  "responsible cc
*asset class
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = I_IMAK-USR03
       IMPORTING
            OUTPUT = I_ANLKL.
*cap.date
  I_AKTIV = I_IMAK-USR09.
  I_OBJNR = IMAV-OBJNR.

*investment profile
  SELECT SINGLE IVPRO INTO I_IVPRO
     FROM TAPRF
     WHERE ANLKL_S = I_ANLKL.
  IF SY-SUBRC = 0.
    L_NODEP = 0.
  ELSE.
* no depr.asset... delete..simulation data.
    L_NODEP = 1.
  ENDIF.

  I_DYNNR = SY-DYNNR.
  I_REPID = SY-REPID.

* controlling area
  SELECT SINGLE KOKRS INTO I_KOKRS
    FROM TKA02
    WHERE BUKRS = I_BUKRS.

  CLEAR: LT_ANIB, LT_ANIB1.
  REFRESH: LT_ANIB, LT_ANIB1.

  CALL FUNCTION 'AIPS_SIMUL_CHECK'
       EXPORTING
            I_KOKRS             = I_KOKRS
            I_BUKRS             = I_BUKRS
            I_ANLKL             = I_ANLKL
            I_AKTIV             = I_AKTIV
            I_OBJNR             = I_OBJNR
            I_IVPRO             = I_IVPRO
            I_DYNNR             = I_DYNNR
            I_REPID             = I_REPID
       IMPORTING
            ES_ANIA             = LS_RANIA
            E_SUBRC             = L_SUBRC
       TABLES
            ET_ANIB             = LT_ANIB
       EXCEPTIONS
            KEIN_BUKRS          = 1
            BUKRS_KEIN_AM_BUKRS = 2
            PERIODEN_NICHT_DEF  = 3.

  CHECK L_SUBRC IS INITIAL.


*IOPAAA0YR0001 0005    |0000022001|     |100.00
*FUNCTION DEPREC_SIMUL_MODIFIKATION.
*
*      CON_INS           LIKE T020-AKTYP  VALUE 'I',
*                                      - Update
*      CON_UPD           LIKE T020-AKTYP  VALUE 'U',
*                                      - Update set XUNVL
*      CON_UPD_SET       LIKE T020-AKTYP  VALUE 'S',
*                                      - delete
*      CON_DEL           LIKE T020-AKTYP  VALUE 'D',

  CLEAR I_IMAKPA.
  REFRESH I_IMAKPA.

  SELECT * INTO TABLE I_IMAKPA
    FROM IMAKPA WHERE POSNR = I_IMAK-POSNR.

  CLEAR LT_ANIA.
  REFRESH LT_ANIA.
  MOVE-CORRESPONDING LS_RANIA TO LT_ANIA.

*change user, date
  LT_ANIA-ERNAM = SY-UNAME.
  LT_ANIA-ERDAT = SY-DATUM.

* set insert/update/...
*  read table lt_anib into ls_anib index 1.
*  lt_ania-KZ    = ls_anib-KZ.
  SELECT SINGLE * FROM ANIA WHERE OBJNR = I_OBJNR.

* << Start of modification on 10.17.2006 by Michelle
*  IF SY-SUBRC = 0.
*    IF L_NODEP = 1.
*      LT_ANIA-KZ    = 'D'.
*      LT_ANIB-KZ    = 'D'.
*    ELSE.
*      LT_ANIA-KZ    = 'U'.
*      LT_ANIB-KZ    = 'U'.
*    ENDIF.
*    MODIFY LT_ANIB TRANSPORTING KZ WHERE OBJNR = I_OBJNR.
*  ELSE.
*    LT_ANIA-KZ    = 'I'.
*  ENDIF.
*
*  SELECT * INTO TABLE I_IMAKPA
*      FROM IMAKPA WHERE POSNR = I_IMAK-POSNR.
*  LOOP AT I_IMAKPA.
*    LT_ANIA-KOSTL = I_IMAKPA-AKOSTL.
*    LT_ANIA-AUFPR = I_IMAKPA-APROZ.
*    APPEND LT_ANIA.
*  ENDLOOP.

  IF SY-SUBRC = 0.
    PERFORM INSERT_APPR_REQ USING I_OBJNR.
    PERFORM DELETE_APPR_REQ USING I_OBJNR.

  ELSE.
    PERFORM INSERT_APPR_REQ USING I_OBJNR.
  ENDIF.
* End of modification on 10.17.2006 by Michelle >>


*  CALL FUNCTION 'AIPS_SIMUL_CREATE'
*       EXPORTING
*            is_ania     = ls_rania
*            i_objnr     = i_objnr
*            i_ivpro     = i_ivpro
*            i_kokrs     = i_kokrs
*       IMPORTING
*            e_parm_flag = c_flg_change  "return
*       TABLES
*            it_afasplit = it_afasplit  "input
*            it_anib     = lt_anib. "input


ENDFORM.                    " update_depr
*&---------------------------------------------------------------------*
*&      Form  get_ar_master
*&---------------------------------------------------------------------*
FORM GET_AR_MASTER.

  SELECT * FROM IMAK INTO TABLE I_IMAK
    WHERE IVART  IN S_IVART
      AND POSNR  IN S_POSNR
      AND VKOSTL IN S_VKOSTL
      AND GJAHR  IN S_GJAHR
      AND USR03  IN S_USR03
      AND WDATU  IN S_WDATU
      AND USR09  IN S_USR09.

ENDFORM.                    " get_ar_master
*&---------------------------------------------------------------------*
*&      Form  reset_depr
*&---------------------------------------------------------------------*
FORM RESET_DEPR USING F_OBJNR.
  REFRESH: LT_ANIA, LT_ANIB.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_ANIA
      FROM ANIA WHERE OBJNR = F_OBJNR.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_ANIB
      FROM ANIB WHERE OBJNR = F_OBJNR.

  IF SY-SUBRC = 0.
    LT_ANIA-KZ    = 'D'.
    LT_ANIB-KZ    = 'D'.

    MODIFY LT_ANIA TRANSPORTING KZ WHERE OBJNR = F_OBJNR.
    MODIFY LT_ANIB TRANSPORTING KZ WHERE OBJNR = F_OBJNR.
  ENDIF.

ENDFORM.                    " reset_depr
*&---------------------------------------------------------------------*
*&      Form  INSERT_APPR_REQ
*&---------------------------------------------------------------------*
*       Get Data for Insertion of Appropriation req.
*----------------------------------------------------------------------*
FORM INSERT_APPR_REQ USING P_OBJNR TYPE J_OBJNR.
  LT_ANIA-KZ = 'I'.
  PERFORM APPEND_LT_ANIA USING 'I'.

  LT_ANIB-LFDNR = LT_ANIA-LFDNR.
  LT_ANIB-KZ = 'I'.
  PERFORM APPEND_LT_ANIB1 USING P_OBJNR.

ENDFORM.                    " INSERT_APPR_REQ
*&---------------------------------------------------------------------*
*&      Form  DELETE_APPR_REQ
*&---------------------------------------------------------------------*
*       Get Data for deletion of Appropriation req.
*----------------------------------------------------------------------*
FORM DELETE_APPR_REQ USING P_OBJNR TYPE J_OBJNR.
  LT_ANIA-KZ = 'D'.
  PERFORM APPEND_LT_ANIA USING 'D'.

  LT_ANIB-LFDNR = LT_ANIA-LFDNR.
  LT_ANIB-KZ = 'D'.
  PERFORM APPEND_LT_ANIB1 USING P_OBJNR.

ENDFORM.                    " DELETE_APPR_REQ
*&---------------------------------------------------------------------*
*&      Form  APPEND_LT_ANIA
*&---------------------------------------------------------------------*
FORM APPEND_LT_ANIA USING P_KZ.
  LOOP AT I_IMAKPA.
    IF P_KZ = 'I'.
      IF ANIA IS INITIAL.
        CLEAR LT_ANIA-LFDNR.
      ELSE.
        LT_ANIA-LFDNR = ANIA-LFDNR + 1.
      ENDIF.

    ELSEIF P_KZ = 'D'.
      MOVE-CORRESPONDING ANIA TO LT_ANIA.
    ENDIF.

    LT_ANIA-KOSTL = I_IMAKPA-AKOSTL.
    LT_ANIA-AUFPR = I_IMAKPA-APROZ.
    APPEND LT_ANIA.
  ENDLOOP.

ENDFORM.                    " APPEND_LT_ANIA
*&---------------------------------------------------------------------*
*&      Form  APPEND_LT_ANIB1
*&---------------------------------------------------------------------*
FORM APPEND_LT_ANIB1 USING P_OBJNR TYPE J_OBJNR.
  MODIFY LT_ANIB TRANSPORTING LFDNR KZ WHERE OBJNR = P_OBJNR.

  LOOP AT LT_ANIB.
    MOVE-CORRESPONDING LT_ANIB TO LT_ANIB1.
    APPEND LT_ANIB1.
    CLEAR LT_ANIB1.
  ENDLOOP.

ENDFORM.                    " APPEND_LT_ANIB1
