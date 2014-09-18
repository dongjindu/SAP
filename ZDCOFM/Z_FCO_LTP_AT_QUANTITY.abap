FUNCTION Z_FCO_LTP_AT_QUANTITY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_KSPP) LIKE  KSPP STRUCTURE  KSPP
*"  TABLES
*"      R_RSLSTAR STRUCTURE  RSLSTAR
*"      IT_ZSCO_LTP_ATQ STRUCTURE  ZSCO_LTP_ATQ
*"  EXCEPTIONS
*"      T442C
*"      CONV_UNIT_ERROR
*"----------------------------------------------------------------------

*  DATA : RUNTIME_EXP LIKE  SY-INDEX.
  DATA : LV_SUBRC     LIKE SY-SUBRC,
         IT_L_CXLST     LIKE CXLST OCCURS 0 WITH HEADER LINE,
         IT_L_PLAF      LIKE PLAF  OCCURS 0 WITH HEADER LINE.

  DATA : IT_L_SAUF_IMP LIKE  SAUF OCCURS 0 WITH HEADER LINE,
         IT_L_MESG_EXP LIKE  MESG OCCURS 0 WITH HEADER LINE.

  DATA : LV_AOTYP  LIKE ONRAO-AOTYP.
  DATA : IT_TMP_ZSCO_LTP_ATQ  LIKE ZSCO_LTP_ATQ
                              OCCURS 0
                              WITH HEADER LINE.
  DATA : WA_L_KSPP  LIKE KSPP.

* Copy KSPP data
  CLEAR WA_L_KSPP.
  MOVE-CORRESPONDING  I_KSPP TO WA_L_KSPP.

* MRP checking (LTP)
* WA_L_KSPP-DETAIL_MW   = 'X'.

* Read Verion Relationship
  CLEAR T442C.
  SELECT SINGLE * FROM  T442C
         WHERE  KOKRS       = WA_L_KSPP-KOKRS
         AND    VERSN       = WA_L_KSPP-VERSN
         AND    GJAHR       = WA_L_KSPP-GJAHR.

* Read Controlling Area Information
  CLEAR TKA01.
  SELECT SINGLE * FROM TKA01
                 WHERE KOKRS = WA_L_KSPP-KOKRS.

* Read MRP data (Only LTP data)
  PERFORM SOP_OR_MRP
     CHANGING
        T442C
        LV_AOTYP.

* End_TO date
  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            I_GJAHR = WA_L_KSPP-GJAHR
            I_PERIV = TKA01-LMONA
            I_POPER = WA_L_KSPP-TO_P
       IMPORTING
            E_DATE  = WA_L_KSPP-TO_D.

  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
       EXPORTING
            I_GJAHR = WA_L_KSPP-GJAHR
            I_PERIV = TKA01-LMONA
            I_POPER = WA_L_KSPP-FROM_P
       IMPORTING
            E_DATE  = WA_L_KSPP-FROM_D.

* Plant Information
  CLEAR : IT_WERKS, IT_WERKS[].
  IT_WERKS-WERKS = WA_L_KSPP-WERKS .
  APPEND IT_WERKS. CLEAR IT_WERKS.

*.... Operativer Plan oder Langfristplanung
  CLEAR : IT_TMP_ZSCO_LTP_ATQ, IT_TMP_ZSCO_LTP_ATQ[].
  DO.
    PERFORM PLAF_SELECT_CO
       TABLES
          IT_WERKS
          IT_L_PLAF
       USING
          T442C-PLSCN
          WA_L_KSPP-FROM_D
          WA_L_KSPP-TO_D
          WA_L_KSPP-PCORR
       CHANGING
          LV_SUBRC.
    IF LV_SUBRC = 1.
      EXIT.
    ENDIF.
    SORT IT_L_PLAF BY MATNR PWWRK.

*.... Leistungen berechnen und ans CO übergeben
    CALL FUNCTION 'K_PARA_PLAF_SAUF_ACTIVITY_CALC'
      EXPORTING
        KSPP_IMP          = WA_L_KSPP
        T442C_IMP         = T442C
*     IMPORTING
*       RUNTIME_EXP       =
      TABLES
        PLAF_IMP          = IT_L_PLAF
        SAUF_IMP          = IT_L_SAUF_IMP
        CXLST_EXP         = IT_L_CXLST
        MESG_EXP          = IT_L_MESG_EXP.

* Collecting DATA
    IF SY-SUBRC = 0.
      LOOP AT IT_L_CXLST  WHERE LSTAR IN R_RSLSTAR.
        MOVE-CORRESPONDING IT_L_CXLST TO IT_TMP_ZSCO_LTP_ATQ.
        COLLECT  IT_TMP_ZSCO_LTP_ATQ.
        CLEAR    IT_TMP_ZSCO_LTP_ATQ.
      ENDLOOP.
    ENDIF.
  ENDDO.

* Checking Period/AT/Conversion
  DATA : LV_LEINH LIKE CSSL-LEINH.
  CLEAR : IT_ZSCO_LTP_ATQ, IT_ZSCO_LTP_ATQ[].
  LOOP AT IT_TMP_ZSCO_LTP_ATQ.
* Check AT
    CLEAR LV_LEINH.
    CALL FUNCTION 'K_CSSL_READ'
      EXPORTING
        GJAHR                  = WA_L_KSPP-GJAHR
        KOKRS                  = WA_L_KSPP-KOKRS
        KOSTL                  = IT_TMP_ZSCO_LTP_ATQ-KOSTL
        LSTAR                  = IT_TMP_ZSCO_LTP_ATQ-LSTAR
*       SEQ_LATYP              = ' '
        S_ENQUEUE_IF_NEW       = 'X'
*       TEST_READ              = ' '
*       OBJNR                  =
      IMPORTING
*       AUSEH                  =
*       AUSFK                  =
*       LATYP                  =
        LEINH                  = LV_LEINH
*       OBJNR                  =
*       LATYPI                 =
*       FOUND                  =
*     TABLES
*       ITPERIODS              =
      EXCEPTIONS
        LSTAR_NOT_FOUND        = 1
        OTHERS                 = 2.
*IF not Found -> Delete It
    IF SY-SUBRC <> 0.
      DELETE IT_TMP_ZSCO_LTP_ATQ.
      CONTINUE.
    ENDIF.

* Adjusting UOM
    IF IT_TMP_ZSCO_LTP_ATQ-MNGEH <>  LV_LEINH.
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          INPUT                      = IT_TMP_ZSCO_LTP_ATQ-MENGE
*         NO_TYPE_CHECK              = 'X'
          ROUND_SIGN                 = 'X'
          UNIT_IN                    = IT_TMP_ZSCO_LTP_ATQ-MNGEH
          UNIT_OUT                   = LV_LEINH
        IMPORTING
*         ADD_CONST                  =
*         DECIMALS                   =
*         DENOMINATOR                =
*         NUMERATOR                  =
          OUTPUT                     = IT_TMP_ZSCO_LTP_ATQ-MENGE
        EXCEPTIONS
          CONVERSION_NOT_FOUND       = 1
          DIVISION_BY_ZERO           = 2
          INPUT_INVALID              = 3
          OUTPUT_INVALID             = 4
          OVERFLOW                   = 5
          TYPE_INVALID               = 6
          UNITS_MISSING              = 7
          UNIT_IN_NOT_FOUND          = 8
          UNIT_OUT_NOT_FOUND         = 9
          OTHERS                     = 10.
      IF SY-SUBRC <> 0.
        RAISE CONV_UNIT_ERROR.
      ENDIF.
      IT_TMP_ZSCO_LTP_ATQ-MNGEH = LV_LEINH.
      MODIFY IT_TMP_ZSCO_LTP_ATQ.
    ENDIF.
* Result TAB
    COLLECT IT_TMP_ZSCO_LTP_ATQ
       INTO IT_ZSCO_LTP_ATQ.
    CLEAR IT_ZSCO_LTP_ATQ.
    CLEAR IT_TMP_ZSCO_LTP_ATQ.
  ENDLOOP.

ENDFUNCTION.
