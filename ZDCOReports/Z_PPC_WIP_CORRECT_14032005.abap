REPORT Z_PPC_WIP_CORRECT_14032005 LINE-SIZE 300.
TABLES: PPC_HEAD, PPC_CONF_ACT_VAR,PPC_ORD_INF,QRP002,
        PPC_ACT.
DATA:
LT_C_ACT LIKE PPC_CONF_ACT_VAR OCCURS 0,
LT_ORD LIKE PPC_ORD_INF OCCURS 0,
LT_ACT LIKE PPC_ACT OCCURS 0,
LS_ACT LIKE PPC_ACT,
LS_ORD LIKE PPC_ORD_INF,
LT_HEAD LIKE PPC_HEAD OCCURS 0,
LS_HEAD LIKE PPC_HEAD,
LT_QRP002 LIKE QRP002 OCCURS 0,
LS_QRP002 LIKE QRP002,
LS_C_ACT LIKE PPC_CONF_ACT_VAR,
*
LF_AUFNR TYPE AUFNR,
LF_PKOSA_PLANT TYPE WERKS_D,
LF_OBJNR TYPE J_OBJNR,
LF_F_OBJNR TYPE F_OBJNR,
LF_PKOSA_ERROR TYPE C,
LF_KOKRS TYPE KOKRS,
LF_GJPER_POST TYPE CO_GJPER,
LF_GJPER_CURR TYPE CO_GJPER,
LF_GJPER_PREV TYPE CO_GJPER,
LF_ZAEHL TYPE COUNT_ZP,
LS_CPZP LIKE CPZP,
LS_CPZP_3 LIKE CPZP,
LT_CPZP LIKE CPZP OCCURS 0,
gT_CPZP LIKE CPZP OCCURS 0,
L_ANSWER(1),
L_TEXT1(70),
L_TEXT2(70).

data:count type I.
count = 0.

SELECT-OPTIONS: SO_H_ID FOR PPC_HEAD-HEADID.
PARAMETERS: P_TEST(1) DEFAULT 'X'.

START-OF-SELECTION.
* This report makes really changes in the system =>
* Check that the current user has debug authorization.
  AUTHORITY-CHECK OBJECT 'S_DEVELOP'
    ID 'DEVCLASS' DUMMY
    ID 'OBJTYPE'  FIELD 'DEBUG'
    ID 'OBJNAME'  DUMMY
    ID 'P_GROUP'  DUMMY
    ID 'ACTVT'    FIELD '03'.
  IF  SY-SUBRC <> 0.
    MESSAGE E895(M7) WITH 'Sorry, no authorization'.
  ENDIF.

  L_TEXT1  =
  'Please make sure that the report will not be started twice'.
  L_TEXT2  = 'Do you want to continue?'.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            TITEL     = 'Do not start the report twice'
            TEXTLINE1 = L_TEXT1
            TEXTLINE2 = L_TEXT2
       IMPORTING
            ANSWER    = L_ANSWER.
  IF L_ANSWER = 'A' OR
     L_ANSWER = 'N'.
    MESSAGE E895(M7) WITH 'Ok, abort the program'.
  ENDIF.


  REFRESH: LT_CPZP, LT_C_ACT, LT_HEAD, LT_ACT.
* search for negative quantity
  SELECT * INTO TABLE LT_C_ACT FROM PPC_CONF_ACT_VAR
                               WHERE DELTA_DUR_VAR < 0
                                 AND HEADID IN SO_H_ID.

* read the PPC_HEAD
  CHECK NOT LT_C_ACT[] IS INITIAL.
  SELECT * INTO TABLE LT_HEAD FROM PPC_HEAD
           FOR ALL ENTRIES IN LT_C_ACT
           WHERE HEADID = LT_C_ACT-HEADID
              AND FLG_SYNCH = 'X'
              AND FLG_ASYNCH = 'X'
              AND FLG_REVERSAL = 'X'.
  SORT LT_HEAD BY HEADID.

* read the ressource GUID
  SELECT * FROM PPC_ACT INTO TABLE LT_ACT
  FOR ALL ENTRIES IN LT_C_ACT
  WHERE ACTID = LT_C_ACT-ACTID.

* read the PPC_ORD_INF
  CHECK NOT LT_HEAD[] IS INITIAL.
  SELECT * INTO TABLE LT_ORD FROM PPC_ORD_INF
           FOR ALL ENTRIES IN LT_HEAD
           WHERE ORDERID = LT_HEAD-ORDERID.
  SORT LT_ORD BY ORDERID.

  READ TABLE LT_ORD WITH KEY DUMMY_ORDER = SPACE
                    TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
    MESSAGE E895(M7) WITH '!There are normal orders TOO!'.
  ENDIF.
*select * into table lt_qrp002 from qrp002
*         for all entries in lt_head
*         where CC_GUID eq lt_head-ACCASSOBJ.
*sort lt_qrp002 by AUFNR.
*DELETE ADJACENT DUPLICATES FROM lt_qrp002 comparing AUFNR.

  LOOP AT LT_C_ACT INTO LS_C_ACT.
    READ TABLE LT_HEAD INTO LS_HEAD
                       WITH KEY HEADID = LS_C_ACT-HEADID
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      BREAK-POINT.
      MESSAGE E001(00) WITH 'Missing HEADID' LS_C_ACT-HEADID.
    ENDIF.
    CHECK SY-SUBRC EQ 0.

    READ TABLE LT_ACT INTO LS_ACT
                      WITH KEY ACTID = LS_C_ACT-ACTID.
    IF SY-SUBRC NE 0.
      BREAK-POINT.
      MESSAGE E001(00) WITH 'Missing ACTID' LS_C_ACT-ACTID.
    ENDIF.
    CHECK SY-SUBRC EQ 0.

    CLEAR: LF_AUFNR,LF_OBJNR,LF_PKOSA_ERROR,
           LF_PKOSA_PLANT,LF_GJPER_POST,LF_GJPER_CURR,LF_GJPER_PREV,
           LF_KOKRS,LF_F_OBJNR.

*   get the OBJNR
    PERFORM OBJNR_GET(SAPLQRPRP) USING    LS_HEAD-ACCASSOBJ
                      CHANGING LF_AUFNR
                               LF_OBJNR
                               LF_PKOSA_ERROR.
    IF NOT LF_PKOSA_ERROR IS INITIAL.
      MESSAGE E001(QRPRP) WITH 'QRP_APO_REPORTINGPOINT_POST'
                        LS_HEAD-ACCASSOBJ SPACE SPACE.

    ENDIF.

*   Werk zum Produktkostensammler lesen und Buchungsperiode bestimmen
*   read plant to pcc and determine posting period
    PERFORM PKOSA_PLANT_GET(SAPLQRPRP) USING LF_AUFNR
                                       CHANGING LF_PKOSA_PLANT
                                                LF_KOKRS.
    PERFORM PERIODS_GET(SAPLQRPRP) USING    LF_PKOSA_PLANT
                                            LS_HEAD-POSTDATE
                                   CHANGING LF_GJPER_POST
                                            LF_GJPER_CURR
                                            LF_GJPER_PREV.
    CALL FUNCTION 'KCR01_GET_COST_RESOURCE'
         EXPORTING
              I_RESOURCE_GUID    = LS_ACT-RESOURCE_GUID
              I_CONTROLLING_AREA = LF_KOKRS
              I_KEY_DATE         = LS_HEAD-POSTDATE
         IMPORTING
              E_OBJECT_NUMBER    = LF_F_OBJNR
         EXCEPTIONS
              OTHERS             = 1.

*   select the CPZP entry
    IF P_TEST IS INITIAL.
      SELECT SINGLE FOR UPDATE * FROM CPZP INTO LS_CPZP
                                       WHERE OBJNR EQ LF_OBJNR
                                         AND F_OBJNR  EQ LF_F_OBJNR
                                         AND GJPER EQ LF_GJPER_POST
                                         AND ZAEHL EQ LF_ZAEHL.
    ELSE.
      SELECT SINGLE * FROM CPZP INTO LS_CPZP
                                       WHERE OBJNR EQ LF_OBJNR
                                         AND F_OBJNR  EQ LF_F_OBJNR
                                         AND GJPER EQ LF_GJPER_POST
                                         AND ZAEHL EQ LF_ZAEHL.
    ENDIF.
    WRITE: /1 'S.No',count,
              'HEADID',LS_HEAD-HEADID,
              'OBJNR',LF_OBJNR,
              'F_OBJNR',LF_F_OBJNR,
              'gjper',LF_GJPER_POST,
              'ZAEHL',LF_ZAEHL,
              'GMPER',LS_CPZP-GMPER,
              'ISTMN',LS_CPZP-ISTMN,
              'VARMN',LS_CPZP-VARMN,
              'delta', LS_C_ACT-DELTA_DUR_VAR.
*   LS_CPZP-VARMN might have GI as well as reversals
*   the total reversals for this entry
*   is it the sam uom?
    IF LS_CPZP-MEINH NE LS_C_ACT-DURUNIT.
      MESSAGE I001(00) WITH 'Uom was converted!'.
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
           EXPORTING
                INPUT                = LS_C_ACT-DELTA_DUR_VAR
                UNIT_IN              = LS_C_ACT-DURUNIT
                UNIT_OUT             = LS_CPZP-MEINH
           IMPORTING
                OUTPUT               = LS_C_ACT-DELTA_DUR_VAR
           EXCEPTIONS
                CONVERSION_NOT_FOUND = 1
                DIVISION_BY_ZERO     = 2
                INPUT_INVALID        = 3
                OUTPUT_INVALID       = 4
                OVERFLOW             = 5
                TYPE_INVALID         = 6
                UNITS_MISSING        = 7
                UNIT_IN_NOT_FOUND    = 8
                UNIT_OUT_NOT_FOUND   = 9
                OTHERS               = 10.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.

    LS_CPZP-GMPER  = LS_CPZP-GMPER  +
                     ( ABS( LS_C_ACT-DELTA_DUR_VAR ) ) * 2.
    LS_CPZP-ISTMN  = LS_CPZP-ISTMN +
                     ( ABS( LS_C_ACT-DELTA_DUR_VAR ) ) * 2.
    LS_CPZP-VARMN = LS_CPZP-VARMN - ABS( LS_C_ACT-DELTA_DUR_VAR ).


*    IF P_TEST IS INITIAL.
    append ls_cpzp to gt_cpzp.
*      UPDATE CPZP FROM LS_CPZP.
    IF P_TEST IS INITIAL.
      SELECT SINGLE FOR UPDATE * FROM CPZP INTO LS_CPZP_3
                                 WHERE OBJNR EQ LF_OBJNR
                                   AND F_OBJNR  EQ LF_F_OBJNR
                                   AND GJPER EQ '2005003'
                                   AND ZAEHL EQ LF_ZAEHL.
    else.
      SELECT SINGLE * FROM CPZP INTO LS_CPZP_3
                                 WHERE OBJNR EQ LF_OBJNR
                                   AND F_OBJNR  EQ LF_F_OBJNR
                                   AND GJPER EQ '2005003'
                                   AND ZAEHL EQ LF_ZAEHL.
    endif.
    IF SY-SUBRC EQ 0.
      Write: ' !period 003 origISTMN', LS_CPZP_3-ISTMN.
      LS_CPZP_3-ISTMN = LS_CPZP_3-ISTMN +
                  ( ABS( LS_C_ACT-DELTA_DUR_VAR ) ) * 2.
*        UPDATE CPZP FROM LS_CPZP_3.
      append ls_cpzp_3 to gt_cpzp.
*      ENDIF.

    ENDIF.
  ENDLOOP.

  IF P_TEST IS INITIAL.
    update cpzp from table gt_cpzp.
    COMMIT WORK.
  endif.
