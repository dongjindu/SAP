*002007974700001014622005
*But there are still a few records in table COSB where the quantity
*has been set in period 2 (field MEG002) but NOT the quantity unit:
*
*Client Lednr Object Year Cat. V Act. Cost ele. Unit Qty Per. 2
*260 0 OR000001000060 2005 32 0 WIPR 540200 0.000 67.398-
*260 0 OR000001000061 2005 32 0 WIPR 540200 0.000 57.494-
*260 0 OR000001000062 2005 32 0 WIPR 540200 0.000 94.976-
*
*These records have to be fixed manually by setting the quantity in
*period 002 to zero. This can be done by a small ABAP.

REPORT Z_PPC_WIP_CORRECT_2 LINE-SIZE 300.
TABLES: COSB.

data: ls_cosb like cosb.

*SELECT-OPTIONS: SO_H_ID FOR PPC_HEAD-HEADID.
parameters:
  P_LEDNR  like cosb-LEDNR,
  P_OBJNR  like cosb-OBJNR,
  P_GJAHR  like cosb-GJAHR,
  P_WRTTP  like cosb-WRTTP,
  P_VERSN  like cosb-VERSN,
  P_ABKAT  like cosb-ABKAT,
  P_KSTAR  like cosb-KSTAR,
  P_HRKFT  like cosb-HRKFT,
  P_VRGNG  like cosb-VRGNG,
  P_PAROB  like cosb-PAROB,
  P_BEKNZ  like cosb-BEKNZ,
  P_AWKUS  like cosb-AWKUS,
  P_TWAER  like cosb-TWAER,
  P_PERBL  like cosb-PERBL.

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

  select single * into ls_cosb
     from cosb
     where LEDNR = P_LEDNR
       and OBJNR = P_OBJNR
       and GJAHR = P_GJAHR
       and WRTTP = P_WRTTP
       and VERSN = P_VERSN
       and ABKAT = P_ABKAT
       and KSTAR = P_KSTAR
       and HRKFT = P_HRKFT
       and VRGNG = P_VRGNG
       and PAROB = P_PAROB
       and BEKNZ = P_BEKNZ
       and AWKUS = P_AWKUS
       and TWAER = P_TWAER
       and PERBL = P_PERBL.
  check sy-subrc = 0.

  write:/ ls_cosb-MEG002, ' will be cleared'.
  clear:  ls_cosb-MEG002.

  IF P_TEST IS INITIAL.
    update cosb from ls_cosb.
    COMMIT WORK.
  endif.
