REPORT Z_PPC_WIP_CORRECT_2 LINE-SIZE 300.
TABLES: CPZP.

data: ls_cpzp like cpzp.
data: ls_wip  like cpzp-gmsum.

*SELECT-OPTIONS: SO_H_ID FOR PPC_HEAD-HEADID.
parameters:
   P_OBJNR   like cpzp-OBJNR  ,
   P_F_OBJ   like cpzp-F_OBJNR,
   P_GJPER   like cpzp-GJPER  ,
   P_ZAEHL   like cpzp-ZAEHL  .
PARAMETERS: P_TEST(1) DEFAULT 'X'.
parameters:
   T_GJPER   like cpzp-GJPER  ,
   ISTMN  like cpzp-ISTMN,
   GMPER  like cpzp-GMPER,
   XMPER  like cpzp-XMPER,
   GMSUM  like cpzp-GMSUM,
   XMSUM  like cpzp-XMSUM,
   VARMN  like cpzp-VARMN.

PARAMETERS: P_MOD(1) DEFAULT ' '.

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

  select single * into ls_cpzp
     from cpzp
     where OBJNR   = P_OBJNR
     and   F_OBJNR = P_F_OBJ
     and   GJPER   = P_GJPER
     and   ZAEHL   = P_ZAEHL.

  check sy-subrc = 0.

  write:/ ls_cpzp-OBJNR, ls_cpzp-F_objnr, ls_cpzp-GJPER, ls_cpzp-zaehl.

  ls_wip = LS_CPZP-ISTMN - LS_CPZP-GMSUM.
  write:/ 'ISTMN', ls_cpzp-ISTMN,
        / 'GMPER', ls_cpzp-GMPER,
        / 'XMPER', ls_cpzp-XMPER,
        / 'GMSUM', ls_cpzp-GMSUM,
        / 'XMSUM', ls_cpzp-XMSUM,
        / 'VARMN', ls_cpzp-VARMN,
        / 'WIP  ', ls_WIP.
  uline.

*new value
  if T_GJPER <> P_GJPER and T_GJPER <> ''.
    ls_cpzp-gjper = t_gjper.
  endif.

  ls_cpzp-ISTMN  = ISTMN.
  ls_cpzp-GMPER  = GMPER.
  ls_cpzp-XMPER  = XMPER.
  ls_cpzp-GMSUM  = GMSUM.
  ls_cpzp-XMSUM  = XMSUM.
  ls_cpzp-VARMN  = VARMN.
  ls_wip = LS_CPZP-ISTMN - LS_CPZP-GMSUM.

  write:/ ls_cpzp-OBJNR, ls_cpzp-F_objnr, ls_cpzp-GJPER, ls_cpzp-zaehl.
  write:/ 'ISTMN', ls_cpzp-ISTMN,
        / 'GMPER', ls_cpzp-GMPER,
        / 'XMPER', ls_cpzp-XMPER,
        / 'GMSUM', ls_cpzp-GMSUM,
        / 'XMSUM', ls_cpzp-XMSUM,
        / 'VARMN', ls_cpzp-VARMN,
        / 'WIP  ', ls_WIP.

  IF P_TEST IS INITIAL.
    case P_mod.
    when 'D'.
      delete from cpzp
        where OBJNR   = P_OBJNR
        and   F_OBJNR = P_F_OBJ
        and   GJPER   = P_GJPER
        and   ZAEHL   = P_ZAEHL.
    when 'A'.
        insert cpzp from ls_cpzp.
    when others.
        update cpzp from ls_cpzp.
    endcase.

    COMMIT WORK.
  endif.
