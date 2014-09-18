REPORT Z_PPC_WIP_CORRECT_1 LINE-SIZE 300.
TABLES: CKMLHD, AUFK,
        PPC_HEAD, PPC_CONF_ACT_VAR,PPC_ORD_INF,QRP002,
        PPC_ACT.
DATA:

*
LF_AUFNR TYPE AUFNR,
LF_PKOSA_PLANT TYPE WERKS_D,
LF_OBJNR TYPE J_OBJNR,
LF_F_OBJNR TYPE F_OBJNR,
LF_PKOSA_ERROR TYPE C,
LF_KOKRS TYPE KOKRS,
LF_GJPER_CURR TYPE CO_GJPER,
LF_GJPER_PREV TYPE CO_GJPER,
LF_ZAEHL TYPE COUNT_ZP,
LS_CPZP LIKE CPZP.


*SELECT-OPTIONS: SO_H_ID FOR PPC_HEAD-HEADID.
parameters: p_aufnr like aufk-aufnr,
            p_GJPER TYPE CO_GJPER,
            t_GJPER TYPE CO_GJPER.

PARAMETERS: P_TEST(1) DEFAULT 'X'.
PARAMETERS: P_MOD(1) DEFAULT ' '.

data: lt_cpzp like cpzp occurs 0 with header line.
data: ln_cpzp like cpzp occurs 0 with header line.

data: f_objnr like cpzp-f_objnr,
      gjper like cpzp-gjper,
      we_gmen like cpzp-gmper,
      zp_gmen like cpzp-gmper,
      zp_istmn like cpzp-istmn,
      l_linnr type i.

data: L_WIP like cpzp-XMSUM.

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

  refresh: lt_cpzp, ln_cpzp.
* get objno
  select single objnr into lf_objnr from aufk
    where aufnr = p_aufnr.

  SELECT * FROM CPZP INTO table LT_CPZP
           WHERE OBJNR EQ LF_OBJNR
             AND GJPER EQ P_GJPER.


  WRITE: /1 LS_CPZP-OBJNR.
  loop at lt_cpzp.
    ls_cpzp = lt_cpzp.
    L_WIP = LS_CPZP-ISTMN - LS_CPZP-GMSUM.

    check l_wip <> 0.

    WRITE: /1 LS_CPZP-F_OBJNR, LS_CPZP-GJPER, LS_CPZP-ZAEHL,
           'WIP  ',  L_WIP color COL_TOTAL,
           'ISTMN',  LS_CPZP-ISTMN,
           'GMPER',  LS_CPZP-GMPER,
           'GMSUM',  LS_CPZP-GMSUM.

    clear: ls_cpzp-ISTMN,
           ls_cpzp-GMPER,
           ls_cpzp-XMPER,
           ls_cpzp-GMSUM,
           ls_cpzp-XMSUM,
           ls_cpzp-VARMN.

    LS_CPZP-GJPER = t_gjper.
    LS_CPZP-GMPER = - L_WIP.
    append ls_cpzp to ln_cpzp.
  endloop.

  IF P_TEST IS INITIAL.
    if p_mod = 'D'.
      delete from cpzp
        where OBJNR   = LF_OBJNR
        and   GJPER   = T_GJPER.
    else.
      insert cpzp from table ln_cpzp.
*    COMMIT WORK.
    endif.
    write:/ 'Data changed'.
  endif.
