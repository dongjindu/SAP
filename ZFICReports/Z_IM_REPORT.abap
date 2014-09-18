REPORT Z_IM_REPORT message-id zfi.

tables: impr, fmfctr, ripasw.

SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME title c010.
parameters: p_prnam like IMPR-PRNAM   memory id IMT obligatory,
            p_posid like IMPR-POSID   memory id IMP obligatory,
            p_gjahr like IMPR-GJAHR   memory id gjr obligatory.

parameters: p_auth(1) type c default 'X' no-display.
SELECTION-SCREEN END OF BLOCK s1.

SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME title c020.
*arameters:
*_oall as checkbox default 'X',
*           PA_JHR  like BPJA-GJAHR   memory id gjr.
* overall
  SELECTION-SCREEN      BEGIN OF BLOCK JHR.
  SELECTION-SCREEN        BEGIN OF LINE.
*   PARAMETER:            PA_OLDMS LIKE RAIP1-OLDMEAS.
*   SELECTION-SCREEN      COMMENT 3(30) P02
*                         FOR FIELD PA_OLDMS.
*   SELECTION-SCREEN      COMMENT (20) c03 for field pa_jhrwt.
*    SELECTION-SCREEN      POSITION 32.
    PARAMETER:            PA_GESWT LIKE RAIP1-GESWT DEFAULT 'X'
                          RADIOBUTTON GROUP WRT.
    SELECTION-SCREEN      COMMENT (25) P03
                          FOR FIELD PA_GESWT.
  SELECTION-SCREEN        END   OF LINE.
* year
  SELECTION-SCREEN        BEGIN OF LINE.
*   PARAMETER:            PA_FUTMS LIKE RAIP1-FUTMEAS.
*   SELECTION-SCREEN      COMMENT 3(30) P07
*                         FOR FIELD PA_FUTMS.
*   SELECTION-SCREEN      POSITION 32.
    PARAMETER:            PA_JHRWT LIKE RAIP1-JHRWT
                          RADIOBUTTON GROUP WRT.
    SELECTION-SCREEN      COMMENT (25) P04
                          FOR FIELD PA_JHRWT.
    SELECTION-SCREEN      POSITION 33.
    PARAMETER:            PA_JHR   LIKE BPJA-GJAHR.
  SELECTION-SCREEN        END  OF LINE.
  SELECTION-SCREEN      END   OF BLOCK JHR.

  SELECTION-SCREEN        BEGIN OF LINE.
    SELECTION-SCREEN      POSITION 3.
    SELECTION-SCREEN      COMMENT (25) c05
                          FOR FIELD PA_ACTFY.
    SELECTION-SCREEN      POSITION 33.
*from period
  PARAMETERS:             PA_ACTFY LIKE RAIP3-ACTFY,
                          PA_ACTFP LIKE RAIP3-ACTFP.
    SELECTION-SCREEN      POSITION 58.
*to period
  PARAMETERS:             PA_ACTTY LIKE RAIP3-ACTTY,
                          PA_ACTTP LIKE RAIP3-ACTTP.
  SELECTION-SCREEN        END OF LINE.

  SELECTION-SCREEN        SKIP 1.

select-options: SO_IPPOS for RIPASW-ippos.

SELECTION-SCREEN END OF BLOCK s2.

SELECTION-SCREEN BEGIN OF BLOCK s3 WITH FRAME title c030.
parameters: p_plan as checkbox default 'X',
            p_versi  like RAIP3-VERSN memory id BP2 default '000'.
SELECTION-SCREEN END OF BLOCK s3.


DATA: L_BEREC.
DATA: G_fik   like fmfctr-fikrs. " memory id fik obligatory,

* possible entries
DATA: HLP_PROGRAM            LIKE D020S-PROG VALUE 'Z_IM_REPORT',
      HLP_DYNPRO             LIKE sy-dynnr   VALUE '1000'.

include LFMAUEQU.

*---------------------------------------------------------------------*
* Initialization.
*---------------------------------------------------------------------*
Initialization.
  c010 = 'Run Parameter'.
  c020 = 'Select option'.
  c030 = 'Option for plan value '.
  p03  = 'Overall values'.
  p04  = 'Annual values (year)'.
  c05  = '- Act. values from periods'.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VERSI.

   CALL FUNCTION 'AIPA_F4_PLAN_VERSION'
        EXPORTING
             I_PROGRAM           = HLP_PROGRAM
             I_DYNNR             = HLP_DYNPRO
             I_FN_GJAHR_PROPOSAL = 'P_GJAHR'
             I_FN_PRNAM_PROPOSAL = 'P_PRNAM'
             I_FN_VERSN          = 'P_VERSI'
             I_TAKEOVER_ALLOWED  = 'X'.


*---------------------------------------------------------------------
*  S T A R T - O F - S E L E C T I O N
*---------------------------------------------------------------------
START-OF-SELECTION.

select single * from impr
  where gjahr = p_gjahr
    and posid = p_posid.

check sy-subrc = 0.

* user controlling area for auth.check
g_fik = impr-KOKRS.

*CALL FUNCTION 'FMAU_AUTHORITY_FIFM'
*     EXPORTING
*          I_ACTVT       = FMAU_DISPLAY
*          I_AUTH_OBJECT = 'F_FICA_CTR'
*          I_FIKRS       = '*'
*          I_FMFCTR      = fmfctr
*          I_DATE        = SY-DATLO
*     IMPORTING
*          EX_AUTH       = L_BEREC.


AUTHORITY-CHECK OBJECT 'Z_FICTR'
         ID 'FM_FIKRS'   FIELD g_fik
         ID 'FM_FICTR'   FIELD '*'.
if sy-subrc = 0.
  clear: p_auth.
endif.

** full authorization
*if l_berec = 'X'.
*  clear: p_auth.
*endif.

if p_auth = space.
  perform show_report.
else.
  data: l_fictr like FMFCTR-fictr.
*--- check FundCenter Auth.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = impr-VERNR
       IMPORTING
            OUTPUT = l_fictr.

*  select single * from fmfctr
*      where fikrs = p_fik
*        and FICTR = l_fictr
*        and datbis >= sy-datum.
*
*  if sy-subrc <> 0.
*    message i101.
*  else.
*    CALL FUNCTION 'FMAU_AUTHORITY_FIFM'
*         EXPORTING
*              I_ACTVT       = FMAU_DISPLAY
*              I_AUTH_OBJECT = 'F_FICA_CTR'
*              I_FIKRS       = p_FIK
*              I_FMFCTR      = fmfctr
*              I_DATE        = SY-DATLO
*         IMPORTING
*              EX_AUTH       = L_BEREC.
*    IF L_BEREC = SPACE.
*      message i101.
    AUTHORITY-CHECK OBJECT 'Z_FICTR'
             ID 'FM_FIKRS'   FIELD g_fik
             ID 'FM_FICTR'   FIELD l_fictr.
    if sy-subrc <> 0.
      message i101.
    else.
      perform show_report.
    endif.

*  endif.
endif.
*&---------------------------------------------------------------------*
*&      Form  show_report
*&---------------------------------------------------------------------*
FORM show_report.
*  data: l_PA_GESWT type c,
*        l_PA_JHRWT type c.
*
*  if p_oall = 'X'.
*    clear PA_JHR.
*    l_pa_geswt = 'X'.
*  else.
*    if pa_jhr = space.
*      exit.
*    endif.
*
*    l_pa_jhrwt = 'X'.
*  endif.

if p_plan = 'X'.
  SUBMIT RAIMINFO
          WITH PA_PRNAM = p_prnam
          WITH PA_POSID = p_posid
          WITH PA_GJAHR = p_gjahr
          WITH PA_RPLA  = '1'  "ar plan
          WITH PA_PLAN  = '2'  "pi plan
          WITH PA_BUDG  = '3'  "budget(program)
          WITH PA_MPLAN = '4'  "order plan
          WITH PA_MBUDG = '5'  "budget(measure)
          WITH PA_IST   = '6'  "actual
          WITH PA_OBLIG = '7'  "commitment
          WITH PA_VERFB = '8'  "avail
          WITH PA_ANZA  = '9'  "downpay
          WITH PA_POSRM = 'X'  "posi,req,measure
          WITH PA_OLDMS = 'X'  "old also
          WITH PA_GESWT = pa_geswt
          WITH PA_JHRWT = pa_jhrwt
          WITH PA_JHR   = pa_jhr
          WITH PA_ACTFY = pa_actfy
          WITH PA_ACTFP = pa_actfp
          WITH PA_ACTTY = pa_actty
          WITH PA_ACTTP = pa_acttp
          WITH SO_IPPOS in so_ippos
          WITH PA_VERSI = p_versi        "version
            and return.

* budget
else.
  SUBMIT RAIMINFO
          WITH PA_PRNAM = p_prnam
          WITH PA_POSID = p_posid
          WITH PA_GJAHR = p_gjahr
          WITH PA_BUDG  = '1'  "budget(program)
          WITH PA_MBUDG = '2'  "budget(measure)
          WITH PA_IST   = '3'  "actual
          WITH PA_OBLIG = '4'  "commitment
          WITH PA_VERFB = '5'  "avail
          WITH PA_ANZA  = '6'  "downpay
          WITH PA_POSRM = 'X'  "posi,req,measure
          WITH PA_OLDMS = 'X'  "old also
          WITH PA_GESWT = pa_geswt
          WITH PA_JHRWT = pa_jhrwt
          WITH PA_JHR   = pa_jhr
          WITH PA_ACTFY = pa_actfy
          WITH PA_ACTFP = pa_actfp
          WITH PA_ACTTY = pa_actty
          WITH PA_ACTTP = pa_acttp
          WITH SO_IPPOS in so_ippos
*                    WITH PA_JHR ...
*                    WITH PA_ALLVS ...
*                    WITH PA_ALLVT ...
*                    WITH PA_ALLWT ...
*                    WITH PA_BEZPL ...
*                    WITH PA_BTR ...
*                    WITH PA_BUDGV ...
*                    WITH PA_FUTMS ...
*                    WITH PA_IDTXT ...
*                    WITH PA_LEAFS ...
*                    WITH PA_LUSRV ...
*                    WITH PA_MBREL ...
*                    WITH PA_MPLAN ...
*                    WITH PA_MSTAT ...
*                    WITH PA_PLAN ...
*                    WITH PA_PLANV ...
*                    WITH PA_POSIT ...
*                    WITH PA_POSRQ ...
*                    WITH PA_PRF ...
*                    WITH PA_PROZ ...
*                    WITH PA_RBREL ...
*                    WITH PA_RBUDG ...
*                    WITH PA_REMP ...
*                    WITH PA_RIST ...
*                    WITH PA_RPLA ...
*                    WITH PA_RPLAN ...
*                    WITH PA_SELSC ...
*                    WITH PA_SIGN ...
*                    WITH PA_STSEL ...
*                    WITH PA_TXTLG ...
*                    WITH PA_USRVL ...
*                    WITH PA_VALTP ...
*                    WITH PA_VALTY ...
*                    WITH PA_VERF ...
*                    WITH PA_VERSI ...  "version
*                    WITH PA_VERSN ...
*                    WITH PA_WBREL ...
*                    WITH PA_WBUDG ...
*                    WITH PA_WIST ...
*                    WITH PA_WPLAN ...
*                    WITH PA_WPROF ...
*                    WITH SO_ALLGJ ...
  and return.
endif.
ENDFORM.                    " show_report
