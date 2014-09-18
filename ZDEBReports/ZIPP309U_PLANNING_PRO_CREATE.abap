REPORT ZIPP309U_PLANNING_PRO_CREATE.

*----------------------------------------------------------------------*
*  Date         Developer     Request        Description
* 06/15/2007    Manju         ED1K900010     Initial coding - Program
*                                            to create & maintain
*                                            planning profile
*----------------------------------------------------------------------*


TABLES : ZTBM_ABYCFIDT,TPHVP,TPLVP,
         CUVTAB,CUVTAB_VALC.

TYPE-POOLS: m60vt.


DATA: BEGIN OF WA_OPT OCCURS 0.
        INCLUDE STRUCTURE CTU_PARAMS.
DATA: END OF WA_OPT.
DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.
DATA: BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESS.

DATA: IT_VAL  LIKE TABLE OF RM60DRL WITH HEADER LINE,
      l_len type i.

data:  begin of it_char_val occurs 0,
        TEXT like ZTBM_FSC_CRE_INF-text,
        VALU1 like ZTBM_FSC_CRE_INF-VALU1,
       end of it_char_val.

data : begin of it_TPSVP occurs 0,
        PROFILID like TPLVP-PROFILID,
        PHCOUNTER like TPLVP-PHCOUNTER,
        CLINT like TPLVP-CLINT,
        LNPOS like TPSVP-LNPOS,
       end of it_TPSVP.


data :  l_opt1 like ZTBM_ABYCFIDT-OPT1,
        l_index type i,
        l_8176(2) type c value '-',
        l_8233(2) type c value '-',
        L_8631(2)  type c value '-',
        L_863C(2) type c value '-',
        l_8652(2) type c value '-',
        l_8811(2) type c value '-'.

DATA: L_COLOREXT LIKE RCTMS-MWERT,
      L_COLORINT LIKE RCTMS-MWERT,
      l_rcnt type i,
      l_rcre type i,
      l_rex type i,
      l_rerr type i.



DATA: IT_ACFI TYPE ZTBM_ABYCFIDT OCCURS 0 WITH HEADER LINE.


select-options : s_date for ZTBM_ABYCFIDT-ZBDAT default sy-datum
obligatory.


*M60V_PROFIL_FOR_PLAN


start-of-selection.

  perform initialization.

* Read Configuratrion Profile data from ZTBM_ABYCFIDT TABLE
  perform read_data.

* Process Data - * Create / Maintain Planning Profile
  perform process_data.


  Perform write_summary.


end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.

  SELECT *
         FROM ZTBM_ABYCFIDT
         INTO TABLE IT_ACFI
         WHERE ZEDAT in  s_DATe .
  IF SY-SUBRC NE 0.
    WRITE: / TEXT-019.
    exit.
  ENDIF.
  l_rcnt  = sy-dbcnt.
ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  data: lw_objekt LIKE inob-objek,
        VT_VALUES TYPE m60vt_profil,
        COUNTER type i.



** Select planning profile relavant charactertics for CR & EM
  select  text valu1 into table it_char_val
           from ZTBM_FSC_CRE_INF
          where ( ITEM like 'CR_PP%' or
                  ITEM like 'EM_PP%'  ) .





  loop at IT_ACFI.

    move  IT_ACFI-mtno to lw_objekt.
uline.
* Check Whether Planning profile exists or not
    CALL FUNCTION 'M60V_PROFIL_FOR_PLAN'
         EXPORTING
              OBJEKT      = lw_objekt
              BUFFER_FREE = 'X'
              KEY_DATE    = SY-DATUM
              I_PL_REL    = ''
         IMPORTING
              EXP_VALUE   = VT_VALUES
         EXCEPTIONS
              NOT_FOUND   = 1
              OTHERS      = 2.
    IF SY-SUBRC <> 0.
    ENDIF.
    DESCRIBE TABLE VT_VALUES-GROUP LINES COUNTER.
    IF not  COUNTER IS INITIAL.
* If planning profile is already maintained then skip such record
      l_rex = l_rex + 1.
      write :/ 'Planning profile already exists for',IT_ACFI-MTNO .
      continue.                      "raising no_relev_char_val.
    ENDIF.

* Create / Maintain Planning Profile
    Perform call_BDC_MDPH.

  endloop.

ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  call_BDC_MDPH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_BDC_MDPH.

  data : l_cnt(2)  type n,
         l_field(30) type c,
         l_field1(30) type c,
         L_MSG  LIKE CFGNL-MSGLIN,
         l_num(3) type n.


* Create Planning Profile
  .
** Initial Screen MDPH
  PERFORM DYNPRO USING:
        'X' 'SAPMM60X'    '0510',
        ' ' 'BDC_CURSOR' 'RM60B-MATNR',
        ' ' 'RM60B-MATNR' IT_ACFI-MTNO,
        ' ' 'BDC_OKCODE'  '=CHAN'.


* Assign Profile
  PERFORM DYNPRO USING:
        'X' 'SAPLM60P'    '0105',
        ''  'BDC_CURSOR' 'RM60PROF-PROFTXT(01)',
        ' ' 'RM60PROF-PROFTXT(01)' IT_ACFI-PROF,
        ' ' 'BDC_CURSOR' 'RM60PROF-VISUD(01)',
        ''  'RM60PROF-VISUD(01)' 'X',
        ''  'RM60PROF-SELKZ(01)' 'X',
        ' ' 'BDC_OKCODE'  '=DATA'.
*
** New Entries
  PERFORM DYNPRO USING:
        'X' 'SAPLM60P'    '0110',
        ' ' 'BDC_CURSOR' 'RM60B-MATNR',
        ' ' 'BDC_OKCODE'  '=NEWL'.

** Assign charactertcis based on profile
** from ZTBM_FSC_CRE_INF table
  l_cnt = 0.
  loop at it_char_val where TEXT eq IT_ACFI-CLID .

    l_cnt =  1.
    CONCATENATE 'RM60REL-ATWTB(' l_cnt ')' INTO l_field.
*   CONCATENATE 'RM60REL-ATWTB(' l_cnt ')' INTO l_field1.

    PERFORM DYNPRO USING:
         'X' 'SAPLM60P'    '0110',
          ' ' 'BDC_CURSOR' l_field,
          ''  l_field  it_char_val-VALU1,
          ''  'BDC_OKCODE'  '=BACK',
         'X' 'SAPLM60P'    '0110',
         ''  'BDC_CURSOR' 'RM60B-MATNR',
         ''  'BDC_OKCODE' '=NEWL'.
  endloop.
*
** Save
  PERFORM DYNPRO USING:
       'X' 'SAPLM60P'    '0110',
        ''  'BDC_CURSOR' 'RM60B-MATNR',
        ' ' 'BDC_OKCODE'  '=SICH'.

* Create Planning Profile
  CALL TRANSACTION 'MDPH'
                          USING IT_BDC
                          OPTIONS FROM WA_OPT
                          MESSAGES INTO IT_MESS.

  write :/ 'Creation of  Planning profile for',IT_ACFI-MTNO .

  loop at it_mess.

    check it_mess-MSGNR ne '352' .

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              MSGID               = IT_MESS-MSGID
              MSGNR               = it_mess-MSGNR
              MSGV1               = IT_MESS-MSGV1
              MSGV2               = IT_MESS-MSGV2
              MSGV3               = IT_MESS-MSGV3
              MSGV4               = IT_MESS-MSGV4
         IMPORTING
              MESSAGE_TEXT_OUTPUT = l_msg.


    write :/ l_msg.
  endloop.

  if sy-subrc eq 0.
    clear it_BDC[].

    clear : l_opt1 , l_index , l_8176,
        l_8233 ,
        L_8631 ,
        L_863C ,
        l_8652 ,
        l_8811 ,
        L_COLOREXT ,
        L_COLORINT .



* Get 8176 / 8631 / 8233 / 863C / 86752 Values
    perform slice_dice_OPT1.

* Read     EXT & INT COLOR
*    PERFORM READ_MARA_COLOR_EXT_INT USING    IT_ACFI-MTNO+5(2)
*                                    CHANGING L_COLOREXT
*                                             L_COLORINT.

* Maintain Profile charactertics Values
    Perform maintain_profile.
  else.

    l_rerr = l_rerr + 1.
  endif.
uline.
ENDFORM.                    " call_BDC_MDPH
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0123   text
*      -->P_0124   text
*      -->P_0125   text
*----------------------------------------------------------------------*
FORM DYNPRO USING  DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR IT_BDC.
    MOVE: NAME TO IT_BDC-PROGRAM,
          VALUE TO IT_BDC-DYNPRO,
          DYNBEGIN TO IT_BDC-DYNBEGIN.
    APPEND IT_BDC.
  ELSE.
    CLEAR IT_BDC.
    MOVE: NAME TO IT_BDC-FNAM,
          VALUE TO IT_BDC-FVAL.
    APPEND IT_BDC.
  ENDIF.
  .

ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  maintain_profile
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM maintain_profile.

  data : l_CHAR like CUVTAB-VTNAM.

* Select Planning profile Data Header Data

  select single * from TPHVP  where OBJEKT  =  IT_ACFI-MTNO  and
                                    PROFTXT =  IT_ACFI-PROF.

* Select Planning profile  charactertics values and Line
* counters
  select  PROFILID
          PHCOUNTER
          CLINT  LNPOS into table it_TPSVP
          from TPSVP
               where PROFILID = TPHVP-PROFILID.


* Select Relavant charactetics values and De-select which are not
* relavant values in planning profile.

  loop at it_tpSvp.
* Get charactertics Name
    select  single * from CUVTAB where VTINT = it_tpSvp-CLINT.
    l_CHAR = CUVTAB-VTNAM.
    l_len  = strlen( l_CHAR ).
    l_len = l_len - 3.


    if l_CHAR(2) = 'EM'.
      perform EM_select_char_values.
    elseif l_CHAR(2) = 'CR'..
      perform cr_select_char_values.
    endif.

  endloop.

  write :/ 'Maintain Planning profile charactertics for',IT_ACFI-MTNO .

* Select / De - Select charactertics values of planning profile
  CALL FUNCTION 'REQUIREMENTS_MAINT_RELE_BACK'
       EXPORTING
            I_MATNR                   = IT_ACFI-MTNo
            I_PROFTXT                 = IT_ACFI-PROF
            I_VISUD                   = TPHVP-VISUD
            I_DATUM                   = sy-datum
            I_PROFILID                = TPHVP-PROFILID
       TABLES
            I_VALUES                  = it_VAL
       EXCEPTIONS
            MATERIAL_NOT_FOUND        = 1
            MATERIAL_NOT_CONFIGURABLE = 2
            NO_RELEV_CHAR_VAL         = 3
            MATERIAL_BLOCKED          = 4
            SYSTEM_ERROR              = 5
            PROFILE_NOT_FOUND         = 6
            NO_UPDATE_POSSIBLE        = 7
            DOUBLE_CHARACTERISTIC     = 8
            OTHERS                    = 9.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  else.
   write :/ 'Maintainence of Planning profile sucessfull',IT_ACFI-MTNO .
    l_rcre = l_rcre + 1.
  ENDIF.
  clear it_val[].

ENDFORM.                    " maintain_profile
*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.

  REFRESH: IT_BDC, IT_MESS.
  CLEAR:   IT_BDC, IT_MESS.
  WA_OPT-DEFSIZE = 'X'.
  WA_OPT-DISMODE = 'N'.
  WA_OPT-UPDMODE = 'S'.


ENDFORM.                    " initialization
*&---------------------------------------------------------------------*
*&      Form  set_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ACFI_DRTY  text
*----------------------------------------------------------------------*
FORM set_value USING    P_VAL.
  select single * from CUVTAB_VALC where vtint = it_tpSvp-CLINT and
*                                 ATINN =  CUVTAB-VTNAM+3(l_len) and
                                  SLNID = it_tpSvp-LNPOS  and
                                  VALC  = P_VAL.
  if sy-subrc ne 0.
* De - Select  values which are not relavant
    it_val-CLINT  = it_tpSvp-CLINT.
    it_val-LNPOS  = it_tpSvp-LNPOS.
    it_val-PL_REL = ''.
    it_val-ATERF  = ''.
    it_val-UPDKZ  = '' .
    append it_val.
  else.
* Select Values
    it_val-CLINT  = it_tpSvp-CLINT.
    it_val-LNPOS  = it_tpSvp-LNPOS.
    it_val-PL_REL = 'X'.
    it_val-ATERF  = ''.
    it_val-UPDKZ  = '' .
    append it_val.
  endif.

ENDFORM.                    " set_value
*&---------------------------------------------------------------------*
*&      Form  cr_select_char_values
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cr_select_char_values.
  case CUVTAB-VTNAM.
    when 'CR_COLOREXT'.
*      perform set_value using L_COLOREXT.
    WHEN 'CR_COLORINT'.
*      perform set_value using L_COLORINT.
    when 'CR_COL_CAR_SPEC'.
      perform set_value using IT_ACFI-CSPEC.
    when 'CR_COL_DT'.
      perform set_value using IT_ACFI-DRTY.
    when 'CR_COL_WT'.
      perform set_value using IT_ACFI-WETY.
    when 'CR_COL_BT'.
      perform set_value using IT_ACFI-BOTY.
    when 'CR_COL_EC'.
      perform set_value using IT_ACFI-ENCAPA.
    when 'CR_COL_ET'.
      perform set_value using IT_ACFI-ENTY.
    when 'CR_COL_FT'.
      perform set_value using IT_ACFI-FUTY.
    when 'CR_COL_TM'.
      perform set_value using IT_ACFI-TMCD.
    when 'CR_COL_SP'.
      perform set_value using '-'.
    when 'CR_COL_NAT'.
      perform set_value using IT_ACFI-NATION.

    when 'CR_COL_GR'.
      perform set_value using IT_ACFI-GRAD.

    when 'CR_COL_8176'.
      perform set_value using l_8176.

    when 'CR_COL_8631'.
      perform set_value using L_8631.

    when 'CR_COL_8233'.
      perform set_value using l_8233.

    when 'CR_COL_863C'.
      perform set_value using l_863C.

    when 'CR_COL_8652'.
      perform set_value using l_8652.

    when 'CR_COL_8811'.
      perform set_value using l_8811.

  endcase.
ENDFORM.                    " cr_select_char_values
*&---------------------------------------------------------------------*
*&      Form  EM_select_char_values
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EM_select_char_values.

  case CUVTAB-VTNAM.
    when 'EM_COLOREXT'.
*      perform set_value using L_COLOREXT.
    WHEN 'EM_COLORINT'.
*      perform set_value using L_COLOREXT.
    when 'EM_COL_CAR_SPEC'.
      perform set_value using  IT_ACFI-CSPEC.
    when 'EM_COL_DT'.
      perform set_value using IT_ACFI-DRTY.
    when 'EM_COL_WT'.
      perform set_value using  IT_ACFI-WETY.
    when 'EM_COL_BT'.
      perform set_value using   IT_ACFI-BOTY.
    when 'EM_COL_EC'.
      perform set_value using  IT_ACFI-ENCAPA.
    when 'EM_COL_ET'.
      perform set_value using IT_ACFI-ENTY.
    when 'EM_COL_FT'.
      perform set_value using IT_ACFI-FUTY.
    when 'EM_COL_TM'.
      perform set_value using IT_ACFI-TMCD.
    when 'EM_COL_SP'.
      perform set_value using '-'.
    when 'EM_COL_NAT'.
      perform set_value using  IT_ACFI-NATION.

    when 'EM_COL_GR'.
      perform set_value using IT_ACFI-GRAD.

    when 'EM_COL_8176'.
      perform set_value using l_8176.

    when 'EM_COL_8631'.
      perform set_value using L_8631.

    when 'EM_COL_8233'.
      perform set_value using l_8233.

    when 'EM_COL_863C'.
      perform set_value using l_863C.

    when 'EM_COL_8652'.
      perform set_value using l_8652.

    when 'EM_COL_8811'.
      perform set_value using l_8811.

  endcase.

ENDFORM.                    " EM_select_char_values
*&---------------------------------------------------------------------*
*&      Form  slice_dice_OPT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM slice_dice_OPT1.

* OPT1 CODE
  l_opt1 = it_acFI-opt1.
  do 1 times.
    clear l_index.
    search l_opt1 for '8176' and MARK.
    if sy-subrc eq 0.
      l_index = SY-FDPOS + 4.
      l_8176 = l_opt1+l_index(2).
    endif.
    search l_opt1 for '8233' and MARK.
    if sy-subrc eq 0.
      l_index = SY-FDPOS + 4.
      l_8233 = l_opt1+l_index(2).
    endif.
    search l_opt1 for '8631' and MARK.
    if sy-subrc eq 0.
      l_index = SY-FDPOS + 4.
      l_8631 = l_opt1+l_index(2).
    endif.
    search l_opt1 for '863C' and MARK.
    if sy-subrc eq 0.
      l_index = SY-FDPOS + 4.
      l_863C = l_opt1+l_index(2).
    endif.
    search l_opt1 for '8652' and MARK.
    if sy-subrc eq 0.
      l_index = SY-FDPOS + 4.
      l_8652 = l_opt1+l_index(2).
    endif.
    search l_opt1 for '8811' and MARK.
    if sy-subrc eq 0.
      l_index = SY-FDPOS + 4.
      l_8811 = l_opt1+l_index(2).
    endif.
    exit.
  enddo.

  if l_8176 is initial.
    l_8176 = '-'.
  endif.
  if l_8233 is initial.
    l_8233 = '-'.
  endif.
  if l_8631 is initial.
    l_8631 = '-'.
  endif.
  if l_863C is initial.
    l_863C = '-'.
  endif.
  if l_8652 is initial.
    l_8652 = '-'.
  endif.
  if l_8811 is initial.
    l_8811 = '-'.
  endif.


ENDFORM.                    " slice_dice_OPT1
*&---------------------------------------------------------------------*
*&      Form  READ_MARA_COLOR_EXT_INT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ACFI_MTNO+5(2)  text
*      <--P_L_COLOREXT  text
*      <--P_L_COLORINT  text
*----------------------------------------------------------------------*
FORM READ_MARA_COLOR_EXT_INT USING    P_MATNR
                             CHANGING P_COLOREXT
                                      P_COLORINT.
  DATA: L_FERTH LIKE MARA-FERTH,
        L_NORMT LIKE MARA-NORMT.
  SELECT SINGLE FERTH
                NORMT
              FROM MARA
              INTO (L_FERTH, L_NORMT)
              WHERE MATNR EQ P_MATNR.
  IF SY-SUBRC EQ 0.
    P_COLOREXT = L_FERTH.
    P_COLORINT = L_NORMT.
  ELSE.
    CLEAR : P_COLOREXT, P_COLORINT.
  ENDIF.

ENDFORM.                    " READ_MARA_COLOR_EXT_INT
*&---------------------------------------------------------------------*
*&      Form  write_summary
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_summary.

  uline.
  Write :/ 'No of Records selected from Z TABLE', l_rcnt.
  Write :/ 'No of Planning profiles created ', l_rcre.
  Write :/ 'No of Planning profiles  already exists', l_rex.
  Write :/ 'No of Planning profiles errered out', l_rerr.
  uline.

ENDFORM.                    " write_summary
