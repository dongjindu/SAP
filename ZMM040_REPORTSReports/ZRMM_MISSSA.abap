REPORT ZRMM_MISSSA.
*&--------------------------------------------------------------------&*
*&  Program id   : ZRMM_MISSSA
*&  Developer    : Furong
*&  Description  : List all materials with/without SA
*&
*&--------------------------------------------------------------------&*
*& Date        Transport        Description
*& 01/21/2005  UD1K913936       initial program.
*& 02/07/2005  UD1K914237       User request to add in a X-plant Status
*&--------------------------------------------------------------------&*
tables: a018.
data: begin of wa_mara,
        matnr like mara-matnr,
        maktx like makt-maktx,
        mtart like mara-mtart,
        profl like mara-profl,
        ekgrp like marc-ekgrp,
        dispo like marc-dispo,
        mstae like mara-mstae,
        mmsta like marc-mmsta,
      end of wa_mara.
data: begin of wa_ekpo,
         matnr like ekpo-matnr,
         lifnr like ekko-lifnr,
         ebeln like ekpo-ebeln,
         kdate like ekko-kdate,
         bsart like ekko-bsart,
         loekz like ekpo-loekz,
         elikz like ekpo-elikz,
         infnr like eina-infnr,
         prdat like eine-prdat,
         werks like ekpo-werks,
      end of wa_ekpo.
data: begin of wa_sa,
        matnr like mara-matnr,
        maktx like makt-maktx,           " Desc
        mtart like mara-mtart,           " Matl group
*        werks like marc-werks,
        profl like mara-profl,           " lp/kd/mip
        ekgrp like eine-ekgrp,           " pur group
        dispo like marc-dispo,
        mstae like mara-mstae,
        mmsta like marc-mmsta,
        lifnr like eina-lifnr,           " vendor
        ebeln like ekpo-ebeln,
*        eindt like eket-eindt,
        kdate like ekko-kdate,
        bsart like ekko-bsart,
        loekz like ekpo-loekz,
        elikz like ekpo-elikz,
        infnr like eina-infnr,
        prdat like eine-prdat,
        source like eord-flifn,
      end of wa_sa.

data: begin of it_info occurs 0,
        matnr like mara-matnr,
        lifnr like ekko-lifnr,
        infnr like eina-infnr,
        prdat like eine-prdat,
      end of it_info.

data: begin of it_eord occurs 0,
        matnr like mara-matnr,
        lifnr like ekko-lifnr,
        ebeln like ekko-ebeln,
      end of it_eord.

data: it_mara like table of wa_mara,
      it_ekpo like table of wa_ekpo,
      it_sa like table of wa_sa.

data: w_lifnr_low like ekko-lifnr,
      w_lifnr_high like ekko-lifnr.

data: mmatnr like mara-matnr.

data: m_nlines type i,
      m_prdat like eine-prdat.

tables: ekko,eina,marc,ekpo.

DATA: OK_CODE LIKE SY-UCOMM,
      GS_VARIANT TYPE DISVARIANT,
      G_CONTAINER TYPE SCRFNAME VALUE 'CC_CONTAINER',
      GRID1  TYPE REF TO CL_GUI_ALV_GRID,
      G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
DATA: ROW_TABLE TYPE LVC_T_ROW.

selection-screen begin of block block1 with frame title text-002.
select-options: s_matnr for wa_sa-matnr,
                s_mtart for wa_sa-mtart default 'ROH',
                s_profl for wa_sa-profl Default 'V',
                s_werks for marc-werks default 'P001',
                s_ekgrp for wa_sa-ekgrp,
                s_dispo for wa_sa-dispo,
                s_mstae for wa_sa-mstae,
                s_mmsta for wa_sa-mmsta.
selection-screen end of block block1.

selection-screen begin of block block2 with frame title text-003.
select-options: s_bsart for wa_sa-bsart.
select-options: s_lifnr for wa_sa-lifnr.
select-options: s_loekz for wa_sa-loekz,
            s_elikz for wa_sa-elikz.
selection-screen skip 1.
parameters: p_insa  radiobutton group rgrp,
            p_nosa  radiobutton group rgrp,
            p_all   radiobutton group rgrp default 'X'.
selection-screen end of block block2.

INITIALIZATION.
  MOVE: 'I'      TO S_LOEKZ-SIGN,
        'NE'     TO S_LOEKZ-OPTION,
        'L'      TO S_LOEKZ-LOW.
  APPEND S_LOEKZ.

  MOVE: 'I'      TO S_ELIKZ-SIGN,
        'NE'     TO S_ELIKZ-OPTION,
        'X'      TO S_ELIKZ-LOW.
  APPEND S_ELIKZ.

start-of-selection.

perform process_data.

check m_nlines ne 0.

CALL SCREEN 100.

end-of-selection.

*******************************
* Forms
*******************************
form process_data.
  data: w_vendorblank(1).
select a~matnr maktx mtart profl ekgrp dispo mstae mmsta
       into table it_mara
       from mara as a
       inner join makt as b on a~matnr = b~matnr
       inner join marc as c on a~matnr = c~matnr
       where a~matnr in s_matnr
             and   mtart in s_mtart
             and   dispo in s_dispo
             and   profl in s_profl
             and   ekgrp in s_ekgrp
             and   werks = 'P001'
             and   a~mstae in s_mstae
             and   c~mmsta in s_mmsta
             and   a~lvorm eq space
             and   spras eq sy-langu.

describe table it_mara lines m_nlines.
if m_nlines = 0 .
   message id 'ZMMM' type 'I' number '009' with text-001.
   exit.
endif.

select e~matnr d~lifnr d~ebeln d~kdate
                   d~bsart e~loekz e~elikz e~werks
                                into corresponding fields
                                of table it_ekpo
                                from ekko as d
                                inner join ekpo as e
                                on d~ebeln = e~ebeln
                                where e~matnr in s_matnr
                                and d~bsart in s_bsart
*                                and d~lifnr in s_lifnr
                                and e~loekz in s_loekz
                               and e~elikz in s_elikz.
if s_lifnr-low is initial.
   if s_lifnr-high is initial.
      clear w_lifnr_low.
      w_lifnr_high = 'ZZZZ'.
      w_vendorblank = 'Y'.
   else.
      clear w_lifnr_low.
      w_lifnr_high = s_lifnr-high.
   endif.
else.
   if s_lifnr-high is initial.
      w_lifnr_low = s_lifnr-low.
      w_lifnr_high = w_lifnr_low.
   else.
      w_lifnr_low = s_lifnr-low.
      w_lifnr_high = s_lifnr-high.
   endif.
endif.

loop at it_mara into wa_mara.
   select * from eina
       where matnr = wa_mara-matnr
             and loekz = ' '.
     if sy-subrc = 0.
        select single prdat into m_prdat from eine
                     where infnr = eina-infnr.
        if sy-subrc = 0.
           if ( eina-lifnr >= w_lifnr_low ) and
              ( eina-lifnr <= w_lifnr_high ).
              if m_prdat < sy-datum.
                 SELECT SINGLE * FROM a018 WHERE kappl =  'M'
                                        AND kschl =  'PB00'
                                        AND matnr =  wa_mara-matnr
                                        AND lifnr =  eina-lifnr
                                        AND ekorg =  'PU01'
                                        AND esokz =  '0'
                                        AND datab <= sy-datum
                                        AND datbi >= sy-datum.
                 if sy-subrc = 0.
                    it_info-prdat = a018-datbi.
                    it_info-matnr = wa_mara-matnr.
                    it_info-lifnr = eina-lifnr.
                    it_info-infnr = eina-infnr.
                 else.
                    it_info-prdat = m_prdat.
                    it_info-matnr = wa_mara-matnr.
                    it_info-lifnr = eina-lifnr.
                    it_info-infnr = eina-infnr.
                 endif.
              else.
                it_info-prdat = m_prdat.
                it_info-matnr = wa_mara-matnr.
                it_info-lifnr = eina-lifnr.
                it_info-infnr = eina-infnr.
              endif.
              append it_info.
           endif.
        endif.
        clear it_info.
     endif.
   endselect.
endloop.

sort it_info by matnr lifnr.

if p_all = 'X'.
  loop at it_mara into wa_mara.
    loop at it_ekpo into wa_ekpo
                    where matnr = wa_mara-matnr.
         move-corresponding: wa_mara to wa_sa,
                            wa_ekpo to wa_sa.
         read table it_info with key  matnr = wa_ekpo-matnr
                            lifnr = wa_ekpo-lifnr.
         if sy-subrc eq 0.
            wa_sa-infnr = it_info-infnr.
            wa_sa-prdat = it_info-prdat.
            append wa_sa to it_sa.
            clear wa_sa.
         else.
            if w_vendorblank = 'Y'.
               append wa_sa to it_sa.
               clear wa_sa.
            endif.
         endif.
    endloop.
  endloop.
  loop at it_mara into wa_mara.
    read table it_ekpo into wa_ekpo with key matnr = wa_mara-matnr.
    if sy-subrc ne 0.
       move-corresponding: wa_mara to wa_sa.
       read table it_info with key  matnr = wa_sa-matnr.
       if sy-subrc = 0.
          if ( it_info-lifnr >= w_lifnr_low ) and
             ( it_info-lifnr <= w_lifnr_high ).
             wa_sa-infnr = it_info-infnr.
             wa_sa-prdat = it_info-prdat.
             wa_sa-lifnr = it_info-lifnr.
             append wa_sa to it_sa.
          else.
             if w_vendorblank = 'Y'.
                append wa_sa to it_sa.
             endif.
          endif.
       else.
         if w_vendorblank = 'Y'.
            append wa_sa to it_sa.
         endif.
       endif.
       clear wa_sa.
*       LOOP AT IT_INFO WHERE matnr = wa_sa-matnr.
*            wa_sa-infnr = it_info-infnr.
*            wa_sa-prdat = it_info-prdat.
*            wa_sa-lifnr = it_info-lifnr.
*            append wa_sa to it_sa.
*        ENDLOOP.
*        if sy-subrc ne 0.
*           append wa_sa to it_sa.
*        endif.
    endif.
  endloop.
  clear w_vendorblank.
  select matnr lifnr ebeln into corresponding fields of table it_eord
       from eord for all entries in it_sa
       where matnr = it_sa-matnr and lifnr = it_sa-lifnr
       and ebeln = it_sa-ebeln.
   loop at it_sa into wa_sa.
     if wa_sa-ebeln <> '  '.
        read table it_eord with key matnr = wa_sa-matnr
                                    lifnr = wa_sa-lifnr
                                    ebeln = wa_sa-ebeln.
        if sy-subrc = 0.
           wa_sa-source = 'X'.
           modify it_sa from wa_sa.
        endif.
     endif.
     clear wa_sa.
   endloop.
endif.

if p_insa = 'X'.
*   loop at it_mara into wa_mara.
*     loop at it_ekpo into wa_ekpo
*                     where matnr = wa_mara-matnr.
*          move-corresponding: wa_mara to wa_sa,
*                              wa_ekpo to wa_sa.
*          LOOP AT IT_INFO WHERE matnr = wa_ekpo-matnr
*                          AND lifnr = wa_ekpo-lifnr.
*             wa_sa-infnr = it_info-infnr.
*             wa_sa-prdat = it_info-prdat.
*            append wa_sa to it_sa.
*          ENDLOOP.
*          if sy-subrc ne 0.
*             append wa_sa to it_sa.
*          endif.
*          clear wa_sa.
*     endloop.
*  endloop.
  loop at it_mara into wa_mara.
    loop at it_ekpo into wa_ekpo
                    where matnr = wa_mara-matnr.
         move-corresponding: wa_mara to wa_sa,
                            wa_ekpo to wa_sa.
         read table it_info with key  matnr = wa_ekpo-matnr
                            lifnr = wa_ekpo-lifnr.
         if sy-subrc eq 0.
            wa_sa-infnr = it_info-infnr.
            wa_sa-prdat = it_info-prdat.
            append wa_sa to it_sa.
            clear wa_sa.
         else.
            if w_vendorblank = 'Y'.
               append wa_sa to it_sa.
               clear wa_sa.
            endif.
         endif.
    endloop.
  endloop.
  select matnr lifnr ebeln into corresponding fields of table it_eord
         from eord for all entries in it_sa
         where matnr = it_sa-matnr and lifnr = it_sa-lifnr
         and ebeln = it_sa-ebeln.
  loop at it_sa into wa_sa.
    if wa_sa-ebeln <> '  '.
       read table it_eord with key matnr = wa_sa-matnr
            lifnr = wa_sa-lifnr
            ebeln = wa_sa-ebeln.
       if sy-subrc = 0.
           wa_sa-source = 'X'.
           modify it_sa from wa_sa.
       endif.
    endif.
    clear wa_sa.
  endloop.
endif.

if p_nosa = 'X'.
*  loop at it_mara into wa_mara.
*      read table it_ekpo into wa_ekpo with key matnr = wa_mara-matnr.
*      if sy-subrc ne 0.
*         move-corresponding: wa_mara to wa_sa.
*         LOOP AT IT_INFO WHERE matnr = wa_sa-matnr.
*              wa_sa-infnr = it_info-infnr.
*              wa_sa-prdat = it_info-prdat.
*              wa_sa-lifnr = it_info-lifnr.
*              append wa_sa to it_sa.
*          ENDLOOP.
*          if sy-subrc ne 0.
*             append wa_sa to it_sa.
*          endif.
*          clear wa_sa.
*      endif.
*   endloop.
loop at it_mara into wa_mara.
    read table it_ekpo into wa_ekpo with key matnr = wa_mara-matnr.
    if sy-subrc ne 0.
       move-corresponding: wa_mara to wa_sa.
       read table it_info with key  matnr = wa_sa-matnr.
       if sy-subrc = 0.
          if ( it_info-lifnr >= w_lifnr_low ) and
             ( it_info-lifnr <= w_lifnr_high ).
             wa_sa-infnr = it_info-infnr.
             wa_sa-prdat = it_info-prdat.
             wa_sa-lifnr = it_info-lifnr.
             append wa_sa to it_sa.
          else.
             if w_vendorblank = 'Y'.
                append wa_sa to it_sa.
             endif.
          endif.
       else.
         if w_vendorblank = 'Y'.
            append wa_sa to it_sa.
         endif.
       endif.
       clear wa_sa.
    endif.
  endloop.
endif.
endform.

*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*
MODULE PBO OUTPUT.
  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'T_100'.
  GS_VARIANT-REPORT = SY-REPID.
  IF G_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONTAINER
           EXPORTING CONTAINER_NAME = G_CONTAINER.
    CREATE OBJECT GRID1
           EXPORTING I_PARENT = G_CUSTOM_CONTAINER.
    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME = 'ZMMSA'
                    I_SAVE                   = 'U'
                    IS_VARIANT               = GS_VARIANT
         CHANGING  IT_OUTTAB        = IT_SA.
  ELSE.
    CALL METHOD grid1->refresh_table_display.
  ENDIF.
ENDMODULE.
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
MODULE PAI INPUT.
*   to react on oi_custom_events:
    call method cl_gui_cfw=>dispatch.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE program.
    WHEN OTHERS.
*     do nothing
  ENDCASE.
  CLEAR OK_CODE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module USER_COMMAND_0100 input.
   CASE OK_CODE.
     WHEN 'CHANIN'.
       PERFORM CHANGE_INFO.
     WHEN 'DISIN'.
       PERFORM DISPLAY_INFO.
     WHEN 'CHANSA'.
       PERFORM CHANGE_SA.
     WHEN 'DISSA'.
       PERFORM DISPLAY_SA.
     WHEN 'CHANSL'.
       PERFORM CHANGE_SL.
     WHEN 'REFRESH'.
       PERFORM REFRESH.
     WHEN 'EXIT'.
       LEAVE PROGRAM.
     WHEN 'BACK'.
       LEAVE TO SCREEN 0.
   ENDCASE.
endmodule.                 " USER_COMMAND_0100  INPUT

FORM CHANGE_INFO.
  DATA : WA_ROW_TABLE LIKE LINE OF ROW_TABLE.
  CLEAR : wa_sa.
  clear WA_ROW_TABLE.

  CALL METHOD GRID1->GET_SELECTED_ROWS IMPORTING ET_INDEX_ROWS =
                                                ROW_TABLE[].
* only one selected row!
  READ TABLE ROW_TABLE INTO WA_ROW_TABLE INDEX 1.
  IF SY-SUBRC = 0.
    READ TABLE IT_SA INTO WA_SA INDEX WA_ROW_TABLE-INDEX.
    IF SY-SUBRC = 0.
       if wa_sa-lifnr is initial.
          message I802(ZMMM) with wa_sa-matnr.
       ELSE.
         SET PARAMETER ID 'MAT' FIELD wa_sa-matnr.
         set parameter id 'LIF' FIELD wa_sa-lifnr.
         set parameter id 'EKO' FIELD 'PU01'.
         CALL TRANSACTION 'ME12' AND SKIP FIRST SCREEN.
       endif.
    ENDIF.
  ELSE.
*    MESSAGE I181.
  ENDIF.

ENDFORM.

FORM DISPLAY_INFO.
  DATA : WA_ROW_TABLE LIKE LINE OF ROW_TABLE.
  CLEAR : wa_sa.
  clear WA_ROW_TABLE.

  CALL METHOD GRID1->GET_SELECTED_ROWS IMPORTING ET_INDEX_ROWS =
                                                ROW_TABLE[].
* only one selected row!
  READ TABLE ROW_TABLE INTO WA_ROW_TABLE INDEX 1.
  IF SY-SUBRC = 0.
    READ TABLE IT_SA INTO WA_SA INDEX WA_ROW_TABLE-INDEX.
    IF SY-SUBRC = 0.
       if wa_sa-lifnr is initial.
          message I802(ZMMM) with wa_sa-matnr.
       ELSE.
          SET PARAMETER ID 'MAT' FIELD wa_sa-matnr.
          set parameter id 'LIF' FIELD wa_sa-lifnr.
          set parameter id 'EKO' FIELD 'PU01'.
          CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.
       endif.
    ENDIF.
  ELSE.
*    MESSAGE I181.
  ENDIF.

ENDFORM.

FORM CHANGE_SA.
  DATA : WA_ROW_TABLE LIKE LINE OF ROW_TABLE.
  CLEAR : wa_sa.
  clear WA_ROW_TABLE.

  CALL METHOD GRID1->GET_SELECTED_ROWS IMPORTING ET_INDEX_ROWS =
                                                ROW_TABLE[].
* only one selected row!
  READ TABLE ROW_TABLE INTO WA_ROW_TABLE INDEX 1.
  IF SY-SUBRC = 0.
    READ TABLE IT_SA INTO WA_SA INDEX WA_ROW_TABLE-INDEX.
    IF SY-SUBRC = 0.
       if wa_sa-ebeln is initial.
          message I801(ZMMM) with wa_sa-matnr.
       ELSE.
         SET PARAMETER ID 'VRT' FIELD wa_sa-ebeln.
         CALL TRANSACTION 'ME32' AND SKIP FIRST SCREEN.
       ENDIF.
    ENDIF.
  ELSE.
*    MESSAGE I181.
  ENDIF.
ENDFORM.

FORM DISPLAY_SA.
  DATA : WA_ROW_TABLE LIKE LINE OF ROW_TABLE.
  CLEAR : wa_sa.
  clear WA_ROW_TABLE.

  CALL METHOD GRID1->GET_SELECTED_ROWS IMPORTING ET_INDEX_ROWS =
                                                ROW_TABLE[].
* only one selected row!
  READ TABLE ROW_TABLE INTO WA_ROW_TABLE INDEX 1.
  IF SY-SUBRC = 0.
    READ TABLE IT_SA INTO WA_SA INDEX WA_ROW_TABLE-INDEX.
    IF SY-SUBRC = 0.
       if wa_sa-ebeln is initial.
          message I801(ZMMM) with wa_sa-matnr.
       ELSE.
         SET PARAMETER ID 'VRT' FIELD wa_sa-ebeln.
         CALL TRANSACTION 'ME33' AND SKIP FIRST SCREEN.
       ENDIF.
    ENDIF.
  ELSE.
*    MESSAGE I181.
  ENDIF.

ENDFORM.

FORM CHANGE_SL.
  DATA : WA_ROW_TABLE LIKE LINE OF ROW_TABLE.
  CLEAR : wa_sa.
  clear wa_ekpo.
  clear WA_ROW_TABLE.

  CALL METHOD GRID1->GET_SELECTED_ROWS IMPORTING ET_INDEX_ROWS =
                                                ROW_TABLE[].
* only one selected row!
  READ TABLE ROW_TABLE INTO WA_ROW_TABLE INDEX 1.
  IF SY-SUBRC = 0.
    READ TABLE IT_SA INTO WA_SA INDEX WA_ROW_TABLE-INDEX.

    IF SY-SUBRC = 0.
       READ TABLE IT_EKPO into wa_ekpo WITH KEY MATNR = wa_sa-matnr
                                                LIFNR = wa_sa-lifnr
                                                EBELN = wa_sa-ebeln.
       if sy-subrc eq 0.
          SET PARAMETER ID 'MAT' FIELD wa_sa-matnr.
          SET PARAMETER ID 'WRK' FIELD wa_ekpo-werks.
          CALL TRANSACTION 'ME01' AND SKIP FIRST SCREEN.
       else.
          SET PARAMETER ID 'MAT' FIELD wa_sa-matnr.
          CALL TRANSACTION 'ME01'.
       endif.
    ENDIF.
  ELSE.
*    MESSAGE I181.
  ENDIF.
ENDFORM.

FORM DATA_INITIALIZATION.
  clear: it_mara, it_mara[], wa_mara,
         it_ekpo, it_ekpo[], wa_ekpo,
         it_sa, it_sa[], wa_sa.
  clear: it_info, it_info[], it_eord, it_eord[].
  clear: mmatnr, m_nlines, m_prdat.
  clear: w_lifnr_low, w_lifnr_high.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH.
  PERFORM DATA_INITIALIZATION.
  PERFORM PROCESS_DATA.
ENDFORM.                    " REFRESH
