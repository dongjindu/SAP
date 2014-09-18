* FM actual total : FMIT
*   RVERS, RYEAR, FIKRS, RFISTL, RFONDS, RFIPEX, TSL01
*
*&--------------------------------------------------------------------
*& Author                 : HS.Jeong
*& Creation Date          : 13/02/2004
*& Specification By       : hs.jeong
*& Pattern                : Report 1-1
*& Development Request No : UD1K906376
*& Addl documentation     :1
*& Description  : FM Cancel released budget
*&
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------

REPORT zcfif04 LINE-SIZE 170
             LINE-COUNT 58
             NO STANDARD PAGE HEADING.

INCLUDE <icon>.

*copy program : WISP_LOESCHE_LAYOUT

TABLES:
        fmfpo, fmfpot,    "commitment
        fmfctr, fmfctrt,  "fund center
        fmhictr,          "fund center hier
        bppe.
TABLES: fmep,fmsu.
TABLES: fkrs, fpos, tbpfm.
DATA: ibppe LIKE bppe OCCURS 0 WITH HEADER LINE.

*actual
TABLES: fmit.
DATA: ifmit LIKE fmit OCCURS 0 WITH HEADER LINE.


DATA: BEGIN OF itab OCCURS 0,
         fictr   LIKE fmfctr-fictr,
         fipos   LIKE fmfpo-fipos,
         bezeich LIKE fmfpot-bezeich,
         geber   LIKE bppe-geber,
*---released
         r01     LIKE bppe-wtp01,
         r02     LIKE bppe-wtp01,
         r03     LIKE bppe-wtp01,
         r04     LIKE bppe-wtp01,
         r05     LIKE bppe-wtp01,
         r06     LIKE bppe-wtp01,
         r07     LIKE bppe-wtp01,
         r08     LIKE bppe-wtp01,
         r09     LIKE bppe-wtp01,
         r10     LIKE bppe-wtp01,
         r11     LIKE bppe-wtp01,
         r12     LIKE bppe-wtp01,
*---commitment
         c01     LIKE ifmit-hsl01,
         c02     LIKE ifmit-hsl01,
         c03     LIKE ifmit-hsl01,
         c04     LIKE ifmit-hsl01,
         c05     LIKE ifmit-hsl01,
         c06     LIKE ifmit-hsl01,
         c07     LIKE ifmit-hsl01,
         c08     LIKE ifmit-hsl01,
         c09     LIKE ifmit-hsl01,
         c10     LIKE ifmit-hsl01,
         c11     LIKE ifmit-hsl01,
         c12     LIKE ifmit-hsl01,
*---invoice
         i01     LIKE ifmit-hsl01,
         i02     LIKE ifmit-hsl01,
         i03     LIKE ifmit-hsl01,
         i04     LIKE ifmit-hsl01,
         i05     LIKE ifmit-hsl01,
         i06     LIKE ifmit-hsl01,
         i07     LIKE ifmit-hsl01,
         i08     LIKE ifmit-hsl01,
         i09     LIKE ifmit-hsl01,
         i10     LIKE ifmit-hsl01,
         i11     LIKE ifmit-hsl01,
         i12     LIKE ifmit-hsl01,
*---actual
         a01     LIKE ifmit-hsl01,
         a02     LIKE ifmit-hsl01,
         a03     LIKE ifmit-hsl01,
         a04     LIKE ifmit-hsl01,
         a05     LIKE ifmit-hsl01,
         a06     LIKE ifmit-hsl01,
         a07     LIKE ifmit-hsl01,
         a08     LIKE ifmit-hsl01,
         a09     LIKE ifmit-hsl01,
         a10     LIKE ifmit-hsl01,
         a11     LIKE ifmit-hsl01,
         a12     LIKE ifmit-hsl01,

         bamt    LIKE bppe-wtp01,
         ramt    LIKE bppe-wtp01,
         camt    LIKE ifmit-hsl01,
         iamt    LIKE ifmit-hsl01,
         aamt    LIKE bppe-wtp02,
         profil  LIKE tbpfm-profil,
         posit   LIKE fmfpo-posit,
         mark(1) TYPE c,
      END OF itab.

DATA: BEGIN OF iout OCCURS 0,
         fictr   LIKE fmfctr-fictr,
         fipos   LIKE fmfpo-fipos,
         bezeich LIKE fmfpot-bezeich,
         geber   LIKE bppe-geber,
         mm(2) TYPE n,
*---released
         r01     LIKE bppe-wtp01,
         r02     LIKE bppe-wtp01,
         r03     LIKE bppe-wtp01,
         r04     LIKE bppe-wtp01,
         r05     LIKE bppe-wtp01,
         r06     LIKE bppe-wtp01,
         r07     LIKE bppe-wtp01,
         r08     LIKE bppe-wtp01,
         r09     LIKE bppe-wtp01,
         r10     LIKE bppe-wtp01,
         r11     LIKE bppe-wtp01,
         r12     LIKE bppe-wtp01,
*---commitment
         c01     LIKE ifmit-hsl01,
         c02     LIKE ifmit-hsl01,
         c03     LIKE ifmit-hsl01,
         c04     LIKE ifmit-hsl01,
         c05     LIKE ifmit-hsl01,
         c06     LIKE ifmit-hsl01,
         c07     LIKE ifmit-hsl01,
         c08     LIKE ifmit-hsl01,
         c09     LIKE ifmit-hsl01,
         c10     LIKE ifmit-hsl01,
         c11     LIKE ifmit-hsl01,
         c12     LIKE ifmit-hsl01,
*---invoice
         i01     LIKE ifmit-hsl01,
         i02     LIKE ifmit-hsl01,
         i03     LIKE ifmit-hsl01,
         i04     LIKE ifmit-hsl01,
         i05     LIKE ifmit-hsl01,
         i06     LIKE ifmit-hsl01,
         i07     LIKE ifmit-hsl01,
         i08     LIKE ifmit-hsl01,
         i09     LIKE ifmit-hsl01,
         i10     LIKE ifmit-hsl01,
         i11     LIKE ifmit-hsl01,
         i12     LIKE ifmit-hsl01,
*---actual
         a01     LIKE ifmit-hsl01,
         a02     LIKE ifmit-hsl01,
         a03     LIKE ifmit-hsl01,
         a04     LIKE ifmit-hsl01,
         a05     LIKE ifmit-hsl01,
         a06     LIKE ifmit-hsl01,
         a07     LIKE ifmit-hsl01,
         a08     LIKE ifmit-hsl01,
         a09     LIKE ifmit-hsl01,
         a10     LIKE ifmit-hsl01,
         a11     LIKE ifmit-hsl01,
         a12     LIKE ifmit-hsl01,

         bamt    LIKE bppe-wtp01,
         ramt    LIKE bppe-wtp01,
         camt    LIKE ifmit-hsl01,
         iamt    LIKE ifmit-hsl01,
         aamt    LIKE bppe-wtp02,
         profil  LIKE tbpfm-profil,
         posit   LIKE fmfpo-posit,
         mark(1) TYPE c,
      END OF iout.

DATA : BEGIN OF ifmfctr OCCURS 0,
         ctr_objnr  LIKE fmfctr-ctr_objnr,
         fictr      LIKE fmfctr-fictr,
         parent_obj LIKE fmhictr-parent_obj,
       END OF ifmfctr.

DATA : BEGIN OF ifmfpo OCCURS 0,
         fipos    LIKE fmfpo-fipos,
         bezeich  LIKE fmfpot-bezeich,
         posit    LIKE fmep-posit,
       END OF ifmfpo.

* Sam file Layout
DATA : BEGIN OF rec OCCURS 10,
             geber LIKE fmps-geber,   "fund
             fistl LIKE bpfmps-fistl, "fund center
             fipos LIKE bpfmps-fipos, "commitment
             wert(15) TYPE c,         "amt
        END OF rec.


* Active availability control on commitment budget
DATA: BEGIN OF fmctl OCCURS 0,
         fictr     LIKE fmfctr-fictr,
         fipos     LIKE fmfpo-fipos,
         geber     LIKE bppe-geber,
         profil    LIKE tbpfm-profil,
      END OF fmctl.
*====FOR BDC
DATA : it_bdc      LIKE bdcdata OCCURS 0 WITH HEADER LINE.
DATA:  it_messtab  LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_result  OCCURS 0,
            text(10),
            year(10), "  LIKE bpja-gjahr,
            msgv1 LIKE bdcmsgcoll-msgv1,
       END OF it_result.

DATA : BEGIN OF it_err  OCCURS 0,
            fictr   LIKE fmfctr-fictr,
            fipos   LIKE fmfpo-fipos,
            MM(2)   TYPE N,
            msgv1 LIKE bdcmsgcoll-msgv1,
       END OF it_err.

DATA:   messtxt(255) TYPE c.
DATA : tcode LIKE tstc-tcode.
DATA : wa_amt(12),
       wa_cnt TYPE i.
* for combobox
TYPE-POOLS: vrm.
DATA: it_val TYPE vrm_values,
      w_line LIKE LINE OF it_val.
*=====2004/02/12
FIELD-SYMBOLS <field>.
DATA : fname(08),
       fname1(08),
       mm(2) TYPE n,
       cc(2) TYPE N.

DATA : wa_mark.

PARAMETERS: p_act(1) TYPE c AS LISTBOX VISIBLE LENGTH 25 OBLIGATORY.

SELECTION-SCREEN BEGIN OF BLOCK sb WITH FRAME TITLE c010.
*p_buk  like fmit-rbukrs memory id BUK obligatory,
PARAMETERS :
             p_fik  LIKE fmps-fikrs  MEMORY ID fik OBLIGATORY
                                      DEFAULT 'H201',
             p_gjr  LIKE bpdy-jahr   DEFAULT sy-datum+0(4),
*            MEMORY ID GJR OBLIGATORY,
             p_per  LIKE bpdy-perio  DEFAULT sy-datum+4(2).
*            memory id per obligatory.
SELECTION-SCREEN END OF BLOCK sb.
* WRTTP: 43 - current, 46 - release
* VORGA: kbud - origin, kbn0 - supp, kbr0 - return, kbus - transfer
*        kbfr - release


SELECTION-SCREEN BEGIN OF BLOCK sl WITH FRAME TITLE c020.
SELECT-OPTIONS: p_fictr FOR fmfctr-fictr,
                p_fipos FOR fmfpo-fipos,
                p_geber FOR bppe-geber,
                p_prof  FOR tbpfm-profil DEFAULT 'B' OPTION NE.
PARAMETERS: p_ver  LIKE bppe-versn DEFAULT '000' NO-DISPLAY.
* ' ' - available.    'X' - released

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) c_scale.
SELECTION-SCREEN POSITION 33.
PARAMETERS: p_r LIKE rfpdo1-ts70skal  DEFAULT '0'.
SELECTION-SCREEN COMMENT 35(1) c_slash.
SELECTION-SCREEN POSITION 37.
PARAMETERS: p_d LIKE rfpdo1-ts70skal  DEFAULT '0'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK sl.


PARAMETERS : p_file LIKE rlgrap-filename DEFAULT
   'c:\temp\fmreturn.xls'.

DATA: g_per(2)  TYPE n.
DATA: g_bldat LIKE bpdy-bldat,
      g_subrc LIKE sy-subrc.

TABLES: t100.

* for function
DATA: g_func(1) TYPE c.

*---------------------------------------------------------------------*
* Initialization.
*---------------------------------------------------------------------*
INITIALIZATION.
* for combo box
  w_line-key  = '1'.
  w_line-text = 'Sort by Fund Center'.
  APPEND w_line TO it_val.
  w_line-key  = '2'.
  w_line-text = 'Sort by Commitment Item'.
  APPEND w_line TO it_val.
  w_line-key  = '3'.
  w_line-text = 'Sort by Fund'.
  APPEND w_line TO it_val.
  p_act = '2'.

* initial screen text
  c_scale = 'Scale/Decimal'.
  c_slash = '/'.
  c010 = 'Run Parameter'.
  c020 = 'Select option'.


*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = 'P_ACT'
            values = it_val.

AT SELECTION-SCREEN.
*---------------------------------------------------------------------
*  S T A R T - O F - S E L E C T I O N
*---------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM get_master_info.

* read database
  REFRESH itab.

  PERFORM get_bbpe.

  PERFORM get_fmit.

  PERFORM get_budget_period.

  PERFORM make_available.

  PERFORM delete_zero.

*---------------------------------------------------------------------
*  END-OF-SELECTION.
*---------------------------------------------------------------------
*--2004/12/12
END-OF-SELECTION.

  PERFORM display_data.
  SET TITLEBAR '9000'.
  SET PF-STATUS 'PF-9000'.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'SALL'.
      PERFORM check_mark_field USING 'X'.
    WHEN 'DALL'.
      PERFORM check_mark_field USING ' '.
    WHEN 'EXEC'.
      sy-index = 2.
      REFRESH : it_err.
      CLEAR   : it_err.
      LOOP AT iout.
        CLEAR   wa_mark.
        READ LINE sy-index FIELD VALUE iout-mark INTO wa_mark.

        IF sy-subrc NE 0.
          sy-index  =  sy-index + 1.
          CONTINUE.
        ENDIF.
        IF    wa_mark EQ space.
          sy-index = sy-index + 1.
          CONTINUE.
*                                                                  EXIT.
        ENDIF.

        CHECK wa_mark NE space.
        sy-index = sy-index + 1.
*----bcd process fr35
        REFRESH : it_bdc, it_messtab.
        CLEAR   : it_bdc, it_messtab, wa_amt.

        DO iout-mm TIMES.
*---released
          cc = sy-index.
          clear wa_amt.
          CONCATENATE 'iout-a' cc INTO  fname.
          ASSIGN (fname) TO <field>.
          wa_amt = <field>.
          CLEAR : fname, <field>.
          if wa_amt = 0.
             continue.
          endif.
          PERFORM bdc_process using wa_amt.

        ENDDO.
      ENDLOOP.


*---ERROR
    WHEN 'ERR'.
      DESCRIBE TABLE it_err LINES wa_cnt.
      IF wa_cnt > 0.
        sy-lsind =  1.
        FORMAT COLOR 6.
        WRITE : /5     '      Error      List           '.
        FORMAT COLOR OFF.
        WRITE : /5     '--------------------------------'.
        WRITE : /5     'Fund Center           Commitment'.
        WRITE : /5     '--------------------------------'.

        LOOP AT it_err.
          WRITE : /5  it_err-fictr,
                   30 it_err-fipos,
                   40 IT_ERR-MM.
        ENDLOOP.
        WRITE : /5     '--------------------------------'.
      ENDIF.

*    WHEN 'BACK'.
*          LEAVE TO SCREEN 0.
  ENDCASE.

************************************************************************
TOP-OF-PAGE.
  PERFORM top_of_page.

************************************************************************
* TOP-OF-PAGE DURING LINE-SELECTION
************************************************************************
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM top_of_page.

************************************************************************
* Line selection                                                       *
************************************************************************
AT LINE-SELECTION.
  PERFORM pick.

************************************************************************
***  (SHIFT+PF1) Execute Download
************************************************************************
AT PF13.
  PERFORM data_download.

************************************************************************
***  (SHIFT+PF4) Release simulation
************************************************************************
AT PF16.

************************************************************************
***  (SHIFT+PF5) Release
************************************************************************
AT PF17.
*&---------------------------------------------------------------------*
*&      Form  get_master_info
*&---------------------------------------------------------------------*
FORM get_master_info.
* commitment Item
  SELECT * INTO CORRESPONDING FIELDS OF TABLE ifmfpo
     FROM fmfpo
     WHERE fikrs =  p_fik
       AND fipos IN p_fipos.

* commitment item text
  LOOP AT ifmfpo.
    SELECT SINGLE bezeich INTO ifmfpo-bezeich
       FROM fmfpot
       WHERE spras = sy-langu
         AND fikrs = p_fik
         AND fipos = ifmfpo-fipos.
    MODIFY ifmfpo.
  ENDLOOP.

* Fund Center
  SELECT * INTO CORRESPONDING FIELDS OF TABLE ifmfctr
     FROM fmfctr
     WHERE fikrs =  p_fik
       AND fictr IN p_fictr.

** : KDM 10/24/2011 block
** Fund Center Hiearchy (select end node only)
*  LOOP AT ifmfctr.
*    SELECT SINGLE * FROM fmhictr
*       WHERE ctr_objnr = ifmfctr-ctr_objnr.
*    IF fmhictr-parent_obj = space.
*      DELETE ifmfctr.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " get_master_info
*&---------------------------------------------------------------------*
*&      Form  data_download
*&---------------------------------------------------------------------*
FORM data_download.
* EDIT_TABLE_WITH_EXCEL
* SEND_TABLE_TO_EXCEL

  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename = p_file
            filetype = 'WK1'
       TABLES
            data_tab = itab.

  WRITE:/ p_file, ' is created...'.
ENDFORM.                    " data_download
*&---------------------------------------------------------------------*
*&      Form  get_bbpe
*&---------------------------------------------------------------------*
FORM get_bbpe.
  DATA: l_amt LIKE bppe-wtp01.

  SELECT * FROM bppe
    INTO TABLE ibppe
    FOR ALL ENTRIES IN ifmfctr
    WHERE objnr =  ifmfctr-ctr_objnr
      AND gjahr =  p_gjr
      AND versn =  p_ver
      AND geber IN p_geber.

  LOOP AT ibppe.
    CLEAR itab.

* if action is 'B', then just select released budget only
    IF p_act = 'B'.
      CHECK ibppe-wrttp = '46'.
    ENDIF.

* filter for commitment item
    READ TABLE ifmfpo  WITH KEY posit = ibppe-posit.
    CHECK sy-subrc = 0.

*   check itab-fipos in p_fipos.
    itab-fipos   = ifmfpo-fipos.
    itab-bezeich = ifmfpo-bezeich.

    READ TABLE ifmfctr WITH KEY ctr_objnr = ibppe-objnr.
    itab-fictr = ifmfctr-fictr.
    CHECK sy-subrc = 0.
*   check itab-fictr in p_fictr.


    MOVE-CORRESPONDING ibppe TO itab.

*   available budget = orgin - released
*===2004/02/12
*    case p_per.  " Period
*      when 1.     l_amt = ibppe-wtp01.
*      when 2.     l_amt = ibppe-wtp02.
*      when 3.     l_amt = ibppe-wtp03.
*      when 4.     l_amt = ibppe-wtp04.
*      when 5.     l_amt = ibppe-wtp05.
*      when 6.     l_amt = ibppe-wtp06.
*      when 7.     l_amt = ibppe-wtp07.
*      when 8.     l_amt = ibppe-wtp08.
*      when 9.     l_amt = ibppe-wtp09.
*      when 10.    l_amt = ibppe-wtp10.
*      when 11.    l_amt = ibppe-wtp11.
*      when 12.    l_amt = ibppe-wtp12.
*    endcase.
*

* Released
    IF ibppe-wrttp = '46'.
*     ITAB-RAMT = l_amt.
*---2004/02/12
      itab-r01 = ibppe-wtp01.
      itab-r02 = ibppe-wtp02.
      itab-r03 = ibppe-wtp03.
      itab-r04 = ibppe-wtp04.
      itab-r05 = ibppe-wtp05.
      itab-r06 = ibppe-wtp06.
      itab-r07 = ibppe-wtp07.
      itab-r08 = ibppe-wtp08.
      itab-r09 = ibppe-wtp09.
      itab-r10 = ibppe-wtp10.
      itab-r11 = ibppe-wtp11.
      itab-r12 = ibppe-wtp12.
    ELSE.
      itab-bamt = l_amt.
    ENDIF.

    COLLECT itab.

  ENDLOOP.
ENDFORM.                    " get_bbpe
*&---------------------------------------------------------------------*
*&      Form  get_fmit
*&---------------------------------------------------------------------*
FORM get_fmit.
  DATA: l_amt LIKE bppe-wtp01.

  SELECT * FROM fmit
    INTO TABLE ifmit
    WHERE rfistl IN p_fictr
      AND rfonds IN p_geber
      AND ryear  =  p_gjr.

  LOOP AT ifmit.
* filter for commitment item
    CHECK ifmit-rfipex IN p_fipos.
    CLEAR itab.

* 50 - p/r, 51 - p/o, 54 - invoice, 57 - payment, 58 - d/p req
* 95 - co posting (secondary cost posting)
    CHECK ( ifmit-rwrttp = '50' OR ifmit-rwrttp = '51'
         OR ifmit-rwrttp = '60' OR ifmit-rwrttp = '61' )
       OR ( ifmit-rbtart = '0100'
            AND ( ifmit-rwrttp = '54'
               OR ifmit-rwrttp = '66'
               OR ifmit-rwrttp = '95' ) ).


    itab-fipos = ifmit-rfipex.
    itab-fictr = ifmit-rfistl.
    itab-geber = ifmit-rfonds.

*   check itab-fictr in p_fictr.

*    case p_per.  " Period
*      when 1.     l_amt = ifmit-tsl01.
*      when 2.     l_amt = ifmit-tsl02.
*      when 3.     l_amt = ifmit-tsl03.
*      when 4.     l_amt = ifmit-tsl04.
*      when 5.     l_amt = ifmit-tsl05.
*      when 6.     l_amt = ifmit-tsl06.
*      when 7.     l_amt = ifmit-tsl07.
*      when 8.     l_amt = ifmit-tsl08.
*      when 9.     l_amt = ifmit-tsl09.
*      when 10.    l_amt = ifmit-tsl10.
*      when 11.    l_amt = ifmit-tsl11.
*      when 12.    l_amt = ifmit-tsl12.
*    endcase.
*
*    itab-aamt = l_amt.
*---commitment
    IF ifmit-rwrttp = '50' OR ifmit-rwrttp = '51'
         OR ifmit-rwrttp = '60' OR ifmit-rwrttp = '61'.
      itab-c01 = ifmit-hsl01.
      itab-c02 = ifmit-hsl02.
      itab-c03 = ifmit-hsl03.
      itab-c04 = ifmit-hsl04.
      itab-c05 = ifmit-hsl05.
      itab-c06 = ifmit-hsl06.
      itab-c07 = ifmit-hsl07.
      itab-c08 = ifmit-hsl08.
      itab-c09 = ifmit-hsl09.
      itab-c10 = ifmit-hsl10.
      itab-c11 = ifmit-hsl11.
      itab-c12 = ifmit-hsl12.
    ENDIF.
*---invoice
    IF ifmit-rbtart = '0100'.
      IF ifmit-rwrttp = '54'
              OR ifmit-rwrttp = '66'
              OR ifmit-rwrttp = '95'.
        itab-i01 = ifmit-hsl01.
        itab-i02 = ifmit-hsl02.
        itab-i03 = ifmit-hsl03.
        itab-i04 = ifmit-hsl04.
        itab-i05 = ifmit-hsl05.
        itab-i06 = ifmit-hsl06.
        itab-i07 = ifmit-hsl07.
        itab-i08 = ifmit-hsl08.
        itab-i09 = ifmit-hsl09.
        itab-i10 = ifmit-hsl10.
        itab-i11 = ifmit-hsl11.
        itab-i12 = ifmit-hsl12.
      ENDIF.
    ENDIF.

    COLLECT itab.

  ENDLOOP.
ENDFORM.                    " get_fmit
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
FORM display_data.

  CASE p_act.
    WHEN '1'.  SORT iout BY fictr fipos.
    WHEN '2'.  SORT iout BY fipos fictr.
    WHEN '3'.  SORT iout BY geber fipos.
  ENDCASE.


  LOOP AT iout.
    PERFORM display_line USING 'L'.

    AT END OF fictr.
      IF p_act = '1'.
        SUM.
        PERFORM display_line USING 'S'.
        ULINE.
      ENDIF.
    ENDAT.

    AT LAST.
      SUM.
      ULINE.
      PERFORM display_line  USING 'S'.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  fill_budget
*&---------------------------------------------------------------------*
FORM fill_budget.
  REFRESH rec.
  LOOP AT itab.

* release - actual
    rec-wert = itab-ramt - itab-aamt.

    rec-fistl = itab-fictr. "fundcenter
    rec-fipos = itab-fipos. "commitment item
    rec-geber = itab-geber. "fund

    IF rec-wert <> 0.
      APPEND rec.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " fill_budget
*&---------------------------------------------------------------------*
*&      Form  delete_zero
*&---------------------------------------------------------------------*
FORM delete_zero.
  DATA: a_amt LIKE itab-ramt.

  LOOP AT iout.
      if iout-a01 = 0 and iout-a02 = 0
        and iout-a03 = 0 and iout-a04 = 0
        and iout-a05 = 0 and iout-a06 = 0
        and iout-a07 = 0 and iout-a08 = 0
        and iout-a09 = 0 and iout-a10 = 0
        and iout-a11 = 0 and iout-a12 = 0.
        delete iout.

      endif.
** available
*    iout-aamt = iout-ramt + iout-camt + iout-iamt.
*    IF iout-aamt < 0.
*      iout-aamt = 0.
*    ENDIF.
**--2004/02/12
*    IF  iout-aamt  = 0.
*      DELETE iout.
*    ELSE.
*      MODIFY iout.
*    ENDIF.
**--
  ENDLOOP.
*  loop at itab.
*    if itab-fipos > '699999' or itab-fipos < '600000'.
*      delete itab index sy-tabix.
*    endif.
*  endloop.
ENDFORM.                    " delete_zero
*&---------------------------------------------------------------------*
*&      Form  get_budget_period
*&---------------------------------------------------------------------*
FORM get_budget_period.


  SELECT * INTO CORRESPONDING FIELDS OF TABLE fmctl
    FROM tbpfm
    WHERE fikrs = p_fik
      AND gjahr = p_gjr.

  REFRESH : iout.
  CLEAR   : iout.

  LOOP AT itab.
    READ TABLE ifmfpo  WITH KEY fipos = itab-fipos.
    itab-posit   = ifmfpo-posit.
    itab-bezeich = ifmfpo-bezeich.

    PERFORM determine_profile_fs USING    p_fik             "H201
                                          itab-fictr  "Funds center
                                          itab-posit  "Item
                                          itab-geber  "Funder
                                          p_gjr
                                 CHANGING itab-profil.
*---2004/02/12
    IF itab-profil = 'Y' OR
       itab-profil = 'B'.
      CONTINUE.
    ENDIF.

    IF itab-profil IN p_prof.
*==============2004/02/12
*      MOVE-CORRESPONDING itab TO iout.
      move itab-fictr    to iout-fictr.
      move itab-fipos    to iout-fipos.
      move itab-bezeich  to iout-bezeich.
      move itab-geber    to iout-geber.
      move itab-profil   to iout-profil.
      CLEAR : mm.
      CASE itab-profil.
        WHEN 'M'.   "Month
          mm = p_per.
*          mm = p_per - 1.
          IF mm > 0.
            PERFORM cal_amt_symbol.
          ENDIF.
        WHEN 'Q'.   "Quarter
          CLEAR : mm.
          IF p_per > 3 AND p_per < 7.
            mm = 03.
          ENDIF.
          IF p_per > 6 AND p_per < 10.
            mm = 06.
          ENDIF.
          IF p_per > 9 AND p_per <= 12.
            mm = 09.
          ENDIF.
          IF mm > 0.
            PERFORM cal_amt_symbol.
          ENDIF.

        WHEN 'H'.   "Half
          IF p_per > 6.
            mm = 6.
          ENDIF.
          IF mm > 0.
            PERFORM cal_amt_symbol.
          ENDIF.
*        WHEN 'Y'.   "Year
*          DELETE itab.
*        WHEN OTHERS.
*          DELETE itab.
      ENDCASE.

    ELSE.
*      delete itab.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_budget_period
*---------------------------------------------------------------------*
*       FORM DETERMINE_PROFILE_FS                                     *
*---------------------------------------------------------------------*
FORM determine_profile_fs USING    l_fikrs
                                   l_fictr
                                   l_posit
                                   l_geber
                                   l_gjahr
                          CHANGING l_bprof.
  DATA: l_objnr LIKE fmfctr-ctr_objnr.
  data: l_farea  like BPJA-FAREA.

  clear: l_farea.
  l_objnr(2) = 'FS'.
  l_objnr+2(4) = l_fikrs.
  l_objnr+6  = l_fictr.

* Profile from TBPFM table.
  CALL FUNCTION 'KBPA_FIFM_GET_PROFIL'
       EXPORTING
            i_objnr         = l_objnr
            i_posit         = l_posit
            i_geber         = l_geber
            i_gjahr         = l_gjahr
            i_farea         = l_farea
       IMPORTING
            e_profil        = l_bprof
       EXCEPTIONS
            no_profil_found = 01.

  IF NOT sy-subrc IS INITIAL.
*   Profile from FundMgt Area
    CALL FUNCTION 'FM5B_GET_PROFILE'
         EXPORTING
              i_fikrs           = l_fikrs
              i_fincode         = l_geber
         IMPORTING
              e_profil          = l_bprof
         EXCEPTIONS
              fm_area_not_found = 1
              OTHERS            = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  pick
*&---------------------------------------------------------------------*
FORM pick.
  CHECK g_func = 'X'.

  IF sy-cucol < 15.     " download
    PERFORM data_download.
  ELSEIF sy-cucol < 30. " test

  ELSE.
    READ LINE 1 FIELD VALUE p_r.
    READ LINE 1 FIELD VALUE p_d.
    sy-lsind = sy-lsind - 1.
    PERFORM display_data.

  ENDIF.

* icon_refresh AS ICON HOTSPOT
* READ CURRENT LINE
*           FIELD VALUE report_lst-optres INTO report_lst-optres.
ENDFORM.                    " pick
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
FORM top_of_page.
*  write:/ 'Period:', g_per input on.
*  write: ' (Round:', p_r no-gap input on,
*         ', Decimal:', p_d no-gap input on, ')'.
*  case p_act.
*    when 'U'. write: ' Uploading...'     color COL_GROUP.
*    when 'R'. write: ' Releasing...'     color COL_GROUP.
*    when 'B'. write: ' Released Budget'  color COL_GROUP.
*  endcase.
*
*  uline.
*  g_func = 'X'.
*  write: icon_execute_object AS ICON HOTSPOT, 'Download(S_F1) ',
*         icon_execute_object AS ICON HOTSPOT, 'Test(S_F4)     ',
*         icon_execute_object AS ICON HOTSPOT, 'Run(S_F5)      ',
*         icon_refresh        AS ICON HOTSPOT, 'Refresh        '.
*
*  hide: g_func.
*  clear: g_func.
*  uline.

  FORMAT COLOR COL_HEADING.
  WRITE:/ 'FndCtr Commitment Item     Fund         P'.
* WRITE:  '         Total       C/F  '.
  WRITE:  '     Jan       Feb       Mar       Apr '.
  WRITE:  '     May       Jun       Juy       Aug '.
  WRITE:  '     Sep       Oct       Nov       Dec '.
*
*  WRITE:  ' Released BDT     Commitment        Invoice      Available '
  .
*  ULINE.
  FORMAT COLOR COL_NORMAL.

ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  display_line
*&---------------------------------------------------------------------*
FORM display_line USING l_type.
* commitment item text
  READ TABLE ifmfpo  WITH KEY fipos = iout-fipos.
  IF sy-subrc <> 0.
    CLEAR ifmfpo-bezeich.
  ENDIF.

  IF l_type = 'S'.
    WRITE:/ ' '.
    FORMAT INTENSIFIED ON.
  ELSE.
    WRITE:/ iout-mark AS CHECKBOX.
    FORMAT INTENSIFIED OFF.
  ENDIF.
  FORMAT COLOR COL_KEY.
  WRITE:  iout-fictr(6),  " no-gap,
          iout-fipos(6),  "  no-gap,
          ifmfpo-bezeich(12), "no-gap,
          iout-geber(10)     ,
          iout-profil(1)     NO-GAP.
  FORMAT COLOR COL_NORMAL.
*  WRITE:
*    45(15) iout-ramt   ROUND p_r DECIMALS p_d NO-GAP,
*    60(15) iout-camt   ROUND p_r DECIMALS p_d NO-GAP,
*    75(15) iout-iamt   ROUND p_r DECIMALS p_d NO-GAP.
*
*  FORMAT COLOR COL_TOTAL.
*  WRITE:90(15) iout-aamt       ROUND p_r DECIMALS p_d NO-GAP.
  FORMAT COLOR COL_TOTAL.
  WRITE:(10) iout-a01       ROUND p_r DECIMALS p_d NO-GAP.
  WRITE:(10) iout-a02       ROUND p_r DECIMALS p_d NO-GAP.
  WRITE:(10) iout-a03       ROUND p_r DECIMALS p_d NO-GAP.
  WRITE:(10) iout-a04       ROUND p_r DECIMALS p_d NO-GAP.
  WRITE:(10) iout-a05       ROUND p_r DECIMALS p_d NO-GAP.
  WRITE:(10) iout-a06       ROUND p_r DECIMALS p_d NO-GAP.
  WRITE:(10) iout-a07       ROUND p_r DECIMALS p_d NO-GAP.
  WRITE:(10) iout-a08       ROUND p_r DECIMALS p_d NO-GAP.
  WRITE:(10) iout-a09       ROUND p_r DECIMALS p_d NO-GAP.
  WRITE:(10) iout-a10       ROUND p_r DECIMALS p_d NO-GAP.
  WRITE:(10) iout-a11       ROUND p_r DECIMALS p_d NO-GAP.
  WRITE:(10) iout-a12       ROUND p_r DECIMALS p_d NO-GAP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM read_data.
*  DATA: $IX    LIKE SY-TABIX VALUE 3,             "..Data Display Line
*        $INDEX LIKE SY-INDEX.                     "..Table Index
*
*  CLEAR:WK_SUTAK[],WK_SUTAK.
*  DESCRIBE TABLE itab LINES SY-TFILL.
*
*  DO SY-TFILL TIMES.
*    ADD 1 TO: $IX, $INDEX.
*
*    CLEAR:itab.
*    READ LINE $IX  FIELD VALUE itab-MARK.
*    CHECK SY-SUBRC EQ 0.
*    CHECK itab-MARK EQ 'X'.
*
*    READ TABLE itab INDEX $INDEX.
*    CHECK SY-SUBRC EQ 0 AND itab-Z_CHKFLAG EQ SPACE. "..????
*
*    MOVE-CORRESPONDING itab TO WK_SUTAK.
*    APPEND WK_SUTAK. CLEAR:WK_SUTAK.
*  ENDDO.
*
*  IF WK_SUTAK[] IS INITIAL.
*    MESSAGE E006.
*  ENDIF.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM popup_to_confirm CHANGING fp_answer.
  DATA: l_defaultoption, l_textline1(70),  l_textline2(70).

  CLEAR fp_answer.
  l_defaultoption = 'N'.
  CASE sy-ucomm.
    WHEN 'EXEC'.
      l_textline1     = text-002.
      l_textline2     = text-003.
  ENDCASE.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            defaultoption = l_defaultoption
            textline1     = l_textline1
            textline2     = l_textline2
            titel         = sy-title
       IMPORTING
            answer        = fp_answer.

ENDFORM.                    " POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  EXEC_DATA
*&---------------------------------------------------------------------*
FORM exec_data.
  DATA: answer.

  PERFORM read_data.
  PERFORM popup_to_confirm CHANGING answer.
  CHECK answer EQ 'J'.

*  PERFORM EXEC_BDC.

*  PERFORM REFRESH_DATA.
ENDFORM.                    " EXEC_DATA

*    WHEN 'EXEC'. PERFORM EXEC_DATA.          "..??
*    WHEN 'SALL'. PERFORM CHECK_MARK_FIELD USING 'X'.
*    WHEN 'DALL'. PERFORM CHECK_MARK_FIELD USING ' '.
*&---------------------------------------------------------------------*
*&      Form  check_mark_field
*&---------------------------------------------------------------------*
FORM check_mark_field USING    p_mark.
  DATA: $ix LIKE sy-tabix.

  LOOP AT iout.
    $ix = sy-tabix.
    iout-mark = p_mark.
    MODIFY iout INDEX $ix.
  ENDLOOP.

  sy-lsind = sy-lsind - 1.
  PERFORM display_data.
ENDFORM.                    " check_mark_field
*&---------------------------------------------------------------------*
*&      Form  cal_amt_symbol
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_amt_symbol.
  DO mm TIMES.
*---released
    cc = sy-index.
    CONCATENATE 'itab-r' cc INTO  fname.
    ASSIGN (fname) TO <field>.
    iout-ramt = iout-ramt + <field>.
    perform make_r_data using 'r' cc <field>.
    CLEAR : fname, fname1, <field>.
*--commitment
    CONCATENATE 'itab-c' cc INTO  fname.
    ASSIGN (fname) TO <field>.
    iout-camt = iout-camt + <field>.
    perform make_r_data using 'c' cc <field>.
    CLEAR : fname, fname1, <field>.
*--invoice
    CONCATENATE 'itab-i' cc INTO  fname.
    ASSIGN (fname) TO <field>.
    iout-iamt = iout-iamt + <field>.
    perform make_r_data using 'i' cc <field>.
    CLEAR : fname, fname1, <field>.
  ENDDO.
  MOVE mm TO iout-mm.
  COLLECT iout.
  clear   iout.
ENDFORM.                    " cal_amt_symbol
*&---------------------------------------------------------------------*
*&      Form  make_bdc_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1009   text
*      -->P_1010   text
*      -->P_1011   text
*----------------------------------------------------------------------*
FORM make_bdc_rtn USING   dynbegin program dynpro.
  CLEAR it_bdc.

  IF dynbegin = 'X'.
    it_bdc-program  = program.
    it_bdc-dynpro   = dynpro.
    it_bdc-dynbegin = 'X'.
  ELSE.
    it_bdc-fnam     = program.
    it_bdc-fval     = dynpro.
  ENDIF.

  APPEND it_bdc.

ENDFORM.                    " make_bdc_rtn
*&---------------------------------------------------------------------*
*&      Form  make_available
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_available.
  LOOP AT iout.
* available
    iout-aamt = iout-ramt + iout-camt + iout-iamt.
    iout-a01 = iout-r01 + iout-c01 + iout-i01.
    iout-a02 = iout-r02 + iout-c02 + iout-i02.
    iout-a03 = iout-r03 + iout-c03 + iout-i03.
    iout-a04 = iout-r04 + iout-c04 + iout-i04.
    iout-a05 = iout-r05 + iout-c05 + iout-i05.
    iout-a06 = iout-r06 + iout-c06 + iout-i06.
    iout-a07 = iout-r07 + iout-c07 + iout-i07.
    iout-a08 = iout-r08 + iout-c08 + iout-i08.
    iout-a09 = iout-r09 + iout-c09 + iout-i09.
    iout-a10 = iout-r10 + iout-c10 + iout-i10.
    iout-a11 = iout-r11 + iout-c11 + iout-i11.
    iout-a12 = iout-r12 + iout-c12 + iout-i12.
    IF iout-aamt < 0.
      iout-aamt = 0.
    ENDIF.
    IF iout-a01  < 0.
      iout-a01  = 0.
    ENDIF.
    IF iout-a02  < 0.
      iout-a02  = 0.
    ENDIF.
    IF iout-a03  < 0.
      iout-a03  = 0.
    ENDIF.
    IF iout-a04  < 0.
      iout-a04  = 0.
    ENDIF.
    IF iout-a05  < 0.
      iout-a05  = 0.
    ENDIF.
    IF iout-a06  < 0.
      iout-a06  = 0.
    ENDIF.
    IF iout-a07  < 0.
      iout-a07  = 0.
    ENDIF.
    IF iout-a08  < 0.
      iout-a08  = 0.
    ENDIF.
    IF iout-a07  < 0.
      iout-a07  = 0.
    ENDIF.
    IF iout-a08  < 0.
      iout-a08  = 0.
    ENDIF.
    IF iout-a09  < 0.
      iout-a09  = 0.
    ENDIF.
    IF iout-a10  < 0.
      iout-a10  = 0.
    ENDIF.
    IF iout-a11  < 0.
      iout-a11  = 0.
    ENDIF.
    IF iout-a12  < 0.
      iout-a12  = 0.
    ENDIF.
*--2004/02/12
*    IF  iout-aamt  = 0.
*      DELETE iout.
*    ELSE.
*      MODIFY iout.
*    ENDIF.
*--
    MODIFY iout.
  ENDLOOP.

ENDFORM.                    " make_available
*&---------------------------------------------------------------------*
*&      Form  bdc_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_process using u_amt.
  REFRESH : IT_BDC.
  CLEAR   : IT_BDC.
  wa_amt =  u_amt * -1.
  TRANSLATE  wa_amt  USING ', '.
  CONDENSE   wa_amt  NO-GAPS.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPMKBUA'           '0111',
                      ' '  'FMPS-FIKRS'         p_fik,
                      ' '  'BPDY-JAHR'          p_gjr,
                      ' '  'BPDY-PERIO'         cc,
                      ' '  'BPDY-VERSN'         '0',
                     ' '  'BDC_OKCODE'          '/00'.

  PERFORM make_bdc_rtn USING :
                      'X'  'SAPMKBUA'           '0320',
                      ' '  'BPFMPS-FISTL(01)'   iout-fictr,
                      ' '  'BPFMPS-FIPOS(01)'   iout-fipos,
                      ' '  'BPAK-WERT(01)'       wa_amt,
                      ' '  'BDC_OKCODE'         '=BUCH'.

  CALL TRANSACTION 'FR35'   USING it_bdc
                           MODE   'E'
                           UPDATE 'S'
*                    OPTIONS  FROM CTU_PARAMS
                    MESSAGES INTO it_messtab.
  READ TABLE it_messtab INDEX 1.
  IF it_messtab-msgtyp = 'S'.
    IF it_messtab-msgid = 'BP'.
      IF it_messtab-msgnr = '043'.
         exit.
      ENDIF.
    ENDIF.
  ENDIF.
  MOVE iout-fictr TO it_err-fictr.
  MOVE iout-fipos TO it_err-fipos.
  MOVE CC         TO IT_ERR-MM.
  APPEND it_err.
  CLEAR  it_err.
ENDFORM.                    " bdc_process
*&---------------------------------------------------------------------*
*&      Form  make_r_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CC  text
*      -->P_<FIELD>  text
*----------------------------------------------------------------------*
form make_r_data using    p_gubun
                          p_cc
                          p_field.
 case p_cc.
   when '01'.
        if p_gubun = 'r'.
           iout-r01 = p_field.
        elseif p_gubun = 'c'.
           iout-c01 = p_field.
        elseif p_gubun = 'i'.
           iout-i01 = p_field.
        endif.
   when '02'.
        if p_gubun = 'r'.
           iout-r02 = p_field.
        elseif p_gubun = 'c'.
           iout-c02 = p_field.
        elseif p_gubun = 'i'.
           iout-i02 = p_field.
        endif.
   when '03'.
        if p_gubun = 'r'.
           iout-r03 = p_field.
        elseif p_gubun = 'c'.
           iout-c03 = p_field.
        elseif p_gubun = 'i'.
           iout-i03 = p_field.
        endif.
   when '04'.
        if p_gubun = 'r'.
           iout-r04 = p_field.
        elseif p_gubun = 'c'.
           iout-c04 = p_field.
        elseif p_gubun = 'i'.
           iout-i04 = p_field.
        endif.
   when '05'.
        if p_gubun = 'r'.
           iout-r05 = p_field.
        elseif p_gubun = 'c'.
           iout-c05 = p_field.
        elseif p_gubun = 'i'.
           iout-i05 = p_field.
        endif.
   when '06'.
        if p_gubun = 'r'.
           iout-r06 = p_field.
        elseif p_gubun = 'c'.
           iout-c06 = p_field.
        elseif p_gubun = 'i'.
           iout-i06 = p_field.
        endif.
   when '07'.
        if p_gubun = 'r'.
           iout-r07 = p_field.
        elseif p_gubun = 'c'.
           iout-c07 = p_field.
        elseif p_gubun = 'i'.
           iout-i07 = p_field.
        endif.
   when '08'.
        if p_gubun = 'r'.
           iout-r08 = p_field.
        elseif p_gubun = 'c'.
           iout-c08 = p_field.
        elseif p_gubun = 'i'.
           iout-i08 = p_field.
        endif.
   when '09'.
        if p_gubun = 'r'.
           iout-r09 = p_field.
        elseif p_gubun = 'c'.
           iout-c09 = p_field.
        elseif p_gubun = 'i'.
           iout-i09 = p_field.
        endif.
   when '10'.
        if p_gubun = 'r'.
           iout-r10 = p_field.
        elseif p_gubun = 'c'.
           iout-c10 = p_field.
        elseif p_gubun = 'i'.
           iout-i10 = p_field.
        endif.
   when '11'.
        if p_gubun = 'r'.
           iout-r11 = p_field.
        elseif p_gubun = 'c'.
           iout-c11 = p_field.
        elseif p_gubun = 'i'.
           iout-i11 = p_field.
        endif.
   when '12'.
        if p_gubun = 'r'.
           iout-r12 = p_field.
        elseif p_gubun = 'c'.
           iout-c12 = p_field.
        elseif p_gubun = 'i'.
           iout-i12 = p_field.
        endif.
 endcase.
endform.                    " make_r_data
