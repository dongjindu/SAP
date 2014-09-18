*
* FM Budget Download / Release Month/Quarter/Half/Year
*
*  made by Andy Choi (2003.1)
*  copyright by Andy Choi
*
* FM actual total : FMIT
*   RVERS, RYEAR, FIKRS, RFISTL, RFONDS, RFIPEX, TSL01
*
REPORT yfmep LINE-SIZE 170
             LINE-COUNT 58
             NO STANDARD PAGE HEADING.

INCLUDE <icon>.

*copy program : WISP_LOESCHE_LAYOUT

TABLES: fmci, fmcit,    "commitment
        fmfctr, fmfctrt,  "fund center
        FMHICTR,          "fund center hier
        bppe.
TABLES: fmep,fmsu.
TABLES: fkrs, fpos, TBPFM.

data: ibppe like bppe occurs 0 with header line.

data: begin of itab occurs 0,
         fistl   like TBPFM-FISTL,
         FIPOS   like TBPFM-FIPOS,
         GEBER   like TBPFM-GEBER,
         profil  like tbpfm-profil,
         bezei LIKE fmcit-bezei,
      end of itab.

data: begin of iftab occurs 0,
         fistl   like TBPFM-FISTL,
         FIPOS   like TBPFM-FIPOS,
         GEBER   like TBPFM-GEBER,
         profil  like tbpfm-profil,
      end of iftab.

DATA : BEGIN OF ifmci OCCURS 0,
         fipos    LIKE fmci-fipos,
         bezei  LIKE fmcit-bezei,
         posit    LIKE fmep-posit,
       END OF ifmci.


* Active availability control on commitment budget
data: begin of fmctl occurs 0,
         fistl     like fmfctr-fictr,
         FIPOS     like fmci-FIPOS,
         GEBER     like BPPE-GEBER,
         profil    like tbpfm-profil,
      end of fmctl.

*******************************************************
* for excel upload - start
TABLES: ALSMEX_TABLINE.

DATA: BEGIN OF IEXCEL OCCURS 0.
        INCLUDE STRUCTURE ALSMEX_TABLINE.
DATA: END OF IEXCEL.

* No of columns
DATA: BEGIN OF data_tab,
       value_0001(50),
       value_0002(50),
       value_0003(50),
       value_0004(50),
       value_0005(50),
       value_0006(50),
       value_0007(50),
       value_0008(50),
       value_0009(50),
       value_0010(50),
       value_0011(50),
       value_0012(50),
       value_0013(50),
       value_0014(50),
       value_0015(50),
       value_0016(50),
       value_0017(50),
       value_0018(50),
       value_0019(50),
       value_0020(50),
       value_0021(50),
       value_0022(50),
       value_0023(50),
       value_0024(50),
       value_0025(50),
       value_0026(50),
       value_0027(50),
       value_0028(50),
       value_0029(50),
       value_0030(50),
       value_0031(50),
       value_0032(50),
       value_0033(50),
       value_0034(50),
       value_0035(50),
       value_0036(50),
       value_0037(50),
       value_0038(50),
       value_0039(50),
       value_0040(50),
       value_0041(50),
       value_0042(50),
       value_0043(50),
       value_0044(50),
       value_0045(50),
       value_0046(50),
       value_0047(50),
       value_0048(50),
       value_0049(50),
       value_0050(50),
       value_0051(50),
       value_0052(50),
       value_0053(50),
       value_0054(50),
       value_0055(50),
       value_0056(50),
       value_0057(50),
       value_0058(50),
       value_0059(50),
       value_0060(50),
       value_0061(50),
       value_0062(50),
       value_0063(50),
       value_0064(50),
       value_0065(50),
       value_0066(50),
       value_0067(50),
       value_0068(50),
       value_0069(50),
       value_0070(50),
       value_0071(50),
       value_0072(50),
       value_0073(50),
       value_0074(50),
       value_0075(50),
       value_0076(50),
       value_0077(50),
       value_0078(50),
       value_0079(50),
       value_0080(50),
       value_0081(50),
       value_0082(50),
       value_0083(50),
       value_0084(50),
       value_0085(50),
       value_0086(50),
       value_0087(50),
       value_0088(50),
       value_0089(50),
       value_0090(50),
       value_0091(50),
       value_0092(50),
       value_0093(50),
       value_0094(50),
       value_0095(50),
       value_0096(50),
       value_0097(50),
       value_0098(50),
       value_0099(50),
       value_0100(50).
DATA: END OF data_tab.

DATA: tind(4) TYPE n.
DATA: zwfeld(19).
FIELD-SYMBOLS: <fs1>.
* for excel upload - end
*******************************************************

* for combobox
type-pools: vrm.
data: it_val type vrm_values,
      w_line like line of it_val.


SELECTION-SCREEN BEGIN OF BLOCK sb WITH FRAME TITLE c010.
PARAMETERS :
        P_FIK  LIKE FMPS-FIKRS MEMORY ID FIK OBLIGATORY,
        P_GJR  LIKE BPDY-JAHR  MEMORY ID GJR OBLIGATORY.
SELECTION-SCREEN END OF BLOCK sb.

PARAMETERS : p_file LIKE RLGRAP-FILENAME DEFAULT
   'c:\temp\fmctl.xls',
             noheader as checkbox default 'X',
             p_del    as checkbox.

SELECTION-SCREEN BEGIN OF BLOCK sl WITH FRAME TITLE c020.
select-options: p_fistl for fmfctr-fictr,
                p_fipos for fmci-fipos,
                p_geber for bppe-geber.
SELECTION-SCREEN END OF BLOCK sl.

*---------------------------------------------------------------------*
* Initialization.
*---------------------------------------------------------------------*
Initialization.
  c010 = 'Target'.
  c020 = 'Filter'.
*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
at selection-screen output.

AT SELECTION-SCREEN.

*---------------------------------------------------------------------
*  S T A R T - O F - S E L E C T I O N
*---------------------------------------------------------------------
START-OF-SELECTION.

  if p_del = 'X' and sy-uname = 'ANDY'.
    delete from tbpfm where GJAHR = p_GJR.
    exit.
  endif.

* commitment Item
  SELECT * into corresponding fields of table ifmci
     FROM fmci
     WHERE fikrs =  p_fik
       and FIPOS in p_FIPOS.
* commitment item text
  loop at ifmci.
    select single bezei into ifmci-bezei
       from fmcit
       where spras = sy-langu
         and fikrs = p_fik
         and fipex = ifmci-fipos.
    modify ifmci.
  endloop.

* upload
  PERFORM UPLOAD_PC_FILE.

  loop at iftab.
* filter selection option
    check iftab-fistl in p_fistl
      and iftab-fipos in p_fipos
      and iftab-geber in p_geber.

    move-corresponding iftab to itab.
    read table ifmci  with key fipos = iftab-fipos.
    itab-bezei = ifmci-bezei.
    append itab.
  endloop.

*---------------------------------------------------------------------
*  END-OF-SELECTION.
*---------------------------------------------------------------------
END-OF-SELECTION.

  perform display_data.


************************************************************************
top-of-page.
  perform top_of_page.

************************************************************************
* TOP-OF-PAGE DURING LINE-SELECTION
************************************************************************
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM TOP_OF_PAGE.

************************************************************************
***  (SHIFT+PF1) Execute Download
************************************************************************
AT pf13.
  PERFORM data_download.

AT pf16.
  PERFORM data_save.

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

  write:/ p_file, ' is created...'.
ENDFORM.                    " data_download
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
FORM display_data.
  sort itab by fistl fipos geber.
  loop at itab.

    format color col_heading.
    at new fistl.
      uline.
      write:/ itab-fistl(7).
    endat.

    format color col_key.
    write:/
            itab-FIPOS(7) no-gap,
            itab-bezei  no-gap,
            itab-GEBER.
    format color col_normal.
    write:  itab-profil(1).

  endloop.
  uline.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PC_FILE
*&---------------------------------------------------------------------*
FORM UPLOAD_PC_FILE.

  call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            filename                = p_file
            i_begin_col             = 1
            i_begin_row             = 1
            i_end_col               = 100
            i_end_row               = 30000
       TABLES
            intern                  = IEXCEL
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2
            OTHERS                  = 3.

  if sy-subrc <> 0.
    WRITE: / 'EXCEL UPLOAD FAILED ', SY-SUBRC.
  ELSE.
    SORT IEXCEL BY row col.
    LOOP AT IEXCEL.
      IF NOHEADER = 'X' AND IEXCEL-row = 1.
        CONTINUE.
      ENDIF.
      tind = IEXCEL-col.
      CONCATENATE 'DATA_TAB-VALUE_' tind INTO zwfeld.
      ASSIGN (zwfeld) TO <fs1>.
      <fs1> = IEXCEL-value.
      AT END OF row.
*       APPEND data_tab.

        iftab-fistl     = data_tab-value_0001.
        iftab-FIPOS     = data_tab-value_0002.
        iftab-GEBER     = data_tab-value_0003.
        iftab-PROFIL    = data_tab-value_0004.
        append iftab.
        CLEAR data_tab.
      ENDAT.
    ENDLOOP.
  endif.

ENDFORM.                    " UPLOAD_PC_FILE
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
FORM top_of_page.


ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  data_save
*&---------------------------------------------------------------------*
FORM data_save.
  delete from tbpfm where GJAHR = p_GJR.

  loop at itab.
    move-corresponding itab to tbpfm.
    tbpfm-fikrs = p_fik.
    tbpfm-gjahr = p_gjr.
    modify tbpfm.
    write:/ itab-fistl,
            itab-FIPOS(7) no-gap,
            itab-bezei  no-gap,
            itab-GEBER,
            '... saved'.

  endloop.

ENDFORM.                    " data_save
