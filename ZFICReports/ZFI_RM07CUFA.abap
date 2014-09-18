REPORT yfmep LINE-SIZE 170
             LINE-COUNT 58
             NO STANDARD PAGE HEADING.

data: begin of iftab occurs 0,
         BWART  LIKE T156B-BWART,
         SAKNR  LIKE SKB1-SAKNR,
      end of iftab.

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
PARAMETERS : p_bukrs like t001-bukrs memory id BUK.
PARAMETERS : p_file LIKE RLGRAP-FILENAME DEFAULT
   'c:\temp\temp1.xls'.
*arameters : noheader as checkbox default ' ' no-display.

data: g_PER(2)  type n.
DATA: G_BLDAT LIKE BPDY-BLDAT,
      g_subrc like sy-subrc.

ranges: r_SAKNR for ska1-SAKNR.

*---------------------------------------------------------------------
*  S T A R T - O F - S E L E C T I O N
*---------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM UPLOAD_PC_FILE.

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
* Line selection                                                       *
************************************************************************
AT LINE-SELECTION.
  PERFORM pick.

*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
FORM display_data.

  loop at iftab.
    at end of bwart.
      write:/ iftab-BWART. " iftab-SAKNR.
      hide iftab-bwart.
    endat.
  endloop.

  clear iftab.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  pick
*&---------------------------------------------------------------------*
FORM pick.
  data: l_bwart like iftab-bwart.
  l_bwart = iftab-bwart.
  refresh r_saknr.
  r_saknr-sign = 'I'.
  r_saknr-option = 'EQ'.
  loop at iftab where bwart = l_bwart.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              INPUT  = iftab-saknr
         IMPORTING
              OUTPUT = r_saknr-low.

    append r_saknr.
  endloop.

  SUBMIT RM07CUFA
          WITH BUKRS = p_bukrs
          WITH BWART = l_bwart
          WITH SAKNR in r_saknr
          and return.

*  write:/ iftab-BWART, iftab-SAKNR.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
FORM top_of_page.

ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PC_FILE
*&---------------------------------------------------------------------*
FORM UPLOAD_PC_FILE.

  call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            filename                = p_file
            i_begin_col             = 2
            i_begin_row             = 4
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
*      IF NOHEADER = 'X' AND IEXCEL-row = 1.
*        CONTINUE.
*      ENDIF.
      tind = IEXCEL-col.
      CONCATENATE 'DATA_TAB-VALUE_' tind INTO zwfeld.
      ASSIGN (zwfeld) TO <fs1>.
      <fs1> = IEXCEL-value.
      AT END OF row.
*       APPEND data_tab.

        iftab-BWART  = data_tab-value_0001.

        iftab-SAKNR  = data_tab-value_0016.
        perform add_itab using data_tab-value_0016.
        perform add_itab using data_tab-value_0017.
        perform add_itab using data_tab-value_0018.
        perform add_itab using data_tab-value_0019.
        perform add_itab using data_tab-value_0020.
        perform add_itab using data_tab-value_0021.
        perform add_itab using data_tab-value_0022.
        perform add_itab using data_tab-value_0023.

        CLEAR data_tab.
      ENDAT.
    ENDLOOP.
  endif.

ENDFORM.                    " UPLOAD_PC_FILE
*&---------------------------------------------------------------------*
*&      Form  add_itab
*&---------------------------------------------------------------------*
FORM add_itab USING    F_VALUE.
  check f_value <> '#N/A'.
  iftab-SAKNR = f_value.
  append iftab.
ENDFORM.                    " add_itab
