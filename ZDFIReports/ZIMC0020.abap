*&--------------------------------------------------------------------
*& Author                 : HS.JEONG
*& Creation Date          : 09/20/2003
*& Specification By       : HS.JEONG
*& Pattern                : Report 1-1
*& Development Request No : UD1K902222
*& Addl documentation     :
*& Description  : AR  Upload
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT  zcfii99 NO STANDARD PAGE HEADING
                LINE-SIZE 132
                LINE-COUNT 65
                MESSAGE-ID zmfi.
TABLES: ztfi_imfm, ztfi_im_num, impr, imak.
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: BEGIN OF iexcel OCCURS 0.
        INCLUDE STRUCTURE alsmex_tabline.
DATA: END OF iexcel.
DATA: zwfeld(19),
      tind   LIKE iexcel-col.

FIELD-SYMBOLS: <fs1>.
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
       value_0029(50).
DATA: END OF data_tab.

*----- Internal Table
DATA : BEGIN OF it_data OCCURS 0,
          posid(24),
          ayear(4),
          seq(4),
          gjahr(4),
          gubun(1),
          prnam(10),
          kostl(10),
          twaer(5),
          tot(15),
          wtp01(15),
          wtp02(15),
          wtp03(15),
          wtp04(15),
          wtp05(15),
          wtp06(15),
          wtp07(15),
          wtp08(15),
          wtp09(15),
          wtp10(15),
          wtp11(15),
          wtp12(15),
          type(10),
          reson(02),
          status(1),
          text(50),
          zdate(10),    "created on
          uname(12),    "uname
          ddate(10),    "doc date
          do_num(10),   "doc number
          versi(3),
       END OF it_data.

*----- Internal Table error
DATA : BEGIN OF it_error OCCURS 0,
          posid(24),
          ayear(4),
          gjahr(4),
          gubun(1),
          tot(15),
       END OF it_error.

*----- Global Variable
DATA: g_pagno(4),  g_lines TYPE i,
      g_position   LIKE  sy-linno,      " Total Page Position
      g_length(10),
      g_cancel,
      g_prctr LIKE cobl-prctr,
      g_gsber LIKE anlz-gsber.
*----- Constant
DATA: g_bukrs LIKE t001-bukrs VALUE 'KRA',
      g_type  LIKE rlgrap-filetype VALUE 'DAT',
      wa_test,
      wa_subrc,
      wa_seq LIKE ztfi_im_num-seq.
DATA : wa_sgtext LIKE bpbk-sgtext,
       wa_cnt TYPE i,
       wa_s_cnt TYPE i,
       wa_e_cnt TYPE i.
DATA: l_kostl LIKE impr-kostl.
*--------------------------------------------------------------*
*----- Selection-Screen
*--------------------------------------------------------------*
PARAMETERS : p_upfile LIKE rlgrap-filename OBLIGATORY
             DEFAULT 'c:\temp\ar2.txt'.
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
PARAMETERS : p_uname  LIKE ztfi_imfm-uname,
             p_zdate  LIKE ztfi_imfm-zdate DEFAULT sy-datum,
             p_ippos  LIKE ripasw-ippos DEFAULT '1',
** ON 03/07/14 add Version
             p_versi  LIKE imavz-versi DEFAULT '0'.
** End
SELECTION-SCREEN END OF BLOCK b01.
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-003.

PARAMETERS : p_r1    RADIOBUTTON GROUP gr1,
             p_r2    RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-004.
PARAMETERS : p_r3    RADIOBUTTON GROUP gr2,
             p_r4    RADIOBUTTON GROUP gr2.
SELECTION-SCREEN END OF BLOCK b03.

*--------------------------------------------------------------*
*
*--------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upfile.
  PERFORM f4_p_upfile.

AT SELECTION-SCREEN.
  CHECK sy-ucomm = 'ONLI'.
  PERFORM read_data USING g_lines.

START-OF-SELECTION.
  IF p_r1 = 'X'.
    PERFORM monthly_upload.
  ELSE.
    PERFORM doc_reason_update.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F4_P_UPFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_p_upfile.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
*     def_path         = p_upfile  "* File Name
      mask             = ',*.xls.'
      mode             = 'O'
    IMPORTING
      filename         = p_upfile
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

ENDFORM.                    " F4_P_UPFILE

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_LINES  text
*----------------------------------------------------------------------*
FORM read_data USING    l_lines.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_upfile
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 100
      i_end_row               = 30000
    TABLES
      intern                  = iexcel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    WRITE: / 'EXCEL UPLOAD FAILED ', sy-subrc.
  ELSE.

    SORT iexcel BY row col.
    LOOP AT iexcel.
      IF iexcel-row = 1.
        CONTINUE.
      ENDIF.
      tind = iexcel-col.
      CONCATENATE 'DATA_TAB-VALUE_' tind INTO zwfeld.
      ASSIGN (zwfeld) TO <fs1>.
      <fs1> = iexcel-value.
      AT END OF row.
        it_data-posid  = data_tab-value_0001.
        it_data-ayear  = data_tab-value_0002.
        it_data-seq    = data_tab-value_0003.
        it_data-gjahr  = data_tab-value_0004.
        it_data-gubun  = data_tab-value_0005.
        it_data-prnam  = data_tab-value_0006.
        it_data-kostl  = data_tab-value_0007.
        it_data-twaer  = data_tab-value_0008.
        it_data-tot    = data_tab-value_0009.
        it_data-wtp01  = data_tab-value_0010.
        it_data-wtp02  = data_tab-value_0011.
        it_data-wtp03  = data_tab-value_0012.
        it_data-wtp04  = data_tab-value_0013.
        it_data-wtp05  = data_tab-value_0014.
        it_data-wtp06  = data_tab-value_0015.
        it_data-wtp07  = data_tab-value_0016.
        it_data-wtp08  = data_tab-value_0017.
        it_data-wtp09  = data_tab-value_0018.
        it_data-wtp10  = data_tab-value_0019.
        it_data-wtp11  = data_tab-value_0020.
        it_data-wtp12  = data_tab-value_0021.
        it_data-type   = data_tab-value_0022.
        it_data-reson  = data_tab-value_0023.
        it_data-status = data_tab-value_0024.
        it_data-text   = data_tab-value_0025.
        it_data-zdate  = data_tab-value_0026.
        it_data-uname  = data_tab-value_0027.
        it_data-ddate  = data_tab-value_0028.
        it_data-do_num = data_tab-value_0029.
** ON 03/07/14 add Version
        it_data-versi = p_versi.
** End
        APPEND it_data.
        CLEAR data_tab.
      ENDAT.
    ENDLOOP.
  ENDIF.

  DATA $ix TYPE i.

  LOOP AT it_data.
    $ix = sy-tabix.
    PERFORM fill_data USING it_data-kostl it_data-kostl.
    IF it_data-status IS INITIAL.
      it_data-status = 'A'.
    ENDIF.
    MODIFY it_data INDEX $ix.
  ENDLOOP.


*  REFRESH : it_data.
*  CLEAR : it_data, it_data[], g_cancel, wa_subrc.
*  CALL FUNCTION 'UPLOAD'
*       EXPORTING
*            filename = p_upfile
*            filetype = g_type
*       IMPORTING
*            filesize = g_length
*            cancel   = g_cancel
*       TABLES
*            data_tab = it_data.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    MESSAGE s000(zmfi) WITH 'Upload error'.
*    EXIT.
*    wa_subrc = 'X'.
*  ENDIF.
*
*  LOOP AT it_data.
*    PERFORM fill_data USING it_data-kostl it_data-kostl.
*    MODIFY it_data.
*  ENDLOOP.
ENDFORM.                    " READ_DATA
*&--------------------------------------------------------------------*
*
*&--------------------------------------------------------------------*
FORM fill_data USING    u_field
               CHANGING c_field.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = u_field
    IMPORTING
      output = c_field.
*get cost center
  SELECT SINGLE kostl INTO it_data-kostl
      FROM impr
       WHERE gjahr EQ it_data-gjahr
         AND posid EQ it_data-posid.


ENDFORM.                    " FILL_DATA
*&---------------------------------------------------------------------*
*&      Form  get_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_number.
  CLEAR ztfi_imfm-seq.
  CLEAR wa_seq.
  SELECT SINGLE MAX( seq ) INTO wa_seq
*  FROM ztfi_im_num
  FROM ztfi_imfm
  WHERE posid = it_data-posid
  AND   gjahr = it_data-ayear.

  wa_seq = wa_seq + 1.

  MOVE it_data-posid    TO   ztfi_im_num-posid.
  MOVE it_data-ayear    TO   ztfi_im_num-gjahr.
  MOVE wa_seq           TO   ztfi_im_num-seq.
  MODIFY ztfi_im_num.
ENDFORM.                    " get_number
*&---------------------------------------------------------------------*
*&      Form  monthly_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monthly_upload.
  IF p_r3 EQ 'X'.
    LOOP AT it_data.
      CHECK it_data-posid <> ' '.
*--number ranges

      IF it_data-gubun = '1'.
        wa_seq = '0000'.
      ELSE.
        PERFORM get_number.
      ENDIF.

      MOVE wa_seq    TO  ztfi_imfm-seq.
*---
*      SELECT SINGLE * FROM impr
*        WHERE posid = it_data-posid.
      SELECT SINGLE * FROM imak
        WHERE posid = it_data-posid.
      CHECK sy-subrc = 0.
      MOVE : it_data-posid   TO ztfi_imfm-posid,
             it_data-ayear   TO ztfi_imfm-ayear,
             wa_seq          TO ztfi_imfm-seq,
             it_data-gjahr   TO ztfi_imfm-gjahr,
             it_data-gubun   TO ztfi_imfm-gubun,
             it_data-prnam   TO ztfi_imfm-prnam,
             imak-posnr      TO ztfi_imfm-posnr,
*             it_data-kostl   TO ztfi_imfm-kostl,
             it_data-twaer   TO ztfi_imfm-twaer,
             it_data-tot     TO ztfi_imfm-tot,
             it_data-wtp01   TO ztfi_imfm-wtp01,
             it_data-wtp02   TO ztfi_imfm-wtp02,
             it_data-wtp03   TO ztfi_imfm-wtp03,
             it_data-wtp04   TO ztfi_imfm-wtp04,
             it_data-wtp05   TO ztfi_imfm-wtp05,
             it_data-wtp06   TO ztfi_imfm-wtp06,
             it_data-wtp07   TO ztfi_imfm-wtp07,
             it_data-wtp08   TO ztfi_imfm-wtp08,
             it_data-wtp09   TO ztfi_imfm-wtp09,
             it_data-wtp10   TO ztfi_imfm-wtp10,
             it_data-wtp11   TO ztfi_imfm-wtp11,
             it_data-wtp12   TO ztfi_imfm-wtp12,
             it_data-type    TO ztfi_imfm-type,
             it_data-reson   TO ztfi_imfm-reson,
             it_data-status  TO ztfi_imfm-status,
** On 03/10/14
             p_zdate   TO ztfi_imfm-zdate,
             sy-tcode TO ztfi_imfm-text,
             p_uname  TO ztfi_imfm-uname,
             p_zdate   TO ztfi_imfm-ddate,
*             it_data-zdate   TO ztfi_imfm-zdate,
*             it_data-text    TO ztfi_imfm-text,
*             it_data-uname   TO ztfi_imfm-uname,
*             it_data-ddate   TO ztfi_imfm-ddate,
             it_data-do_num  TO ztfi_imfm-belnr.
** ON 03/07/14 add Version
      ztfi_imfm-versi = p_versi.
** End
      INSERT ztfi_imfm.
      IF sy-subrc EQ 0.
        WRITE:/ it_data-posid,
                it_data-ayear,
                wa_seq,
                it_data-gjahr,
                it_data-gubun,
                'Successfully Updated.'.
      ELSE.
        WRITE:/ it_data-posid,
                it_data-ayear,
                wa_seq,
                it_data-gjahr,
                it_data-gubun,
                'Update failed.'.
      ENDIF.
    ENDLOOP.
  ELSEIF p_r4 EQ 'X'.
    LOOP AT it_data.

      SELECT SINGLE * FROM ztfi_imfm
       WHERE  posid = it_data-posid
         AND  ayear = it_data-ayear
         AND  seq   = it_data-seq
         AND  gjahr = it_data-gjahr
         AND  gubun = it_data-gubun.

      IF sy-subrc = 0.
        MOVE :
*        it_data-kostl   TO ztfi_imfm-kostl,
        it_data-twaer   TO ztfi_imfm-twaer,
        it_data-tot     TO ztfi_imfm-tot,
        it_data-wtp01   TO ztfi_imfm-wtp01,
        it_data-wtp02   TO ztfi_imfm-wtp02,
        it_data-wtp03   TO ztfi_imfm-wtp03,
        it_data-wtp04   TO ztfi_imfm-wtp04,
        it_data-wtp05   TO ztfi_imfm-wtp05,
        it_data-wtp06   TO ztfi_imfm-wtp06,
        it_data-wtp07   TO ztfi_imfm-wtp07,
        it_data-wtp08   TO ztfi_imfm-wtp08,
        it_data-wtp09   TO ztfi_imfm-wtp09,
        it_data-wtp10   TO ztfi_imfm-wtp10,
        it_data-wtp11   TO ztfi_imfm-wtp11,
        it_data-wtp12   TO ztfi_imfm-wtp12,
        it_data-type    TO ztfi_imfm-type,
        it_data-reson   TO ztfi_imfm-reson,
        it_data-status  TO ztfi_imfm-status,
** On 03/10/14
             p_zdate   TO ztfi_imfm-zdate,
             sy-tcode TO ztfi_imfm-text,
             p_uname  TO ztfi_imfm-uname,
             p_zdate   TO ztfi_imfm-ddate,
*        it_data-text    TO ztfi_imfm-text,
*        it_data-uname   TO ztfi_imfm-uname,
*        it_data-ddate   TO ztfi_imfm-ddate,
*        it_data-zdate   TO ztfi_imfm-zdate,
** End on 03/10/14
        it_data-do_num  TO ztfi_imfm-belnr,
** ON 03/07/14 add Version
        p_versi  TO ztfi_imfm-versi.
** End
        MODIFY ztfi_imfm FROM ztfi_imfm.

        IF sy-subrc EQ 0.
          WRITE:/ it_data-posid,
                  it_data-ayear,
                  wa_seq,
                  it_data-gjahr,
                  it_data-gubun,
                  'Successfully Updated.'.
        ELSE.
          WRITE:/ it_data-posid,
                  it_data-ayear,
                  wa_seq,
                  it_data-gjahr,
                  it_data-gubun,
                  'Update failed.'.
        ENDIF.
      ELSE.
        WRITE:/ it_data-posid,
                it_data-ayear,
                wa_seq,
                it_data-gjahr,
                it_data-gubun,
                'Incorrect record in file.'.
      ENDIF.
    ENDLOOP.


  ENDIF.
ENDFORM.                    " monthly_upload
*&---------------------------------------------------------------------*
*&      Form  doc_reason_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM doc_reason_update.
  CLEAR : wa_cnt.
  REFRESH : it_error.
  CLEAR   : it_error.
  DESCRIBE TABLE it_data LINES wa_cnt.
  LOOP AT it_data.
    PERFORM update_bpbk USING it_data-posid
                              it_data-ayear
                              it_data-do_num
                              it_data-gubun
                              it_data-reson
                              it_data-text.
  ENDLOOP.

  WRITE : /'Result'.
  WRITE : /'------'.
  SKIP 2.
  WRITE : /'Total   Count :' , wa_cnt,
          /'Success Count : ', wa_s_cnt,
          /'Error   Count :' , wa_e_cnt.
  SKIP 2.
  LOOP AT it_error.
    AT NEW posid.
      WRITE : /'Error List'.
      WRITE : /'----------'.
    ENDAT.
    WRITE : / it_error-posid,
              it_error-ayear,
              it_error-gubun.
  ENDLOOP.
ENDFORM.                    " doc_reason_update
*&---------------------------------------------------------------------*
*&      Form  update_bpbk
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA_DO_NUM  text
*----------------------------------------------------------------------*
FORM update_bpbk USING    u_posid
                          u_ayear
                          u_do_num
                          u_gubun
                          u_reson
                          u_text.
  CLEAR : wa_sgtext.
  CONCATENATE u_gubun u_reson u_text INTO wa_sgtext.
  UPDATE bpbk SET sgtext = wa_sgtext
  WHERE belnr = u_do_num.
  IF sy-subrc = 0.
    wa_s_cnt = wa_s_cnt + 1.
  ELSE.
    wa_e_cnt = wa_e_cnt + 1.
    MOVE  u_posid TO it_error-posid.
    MOVE  u_ayear TO it_error-ayear.
    MOVE  u_gubun TO it_error-gubun.
    APPEND it_error.
    CLEAR  it_error.
  ENDIF.
ENDFORM.                    " update_bpbk
