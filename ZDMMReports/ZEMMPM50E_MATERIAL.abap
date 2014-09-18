************************************************************************
* Program Name      : ZEMMPM50E_MATERIAL
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.08.25.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K912027
* Addl Documentation:
* Description       : Creation Z-Table for Material Master
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.08.25.     Sung-Tae Lim     UD1K912027     Initial Coding
* 5/4/2012        t-code is deleted by APM Monitoring
*
************************************************************************

REPORT zemmpm50e_material NO STANDARD PAGE HEADING
                          LINE-SIZE 132
                          LINE-COUNT 64(1)
                          MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

TABLES : ztmm_mara,
         qmat,
         mbew.


**--- Internal Tables
DATA : it_itab LIKE ztmm_mara OCCURS 0 WITH HEADER LINE.

DATA : it_temp LIKE it_itab OCCURS 0 WITH HEADER LINE.


**--- Variables


**--- Constants
CONSTANTS : c_fert LIKE mara-mtart VALUE 'FERT',
            c_halb LIKE mara-mtart VALUE 'HALB',
            c_roh  LIKE mara-mtart VALUE 'ROH',
            c_dispo_m02 like marc-dispo value 'M02'.


**--- Macro
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

DEFINE append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    concatenate &3 '~' &4 into w_line-info separated by space.
    append w_line to w_top_of_page.
  endif.
END-OF-DEFINITION.

DEFINE append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
END-OF-DEFINITION.


**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_matnr FOR mara-matnr,
                 s_ersda FOR mara-ersda,
                 s_ernam FOR mara-ernam.
SELECTION-SCREEN END OF BLOCK block1.


**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].


**---
TOP-OF-PAGE.
  PERFORM top_of_page.


**---
START-OF-SELECTION.
  PERFORM get_data.


**---
END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM insert_table.
    PERFORM comment_build.
    PERFORM make_alv_grid.
  ENDIF.



**---

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*---
  CLEAR : it_itab, it_itab[], it_temp, it_temp[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_temp
           FROM mara AS a INNER JOIN marc AS b
             ON a~mandt EQ b~mandt
            AND a~matnr EQ b~matnr
          WHERE a~matnr IN s_matnr
            AND a~mtart IN (c_fert, c_halb, c_roh)
            AND a~ersda IN s_ersda
            AND a~ernam IN s_ernam
            AND a~lvorm EQ space
            AND NOT exists ( SELECT * FROM ztmm_mara
                                     WHERE matnr EQ b~matnr
                                       AND werks EQ b~werks ).

*---
*--- insert by stlim (2004/10/26)
  DELETE it_temp WHERE
                 NOT ( ( mtart EQ c_fert AND ntgew EQ 0 ) OR
                       ( mtart EQ c_halb AND
                         ( matnr+0(1) EQ '6' OR matnr+0(1) EQ '7' ) AND
                         ( matnr+4(1) NE 'E' AND matnr+4(1) NE 'M' ) AND
                         ntgew EQ 0 ) OR
                       ( mtart EQ c_roh  AND matkl EQ 'INIT' ) ).
*--- end of insert

*--- blocked by stlim (2004/10/26)
*  DELETE it_temp WHERE NOT ( ( mtart EQ c_fert AND ntgew EQ 0 ) OR
*                             ( mtart EQ c_halb AND matnr+4(1) NE 'E'
*                                               AND ntgew EQ 0 ) OR
*                             ( mtart EQ c_roh  AND matkl EQ 'INIT' ) ).
*--- end of block

*---
  DATA : BEGIN OF it_qmat OCCURS 0,
           art LIKE qmat-art,
           aktiv LIKE qmat-aktiv,
         END OF it_qmat.

  LOOP AT it_temp.
    MOVE-CORRESPONDING it_temp TO it_itab.
*---
    IF it_temp-profl EQ 'K'.
      it_itab-duedt = it_itab-mstde - 63.
    ELSEIF it_temp-profl EQ 'V'.
      it_itab-duedt = it_itab-mstde - 7.
    ENDIF.
*---
    CLEAR : mbew.
    SELECT SINGLE bklas INTO it_itab-bklas
                        FROM mbew
                       WHERE matnr EQ it_itab-matnr
                         AND bwkey EQ it_itab-werks.
*---
    CLEAR : it_qmat, it_qmat[].
    SELECT art aktiv INTO CORRESPONDING FIELDS OF TABLE it_qmat
                     FROM qmat
                    WHERE matnr EQ it_itab-matnr
                      AND werks EQ it_itab-werks.
    READ TABLE it_qmat INDEX 1.
    MOVE : it_qmat-art   TO it_itab-art01,
           it_qmat-aktiv TO it_itab-akti1.
    READ TABLE it_qmat INDEX 2.
    MOVE : it_qmat-art   TO it_itab-art02,
           it_qmat-aktiv TO it_itab-akti2.
*---
    it_itab-ernam = it_itab-aenam = sy-uname.
    it_itab-erdat = it_itab-aedat = sy-datum.
    it_itab-erzet = it_itab-aezet = sy-uzeit.
*--- if source = 'V', then PUR confirmation indicator : 'X'
    IF it_itab-profl EQ 'V' or it_itab-dispo eq c_dispo_m02.
      MOVE : 'X' TO it_itab-conf2.
    ENDIF.
*--- if 'HALB', move 'X' to QM indicator...
*---                        except the first digit has '6' or '7'...
    IF it_itab-mtart EQ 'HALB'.
      IF NOT ( it_itab-matnr(1) EQ '6' OR it_itab-matnr(1) EQ '7' ).
        MOVE : 'X' TO it_itab-conf4.
      ENDIF.
    ENDIF.
*--- if 'ROH' and source = 'K', then move 'X' to QM indicator...
    IF it_itab-mtart EQ 'ROH'.
      IF it_itab-profl EQ 'K'.
        MOVE : 'X' TO it_itab-conf4.
      ENDIF.
    ENDIF.
*---
    APPEND it_itab.
    CLEAR : it_itab, it_temp.
  ENDLOOP.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  insert_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_table.
*---
  INSERT ztmm_mara FROM TABLE it_itab ACCEPTING DUPLICATE KEYS.
ENDFORM.                    " insert_table

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.

ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid.
*---
  DATA : l_lines TYPE i.

  DESCRIBE TABLE it_itab LINES l_lines.

  WRITE : / l_lines, text-002.
ENDFORM.                    " make_alv_grid
