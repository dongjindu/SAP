************************************************************************
* Program Name      : ZFII_UPDATE_ZTCO_MAT
* Author            : Valerian Utama
* Creation Date     : 06/16/2011
* Specifications By : Michael Yoon
* Pattern           :
* Development Request No: UD1K952048
* Add documentation :
* Description       : Update/Delete table ZTCO_MAT
*
* Modifications Log
* Date       Developer   Request ID    Description
* 06/16/2011 Valerian    UD1K952048    Initial Program Development
*
************************************************************************
REPORT zfii_update_ztco_mat.

DATA: t_ztco_mat LIKE ztco_mat OCCURS 0 WITH HEADER LINE,
      g_ans(1)   TYPE c,
      g_line(50) TYPE c.

DATA: lt_deleted LIKE ztco_mat_deleted OCCURS 0 WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_matnr FOR t_ztco_mat-matnr,
                s_mtart FOR t_ztco_mat-mtart,
                s_matkl FOR t_ztco_mat-matkl.

SELECTION-SCREEN SKIP.
PARAMETERS: r_upload RADIOBUTTON GROUP gr1,
            r_delete RADIOBUTTON GROUP gr1.

SELECTION-SCREEN END OF BLOCK blk1.

AT SELECTION-SCREEN.

START-OF-SELECTION.
  CASE 'X'.
    WHEN r_upload.
      g_line = text-m03.
    WHEN r_delete.
      g_line = text-m06.
  ENDCASE.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      textline1      = g_line
      textline2      = text-m04
      titel          = text-m05
      cancel_display = ' '
    IMPORTING
      answer         = g_ans.

  CHECK g_ans = 'J'.

  CASE 'X'.
    WHEN r_upload.
      SELECT a~matnr
             b~maktx
             a~mtart
             a~matkl
             a~zeinr AS zgrp1
             a~zeiar AS zgrp2
      INTO CORRESPONDING FIELDS OF TABLE t_ztco_mat
      FROM mara AS a JOIN makt AS b
                       ON a~matnr = b~matnr
      WHERE a~matnr IN s_matnr
        AND a~mtart IN s_mtart
        AND a~matkl IN s_matkl
        AND b~spras = sy-langu.

      IF NOT t_ztco_mat[] IS INITIAL.
** By Furong on 02/21/14 (
          LOOP AT t_ztco_mat.
            t_ztco_mat-zch_user = sy-uname.
            t_ztco_mat-zch_date = sy-datum.
            t_ztco_mat-zch_time = sy-uzeit.
            MODIFY t_ztco_mat TRANSPORTING
             zch_user zch_date zch_time.
          ENDLOOP.
** ) End
        MODIFY ztco_mat FROM TABLE t_ztco_mat.
        COMMIT WORK.
        WRITE: / text-m01, sy-dbcnt.
      ENDIF.
    WHEN r_delete.

** By Furong on 02/21/14 (
      SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_deleted
        FROM ztco_mat
      WHERE matnr IN s_matnr
        AND mtart IN s_mtart
        AND matkl IN s_matkl.

      IF sy-subrc = 0.
        DELETE FROM ztco_mat WHERE matnr IN s_matnr
                             AND mtart IN s_mtart
                             AND matkl IN s_matkl.
        COMMIT WORK.
        IF sy-subrc = 0.
          LOOP AT lt_deleted.
            lt_deleted-zdel_user = sy-uname.
            lt_deleted-zdel_date = sy-datum.
            lt_deleted-zdel_time = sy-uzeit.
            MODIFY lt_deleted TRANSPORTING
             zdel_user zdel_date zdel_time.
          ENDLOOP.
          MODIFY ztco_mat_deleted FROM TABLE lt_deleted.
        ENDIF.
*      DELETE FROM ztco_mat WHERE matnr IN s_matnr
*                             AND mtart IN s_mtart
*                             AND matkl IN s_matkl.
*      COMMIT WORK.
** ) End
        WRITE: / text-m02, sy-dbcnt.
      ENDIF.
  ENDCASE.
