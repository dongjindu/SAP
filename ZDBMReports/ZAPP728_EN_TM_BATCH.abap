*&--------------------------------------------------------------------
*& REPORT                 : ZAPP728_EN_TM_BATCH
*& Author                 : WSKIM
*& Creation Date          : 03/16/2005
*& Specification By       :
*& Pattern                : Report 1-1 1
*& Development Request No :
*& Addl documentation     :
*& Description            :  Update at ZTBM_FSC_CRE_INF
*& Modification Log
*& Date       Developer    Request ID      Description
*&
*&--------------------------------------------------------------------
REPORT zapp728_en_tm_batch MESSAGE-ID zdbm
                           LINE-COUNT 65
                           LINE-SIZE  80.

TABLES : mara,ztbm_fsc_cre_inf.
DATA : BEGIN OF it_mara OCCURS 0,
        matnr LIKE mara-matnr,
        maktx LIKE makt-maktx,
       END OF it_mara.
DATA : w_int TYPE i.
*----------------------------------------------------------------------
* INITIALIZATION
*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
*---------------------------------------------------------------------
*    M   A   I   N
*---------------------------------------------------------------------
START-OF-SELECTION.
*  IF sy-batch = 'X'.
  PERFORM read_data.
*  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  REFRESH it_mara.
  SELECT a~matnr b~maktx INTO TABLE it_mara
       FROM mara AS a INNER JOIN makt AS b
        ON a~matnr = b~matnr
        WHERE ( a~matnr LIKE 'EN_%' OR
                a~matnr LIKE 'TM_%' )
           AND b~spras EQ 'EN'.
*UPDATE
  DESCRIBE TABLE it_mara LINES w_int.
  IF w_int <> 0.
    PERFORM update.
  ENDIF.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update.
  WRITE : / 'Record count :', w_int ,'EA'.
  WRITE : /1(18) 'ITEM', 20(20) 'Description',42(20) 'value'.
  ULINE.
  LOOP AT it_mara.
    MOVE : it_mara-matnr       TO ztbm_fsc_cre_inf-item,
           it_mara-maktx       TO ztbm_fsc_cre_inf-text,
           it_mara-matnr+5(13) TO ztbm_fsc_cre_inf-valu1.
    MODIFY ztbm_fsc_cre_inf.
    WRITE : /1(18) ztbm_fsc_cre_inf-item,
             20(20) ztbm_fsc_cre_inf-text,
             42(20) ztbm_fsc_cre_inf-valu1.
    CLEAR: ztbm_fsc_cre_inf,it_mara.
  ENDLOOP.
 ULINE.
ENDFORM.                    " UPDATE
