*&---------------------------------------------------------------------*
*& Report  ZAPP724U_DELETE_M
*
*& Author                 : WSKIM
*& Creation Date          : 11/17/2004
*& Specification By       :
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description  : AR  Upload
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT  zapp724u_delete_m    MESSAGE-ID zmbm.

TABLES : ztbm_abxstrdt, ztbm_abxitmdt, ztbm_abxc01dt,ztbm_abxc23dt.

DATA: it_abxstrdt TYPE ztbm_abxstrdt OCCURS 0 WITH HEADER LINE,
      it_abxitmdt TYPE ztbm_abxitmdt OCCURS 0 WITH HEADER LINE,
      it_abxc01dt TYPE ztbm_abxc01dt OCCURS 0 WITH HEADER LINE,
      it_abxc23dt TYPE ztbm_abxc23dt OCCURS 0 WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

SELECT-OPTIONS : s_datum FOR ztbm_abxstrdt-zsdat.

SELECTION-SCREEN END OF BLOCK b1.


INITIALIZATION.
  PERFORM itialization.

START-OF-SELECTION.
  PERFORM delete.

END-OF-SELECTION.
  PERFORM write.
*&---------------------------------------------------------------------*
*&      Form  itialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM itialization.
  REFRESH : it_abxstrdt,it_abxitmdt,it_abxc01dt,it_abxc23dt.
ENDFORM.                    " itialization
*&---------------------------------------------------------------------*
*&      Form  delete
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete.
*1st : ZTBM_ABXSTRDT
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_abxstrdt
     FROM ztbm_abxstrdt
      WHERE zsdat IN s_datum.

  DELETE ztbm_abxstrdt FROM TABLE it_abxstrdt.
  IF sy-subrc = 0.
    MESSAGE s002 WITH text-003 'ztbm_abxstrdt'.
  ELSE.
    MESSAGE s002 WITH text-004 'ztbm_abxstrdt'.
  ENDIF.
*2nd : ZTBM_ABXITMDT
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_abxitmdt
     FROM ztbm_abxitmdt
      WHERE zsdat IN s_datum.

  DELETE ztbm_abxitmdt FROM TABLE it_abxitmdt.
  IF sy-subrc = 0.
    MESSAGE s002 WITH text-003 'ztbm_abximtdt'.
  ELSE.
    MESSAGE s002 WITH text-004 'ztbm_abximtdt'.
  ENDIF.
*3rd : ZTBM_ABXC01DT
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_abxc01dt
   FROM ztbm_abxc01dt
     WHERE zsdat IN s_datum.

  DELETE ztbm_abxc01dt FROM TABLE it_abxc01dt.
  IF sy-subrc = 0.
    MESSAGE s002 WITH text-003 'ztbm_abxc01dt'.
  ELSE.
    MESSAGE s002 WITH text-004 'ztbm_abxc01dt'.
  ENDIF.

*4th " ZTBM_ABXC23DT
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_abxc23dt
     FROM ztbm_abxc23dt
      WHERE zsdat IN s_datum.

  DELETE ztbm_abxc23dt FROM TABLE it_abxc23dt.
  IF sy-subrc = 0.
    MESSAGE s002 WITH text-003 'ztbm_abxc23dt'.
  ELSE.
    MESSAGE s002 WITH text-004 'ztbm_abxc23dt'.
  ENDIF.

ENDFORM.                    " delete
*&---------------------------------------------------------------------*
*&      Form  write
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write.

ENDFORM.                    " write
