*
* careful. Ask ANDY
*
REPORT z_ckmlcr .

TABLES: mbew, ckmlpp, ckmlcr, ckmlhd.
DATA:
     l_lbkum  LIKE mbew-lbkum,
     l_salk3  LIKE mbew-salk3,
     l_peinh  LIKE mbew-peinh,
     l_verpr  LIKE mbew-verpr,
     l_stprs  LIKE mbew-stprs,
     l_salkv  LIKE mbew-salkv,
     l_vprsv  LIKE mbew-vprsv.

SELECT-OPTIONS:
  p_matnr FOR  mbew-matnr.
PARAMETERS:
  p_bwkey LIKE mbew-bwkey,
  p_bwtar LIKE mbew-bwtar,

  p_run AS CHECKBOX.

DATA: i_mbew LIKE mbew OCCURS 0 WITH HEADER LINE.

SELECT * INTO TABLE i_mbew
  FROM mbew
  WHERE matnr in p_matnr
    AND bwkey = p_bwkey
    AND bwtar = p_bwtar.

LOOP AT i_mbew.
  PERFORM get_ckml.
  PERFORM write_detail.
  IF p_run = 'X'.
    PERFORM update_ckml.
  ENDIF.
endloop.

*&---------------------------------------------------------------------*
*&      Form  get_ckml
*&---------------------------------------------------------------------*
FORM get_ckml.
  SELECT SINGLE * FROM ckmlcr
    WHERE kalnr = i_mbew-kaln1
      AND bdatj = i_mbew-lfgja
      AND poper = i_mbew-lfmon.

  SELECT SINGLE * FROM ckmlpp
    WHERE kalnr = i_mbew-kaln1
      AND bdatj = i_mbew-lfgja
      AND poper = i_mbew-lfmon.
ENDFORM.                    " get_ckml
*&---------------------------------------------------------------------*
*&      Form  write_detail
*&---------------------------------------------------------------------*
FORM write_detail.
  write:/ '****', i_mbew-matnr.
  WRITE:/ 'HEAD      ',
         'mbew-lbkum,     mbew-salk3,    mbew-peinh,     mbew-verpr,  ',
          'mbew-stprs,     mbew-salkv'.


  WRITE:/ 'MBEW',
          i_mbew-lbkum,
          i_mbew-salk3,
          i_mbew-peinh,
          i_mbew-verpr,
          i_mbew-stprs,
          i_mbew-salkv,
          i_mbew-vprsv.

  WRITE:/ 'CKML',
          ckmlpp-lbkum,
          ckmlcr-salk3,
          ckmlcr-peinh,
          ckmlcr-pvprs,
          ckmlcr-stprs,
          ckmlcr-salkv,
          ckmlcr-vprsv.

  l_lbkum = i_mbew-lbkum - ckmlpp-lbkum.
  l_salk3 = i_mbew-salk3 - ckmlcr-salk3.
  l_peinh = i_mbew-peinh - ckmlcr-peinh.
  l_verpr = i_mbew-verpr - ckmlcr-pvprs.
  l_stprs = i_mbew-stprs - ckmlcr-stprs.
  l_salkv = i_mbew-salkv - ckmlcr-salkv.

  WRITE:/ 'DIFF',
          l_lbkum,
          l_salk3,
          l_peinh,
          l_verpr,
          l_stprs,
          l_salkv.
ENDFORM.                    " write_detail
*&---------------------------------------------------------------------*
*&      Form  update_ckml
*&---------------------------------------------------------------------*
FORM update_ckml.
* This report makes really changes in the system =>
* Check that the current user has debug authorization.
  AUTHORITY-CHECK OBJECT 'S_DEVELOP'
    ID 'DEVCLASS' DUMMY
    ID 'OBJTYPE'  FIELD 'DEBUG'
    ID 'OBJNAME'  DUMMY
    ID 'P_GROUP'  DUMMY
    ID 'ACTVT'    FIELD '03'.

  IF  sy-subrc <> 0.
    WRITE:/ 'Sorry, no authorization'.
  ELSE.
    ckmlpp-lbkum = i_mbew-lbkum.
    ckmlcr-salk3 = i_mbew-salk3.
    ckmlcr-pvprs = i_mbew-verpr.
    ckmlcr-stprs = i_mbew-stprs.
    ckmlcr-salkv = i_mbew-salkv.
    UPDATE: ckmlpp, ckmlcr.
    WRITE:/ '***CKMLPP,CR is updated'.
  ENDIF.

ENDFORM.                    " udate_ckml
