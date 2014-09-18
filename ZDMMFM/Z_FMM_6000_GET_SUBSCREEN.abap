FUNCTION z_fmm_6000_get_subscreen.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(IM_NODE_KEY) TYPE  TM_NODEKEY
*"  EXPORTING
*"     REFERENCE(EX_DYNNR) LIKE  SY-DYNNR
*"-----------------------------------------------------------------2----
  ex_dynnr = '9000'.

  DATA: node_key LIKE im_node_key.

  CLEAR: NODE_KEY. NODE_KEY = im_node_key.
  TRANSLATE node_key TO UPPER CASE.

  IF node_key = 'CHILD1'.
    ex_dynnr = '9001'.
  ELSEIF node_key = 'CHILD2'.
    ex_dynnr = '9002'.
  ELSEIF node_key = 'CHILD3'.
    ex_dynnr = '9003'.
  ELSEIF node_key = 'CHILD4'.
    ex_dynnr = '9004'.
  ELSEIF node_key = 'CHILD5'.
    ex_dynnr = '9005'.
  ELSEIF node_key = 'CHILD6'.
    ex_dynnr = '9006'.
  ENDIF.
ENDFUNCTION.
