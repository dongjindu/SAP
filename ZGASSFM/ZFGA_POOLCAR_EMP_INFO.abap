FUNCTION zfga_poolcar_emp_info.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_PERNR) TYPE  P_PERNR
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  BAPIRETURN
*"  TABLES
*"      T_DATA STRUCTURE  ZESS_EMP_TM_LIST...
*"----------------------------------------------------------------------
  DATA : BEGIN OF it_emp OCCURS 0.
          INCLUDE STRUCTURE zess_emp_tm_list.
  DATA : endda LIKE t527x-endda.
  data : endda2 like pa0002-endda.
  DATA : END OF it_emp.

  SELECT  a~pernr    a~orgeh  b~orgtx b~endda  c~nachn AS lastname
          c~vorna AS firstname c~endda as endda2
    INTO CORRESPONDING FIELDS OF TABLE it_emp
  FROM pa0001 AS a  LEFT OUTER JOIN t527x AS b
                    ON a~orgeh  = b~orgeh
                    LEFT OUTER JOIN pa0002 AS c
                    ON a~pernr  = c~pernr
  WHERE a~pernr = i_pernr
    AND a~endda = '99991231'.

  DELETE it_emp WHERE endda  <> '99991231'.
  DELETE it_emp WHERE endda2 <> '99991231'.
  IF it_emp[] IS INITIAL.
    e_return-type    = 'E'.
    e_return-message = 'Employee info does Not exist'.
  ELSE.
    LOOP AT it_emp.
      MOVE-CORRESPONDING it_emp TO t_data.
      APPEND t_data.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
