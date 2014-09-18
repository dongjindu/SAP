report z_feb_del .
*
* Andy Choi
* - Delete bank statement data
* - caution: if posting data exist, reverse it.
* - for maintenance purpose
*
* FEBKO Number Range Object (SNRO)
* SAP use function GET_SHORTKEY_FOR_FEBKO to get new KUKEY
*

tables: febko, febep, febcl, febvw, febre, t012.

select-options: s_kukey for febko-kukey.
parameters: p_run as checkbox.

data: ifebko like febko occurs 0 with header line.
data: h_dontpanic   like sy-datlo.

check not s_kukey is initial.


get parameter id 'DONTPANIC' field h_dontpanic.
if h_dontpanic <> sy-datlo.
  write:/ 'Consult to Andy'.
else.
  perform delete_entry.
endif.

*&---------------------------------------------------------------------*
*&      Form  delete_entry
*&---------------------------------------------------------------------*
form delete_entry.
select * from febko into table ifebko
         where anwnd = '0001'
           and kukey in s_kukey.


loop at ifebko.
  write:/ ifebko-kukey.

  check p_run = 'X'.

  delete from febre where kukey = ifebko-kukey.
  delete from febep where kukey = ifebko-kukey.
  delete from febcl where kukey = ifebko-kukey.
  delete from febvw where anwnd = '0001'
                      and absnd = ifebko-absnd
                      and azidt = ifebko-azidt.

  move-corresponding ifebko to febko.
  delete febko.
  write: '... deleted'.
endloop.

endform.                    " delete_entry
