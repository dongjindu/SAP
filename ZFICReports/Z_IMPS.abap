REPORT Z_IMPS .

tables: imps.
data: i_imps like imps occurs 0 with header line.

check sy-uname = 'ANDY'.

select * into table i_imps
  from imps
  where OBART = 'OR'.


*loop at i_imps.
*
*
*endloop.

loop at i_imps.
  imps = i_imps.
  imps-obart = 'IQ'.
  insert imps.

endloop.
