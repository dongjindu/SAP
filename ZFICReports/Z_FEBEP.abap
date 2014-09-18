REPORT Z_FEBEP .
* for correct bank statement error.
* andy choi
tables: febep.

parameters: p_kukey like febep-kukey,
            p_esnum like febep-esnum.

parameters: p_epvoz like febep-epvoz,
            p_upd1  as checkbox.

parameters: p_chect like febep-chect,
            p_upd2  as checkbox.



select single * from febep
  where kukey = p_kukey
    and esnum = p_esnum.


write:/ 'Current...'.
write:/ febep-vgext, febep-vgint, febep-chect, febep-epvoz.


if p_upd1 = 'X'.
  febep-epvoz = p_epvoz.
  update febep.
  write:/ 'Result:', sy-subrc.
endif.

if p_upd2 = 'X'.
  febep-chect = p_chect.
  update febep.
  write:/ 'Result:', sy-subrc.
endif.
