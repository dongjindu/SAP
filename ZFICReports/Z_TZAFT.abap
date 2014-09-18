REPORT Z_TZAFT .

tables: tzaft.

parameters: p_sanlf like tzaft-sanlf,
            p_ltx   like tzaft-ltx,
            p_run as checkbox.

select single * from tzaft
  where spras = sy-langu
    and sanlf = p_sanlf.


write:/ tzaft-sanlf, tzaft-ltx.
tzaft-ltx = p_ltx.
if p_run = 'X'.
  update tzaft.
endif.
