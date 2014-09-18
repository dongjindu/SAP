REPORT Z_PAYR_CLEAR .

tables: payr.


parameters: p_gjahr like payr-gjahr.

select-options: s_PRIDT for payr-PRIDT,
                s_chect for payr-CHECT,
                s_IREFE for payr-IREFE.

parameters: p_run as checkbox.


select * from payr
  where gjahr = p_gjahr
    and pridt in s_pridt
    and chect in s_chect
    and irefe in s_irefe
    and irefe <> space.

  write:/ payr-CHECT, payr-pridt, ' status: ', payr-IREFE.

  clear: payr-IREFE.
  if p_run = 'X'.
    update payr.
    write: '... cleared !'.
  endif.
endselect.
