FUNCTION Z_K_KKB_BEWEG_SET.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_COSSA) TYPE  COSSA OPTIONAL
*"     REFERENCE(I_COSPA) TYPE  COSPA OPTIONAL
*"     REFERENCE(I_COSBA) TYPE  COSBA OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_BEWEG) TYPE  KKB_BEWEG
*"  EXCEPTIONS
*"      NO_INPUT
*"----------------------------------------------------------------------
data: begin of c_beweg,
        warenausgang type c value 'C',
        rueckmeldung type c value 'F',
        zuschlaege type c value 'I',
        sonstige type c value 'L',
        wareneingang type c value 'P',
        abrechnung type c value 'X',
        verteilung type c value 'O',
      end of c_beweg.

if i_cospa is initial
and i_cossa is initial
and i_cosba is initial.
    raise no_input.
endif.

if not i_cospa is initial.
   if i_cospa-vrgng(3) = 'KPI'.
      move c_beweg-verteilung to e_beweg.
   elseif i_cospa-vrgng(3) = 'RKU'.
    move c_beweg-sonstige to e_beweg.
   elseif i_cospa-beknz = 'A'.
      move c_beweg-abrechnung to e_beweg.
   elseif i_cospa-beknz = 'L'.
      move c_beweg-wareneingang to e_beweg.
   else.
      move c_beweg-warenausgang to e_beweg.
   endif.
elseif not I_cossa is initial.
   if i_cossa-vrgng(3) = 'KPI'.
      move c_beweg-verteilung to e_beweg.
   elseif i_cossa-beknz = 'A'.
      move c_beweg-abrechnung to e_beweg.
   elseif i_cossa-beknz = 'L'.
      move c_beweg-wareneingang to e_beweg.
   elseif i_cossa-parob(2) = 'KL'.
      move c_beweg-rueckmeldung to e_beweg.
   elseif i_cossa-parob(2) <> 'KS'.
      move c_beweg-sonstige to e_beweg.
   else.
      move c_beweg-zuschlaege to e_beweg.
   endif.
elseif not I_cosba is initial.
   if i_cosba-vrgng(3) = 'KPI'.
      move c_beweg-verteilung to e_beweg.
   elseif i_cosba-vrgng = 'SUR'.
      move c_beweg-zuschlaege to e_beweg.
   elseif i_cosba-beknz = 'A'.
      move c_beweg-abrechnung  to e_beweg.
   elseif i_cosba-beknz = 'L'.
      move c_beweg-wareneingang to e_beweg.
   elseif i_cosba-parob is initial.
      move c_beweg-warenausgang to e_beweg.
   elseif i_cosba-parob(2) = 'KL'.
      move c_beweg-rueckmeldung to e_beweg.
   elseif i_cossa-parob(2) <> 'KS'.
      move c_beweg-sonstige to e_beweg.
   else.
      move c_beweg-zuschlaege to e_beweg.
   endif.
endif.
ENDFUNCTION.
