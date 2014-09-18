*
* For converting data that is not managed by budget category
*  - budget category: 1 - investment cost
*  - budget category: 2 - overhead cost
*
* by Andy Choi
*
REPORT Z_IMCONV .

tables: imtp, impr,
        bptr, bpja, bpge,
        bpej, bpbk,
        aufk, imzo.

parameters: p_prnam like IMPR-PRNAM   memory id IMT obligatory,
            p_gjahr like IMPR-GJAHR   memory id gjr obligatory.
parameters: p_update as checkbox.

parameters: p_reset(1) type c.


data: iimpr like impr occurs 0 with header line.
data: iaufk like aufk occurs 0 with header line.

data: ibptr like bptr occurs 0 with header line.
data: ibpja like bpja occurs 0 with header line.
data: ibpge like bpge occurs 0 with header line.
data: ibpej like bpej occurs 0 with header line.

data: begin of iimzo occurs 0.
        include structure imzo.
data:   aufnr like aufk-aufnr,
      end of iimzo.

*check sy-UNAME = 'ANDY'.

select single * from imtp
  where PRNAM = p_prnam
    and GJAHR = p_gjahr.

* only not managed by budget category
if p_reset = space and imtp-capex <> space.
  exit.
endif.

if p_update = 'X'.
  if p_reset = 'X'.
    imtp-capex = ' '.
  else.
    imtp-capex = 'X'.
  endif.
  update imtp.
  write:/ 'IMTP ...updated RC:', sy-subrc.
endif.

select * from impr into table iimpr
  where PRNAM = p_prnam
    and GJAHR = p_gjahr.


loop at iimpr.
  select * appending table iimzo
    from imzo
    where posnr = iimpr-posnr.

endloop.

* order - pi assignment
loop at iimzo.
  select single * from aufk where objnr = iimzo-objnr.
  iimzo-aufnr = aufk-aufnr.
  modify iimzo.
  move-corresponding iimzo to imzo.

  write:/ iimzo-POSNR, iimzo-objnr, iimzo-aufnr, imzo-ippos, imzo-baprz.

  if p_update = 'X'.
    delete imzo from imzo.
    if p_reset = 'X'.
      clear: imzo-ippos, imzo-baprz.
    else.
      imzo-ippos = '1'.
      imzo-baprz = 100.
    endif.
    insert imzo. write: '...updated RC:', sy-subrc.
    endif.
endloop.

* pi
loop at iimpr.
  write:/ 'Position ID Number:', iimpr-objnr.

  select * from bptr appending table ibptr
     where objnr = iimpr-objnr.

  select * from bpge appending table ibpge
     where objnr = iimpr-objnr.

  select * from bpja appending table ibpja
     where objnr = iimpr-objnr.

  select * from bpej appending table ibpej
     where objnr = iimpr-objnr.

endloop.

check p_update = 'X'.


* delete & insert (table key)
loop at ibptr.
  bptr = ibptr.
  delete bptr from bptr.

  if p_reset = 'X'.
    clear bptr-posit.
  else.
    bptr-posit = 'IM000001'.
  endif.

  insert bptr. write:/ '..BPTR...updated RC:', sy-subrc.
endloop.

uline.
loop at ibpja.
  bpja = ibpja.
  delete bpja from bpja.

  if p_reset = 'X'.
    clear bpja-posit.
  else.
    bpja-posit = 'IM000001'.
  endif.

  insert bpja. write:/ '..BPJA...updated RC:', sy-subrc.
endloop.

loop at ibpge.
  bpge = ibpge.
  delete bpge from bpge.

  if p_reset = 'X'.
    clear bpge-posit.
  else.
    bpge-posit = 'IM000001'.
  endif.

  insert bpge. write:/ '..BPGE...updated RC:', sy-subrc.
endloop.

* just update..
loop at ibpej.
  bpej = ibpej.
  if p_reset = 'X'.
    clear bpej-posit.
  else.
    bpej-posit = 'IM000001'.
  endif.

  update bpej. write:/ '..BPEJ...updated RC:', sy-subrc.
endloop.
