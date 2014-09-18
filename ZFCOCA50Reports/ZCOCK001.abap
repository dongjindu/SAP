*
* Andy
*
* Update valid date of cost est.
*

REPORT ZCOCK001 .

tables: keko.

parameters: MATNR like  keko-MATNR  OBLIGATORY,
            P_BWKEY like keko-BWKEY obligatory,
            kadat like  keko-kadat  OBLIGATORY.

parameters: pfr as checkbox default ' ',
            pfrdt like  keko-kadat.
parameters: pto as checkbox default ' ',
            ptodt like  keko-kadat.


parameters: ptest as checkbox default 'X'.

* This report makes really changes in the system =>
* Check that the current user has debug authorization.
  AUTHORITY-CHECK OBJECT 'S_DEVELOP'
    ID 'DEVCLASS' DUMMY
    ID 'OBJTYPE'  FIELD 'DEBUG'
    ID 'OBJNAME'  DUMMY
    ID 'P_GROUP'  DUMMY
    ID 'ACTVT'    FIELD '03'.
  IF  SY-SUBRC <> 0.
    MESSAGE E895(M7) WITH 'Sorry, no authorization'.
  ENDIF.

select single * from keko
  where matnr = matnr
    and BWKEY = p_BWKEY
    and kadat = kadat.

check sy-subrc = 0.

write:/ keko-matnr, keko-kadat.

if pfr = 'X'.
  keko-kadat = pfrdt.
endif.

if pto = 'X'.
  keko-BIDAT = ptodt.
endif.

if ptest = space.
  update keko  from keko.
  if sy-subrc = 0.
    write:/ '... updated'.
  endif.
endif.
