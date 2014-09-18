*$*$                                                              $*$*
*$ Correction Inst.         0120061532 0000302780                     $*
*$                                                                  $*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 40A          To SAPKH40A25                                $*
*$  Release 40B          All Support Package Levels                   $*
*$  Release 45A          All Support Package Levels                   $*
*$  Release 45B          All Support Package Levels                   $*
*$  Release 46A          To SAPKH46A34                                $*
*$  Release 46B          All Support Package Levels                   $*
*$  Release 46C          All Support Package Levels                   $*
*$                                                                  $*
*$ Changes/Objects Not Contained in Standard SAP System               $*
*$*$                                                              $*$*
*&                                                                   *
*& Object          REPS ZACORR145
*& Object Header   PROG ZACORR145
*&                                                                   *
*& REPORT RACORR145
*&                                                                   *

*Note that it is NOT checked in the correction program,
*whether the areas have the same currency.
*You can only use the program if the currency is equal.
*You can correct the assets with the attached correction program.
*After a production run, you must still carry out a 'Recalculate values'
*(RAAFAR00) for the company code.
*Afterwards you must correct the values on the reconciliation
*accounts by using Transaction FBB1.
*(After resetting the reconciliation indicator).

*>>>> START OF INSERTION <<<<
REPORT ZACORR145 .
tables: anep, v_anepk.

parameters: par_area like t093-afaber default '25',
            fiscyear like anep-gjahr default '2001'.

select-options: compcode for anep-bukrs,
                asset for anep-anln1,
                date  for v_anepk-cpudt.
parameters: testrun default 'X'.

data: lt_anep like anep occurs 0 with header line,
      ls_anep_par like anep.


select * from v_anepk
as a
into corresponding fields of table lt_anep
where
not exists ( select * from anep
  where bukrs = a~bukrs
  and   anln1 = a~anln1
  and   anln2 = a~anln2
  and   gjahr = a~gjahr
  and   afabe = par_area
  and   lnran = a~lnran
  and   anbtr = a~anbtr )
and
  bukrs in compcode
  and gjahr = fiscyear
  and afabe = '01'
  and anln1 in asset
  and cpudt in date
  and tcode like 'M%'.

write: / sy-dbcnt.

loop at lt_anep.
  select single * from anep into ls_anep_par
         where bukrs = lt_anep-bukrs
         and   afabe = par_area
         and   anln1 = lt_anep-anln1
         and   anln2 = lt_anep-anln2
         and   gjahr = lt_anep-gjahr
         and   lnran = lt_anep-lnran.
  if lt_anep-anbtr <> ls_anep_par-anbtr.
    write: / lt_anep-bukrs, lt_anep-anln1, lt_anep-anln2, lt_anep-lnran,
             lt_anep-anbtr, ls_anep_par-anbtr.
    if testrun is initial.
       ls_anep_par-anbtr = lt_anep-anbtr.
       update anep from ls_anep_par.
    endif.
  endif.

endloop.
*>>>> END OF INSERTION <<<<<<
