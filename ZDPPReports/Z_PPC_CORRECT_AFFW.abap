REPORT Z_PPC_CORRECT_AFFW .
PARAMETERS: TEST TYPE XFELD AS CHECKBOX DEFAULT 'X'.
*----------------------------------------------------------------------*
* Correction report for DI backflush (PPC)                             *
* only used for APO-DI product process confirmation                    *
* This report corrects the field FLG_ORIG in the table AFFW            *
* the FLG_ORIG shoulb be always 1 if the AUTYP = 5 (repetitive man.)   *
* reason for the failure could be a wong SPAU                          *
*----------------------------------------------------------------------*
data: lt_ppc_reshdr like ppc_res_hdr occurs 0,
      lt_affw like affw occurs 0.
field-symbols <fs> like affw.

if test is initial.
* lock the entries
  CALL FUNCTION 'ENQUEUE_ESAFFW'
   EXPORTING
     MODE_AFFW            = 'E'
     MANDT                = SY-MANDT
     _SCOPE               = '2'
*     _WAIT                = ' '
   EXCEPTIONS
     FOREIGN_LOCK         = 1
     SYSTEM_FAILURE       = 2
     OTHERS               = 3.
  if sy-subrc ne 0.
    MESSAGE E167(RM).
  endif.
endif.

* check the entries
Select * into table lt_affw from AFFW where FLG_ORIG eq space and
                                            autyp = '05'.
select * from ppc_res_hdr into table lt_ppc_reshdr
         for all entries in lt_affw
         where rsnum = lt_affw-rsnum.
sort lt_affw by rsnum.
* check entries in the PPC_RES_HDR
loop at lt_affw assigning <fs>.
  read table lt_ppc_reshdr with key rsnum = <fs>-rsnum
             binary search transporting no fields.
  if sy-subrc ne 0.
    delete lt_affw.
  endif.
endloop.

if test is initial.
  loop at lt_affw  assigning <fs>.
    update AFFW SET FLG_ORIG = '1'
                where weblnr = <fs>-weblnr and
                      weblpos = <fs>-weblpos.
    COMMIT WORK.
  endloop.
* unlock
  CALL FUNCTION 'DEQUEUE_ESAFFW'
   EXPORTING
     MODE_AFFW       = 'E'
     MANDT           = SY-MANDT.
endif.

* protocol
write: /1 'WEBLNR',
       12 'WEBLPOS',
       21 'RSNUM',
       32 'RSPOS',
       38 'ERSDA',
       48 'ERNAM',
       61 'LAEDA',
       72 'AENAM',
       85 'MATNR'.

loop at lt_affw assigning <fs>
                where flg_orig ne '1'.
  write: /1 <fs>-weblnr,
            <fs>-weblpos,
            <fs>-rsnum,
            <fs>-rspos,
            <fs>-ersda,
            <fs>-ernam,
            <fs>-laeda,
            <fs>-aenam,
            <fs>-matnr.
endloop.
