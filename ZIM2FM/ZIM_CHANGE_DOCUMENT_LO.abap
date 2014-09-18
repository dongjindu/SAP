FUNCTION ZIM_CHANGE_DOCUMENT_LO.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTLLCHD) LIKE  ZTLLCHD STRUCTURE  ZTLLCHD
*"     REFERENCE(O_ZTLLCHD) LIKE  ZTLLCHD STRUCTURE  ZTLLCHD
*"     REFERENCE(N_ZTLLCSG23) LIKE  ZTLLCSG23 STRUCTURE  ZTLLCSG23
*"     REFERENCE(O_ZTLLCSG23) LIKE  ZTLLCSG23 STRUCTURE  ZTLLCSG23
*"     REFERENCE(N_ZTREQHD) LIKE  ZTREQHD STRUCTURE  ZTREQHD
*"     REFERENCE(O_ZTREQHD) LIKE  ZTREQHD STRUCTURE  ZTREQHD
*"     REFERENCE(N_ZTREQST) LIKE  ZTREQST STRUCTURE  ZTREQST
*"     REFERENCE(O_ZTREQST) LIKE  ZTREQST STRUCTURE  ZTREQST
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : it_CdtXT.
   clear : it_CdtXT.
   objecid = N_ZTreqst+3(15).

   CALL FUNCTION 'ZILODOC_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTLLCHD          =    N_ZTLLCHD
            O_ZTLLCHD          =    O_ZTLLCHD
            UPD_ZTLLCHD        =    UPD_CHNGIND
            N_ZTLLCSG23        =    N_ZTLLCSG23
            O_ZTLLCSG23        =    O_ZTLLCSG23
            UPD_ZTLLCSG23      =    UPD_CHNGIND
            N_ZTREQHD          =    N_ZTREQHD
            O_ZTREQHD          =    O_ZTREQHD
            UPD_ZTREQHD        =    UPD_CHNGIND
            N_ZTREQST          =    N_ZTREQST
            O_ZTREQST          =    O_ZTREQST
            UPD_ZTREQST        =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZILODOC      = ''
      tables
            ICDTXT_ZILODOC     =    it_CdtXT.


ENDFUNCTION.
