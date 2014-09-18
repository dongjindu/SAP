FUNCTION ZIM_CHANGE_DOCUMENT_LC.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTMLCHD) LIKE  ZTMLCHD STRUCTURE  ZTMLCHD OPTIONAL
*"     REFERENCE(O_ZTMLCHD) LIKE  ZTMLCHD STRUCTURE  ZTMLCHD OPTIONAL
*"     REFERENCE(N_ZTMLCSG2) LIKE  ZTMLCSG2 STRUCTURE  ZTMLCSG2
*"       OPTIONAL
*"     REFERENCE(O_ZTMLCSG2) LIKE  ZTMLCSG2 STRUCTURE  ZTMLCSG2
*"       OPTIONAL
*"     REFERENCE(N_ZTMLCSG910) LIKE  ZTMLCSG910 STRUCTURE  ZTMLCSG910
*"       OPTIONAL
*"     REFERENCE(O_ZTMLCSG910) LIKE  ZTMLCSG910 STRUCTURE  ZTMLCSG910
*"       OPTIONAL
*"     REFERENCE(N_ZTREQHD) LIKE  ZTREQHD STRUCTURE  ZTREQHD OPTIONAL
*"     REFERENCE(O_ZTREQHD) LIKE  ZTREQHD STRUCTURE  ZTREQHD OPTIONAL
*"     REFERENCE(N_ZTREQST) LIKE  ZTREQST STRUCTURE  ZTREQST OPTIONAL
*"     REFERENCE(O_ZTREQST) LIKE  ZTREQST STRUCTURE  ZTREQST OPTIONAL
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : it_CdtXT.
   clear : it_CdtXT.
   objecid = N_ZTreqst+3(15).

   CALL FUNCTION 'ZILCDOC_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTMLCHD          =    N_ZTMLCHD
            O_ZTMLCHD          =    O_ZTMLCHD
            UPD_ZTMLCHD        =    UPD_CHNGIND
            N_ZTMLCSG2         =    N_ZTMLCSG2
            O_ZTMLCSG2         =    O_ZTMLCSG2
            UPD_ZTMLCSG2       =    UPD_CHNGIND
            N_ZTMLCSG910       =    N_ZTMLCSG910
            O_ZTMLCSG910       =    O_ZTMLCSG910
            UPD_ZTMLCSG910     =    UPD_CHNGIND
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
            UPD_ICDTXT_ZILCDOC      = ''
      tables
            ICDTXT_ZILCDOC     =    it_CdtXT.
*            xztreqorj          =    it_ztreqorj
*            yztreqorj          =    it_ztreqorj_old.


ENDFUNCTION.
