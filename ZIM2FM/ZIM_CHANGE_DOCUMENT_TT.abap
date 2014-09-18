FUNCTION ZIM_CHANGE_DOCUMENT_TT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTTTHD) LIKE  ZTTTHD STRUCTURE  ZTTTHD
*"     REFERENCE(O_ZTTTHD) LIKE  ZTTTHD STRUCTURE  ZTTTHD
*"     REFERENCE(N_ZTREQHD) LIKE  ZTREQHD STRUCTURE  ZTREQHD
*"     REFERENCE(O_ZTREQHD) LIKE  ZTREQHD STRUCTURE  ZTREQHD
*"     REFERENCE(N_ZTREQST) LIKE  ZTREQST STRUCTURE  ZTREQST
*"     REFERENCE(O_ZTREQST) LIKE  ZTREQST STRUCTURE  ZTREQST
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : it_CdtXT.
   clear : it_CdtXT.
   objecid = N_ZTREQST+3(10).

   CALL FUNCTION 'ZITTDOC_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTTTHD           =    N_ZTTTHD
            O_ZTTTHD           =    O_ZTTTHD
            UPD_ZTTTHD         =    UPD_CHNGIND
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
            UPD_ICDTXT_ZITTDOC      = ''
      tables
            ICDTXT_ZITTDOC     =    it_CdtXT.


ENDFUNCTION.
