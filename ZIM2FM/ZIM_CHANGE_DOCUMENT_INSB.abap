FUNCTION ZIM_CHANGE_DOCUMENT_INSB.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTINSB) LIKE  ZTINSB STRUCTURE  ZTINSB
*"     REFERENCE(O_ZTINSB) LIKE  ZTINSB STRUCTURE  ZTINSB
*"     REFERENCE(N_ZTINSBRSP) LIKE  ZTINSBRSP STRUCTURE  ZTINSBRSP
*"     REFERENCE(O_ZTINSBRSP) LIKE  ZTINSBRSP STRUCTURE  ZTINSBRSP
*"     REFERENCE(N_ZTINSBSG3) LIKE  ZTINSBSG3 STRUCTURE  ZTINSBSG3
*"     REFERENCE(O_ZTINSBSG3) LIKE  ZTINSBSG3 STRUCTURE  ZTINSBSG3
*"----------------------------------------------------------------------
   DATA : OBJECID     LIKE    CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR : IT_CDTXT.
   OBJECID = N_ZTINSB+3(20).

   CALL FUNCTION 'ZIINBDOC_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    OBJECID
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTINSB           =    N_ZTINSB
            O_ZTINSB           =    O_ZTINSB
            UPD_ZTINSB         =    UPD_CHNGIND
            N_ZTINSBRSP        =    N_ZTINSBRSP
            O_ZTINSBRSP        =    O_ZTINSBRSP
            UPD_ZTINSBRSP      =    UPD_CHNGIND
            N_ZTINSBSG3        =    N_ZTINSBSG3
            O_ZTINSBSG3        =    O_ZTINSBSG3
            UPD_ZTINSBSG3      =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZIINBDOC     = ''
      tables
            ICDTXT_ZIINBDOC    =    it_CdtXT.

ENDFUNCTION.
