FORM CD_CALL_ZTTAXBKHD                     .
   IF   ( UPD_ZTTAXBKHD                      NE SPACE )
     OR ( UPD_ICDTXT_ZTTAXBKHD       NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTTAXBKHD_WRITE_DOCUMENT      ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTTAXBKHD
                      = *ZTTAXBKHD
                  N_ZTTAXBKHD
                      = ZTTAXBKHD
                  UPD_ZTTAXBKHD
                      = UPD_ZTTAXBKHD
                  UPD_ICDTXT_ZTTAXBKHD
                      = UPD_ICDTXT_ZTTAXBKHD
          TABLES  ICDTXT_ZTTAXBKHD
                      = ICDTXT_ZTTAXBKHD
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
