FORM CD_CALL_ZIBLINOU                      .
   IF   ( UPD_ZTBLINOU                       NE SPACE )
     OR ( UPD_ICDTXT_ZIBLINOU        NE SPACE )
   .
     CALL FUNCTION 'ZIBLINOU_WRITE_DOCUMENT       ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTBLINOU
                      = *ZTBLINOU
                  N_ZTBLINOU
                      = ZTBLINOU
                  UPD_ZTBLINOU
                      = UPD_ZTBLINOU
                  UPD_ICDTXT_ZIBLINOU
                      = UPD_ICDTXT_ZIBLINOU
          TABLES  ICDTXT_ZIBLINOU
                      = ICDTXT_ZIBLINOU
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
