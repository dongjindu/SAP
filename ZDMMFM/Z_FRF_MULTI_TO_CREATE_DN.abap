FUNCTION Z_FRF_MULTI_TO_CREATE_DN.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_PERNR) LIKE  LTAK-PERNR OPTIONAL
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_RECEIVER STRUCTURE  ZSRF_LOCA_RECEIVER
*"      T_TO STRUCTURE  ZSRF_VBELN_TANUM
*"----------------------------------------------------------------------
*  DATA: LT_LTAP_VB LIKE ZSMM_LTAP_VB OCCURS 0.

*  DATA: L_INDEX LIKE SY-TABIX.
  DATA: L_TANUM LIKE T_TO-TANUM.
  DATA:  SUBRC LIKE SY-SUBRC.

  LOOP AT T_RECEIVER.

*+ 6/13/2008 by ig.moon   {

*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< get storage information
    SELECT SINGLE LGTYP LGPLA INTO (*LAGP-LGTYP,*LAGP-LGPLA)
                  FROM LIPS
                  WHERE VBELN EQ T_RECEIVER-VBELN .

    IF SY-SUBRC EQ 0.
      CLEAR *LAGP.
       *LAGP-LGNUM = 'P01'.

*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< check lock status
      PERFORM TABLE_ENTRY_ENQUEUE USING *LAGP 'LAGP'
                                     CHANGING SUBRC.
      CASE SUBRC.
        WHEN 0. " no ploblem, go ahead!
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< unlock
          PERFORM TABLE_ENTRY_DEQUEUE USING *LAGP 'LAGP'.

        WHEN 1. " foreign_lock
          E_MESS = 'foreign_lock'.
          EXIT.
        WHEN 2. " system_failure
          E_MESS = 'system_failure'.
          EXIT.
        WHEN 3. " others
          E_MESS = 'others'.
          EXIT.
        WHEN OTHERS.
          E_MESS = 'error unknown'.
          EXIT.
      ENDCASE.
    ENDIF.

* }

*    L_INDEX = SY-TABIX.
    CALL FUNCTION 'L_TO_CREATE_DN'
      EXPORTING
        I_LGNUM                          = 'P01'
        I_VBELN                          = T_RECEIVER-VBELN
*   I_REFNR                          = ' '
*   I_SQUIT                          = ' '
*   I_NIDRU                          = ' '
*   I_DRUKZ                          = ' '
*   I_LDEST                          = ' '
*   I_KOMIM                          = ' '
        I_EINLM                          = '2'
*   I_EINTA                          = ' '
*   I_NOSPL                          = ' '
*   I_UPDATE_TASK                    = ' '
        I_COMMIT_WORK                    = 'X'
        I_BNAME                          = SY-UNAME
*   I_TEILK                          = ' '
*   I_SOLEX                          = 0
    I_PERNR                          =  I_PERNR
 IMPORTING
    E_TANUM                          = L_TANUM
*   E_TEILK                          =
* TABLES
*   T_LTAK                           =
*       T_LTAP_VB                        = LT_LTAP_VB
*   T_WMGRP_MSG                      =
 EXCEPTIONS
   FOREIGN_LOCK                     = 1
   DN_COMPLETED                     = 2
   PARTIAL_DELIVERY_FORBIDDEN       = 3
   XFELD_WRONG                      = 4
   LDEST_WRONG                      = 5
   DRUKZ_WRONG                      = 6
   DN_WRONG                         = 7
   SQUIT_FORBIDDEN                  = 8
   NO_TO_CREATED                    = 9
   TEILK_WRONG                      = 10
   UPDATE_WITHOUT_COMMIT            = 11
   NO_AUTHORITY                     = 12
   NO_PICKING_ALLOWED               = 13
   DN_HU_NOT_CHOOSABLE              = 14
   OTHERS                           = 15
              .
    IF SY-SUBRC = 0.
      clear: ZRESULT.
*      APPEND LINES OF LT_LTAP_VB TO T_LTAP_VB.
    ELSE.

** changed by Furong on 07/24/08
*   e_mess  = '1'.
      ZRESULT  = '1'.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                MSGID               = SY-MSGID
                MSGNR               = SY-MSGNO
                MSGV1               = SY-MSGV1
                MSGV2               = SY-MSGV2
                MSGV3               = SY-MSGV3
                MSGV4               = SY-MSGV4
           IMPORTING
                MESSAGE_TEXT_OUTPUT = E_MESS.
** End of change on 07/24/08

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    T_TO-VBELN = T_RECEIVER-VBELN.
    T_TO-TANUM = L_TANUM.
    APPEND T_TO.
*    CLEAR: LT_LTAP_VB, LT_LTAP_VB[].
    CLEAR: T_TO, L_TANUM.
  ENDLOOP.

*  IF SY-SUBRC = 0.
*    DELETE ADJACENT DUPLICATES FROM T_RECEIVER.
*    E_MESS  = '0'.
*    ZRESULT = 'Success !'.
*  ELSE.
*    E_MESS  = '1'.
*    ZRESULT = 'Failure !'.
*  ENDIF.

ENDFUNCTION.

*---------------------------------------------------------------------*
*       FORM table_entry_enqueue                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TABENTRY                                                      *
*  -->  TABNAME                                                       *
*  -->  SUBRC                                                         *
*---------------------------------------------------------------------*
FORM TABLE_ENTRY_ENQUEUE USING TABENTRY
                               TABNAME
                         CHANGING SUBRC   .
  DATA: SPERRDAT LIKE RSTABLE-VARKEY.

  SPERRDAT = TABENTRY.

  CALL FUNCTION 'ENQUEUE_E_TABLEE'
       EXPORTING
            TABNAME        = TABNAME
            VARKEY         = SPERRDAT
       EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2
            OTHERS         = 3.

  SUBRC = SY-SUBRC.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM table_entry_dequeue                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TABENTRY                                                      *
*  -->  TABNAME                                                       *
*---------------------------------------------------------------------*
FORM TABLE_ENTRY_DEQUEUE USING TABENTRY
                               TABNAME        .
  DATA: SPERRDAT LIKE RSTABLE-VARKEY.

  SPERRDAT = TABENTRY.

  CALL FUNCTION 'DEQUEUE_E_TABLEE'
       EXPORTING
            TABNAME = TABNAME
            VARKEY  = SPERRDAT
       EXCEPTIONS
            OTHERS  = 1.
ENDFORM.
