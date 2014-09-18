*----------------------------------------------------------------------*
*   INCLUDE ZXF01U01                                                   *
*----------------------------------------------------------------------*
*"  IMPORTING
*"     VALUE(I_FEBEP) LIKE  FEBEP STRUCTURE  FEBEP
*"     VALUE(I_FEBKO) LIKE  FEBKO STRUCTURE  FEBKO
*"     VALUE(I_TESTRUN) TYPE  XFLAG
*"  EXPORTING
*"     VALUE(E_FEBEP) LIKE  FEBEP STRUCTURE  FEBEP
*"     VALUE(E_FEBKO) LIKE  FEBKO STRUCTURE  FEBKO
*"     VALUE(E_MSGTEXT) LIKE  FEBMKA-MESSG
*"     VALUE(E_MSGTYP) LIKE  FEBMKA-MSTYP
*"     VALUE(E_UPDATE) LIKE  FEBMKA-MSTYP
*"  TABLES
*"      T_FEBCL STRUCTURE  FEBCL
*"      T_FEBRE STRUCTURE  FEBRE

      TABLES: PAYR,
              BKPF,
              BSEG.
      TABLES: KNBK.  "Customer Bank Detail

      CASE I_FEBEP-INTAG.
* STB Check cashed
        WHEN '901'.
          DATA:  TPAYR-RWBTR LIKE PAYR-RWBTR.  "insert167129??????
          DATA:  TFEBEP-KWBTR LIKE FEBEP-KWBTR."insert167129??????
          DATA: CHECT LIKE FEBEP-CHECT.

*OLD LOGIC
*          read table t_febre index 1.
*          CHECT = T_FEBRE-VWEZW+9(6).   " check no!!!
          pack  I_FEBEP-CHECT to chect.
          shift chect left deleting leading space.

          E_FEBEP       = I_FEBEP.
          E_FEBEP-CHECT = CHECT.
*-- get lock for check ( will be released with 'commit work' )    209716
          CALL FUNCTION 'ENQUEUE_EFPAYR'
               EXPORTING
                    ZBUKR        = I_FEBKO-BUKRS
                    HBKID        = I_FEBKO-HBKID
                    HKTID        = I_FEBKO-HKTID
                    CHECT        = CHECT
               EXCEPTIONS
                    FOREIGN_LOCK = 8.

          IF NOT SY-SUBRC IS INITIAL.
            E_FEBEP-INFO1 = '??Lock ?? (&)'.
            REPLACE '&' WITH SY-MSGV1 INTO E_FEBEP-INFO1.
            E_FEBEP-INFO2 = '"??"?? ???? ??'.
            EXIT.
          ENDIF.

          SELECT * FROM PAYR WHERE ICHEC = SPACE
                             AND   ZBUKR = I_FEBKO-BUKRS
                             AND   HBKID = I_FEBKO-HBKID
                             AND   HKTID = I_FEBKO-HKTID
                             AND   CHECT = CHECT.
          ENDSELECT.

          IF SY-DBCNT = 1.
            TPAYR-RWBTR = ABS( PAYR-RWBTR ).   "insert 167129
            TFEBEP-KWBTR = ABS( I_FEBEP-KWBTR ). "insert 167129

* For prenumbered checks sy-dbcnt is ALWAYS sy-dbcnt = 1.
            IF NOT PAYR-VOIDR IS INITIAL.
*   check is voided in check register -> no posting
              E_FEBEP-INFO1 = 'Check voided'.
            ELSEIF NOT PAYR-XBANC IS INITIAL. "????
*   check has already been cashed -> no posting
              E_FEBEP-INFO1 = 'Check already cashed'.
            ELSEIF ( TPAYR-RWBTR <> TFEBEP-KWBTR ) AND
                       ( PAYR-WAERS = I_FEBEP-KWAER ).
*   check amount not match, don't mark it as cashed
              E_FEBEP-INFO1 = 'Check amnt incorrect. Doc no.:'.
              E_FEBEP-INFO2 = PAYR-VBLNR.
            ELSE.
*   check is not voided nor cashed
              SELECT SINGLE * FROM BKPF WHERE BUKRS = I_FEBKO-BUKRS
                                          AND BELNR = PAYR-VBLNR
                                          AND GJAHR = PAYR-GJAHR.
              IF SY-SUBRC = 0.
                PAYR-XBANC = 'X'.
                PAYR-BANCD = I_FEBEP-VALUT.
                UPDATE PAYR.
                E_FEBEP-INFO1 = 'Check marked as "cashed"'.

                T_FEBCL-KUKEY  = I_FEBEP-KUKEY.
                T_FEBCL-ESNUM  = I_FEBEP-ESNUM.
                T_FEBCL-CSNUM  = 1.
                T_FEBCL-KOART  = 'S'.
                T_FEBCL-SELFD  = 'BELNR'.
                T_FEBCL-SELVON = BKPF-BELNR.
                T_FEBCL-SELVON+10(4) = PAYR-GJAHR.          "hw485303
                APPEND T_FEBCL.

              ENDIF.
            ENDIF.
          ELSEIF SY-DBCNT GT 1.
*   check number found with several payment methods -> ERROR
            E_FEBEP-INFO1 = 'More than one check pymt meth.'.
            E_FEBEP-INFO2 = 'No posting'.
          ELSE.
*   no check found -> no processing
            E_FEBEP-INFO1 = 'No entry in check register'.
            E_FEBEP-INFO2 = 'No posting'.
          ENDIF.


      endcase.
