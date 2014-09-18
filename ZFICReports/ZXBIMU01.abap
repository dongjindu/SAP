*----------------------------------------------------------------------*
*   INCLUDE ZXBIMU01                                                   *
*----------------------------------------------------------------------*
*"       IMPORTING
*"             VALUE(I_IMTP) LIKE  IMTP STRUCTURE  IMTP OPTIONAL
*"       TABLES
*"              T_IMZO STRUCTURE  IMZO OPTIONAL
*"              T_RAIMACT STRUCTURE  RAIMACT OPTIONAL
*"----------------------------------------------------------------------
* Designed by Andy Choi (choibc@gmail.com)
* expense that can be capitalized (that is settled to assets)
* expense that cannot be capitalized (that is settled to CO receivers).
*"----------------------------------------------------------------------

  check I_IMTP-CAPEX = 'X'.

* SAP Standard Logic
* 1st process : ASSIGN_ACTUALS_TO_BUDGTYPES.
  DATA: T_NEW LIKE RAIMACT OCCURS 0 WITH HEADER LINE.

  LOOP AT T_RAIMACT.
    case t_raimact-abrkz.
* settled to asset
      when '01'.
**...... if account is investment account
        if ( t_raimact-KSTAR >= '0000901000' and
             t_raimact-KSTAR <= '0000901999' )
        or t_raimact-KSTAR < '0000200000'.
          T_RAIMACT-IPPOS = '1'.
        else.
          T_RAIMACT-IPPOS = '2'.
        endif.

        T_RAIMACT-WKG   = 0 - T_RAIMACT-WKG.
        T_RAIMACT-WTG   = 0 - T_RAIMACT-WTG.
        MODIFY T_RAIMACT.

* settled to expense
      when '02'.
        T_RAIMACT-IPPOS = '2'.
        T_RAIMACT-WKG   = 0 - T_RAIMACT-WKG.
        T_RAIMACT-WTG   = 0 - T_RAIMACT-WTG.
        MODIFY T_RAIMACT.

      when others.
*.....if program use budget category.. to be capitalized
*        if i_imtp-CAPEX = 'X' and t_raimact-wrttp = '04'.
*          T_NEW = T_RAIMACT.
*          T_NEW-IPPOS = '9'.
*          T_NEW-WKG   = 0 - T_RAIMACT-WKG.
*          T_NEW-WTG   = 0 - T_RAIMACT-WTG.
*          APPEND T_NEW.
*        endif.
    endcase.
  endloop.



* 2nd process : MODIFY_NOT_ASSIGNED_ACTUALS.
* CATEGORY --> (-) to blank category
  DATA: T_RIPASW LIKE RAIMACT OCCURS 0 WITH HEADER LINE.

  LOOP AT T_RAIMACT  INTO T_RIPASW
    WHERE ( WRTTP = '04'  OR  " actual
            WRTTP = '11'  OR  " stat. actual
            WRTTP = '12'  OR  " downpayment
            WRTTP = '22'  OR  " PO
            WRTTP = '23'  OR  " reservation
            WRTTP = '21'  OR  " PR
            WRTTP = '24'  OR  " Fund commitment
            WRTTP = '2A'  OR  " Fund reserve
            WRTTP = '2B'  OR  " Fund precommit
            WRTTP = '25'  OR  " order commitment
            WRTTP = '26'  )   " commitment
    AND     IPPOS <> SPACE.
    T_RIPASW-IPPOS = SPACE.
    T_RIPASW-ABRKZ = SPACE.
    T_RIPASW-WKG = 0 - T_RIPASW-WKG.
    T_RIPASW-WTG = 0 - T_RIPASW-WTG.
*
    APPEND T_RIPASW.
  ENDLOOP.

  LOOP AT T_RIPASW INTO T_RAIMACT.
    COLLECT T_RAIMACT.
  ENDLOOP.
