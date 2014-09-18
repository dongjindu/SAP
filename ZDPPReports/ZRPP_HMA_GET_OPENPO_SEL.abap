
*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_SEL                                        *
*----------------------------------------------------------------------*
*-------------------------------------------------------------------*
*  SELECT-OPTIONS
*-------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_WOSER FOR ZTPP_WOSUM-WO_SER
                  DEFAULT 'E1*' OPTION CP SIGN I.
*                 S_DATUM FOR SY-DATUM DEFAULT SY-DATUM .
PARAMETER : P_NATION LIKE ZTPP_KSBOHMM_IF-NATION DEFAULT 'B28',
            P_DEALER LIKE ZTPP_KSBOHMM_IF-DEALER DEFAULT 'AA',
            P_EXTC LIKE ZTPP_KSBOHMM_IF-EXTC,
            P_INTC LIKE ZTPP_KSBOHMM_IF-INTC,
            P_RP LIKE ZTPP_VM-RP_CSTATUS.
PARAMETER : P_SUBMIT DEFAULT 'X' NO-DISPLAY .
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
*     AT SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*
