FUNCTION Z_FPP_GET_FSC.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_OBJEK) LIKE  AUSP-OBJEK
*"  EXPORTING
*"     REFERENCE(O_FSC) LIKE  ZTPP_WOSUM-FSC
*"     REFERENCE(O_219) TYPE  ZS219
*"----------------------------------------------------------------------


  PERFORM READ_NORMAL_CLASS USING I_OBJEK 'P_MODEL_YEAR'
                                   CHANGING L_ATWRT.
  L_MODEL_YEAR = L_ATWRT.
  CLEAR: L_ATWRT.

  PERFORM READ_NORMAL_CLASS USING I_OBJEK
                              'P_DESTINATION_CODE'
                              CHANGING L_ATWRT.

  L_NATION = L_ATWRT+0(3).
  L_DEALER = L_ATWRT+3(2).

  PERFORM READ_NORMAL_CLASS USING I_OBJEK
                              'P_MI'
                               CHANGING L_ATWRT.

  L_MI = L_ATWRT.

  PERFORM READ_NORMAL_CLASS USING I_OBJEK
                              'P_OCN'
                               CHANGING L_ATWRT.

  L_LENTH = STRLEN( L_MI ).
  IF L_LENTH > 7.
    CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
         EXPORTING
              OLD_DEALER = L_DEALER
         IMPORTING
              NEW_DEALER = L_DEALER1.

    CONCATENATE L_MODEL_YEAR L_NATION L_DEALER1 L_MI L_ATWRT
                INTO O_FSC.

    SELECT SINGLE MTNO ALCD MAX( ALTN )
      INTO (L_MTNO, O_219, L_ALTN)
      FROM ZTBM_ABYALCDT
      WHERE MTNO = O_FSC
      GROUP BY MTNO ALCD.  " ALTN .

  ELSE.
    CONCATENATE L_MODEL_YEAR L_NATION L_DEALER L_MI
                INTO O_FSC.
    CONCATENATE O_FSC L_ATWRT INTO O_FSC
                SEPARATED BY SPACE.
    SELECT SINGLE MTNO ALCD MAX( ALTN )
     INTO (L_MTNO, O_219, L_ALTN)
     FROM ZTBM_ABXALCDT
     WHERE MTNO = O_FSC
     GROUP BY MTNO ALCD.  " ALTN .
  ENDIF.
ENDFUNCTION.
