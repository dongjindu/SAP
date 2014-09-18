FUNCTION Z_FPP_GET_FSC_219.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_OBJEK) LIKE  AUSP-OBJEK
*"     REFERENCE(I_219) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_FSC_29) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     REFERENCE(O_FSC) TYPE  ZTPP_MDM_HMC-FSC_NO
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
  L_OCN = L_ATWRT.
  PERFORM READ_NORMAL_CLASS USING I_OBJEK
                              'P_VERSION'
                               CHANGING L_ATWRT.
  L_VERS = L_ATWRT+1(2).

  L_LENTH = STRLEN( L_MI ).
  IF L_LENTH > 7.
    CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
         EXPORTING
              OLD_DEALER = L_DEALER
         IMPORTING
              NEW_DEALER = L_DEALER1.

** Changed on 02/16/11
*    CONCATENATE L_MODEL_YEAR L_NATION L_DEALER1 L_MI L_OCN L_VERS
*                INTO O_FSC.
    CONCATENATE L_MODEL_YEAR L_NATION L_DEALER1 L_MI L_OCN
                INTO O_FSC.
** End of change
    if I_219 = 'X'.
    SELECT SINGLE MTNO ALCD MAX( ALTN )
      INTO (L_MTNO, O_219, L_ALTN)
      FROM ZTBM_ABYALCDT
      WHERE MTNO = O_FSC
      GROUP BY MTNO ALCD.
    ENDIF.
    if I_FSC_29 = 'X'.
       MOVE O_FSC+0(14) TO L_FSC_29+0(14).
       MOVE O_FSC+14(6) TO L_FSC_29+17(6).
        O_FSC = L_FSC_29.
    ENDIF.

* by Daniel on 02/25/11 {
    CONCATENATE O_FSC L_VERS INTO O_FSC.
* }

  ELSE.
    CONCATENATE L_MODEL_YEAR L_NATION L_DEALER L_MI
                INTO O_FSC.

* by Daniel on 02/25/11 {
*    CONCATENATE O_FSC L_mi INTO O_FSC
*                SEPARATED BY SPACE.

    CONCATENATE O_FSC L_OCN INTO O_FSC
                SEPARATED BY SPACE.
* }

    if I_219 = 'X'.
    SELECT SINGLE MTNO ALCD MAX( ALTN )
     INTO (L_MTNO, O_219, L_ALTN)
     FROM ZTBM_ABXALCDT
     WHERE MTNO = O_FSC
     GROUP BY MTNO ALCD.
   ENDIF.
  ENDIF.

* by Daniel on 02/25/11 {
*  CONCATENATE O_FSC L_VERS INTO O_FSC.
* }

ENDFUNCTION.
