*----------------------------------------------------------------------*
*   INCLUDE ZXQQMU01                                                   *
*----------------------------------------------------------------------*
*<<<<<<<< Start of EQM01 - ZQMEX_02 QM-Notification Enhancement >>>>>>

CHECK SY-TCODE NE 'QM01'.


*-- Display/Change Mode : Retrieve Long Text and Export to Memory ID
*-- for Saving Long Text during saving Notification.

*--   Description of Improvement  _ UD1K940659 / UD1K940788
*      PERFORM RETRIEVE_TEXT_TO_ITAB TABLES IT_EDITOR_A
*                                           IT_TLINE_A
*                                    USING  C_TDID_DS
*                                           I_VIQMEL-QMNUM.

*--   Content of Confirmation
      PERFORM RETRIEVE_TEXT_TO_ITAB TABLES IT_EDITOR_B
                                           IT_TLINE_B
                                    USING  C_TDID_CT
                                           I_VIQMEL-QMNUM.

**-- Export Long Text in ABAP memory
*---  Description of improvement  data.

  EXPORT IT_TLINE_A TO MEMORY ID 'ZQMELDI'.
*---  Content of Confirmation data.
  EXPORT IT_TLINE_B TO MEMORY ID 'ZQMELCC'.

*<<<<<<<< End of EQM01 - ZQMEX_02 >>>>>>
