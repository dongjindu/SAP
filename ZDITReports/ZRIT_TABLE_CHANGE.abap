************************************************************************
* Program Name      : ZRIT_TABLE_CHANGE
* Author            :
* Creation Date     :
* Specifications By :
* Pattern           : Report 1-1
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
*
************************************************************************
REPORT zrit_table_change NO STANDARD PAGE HEADING
                        LINE-SIZE 132
                        LINE-COUNT 64(1)
                        MESSAGE-ID zmmm.

TABLES: ztfi_imfm.

DATA: it_data LIKE TABLE OF ztfi_imfm WITH HEADER LINE.

SELECT-OPTIONS: s_ayear FOR ztfi_imfm-ayear,
                s_posid FOR ztfi_imfm-posid.

PARAMETERS: p_versi LIKE ztfi_imfm-versi OBLIGATORY.

CHECK SY-DATUM < '20140326'.
CHECK sy-uname = 'HIS20037'
 OR sy-uname = 'HIS20164'.

SELECT * INTO TABLE it_data
  FROM ztfi_imfm
  WHERE ayear IN s_ayear
    AND posid IN s_posid
    AND versi = ' '
  .
IF sy-subrc = 0.
  it_data-versi = p_versi.
  MODIFY it_data TRANSPORTING versi
    WHERE versi = ' '.
  IF sy-subrc = 0.
    DELETE FROM ztfi_imfm
   WHERE ayear IN s_ayear
     AND posid IN s_posid
     AND versi = ' '.
    IF sy-subrc = 0.
      INSERT ztfi_imfm FROM TABLE it_data
         ACCEPTING DUPLICATE KEYS.
      IF sy-subrc = 0.
        COMMIT WORK.
        WRITE:/ 'TABLE IS CHANGED...'.
      ELSE.
        ROLLBACK WORK.
        WRITE:/ 'Table Deletion Erroe'.
      ENDIF.
    ENDIF.
  endif.
ELSE.
   WRITE:/ 'No data'.
ENDIF.
