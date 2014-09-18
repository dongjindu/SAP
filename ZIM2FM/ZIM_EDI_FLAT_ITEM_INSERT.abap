FUNCTION ZIM_EDI_FLAT_ITEM_INSERT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"  TABLES
*"      IT_ZTDDF1 STRUCTURE  ZTDDF1
*"  EXCEPTIONS
*"      DB_ERROR
*"----------------------------------------------------------------------
DATA : W_ZFDDSEQ   LIKE ZTDDF1-ZFDDSEQ.

   DELETE FROM ZTDDF1 WHERE ZFDDENO EQ W_ZFDHENO.

   W_ZFDDSEQ = '000000'.

   LOOP AT IT_ZTDDF1.
       W_TABIX = SY-TABIX.

       ASSIGN  IT_ZTDDF1-ZFDDFDA  TO    <FS_F>.
       IF IT_ZTDDF1-ZFDDFDA(1) NE '{' AND IT_ZTDDF1-ZFDDFDA(1) NE '}'.
          PERFORM P2000_SPACE_CUT    USING <FS_F>.   " 첫 SPACE 제거
* 특수문자 제거
          PERFORM    P2000_CHANGE_SYMBOL    USING <FS_F> '~' ' '.
          PERFORM    P2000_CHANGE_SYMBOL    USING <FS_F> '`' ' '.
          PERFORM    P2000_CHANGE_SYMBOL    USING <FS_F> '_' ' '.
          PERFORM    P2000_CHANGE_SYMBOL    USING <FS_F> '@' ' '.
          PERFORM    P2000_CHANGE_SYMBOL    USING <FS_F> '#' ' '.
          PERFORM    P2000_CHANGE_SYMBOL    USING <FS_F> '$' ' '.
          PERFORM    P2000_CHANGE_SYMBOL    USING <FS_F> '|' ' '.
          PERFORM    P2000_CHANGE_SYMBOL    USING <FS_F> '\' ' '.
          PERFORM    P2000_CHANGE_SYMBOL    USING <FS_F> '[' ' '.
          PERFORM    P2000_CHANGE_SYMBOL    USING <FS_F> ']' ' '.
          PERFORM    P2000_CHANGE_SYMBOL    USING <FS_F> '{' ' '.
          PERFORM    P2000_CHANGE_SYMBOL    USING <FS_F> '}' ' '.
      ENDIF.

      W_ZFDDSEQ = W_ZFDDSEQ + 1.
      MOVE : W_ZFDHENO         TO     IT_ZTDDF1-ZFDDENO,
             W_ZFDDSEQ         TO     IT_ZTDDF1-ZFDDSEQ,
             <FS_F>            TO     IT_ZTDDF1-ZFDDFDA.

      MODIFY IT_ZTDDF1 INDEX W_TABIX.

   ENDLOOP.

   INSERT ZTDDF1    FROM TABLE IT_ZTDDF1.

   IF SY-SUBRC NE 0.
       RAISE  DB_ERROR.
   ENDIF.


ENDFUNCTION.
