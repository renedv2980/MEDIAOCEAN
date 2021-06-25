*          DATA SET PRSFM20    AT LEVEL 006 AS OF 07/17/18                      
*PHASE T41C20A                                                                  
*INCLUDE SRCHCALL                                                               
*INCLUDE BINSRCH2                                                               
         TITLE 'T41C20 SPECIAL CHARGE RECORDS'                                  
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
* SMUR 04/18  SPEC-17729  ADD NEW MEDIA D                                       
*                                                                               
* BPLA 06/15    NEW MEDIA CODES B, V, W                                         
*                                                                               
* SMYE 11/09/07 DISALLOW UES OF FX AS A CHARGE CODE                             
*                                                                               
*                                                                               
*                                                                               
T41C20   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C20,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         L     R7,=A(SUBROUTS)     SET UP ADDRESSABILITY TO SUBROUTINES         
         A     R7,RELO             RELOCATE ADDRESS                             
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
*===>                                                                           
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*===>                                                                           
         MVI   IPSTAT,0            INIT INPUT STATISTICS                        
         MVI   SAVMSGNO,0          INIT MESSAGE NUMBER SAVEAREA                 
         MVI   ERROR,0             INIT MESSAGE NUMBER                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         NOP   DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECADD        RE-DISPLAY RECORD                            
         NOP   DR                                                               
         CLI   MODE,XRECPUT        RE-DISPLAY RECORD                            
         NOP   DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         NOP   LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         NOP   PR                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    ACTERRX                                                          
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    ACTERRX                                                          
         B     XIT                                                              
*                                                                               
ACTERRX  MVI   ERROR,INVRCACT                                                   
         LA    R2,CONACTH                                                       
         B     ERRX                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       DS    0H                                                               
*                                                                               
         LA    R2,SCIMEDH         MEDIA                                         
         GOTO1 VALIMED                                                          
*                                                                               
VK5      DS    0H                                                               
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING PSPLREC,R4                                                       
         MVC   PSPLKAGY,AGENCY                                                  
         MVC   PSPLKMED,QMED                                                    
         MVI   PSPLKRCD,PSPLKIDQ   X'60'                                        
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
         CLC   TWAKEYSV(L'PSPLKEY),KEY   CHECK FOR NEWKEY                       
         BE    *+8                                                              
         MVI   NEWKEY,C'Y'                                                      
         B     VKXIT                                                            
*                                                                               
*        VALIDATE LIST KEY                                                      
*                                                                               
*NOP*VKL      LA    R2,SCLMEDH          MEDIA                                   
*NOP*         GOTO1 VALIMED                                                     
*                                                                               
VKXIT    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING PSPLREC,R6                                                       
         MVC   SCIMED,PSPLKMED     MEDIA                                        
         OI    SCIMEDH+6,X'80'     TRANSMIT                                     
*                                                                               
         MVI   NEWKEY,C'Y'         FORCE RE-DISPLAY FROM START OF PUB           
*                                                                               
DK5      DS    0H                                                               
*                                                                               
         MVC   MYKEY(L'PSPLKEY),PSPLKEY                                         
*                                                                               
         LA    R2,SCIMEDH                                                       
         MVI   5(R2),1                                                          
         GOTO1 VALIMED                                                          
*                                                                               
*                                  RESTORE KEY AND RECORD                       
         MVC   KEY(L'PSPLKEY),MYKEY                                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'PSPLKEY),MYKEY         SAME RECORD ?                       
         BE    *+6                 YES - OK                                     
         DC    H'0'                SOMETHING WRONG                              
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
***********************************************************************         
*                                                                     *         
* VALIDATE DETAIL INPUT VIA LINUP                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         LA    R0,SVLSVTAB         SET UP FOR MVCL                              
         LA    R1,L'SVLSVTAB                                                    
         LA    RE,LSVTAB                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE SAVE COPY OF LINUP SAVE TABLE           
*                                  TO RESTORE AFTER VALIDATION ERRORS           
         GOTO1 =A(LINSET),RR=RELO  LINUP INTERFACE                              
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
VRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
***********************************************************************         
*                                                                     *         
*  DISPLAY DETAILS VIA LINUP                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         GOTO1 =A(LINSET),RR=RELO    INTERFACE WITH LINUP                       
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
DRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       DS    0H                                                               
         OI    GLSTSTAT,RETEXTRA                                                
         LA    R6,KEY                                                           
         USING PSPLREC,R6                                                       
*NOP*    OC    KEY,KEY             TEST FIRST TIME                              
*NOP*    BNZ   LR030               KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVC   PSPLKAGY,AGENCY     CREATE KEY - AGENCY                          
         MVI   PSPLKRCD,PSPLKIDQ                X'60'                           
         LA    R4,MEDTBL           ALL 6 "POSSIBLE" MEDIA                       
*                                                                               
LR010    CLI   0(R4),X'FF'                                                      
         BE    LRX                 DONE WITH LIST                               
         MVC   PSPLKMED,0(R4)      MEDIA CODE FROM MEDTBL                       
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(4),KEYSAVE      TEST FOR AGY/MED/RCD                         
         BE    LR035                                                            
         LA    R4,1(R4)            BUMP TO NEXT "MEDIA"                         
         B     LR010                                                            
*                                                                               
LR035    DS    0H                                                               
         GOTO1 GETREC              GET THE RECORD                               
         MVC   MYDSKADD,DMDSKADD   SAVE D/A FOR LIST                            
*                                                                               
         L     R6,AIO                                                           
         USING LISTD,R5                                                         
         LA    R5,P1               USE P LINES                                  
         CLI   MODE,PRINTREP                                                    
         BE    *+8                                                              
         LA    R5,LISTAR           OR LIST AREA                                 
         MVC   LISTAR,SPACES                                                    
*                                                                               
         CLI   MODE,PRINTREP       SEE IF PRINTING REPORT                       
         BNE   LR085               NO THEN GO TO NEXT RECORD                    
*                                                                               
         USING PSPLELEM,R6                                                      
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR080                                                            
         B     LR050                                                            
*                                                                               
LR040    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   LR080                                                            
*                                                                               
LR050    MVC   LMED,KEY+2          MEDIA                                        
*                                                                               
LR070    DS    0H                                                               
         CLI   MODE,PRINTREP       SEE IF PRINTING REPORT                       
         BE    LR090                                                            
*                                                                               
LR080    DS    0H                  END OF RECORD                                
*                                                                               
         CLI   MODE,PRINTREP       SEE IF PRINTING REPORT                       
         BNE   LR085                                                            
         GOTO1 SPOOL,DMCB,SPOOLD   SKIP A LINE                                  
         LA    R6,KEY              MUST RESET R6 TO KEY                         
         LA    R4,1(R4)            BUMP TO NEXT "MEDIA"                         
         B     LR010                                                            
*                                                                               
LR085    MVC   DMDSKADD,MYDSKADD   RESTORE D/A                                  
         GOTO1 LISTMON             CALL LISTMON                                 
         LA    R6,KEY              MUST RESET R6 TO KEY                         
         LA    R4,1(R4)            BUMP TO NEXT "MEDIA"                         
         B     LR010                                                            
*                                                                               
LR090    DS    0H                                                               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR040               GO BACK AND DO NEXT ELEM                     
*                                                                               
LRX      DS    0H                                                               
         XC    KEY,KEY                                                          
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
MEDTBL   DC    C'BDILMNOSTVW',X'FF'                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
* PRINT REPORT                                                                  
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         B     LR                  USE LIST REC LOGIC                           
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
         MVC   H2(5),=C'MEDIA'                                                  
         MVC   H2+10(1),QMED                                                    
         MVC   H2+15(10),MEDNM                                                  
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*   LINUP INTERFACE                                                  *          
*     - BUILD LUBLK, CREATE ELEMENT TABLE, CALL LINUP                *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
         DS    0D                                                               
LINSET   NMOD1 0,**LINSET                                                       
*                                                                               
         LA    RC,SPOOLEND         POINT TO WORKING STORAGE                     
*                                                                               
         LA    R5,LUBLK            POINT TO  LINUP CONTROL BLOCK                
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         XC    LUBLKD(LUBLKL),LUBLKD   CLEAR LINUP CONTROL BLOCK                
*                                                                               
         MVC   LUNEW,NEWKEY        SET NEW OR OLD RECORD INDICATOR              
*                                                                               
         MVC   LUATWA,ATWA         PASS A(TWA)                                  
         L     R1,SYSPARMS         GET A(TIOB)                                  
         L     R1,0(R1)                                                         
         ST    R1,LUATIOB                                                       
*                                                                               
         MVI   LUNLINS,NLINS       SET NUMBER OF LINES ON SCREEN                
         MVI   LUNFLDS,3               FIELDS PER LINE                          
*                                                                               
*                                  BUILD LIST OF FIELD DISPLACEMENTS            
*                                                                               
         LA    R3,LINDSPS          POINT TO LIST OF DISPLACEMENTS               
         ST    R3,LUADSPS          A(LIST OF DISPLACEMENTS)                     
*                                                                               
         LA    R2,SCICOD1H         A(FIRST FIELD)                               
         ZIC   R4,LUNLINS          NUMBER OF LINES                              
         SRL   R4,1                HALVE SINCE TWO SETS ON A LINE               
*                                                                               
LS04     DS    0H                                                               
*                                                                               
         LA    RF,0(R2)            POINT TO FIRST FIELD ON NEXT LINE            
         S     RF,ATWA             GET DISPLACEMENT                             
         STH   RF,0(R3)            SET DISPLACEMENT IN LIST                     
*                                                                               
         LA    R3,2(R3)            BUMP TO NEXT SLOT IN LIST                    
*                                                                               
         ZIC   R0,LUNFLDS          GET NUMBER OF FIELDS ON LINE                 
         SLL   R0,1                DOUBLE SINCE 2 SETS ON A LINE                
         BAS   RE,BUMP             BUMP TO START OF NEXT LINE                   
         BCT   R0,*-4                BY BUMPING THRU ALL FLDS ON LINE           
*                                                                               
         BCT   R4,LS04             ADD NEXT LINE TO LIST                        
*                                                                               
         LA    R2,SCICOD2H         A(FIRST FIELD OF SECOND COLUMN)              
         ZIC   R4,LUNLINS          NUMBER OF LINES                              
         SRL   R4,1                HALVE SINCE TWO SETS ON A LINE               
*                                                                               
LS05     DS    0H                                                               
*                                                                               
         LA    RF,0(R2)            POINT TO FIRST FIELD ON NEXT LINE            
         S     RF,ATWA             GET DISPLACEMENT                             
         STH   RF,0(R3)            SET DISPLACEMENT IN LIST                     
*                                                                               
         LA    R3,2(R3)            BUMP TO NEXT SLOT IN LIST                    
*                                                                               
         ZIC   R0,LUNFLDS          GET NUMBER OF FIELDS ON LINE                 
         SLL   R0,1                DOUBLE SINCE 2 SETS ON A LINE                
         BAS   RE,BUMP             BUMP TO START OF NEXT LINE                   
         BCT   R0,*-4                BY BUMPING THRU ALL FLDS ON LINE           
*                                                                               
         BCT   R4,LS05             ADD NEXT LINE TO LIST                        
*                                                                               
         XC    0(2,R3),0(R3)       FORCE NULLS AT END OF LIST                   
*                                                                               
*NOP*    MVI   LUSCROLL,LUPAGEQ    SCROLL FACTOR OF A PAGE  IS DEFAULT          
         MVI   LUSCROLL,LUHALFQ    SCROLL FACTOR OF HALF PG IS DEFAULT          
*                                                                               
         CLI   SCISCRL,C'P'        CHECK FOR PAGE SCROLL                        
         BE    *+8                                                              
         CLI   SCISCRL,X'97'         LOWERCASE 'P'                              
         BNE   *+8                                                              
         MVI   LUSCROLL,LUPAGEQ                                                 
*                                                                               
         CLI   SCISCRL,C'H'        CHECK FOR HALF PAGE SCROLL                   
         BE    *+8                                                              
         CLI   SCISCRL,X'88'            LOWERCASE 'H'                           
         BNE   *+8                                                              
         MVI   LUSCROLL,LUHALFQ                                                 
*                                                                               
         TM    SCISCRLH+4,X'08'    SKIP IF NOT A NUMERIC FIELD                  
         BNO   LS051                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,SCISCRLH+5     FIELD INPUT LENGTH                           
         BZ    LS051               NO ENTRY                                     
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SCISCRL(0)      CONVERT SCROLL AMOUNT TO NUMBER              
*                                                                               
         CVB   RF,DUB                                                           
         STC   RF,LUSCROLL         PASS SCROLL AMOUNT                           
*                                                                               
LS051    DS    0H                                                               
*                                                                               
         CLI   PFAID,19            CHECK FOR UP KEY                             
         BE    *+8                                                              
         CLI   PFAID,7             CHECK FOR UP KEY                             
         BNE   *+8                                                              
         MVI   LUPFKEY,LUPFUPQ                                                  
*                                                                               
         CLI   PFAID,20            CHECK FOR DOWN KEY                           
         BE    *+8                                                              
         CLI   PFAID,8             CHECK FOR DOWN KEY                           
         BNE   *+8                                                              
         MVI   LUPFKEY,LUPFDNQ                                                  
*                                                                               
*        CLI   LUPFKEY,0           IF VALID PFKEY HIT                           
*        BE    *+8                                                              
*        OI    GENSTAT2,RETEQSEL   RE-DISPLAY SAME SCREEN                       
*                                                                               
         CLI   MODE,VALREC         SET LINUP MODE                               
         BNE   *+8                                                              
         MVI   LUAPMODE,LUAPVALQ   VALIDATE                                     
*                                                                               
         CLI   MODE,DISPREC        SET LINUP MODE - DISPREC                     
         BE    *+8                                                              
         CLI   MODE,XRECADD        RE-DISPLAY AFTER ADD                         
         BE    *+8                                                              
         CLI   MODE,XRECPUT        RE-DISPLAY AFTER CHANGE                      
         BE    *+8                                                              
         CLI   MODE,XRECDEL        RE-DISPLAY AFTER DELETE                      
         BE    *+8                                                              
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    *+8                                                              
         CLI   MODE,XRECREST       RE-DISPLAY AFTER RESTORE                     
         BNE   *+8                                                              
         MVI   LUAPMODE,LUAPDSPQ   DISPLAY                                      
*                                                                               
         MVI   LUCNTL,LUBACKQ      WINDOW SUPPORTS BACKWARD SCROLLING           
*                                                                               
         TM    IPSTAT,LUWCURSQ     CHECK IF CURSOR FOUND YET                    
         BNO   *+8                 NO - NO CURSOR SELECT                        
         OI    LUCNTL,LUCURSQ      YES - MAKE CURSOR SENSITIVE                  
*                                                                               
         LA    RF,LINHOOK          PROCESSING ROUTINE                           
         ST    RF,LUHOOK                                                        
*                                                                               
         MVC   LUSVLEN,=AL2(LSVTABL) SAVED BYTES PER LINE                       
*                                                                               
         LA    RF,LSVTAB           LINUP SAVE AREA                              
         ST    RF,LUSVTAB                                                       
*                                                                               
*              BUILD TABLE OF ELEMENTS                                          
*                                                                               
         BAS   RE,LSBLDTAB            BUILD ELEM TABLE                          
*                                                                               
*                                     FIRST LINE UP CALL                        
*                                     ------------------                        
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD                                               
         MVI   SVDIR,0             INIT DIRECTION SAVEAREA                      
*                                                                               
         BAS   RE,CLRFLD           UNPROTECT BLANK CODE FIELDS                  
*                                                                               
         OC    IPSTAT,LUWSTAT      OR IN WINDOW INPUT STATUS                    
*                                                                               
         CLI   LUAPMODE,LUAPVALQ      TEST VALIDATING                           
         BNE   LSMOR                                                            
*                                                                               
         TM    IPSTAT,LUWVERRQ     CONTINUE IF NO ERRORS                        
         BNO   LS22                                                             
*                                  ELSE                                         
         LA    R0,SVLSVTAB         SET UP FOR MVCL                              
         LA    R1,L'SVLSVTAB                                                    
         LA    RE,LSVTAB                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0               RESTORE LINUP SAVE TABLE                     
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         B     LSX                 AND DON'T UPDATE ELEMS                       
*                                                                               
LS22     DS    0H                                                               
*                                                                               
         TM    LUWSTAT,LUSNPVQ     UPDATE RECORD IF THERE WAS AT LEAST          
         BNO   *+8                 ONE NON-PREVIOUSLY VALIDATED FIELD           
         TM    LUWSTAT,LUSDATQ     AND DATA ENTERED IN SOME FIELD               
         BNO   *+8                                                              
         BAS   RE,LSWRTTAB         WRITES CHANGES TO RECORD                     
*                                                                               
         TM    IPSTAT,LUSNPVQ      IF ALL PREVIOUSLY VALIDATED                  
         BO    LSNCHA                                                           
         CLI   ACTNUM,ACTCHA       AND ACTION IS CHANGE                         
         BNE   LSNCHA              THEN WANT TO RE-DISPLAY IN CASE OF           
*                                    NEED TO SCROLL                             
         TM    LUSTAT,LUCLEARQ     SKIP IF CLEAR COMMAND ISSUED                 
         BO    LSNCHA                                                           
*                                    NEED TO SCROLL                             
         MVI   LUAPMODE,LUAPDSPQ   SET FOR DISPLAY                              
         MVI   MODE,DISPREC        SET FOR DISPLAY RECORD                       
         MVI   LUWSTAT,0           RESET WINDOW STAT                            
*                                                                               
         MVI   SVDIR,0             INIT DIRECTION SAVEAREA                      
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD SCROLL IF NEEDED                              
*                                                                               
LSNCHA   DS    0X                                                               
*                                                                               
LSMOR    DS    0X                  SET 'MORE' FIELDS                            
*                                                                               
         BAS   RE,CLRFLD           UNPROTECT BLANK CODE FIELDS                  
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOW                 CLEARED - LEAVE UPPER AS IS                
*                                                                               
         CLI   LUAPMODE,LUAPDSPQ   ONLY IF IN DISPLAY MODE                      
         BNE   LSMORX                                                           
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,SCIMOR1H         POINT TO FIRST MORE FIELD                    
*                                                                               
         L     R3,LUSVTAB          POINT TO FIRST SAVED ENTRY                   
         USING LSVTABD,R3          ESTABLISH ENTRY                              
*                                                                               
         L     R4,BSPATAB          POINT TO FIRST TABLE ENTRY                   
         USING ELTABD,R4           ESTABLISH ENTRY                              
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT START AT START            
         BNH   *+10                OF TABLE THEN SET UP INDICATOR               
         MVC   FLD(2),=C'<<'                                                    
*                                                                               
         BAS   RE,DSPFLD           DISPLAY IT                                   
*                                                                               
LSLOW    DS    0H                                                               
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,SCIMORLH         POINT TO LAST MORE FIELD                     
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOWOUT              CLEARED                                    
*                                                                               
         ZIC   RF,LUNLINS          GET NUMBER OF LINES ON SCREEN                
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         MH    RF,=Y(LSVTABL)      GET INDEX                                    
         AR    R3,RF               POINT TO LAST ELEMENT IN TABLE               
         L     R4,ELTLAST          POINT TO LAST ELEMENT IN TABLE               
*                                                                               
         OC    LSVKEY,LSVKEY       NULLS INDICATE END OF TABLE ALREADY          
         BZ    LSLOWOUT            ON SCREEN                                    
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT END AT END                
         BNL   *+10                OF TABLE THEN SET DOWN INDICATOR             
         MVC   FLD(2),=C'>>'                                                    
*                                                                               
LSLOWOUT DS    0H                                                               
*                                                                               
         BAS   RE,DSPFLD           DISPLAY IT                                   
*                                                                               
LSMORX   DS    0X                                                               
*                                                                               
LSX      DS    0H                                                               
         CLI   LUERR,0             RESET CC                                     
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
*                                  LINES IN WINDOW                              
*NLINS    EQU   6                                                               
NLINS    EQU   ((SCICODLH-SCICOD1H)/(SCICOD2H-SCICOD1H))+1                      
         DROP  R3                                                               
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP PROCESSING HOOK                                  *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LINHOOK  NTR1                                                                   
         CLI   LUMODE,LUVALQ       VALIDATE                                     
         BE    LHVAL                                                            
         CLI   LUMODE,LUDSPQ       DISPLAY                                      
         BE    LHDIS                                                            
         CLI   LUMODE,LUMOREQ      MORE TO DISPLAY                              
         BE    LHMORE                                                           
         DC    H'0'                INVALID MODE                                 
         EJECT                                                                  
**********************************************************************          
*   LINHOOK - LINUP VALIDATION HOOK ROUTINE                          *          
**********************************************************************          
         SPACE 2                                                                
LHVAL    DS    0H                                                               
*                                                                               
*              IF FIRST INPUT FIELD HAS '++' THEN USER WANTS TO CLEAR           
*              OUT WINDOW FROM THIS POINT ON                                    
*                                                                               
         L     R2,LUACLIN          TEST FOR SCREEN CLEAR REQUEST                
*                                                                               
         CLC   8(2,R2),=C'++'      CHECK FOR CLEARING INDICATOR                 
         BNE   LHV04                                                            
*                                                                               
         NI    LULSTAT,X'FF'-LUSNPVQ  TURN OFF NOT VALIDATED INDICATOR          
         OI    LUSTAT,LUCLEARQ     TELL LINUP TO CLEAR FROM HERE ON             
*                                                                               
         OC    ACURFORC,ACURFORC   SET CURSOR IF IT IS NOT SET ALREADY          
         BNZ   *+8                                                              
         ST    R2,ACURFORC         FORCE CURSOR TO THIS FIELD                   
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         B     LHVALX                                                           
*                                                                               
LHV04    DS    0H                                                               
*                                                                               
         BAS   RE,LHVALLIN         YES, VALIDATE LINE AND ADD TO TABLE          
         BNE   LHVALX              IF ERROR DONT DELETE OLD AND DONT            
*                                  CLEAR SAVE TABLE ENTRY                       
         EJECT                                                                  
LHV06    DS    0H                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED TABLE ENTRY           
         USING LSVTABD,R2          ESTABLISH ENTRY                              
*                                                                               
         OC    LSVKEY,LSVKEY       WAS ANYTHING THERE BEFORE                    
         BZ    LHV10               NO                                           
*                                  YES, MARK OLD ENTRY DELETED                  
*                                  NOTE- ADD+DEL=CHANGE, THIS ENTRY             
*                                        MAY BE SAME AS ABOVE                   
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',(R2)),RR=RELO                     
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,BSPAREC          POINT TO FOUND ELEMENT                       
         USING ELTABD,RF           ESTABLISH ELEMENT                            
*                                                                               
         OI    ELTCTL,ELTDELQ      SET ENTRY DELETED                            
         DROP  RF                                                               
*                                                                               
LHV10    DS    0H                  SET NEW SAVE TABLE ENTRY                     
         XC    LSVKEY,LSVKEY       INIT SAVE TABLE ENTRY                        
         ICM   RF,15,ELTENT        GET ADDRESS OF ELEMENT                       
         BZ    *+10                PUT IN TABLE IF FOUND                        
         MVC   LSVKEY,ELTKEY-ELTABD(RF)                                         
*                                                                               
LHVALX   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*   LINHOOK - LINUP DISPLAY HOOK ROUTINE                             *          
**********************************************************************          
         SPACE 2                                                                
LHDIS    DS    0H                                                               
         MVI   LUSTAT,0            INIT STATUS                                  
         L     R4,LUACTAB          CURRENT SAVE TABLE ENTRY                     
         USING LSVTABD,R4                                                       
         XC    0(LSVTABL,R4),0(R4)  CLEAR IT                                    
*                                                                               
         BAS   RE,LHSRCH           FIND ELEM TO DISPLAY                         
         BAS   RE,LHDISLIN         BUILD SCREEN LINE                            
*                                                                               
         BAS   RE,CLRFLD           UNPROTECT BLANK CODE FIELDS                  
*                                                                               
*                                                                               
LHDISX   DS    0H                                                               
         B     LHOOKX                                                           
         EJECT                                                                  
*                                  DO 'MORE' MESSAGE                            
*                                  -----------------                            
LHMORE   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
LHOOKX   DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
         TITLE 'PRSFM0C - BUILD TABLE OF ELEMENTS ON FILE'                      
***********************************************************************         
*                                                                     *         
* ROUTINE TO BUILD ELEMENT TABLE FOR LINUP                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LSBLDTAB NTR1                                                                   
         XC    BSPPRMS(BSPPRML),BSPPRMS INIT BINSRCH PARAMETERS                 
         L     R1,AIO2             STORE TABLE IN I/O2 & I/O3                   
         ST    R1,BSPATAB          PASS TABLE ADDRESS                           
         LA    R1,ELTABL           PASS ENTRY LENGTH                            
         ST    R1,BSPLENR                                                       
         LA    R1,ELTKEYL          PASS KEY LENGTH                              
         ST    R1,BSPLENK                                                       
         MVI   BSPKEYD,0           PASS KEY DISPLACEMENT                        
         LA    R1,ELTMAX           PASS MAXIMUM COUNT                           
         ST    R1,BSPMAX                                                        
*                                                                               
         LA    R4,WRKELTAB         POINT TO ELTAB WORK ELEMENT                  
         USING ELTABD,R4           ESTABLISH TABLE ENTRY                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         USING PSPLELEM,R6                                                      
         BAS   RE,GETEL                                                         
         B     LSBT20                                                           
*                                                                               
LSBT10   BAS   RE,NEXTEL                                                        
*                                                                               
LSBT20   BNE   LSBTX               NO ELEMENTS LEFT                             
*                                                                               
         XC    ELTABD(ELTABL),ELTABD INIT ENTRY                                 
*                                                                               
         MVC   ELTELEM,PSPLELEM    ADD ELEMENT TO TABLE                         
         MVC   ELTSORT,PSPLCODE    SET KEY                                      
*                                                                               
LSBT30   DS    0H                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',WRKELTAB),RR=RELO                  
         OC    1(3,R1),1(R1)       TEST IF ELEMENT FIT INTO TABLE               
         BNZ   *+6                                                              
         DC    H'0'                TOO MANY LINES (SHOULD NOT HAPPEN)           
*                                                                               
         B     LSBT10                                                           
*                                                                               
LSBTX    DS    0H                                                               
*                                                                               
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+10                                                             
         BCTR  R1,R0               MINUS ONE                                    
         MH    R1,=Y(ELTABL)       TIMES ENTRY LENGTH                           
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
*                                                                               
         XIT1                                                                   
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO SEARCH TABLE FOR ELEMENT                                 *         
* AND SET ADDRESS IN ELTENT                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHSRCH   NTR1                                                                   
         XC    ELTENT,ELTENT       INIT ELEMENT ADDRESS RETURN                  
         NI    LUSTAT,255-LUEOLQ   SET OFF END OF LIST INDICATOR                
*                                                                               
         OC    BSPNOR,BSPNOR       IF NO ENTRIES                                
         BZ    LHSRCHX             RETURN EMPTY-HANDED                          
*                                                                               
         L     R4,BSPATAB          DEFAULT TO FIRST ENTRY IN TABLE              
         USING ELTABD,R4                                                        
*                                                                               
         L     R3,LUAPTAB          A(PREVIOUS SAVE TABLE)                       
         USING LSVTABD,R3                                                       
*                                                                               
         OC    LSVKEY,LSVKEY       NO PREVIOUS ENTRY MEANS FIRST TIME           
         BNZ   LHSRCH02            OR SCROLLING FROM A NON-FILLED               
*                                  SCREEN - USE DEFAULT                         
*                                                                               
         CLI   SVDIR,C'-'          IF FILLING BOTTOM OF A SCREEN                
         BNE   LHSRCH11               AFTER AN UP SCROLL                        
*                                                                               
         OI    LUSTAT,LUEOLQ       EXIT WITHOUT FINDING ELEMENT                 
         B     LHSRCHX                                                          
*                                                                               
LHSRCH02 DS    0H                                                               
*                                                                               
         CLC   LSVKEY,HIVALS       IF PREVIOUS IS X'FF'S                        
         BNE   LHSRCH05                                                         
*                                                                               
         L     RE,BSPNOR              USE LAST ENTRY IN TABLE                   
*****    BCTR  RE,0                   DECREMENT FOR INDEXING                    
         L     RF,BSPLENR             RECORD LENGTH                             
         MR    RE,RE                  INDEX TO LAST ENTRY                       
         LA    R4,0(RF,R4)            A(LAST TABLE ENTRY)                       
         B     LHSRCH10                                                         
*                                                                               
LHSRCH05 DS    0H                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',LSVKEY),RR=RELO                   
*                                                                               
         L     R4,BSPAREC          GET TABLE ENTRY ADDRESS                      
*                                                                               
         CLI   BSPCNTL,BSPNF       DEFAULT TO FIRST OF TABLE IF                 
         BNE   LHSRCH10            PREVIOUS ENTRY WAS NOT FOUND                 
*                                                                               
         L     R4,BSPATAB          NO, POINT TO FIRST-(END OF TABLE)            
         B     LHSRCH11            DONE (NO MOVEMENT)                           
         EJECT                                                                  
LHSRCH10 DS    0H                                                               
         CLI   LUDIR,C'-'          CHECK FOR BACKWARD SCROLLING                 
         BE    LHSRCH16                                                         
*                                                                               
         CLI   LUDIR,C'='          SKIP BUMPING ENTRY IF RE-DISPLAYING          
         BE    *+8                                                              
         LA    R4,ELTABL(R4)       BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
LHSRCH11 DS    0H                                                               
*                                                                               
         C     R4,ELTLAST          DONE IF NOT AT END OF TABLE                  
         BL    LHSRCH30                                                         
         BE    LHSRCH12            AT END OF TABLE TELL LINUP                   
*                                  PAST END - ONLY IF SCROLLING DOWN            
*                                  AND PRIOR SCREEN ENDED WITH LAST             
*                                  ELEMENT IN TABLE - USE DEFAULT               
*                                  OR UP SCROLLING AND TABLE HAS ONLY           
*                                  ONE ELEMENT - STOP WITH NO DISPLAY           
         CLI   SVDIR,C'-'          IF FILLING BOTTOM OF A SCREEN                
         BNE   *+12                AFTER AN UP SCROLL                           
         OI    LUSTAT,LUEOLQ       EXIT WITHOUT FINDING ELEMENT                 
         B     LHSRCHX                                                          
*                                                                               
         L     R4,BSPATAB          POINT TO FIRST ENTRY IN TABLE                
         CLC   BSPNOR,=F'1'        DONE IF MORE THAN ONE ENTRY IN TABLE         
         BH    LHSRCH30                                                         
*                                  EXIT WITHOUT FINDING ELEMENT                 
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCHX                                                          
*                                                                               
LHSRCH12 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCH30                                                         
         EJECT                                                                  
LHSRCH16 DS    0H                  GOING BACKWARDS                              
         C     R4,BSPATAB          IF AT START                                  
         BNH   LHSRCH18            DONT GO FURTHER                              
         SH    R4,=Y(ELTABL)       BACK UP AN ENTRY                             
LHSRCH17 DS    0H                                                               
         C     R4,BSPATAB          IF AT START                                  
         BH    *+8                                                              
LHSRCH18 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
*                                                                               
LHSRCH30 DS    0H                                                               
         TM    ELTCTL,ELTDELQ+ELTADDQ DISPLAY CHANGED ELEMENTS                  
         BO    LHSRCH40                                                         
         TM    ELTCTL,ELTDELQ      BYPASS DELETED ELEMENTS                      
         BNO   LHSRCH40                                                         
         TM    LUSTAT,LUEOLQ       EXIT IF AT END OF LIST                       
         BO    LHSRCHX                                                          
         CLI   LUDIR,C'='          QUIT IF RE-DISPLAY                           
         BE    LHSRCH40                                                         
         B     LHSRCH10            ELSE GO FIND NEXT ELEMENT                    
*                                                                               
LHSRCH40 DS    0H                                                               
         ST    R4,ELTENT           RETURN TABLE ENTRY ADDRESS                   
         L     R3,LUACTAB          POINT TO CURRENT SAVE TABLE ENTRY            
         MVC   LSVKEY,ELTKEY       SAVE APPROPRIATE DATA                        
*                                                                               
LHSRCHX  DS    0H                                                               
         MVC   SVDIR,LUDIR         SAVE LAST TIME DIRECTION                     
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         DROP  R4                                                               
HIVALS   DC    32X'FF'             HIGH VALUES                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO VALIDATE WINDOW LINE                                     *         
* BUILD ENTRY IN APWORK AND ADD TO ELEM TABLE                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHVALLIN NTR1                                                                   
*                                                                               
         XC    ELTENT,ELTENT       CLEAR A(ENTRY)                               
*                                                                               
         TM    LULSTAT,LUSNPVQ     SKIP IF ALL FIELDS PREVIOUSLY                
         BO    *+8                   VALIDATED                                  
         TM    LULSTAT,LUSDATQ     AND NO DATA IN FIELDS ON THIS LINE           
         BZ    LHVLX                                                            
*                                                                               
         XC    WRKELTAB,WRKELTAB   INIT WORK AREA                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
         USING ELTABD,R4           IN WORK AREA                                 
*                                                                               
         LA    R3,ELTELEM          INITIALIZE ELEMENT                           
         USING PSPLELEM,R3         ESTABLISH SPECIAL CHARGES ELEMENT            
*                                                                               
         SLR   R4,R4               INIT TABLE ELEMENT POINTER                   
*                                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE =CODE           
*                                                                               
*NOP*    CLI   5(R2),0             IF INPUT IN CODE FIELD                       
*NOP*    BNE   LHV30                                                            
         CLI   8(R2),C' '          IF INPUT IN CODE FIELD                       
         BH    LHV30                                                            
*                                                                               
**********************************************************************          
* AS OF FEB/01, NO DELETION OF ADDITIONAL CHARGE CODES IS PERMITTED             
* ABSENCE OF INPUT FOR BOTH TYPE AND DESC COULD BE USED FOR DELETION            
**********************************************************************          
*                                                                               
         LR    R0,R2               SAVE R2                                      
         BAS   RE,BUMP             & SEE IF ANY INPUT ON THIS LINE              
         CLI   5(R2),0                                                          
         BNE   LHV20                                                            
         BAS   RE,BUMP             ANY INPUT ON THIS LINE                       
         CLI   5(R2),0                                                          
         BE    LHV50                                                            
LHV20    LR    R2,R0               RESET R2 FOR ERROR                           
         B     MISSERR                                                          
*                                                                               
LHV30    DS    0H                                                               
*                                                                               
         CLC   =C'FX',8(R2)        DISALLOW THIS CODE                           
         BE    CODEINV              (RESERVED FOR "FOREIGN EXCHANGE")           
*                                                                               
*NOP*    CLC   =C'DELETE',8(R2)    DELETE THIS ENTRY                            
*NOP*    BE    LHVSPOK             IGNORE ANY OTHER INPUT                       
*                                                                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
*                                                                               
LHV40    DS    0H                                                               
         MVI   0(R3),X'10'         ELEMENT CODE                                 
         MVI   1(R3),PSPLELLN      ELEMENT LENGTH                               
         MVC   PSPLCODE,8(R2)                                                   
         OC    PSPLCODE,SPACES                                                  
*                                                                               
LHV40D   DS    0H                                                               
*                                                                               
         MVC   ELTSORT,PSPLCODE    SET SORT KEY                                 
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         BAS   RE,BUMP             BUMP TO FIRST TYP FIELD                      
*                                                                               
         LTR   R4,R4               IF DELETING ENTRY                            
         BZ    LHVTYPOK                                                         
*                                                                               
         ZIC   RF,5(R2)                                                         
         LTR   RF,RF                                                            
         BZ    MISSERR             NO INPUT - MISSING                           
*                                                                               
         CLI   8(R2),C'M'          "M, & P" ARE OK                              
         BE    LHV40H                                                           
         CLI   8(R2),C'P'                                                       
         BNE   INVERR                                                           
*                                                                               
LHV40H   DS    0H                                                               
         MVC   PSPLTYPE,8(R2)                                                   
*                                                                               
LHVTYPOK DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         BAS   RE,BUMP             BUMP TO DESCRIPTION FIELD                    
*                                                                               
         LTR   R4,R4               SKIP IF DELETING FIELD                       
         BZ    LHVDSCOK                                                         
*                                                                               
         ZIC   RF,5(R2)                                                         
         LTR   RF,RF                                                            
         BZ    MISSERR             NO INPUT - ERROR                             
*                                                                               
         MVC   PSPLDESC,8(R2)                                                   
         OC    PSPLDESC,SPACES                                                  
*                                                                               
LHV49    DS    0H                                                               
         CLC   PSPLCODE,SPACES     SEE IF CODE ENTERED                          
         BNH   LHV49X              SKIP THAT SPACE                              
         OI    ELTCTL,ELTADDQ      ADD ELEMENT                                  
         B     *+8                                                              
LHV49X   OI    ELTCTL,ELTDELQ      DELETE ELEMENT                               
*                                                                               
LHV50    DS    0H                  BUMP TO NEXT LINE                            
*                                                                               
LHVDSCOK DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
LHVCMP   DS    0H                                                               
*                                                                               
         LTR   R4,R4               DONE IF NOT BUILDING AN ELEMENT              
         BZ    LHVLX                                                            
*                                                                               
         MVC   SAVMSGNO,ERROR      SAVE CURRENT MESSAGE NUMBER                  
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',ELTABD),RR=RELO                    
*                                                                               
         OC    1(3,R1),1(R1)       TEST ROOM                                    
         BZ    LHVCMPE1                                                         
*                                                                               
         L     R4,BSPAREC          POINT TO FOUND TABLE ELEMENT                 
*                                                                               
         STCM  R4,7,ELTENT+1                                                    
*                                                                               
         CLI   BSPCNTL,BSPNF       TEST IF NO MATCH FOUND                       
         BE    LHVL92              YES - NEW ENTRY FOR TABLE                    
*                                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED ELEMENT               
         USING LSVTABD,R2                                                       
*                                                                               
         CLC   LSVKEY,ELTKEY       ALSO OK IF ENTRY KEY NOT CHANGED             
         BNE   LHVCMPE2                                                         
*                                                                               
LHVL92   DS    0H                                                               
*                                                                               
         OI    ELTCTL,ELTADDQ      SET ADDED (NB- ADD+DEL=CHA)                  
         MVC   ELTELEM,PSPLELEM    SET NEW ELEM IN TABLE                        
*                                  SET NEW ELTLAST                              
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MH    R1,=Y(ELTABL)                                                    
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
         B     LHVLX                                                            
*                                                                               
         EJECT                                                                  
LHVCMPE1 DS    0H                                                               
         MVI   ERROR,RECFULL             TOO MANY DETAIL LINES                  
         B     LHVCMPER                                                         
*                                                                               
LHVCMPE2 DS    0H                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE =CODE           
         MVI   ERROR,DUPEDATA      DUPLICATE                                    
         B     LHVCMPER                                                         
*                                                                               
LHVCMPER DS    0H                                                               
*                                                                               
         NI    4(R2),X'FF'-X'20'   TURN OFF VALIDATED STATUS                    
         GOTO1 ERREX               HANDLE FIELD IN ERROR                        
*                                                                               
LHVLX    DS    0H                                                               
         CLI   ERROR,0             SET CC                                       
         XIT1                                                                   
         DROP  R4                                                               
         DROP  R3                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD WINDOW LINE                                 *         
*        FROM TABLE ENTRY IN ELTENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
LHDISLIN NTR1                                                                   
*                                                                               
         L     R2,LUACLIN          A(FIRST FIELD)                               
*                                                                               
         MVC   FLD,SPACES          DEFAULT TO CLEARING                          
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         BZ    LHDSPCX             CHECK IF NONE FOUND                          
*                                                                               
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
         LA    R3,ELTELEM          ESTABLISH RECORD ELEMENT PART                
         USING PSPLELEM,R3                                                      
*                                                                               
*              DISPLAY SPECIAL CHARGE ELEMENTS                                  
*                                                                               
         CLC   PSPLCODE,SPACES                                                  
*NOP*    BNH   LHD25                                                            
         BNH   LHDSPCX                                                          
*                                                                               
LHD24    MVC   8(L'PSPLCODE,R2),PSPLCODE                                        
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
         OI    1(R2),X'20'         SET FIELD PROTECTED                          
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         B     LHD25                                                            
*                                                                               
LHDSPCX  DS    0H                                                               
*                                                                               
         NI    1(R2),X'FF'-X'20'   UNPROTECT FIELD                              
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
LHD25    BAS   RE,BUMP                                                          
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHD27                                                            
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
         CLI   PSPLTYPE,C' '                                                    
         BNH   LHD27                                                            
         MVC   8(L'PSPLTYPE,R2),PSPLTYPE                                        
*                                                                               
LHD27    DS    0H                                                               
         BAS   RE,BUMP                                                          
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHD30                                                            
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
         CLC   PSPLDESC,SPACES                                                  
         BNH   LHD30                                                            
         MVC   8(L'PSPLDESC,R2),PSPLDESC                                        
*                                                                               
LHD30    DS    0H                                                               
*                                                                               
LHDLX    DS    0H                                                               
         XIT1                                                                   
         DROP  R3                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO UPDATE RECORD BASED ON ELEMENT TABLE CHANGES             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LSWRTTAB NTR1                                                                   
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENT                          
*                                                                               
         MVC   SAVMSGNO,ERROR      SAVE MESSAGE NUMBER                          
*                                                                               
*        FIRST DELETE ALL CURRENT ELEMENTS                                      
*                                                                               
         L     R6,AIO1             GET RECORD ADDRESS                           
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
         LA    R6,PSPLELEM-PSPLREC(R6)  POINT TO FIRST ELEMENT IN REC           
*                                                                               
         XC    DMCB+8(4),DMCB+8    SET FOR DELETE                               
*                                                                               
         SR    R0,R0                                                            
         LA    R1,DMCB             POINT TO PARAMETER LIST                      
         L     RF,VRECUP           POINT TO UPDATING ROUTINE                    
*                                                                               
LSWTDELL DS    0H                                                               
*                                                                               
         USING PSPLELEM,R6         ESTABLISH SPECIAL CHARGES ELEMENT            
*                                                                               
         CLI   PSPLELEM,0          DONE IF EOR REACHED                          
         BE    LSWTDELD                                                         
*                                                                               
         CLI   PSPLELEM,X'10'      SKIP IF NOT SPECIAL CHARGES ELEMENT          
         BNE   LSWTDELC                                                         
*                                                                               
         GOTO1 (RF),(R1),,(R6)     DELETE ELEMENT                               
*                                                                               
         B     LSWTDELX            BECAUSE R6 ==> NEXT ELM NOW                  
*                                                                               
LSWTDELC DS    0H                                                               
*                                                                               
         IC    R0,PSPLELEM+1       ELEMENT LENGTH                               
         AR    R6,R0               NEXT ELEMENT                                 
*                                                                               
LSWTDELX DS    0H                                                               
*                                                                               
         B     LSWTDELL                                                         
*                                                                               
LSWTDELD DS    0H                  ALL ELEMENTS DELETED                         
*                                                                               
         DROP  R6                                                               
*                                                                               
*        ADD ELEMENTS IN TABLE TO RECORD                                        
*                                                                               
         L     R4,BSPATAB          START OF TABLE                               
         USING ELTABD,R4           ESTABLISH ENTRY IN TABLE                     
*                                                                               
         OC    BSPNOR,BSPNOR       SKIP IF NO ENTRIES                           
         BZ    LSWTLPDN                                                         
*                                                                               
         L     R6,AIO1             GET RECORD ADDRESS                           
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
         LA    R6,PSPLELEM-PSPLREC(R6)  POINT TO FIRST ELEMENT IN REC           
*                                                                               
         SR    R0,R0                                                            
         LA    R1,DMCB             POINT TO PARAMETER LIST                      
*                                                                               
LSWTLOOP DS    0H                                                               
*                                                                               
         TM    ELTCTL,ELTADDQ      ADD ELEMENTS FLAGGED FOR ADD                 
         BO    LSWTADD                                                          
         TM    ELTCTL,ELTDELQ      AND THOSE NOT TO BE DELETED                  
         BO    LSWTNADD                                                         
*                                                                               
LSWTADD  DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR WORKAREA                               
*                                                                               
         LA    R3,ELTELEM          POINT TO ELEMENT IN TABLE                    
         USING PSPLELEM,R3         ESTABLISH SPECIAL CHARGES ELEMENT            
*                                                                               
         CLI   PSPLELEM+1,0        GET ELEMENT LENGTH                           
         BZ    LSWTADDX            SKIP IF NO ENTRY                             
*                                                                               
         GOTO1 (RF),(R1),,(R3),(C'R',(R6))  ADD ELEMENT                         
*                                                                               
         CLI   DMCB+8,0            ONLY ERROR CAN BE NO ROOM IN REC             
         BE    NOMORE              NO ERROR TOLERATED                           
*                                                                               
         IC    R0,1(R6)            ELEMENT LENGTH                               
         AR    R6,R0               NEXT INSERTION POINT                         
*                                                                               
LSWTADDX DS    0H                                                               
*                                                                               
         B     LSWTLPCN                                                         
*                                                                               
LSWTNADD DS    0H                                                               
*                                                                               
LSWTLPCN DS    0H                                                               
*                                                                               
         A     R4,BSPLENR          BUMP TO NEXT ENTRY                           
         C     R4,ELTLAST          LOOP IF NOT LAST ENTRY                       
         BNH   LSWTLOOP                                                         
*                                                                               
LSWTLPDN DS    0H                                                               
*                                                                               
LSWRTX   DS    0H                                                               
         MVC   ERROR,SAVMSGNO      RESTORE MESSAGE NUMBER                       
         XIT1                                                                   
         DROP  R3                                                               
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMONLY ADDRESSABLE ROUTINES                                       *         
***********************************************************************         
         SPACE 1                                                                
SUBROUTS DS    0D                                                               
***********************************************************************         
*                                                                     *         
* HANDLE FIELD IN ERROR - R1 -POINTS TO FIELD                         *         
*         HIGHLIGHT FIELD                                             *         
*         ERROR MESSAGE IS IN ERROR                                   *         
*         IF SAVMSGNO IS NOT FVFOK THEN THIS IS NOT FIRST ERROR       *         
*            ROUTINE RESTORES ERROR TO SAVMSGNO                       *         
*         ELSE                                                        *         
*            ROUTINE SETS CURSOR TO THIS FIELD                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         USING FLDHDRD,R1          ESTABLISH HEADER                             
ERRFLD   OI    IPSTAT,LUWVERRQ     INDICATE VALIDATION ERROR                    
         OI    FLDATB,FATBHIGH     HIGHLIGHT FIELD                              
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         NI    FLDIIND,X'FF'-FINPVAL TURN OFF VALID INDICATOR                   
         CLI   SAVMSGNO,0          IF NOT FIRST ERROR                           
         BE    *+14                                                             
         MVC   ERROR,SAVMSGNO      RESTORE PRIOR MESSAGE                        
         B     ERRFLDX                                                          
*                                                                               
         ST    R1,ACURFORC         PUT CURSOR HERE                              
*                                                                               
ERRFLDX  DS    0H                                                               
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER LITTLE ROUTINES                                               *         
***********************************************************************         
         SPACE 2                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
         SPACE 2                                                                
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
***********************************************************************         
*        DISPLAY DATA IN  FLD IN FIELD POINTED TO BY R1               *         
***********************************************************************         
         SPACE 2                                                                
DSPFLD   NTR1                      BUMP TO NEXT SCREEN FIELD                    
*                                                                               
         USING FLDHDRD,R1          ESTABLISH SCREEN FIELD                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         SH    RF,=H'8'            HEADER LENGTH                                
*                                                                               
         TM    FLDATB,X'02'        IF THERE IS EXTENED HEADER                   
         BNO   *+8                                                              
         SH    RF,=H'8'               TAKE OFF HEADER LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R1),FLD         MOVE DATA TO OUTPUT                          
*                                                                               
         OI    FLDOIND,X'80'       TRANSMIT FIELD                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        UNPROTECT CODE FIELD IF NO "DATA" FOUND THERE                          
***********************************************************************         
CLRFLD   NTR1                                                                   
         LA    R2,SCICOD1H         FIRST CODE FIELD                             
         LA    R3,SCICODLH         LAST CODE FIELD                              
CLFTST   DS    0H                                                               
         CR    R2,R3                                                            
         BH    CLFXIT              DONE                                         
         CLI   8(R2),C' '          ANYTHING THERE ?                             
         BH    CLFNXT              YES                                          
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
         OI    6(R2),X'80'         TRANSMIT                                     
CLFNXT   BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP             NEXT CODE FIELD                              
         B     CLFTST                                                           
CLFXIT   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
CODEINV  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'** CODE FX NOT PERMITTED **'                      
         GOTO1 ERREX2                                                           
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         B     ERRX                                                             
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*                                                                               
DUPERR   MVI   ERROR,DUPEDATA      DUPLICATE DATA                               
         B     ERRX                                                             
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRX                                                             
*                                                                               
NOMORE   MVI   ERROR,RECFULL       NO ROOM IN RECORD OR TABLE                   
         LA    R2,SCICOD1H                                                      
         B     ERRX                                                             
*                                                                               
ERRX     GOTO1 ERREX                                                            
*                                                                               
DUPEDATA EQU   179                 DUPLICATE DATA                               
RECFULL  EQU   180                 RECORD FULL                                  
         EJECT                                                                  
*                                                                               
HEDSPECS SSPEC H1,58,C'SPECIAL CHARGES REPORT'                                  
         SSPEC H2,58,C'----------------------'                                  
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H7,1,C'XXXXXXXX        XXXXXXXX             XXXXXX'              
         SSPEC H7,46,C'XXXXXX             XXXXXXX    XXXXXXX'                   
         SSPEC H8,1,C'--------        --------             ------'              
         SSPEC H8,46,C'------             -------    -------'                   
         DC    X'00'                                                            
MYDSKADD DS    XL4                                                              
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMD2D                                                       
*                                                                               
LSVTAB   DS    XL(NLINS*LSVTABL)                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    F                                                                
ORIGKEY  DS    XL(L'KEY)                                                        
MYKEY    DS    XL(L'KEY)                                                        
ALLZE    DS    CL1                                                              
SCANBLK  DS    CL70                                                             
WRKELTAB DS    XL(ELTABL)          WORK TABLE ELEMENT                           
SAVMSGNO DS    XL1                 CURRENT MESSAGE NUMBER SAVEAREA              
SAVCURI  DS    XL1                 INDEX OF ERROR INTO FIELD                    
IPSTAT   DS    XL1                 CUMULATIVE INPUT STATISTICS                  
NEWKEY   DS    XL1                 C'Y' - BASIC KEY HAS CHENGED                 
SVDIR    DS    XL1                 DIRECTION SAVEAREA                           
ELTENT   DS    A                   A(ELEM TABLE ENTRY)                          
ELTLAST  DS    A                   A(LAST ENTRY)                                
         DS    0F                                                               
*                                                                               
         DS    0F                                                               
       ++INCLUDE DDBSRPRMD                                                      
         DS    0D                                                               
LUBLK    DS    XL(LUBLKL)          LINUP CONTROL BLOCK                          
         DS    0F                                                               
LINDSPS  DS    XL((NLINS+1)*2)                                                  
SVLSVTAB DS    XL(NLINS*LSVTABL)      HOLD COPY OF LINUP SAVE TABLE             
ELTMAX   EQU   (3900/PSPLELLN)-1      MAX NUMBER OF ELEMENTS IN TABLE           
*                                                                               
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
*                                                                               
LISTD    DSECT                                                                  
         DS    CL1                                                              
LMED     DS    CL1                 MEDIA CODE                                   
         DS    CL2                                                              
LMEDN    DS    CL11                MEDIA NAME                                   
         EJECT                                                                  
       ++INCLUDE PSPCGREC                                                       
         EJECT                                                                  
*DDSPOOLD                                                                       
*DDCOMFACS                                                                      
*DDFLDIND                                                                       
*DDFLDHDR                                                                       
*PPSRCHPARM                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE PPSRCHPARM                                                     
         EJECT                                                                  
       ++INCLUDE DDLINUPD                                                       
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
LSVTABD  DSECT                     LINUP SAVE AREA DSECT                        
LSVKEY   DS    0XL(L'LSVSORT)                                                   
LSVSORT  DS    XL(L'PSPLCODE)                                                   
LSVKEYL  EQU   *-LSVTABD                                                        
LSVTABL  EQU   *-LSVTABD                                                        
         SPACE 2                                                                
ELTABD   DSECT                     DSECT FOR ELEM TABLE                         
ELTKEY   DS    0XL(L'ELTSORT)                                                   
ELTSORT  DS    XL(L'PSPLCODE)      SORT VALUE                                   
ELTKEYL  EQU   *-ELTABD            KEY LENGTH                                   
ELTCTL   DS    XL1                 CONTROL BYTE                                 
ELTDELQ  EQU   X'80'                 DELETE                                     
ELTADDQ  EQU   X'40'                 ADD                                        
ELTELEM  DS    CL(PSPLELLN)        ELEMENT                                      
ELTABL   EQU   *-ELTABD            ENTRY LENGTH                                 
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PRSFM20   07/17/18'                                      
         END                                                                    
