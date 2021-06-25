*          DATA SET PPADS14    AT LEVEL 009 AS OF 02/12/99                      
*PHASE T40B14A,*                                                                
*        TITLE 'PPADS14 - MATERIAL SPECS'                                       
         TITLE 'PPADS14 - MATERIAL SPECS - HISTORY'                             
**********************************************************************          
*                                                                    *          
*        PPADS14 (T40B14) --- MATERIAL SPECS                         *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
*   OCT98  (BOBY)--- LET THERE BE LIGHT                              *          
*                                                                    *          
**********************************************************************          
         TITLE 'PPADS14 - MATERIAL SPECS - INIT'                                
**********************************************************************          
*                                                                    *          
*        PPADS14 (T40B14) --- MATERIAL SPECS                         *          
*                                                                    *          
*        INITIALIZATION                                              *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
T40B14   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40B14**,RR=R3                                                 
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         ST    R3,RELO                                                          
*===>                                                                           
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*===>                                                                           
         MVI   IPSTAT,0            INIT INPUT STATUS                            
*                                                                               
         GOTO1 VHELPCHK CHECK IF IN MIDDLE OF HELP CALL                         
*                                                                               
         CLI   PFAID,12            IF PF12 GOTO NEXT SELECTION                  
         BE    *+8                                                              
         CLI   PFAID,24                                                         
         BE    *+8                                                              
         OI    GENSTAT2,NEXTSEL                                                 
         B     *+8                                                              
         OI    GENSTAT2,RETEQSEL   ELSE RETURN HERE NEXT TIME                   
*                                                                               
         MVI   ACTELOPT,C'N'       DON'T ADD GENCON ACTIVITY ELEMENT            
*                                                                               
         TITLE 'PPADS14 - MATERIAL SPECS - VALMODE'                             
**********************************************************************          
*                                                                    *          
*        PPADS14 (T40B14) --- MATERIAL SPECS                         *          
*                                                                    *          
*        DETERMINE CALLING MODE                                      *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
VALMODE  DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    DREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
*                                  UNKNOWN MODE                                 
         XIT1                                                                   
*                                                                               
         TITLE 'PPADS14 - MATERIAL SPECS - VKEY'                                
**********************************************************************          
*                                                                    *          
*        VALIDATE KEY ROUTINE                                        *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
VKEY     DS    0H                                                               
*                                                                               
VKTYP    DS    0H                  VALIDATE SRDS PUBLICATION TYPE               
*                                                                               
         LA    R2,MATTYPH          TYPE - REQUIRED                              
*                                                                               
         GOTO1 VVALSTYP            COMMON VALIDATION ROUTINE                    
*                                                                               
VKTYPX   DS    0H                                                               
*                                                                               
VKTIT    DS    0H                  VALIDATE SRDS PUBLICATION TITLE              
*                                                                               
         LA    R2,MATTITH          TITLE - NOT REQUIRED                         
*                                                                               
         GOTO1 VVALSTIT            COMMON VALIDATION ROUTINE                    
*                                                                               
VKTITX   DS    0H                                                               
*                                                                               
*        BUILD HEADER RECORD KEY                                                
*                                                                               
VKKEY    DS    0H                                                               
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
         LA    R6,KEY              ESTABLISH AS CONTROL PUB KEY                 
         USING GPUBKEYD,R6                                                      
*                                                                               
         MVI   GPUBREC,GPUBRECQ    SET RECORD CODE                              
         MVI   GPUBTYP,GPUBTYPQ    SET RECORD TYPE                              
*                                                                               
         MVC   GPUBPUBT,SRDTYP     SET PUB TYPE                                 
         MVC   GPUBPUB,SRDNUM      SET PUB NUMBER                               
         XC    GPUBAGY,GPUBAGY     SKIP AGENCY OVERRIDE                         
         MVI   GPUBSTYP,0          SET FOR HEADER RECORD                        
*                                                                               
*        CHECK FOR CHANGE OF KEY                                                
*        JUST RE-ENTERING A KEY FIELD CAUSES RE-START OF DISPLAY                
*                                                                               
VKNEW    DS    0H                                                               
*                                                                               
         MVI   NEWKEY,C'N'         ASSUME KEY UNCHANGED                         
*                                                                               
         CLC   HDRKEY,KEY          RESTART WITH NEW KEY                         
         BE    *+12                                                             
         MVI   NEWKEY,C'Y'            FORCE NEW KEY                             
         B     VKNEWX                                                           
*                                                                               
         TM    MATTYPH+4,X'80'     IF PUB TYPE INPUT THIS TIME                  
         BO    *+8                                                              
         TM    MATTITH+4,X'80'     IF TITLE    INPUT THIS TIME                  
         BNO   VKNEWX                                                           
*                                                                               
         MVI   NEWKEY,C'Y'            INDICATE NEW KEY                          
*                                                                               
VKNEWX   DS    0H                                                               
*                                                                               
         MVC   HDRKEY,KEY          SAVE HEADER KEY                              
*                                                                               
VKEYX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PPADS14 - MATERIAL SPECS - DKEY'                                
***********************************************************************         
*                                                                     *         
*              DISPLAY KEY ROUTINE                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DKEY     DS    0H                                                               
*                                                                               
DKTYP    DS    0H                  DISPLAY SRDS PUBLICATION TYPE                
*                                                                               
         LA    R2,MATTYPH          TYPE                                         
*                                                                               
         GOTO1 VDSPSTYP            COMMON DISPLAY ROUTINE                       
*                                                                               
DKTYPX   DS    0H                                                               
*                                                                               
DKTIT    DS    0H                  DISPLAY SRDS PUBLICATION TITLE               
*                                                                               
         LA    R2,MATTITH          TITLE                                        
*                                                                               
         GOTO1 VDSPSTIT            COMMON DISPLAY ROUTINE                       
*                                                                               
DKTITX   DS    0H                                                               
*                                                                               
*        DISPLAY DATA TYPE                                                      
*                                                                               
DKDAT    DS    0H                  DISPLAY SRDS DATA TYPE                       
*                                                                               
         LA    R2,MATDATAH         DATA TYPE                                    
*                                                                               
         GOTO1 VVALSDAT            IN CASE SRDDATE RESTORED WHNE                
*                                  SCREEN LOADED                                
         GOTO1 VDSPSDAT            COMMON DISPLAY ROUTINE                       
*                                                                               
DKDATX   DS    0H                                                               
*                                                                               
DKEYX    XIT1                                                                   
*                                                                               
         TITLE 'PPADS14 - MATERIAL SPECS - DREC'                                
***********************************************************************         
*                                                                     *         
*              DISPLAY RECORD ROUTINE - USES DDLINUP                  *         
*                                                                     *         
***********************************************************************         
         SPACE 3                                                                
DREC     DS    0H                                                               
*                                                                               
         MVC   KEY,HDRKEY          COPY HEADER KEY                              
         LA    R6,KEY              ESTABLISH AS CONTROL PUB KEY                 
         USING GPUBKEYD,R6                                                      
*                                                                               
         MVI   GPUBSTYP,X'04'      WANT NOTES RECORD                            
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   GPUBKEY,KEYSAVE     MUST FIND RECORD                             
         BNE   DRNOTFE                                                          
*                                                                               
         ICM   R0,15,AIO           SAVE CURRENT IOAREA POINTER                  
         MVC   AIO,AIO2            READ INTO I/O2                               
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         STCM  R0,15,AIO           RESTORE IOAREA POINTER                       
*                                                                               
         BRAS  RE,LINSET           INTERFACE WITH LINUP                         
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
         MVC   KEY,HDRKEY                                                       
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTER                         
*                                                                               
DRECX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
DRNOTFE  DS    0H                  NOTES RECORD NOT FOUND                       
         LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         LHI   RF,PPEMATNF                                                      
         MVI   GMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
*                                                                               
         B     DRERR                                                            
*                                                                               
DRERR    DS    0H                                                               
         STCM  RF,3,GERROR         SET ERROR CODE                               
         GOTO1 VCURSERR                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS14 - MATERIAL SPECS - BUMP'                                
***********************************************************************         
*                                                                     *         
*        BUMP SCREEN POINTER TO NEXT FIELD                            *         
*                                                                     *         
*NTRY    R2==> CURRENT FIELD                                          *         
*                                                                     *         
*EXIT    R2==> NEXT FIELD - UNPROTECTED OR PROTECTED                  *         
*                           DEPENDING ON ENTRY POINT                  *         
*        CC '='  INDICATES END OF SCREEN REACHED                      *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
*                                                                               
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         JNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
*                                                                               
         TITLE 'PPADS14 - MATERIAL SPECS - GETEL'                               
***********************************************************************         
*                                                                     *         
*        GETEL MACRO                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         GETEL R6,32,ELCODE        GETEL MACRO                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS14 - MATERIAL SPECS - LINSET'                              
**********************************************************************          
*                                                                    *          
*   LINUP INTERFACE                                                  *          
*     - BUILD LUBLK, CREATE ELEMENT TABLE, CALL LINUP                *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
         DS    0D                                                               
LINSET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         XC    GERROR,GERROR       INIT ERROR FIELD                             
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
         MVI   LUNFLDS,2               FIELDS PER LINE                          
*                                                                               
*                                  BUILD LIST OF FIELD DISPLACEMENTS            
*                                                                               
         LA    R3,LINDSPS          POINT TO LIST OF DISPLACEMENTS               
         ST    R3,LUADSPS          A(LIST OF DISPLACEMENTS)                     
*                                                                               
         LA    R2,MATMAT1H         A(FIRST FIELD)                               
         ZIC   R4,LUNLINS          NUMBER OF LINES                              
*                                                                               
         LA    RF,0(R2)            POINT TO FIRST FIELD ON NEXT LINE            
         S     RF,ATWA             GET DISPLACEMENT                             
         STH   RF,0(R3)            SET DISPLACEMENT IN LIST                     
         LA    R3,2(R3)            BUMP TO NEXT SLOT IN LIST                    
         BRAS  RE,BUMPU            BUMP TO START OF NEXT LINE                   
         BRCT  R4,*-20             ADD NEXT LINE TO LIST                        
*                                                                               
         XC    0(2,R3),0(R3)       FORCE NULLS AT END OF LIST                   
*                                                                               
         MVI   LUSCROLL,LUHALFQ    SCROLL FACTOR OF A HALF IS DEFAULT           
*                                                                               
         CLI   MATSCRL,C'P'        CHECK FOR PAGE SCROLL                        
         JE    *+8                                                              
         CLI   MATSCRL,X'97'         LOWERCASE 'P'                              
         JNE   *+8                                                              
         MVI   LUSCROLL,LUPAGEQ                                                 
*                                                                               
         CLI   MATSCRL,C'H'        CHECK FOR HALF PAGE SCROLL                   
         JE    *+8                                                              
         CLI   MATSCRL,X'88'            LOWERCASE 'H'                           
         JNE   *+8                                                              
         MVI   LUSCROLL,LUHALFQ                                                 
*                                                                               
         TM    MATSCRLH+4,X'08'    SKIP IF NOT A NUMERIC FIELD                  
         JNO   LS051                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,MATSCRLH+5     FIELD INPUT LENGTH                           
         JZ    LS051               NO ENTRY                                     
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                ESTABLISH WHERE WE ARE                       
         EX    RF,8(RE)                                                         
         B     *+10                                                             
         PACK  DUB,MATSCRL(0)      CONVERT SCROLL AMOUNT TO NUMBER              
*                                                                               
         CVB   RF,DUB                                                           
         STC   RF,LUSCROLL         PASS SCROLL AMOUNT                           
*                                                                               
LS051    DS    0H                                                               
*                                                                               
         CLI   PFAID,19            CHECK FOR UP KEY                             
         JE    *+8                                                              
         CLI   PFAID,7             CHECK FOR UP KEY                             
         JNE   *+8                                                              
         MVI   LUPFKEY,LUPFUPQ                                                  
*                                                                               
         CLI   PFAID,20            CHECK FOR DOWN KEY                           
         JE    *+8                                                              
         CLI   PFAID,8             CHECK FOR DOWN KEY                           
         JNE   *+8                                                              
         MVI   LUPFKEY,LUPFDNQ                                                  
*                                                                               
*        CLI   LUPFKEY,0           IF VALID PFKEY HIT                           
*        BE    *+8                                                              
*        OI    GENSTAT2,RETEQSEL   RE-DISPLAY SAME SCREEN                       
*                                                                               
         CLI   MODE,VALREC         SET LINUP MODE                               
         JNE   *+8                                                              
         MVI   LUAPMODE,LUAPVALQ   VALIDATE                                     
*                                                                               
         CLI   MODE,DISPREC        SET LINUP MODE - DISPREC                     
         JE    *+8                                                              
         CLI   MODE,XRECADD        RE-DISPLAY AFTER ADD                         
         JE    *+8                                                              
         CLI   MODE,XRECPUT        RE-DISPLAY AFTER CHANGE                      
         JE    *+8                                                              
         CLI   MODE,XRECDEL        RE-DISPLAY AFTER DELETE                      
         JE    *+8                                                              
         CLI   MODE,RECDEL         DELETE RECORD                                
         JE    *+8                                                              
         CLI   MODE,XRECREST       RE-DISPLAY AFTER RESTORE                     
         JNE   *+8                                                              
         MVI   LUAPMODE,LUAPDSPQ   DISPLAY                                      
*                                                                               
         MVI   LUCNTL,LUBACKQ      WINDOW SUPPORTS BACKWARD SCROLLING           
*                                                                               
         TM    IPSTAT,LUWCURSQ     CHECK IF CURSOR FOUND YET                    
         JNO   *+8                 NO - NO CURSOR SELECT                        
         OI    LUCNTL,LUCURSQ      YES - MAKE CURSOR SENSITIVE                  
*                                                                               
         LA    RF,LINHOOK          PROCESSING ROUTINE                           
         ST    RF,LUHOOK                                                        
*                                                                               
         LHI   RF,LSVTABL                                                       
         STCM  RF,3,LUSVLEN        SAVED BYTES PER LINE                         
*                                                                               
         LA    RF,LSVTAB           LINUP SAVE AREA                              
         ST    RF,LUSVTAB                                                       
*                                                                               
         XC    SVELTKEY,SVELTKEY   INIT ELEMENT KEY SAVEAREA                    
*                                                                               
*              BUILD TABLE OF ELEMENTS                                          
*                                                                               
         BRAS  RE,LSBLDTAB            BUILD ELEM TABLE                          
*                                                                               
*                                     FIRST LINE UP CALL                        
*                                     ------------------                        
*                                                                               
         MVI   SVDIR,0             INIT DIRECTION SAVEAREA                      
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD     LINUP                                     
*                                                                               
         OC    IPSTAT,LUWSTAT      OR IN WINDOW INPUT STATUS                    
*                                                                               
         CLI   LUAPMODE,LUAPVALQ      TEST VALIDATING                           
         JNE   LSMOR                                                            
*                                                                               
         TM    IPSTAT,LUWVERRQ     CONTINUE IF NO ERRORS                        
         JNO   LS22                                                             
*                                  ELSE                                         
         MVC   LSVTAB,SVLSVTAB     RESTORE LINUP SAVE TABLE                     
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         J     LSX                 AND DON'T UPDATE ELEMS                       
*                                                                               
LS22     DS    0H                                                               
*                                                                               
*        ALWAYS UPDATE THE RECORD                                               
*                                                                               
****     TM    IPSTAT,LUSNPVQ      UPDATE RECORD IF THERE WAS AT LEAST          
****     BNO   *+8                 ONE NON-PREVIOUSLY VALIDATED FIELD           
****     TM    IPSTAT,LUSDATQ      AND DATA EMATRED IN SOME FIELD               
****     BNO   LS23                                                             
*                                                                               
*****    GOTO1 =A(LSWRTTAB),RR=RELO  WRITES CHANGES TO RECORD                   
*                                                                               
LS23     DS    0H                                                               
*                                                                               
         TM    IPSTAT,LUSNPVQ      IF ALL PREVIOUSLY VALIDATED                  
         JO    LSNCHA                                                           
         CLI   ACTNUM,ACTCHA       AND ACTION IS CHANGE                         
         JNE   LSNCHA              THEN WANT TO RE-DISPLAY IN CASE OF           
*                                    NEED TO SCROLL                             
         TM    LUSTAT,LUCLEARQ     SKIP IF CLEAR COMMAND ISSUED                 
         JO    LSNCHA                                                           
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
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         JO    LSLOW                 CLEARED - LEAVE UPPER AS IS                
*                                                                               
         CLI   LUAPMODE,LUAPDSPQ   ONLY IF IN DISPLAY MODE                      
         JNE   LSMORX                                                           
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,MATMOR1H         POINT TO FIRST MORE FIELD                    
*                                                                               
         L     R3,LUSVTAB          POINT TO FIRST SAVED ENTRY                   
         USING LSVTABD,R3          ESTABLISH ENTRY                              
*                                                                               
         L     R4,BSPATAB          POINT TO FIRST TABLE ENTRY                   
         USING ELTABD,R4           ESTABLISH ENTRY                              
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT START AT START            
         JNH   *+10                OF TABLE THEN SET UP INDICATOR               
         MVC   FLD(2),=C'<<'                                                    
*                                                                               
         MVC   MATMOR1,FLD         DISPLAY IT                                   
         OI    MATMOR1H+6,FOUTTRN  TRANSMIT FIELD                               
*                                                                               
LSLOW    DS    0H                                                               
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,MATMORLH         POINT TO LAST MORE FIELD                     
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         JO    LSLOWOUT              CLEARED                                    
*                                                                               
         ZIC   RF,LUNLINS          GET NUMBER OF LINES ON SCREEN                
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         MHI   RF,LSVTABL          GET INDEX                                    
         AR    R3,RF               POINT TO LAST ELEMENT IN TABLE               
         L     R4,ELTLAST          POINT TO LAST ELEMENT IN TABLE               
*                                                                               
         OC    LSVKEY,LSVKEY       NULLS INDICATE END OF TABLE ALREADY          
         JZ    LSLOWOUT            ON SCREEN                                    
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT END AT END                
         JNL   *+10                OF TABLE THEN SET DOWN INDICATOR             
         MVC   FLD(2),=C'>>'                                                    
*                                                                               
LSLOWOUT DS    0H                                                               
*                                                                               
         MVC   MATMORL,FLD         DISPLAY IT                                   
         OI    MATMORLH+6,FOUTTRN  TRANSMIT FIELD                               
*                                                                               
LSMORX   DS    0X                                                               
*                                                                               
LSX      DS    0H                                                               
*                                                                               
         CLI   LUERR,0             RESET CC                                     
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
*                                  LINES IN WINDOW                              
NLINS    EQU   ((MATMATLH-MATMAT1H)/(MATMAT2H-MATMAT1H))+1                      
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS14 - MATERIAL SPECS - LINHOOK'                             
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP PROCESSING HOOK                                  *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LINHOOK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         CLI   LUMODE,LUVALQ       VALIDATE                                     
         JE    LHVAL                                                            
         CLI   LUMODE,LUDSPQ       DISPLAY                                      
         JE    LHDIS                                                            
         CLI   LUMODE,LUMOREQ      MORE TO DISPLAY                              
         JE    LHMORE                                                           
*                                                                               
         DC    H'0'                INVALID MODE                                 
*                                                                               
         TITLE 'PPADS14 - MATERIAL SPECS - LHVAL'                               
**********************************************************************          
*                                                                    *          
*        LINHOOK - LINUP VALIDATION HOOK ROUTINE                     *          
*                                                                    *          
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
         JNE   LHV04                                                            
*                                                                               
         NI    LULSTAT,X'FF'-LUSNPVQ  TURN OFF NOT VALIDATED INDICATOR          
         OI    LUSTAT,LUCLEARQ     TELL LINUP TO CLEAR FROM HERE ON             
*                                                                               
         OC    ACURFORC,ACURFORC   SET CURSOR IF IT IS NOT SET ALREADY          
         JNZ   *+8                                                              
         ST    R2,ACURFORC         FORCE CURSOR TO THIS FIELD                   
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         J     LHVALX                                                           
*                                                                               
LHV04    DS    0H                                                               
*                                                                               
****     BRAS  RE,LHVALLIN         YES, VALIDATE LINE AND ADD TO TABLE          
*        JNE   LHVALX              IF ERROR DONT DELETE OLD AND DONT            
*                                  CLEAR SAVE TABLE ENTRY                       
*                                                                               
LHV06    DS    0H                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED TABLE ENTRY           
         USING LSVTABD,R2          ESTABLISH ENTRY                              
*                                                                               
         OC    LSVKEY,LSVKEY       WAS ANYTHING THERE BEFORE                    
         JZ    LHV10               NO                                           
*                                                                               
         MVC   HALF,LSVKEY                                                      
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         JZ    *+10                                                             
         MVC   HALF,LSVKEYNW          USE IT                                    
*                                  YES, MARK OLD ENTRY DELETED                  
*                                  NOTE- ADD+DEL=CHANGE, THIS ENTRY             
*                                        MAY BE SAME AS ABOVE                   
         GOTO1 VBINSRCH,BSPPRMS,('BSPFIND',HALF)                                
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         JE    LHV10                                                            
*                                                                               
****     BNE   *+6                                                              
****     DC    H'0'                                                             
*                                                                               
         L     RF,BSPAREC          POINT TO FOUND ELEMENT                       
         USING ELTABD,RF           ESTABLISH ELEMENT                            
*                                                                               
         OI    ELTCTL,ELTDELQ      SET ENTRY DELETED                            
         DROP  RF                                                               
*                                                                               
LHV10    DS    0H                  SET NEW SAVE TABLE ENTRY                     
*                                                                               
         XC    LSVKEY,LSVKEY       INIT SAVE TABLE ENTRY                        
         XC    LSVKEYNW,LSVKEYNW   INIT SAVE TABLE ENTRY                        
         ICM   RF,15,ELTENT        GET ADDRESS OF ELEMENT                       
         JZ    *+16                PUT IN TABLE IF FOUND                        
         MVC   LSVKEY,ELTKEY-ELTABD(RF)                                         
         MVC   LSVKEYNW,ELTKEYNW-ELTABD(RF)                                     
*                                                                               
LHVALX   DS    0H                                                               
         J     LHOOKX                                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
         TITLE 'PPADS14 - MATERIAL SPECS - LHDIS'                               
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP DISPLAY HOOK ROUTINE                             *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LHDIS    DS    0H                                                               
*                                                                               
         MVI   LUSTAT,0            INIT STATUS                                  
*                                                                               
         L     R4,LUACTAB          CURRENT SAVE TABLE ENTRY                     
         USING LSVTABD,R4                                                       
         XC    0(LSVTABL,R4),0(R4)  CLEAR IT                                    
*                                                                               
         BRAS  RE,LHSRCH           FIND ELEM TO DISPLAY                         
         BRAS  RE,LHDISLIN         BUILD SCREEN LINE                            
*                                                                               
LHDISX   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
         TITLE 'PPADS14 - MATERIAL SPECS - LHMORE'                              
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP MORE    HOOK ROUTINE                             *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
*                                  DO 'MORE' MESSAGE                            
*                                  -----------------                            
LHMORE   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
LHOOKX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS14 - BUILD TABLE OF ELEMENTS ON FILE - LSBLDTAB'           
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD ELEMENT TABLE FOR LINUP                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LSBLDTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         XC    BSPPRMS(BSPPRML),BSPPRMS INIT BINSRCH PARAMETERS                 
*                                                                               
         L     R1,AIO3             STORE TABLE IN I/O3                          
         ST    R1,BSPATAB          PASS TABLE ADDRESS                           
         LA    R1,ELTABL           PASS ENTRY LENGTH                            
         ST    R1,BSPLENR                                                       
         LA    R1,ELTKEYL          PASS KEY LENGTH                              
         ST    R1,BSPLENK                                                       
         MVI   BSPKEYD,0           PASS KEY DISPLACEMENT                        
         LA    R1,ELTMAX           PASS MAXIMUM COUNT                           
         ST    R1,BSPMAX                                                        
*                                                                               
*        INITIALIZE TABLE AREA                                                  
*                                                                               
         LA    R0,ELTMAX           MAX ENTRIES IN TABLE                         
         L     R1,BSPATAB          TABLE AREA                                   
         LA    RF,ELTABL           LENGTH OF ONE ENTRY IN TABLE                 
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         B     *+10                                                             
         XC    0(0,R1),0(R1)       CLEAR ENTRY                                  
         LA    R1,1(RF,R1)         NEXT ENTRY                                   
         BCT   R0,*-18                                                          
*                                                                               
         LA    R4,WRKELTAB         POINT TO ELTAB WORK ELEMENT                  
         USING ELTABD,R4           ESTABLISH TABLE ENTRY                        
*                                                                               
         SR    R0,R0               INITIALIZE COUMATR                           
*                                                                               
         L     R6,AIO2             POINT TO NOTES RECORD                        
         MVI   ELCODE,GPUBMEQU     SET NOTES ELEMENT ID                         
         BRAS  RE,GETEL            FIND FIRST NOTE                              
*                                                                               
LSBTLOOP DS    0H                                                               
*                                                                               
         USING GPUBMD,R6           ESTABLISH NOTES ELEMENT                      
*                                                                               
         JNE   LSBTDONE            NO ELEMENTS LEFT                             
*                                                                               
         XC    ELTABD(ELTABL),ELTABD INIT ENTRY                                 
*                                                                               
         SR    RF,RF                                                            
         IC    RF,GPUBMLN          ELEMENT LENGTH                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                ESTABLISH WHERE WE ARE                       
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   ELTELEM(0),GPUBMD   ADD ELEMENT TO TABLE                         
*                                                                               
         STC   R0,ELTSORT          SET KEY                                      
         AHI   R0,1                BUMP COUMATR                                 
*                                                                               
         GOTO1 VBINSRCH,BSPPRMS,('BSPADD',WRKELTAB)                             
         OC    1(3,R1),1(R1)       TEST IF ELEMENT FIT INTO TABLE               
         BNZ   *+6                                                              
         DC    H'0'                TOO MANY LINES (SHOULD NOT HAPPEN)           
*                                                                               
LSBTCONT DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL                                                        
*                                                                               
         J     LSBTLOOP                                                         
*                                                                               
LSBTDONE DS    0H                                                               
*                                                                               
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         JZ    *+10                                                             
         BCTR  R1,0                MINUS ONE                                    
         MHI   R1,ELTABL           TIMES ENTRY LENGTH                           
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
*                                                                               
LSBLDTBX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS14 - BUILD TABLE OF ELEMENTS ON FILE - LHSRCH'             
***********************************************************************         
*                                                                     *         
*        ROUTINE TO SEARCH TABLE FOR ELEMENT                          *         
*        AND SET ADDRESS IN ELTENT                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHSRCH   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         XC    ELTENT,ELTENT       INIT ELEMENT ADDRESS RETURN                  
         NI    LUSTAT,255-LUEOLQ   SET OFF END OF LIST INDICATOR                
*                                                                               
         OC    BSPNOR,BSPNOR       IF NO ENTRIES                                
         JZ    LHSRCHX             RETURN EMPTY-HANDED                          
*                                                                               
         L     R4,BSPATAB          DEFAULT TO FIRST ENTRY IN TABLE              
         USING ELTABD,R4                                                        
*                                                                               
         L     R3,LUAPTAB          A(PREVIOUS SAVE TABLE)                       
         USING LSVTABD,R3                                                       
*                                                                               
         MVC   HALF,LSVKEY         COPY KEY                                     
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         JZ    *+10                                                             
         MVC   HALF,LSVKEYNW          USE IT                                    
*                                                                               
         OC    HALF,HALF           NO PREVIOUS ENTRY MEANS FIRST TIME           
         JNZ   LHSRCH02            OR SCROLLING FROM A NON-FILLED               
*                                  SCREEN - USE DEFAULT                         
*                                                                               
         CLI   SVDIR,C'-'          IF FILLING BOTTOM OF A SCREEN                
         JNE   LHSRCH11               AFTER AN UP SCROLL                        
*                                                                               
         OI    LUSTAT,LUEOLQ       EXIT WITHOUT FINDING ELEMENT                 
         J     LHSRCHX                                                          
*                                                                               
LHSRCH02 DS    0H                                                               
*                                                                               
         CLC   HALF,HIVALS         IF PREVIOUS IS X'FF'S                        
         JNE   LHSRCH05                                                         
*                                                                               
         L     RE,BSPNOR              USE LAST ENTRY IN TABLE                   
         L     RF,BSPLENR             RECORD LENGTH                             
         MR    RE,RE                  INDEX TO LAST ENTRY                       
         LA    R4,0(RF,R4)            A(LAST TABLE ENTRY)                       
         J     LHSRCH10                                                         
*                                                                               
LHSRCH05 DS    0H                                                               
*                                                                               
         GOTO1 VBINSRCH,BSPPRMS,('BSPRDHI',HALF)                                
*                                                                               
         L     R4,BSPAREC          GET TABLE ENTRY ADDRESS                      
*                                                                               
         CLI   BSPCNTL,BSPNF       DEFAULT TO FIRST OF TABLE IF                 
         JNE   LHSRCH10            PREVIOUS ENTRY WAS NOT FOUND                 
*                                                                               
         L     R4,BSPATAB          NO, POINT TO FIRST-(END OF TABLE)            
         J     LHSRCH11            DONE (NO MOVEMENT)                           
*                                                                               
LHSRCH10 DS    0H                                                               
*                                                                               
         CLI   LUDIR,C'-'          CHECK FOR BACKWARD SCROLLING                 
         JE    LHSRCH16                                                         
*                                                                               
         CLI   LUDIR,C'='          SKIP BUMPING ENTRY IF RE-DISPLAYING          
         JE    *+8                                                              
         LA    R4,ELTABL(R4)       BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
LHSRCH11 DS    0H                                                               
*                                                                               
         C     R4,ELTLAST          DONE IF NOT AT END OF TABLE                  
         JL    LHSRCH30                                                         
         JE    LHSRCH12            AT END OF TABLE TELL LINUP                   
*                                  PAST END - ONLY IF SCROLLING DOWN            
*                                  AND PRIOR SCREEN ENDED WITH LAST             
*                                  ELEMENT IN TABLE - USE DEFAULT               
*                                  OR UP SCROLLING AND TABLE HAS ONLY           
*                                  ONE ELEMENT - STOP WITH NO DISPLAY           
         CLI   SVDIR,C'-'          IF FILLING BOTTOM OF A SCREEN                
         JNE   *+12                AFTER AN UP SCROLL                           
         OI    LUSTAT,LUEOLQ       EXIT WITHOUT FINDING ELEMENT                 
         J     LHSRCHX                                                          
*                                                                               
         L     R4,BSPATAB          POINT TO FIRST ENTRY IN TABLE                
         CLC   BSPNOR,=F'1'        DONE IF MORE THAN ONE ENTRY IN TABLE         
         JH    LHSRCH30                                                         
*                                  EXIT WITHOUT FINDING ELEMENT                 
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         J     LHSRCHX                                                          
*                                                                               
LHSRCH12 DS    0H                                                               
*                                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         J     LHSRCH30                                                         
*                                                                               
LHSRCH16 DS    0H                  GOING BACKWARDS                              
*                                                                               
         C     R4,BSPATAB          IF AT START                                  
         JNH   LHSRCH18               DONT GO FURTHER                           
*                                                                               
         AHI   R4,-ELTABL             BACK UP AN ENTRY                          
*                                                                               
LHSRCH17 DS    0H                                                               
*                                                                               
         C     R4,BSPATAB          IF AT START                                  
         JH    *+8                                                              
LHSRCH18 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
*                                                                               
LHSRCH30 DS    0H                                                               
*                                                                               
         TM    ELTCTL,ELTDELQ+ELTADDQ DISPLAY CHANGED ELEMENTS                  
         JO    LHSRCH40                                                         
         TM    ELTCTL,ELTDELQ      BYPASS DELETED ELEMENTS                      
         JNO   LHSRCH40                                                         
         TM    LUSTAT,LUEOLQ       EXIT IF AT END OF LIST                       
         JO    LHSRCHX                                                          
         CLI   LUDIR,C'='          QUIT IF RE-DISPLAY                           
         JE    LHSRCH40                                                         
         J     LHSRCH10            ELSE GO FIND NEXT ELEMENT                    
*                                                                               
LHSRCH40 DS    0H                                                               
*                                                                               
         ST    R4,ELTENT           RETURN TABLE ENTRY ADDRESS                   
         L     R3,LUACTAB          POINT TO CURRENT SAVE TABLE ENTRY            
         MVC   LSVKEY,ELTKEY       SAVE APPROPRIATE DATA                        
         MVC   LSVKEYNW,ELTKEYNW   SAVE APPROPRIATE DATA                        
*                                                                               
LHSRCHX  DS    0H                                                               
         MVC   SVDIR,LUDIR         SAVE LAST TIME DIRECTION                     
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
HIVALS   DC    32X'FF'             HIGH VALUES                                  
*                                                                               
         TITLE 'PPADS14 - MATERIAL SPECS - LHDISLIN'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD WINDOW LINE                                 *         
*        FROM TABLE ENTRY IN ELTENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHDISLIN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         L     R2,LUACLIN          A(FIRST FIELD)                               
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         SH    RF,=H'8'            HEADER LENGTH                                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENED HEADER                
         BNO   *+8                                                              
         SH    RF,=H'8'               TAKE OFF HEADER LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   8(0,R1),SPACES      INIT FIELD                                   
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         JZ    LHDTXTX             CHECK IF NONE FOUND                          
*                                                                               
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
         LA    R3,ELTELEM          ESTABLISH RECORD ELEMENT PART                
         USING GPUBMD,R3                                                        
*                                                                               
*              DISPLAY TEXT                                                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,GPUBMLN        ELEMENT LENGTH                               
         AHI   RF,-(GPUBM1-GPUBMD)  TEXT LENGTH                                 
         JNP   LHDTXTX             NOTHING TO DISPLAY                           
         STC   RF,FLDILEN          SET INPUT LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDDATA(0),GPUBM1    MOVE LINE OF TEXT TO SCREEN                 
*                                                                               
LHDTXTX  DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
LHDLX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS14 - TEXT POSTING - DSECTS'                                
***********************************************************************         
*                                                                     *         
*        HIDDEN DSECTS                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* PPADSFFD                                                                      
* DDGENTWA                                                                      
* PPADSD6D                                                                      
* PPADSWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PPADSFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PPADSD4D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE PPADSWORKD                                                     
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
         DS    16A                 RELOCATION FACTORS                           
LSVTAB   DS    XL(NLINS*LSVTABL)   LINE UP SAVE TABLE                           
*                                                                               
*               WORK AREA                                                       
*                                                                               
         DS    0F                                                               
HDRKEY   DS    XL(L'KEY)           MASTER KEY SAVEAREA                          
*                                                                               
FLD      DS    CL80                FIELD WORKAREA                               
ALINCUR  DS    A                   A(LINE WITH CURSOR)                          
LINLAST  DS    A                   A(LAST LINE WITH TEXT)                       
*                                                                               
WRKELTAB DS    XL(ELTABL)          WORK TABLE ELEMENT                           
SAVMSGNO DS    XL1                 CURRENT MESSAGE NUMBER SAVEAREA              
SAVCURI  DS    XL1                 INDEX OF ERROR INTO FIELD                    
IPSTAT   DS    XL1                 CUMULATIVE INPUT STATISTICS                  
NEWKEY   DS    XL1                 C'Y' - BASIC KEY HAS CHENGED                 
ELTENT   DS    A                   A(ELEM TABLE ENTRY)                          
ELTLAST  DS    A                   A(LAST ENTRY)                                
SVELTKEY DS    XL(L'ELTKEY)        ELEMENT KEY SAVEAREA                         
         DS    0F                                                               
*                                                                               
         DS    0F                                                               
       ++INCLUDE DDBSRPRMD                                                      
         DS    0D                                                               
LUBLK    DS    XL(LUBLKL)          LINUP CONTROL BLOCK                          
         DS    0F                                                               
LINDSPS  DS    XL((NLINS+1)*2)                                                  
SVLSVTAB DS    XL(NLINS*LSVTABL)      HOLD COPY OF LINUP SAVE TABLE             
ELTMAX   EQU   40                     MAX NUMBER OF ELEMENTS IN TABLE           
DISPSW   DS    X                   DISPLAY SWITCH                               
DISPREDO EQU   X'80'               RE-DISPLAY SCREEN                            
SVDIR    DS    XL1                 DIRECTION SAVEAREA                           
*                                                                               
*DDFLDIND                                                                       
*DDFLDHDR                                                                       
*FATIOB                                                                         
*FAGETTXTD                                                                      
*CTGENPUB                                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENPUBS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE DDLINUPD                                                       
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
LSVTABD  DSECT                     LINUP SAVE AREA DSECT                        
LSVKEY   DS    0XL(L'LSVSORT)                                                   
LSVSORT  DS    XL2                 SORT VALUE - LINE #                          
LSVKEYL  EQU   *-LSVTABD                                                        
LSVKEYNW DS    XL2                 NEW KEY AFTER WRITING RECORD                 
LSVTABL  EQU   *-LSVTABD                                                        
*                                                                               
ELTABD   DSECT                     DSECT FOR ELEM TABLE                         
ELTKEY   DS    0XL(L'ELTSORT)                                                   
ELTSORT  DS    XL2                 SORT VALUE- LINE #                           
ELTKEYL  EQU   *-ELTABD            KEY LENGTH                                   
ELTKEYNW DS    XL2                 NEW KEY AFTER WRITING RECORD                 
ELTCTL   DS    XL1                 CONTROL BYTE                                 
ELTDELQ  EQU   X'80'                 DELETE                                     
ELTADDQ  EQU   X'40'                 ADD                                        
ELTELEM  DS    CL86                ELEMENT                                      
ELTABL   EQU   *-ELTABD            ENTRY LENGTH                                 
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009PPADS14   02/12/99'                                      
         END                                                                    
