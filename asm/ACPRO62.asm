*          DATA SET ACPRO62    AT LEVEL 005 AS OF 09/12/02                      
*PHASE T60B62A,*                                                                
         TITLE 'T60B62 - JOB GROUP MAINT'                                       
T60B62   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B62**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         LA    R5,BUFF             SAVED AREA FOR TABLE                         
         USING BLOCKSD,R5                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R2,RELO                                                          
         ST    RC,SAVERC                                                        
*                                                                               
         OI    GENSTAT2,RETEQSEL   RETURN SAME SELECTION (FOR PAGING)           
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VK                                                            
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VR                                                            
         BAS   RE,DK                                                            
         BAS   RE,DR                                                            
         B     XIT                                                              
*                                                                               
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DK                                                            
         B     XIT                                                              
*                                                                               
MODE6    CLI   MODE,DISPREC                                                     
         BNE   XIT                                                              
         BAS   RE,DR                                                            
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE KEY FIELDS                                              
***********************************************************************         
         SPACE 1                                                                
VK       NTR1                      JOB GROUP                                    
         MVC   CUL+1(2),=C'SJ'     SET UNIT/LEDGER                              
         GOTO1 SETHEIR             GET LEDGER LENGTHS                           
         MVC   LVLNA,LCLI          SAVE SJ LEDGER LENGTHS                       
         MVC   LVLNB,LPRO                                                       
         MVC   LVLNC,LJOB                                                       
         MVI   BIT,0                                                            
         XC    PREVCLI,PREVCLI                                                  
         XC    PREVPRO,PREVPRO                                                  
         XC    PREVJOB,PREVJOB                                                  
         MVC   SVJCODE,SPACES                                                   
*                                                                               
         MVI   ERROR,MISSING                                                    
         LA    R2,PROJGRH          JOB GROUP CODE                               
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
*                                                                               
         MVI   ERROR,ERRJCODE                                                   
         ZIC   R3,5(R2)            LENGTH OF INPUT                              
         LA    R1,8(R2)                                                         
VK05     CLI   0(R1),X'40'         NO INBEDDED SPACES                           
         BE    ERREND                                                           
         LA    R1,1(R1)                                                         
         BCT   R3,VK05                                                          
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,SVJCODE,8(R2)    SAVE JOB GROUP CODE                          
         OC    SVJCODE,SPACES                                                   
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BNE   VK07                                                             
         MVI   ERROR,MISSING                                                    
         LA    R2,PROJGRNH                                                      
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
*                                                                               
VK07     LA    R6,KEY                                                           
         USING JGRRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   JGRKTYP,JGRKTYPQ   X'2C'                                         
         MVI   JGRKSUB,JGRKSUBQ    X'12'                                        
         MVC   JGRKCPY,CUL                                                      
         MVC   JGRKUNT(2),CUL+1    UNIT/LEDGER                                  
         LA    R2,PROJGRH          JOB GROUP CODE                               
         MVC   JGRKCODE,SVJCODE    MOVE IN SAVED CODE                           
*                                                                               
VK12     CLC   SAVEKEY(L'ACTKEY),KEY                                            
         BE    VKX                                                              
         MVC   STDISP,=H'0'                                                     
         MVC   PRVSTDSP,=H'0'                                                   
         GOTO1 =A(CLRSCRN),DMCB,RR=RELO                                         
         GOTO1 =A(UNPROT),DMCB,RR=RELO                                          
*                                                                               
VKX      MVC   SAVEKEY,KEY                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              VALIDATE RECORD                                                  
***********************************************************************         
*                                                                               
VR       NTR1                                                                   
*                                                                               
         OI    GENSTAT2,RETEQSEL                                                
         ZAP   SVDTOT,=P'0'                                                     
         MVC   AIO,AIO1                                                         
         GOTO1 =A(UNPROT),DMCB,RR=RELO                                          
         GOTO1 =A(BLDTAB),DMCB,RR=RELO                                          
*                                                                               
VR05     MVI   ERROR,MISSING                                                    
         LA    R2,PROJGRNH                                                      
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         GOTO1 NAMEIN              ADD X'20' NAME ELEMENT                       
         GOTO1 PERSIN              ADD X'1A' PERSON ELEMENT                     
*                                                                               
         USING ELEMTABD,R4                                                      
         USING DSPLINED,R2                                                      
VR10     LA    R2,PROLNC1H                                                      
         LA    R4,ELEMBLK                                                       
         AH    R4,STDISP                                                        
*                                                                               
VR20     ST    R2,SVADDR           SAVE ADDRESS OF INPUT LINE                   
         NI    BIT,X'FF'-(CLISET+PROSET+JOBSET)                                 
         NI    BIT,X'FF'-NEWLINE                                                
         ZAP   SVJTOT,=P'0'                                                     
         OC    ELPRCLI,ELPRCLI     NO PREVIOUS CLIENT=NEW ELEM                  
         BNZ   *+12                                                             
         OI    BIT,NEWLINE                                                      
         B     VR50                                                             
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*        MAKING A CHANGE - DELETE OLD ELEM                                      
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*                                                                               
         USING JBPELD,R6                                                        
VR25     L     R6,AIO              LOOK FOR MATCH ON ELEMENT                    
         MVI   ELCODE,JBPELQ                                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VR30NX   BAS   RE,NEXTEL                                                        
         BNE   VR50                GO ADD                                       
         CLC   ELPRCLI,JBPCLI     SAME CLIENT                                   
         BNE   VR30NX                                                           
         CLC   ELPRPRO,JBPPRO     SAME PRODUCT                                  
         BNE   VR30NX                                                           
         CLC   ELPRJOB,JBPJOB      SAME JOB                                     
         BNE   VR30NX                                                           
*                                                                               
         MVC   SVJTOT,JBPPERC      BEFORE YOU DELETE SAVE THE AMOUNT            
*                                                                               
         MVI   0(R6),X'FF'         DELETE OLD ELEM                              
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         XC    0(ELLNQ,R4),0(R4)   CLEAR ENTRY IN TABLE                         
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*        CREATE NEW ENTRY                                                       
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*                                                                               
VR50     DS    0H                  CHECK FOR ELEMENTS TO ADD                    
         MVC   SVCLI,SPACES                                                     
         MVC   SVPRO,SPACES                                                     
         MVC   SVJOB,SPACES                                                     
         CLI   DSPCLIH+5,0        ANYTHING ON LINE                              
         BNE   VR60                                                             
         CLI   DSPPROH+5,0                                                      
         BNE   VR60                                                             
         CLI   DSPJOBH+5,0                                                      
         BNE   VR60                                                             
         XC    0(ELLNQ,R4),0(R4)   CLEAR ENTRY IN TABLE                         
         B     VR140               NO, CHECK NEXT LINE                          
*                                                                               
* VALIDATE CLIENT/PRODUCT/JOB                                                   
*                                                                               
VR60     TM    BIT,NEWLINE         IS THIS A NEW ENTRY                          
         BZ    VR62                                                             
         MVI   ERROR,REC2BIG                                                    
         CLC   TABCOUNT,=Y(MAXCOUNT) MAKE SURE THERE'S ROOM FOR MORE            
         BL    *+8                 THERE'S ROOM                                 
         B     ERREND                                                           
*                                                                               
VR62     LA    R2,DSPCLIH                                                       
         CLI   ACTEQU,ACTADD       REQUIRED ON ADD                              
         BE    VR65                                                             
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BNE   VR65                                                             
         TM    BIT,PROSET+JOBSET                                                
         BNZ   ERREND                                                           
*                                                                               
VR65     MVI   ERROR,MISSING                                                    
         CLI   5(R2),0               ANY CLIENT                                 
         BE    ERREND                                                           
         MVI   ERROR,NOVALONE                                                   
         CLI   PROLNC1,C'"'        NOT AVAILABLE ON FIRST LINE                  
         BE    ERREND                                                           
         CLI   8(R2),C'"'          " = SAME CLIENT AS PREVIOUS                  
         BNE   *+16                                                             
         MVC   8(L'DSPCLI,R2),PREVCLI                                           
         MVC   5(1,R2),PREVCLN                                                  
         MVC   PREVCLI,8(R2)       SAVE PREVIOUS CLIENT                         
         MVC   PREVCLN,5(R2)      SAVE PREVIOUS CLIENT LENGTH                   
         MVC   AIO,AIO2                                                         
         GOTO1 VALCLI              VALIDATE CLIENT                              
         MVC   AIO,AIO1                                                         
         ZIC   RF,5(R2)            CLIENT LENGTH                                
         BCTR  RF,0                                                             
         EXMVC RF,SVCLI,CLICODE      SAVE CLIENT                                
         OC    SVCLI,SPACES                                                     
         OI    BIT,CLISET                                                       
*                                                                               
VR70     L     R2,SVADDR           RESTORE R2 TO BEGINNING OF LINE              
         LA    R2,DSPPROH                                                       
         TM    BIT,CLISET+JOBSET   WAS CLIENT ENTERED                           
         BNZ   *+12                YES THAN MUST ENTER PRODUCT                  
         CLI   5(R2),0                                                          
         BE    VR80                                                             
*                                                                               
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVI   ERROR,NOVALONE                                                   
         CLI   PROLNP1,C'"'        NOT AVAILABLE ON FIRST LINE                  
         BE    ERREND                                                           
         CLI   8(R2),C'"'          SAME PRODUCT AS PREVIOUS                     
         BNE   *+16                                                             
         MVC   8(L'DSPPRO,R2),PREVPRO                                           
         MVC   5(1,R2),PREVPRLN                                                 
         MVC   PREVPRO,8(R2)       SAVE PREVIOUS PRODUCT                        
         MVC   PREVPRLN,5(R2)      SAVE PREVIOUS PRODUCT LENGTH                 
         MVC   AIO,AIO2                                                         
         GOTO1 VALPROD                                                          
         MVC   AIO,AIO1                                                         
         ZIC   RF,5(R2)            PRODUCT LENGTH                               
         BCTR  RF,0                                                             
         EXMVC RF,SVPRO,PRODCODE     SAVE PRODUCT                               
         OC    SVPRO,SPACES                                                     
         OI    BIT,PROSET                                                       
*                                                                               
VR80     L     R2,SVADDR           RESTORE R2 TO BEGINNING OF LINE              
         LA    R2,DSPJOBH          JOB IS NOT REQUIRED                          
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         L     R2,SVADDR           POINT BACK TO PRODUCT AND CONTINUE           
         LA    R2,DSPPROH                                                       
         B     VR85                                                             
*                                                                               
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVI   ERROR,NOVALONE                                                   
         CLI   PROLNJ1,C'"'        NOT AVAILABLE ON FIRST LINE                  
         BE    ERREND                                                           
         CLI   8(R2),C'"'          SAME PRODUCT AS PREVIOUS                     
         BNE   *+16                                                             
         MVC   8(L'DSPJOB,R2),PREVJOB                                           
         MVC   5(1,R2),PREVJBLN                                                 
         MVC   PREVJOB,8(R2)       SAVE PREVIOUS JOB                            
         MVC   PREVJBLN,5(R2)      SAVE PREVIOUS JOB LENGTH                     
         MVC   AIO,AIO2                                                         
         GOTO1 VALJOB                                                           
         MVC   AIO,AIO1                                                         
         MVI   ERROR,CLOSJOB                                                    
         TM    JOBSTAT,X'40'       IS JOB CLOSED?                               
         BO    ERREND                                                           
         MVI   ERROR,LOCKJOB                                                    
         TM    JOBSTAT,X'20'       IS JOB LOCKED?                               
         BO    ERREND                                                           
         MVI   ERROR,NOXJOB                                                     
         TM    JOBJSTAT,X'10'       IS JOB AN EXPENSE (XJOB?)                   
         BO    ERREND                                                           
         ZIC   RF,5(R2)            LENGTH OF JOB                                
         BCTR  RF,0                                                             
         EXMVC RF,SVJOB,JOBNUM       SAVE JOB NUMBER                            
         OC    SVJOB,SPACES                                                     
         OI    BIT,JOBSET                                                       
VR85     MVI   ERROR,DUPINPUT                                                   
         BAS   RE,DUPCHK           CHECK FOR DUPLICATE ENTRY                    
         BE    ERREND                                                           
         MVC   ELCLI,SVCLI         MOVE TO TABLE                                
         MVC   ELPRO,SVPRO                                                      
         MVC   ELJOB,SVJOB                                                      
*                                                                               
* VALIDATE PERCENTAGE AMOUNT                                                    
*                                                                               
VR90     MVI   ERROR,MISSPERC                                                   
         L     R2,SVADDR           RESTORE R2 TO BEGINNING OF LINE              
         LA    R2,DSPAMTH                                                       
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
*                                                                               
         ZAP   WORK(8),=P'0'                                                    
         ZIC   R1,0(R2)            NEED TO CHECK FOR % SIGN                     
         SH    R1,=H'8'                                                         
         TM    1(R2),X'02'         HEADER                                       
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         LA    RF,8(R2)                                                         
         SR    R3,R3                                                            
VR95     CLI   0(RF),C'%'                                                       
         BE    VR100                                                            
         LA    R3,1(R3)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,VR95                                                          
*                                                                               
         ZIC   R3,5(R2)                                                         
VR100    GOTO1 CASHVAL,DMCB,(X'84',8(R2)),(R3)                                  
         CLI   DMCB,X'FF'                                                       
         BE    VR105                                                            
         CP    DMCB+4(8),=P'100.0000'                                           
         BH    VR105                                                            
         CP    DMCB+4(8),=P'0'                                                  
         BNH   VR105                                                            
         MVC   WORK,SPACES                                                      
         ZAP   WORK(4),DMCB+4(8)                                                
         TM    BIT,NEWLINE         IS THIS A NEW LINE?                          
         BO    VR102               THEN ADD IT TO THE RUNNING TOTAL             
         ZAP   WORK+4(4),SVJTOT    ELSE ADD THE DIFFERENCE TO THE TOTAL         
         SP    WORK(4),WORK+4(4)                                                
VR102    AP    SVDTOT(4),WORK(4)                                                
         MVI   ERROR,PCTHIGH                                                    
         CP    SVDTOT(4),=P'100.0000' MAKE SURE TOTAL NOT > 100 %               
         BH    ERREND                                                           
*                                                                               
VR103    ZAP   ELAMT(4),DMCB+4(8)  MOVE TO TABLE                                
         L     R2,SVADDR           RESET R2 TO BEGINNING OF LINE                
         B     VR110                                                            
*                                                                               
VR105    MVI   ERROR,ERRPCT                                                     
         B     ERREND                                                           
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*        CREATE ELEMENT                                                         
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*                                                                               
VR110    XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING JBPELD,R6                                                        
         MVI   JBPEL,JBPELQ        X'D0'                                        
         MVI   JBPLN,JBPLNQ        ELEMENT LENGTH                               
         MVC   JBPCLI,ELCLI        CLIENT                                       
         MVC   JBPPRO,ELPRO        PRODUCT                                      
         MVC   JBPJOB,ELJOB        JOB                                          
         ZAP   JBPPERC(4),ELAMT(4) PERCENTAGE                                   
         GOTO1 ADDELEM                                                          
*                                                                               
         MVC   ELPRCLI,ELCLI       NOW FILL IN PREVIOUS                         
         MVC   ELPRPRO,ELPRO                                                    
         MVC   ELPRJOB,ELJOB                                                    
         OI    ELSTAT,ELPROC       THIS LINE HAS BEEN PROCESSED                 
*                                                                               
         TM    BIT,NEWLINE         INCREMENT TAB COUNTER IF A NEW LINE          
         BZ    VR140                                                            
         LH    R1,TABCOUNT         INCREMENT TABLE COUNTER                      
         LA    R1,1(R1)                                                         
         STH   R1,TABCOUNT                                                      
VR140    LA    R4,ELLNQ(R4)        NEXT ENTRY IN TABLE                          
         LA    R2,DSPLNQ(R2)      NEXT SCREEN LINE                              
         LA    R1,PROENDH          END OF SCREEN                                
         CR    R2,R1                                                            
         BNH   VR20                ADD NEXT LINE                                
*                                                                               
         CLI   ACTEQU,ACTADD       IF ADDING A NEW REC MAKE SURE THEY           
         BNE   VR150               ENTER SOME INPUT                             
         MVI   ERROR,MISSING                                                    
         LA    R4,ELEMBLK                                                       
         OC    0(ELLNQ,R4),0(R4)                                                
         BNZ   *+12                                                             
         LA    R2,PROLNC1H                                                      
         B     ERREND                                                           
*                                                                               
VR150    MVC   AIO,AIO1                                                         
         MVC   KEY(L'ACTKEY),KEYSAVE                                            
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*        ADD/PUT BACK RECORD                                                    
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   IOOPT,C'Y'          DOING MY OWN IO                              
         MVC   KEY(L'ACTKEY),SAVEKEY                                            
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'ACTKEY),KEYSAVE                                            
         BNE   VR160               GO ADD THIS NEW REC                          
         MVC   AIO,AIO1                                                         
         GOTO1 =A(DELOFF),DMCB,RR=RELO   TURN OFF DELETE BITS                   
         L     R6,AIO2                                                          
         CLC   KEY(L'ACTKEY),0(R6)                                              
         BNE   VRX                                                              
         BAS   RE,CHKDEL           CHECK IF SHOULD BE DELETED                   
         GOTO1 WRITE                                                            
         B     VRX                                                              
*                                                                               
VR160    MVC   AIO,AIO1                                                         
         GOTO1 ADD                                                              
*                                                                               
VRX      B     XIT                 DISPLAY REC CHANGES                          
*                                                                               
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        CHECK DELETE                                                           
***********************************************************************         
*                                                                               
CHKDEL   NTR1                                                                   
         USING JGRRECD,R6                                                       
         L     R6,AIO                                                           
         NI    JGRRSTA,X'FF'-X'80'    MAKE SURE NOT DELETED                     
         MVI   ELCODE,JBPELQ                                                    
         BAS   RE,GETEL                                                         
         BE    XIT                                                              
*                                                                               
         L     R6,AIO                                                           
         OI    JGRRSTA,X'80'       DELETE IF NO ELEMS                           
         LA    R6,BIGKEY                                                        
         OI    JGRKSTA,X'80'       DELETE IF NO ELEMS                           
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              CHECK FOR DUPLICATE ENTRY                                        
***********************************************************************         
*                                                                               
         USING ELEMTABD,R4                                                      
DUPCHK   NTR1                                                                   
         LR    R3,R4               R3 = ENTRY BEING CHECKED                     
         LA    R4,ELEMBLK          USE TABLE OF ELEMENTS                        
         B     DC20                                                             
DC10NX   LA    R4,ELLNQ(R4)                                                     
DC20     OC    0(ELLNQ,R4),0(R4)                                                
         BZ    DCXNO                                                            
DC20A    CR    R4,R3               DON'T CHECK THE ONE BEING CHECKED            
         BE    DC10NX                                                           
*                                                                               
         CLC   ELCLI,SVCLI        CHECK FOR MATCH ON CLT/PRD/JOB                
         BNE   DC10NX                                                           
         CLC   ELPRO,SVPRO                                                      
         BNE   DC10NX                                                           
         CLC   ELJOB,SVJOB                                                      
         BNE   DC10NX                                                           
         B     DCXYES              YES FOUND MATCH                              
*                                                                               
DCXNO    B     XNO                                                              
DCXYES   B     XYES                                                             
         EJECT                                                                  
***********************************************************************         
*              DISPLAY KEY                                                      
***********************************************************************         
         SPACE 1                                                                
DK       NTR1                                                                   
         L     R4,AIO                                                           
         USING JGRRECD,R4                                                       
         MVC   PROJGR,JGRKCODE                                                  
         LA    R2,PROJGRH                                                       
         OI    6(R2),X'80'                                                      
         MVC   KEY(L'ACTKEY),0(R4) FILL IN KEY                                  
         MVC   SAVEKEY,KEY                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              DISPLAY RECORD                                                   
***********************************************************************         
         SPACE 1                                                                
DR       NTR1                                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 SETHEIR             GET LEDGER LENGTHS                           
         MVC   AIO,AIO1                                                         
         MVC   LVLNA,LCLI          SAVE SJ LEDGER LENGTHS                       
         MVC   LVLNB,LPRO                                                       
         MVC   LVLNC,LJOB                                                       
         LA    R2,PROJGRNH                                                      
         GOTO1 NAMEOUT             DISPLAY NAME ELEMENT                         
DR05     GOTO1 PERSOUT             DISPLAY PERSON ELEMENT                       
         MVC   PROLACT,SPACES                                                   
         MVC   PROLACT(14),=C'LAST ACTIVITY:'                                   
         MVC   PROLACT+15(20),WORK+20                                           
         OI    PROLACTH+6,X'80'                                                 
*                                                                               
         GOTO1 =A(CLRSCRN),DMCB,RR=RELO                                         
         GOTO1 =A(BLDTAB),DMCB,RR=RELO          BUILD TABLE                     
*                                                                               
         USING ELEMTABD,R4                                                      
         USING DSPLINED,R2                                                      
         LA    R4,ELEMBLK                                                       
         LA    R2,PROLNC1H                                                      
*                                                                               
         CLI   PFKEY,0                                                          
         BNE   DR32                                                             
         CLI   ACTEQU,ACTSEL                                                    
         BE    DR37                                                             
         B     DR38                                                             
*                                                                               
DR32     CLI   PFKEY,7             UP                                           
         BNE   DR34                                                             
         MVC   STDISP,PRVSTDSP                                                  
         LA    R1,ELLNQ                                                         
         MH    R1,=H'8'            8 LINES                                      
         LH    R0,PRVSTDSP                                                      
         SR    R0,R1                                                            
         CH    R0,=H'0'                                                         
         BNL   *+6                 DISP FROM TOP                                
         SR    R0,R0                                                            
         STH   R0,PRVSTDSP                                                      
         B     DR38                                                             
*                                                                               
DR34     CLI   PFKEY,8             DOWN                                         
         BNE   DR38                                                             
         MVC   PRVSTDSP,STDISP                                                  
         MVC   STDISP,DLINE1                                                    
         B     DR38                                                             
*                                                                               
DR37     MVC   STDISP,=H'0'        DEFAULT TO BEGINNING                         
         MVC   PRVSTDSP,=H'0'                                                   
*                                                                               
DR38     LA    R0,ELLNQ            LENGTH OF ONE ENTRY                          
         MH    R0,TABCOUNT         NUMBER OF ENTRIES                            
         LH    R1,STDISP                                                        
         CR    R0,R1                                                            
         BH    DR39                                                             
         LA    R1,0                                                             
         STH   R1,STDISP                                                        
DR39     AR    R4,R1                                                            
*                                                                               
DR40NX   LA    R1,ELEMBLK                                                       
         LR    R0,R4               R4 POINTS TO ELEMENT BEING DISPLAYED         
         SR    R0,R1                                                            
         STH   R0,DLINE1           SAVE DISPLACEMENT INTO TABLE                 
*                                                                               
         OC    0(ELLNQ,R4),0(R4)        ANY MORE ENTRIES?                       
         BNZ   DR42                                                             
         NI    GENSTAT2,X'FF'-RETEQSEL     GET NEXT SELECTION                   
         LA    R0,0                START FROM TOP NEXT ENTER                    
         STH   R0,DLINE1                                                        
         B     DR100                                                            
*                                                                               
DR42     DS    0H                                                               
         MVC   DSPCLI,SPACES                                                    
         MVC   DSPPRO,SPACES                                                    
         MVC   DSPJOB,SPACES                                                    
*                                                                               
         CLC   ELCLI,SPACES        IS THERE A CLIENT                            
         BE    *+10                                                             
         MVC   DSPCLI,ELCLI                                                     
         OI    DSPCLIH+6,X'80'     XMIT                                         
         MVC   DSPPRO,ELPRO        DISPLAY PRODUCT                              
         OI    DSPPROH+6,X'80'                                                  
         CLC   ELJOB,SPACES        IS THERE A JOB                               
         BE    *+10                                                             
         MVC   DSPJOB,ELJOB                                                     
         OI    DSPJOBH+6,X'80'                                                  
*                                                                               
         MVC   DSPNAME,SPACES      READ SJ JOB REC TO GET NAME                  
         MVC   AIO,AIO2                                                         
         USING ACTRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   KEY(L'ACTKEY),SPACES                                             
         MVC   ACTKCPY(3),CUL                                                   
         LA    R3,ACTKACT                                                       
         ZIC   R1,LVLNA                                                         
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),ELCLI      CLIENT                                       
         LA    R3,1(R1,R3)                                                      
         ZIC   R1,LVLNB                                                         
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),ELPRO      PRODUCT                                      
         LA    R3,1(R1,R3)                                                      
         CLC   ELJOB,SPACES        FIRST CHECK IF THEY ENTERED A JOB            
         BE    DR45                                                             
         ZIC   R1,LVLNC                                                         
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),ELJOB      JOB                                          
DR45     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO1                                                         
         CLC   KEY(L'ACTKEY),KEYSAVE                                            
         BNE   DR50                                                             
         GOTO1 SETNAME,DMCB,AIO2,WORK    DISPLAY PRODUCT OR JOB NAME            
         MVC   DSPNAME,WORK                                                     
         OI    DSPNAMEH+1,X'20'                                                 
         OI    DSPNAMEH+6,X'80'                                                 
*                                                                               
DR50     LA    R3,ELAMT            DISPLAY AMOUNT                               
         LA    R6,DSPAMTH                                                       
         OC    DSPAMT,SPACES                                                    
         BAS   RE,DISAMT                                                        
         OI    DSPAMTH+6,X'80'                                                  
*                                                                               
         LA    R2,DSPLNQ(R2)       NEXT SCREEN LINE                             
         LA    R1,PROENDH          END OF LIST                                  
         CR    R2,R1                                                            
         BH    DR100               DISP TOTALS                                  
         LA    R4,ELLNQ(R4)        NEXT TABLE ENTRY                             
         B     DR40NX                                                           
*                                                                               
* DISPLAY TOTAL                                                                 
*                                                                               
DR100    DS    0H                                                               
         LA    R3,SVDTOT                                                        
         LA    R6,PROTOTH                                                       
         OC    PROTOT,SPACES                                                    
         CP    SVDTOT,=P'0'                                                     
         BZ    XIT                                                              
         BAS   RE,DISAMT                                                        
         OI    PROTOTH+6,X'80'                                                  
         OI    PROTOTH+1,X'20'                                                  
*                                                                               
         MVC   KEY(L'ACTKEY),SAVEKEY     RESTORE KEY                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              DISPLAY PERCENTAGE AMOUNT                                        
***********************************************************************         
*                                                                               
DISAMT   NTR1                                                                   
*                                                                               
         CURED (P4,(R3)),(8,8(R6)),4,FLOAT=-,ZERO=NOBLANK,ALIGN=RIGHT           
         STC   R0,5(R6)                                                         
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        CLEAR SOME FIELDS                                                      
***********************************************************************         
*                                                                               
CLRSCRN  NMOD1 0,*CLRSCR*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         LA    R2,PROLNC1H         CLEAR ALL FIELDS                             
         LA    R3,PROENDLH                                                      
*                                                                               
CSCLR    DS    0H                                                               
         ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         MVI   5(R2),X'00'                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BNH   CSCLR               NO                                           
*                                                                               
         LA    R2,PROTOTH          TOTAL LINE                                   
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'            8 FOR HEADER+1 FOR EX                        
         EXMVC R1,8(R2),SPACES                                                  
         MVI   5(R2),X'00'                                                      
         OI    6(R2),X'80'         XMIT                                         
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        UNPROTECT SOME FIELDS                                                  
***********************************************************************         
*                                                                               
         USING DSPLINED,R2                                                      
UNPROT   NMOD1 0,*UNPROT*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         LA    R2,PROLNC1H                                                      
UNPR10   NI    DSPCLIH+1,X'FF'-X'20'                                            
         NI    DSPCLIH+1,X'FF'-X'01'                                            
         OI    DSPCLIH+6,X'80'                                                  
         NI    DSPPROH+1,X'FF'-X'20'                                            
         NI    DSPPROH+1,X'FF'-X'01'                                            
         OI    DSPPROH+6,X'80'                                                  
         NI    DSPJOBH+1,X'FF'-X'20'                                            
         NI    DSPJOBH+1,X'FF'-X'01'                                            
         OI    DSPJOBH+6,X'80'                                                  
         NI    DSPAMTH+1,X'FF'-X'20'                                            
         NI    DSPAMTH+1,X'FF'-X'01'                                            
         OI    DSPAMTH+6,X'80'                                                  
         LA    R2,DSPLNQ(R2)                                                    
         LA    R3,PROENDLH                                                      
         CR    R2,R3                                                            
         BNH   UNPR10                                                           
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        TURN OFF DELETE BITS                                                   
***********************************************************************         
*                                                                               
         USING JGRRECD,R6                                                       
DELOFF   NMOD1 0,*DELOFF*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         L     R6,AIO                                                           
         NI    JGRRSTA,X'FF'-X'80'                                              
         LA    R6,KEY                                                           
         NI    JGRKSTA,X'FF'-X'80'                                              
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*    CREATE ELEMENTS TABLE FOR SCROLLING                                        
***********************************************************************         
*                                                                               
BLDTAB   NMOD1 0,*BLDTAB*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         USING ELEMTABD,R4                                                      
         LA    R4,ELEMBLK                                                       
         LR    R0,R4               CLEAR BLOCK FOR TABLE                        
         LH    R1,=Y(ELEMLNQ)                                                   
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    TABCOUNT,TABCOUNT   COUNT OF TABLE ENTRIES                       
         ZAP   SVDTOT,=P'0'                                                     
*                                                                               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'ACTKEY),KEYSAVE                                            
         BNE   BLDX                                                             
         CLI   ACTEQU,ACTDIS                                                    
         BE    *+8                                                              
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 =A(DELOFF),DMCB,RR=RELO                                          
         USING JGRRECD,R6                                                       
         L     R6,AIO2                                                          
*                                                                               
         USING JBPELD,R6                                                        
         MVI   ELCODE,JBPELQ       D0 JOB PERCENTAGE ELEM                       
         BAS   RE,GETELIO                                                       
         B     *+8                                                              
BLD20    BAS   RE,NEXTEL                                                        
         BNE   BLDX                                                             
         MVC   ELCLI,JBPCLI        CLIENT                                       
         MVC   ELPRO,JBPPRO        PRODUCT                                      
         MVC   ELJOB,JBPJOB        JOB                                          
         MVC   ELPRCLI,JBPCLI                                                   
         MVC   ELPRPRO,JBPPRO                                                   
         MVC   ELPRJOB,JBPJOB                                                   
         ZAP   ELAMT,JBPPERC       PERCENTAGE                                   
         AP    SVDTOT(4),ELAMT                                                  
         NI    ELSTAT,X'FF'-ELPROC                                              
         LH    R1,TABCOUNT         INCREMENT TABLE COUNTER                      
         LA    R1,1(R1)                                                         
         STH   R1,TABCOUNT                                                      
*                                                                               
         MVI   ERROR,REC2BIG                                                    
         CLC   TABCOUNT,=Y(MAXCOUNT)  END OF TABLE REACHED?                     
         BH    *+12                                                             
         LA    R4,ELLNQ(R4)                                                     
         B     BLD20                                                            
         LA    R2,PROJGRH                                                       
         B     ERREND                                                           
         DC    H'0'                MUST EXPAND TABLE                            
*                                                                               
BLDX     MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'ACTKEY),SAVEKEY   RESTORE KEY                              
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* ERROR MESSAGES AND OTHER STUFF                                                
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
*                                                                               
ERRTOOBG MVI   ERROR,REC2BIG                                                    
         B     ERREND                                                           
EINVPCT  MVI   ERROR,BADPER                                                     
         B     ERREND                                                           
ERRDUP   MVI   ERROR,DUPINPUT                                                   
         B     ERREND                                                           
ERRLINE1 MVI   ERROR,NOVALONE                                                   
         B     ERREND                                                           
ERRCLOS  MVI   ERROR,CLOSJOB                                                    
         B     ERREND                                                           
ERRLOCK  MVI   ERROR,LOCKJOB                                                    
         B     ERREND                                                           
ERRMPERC MVI   ERROR,MISSPERC                                                   
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* DSECTS ARE HIDDEN IN HERE                                                     
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*                                                                               
*DDSPOOLD                                                                       
         PRINT  OFF                                                             
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT  OFF                                                             
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT  OFF                                                             
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT  OFF                                                             
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROB9D                                                       
         EJECT                                                                  
***********************************************************************         
*  REMAINING WORK AREA                                                          
***********************************************************************         
*                                                                               
RELO     DS    A                                                                
SAVERC   DS    F                   SAVED RC                                     
SVADDR   DS    F                                                                
STDISP   DS    H                                                                
PRVSTDSP DS    H                                                                
DLINE1   DS    H                                                                
TABCOUNT DS    H                                                                
SVDTOT   DS    PL4                 TOTAL PERCENTAGE OF ALL JOBS                 
SVJTOT   DS    PL4                 SAVED % FROM ELEMENT BEING CHANGED           
SVJCODE  DS    CL8                 SAVED JOB GROUP CODE                         
SVCLI    DS    CL5                                                              
SVPRO    DS    CL6                                                              
SVJOB    DS    CL6                                                              
PREVCLI  DS    CL5                 PREVIOUS CLEINT                              
PREVCLN  DS    XL1                 PREVIOUS CLIENT LENGTH                       
PREVPRO  DS    CL6                 PREVIOUS PRODUCT                             
PREVPRLN DS    XL1                 PREVIOUS PRODUCT LENGTH                      
PREVJOB  DS    CL6                 PREVIOUS JOB                                 
PREVJBLN DS    CL6                 PREVIOUS JOB LENGTH                          
LVLNA    DS    XL1                 SAVED SJ LEDGER LENGTHS                      
LVLNB    DS    XL1                                                              
LVLNC    DS    XL1                                                              
BIT      DS    XL1                                                              
CLISET   EQU   X'80'               ENTERED CLIENT                               
PROSET   EQU   X'40'               ENTERED PRODUCT                              
JOBSET   EQU   X'20'               ENTERED JOB                                  
NEWLINE  EQU   X'10'               ADDING A NEW ENTRY                           
*                                                                               
SAVEKEY  DS    XL42                                                             
*                                                                               
MAXCOUNT EQU   50                  MAX # OF TABLE ENTRIES                       
*                                                                               
BLOCKSD  DSECT                                                                  
ELEMBLK  DS    50CL(ELLNQ)                                                      
ELEMLNQ  EQU   *-ELEMBLK                                                        
*                                                                               
***********************************************************************         
*  DSECTS                                                                       
***********************************************************************         
*                                                                               
* DSECT TO COVER ELEMENT TABLE                                                  
*                                                                               
ELEMTABD DSECT                                                                  
ELPRCLI  DS    CL5                 PREVIOUS CLIENT                              
ELPRPRO  DS    CL6                 PREVIOUS PRODUCT                             
ELPRJOB  DS    CL6                 PREVIOUS JOB                                 
ELCLI    DS    CL5                 CLIENT                                       
ELPRO    DS    CL6                 PROD                                         
ELJOB    DS    CL6                 JOB                                          
ELAMT    DS    CL4                 PERCENTAGE                                   
ELSTAT   DS    CL1                 STATUS BYTE                                  
ELPROC   EQU   X'80'               ALREADY VALIDATED THIS LINE                  
ELLNQ    EQU   *-ELEMTABD                                                       
*                                                                               
* DSECT TO COVER DISPLAY LINE                                                   
*                                                                               
DSPLINED DSECT                                                                  
DSPCLIH  DS    CL8                                                              
DSPCLI   DS    CL5                 CLIENT                                       
DSPPROH  DS    CL8                                                              
DSPPRO   DS    CL6                 PRODUCT                                      
DSPJOBH  DS    CL8                                                              
DSPJOB   DS    CL6                 JOB                                          
DSPNAMEH DS    CL8                                                              
DSPNAME  DS    CL36                JOB NAME                                     
DSPAMTH  DS    CL8                                                              
DSPAMT   DS    CL8                 PERCENTAGE                                   
DSPLNQ   EQU   *-DSPLINED                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACPRO62   09/12/02'                                      
         END                                                                    
