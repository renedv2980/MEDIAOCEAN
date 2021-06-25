*          DATA SET RERMP1FB   AT LEVEL 017 AS OF 05/01/02                      
*PHASE T8101FB,+0                                                               
*INCLUDE INVDAY                                                                 
*INCLUDE REBKLST                                                                
         TITLE 'T8101F - REPPAK FILE MAINT - DEMO MENU ADD/CHA/DIS/DEL'         
T8101F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8101F,RR=R5                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T8101F+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
         ST    R5,RELO                                                          
*                                                                               
         OI    GENSTAT4,CONFDEL    CONFIRM DELETES                              
*                                                                               
         EJECT                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
*        CLI   MODE,RECDEL         DELETE                                       
*        BE    DEL                                                              
*        CLI   MODE,RECREST        AND RESTORE ARE INVALID                      
*        BE    RESTORE                                                          
         B     EXIT                                                             
         MVC   RERROR(2),=AL2(INVACT)                                           
         LA    R2,CONACTH                                                       
         B     ERREND                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
VKEY     DS    0H                                                               
*              INIT WORK AREA                                                   
         XC    KEY,KEY                                                          
         SPACE 2                                                                
*              VALIDATE THE CODE                                                
         XC    CODEHLD,CODEHLD                                                  
         LA    R2,MINCODEH                                                      
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0             NOT REQUIRED IF LIST                         
         BNE   VK50                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK100                                                            
         CLI   ACTNUM,ACTREP       OR REPORT                                    
         BE    VK100                                                            
VK50     CLI   5(R2),0             REQUIRED                                     
         BE    ERREND                                                           
         MVC   CODEHLD,8(R2)                                                    
         OC    CODEHLD,=2X'40'                                                  
         SPACE 2                                                                
*                                                                               
*                                                                               
VK100    MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RDEMREC,R6                                                       
*                                                                               
         MVI   RDEMKTYP,X'23'                                                   
         MVC   RDEMKREP,AGENCY                                                  
         MVC   RDEMKDEM,CODEHLD                                                 
         MVC   SAVEKEY,KEY                                                      
*                                                                               
VKXIT    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
DKEY     DS    0H                                                               
         L     R6,AIO                                                           
         USING RDEMREC,R6                                                       
*                                                                               
         LA    R2,MINCODEH                                                      
         MVC   8(2,R2),RDEMKDEM                                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         MVC   CODEHLD,RDEMKDEM                                                 
*                                                                               
DKXIT    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
DREC     DS    0H                                                               
*                                                                               
         L     R6,AIO                                                           
         USING RDEMREC,R6                                                       
*                                                                               
         BAS   RE,CLRSCRN                                                       
*                                                                               
         CLC   CONACT(3),=CL3'DEL'                                              
         BNE   *+8                                                              
         OI    CONACTH+6,X'01'                                                  
*                                                                               
*  COMMENT                                                                      
         MVC   MINCOMM,RDEMDES                                                  
         OI    MINCOMMH+6,X'80'                                                 
*                                                                               
*  DEMOS                                                                        
         USING RDEMDEL,R3                                                       
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
*                                                                               
         LA    R4,4                                                             
         LA    R2,MINDEM1H         DEMOS                                        
DEM5     MVC   8(L'MINDEM1,R2),SPACES                                           
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTUF           NEXT DEMO FIELD                              
         BCT   R4,DEM5                                                          
         SPACE 1                                                                
         XC    WORK2(200),WORK2                                                 
         LA    R2,RDEMDEM          DEMO LIST                                    
         LA    R4,WORK2                                                         
         ZIC   R5,RDEMNUM                                                       
         SPACE 1                                                                
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         SPACE 1                                                                
DEM10    CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         GOTO1 DEMOCON,DMCB,(0,0(R2)),(6,0(R4)),(0,DBLOCK),0                    
         CLI   1(R2),C'I'          REVERSE FUDGE FOR DEMOCON                    
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
         SPACE 1                                                                
         LR    RE,R4                                                            
         LA    RF,6                                                             
DEM13    CLI   0(RE),C' '          PUT COMMA AT END OF DEMO EXPRESSION          
         BE    DEM14                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,DEM13                                                         
DEM14    MVI   0(RE),C','                                                       
         LA    R4,1(RE)                                                         
         LA    R2,3(R2)                                                         
         CLI   0(R2),X'FF'         END OF LIST                                  
         BE    *+8                                                              
         BCT   R5,DEM10                                                         
         SPACE 1                                                                
         LA    R4,WORK2                                                         
         LA    R2,MINDEM1H                                                      
         SPACE 1                                                                
*  BREAK UP WORK2 INTO CHUNKS THAT WILL FIT ON SCREEN PROPERLY                  
         SPACE 1                                                                
DEM15    ZIC   RE,0(R2)            LENGTH OF FIELD                              
         SH    RE,=H'8'            MINUS HEADER                                 
         LA    R5,0(RE,R4)                                                      
DEM20    CLI   0(R5),C','                                                       
         BE    DEM30                                                            
         BCTR  R5,R0               BACK UP ONE                                  
         BCTR  RE,R0                                                            
         B     DEM20                                                            
         SPACE 1                                                                
DEM30    BCTR  RE,R0               BACK UP FROM COMMA                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R4)                                                    
         SPACE 1                                                                
         LR    R4,R5                                                            
         BAS   RE,NEXTUF                                                        
         LA    R4,1(R4)            GET PAST COMMA                               
         OC    0(3,R4),0(R4)       MORE DEMOS                                   
         BNZ   DEM15                                                            
         SPACE 1                                                                
DEMX     B     EXIT                                                             
         DROP  R6,R3                                                            
         SPACE 5                                                                
*                                                                               
* CLEAR THE SCREEN                                                              
*                                                                               
CLRSCRN  NTR1                                                                   
         LA    R2,MINCOMMH         FIRST FIELD                                  
         LA    R3,MINTAGH          END OF SCREEN                                
*                                                                               
CLRSC20  CR    R2,R3                                                            
         BNL   CLRSCEX                                                          
         TM    1(R2),X'20'         CHECK IF PROTECTED                           
         BO    CLRSC60                                                          
*                                                                               
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1               ANY INPUT                                    
         BZ    CLRSC60             NO, BYPASS                                   
         BCTR  R1,0                                                             
         EX    R1,MVESPACE                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
CLRSC60  ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     CLRSC20                                                          
*                                                                               
CLRSCEX  B     EXIT                                                             
*                                                                               
MVESPACE MVC   8(0,R2),SPACES                                                   
         SPACE 5                                                                
* SUBROUTINE TO POINT R2 TO NEXT UNPROTECTED FIELD                              
NEXTUF   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BCR   8,RE                                                             
         CLI   0(R2),9                                                          
         BE    NEXTUF2                                                          
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUF                                                           
         BR    RE                                                               
NEXTUF2  CLI   9(R2),0             CHECK FOR LAST                               
         BNE   NEXTUF4                                                          
         CR    R2,R2               IF LAST, SET CC=                             
         BR    RE                                                               
NEXTUF4  LTR   R2,R2               NOT LAST, SET CC NOT=                        
         BR    RE                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              LIST RECORD ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
LIST     DS    0H                                                               
         OC    KEY(27),KEY                                                      
         BNZ   LR100                                                            
*                                                                               
         SPACE                                                                  
         LA    R6,KEY                                                           
         USING RDEMREC,R6                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(27),SAVEKEY                                                  
LR100    GOTO1 HIGH                                                             
         B     LR220                                                            
*                                                                               
LR200    GOTO1 SEQ                                                              
LR220    LA    R6,KEY                                                           
         CLC   SAVEKEY(25),KEY                                                  
         BNE   LREXT                                                            
         OC    CODEHLD,CODEHLD                                                  
         BZ    LR230                                                            
         CLC   SAVEKEY+25(2),KEY+25  CHECK CODE MATCH                           
         BNE   LREXT                                                            
LR230    MVC   SAVEKEY,KEY                                                      
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
*                                                                               
         MVC   LRTCODE,RDEMKDEM    AVAIL CODE                                   
*                                                                               
         MVC   LRTCOMM,RDEMDES     COMMENT                                      
*                                                                               
         GOTO1 LISTMON                                                          
         B     LR200               GOTO READ SEQ                                
*                                                                               
LREXT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R5,R6                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                                  
***********************************************************************         
PR       DS    0H                                                               
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
                                                                                
         MVI   ALLOWLIN,8          NEED 8 LINES TO PRINT A MENU                 
         MVI   FORCEHED,C'Y'       START OF REPORT                              
*                                                                               
         XC    KEY,KEY             INIT KEYAREA                                 
*                                                                               
         LA    R6,KEY              ESTABLISH KEY                                
         USING RDEMKEY,R6                                                       
                                                                                
         MVC   RDEMKEY,SAVEKEY     STARTING KEY                                 
*                                                                               
         GOTO1 HIGH                                                             
                                                                                
PRLOOP   DS    0H                                                               
                                                                                
         CLC   RDEMKEY(RDEMKDEM-RDEMKEY),SAVEKEY  SAME REP & REC TYPE           
         BNE   PRX                                                              
*                                                                               
         OC    CODEHLD,CODEHLD     IF MENU CODE ENTERED                         
         BZ    PR30                                                             
*                                                                               
         CLC   RDEMKDEM,SAVEKEY+RDEMKDEM-RDEMKEY  CODE MUST MATCH               
         BNE   PRCONT                                                           
*                                                                               
PR30     MVC   SAVEKEY,KEY         SAVE CURRENT KEY                             
*                                                                               
         GOTO1 GETREC              READ IN DEMO MENU RECORD                     
*                                                                               
         L     R6,AIO              SWITCH POINTER                               
         USING RDEMREC,R6                                                       
                                                                                
         MVC   P(4),=C'CODE'                                                    
         MVC   P+6(L'RDEMKDEM),RDEMKDEM                                         
                                                                                
         MVC   P+17(11),=C'DESCRIPTION'                                         
         MVC   P+30(L'RDEMDES),RDEMDES                                          
                                                                                
         BAS   RE,PRINT                                                         
         BAS   RE,PRINT            PRINT SPACING LINE                           
                                                                                
         MVC   P(09),=C'DEMO LIST'                                              
                                                                                
         MVI   ALLOWLIN,8          NEED 8 LINES TO PRINT A MENU                 
         BAS   RE,PRINT                                                         
         BAS   RE,PRINT            PRINT SPACING LINE                           
         MVI   ALLOWLIN,0          CLEAR LINE RESERVATION                       
                                                                                
*                                                                               
*        PRINT DEMOS DEMOS                                                      
*                                                                               
*        FIND DEMO ELEMENT                                                      
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO),0                          
         CLI   12(R1),0                                                         
         BE    PRCONT              NONE FOUND                                   
*                                                                               
         L     R3,12(R1)           POINT TO FOUND ELEMENT                       
*                                                                               
         USING RDEMDEL,R3          ESTABLISH DEMOS ELEMENT                      
*                                                                               
         LA    R2,RDEMDEM          DEMO LIST                                    
*                                                                               
         LA    R4,P+6              PRINT AREA                                   
         ZIC   R5,RDEMNUM          NUMBER OF DEMOS IN ELEMENT                   
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         LA    R0,12               NUMBER OF DEMOS PER LINE                     
*                                                                               
PRDEMLP  DS    0H                                                               
*                                                                               
         CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
*                                                                               
         GOTO1 DEMOCON,DMCB,(0,0(R2)),(6,0(R4)),(0,DBLOCK),0                    
*                                                                               
         CLI   1(R2),C'I'          REVERSE FUDGE FOR DEMOCON                    
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
*                                                                               
PRDEMCN  DS    0H                                                               
*                                                                               
         BCT   R0,PRDEMCN1         TEST FOR END OF PRINT LINE                   
*                                  END REACHED                                  
*                                                                               
         BAS   RE,PRINT            PRINT LINE                                   
         LA    R0,12               RESET COUNTER                                
         LA    R4,P+8              START OF PRINT AREA                          
         B     *+8                                                              
*                                                                               
PRDEMCN1 DS    0H                                                               
*                                                                               
         LA    R4,7(R4)            BUMP TO NEXT PRINT AREA                      
*                                                                               
         BCT   R5,PRDEMLP                                                       
*                                                                               
PRDEMDN  DS    0H                                                               
*                                                                               
         CLI   RDEMNUM,12          UNLESS EXACTLY 12                            
         BE    *+8                                                              
         CLI   RDEMNUM,24            OR 24 DEMOS                                
         BE    *+8                                                              
         BAS   RE,PRINT            PRINT CURRENT LINE                           
*                                                                               
         BAS   RE,PRINT            PRINT SPACING LINE                           
*                                                                               
                                                                                
PRCONT   GOTO1 SEQ                 GET NEXT DEMO MENU RECORD                    
         LA    R6,KEY              RESET KEY POINTER                            
*                                                                               
         B     PRLOOP                                                           
*                                                                               
PRDONE   DS    0H                                                               
*                                                                               
                                                                                
PRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                                  
***********************************************************************         
PRINT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* REPORT HEADLINE SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,AGYNAME                                                     
         PSPEC H1,76,RUN                                                        
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,76,REQUESTOR                                                  
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
***********************************************************************         
HOOK     NTR1                                                                   
         L     R6,AIO                                                           
         USING RDEMREC,R6                                                       
                                                                                
         MVC   H6(16),=C'DEMO MENU REPORT'                                      
                                                                                
HOOK20   DS    0H                                                               
                                                                                
HOOK80   DS    0H                                                               
         GOTO1 CENTER,DMCB,H6,88                                                
                                                                                
HOOKX    B     EXIT                                                             
         DROP  R6                                                               
         TITLE 'T8101B - RERMP1B - SETS - DAYPART SET - VALDPT'                 
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
VREC     DS    0H                                                               
*                                                                               
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING RDEMREC,R6                                                       
*                                                                               
         MVI   RDEMKTYP,X'23'                                                   
         MVC   RDEMKREP,AGENCY                                                  
         MVC   RDEMKDEM,CODEHLD                                                 
*                                                                               
         MVC   RDEMLEN,=Y(114)     TOTAL LENGTH OF KEY AND X'01'                
         MVC   RDEMCODE(2),=X'0150'                                             
         SPACE 1                                                                
         MVI   ERROR,MISSING                                                    
         LA    R2,MINCOMMH         DESCRIPTION                                  
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVC   RDEMDES,8(R2)                                                    
         OC    RDEMDES,SPACES                                                   
         XC    RDEMSPR,RDEMSPR                                                  
         SPACE 1                                                                
         XC    WORK2(80),WORK2    BUILD DEMO ELEMENT                            
         XC    WORK3(80),WORK3    FOR DEMOVAL                                   
         LA    R3,WORK2                                                         
         MVI   0(R3),X'02'                                                      
         LA    R2,MINDEM1H         DEMO                                         
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         SPACE 1                                                                
         MVI   ERROR,INVALID                                                    
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         SPACE 1                                                                
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         SPACE 1                                                                
         GOTO1 CDEMOVAL,DMCB,(4,(R2)),(24,WORK3),(0,DBLOCK)                     
         DROP  R7                                                               
         CLI   4(R1),0                                                          
         BE    ERREND              INVALID INPUT FIELD                          
         MVC   RDEMNUM,4(R1)       SAVE NUMBER OF DEMOS                         
         ZIC   R1,RDEMNUM                                                       
         MH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R3),WORK3       DEMO LIST TO ELEMENT                         
         SPACE 1                                                                
         LA    R1,RDEMDEM-RDEMDEL+1(R1)                                         
         STC   R1,1(R3)            COMPUTED ELEMENT LENGTH                      
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),(R3),=C'ADD=CODE'              
         B     DREC                                                             
         DROP  R6                                                               
         EJECT                                                                  
ERREND   GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
REPFILE  DC    CL8'REPFILE'                                                     
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
LLINED   DSECT                                                                  
LRTCODE  DS    CL2                                                              
         DS    CL6                                                              
LRTCOMM  DS    CL60                                                             
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RERMPFFD                                                                      
* DDGENTWA                                                                      
* RERMPWTWA                                                                     
* RERMPC1D                                                                      
* RERMPC2D                                                                      
* REGENMKT                                                                      
* REGENREP(A)                                                                   
* RERMPWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPC1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPC2D                                                       
         EJECT                                                                  
       ++INCLUDE RERMPWTWA                                                      
         EJECT                                                                  
REINVREC DSECT                                                                  
       ++INCLUDE REGENDEM                                                       
         EJECT                                                                  
       ++INCLUDE RERMPWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*              WORK AREA                                                        
*                                                                               
CODEHLD  DS    CL2                 DEMO MENU CODE                               
WORK2    DS    CL200               EXTRA WORK AREA                              
WORK3    DS    CL100               EXTRA WORK AREA                              
SAVEKEY  DS    CL27                                                             
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
RINVD    DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DDCOMFACS                                                      
         SPACE 5                                                                
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
T813FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T813FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017RERMP1FB  05/01/02'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
