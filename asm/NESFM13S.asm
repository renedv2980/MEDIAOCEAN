*          DATA SET NESFM13S   AT LEVEL 153 AS OF 05/01/02                      
*PHASE T31C13A,+0                                                               
         TITLE 'NESFM13 -  PROGRAM REC'                                         
         PRINT NOGEN                                                            
T31C13   CSECT                                                                  
         NMOD1 0,T31C13,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         USING T31C13,RB,R8                                                     
         ST    R3,RELO                                                          
         MVI   SPFIL20,C' '                                                     
         MVC   SPFIL20+1(L'SPFIL20-1),SPFIL20                                   
         SPACE                                                                  
         MVI   GOAGAIN,C'N'        RESET SWAP SWITCH                            
         MVC   AIO,AIO1                                                         
         L     R7,AIO1                                                          
         USING NPGRECD,R7                                                       
         SPACE 3                                                                
         OI    CONRECH+6,X'01'     VALIDATE EVERY TIME (PF KEY NEED)            
*                                                                               
         BAS   RE,CHKCABL          IF CABLE SET CABLE BIT                       
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   VKCHK                                                            
         MVC   SVDMWORK,DMWORK                                                  
         BAS   RE,GOPROCPF                                                      
         BAS   RE,VR                                                            
         MVC   DMWORK(96),SVDMWORK                                              
         B     EXIT                                                             
VKCHK    CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   XRADD                                                            
         BAS   RE,GOPROCPF                                                      
         GOTO1 =A(OPPFRTN),DMCB,(3,DUB),(R9),(RA),(RC),RR=RELO  VK              
         B     EXIT                                                             
XRADD    CLI   MODE,XRECADD        ADD PAASIVE KEY                              
         BE    GOAK                                                             
         CLI   MODE,XRECPUT        CHANGE PAASIVE KEY                           
         BE    GOAK                                                             
         CLI   MODE,XRECREST       RESTORE PASSIVE KEY                          
         BE    RK                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   LRCHK                                                            
         BAS   RE,GOPROCPF                                                      
         B     DR                                                               
LRCHK    CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LR                                                               
         CLI   MODE,RECDEL         DELETE RECORDS                               
         BE    DELMSG                                                           
         CLI   MODE,PROCPFK        PROCESS PF KEYS                              
         BNE   EXIT                                                             
         GOTO1 =A(OPPFRTN),DMCB,(1,DUB),(R9),(RA),(RC),RR=RELO  PF              
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* GOTO PROCPF ROUTINE                                                           
GOPROCPF NTR1                                                                   
         GOTO1 =A(OPPFRTN),DMCB,(2,DUB),(R9),(RA),(RC),RR=RELO PROCPF           
         B     EXIT                                                             
         EJECT                                                                  
DK       DS     0H                                                              
*                                                                               
         MVC   PGMNET(4),QNET                                                   
         FOUT  PGMNETH                                                          
*                                                                               
         MVC   PGMPGR(6),KEY+5                                                  
         FOUT  PGMPGRH                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,KEY+11),(5,PGMEDT)                                
         FOUT  PGMEDTH                                                          
*                                                                               
         GOTO1 VSETSPT                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
DR       DS    0H                                                               
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   *+10                                                             
         MVC   SVLIST,LISTDIR                                                   
         CLI   PGMSPECH+5,0        CHK FOR SPECIAL INPUT                        
         BE    DR100                                                            
         CLC   PGMSPEC(4),=C'NTI,'                                              
         BE    DR100                                                            
         B     FMTPCT              MUST BE PERCENTAGE ADJUSTMENT                
*                                                                               
DR100    LA    R3,24(R7)                                                        
         MVI   ELCODE,X'5D'                                                     
         BAS   RE,NXTEL                                                         
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR MUST HAVE 5D ELEM                
         MVC   PGMNO,=C' N'                                                     
         CLC   5(2,R3),=X'580E'   SEE IF NEW PROGRAM                            
         BE    DR120              YES                                           
         MVC   PGMNO,=C' 0'                                                     
DR120    FOUT  PGMNOH                                                           
*                                                                               
         XC    PGMDYPT,PGMDYPT                                                  
         XC    PGMSDAT,PGMSDAT                                                  
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'93'                                                     
         BAS   RE,NXTEL                                                         
         BNE   DR130                                                            
         MVC   PGMDYPT(1),4(R3)                                                 
         OC    2(2,R3),2(R3)                                                    
         BZ    DR130                                                            
         GOTO1 DATCON,DMCB,(2,2(R3)),(8,PGMSDAT)                                
DR130    FOUT  PGMDYPTH                                                         
         FOUT  PGMSDATH                                                         
*                                                                               
         XC    PGMNCDE,PGMNCDE                                                  
         XC    PGMOTH,PGMOTH                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR180                                                            
*                                                                               
         MVC   PGMNCDE(6),2(R6)    NAD CODE                                     
*                                                                               
*--OTHER FIELD DISPLAY                                                          
         XC    BYTE,BYTE                                                        
         LA    R2,PGMOTH                                                        
         OC    8(12,R6),8(R6)      FAX NUMBER                                   
         BZ    DR140A                                                           
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(2,R2),=C'F='                                                   
         MVC   2(12,R2),8(R6)                                                   
         MVI   BYTE,C'Y'                                                        
         BAS   RE,SETR2                                                         
*                                                                               
DR140A   CLI   21(R6),0         MIRROR CODE                                     
         BE    DR140B                                                           
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(2,R2),=C'M='                                                   
         MVC   2(1,R2),21(R6)                                                   
         MVI   BYTE,C'Y'                                                        
         BAS   RE,SETR2                                                         
*                                                                               
DR140B   TM    20(R6),X'80'                                                     
         BZ    DR140C                                                           
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(6,R2),=CL6'WINDOW'                                             
         LA    R2,6(R2)                                                         
         MVI   BYTE,C'Y'                                                        
         BAS   RE,SETR2                                                         
*                                                                               
         USING NPGEL03,R6                                                       
DR140C   OC    NPGSDPT,NPGSDPT     SUB-DAYPART                                  
         BZ    DR140E                                                           
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(3,R2),=C'SD='                                                  
         MVC   3(3,R2),NPGSDPT                                                  
         LA    R2,6(R2)                                                         
*                                                                               
DR140E   OC    NPGTCAR,NPGTCAR     TCAR LEVEL                                   
         BZ    DR140G                                                           
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(5,R2),=C'TCAR='                                                
         MVC   5(1,R2),NPGTCAR                                                  
         LA    R2,6(R2)                                                         
*                                                                               
DR140G   OC    NPGMRUN,NPGMRUN     MULTI-RUN                                    
         BZ    DR180                                                            
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(3,R2),=C'MR='                                                  
         LA    R2,3(R2)                                                         
         EDIT  (1,NPGMRUN),(2,(R2)),ALIGN=LEFT                                  
         AR    R2,R0                                                            
         DROP  R6                                                               
*                                                                               
DR180    FOUT  PGMOTHH                                                          
         FOUT  PGMNCDEH                                                         
*                                                                               
DR190    LA    R3,24(R7)                                                        
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NXTEL                                                         
         BE    DR200                                                            
         DC    H'0'                MUST FIND 92 ELEM                            
*                                                                               
DR200    DS    0H                  FMTPCT RETURNS HERE                          
         USING NPGELEM,R3                                                       
         XC    WORK(4),WORK                                                     
         GOTO1 UNDAY,DMCB,NPGDAY,WORK                                           
         FOUT  PGMDAYH,WORK,4                                                   
         XC    PGMTIME,PGMTIME                                                  
         GOTO1 UNTIME,DMCB,NPGTIME,PGMTIME                                      
         FOUT  PGMTIMEH                                                         
         FOUT  PGMNAMEH,NPGNAME                                                 
         XC    PGMSHR,PGMSHR                                                    
         OC    NPGSHARE,NPGSHARE                                                
         BZ    DR240                                                            
         LA    R2,PGMSHR                                                        
         TM    NPGSTAT,X'80'                                                    
         BZ    DR220                                                            
         LA    R2,PGMSHR+1                                                      
         MVI   PGMSHR,C'R'                                                      
*                                                                               
DR220    DS    0H                                                               
*        CLI   QPTYPE,C'N'                                                      
*        BE    DR225        TAKE COMMENTS OUT WHEN 2 DEC RATING                 
*        CLI   QPTYPE,C'S'  IS READY TO GO LIVE.                                
*        BE    DR225                                                            
*        CLI   PGMSHR,C'R'                                                      
*        BE    DR230                                                            
DR225    EDIT  (B2,NPGSHARE),(5,0(R2)),1,ALIGN=LEFT                             
         B     DR240                                                            
DR230    EDIT  (B2,NPGSHARE),(5,0(R2)),2,ALIGN=LEFT                             
DR240    FOUT  PGMSHRH                                                          
         FOUT  PGMFLTRH,NPGFILT                                                 
         XC    PGMNTI,PGMNTI                                                    
         OC    NPGPPNO,NPGPPNO                                                  
         BZ    DR300                                                            
         SR    R0,R0                                                            
         ICM   R0,3,NPGPPNO                                                     
         EDIT  (R0),(5,PGMNTI),ALIGN=LEFT,FILL=0                                
*                                                                               
DR300    FOUT  PGMNTIH                                                          
*                                                                               
         OC    NPGROT,NPGROT                                                    
         BE    DR310                                                            
         XC    PGMROT(16),PGMROT                                                
         GOTO1 UNDAY,DMCB,NPGROT,PGMROT                                         
         FOUT  PGMROTH                                                          
*                                                                               
DR310    CLI   PGMSPECH+5,0        CHK FOR INPUT IN SPECIAL                     
         BE    *+14                NO                                           
         CLC   PGMSPEC(4),=C'NTI,'                                              
         BE    FMTNTI                                                           
         DROP  R3                                                               
*                                                                               
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'93'                                                     
         BAS   RE,NXTEL                                                         
         BNE   DR350                                                            
         MVC   ELEMCODE,0(R3)                                                   
         LA    R3,5(R3)            POINT TO BEGINNING OF DEMOS                  
         B     DR360                                                            
*                                                                               
DR350    LA    R3,24(R7)                                                        
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NXTEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND 92 OR 93 ELEMENT                   
         MVC   ELEMCODE,0(R3)                                                   
         LA    R3,32(R3)            POINT TO BEGINNING OF DEMOS                 
*                                                                               
DR360    LA    R2,PGMW1H           FIRST VPH FIELD                              
         LA    R5,MAXVPHF          FOR BCT                                      
*        LA    R4,DISPTAB                                                       
         L     R4,=A(DISPTAB)                                                   
         A     R4,RELO                                                          
*                                                                               
DR400    LR    R6,R3                                                            
         MVC   8(4,R2),=C'0   '                                                 
         CLI   ELEMCODE,X'92'                                                   
         BNE   DR410                                                            
         CLI   0(R4),32            92 ELEMENT ONLY HAS 32 DEMOS                 
         BH    DR420                                                            
         ZIC   R0,0(R4)                                                         
         AR    R6,R0               ADD DISPLACEMENT                             
         CLI   0(R6),0                                                          
         BE    DR420                                                            
         ZIC   R1,0(R6)                                                         
         LA    RE,10                                                            
         MR    R0,RE                                                            
         EDIT  (R1),(4,8(R2)),0,ALIGN=LEFT                                      
         B     DR420                                                            
*                                                                               
DR410    ZIC   R0,0(R4)                                                         
         SLL   R0,1                MULTIPLY R0 BY 2                             
         AR    R6,R0               ADD DISPLACEMENT                             
         OC    0(2,R6),0(R6)                                                    
         BZ    DR420                                                            
         CLI   2(R4),C'R'          DISPLAY AS A RATING                          
         BE    DR415                                                            
         EDIT  (B2,0(R6)),(4,8(R2)),0,ALIGN=LEFT                                
         B     DR420                                                            
DR415    EDIT  (B2,0(R6)),(5,8(R2)),1,ALIGN=LEFT                                
         B     DR420                                                            
DR420    OI    4(R2),X'20'         SET PREVALID BIT                             
         FOUT  (R2)                                                             
         BAS   RE,NEXTUN           POINT R2 TO NEXT UNPROCTED FLD               
         LA    R4,3(R4)                                                         
         BCT   R5,DR400                                                         
         CLI   PGMSPECH+5,0        COULD GO THROUGH FMT LOGIC                   
*                                  FROM FMTPCT - PCT ADJUSTMENT                 
         BE    DR500                                                            
         XC    PGMACTV,PGMACTV                                                  
         FOUT  PGMACTVH,PGMSPEC,16                                              
         FOUT  PGMSPECH,SPFIL20,16                                              
         MVI   PGMSPECH+5,0                                                     
         XC    PGMUVPH,PGMUVPH                                                  
         FOUT  PGMUVPHH                                                         
         MVI   SVFMTSW,0           SET TO FORMAT MODE                           
         OI    PGMDAYH+1,X'01'     SET TO MODIFIED FOR INPUT                    
         B     SPECPCT                                                          
*                                                                               
DR500    FOUT  PGMSPECH,SPFIL20,7                                               
         XC    PGMACTV,PGMACTV                                                  
*                                                                               
DR600    MVC   PGMACTV(13),=C'LAST ACTIVITY'                                    
         GOTO1 DATCON,DMCB,(3,NPGACTD),(5,PGMACTV+14)                           
         MVC   PGMACTV+24(3),=C'ADD'                                            
         CLI   NPGACT,C'A'                                                      
         BE    *+10                                                             
         MVC   PGMACTV+24(6),=C'CHANGE'                                         
*                                                                               
DR600X   FOUT  PGMACTVH                                                         
*                                                                               
DR800    DS    0H                                                               
         XC    PGMUVPH,PGMUVPH                                                  
         LA    R3,24(R7)                                                        
         USING UDEVPD,R3                                                        
         MVI   ELCODE,X'C3'                                                     
         LA    R5,PGMUVPH                                                       
         LA    R1,L'PGMUVPH(R5)                                                 
         CLI   0(R3),X'C3'         USER DEMO VPH ELEM                           
         BE    DR860                                                            
DR820    BAS   RE,NXTEL                                                         
         BNE   DR900                                                            
*                                                                               
DR860    BAS   RE,FMTDEMO                                                       
         ZIC   RE,WORK             WORK HAS LENGHT                              
         LR    R0,R5               SEE IF IT WILL FIT ON THIS LINE              
         AR    R0,RE                                                            
         AH    R0,=H'4'            ADJUST FOR VALUE                             
         CR    R0,R1                                                            
         BH    EXIT                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK+1      WORK+1 HAS DESC                              
         AR    R5,RE                                                            
         MVI   1(R5),C'='                                                       
         LA    R5,2(R5)                                                         
         BAS   RE,EDITB4                                                        
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         B     DR820               NEXT DEMO                                    
*                                                                               
DR900    BCTR  R5,0                                                             
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '          BLANK OUT LAST COMMA                         
         FOUT  PGMUVPHH                                                         
         B     EXIT                                                             
         SPACE 4                                                                
*--POINT R2 TO NEXT POSITION IN THE "OTHER FIELD"                               
SETR2    LA    R2,PGMOTH                                                        
         LA    RF,48                                                            
*                                                                               
SETR2A   CLI   0(R2),X'40'                                                      
         BNH   SETR2EX                                                          
         LA    R2,1(R2)                                                         
         BCT   RF,SETR2A                                                        
SETR2EX  BR    RE                                                               
         EJECT                                                                  
FMTDEMO  NTR1                                                                   
         MVC   WORK(10),SPFIL20                                                 
         MVI   WORK,0                                                           
         MVC   WORK+1(7),UDEVPNAM                                               
         LA    R1,7                                                             
         LA    R4,WORK+7                                                        
FD100    CLI   0(R4),C' '          SCAN BACKWARD FOR NON-SPACE                  
         BNE   FD200                                                            
         BCTR  R4,0                                                             
         BCT   R1,FD100                                                         
FD200    STC   R1,WORK                                                          
         B     EXIT                                                             
         SPACE 2                                                                
EDITB4   DS    0H                                                               
         MVC   DUB(2),UDEVPH                                                    
         CLI   UDEVPFMT,C'T'                                                    
         BE    EDTB4A                                                           
         LH    RF,DUB                                                           
         MH    RF,=H'10'                                                        
         STH   RF,DUB                                                           
EDTB4A   EDIT  (B2,DUB),(4,WORK2),0,ALIGN=LEFT                                  
         LR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,MOVEIT                                                        
         AR    R5,R0                                                            
         CR    R5,R1               ????????????????????????                     
         BNH   0(RE)               NORMAL RETURN                                
         DC    H'0'                CAN'T FIT ON LINE                            
*                                                                               
MOVEIT   MVC   0(0,R5),WORK2                                                    
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
FMTPCT   DS    0H                  PERCENTAGE FORMAT                            
         LA    R2,PGMSPECH         RESET R2  FOR CURSOR                         
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'93'                                                     
         BAS   RE,NXTEL                                                         
         BE    FM200                                                            
*                                                                               
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NXTEL                                                         
         BE    FM100                                                            
         DC    H'0'                MUST FIND 92 ELEM                            
*- CONVER 92 ELEMENT INTO 93 ELEMENT                                            
FM100    XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'9392'                                                 
         LA    R4,ELEM+5                                                        
         LA    R3,32(R3)                                                        
         LA    R5,33                                                            
FM120    ZIC   RF,0(R3)                                                         
         LA    R0,10                                                            
         MR    RE,R0                                                            
         STH   RF,0(R4)                                                         
         MVI   0(R3),0                                                          
         LA    R3,1(R3)                                                         
         LA    R4,2(R4)                                                         
         BCT   R5,FM120                                                         
         BAS   RE,PUTEL                                                         
         B     FMTPCT                                                           
*                                                                               
         USING NPG2ELEM,R3                                                      
FM200    LA    R6,NPG2VPHS                                                      
         LA    R5,MAX93VPH         FOR BCT                                      
*                                  BUILD AND EXPRESSION FOR CASHVAL             
*                                  TO EDIT                                      
         XC    WORK(20),WORK                                                    
         ZIC   R4,5(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),8(R2)        MOVE ADJUSTMENT                           
         LA    R3,WORK+8                                                        
         AR    R3,R4               POINT TO LAST CHAR                           
         CLI   0(R3),C'%'                                                       
         BE    FM300                                                            
         MVI   1(R3),C'%'          % NOT INPUT - ADD IT                         
         LA    R4,1(R4)                                                         
*                                                                               
FM300    DS    0H                                                               
         AH    R4,=H'9'            SET TOTAL LENGHT OF EXPRESSION               
FM320    SR    R0,R0                                                            
         LH    R0,0(R6)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+1(7),DUB                                                    
         MVC   WORK(2),WORK+2                                                   
         MVI   WORK+2,C'.'         ALTER TO N.NN FOR CASH VAL                   
         GOTO1 CASHVAL,DMCB,(5,WORK),(R4)                                       
         CLI   DMCB,0                                                           
         BE    FM340                                                            
FMERR    MVI   ERROR,INVALID                                                    
         B     INVERR                                                           
*                                                                               
FM340    L     R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BL    FMERR               CAN'T GO NEGATIVE                            
         STH   R0,0(R6)                                                         
         LA    R6,2(R6)                                                         
         BCT   R5,FM320                                                         
*                                                                               
FMEXT    DS    0H                                                               
         B     DR190                                                            
         DROP  R3                                                               
         EJECT                                                                  
SPECNTI  DS    0H                                                               
         LA    R2,PGMDAYH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(21),=C'* ENTER RECORD DATA *'                            
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
SPECPCT  DS    0H                                                               
         LA    R2,PGMDAYH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(23),=C'* ENTER AMMENDED DATA *'                          
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
DELMSG   DS    0H                                                               
         LA    R2,CONACTH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(43),=C'LAST SELECTION - HIT ENTER TO CONTINUE LIX        
               ST'                                                              
         FOUT  CONHEADH                                                         
         MVC   CONACT(8),=C'SELECT  '                                           
         FOUT  CONACTH                                                          
         GOTO1 ERREX2                                                           
         EJECT                                                                  
VR       NTR1                                                                   
*              FIRST CHK FOR VPH DISPLAY FROM NTI POCKET PIECE                  
         CLC   PGMSPEC(4),=C'NTI,'                                              
         BE    FMTNTI                                                           
         CLI   PGMSPECH+5,0        CHK FOR OTHER SPECIAL INPUT                  
         BE    VR100               NO                                           
         LA    R2,PGMSPECH                                                      
         CLI   ACTNUM,ACTADD                                                    
         BE    INVERR                                                           
         B     FMTPCT                                                           
*                                                                               
FMTNTI   DS    0H                                                               
         L     R5,AIO3                                                          
         XC    0(250,R5),0(R5)                                                  
         USING DBLOCKD,R5                                                       
         MVC   DBSELAGY,AGENCY                                                  
         MVC   DBFILE(3),=C'NTI'                                                
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBAUTH,TWAAUTH                                                   
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELSTA(4),PGMNET  GET CALL LETTERS FROM KEY                    
         CLI   QNTISTA,X'40'                                                    
         BNH   *+10                                                             
         MVC   DBSELSTA,QNTISTA                                                 
         CLI   DBSELSTA+3,X'00'                                                 
         BNE   *+8                                                              
         MVI   DBSELSTA+3,C' '                                                  
         MVI   DBSELSTA+4,C'T'                                                  
         CLI   QPTYPE,C'H'                                                      
         BNE   FN020                                                            
         MVI   DBSELSTA+4,C'H'     SET FOR HISPANIC                             
         MVC   DBFILE,=C'NAD'                                                   
FN020    L     R3,AIO2                                                          
         ST    R3,DBAREC                                                        
         MVI   DBFUNCT,DBGETNTI                                                 
         LA    R2,PGMSPECH                                                      
         GOTO1 SCANNER,DMCB,(R2),(4,0(R3)),C',=,/'                              
         CLI   DMCB+4,3                                                         
         BNE   INVERR                                                           
         USING SCAND,R3                                                         
         LA    R3,32(R3)           POINT R3 TO NTI NUMBER                       
         TM    FLD1VAL,X'80'       MUST BE NUMERIC                              
         BZ    INVERR                                                           
         CLI   FLD2LEN,0           NO SECOND HALF ALLOWED                       
         BNE   INVERR                                                           
         CLI   FLD1LEN,5                                                        
         BH    INVERR                                                           
         MVC   DBSELPRG,FLD1B+2    BINARY PROGRAM                               
*                                  EDIT BOOK WK/YY                              
         LA    R3,32(R3)                                                        
         TM    FLD1VAL,X'80'       MUST BE NUMERIC                              
         BZ    INVERR                                                           
         TM    FLD2VAL,X'80'       MUST BE NUMERIC                              
         BZ    INVERR                                                           
         CLI   FLD1LEN,2           WEEK/YEAR                                    
         BH    INVERR                                                           
         CLI   FLD2LEN,2           SECOND HALF                                  
         BNE   INVERR              YEAR REQUIRED                                
         CLI   QPTYPE,C'H'         CHECK FOR HISPANIC                           
         BNE   *+18                                                             
         CLC   FLD1(2),=C'12'                                                   
         BH    INVERR              MONTH CAN'T EXCEED 12                        
         B     *+14                                                             
         CLC   FLD1(2),=C'52'                                                   
         BH    INVERR              WEEK CAN'T EXCEED 52                         
         MVC   DBSELBK,FLD2B+3     YEAR                                         
         MVC   DBSELBK+1,FLD1B+3   WEEK OR MONTH                                
         MVC   DMCB+4(4),=X'D9000ADD'      DEMAND                               
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         L     R4,AIO3                                                          
         GOTO1 (RF),(R1),0(R4),0                                                
         OC    DBFACTOR,DBFACTOR                                                
         BNZ   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     INVERR                                                           
*                                                                               
         MVC   DMCB+4(4),=X'D9000ADF'       DEMOUT                              
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         L     R4,AIO3                                                          
         LA    R3,500(R4)                                                       
         GOTO1 (RF),(R1),(C'L',DEMTAB1),0(R4),0(R3)                             
         LA    R2,PGMW1H           FIRST VPH FIELD                              
         LA    R5,MAXVPHF          FOR BCT                                      
*                                                                               
FN100    MVC   8(4,R2),=C'0   '                                                 
         L     R0,0(R3)                                                         
         LTR   R0,R0                                                            
         BZ    FN200                                                            
         CVD   R0,DUB                                                           
         EDIT  (B4,0(R3)),(4,8(R2)),0,ALIGN=LEFT                                
FN200    FOUT  (R2)                                                             
         BAS   RE,NEXTUN           POINT R2 TO NEXT UNPROCTED FLD               
         LA    R3,4(R3)                                                         
         BCT   R5,FN100                                                         
*                                                                               
         XC    PGMACTV,PGMACTV                                                  
         FOUT  PGMACTVH,PGMSPEC,16                                              
         FOUT  PGMSPECH,SPFIL20,16                                              
         XC    PGMUVPH,PGMUVPH                                                  
         FOUT  PGMUVPHH                                                         
         MVI   SVFMTSW,0           SET TO FORMAT MODE                           
         OI    PGMDAYH+1,X'01'     SET TO MODIFIED FOR INPUT                    
         B     SPECNTI                                                          
         EJECT                                                                  
VR100    BAS   RE,ACTIVITY         ADD OR UPDATE ACTIVITY ELEM                  
         BAS   RE,OTHEREL          ADD OR UPDATE EXTRA SPACE ELEMENT            
         CLI   ACTNUM,ACTSEL                                                    
         BNE   *+10                                                             
         MVC   SVKEY(13),0(R7)     LOAD SELECT KEY                              
         XC    ELEM(100),ELEM                                                   
         MVC   ELEM(2),=X'9250'       SET CODE AND LENGHT                       
         XC    ODAYTIM,ODAYTIM     USED TO SAVE OLD DAY AND TIME                
         XC    NDAYTIM,NDAYTIM                                                  
         XC    OLDVPHS,OLDVPHS     USED TO SAVE OLD VPHS                        
         CLI   ACTNUM,ACTADD       NO DELETE FOR ADD                            
         BE    VR160                                                            
*                                                                               
*                                  SEE IF PASSIVE POINTER EXISTS FOR            
*                                  DAY AND TIME / END DATE                      
*                                                                               
         LA    R3,24(R7)                                                        
         USING NPGELEM,R3                                                       
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NXTEL                                                         
         BE    *+6                                                              
         DC    H'0'                BAD RECORD                                   
         MVC   OLDVPHS,NPGVPHS     SAVE OLD VPHS                                
         MVC   SVCUPLD,NPGUPLD     SAVE CABLE UPLOAD SETTING                    
*                                                                               
         CLI   NPGUPLD,C'Y'        CABLE UPLOAD PROGRAM RECORD                  
         BE    VR110               NO SECONDARY KEY                             
*                                                                               
         MVC   SVKEY(13),NPGKEY                                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DA0'                                                  
*        MVC   KEY+2(3),SVKEY+2       AGY/MED NTWK MKT NUMBER                   
         MVC   KEY+2(1),NPGKAM        AGY/MED                                   
         MVC   KEY+3(2),NPGKNET       NTWK MKT NUMBER                           
         MVC   KEY+5(1),NPGRDAY                                                 
         MVC   KEY+6(4),NPGTIME                                                 
         MVC   KEY+10(1),NPGUNIQ                                                
*        MVC   KEY+11(2),SVKEY+11  END DATE                                     
         MVC   KEY+11(2),NPGKEND   END DATE                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST FIND PASSIVE POINTER                    
VR110    CLC   PGMDAY(3),=C'DEL'      DELETE CODE                               
         BNE   VR140                                                            
         CLI   T31CFFD+1,C'*'      DDS ONLY CAN DELETE                          
         BE    VR120                                                            
         LA    R2,PGMDAYH          CURSOR TO DAY                                
         B     INVERR                                                           
*                                                                               
VR120    CLI   NPGUPLD,C'Y'        CABLE UPLOAD PROGRAM RECORD                  
         BE    VR130               NO SECONDARY KEY                             
         MVI   KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
VR130    MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         MVI   15(R7),X'80'                                                     
         GOTO1 PUTREC                                                           
         MVI   GENSTAT2,USMYOK                                                  
         MVC   CONHEAD(21),=C'** PROGRAM DELETED **'                            
         MVC   NDAYTIM,=C'DEL'                                                  
         MVI   IOOPT,C'Y'                                                       
*                                                                               
         MVC   ODAYTIM(1),NPGRDAY  SAVE FOR PASSIVE DELETE                      
         MVC   ODAYTIM+1(4),NPGTIME  SAVE FOR PASSIVE DELETE                    
         MVC   ODAYTIM+5(1),NPGUNIQ  SAVE FOR PASSIVE DELETE                    
*                                                                               
         B     VKEXT                                                            
*                                                                               
VR140    MVC   ODAYTIM,KEY+5       SAVE OLD DAY/TIME/UNIQ                       
*                                                                               
VR160    EQU   *                                                                
         LA    R3,ELEM                                                          
         USING NPGELEM,R3                                                       
         MVC   NPGUPLD,SVCUPLD     RE-STORE CABLE UPLOAD SETTING                
         LA    R2,PGMDAYH                                                       
         GOTO1 ANY                 REQUIRED                                     
         ZIC   R5,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R5),8(R2)),NPGDAY,NPGDAYNO                         
         CLI   NPGDAY,0                                                         
         BE    INVERR                                                           
         MVI   NPGRDAY,X'08'       REP DAY M-S                                  
         CLI   NPGDAY,X'7F'        M-SU                                         
         BE    VR240                                                            
         MVI   NPGRDAY,X'00'                                                    
         CLI   NPGDAY,X'7C'        M-F                                          
         BE    VR240                                                            
*                                                                               
         MVI   WORK,X'40'          ELSE CAN ONLY BE ONE DAY                     
         MVI   WORK+1,0                                                         
         MVI   WORK+2,X'01'        MON - REP CODE                               
         LA    R6,7                FOR BCT - CHECK EACH DAY M-SUN               
*                                                                               
VR200    MVC   ALMSK+1(1),WORK     ALTER MASK                                   
ALMSK    TM    NPGDAY,X'00'                                                     
         BNO   VR220                                                            
         CLI   WORK+1,0            SEE IF I ALREADY HAVE A DAY                  
         BNE   INVERR              YES                                          
         MVI   WORK+1,1                                                         
         MVC   NPGRDAY,WORK+2      REP DAY                                      
*                                                                               
VR220    ZIC   R0,WORK                                                          
         SRL   R0,1                FOR NEXT MASK                                
         STC   R0,WORK                                                          
         ZIC   R1,WORK+2                                                        
         LA    R1,1(R1)                                                         
         STC   R1,WORK+2           NEXT REP DAY                                 
         BCT   R6,VR200                                                         
*                                                                               
VR240    LA    R2,PGMTIMEH                                                      
         GOTO1 ANY                 REQUIRED                                     
         ZIC   R5,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R5),8(R2)),NPGTIME                                 
         CLI   DMCB,X'FF'          INVALID TIME                                 
         BE    INVERR                                                           
         CLC   NPGTIME,=C'NONE'                                                 
         BE    INVERR                                                           
         CLC   NPGTIME,=C'VARY'                                                 
         BE    INVERR                                                           
         CLC   NPGTIME+2(2),=2X'00'                                             
         BE    INVERR              END TIME REQUIRED                            
         CLC   NPGTIME+2(2),=C'CC'                                              
         BE    INVERR                                                           
*                                                                               
*                                  SEE IF PASSIVE POINTER EXISTS FOR            
*                                  DAY AND TIME / END DATE                      
*                                  ON DAY TIME CHANGES                          
         MVC   NDAYTIM(1),NPGRDAY                                               
         MVC   NDAYTIM+1(4),NPGTIME                                             
         CLI   NPGUPLD,C'Y'        CABLE UPLOAD PROGRAM RECORD                  
         BE    VR400               NO SECONDARY KEY                             
*                                                                               
         CLC   ODAYTIM(5),NDAYTIM                                               
         BNE   *+14                                                             
         MVC   NPGUNIQ,ODAYTIM+5       PRESERVE OLD UNIQ                        
         B     VR400                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DA0'                                                  
         MVC   KEY+2(3),SVKEY+2       AGY/MED NTWK MKT NUMBER                   
         MVC   KEY+5(1),NPGRDAY                                                 
         MVC   KEY+6(4),NPGTIME                                                 
         MVC   KEY+11(2),SVKEY+11  END DATE                                     
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
VR340    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VR360                                                            
         ZIC   R2,KEYSAVE+10                                                    
         LA    R2,1(R2)                                                         
         MVC   KEY(13),KEYSAVE                                                  
         STC   R2,KEY+10           TRY FOR NEXT UNIQ                            
         B     VR340                                                            
*                                                                               
VR360    MVC   NPGUNIQ,KEYSAVE+10     SAVE NEXT AVAIL UNIQ                      
         NI    DMINBTS,X'F7'                                                    
*                                                                               
VR400    LA    R2,PGMNAMEH                                                      
         GOTO1 ANY                 REQUIRED                                     
         MVC   NPGNAME,8(R2)                                                    
         CLI   NPGNAME,C'='        CAN'T START WITH =                           
         BE    INVERR                                                           
*                                                                               
         LA    R2,PGMROTH          * ROTATION                                   
         MVC   NPGROT,NPGDAY                                                    
         MVC   NPGROTNO,NPGDAYNO                                                
         CLI   5(R2),0                                                          
         BE    VR410                                                            
         ZIC   R0,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R0),8(R2)),NPGROT,NPGROTNO                         
         OC    NPGROT,NPGROT       ZERO RETURN ERROR                            
         BZ    INVERR                                                           
         MVC   BYTE,NPGDAY                                                      
         OC    BYTE,NPGROT         CHECK SEE IF ROTATION WITHIN DAY             
         CLC   BYTE,NPGDAY                                                      
         BNE   INVERR              ROTATION OUT OF DAY RANGE                    
*                                                                               
VR410    LA    R2,PGMSHRH                                                       
         GOTO1 ANY                                                              
         ZIC   R5,5(R2)                                                         
         NI    NPGSTAT,X'7F'       SET OFF X'80'                                
         LA    R6,8(R2)                                                         
         CLI   0(R6),C'R'          RATING                                       
         BNE   VR420                                                            
         CLI   5(R2),2                                                          
         BL    INVERR              MUST BE AT LEAST 2                           
         OI    NPGSTAT,X'80'                                                    
         LA    R6,1(R6)                                                         
         BCTR  R5,0                                                             
*                                                                               
VR420    DS    0H                                                               
         GOTO1 CASHVAL,DMCB,0(R6),(R5)                                          
         CLI   DMCB,0                                                           
         BNE   INVERR                                                           
         L     R5,DMCB+4                                                        
         C     R5,=F'10000'        MAX IS 100.00                                
         BH    INVERR                                                           
         LTR   R5,R5                                                            
         BL    INVERR                                                           
         CVD   R5,DUB                                                           
*-IF CABLE OR OTHER SET RATING AT 2 DECIMAL PLACES                              
*-TAKE *'S OUT WHEN 2 DECIMAL RATING READY TO GO LIVE                           
*        CLI   QPTYPE,C'N'         CHECK NETWORK                                
*        BE    VR430                                                            
*        CLI   QPTYPE,C'S'         CHECK SYNDICATION                            
*        BE    VR430                                                            
*        CLI   8(R2),C'R'          CHECK RATING                                 
*        BE    VR440                                                            
*                                                                               
VR430    DP    DUB,=P'10'                                                       
         CP    DUB+6(2),=P'0'      MUST GET ZERO REMAINDER                      
         BNE   INVERR                                                           
         MVC   WORK(6),DUB                                                      
         ZAP   DUB,WORK(6)                                                      
VR440    CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   NPGSHARE,HALF                                                    
*                                                                               
         LA    R2,PGMFLTRH                                                      
         CLI   5(R2),0                                                          
         BE    VR460                                                            
         CLI   5(R2),3                                                          
         BH    INVERR                                                           
         MVC   NPGFILT,8(R2)                                                    
VR460    LA    R2,PGMNTIH                                                       
         XC    NPGPPNO,NPGPPNO                                                  
         CLI   5(R2),0                                                          
         BE    VR560                                                            
         GOTO1 ANY                                                              
         BAS   RE,PACKSI                                                        
         STH   R0,HALF                                                          
         CH    R0,=H'0'                                                         
         BNH   INVERR                                                           
         MVC   NPGPPNO,HALF                                                     
*                                                                               
VR560    XC    NPGVPHS(34),NPGVPHS   ZERO VPHS IN 92 ELEMENT                    
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'92'                                                     
         BAS   RE,DELEL                                                         
*                                                                               
         BAS   RE,PUTEL                                                         
         DROP  R3                                                               
*                                                                               
VR570    LA    R3,ELEM                                                          
         USING NPG2ELEM,R3                                                      
         XC    ELEM,ELEM                                                        
         MVC   NPG2ELEM(2),=X'9392'                                             
*                                                                               
         LA    R2,PGMSDATH         * END DATE                                   
         XC    NPG2STD,NPG2STD                                                  
         CLI   5(R2),0                                                          
         BE    VR580                                                            
         GOTO1 VALIFLD                                                          
         BZ    VR580                                                            
         GOTO1 VALIDAT                                                          
         GOTO1 DATCON,DMCB,(0,QDATE),(2,NPG2STD)                                
         CLC   NPG2STD,NPGKEND     CHECK START > END                            
         BH    INVERR                                                           
*                                                                               
VR580    LA    R2,PGMDYPTH         * DAYPART                                    
         XC    NPG2DYP,NPG2DYP                                                  
         CLI   5(R2),0                                                          
         BNE   VR585                                                            
*        CLC   AGENCY,=CL2'JW'                                                  
*        BNE   VR590                                                            
         MVI   ERROR,MISSING                                                    
         B     TRAPERR              FOR AGENCY JW DAYPART REQUIRED              
VR585    GOTO1 VALIFLD                                                          
         BZ    VR590                                                            
         GOTO1 VALIDPT                                                          
         MVC   NPG2DYP,QDPT                                                     
*                                                                               
VR590    L     R6,AIO                                                           
         MVI   ELCODE,X'03'        SECONDARY DATA ELEMENT                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    8(12,R6),8(R6)      CLEAR OTHER DATA FIELDS                      
         MVI   21(R6),0            CLEAR OTHER DATA FIELDS                      
         LA    R2,PGMOTHH                                                       
         CLI   5(R2),0                                                          
         BE    VR610                                                            
*                                                                               
         L     R4,AIO2                                                          
         GOTO1 SCANNER,DMCB,(20,(R2)),(4,0(R4))                                 
         CLI   DMCB+4,0                                                         
         BE    INVERR                                                           
         ZIC   R5,DMCB+4                                                        
*                                                                               
VR595    CLI   12(R4),C'F'                                                      
         BNE   VR595A                                                           
         MVC   8(12,R6),22(R4)                                                  
         B     VR600                                                            
*                                                                               
VR595A   CLC   12(6,R4),=CL6'WINDOW'                                            
         BNE   VR595E                                                           
         OI    20(R6),X'80'        SET WINDOW BYTE                              
         B     VR600                                                            
*                                                                               
VR595E   CLC   12(2,R4),=C'SD'     SUB-DAYPART                                  
         BNE   VR595G                                                           
         USING NPGEL03,R6                                                       
         MVC   NPGSDPT,22(R4)                                                   
         B     VR600                                                            
*                                                                               
VR595G   CLC   12(4,R4),=C'TCAR'                                                
         BNE   VR595J                                                           
         LA    RE,TCARTAB                                                       
VR595H   CLI   0(RE),X'FF'                                                      
         BE    INVERR                                                           
         MVC   NPGTCAR,22(R4)                                                   
         CLC   0(1,RE),22(R4)                                                   
         BE    VR600                                                            
         LA    RE,1(RE)                                                         
         B     VR595H                                                           
*                                                                               
VR595J   CLC   12(2,R4),=C'MR'     MULTI-RUN                                    
         BNE   VR595M                                                           
VR595K   TM    3(R4),X'80'         TEST NUMERIC                                 
         BZ    INVERR                                                           
         CLI   11(R4),2                                                         
         BL    INVERR                                                           
         CLI   11(R4),15                                                        
         BH    INVERR                                                           
         OC    NPGMRUN,NPGMRUN                                                  
         BZ    *+14                                                             
         CLC   NPGMRUN,11(R4)                                                   
         BNE   INVERR                                                           
         MVC   NPGMRUN,11(R4)                                                   
         B     VR600                                                            
         DROP  R6                                                               
*                                                                               
VR595M   CLI   12(R4),C'M'                                                      
         BNE   INVERR                                                           
         LA    RE,MIRTAB                                                        
VR595N   CLI   0(RE),X'FF'                                                      
         BE    INVERR                                                           
         MVC   21(1,R6),22(R4)                                                  
         CLC   0(1,RE),22(R4)                                                   
         BE    VR600                                                            
         LA    RE,1(RE)                                                         
         B     VR595N                                                           
*                                                                               
MIRTAB   DC    CL1'A'              +4 HOURS                                     
         DC    CL1'B'              -5 HOURS                                     
         DC    CL1'C'              -11 HOURS                                    
         DC    CL1'D'              -6 HOURS                                     
         DC    CL1'E'              -9 HOURS                                     
         DC    CL1'F'              +3 HOURS                                     
         DC    XL1'FF'                                                          
*                                                                               
TCARTAB  DC    CL1'1'              LEVEL 1                                      
         DC    CL1'2'              LEVEL 2                                      
         DC    CL1'3'              LEVEL 3                                      
         DC    CL1'4'              LEVEL 4                                      
         DC    CL1'5'              LEVEL 5                                      
         DC    XL1'FF'                                                          
*                                                                               
VR600    LA    R4,42(R4)                                                        
         BCT   R5,VR595                                                         
************                                                                    
VR610    LA    R2,PGMW1H           FIRST VPH FIELD                              
         LA    R5,MAXVPHF          MAX VPH FIELDS                               
*        LA    R4,DISPTAB                                                       
         L     R4,=A(DISPTAB)                                                   
         A     R4,RELO                                                          
*                                                                               
VR620    SR    R0,R0                                                            
         CLI   5(R2),0                                                          
         BNE   VR660                                                            
         B     VR700               INPUT NOT REQ NOW                            
*                                                                               
VR660    ZIC   R6,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R6)                                          
         CLI   DMCB,0                                                           
         BNE   INVERR                                                           
         L     R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BL    INVERR              CAN'T BE NEGATIVE                            
*                                                                               
         CLI   2(R4),C'R'          VALIDATE RATING INPUT                        
         BNE   VR680                                                            
         C     R0,=F'10000'                                                     
         BH    INVERR                                                           
         CVD   R0,DUB                                                           
         DP    DUB,=P'10'                                                       
         CP    DUB+6(2),=P'0'      MUST GET ZERO REMAINDER                      
         BNE   INVERR                                                           
         B     VR690                                                            
*                                                                               
VR680    CVD   R0,DUB              VALIDATE VPH INPUT                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET ZERO REMAINDER                      
         BNE   INVERR              NO DECIMAL                                   
*                                                                               
VR690    MVC   WORK(6),DUB                                                      
         ZAP   DUB,WORK(6)                                                      
         CVB   R0,DUB                                                           
VR700    LA    R6,NPG2VPHS                                                      
         ZIC   R1,0(R4)            GET DISP                                     
         SLL   R1,1                MULTIPLY R0 BY 2                             
         AR    R6,R1                                                            
         TM    1(R2),X'02'         TEST IF ADULT FIELD                          
         BZ    VR710               IF 0 IT IS NOT                               
         TM    4(R2),X'20'         WAS FIELD PREVIOSLY VALIDATED                
         BZ    VR710               NO                                           
         OC    0(2,R6),0(R6)       SHOULD FIELD BE RECALCULATED                 
         BZ    VR710               NO                                           
         SR    R0,R0               ZERO OUT ADULT FIELD                         
VR710    STH   R0,0(R6)                                                         
*                                                                               
*--SET FLAG IN ADULT FIELD IF IT MUST BE RECALCULATED                           
         TM    4(R2),X'20'         WAS FIELD PREVIOSLY VALIDATED                
         BNZ   VR720               YES                                          
         CLI   1(R4),X'99'                                                      
         BE    VR720                                                            
*                                                                               
         LA    R6,NPG2VPHS                                                      
         ZIC   R1,1(R4)            GET DISP TO THE ADULT FIELD                  
         SLL   R1,1                MULTIPLY R0 BY 2                             
         AR    R6,R1                                                            
         MVC   0(2,R6),=XL2'FFFF'                                               
*                                                                               
VR720    LA    R4,3(R4)            NEXT VPH DISP                                
         BAS   RE,NEXTUN           SET R2 TO NEXT UNPROTECTED FLD               
         BCT   R5,VR620                                                         
*                                                                               
*        DS    0H                                                               
*        CLI   ACTNUM,ACTADD       SEE IF ADD                                   
*        BE    *+16                                                             
*              PRESERVE OLD VPHS - BUT                                          
*              THEY WILL BE ADJUSTED BY PERCENTAGE                              
*              IN SPECIAL FIELD                                                 
*                                                                               
*              MEN TEENS,CH2-5 WILL NOT BE PERSERVED                            
*                                                                               
*        MVC   NPG2VPHS+9(3),OLDVPHS+9    WM,MN,AD 25-49                        
*        MVC   NPG2VPHS+15(3),OLDVPHS+15   WM,MN,AD 55-64                       
*                                                                               
*                                  CHECK FIRST 7 SETS OF 3 VPHS                 
*                                  IF THIRD IS MISSING - CALCULATE IT           
         LA    R6,NPG2VPHS                                                      
         LA    R5,8                FOR BCT                                      
         LA    R4,2                                                             
VR730    OC    4(2,R6),4(R6)                                                    
         BNZ   VR740               3 WAS INPUT NO  CALC                         
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         LH    R2,0(R6)            3 = 1+2                                      
         LH    R1,2(R6)                                                         
         CLI   N0PROF+6,C'N'       CHECK IF TOTAL REQUIRED                      
         BNE   VR735                                                            
         LTR   R2,R2               IS FIRST CELL ZERO                           
         BZ    VR740               YES DON'T TOTAL                              
         LTR   R1,R1               IS SECOND CELL ZERO                          
         BZ    VR740               YES DONT TOTAL                               
VR735    AR    R2,R1                                                            
         STH   R2,4(R6)                                                         
*                                                                               
VR740    LA    R6,6(R6)            NEXT SET                                     
         BCT   R5,VR730                                                         
*                                                                               
         LA    R6,NPG2VPHS+86                                                   
         LA    R5,3                FOR BCT                                      
         BCT   R4,VR730                                                         
*                                                                               
VR760    LA    R6,NPG2VPHS+60      DO 35-64                                     
         OC    4(2,R6),4(R6)                                                    
         BNZ   VR770               3 WAS INPUT NO  CALC                         
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         LH    R2,0(R6)            3 = 1+2                                      
         LH    R1,2(R6)                                                         
         CLI   N0PROF+6,C'N'       CHECK IF TOTAL REQUIRED                      
         BNE   VR765                                                            
         LTR   R2,R2               IS FIRST CELL ZERO                           
         BZ    VR770               YES DON'T TOTAL                              
         LTR   R1,R1               IS SECOND CELL ZERO                          
         BZ    VR770               YES DONT TOTAL                               
VR765    AR    R2,R1                                                            
         STH   R2,4(R6)                                                         
*                                                                               
VR770    LA    R6,NPG2VPHS+66      DO 2-11                                      
         CLC   NPG2VPHS+52(2),=XL2'0000'                                        
         BNE   VR780               3 WAS INPUT NO  CALC                         
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         LH    R2,0(R6)            3 = 1+2                                      
         LH    R1,2(R6)                                                         
         CLI   N0PROF+6,C'N'       CHECK IF TOTAL REQUIRED                      
         BNE   VR775                                                            
         LTR   R2,R2               IS FIRST CELL ZERO                           
         BZ    VR780               YES DON'T TOTAL                              
         LTR   R1,R1               IS SECOND CELL ZERO                          
         BZ    VR780               YES DONT TOTAL                               
VR775    AR    R2,R1                                                            
         LA    R6,NPG2VPHS+52                                                   
         STH   R2,0(R6)                                                         
*                                                                               
VR780    LA    R6,NPG2VPHS+70      DO 6-11                                      
         CLC   NPG2VPHS+50(2),=XL2'0000'                                        
         BNE   VR790               3 WAS INPUT NO  CALC                         
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         LH    R2,0(R6)            3 = 1+2                                      
         LH    R1,2(R6)                                                         
         CLI   N0PROF+6,C'N'       CHECK IF TOTAL REQUIRED                      
         BNE   VR785                                                            
         LTR   R2,R2               IS FIRST CELL ZERO                           
         BZ    VR790               YES DON'T TOTAL                              
         LTR   R1,R1               IS SECOND CELL ZERO                          
         BZ    VR790               YES DONT TOTAL                               
VR785    AR    R2,R1                                                            
         LA    R6,NPG2VPHS+50                                                   
         STH   R2,0(R6)                                                         
*                                                                               
VR790    LA    R6,NPG2VPHS+74      DO 15-24                                     
         CLC   NPG2VPHS+78(2),=XL2'0000'                                        
         BNE   VR800               3 WAS INPUT NO  CALC                         
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         LH    R2,0(R6)            3 = 1+2                                      
         LH    R1,2(R6)                                                         
         CLI   N0PROF+6,C'N'       CHECK IF TOTAL REQUIRED                      
         BNE   VR795                                                            
         LTR   R2,R2               IS FIRST CELL ZERO                           
         BZ    VR800               YES DON'T TOTAL                              
         LTR   R1,R1               IS SECOND CELL ZERO                          
         BZ    VR800               YES DONT TOTAL                               
VR795    AR    R2,R1                                                            
         LA    R6,NPG2VPHS+78                                                   
         STH   R2,0(R6)                                                         
*                                                                               
VR800    LA    R3,24(R7)                                                        
         MVI   ELCODE,X'93'                                                     
         BAS   RE,DELEL                                                         
*                                                                               
         BAS   RE,PUTEL                                                         
*                                                                               
* CHK FOR USER DEMO VPHS                                                        
VR840    LA    R3,24(R7)                                                        
         MVI   ELCODE,X'C3'                                                     
         BAS   RE,NXTEL                                                         
         BNE   VR900                                                            
*                                                                               
         BAS   R3,PUTEL                                                         
         B     VR840                                                            
*                                                                               
VR900    LA    R3,24(R7)                                                        
         MVI   ELCODE,X'C3'                                                     
         BAS   RE,DELEL                                                         
*                                                                               
         LA    R2,PGMUVPHH         CHK FOR USER DEMO UNIVS                      
         CLI   5(R2),0                                                          
         BE    VKEXT                                                            
         L     R3,AIO2                                                          
         LA    R4,8                                                             
VR920    XC    0(250,R3),0(R3)                                                  
         LA    R3,250(R3)                                                       
         BCT   R4,VR920                                                         
         L     R3,AIO2                                                          
         GOTO1 SCANNER,DMCB,(R2),(10,0(R3))                                     
         CLI   DMCB+4,0                                                         
         BE    INVERR                                                           
         L     R5,AIO2                                                          
         USING SCAND,R5                                                         
VR940    CLC   0(2,R5),=X'0000'    END OF TABLE                                 
         BE    VKEXT                                                            
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING UDEVPD,R6                                                        
         MVC   ELEM(2),=X'C30D'                                                 
         CLI   FLD1LEN,0                                                        
         BNH   INVERR                                                           
         CLI   FLD1LEN,7                                                        
         BH    INVERR                                                           
         MVC   UDEVPNAM,FLD1                                                    
         TM    FLD2VAL,X'80'       CHK NUMERIC                                  
         BZ    INVERR                                                           
         CLI   FLD2LEN,4           MAX IS 4 CHARS                               
         BH    INVERR                                                           
         L     R0,FLD2B                                                         
         LTR   R0,R0                                                            
         BNH   INVERR              CAN'T BE NEGATIVE                            
         STH   R0,DUB                                                           
         MVC   UDEVPH,DUB                                                       
         MVI   UDEVPFMT,C'T'                                                    
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'F9'        GET ME TO END OF REC                         
         BAS   RE,NXTEL                                                         
         BAS   RE,PUTEL                                                         
         LA    R5,32(R5)           NEXT SCANNER ENTRY                           
         B     VR940                                                            
*                                                                               
VKEXT    B     DR                                                               
         DROP  R3                                                               
         EJECT                                                                  
GOAK     DS    0H                                                               
         GOTO1 =A(OPPFRTN),DMCB,(4,DUB),(R9),(RA),(RC),RR=RELO AK               
         B     EXIT                                                             
         EJECT                                                                  
RK       DS    0H                                                               
         LA    R3,24(R7)                                                        
         USING NPGELEM,R3                                                       
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NXTEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NPGUPLD,C'Y'        CABLE UPLOAD PROGRAM RECORD                  
         BE    RKEXT               NO SECONDARY KEY                             
*                                                                               
         XC    KEY,KEY             ADD NEW POINTER                              
         MVC   KEY(2),=X'0DA0'                                                  
         MVC   KEY+2(3),SVKEY+2                                                 
         MVC   KEY+5(1),NPGRDAY                                                 
         MVC   KEY+6(4),NPGTIME                                                 
         MVC   KEY+10(1),NPGUNIQ                                                
         MVC   KEY+11(2),SVKEY+11                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE   *+6                                                               
         DC    H'0'                                                             
         NI    KEY+13,X'7F'        UNDELETE KEY                                 
         GOTO1 WRITE                                                            
*                                                                               
RKEXT    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
OTHEREL  NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'        SECONDARY DATA ELEMENT                       
         BAS   RE,GETEL                                                         
         BE    OTHEREX                                                          
         XC    ELEM(40),ELEM                                                    
         MVC   ELEM(2),=XL2'0328'                                               
         BAS   RE,PUTEL                                                         
OTHEREX  B     EXIT                                                             
*                                                                               
*--CHECK IF UNIT CABLE UPLOADED                                                 
CHKCABL  NTR1                                                                   
         CLC   0(2,R7),=XL2'0D20'  IS IT A PROGRAM RECORD                       
         BNE   CHKCABEX                                                         
         LA    R3,24(R7)                                                        
         USING NPGELEM,R3                                                       
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NXTEL                                                         
         BNE   CHKCABEX                                                         
         CLI   NPGUPLD,C'Y'                                                     
         BE    CHKCABEX                                                         
         TM    NPGKPROG+3,X'F0'                                                 
         BNO   CHKCABEX                                                         
         TM    NPGKPROG+4,X'F0'                                                 
         BNO   CHKCABEX                                                         
         TM    NPGKPROG+5,X'F0'                                                 
         BNO   CHKCABEX                                                         
         MVI   NPGUPLD,C'Y'                                                     
CHKCABEX B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
ACTIVITY NTR1                                                                   
         XC    ELEM(10),ELEM                                                    
         MVC   NPGMAINL(2),=X'0108'                                             
         GOTO1 DATCON,DMCB,(5,0),(3,NPGACTD)                                    
         MVC   NPGACT,CONACT       SAVE ACTION                                  
*                                                                               
ACT5     LA    R3,24(R7)                                                        
         MVI   ELCODE,X'5D'                                                     
         BAS   RE,DELEL                                                         
         XC    ELEM(8),ELEM                                                     
         MVC   ELEM(2),=X'5D07'                                                 
         MVC   ELEM+2(3),=C'EVN'                                                
         MVC   ELEM+5(2),=X'580E'      APR/1988                                 
         BAS   RE,PUTEL                                                         
*                                                                               
ACTX     XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
LR       DS    0H                                                               
         SPACE                                                                  
         GOTO1 =A(OPPFRTN),DMCB,(0,DUB),(R9),(RA),(RC),RR=RELO SETOPT           
         MVC   AIO,AIO1                                                         
         MVI   NFILE,C'S'          SPOT FILE                                    
         OC    KEY(17),KEY                                                      
         BNZ   LR100                                                            
         MVC   KEY,SVKEY                                                        
*                                                                               
LR100    GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         B     LR220                                                            
*                                                                               
LR200    GOTO1 VSETSPT                                                          
         GOTO1 SEQ                                                              
LR220    CLC   SVKEY(5),KEY                                                     
         BNE   LREXT                                                            
         MVC   SVKEY,KEY                                                        
         GOTO1 GETREC                                                           
*  CHECK DATE FILTER                                                            
         OC    OPTSPER,OPTSPER                                                  
         BZ    LR230                                                            
         CLC   OPTSPER,KEY+11                                                   
         BH    LR200                                                            
         CLC   OPTEPER,KEY+11                                                   
         BL    LR200                                                            
*  CHECK DAYPART FILTER                                                         
LR230    OC    OPTDAYP,OPTDAYP                                                  
         BZ    LR240                                                            
*                                                                               
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'93'                                                     
         BAS   RE,NXTEL                                                         
         BNE   LR200                                                            
*                                                                               
         USING NPGEL93,R3                                                       
*                                                                               
         CLC   OPTDAYP,NPG2DYP                                                  
         BNE   LR200                                                            
         DROP  R3                                                               
*                                                                               
LR240    CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
         LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
         GOTO1 DATCON,DMCB,(2,NPGKEND),(5,LRDAT)                                
         MVC   LRCODE,NPGKPROG                                                  
*                                                                               
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NXTEL                                                         
*                                                                               
         BNE   LR200               IF NOT THERE WHY NOT JUST SKIP IT??          
*                                                                               
*        BE    *+6                                                              
*        DC    H'0'                MUST FIND 92 ELEM                            
*                                                                               
         USING NPGELEM,R3                                                       
*  CHECK FILTERS FIELD FILTER                                                   
         OC    OPTFILT,OPTFILT                                                  
         BZ    LR244                                                            
         CLC   OPTFILT,NPGFILT                                                  
         BNE   LR200                                                            
*  CHECK TIME FILTER                                                            
LR244    OC    OPTTIME,OPTTIME                                                  
         BZ    LR248                                                            
         CLC   OPTTIME(2),NPGTIME+2     START>END                               
         BH    LR200                                                            
         CLC   OPTTIME+2(2),NPGTIME     END<START                               
         BL    LR200                                                            
*  CHECK DAY FILTER                                                             
LR248    OC    OPTDAY,OPTDAY                                                    
         BZ    LR250                                                            
         MVC   BYTE,OPTDAY                                                      
         NC    BYTE,NPGDAY                                                      
         BZ    LR200                                                            
*                                                                               
LR250    MVC   LRNAM,NPGNAME                                                    
         GOTO1 UNDAY,DMCB,NPGDAY,LRDAY                                          
         CLI   OPTSTER,C'Y'                                                     
         BE    LR280                                                            
         OC    NPGROT,NPGROT                                                    
         BZ    LR280                                                            
         XC    LRDAY,LRDAY                                                      
         GOTO1 UNDAY,DMCB,NPGROT,LRDAY                                          
LR280    GOTO1 UNTIME,DMCB,NPGTIME,LRTIME                                       
*                                                                               
         CLI   OPTSTER,C'Y'                                                     
         BNE   *+14                                                             
         MVC   LRSHR(3),NPGFILT                                                 
         B     LR320                                                            
*                                                                               
         OC    NPGSHARE,NPGSHARE                                                
         BZ    LR320                                                            
         LA    R2,LRSHR                                                         
         TM    NPGSTAT,X'80'                                                    
         BZ    LR300                                                            
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
LR300    DS    0H                                                               
         EDIT  (B2,NPGSHARE),(5,0(R2)),1,ALIGN=LEFT                             
*                                                                               
LR320    OC    NPGPPNO,NPGPPNO                                                  
         BZ    LR500                                                            
         SR    R0,R0                                                            
         ICM   R0,3,NPGPPNO                                                     
         EDIT  (R0),(5,LRNTI),ALIGN=LEFT,FILL=0                                 
*                                                                               
         SPACE                                                                  
LR500    GOTO1 LISTMON                                                          
         B     LR200               GOTO READ SEQ                                
*                                                                               
LREXT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* PRINTING THE LINE                                                             
PR       DS    0H                                                               
         L     R6,ASPOOLD                                                       
         USING SPOOLD,R6                                                        
         SPACE                                                                  
         MVI   NFILE,C'T'          STATION FILE                                 
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R4,P+10                                                          
         USING PLINED,R4                                                        
         MVC   PRNET,QNET          NETWORK NAME                                 
         MVC   PRCODE,NPGKPROG     PROGRAM CODE                                 
*                                                                               
         MVI   PRDAT+8,C'-'                                                     
         GOTO1 DATCON,DMCB,(2,NPGKEND),(5,PRDAT+9)                              
*                                                                               
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NXTEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND 92 ELEM                            
*                                                                               
         USING NPGELEM,R3                                                       
*  CHECK FILTERS FIELD FILTER                                                   
         OC    OPTFILT,OPTFILT                                                  
         BZ    PR250                                                            
         CLC   OPTFILT,NPGFILT                                                  
         BNE   LR200                                                            
*                                                                               
PR250    MVC   PRNAM,NPGNAME                                                    
         GOTO1 UNDAY,DMCB,NPGDAY,PRDAY                                          
         OC    NPGROT,NPGROT                                                    
         BZ    PR280                                                            
         XC    PRDAY,PRDAY                                                      
         GOTO1 UNDAY,DMCB,NPGROT,PRDAY                                          
PR280    GOTO1 UNTIME,DMCB,NPGTIME,PRTIME                                       
         OC    NPGSHARE,NPGSHARE                                                
         BZ    PR320                                                            
         LA    R2,PRSHR                                                         
         TM    NPGSTAT,X'80'                                                    
         BZ    PR300                                                            
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
PR300    DS    0H                                                               
         EDIT  (B2,NPGSHARE),(5,0(R2)),1,ALIGN=LEFT                             
*                                                                               
PR320    OC    NPGPPNO,NPGPPNO                                                  
         BZ    PR400                                                            
         SR    R0,R0                                                            
         ICM   R0,3,NPGPPNO                                                     
         EDIT  (R0),(5,PRNTI),ALIGN=LEFT,FILL=0                                 
*                                                                               
         DROP  R3                                                               
*                                                                               
PR400    LA    R3,24(R7)                                                        
         MVI   ELCODE,X'93'                                                     
         BAS   RE,NXTEL                                                         
         BNE   PR500                                                            
         USING NPG2ELEM,R3                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,NPG2STD),(5,PRDAT)                                
*                                                                               
PR500    GOTO1 SPOOL,DMCB,(R6)                                                  
         B     LR200                                                            
         SPACE                                                                  
PREXT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
NXTEL    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    NXTEL2                                                           
         CLC   ELCODE,0(R3)                                                     
         BER   RE                                                               
         B     NXTEL                                                            
*                                                                               
NXTEL2   LTR   R3,R3                                                            
         BR    RE                                                               
         SPACE 2                                                                
NEXTUN   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUN                                                           
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINES FOR ELEMENT MAINTENANCE                                          
*                                                                               
DELEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFILE'),(ELCODE,0(R7)),0                   
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
PUTEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFILE'),0(R7),ELEM                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* PACK A NUMBER FROM A  HEADER FIELD                                            
*   R0 = RESULT                                                                 
PACKSI   SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1               EXIT ON ZERO LENGTH                          
         BZ    PACKX                                                            
         TM    4(R2),X'08'         OR NON-NUMERIC                               
         BZ    PACKX                                                            
         BCTR  R1,0                                                             
         EX    R1,*+10                                                          
         CVB   R0,DUB                                                           
PACKX    BR    RE                                                               
         PACK  DUB,8(0,R2)   * EXECUTED *                                       
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,42,C'NETWORK PROGRAM RECORDS'                                 
         SSPEC H2,42,C'-----------------------'                                 
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         L     R6,ASPOOLD                                                       
         USING SPOOLD,R6                                                        
*                                                                               
         LA    R2,H8+10                                                         
         USING PLINED,R2                                                        
         MVC   PRNET(3),=C'NET'                                                 
         MVC   PRNET+132(4),=4C'-'                                              
         MVC   PRCODE+1(4),=C'CODE'                                             
         MVC   PRCODE+132(6),=6C'-'                                             
         MVC   PRDAT+1(14),=C'START-END DATE'                                   
         MVC   PRDAT+132(17),=17C'-'                                            
         MVC   PRNAM+2(12),=C'PROGRAM NAME'                                     
         MVC   PRNAM+132(16),=16C'-'                                            
         MVC   PRDAY(3),=C'DAY'                                                 
         MVC   PRDAY+132(4),=4C'-'                                              
         MVC   PRTIME+3(4),=C'TIME'                                             
         MVC   PRTIME+132(11),=11C'-'                                           
         MVC   PRSHR(5),=C'SHARE'                                               
         MVC   PRSHR+132(6),=6C'-'                                              
         MVC   PRNTI(3),=C'NTI'                                                 
         MVC   PRNTI+132(4),=4C'-'                                              
         B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
         LTORG                                                                  
*                                                                               
RELO     DS    A                                                                
*                                                                               
DEMTAB1  DS    0H                                                               
         DC    XL2'00E5',AL1(45)   WOMEN 18+                                    
         DC    XL2'00E5',AL1(41)   WOMEN 18-34                                  
         DC    XL2'00E5',AL1(42)   WOMEN 18-49                                  
         DC    XL2'00E5',AL1(67)   WOMEN 21+                                    
         DC    XL2'00E5',AL1(71)   WOMEN 21-49                                  
         DC    XL2'00E5',AL1(47)   WOMEN 25-49                                  
         DC    XL2'00E5',AL1(48)   WOMEN 25-54                                  
         DC    XL2'00E5',AL1(53)   WOMEN 35-64                                  
         DC    XL2'00E5',AL1(206)  WOMEN 45+                                    
         DC    XL2'00E5',AL1(59)   WOMEN 55+                                    
*                                                                               
         DC    XL2'00E5',AL1(95)   MEN 18+                                      
         DC    XL2'00E5',AL1(91)   MEN 18-34                                    
         DC    XL2'00E5',AL1(92)   MEN 18-49                                    
         DC    XL2'00E5',AL1(118)  MEN 21+                                      
         DC    XL2'00E5',AL1(115)  MEN 21-49                                    
         DC    XL2'00E5',AL1(97)   MEN 25-49                                    
         DC    XL2'00E5',AL1(98)   MEN 25-54                                    
         DC    XL2'00E5',AL1(103)  MEN 35-64                                    
         DC    XL2'00E5',AL1(216)  MEN 45+                                      
         DC    XL2'00E5',AL1(109)  MEN 55+                                      
*                                                                               
         DC    XL2'00E5',AL1(145)  ADULTS 18+                                   
         DC    XL2'00E5',AL1(141)  ADULTS 18-34                                 
         DC    XL2'00E5',AL1(142)  ADULTS 18-49                                 
         DC    XL2'00E5',AL1(194)  ADULTS 21+                                   
         DC    XL2'00E5',AL1(191)  ADULTS 21-49                                 
         DC    XL2'00E5',AL1(147)  ADULTS 25-49                                 
         DC    XL2'00E5',AL1(148)  ADULTS 25-54                                 
         DC    XL2'00E5',AL1(153)  ADULTS 35-64                                 
         DC    XL2'00E5',AL1(226)  ADULTS 45+                                   
         DC    XL2'00E5',AL1(159)  ADULTS 55+                                   
*                                                                               
         DC    XL2'00E5',AL1(25)   WOMEN TEENS                                  
         DC    XL2'00E5',AL1(34)   WOMEN 15-24                                  
*                                                                               
         DC    XL2'00E5',AL1(21)   GIRLS 6-11                                   
         DC    XL2'00E5',AL1(119)  GIRLS 2-11                                   
*                                                                               
         DC    XL2'00E5',AL1(75)   MEN TEENS                                    
         DC    XL2'00E5',AL1(84)   MEN 15-24                                    
*                                                                               
         DC    XL2'00E5',AL1(22)   BOYS 6-11                                    
         DC    XL2'00E5',AL1(20)   BOYS 2-11                                    
*                                                                               
         DC    XL2'00E5',AL1(127)  ADULTS 2+                                    
         DC    XL2'00E5',AL1(125)  ADULTS TEENS                                 
         DC    XL2'00E5',AL1(134)  ADULTS 15-24                                 
*                                                                               
         DC    XL2'00E5',AL1(123)  CHILD 6-11                                   
         DC    XL2'00E5',AL1(162)  CHILD 9-11                                   
         DC    XL2'00E5',AL1(252)  CHILD 9-14                                   
         DC    XL2'00E5',AL1(122)  CHILD 2-11                                   
*                                                                               
         DC    XL2'00E5',AL1(65)   WORKW 18+                                    
         DC    XL2'00E5',AL1(237)  WORKW 18-49                                  
         DC    XL2'00E5',AL1(238)  WORKW 25-54                                  
         DC    XL2'00E5',AL1(66)   MOMS                                         
*                                                                               
         DC    XL2'00E5',AL1(242)  HHWCH <18                                    
         DC    XL2'00E5',AL1(241)  HHWCH <12                                    
         DC    XL2'00E5',AL1(240)  HHWCH <6                                     
         DC    X'FFFFFF'                                                        
         EJECT                                                                  
DISPTAB  DS    0H                                                               
         DC    AL1(00),AL1(02),CL1'V'  WOMEN 18+     , ADULT 18+                
         DC    AL1(03),AL1(05),CL1'V'  WOMEN 18-34   , ADULT 18-34              
         DC    AL1(06),AL1(08),CL1'V'  WOMEN 18-49   , ADULT 18-49              
         DC    AL1(49),AL1(51),CL1'V'  WOMEN 21+     , ADULT 21+                
         DC    AL1(43),AL1(45),CL1'V'  WOMEN 21-49   , ADULT 21-49              
         DC    AL1(09),AL1(11),CL1'V'  WOMEN 25-49   , ADULT 25-49              
         DC    AL1(12),AL1(14),CL1'V'  WOMEN 25-54   , ADULT 25-54              
         DC    AL1(30),AL1(32),CL1'V'  WOMEN 35-64   , ADULT 35-64              
         DC    AL1(46),AL1(48),CL1'V'  WOMEN 45+     , ADULT 45+                
         DC    AL1(18),AL1(20),CL1'V'  WOMEN 55+     , ADULT 55+                
*                                                                               
         DC    AL1(01),AL1(02),CL1'V'  MEN 18+    , ADULT 18+                   
         DC    AL1(04),AL1(05),CL1'V'  MEN 18-34  , ADULT 18-34                 
         DC    AL1(07),AL1(08),CL1'V'  MEN 18-49  , ADULT 18-49                 
         DC    AL1(50),AL1(51),CL1'V'  MEN 21+    , ADULT 21+                   
         DC    AL1(44),AL1(45),CL1'V'  MEN 21-49  , ADULT 21-49                 
         DC    AL1(10),AL1(11),CL1'V'  MEN 25-49  , ADULT 25-49                 
         DC    AL1(13),AL1(14),CL1'V'  MEN 25-54  , ADULT 25-54                 
         DC    AL1(31),AL1(32),CL1'V'  MEN 35-64  , ADULT 35-64                 
         DC    AL1(47),AL1(48),CL1'V'  MEN 45+    , ADULT 45+                   
         DC    AL1(19),AL1(20),CL1'V'  MEN 55+    , ADULT 55+                   
*                                                                               
         DC    AL1(02),AL1(99),CL1'V'  ADULTS 18+ , NULL                        
         DC    AL1(05),AL1(99),CL1'V'  ADULTS 18-34 , NULL                      
         DC    AL1(08),AL1(99),CL1'V'  ADULTS 18-49 , NULL                      
         DC    AL1(51),AL1(99),CL1'V'  ADULTS 21+ , NULL                        
         DC    AL1(45),AL1(99),CL1'V'  ADULTS 21-49 , NULL                      
         DC    AL1(11),AL1(99),CL1'V'  ADULTS 25-49 , NULL                      
         DC    AL1(14),AL1(99),CL1'V'  ADULTS 25-54 , NULL                      
         DC    AL1(32),AL1(99),CL1'V'  ADULTS 35-64 , NULL                      
         DC    AL1(48),AL1(99),CL1'V'  ADULTS 45+ , NULL                        
         DC    AL1(20),AL1(99),CL1'V'  ADULTS 55+ , NULL                        
*                                                                               
         DC    AL1(21),AL1(23),CL1'V'  WOMEN TEENS , ADULT TEENS                
         DC    AL1(37),AL1(39),CL1'V'  WOMEN 15-24 , ADULT 15-24                
*                                                                               
         DC    AL1(35),AL1(25),CL1'V'  GIRLS 6-11 , ADULT 6-11                  
         DC    AL1(33),AL1(26),CL1'V'  GIRLS 2-11 , ADULT 2-11                  
*                                                                               
         DC    AL1(22),AL1(23),CL1'V'  MEN TEENS  , ADULT TEENS                 
         DC    AL1(38),AL1(39),CL1'V'  MEN 15-24  ,    ADULT 15-24              
*                                                                               
         DC    AL1(36),AL1(25),CL1'V'  BOYS 6-11  ,    ADULT 6-11               
         DC    AL1(34),AL1(26),CL1'V'  BOYS 2-11  ,    ADULT 2-11               
*                                                                               
         DC    AL1(29),AL1(99),CL1'V'  ADULTS 2+  ,    NULL                     
         DC    AL1(23),AL1(99),CL1'V'  ADULTS TEENS ,  NULL                     
         DC    AL1(39),AL1(99),CL1'V'  ADULTS 15-24 ,  NULL                     
*                                                                               
         DC    AL1(25),AL1(99),CL1'V'  CHILDREN 6-11 , NULL                     
         DC    AL1(52),AL1(99),CL1'V'  CHILDREN 9-11 , NULL                     
         DC    AL1(53),AL1(99),CL1'V'  CHILDREN 9-14 , NULL                     
         DC    AL1(26),AL1(99),CL1'V'  CHILDREN 2-11 , NULL                     
*                                                                               
         DC    AL1(28),AL1(99),CL1'V'  WORKING 18+ ,   NULL                     
         DC    AL1(55),AL1(99),CL1'V'  WORKING 18-49 , NULL                     
         DC    AL1(56),AL1(99),CL1'V'  WORKING 25-54 , NULL                     
         DC    AL1(57),AL1(99),CL1'V'  MOMS       ,    NULL                     
*                                                                               
         DC    AL1(40),AL1(99),CL1'V'  HHWCH <18  ,    NULL                     
         DC    AL1(41),AL1(99),CL1'V'  HHWCH <12  ,    NULL                     
         DC    AL1(42),AL1(99),CL1'V'  HHWCH <6   ,    NULL                     
*                                                                               
*                                                                               
         EJECT                                                                  
*--SET OPTION AND PF KEY ROUTINES                                               
*                                                                               
         DS    0F                                                               
         DROP  R8,RB                                                            
OPPFRTN  NMOD1 0,**C13OP*                                                       
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING OPPFRTN,RB,R6                                                    
         L     R9,4(R1)                                                         
         L     RA,8(R1)                                                         
         L     RC,12(R1)                                                        
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   0(R1),0                                                          
         BE    OPBR0                                                            
         CLI   0(R1),1                                                          
         BE    OPBR1                                                            
         CLI   0(R1),2                                                          
         BE    OPBR2                                                            
         CLI   0(R1),3                                                          
         BE    OPBR3                                                            
         CLI   0(R1),4                                                          
         BE    OPBR4                                                            
         B     OPXIT                                                            
*                                                                               
OPBR0    BAS   RE,SETOPT                                                        
         B     OPXIT                                                            
OPBR1    BAS   RE,PF                                                            
         B     OPXIT                                                            
OPBR2    BAS   RE,PROCPF                                                        
         B     OPXIT                                                            
OPBR3    BAS   RE,VK                                                            
         B     OPXIT                                                            
OPBR4    BAS   RE,AK                                                            
         B     OPXIT                                                            
         EJECT                                                                  
*                                                                               
*  SET OPTION FILTERS FOR LIST SCREEN                                           
*                                                                               
SETOPT   NTR1                                                                   
         XC    OPTSPER,OPTSPER                                                  
         XC    OPTFILT,OPTFILT                                                  
         XC    OPTDAYP,OPTDAYP                                                  
         XC    OPTTIME,OPTTIME                                                  
         XC    OPTDAY,OPTDAY                                                    
         XC    OPTSTER,OPTSTER                                                  
*                                                                               
         LA    R2,LPROPTH          * OPTIONS                                    
         GOTO1 VALIFLD                                                          
         BZ    OPEXIT                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BE    STO310                                                           
         CLI   ACTNUM,ACTLIST                                                   
         BNE   INVERR2                                                          
STO310   L     R3,AIO2                                                          
         USING SCAND,R3                                                         
         XC    DMCB,DMCB                                                        
         GOTO1 SCANNER,DMCB,(20,(R2)),(4,0(R3))                                 
         CLI   DMCB+4,0                                                         
         BE    INVERR2                                                          
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,1,DMCB+4                                                      
*                                                                               
STO320   CLI   FLD1,C'F'           FILTER                                       
         BE    STO360                                                           
*                                                                               
         CLC   FLD1(2),=C'DY'      DAY                                          
         BE    STO370                                                           
*                                                                               
         CLI   FLD1,C'T'           TIME                                         
         BE    STO380                                                           
*                                                                               
         CLI   FLD1,C'D'           DAYPART                                      
         BE    STO390                                                           
*                                                                               
         CLC   FLD1(2),=C'SR'      STEREO REQUEST                               
         BE    STO400                                                           
*                                                                               
         CLC   FLD1(6),=C'STEREO'  STEREO REQUEST                               
         BE    STO400                                                           
*                                                                               
         CLI   FLD1,C'P'           PERIOD                                       
         BNE   INVERR2                                                          
* VALIDATE THE DATE FIELD                                                       
*                                                                               
         XC    SCANAREA,SCANAREA                                                
         MVC   SCANAREA+5(1),FLD2LEN                                            
         MVC   SCANAREA+8(20),FLD2                                              
         LA    R5,WORK2                                                         
         GOTO1 SCANNER,DMCB,SCANAREA,(2,0(R5)),C',=,-'                          
         CLI   4(R1),0                                                          
         BE    INVERR2                                                          
         CLI   1(R5),0             ARE 2 DATES INPUTTED                         
         BNE   STO330                                                           
         MVC   WORK2+1(1),WORK2                                                 
         MVC   WORK2+8(4),WORK2+4                                               
         MVC   WORK2+22(10),WORK2+12                                            
STO330   CLI   0(R3),0             TEST FIRST DATE GIVEN                        
         BE    INVERR2                                                          
         GOTO1 DATVAL,DMCB,(0,12(R5)),(0,50(R5))                                
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR2                                                          
         CLC   3(1,R1),0(R5)                                                    
         BNE   INVERR2                                                          
STO335   CLI   1(R3),0             TEST SECOND DATE GIVEN                       
         BE    INVERR2                                                          
         GOTO1 DATVAL,DMCB,(0,22(R5)),(0,56(R5))                                
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR2                                                          
         CLC   3(1,R1),1(R5)                                                    
         BNE   INVERR2                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,50(R5)),(2,OPTSPER)                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,56(R5)),(2,OPTEPER)                               
*                                                                               
         CLC   OPTSPER(2),OPTEPER   START CANNOT BE GREATER THEN END            
         BH    INVERR2                                                          
         B     STO500                                                           
*                                                                               
STO360   ICM   RE,1,FLD2LEN                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   OPTFILT(0),FLD2                                                  
         B     STO500                                                           
*                                                                               
STO370   ZIC   R5,FLD2LEN                                                       
         CLI   FLD2LEN,0                                                        
         BE    INVERR2                                                          
         GOTO1 DAYVAL,DMCB,((R5),FLD2),OPTDAY,BYTE                              
         CLI   OPTDAY,0                                                         
         BE    INVERR2                                                          
         B     STO500                                                           
*                                                                               
STO380   ZIC   R5,FLD2LEN                                                       
         GOTO1 TIMVAL,DMCB,((R5),FLD2),OPTTIME                                  
         CLI   DMCB,X'FF'          INVALID TIME                                 
         BE    INVERR2                                                          
         CLC   OPTTIME,=C'NONE'                                                 
         BE    INVERR2                                                          
         CLC   OPTTIME,=C'VARY'                                                 
         BE    INVERR2                                                          
         CLC   OPTTIME+2(2),=2X'00'                                             
         BE    INVERR2             END TIME REQUIRED                            
         CLC   OPTTIME+2(2),=C'CC'                                              
         BE    INVERR2                                                          
         B     STO500                                                           
*                                                                               
STO390   MVC   OPTDAYP,FLD2                                                     
         B     STO500                                                           
*                                                                               
STO400   MVI   OPTSTER,C'Y'                                                     
         B     STO500                                                           
*                                                                               
STO500   LA    R3,42(R3)                                                        
         BCT   R4,STO320                                                        
         B     OPEXIT                                                           
         EJECT                                                                  
*                                                                               
*--PFKEY MODE                                                                   
PF       NTR1                                                                   
         CLI   PFAID,0                                                          
         BE    OPEXIT                                                           
         CLI   PFAID,12                                                         
         BNE   PF50                                                             
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   *+10                                                             
         MVC   LISTDIR,SVLIST                                                   
         B     OPEXIT                                                           
PF50     OI    GENSTAT2,X'08'      SAVE THE SCREEN                              
         BAS   RE,PROCPF                                                        
         B     OPEXIT                                                           
*                                                                               
***********************************************************************         
*                      PROCESS PF KEYS                                *         
***********************************************************************         
*                                                                               
PROCPF   NTR1                                                                   
         LA    R2,PGMNETH                                                       
         SPACE 1                                                                
PROCPFA  CLI   PFAID,PF5                                                        
         BE    PROCPF4                                                          
         CLI   PFAID,PF4                                                        
         BNE   PROCPFX                                                          
         SPACE 1                                                                
         LA    R1,=C'DIS'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         SPACE 1                                                                
PROCPF2  MVI   PFAID,0                                                          
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   PROCPF3                                                          
         GOTO1 VCALL,WORK,=C'DPROG',,(4,PGMNET),(6,PGMPGR),(8,PGMEDT),0         
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPF3  GOTO1 VTRANSF,WORK,=C'DPROG',,(4,PGMNET),(6,PGMPGR),          X        
               (8,PGMEDT),0                                                     
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPF4  LA    R1,=C'CHA'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         B     PROCPF2                                                          
         SPACE 1                                                                
PROCPFX  B     OPEXIT                                                           
*                                                                               
         EJECT                                                                  
*    VALIDATE KEY                                                               
VK       NTR1                                                                   
*                                                                               
*  READ N0 PROFILE AT THE AGENCY LEVEL                                          
*                                                                               
         XC    KEY,KEY             GET USER PROFILE INTO NBUSER                 
         MVC   KEY(4),=C'S0N0'                                                  
         MVC   KEY+4(2),AGENCY                                                  
         GOTO1 GETPROF,DMCB,KEY,N0PROF,DATAMGR                                  
*                                                                               
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(2),=X'0D20'                                                
         SPACE                                                                  
         LA    R2,WORK             * MEDIA                                      
         XC    0(8,R2),0(R2)                                                    
         MVI   8(R2),C'N'          SET UP DUMMY HEADER                          
         GOTO1 VALIMED                                                          
         MVC   NPGKAM,BAGYMD                                                    
         MVC   SVKEY+2(1),BAGYMD                                                
         SPACE                                                                  
         MVI   NOPTFLG,0           SET OPTIONAL FLAG                            
         CLI   ACTNUM,ACTLIST      LIST/PRINT OPTIONAL                          
         BE    *+8                                                              
         CLI   ACTNUM,ACTREP       LIST/PRINT OPTIONAL                          
         BNE   *+8                                                              
         MVI   NOPTFLG,1           SET OPTIONAL FLAG                            
         SPACE                                                                  
         LA    R2,PGMNETH          * NETWORK                                    
         GOTO1 VALIFLD                                                          
         BNZ   VK050                                                            
         MVI   ERROR,INVALID                                                    
         B     OPINVERR                                                         
VK050    GOTO1 VALINTWK                                                         
         MVC   NPGKNET,QNETMKT                                                  
         MVC   SVKEY+3(2),QNETMKT                                               
         SPACE                                                                  
VK100    LA    R2,PGMPGRH          * PROGRAM                                    
         GOTO1 VALIFLD                                                          
         BNZ   VK150                                                            
         CLI   NOPTFLG,1                                                        
         BE    VK200                                                            
         MVI   ERROR,INVALID                                                    
         B     OPINVERR                                                         
VK150    MVI   SOFTCHK,0                                                        
         CLI   PGMEDTH+5,0              WAS DATE INPUTTED                       
         BNZ   VK180                    YES, FULLY VALDATE PROGRAM              
         CLI   NOPTFLG,1                IS ACTION LIST OR PRINT                 
         BNE   VK180                    NO, FULLY VALIDATE PROGRAM              
         MVI   SOFTCHK,1                SOFT VALIDATE THE PROGRAM               
VK180    GOTO1 VALIPRO                                                          
         MVC   NPGKPROG,NFLD                                                    
         MVC   SVKEY+5(6),NFLD                                                  
         SPACE                                                                  
VK200    LA    R2,PGMEDTH          * END DATE                                   
         GOTO1 VALIFLD                                                          
         BNZ   VK250                                                            
         CLI   NOPTFLG,1                                                        
         BE    VK300                                                            
         MVI   ERROR,INVALID                                                    
         B     OPINVERR                                                         
VK250    GOTO1 VALIDAT                                                          
         GOTO1 DATCON,DMCB,(0,QDATE),(2,SVKEY+11)                               
         MVC   NPGKPROG,SVKEY+11                                                
*                                                                               
         MVC   KEY,SVKEY                                                        
         CLI   ACTNUM,ACTADD                                                    
         BE    VK300                                                            
         GOTO1 VSETSPT                                                          
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         GOTO1 HIGH                NO/SO CHECK IF END DATE MATCHES              
         CLC   KEY(11),KEYSAVE          A RECORD                                
         BNE   VK300                                                            
         CLC   KEY+11(2),KEYSAVE+11     A RECORD                                
         BNE   ENDATERR                                                         
*                                                                               
VK300    MVC   KEY,SVKEY                                                        
         GOTO1 VSETSPT                                                          
         B     OPEXIT                                                           
         EJECT                                                                  
*                                                                               
*  PASSIVE POINTER MAINTENANCE                                                  
*                                                                               
AK       NTR1                                                                   
         CLC   NDAYTIM(3),=C'DEL'                                               
         BE    AKEXT                                                            
         CLI   PGMSPECH+5,0                                                     
         BNE   AKEXT                                                            
         MVC   KEY(13),SVKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                SHOULD HAVE BEEN FOUND IN EDIT               
         MVC   SVKEY+14(4),KEY+14  DISK ADDRESS IN KEY                          
         GOTO1 HELLO,DMCB,(C'G',=C'SPTFILE'),(X'92',0(R7)),0                    
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
         USING NPGELEM,R3                                                       
         CLI   NPGUPLD,C'Y'        CABLE UPLOAD PROGRAM RECORD                  
         BE    AKEXT               NO SECONDARY KEY                             
         CLI   ACTNUM,ACTADD       SEE IF ADD                                   
         BE    AK100                                                            
         XC    KEY,KEY             FIND OLD POINTER AND DELETE IT               
         MVC   KEY(2),=X'0DA0'                                                  
         MVC   KEY+2(3),SVKEY+2                                                 
         MVC   KEY+5(6),ODAYTIM       OLD DAY AND TIME                          
         MVC   KEY+11(2),SVKEY+11  END DATE                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST FIND OLD POINTER                        
         CLC   ODAYTIM(5),NDAYTIM                                               
         BE    AK100               NO CHG OF DAY OR TIME                        
*                                  LEAVE POINTER ALONE                          
         OI    KEY+13,X'80'        MARK DELETED                                 
         GOTO1 WRITE                                                            
*                                                                               
AK100    DS    0H                                                               
         CLC   ODAYTIM(5),NDAYTIM                                               
         BE    AKEXT               NO CHG OF DAY OR TIME                        
*                                  DON'T TRY TO ADD NEW POINTER                 
         GOTO1 HELLO,DMCB,(C'G',=C'SPTFILE'),(X'92',0(R7)),0                    
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
*                                                                               
         XC    KEY,KEY             ADD NEW POINTER                              
         MVC   KEY(2),=X'0DA0'                                                  
         MVC   KEY+2(3),SVKEY+2                                                 
         MVC   KEY+5(1),NPGRDAY                                                 
         MVC   KEY+6(4),NPGTIME                                                 
         MVC   KEY+10(1),NPGUNIQ                                                
         MVC   KEY+11(2),SVKEY+11                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),KEYSAVE                                                  
         BNE   *+6                                                              
         DC    H'0'                SHOULD HAVE BEEN FOUND IN EDIT               
*                                                                               
         MVC   KEY(2),=X'0DA0'                                                  
         MVC   KEY+2(3),SVKEY+2                                                 
         MVC   KEY+5(1),NPGRDAY                                                 
         MVC   KEY+6(4),NPGTIME                                                 
         MVC   KEY+10(1),NPGUNIQ                                                
         MVC   KEY+11(2),SVKEY+11                                               
         MVI   KEY+13,X'00'                                                     
         MVC   KEY+14(4),SVKEY+14                                               
         GOTO1 ADD                                                              
AKEXT    B     OPEXIT                                                           
         EJECT            &                                                     
*                                                                               
OPINVERR MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
*                                                                               
ENDATERR DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'*** END DATE ERROR - NEXT DATE'                   
         GOTO1 DATCON,DMCB,(2,KEY+11),(5,CONHEAD+31)                            
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
OPEXIT   XIT1                                                                   
*                                                                               
OPXIT    XMOD1                                                                  
         DROP  R8                                                               
*                                                                               
INVERR2  MVI   ERROR,INVALID                                                    
*                                                                               
TRAPERR2 GOTO1 ERREX                                                            
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
PLINED   DSECT                                                                  
PRNET    DS    CL4                                                              
         DS    CL4                                                              
PRCODE   DS    CL6                                                              
         DS    CL4                                                              
PRDAT    DS    CL17                                                             
         DS    CL4                                                              
PRNAM    DS    CL16                                                             
         DS    CL4                                                              
PRDAY    DS    CL8                                                              
         DS    CL4                                                              
PRTIME   DS    CL11                                                             
         DS    CL4                                                              
PRSHR    DS    CL6                                                              
         DS    CL4                                                              
PRNTI    DS    CL4                                                              
         SPACE 2                                                                
LLINED   DSECT                                                                  
LRDAT    DS    CL8                                                              
         DS    CL1                                                              
LRCODE   DS    CL6                                                              
         DS    CL3                                                              
LRNAM    DS    CL16                                                             
         DS    CL3                                                              
LRDAY    DS    CL8                                                              
         DS    CL1                                                              
LRTIME   DS    CL11                                                             
         DS    CL1                                                              
LRSHR    DS    CL6                                                              
         DS    CL2                                                              
LRNTI    DS    CL4                                                              
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONHEAD-64                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*DSECT TO COVER SAVED STORAGE IN TWA0 FOR NESFM00                               
T31CFFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEADH+3520)-(T31CFFD+3072))                                 
         ORG   CONHEADH+3520-SAVAREAL                                           
SAVAREA  DS    0C                                                               
CALLSP   DS    X                   CALL ROUTINE STACK POINTER                   
CALLSTK  DS    XL9                 STACK (LIST OF OVERLAYS)                     
LASTOV   DS    X                   LAST OVERLAY                                 
SVLIST   DS    XL188               LISTDIR SAVE AREA                            
AUTHCODE DS    H                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMF7D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMF8D                                                       
         EJECT                                                                  
*                                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUSER                                                      
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C13 WORK AREA  *******                  
WORKAREA DS    0CL500                                                           
WORK2    DS    CL64                                                             
SVFMTSW  DS    H                                                                
OLDVPHS  DS    CL34                OLD VPHS                                     
SPFIL20  DS    CL20                20 SPACES                                    
ODAYTIM  DS    CL6                 OLD DAY/TIME/UNIQ                            
NDAYTIM  DS    CL6                 NEW DAY/TIME/UNIQ                            
SVCUPLD  DS    CL1                 CABLE UPLOAD SETTING                         
ELEMCODE DS    CL1                 92 OR 93 ELEMNT CODE                         
OPTDAYP  DS    CL1                 OPTION DAYPART                               
OPTFILT  DS    CL3                 OPTION FILTER                                
OPTSPER  DS    CL2                 OPTION START PERIOD                          
OPTEPER  DS    CL2                 OPTION END PERIOD                            
OPTDAY   DS    CL1                 OPTION DAY                                   
OPTTIME  DS    CL4                 OPTION TIME                                  
OPTSTER  DS    CL1                 Y=STEREO REQUEST                             
SCANAREA DS    CL28                                                             
SVDMWORK DS    CL96                DEMWORK SAVE AREA                            
MAXVPHF  EQU   52                  MAXIMUM VPH FIELDS                           
MAX93VPH EQU   58                  MAXIMUM VPHS IN RECORD (93 ELEMENT)          
         EJECT                                                                  
*                                                                               
SCAND    DSECT                                                                  
*              DSECT TO COVER SCANNER LINES                                     
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'153NESFM13S  05/01/02'                                      
         END                                                                    
