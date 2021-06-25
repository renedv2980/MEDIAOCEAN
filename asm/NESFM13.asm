*          DATA SET NESFM13    AT LEVEL 011 AS OF 06/02/20                      
*PROCESS USING(WARN(15))                                                        
***DEIS NOTE: CHANGES TO SUPPORT B6-14 WERE AT LEVEL **8                        
*PHASE T31C13E                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'NESFM13 -  PROGRAM REC'                                         
         PRINT NOGEN                                                            
T31C13   CSECT                                                                  
         NMOD1 0,T31C13,R8,RR=R3                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         MVI   SPFIL20,C' '                                                     
         MVC   SPFIL20+1(L'SPFIL20-1),SPFIL20                                   
*                                                                               
         MVI   AEFLAGS,0                                                        
*                                                                               
         BRAS  RE,SETSCRN          SET SCREEN FOR COMSCORE                      
*                                                                               
         MVI   GOAGAIN,C'N'        RESET SWAP SWITCH                            
         MVC   AIO,AIO1                                                         
         L     R7,AIO1                                                          
         USING NPGRECD,R7                                                       
         SPACE 3                                                                
         OI    CONRECH+6,X'01'     VALIDATE EVERY TIME (PF KEY NEED)            
*                                                                               
*!!!!    BAS   RE,CHKCABL          IF CABLE SET CABLE BIT                       
*                                                                               
         MVI   SVCUPLD,X'40'       CLEAR SAVE AREA                              
         CLC   0(2,R7),=XL2'0D20'  IS IT A PROGRAM RECORD                       
         BNE   MAIN10                                                           
         LA    R3,24(R7)                                                        
         USING NPGELEM,R3                                                       
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NXTEL                                                         
         BNE   MAIN10                                                           
         CLI   NPGUPLD,C'Y'                                                     
         BE    MAIN10                                                           
         TM    NPGKPROG+3,X'F0'                                                 
         BNO   MAIN10                                                           
         TM    NPGKPROG+4,X'F0'                                                 
         BNO   MAIN10                                                           
         TM    NPGKPROG+5,X'F0'                                                 
         BNO   MAIN10                                                           
         MVI   NPGUPLD,C'Y'                                                     
         DROP  R3                                                               
*                                                                               
MAIN10   CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   VKCHK                                                            
         MVC   SVDMWORK,DMWORK                                                  
         BAS   RE,GOPROCPF                                                      
         CLI   RECNUM,60           FOR AUDIENCE ESTIMATOR                       
         BNE   *+12                                                             
         BRAS  RE,VERSNUM          GET VERSION NUMBER                           
         BRAS  RE,GETATBS          GET A(TABLES) FROM DATASPACE                 
*                                                                               
         BAS   RE,VR                                                            
         MVC   DMWORK(96),SVDMWORK                                              
         B     EXIT                                                             
VKCHK    CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   XRADD                                                            
         BAS   RE,GOPROCPF                                                      
         GOTOR OPPFRTN,DMCB,(3,DUB),(R9),(RA),(RC)    VK                        
         B     EXIT                                                             
XRADD    CLI   MODE,XRECADD        ADD PASSIVE KEY                              
         BE    GOAK                                                             
         CLI   MODE,XRECPUT        CHANGE PASSIVE KEY                           
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
         GOTOR OPPFRTN,DMCB,(1,DUB),(R9),(RA),(RC)          PF                  
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* GOTO PROCPF ROUTINE                                                           
GOPROCPF NTR1                                                                   
         GOTOR OPPFRTN,DMCB,(2,DUB),(R9),(RA),(RC)         PROCPF               
         B     EXIT                                                             
         EJECT                                                                  
DK       DS    0H                                                               
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
         CLI   RECNUM,60           DON'T DISPLAY RECORD UNLESS INVOKED          
         BE    EXIT                 BY FACPAK (NOT AUDIENCE ESTIMATOR!)         
*                                                                               
         NI    PRECFLAG,X'FF'-PREC1DEC                                          
         NI    PRECFLAG,X'FF'-PREC2DEC                                          
*                                                                               
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   *+10                                                             
         MVC   SVLIST,LISTDIR                                                   
*                                                                               
         CLI   PGMSPECH+5,0        CHK FOR SPECIAL INPUT                        
         BE    DR100                                                            
         CLC   PGMSPEC(4),=C'NTI,'                                              
         BE    DR100                                                            
         B     FMTPCT              MUST BE PERCENTAGE ADJUSTMENT                
*                                                                               
DR100    DS    0H                                                               
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'5D'                                                     
         BAS   RE,NXTEL                                                         
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR MUST HAVE 5D ELEM                
*                                                                               
         CLC   5(2,R3),=X'5901'    2 DECIMAL PRECISION?                         
         BNE   DR110                                                            
         OI    PRECFLAG,PREC2DEC                                                
         MVC   PGMNO,=C' 0'                                                     
         B     DR120                                                            
*                                                                               
DR110    DS    0H                                                               
         OI    PRECFLAG,PREC1DEC   1 DECIMAL PRECISION                          
*                                                                               
         MVC   PGMNO,=C' N'                                                     
         CLC   5(2,R3),=X'580E'    SEE IF NEW PROGRAM                           
         BNE   *+10                                                             
         MVC   PGMNO,=C' 0'                                                     
*                                                                               
DR120    FOUT  PGMNOH                                                           
*                                                                               
         XC    PGMDYPT,PGMDYPT                                                  
         XC    PGMSDAT,PGMSDAT                                                  
         XC    PGMVWSC,PGMVWSC                                                  
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'93'                                                     
         BAS   RE,NXTEL                                                         
         BNE   DR130                                                            
         USING NPGEL93,R3                                                       
         MVC   PGMDYPT(1),NPG2DYP                                               
         OC    NPG2DYPA,NPG2DYPA                                                
         BZ    *+10                                                             
         MVC   PGMDYPT(2),NPG2DYPA      ALPHA DAYPART                           
         MVC   PGMVWSC(1),NPG2SRC       SOURCE                                  
         MVC   PGMVWSC+1(1),NPG2VTYP    VIEWING TYPE                            
         DROP  R3                                                               
*                                                                               
         OC    2(2,R3),2(R3)                                                    
         BZ    DR130                                                            
         GOTO1 DATCON,DMCB,(2,2(R3)),(8,PGMSDAT)                                
DR130    FOUT  PGMDYPTH                                                         
         FOUT  PGMSDATH                                                         
         FOUT  PGMVWSCH                                                         
*                                                                               
         XC    PGMNCDE,PGMNCDE                                                  
         XC    PGMOTH,PGMOTH                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR180                                                            
         BRAS  RE,DISP03                                                        
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
*                                                                               
         L     R7,AIO1                                                          
         USING NPGRECD,R7                                                       
*                                                                               
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NXTEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND 92 ELEM                            
*                                                                               
         BRAS  RE,DRATSHR          DISPLAY RATING/SHARE                         
         MVC   PGMSHR,RSHRFLD                                                   
         FOUT  PGMSHRH                                                          
*                                                                               
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
DR360    BRAS  RE,AEPGDIS          DISPLAY AE-CREATED PROG RECDS                
         BE    DR430                                                            
         LA    R2,PGMW1H           FIRST VPH FIELD                              
         LA    R5,MAXVPHF          FOR BCT                                      
         L     R4,=A(DISPTAB)                                                   
         A     R4,RELO                                                          
*                                                                               
* DISPTAB IS IN NEPROGDEMS AND CONTAINS THE DISPLACEMENT OF THE                 
* VPH BUILDING BLOCKS IN THE X'92' ELEMENT OF THE PROGRAM RECORD.               
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
         BAS   RE,NEXTUN           POINT R2 TO NEXT UNPROTECTED FLD             
         LA    R4,3(R4)                                                         
         BCT   R5,DR400                                                         
*                                                                               
* AT THIS POINT, R6 IS AT NPG2DYPA                                              
*                                                                               
         LR    R6,R3               POINT TO BEGINNING OF DEMOS                  
         AHI   R6,NPG2VPH2-NPG2VPHS   R6 = A(NPG2VPH2)                          
         LA    R2,PGMVPH2H         M1214                                        
         OC    0(2,R6),0(R6)                                                    
         BZ    DR430                                                            
         EDIT  (B2,0(R6)),(4,8(R2)),0,ALIGN=LEFT                                
         OI    4(R2),X'20'         SET PREVALID BIT                             
         FOUT  (R2)                                                             
*                                                                               
DR430    CLI   PGMSPECH+5,0        COULD GO THROUGH FMT LOGIC                   
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
         MVC   PGMACTV(13),=C'LAST ACTIVITY'                                    
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
         ZIC   RE,WORK             WORK HAS LENGTH                              
         LR    R0,R5               SEE IF IT WILL FIT ON THIS LINE              
         AR    R0,RE                                                            
         AHI   R0,4                ADJUST FOR VALUE                             
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
SETR2    MVI   BYTE,C'Y'                                                        
         LA    R2,PGMOTH                                                        
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
         MHI   RF,10                                                            
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
         AHI   R4,9                SET TOTAL LENGTH OF EXPRESSION               
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
         MVC   CONHEAD(19),=C'* ENTER RECORD DATA'                              
         FOUT  CONHEADH                                                         
         B     TRAPERX2                                                         
*                                                                               
SPECPCT  DS    0H                                                               
         LA    R2,PGMDAYH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(20),=C'* ENTER AMENDED DATA'                             
         FOUT  CONHEADH                                                         
         B     TRAPERX2                                                         
*                                                                               
DELMSG   DS    0H                                                               
         LA    R2,CONACTH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(43),=C'LAST SELECTION - HIT ENTER TO CONTINUE LIX        
               ST'                                                              
         FOUT  CONHEADH                                                         
         MVC   CONACT(8),=C'SELECT  '                                           
         FOUT  CONACTH                                                          
         B     TRAPERX2                                                         
*                                                                               
TRAPERX2 BRAS  RE,AEFLDNO          (AE) SEND FIELD NUMBER TO PC                 
         GOTO1 ERREX2                                                           
***********************************************************************         
VR       NTR1                                                                   
*                                                                               
         CLI   T31CFFD+1,C'*'      DDS ONLY?                                    
         BE    VR10                                                             
         TM    T31CFFD+13,X'10'    RESTRICT ACCESS TO JUST CABLE                
         BZ    VR10                                                             
         CLI   QPTYPE,C'C'         CABLE POSTING TYPE?                          
         BNE   INVAERR                                                          
*              FIRST CHK FOR VPH DISPLAY FROM NTI POCKET PIECE                  
VR10     CLI   RECNUM,60           THIS TWA FIELD DOESN'T EXIST...              
         BE    VR100               ...UNDER AUDIENCE ESTIMATOR                  
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
*                                                                               
*        GET 2K COMPLIANT YEAR                                                  
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),FLD2        2 CHARACTER YEAR                             
         MVC   WORK+2(4),=C'0101'  DEFAULT TO JAN 1                             
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
         MVC   DBSELBK,WORK+10     MOVE IN 2K COMPLIANT YEAR                    
*                                                                               
*!!!     MVC   DBSELBK,FLD2B+3     YEAR                                         
*                                                                               
         MVC   DBSELBK+1(1),FLD1B+3   WEEK OR MONTH                             
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
         L     R0,=A(DEMTAB1)                                                   
         A     R0,RELO                                                          
         ST    R0,0(R1)            P1 TO DEMOUT IS A(DEMTAB1)                   
         MVI   0(R1),C'L'          P1 HOB IS C'L'                               
         GOTO1 (RF),(R1),,0(R4),0(R3)                                           
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
         BAS   RE,NEXTUN           POINT R2 TO NEXT UNPROTECTED FLD             
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
VR100    BRAS  RE,ACTIVITY         ADD OR UPDATE ACTIVITY ELEM                  
*!!!!    BAS   RE,OTHEREL          ADD OR UPDATE EXTRA SPACE ELEMENT            
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'        SECONDARY DATA ELEMENT                       
         BRAS  RE,GETEL                                                         
         BE    VR105                                                            
         XC    ELEM(40),ELEM                                                    
*        MVC   ELEM(2),=XL2'0328'                                               
         MVI   ELEM,NPG3ELQ                                                     
         MVI   ELEM+1,NPG3LNQ2                                                  
         BAS   RE,PUTEL                                                         
*                                                                               
VR105    CLI   ACTNUM,ACTSEL                                                    
         BNE   *+10                                                             
         MVC   SVKEY(13),0(R7)     LOAD SELECT KEY                              
         XC    ELEM(100),ELEM                                                   
         MVC   ELEM(2),=X'9250'       SET CODE AND LENGTH                       
         XC    ODAYTIM,ODAYTIM     USED TO SAVE OLD DAY AND TIME                
         XC    NDAYTIM,NDAYTIM                                                  
         XC    OLDVPHS,OLDVPHS     USED TO SAVE OLD VPHS                        
         MVI   SVCUPLD,X'40'       CLEAR UPLOAD TYPE                            
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
*                                                                               
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
         MVC   CONHEAD(17),=C'* PROGRAM DELETED'                                
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
         MVI   KEY+10,X'FF'        CHECK IF LIMIT EXCEEDED                      
         MVC   KEY+11(2),SVKEY+11  END DATE                                     
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VR330                                                            
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'* LIMIT EXCEEDED FOR DAY/TIME/END DATE'           
         MVC   CONHEAD+38(10),=C', CALL DDS'                                    
         FOUT  CONHEADH                                                         
         B     TRAPERX2                                                         
*                                                                               
VR330    DS    0H                                                               
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
VR410    DS    0H                                                               
         BRAS  RE,VRATSHR          VALIDATE RATING AND SHARE                    
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
         BE    VR470                                                            
         GOTO1 ANY                                                              
         BAS   RE,PACKSI                                                        
         STH   R0,HALF                                                          
         CHI   R0,0                                                             
         BNH   INVERR                                                           
         MVC   NPGPPNO,HALF                                                     
*                                                                               
VR470    XC    SVCSSN,SVCSSN       COMSCORE SERIES #                            
         LA    R2,PGMCSNH                                                       
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   SVCSSN,8(R2)                                                     
*                                                                               
VR560    XC    NPGVPHS(34),NPGVPHS   ZERO VPHS IN 92 ELEMENT                    
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'92'                                                     
         BAS   RE,DELEL                                                         
*                                                                               
         BAS   RE,PUTEL                                                         
         DROP  R3                                                               
*                                                                               
* UPDATE LENGTH OF 03 ELEM                                                      
*                                                                               
VR562    L     R6,AIO                                                           
         MVI   ELCODE,X'03'        SECONDARY DATA ELEMENT                       
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   1(R6),NPG3LNQ1      OLD STYLE LENGTH?                            
         JH    VR570                                                            
         XC    ELEM,ELEM                                                        
         MVC   ELEM(NPG3LNQ1),0(R6)                                             
         MVI   ELEM+1,NPG3LNQ2     UPDATE LENGTH                                
         BAS   RE,DELEL            DELETE OLD LENGTH                            
         BAS   RE,PUTEL                                                         
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
*                                                                               
         MVC   TEMPKEY,KEY                                                      
         GOTO1 VALIDPT                                                          
         MVC   NPG2DYP,QDPT                                                     
         MVC   NPG2DYPA,QDPT2                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),TEMPKEY                                                  
         GOTO1 HIGH                                                             
*                                                                               
VR590    L     R6,AIO                                                           
         USING NPGEL03,R6                                                       
         MVI   ELCODE,X'03'        SECONDARY DATA ELEMENT                       
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   NPGCSN,SVCSSN       COMSCORE SERIES #                            
*                                                                               
         LA    R2,PGMNEWH          * PROGRAM STATUS                             
         MVC   NPPRNEW,8(R2)                                                    
         CLI   5(R2),0                                                          
         BE    VR591                                                            
         CLI   8(R2),C'N'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   INVERR                                                           
*                                                                               
VR591    LA    R2,PGMCNTH          * PROGRAM CONTENT                            
         XC    NPPRGRAT,NPPRGRAT                                                
         CLI   5(R2),0                                                          
         BE    VR592                                                            
         L     RE,=A(CONTAB)                                                    
         A     RE,RELO                                                          
         USING CONTABD,RE                                                       
         OI    9(R2),X'40'                                                      
VR591B   CLI   0(RE),X'FF'                                                      
         BE    INVERR                                                           
         CLC   CONTCD,8(R2)                                                     
         BE    VR591D                                                           
         LA    RE,CONTABL(RE)                                                   
         B     VR591B                                                           
VR591D   MVC   NPPRGRAT,CONTCD                                                  
         DROP  RE                                                               
*                                                                               
VR592    LA    R2,PGMTYPH          * PROGRAM TYPE                               
         XC    NPPRGTYP,NPPRGTYP                                                
         XC    NPPRGSTP,NPPRGSTP                                                
         CLI   5(R2),0                                                          
         BE    VR594                                                            
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         USING PTYRECD,RE                                                       
         L     RF,ASPOOLD                                                       
         USING SPOOLD,RF                                                        
         MVC   PTYKTYPE,=XL2'0D54'                                              
         MVC   PTYKAGY,AGENCY                                                   
         MVC   PTYKCODE,8(R2)                                                   
         OC    PTYKCODE,SPACES                                                  
         MVC   NPPRGTYP,PTYKCODE                                                
         CLI   PGMSTYPH+5,0         CHECK SUB PROGRAM TYPE                      
         BE    VR593                                                            
         MVC   PTYKSUB,PGMSTYP                                                  
         OC    PTYKSUB,SPACES                                                   
         MVC   NPPRGSTP,PTYKSUB                                                 
VR593    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVERR                                                           
         DROP  RF                                                               
*                                                                               
*                                                                               
VR594    XC    NPGTRFAX,NPGTRFAX   CLEAR OTHER DATA FIELDS                      
         MVI   NPGMIRCD,0          CLEAR OTHER DATA FIELDS                      
         MVI   NPTIER,0            CLEAR OTHER DATA FIELDS                      
******   NI    NPGSTATB,X'FF'-RCPREC    CLEAR OTHER DATA FIELDS                 
*                                                                               
         CLI   RECNUM,60           "OTHER" TWA FIELD DOESN'T EXIST...           
         BNE   VR594X              ...UNDER AUDIENCE ESTIMATOR                  
*                                                                               
         CLI   PGAFAXH+5,0         FAX NUMBER                                   
         BE    *+10                                                             
         MVC   NPGTRFAX,PGAFAX                                                  
*                                                                               
         SR    R0,R0               WINDOW                                       
         ICM   R0,1,PGAWINDH+5                                                  
         BZ    VR594E                                                           
         OI    NPGSTATB,X'80'      SET WINDOW FLAG                              
         CLI   PGAWIND,C'W'        WINDOW WITHOUT A VALUE                       
         BNE   *+12                                                             
         MVI   NPWINNUM,0                                                       
         B     VR594E                                                           
         GOTO1 =V(NUMVAL),DMCB,PGAWIND,(R0),RR=RELO                             
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   DMCB+4(4),=F'100'   MUST BE <= 100                               
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   NPWINNUM,DMCB+7     SAVE TO RECORD                               
*                                                                               
VR594E   DS    0H                                                               
         CLI   PGASDPTH+5,0        SUB-DAYPART                                  
         BE    *+10                                                             
         MVC   NPGSDPT,PGASDPT                                                  
*                                                                               
         NI    NPGSTATB,X'FF'-TCAR5  TCAR/WB1                                   
         CLI   PGATCARH+5,0                                                     
         BE    VR594J                                                           
         L     RE,=A(TCARTAB)                                                   
         A     RE,RELO                                                          
         CLI   PGATCAR,C'W'        WB1?                                         
         BE    VR594G                                                           
         L     RE,=A(TCARTAB2)     NO, TCAR                                     
         A     RE,RELO                                                          
         OI    NPGSTATB,TCAR5                                                   
VR594G   CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   NPGTCAR,PGATCAR+1                                                
         CLC   0(1,RE),PGATCAR+1                                                
         BE    VR594J                                                           
         LA    RE,1(RE)                                                         
         B     VR594G                                                           
*                                                                               
VR594J   DS    0H                                                               
         SR    R0,R0               MULTI-RUN                                    
         ICM   R0,1,PGAMRH+5                                                    
         BZ    VR594M                                                           
         GOTO1 =V(NUMVAL),DMCB,PGAMR,(R0),RR=RELO                               
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   DMCB+4(4),=F'2'                                                  
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   DMCB+4(4),=F'15'                                                 
         BNH   *+6                                                              
         DC    H'0'                                                             
         OC    NPGMRUN,NPGMRUN                                                  
         BZ    *+16                                                             
         CLC   NPGMRUN,DMCB+7                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NPGMRUN,DMCB+7                                                   
*                                                                               
VR594M   DS    0H                                                               
         CLI   PGAMIRH+5,0         MIRRORED PROGRAM                             
         BE    VR594O                                                           
         L     RE,=A(MIRTAB)                                                    
         A     RE,RELO                                                          
         USING MIRTABD,RE                                                       
VR594N   CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   NPGMIRCD,PGAMIR                                                  
         CLC   MIRCODE,PGAMIR                                                   
         BE    *+12                                                             
         LA    RE,MIRTABL(RE)                                                   
         B     VR594N                                                           
         DROP  RE                                                               
*                                                                               
VR594O   DS    0H                                                               
         CLI   PGATIERH+5,0        TIER                                         
         BE    VR594P                                                           
         CLI   PGATIER,C'1'                                                     
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLI   PGATIER,C'4'                                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   NPTIER,PGATIER                                                   
*                                                                               
VR594P   DS    0H                                                               
         CLI   PGARCPH+5,0          RCP                                         
         BE    VR594R                                                           
         CLI   NPGACT,C'A'          FIELD CANNOT BE CHANGED AFTER ADD           
         BNE   VR594R                                                           
         CLI   PGARCP,C'N'                                                      
         BE    VR594R                                                           
         CLI   PGARCP,C'Y'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    NPGSTATB,RCPREC                                                  
*                                                                               
VR594R   XC    NPGNADDM,NPGNADDM   DEMDEF CODE                                  
         CLI   PGADDEFH+5,0                                                     
         BE    VR594R6                                                          
         MVC   NPGNADDM,PGADDEF                                                 
         LA    RE,NPGNADDM+L'NPGNADDM-1  LAST BYTE OF DEMDEF CODE               
VR594R2  CLI   0(RE),C' '          RIGHT-PADDED WITH ZEROS                      
         BNE   VR594R4                                                          
         MVI   0(RE),0                                                          
         SHI   RE,1                                                             
         B     VR594R2                                                          
VR594R4  DS    0X                                                               
*                                                                               
VR594R6  XC    NPGCDEF,NPGCDEF     COMDEF CODE                                  
         CLI   PGACDEFH+5,0                                                     
         BE    VR594R12                                                         
         MVC   NPGCDEF,PGACDEF                                                  
         LA    RE,NPGCDEF+L'NPGCDEF-1  LAST BYTE OF COMDEF CODE                 
VR594R8  CLI   0(RE),C' '          RIGHT-PADDED WITH ZEROS                      
         BNE   VR594R10                                                         
         MVI   0(RE),0                                                          
         SHI   RE,1                                                             
         B     VR594R8                                                          
VR594R10 DS    0X                                                               
*                                                                               
VR594R12 XC    NPGCSN,NPGCSN       COMSCORE SERIES # FOR POSTING                
         CLI   PGACSNH+5,0                                                      
         BE    VR594R18                                                         
         MVC   NPGCSN,PGACSN                                                    
         LA    RE,NPGCSN+L'NPGCSN-1  LAST BYTE OF CS SERIES #                   
VR594R14 CLI   0(RE),C' '          RIGHT-PADDED WITH ZEROS                      
         BNE   VR594R16                                                         
         MVI   0(RE),0                                                          
         SHI   RE,1                                                             
         B     VR594R14                                                         
VR594R16 DS    0X                                                               
*                                                                               
VR594R18 XC    NPGCSVT,NPGCSVT     COMSCORE VIEWING TYPE                        
         CLI   PGACVTYH+5,0                                                     
         BE    VR594R20                                                         
         MVI   NPGCSVT,NPCVTRLQ    DEFAULT TO LIVE                              
         CLC   =C'RC',PGACVTY                                                   
         JNE   *+8                                                              
         MVI   NPGCSVT,NPCVTRCQ    LIVE COMMERCIAL                              
         CLC   =C'R3',PGACVTY                                                   
         JNE   *+8                                                              
         MVI   NPGCSVT,NPCVTR3Q    LIVE + 3                                     
         CLC   =C'R7',PGACVTY                                                   
         JNE   *+8                                                              
         MVI   NPGCSVT,NPCVTR7Q    LIVE + 7                                     
*                                                                               
VR594R20 NI    AEFLAGS,X'FF'-AEFCSOQ     COMSCORE ONLY PROGRAM                  
         CLI   PGACPO,C'Y'                                                      
         BNE   *+8                                                              
         OI    AEFLAGS,AEFCSOQ                                                  
*                                                                               
VR594S   MVI   NPG2SRC,0           VIEWING SOURCE                               
         CLI   PGAVSRCH+5,0                                                     
         BE    VR594T                                                           
         MVC   NPG2SRC,PGAVSRC                                                  
*                                                                               
VR594T   MVI   NPG2VTYP,0          VIEWING TYPE                                 
         CLI   PGAVTYPH+5,0                                                     
         BE    VR594U                                                           
         L     RF,=A(VWTYPTB)                                                   
         A     RF,RELO                                                          
         USING VWTYPTBD,RF                                                      
VR594T1  CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID VIEWING TYPE FROM PC                 
         CLC   PGAVTYP,VWPC                                                     
         BE    *+12                                                             
         LA    RF,VWTYPTBL(RF)                                                  
         B     VR594T1                                                          
         MVC   NPG2VTYP,VWMF                                                    
*                                                                               
VR594U   DS    0X                                                               
         B     VR610                                                            
*                                                                               
VR594X   DS    0H                   'OPTIONS' FIELD ON FACPAK SCREEN            
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
         MVC   NPGTRFAX,22(R4)                                                  
         B     VR600                                                            
*                                                                               
VR595A   CLC   12(6,R4),=CL6'WINDOW'                                            
         BNE   VR595E                                                           
         OI    NPGSTATB,X'80'      SET WINDOW FLAG                              
         CLI   1(R4),0             CHECK WINDOW DISPLACEMENT                    
         BE    VR600                                                            
         TM    3(R4),X'80'         MUST BE NUMERIC                              
         BZ    INVERR                                                           
* NOTE: DEIS SAYS NEXT COMMENT IS WRONG: WINDOW MUST BE <= 100 !)               
         CLC   8(4,R4),=F'100'     MUST BE LESS THEN 100                        
         BH    INVERR                                                           
         MVC   NPWINNUM,11(R4)     SAVE TO RECORD                               
         B     VR600                                                            
*                                                                               
VR595E   CLC   12(2,R4),=C'SD'     SUB-DAYPART                                  
         BNE   VR595G                                                           
         MVC   NPGSDPT,22(R4)                                                   
         B     VR600                                                            
*                                                                               
VR595G   DS    0H                                                               
         NI    NPGSTATB,X'FF'-TCAR5                                             
         CLC   12(3,R4),=C'WB1'                                                 
         BNE   VR595H10                                                         
         L     RE,=A(TCARTAB)                                                   
         A     RE,RELO                                                          
VR595H   CLI   0(RE),X'FF'                                                      
         BE    INVERR                                                           
         MVC   NPGTCAR,22(R4)                                                   
         CLC   0(1,RE),22(R4)                                                   
         BE    VR600                                                            
         LA    RE,1(RE)                                                         
         B     VR595H                                                           
*                                                                               
VR595H10 CLC   12(4,R4),=C'TCAR'                                                
         BNE   VR595J                                                           
         L     RE,=A(TCARTAB2)                                                  
         A     RE,RELO                                                          
VR595H20 CLI   0(RE),X'FF'                                                      
         BE    INVERR                                                           
         OI    NPGSTATB,TCAR5                                                   
         MVC   NPGTCAR,22(R4)                                                   
         CLC   0(1,RE),22(R4)                                                   
         BE    VR600                                                            
         LA    RE,1(RE)                                                         
         B     VR595H20                                                         
*                                                                               
VR595J   CLC   12(2,R4),=C'MR'     MULTI-RUN                                    
         BNE   VR595M                                                           
         TM    3(R4),X'80'         TEST NUMERIC                                 
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
*                                                                               
VR595M   CLI   12(R4),C'M'                                                      
         BNE   VR595O                                                           
         L     RE,=A(MIRTAB)                                                    
         A     RE,RELO                                                          
         USING MIRTABD,RE                                                       
VR595N   CLI   0(RE),X'FF'                                                      
         BE    INVERR                                                           
         MVC   NPGMIRCD,22(R4)                                                  
         CLC   MIRCODE,22(R4)                                                   
         BE    VR600                                                            
         LA    RE,MIRTABL(RE)                                                   
         B     VR595N                                                           
         DROP  RE                                                               
*                                                                               
VR595O   CLC   12(4,R4),=C'TIER'                                                
         BNE   VR595P                                                           
         CLI   1(R4),1                                                          
         BH    INVERR                                                           
         MVC   NPTIER,22(R4)                                                    
         CLI   22(R4),C'1'                                                      
         BL    INVERR                                                           
         CLI   22(R4),C'4'                                                      
         BH    INVERR                                                           
         B     VR600                                                            
*                                                                               
VR595P   CLC   12(3,R4),=C'RCP'                                                 
         BNE   VR595Q                                                           
         CLI   NPGACT,C'A'          FIELD CANNOT BE CHANGED AFTER ADD           
         BNE   VR600                                                            
         CLI   1(R4),1                                                          
         BH    INVERR                                                           
         CLI   22(R4),C'N'                                                      
         BE    VR600                                                            
         OI    NPGSTATB,RCPREC                                                  
         CLI   22(R4),C'Y'                                                      
         BNE   INVERR                                                           
         B     VR600                                                            
*                                                                               
VR595Q   CLC   12(3,R4),=C'PVS'                                                 
         BNE   INVERR                                                           
         CLI   22(R4),NPG2SPGM                                                  
         BE    VR595Q1                                                          
         CLI   22(R4),NPG2SCOM                                                  
         BE    VR595Q1                                                          
         CLI   22(R4),NPG2SAPG                                                  
         BE    VR595Q1                                                          
         CLI   22(R4),NPG2STP                                                   
         BNE   INVERR                                                           
VR595Q1  MVC   NPG2SRC,22(R4)                                                   
         L     RF,=A(VWTYPTB)                                                   
         A     RF,RELO                                                          
         USING VWTYPTBD,RF                                                      
VR595Q2  CLI   0(RF),X'FF'                                                      
         BE    INVERR                                                           
         CLC   VWMF,23(R4)                                                      
         BE    *+12                                                             
         LA    RF,VWTYPTBL(RF)                                                  
         B     VR595Q2                                                          
         MVC   NPG2VTYP,23(R4)                                                  
         DROP  RF                                                               
         B     VR600                                                            
*                                                                               
VR600    LA    R4,42(R4)                                                        
         BCT   R5,VR595                                                         
************                                                                    
VR610    DS    0H                                                               
         CLI   RECNUM,60           VALIDATE VPH FIELDS USING...                 
         BNE   VR615               ...FACPAK SCREEN                             
         BRAS  RE,PROCDEMO         NO: IT'S AUDIENCE ESTIMATOR                  
         TM    AEFLAGS,AEFCSOQ     COMSCORE ONLY PROGRAM?                       
         BZ    VR870               GO ADD DEMO ELEMENT                          
         BAS   RE,PUTEL            ADD '93' ELEMENT                             
         J     VR72D                                                            
*                                                                               
VR615    DS    0H                                                               
*                                                                               
VR617    LA    R2,PGMW1H           FIRST VPH FIELD                              
         LA    R5,MAXVPHF          MAX VPH FIELDS                               
         L     R4,=A(DISPTAB)                                                   
         A     R4,RELO                                                          
*                                                                               
VR620    SR    R0,R0                                                            
         CLI   5(R2),0                                                          
         BE    VR700               INPUT NOT REQ NOW                            
*                                                                               
         ZIC   R6,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R6)                                          
         CLI   DMCB,0                                                           
         BNE   INVERR                                                           
         L     R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BL    INVERR              CAN'T BE NEGATIVE                            
*                                                                               
         CLI   2(R4),C'R'          VALIDATE RATING INPUT                        
         BNE   VR680                                                            
         CHI   R0,10000                                                         
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
         BZ    VR710               NO EXTENSION: CAN'T BE ADULT FIELD           
         ZIC   RF,0(R2)                                                         
         AR    RF,R2               POINT RF TO NEXT FIELD                       
         SHI   RF,8                BACK UP TO START OF FIELD EXTENSION          
         CLI   0(RF),1             ID NUMBER 1 = ADULT                          
         BNE   VR710               IT'S SOME OTHER ID NUMBER                    
         TM    4(R2),X'20'         WAS FIELD PREVIOUSLY VALIDATED               
         BZ    VR710               NO                                           
         OC    0(2,R6),0(R6)       SHOULD FIELD BE RECALCULATED                 
         BZ    VR710               NO                                           
         SR    R0,R0               ZERO OUT ADULT FIELD                         
VR710    STH   R0,0(R6)                                                         
*                                                                               
*--SET FLAG IN ADULT FIELD IF IT MUST BE RECALCULATED                           
         TM    4(R2),X'20'         WAS FIELD PREVIOUSLY VALIDATED               
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
         BNE   VR810               3 WAS INPUT NO  CALC                         
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         LH    R2,0(R6)            3 = 1+2                                      
         LH    R1,2(R6)                                                         
         CLI   N0PROF+6,C'N'       CHECK IF TOTAL REQUIRED                      
         BNE   VR795                                                            
         LTR   R2,R2               IS FIRST CELL ZERO                           
         BZ    VR810               YES DON'T TOTAL                              
         LTR   R1,R1               IS SECOND CELL ZERO                          
         BZ    VR810               YES DONT TOTAL                               
VR795    AR    R2,R1                                                            
         LA    R6,NPG2VPHS+78                                                   
         STH   R2,0(R6)                                                         
*                                                                               
*    DO 9-14                                                                    
VR810    CLC   NPG2VPHS+106(2),=XL2'0000'                                       
         BNE   VR870               3 WAS INPUT NO  CALC                         
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         ICM   R2,3,NPG2VPHS+108   3 = 1+2                                      
         ICM   R1,3,NPG2VPHS+48                                                 
         CLI   N0PROF+6,C'N'       CHECK IF TOTAL REQUIRED                      
         BNE   VR815                                                            
         LTR   R2,R2               IS FIRST CELL ZERO                           
         BZ    VR870               YES DON'T TOTAL                              
         LTR   R1,R1               IS SECOND CELL ZERO                          
         BZ    VR870               YES DONT TOTAL                               
VR815    AR    R2,R1                                                            
         LA    R6,NPG2VPHS+106                                                  
         STH   R2,0(R6)                                                         
*                                                                               
* M12-14                                                                        
*                                                                               
VR870    LA    R2,PGMVPH2H                                                      
         CLI   5(R2),0                                                          
         JE    VR872                                                            
*                                                                               
         ZIC   R6,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R6)                                          
         CLI   DMCB,0                                                           
         BNE   INVERR                                                           
         L     R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BL    INVERR              CAN'T BE NEGATIVE                            
*                                                                               
         CVD   R0,DUB              VALIDATE VPH INPUT                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET ZERO REMAINDER                      
         BNE   INVERR              NO DECIMAL                                   
*                                                                               
         MVC   WORK(6),DUB                                                      
         ZAP   DUB,WORK(6)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,NPG2VPH2                                                    
*                                                                               
VR872    LA    R3,24(R7)                                                        
         MVI   ELCODE,X'93'                                                     
         BAS   RE,DELEL                                                         
*&&DO                                                                           
         CLC   AGENCY,=C'SJ'       SJR                                          
         BNE   *+8                                                              
         BRAS  RE,MMDEMOS          VALIDATE NEW MEDIAMATH DEMOS                 
*&&                                                                             
         BRAS  RE,AEMFCHG          IS THIS AN AE-ORIG RECORD?                   
         BNE   VR72                                                             
         MVI   BYTE,DDAE           DELETE ALL AE OVERRIDES                      
         BRAS  RE,DELDD                                                         
*                                                                               
VR72     BAS   RE,PUTEL            ADD '93' ELEMENT                             
*                                  ADD BUILDING BLOCK ELEMENTS                  
         CLI   RECNUM,60           FOR AUDIENCE ESTIMATOR ADD/CHANGE            
         BNE   VR890                                                            
*                                                                               
         MVI   ELCODE,NPGA1CDQ     DELETE EXISTENT BLD BLOCK ELEMENT            
         BAS   RE,DELEL                                                         
         MVI   ELCODE,NPGA2CDQ     GAA BUILDING BLOCKS TOO                      
         BAS   RE,DELEL                                                         
*                                                                               
DEMIMPEQ EQU   X'41'               IMPRESSIONS ELEMENT ON DEMO RECORD           
DEMGAIEQ EQU   X'55'               GAA IMPRESSIONS ELEMENT                      
DEMUNVEQ EQU   X'49'               UNIVERSES ELEMENT ON DEMO RECORD             
         L     R6,AIO3             CREATE IMPRESSIONS BLD BLOCK ELEMENT         
         LA    R6,2(R6)            POINT TO FIRST ELEMENT                       
         MVI   ELCODE,DEMIMPEQ                                                  
         BRAS  RE,FIRSTEL                                                       
         BE    VR72A                                                            
         L     R6,AIO3             NO IMPRESSIONS ELEM. SHOULD BE GAA           
         LA    R6,2(R6)            RE-POINT TO FIRST ELEMENT                    
         MVI   ELCODE,DEMGAIEQ                                                  
         BRAS  RE,FIRSTEL                                                       
         BE    VR72A                                                            
         DC    H'0'                                                             
VR72A    XC    ELEM,ELEM                                                        
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
         CLI   ELEM,DEMGAIEQ       CHANGE TO ELEMNT CODE OF PROG RECORD         
         BE    VR72B                                                            
         MVI   ELEM,NPGA1CDQ                                                    
         MVI   ELCODE,NPGA1CDQ                                                  
         B     VR72C                                                            
VR72B    MVI   ELEM,NPGA2CDQ                                                    
         MVI   ELCODE,NPGA2CDQ                                                  
VR72C    DS    0X                                                               
*                                                                               
         BAS   RE,PUTEL            ADD NEW BUILDING BLOCK ELEMENT               
*                                                                               
         L     R6,AIO3             CREATE UNIVERSE BLD BLOCK ELEMENT            
         LA    R6,2(R6)            POINT TO FIRST ELEMENT                       
         MVI   ELCODE,DEMUNVEQ                                                  
         BRAS  RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    ELEM,ELEM                                                        
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
         MVI   ELEM,NPGA9CDQ       CHANGE TO ELEMNT CODE OF PROG RECORD         
*                                                                               
         MVI   ELCODE,NPGA9CDQ                                                  
         BAS   RE,DELEL            DELETE EXISTENT BLD BLOCK ELEMENT            
         BAS   RE,PUTEL            AND CREATE NEW ONE                           
*                                                                               
VR72D    BRAS  RE,PROCOV           PROCESS OVERRIDES AS 'DD' ELEMENTS           
*                                                                               
         BRAS  RE,AEINFEL          CREATE AUDIENCE ESTIMATOR INFO ELEM          
*                                                                               
* CHK FOR USER DEMO VPHS                                                        
VR890    LA    R3,24(R7)                                                        
         MVI   ELCODE,X'C3'                                                     
         BAS   RE,NXTEL                                                         
         BNE   VR900                                                            
*                                                                               
         BAS   R3,PUTEL                                                         
         B     VR890                                                            
*                                                                               
VR900    LA    R3,24(R7)                                                        
         MVI   ELCODE,X'C3'                                                     
         BAS   RE,DELEL                                                         
*                                                                               
VR910    DS    0H                                                               
         LA    R2,PGMUVPHH         CHK FOR USER DEMO UNIVS                      
         CLI   RECNUM,60                                                        
         BNE   *+8                                                              
         LA    R2,PGAUVPHH         SAME FIELD IN AE SCREEN                      
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
*!!!     MVC   ELEM(2),=X'C30D'                                                 
         MVI   ELEM,X'C3'                                                       
         MVI   ELEM+1,X'0D'                                                     
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
         DROP  R3,R5                                                            
         EJECT                                                                  
GOAK     DS    0H                                                               
         GOTOR OPPFRTN,DMCB,(4,DUB),(R9),(RA),(RC)         AK                   
         B     EXIT                                                             
         EJECT                                                                  
RK       DS    0H                                                               
         GOTOR OPPFRTN,DMCB,(5,DUB),(R9),(RA),(RC)         RK                   
         B     EXIT                                                             
         EJECT                                                                  
*&&DO                                                                           
OTHEREL  NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'        SECONDARY DATA ELEMENT                       
         BRAS  RE,GETEL                                                         
         BE    OTHEREX                                                          
         XC    ELEM(40),ELEM                                                    
         MVC   ELEM(2),=XL2'0328'                                               
         BAS   RE,PUTEL                                                         
OTHEREX  B     EXIT                                                             
*&&                                                                             
*--CHECK IF UNIT CABLE UPLOADED                                                 
*&&DO                                                                           
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
*&&                                                                             
         EJECT                                                                  
         DROP  R6                                                               
*********************************************************************           
LR       DS    0H                                                               
         GOTOR OPPFRTN,DMCB,(6,DUB),(R9),(RA),(RC)          LR                  
         B     EXIT                                                             
         EJECT                                                                  
NXTEL    DS    0H                                                               
         ZIC   R0,1(R3)                                                         
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
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)   * EXECUTED *                                       
         CVB   R0,DUB                                                           
PACKX    BR    RE                                                               
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
INVAERR  MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
TRAPERR  BRAS  RE,AEFLDNO          (AE) SEND FIELD NUMBER TO PC                 
         GOTO1 ERREX                                                            
         LTORG                                                                  
         EJECT                                                                  
                                                                                
         DROP  R8,RB                                                            
*********************************************************************           
*        SET SCREEN FOR COMSCORE                                                
*********************************************************************           
SETSCRN  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTLIST                                                   
         JE    EXIT                                                             
*                                                                               
         MVC   PGMNCS,=C'NTI NUM'                                               
         OI    PGMCSNH+1,X'20'     MARK PROTECTED                               
         XC    PGMCDEN,PGMCDEN                                                  
*                                                                               
         TM    USRIDFLG,USRRNTKQ   USER HAS ACCESS TO COMSCORE?                 
         JZ    SSCRNX                                                           
         MVC   PGMNCS,=C'NTI/CSN'                                               
         NI    PGMCSNH+1,X'FF'-X'20' TURN OFF PROTECT BIT                       
         MVC   PGMCDEN,=C'COMDEF'                                               
*                                                                               
SSCRNX   FOUT  PGMNCSH                                                          
         FOUT  PGMCSNH                                                          
         FOUT  PGMCDENH                                                         
         J     EXIT                                                             
         LTORG                                                                  
*********************************************************************           
*        DISPLAY 03 ELEMENT DATA                                                
*********************************************************************           
         USING NPGEL03,R6                                                       
DISP03   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    PGMCVTY,PGMCVTY                                                  
         FOUT  PGMCVTYH                                                         
         MVC   PGMNCDE(6),NPGNADDM NAD CODE                                     
         MVC   PGMCNT(2),NPPRGRAT  PROGRAM CONTENT                              
         FOUT  PGMCNTH                                                          
         MVC   PGMNEW(1),NPPRNEW   PROGRAM STATUS                               
         FOUT  PGMNEWH                                                          
         MVC   PGMTYP(2),NPPRGTYP  PROGRAM TYPE                                 
         FOUT  PGMTYPH                                                          
         MVC   PGMSTYP(4),NPPRGSTP  SUB PROGRAM TYPE                            
         FOUT  PGMSTYPH                                                         
*                                                                               
         CLI   NPGLEN3,NPG3LNQ2                                                 
         BL    D0312                                                            
         OC    NPGCDEF,NPGCDEF     ANY COMDEF?                                  
         JZ    *+10                                                             
         MVC   PGMCDEF,NPGCDEF                                                  
         FOUT  PGMCDEFH                                                         
         OC    NPGCSN,NPGCSN       ANY COMSCORE SERIES #?                       
         JZ    *+10                                                             
         MVC   PGMCSN,NPGCSN                                                    
         FOUT  PGMCSNH                                                          
         OC    NPGCSVT,NPGCSVT     ANY COMSCORE VIEWING TYPE?                   
         JZ    D0312                                                            
         MVC   PGMCVTY,=C'RL'      DEFAULT TO LIVE                              
         CLI   NPGCSVT,NPCVTRCQ                                                 
         JNE   *+10                                                             
         MVC   PGMCVTY,=C'RC'      LIVE COMMERCIAL?                             
         CLI   NPGCSVT,NPCVTR3Q                                                 
         JNE   *+10                                                             
         MVC   PGMCVTY,=C'R3'      LIVE + 3?                                    
         CLI   NPGCSVT,NPCVTR7Q                                                 
         JNE   *+10                                                             
         MVC   PGMCVTY,=C'R7'      LIVE + 7                                     
*                                                                               
*--OTHER FIELD DISPLAY                                                          
D0312    XC    BYTE,BYTE                                                        
         LA    R2,PGMOTH                                                        
         OC    8(12,R6),8(R6)      FAX NUMBER                                   
         BZ    D0314                                                            
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(2,R2),=C'F='                                                   
         MVC   2(12,R2),8(R6)                                                   
         BAS   RE,SET03R2                                                       
*                                                                               
D0314    CLI   21(R6),0         MIRROR CODE                                     
         BE    D0316                                                            
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(2,R2),=C'M='                                                   
         MVC   2(1,R2),21(R6)                                                   
         BAS   RE,SET03R2                                                       
*                                                                               
D0316    TM    20(R6),X'80'                                                     
         BZ    D0320                                                            
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(6,R2),=CL6'WINDOW'                                             
         CLI   NPWINNUM,0                                                       
         BE    D0318                                                            
         MVI   6(R2),C'='                                                       
         EDIT  (1,NPWINNUM),(3,7(R2)),ALIGN=LEFT                                
D0318    BAS   RE,SET03R2                                                       
*                                                                               
D0320    OC    NPGSDPT,NPGSDPT     SUB-DAYPART                                  
         BZ    D0322                                                            
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(3,R2),=C'SD='                                                  
         MVC   3(3,R2),NPGSDPT                                                  
         BAS   RE,SET03R2                                                       
*                                                                               
D0322    OC    NPGTCAR,NPGTCAR     TCAR LEVEL                                   
         BZ    D0326                                                            
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         TM    NPGSTATB,TCAR5                                                   
         BO    D0324                                                            
         MVC   0(4,R2),=C'WB1='                                                 
         MVC   4(1,R2),NPGTCAR                                                  
         BAS   RE,SET03R2                                                       
         B     D0326                                                            
*                                                                               
D0324    MVC   0(5,R2),=C'TCAR='                                                
         MVC   5(1,R2),NPGTCAR                                                  
         BAS   RE,SET03R2                                                       
*                                                                               
D0326    OC    NPGMRUN,NPGMRUN     MULTI-RUN                                    
         BZ    D0328                                                            
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(3,R2),=C'MR='                                                  
         LA    R2,3(R2)                                                         
         EDIT  (1,NPGMRUN),(2,(R2)),ALIGN=LEFT                                  
         BAS   RE,SET03R2                                                       
*                                                                               
D0328    OC    NPTIER,NPTIER       TIER                                         
         BZ    D0330                                                            
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(5,R2),=C'TIER='                                                
         MVC   5(1,R2),NPTIER                                                   
         BAS   RE,SET03R2                                                       
*                                                                               
D0330    TM    NPGSTATB,RCPREC     CPR (CROSS PLATFORM RECORD)                  
         BZ    DISP03X                                                          
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(5,R2),=C'RCP=Y'                                                
         BAS   RE,SET03R2                                                       
*                                                                               
DISP03X  J     EXIT                                                             
*                                                                               
*--POINT R2 TO NEXT POSITION IN THE "OTHER FIELD"                               
SET03R2  MVI   BYTE,C'Y'                                                        
         LA    R2,PGMOTH                                                        
         LA    RF,48                                                            
*                                                                               
SET03R2A CLI   0(R2),X'40'                                                      
         BNH   SET03R2X                                                         
         LA    R2,1(R2)                                                         
         BCT   RF,SET03R2A                                                      
SET03R2X BR    RE                                                               
         DROP  R6                                                               
         LTORG                                                                  
*********************************************************************           
*        CREATE AE INFO ELEMENT                                                 
*********************************************************************           
AEINFEL  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ELEM             CREATE AUDIENCE ESTIMATOR INFO ELEM          
         USING NPGAEIFD,R4                                                      
         MVI   NPGAICD,NPGAICDQ                                                 
         MVI   NPGAILN,NPGAEILM                                                 
         MVC   NPGAITID,PGATBID                                                 
         MVI   NPGAIFLG,0          BRAND NEW ELEMENT. RESET FLAG                
         CLI   GAAFLAG,GAAYESQ     GAA BASED PROGRAM RECORD                     
         BNE   *+8                                                              
         OI    NPGAIFLG,NPGAIGAQ                                                
                                                                                
         MVI   NPGAIMET,0          SAVE METHODOLOGY INDICATOR                   
         CLI   PGAMETHH+5,0                                                     
         BE    AEINF10                                                          
         MVC   NPGAIMET,PGAMETH                                                 
                                                                                
AEINF10  XC    NPGAHUT,NPGAHUT     SAVE THE HUT VALUE                           
         SR    R1,R1                                                            
         ICM   R1,1,PGAHUTH+5                                                   
         BZ    AEINF20                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,PGAHUT(0)                                                    
         CVB   RF,DUB                                                           
         STCM  RF,15,NPGAHUT                                                    
                                                                                
AEINF20  DS    0X                                                               
                                                                                
         DROP  R4                                                               
                                                                                
         MVI   ELCODE,NPGAICDQ     ADD AE ELEMENT TO PROG RECORD                
         BRAS  RE,DELEL            DELETE EXISTENT ELEMENT                      
         BRAS  RE,PUTEL            AND CREATE NEW ONE                           
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* PROCESS OVERRIDE DEMOS                                                        
* UPON ENTRY: LSTTIAE HAS ADDRESS OF LAST ELEMENT IN TIA                        
* UPON EXIT: IF OVERRIDES HAVE BEEN SENT BY PC, THEN THEY'RE ADDED              
*            TO THE PROGRAM RECORD AS 'DD' ELEMENTS                             
*********************************************************************           
PROCOV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   BYTE,DDAE+DDNAD+DDCDEF   DEL AE-OVERRIDES, NAD, COMSCORE         
         BRAS  RE,DELDD                 DEMOS                                   
*                                                                               
         L     R3,LSTTIAE                                                       
         USING LQ_D,R3                                                          
PROCO05  CLI   LQ_EL,LQ_DLDDQ      IS THIS THE DOWNLOAD DATA ELEMENT?           
         BE    PROCOX              YES: WE'RE DONE                              
*                                                                               
         BRAS  RE,BMPFEF4          BUMP PAST 'FEF4'                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DMCB,OVINDQ                                                      
*                                                                               
         CLI   LQ_VALUE,130        COMSCORE PROGRAMS - SKIP                     
         BNE   PROCO07                                                          
         BRAS  RE,PROCCSP          ADD COMSCORE PROGRAM TO RECORD               
         L     R3,ANXTMAP                                                       
         B     PROCO05                                                          
*                                                                               
PROCO07  BRAS  RE,GETMODF          HALF=OVERRIDE MODIFIER                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVMODIF,HALF+1                                                   
*                                                                               
PROCO10  CLI   LQ_EL,LQ_DLDDQ      PROTECTIVE CODE UNTIL PC FIGURES OUT         
         BE    PROCOX              WHY 'OV' NOT FOLLOWED BY VALUES              
         BRAS  RE,BMPFEF4          BUMP PAST 'FEF4'                             
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,GETODEMS         GET OVERRIDE DEMOS CATGS INTO AIO3           
*                                                                               
         BRAS  RE,BMPFEF4          BUMP PAST 'FEF4'                             
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,GETDVALS         GET OVERRIDE DEMOS VALUES INTO AIO2          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO2                                                          
         L     R4,AIO3                                                          
*                                                                               
         TM    AEFLAGS,AEFCSQ      COMSCORE DEMOS?                              
         JNZ   PROCO40                                                          
*                                                                               
         USING NPGELDD,RF                                                       
PROCO20  OC    0(3,R4),0(R4)                                                    
         BZ    PROCO50                                                          
         LA    RF,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   NPGDEMEL,EDDOVER                                                 
         MVI   NPGDELEN,12                                                      
         MVC   NPGDCAT,0(R4)       NAD CATEGORY                                 
         MVC   NPGDMOD,SVMODIF     MODIFIER                                     
         CLI   NPGDMOD,GAAVPHQ     GAA VPH OVERRIDES SHOULD GO AS VPHS          
         BNE   *+8                                                              
         MVI   NPGDMOD,VPHINDQ                                                  
         CLI   NPGDMOD,VPHINDQ     FOR NOW ONLY VPH'S                           
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    NPGDFLG,NPGDNADQ    DEMO VALUE SET                               
         OC    NPGDCAT,NPGDCAT     NAD DEMO ISN'T AE OVERRIDE                   
         BNZ   *+8                                                              
         OI    NPGDFLG,NPGDAEQ     AE OVERRIDE                                  
         MVI   NPGDPRE,X'40'       PRECISION                                    
         MVC   NPGDAMT,0(R2)                                                    
*                                                                               
         MVC   HALF,1(R4)          TYPE-4 DEMO NUMBER                           
         MVI   DMCB,CONV4TO3                                                    
         BRAS  RE,DTYPCONV                                                      
         MVC   NPGDNUM,HALF        TYPE-3 DEMO NUMBER                           
*                                                                               
         BRAS  RE,PUTEL            ADD THIS OVERRIDE ELEMENT                    
         LA    R2,4(R2)            NEXT DEMO VALUE                              
         LA    R4,3(R4)            NEXT DEMO CATEGORY                           
         B     PROCO20                                                          
*                                                                               
         USING NPGCELDD,RF                                                      
PROCO40  OC    0(L'NPGCCAT,R4),0(R4)                                            
         BZ    PROCO50                                                          
         LA    RF,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   NPGCEMEL,NPGCELQ                                                 
         MVI   NPGCELEN,NPGCLNQ                                                 
         MVC   NPGCCAT,0(R4)       DEMO CATEGORY                                
         OI    NPGCFLG,NPGCNADQ    DEMO VALUE SET                               
         OI    NPGCFLG,NPGCAEQ     AE OVERRIDE                                  
         MVC   NPGCAMT,0(R2)       DEMO AMOUNT                                  
*                                                                               
         BRAS  RE,PUTEL            ADD THIS OVERRIDE ELEMENT                    
         LA    R2,4(R2)            NEXT DEMO VALUE                              
         AHI   R4,L'NPGCCAT        NEXT DEMO CATEGORY                           
         B     PROCO40                                                          
*                                                                               
PROCO50  B     PROCO05                                                          
*                                                                               
PROCOX   J     EXIT                                                             
         DROP  R3,RF                                                            
         EJECT                                                                  
*********************************************************************           
* PROCESS COMSCORE PROGRAMS                                                     
* UPON ENTRY: LSTTIAE HAS ADDRESS OF LAST ELEMENT IN TIA                        
*********************************************************************           
PROCCSP  NTR1  BASE=*,LABEL=*                                                   
         USING LQ_D,R3                                                          
*                                                                               
         XC    ANXTMAP,ANXTMAP                                                  
*                                                                               
PROCC10  CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA?                                
         BNE   PROCCSPX                                                         
*                                                                               
         CLI   LQ_VALUE,130        COMSCORE PROGRAM INFORMATION                 
         JNE   PROCC50                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING NPCPELD,R4                                                       
*                                                                               
         MVI   NPCPEL,NPCPELQ                                                   
         MVI   NPCPLEN,NPCPLNQ                                                  
*                                                                               
         SR    R1,R1               NETWORK NUMBER                               
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
         ICM   R1,3,1(R3)          L'DATA                                       
         SHI   R1,6                                                             
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NPCPNET(0),LQ_VALUE                                              
*                                                                               
         SR    R1,R1               SERIES NUMBER                                
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
         ICM   R1,3,1(R3)          L'DATA                                       
         SHI   R1,6                                                             
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NPCPSER(0),LQ_VALUE                                              
*                                                                               
         SR    R1,R1               START DATE                                   
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
         GOTO1 DATVAL,DMCB,(0,LQ_VALUE),(0,WORK)                                
         GOTO1 DATCON,DMCB,(0,WORK),(2,NPCPSDTE)                                
*                                                                               
         SR    R1,R1               END DATE                                     
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
         GOTO1 DATVAL,DMCB,(0,LQ_VALUE),(0,WORK)                                
         GOTO1 DATCON,DMCB,(0,WORK),(2,NPCPEDTE)                                
*                                                                               
         SR    R1,R1               ROTATION                                     
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
         GOTOR SETROTH,DMCB,LQ_VALUE,NPCPROT                                    
*                                                                               
         SR    R1,R1               START/END TIME                               
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
         SR    R5,R5                                                            
         ICM   R5,3,LQ_LN          L'ELEMENT                                    
         SHI   R5,6                L'HEADER                                     
         GOTO1 TIMVAL,DMCB,((R5),LQ_VALUE),WORK                                 
         CLI   DMCB,X'FF'          INVALID TIME                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   NPCPSTIM,WORK       START TIME                                   
         MVC   NPCPETIM,WORK+2     END TIME                                     
*                                                                               
         SR    R1,R1               SOURCE                                       
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
         MVC   NPCVSRCE,LQ_VALUE                                                
*                                                                               
         SR    R1,R1               VIEWING TYPE                                 
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
         MVI   NPCVT,NPCVTRLQ      DEFAULT TO LIVE                              
         CLC   =C'RC',LQ_VALUE                                                  
         JNE   *+8                                                              
         MVI   NPCVT,NPCVTRCQ      LIVE COMMERCIAL                              
         CLC   =C'R3',LQ_VALUE                                                  
         JNE   *+8                                                              
         MVI   NPCVT,NPCVTR3Q      LIVE + 3                                     
         CLC   =C'R7',LQ_VALUE                                                  
         JNE   *+8                                                              
         MVI   NPCVT,NPCVTR7Q      LIVE + 7                                     
*                                                                               
         BRAS  RE,PUTEL            ADD THIS ELEMENT                             
*                                                                               
PROCC50  SR    R1,R1               GET NEXT MAP                                 
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
         B     PROCC10                                                          
*                                                                               
PROCCSPX ST    R3,ANXTMAP                                                       
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SET ROTATION                                                                  
* PARAM 1 = ROTATION (HEX)                                                      
* PARAM 2 = A(OUTPUT IN EBCDIC)                                                 
***********************************************************************         
SETROTH  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            ROTATION (EBCDIC)                            
         L     R3,4(R1)            A(OUTPUT)                                    
*                                                                               
         MVI   0(R3),0                                                          
         CLI   0(R2),C'Y'          MONDAY?                                      
         JNE   *+8                                                              
         OI    0(R3),NPCPMONQ                                                   
         CLI   1(R2),C'Y'          TUESDAY?                                     
         JNE   *+8                                                              
         OI    0(R3),NPCPTUEQ                                                   
         CLI   2(R2),C'Y'          WEDNESDAY?                                   
         JNE   *+8                                                              
         OI    0(R3),NPCPWEDQ                                                   
         CLI   3(R2),C'Y'          THURSDAY?                                    
         JNE   *+8                                                              
         OI    0(R3),NPCPTHUQ                                                   
         CLI   4(R2),C'Y'          FRIDAY?                                      
         JNE   *+8                                                              
         OI    0(R3),NPCPFRIQ                                                   
         CLI   5(R2),C'Y'          SATURDAY?                                    
         JNE   *+8                                                              
         OI    0(R3),NPCPSATQ                                                   
         CLI   6(R2),C'Y'          SUNDAY?                                      
         JNE   *+8                                                              
         OI    0(R3),NPCPSUNQ                                                   
         J     EXIT                                                             
         LTORG                                                                  
*********************************************************************           
*        ADD/UPDATE ACTIVITY ELEMENT                                            
*********************************************************************           
ACTIVITY NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM(10),ELEM                                                    
         MVC   NPGMAINL(2),=X'0108'                                             
         GOTO1 DATCON,DMCB,(5,0),(3,NPGACTD)                                    
         MVC   NPGACT,CONACT       SAVE ACTION                                  
*                                                                               
ACT5     LA    R3,24(R7)                                                        
         MVI   ELCODE,X'5D'                                                     
         BRAS  RE,DELEL                                                         
         XC    ELEM(8),ELEM                                                     
         MVC   ELEM(2),=X'5D07'                                                 
         MVC   ELEM+2(3),=C'EVN'                                                
         MVC   ELEM+5(2),=X'580E'      APR/1988                                 
*                                                                               
         TM    SVAGYFL2,SVAGYFL2_2DP   2 DECIMAL PRECISION                      
         JZ    *+14                                                             
         MVC   ELEM+5(2),=X'5901'                                               
         J     ACT30                                                            
*                                                                               
         TM    SVAGYFL2,SVAGYFL2_BDP   USER DEFINED DEC PRECISION               
         JZ    ACT20                                                            
         CLI   PGMSHR,C'*'             1 DECIMAL SHARE?                         
         JE    ACT20                                                            
         CLC   PGMSHR(2),=C'1R'        IF NOT 1 DECIMAL                         
         JE    ACT20                                                            
*!!!     CLC   PGMSHR(2),=C'2R'        IS IT A SHARE?                           
*!!!     JNE   *+14                                                             
*                                                                               
ACT10    DS    0H                                                               
         MVC   ELEM+5(2),=X'5901'      MUST BE 2                                
         J     ACT30                                                            
*                                                                               
         CLI   ACTNUM,ACTADD           IF IT'S USER DEFINED AND ACTION          
         JNE   ACT20                   ADD, AND A SHARE, THEN IT                
         MVC   ELEM+5(2),=X'5901'      MUST BE 2                                
         J     ACT30                                                            
*                                                                               
ACT20    DS    0H                                                               
         TM    PRECFLAG,PREC2DEC       IF IT WAS 2 DECIMAL BEFORE, THEN         
         JZ    *+10                                                             
         MVC   ELEM+5(2),=X'5901'      IT MUST STAY 2 DECIMAL                   
*                                                                               
ACT30    DS    0H                                                               
         BRAS  RE,PUTEL                                                         
*                                                                               
*        CLI   RECNUM,60                                                        
*        BNE   ACTX                                                             
         L     RF,ACOMFACS         INITIALIZE SECRET BLOCK                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         LH    R5,=Y(SVSECRET-T31CFFD)                                          
         AR    R5,RA                                                            
         GOTO1 (RF),DMCB,('SECPINIT',(R5)),0                                    
         BNE   ACTX                ERRORS WITH SECRET                           
         USING SECD,R5                                                          
                                                                                
         CLI   ACTNUM,ACTADD       EXTENDED ACTIVITY ELEMENT                    
         BE    ACT50                                                            
         LR    R6,R7                                                            
         MVI   ELCODE,NPG0ACDQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   ACT60                                                            
         MVC   ELEM(NPG0ALNQ),0(R6)   START WITH OLD ELEMENT                    
         USING NPGEL0A,ELEM           AND ADD...                                
         MVI   NPG0ALN,NPG0ALNQ       NEW LENGTH                                
         GOTO1 DATCON,DMCB,(5,0),(3,NPG0ALDT)  LAST CHANGE DATE                 
         MVC   NPG0ALPD,SECPID     LAST CHANGE PID                              
         TIME  TU                                                               
         STCM  R0,15,NPG0ATIM      TIME STAMP                                   
         LA    R3,24(R7)                                                        
         MVI   ELCODE,NPG0ACDQ                                                  
         BRAS  RE,DELEL            DELETE OLD EXTENDED ACTIVITY ELEM            
         BRAS  RE,PUTEL            ADD NEW EXTENDED ACTIVITY ELEM               
         B     ACTX                                                             
                                                                                
ACT50    XC    ELEM(NPG0ALNQ),ELEM  CREATE ELEMENT FROM SCRATCH                 
         MVI   NPG0ACD,NPG0ACDQ                                                 
         MVI   NPG0ALN,NPG0ALNQ                                                 
         GOTO1 DATCON,DMCB,(5,0),(3,NPG0ACDT)  CREATION DATE                    
         MVC   NPG0ACPD,SECPID     CREATION PID                                 
         MVC   NPG0ALDT,NPG0ACDT   LAST CHANGE DATE                             
         MVC   NPG0ALPD,NPG0ACPD   LAST CHANGE PID                              
         TIME  TU                                                               
         STCM  R0,15,NPG0ATIM      TIME STAMP                                   
         LA    R3,24(R7)                                                        
         BRAS  RE,PUTEL            ADD NEW EXTENDED ACTIVITY ELEM               
         B     ACTX                                                             
                                                                                
ACT60    XC    ELEM(NPG0ALNQ),ELEM  ELEMENT W/UNKNOWN CREATION DATE/PID         
         MVI   NPG0ACD,NPG0ACDQ                                                 
         MVI   NPG0ALN,NPG0ALNQ                                                 
         XC    NPG0ACDT,NPG0ACDT   UNKNOWN CREATION DATE                        
         XC    NPG0ACPD,NPG0ACPD   UNKNOWN CREATION PID                         
         GOTO1 DATCON,DMCB,(5,0),(3,NPG0ALDT)  LAST CHANGE DATE                 
         MVC   NPG0ALPD,SECPID     LAST CHANGE PID                              
         TIME  TU                                                               
         STCM  R0,15,NPG0ATIM      TIME STAMP                                   
         LA    R3,24(R7)                                                        
         BRAS  RE,PUTEL            ADD NEW EXTENDED ACTIVITY ELEM               
         B     ACTX                                                             
         DROP  R5                                                               
*                                                                               
ACTX     J     EXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*        VALIDATE RATING/SHARE                                                  
*********************************************************************           
VRATSHR  NTR1  BASE=*,LABEL=*                                                   
         L     R7,AIO1                                                          
         USING NPGRECD,R7                                                       
*                                                                               
         LA    R3,ELEM                                                          
         USING NPGELEM,R3                                                       
*                                                                               
         LA    R2,PGMSHRH                                                       
         GOTO1 ANY                                                              
*                                                                               
         ZIC   R5,5(R2)                                                         
         NI    NPGSTAT,X'7F'      SET OFF X'80'                                 
*                                                                               
         LA    R6,8(R2)                                                         
         CLI   0(R6),C'R'          RATING                                       
         JE    VRS50                                                            
*                                                                               
         CLI   0(R6),C'*'          1 DECIMAL SHARE?                             
         JE    VRS20                                                            
         CLC   0(2,R6),=C'1R'                                                   
         JE    VRS30                                                            
         CLC   0(2,R6),=C'2R'                                                   
         JE    VRS30                    MUST BE SHARE                           
*                                                                               
         J     VRS200                                                           
*                                                                               
VRS20    DS    0H                                                               
         LA    R6,1(R6)                                                         
         SHI   R5,1                                                             
         J     VRS200                                                           
*                                                                               
VRS30    DS    0H                                                               
         CLC   0(2,R6),=C'1R'      1 DECIMAL RATING?                            
         JNE   VRS40                                                            
         TM    PRECFLAG,PREC2DEC   IF IT WAS 2 DECIMAL BEFORE, THEN             
         JO    INVERR              IT MUST STAY 2 DECIMAL                       
         J     VRS60                                                            
*                                                                               
VRS40    DS    0H                                                               
         CLC   0(2,R6),=C'2R'      2 DECIMAL RATING?                            
         JE    VRS60                                                            
         J     VRS200              MUST BE SHARE                                
*                                                                               
VRS50    DS    0H                                                               
         TM    SVAGYFL2,SVAGYFL2_BDP    IF USER DEFINED, MUST USE 1R            
         JO    INVERR                   OR 2R                                   
*                                                                               
         CLI   5(R2),2                                                          
         JL    INVERR                                                           
         OI    NPGSTAT,X'80'                                                    
         LA    R6,1(R6)                                                         
         SHI   R5,1                                                             
         J     VRS200                                                           
*                                                                               
VRS60    DS    0H                                                               
         CLI   5(R2),2                                                          
         JL    INVERR                                                           
         OI    NPGSTAT,X'80'                                                    
         LA    R6,2(R6)                                                         
         SHI   R5,2                                                             
         J     VRS200                                                           
*                                                                               
VRS200   DS    0H                                                               
         GOTO1 CASHVAL,DMCB,0(R6),(R5)                                          
         CLI   DMCB,0                                                           
         JNE   INVERR                                                           
         L     R5,DMCB+4                                                        
         CHI   R5,10000            MAX IS 100.00                                
         JH    INVERR                                                           
         LTR   R5,R5                                                            
         JL    INVERR                                                           
         CVD   R5,DUB                                                           
*                                                                               
VRS210   DS    0H                                                               
         CLI   8(R2),C'R'          RATING?                                      
         JNE   VRS220                                                           
         TM    SVAGYFL2,SVAGYFL2_2DP  2 DECIMAL PRECISION?                      
         JZ    VRS300                                                           
         MVC   WORK(6),DUB+2                                                    
         ZAP   DUB,WORK(6)                                                      
         J     VRS310                                                           
*                                                                               
VRS220   DS    0H                                                               
         CLC   8(2,R2),=C'1R'                                                   
         JNE   VRS230                                                           
         TM    SVAGYFL2,SVAGYFL2_2DP   MUST BE 1 DEC PRECISION                  
         JO    INVERR                                                           
         J     VRS300                                                           
*                                                                               
VRS230   DS    0H                                                               
         CLC   8(2,R2),=C'2R'                                                   
         JNE   VRS300              IT'S A SHARE                                 
         TM    SVAGYFL2,SVAGYFL2_2DP   2 DEC PRECISION                          
         JO    VRS235                                                           
         TM    SVAGYFL2,SVAGYFL2_BDP   USER DEFINED?                            
         JZ    INVERR                                                           
*                                                                               
VRS235   MVC   WORK(6),DUB+2                                                    
         ZAP   DUB,WORK(6)                                                      
         J     VRS310                                                           
*                                                                               
VRS300   DS    0H                                                               
         DP    DUB,=P'10'                                                       
         CP    DUB+6(2),=P'0'      MUST GET ZERO REMAINDER                      
         JNE   INVERR                                                           
         MVC   WORK(6),DUB                                                      
         ZAP   DUB,WORK(6)                                                      
*                                                                               
VRS310   DS    0H                                                               
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   NPGSHARE,HALF                                                    
*                                                                               
VRATSHRX DS    0H                                                               
         J     EXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DISPLAY RATING/SHARE                                                   
*              ON ENTRY, R3 = A(92 ELEMENT)                                     
***********************************************************************         
DRATSHR  NTR1  BASE=*,LABEL=*                                                   
         L     R7,AIO1                                                          
         USING NPGRECD,R7                                                       
         USING NPGELEM,R3                                                       
*                                                                               
         XC    RSHRFLD,RSHRFLD                                                  
*                                                                               
         OC    NPGSHARE,NPGSHARE       ANY RATING OR SHARE?                     
         JZ    DRS500                                                           
*                                                                               
         LA    R2,RSHRFLD                                                       
         TM    NPGSTAT,X'80'           RATING?                                  
         JZ    DRS200                  NO - SHARE, DISPLAY 1 DEC PREC.          
*                                                                               
         TM    SVAGYFL2,SVAGYFL2_2DP   2 DEC PRECISION?                         
         JNZ   DRS20                                                            
         TM    SVAGYFL2,SVAGYFL2_BDP   USER DEFINED PRECISION?                  
         JNZ   DRS50                                                            
         MVI   0(R2),C'R'              MUST BE 1 DECIMAL PRECISION              
         LA    R2,1(R2)                                                         
         J     DRS210                                                           
*                                                                               
DRS20    DS    0H                                                               
         TM    SVAGYFL2,SVAGYFL2_2DP   2 DEC PRECISION?                         
         JZ    DRS50                                                            
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)                                                         
         J     DRS220                                                           
*                                                                               
DRS50    DS    0H                                                               
         TM    SVAGYFL2,SVAGYFL2_BDP   2 DEC PRECISION?                         
         JNZ   *+6                                                              
         DC    H'00'                   SOMETHING'S WRONG                        
*                                                                               
         TM    PRECFLAG,PREC1DEC       1 DEC PRECSION?                          
         JZ    DRS60                                                            
         MVC   0(2,R2),=C'1R'                                                   
         LA    R2,2(R2)                                                         
         J     DRS210                                                           
*                                                                               
DRS60    DS    0H                                                               
         TM    PRECFLAG,PREC2DEC       2 DEC PRECISION?                         
         JNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   0(2,R2),=C'2R'                                                   
         LA    R2,2(R2)                                                         
         J     DRS220                                                           
*                                                                               
DRS200   DS    0H                      DISPLAY 1 DECIMAL PRECISION              
         TM    SVAGYFL2,SVAGYFL2_BDP   BOTH PRECISION MODE                      
         JZ    DRS210                                                           
         TM    PRECFLAG,PREC2DEC       2 DEC PRECISION?                         
         JO    *+12                                                             
         MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
DRS210   DS    0H                                                               
         EDIT  (B2,NPGSHARE),(5,0(R2)),1,ALIGN=LEFT                             
         J     DRS500                                                           
*                                                                               
DRS220   DS    0H                      DISPLAY 2 DECIMAL PRECISION              
         EDIT  (B2,NPGSHARE),(5,0(R2)),2,ALIGN=LEFT                             
         J     DRS500                                                           
*                                                                               
DRS500   DS    0H                                                               
*                                                                               
DRATSHRX DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
         LTORG                                                                  
*                                                                               
VWTYPTB  DS    0X                                                               
         DC    C'L',AL1(NPG2VLV)       LIVE                                     
         DC    C'1',AL1(NPG2VSD)       LIVE+SD                                  
         DC    C'0',AL1(NPG2VL1)       LIVE+1                                   
         DC    C'2',AL1(NPG2VL2)       LIVE+2                                   
         DC    C'3',AL1(NPG2VL3)       LIVE+3                                   
         DC    C'7',AL1(NPG2VL7)       LIVE+7                                   
         DC    X'FF'                                                            
*                                                                               
       ++INCLUDE NEPROGTABS                                                     
*                                                                               
DEMTAB1  DS    0H                                                               
       ++INCLUDE NEPROGDEMS                                                     
         EJECT                                                                  
*********************************************************************           
*        VALIDATE NEW MEDIAMATH DEMOS                                           
*********************************************************************           
MMDEMOS  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,ELEM                                                          
*                                                                               
         CLI   0(R3),X'93'                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,5(R3)            POINT TO DEMOS                               
*                                                                               
MMD10    DS    0H                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R4,3,46(R3)         TEENS                                        
         ICM   R5,3,42(R3)         WTEENS                                       
*                                                                               
         SR    R4,R5                                                            
         BM    DERR1                                                            
         ST    R4,B1217            MTEENS                                       
*                                                                               
MMD15    DS    0H                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R4,3,52(R3)         CH2-11                                       
         ICM   R5,3,50(R3)         CH6-11                                       
*                                                                               
         CR    R4,R5               CH2-11 CAN'T BE EQUAL TO CH6-11              
         BE    DERR2A                                                           
*                                                                               
         SR    R4,R5                                                            
         BM    DERR2                                                            
         ST    R4,K0205            CH2-5                                        
*                                                                               
MMD20    DS    0H                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R4,3,46(R3)         TEENS                                        
         ICM   R5,3,42(R3)         WTEENS                                       
         SR    R4,R5                                                            
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,3,2(R3)          M18+                                         
*                                                                               
         AR    R4,R5                                                            
         ST    R4,M1299            M12+                                         
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,2(R3)          M18+                                         
         L     R5,M1299            M12+                                         
         CR    R4,R5               M18+ CAN'T BE < M12+                         
         BNL   DERR3                                                            
*                                                                               
MMD25    DS    0H                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R4,3,26(R3)         M25-54                                       
         ICM   R5,3,38(R3)         M55+                                         
         AR    R4,R5                                                            
         ST    R4,M2599            M25+                                         
*                                                                               
         SR    R5,R5                                                            
         L     R4,M2599            M25+                                         
         ICM   R5,3,2(R3)          M18+                                         
         CR    R4,R5               M25+ CAN'T BE < M18+                         
         BNL   DERR4                                                            
*                                                                               
MMD30    DS    0H                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R4,3,2(R3)          M18+                                         
         ICM   R5,3,8(R3)          M18-34                                       
*                                                                               
         SR    R4,R5                                                            
         BM    DERR6                                                            
         ST    R4,M3599            M35+                                         
*                                                                               
         L     R5,M2599            M25+                                         
         CR    R4,R5               M35+ CAN'T BE < M25+                         
         BNL   DERR5                                                            
*                                                                               
MMD35    DS    0H                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R4,3,2(R3)          M18+                                         
         ICM   R5,3,14(R3)         M18-49                                       
*                                                                               
         SR    R4,R5                                                            
         BM    DERR8                                                            
         ST    R4,M5099            M50+                                         
*                                                                               
         L     R5,M3599            M35+                                         
         CR    R4,R5               M50+ CAN'T BE < M35+                         
         BNL   DERR7                                                            
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,38(R3)         M55+                                         
         L     R5,M5099            M50+                                         
         CR    R4,R5               M55+ CAN'T BE < M50+                         
         BNL   DERR9                                                            
*                                                                               
MMD40    DS    0H                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R4,3,62(R3)         M35-64                                       
         ICM   R5,3,8(R3)          M18-34                                       
         AR    R5,R4                                                            
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,2(R3)          M18+                                         
*                                                                               
         SR    R4,R5                                                            
         BM    DERR11                                                           
         ST    R4,M6599            M65+                                         
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,3,38(R3)         M55+                                         
         CR    R4,R5               M65+ CAN'T BE < M55+                         
         BNL   DERR10                                                           
*                                                                               
MMD45    DS    0H                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R4,3,50(R3)         CH6-11                                       
         ICM   R5,3,46(R3)         TEENS                                        
         AR    R4,R5                                                            
         ST    R4,P0617            P0617                                        
*                                                                               
MMD50    DS    0H                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R4,3,0(R3)          W18+                                         
         ICM   R5,3,42(R3)         WTEENS                                       
         AR    R4,R5                                                            
         ST    R4,W1299            W12+                                         
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,0(R3)          W18+                                         
         L     R5,W1299            W12+                                         
         CR    R4,R5               W18+ CAN'T BE < W12+                         
         BNL   DERR12                                                           
*                                                                               
MMD55    DS    0H                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R4,3,24(R3)         W25-54                                       
         ICM   R5,3,36(R3)         W55+                                         
         AR    R4,R5                                                            
         ST    R4,W2599            W25+                                         
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,3,0(R3)          W18+                                         
         CR    R4,R5               W25+ CAN'T BE < W18+                         
         BNL   DERR13                                                           
*                                                                               
MMD60    DS    0H                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R4,3,0(R3)          W18+                                         
         ICM   R5,3,6(R3)          W18-34                                       
*                                                                               
         SR    R4,R5                                                            
         BM    DERR15                                                           
         ST    R4,W3599            W35+                                         
*                                                                               
         L     R5,W2599            W25+                                         
         CR    R4,R5               W35+ CAN'T BE < W25+                         
         BNL   DERR14                                                           
*                                                                               
MMD65    DS    0H                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R4,3,0(R3)          W18+                                         
         ICM   R5,3,12(R3)         W18-49                                       
*                                                                               
         SR    R4,R5                                                            
         BM    DERR17                                                           
         ST    R4,W5099            W50+                                         
*                                                                               
         L     R5,W3599            W35+                                         
         CR    R4,R5               W50+ CAN'T BE < W35+                         
         BNL   DERR16                                                           
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,36(R3)         W55+                                         
         L     R5,W5099            W50+                                         
         CR    R4,R5               W55+ CAN'T BE < W50+                         
         BNL   DERR18                                                           
*                                                                               
MMD70    DS    0H                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R4,3,60(R3)         W35-64                                       
         ICM   R5,3,6(R3)          W18-34                                       
         AR    R5,R4                                                            
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,0(R3)          W18+                                         
*                                                                               
         SR    R4,R5                                                            
         BM    DERR20                                                           
         ST    R4,W6599            W65+                                         
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,3,36(R3)         W55+                                         
         CR    R4,R5               W65+ CAN'T BE < W55+                         
         BNL   DERR19                                                           
*                                                                               
MMD100   DS    0H                                                               
*                                                                               
MMDEMOSX DS    0H                                                               
         J     EXIT                                                             
*                                                                               
DERR1    DS    0H                                                               
         LA    R2,PGMW11H                                                       
         MVC   ERRNUM,=AL2(DERR1Q)                                              
         B     MMERREX                                                          
*                                                                               
DERR2    DS    0H                                                               
         LA    R2,PGMC1H                                                        
         MVC   ERRNUM,=AL2(DERR2Q)                                              
         B     MMERREX                                                          
*                                                                               
DERR2A   DS    0H                                                               
         LA    R2,PGMC1H                                                        
         MVC   ERRNUM,=AL2(DERR2AQ)                                             
         B     MMERREX                                                          
*                                                                               
DERR3    DS    0H                                                               
         LA    R2,PGMM1H                                                        
         MVC   ERRNUM,=AL2(DERR3Q)                                              
         B     MMERREX                                                          
*                                                                               
DERR4    DS    0H                                                               
         LA    R2,PGMM1H                                                        
         MVC   ERRNUM,=AL2(DERR4Q)                                              
         B     MMERREX                                                          
*                                                                               
DERR5    DS    0H                                                               
         LA    R2,PGMM1H                                                        
         MVC   ERRNUM,=AL2(DERR5Q)                                              
         B     MMERREX                                                          
*                                                                               
DERR6    DS    0H                                                               
         LA    R2,PGMM2H                                                        
         MVC   ERRNUM,=AL2(DERR6Q)                                              
         B     MMERREX                                                          
*                                                                               
DERR7    DS    0H                                                               
         LA    R2,PGMM1H                                                        
         MVC   ERRNUM,=AL2(DERR7Q)                                              
         B     MMERREX                                                          
*                                                                               
DERR8    DS    0H                                                               
         LA    R2,PGMM3H                                                        
         MVC   ERRNUM,=AL2(DERR8Q)                                              
         B     MMERREX                                                          
*                                                                               
DERR9    DS    0H                                                               
         LA    R2,PGMM10H                                                       
         MVC   ERRNUM,=AL2(DERR9Q)                                              
         B     MMERREX                                                          
*                                                                               
DERR10   DS    0H                                                               
         LA    R2,PGMM10H                                                       
         MVC   ERRNUM,=AL2(DERR10Q)                                             
         B     MMERREX                                                          
*                                                                               
DERR11   DS    0H                                                               
         LA    R2,PGMM1H                                                        
         MVC   ERRNUM,=AL2(DERR11Q)                                             
         B     MMERREX                                                          
*                                                                               
DERR12   DS    0H                                                               
         LA    R2,PGMW1H                                                        
         MVC   ERRNUM,=AL2(DERR12Q)                                             
         B     MMERREX                                                          
*                                                                               
DERR13   DS    0H                                                               
         LA    R2,PGMW1H                                                        
         MVC   ERRNUM,=AL2(DERR13Q)                                             
         B     MMERREX                                                          
*                                                                               
DERR14   DS    0H                                                               
         LA    R2,PGMW1H                                                        
         MVC   ERRNUM,=AL2(DERR14Q)                                             
         B     MMERREX                                                          
*                                                                               
DERR15   DS    0H                                                               
         LA    R2,PGMW1H                                                        
         MVC   ERRNUM,=AL2(DERR15Q)                                             
         B     MMERREX                                                          
*                                                                               
DERR16   DS    0H                                                               
         LA    R2,PGMW1H                                                        
         MVC   ERRNUM,=AL2(DERR16Q)                                             
         B     MMERREX                                                          
*                                                                               
DERR17   DS    0H                                                               
         LA    R2,PGMW1H                                                        
         MVC   ERRNUM,=AL2(DERR17Q)                                             
         B     MMERREX                                                          
*                                                                               
DERR18   DS    0H                                                               
         LA    R2,PGMW10H                                                       
         MVC   ERRNUM,=AL2(DERR18Q)                                             
         B     MMERREX                                                          
*                                                                               
DERR19   DS    0H                                                               
         LA    R2,PGMW10H                                                       
         MVC   ERRNUM,=AL2(DERR19Q)                                             
         B     MMERREX                                                          
*                                                                               
DERR20   DS    0H                                                               
         LA    R2,PGMW1H                                                        
         MVC   ERRNUM,=AL2(DERR20Q)                                             
         B     MMERREX                                                          
*                                                                               
MMERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*--SET OPTION AND PF KEY ROUTINES                                               
*                                                                               
OPPFRTN  NMOD1 0,**C13OP*,R6                                                    
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
         CLI   0(R1),5                                                          
         BE    OPBR5                                                            
         CLI   0(R1),6                                                          
         BE    OPBR6                                                            
         B     OPXIT                                                            
*                                                                               
OPBR0    BRAS  RE,SETOPT                                                        
         B     OPXIT                                                            
OPBR1    BAS   RE,PF                                                            
         B     OPXIT                                                            
OPBR2    BAS   RE,PROCPF                                                        
         B     OPXIT                                                            
OPBR3    BAS   RE,VK                                                            
         B     OPXIT                                                            
OPBR4    BAS   RE,AK                                                            
         B     OPXIT                                                            
OPBR5    BAS   RE,RKEY                                                          
         B     OPXIT                                                            
OPBR6    BAS   RE,LROV                                                          
         B     OPXIT                                                            
         EJECT                                                                  
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
*                                                                               
         LA    R2,PGMNETH                                                       
         MVC   BYTE,PFAID                                                       
         SPACE 1                                                                
PROCPFA  CLI   PFAID,PF5           CHANGE DPROG                                 
         BE    PROCPF4                                                          
         CLI   PFAID,PF6           DISPLAY CPROG                                
         BE    PROCPF1                                                          
         CLI   PFAID,PF7           CHANGE CPROG                                 
         BE    PROCPF4                                                          
         CLI   PFAID,PF4           DISPLAY DPROG                                
         BNE   PROCPFX                                                          
         SPACE 1                                                                
PROCPF1  LA    R1,=C'DIS'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         SPACE 1                                                                
PROCPF2  MVI   PFAID,0                                                          
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   PROCPF3                                                          
         CLI   BYTE,PF6                                                         
         JE    PROCPF2A                                                         
         CLI   BYTE,PF7                                                         
         JE    PROCPF2A                                                         
         GOTO1 VCALL,WORK,=C'DPROG',,(4,PGMNET),(6,PGMPGR),(8,PGMEDT),0         
         B     PROCPFX                                                          
PROCPF2A GOTO1 VCALL,WORK,=C'CPROG',,(4,PGMNET),(6,PGMPGR),(8,PGMEDT),0         
         B     PROCPFX                                                          
                                                                                
PROCPF3  DS    0H                                                               
         CLI   BYTE,PF6                                                         
         JE    PROCPF3A                                                         
         CLI   BYTE,PF7                                                         
         JE    PROCPF3A                                                         
         GOTO1 VTRANSF,WORK,=C'DPROG',,(4,PGMNET),(6,PGMPGR),          X        
               (8,PGMEDT),0                                                     
         B     PROCPFX                                                          
PROCPF3A GOTO1 VTRANSF,WORK,=C'CPROG',,(4,PGMNET),(6,PGMPGR),          X        
               (8,PGMEDT),0                                                     
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPF4  LA    R1,=C'CHA'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         B     PROCPF2                                                          
         SPACE 1                                                                
*                                                                               
PROCPFX  B     OPEXIT                                                           
*                                                                               
         EJECT                                                                  
*    VALIDATE KEY                                                               
VK       NTR1                                                                   
         MVI   PRECFLAG,0          RATING PRECISION FLAG                        
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
*                                                                               
         CLC   =C'DIS',CONACT                                                   
         BE    VK100                                                            
         CLC   =C'LIS',CONACT                                                   
         BE    VK100                                                            
         CLC   =C'SEL',CONACT                                                   
         BE    VK100                                                            
*                                                                               
         CLI   RECNUM,60           FOR AUDIENCE ESTIMATOR UPLOAD                
         BNE   VK80                                                             
         GOTO1 VTERMACC,DMCB,,(RA)  CHECK TERMINAL ACCESS                       
         CLC   =C'INVALID ',CONACT INVALID ACTION                               
         BE    INVACTER                                                         
*                                                                               
VK80     CLI   T31CFFD+1,C'*'      DDS ONLY?                                    
         BE    VK100                                                            
         TM    T31CFFD+13,X'10'    RESTRICT ACCESS TO JUST CABLE                
         BZ    VK100                                                            
         CLI   QPTYPE,C'C'         CABLE POSTING TYPE?                          
         BNE   INVACTER                                                         
*                                                                               
VK100    LA    R2,PGMPGRH          * PROGRAM                                    
         GOTO1 VALIFLD                                                          
         BNZ   VK150                                                            
         CLI   NOPTFLG,1                                                        
         BE    VK200                                                            
         MVI   ERROR,INVALID                                                    
         B     OPINVERR                                                         
VK150    MVI   SOFTCHK,0                                                        
         CLI   PGMEDTH+5,0              WAS DATE INPUTTED                       
         BNE   VK180                    YES, FULLY VALIDATE PROGRAM             
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
         GOTO1 DATCON,DMCB,(0,QDATE),(3,DUB)                                    
         CLI   DUB,X'7F'           > 2027?                                      
         JH    OPINVERR                                                         
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
         CLI   RECNUM,60           THIS TWA FIELD DOESN'T EXIST...              
         BE    *+12                ...UNDER AUDIENCE ESTIMATOR                  
         CLI   PGMSPECH+5,0                                                     
         BNE   AKEXT                                                            
         MVC   KEY(13),SVKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   AKEXT                                                            
******   DC    H'0'                SHOULD HAVE BEEN FOUND IN EDIT               
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
         B     TRAPERR2                                                         
*                                                                               
INVACTER MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR2                                                         
*                                                                               
ENDATERR DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(28),=C'* END DATE ERROR - NEXT DATE'                     
         GOTO1 DATCON,DMCB,(2,KEY+11),(5,CONHEAD+31)                            
         FOUT  CONHEADH                                                         
         B     TRAPERR2                                                         
*                                                                               
RKEY     NTR1                                                                   
         L     R7,AIO1                                                          
         LA    R3,24(R7)                                                        
         USING NPGELEM,R3                                                       
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NXTELV2                                                       
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
RKEXT    B     OPEXIT                                                           
         DROP  R3                                                               
*                                                                               
NXTELV2  DS    0H                                                               
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    NXTELV22                                                         
         CLC   ELCODE,0(R3)                                                     
         BER   RE                                                               
         B     NXTELV2                                                          
*                                                                               
NXTELV22 LTR   R3,R3                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*  LIST RECORD ROUTINE                                                          
*                                                                               
LROV     NTR1                                                                   
         BRAS  RE,SETOPT                                                        
*                                                                               
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
*SM                                                                             
* CHECK FAX FILTER                                                              
*                                                                               
         CLI   OPTFAX,C'Y'         DO THEY WANT RECS W/FAX                      
         BNE   LR222                                                            
*                                                                               
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'03'                                                     
         BAS   RE,NXTELV2                                                       
         BNE   LR200               SKIP THIS RECORD                             
         USING NPGEL03,R3                                                       
*                                                                               
         OC    NPGTRFAX,NPGTRFAX   ANY FAX                                      
         BZ    LR200                                                            
*                                                                               
*  CHECK DATE FILTER                                                            
LR222    OC    OPTSPER,OPTSPER                                                  
         BNZ   *+16                                                             
         CLI   OPTFAX,C'Y'         IF THEY WANT RECS W/FAX                      
         BE    LR243               DO NOT CARE ABOUT OTHER FILTERS!!!           
         B     LR230                                                            
*                                                                               
         CLC   OPTSPER,KEY+11                                                   
         BH    LR200                                                            
         CLC   OPTEPER,KEY+11                                                   
         BL    LR200                                                            
*                                                                               
         CLI   OPTFAX,C'Y'         IF THEY WANT RECS W/FAX                      
         BE    LR243               DO NOT CARE ABOUT OTHER FILTERS!!!           
*                                                                               
*  CHECK DAYPART FILTER                                                         
LR230    OC    OPTDAYP,OPTDAYP                                                  
         BZ    LR235                                                            
*                                                                               
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'93'                                                     
         BAS   RE,NXTELV2                                                       
         BNE   LR200                                                            
*                                                                               
         USING NPGEL93,R3                                                       
*                                                                               
         CLC   OPTDAYP,NPG2DYP                                                  
         BNE   LR200                                                            
         DROP  R3                                                               
* CHECK VIEWING SOURCE FILTER (PVS)                                             
LR235    CLI   OPTVWSC,0           FILTER ON PVS                                
         BE    LR240                                                            
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'93'                                                     
         BAS   RE,NXTELV2                                                       
         BNE   LR200                                                            
         USING NPGEL93,R3                                                       
         CLI   OPTVWSC,C'*'        WILD CARD FOR SOURCE                         
         BE    *+14                                                             
         CLC   OPTVWSC,NPG2SRC                                                  
         BNE   LR200               NO MATCH ON SOURCE                           
         CLI   OPTVWST,C'*'                                                     
         BE    *+14                WILD CARD FOR VIEWING TYPE                   
         CLC   OPTVWST,NPG2VTYP                                                 
         BNE   LR200               NO MATCH ON VIEWING TYPE                     
         DROP  R3                                                               
*                                                                               
LR240    CLI   OPTAE,0             FILTER ON AE-GENERATED RECORDS               
         BE    LR242                                                            
         LA    R3,24(R7)                                                        
         MVI   ELCODE,NPGAICDQ                                                  
         BAS   RE,NXTELV2                                                       
         BE    LR240AE                                                          
         CLI   OPTAE,OPTAENQ                                                    
         BNE   LR200                                                            
         B     LR242                                                            
LR240AE  CLI   OPTAE,OPTAEYQ                                                    
         BNE   LR200                                                            
*                                                                               
LR242    CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
LR243    LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
         GOTO1 DATCON,DMCB,(2,NPGKEND),(5,LRDAT)                                
         MVC   LRCODE,NPGKPROG                                                  
*                                                                               
         MVI   PRECFLAG,0          RATING PRECISION FLAG                        
*                                                                               
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'5D'                                                     
         BAS   RE,NXTELV2                                                       
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR MUST HAVE 5D ELEM                
*                                                                               
         CLC   5(2,R3),=X'5901'    2 DECIMAL PRECISION?                         
         BNE   *+12                                                             
         OI    PRECFLAG,PREC2DEC                                                
         B     *+8                                                              
         OI    PRECFLAG,PREC1DEC   1 DECIMAL PRECISION                          
*                                                                               
         LA    R3,24(R7)                                                        
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NXTELV2                                                       
*                                                                               
         BNE   LR200               IF NOT THERE WHY NOT JUST SKIP IT??          
*                                                                               
*        BE    *+6                                                              
*        DC    H'0'                MUST FIND 92 ELEM                            
*                                                                               
         CLI   OPTFAX,C'Y'         FOR FAX FILTER                               
         BE    LR250               NO MULTI FILTERS FOR NOW !!!                 
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
*                                                                               
         CLI   OPTFAX,C'Y'         FOR FAX FILTER                               
         BE    LR280               DO NOT CARE ABOUT OTHER FILTERS!!            
*                                                                               
         CLI   OPTSTER,C'Y'                                                     
         BE    LR280                                                            
         OC    NPGROT,NPGROT                                                    
         BZ    LR280                                                            
         XC    LRDAY,LRDAY                                                      
         GOTO1 UNDAY,DMCB,NPGROT,LRDAY                                          
LR280    GOTO1 UNTIME,DMCB,NPGTIME,LRTIME                                       
*                                                                               
*                                                                               
         CLI   OPTFAX,C'Y'         FOR FAX FILTER                               
         BE    LR290               DO NOT CARE ABOUT OTHER FILTERS!!            
*                                                                               
         CLI   OPTSTER,C'Y'                                                     
         BNE   *+14                                                             
         MVC   LRSHR(3),NPGFILT                                                 
         B     LR320                                                            
*                                                                               
LR290    BRAS  RE,DRATSHR                                                       
         MVC   LRSHR,RSHRFLD                                                    
*                                                                               
LR320    OC    NPGPPNO,NPGPPNO                                                  
         BZ    LR330                                                            
         SR    R0,R0                                                            
         ICM   R0,3,NPGPPNO                                                     
         EDIT  (R0),(5,LRNTI),ALIGN=LEFT,FILL=0                                 
*                                                                               
LR330    LA    R3,24(R7)                                                        
         MVI   ELCODE,NPGAICDQ     AE ELEMENT                                   
         BAS   RE,NXTELV2                                                       
         BNE   LR340               NO AE ELEMENT,IS MF-CREATED RECORD           
         USING NPGAEIFD,R3                                                      
         TM    NPGAIFLG,NPGAIGAQ                                                
         BNO   *+8                                                              
         MVI   LRGAA,C'G'                                                       
*                                                                               
LR340    DS    0X                                                               
*                                                                               
         SPACE                                                                  
LR500    GOTO1 LISTMON                                                          
         B     LR200               GOTO READ SEQ                                
*                                                                               
LREXT    DS    0H                                                               
         B     OPEXIT                                                           
         DROP  R5                                                               
         EJECT                                                                  
* PRINTING THE LINE                                                             
PR       DS    0H                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         SPACE                                                                  
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         MVI   NFILE,C'T'          STATION FILE                                 
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
         BAS   RE,NXTELV2                                                       
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
         BAS   RE,NXTELV2                                                       
         BNE   PR500                                                            
         USING NPG2ELEM,R3                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,NPG2STD),(5,PRDAT)                                
*                                                                               
PR500    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR200                                                            
         SPACE                                                                  
PREXT    DS    0H                                                               
         B     OPEXIT                                                           
         DROP  R3,R4,R6                                                         
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
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
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
         B     OPEXIT                                                           
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
OPEXIT   XIT1                                                                   
*                                                                               
OPXIT    XMOD1                                                                  
         DROP  R8                                                               
*                                                                               
INVERR2  MVI   ERROR,INVALID                                                    
*                                                                               
TRAPERR2 BRAS  RE,AEFLDNO          (AE) SEND FIELD NUMBER TO PC                 
         GOTO1 ERREX                                                            
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* DELETE AE OVERRIDE ELEMENTS AND ELEMENTS WITH NAD DEMOS                       
* ROUTINE USES IO3                                                              
* BYTE(1) = TYPE OF 'DD' ELEMENTS TO DELETE                                     
DDAE     EQU   X'80'     DELETE DD ELEMENTS THAT ARE AE-OVERRIDES               
DDNAD    EQU   X'40'     DELETE DD ELEMENTS THAT ARE NAD DEMOS                  
DDCDEF   EQU   X'20'     DELETE DE ELEMENTS THAT ARE COMSCORE DEMOS             
*********************************************************************           
DELDD    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,AIO3             SAVE ENTIRE RECORD TO AIO3                   
         SR    RF,RF                                                            
         ICM   RF,3,NPGRLEN                                                     
         LR    R1,RF                                                            
         LR    R0,R7                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   ELCODE,EDDOVER                                                   
         BRAS  RE,DELEL            DELETE ALL DD ELEMENTS FROM RECD             
         MVI   ELCODE,NPGCELQ                                                   
         BRAS  RE,DELEL            DELETE ALL DE ELEMENTS FROM RECD             
         MVI   ELCODE,NPCPELQ                                                   
         BRAS  RE,DELEL            DELETE ALL 04 ELEMENTS FROM RECD             
*                                                                               
         L     RE,AIO3             SAVE ELEM AT IO3+1500                        
         LA    RE,1500(RE)                                                      
         LA    RF,L'ELEM                                                        
         LA    R0,ELEM                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R4,AIO3             GO THOUGH ELEMENTS IN SAVED RECD             
         LA    R4,24(R4)                                                        
DELDD10  CLI   0(R4),0                                                          
         BE    DELDDX                                                           
         CLI   0(R4),EDDOVER                                                    
         BE    DELDD15                                                          
         CLI   0(R4),NPGCELQ       COMSCORE?                                    
         BNE   DELDD50                                                          
*                                                                               
         USING NPGCELDD,R4                                                      
         TM    NPGCFLG,NPGCAEQ     CK IF THIS IS AE-OVERRIDE                    
         BNO   DELDD12                                                          
         TM    BYTE,DDAE           DELETE AE-OVERRIDES?                         
         BO    DELDD50             YES. SKIP IT                                 
         B     DELDD40             NO. ADD IT TO RECORD                         
*                                                                               
DELDD12  TM    BYTE,DDCDEF         DELETE NAD DEMOS?                            
         BO    DELDD50             YES. SKIP IT                                 
         B     DELDD40             NO. ADD IT TO RECORD                         
*                                                                               
         USING NPGELDD,R4                                                       
DELDD15  TM    NPGDFLG,NPGDAEQ     CK IF THIS IS AE-OVERRIDE                    
         BNO   DELDD20                                                          
         TM    BYTE,DDAE           DELETE AE-OVERRIDES?                         
         BO    DELDD50             YES. SKIP IT                                 
         B     DELDD40             NO. ADD IT TO RECORD                         
                                                                                
DELDD20  OC    NPGDCAT,NPGDCAT     CK IF THIS IS NAD DEMO                       
         BZ    DELDD40                                                          
         TM    BYTE,DDNAD          DELETE NAD DEMOS?                            
         BO    DELDD50             YES. SKIP IT                                 
         B     DELDD40             NO. ADD IT TO RECORD                         
         DROP  R4                                                               
                                                                                
DELDD40  XC    ELEM,ELEM                                                        
         ZIC   R1,1(R4)            ADD ELEMENT TO ORIG RECORD                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R4)                                                    
         BRAS  RE,PUTEL                                                         
*                                                                               
DELDD50  ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DELDD10                                                          
*                                                                               
DELDDX   L     RE,AIO3             RESTORE ELEM FROM AIO3+1500                  
         LA    RE,1500(RE)                                                      
         LA    RF,L'ELEM                                                        
         LA    R0,ELEM                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         J     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*        CONVERT CHARACTER TO BINARY VERSION NUMBER (AE ONLY)                   
*        INPUT CHARACTER VERSION IS XXX.XXX.XXX.XXX                             
*        OUTPUT VERSION IS BINARY, 1 BYTE FOR EACH GROUPING                     
*********************************************************************           
VERSNUM  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    AEVERSN,AEVERSN                                                  
         CLI   PGAVERSH+5,0        IS PC SENDING VERSION NUMBER?                
         BE    VERSNUMX            NO.                                          
                                                                                
         LA    R3,PGAVERS                                                       
         LA    R5,AEVERSN                                                       
                                                                                
VERS05   SR    R1,R1                                                            
         LR    RE,R3                                                            
VERS10   CLI   0(RE),C'.'                                                       
         BE    VERS20                                                           
         TM    0(RE),X'F0'         NUMERIC?                                     
         BNO   VERS20              NO. END OF STRING. OUTPUT LAST GROUP         
         LA    RE,1(RE)            POINT TO NEXT DIGIT IN THE GROUP             
         LA    R1,1(R1)            UPDATE NO OF DIGITS                          
         B     VERS10                                                           
                                                                                
VERS20   BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,0(R5)            OUTPUT GROUP IN BINARY                       
         CLI   0(RE),C'.'                                                       
         BNE   VERSNUMX            END OF INPUT. DONE                           
         LA    R5,1(R5)                                                         
         LA    R3,1(RE)            POINT TO CHARACTER AFTER '.'                 
         B     VERS05                                                           
                                                                                
VERSNUMX J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*        GET ALET TO BE USED FOR ACCESS REGISTER MODE                           
*********************************************************************           
GETATBS  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    DUB,DUB             SPECIAL CALL FOR SYSFACS                     
         MVC   DUB(4),=XL4'FEFFFFFF'                                            
         GOTO1 SWITCH,DUB                                                       
         L     R1,0(R1)                                                         
         L     R1,VSSB-SYSFACD(R1)                                              
         MVC   ALET,SSBTBLET-SSBD(R1)                                           
         OC    ALET,ALET                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CDEMADDR-COMFACSD(RF)                                         
         MVC   DTABLES(DTABLELQ),TABLESL                                        
         GOTOR (RF),DMCB,(X'FF',DTABLES),ACOMFACS                               
                                                                                
GETATBSX J     EXIT                                                             
         LTORG                                                                  
                                                                                
TABLESL  DC    X'D0',3X'00'        DEMDISP TABLE                                
         DC    X'E7',3X'00'        TABLE OF NEW FORMULAS FOR AE                 
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* FOR AUDIENCE ESTIMATOR,SEND FIELD NUMBER TO PC UNDER MAP CODE #10   *         
* AT ENTRY, R2 POINTS TO FIELD IN ERROR                               *         
***********************************************************************         
                                                                                
AEFLDNO  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   RECNUM,60           TEST REQUEST FROM AE SCREEN/PC               
         BNE   AEFLDNOX                                                         
                                                                                
         USING FLDHDRD,R2                                                       
         TM    FLDATB,FATBXHDR     TEST EXTENDED FIELD HEADER                   
         BNO   AEFLDNOX                                                         
                                                                                
         ZIC   RE,0(R2)                                                         
         AR    RE,R2               POINT RF TO NEXT FIELD                       
         SHI   RE,8                BACK UP TO START OF FIELD EXTENSION          
         MVC   BYTE,0(RE)          FIELD NUMBER                                 
         DROP  R2                                                               
                                                                                
         GOTOR RESLIOB             SET RELEASED FLAG FOR GENNEW                 
                                                                                
         L     R3,ALIOB            SEND FIELD NUMBER                            
         USING LIOBD,R3                                                         
         L     RF,ACOMFACS                                                      
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('LIOAPUT',LIOBD),('LIOTRAW',MAPFLDQ),        *        
               ('LD_CBINQ',BYTE),(L'BYTE,0)                                     
         DROP  R3                                                               
                                                                                
AEFLDNOX J     EXIT                                                             
                                                                                
MAPFLDQ  EQU   10                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS DEMO VALUES FROM AUDIENCE ESTIMATOR                         *         
***********************************************************************         
                                                                                
PROCDEMO NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   WORK(3),PGATBID     TABLE ID (3-BYTE ALPHA)                      
         GOTO1 =V(NUMVAL),DMCB,PGATBID+3,2,RR=RELO                              
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK+4(1),DMCB+7    MONTH (OR WEEK)                              
         MVC   DUB(2),PGATBID+5    YY                                           
         MVC   DUB+2(4),=C'0101'   ANY MONTH/DAY WILL DO                        
         GOTO1 DATCON,DMCB,DUB,(3,FULL)                                         
         MVC   WORK+3(1),FULL      BINARY YEAR                                  
         MVC   AEBOOKYM,WORK+3     SAVE BINARY YEAR/MONTH                       
         XC    WORK+3(2),=X'FFFF'  FLIP BITS FOR DEMDISP TABLE KEY              
                                                                                
         ICM   R3,15,ADEMDISP                                                   
         SR    R1,R1                                                            
RD10     CLC   =XL2'00',0(R3)      END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                YES: BAD TABLE ID FROM PC???                 
         CLC   WORK(5),0(R3)       FIND MATCHING DEMDISP TABLE SEGMENT          
         BE    *+16                R3 = A(DEMDISP TABLE SEGMENT)                
         ICM   R1,7,7(R3)          L'TABLE SEGMENT                              
         LA    R3,1(R1,R3)         BUMP TO NEXT SEGMENT                         
         B     RD10                                                             
         ST    R3,FULL             SAVE ADDRESS                                 
                                                                                
         ICM   R2,15,AAEFORMS      FIND CORRESPONDING FORMULA TABLE             
         BZ    RDX                                                              
         STAR  CLEAR=Y,ARS=ON                                                   
         LAM   AR2,AR2,ALET                                                     
         USING DEMFFMSD,R2                                                      
RD12     CLC   =X'FFFF',0(R2)                                                   
         BNE   *+6                                                              
         DC    H'0'                BAD TABLE ID PASSED                          
         CLC   PGATBID,DEMFFTID                                                 
         BE    RD14                                                             
         ICM   RF,15,DEMFFLQ(R2)   DISPLACEMENT TO NEXT TABLE ID                
         AR    R2,RF                                                            
         B     RD12                                                             
                                                                                
RD14     LA    R2,DEMFFLQ+4(R2)    BUMP PAST THE HEADER AND LENGTH              
         ST    R2,ADEMFORM                                                      
         LAM   AR2,AR2,ARZEROS     CLEAR XR2 IN CASE IN ROUTINES USE R2         
                                                                                
         SR    R3,R3                                                            
         ICM   R3,7,LINK$DTA+1     CONFIRM THAT R3 = A('STOP' ELEMENT)          
         USING LQ_D,R3                                                          
         CLI   LQ_EL,LQ_RQSTQ      MUST BE REQUEST DATA ELEMENT                 
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,LQ_LN          L'ELEMENT                                    
         CHI   R0,10               BETTER BE 10                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   LQ_DCODE,=AL2(3)    DATA CODE MUST BE 3                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   LQ_TYPE,LQ_TSINQ    MUST BE A SINGLE VALUE                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'STOP',LQ_VALUE   MUST BE 'STOP' ELEMENT                       
         BE    *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0               BUMP TO NEXT ELEMENT                         
                                                                                
         TM    AEFLAGS,AEFCSOQ     COMSCORE ONLY PROGRAM?                       
         BZ    RD16                                                             
         REAR  ARS=OFF             XA MODE OFF                                  
         J     RDX                                                              
                                                                                
RD16     MVI   GAAFLAG,GAANOQ                                                   
         CLI   LQ_EL,LQ_DLDDQ      IS THIS THE DOWNLOAD DATA ELEMENT?           
         BE    RDX                 YES: WE'RE DONE                              
         BRAS  RE,BMPFEF4          BUMP PAST 'FEF4' MAP NUMBER                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DMCB,BBLOCKMQ                                                    
                                                                                
         BRAS  RE,GETMODF          HALF=BLD BLOCK MODIFIER                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
RD20     CLI   HALF+1,GAAIMPQ      IF MODIFIER IS 'B'                           
         BNE   *+8                                                              
         MVI   GAAFLAG,GAAYESQ     TURN FLAG ON FOR GAA                         
                                                                                
         BRAS  RE,BMPFEF4          BUMP PAST 'FEF4' MAP NUMBER                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,GETDVALS         PUT DEMO VALUES AT AIO2                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LAM   AR2,AR2,ALET                                                     
         L     R2,ADEMFORM         A(FORMULA TABLES)                            
         ICM   R0,15,(DEMFFFOR-DEMFFRMD)(R2)                                    
         AR    R2,R0               A(FORMULAS BY MODIFIER)                      
*                                                                               
         USING DEMFMHDD,R2                                                      
RD30     CLC   =XL4'00',0(R2)      END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                YES: BAD DEMO MODIFIER FROM PC               
         CLC   DEMFM2MD,HALF       FIND MATCH ON DEMO MODIFIER                  
         BE    *+14                                                             
         L     R0,DEMFMLN          L'TABLE ENTRY                                
         AR    R2,R0                                                            
         B     RD30                                                             
                                                                                
         ICM   R0,15,DEMFMAB3      DISP TO TYPE3 ID TABLE                       
         AR    R2,R0               R2 = A(TYPE3 ID TABLE)                       
         USING DEMFBL3D,R2                                                      
         LH    R0,DEMFB3CT         DEMO COUNT                                   
         LA    R2,DEMFB3DL         A(LIST OF BUILDING BLOCKS)                   
         L     R6,AIO2             R6->START OF BINARY DEMO VALUES              
                                                                                
RD40     DS    0H                                                               
         L     RF,FULL             A(DEMDISP TABLE SEGMENT)                     
         SR    R4,R4                                                            
         ICM   R4,3,5(RF)          L'EACH ELEMENT IN TABLE                      
         LA    RF,10(RF)           BUMP PAST SEGMENT HEADER                     
         LR    R5,RF               SAVE A(START OF ELEMENTS)                    
         MVC   BYTE,HALF+1         DEMO MODIFIER                                
                                                                                
RD50     DS    0H                                                               
         CLI   0(RF),X'FF'         EOT?                                         
         BNE   *+6                                                              
         DC    H'0'                BAD DEMO DESCRIPTION FROM PC!                
         CLC   BYTE,0(RF)          MATCH ON DEMO MODIFIER?                      
         BNE   *+14                                                             
         CLC   1(1,RF),0(R2)       MATCH ON DEMO NUMBER?                        
         BE    *+10                                                             
         AR    RF,R4               BUMP TO NEXT ELEMENT                         
         B     RD50                                                             
                                                                                
         ICM   R1,15,0(R6)         R1=NEXT DEMO VALUE                           
         SR    RF,R5               DISPLACEMENT TO THIS TABLE ELEMENT           
         SR    RE,RE               PREPARE FOR DIVIDE                           
         DR    RE,R4               PUTS NUMBER OF ENTRIES IN RF                 
         MHI   RF,4                * L'FULLWORD: RF= DISP INTO ARRAY            
         L     RE,AIO3             BUILD ARRAY IN AIO3                          
         AR    RE,RF               RE = A(THIS VALUE'S SLOT)                    
         ST    R1,0(RE)                                                         
         LA    R6,4(R6)            POINT TO NEXT BINARY VALUE                   
         LA    R2,1(R2)            NEXT DEMO NUMBER                             
         BCT   R0,RD40                                                          
         DROP  R2                                                               
         LAM   AR2,AR2,ARZEROS     CLEAR XR2 IN CASE ANY RTINES USE R2          
                                                                                
         CLI   LQ_EL,LQ_DLDDQ      NOW TRY FOR ANOTHER MODIFIER                 
         BE    RD60                DOWNLOAD DATA ELEM. DONE                     
         LR    R4,R3                                                            
         BRAS  RE,BMPFEF4          BUMP PAST 'FEF4' MAP CODE                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   DMCB,BBLOCKMQ                                                    
         BRAS  RE,GETMODF          GET NEXT MODIFIER                            
         BE    RD20                                                             
                                                                                
         LR    R3,R4               NO MORE.RESTORE A(ELEMENT IN TIA)            
                                                                                
RD60     LAM   AR2,AR2,ARZEROS     CLEAR XR'S                                   
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
         REAR  ARS=OFF             XA MODE OFF                                  
                                                                                
* DONE W/ ALL BUILDING BLOCKS. USE THEM TO CREATE PROGRAM RECORD DEMOS          
         ST    R3,FULL             STORE A(ELEMENT IN TIA)                      
         LA    R4,BLOCK            CREATE DUMMY DEMO RECORD                     
         USING DBLOCKD,R4          AT AIO3                                      
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NTI'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         L     RF,ACOMFACS                                                      
         L     RF,CDEMEL-COMFACSD(,RF)                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'NTIN'                                                 
         MVC   WORK+4(3),PGATBID   TABLE ID                                     
         MVC   WORK+7(2),AEBOOKYM  BOOK YR/MN                                   
         GOTO1 (RF),DMCB,(C'C',WORK),DBLOCKD,AIO3     DEMEL                     
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RE,AIO3             EXTRACT DEMOS FOR  THE PROG RECD             
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         LA    RE,2(RE)            SKIP LENGTH                                  
         AR    RF,RE                                                            
         MVI   0(RF),0             MAKE SURE RECORD ENDS WITH EOR               
         STCM  RE,15,DBAQUART      FIRST ELEMENT IN THE DUMMY RECORD            
                                                                                
         MVC   DMCB+4(4),=X'D9000ADF'       DEMOUT                              
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
                                                                                
         MVI   DBDEMTYP,C'4'       THIS IS TYPE-4 DEMO LIST                     
         L     R2,AIO2             CREATE INPUT LIST FOR DEMOUT AT AIO2         
         L     R5,=A(DEMTAB1)                                                   
         A     R5,RELO                                                          
RD65     CLC   =X'FFFFFF',0(R5)                                                 
         BE    RD68                                                             
         MVC   0(2,R2),0(R5)       NAD CATEGORY AND MODIFIER                    
         CLI   GAAFLAG,GAAYESQ     IF GAA DATA                                  
         BNE   *+8                                                              
         MVI   1(R2),GAAVPHQ       REQUEST GAA VPH'S                            
         MVC   HALF(1),2(R5)                                                    
         MVI   DMCB,CONV3TO4                                                    
         BRAS  RE,DTYPCONV                                                      
         MVC   2(2,R2),HALF        MAKE SURE TO SEND TYPE-4 DEMO LIST           
         LA    R2,4(R2)                                                         
         LA    R5,PRGDENTL(R5)                                                  
         B     RD65                                                             
RD68     MVC   0(3,R2),=X'FFFFFF'                                               
                                                                                
         LA    RE,DENGEXT          CREATE 'DENG' EXTENTION                      
         STCM  RE,15,DBEXTEND                                                   
         USING DBXDEND,RE                                                       
         MVI   DBXDENG,DBXDEONQ    TURN DEMO ENGINE OPTION ON                   
         DROP  RE                                                               
                                                                                
         MVC   DMCB,AIO2           INPUT LIST FOR DEMOUT                        
         MVI   DMCB,C'L'           INPUT IS A LIST OF DEMOS                     
         L     R2,AIO2                                                          
         LA    R2,1000(R2)         OUTPUT AREA FOR DEMOUT                       
                                                                                
         GOTO1 (RF),DMCB,,DBLOCKD,0(R2)  CALL DEMOUT FOR DEMOS                  
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
                                                                                
         LA    RE,ELEM                                                          
         USING NPG2ELEM,RE                                                      
         LA    R3,NPG2VPHS         FIRST VPH FIELD                              
         DROP  RE                                                               
         LHI   R5,PRGDENUM         NO OF ENTRIES                                
         L     RF,=A(DISPTAB)      ENTRY IN TABLE OF DISPLACEMENTS              
         A     RF,RELO                                                          
RD70     ZIC   R1,0(RF)            INDEX                                        
         SLL   R1,1                *2 (LENGTH OF ONE VPH VALUE)                 
         AR    R1,R3               POSITION OF VPH VALUE IN 93 ELEMENT          
         MVC   0(2,R1),2(R2)       MOVE 2-BYTE VPH VALUE                        
         LA    R2,4(R2)                                                         
         LA    RF,3(RF)                                                         
         BCT   R5,RD70                                                          
                                                                                
* DONE CREATING PROG RECORD DEMOS.                                              
* CONTINUE READING PC-COMPUTED DEMOS AND/OR OVERRIDES                           
RD80     L     R3,FULL                                                          
         CLI   LQ_EL,LQ_DLDDQ      IS THIS THE DOWNLOAD DATA ELEMENT?           
         BE    RDX                 YES: WE'RE DONE                              
         LR    R4,R3               SAVE START OF THIS ELEM SERIES               
         BRAS  RE,BMPFEF4          BUMP PAST 'FEF4' MAP CODE                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   DMCB,COMPINDQ                                                    
         BRAS  RE,GETMODF          GET MODIFIER FOR COMPUTED DEMOS              
         BE    RD90                                                             
         LR    R3,R4               RESTORE BEGINNING OF ELEMENTS                
         B     RD150               NO COMPUTED DEMOS. SKIP CHECKING             
                                                                                
* CHECK IF MY NUMBERS ARE THE SAME AS THE PC-COMPUTED NUMBERS                   
RD90     CLC   HALF,COMPVPH        MAKE SURE COMPUTED VPH ELEMENTS              
         BE    *+10                                                             
         CLC   HALF,COMPVPHG       OR COMPUTED GAA VPHS                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LR    R2,R3               SAVE POINTER TO 'FEF4'                       
         BRAS  RE,BMPFEF4          BUMP PAST 'FEF4' MAP CODE                    
*        BE    *+6                                                              
*        DC    H'0'                                                             
         BNE   RD150               'CV' INDICATOR WITHOUT DATA                  
                                                                                
         BRAS  RE,GETDVALS         PUT COMPUTED VALUES AT AIO2                  
         BE    RD100                                                            
         LR    R3,R2               RESTORE.'FEF4' BELONGS TO DIFF MODF          
         B     RD150               'CV' INDICATOR WITHOUT DATA                  
                                                                                
* VALUES FROM PC SHOULD COME IN THE SAME ORDER AS OUTPUT OF DEMOUT              
* BOTH ARE DRIVEN BY TABLE DEMTAB1                                              
RD100    L     R2,AIO2                                                          
         LA    R2,1000(R2)         OUTPUT AREA FROM DEMOUT                      
         LHI   R4,PRGDENUM         NUMBER OF ENTRIES IN DEMTAB1                 
         L     R6,AIO2             R6->START OF BINARY DEMO VALUES              
RD110    ICM   R1,15,0(R6)         R1= BINARY VALUE FROM PC                     
         ICM   R0,15,0(R2)         R0= BINARY VALUE FROM DEMOUT                 
*        CR    R1,R0                                                            
*        BE    *+6       DISABLE COMPARE. THIS WAS FOR TESTING PURPOSES         
*        DC    H'0'                                                             
         LA    R2,4(R2)                                                         
         LA    R6,4(R6)                                                         
         BCT   R4,RD110                                                         
                                                                                
RD150    DS    0X                                                               
                                                                                
RDX      ST    R3,LSTTIAE          SAVE A(LAST ELEMENT IN TIA)                  
         J     XIT                                                              
                                                                                
OVINDQ   EQU   C'O'                OVERRIDE INDICATOR                           
COMPINDQ EQU   C'C'                COMPUTED DEMOS INDICATOR                     
BBLOCKMQ EQU   C'W'                BUILDING BLOCK INDICATOR                     
VPHINDQ  EQU   C'V'                VPH DEMOS                                    
GAAIMPQ  EQU   C'B'                GAA IMPRESSIONS                              
GAAVPHQ  EQU   C'M'                GAA VPH'S                                    
COMPVPH  DC    C'CV'               COMPUTED VPH'S                               
COMPVPHG DC    C'CM'               COMPUTED VPH'S FOR GAA                       
                                                                                
DENGEXT  DC    C'DENG',AL4(0),X'00'   DEMO ENGINE EXTENTION                     
                                                                                
ARZEROS  DC    16F'0'                                                           
                                                                                
         LTORG                                                                  
                                                                                
XITNO    CHI   RB,0                                                             
         J     XIT                                                              
XITYES   CR    RB,RB                                                            
         J     XIT                                                              
XIT      XIT1                                                                   
                                                                                
                                                                                
XIT3NO   CHI   RB,0                                                             
         J     XIT3                                                             
XIT3YES  CR    RB,RB                                                            
         J     XIT3                                                             
XIT3     XIT1  REGS=(R3)                                                        
         EJECT                                                                  
***********************************************************************         
* ADVANCE ONE 'FEF4' ELEMENT (MAP CODE)                               *         
* UPDATE R3 TO THE ADDRESS OF NEW ELEMENT IN TIA                      *         
***********************************************************************         
                                                                                
BMPFEF4  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   LQ_EL,LQ_IMAPQ      MUST BE INPUT MAP NUMBER ELEMENT             
         JNE   XIT3NO                                                           
         CLC   LQ_IMAPN,=X'FEF4'   DATA CODE MUST BE X'FEF4'                    
         JNE   XIT3NO                                                           
         SR    R1,R1                                                            
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
         J     XIT3YES                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADVANCE PAST THE MODIFIER MAP CODE                                  *         
* UPDATE R3 TO ADDRESS OF NEW ELEMENT IN TIA                          *         
* AT ENTRY, DMCB(1) = TYPE OF MODIFIER REQUESTED                      *         
* AT EXIT, HALF = THE 2-CHAR MODIFIER                                 *         
***********************************************************************         
                                                                                
GETMODF  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   LQ_EL,LQ_RQSTQ      MUST BE REQUEST DATA ELEMENT                 
         JNE   XIT3NO                                                           
         SR    R1,R1                                                            
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         CHI   R1,7                BETTER BE 7                                  
         JNE   XIT3NO                                                           
         CLI   LQ_VALUE,101        MODIFIER FOR BUILDING BLOCKS                 
         BE    *+8                                                              
         CLI   LQ_VALUE,111        MODIFIER FOR COMPUTED DEMOS                  
         BE    *+8                                                              
         CLI   LQ_VALUE,121        MODIFIER FOR COMPUTED DEMOS                  
         BE    *+8                                                              
         CLI   LQ_VALUE,126        MODIFIER FOR COMSCORE DEMOS                  
         JNE   XIT3NO                                                           
*                                                                               
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
                                                                                
         CLI   LQ_EL,LQ_RQSTQ      MUST BE REQUEST DATA ELEMENT                 
         JNE   XIT3NO                                                           
         SR    R1,R1                                                            
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         CLC   LQ_DCODE,=AL2(3)    DATA CODE MUST BE 3                          
         JNE   XIT3NO                                                           
         CLI   LQ_TYPE,LQ_TSINQ    MUST BE A SINGLE VALUE                       
         JNE   XIT3NO                                                           
         LR    R0,R1               SAVE L'ELEMENT                               
         SHI   R1,LQ_VALUE-LQ_D    SUBTRACT OVERHEAD LENGTH                     
         CHI   R1,2                DEMO MODIFIER MUST BE 2 BYTES LONG           
         JNE   XIT3NO                                                           
         MVC   HALF,LQ_VALUE       SAVE DEMO MODIFIER                           
                                                                                
         CLC   DMCB(1),HALF                                                     
         JNE   XIT3NO              NOT THE REQUESTED KIND OF MODIFIER           
                                                                                
         AR    R3,R0               BUMP TO NEXT ELEMENT                         
         J     XIT3YES                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT DEMO VALUES INTO AIO2. VALUES COME FROM PC                  *         
*   UPON ENTRY:  R3= ADDRESS OF DEMO VALUES ELEMENT IN TIA            *         
*   UPON RETURN: R3= UPDATED ADDRES IN TIA                            *         
*                AIO2 HAS A LIST OF 4-BYTE DEMO VALUES                *         
***********************************************************************         
                                                                                
GETDVALS NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING LQ_D,R3                                                          
         CLI   LQ_EL,LQ_RQSTQ      MUST BE REQUEST DATA ELEMENT                 
         BNE   GETDVNO                                                          
         SR    R1,R1                                                            
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         CHI   R1,7                BETTER BE 7                                  
         BNE   GETDVNO                                                          
         CLI   LQ_VALUE,102        MAP CODE FOR DEMO VALUES                     
         BE    *+8                                                              
         CLI   LQ_VALUE,112        MAP CODE FOR COMPUTED DEMO VALUES            
         BE    *+8                                                              
         CLI   LQ_VALUE,123        MAP CODE FOR OVERRIDE DEMO VALUES            
         BE    *+8                                                              
         CLI   LQ_VALUE,128        MAP CODE FOR OVERRIDE DEMO VALUES            
         BNE   GETDVNO                                                          
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
                                                                                
         SR    R5,R5               TOTAL NUMBER OF DEMOS                        
GETDV05  CLI   LQ_EL,LQ_RQSTQ      MUST BE REQUEST DATA ELEMENT                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   LQ_DCODE,=AL2(3)    DATA CODE MUST BE 3                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LR    R0,R5               DETERMINE NEXT AVAILABLE POSITION            
         MHI   R0,4                FOR DEMOS AT AIO2                            
         L     R2,AIO2             OUTPUT AREA FOR DEMO VALUES                  
         AR    R2,R0                                                            
         BRAS  RE,EXTRDEMS         EXTRACT DEMO VALUES AT ADDRESS IN R2         
                                                                                
         AR    R5,R0               UPDATE TOTAL NUMBER OF DEMOS                 
         CHI   R5,500              500*4<2000                                   
         BNH   *+6                                                              
         DC    H'0'                TOO MANY DEMOS, OVERFLOW AIO2                
                                                                                
         SR    R1,R1               BUMP TO NEXT ELEMENT                         
         ICM   R1,3,LQ_LN                                                       
         AR    R3,R1                                                            
         LR    R6,R3               SAVE ADDRESS IN TIA                          
                                                                                
         BRAS  RE,BMPFEF4                                                       
         BNE   GETDV20             DONE WITH DEMO VALUES                        
         CLI   LQ_EL,LQ_RQSTQ      MORE DEMO VALUES ON NEXT ELEM?               
         BNE   GETDV20                                                          
         SR    R1,R1                                                            
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         CHI   R1,7                BETTER BE 7                                  
         BNE   GETDV20                                                          
         CLI   LQ_VALUE,102        MAP CODE FOR BLD BLK DEMO VALUES             
         BE    *+8                                                              
         CLI   LQ_VALUE,112        MAP CODE FOR COMPUTED DEMO VALUES            
         BE    *+8                                                              
         CLI   LQ_VALUE,123        MAP CODE FOR OVERRIDE DEMO VALUES            
         BE    *+8                                                              
         CLI   LQ_VALUE,128        MAP CODE FOR OVERRIDE DEMO VALUES            
         BNE   GETDV20                                                          
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
         B     GETDV05                                                          
                                                                                
GETDV20  LR    R3,R6               RESTORE A IN TIA                             
         B     GETDVYES                                                         
                                                                                
GETDVYES J     XIT3YES                                                          
GETDVNO  J     XIT3NO                                                           
         DROP  R3                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT A LIST OF BINARY VALUES FROM A COMMA SEPARATED STRING       *         
*   UPON ENTRY:  R2-> AREA TO EXTRACT BINARY DEMO VALUES              *         
*                R3-> ADDRESS OF INPUT STRING ELEMENT IN TIA          *         
*   UPON RETURN: A LIST OF 4-BYTE DEMO VALUES IS RETURNED AT 0(R2)    *         
*                R0 CONTAINS NUMBER OF DEMOS EXTRACTED                *         
***********************************************************************         
                                                                                
EXTRDEMS NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING LQ_D,R3                                                          
         LA    R5,LQ_VALUE                                                      
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         SHI   R1,LQ_VALUE-LQ_D    LENGTH OF COMMA SEPARATED STRING             
         LR    R4,R5                                                            
         AR    R4,R1               R4->FIRST BYTE AFTER END OF STRING           
                                                                                
         SR    R0,R0               COUNTER FOR NUMBER OF DEMOS                  
EXD10    XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         SR    R1,R1                                                            
EXD20    CLI   0(R5),C','                                                       
         BE    EXD30                                                            
         TM    0(R5),X'F0'                                                      
         BNO   EXD30               NOT A DIGIT NOR COMMA. DONE                  
         CR    R5,R4                                                            
         BNL   EXD30               END OF THE STRING                            
         MVC   0(1,RE),0(R5)       MOVE DIGIT                                   
         LA    R5,1(R5)            NEXT CHARACTER IN THE INPUT STRING           
         LA    RE,1(RE)            POSITION FOR NEXT DIGIT                      
         LA    R1,1(R1)            UPDATE NO OF DIGITS IN THIS NUMBER           
         B     EXD20                                                            
                                                                                
EXD30    XC    0(4,R2),0(R2)                                                    
         LTR   R1,R1                                                            
         BZ    EXD40               NO VALUE MEANS ZERO                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   RF,DUB                                                           
         STCM  RF,15,0(R2)         STORE BINARY VALUE AT NEXT POSITION          
                                                                                
EXD40    LA    R2,4(R2)            POSITION FOR NEXT DEMO VALUE                 
         AHI   R0,1                UPDATE NUMBER OF DEMO VALUES                 
         CR    R5,R4                                                            
         BNL   EXD50               END OF STRING. DONE                          
         CLI   0(R5),C','                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,1(R5)            GO PAST THE COMMA                            
         B     EXD10                                                            
                                                                                
EXD50    DS    0H                                                               
         XIT1  REGS=(R0)                                                        
         DROP  R3                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT DEMO CATEGORIS INTO AIO3                                    *         
*   UPON ENTRY:  R3= ADDRESS OF DEMO VALUES ELEMENT IN TIA            *         
*   UPON RETURN: R3= UPDATED ADDRES IN TIA                            *         
*                AIO3 HAS A LIST OF 3-BYTE DEMO CATEGORIES            *         
*                1-BYTE NAD CATEGORY, 2-BYTE DEMO NUMBER              *         
***********************************************************************         
                                                                                
GETODEMS NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING LQ_D,R3                                                          
         CLI   LQ_EL,LQ_RQSTQ      MUST BE REQUEST DATA ELEMENT                 
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         CHI   R1,7                BETTER BE 7                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   LQ_VALUE,122        MAP CODE FOR DEMO CATEGORIES                 
         BE    GETOV02                                                          
         CLI   LQ_VALUE,127        MAP CODE FOR COMSCORE DEMO CATS              
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    AEFLAGS,AEFCSQ      COMSCORE DEMOS                               
GETOV02  AR    R3,R1               BUMP TO NEXT ELEMENT                         
                                                                                
         SR    R5,R5               TOTAL NUMBER OF DEMOS                        
GETOV05  CLI   LQ_EL,LQ_RQSTQ      MUST BE REQUEST DATA ELEMENT                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   LQ_DCODE,=AL2(3)    DATA CODE MUST BE 3                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LR    R0,R5               DETERMINE NEXT AVAILABLE POSITION            
*                                                                               
         TM    AEFLAGS,AEFCSQ      COMSCORE DEMOS                               
         BZ    *+12                                                             
         MHI   R0,L'NPGCCAT                                                     
         B     *+8                                                              
         MHI   R0,3                FOR DEMO CATEGORIES IN AIO3                  
*                                                                               
         L     R2,AIO3             OUTPUT AREA FOR DEMO VALUES                  
         AR    R2,R0                                                            
         BRAS  RE,EXTRCATS         EXTRACT CATEGORIES AT ADDRESS IN R2          
                                                                                
         AR    R5,R0               UPDATE TOTAL NUMBER OF DEMOS                 
         CHI   R5,500              500*4<2000                                   
         BNH   *+6                                                              
         DC    H'0'                TOO MANY DEMOS, OVERFLOW VALUES AREA         
                                                                                
         SR    R1,R1               BUMP TO NEXT ELEMENT                         
         ICM   R1,3,LQ_LN                                                       
         AR    R3,R1                                                            
         LR    R6,R3               SAVE ADDRESS IN TIA                          
                                                                                
         BRAS  RE,BMPFEF4                                                       
         BNE   GETOV20             DONE WITH DEMO CATEGORIES                    
         CLI   LQ_EL,LQ_RQSTQ      MORE DEMO CATEGORIES IN NEXT ELEM?           
         BNE   GETOV20                                                          
         SR    R1,R1                                                            
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         CHI   R1,7                BETTER BE 7                                  
         BNE   GETOV20                                                          
         CLI   LQ_VALUE,122        MAP CODE FOR DEMO CATEGORIES                 
         BE    *+8                                                              
         CLI   LQ_VALUE,127        MAP CODE FOR DEMO CATEGORIES                 
         BNE   GETOV20                                                          
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
         B     GETOV05                                                          
                                                                                
GETOV20  TM    AEFLAGS,AEFCSQ      COMSCORE DEMOS?                              
         BZ    GETOV30                                                          
         MHI   R5,L'NPGCCAT                                                     
         L     R2,AIO3                                                          
         AR    R2,R5                                                            
         XC    0(L'NPGCCAT,R2),0(R2)                                            
         B     GETOV40                                                          
*                                                                               
GETOV30  MHI   R5,3                PUT END OF DEMOS MARKER                      
         L     R2,AIO3                                                          
         AR    R2,R5                                                            
         XC    0(3,R2),0(R2)                                                    
                                                                                
GETOV40  LR    R3,R6               RESTORE A IN TIA                             
         J     XIT3                                                             
         DROP  R3                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT A LIST OF BINARY DEMO CATEGS FROM A COMMA SEPARATED STRING  *         
* A DEMO CATEGORY CAN BE PRECEEDED BY A NAD CATEGORY FOLLOWED BY '.'  *         
* EXAMPLE INPUT LIST: 35,11.35                                        *         
*   UPON ENTRY:  R2-> AREA TO EXTRACT BINARY DEMO CATEGORIES          *         
*                R3-> ADDRESS OF INPUT STRING ELEMENT IN TIA          *         
*   UPON RETURN: A LIST OF 3-BYTE DEMO CATEGS IS RETURNED AT 0(R2)    *         
*                3-BYTE= 1-BYTE NAD CATEGORY, 2-BYTE DEMO CATEGORY    *         
*                R0 CONTAINS NUMBER OF DEMOS EXTRACTED                *         
***********************************************************************         
                                                                                
EXTRCATS NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING LQ_D,R3                                                          
         LA    R5,LQ_VALUE                                                      
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,LQ_LN          L'ELEMENT                                    
         SHI   R1,LQ_VALUE-LQ_D    LENGTH OF COMMA SEPARATED STRING             
         LR    R4,R5                                                            
         AR    R4,R1               R4->FIRST BYTE AFTER END OF STRING           
*                                                                               
         SR    R0,R0               COUNTER FOR NUMBER OF DEMOS                  
*                                                                               
         TM    AEFLAGS,AEFCSQ      COMSCORE DEMOS?                              
         JO    EXC100                                                           
*                                                                               
         XC    0(3,R2),0(R2)                                                    
EXC10    XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         SR    R1,R1                                                            
EXC20    CLI   0(R5),C','                                                       
         BE    EXC30                                                            
         CLI   0(R5),C'.'                                                       
         BE    EXC25               THIS WAS A NAD CATEGORY                      
         TM    0(R5),X'F0'                                                      
         BNO   EXC30               NOT A DIGIT NOR COMMA. DONE                  
         CR    R5,R4                                                            
         BNL   EXC30               END OF THE STRING                            
         MVC   0(1,RE),0(R5)       MOVE DIGIT                                   
         LA    R5,1(R5)            NEXT CHARACTER IN THE INPUT STRING           
         LA    RE,1(RE)            POSITION FOR NEXT DIGIT                      
         LA    R1,1(R1)            UPDATE NO OF DIGITS IN THIS NUMBER           
         B     EXC20                                                            
                                                                                
EXC25    LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                NAD CATEGORY CAN'T BE LENGTH 0               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   RF,DUB                                                           
         STC   RF,0(R2)            STORE NAD CATEGORY                           
         LA    R5,1(R5)            GO PAST '.'                                  
         B     EXC10                                                            
                                                                                
EXC30    LTR   R1,R1                                                            
         BZ    EXC40               NO VALUE MEANS ZERO                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   RF,DUB                                                           
         STCM  RF,3,1(R2)          STORE BINARY DEMO CATEGORY                   
                                                                                
EXC40    LA    R2,3(R2)            POSITION FOR NEXT DEMO CATEGORY              
         AHI   R0,1                UPDATE NUMBER OF DEMO CATEGORIES             
         CR    R5,R4                                                            
         BNL   EXC50               END OF STRING. DONE                          
         CLI   0(R5),C','                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,1(R5)            GO PAST THE COMMA                            
         XC    0(3,R2),0(R2)       CLEAR NEXT AVAILABLE SPOT                    
         B     EXC10                                                            
                                                                                
EXC50    J     EXCX                                                             
*                                                                               
* EXTRACT COMSCORE DEMO CATEGORIES                                              
*                                                                               
EXC100   XC    0(L'NPGCCAT,R2),0(R2)                                            
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
EXC120   CLI   0(R5),C','                                                       
         BE    EXC130                                                           
         CR    R5,R4                                                            
         BNL   EXC130              END OF THE STRING                            
         MVC   0(1,RE),0(R5)       MOVE CHARACTER                               
         LA    R5,1(R5)            NEXT CHARACTER IN THE INPUT STRING           
         LA    RE,1(RE)            POSITION FOR NEXT DIGIT                      
         B     EXC120                                                           
*                                                                               
EXC130   MVC   0(L'NPGCCAT,R2),WORK                                             
         AHI   R2,L'NPGCCAT        POSITION FOR NEXT DEMO CATEGORY              
         XC    0(L'NPGCCAT,R2),0(R2)                                            
         AHI   R0,1                UPDATE NUMBER OF DEMO CATEGORIES             
         CR    R5,R4                                                            
         BNL   EXCX                END OF STRING. DONE                          
         CLI   0(R5),C','                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,1(R5)            GO PAST THE COMMA                            
         B     EXC100                                                           
*                                                                               
EXCX     XIT1  REGS=(R0)                                                        
         DROP  R3                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT BETWEEN TYPE-3 AND TYPE-4 DEMO NUMBER                       *         
*                                                                     *         
* DMCB(1) = CONV4TO3 : CONVERT FROM TYPE 4 TO TYPE 3                  *         
*   UPON ENTRY:  HALF CONTAINS TYPE-4 DEMO NUMBER (2-BYTES)           *         
*   UPON RETURN: HALF(1) CONTAINS TYPE-3 DEMO NUMBER (1-BYTE)         *         
*                                                                     *         
* DMCB(1) = CONV3TO4 : CONVERT FROM TYPE 3 TO TYPE 4                  *         
*   UPON ENTRY:  HALF(1) CONTAINS TYPE-3 DEMO NUMBER (1-BYTE)         *         
*   UPON RETURN: HALF CONTAINS TYPE-4 DEMO NUMBER (2-BYTES)           *         
***********************************************************************         
CONV4TO3 EQU   1                                                                
CONV3TO4 EQU   2                                                                
                                                                                
DTYPCONV NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   DMCB,CONV4TO3                                                    
         BE    D4TO3                                                            
         CLI   DMCB,CONV3TO4                                                    
         BE    D3TO4                                                            
         DC    H'0'                                                             
                                                                                
D4TO3    L     RE,=A(NTIDEMS)                                                   
         A     RE,RELO                                                          
         USING NTIDTD,RE                                                        
D4TO3_10 CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                DEMO NUMBER NOT DEFINED                      
         CLC   NTDTYP4,HALF                                                     
         BE    D4TO3_20                                                         
         LA    RE,NTIDTL(RE)                                                    
         B     D4TO3_10                                                         
D4TO3_20 MVC   HALF(1),NTDTYP3                                                  
         B     DTYPCNVX                                                         
                                                                                
D3TO4    L     RE,=A(NTIDEMS)                                                   
         A     RE,RELO                                                          
D3TO4_10 CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                DEMO NUMBER NOT DEFINED                      
         CLC   NTDTYP3,HALF                                                     
         BE    D3TO4_20                                                         
         LA    RE,NTIDTL(RE)                                                    
         B     D3TO4_10                                                         
D3TO4_20 MVC   HALF,NTDTYP4                                                     
                                                                                
         DROP  RE                                                               
DTYPCNVX XIT1                                                                   
                                                                                
         LTORG                                                                  
       ++INCLUDE DENTIDEMS                                                      
         EJECT                                                                  
***********************************************************************         
* DISPLAY PROGRAM RECORD CREATED IN AUDIENCE ESTIMATOR (AE)           *         
* THIS LOGIC USES DEMOUT AND ACCOUNTS FOR OVERRIDE ELEMENTS           *         
* AT ENTRY: R3->BEGINNING OF DEMOS                                    *         
*           R7->BEGINNING OF PROGRAM RECORD                           *         
***********************************************************************         
                                                                                
AEPGDIS  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   PGMGAA,BLANKS       NOT A GAA RECORD UNLESS AE                   
         FOUT  PGMGAAH                                                          
         MVC   PGMAE,BLANKS        NOT AN AE RECORD                             
         FOUT  PGMAEH                                                           
                                                                                
         LR    R5,R3               SAVE A(FIRST DEMO)                           
         LA    R3,24(R7)                                                        
         MVI   ELCODE,NPGAICDQ     AE ELEMENT                                   
         BRAS  RE,NXTELV3                                                       
         BNE   AENO                NO AE ELEMENT,IS MF-CREATED RECORD           
                                                                                
         MVC   PGMAE,=C'(AE) '     DEFAULT AE INDICATOR                         
                                                                                
         USING NPGAEIFD,R3                                                      
         TM    NPGAIFLG,NPGAIGAQ                                                
         BNO   *+10                                                             
         MVC   PGMGAA,=C'GAA'                                                   
         FOUT  PGMGAAH                                                          
                                                                                
         TM    NPGAIFLG,NPGAIMFQ                                                
         BZ    AEPG03                                                           
         MVC   PGMAE+3(2),=C'X)'   AE-RECORD CHANGED FROM SFM SCREEN            
         B     AEPG05                                                           
AEPG03   CLI   NPGAILN,NPGAEIL1                                                 
         BE    AEPG05                                                           
         OC    NPGAIMET,NPGAIMET   APPEND METHOD-CHAR AFTER DEFAULT             
         BZ    AEPG05                                                           
         MVC   PGMAE+3(L'NPGAIMET),NPGAIMET                                     
         MVI   PGMAE+3+L'NPGAIMET,C')'  AND CLOSE PARNETHESIS                   
AEPG05   FOUT  PGMAEH                                                           
                                                                                
         DROP  R3                                                               
                                                                                
         L     R4,AIO2             CREATE A DUMMY RECORD HERE                   
         MVI   0(R4),E33CODE       MIMIC A '33' ESTIMATED DEMO ELEMENT          
         MVI   1(R4),MAX93VPH*2+3  LENGTH                                       
         MVI   2(R4),E33IND        INDICATOR                                    
         MVC   3(MAX93VPH*2,R4),0(R5)  DEMOS                                    
*                                                                               
* PROCESS M12-14 HERE                                                           
*                                                                               
* SUSHEEL SAYS: WE'RE GETTING DEMOS FROM A.E. THAT WE SHOULD *NOT*              
* DISPLAY ON THE SCREEN, BUT WE *DO* NEED TO STORE THEM IN THE                  
* PROGRAM RECORD.                                                               
* I.E., HHWC < 6 IS CURRENTLY THE LAST DISPLAYED CATEGORY. NOW FOR THE          
* FIRST TIME, WE HAVE CATEGORIES COMING BACK FROM A.E. WHICH ARE NOT            
* BEING DISPLAYED. SO I NEED TO A) KNOW TO NOT DISPLAY THEM, AND B)             
* SAVE THEM IN THE PROGRAM RECORD.                                              
         LA    RF,3(R4)                                                         
         AHI   RF,L'NPG2VPHS       MATCH DEDEMDISP EVN 33 TABLE                 
         LA    RE,0(R5)                                                         
         AHI   RE,L'NPG2VPHS+4     NPG2VPH2                                     
         MVC   0(2,RF),0(RE)                                                    
         ZIC   RF,1(R4)                                                         
         AHI   RF,2                                                             
         STC   RF,1(R4)                                                         
*                                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0               SPOT FOR NEXT ELEMENT                        
                                                                                
         LA    R3,24(R7)                                                        
         MVI   ELCODE,EDDOVER      COPY OVERRIDE ELEMENTS TO DUMMY RECD         
AEPG10   BRAS  RE,NXTELV3                                                       
         BNE   AEPG20                                                           
         ZIC   R1,1(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     AEPG10                                                           
                                                                                
AEPG20   MVI   0(R4),0             MAKE SURE I HAVE EOR                         
         LA    RE,BLOCK                                                         
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'EVN'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBAQUART,AIO2       GET DEMOS FROM DUMMY RECD                    
         DROP  RE                                                               
                                                                                
         MVC   DMCB+4(4),=X'D9000ADF'       DEMOUT                              
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         L     R3,AIO3                                                          
         L     R0,=A(DEMTAB1)                                                   
         A     R0,RELO                                                          
         ST    R0,0(R1)            P1 TO DEMOUT IS A(DEMTAB1)                   
         MVI   0(R1),C'L'          P1 HOB IS C'L'                               
         GOTO1 (RF),(R1),,BLOCK,0(R3)                                           
         LA    R2,PGMW1H           FIRST VPH FIELD                              
         LA    R5,MAXVPHF          FOR BCT                                      
                                                                                
AEPG30   MVC   8(4,R2),=C'0   '                                                 
         ICM   R0,15,0(R3)                                                      
         BZ    AEPG40                                                           
         CVD   R0,DUB                                                           
         EDIT  (B4,0(R3)),(4,8(R2)),0,ALIGN=LEFT                                
AEPG40   OI    4(R2),X'20'         SET PREVALID BIT                             
         FOUT  (R2)                                                             
                                                                                
AEPG50   ZIC   R0,0(R2)            ADVANCE TO NEXT UNPROTECTED FIELD            
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    AEPG50                                                           
                                                                                
         LA    R3,4(R3)                                                         
         BCT   R5,AEPG30                                                        
         B     AEYES                                                            
                                                                                
AEYES    CR    RB,RB                                                            
         B     AEPGRECX                                                         
AENO     CHI   RB,0                                                             
         B     AEPGRECX                                                         
AEPGRECX J     EXIT                                                             
                                                                                
NXTELV3  DS    0H                                                               
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         JE    NXTELV32                                                         
         CLC   ELCODE,0(R3)                                                     
         BER   RE                                                               
         J     NXTELV3                                                          
NXTELV32 LTR   R3,R3                                                            
         BR    RE                                                               
                                                                                
E33CODE  EQU   X'33'               ESTIMATED DEMOS ELEM CODE                    
E33IND   EQU   X'42'               INDICATOR: PRECISION+LENGTH OF DEMO          
EDDOVER  EQU   X'DD'               OVERRIDE ELEMENT CODE                        
                                                                                
***********************************************************************         
* CHECK IF WE ARE CHANGING AN AE-ORIGINATED RECORD FROM MF SCREEN     *         
* IF SO, TURN ON THE FLAG IN AE ELEMENT                               *         
***********************************************************************         
                                                                                
AEMFCHG  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   RECNUM,60           AE TRANSACTION?                              
         BE    AEMFCNO             YES. NOT CHANGING IT FROM MAINFRAME          
         CLI   ACTNUM,ACTCHA       MAKE SURE IT'S CHANGE ACTION                 
         BNE   AEMFCNO                                                          
                                                                                
         LA    R3,24(R7)                                                        
         MVI   ELCODE,NPGAICDQ     RECORD HAS AE ELEMENT                        
         BRAS  RE,NXTELV3                                                       
         BNE   AEMFCNO             NO. IT DIDN'T ORIGINATE FROM AE              
                                                                                
         L     RE,AIO3             SAVE ELEM AT AIO3+1000                       
         LA    RE,1000(RE)                                                      
         LA    RF,1000                                                          
         LA    R0,ELEM                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
                                                                                
         MVC   ELEM(NPGAEILM),0(R3)  UPDATE AE ELEMENT                          
         LA    RE,ELEM                                                          
         USING NPGAEIFD,RE                                                      
         OI    NPGAIFLG,NPGAIMFQ                                                
                                                                                
         CLI   NPGAILN,NPGAEIL1                                                 
         BH    *+8                                                              
         MVI   NPGAIMET,0          NO METHODOLOGY FOR OLDER RECORDS             
         CLI   NPGAILN,NPGAEIL2                                                 
         BH    *+10                                                             
         XC    NPGAHUT,NPGAHUT     NO HUT FOR OLDER RECORDS                     
                                                                                
         MVI   NPGAILN,NPGAEILM    UPDATE TO NEWEST LENGTH                      
         DROP  RE                                                               
                                                                                
AEMF20   MVI   ELCODE,NPGAICDQ                                                  
         BRAS  RE,DELEL            DELETE EXISTENT AE ELEMENT                   
         BRAS  RE,PUTEL            AND CREATE NEW ONE                           
                                                                                
         L     RE,AIO3             RESTORE ELEM FROM AIO3+1000                  
         LA    RE,1000(RE)                                                      
         LA    RF,1000                                                          
         LA    R0,ELEM                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     AEMFCYES                                                         
                                                                                
AEMFCNO  J     XITNO                                                            
AEMFCYES J     XITYES                                                           
BLANKS   DC    CL10' '                                                          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FOR AE-ORIGINATED RECORDS, STORE VALUES ENTERED FROM MF SCREEN AS   *         
* OVERRIDE 'DD' ELEMENTS.                                             *         
* UPON ENTRY: AIO3 POINTS TO AE-ORIGINATED VPH VALUES                 *         
*             ELEM HAS '93' ELEMENT WITH MF-INPUT VPH VALUES          *         
* UPON EXIT: ELEM '93' GETS REPLACED WITH AE-ORIGINATED VPH VALUES.   *         
*            FOR DEMOS THAT CHANGED, CREATE 'DD' OVERRIDE ELEMENTS.   *         
*            IF AN OVERRIDE ALREADY EXISTS FOR A DEMO, REPLACE IT.    *         
***********************************************************************         
                                                                                
AEMFOVRD NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     RE,AIO3             SAVE ELEM '93' AT AIO3+1000                  
         LA    RE,1000(RE)                                                      
         LA    RF,1000                                                          
         LA    R0,ELEM                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
                                                                                
         L     RE,AIO3                                                          
         LA    RE,1000(RE)                                                      
         USING NPGEL93,RE                                                       
         LA    R2,NPG2VPHS         MF-ADJUSTED VPH VALUES                       
         DROP  RE                                                               
                                                                                
         LA    R5,MAX93VPH                                                      
         L     R4,AIO3             AE-ORIGINATED VPH VALUES                     
                                                                                
AMOV10   CLC   0(2,R4),0(R2)                                                    
         BE    AMOV50                                                           
         BRAS  RE,UPDOVRD          GO UPDATE OVERRIDE ELEMENT                   
         MVC   0(2,R2),0(R4)       KEEP THE ORIGINAL '93' ELEM VALUES           
                                                                                
AMOV50   LA    R4,2(R4)                                                         
         LA    R2,2(R2)                                                         
         BCT   R5,AMOV10                                                        
                                                                                
         L     RE,AIO3             RESTORE ELEM '93' IN ELEM                    
         LA    RE,1000(RE)                                                      
         LA    RF,1000                                                          
         LA    R0,ELEM                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
AEMFOVRX J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE OVERRIDE ELEMENT: CHANGE VALUE IF IT EXISTS, ELSE ADD NEW    *         
* AT ENTRY, R5=MAX93VPH-INDEX OF DEMO TO OVERRIDE                     *         
*           R2 POINTS TO VALUE OF DEMO TO OVERRIDE                    *         
***********************************************************************         
                                                                                
UPDOVRD  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R3,MAX93VPH                                                      
         SR    R3,R5               INDEX OF DEMO IN '93' ELEMENT                
                                                                                
         L     R4,=A(DISPTAB)                                                   
         A     R4,RELO                                                          
         L     R6,=A(DEMTAB1)                                                   
         A     R6,RELO                                                          
                                                                                
UPDO10   ZIC   RE,0(R4)                                                         
         CR    RE,R3                                                            
         BE    UPDO20                                                           
         LA    R4,3(R4)                                                         
         LA    R6,PRGDENTL(R6)                                                  
         B     UPDO10                                                           
                                                                                
UPDO20   LA    R3,24(R7)                                                        
         MVI   ELCODE,EDDOVER      LOOK FOR EXISTENT OVERRD ELEMENTS            
UPDO30   BRAS  RE,NXTELV3                                                       
         BNE   UPDO50              NONE, GO ADD IT                              
         USING NPGELDD,R3                                                       
         TM    NPGDFLG,NPGDAEQ     IS THIS AN AE OVERRIDE?                      
         BNO   UPDO30              NO. TRY NEXT ELEMENT                         
         CLC   NPGDNUM,2(R6)       IS THIS OVERRIDE FOR THE SAME DEMO?          
         BNE   UPDO30              NO. TRY NEXT OVERRIDE ELEMENT                
         MVC   NPGDAMT+2(2),0(R2)  NEW OVERRIDE VALUE                           
         B     UPDOVRDX                                                         
         DROP  R3                                                               
                                                                                
UPDO50   LA    RE,ELEM             ADD NEW OVERRIDE ELEMENT                     
         XC    ELEM,ELEM                                                        
         USING NPGELDD,RE                                                       
         MVI   NPGDEMEL,EDDOVER    ELEMENT CODE                                 
         MVI   NPGDELEN,12         ELEMENT LENGTH                               
         MVI   NPGDCAT,0           NAD CATEGORY                                 
         MVI   NPGDMOD,VPHINDQ     MODIFIER FOR VPH                             
         MVC   NPGDNUM,2(R6)       DEMO NUMBER                                  
         OI    NPGDFLG,NPGDNADQ    DEMO VALUE SET                               
         OI    NPGDFLG,NPGDAEQ     AE OVERRIDE                                  
         MVI   NPGDPRE,X'40'       PRECISION                                    
         MVC   NPGDAMT+2(2),0(R2)  DEMO VALUE                                   
         DROP  RE                                                               
         BRAS  RE,PUTEL                                                         
                                                                                
UPDOVRDX J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SAVE AE-ORIGINATED DEMO VALUES AT AIO3                              *         
***********************************************************************         
                                                                                
SVAEDEM  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LR    R6,R7                                                            
         MVI   ELCODE,X'93'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                ALWAYS HAVE '93' ELEM ON AE RECORDS          
                                                                                
         USING NPGEL93,R6                                                       
         L     RE,AIO3                                                          
         MVC   0(L'NPG2VPHS,RE),NPG2VPHS                                        
         DROP  R6                                                               
                                                                                
SVAEDEMX J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET OPTION FILTERS FOR LIST SCREEN                                  *         
***********************************************************************         
                                                                                
SETOPT   NTR1  BASE=*,LABEL=*                                                   
         XC    OPTSPER,OPTSPER                                                  
         XC    OPTFILT,OPTFILT                                                  
         XC    OPTDAYP,OPTDAYP                                                  
         XC    OPTTIME,OPTTIME                                                  
         XC    OPTFAX,OPTFAX                                                    
         XC    OPTDAY,OPTDAY                                                    
         XC    OPTSTER,OPTSTER                                                  
         MVI   OPTAE,0                                                          
         MVI   OPTVWSC,0                                                        
         MVI   OPTVWST,0                                                        
         CLI   MODE,PRINTREP       PRINT DOES NOT SUPPORT OPTIONS               
         BE    SETOPTX                                                          
*                                                                               
         LA    R2,LPROPTH          * OPTIONS                                    
         GOTO1 VALIFLD                                                          
         BZ    SETOPTX                                                          
         CLI   ACTNUM,ACTSEL                                                    
         BE    STO310                                                           
         CLI   ACTNUM,ACTLIST                                                   
         JNE   INVERR2                                                          
STO310   L     R3,AIO2                                                          
         USING SCAND,R3                                                         
         XC    DMCB,DMCB                                                        
         GOTO1 SCANNER,DMCB,(20,(R2)),(4,0(R3))                                 
         CLI   DMCB+4,0                                                         
         JE    INVERR2                                                          
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,1,DMCB+4                                                      
*                                                                               
STO315   CLC   =C'FAX',FLD1        FAX                                          
         BNE   STO320                                                           
*                                                                               
         MVI   OPTFAX,C'Y'         SHOW RECORDS THAT HAVE FAX                   
         B     STO500                                                           
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
         CLC   =C'PIVAE',FLD1      AUDIENCE ESTIMATOR RECORDS                   
         BE    STO410                                                           
*                                                                               
         CLC   =C'PVS',FLD1        VIEWING SOURCE FILTER                        
         BE    STO420                                                           
*                                                                               
         CLI   FLD1,C'P'           PERIOD                                       
         JNE   INVERR2                                                          
* VALIDATE THE DATE FIELD                                                       
*                                                                               
         XC    SCANAREA,SCANAREA                                                
         MVC   SCANAREA+5(1),FLD2LEN                                            
         MVC   SCANAREA+8(20),FLD2                                              
         LA    R5,WORK2                                                         
         GOTO1 SCANNER,DMCB,SCANAREA,(2,0(R5)),C',=,-'                          
         CLI   4(R1),0                                                          
         JE    INVERR2                                                          
         CLI   1(R5),0             ARE 2 DATES INPUTTED                         
         BNE   STO330                                                           
         MVC   WORK2+1(1),WORK2                                                 
         MVC   WORK2+8(4),WORK2+4                                               
         MVC   WORK2+22(10),WORK2+12                                            
STO330   CLI   0(R3),0             TEST FIRST DATE GIVEN                        
         JE    INVERR2                                                          
         GOTO1 DATVAL,DMCB,(0,12(R5)),(0,50(R5))                                
         OC    0(4,R1),0(R1)                                                    
         JZ    INVERR2                                                          
         CLC   3(1,R1),0(R5)                                                    
         JNE   INVERR2                                                          
STO335   CLI   1(R3),0             TEST SECOND DATE GIVEN                       
         JE    INVERR2                                                          
         GOTO1 DATVAL,DMCB,(0,22(R5)),(0,56(R5))                                
         OC    0(4,R1),0(R1)                                                    
         JZ    INVERR2                                                          
         CLC   3(1,R1),1(R5)                                                    
         JNE   INVERR2                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,50(R5)),(2,OPTSPER)                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,56(R5)),(2,OPTEPER)                               
*                                                                               
         CLC   OPTSPER(2),OPTEPER   START CANNOT BE GREATER THEN END            
         JH    INVERR2                                                          
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
         JE    INVERR2                                                          
         GOTO1 DAYVAL,DMCB,((R5),FLD2),OPTDAY,BYTE                              
         CLI   OPTDAY,0                                                         
         JE    INVERR2                                                          
         B     STO500                                                           
*                                                                               
STO380   ZIC   R5,FLD2LEN                                                       
         GOTO1 TIMVAL,DMCB,((R5),FLD2),OPTTIME                                  
         CLI   DMCB,X'FF'          INVALID TIME                                 
         JE    INVERR2                                                          
         CLC   OPTTIME,=C'NONE'                                                 
         JE    INVERR2                                                          
         CLC   OPTTIME,=C'VARY'                                                 
         JE    INVERR2                                                          
         CLC   OPTTIME+2(2),=2X'00'                                             
         JE    INVERR2             END TIME REQUIRED                            
         CLC   OPTTIME+2(2),=C'CC'                                              
         JE    INVERR2                                                          
         B     STO500                                                           
*                                                                               
STO390   DS    0H                                                               
         LA    R2,TEMPHEAD                                                      
         XC    TEMPHEAD,TEMPHEAD                                                
*                                                                               
         MVI   0(R2),X'0A'                                                      
         MVC   8(2,R2),FLD2                                                     
*                                                                               
         MVC   TEMPKEY,KEY                                                      
         GOTO1 VALIDPT                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(13),TEMPKEY                                                  
         CLI   QDPT,0              VALID DAYPART?                               
         JE    INVERR2                                                          
         MVC   OPTDAYP,QDPT                                                     
         B     STO500                                                           
*                                                                               
STO400   MVI   OPTSTER,C'Y'                                                     
         B     STO500                                                           
*                                                                               
STO410   ICM   RE,1,FLD2LEN                                                     
         JZ    INVERR2                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FLD2(0),=C'YES'     PIVAE=YES                                    
         BNE   STO412                                                           
         MVI   OPTAE,OPTAEYQ                                                    
         B     STO500                                                           
STO412   ICM   RE,1,FLD2LEN                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FLD2(0),=C'NO'      PIVAE=NO                                     
         JNE   INVERR2                                                          
         MVI   OPTAE,OPTAENQ                                                    
         B     STO500                                                           
*                                                                               
STO420   CLI   FLD2,C'*'           WILD CARD                                    
         BE    STO421                                                           
         CLI   FLD2,NPG2SPGM       FIRST CHAR SHOULD BE SOURCE                  
         BE    STO421              P (PROGRAM AVERAGE) OR                       
         CLI   FLD2,NPG2SCOM       C (COMMERCIAL AVG) OR                        
         BE    STO421              A (PROG AVERAGE FROM ACM TAPES)              
         CLI   FLD2,NPG2SAPG       OR                                           
         BE    STO421              T (TIME PERIOD)                              
         CLI   FLD2,NPG2STP                                                     
         JNE   INVERR2                                                          
STO421   CLI   FLD2+1,C'*'                                                      
         BE    STO423                                                           
         L     RE,=A(VWTYPTB)                                                   
         A     RE,RELO                                                          
         USING VWTYPTBD,RE                                                      
STO422   CLI   0(RE),X'FF'         SECOND CHARACTER IS VIEWING TYPE             
         JE    INVERR2                                                          
         CLC   VWMF,FLD2+1                                                      
         BE    *+12                                                             
         LA    RE,VWTYPTBL(RE)                                                  
         B     STO422                                                           
STO423   MVC   OPTVWSC,FLD2        VIEWING SOURCE                               
         MVC   OPTVWST,FLD2+1      VIEWING TYPE                                 
         B     STO500                                                           
         DROP  RE                                                               
*                                                                               
*                                                                               
STO500   LA    R3,42(R3)                                                        
         BCT   R4,STO315                                                        
                                                                                
SETOPTX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
VWTYPTBD DSECT                                                                  
VWPC     DS    C                   VIEWING TYPE CHAR FROM PC                    
VWMF     DS    C                   VIEWING TYPE CHAR FOR MAINFRAME              
VWTYPTBL EQU   *-VWTYPTBD                                                       
                                                                                
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
LRSHR    DS    CL7                                                              
         DS    CL1                                                              
LRNTI    DS    CL5                                                              
         DS    CL1                                                              
LRGAA    DS    CL1                                                              
*                                                                               
DEMOPARD DSECT                     3-BYTE INPUT DEMO EXPRSN FOR DEMOUT          
DEMPNAD  DS    HL1                 NAD CATEGORY                                 
DEMPMOD  DS    C                   MODIFIER                                     
DEMPDEMO DS    HL1                 DEMO NUMBER TYPE-3                           
DEMOPARL EQU   *-DEMOPARD                                                       
*                                                                               
       ++INCLUDE NEPROGTABD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPTYP                                                      
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
                                                                                
         ORG   CONHEADH-64+BASETWA2                                             
*  SECOND SAVE STORAGE AREA                                                     
SVAREA   DS    0H                                                               
SVSECRET DS    CL1024                                                           
SVOFFBLK DS    CL100               OFICCER BLOCK                                
                                                                                
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
* FACPAK MAINTENANCE SCREEN                                                     
       ++INCLUDE NESFMF7D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
* AUDIENCE ESTIMATOR MAINTENANCE SCREEN                                         
       ++INCLUDE NESFMC5D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMF8D                                                       
         EJECT                                                                  
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUSER                                                      
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDLINKD                                                        
         EJECT                                                                  
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C13 WORK AREA  *******                  
WORKAREA DS    0CL500                                                           
WORK2    DS    CL64                                                             
ADEMFORM DS    A                   A(NEW STYLE DEMO FORMULA TABLE)              
LSTTIAE  DS    A                   LAST ADDRESS IN TIA                          
SVFMTSW  DS    H                                                                
SVMODIF  DS    C                   SAVED MODIFIER                               
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
OPTFAX   DS    C                   OPTION FAX (SHOW ALL RECORDS W/FAX)          
OPTSTER  DS    CL1                 Y=STEREO REQUEST                             
OPTAE    DS    CL1                                                              
OPTAEYQ  EQU   C'Y'                Y=ONLY AE                                    
OPTAENQ  EQU   C'N'                N=ONLY NON-AE                                
OPTVWSC  DS    C                   VIEWING SOURCE                               
OPTVWST  DS    C                   VIEWING TYPE                                 
AEBOOKYM DS    XL2                 AUDIENCE ESTIMATOR BOOK Y/M                  
GAAFLAG  DS    X                   AUDIENCE ESTIMATOR GAA FLAG                  
GAANOQ   EQU   0                   NOT A GAA BASED PROGRAM RECORD               
GAAYESQ  EQU   1                   GAA BASED PROGRAM RECORD                     
*                                                                               
AEFLAGS  DS    X                   AUDIENCE ESTIMATOR FLAGS                     
AEFCSQ   EQU   X'80'               COMSCORE DEMOS                               
AEFCSOQ  EQU   X'40'               COMSCORE ONLY PROGRAM                        
*                                                                               
ANXTMAP  DS    F                   A(NEXT MAP)                                  
*                                                                               
AEVERSN  DS    XL4                 VERSION NUMBER                               
SCANAREA DS    CL28                                                             
SVDMWORK DS    CL96                DEMWORK SAVE AREA                            
TEMPKEY  DS    CL13                                                             
TEMPHEAD DS    XL10                                                             
MAXVPHF  EQU   55                  MAXIMUM VPH FIELDS                           
MAX93VPH EQU   NPG2#VPHS           MAXIMUM VPHS IN RECORD (93 ELEMENT)          
*                                                                               
RSHRFLD  DS    CL7                 RATING/SHARE FIELD                           
SVCSSN   DS    CL10                COMSCORE SERIES NUMBER                       
*                                                                               
PRECFLAG DS    XL1                 RATING PRECISION FLAG                        
PREC1DEC EQU   X'01'               1 DECIMAL PRECISION                          
PREC2DEC EQU   X'02'               2 DECIMAL PRECISION                          
*                                                                               
B1217    DS    F                                                                
K0205    DS    F                                                                
M1299    DS    F                                                                
M2599    DS    F                                                                
M3599    DS    F                                                                
M5099    DS    F                                                                
M6599    DS    F                                                                
P0617    DS    F                                                                
W1299    DS    F                                                                
W2599    DS    F                                                                
W3599    DS    F                                                                
W5099    DS    F                                                                
W6599    DS    F                                                                
*                                                                               
ERRNUM   DS    XL2                                                              
*                                                                               
RELO     DS    A                                                                
ALET     DS    A                 TABS DATASPACE ALET                            
*                                                                               
DTABLES  DS    0F                                                               
ADEMDISP DS    A                   A(DEMDISP TABLES)                            
AAEFORMS DS    A                   A(NEW FORMULA TABLES FOR AE)                 
TABEOT   DS    X                   EOT                                          
DTABLELQ EQU   *-DTABLES                                                        
*                                                                               
WORKAREL EQU   *-WORKAREA                                                       
         EJECT                                                                  
DERR1Q   EQU   1226                                                             
DERR2Q   EQU   1227                                                             
DERR2AQ  EQU   1228                                                             
DERR3Q   EQU   1229                                                             
DERR4Q   EQU   1230                                                             
DERR5Q   EQU   1231                                                             
DERR6Q   EQU   1232                                                             
DERR7Q   EQU   1233                                                             
DERR8Q   EQU   1234                                                             
DERR9Q   EQU   1235                                                             
DERR10Q  EQU   1236                                                             
DERR11Q  EQU   1237                                                             
DERR12Q  EQU   1238                                                             
DERR13Q  EQU   1239                                                             
DERR14Q  EQU   1240                                                             
DERR15Q  EQU   1241                                                             
DERR16Q  EQU   1242                                                             
DERR17Q  EQU   1243                                                             
DERR18Q  EQU   1244                                                             
DERR19Q  EQU   1245                                                             
DERR20Q  EQU   1246                                                             
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
         SPACE 2                                                                
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DDFLDHDR                                                       
LIOBD    DSECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE FASSB                                                          
       ++INCLUDE FASYSFAC                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011NESFM13   06/02/20'                                      
         END                                                                    
