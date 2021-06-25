*          DATA SET SPREPI402  AT LEVEL 011 AS OF 05/01/02                      
*PHASE SPI402A                                                                  
*INCLUDE DAYUNPK                                                                
*INCLUDE TIMVAL                                                                 
         TITLE 'SPI402 - SPOT INVOICE RECORD BUY CREATION'                      
***********************************************************************         
*                                                                               
*        QOPT1- Y=ADD BUYS TO FILE                                              
*                                                                               
*        QOPT2- Y=PRINT BUY RECORD DUMP                                         
*                                                                               
*        QOPT3- Y=USE INVOICE ITEM ESTIMATE NUMBERS                             
*               N=DO NOT USE THEM                                               
*               BLANK=DEFAULT TO I4 PROFILE                                     
*                                                                               
***********************************************************************         
SPI402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPI4                                                           
*                                                                               
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING WKD,RC                                                           
*                                                                               
         RELOC RELO                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,CLTLAST                                                     
         BE    CLTL                                                             
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
         CLI   MODE,ESTFRST                                                     
         BE    ESTF                                                             
         CLI   MODE,PROCBUY                                                     
         BE    PRBUY                                                            
         CLI   MODE,RUNFRST                                                     
         BE    RFRST                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*        RUN FIRST                                                              
RFRST    DS    0H                                                               
         L     RE,SSB                                                           
         OI    3(RE),X'08'         RECOVERY                                     
*                                                                               
         LA    R1,ACONS                                                         
         LA    R0,ACONSN                                                        
         LA    R2,ABUFFC                                                        
*                                                                               
RUNF2    DS    0H                                                               
         L     RF,0(R1)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R2)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,RUNF2                                                         
*                                                                               
*                                  SET BINSRCH PARS FOR ID TABLE                
         SR    R0,R0                                                            
         L     R1,=A(IDTAB)                                                     
         A     R1,RELO                                                          
         SR    R2,R2                                                            
         LA    R3,13                                                            
         LA    R4,1                                                             
         LA    R5,IDTMAX                                                        
         STM   R0,R5,IDPARS                                                     
*                                  SET BINSRCH PARS FOR FILM TABLE              
         SR    R0,R0                                                            
         L     R1,=A(FLMTAB)                                                    
         A     R1,RELO                                                          
         SR    R2,R2                                                            
         LA    R3,11                                                            
         LA    R4,1                                                             
         LA    R5,FLMMAX                                                        
         STM   R0,R5,FLMPARS                                                    
*                                                                               
         B     EXIT                                                             
*                                                                               
ACONS    DS    0F                                                               
         DC    A(BUFFALOC)                                                      
         DC    A(SETBUY)                                                        
         DC    V(TIMVAL)                                                        
         DC    A(INVREC)                                                        
         DC    A(RPRT)                                                          
         DC    A(RDINV)                                                         
         DC    A(DMWRK)                                                         
         DC    A(TIMTDIR)                                                       
*                                                                               
ACONSN   EQU   (*-ACONS)/4                                                      
*                                                                               
         SPACE 2                                                                
*        REQUEST FIRST                                                          
*                                                                               
REQF     DS    0H                                                               
*                                                                               
         L     RF,=A(BUFFALOC)                                                  
         A     RF,RELO                                                          
         ST    RF,ABUFFC                                                        
*                                                                               
         MVI   CSW,C'C'            ONE CLT                                      
         CLC   QCLT,=C'ALL'                                                     
         BNE   REQF4                                                            
         MVI   CSW,C'A'            ALL -EACH                                    
*                                                                               
REQF4    DS    0H                                                               
         XC    RTOTS,RTOTS                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'15'                                                 
         MVC   WORK+6(6),WORK                                                   
         LA    R6,60                                                            
         GOTO1 ADDAY,DMCB,WORK+6,WORK+6,(R6)                                    
         GOTO1 MOBILE,DMCB,(1,WORK),(1,WORK+12)                                 
         MVC   BQSTARTP,WORK+12                                                 
         MVC   BQENDP,WORK+14                                                   
         GOTO1 DATCON,DMCB,(2,WORK+12),(3,BQSTART)                              
         GOTO1 (RF),(R1),,QSTART                                                
         GOTO1 (RF),(R1),(2,WORK+14),(3,BQEND)                                  
         GOTO1 (RF),(R1),,QEND                                                  
*                                                                               
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFFC                                      
*                                                                               
         CLC   QPRD,=C'ALL'        CHANGE ALL TO POL                            
         BNE   *+10                                                             
         MVC   QPRD,=C'POL'                                                     
         CLI   QPRD,C' '                                                        
         BNE   *+10                                                             
         MVC   QPRD,=C'POL'                                                     
*                                                                               
         CLI   QOPT1,C' '                                                       
         BNE   *+8                                                              
         MVI   QOPT1,C'N'                                                       
         MVI   FCRDBUYS,C'N'                                                    
         CLI   QOPT1,C'N'          IF ADDING BUYS                               
         BE    REQF6                                                            
         MVI   FCRDBUYS,C'Y'       THEN  READ BUYS                              
*                                                                               
         CLC   =C'ALL',QMKT                                                     
         BE    REQF20                                                           
         CLC   =C'ALL',QSTA                                                     
         BE    REQF20                                                           
*                                                                               
REQF6    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
REQF20   DS    0H                                                               
         MVC   P(80),QAREA                                                      
         MVC   P+85(14),=C'INVALID REQUEST'                                     
         GOTO1 ARPRT                                                            
         GOTO1 AENDREQ                                                          
*                                                                               
         SPACE 2                                                                
*        CLIENT FIRST                                                           
*                                                                               
CLTF     DS    0H                                                               
         MVI   QESTB,0                                                          
         MVC   IESTOPT,PROGPROF     SET INVOICE ITEM EST OPT                    
         MVC   NOESTERR,PROGPROF+1  SET INVOICE ITEM EST OPT                    
         CLI   QOPT3,C' '                                                       
         BNH   *+10                                                             
         MVC   IESTOPT,QOPT3                                                    
         CLI   IESTOPT,C' '                                                     
         BH    *+8                                                              
         MVI   IESTOPT,C'N'                                                     
*                                                                               
         CLI   IESTOPT,C'N'        IF NOT DOING INV ESTS                        
         BNE   CLTF08                                                           
         CLI   QEST,C' '           EST MUST BE BLANK                            
         BE    CLTF06                                                           
         CLC   QEST,=C'ALL'        OR ALL                                       
         BE    CLTF06                                                           
         CLC   QEST,=C'NO '        OR NO                                        
         BE    CLTF10                                                           
         B     REQF20                                                           
*                                                                               
CLTF06   DS    0H                                                               
         MVC   QEST,=C'NO '        SET EST TO NO                                
         B     CLTF10                                                           
*                                                                               
CLTF08   DS    0H                  DOING INVOICE ESTIMATES                      
         CLC   QEST,=C'ALL'                                                     
         BE    CLTF10                                                           
         CLC   QEST,=C'NO '                                                     
         BE    CLTF10                                                           
         CLI   QEST,C' '                                                        
         BE    CLTF10                                                           
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STC   R0,QESTB                                                         
*                                                                               
CLTF10   DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        FIRST FOR EST                                                          
*                                                                               
ESTF     DS    0H                                                               
         CLI   QOPT1,C'N'          ONLY IF ADDING BUYS                          
         BE    EXIT                                                             
         CLI   IESTOPT,C'Y'        AND NOT USING INVOICE ESTS                   
         BE    EXIT                                                             
         LA    R4,ESTLST           FIND LOWEST ACTIVE EST NUMBER                
         LA    R0,256                                                           
ESTF2    DS    0H                                                               
         CLI   0(R4),0                                                          
         BNE   ESTF4                                                            
ESTF3    LA    R4,1(R4)                                                         
         BCT   R0,ESTF2                                                         
         B     ESTF6                                                            
ESTF4    DS    0H                                                               
         LA    RE,ESTLST                                                        
         LR    R5,R4                                                            
         SR    R5,RE                                                            
         STC   R5,BEST             SAVE NUMBER                                  
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEST,DUB            SET EST IN QEST                              
*                                  READ EST HDR                                 
         L     RF,ADCLT                                                         
         MVC   KEY(13),0(RF)                                                    
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),BEST                                                    
*                                                                               
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
*                                  END DATE MUST FALL WITHIN EST                
         GOTO1 DATCON,DMCB,(3,BQEND),WORK                                       
         L     RF,ADEST                                                         
         LA    RF,EEND-ESTHDR(RF)                                               
         CLC   WORK(6),0(RF)                                                    
         BH    ESTF3               TRY ANOTHER EST                              
*                                                                               
*                                                                               
         B     ESTF7                                                            
ESTF6    DS    0H                                                               
         MVC   P(36),=C'NO ESTIMATE FOUND - REQUEST BYPASSED'                   
         GOTO1 ARPRT                                                            
         GOTO1 AENDREQ                                                          
         SPACE 2                                                                
*                                                                               
ESTF7    DS    0H                                                               
ESTF9    DS    0H                                                               
         B     EXIT                                                             
*                                  PROCESS BUY                                  
         SPACE 2                                                                
PRBUY    DS    0H                                                               
         L     R7,ADBUY                                                         
         USING BUYREC,R7                                                        
         CLC   BDPROGRM(9),=C'AUTO - I4'                                        
         BNE   EXIT                                                             
         CLC   BDSTART,BQEND                                                    
         BH    EXIT                                                             
         CLC   BDEND,BQSTART                                                    
         BL    EXIT                                                             
*                                                                               
         MVC   P(36),=C'RERUN ATTEMPT (EST NNN)-REQ BYPASSED'                   
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+18(3),DUB                                                      
         GOTO1 ARPRT                                                            
         GOTO1 AENDREQ                                                          
         SPACE 2                                                                
*        CLIENT LAST                                                            
*                                                                               
CLTL     DS    0H                                                               
         CLI   QOPT1,C'N'          UNLESS NOT ADDING BUYS                       
         BE    CLTL4                                                            
         CLI   IESTOPT,C'Y'        OR USING INV ITEM ESTS                       
         BE    CLTL4                                                            
         CLI   BEST,0              MUST HAVE EST                                
         BE    ESTF6               NO EST ERROR                                 
*                                                                               
CLTL4    DS    0H                                                               
         MVI   MODE,CLTFRST        FOOL REPORT INTO PRINTING HEADLINES          
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    CTOTS,CTOTS                                                      
*                                                                               
CLTL6    DS    0H                                                               
         GOTO1 ARDINV                                                           
         MVI   MODE,CLTLAST        RESET MODE                                   
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                             INVOICE READER                                    
         SPACE 2                                                                
RDINV    CSECT                                                                  
         NMOD1 0,RDINV                                                          
         LA    R8,1(RB)                                                         
         LA    R8,4095(R8)                                                      
         USING RDINV+4096,R8                                                    
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         LA    R7,KEY                                                           
         USING INVRECD,R7                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         XC    BSTA,BSTA                                                        
         XC    QSTAP,QSTAP                                                      
         CLC   =C'ALL',QSTA                                                     
         BE    RD2                                                              
         GOTO1 MSPACK,DMCB,=C'0000',QSTA,WORK                                   
         MVC   KEY+2(3),WORK+2                                                  
         MVC   QSTAP,WORK+2                                                     
*                                                                               
RD2      DS    0H                                                               
         GOTO1 HIGH                                                             
         MVC   LASTKEY,KEY                                                      
         B     RD4B                                                             
RD4      DS    0H                                                               
         GOTO1 SEQ                                                              
RD4B     DS    0H                                                               
         CLC   KEY(9),LASTKEY      TEST SAME INVOICE                            
         BE    RD9                                                              
         MVC   LASTKEY,KEY                                                      
*                                                                               
         L     RF,ABUFFC           TEST FIRST TIME                              
         LA    RF,BUFFSOFA-BUFFALOD(RF)                                         
         OC    0(4,RF),0(RF)                                                    
         BZ    RD9                                                              
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
RD5      DS    0H                                                               
         XC    THISREC,THISREC                                                  
         XC    LASTREC,LASTREC                                                  
         XC    ITOTS,ITOTS                                                      
         XC    WKCNT,WKCNT                                                      
         XC    SVWKCNTS,SVWKCNTS                                                
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFFC,THISREC,0                           
         B     RD5D                                                             
RD5B     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFFC,THISREC,0                            
RD5D     DS    0H                                                               
         TM    DMCB+8,X'80'                                                     
         BZ    *+8                                                              
         MVI   THISREC,X'FF'                                                    
*                                                                               
         LA    R5,THISREC                                                       
         USING IRECD,R5                                                         
         LA    R6,P                                                             
         USING PLIND,R6                                                         
*                                                                               
         CLC   THISREC(IRKL),LASTREC                                            
         BE    RD7                                                              
*                                                                               
         OC    WKCNT,WKCNT                                                      
         BZ    RD6                                                              
*                                  PRINT COUNT FOR LAST WEEK                    
         MVI   PLCNT,C'('                                                       
         EDIT  WKCNT,(3,PLCNT+1),ALIGN=LEFT                                     
         LA    R1,PLCNT+1                                                       
         AR    R1,R0                                                            
         MVI   0(R1),C')'                                                       
*                                  SAVE COUNT PER WEEK                          
         ZIC   R1,SVWKN2                                                        
         SLL   R1,2                                                             
         LA    R1,SVWKCNTS-4(R1)                                                
         MVC   0(4,R1),WKCNT                                                    
*                                                                               
         XC    WKCNT,WKCNT                                                      
         GOTO1 ARPRT                                                            
*                                                                               
         CLC   THISREC(IRKL-2),LASTREC   TEST NEW BUY                           
         BE    RD6A                                                             
*                                                                               
         LA    R4,PLBUY                                                         
         MVC   0(2,R4),=C'B,'                                                   
*                                                                               
         MVC   2(5,R4),SVWEEKP                                                  
         MVC   7(4,R4),=C'-NW,'                                                 
*                                                                               
         ZIC   RF,SVWKN2                                                        
         ZIC   RE,SVWKN1                                                        
         SR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         STC   RF,NWKS             SAVE NO OF WEEKS                             
         STC   RF,8(R4)                                                         
         OI    8(R4),X'F0'                                                      
*                                  DAY OF WEEK                                  
         GOTO1 =V(DAYUNPK),DMCB,(0,SVSTAT),11(R4),RR=RB                         
         LA    R4,19(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
*                                                                               
         MVC   0(2,R4),=C'1,'                                                   
*                                                                               
         MVI   SKEDSW,C'Y'                                                      
         CLI   NWKS,1                                                           
         BE    RD5D6                                                            
*                                                                               
         MVI   SVNOWK,1            NPW = 1 IF SKED                              
*                                  SEE IF ALL WEEKS THE SAME                    
         ZIC   RF,NWKS                                                          
         ZIC   RE,SVWKN1                                                        
         SLL   RE,2                                                             
         LA    RE,SVWKCNTS-4(RE)                                                
         LR    R0,RF                                                            
         BCTR  R0,R0                                                            
*                                                                               
RD5D2    DS    0H                                                               
         CLC   0(4,RE),4(RE)                                                    
         BNE   RD5E                                                             
         LA    RE,4(RE)                                                         
         BCT   R0,RD5D2                                                         
*                                                                               
*                                  PUT NPW IN LINE-NOT SKED                     
RD5D6    DS    0H                                                               
         MVI   SKEDSW,C'N'                                                      
         ZIC   R3,SVWKN1                                                        
         SLL   R3,2                                                             
         LA    R3,SVWKCNTS-4(R3)                                                
         MVC   SVNOWK,3(R3)        SAVE NPW                                     
         EDIT  (B4,0(R3)),(2,0(R4)),ALIGN=LEFT                                  
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         B     RD5E2                                                            
RD5E     DS    0H                                                               
*                                                                               
         LA    R4,2(R4)                                                         
RD5E2    DS    0H                                                               
*                                                                               
*              GET DPT TIMES                                                    
         L     R2,ATIMTAB                                                       
RD5F     DS    0H                                                               
         CLC   SVSTAT,0(R2)                                                     
         BNE   RD65                                                             
         CLC   SVDPT,3(R2)                                                      
         BE    RD5H                                                             
RD65     DS    0H                                                               
         LA    R2,TIMTABL(R2)                                                   
         B     RD5F                                                             
*                                                                               
RD5H     DS    0H                                                               
         MVC   0(10,R4),4(R2)                                                   
         LA    R4,10(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C','                                                       
*                                                                               
         MVC   2(1,R4),SVDPT                                                    
         MVI   3(R4),C','                                                       
*                                                                               
         EDIT  (B1,SVLEN),(3,4(R4)),ALIGN=LEFT                                  
         LA    R4,4(R4)                                                         
         AR    R4,R0                                                            
*                                                                               
         MVC   0(9,R4),=C',AUTO-I4,'                                            
         LA    R4,9(R4)                                                         
*                                  COST                                         
*                                  FIRST TEST SPECIAL RATE TYPE                 
         L     RF,ADEST            LOOK FOR EST LEVEL OVERRIDE                  
         LA    RF,ERATE-ESTHDR(RF)                                              
         CLI   0(RF),C'*'          OVERRIDE TO NONE                             
         BE    RD5I                                                             
         CLI   0(RF),C'0'                                                       
         BH    RD5H3                                                            
*                                                                               
RD5H2    DS    0H                                                               
         L     RF,ADCLT            THEN TRY CLIENT HEADER                       
         LA    RF,CPROF+14-CLTHDR(RF)                                           
         CLI   0(RF),C'*'                                                       
         BE    RD5I                                                             
         CLI   0(RF),C'0'                                                       
         BNH   RD5I                                                             
RD5H3    DS    0H                                                               
         MVC   BYTE,0(RF)                                                       
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,8              8 COST TYPE CODES                            
         BH    RD5I                                                             
*                                                                               
         ZIC   RE,BYTE             PICK UP CODE FROM TABLE                      
         LA    RE,CPTAB-1(RE)                                                   
         MVC   0(1,R4),0(RE)                                                    
         LA    R4,1(R4)                                                         
         B     RD5I                                                             
*                                                                               
CPTAB    DC    C'SFNQVXPC'         COST TYPE CODES (ONLY 1-8 USED)              
*                                                                               
RD5I     DS    0H                                                               
         EDIT  (B3,SVCOST),(10,0(R4)),2,MINUS=YES,ALIGN=LEFT                    
         LA    R4,10(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LR    RF,R4                                                            
         SH    RF,=H'3'                                                         
         CLC   1(3,RF),=C'.00'                                                  
         BNE   RD5J                                                             
         MVC   1(3,RF),SPACES                                                   
         LR    R4,RF                                                            
RD5J     DS    0H                                                               
         MVC   1(3,R4),=C',M='                                                  
         MVC   4(7,R4),SVPROD                                                   
*                                                                               
         CLI   SVIDNAME,C' '                                                    
         BNH   RD5K                                                             
         LA    R4,P2+PLBUY-PLIND                                                
         MVC   0(3,R4),=C'ID='                                                  
         MVC   3(12,R4),SVIDNAME                                                
*                                  PRINT WEEKS SCHED                            
RD5K     DS    0H                                                               
         CLI   SKEDSW,C'Y'                                                      
         BNE   RD5P                                                             
         ZIC   RE,NWKS                                                          
*                                                                               
         ZIC   R3,SVWKN1                                                        
         SLL   R3,2                                                             
         LA    R3,SVWKCNTS-4(R3)                                                
*                                                                               
         LA    R4,P2+PLBUY-PLIND                                                
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,132(R4)          NEXT LINE                                    
         MVC   0(7,R4),=C'C,SKED='                                              
         LA    R4,7(R4)                                                         
RD5M     DS    0H                                                               
         EDIT  (B4,0(R3)),(2,0(R4)),ALIGN=LEFT                                  
*                                                                               
         CLI   0(R4),C' '                                                       
         BH    *+12                                                             
         MVI   0(R4),C'0'                                                       
         LA    R0,1                                                             
         AR    R4,R0                                                            
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         LA    R3,4(R3)                                                         
         BCT   RE,RD5M                                                          
         BCTR  R4,R0                                                            
         MVI   0(R4),C' '                                                       
*                                                                               
RD5P     DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 ARPRT                                                            
         GOTO1 ASETBUY,DMCB,3      FINISH UP BUY                                
*                                                                               
         CLI   NOBUY,C'Y'          TEST ERROR                                   
         BNE   RD5R                                                             
*                                                                               
         MVC   P(27),=C'INVALID EST-  BUY NOT ADDED'                            
         GOTO1 ARPRT                                                            
         XC    BTOTS,BTOTS                                                      
         B     RD5Z                                                             
*                                                                               
RD5R     DS    0H                                                               
         CLI   QOPT1,C'Y'          IS IT A TEST                                 
         BNE   RD5T                YES SKIP LINE NUM                            
         L     RF,ADBUY                                                         
         ZIC   R0,10(RF)                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   PLBUY(5),=C'LINE='                                               
         UNPK  PLBUY+5(3),DUB                                                   
RD5T     DS    0H                                                               
         MVC   PLBUY+10(5),=C'COST='                                            
         EDIT  BDOLLS,(14,PLBUY+15),2,COMMAS=YES,FLOAT=$,ALIGN=LEFT             
         MVC   PLBUY+30(6),=C'SPOTS='                                           
         EDIT  BSPTS,(10,PLBUY+37),COMMAS=YES,ALIGN=LEFT                        
         MVI   SPACING,2                                                        
         GOTO1 ARPRT                                                            
*                                  ADD BUY AMOUNTS TO INVOICE TOTALS            
         L     R0,IDOLLS                                                        
         A     R0,BDOLLS                                                        
         ST    R0,IDOLLS                                                        
         L     R0,ISPTS                                                         
         A     R0,BSPTS                                                         
         ST    R0,ISPTS                                                         
         L     RF,IBUYS                                                         
         LA    RF,1(RF)                                                         
         ST    RF,IBUYS                                                         
         XC    BTOTS,BTOTS                                                      
*                                                                               
RD5Z     DS    0H                                                               
         XC    SVWKCNTS,SVWKCNTS                                                
RD6      DS    0H                                                               
         CLI   THISREC,X'FF'            END                                     
         BE    RD8                                                              
*                                                                               
         CLC   IPRD,LASTREC                                                     
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
*                                  NEW BUY                                      
         MVC   SVWKN1,IWKN         SAVES                                        
         MVC   SVCOST,ICOST                                                     
         MVC   SVLEN,ILEN                                                       
         MVC   SVPROD,IPRD                                                      
         MVC   SVEST,IEST                                                       
         MVC   SVSTAT,ISTAT                                                     
         MVC   SVWEEK,IWEEK                                                     
         MVC   SVIDNAME,IIDNAME                                                 
         MVC   SVDPT,IDPT                                                       
         MVC   PLPRD,IPRD                                                       
         SR    R0,R0                                                            
         ICM   R0,1,IEST           USE IEST IF THERE                            
         BNE   *+8                                                              
         IC    R0,BEST             ELSE BEST                                    
         EDIT  (R0),(3,PLEST)                                                   
         EDIT  (B1,ILEN),(3,PLLEN)                                              
         EDIT  (B3,ICOST),(10,PLCOST),2,MINUS=YES,COMMAS=YES                    
         MVC   PLDPT,IDPT                                                       
*                                                                               
         GOTO1 ASETBUY,DMCB,1      INITIALIZE BUY REC                           
*                                                                               
RD6A     DS    0H                                                               
         TM    ISTAT,X'7C'         TEST M-F (= MON)                             
         BZ    RD6A2                                                            
*                                  YES - DATE IS OK                             
         MVC   SVWEEKA,IWEEK                                                    
         GOTO1 DATCON,DMCB,(2,IWEEK),(5,PLWEEK)                                 
         B     RD6D                                                             
*                                                                               
RD6A2    DS    0H                  ELSE ADJUST FOR DAY                          
*                                                                               
         LA    R0,5                                                             
         TM    ISTAT,X'02'         SAT                                          
         BNZ   *+8                                                              
         LA    R0,6                SUN                                          
         GOTO1 DATCON,DMCB,(2,IWEEK),WORK                                       
         GOTO1 ADDAY,(R1),WORK,,(R0)                                            
         GOTO1 DATCON,(R1),,(5,PLWEEK)                                          
         GOTO1 (RF),(R1),,(2,SVWEEKA)     ADJUSTED WEEK                         
*                                                                               
RD6D     DS    0H                                                               
         CLC   SVWKN1,IWKN         IF FIRST WEEK                                
         BNE   *+10                                                             
         MVC   SVWEEKP,PLWEEK SAVE PRINT WEEK                                   
         MVC   SVWKN2,IWKN         SAVE LAST WK NO                              
*                                                                               
*                                  DATE + DAY                                   
RD7      DS    0H                                                               
         MVC   LASTREC,THISREC                                                  
*                                                                               
         MVC   PLFILM(8),IFILMC    FILM CODE(S)                                 
         CLI   IFILMC2,C' '                                                     
         BNH   *+14                                                             
         MVI   PLFILM+8,C'-'                                                    
         MVC   PLFILM+9(8),IFILMC2                                              
*                                                                               
         GOTO1 DATCON,DMCB,(2,IDATE),(4,PLDATE)                                 
         MVC   PLDAY,IDAY                                                       
*                                  TIME                                         
         MVC   WORK(2),ITIME                                                    
         XC    WORK+2(2),WORK+2                                                 
         CLC   WORK(2),=H'2400'                                                 
         BNH   RD7B                                                             
         LH    R0,WORK                                                          
         SH    R0,=H'2400'                                                      
         STH   R0,WORK                                                          
RD7B     DS    0H                                                               
         GOTO1 UNTIME,DMCB,WORK,PLTIME                                          
*                                                                               
*                                  ADD TO INVOICE TOTALS                        
         MVC   DUB(3),ICOST                                                     
         L     R0,DUB                                                           
         SRA   R0,8                                                             
         A     R0,BDOLLS                                                        
         ST    R0,BDOLLS                                                        
*                                                                               
         L     R1,BSPTS                                                         
         LA    R1,1(R1)                                                         
         ST    R1,BSPTS                                                         
*                                                                               
         L     R1,WKCNT            BUMP NUM/WEEK COUNT                          
         LA    R1,1(R1)                                                         
         ST    R1,WKCNT                                                         
*                                                                               
         GOTO1 ARPRT                                                            
*                                                                               
         GOTO1 ASETBUY,DMCB,2      ADD SPOT + AFFID ELEMS TO BUY                
*                                                                               
         B     RD5B                                                             
*                                                                               
*                                                                               
RD8      DS    0H                                                               
         L     R1,CTOTS            COUNT INVOICES                               
         LA    R1,1(R1)                                                         
         ST    R1,CTOTS                                                         
*                                                                               
         LA    R0,3                                                             
         LA    RE,CTOTS+4                                                       
         LA    RF,ITOTS                                                         
RD8B     DS    0H                                                               
         L     R1,0(RE)                                                         
         A     R1,0(RF)                                                         
         ST    R1,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,RD8B                                                          
*                                                                               
         GOTO1 ARPRT                                                            
*                                                                               
         MVC   P+10(14),=C'INVOICE TOTALS'                                      
         MVC   P+30(12),=C'NO. OF BUYS='                                        
         MVC   P2+30(13),=C'NO. OF SPOTS='                                      
         MVC   P3+30(11),=C'GROSS COST='                                        
         EDIT  IBUYS,(4,P+44),ALIGN=LEFT                                        
         EDIT  ISPTS,(4,P2+44),ALIGN=LEFT                                       
         EDIT  IDOLLS,(12,P3+44),2,FLOAT=$,ALIGN=LEFT,MINUS=YES,       X        
               COMMAS=YES                                                       
*                                                                               
         GOTO1 ARPRT                                                            
         XC    ITOTS,ITOTS                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',ABUFFC                                    
*                                                                               
*                                                                               
*                                  PROCESS FILE INVOICE RECORDS                 
*                                  ----------------------------                 
*                                                                               
RD9      DS    0H                                                               
         CLC   KEY(2),KEYSAVE      AGY/MED                                      
         BNE   RD40                                                             
         CLC   =C'ALL',QSTA                                                     
         BE    RD9B                                                             
         CLC   QSTAP,KEY+2                                                      
         BNE   RD40                                                             
*                                                                               
RD9B     DS    0H                                                               
         CLC   INVKCLT,BCLT                                                     
         BNE   RD4                                                              
         CLC   INVKDAT,BQSTARTP                                                 
         BL    RD4                                                              
         CLC   INVKDAT,BQENDP                                                   
         BH    RD4                                                              
*                                                                               
RD10     DS    0H                                                               
         MVC   AREC,ADINV                                                       
         GOTO1 GET                                                              
*                                                                               
         L     R6,ADINV                                                         
         CLC   BSTA,2(R6)          TEST SAME STATION                            
         BE    RD10C                                                            
*                                  NEW STATION                                  
         XC    IDPARS+8(4),IDPARS+8    CLEAR ID BINSRCH PARS                    
         XC    FLMPARS+8(4),FLMPARS+8  CLEAR FILM BINSRCH PARS                  
*                                                                               
         MVC   BSTA,2(R6)                                                       
         MVC   WORK,KEY                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
*                                                                               
*                                  READ STATION FOR MKT                         
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         GOTO1 MSUNPK,DMCB,(R6),FULL,W                                          
         CLI   W+4,C' '                                                         
         BNE   *+8                                                              
         MVI   W+4,C'T'                                                         
         MVC   KEY+2(5),W                                                       
         MVC   STAPRINT(4),W                                                    
         MVC   STAPRINT+4(3),=C'-TV'                                            
         CLI   W+4,C'T'                                                         
         BE    RD10B2                                                           
         CLI   W+4,C'N'                                                         
         BE    RD10B2                                                           
         MVC   STAPRINT+5(1),W+4                                                
         MVI   STAPRINT+6,C'M'                                                  
*                                                                               
RD10B2   DS    0H                                                               
         MVC   KEY+7(2),AGY                                                     
         MVC   KEY+9(3),CLT                                                     
*                                                                               
         GOTO1 READSTA                                                          
         L     RF,ADSTAT                                                        
         MVC   MKT,SMKT-STAREC(RF)                                              
*                                  READ MARKET                                  
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),MKT                                                     
         MVC   KEY+6(2),AGY                                                     
*                                                                               
         GOTO1 READMKT                                                          
         L     RF,ADMARKET                                                      
         MVC   MKTNM,MKTNAME-MKTREC(RF)                                         
*                                                                               
         MVC   KEY(64),WORK        RESTORE KEY                                  
RD10C    DS    0H                                                               
*                                                                               
         L     R6,ADINV                                                         
*                                                                               
         LA    R5,INVKDAT-INVKEY(R6)                                            
         GOTO1 DATCON,DMCB,(2,(R5)),WORK                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6,15                                        
         GOTO1 DATCON,DMCB,WORK+6,(6,HLDMOS)                                    
         GOTO1 ADDAY,DMCB,WORK,WORK+6,35                                        
         GOTO1 MOBILE,DMCB,(6,WORK),(4,MONLIST)                                 
*                                                                               
         LA    R6,24(R6)                                                        
*                                                                               
RD10D    DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    RD14                                                             
         CLI   0(R6),X'B1'         INVOICE ITEM ELEM                            
         BE    RD12                                                             
         CLI   0(R6),X'02'         ID TRANS ELEM                                
         BE    RD11F                                                            
         CLI   0(R6),X'04'         FILM TRANS ELEM                              
         BE    RD11J                                                            
*                                                                               
RD11     DS    0H                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RD10D                                                            
*                                                                               
RD11F    DS    0H                  ID TRANS ELEMS                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),14(R6)      INTERNAL CODE                                
         ZIC   RF,1(R6)                                                         
         SH    RF,=H'4'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),2(R6)    ID NAME                                       
         GOTO1 BINSRCH,IDPARS,(1,WORK)                                          
         OC    1(3,R1),1(R3)       TEST TABLE FULL                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     RD11                                                             
*                                                                               
RD11J    DS    0H                  FILM TRANS ELEMS                             
         XC    WORK,WORK                                                        
         MVC   WORK(11),2(R6)      FILM INTERNAL CODE+CODE+CMML SEQ             
         GOTO1 BINSRCH,FLMPARS,(1,WORK)                                         
         OC    1(3,R1),1(R3)       TEST TABLE FULL                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     RD11                                                             
*                                                                               
RD12     DS    0H                                                               
         USING INVELEM,R6                                                       
         LA    R5,THISREC                                                       
         USING IRECD,R5                                                         
         XC    THISREC,THISREC                                                  
*                                                                               
         MVC   IPRD,SPACES                                                      
         LA    R2,INVPRD                                                        
         LA    R3,IPRD                                                          
         BAS   RE,GTPRD                                                         
         CLC   QPRD,=C'POL'                                                     
         BE    RD12A                                                            
         CLC   IPRD(3),QPRD        FILTER ON PROD                               
         BNE   RD11                                                             
*                                                                               
RD12A    DS    0H                                                               
         TM    INVSTAT,X'48'       TEST EST OR ID INV                           
         BNZ   RD12D                                                            
*                                                                               
         CLI   INVPRD2,0                                                        
         BE    RD12D                                                            
*                                                                               
RD12B    DS    0H                                                               
         LA    R2,INVPRD2                                                       
         LA    R3,IPRD+4                                                        
         BAS   RE,GTPRD                                                         
         MVI   IPRD+3,C'-'                                                      
*                                                                               
RD12D    DS    0H                                                               
         CLI   IESTOPT,C'Y'                                                     
         BNE   RD12D1N                                                          
         TM    INVSTAT,X'40'       IS IT AN EST INV ITEM                        
         BZ    RD12D1L             NO, REJECT ITEM                              
         MVC   IEST,INVPRD2        ELSE, PRD2 IS EST                            
         CLI   QESTB,0                                                          
         BE    RD12D1N                                                          
         CLC   IEST,QESTB          IS IT RIGHT ESTIMATE                         
         BNE   RD11                NO, REJECT                                   
         B     RD12D1N                                                          
*                                                                               
RD12D1L  DS    0H                  NO EST ON INVOICE ITEM                       
         CLI   QESTB,0             SINGLE EST REQ?                              
         BNE   RD11                YES, REJECT SPOT                             
*                                  IF DOING ALL ESTS, PRINT ERROR               
         CLI   NOESTERR,C'Y'       IF PROFILE OPTION SET                        
         BNE   RD11                                                             
         MVC   P(45),=C'** INVOICE ITEM WITHOUT ESTIMATE -BYPASSED **'          
         LA    R3,P+47                                                          
         GOTO1 DATCON,DMCB,(2,INVDAT),(4,0(R3))                                 
         MVC   7(7,R3),IPRD                                                     
         GOTO1 ARPRT                                                            
         B     RD11                                                             
*                                                                               
RD12D1N  DS    0H                                                               
         MVC   ILEN,INVLEN         LENGTH                                       
         CLI   ILEN,0              MUST NOT BE ZERO                             
         BNE   RD12D1P                                                          
         MVC   P(38),=C'INVALID SPOT LENGTH - REQUEST BYPASSED'                 
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
RD12D1P  DS    0H                                                               
         MVC   ICOST,INVCOST       COST                                         
*                                  GET DPT FROM TABLE                           
*                                  FIRST FIND RIGHT TABLE                       
         MVC   WORK,SPACES                                                      
         MVC   WORK(3),QAGY                                                     
         L     RF,ADMARKET                                                      
         LA    RF,MKTZONE-MKTREC(RF)                                            
         MVC   WORK+3(1),0(RF)                                                  
         L     RF,ADSTAT                                                        
         LA    RF,SNETWRK-STAREC(RF)                                            
         MVC   WORK+4(1),0(RF)                                                  
         CLI   0(RF),C'I'            IF NOT INDEPENDENT                         
         BE    RD12D1R                                                          
         MVI   WORK+4,C'N'           SET TO NETWORK                             
         CLI   PROGPROF+3,C'Y'       TEST FOX OPTION                            
         BNE   RD12D1R                                                          
         CLC   0(3,RF),=C'FOX'                                                  
         BNE   RD12D1R                                                          
         MVI   WORK+4,C'F'         SPECIAL DAYPARTS FOR FOX                     
*                                                                               
RD12D1R  DS    0H                                                               
         L     R2,ATIMTDIR                                                      
*                                                                               
RD12D2   DS    0H                                                               
         CLI   0(R2),X'FF'         EOL                                          
         BE    RD12D4                                                           
         CLC   WORK(8),0(R2)       TOTAL MATCH                                  
         BE    RD12E                                                            
         CLC   WORK(2),0(R2)       AGENCY                                       
         BE    *+14                                                             
         CLC   0(2,R2),=C'AA'      DEFAULT TO AA (4 A'S)                        
         BNE   RD12D3                                                           
         CLC   WORK+2(1),2(R2)     MEDIA                                        
         BNE   RD12D3                                                           
         CLC   WORK+3(1),3(R2)     TIME ZONE                                    
         BE    *+12                                                             
         CLI   3(R2),C'Z'          Z = ALL                                      
         BNE   RD12D3                                                           
         CLC   WORK+4(1),4(R2)     STATION TYPE                                 
         BE    *+12                                                             
         CLI   4(R2),C'Z'          Z = ALL                                      
         BNE   RD12D3                                                           
         B     RD12E                                                            
*                                                                               
RD12D3   DS    0H                                                               
         LA    R2,12(R2)                                                        
         B     RD12D2                                                           
*                                                                               
RD12D4   DS    0H                                                               
         MVC   P(67),=C'NO DAYPART TABLE FOR THIS TIME ZONE/STATION TYPX        
               E - REQUEST BYPASSED'                                            
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
RD12E    DS    0H                                                               
         L     R2,8(R2)                                                         
         A     R2,RELO                                                          
         ST    R2,ATIMTAB          SAVE A(RIGHT TIME TABLE)                     
*                                                                               
         CLC   QAGY,=C'MC'          FOR MCCANN                                  
         BNE   RD12E4                                                           
*                                                                               
RD12E3   DS    0H                                                               
         OC    ICOST,ICOST         IF COST = ZERO                               
         BNZ   RD12E4                                                           
         MVI   IDPT,C'B'           SET BONUS DAYPART                            
         MVI   ISTAT,X'7F'                                                      
         B     RD12H4                                                           
*                                                                               
RD12E4   DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,INVDAT),WORK                                      
         GOTO1 GETDAY,DMCB,WORK,IDAY                                            
*                                                                               
         ZIC   R1,DMCB                                                          
         LA    R1,DAYLIST-1(R1)                                                 
         MVC   IDAYB,0(R1)         DAY BIT                                      
*                                                                               
         ZIC   R1,IDAYB                                                         
RD12F    DS    0H                                                               
         CLI   0(R2),X'FF'         EOT                                          
         BNE   *+6                                                              
         DC    H'0'                NO DAYPART FOUND FOR DAY/TIME                
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(R2),0             TEST DAY                                     
         BZ    RD12G                                                            
         CLC   INVTIM,1(R2)                                                     
         BNH   RD12H                                                            
RD12G    DS    0H                                                               
         LA    R2,TIMTABL(R2)                                                   
         B     RD12F                                                            
*                                                                               
RD12H    DS    0H                                                               
         MVC   IDPT,3(R2)                                                       
         MVC   ISTAT,0(R2)                                                      
*                                                                               
RD12H4   DS    0H                                                               
         LA    R2,MONLIST                                                       
*                                  GET MONDAY DATE                              
RD12J    DS    0H                                                               
         CLC   INVDAT,2(R2)                                                     
         BNH   RD12L                                                            
         LA    R2,4(R2)                                                         
         B     RD12J                                                            
*                                                                               
RD12L    DS    0H                                                               
         MVC   IWEEK,0(R2)         START OF WEEK                                
         LA    R0,MONLIST                                                       
         SR    R2,R0                                                            
         SRL   R2,2                                                             
         LA    R2,1(R2)                                                         
         STC   R2,IWKN             WEEK NUMBER                                  
*                                                                               
         MVC   IDATE,INVDAT        DATE                                         
         MVC   ITIME,INVTIM        TIME                                         
         MVC   IPRDB(2),INVPRD     PRD CODES                                    
         TM    INVSTAT,X'48'       TEST EST INV OR ID INV                       
         BZ    *+8                                                              
         MVI   IPRDB2,0            NO 2ND PRD                                   
*                                                                               
         TM    INVSTAT,X'08'       TEST ID PRESENT                              
         BZ    RD12P                                                            
*                                                                               
         MVC   WORK(1),INVPRD2     2ND PRD IS ID CODE                           
         GOTO1 BINSRCH,IDPARS,WORK                                              
         CLI   0(R1),1             TEST FOUND                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         MVC   IIDNAME,1(RF)       SET ID NAME                                  
*                                                                               
RD12P    DS    0H                                                               
         CLI   INVFILM,0           TEST HAVE FILM CODE                          
         BE    RD12R                                                            
         MVC   WORK(1),INVFILM                                                  
         GOTO1 BINSRCH,FLMPARS,WORK                                             
         CLI   0(R1),1             TEST FOUND                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         MVC   IFILMC,1(RF)       SET FILM CODE                                 
         MVC   IFLMSQ,9(RF)       SET CMML SEQ NO                               
*                                                                               
RD12R    DS    0H                                                               
         CLI   INVFILM2,0         TEST HAVE 2ND FILM CODE                       
         BE    RD12T                                                            
         MVC   WORK(1),INVFILM2                                                 
         GOTO1 BINSRCH,FLMPARS,WORK                                             
         CLI   0(R1),1             TEST FOUND                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         MVC   IFILMC2,1(RF)       SET FILM CODE                                
         MVC   IFLMSQ2,9(RF)       SET CMML SEQ NO                              
*                                                                               
RD12T    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFC,THISREC                              
         B     RD11                                                             
*                                                                               
RD14     DS    0H                                                               
         B     RD4                                                              
*                                                                               
RD30     DS    0H                                                               
         B     RD4                                                              
*                                                                               
RD40     DS    0H                                                               
         LA    R4,CTOTS                                                         
         BAS   RE,PRTOT                                                         
*                                                                               
         XC    CTOTS,CTOTS                                                      
         B     RDEXT                                                            
         SPACE 3                                                                
PRTOT    NTR1                                                                   
         GOTO1 ARPRT                                                            
         MVC   P+10(10),=C'**TOTALS**'                                          
         MVC   P+30(09),=C'INVOICES='                                           
         EDIT  (B4,0(R4)),(8,P+44),ALIGN=LEFT                                   
         MVC   P2+30(12),=C'NO. OF BUYS='                                       
         EDIT  (B4,4(R4)),(8,P2+44),ALIGN=LEFT                                  
         MVC   P3+30(13),=C'NO. OF SPOTS='                                      
         EDIT  (B4,8(R4)),(8,P3+44),ALIGN=LEFT                                  
         MVC   P4+30(11),=C'GROSS COST='                                        
         EDIT  (B4,12(R4)),(14,P4+44),2,FLOAT=$,ALIGN=LEFT,COMMAS=YES           
         GOTO1 ARPRT                                                            
         B     RDEXT                                                            
RDEXT    DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
GTPRD    DS    0H                  GET PRODUCT CODE                             
         L     R1,ADCLT                                                         
         LA    R1,CLIST-CLTHDR(R1)                                              
*                                                                               
GP4      DS    0H                                                               
         CLI   0(R1),0                                                          
         BER   RE                                                               
         CLC   0(1,R2),3(R1)                                                    
         BE    GP5                                                              
         LA    R1,4(R1)                                                         
         B     GP4                                                              
*                                                                               
GP5      DS    0H                                                               
         MVC   0(3,R3),0(R1)                                                    
         BR    RE                                                               
         SPACE 3                                                                
DAYLIST  DC    X'4020100804020100'                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                  PRINT ROUTINE                                
         SPACE 2                                                                
RPRT     CSECT                                                                  
         NMOD1 0,RPRT                                                           
RP4      LA    RC,SPACEND                                                       
*                                                                               
         MVC   HEAD4+49(7),STAPRINT                                             
         MVC   HEAD4+60(6),HLDMOS                                               
*                                                                               
         MVC   HEAD7+61(9),=C'TIME ZONE'                                        
         L     RF,ADMARKET                                                      
         LA    RF,MKTZONE-MKTREC(RF)                                            
         MVC   HEAD7+61+10(1),0(RF)                                             
         LA    RF,=C'LIST'                                                      
         CLI   QOPT1,C'N'                                                       
         BE    RP7                                                              
         LA    RF,=C'TEST'                                                      
         CLI   QOPT1,C'T'                                                       
         BE    RP7                                                              
         LA    RF,=C'LIVE'                                                      
RP7      DS    0H                                                               
         MVC   HEAD6(4),0(RF)                                                   
         MVC   HEAD6+5(4),=C'MODE'                                              
*                                                                               
         MVC   HEAD7(14),=C'BUYS NOT ADDED'                                     
         LA    RF,HEAD7+15                                                      
         CLI   QOPT1,C'Y'                                                       
         BNE   RP8                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   RP8                                                              
         MVC   HEAD7+5(6),=C'ADDED '                                            
         LA    RF,HEAD7+11                                                      
RP8      DS    0H                                                               
         MVC   0(7,RF),=C'TO FILE'                                              
*                                                                               
         GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 3                                                                
         EJECT                                                                  
*        SETBUY - SET BUY RECORD                                                
         SPACE 2                                                                
SETBUY   CSECT                                                                  
         NMOD1 0,SETBUY                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         CLI   QOPT1,C'N'          TEST ADDING BUYS                             
         BE    SBX                                                              
         USING IRECD,R5                                                         
         L     R7,ADBUY                                                         
         USING BUYREC,R7                                                        
*                                                                               
         CLI   DMCB+3,1            INITIALIZE                                   
         BE    SB3                                                              
         CLI   DMCB+3,2            SPOT                                         
         BE    SB30                                                             
         CLI   DMCB+3,3            FINISH BUY                                   
         BE    SB60                                                             
*                                                                               
         EJECT                                                                  
*        INITIALIZE BUY                                                         
         SPACE 2                                                                
SB3      DS    0H                                                               
         MVI   NOBUY,C'N'                                                       
         LR    RE,R7                                                            
         LA    RF,2000                                                          
         XCEF                                                                   
*                                       BUILD KEY                               
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
         MVI   BUYKPRD,X'FF'                                                    
         PACK  DUB,MKT                                                          
         CVB   R0,DUB                                                           
         STCM  R0,3,BUYMSTA                                                     
         MVC   BUYMSTA+2(3),BSTA                                                
         CLI   IEST,0              IF HAVE INVOICE ITEM EST                     
         BE    *+10                                                             
         MVC   BEST,IEST           SET BEST ALSO                                
         MVC   BUYKEST,BEST                                                     
         CLI   BUYKEST,0           CATCH BUG OF EST=0 BUYS                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                       READ TO GET NEXT LINE NO.               
         MVC   HOLDKEY,KEY                                                      
         SR    R3,R3                                                            
         MVC   KEY(13),BUYREC                                                   
SB4      DS    0H                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         B     SB4D                                                             
SB4B     DS    0H                                                               
         GOTO1 SEQ                                                              
SB4D     DS    0H                                                               
         CLC   KEY(10),KEYSAVE                                                  
         BNE   SB6                                                              
         ZIC   R3,KEY+11           KEEP LINE NO                                 
         B     SB4B                                                             
SB6      DS    0H                                                               
         CH    R3,=H'255'                                                       
         BL    SB6A                                                             
         MVC   P(46),=C'**MAXIMUM LINES EXCEEDED** -REQUEST TERMINATED'         
         MVC   P2(80),QAREA                                                     
         GOTO1 ARPRT                                                            
         GOTO1 AENDREQ                                                          
*                                                                               
SB6A     DS    0H                                                               
         LA    R3,1(R3)                                                         
         STC   R3,BUYKBUY                                                       
         MVI   BUYKBUY+1,1                                                      
*                                                                               
*                                  SEE IF ALL ESTS ON FILE                      
         CLI   BEST,0                                                           
         BE    SB7                                                              
         L     RF,ADCLT                                                         
         MVC   KEY(13),0(RF)                                                    
         MVC   KEY+4(3),IPRD                                                    
         MVC   KEY+7(1),BEST                                                    
SB6B     DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    SB6C                                                             
SB6B2    DS    0H                                                               
         MVI   NOBUY,C'Y'                                                       
         B     SB7                                                              
SB6C     DS    0H                                                               
         CLI   IESTOPT,C'Y'        IF DOING INVOICE ITEM ESTS                   
         BNE   SB6D                                                             
         GOTO1 GETEST              CHECK EST START-END DATES                    
         L     R2,ADEST                                                         
         CLC   QSTART,ESTART-ESTHDR(R2)                                         
         BL    SB6B2                                                            
         CLC   QEND,EEND-ESTHDR(R2)                                             
         BH    SB6B2                                                            
*                                                                               
SB6D     DS    0H                                                               
         CLI   IPRD+4,C' '                                                      
         BNH   SB7                                                              
         CLC   IPRD+4(3),KEYSAVE+4                                              
         BE    SB7                                                              
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+4(3),IPRD+4                                                  
         B     SB6B                                                             
*                                                                               
SB7      DS    0H                                                               
         MVC   BUYALPHA,AGY                                                     
         MVC   BUYRLEN,=H'94'      LEN=24+70                                    
         MVI   BDCODE,X'01'                                                     
         MVI   BDLEN,70                                                         
*                                  START DATE                                   
         TM    ISTAT,X'7C'         TEST M-F                                     
         BZ    SB8                                                              
         GOTO1 DATCON,DMCB,(2,IWEEK),(3,BDSTART)                                
         B     SB9                                                              
*                                                                               
SB8      DS    0H                                                               
         LA    R0,5                                                             
         TM    ISTAT,X'02'         SAT                                          
         BNZ   *+8                                                              
         LA    R0,6                SUN                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,IWEEK),WORK                                       
         GOTO1 ADDAY,(R1),WORK,,(R0)                                            
         GOTO1 DATCON,(R1),,(3,BDSTART)                                         
*                                                                               
SB9      DS    0H                                                               
         MVC   BDEND,BDSTART       SET INITIAL BDEND FOR GETDEMO                
         MVI   BDINPUT,1           WEEKS INPUT                                  
         MVI   BDWKIND,C'O'        1/WK                                         
         MVI   BDSEDAY,0           DONT NEED START/END DAYS                     
         MVC   BDDAY,ISTAT         DAYS                                         
         MVC   BDSEC,ILEN          LENGTH                                       
         MVC   BDDAYPT,IDPT        DAYPART                                      
*                                                                               
         L     R2,ATIMTAB                                                       
SB10     DS    0H                                                               
         CLC   ISTAT,0(R2)                                                      
         BNE   SB11                                                             
         CLC   IDPT,3(R2)                                                       
         BE    SB12                                                             
*                                                                               
SB11     DS    0H                                                               
         LA    R2,TIMTABL(R2)                                                   
         B     SB10                                                             
*                                                                               
SB12     DS    0H                                                               
         MVC   BDTIMST(4),14(R2)   START - END                                  
*                                                                               
         MVC   BDPROGRM(18),=CL18'AUTO - I4'                                    
         MVC   BDCOST,ICOST                                                     
         MVI   BDCIND,X'20'        GROSS                                        
         L     RF,ADEST            LOOK TO EST HDR FIRST FOR RATE TYPE          
         LA    RF,ERATE-ESTHDR(RF)                                              
         CLI   0(RF),C'*'          OVERRIDE TO NO RATE TYPE                     
         BE    SB13                                                             
         CLI   0(RF),C'0'          TEST ANY RATE CONTROL                        
         BH    SB12D               YES, USE IT                                  
*                                  NO, TRY CLIENT                               
         L     RF,ADCLT                                                         
         LA    RF,CPROF+14-CLTHDR(RF)                                           
         CLI   0(RF),C'*'                                                       
         BE    SB13                                                             
         CLI   0(RF),C'0'          TEST ANY RATE CONTROL                        
         BNH   SB13                NO                                           
*                                                                               
SB12D    DS    0H                                                               
         MVC   BYTE,0(RF)                                                       
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,8              8 COST TYPES                                 
         BH    SB13                IGNORE IF INVALID                            
         BNE   *+8                                                              
         OI    BDCIND2,X'80'       TYPE 8 (C) USES BDCIND2 AS WELL              
         ZIC   RE,BYTE                                                          
         LA    RE,COSTAB-1(RE)                                                  
         MVC   BDCIND,0(RE)                                                     
         B     SB13                                                             
*                                                                               
COSTAB   DC    X'0480104008020020'   SFNQVXPC - 8 TYPES                         
*                                                                               
SB13     DS    0H                                                               
         MVC   BDMASPRD,IPRDB                                                   
*                                                                               
         MVC   BDCHG,TODAYB                                                     
         OI    BDWHY,X'80'                                                      
*                                                                               
         L     RF,ADSTAT                                                        
         LA    RF,SNEWTAX-STAREC(RF)                                            
         MVC   BDNTAX,0(RF)                                                     
         DROP  R7                                                               
         EJECT                                                                  
*                                  ADD ID ELEMENT                               
         CLI   IIDNAME,C' '        TEST ANY ID                                  
         BNH   SB13D               NO                                           
         XC    X,X                                                              
         LA    R4,X                ID ELEMENT                                   
         MVI   0(R4),X'70'                                                      
         MVI   1(R4),15                                                         
         MVC   3(12,R4),IIDNAME                                                 
         BAS   RE,SBADEL                                                        
*                                  ADD DEMO ELEM                                
SB13D    DS    0H                                                               
         XC    X,X                                                              
         LA    R4,X                                                             
         L     R7,ADEST                                                         
         USING ESTHDR,R7                                                        
         MVI   0(R4),2                                                          
         MVC   2(2,R4),EBOOK                                                    
         MVC   22(1,R4),EHUTADJ                                                 
*                                                                               
         LA    R5,24(R4)                                                        
         LA    R6,EDEMOS                                                        
         LA    R0,20                                                            
*                                                                               
SB14     DS    0H                                                               
         OC    0(3,R6),0(R6)       EOL                                          
         BZ    SB16                                                             
         MVC   0(3,R5),0(R6)                                                    
         LA    R5,8(R5)                                                         
         LA    R6,3(R6)                                                         
         BCT   R0,SB14                                                          
*                                                                               
SB16     DS    0H                                                               
         SR    R5,R4                                                            
         STC   R5,1(R4)            ELEM LENGTH                                  
*                                                                               
         L     RF,ADCLT                                                         
         MVC   BYTE,CPROF+3-CLTHDR(RF)  RATING SERVICE                          
         NI    BYTE,X'0F'                                                       
         MVC   WORD(1),EHUTADJ                                                  
         L     RE,ADMWRK                                                        
         LA    RF,L'DMWRK                                                       
         XCEF                                                                   
*                                                                               
         BAS   RE,SBADEL                                                        
         XC    ADRLIST,ADRLIST                                                  
         MVC   ADRLIST+8(4),ACOMFACS                                            
         GOTO1 GETDEMO,DMCB,ADBUY,ELAD,(BYTE,WORD),ADMWRK,ADRLIST               
         B     SBX                                                              
         DROP  R7                                                               
         EJECT                                                                  
*        ADD SPOT AND AFFID ELEMS                                               
SB30     DS    0H                                                               
         LA    R7,X                                                             
         XC    X,X                                                              
         USING REGELEM,R7                                                       
*                                                                               
         MVI   RCODE,X'0B'                                                      
         MVI   RLEN,X'0E'                                                       
         CLI   IPRDB2,0                                                         
         BE    *+8                                                              
         MVI   RLEN,X'12'          PIGGY                                        
*                                                                               
         MVC   RDATE,SVWEEKA                                                    
         MVC   RPPRD,IPRDB                                                      
         MVC   RPTIME,ILEN                                                      
*                                                                               
         CLI   IPRDB2,0            PIGGY                                        
         BE    SB34                                                             
         MVC   RPPRD+4,IPRDB2                                                   
         ZIC   RE,ILEN             SPLIT LEN                                    
         SRL   RE,1                                                             
         STC   RE,RPTIME                                                        
         STC   RE,RPTIME+4                                                      
*                                                                               
SB34     DS    0H                                                               
         BAS   RE,SBADEL                                                        
*                                                                               
         XC    X,X                                                              
         USING AFFELEM,R7          AFFIDS                                       
         MVI   ACODE,X'10'                                                      
         MVI   ALEN,X'06'                                                       
         MVC   ADATE,IDATE                                                      
         MVC   ATIME,ITIME                                                      
         BAS   RE,SBADEL                                                        
*                                                                               
         CLI   IFILMC,C' '         TEST ANY FILMS                               
         BNH   SB36                NO                                           
         XC    X,X                                                              
         USING FLMELEM,R7          FILM CODES                                   
         MVI   0(R7),X'12'                                                      
         MVI   1(R7),X'5'                                                       
         MVC   FLMDAY,IDAYB                                                     
         MVC   FLMNUM,IFLMSQ                                                    
         CLI   IFILMC2,C' '        TEST SECOND FILM                             
         BNH   SB35                                                             
         MVC   FLMNUM+2(2),IFLMSQ2                                              
         MVI   1(R7),X'7'          NEW ELEM LENGTH                              
*                                                                               
SB35     DS    0H                                                               
         BAS   RE,SBADEL                                                        
*                                                                               
SB36     DS    0H                                                               
         B     SBX                                                              
         SPACE 2                                                                
SBADEL   NTR1                                                                   
         L     R3,ADBUY                                                         
         MVC   HALF,13(R3)                                                      
         LR    R4,R3                                                            
         AH    R4,HALF                                                          
         ST    R4,ELAD             SAVE ELEM ADDR                               
         GOTO1 RECUP,DMCB,(R3),X,(C'R',(R4))                                    
         CLI   DMCB+8,0            TEST OVERFLOW ERROR                          
         BNE   SBADELX                                                          
         MVC   P(37),=C'*** RECORD OVERFLOW - CONTACT DDS ***'                  
         GOTO1 REPORT                                                           
*                                                                               
SBADELX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        FINISH BUY                                                             
         SPACE 2                                                                
SB60     DS    0H                                                               
         L     R7,ADBUY                                                         
         USING BUYREC,R7                                                        
         GOTO1 DATCON,DMCB,(2,SVWEEK),WORK                                      
         ZIC   R2,NWKS                                                          
         MH    R2,=H'7'                                                         
         BCTR  R2,R0                                                            
         ZIC   R0,SVSTAT                                                        
         SR    R1,R1                                                            
SB62     DS    0H                                                               
         SRDL  R0,1                                                             
         LTR   R1,R1                                                            
         BM    SB64                                                             
         BCTR  R2,R0                                                            
         B     SB62                                                             
*                                                                               
SB64     DS    0H                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK,(R2)                                        
         GOTO1 DATCON,DMCB,WORK,(3,BDEND)                                       
*                                                                               
         MVC   BDWKS,NWKS                                                       
         MVC   BDNOWK,SVNOWK                                                    
*                                                                               
         MVC   AREC,ADBUY                                                       
         CLI   QOPT2,C' '                                                       
         BE    *+8                                                              
         BAS   RE,DMPREC                                                        
*                                  MAKE SURE NOT ALREADY ON FILE                
         MVC   KEY(10),BUYREC                                                   
         MVI   KEY+10,0                                                         
         MVC   KEY+11(1),BUYKBUY                                                
         MVI   KEY+12,1                                                         
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   SB68                                                             
         CLI   QOPT1,C'Y'          TEST LIVE RUN                                
         BNE   SB68                                                             
         CLI   NOBUY,C'Y'          DONT ADD IF ERROR                            
         BE    SB68                                                             
         GOTO1 ADD                                                              
SB68     DS    0H                                                               
         B     SBX                                                              
         SPACE 3                                                                
DMPREC   NTR1                                                                   
         SPACE 2                                                                
         L     R5,AREC                                                          
         MVC   HALF,13(R5)                                                      
         LH    R2,HALF                                                          
         LA    R3,0(R5,R2)                                                      
*                                                                               
DR2      DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   DR8                                                              
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+40(8),WORK+32                                                  
         MVC   P+49(8),WORK+40                                                  
         MVC   P+58(8),WORK+48                                                  
         MVC   P+67(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+79(0),WORK                                                     
         GOTO1 REPORT                                                           
         LA    R5,1(R5,R4)                                                      
         B     DR2                                                              
*                                                                               
DR8      DS    0H                                                               
         GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE 2                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
SBX      DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
*                                  DIRECTORY FOR TIM/DPT TABLES                 
*                                                                               
*                                  AGYMED(3),TIME ZONE(1),                      
*                                  STATION TYPE(1),SPARE(3)                     
*                                  A(TABLE)                                     
*                                                                               
TIMTDIR  DS    0F                                                               
         DC    C'MCT1I   ',AL4(TTMCT1I)    MCCANN                               
         DC    C'MCT1N   ',AL4(TTMCT1N)                                         
         DC    C'MCT2I   ',AL4(TTMCT2I)                                         
         DC    C'MCT2N   ',AL4(TTMCT2N)                                         
*                                                                               
         DC    C'PET2I   ',AL4(TTPET2I)    PEPSI                                
         DC    C'PET2N   ',AL4(TTPET2N)                                         
         DC    C'PET2F   ',AL4(TTPET2F)     (FOX)                               
         DC    C'PET4I   ',AL4(TTPET4I)                                         
         DC    C'PET4N   ',AL4(TTPET4N)                                         
         DC    C'PET4F   ',AL4(TTPET4F)      (FOX)                              
*                                                                               
         DC    C'CCT1I   ',AL4(TTCCT1I)    COKE                                 
         DC    C'CCT1N   ',AL4(TTCCT1N)                                         
         DC    C'CCT2I   ',AL4(TTCCT2I)                                         
         DC    C'CCT2N   ',AL4(TTCCT2N)                                         
*                                                                               
         DC    C'CKT1I   ',AL4(TTCCT1I)    MORE COKE (CK SAME AS CC)            
         DC    C'CKT1N   ',AL4(TTCCT1N)                                         
         DC    C'CKT2I   ',AL4(TTCCT2I)                                         
         DC    C'CKT2N   ',AL4(TTCCT2N)                                         
*                                          WUNDERMAN                            
         DC    C'WWT1Z   ',AL4(TTWWT1Z)                                         
         DC    C'WWT2Z   ',AL4(TTWWT2Z)                                         
         DC    C'WWT3Z   ',AL4(TTWWT2Z)                                         
         DC    C'WWT4Z   ',AL4(TTWWT1Z)                                         
*                                                                               
         DC    C'JWT1I   ',AL4(TTJWT1I)    JWT                                  
         DC    C'JWT1N   ',AL4(TTJWT1N)    JWT                                  
         DC    C'JWT2I   ',AL4(TTJWT1I)                                         
         DC    C'JWT2N   ',AL4(TTJWT1N)                                         
         DC    C'JWT3I   ',AL4(TTJWT2I)                                         
         DC    C'JWT3N   ',AL4(TTJWT2N)                                         
         DC    C'JWT4I   ',AL4(TTJWT2I)                                         
         DC    C'JWT4N   ',AL4(TTJWT2N)                                         
*                                                                               
         DC    C'PBRZZ   ',AL4(TTPBRZZ)    PREMIUM                              
         DC    C'PBTZZ   ',AL4(TTPBTZZ)                                         
*                                                                               
         DC    C'SSRZZ   ',AL4(TTSSZZZ)    SSCB                                 
         DC    C'SSTZZ   ',AL4(TTSSZZZ)                                         
*                                                                               
         DC    C'BNRZZ   ',AL4(TTBNZZZ)    BBDO (BN)                            
         DC    C'BNTZZ   ',AL4(TTBNZZZ)                                         
*                                                                               
         DC    C'WITZZ   ',AL4(TTWITZZ)    WILA - TV                            
         DC    C'WIRZZ   ',AL4(TTWIRZZ)    WILA - RADIO                         
*                                                                               
         DC    C'WRTZZ   ',AL4(TTWITZZ)    WILA - TV                            
         DC    C'WRRZZ   ',AL4(TTWIRZZ)    WILA - RADIO                         
*                                                                               
         DC    C'BJT1Z   ',AL4(TTBJT1Z)    BJ                                   
         DC    C'BJT2Z   ',AL4(TTBJT2Z)                                         
         DC    C'BJT3Z   ',AL4(TTBJT2Z)                                         
         DC    C'BJT4Z   ',AL4(TTBJT1Z)                                         
*                                                                               
         DC    C'AAT1Z   ',AL4(TTAAT1Z)     4 A'S (DRUG CAMPAIGN)               
         DC    C'AAT2Z   ',AL4(TTAAT2Z)     NOTE - 4 A'S IS DEFAULT             
         DC    C'AARZZ   ',AL4(TTAARZZ)     MUST BE AT END OF LIST              
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         SPACE 3                                                                
*                                  TIME/DPT TABLE                               
*              BYTE   0    DAY MASK                                             
*              BYTES  1-2  END TIME                                             
*              BYTE   3    CODE                                                 
*              BYTE   4(10)  DESCRIPTION                                        
*              BYTE   14(4)  MILITARY TIME                                      
TIMTABL  EQU   18                                                               
*                                                                               
TTMCT1N  DS    0C                   MC/T/EAST-WEST/NETWORK                      
*                                                                               
         DC    X'03',AL2(1859),C'E',CL10'6A-7P ',X'0258076C'                    
         DC    X'7C',AL2(1559),C'D',CL10'6A-4P ',X'02580640'                    
         DC    X'7C',AL2(1859),C'E',CL10'4-7P  ',X'0640076C'                    
         DC    X'7E',AL2(1959),C'A',CL10'7-8P  ',X'076C07D0'                    
         DC    X'7E',AL2(2259),C'P',CL10'8-11P ',X'07D008FC'                    
         DC    X'7F',AL2(2259),C'P',CL10'7-11P ',X'076C08FC'                    
         DC    X'7F',AL2(2559),C'L',CL10'11P-2A',X'08FC00C8'                    
         DC    X'7F',AL2(2959),C'Z',CL10'2A-6A ',X'00C80258'                    
         DC    X'7F',AL2(2959),C'B',CL10'6A-2A ',X'025800C8'  *BONUS*           
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTMCT2N  DS    0C              MC/T/CENTRAL-MOUNTAIN/NETWORK                    
*                                                                               
         DC    X'03',AL2(1759),C'E',CL10'6A-6P ',X'02580708'                    
         DC    X'7C',AL2(1459),C'D',CL10'6A-3P ',X'025805DC'                    
         DC    X'7C',AL2(1759),C'E',CL10'3-6P  ',X'05DC0708'                    
         DC    X'7E',AL2(1859),C'A',CL10'6-7P  ',X'0708076C'                    
         DC    X'7E',AL2(2159),C'P',CL10'7-10P ',X'076C0898'                    
         DC    X'7F',AL2(2159),C'P',CL10'6-10P ',X'07080898'                    
         DC    X'7F',AL2(2459),C'L',CL10'10P-1A',X'08980064'                    
         DC    X'7F',AL2(2959),C'Z',CL10'1A-6A ',X'00640258'                    
         DC    X'7F',AL2(2959),C'B',CL10'6A-2A ',X'025800C8'  *BONUS*           
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTMCT1I  DS    0C                   MC/T/EAST-WEST/INDEPENDENT                  
*                                  (SAME AS NETWORK AFFILIATES)                 
         DC    X'03',AL2(1859),C'E',CL10'6A-7P ',X'0258076C'                    
         DC    X'7C',AL2(1559),C'D',CL10'6A-4P ',X'02580640'                    
         DC    X'7C',AL2(1859),C'E',CL10'4-7P  ',X'0640076C'                    
         DC    X'7F',AL2(1959),C'A',CL10'7-8P  ',X'076C07D0'                    
         DC    X'7F',AL2(2259),C'P',CL10'8-11P ',X'07D008FC'                    
         DC    X'7F',AL2(2559),C'L',CL10'11P-2A',X'08FC00C8'                    
         DC    X'7F',AL2(2959),C'Z',CL10'2A-6A ',X'00C80258'                    
         DC    X'7F',AL2(2959),C'B',CL10'6A-2A ',X'025800C8'  *BONUS*           
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTMCT2I  DS    0C              MC/T/CENTRAL-MOUNTAIN/INDEPENDENT                
*                                                                               
         DC    X'03',AL2(1759),C'E',CL10'6A-6P ',X'02580708'                    
         DC    X'7C',AL2(1459),C'D',CL10'6A-3P ',X'025805DC'                    
         DC    X'7C',AL2(1759),C'E',CL10'3-6P  ',X'05DC0708'                    
         DC    X'7F',AL2(1859),C'A',CL10'6-7P  ',X'0708076C'                    
         DC    X'7F',AL2(2159),C'P',CL10'7-10P ',X'076C0898'                    
         DC    X'7F',AL2(2459),C'L',CL10'10P-1A',X'08980064'                    
         DC    X'7F',AL2(2959),C'Z',CL10'1A-6A ',X'00640258'                    
         DC    X'7F',AL2(2959),C'B',CL10'6A-2A ',X'025800C8'  *BONUS*           
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                                                                               
TTPET2N  DS    0C                PEPSI/T/EAST-WEST/NETWORK                      
*                                                                               
         DC    X'03',AL2(1857),C'E',CL10'6A-657P   ',AL2(0600,1857)             
         DC    X'7C',AL2(1557),C'D',CL10'6A-357P   ',AL2(0600,1557)             
         DC    X'7C',AL2(1857),C'E',CL10'358-657P  ',AL2(1558,1857)             
         DC    X'7E',AL2(1957),C'A',CL10'658-757P  ',AL2(1858,1957)             
         DC    X'7E',AL2(2300),C'P',CL10'758-11P   ',AL2(1958,2300)             
         DC    X'01',AL2(2300),C'P',CL10'658-11P   ',AL2(1858,2300)             
         DC    X'7F',AL2(2959),C'L',CL10'1101P-559A',AL2(2301,0559)             
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTPET4N  DS    0C           PEPSI/T/CENTRAL-MOUNTAIN/NETWORK                    
*                                                                               
         DC    X'03',AL2(1757),C'E',CL10'6A-557P   ',AL2(0600,1757)             
         DC    X'7C',AL2(1457),C'D',CL10'6A-257P   ',AL2(0600,1457)             
         DC    X'7C',AL2(1757),C'E',CL10'258-557P  ',AL2(1458,1757)             
         DC    X'7E',AL2(1857),C'A',CL10'558-657P  ',AL2(1758,1857)             
         DC    X'7E',AL2(2200),C'P',CL10'658-10P   ',AL2(1858,2200)             
         DC    X'01',AL2(2200),C'P',CL10'558-10P   ',AL2(1758,2200)             
         DC    X'7F',AL2(2959),C'L',CL10'1001P-559A',AL2(2201,0559)             
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTPET2I  DS    0C                PEPSI/T/EAST-WEST/INDEPENDENT                  
         DC    X'03',AL2(1857),C'I',CL10'6A-657P   ',AL2(0600,1857)             
         DC    X'7C',AL2(1557),C'D',CL10'6A-357P   ',AL2(0600,1557)             
         DC    X'7C',AL2(1857),C'I',CL10'358-657P  ',AL2(1558,1857)             
         DC    X'7E',AL2(1957),C'A',CL10'658-757P  ',AL2(1858,1957)             
         DC    X'7E',AL2(2300),C'N',CL10'758-11P   ',AL2(1958,2300)             
         DC    X'01',AL2(2300),C'N',CL10'658-11P   ',AL2(1858,2300)             
         DC    X'7F',AL2(2959),C'M',CL10'1101P-559A',AL2(2301,0559)             
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTPET4I  DS    0C           PEPSI/T/CENTRAL-MOUNTAIN/INDEPENDENT                
*                                                                               
*                                                                               
         DC    X'03',AL2(1757),C'I',CL10'6A-557P   ',AL2(0600,1757)             
         DC    X'7C',AL2(1457),C'D',CL10'6A-257P   ',AL2(0600,1457)             
         DC    X'7C',AL2(1757),C'I',CL10'258-557P  ',AL2(1458,1757)             
         DC    X'7E',AL2(1857),C'A',CL10'558-657P  ',AL2(1758,1857)             
         DC    X'7E',AL2(2200),C'N',CL10'658-10P   ',AL2(1858,2200)             
         DC    X'01',AL2(2200),C'N',CL10'558-10P   ',AL2(1758,2200)             
         DC    X'7F',AL2(2959),C'M',CL10'1001P-559A',AL2(2201,0559)             
         DC    X'FFFFFFFF'                                                      
*                                                                               
         SPACE 3                                                                
TTPET2F  DS    0C                PEPSI/T/EAST-WEST/FOX                          
*                                (LIKE IND, BUT PRIME IS U, NOT N)              
*                                                                               
         DC    X'03',AL2(1857),C'I',CL10'6A-657P   ',AL2(0600,1857)             
         DC    X'7C',AL2(1557),C'D',CL10'6A-357P   ',AL2(0600,1557)             
         DC    X'7C',AL2(1857),C'I',CL10'358-657P  ',AL2(1558,1857)             
         DC    X'7E',AL2(1957),C'A',CL10'658-757P  ',AL2(1858,1957)             
         DC    X'7E',AL2(2200),C'U',CL10'758-10P   ',AL2(1958,2200)             
         DC    X'7E',AL2(2300),C'N',CL10'1001-11P  ',AL2(2201,2300)             
         DC    X'01',AL2(2300),C'U',CL10'658-11P   ',AL2(1858,2300)             
         DC    X'7F',AL2(2959),C'M',CL10'1101P-559A',AL2(2301,0559)             
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTPET4F  DS    0C           PEPSI/T/CENTRAL-MOUNTAIN/FOX                        
*                           (LIKE IND, BUT PRIME IS U, NOT N)                   
*                                                                               
         DC    X'03',AL2(1757),C'I',CL10'6A-557P   ',AL2(0600,1757)             
         DC    X'7C',AL2(1457),C'D',CL10'6A-257P   ',AL2(0600,1457)             
         DC    X'7C',AL2(1757),C'I',CL10'258-557P  ',AL2(1458,1757)             
         DC    X'7E',AL2(1857),C'A',CL10'558-657P  ',AL2(1758,1857)             
         DC    X'7E',AL2(2100),C'U',CL10'658-9P    ',AL2(1858,2100)             
         DC    X'7E',AL2(2200),C'N',CL10'901-10P   ',AL2(2101,2200)             
         DC    X'01',AL2(2200),C'U',CL10'558-10P   ',AL2(1758,2200)             
         DC    X'7F',AL2(2959),C'M',CL10'1001P-559A',AL2(2201,0559)             
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                                                                               
TTCCT1N  DS    0C                 COKE/T/EAST-WEST/NETWORK                      
*                                                                               
         DC    X'03',AL2(1859),C'E',CL10'6A-7P ',X'0258076C'                    
         DC    X'7C',AL2(1559),C'D',CL10'6A-4P ',X'02580640'                    
         DC    X'7C',AL2(1859),C'E',CL10'4-7P  ',X'0640076C'                    
         DC    X'7E',AL2(1959),C'A',CL10'7-8P  ',X'076C07D0'                    
         DC    X'7E',AL2(2259),C'P',CL10'8-11P ',X'07D008FC'                    
         DC    X'7F',AL2(2259),C'P',CL10'7-11P ',X'076C08FC'                    
         DC    X'7F',AL2(2559),C'L',CL10'11P-2A',X'08FC00C8'                    
         DC    X'7F',AL2(2959),C'Z',CL10'2A-6A ',X'00C80258'                    
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTCCT2N  DS    0C            COKE/T/CENTRAL-MOUNTAIN/NETWORK                    
*                                                                               
         DC    X'03',AL2(1759),C'E',CL10'6A-6P ',X'02580708'                    
         DC    X'7C',AL2(1459),C'D',CL10'6A-3P ',X'025805DC'                    
         DC    X'7C',AL2(1759),C'E',CL10'3-6P  ',X'05DC0708'                    
         DC    X'7E',AL2(1859),C'A',CL10'6-7P  ',X'0708076C'                    
         DC    X'7E',AL2(2159),C'P',CL10'7-10P ',X'076C0898'                    
         DC    X'7F',AL2(2159),C'P',CL10'6-10P ',X'07080898'                    
         DC    X'7F',AL2(2459),C'L',CL10'10P-1A',X'08980064'                    
         DC    X'7F',AL2(2959),C'Z',CL10'1A-6A ',X'00640258'                    
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTCCT1I  DS    0C                 COKE/T/EAST-WEST/INDEPENDENT                  
*                                  (SAME AS NETWORK AFFILIATES)                 
         DC    X'03',AL2(1859),C'E',CL10'6A-7P ',X'0258076C'                    
         DC    X'7C',AL2(1559),C'D',CL10'6A-4P ',X'02580640'                    
         DC    X'7C',AL2(1859),C'E',CL10'4-7P  ',X'0640076C'                    
         DC    X'7F',AL2(1959),C'A',CL10'7-8P  ',X'076C07D0'                    
         DC    X'7F',AL2(2259),C'P',CL10'8-11P ',X'07D008FC'                    
         DC    X'7F',AL2(2559),C'L',CL10'11P-2A',X'08FC00C8'                    
         DC    X'7F',AL2(2959),C'Z',CL10'2A-6A ',X'00C80258'                    
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTCCT2I  DS    0C            COKE/T/CENTRAL-MOUNTAIN/INDEPENDENT                
*                                                                               
         DC    X'03',AL2(1759),C'E',CL10'6A-6P ',X'02580708'                    
         DC    X'7C',AL2(1459),C'D',CL10'6A-3P ',X'025805DC'                    
         DC    X'7C',AL2(1759),C'E',CL10'3-6P  ',X'05DC0708'                    
         DC    X'7F',AL2(1859),C'A',CL10'6-7P  ',X'0708076C'                    
         DC    X'7F',AL2(2159),C'P',CL10'7-10P ',X'076C0898'                    
         DC    X'7F',AL2(2459),C'L',CL10'10P-1A',X'08980064'                    
         DC    X'7F',AL2(2959),C'Z',CL10'1A-6A ',X'00640258'                    
         DC    X'FFFFFFFF'                                                      
*                                                                               
TTWWT1Z  DS    0C            WUNDERMAN/T/EAST-WEST/ALL STATIONS                 
*                                                                               
         DC    X'03',AL2(1559),C'W',CL10'6A-4P ',X'02580640'                    
         DC    X'03',AL2(1759),C'F',CL10'4-6P  ',X'06400708'                    
         DC    X'01',AL2(1859),C'A',CL10'6-7P  ',X'0708076C'                    
         DC    X'01',AL2(2259),C'P',CL10'7-11P ',X'076C08FC'                    
         DC    X'7C',AL2(0859),C'E',CL10'6A-9A ',X'02580384'                    
         DC    X'7C',AL2(1559),C'D',CL10'9A-4P ',X'03840640'                    
         DC    X'7E',AL2(1759),C'F',CL10'4-6P  ',X'06400708'                    
         DC    X'7E',AL2(1959),C'A',CL10'6-8P  ',X'070807D0'                    
         DC    X'7E',AL2(2259),C'P',CL10'8-11P ',X'07D008FC'                    
         DC    X'7F',AL2(1759),C'F',CL10'4-6P  ',X'06400708'                    
         DC    X'7F',AL2(2559),C'L',CL10'11P-2A',X'08FC00C8'                    
         DC    X'7F',AL2(2959),C'O',CL10'2-6A  ',X'00C80258'                    
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTWWT2Z  DS    0C       WUNDERMAN/T/CENTRAL-MOUNTAIN/ALL STATIONS               
*                                                                               
         DC    X'03',AL2(1459),C'W',CL10'6A-3P ',X'025805DC'                    
         DC    X'03',AL2(1659),C'F',CL10'3-5P  ',X'05DC06A4'                    
         DC    X'01',AL2(1759),C'A',CL10'5-6P  ',X'06A40708'                    
         DC    X'01',AL2(2159),C'P',CL10'6-10P ',X'07080898'                    
         DC    X'7C',AL2(0859),C'E',CL10'6A-9A ',X'02580384'                    
         DC    X'7C',AL2(1459),C'D',CL10'9A-3P ',X'038405DC'                    
         DC    X'7E',AL2(1659),C'F',CL10'3-5P  ',X'05DC06A4'                    
         DC    X'7E',AL2(1859),C'A',CL10'5-7P  ',X'06A4076C'                    
         DC    X'7E',AL2(2159),C'P',CL10'7-10P ',X'076C0898'                    
         DC    X'7F',AL2(1659),C'F',CL10'3-5P  ',X'05DC06A4'                    
         DC    X'7F',AL2(2559),C'L',CL10'10P-2A',X'089800C8'                    
         DC    X'7F',AL2(2959),C'O',CL10'2-6A  ',X'00C80258'                    
*                                                                               
         DC    X'FFFFFFFF'                                                      
*                                                                               
TTWWT1N  DS    0C            WUNDERMAN/T/EAST-WEST/NETWORK                      
*                                                                               
         DC    X'03',AL2(1159),C'D',CL10'6A-12N',X'025804B0'                    
         DC    X'03',AL2(1859),C'E',CL10'12N-7P',X'04B0076C'                    
         DC    X'03',AL2(2259),C'P',CL10'7-11P ',X'076C08FC'                    
         DC    X'7C',AL2(0859),C'M',CL10'6A-9A ',X'02580384'                    
         DC    X'7C',AL2(1559),C'D',CL10'9A-4P ',X'03840640'                    
         DC    X'7C',AL2(1929),C'E',CL10'4-730P',X'0640078A'                    
         DC    X'7C',AL2(1959),C'A',CL10'730-8P',X'078A07D0'                    
         DC    X'7C',AL2(2259),C'P',CL10'8-11P ',X'07D008FC'                    
         DC    X'7F',AL2(2959),C'L',CL10'11P-6A',X'08FC0258'                    
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTWWT2N  DS    0C       WUNDERMAN/T/CENTRAL-MOUNTAIN/NETWORK                    
*                                                                               
         DC    X'03',AL2(1159),C'D',CL10'6A-12N',X'025804B0'                    
         DC    X'03',AL2(1859),C'E',CL10'12N-7P',X'04B0076C'                    
         DC    X'03',AL2(2259),C'P',CL10'7-11P ',X'076C08FC'                    
         DC    X'03',AL2(2959),C'L',CL10'11P-6A',X'08FC0258'                    
         DC    X'7C',AL2(0859),C'M',CL10'6A-9A ',X'02580384'                    
         DC    X'7C',AL2(1459),C'D',CL10'9A-3P ',X'038405DC'                    
         DC    X'7C',AL2(1829),C'E',CL10'3-630P',X'05DC0726'                    
         DC    X'7C',AL2(1859),C'A',CL10'630-7P',X'0726076C'                    
         DC    X'7C',AL2(2159),C'P',CL10'7-10P ',X'076C0898'                    
         DC    X'7C',AL2(2959),C'L',CL10'10P-6A',X'08980258'                    
*                                                                               
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTWWT1I  DS    0C            WUNDERMAN/T/EAST-WEST/INDEPENDENT                  
*                                                                               
         DC    X'03',AL2(1159),C'D',CL10'6A-12N',X'025804B0'                    
         DC    X'03',AL2(1859),C'E',CL10'12N-7P',X'04B0076C'                    
         DC    X'03',AL2(2259),C'P',CL10'7-11P ',X'076C08FC'                    
         DC    X'03',AL2(2959),C'L',CL10'11P-6A',X'08FC0258'                    
         DC    X'7C',AL2(0859),C'M',CL10'6A-9A ',X'02580384'                    
         DC    X'7C',AL2(1559),C'D',CL10'9A-4P ',X'03840640'                    
         DC    X'7C',AL2(1929),C'E',CL10'4-730P',X'064007A8'                    
         DC    X'7C',AL2(1959),C'A',CL10'730-8P',X'07A807D0'                    
         DC    X'7C',AL2(2959),C'P',CL10'8P-6A ',X'07D00258'                    
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTWWT2I  DS    0C       WUNDERMAN/T/CENTRAL-MOUNTAIN/INDEPENDENT                
*                                                                               
         DC    X'03',AL2(1159),C'D',CL10'6A-12N',X'025804B0'                    
         DC    X'03',AL2(1859),C'E',CL10'12N-7P',X'04B0076C'                    
         DC    X'03',AL2(2259),C'P',CL10'7-11P ',X'076C08FC'                    
         DC    X'03',AL2(2959),C'L',CL10'11P-6A',X'08FC0258'                    
         DC    X'7C',AL2(0859),C'M',CL10'6A-9A ',X'02580384'                    
         DC    X'7C',AL2(1459),C'D',CL10'9A-3P ',X'038405DC'                    
         DC    X'7C',AL2(1829),C'E',CL10'3-630P',X'05DC0726'                    
         DC    X'7C',AL2(1859),C'A',CL10'630-7P',X'0726076C'                    
         DC    X'7C',AL2(2959),C'P',CL10'7P-6A ',X'076C0258'                    
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                                  JW/T/TZ 1+2 (E+W) /IND STATIONS              
TTJWT1I  DS    0C                                                               
         DC    X'01',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'01',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'01',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'01',AL2(2259),C'I',CL10'7P-11P  ',AL2(1900,2300)               
         DC    X'03',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'03',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'03',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'7C',AL2(0859),C'B',CL10'6A-9A   ',AL2(0600,0900)               
         DC    X'7C',AL2(1559),C'D',CL10'9A-4P   ',AL2(0900,1600)               
         DC    X'7C',AL2(1759),C'E',CL10'4P-6P   ',AL2(1600,1800)               
         DC    X'7F',AL2(1859),C'M',CL10'6P-7P   ',AL2(1800,1900)               
         DC    X'7E',AL2(1959),C'A',CL10'7P-8P   ',AL2(1900,2000)               
         DC    X'7E',AL2(2259),C'I',CL10'8P-11P  ',AL2(2000,2300)               
         DC    X'7F',AL2(2329),C'N',CL10'11-1130P',AL2(2300,2330)               
         DC    X'7F',AL2(2559),C'L',CL10'1130P-2A',AL2(2330,0200)               
         DC    X'7F',AL2(2959),C'X',CL10'2A-6A   ',AL2(0200,0600)               
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                                  JW/T/TZ 1+2 (E+W) /NET STATIONS              
TTJWT1N  DS    0C                                                               
         DC    X'01',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'01',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'01',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'01',AL2(2259),C'P',CL10'7P-11P  ',AL2(1900,2300)               
         DC    X'03',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'03',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'03',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'7C',AL2(0859),C'B',CL10'6A-9A   ',AL2(0600,0900)               
         DC    X'7C',AL2(1559),C'D',CL10'9A-4P   ',AL2(0900,1600)               
         DC    X'7C',AL2(1759),C'E',CL10'4P-6P   ',AL2(1600,1800)               
         DC    X'7F',AL2(1859),C'M',CL10'6P-7P   ',AL2(1800,1900)               
         DC    X'7E',AL2(1959),C'A',CL10'7P-8P   ',AL2(1900,2000)               
         DC    X'7E',AL2(2259),C'P',CL10'8P-11P  ',AL2(2000,2300)               
         DC    X'7F',AL2(2329),C'N',CL10'11-1130P',AL2(2300,2330)               
         DC    X'7F',AL2(2559),C'L',CL10'1130P-2A',AL2(2330,0200)               
         DC    X'7F',AL2(2959),C'X',CL10'2A-6A   ',AL2(0200,0600)               
         DC    X'FFFFFFFF'                                                      
*                                  JW/T/TZ 3+4 (C+M) /IND STATIONS              
TTJWT2I  DS    0C                                                               
         DC    X'01',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'01',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'01',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'01',AL2(2159),C'I',CL10'6P-10P  ',AL2(1800,2200)               
         DC    X'03',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'03',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'03',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'7C',AL2(0859),C'B',CL10'6A-9A   ',AL2(0600,0900)               
         DC    X'7C',AL2(1459),C'D',CL10'9A-3P   ',AL2(0900,1500)               
         DC    X'7C',AL2(1659),C'E',CL10'3P-5P   ',AL2(1500,1700)               
         DC    X'7C',AL2(1759),C'M',CL10'5P-6P   ',AL2(1700,1800)               
         DC    X'7E',AL2(1859),C'A',CL10'6P-7P   ',AL2(1800,1900)               
         DC    X'7E',AL2(2159),C'I',CL10'7P-10P  ',AL2(1900,2200)               
         DC    X'7F',AL2(1759),C'M',CL10'5P-6P   ',AL2(1700,1800)               
         DC    X'7F',AL2(2229),C'N',CL10'10-1030P',AL2(2200,2230)               
         DC    X'7F',AL2(2459),C'L',CL10'1030P-1A',AL2(2230,0100)               
         DC    X'7F',AL2(2959),C'X',CL10'1A-6A   ',AL2(0100,0600)               
         DC    X'FFFFFFFF'                                                      
*                                  JW/T/TZ 3+4 (C+M) /NET STATIONS              
TTJWT2N  DS    0C                                                               
         DC    X'01',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'01',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'01',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'01',AL2(2159),C'P',CL10'6P-10P  ',AL2(1800,2200)               
         DC    X'03',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'03',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'03',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'7C',AL2(0859),C'B',CL10'6A-9A   ',AL2(0600,0900)               
         DC    X'7C',AL2(1459),C'D',CL10'9A-3P   ',AL2(0900,1500)               
         DC    X'7C',AL2(1659),C'E',CL10'3P-5P   ',AL2(1500,1700)               
         DC    X'7C',AL2(1759),C'M',CL10'5P-6P   ',AL2(1700,1800)               
         DC    X'7E',AL2(1859),C'A',CL10'6P-7P   ',AL2(1800,1900)               
         DC    X'7E',AL2(2159),C'P',CL10'7P-10P  ',AL2(1900,2200)               
         DC    X'7F',AL2(1759),C'M',CL10'5P-6P   ',AL2(1700,1800)               
         DC    X'7F',AL2(2229),C'N',CL10'10-1030P',AL2(2200,2230)               
         DC    X'7F',AL2(2459),C'L',CL10'1030P-1A',AL2(2230,0100)               
         DC    X'7F',AL2(2959),C'X',CL10'1A-6A   ',AL2(0100,0600)               
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                               PREMIUM INC (PB) TV                             
TTPBTZZ  DS    0C                                                               
         DC    X'7F',AL2(1559),C'D',CL10'6A-4P   ',X'02580640'                  
         DC    X'7F',AL2(1859),C'E',CL10'4P-7P   ',X'0640076C'                  
         DC    X'7F',AL2(1959),C'A',CL10'7P-8P   ',X'076C07D0'                  
         DC    X'7F',AL2(2259),C'P',CL10'8P-11P  ',X'07D008FC'                  
         DC    X'7F',AL2(2959),C'L',CL10'11P-6A  ',X'08FC0258'                  
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                               PREMIUM INC (PB) RADIO                          
TTPBRZZ  DS    0C                                                               
         DC    X'7F',AL2(2959),C'R',CL10'6A-559A ',X'02580257'                  
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                               SSCB - TV AND RADIO                             
TTSSZZZ  DS    0C                                                               
         DC    X'03',AL2(2959),C'W',CL10'6A-559A ',X'02580257'                  
         DC    X'7C',AL2(0959),C'A',CL10'530A-10A',X'021203E8'                  
         DC    X'7C',AL2(1459),C'H',CL10'10A-3P  ',X'03E805DC'                  
         DC    X'7C',AL2(1959),C'P',CL10'3P-8P   ',X'05DC07D0'                  
         DC    X'7C',AL2(2929),C'N',CL10'8P-530A ',X'07D00212'                  
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                               BBDO (BN) TV AND RADIO                          
TTBNZZZ  DS    0C                                                               
         DC    X'7F',AL2(2959),C'P',CL10'6A-559A ',X'02580257'                  
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                               WILA - TV                                       
TTWITZZ  DS    0C                                                               
         DC    X'7F',AL2(0859),C'M',CL10'6A-9A   ',X'02580384'                  
         DC    X'7F',AL2(1559),C'D',CL10'9A-4P   ',X'03840640'                  
         DC    X'7F',AL2(1859),C'F',CL10'4P-7P   ',X'0640076C'                  
         DC    X'7F',AL2(1959),C'A',CL10'7P-8P   ',X'076C07D0'                  
         DC    X'7F',AL2(2259),C'P',CL10'8P-11P  ',X'07D008FC'                  
         DC    X'7F',AL2(2329),C'N',CL10'11-1130P',X'08FC091A'                  
         DC    X'7F',AL2(2959),C'L',CL10'1130P-6A',X'091A0258'                  
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                               WILA - RADIO                                    
*                                                                               
*            NOTE- DAYPART A IS SPLIT BECAUSE 5A-10A HAS TO                     
*                  TREATED AS 5-6A AND 6-10A. NOTE THAT IN THE                  
*                  2ND ENTRY FOR A THE END TIME (AT +1) IS 6A, NOT 10A          
*                                                                               
TTWIRZZ  DS    0C                                                               
         DC    X'7F',AL2(0959),C'A',CL10'5A-10A  ',AL2(0500,1000)               
         DC    X'7F',AL2(1459),C'D',CL10'10A-3P  ',AL2(1000,1500)               
         DC    X'7F',AL2(1959),C'P',CL10'3P-8P   ',AL2(1500,2000)               
         DC    X'7F',AL2(2359),C'E',CL10'8P-12M  ',AL2(2000,2400)               
         DC    X'7F',AL2(2859),C'L',CL10'12M-5A  ',AL2(2400,0500)               
         DC    X'7F',AL2(2959),C'A',CL10'5A-10A  ',AL2(0500,1000)               
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                                  BJ/T/TZ 1+4/ALL STATIONS                     
TTBJT1Z  DS    0C                                                               
         DC    X'03',AL2(1159),C'W',CL10'6A-12N  ',AL2(0600),AL2(1200)          
         DC    X'03',AL2(1559),C'X',CL10'12N-4P  ',AL2(1200),AL2(1600)          
         DC    X'03',AL2(1859),C'J',CL10'4-7P    ',AL2(1600),AL2(1900)          
         DC    X'03',AL2(2259),C'P',CL10'7-11P   ',AL2(1900),AL2(2300)          
         DC    X'7C',AL2(0859),C'M',CL10'6-9A    ',AL2(0600),AL2(0900)          
         DC    X'7C',AL2(1159),C'E',CL10'9A-12N  ',AL2(0900),AL2(1200)          
         DC    X'7C',AL2(1559),C'D',CL10'12N-4P  ',AL2(1200),AL2(1600)          
         DC    X'7C',AL2(1959),C'A',CL10'4-8P    ',AL2(1600),AL2(2000)          
         DC    X'7C',AL2(2259),C'P',CL10'8-11P   ',AL2(2000),AL2(2300)          
         DC    X'7F',AL2(2559),C'L',CL10'11P-2A  ',AL2(2300),AL2(0200)          
         DC    X'7F',AL2(2959),C'V',CL10'2-6A    ',AL2(0200),AL2(0600)          
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                                  BJ/T/TZ 2+3/ALL STATIONS                     
TTBJT2Z  DS    0C                                                               
         DC    X'03',AL2(1159),C'W',CL10'6A-12N  ',AL2(0600),AL2(1200)          
         DC    X'03',AL2(1559),C'X',CL10'12N-4P  ',AL2(1200),AL2(1600)          
         DC    X'03',AL2(1859),C'J',CL10'4-7P    ',AL2(1600),AL2(1900)          
         DC    X'03',AL2(2159),C'P',CL10'7-10P   ',AL2(1900),AL2(2200)          
         DC    X'7C',AL2(0859),C'M',CL10'6-9A    ',AL2(0600),AL2(0900)          
         DC    X'7C',AL2(1159),C'E',CL10'9A-12N  ',AL2(0900),AL2(1200)          
         DC    X'7C',AL2(1559),C'D',CL10'12N-4P  ',AL2(1200),AL2(1600)          
         DC    X'7C',AL2(1859),C'A',CL10'4-7P    ',AL2(1600),AL2(1900)          
         DC    X'7C',AL2(2159),C'P',CL10'7-10P   ',AL2(1900),AL2(2200)          
         DC    X'7F',AL2(2559),C'L',CL10'10P-2A  ',AL2(2200),AL2(0200)          
         DC    X'7F',AL2(2959),C'V',CL10'2-6A    ',AL2(0200),AL2(0600)          
         DC    X'FFFFFFFF'                                                      
*                                                                               
TTAAT1Z  DS    0C                   AA/T/EAST-WEST  (4 A'S)                     
*                                                                               
         DC    X'01',AL2(1859),C'E',CL10'6A-7P  ',X'0258076C'                   
         DC    X'01',AL2(2259),C'P',CL10'7-11P  ',X'076C08FC'                   
         DC    X'02',AL2(1929),C'E',CL10'6A-730P',X'0258078A'                   
         DC    X'7C',AL2(1559),C'D',CL10'6A-4P  ',X'02580640'                   
         DC    X'7C',AL2(1929),C'E',CL10'4-730P ',X'0640078A'                   
         DC    X'7E',AL2(1959),C'A',CL10'730-8P ',X'078A07D0'                   
         DC    X'7E',AL2(2259),C'P',CL10'8-11P  ',X'07D008FC'                   
         DC    X'7F',AL2(2959),C'L',CL10'11P-6A ',X'08FC0258'                   
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTAAT2Z  DS    0C              AA/T/CENTRAL-MOUNTAIN                            
*                                                                               
         DC    X'01',AL2(1759),C'E',CL10'6A-6P  ',X'02580708'                   
         DC    X'01',AL2(2159),C'P',CL10'6-10P  ',X'07080898'                   
         DC    X'02',AL2(1829),C'E',CL10'6A-630P',X'02580726'                   
         DC    X'7C',AL2(1459),C'D',CL10'6A-3P  ',X'025805DC'                   
         DC    X'7C',AL2(1829),C'E',CL10'3-630P ',X'05DC0726'                   
         DC    X'7E',AL2(1859),C'A',CL10'630-7P ',X'0726076C'                   
         DC    X'7E',AL2(2159),C'P',CL10'7-10P  ',X'076C0898'                   
         DC    X'7F',AL2(2959),C'L',CL10'10P-6A ',X'08980258'                   
         DC    X'FFFFFFFF'                                                      
*                               AA RADIO                                        
TTAARZZ  DS    0C                                                               
         DC    X'7F',AL2(1959),C'R',CL10'6A-8P   ',X'025807D0'                  
         DC    X'7F',AL2(2959),C'R',CL10'8P-6A   ',X'07D00258'                  
         DC    X'FFFFFFFF'                                                      
*                                                                               
         SPACE 3                                                                
*                                                                               
SPI4WC   CSECT                                                                  
INVREC   DS    2100C                                                            
DMWRK    DS    CL3000                                                           
*                                                                               
IDTMAX   EQU   100                                                              
IDTAB    DS    CL(13*IDTMAX)                                                    
*                                                                               
FLMMAX   EQU   200                                                              
FLMTAB   DS    CL(11*FLMMAX)                                                    
*                                                                               
*                                                                               
         BUFF  LINES=2600,FLAVOR=DATA,KEYLIST=(60,A)                            
         SPACE 3                                                                
WKD      DSECT                                                                  
ABUFFC   DS    A                                                                
ASETBUY  DS    A                                                                
ATIMVAL  DS    A                                                                
ADINV    DS    A                                                                
ARPRT    DS    A                                                                
ARDINV   DS    A                                                                
ADMWRK   DS    A                                                                
ATIMTDIR DS    A                                                                
         DS    5F                                                               
*                                                                               
RELO     DS    A                                                                
ATIMTAB  DS    A                                                                
CTOTS    DS    XL16                                                             
RTOTS    DS    XL16                                                             
CSW      DS    X                                                                
W        DS    XL100                                                            
X        DS    XL256                                                            
QSTAP    DS    XL3                                                              
QESTB    DS    X                                                                
IESTOPT  DS    C                                                                
NOESTERR DS    C                                                                
THISREC  DS    XL100                                                            
LASTREC  DS    XL100                                                            
HLDMOS   DS    CL6                                                              
MONLIST  DS    XL24                                                             
         DS    0F                                                               
ITOTS    DS    0XL12                                                            
IBUYS    DS    F                                                                
ISPTS    DS    F                                                                
IDOLLS   DS    F                                                                
*                                                                               
BTOTS    DS    0XL8                                                             
BSPTS    DS    F                                                                
BDOLLS   DS    F                                                                
*                                                                               
ELAD     DS    A                                                                
WKCNT    DS    F                                                                
FLMPARS  DS    6F                                                               
IDPARS   DS    6F                                                               
SVWKN1   DS    X                                                                
SVWKN2   DS    X                                                                
SVCOST   DS    XL3                                                              
SVPROD   DS    CL7                                                              
SVEST    DS    X                                                                
SVLEN    DS    X                                                                
SVDPT    DS    C                                                                
SVWEEK   DS    XL2                                                              
SVWEEKP  DS    CL6                                                              
SVIDNAME DS    CL12                                                             
         DS    0F                                                               
SVWKCNTS DS    XL20                                                             
SVNOWK   DS    X                                                                
SVWEEKA  DS    XL2                                                              
SKEDSW   DS    C                                                                
NWKS     DS    X                                                                
SVSTAT   DS    X                                                                
NOBUY    DS    X                                                                
HOLDKEY  DS    XL64                                                             
LASTKEY  DS    XL32                                                             
         DS    0F                                                               
ADRLIST  DS    XL24                                                             
         SPACE 3                                                                
*                   INVOICE ITEM ELEMENT                                        
INVRECD  DSECT                                                                  
       ++INCLUDE SPGENINV                                                       
         SPACE 2                                                                
PLIND    DSECT                                                                  
PLIN     DS    0CL132                                                           
         DS    CL3                                                              
PLPRD    DS    CL7                                                              
         DS    CL3                                                              
PLEST    DS    CL3                                                              
         DS    CL3                                                              
PLLEN    DS    CL3                                                              
         DS    CL4                                                              
PLCOST   DS    CL11                                                             
         DS    CL3                                                              
PLDPT    DS    CL1                                                              
         DS    CL3                                                              
PLWEEK   DS    CL8                                                              
         DS    CL4                                                              
PLCNT    DS    CL5                                                              
PLDATE   DS    CL5                                                              
         DS    CL2                                                              
PLDAY    DS    CL3                                                              
         DS    CL3                                                              
PLTIME   DS    CL5                                                              
PLBUY    DS    0CL56                                                            
         DS    CL2                                                              
PLFILM   DS    CL17                                                             
         SPACE 3                                                                
IRECD    DSECT                                                                  
IPRD     DS    CL7                                                              
IEST     DS    X                                                                
ILEN     DS    X                                                                
ICOST    DS    XL3                                                              
IDPT     DS    CL1                                                              
ISTAT    DS    XL1                                                              
IIDNAME  DS    CL12                                                             
IWEEK    DS    XL2                                                              
IRKL     EQU   *-IRECD                                                          
IDATE    DS    XL2                                                              
IDAY     DS    CL3                                                              
IDAYB    DS    XL1                                                              
IWKN     DS    XL1                                                              
ITIME    DS    XL2                                                              
IPRDB    DS    XL1                                                              
IPRDB2   DS    XL1                                                              
IFILMC   DS    CL8                                                              
IFLMSQ   DS    XL2                                                              
IFILMC2  DS    CL8                                                              
IFLMSQ2  DS    XL2                                                              
IRECL    EQU   *-IRECD             ***CHANGE BUFF MACRO***                      
*                                                                               
       ++INCLUDE DDBUFFALOD                                                     
         SPACE 3                                                                
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPREPI402 05/01/02'                                      
         END                                                                    
