*          DATA SET ACREP4002  AT LEVEL 016 AS OF 06/03/15                      
*PHASE AC4002A,*                                                                
*INCLUDE SORTER                                                                 
*INCLUDE ACCEDIT                                                                
*INCLUDE UNDERLIN                                                               
         TITLE 'HISTORICAL CLIENT SALES ANALYSIS'                               
AC4002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC40**,R9,RR=R5                                              
         L     RA,0(R1)            RA=A(ACWORKD)                                
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND          RC=A(PROGRAM W/S)                            
         USING AC40D,RC                                                         
         ST    R5,PRELOC                                                        
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   ACP8                                                             
         L     RF,=A(SORTAREA)                                                  
         A     RF,PRELOC                                                        
         ST    RF,ASORTAR                                                       
         L     RF,=V(ACCEDIT)                                                   
         A     RF,PRELOC                                                        
         ST    RF,ACCEDIT                                                       
         L     RF,=V(UNDERLIN)                                                  
         A     RF,PRELOC                                                        
         ST    RF,VUNDER                                                        
         B     EXIT                                                             
         EJECT                                                                  
ACP8     CLI   MODE,REQFRST                                                     
         BNE   ACP20                                                            
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         BAS   RE,GETHEIR                                                       
         MVC   HSTART,SPACES                                                    
         MVC   HEND,SPACES                                                      
         MVC   QSTART+4(2),=C'15'                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(1,START)                                 
         GOTO1 (RF),(R1),,(9,HSTART)                                            
         MVC   QEND+4(2),=C'15'                                                 
         GOTO1 DATCON,DMCB,(0,QEND),(1,END)                                     
         GOTO1 (RF),(R1),,(9,HEND)                                              
         MVC   CLICOST(12),=2PL6'0'                                             
         MVC   PRDCOST(12),=2PL6'0'                                             
         MVC   REQTOTS(12),=2PL6'0'                                             
         SPACE 1                                                                
         BAS   RE,SETSORT                                                       
         MVC   LASTKEY,SPACES      FOR FIRST TIME TEST IN PRINT                 
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
ACP20    CLI   MODE,PROCACC                                                     
         BNE   ACP30                                                            
         B     EXIT                                                             
         EJECT                                                                  
ACP30    CLI   MODE,SBACFRST                                                    
         BNE   ACP40                                                            
         MVI   WANT,C'N'                                                        
         L     R2,ADSUBAC                                                       
         USING TRSUBHD,R2                                                       
         CLC   TRSBACNT+1(2),=C'SJ'    WANT ONLY SJ CONTRAS                     
         BE    ACP32                                                            
*&&US                                                                           
         CLC   TRSBACNT+1(2),=C'SM'    AND SM                                   
         BE    ACP32                                                            
*&&                                                                             
         CLC   TRSBACNT+1(2),=C'SR'    AND SR                                   
         BNE   EXIT                                                             
ACP32    CLC   QSELECT,SPACES      ANY CLIENT FILTER                            
         BE    ACP34                                                            
         LA    R3,SAVE16                                                        
         USING ACHEIRD,R3                                                       
         ZIC   RF,ACHRLEVA                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRSBACNT+3(0),QSELECT                                            
         BNE   EXIT                                                             
         SPACE 1                                                                
ACP34    MVI   WANT,C'Y'                                                        
         MVI   ACTIVE,C'N'                                                      
         BAS   RE,INITREC                                                       
         B     EXIT                                                             
         EJECT                                                                  
ACP40    CLI   MODE,PROCHIST       IS THIS A HISTORY RECORD ?                   
         BNE   ACP50               NO, SEE IF SUB-ACCT LAST                     
         CLI   WANT,C'N'                                                        
         BE    EXIT                                                             
         L     R2,ADTRANS                                                       
         CLI   0(R2),X'45'                                                      
         BNE   EXIT                                                             
         USING TRHISTD,R2                                                       
         CLC   TRHSYEAR(2),START                                                
         BL    EXIT                                                             
         CLC   TRHSYEAR(2),END                                                  
         BH    EXIT                                                             
         MVI   ACTIVE,C'Y'                                                      
         BAS   RE,BUILDREC                                                      
         B     EXIT                                                             
         EJECT                                                                  
ACP50    CLI   MODE,SBACLAST                                                    
         BNE   ACP80                                                            
         CLI   WANT,C'Y'                                                        
         BNE   EXIT                                                             
         CLI   ACTIVE,C'Y'                                                      
         BNE   EXIT                                                             
         BAS   RE,PUTSORT          TUCK AWAY IN SORTER                          
         B     EXIT                                                             
         EJECT                                                                  
ACP80    CLI   MODE,ACCLAST                                                     
         BNE   ACP100                                                           
         B     EXIT                                                             
         EJECT                                                                  
ACP100   CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         LA    R6,IOA                                                           
         USING SORTRECD,R6                                                      
         BAS   RE,GETSORT          READ FIRST RECORD                            
         CLC   IOA(L'SORTKEY),SPACES                                            
         BE    ACP119              NOTHING TO PRINT                             
         CLI   QOPT1,C'Y'          NEW PAGE PER PRODUCT                         
         BE    ACP111                                                           
         MVC   P+1(L'SORTPRO),SORTPRO                                           
         MVC   P+8(L'SORTCPN),SORTCPN                                           
         GOTO1 VUNDER,DMCB,(45,P+1),PSECOND+1                                   
         BAS   RE,MYREPORT                                                      
         B     ACP111                                                           
ACP110   BAS   RE,GETSORT          READ SORTED RECORDS                          
         CLC   IOA(L'SORTKEY),SPACES                                            
         BE    ACP112                                                           
ACP111   BAS   RE,PRINTREC         AND PRINT THEM                               
         B     ACP110                                                           
ACP112   BAS   RE,MYREPORT                                                      
         MVC   P+1(17),=C'TOTAL FOR PRODUCT'                                    
         MVC   P+19(6),LASTPRD                                                  
         LA    R3,PRDCOST                                                       
         BAS   RE,EDVALS                                                        
         BAS   RE,MYREPORT                                                      
         BAS   RE,MYREPORT                                                      
         LA    R4,CLICOST                                                       
         BAS   RE,ADDUP                                                         
         MVC   P+1(16),=C'TOTAL FOR CLIENT'                                     
         MVC   P+18(6),LASTCLI                                                  
*&&US*&& MVC   P+21(3),SPACES      DON'T PRINT U/L                              
         LA    R3,CLICOST          TOTALS FOR LAST CLIENT                       
         BAS   RE,EDVALS                                                        
         BAS   RE,MYREPORT                                                      
         SPACE 1                                                                
ACP114   BAS   RE,MYREPORT                                                      
         LA    R3,REQTOTS                                                       
         BAS   RE,EDVALS                                                        
         MVC   P+1(17),=C'TOTAL FOR REQUEST'                                    
         BAS   RE,MYREPORT                                                      
ACP119   BAS   RE,ENDSORT                                                       
         B     EXIT                                                             
         EJECT                                                                  
*              INITIALISE A RECORD FOR SORTER                                   
         SPACE 2                                                                
INITREC  NTR1                                                                   
         LA    R6,IOA                                                           
         USING SORTRECD,R6                                                      
         LR    RE,R6                                                            
         LA    RF,L'SORTREC                                                     
         XCEF                                                                   
*                                                                               
         MVC   SORTKEY,SPACES                                                   
         MVC   SORTGRS(12),=2PL6'0'                                             
         L     RF,ADACC                                                         
         MVC   SORTINC,1(RF)       INCOME ACCT CODE                             
         L     R2,ADSUBAC                                                       
         USING TRSUBHD,R2                                                       
         LA    R3,SAVE16                                                        
         USING ACHEIRD,R3                                                       
         ZIC   RF,ACHRLEVA                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SORTCLI(0),TRSBACNT+3   CLIENT                                   
*&&US*&& MVC   SORTUL(2),TRSBACNT+1    UNIT/LEDGER (MAY BE SM)                  
         IC    RF,ACHRLEVB                                                      
         ZIC   R1,ACHRLEVA                                                      
         SR    RF,R1                                                            
         LA    RE,TRSBACNT+3(R1)                                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SORTPRO(0),0(RE)    PRODUCT                                      
         MVC   SORTINCN,SPACES                                                  
         USING ACNAMED,R2                                                       
         L     R2,ADACCNAM                                                      
         ZIC   RE,ACNMLEN                                                       
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SORTINCN(0),2(R2)                                                
         SPACE 1                                                                
         L     R2,ADSUBAC                                                       
         USING TRSUBHD,R2                                                       
         MVC   SORTCPN,SPACES                                                   
         ZIC   RF,TRSBLEN                                                       
         SH    RF,=H'18'                                                        
         BM    EXIT                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SORTCPN(0),TRSBNAME                                              
         B     EXIT                                                             
         EJECT                                                                  
*              ADD AMOUNTS TO SORTER RECORD                                     
         SPACE 2                                                                
BUILDREC NTR1                                                                   
         LA    R6,IOA                                                           
         USING SORTRECD,R6                                                      
         L     R2,ADTRANS                                                       
         USING TRHISTD,R2                                                       
         LA    RF,SORTCOM                                                       
         CLI   BUCKTYPE,C' '                                                    
         BE    BLDR20                                                           
         LA    RF,SORTGRS                                                       
         CLI   BUCKTYPE,C'G'                                                    
         BNE   EXIT                                                             
BLDR20   AP    0(6,RF),TRHSCR                                                   
         B     EXIT                                                             
         EJECT                                                                  
*              PRINT A SORTED RECORD                                            
         SPACE 1                                                                
PRINTREC NTR1                                                                   
         LA    R6,IOA                                                           
         USING SORTRECD,R6                                                      
         CLC   LASTKEY,SPACES                                                   
         BE    PRT10                                                            
         CLC   LASTKEY,SORTKEY                                                  
         BE    PRT20                                                            
         BAS   RE,MYREPORT                                                      
         LA    R3,PRDCOST                                                       
         BAS   RE,EDVALS                                                        
         MVC   P+1(17),=C'TOTAL FOR PRODUCT'                                    
         MVC   P+19(6),LASTPRD                                                  
         BAS   RE,MYREPORT                                                      
         BAS   RE,MYREPORT                                                      
         LA    R4,CLICOST                                                       
         BAS   RE,ADDUP                                                         
         MVC   PRDCOST(12),=2PL6'0'                                             
         CLC   LASTCLI,SORTKEY                                                  
         BE    PRT8                                                             
         MVC   P+1(16),=C'TOTAL FOR CLIENT'                                     
         MVC   P+18(6),LASTCLI                                                  
*&&US*&& MVC   P+21(3),SPACES      DON'T PRINT U/L                              
         LA    R3,CLICOST                                                       
         BAS   RE,EDVALS                                                        
         BAS   RE,MYREPORT                                                      
         MVC   CLICOST(12),=2PL6'0'                                             
         MVI   FORCEHED,C'Y'                                                    
PRT8     CLI   QOPT1,C'Y'          NEW PAGE PER PRODUCT                         
         BNE   PRT9                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     PRT10                                                            
PRT9     MVC   P+1(L'SORTPRO),SORTPRO                                           
         MVC   P+8(L'SORTCPN),SORTCPN                                           
         MVI   SPACING,2                                                        
         GOTO1 VUNDER,DMCB,(45,P+1),PSECOND+1                                   
         BAS   RE,MYREPORT                                                      
PRT10    MVC   LASTKEY,SORTKEY                                                  
         SPACE 1                                                                
PRT20    MVC   P+1(12),SORTINC+2                                                
         MVC   P+14(36),SORTINCN                                                
         LA    R3,SORTGRS                                                       
         BAS   RE,EDVALS                                                        
         BAS   RE,MYREPORT                                                      
         LA    R3,SORTGRS                                                       
         LA    R4,REQTOTS                                                       
         BAS   RE,ADDUP                                                         
         LA    R4,PRDCOST                                                       
         BAS   RE,ADDUP                                                         
         B     EXIT                                                             
         EJECT                                                                  
*              READ FOR SJ HEIRARCHY ELEMENT                                    
         SPACE 1                                                                
GETHEIR  NTR1                                                                   
         MVC   SAVEKEY,KEY                                                      
         MVC   IOB(42),SPACES                                                   
         MVC   IOB(1),QCOMPANY                                                  
         MVC   IOB+1(2),=C'SJ'                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCFIL',IOB,IOB                       
         CLI   DMCB+8,0                                                         
         BE    GETH2                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
GETH2    LA    R3,IOB                                                           
         AH    R3,DATADISP                                                      
         SR    RF,RF                                                            
GETH4    CLI   0(R3),0                                                          
         BE    GETH10                                                           
         CLI   0(R3),X'16'                                                      
         BE    GETH6                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GETH4                                                            
         SPACE 1                                                                
GETH6    MVC   SAVE16,0(R3)                                                     
         SPACE 1                                                                
GETH10   GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCFIL',SAVEKEY,IOB                   
         B     EXIT                                                             
         EJECT                                                                  
*              READ FOR CLIENT NAME                                             
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         MVC   WORK(36),SPACES                                                  
         MVC   SAVEKEY,KEY                                                      
         MVC   IOB(42),SPACES                                                   
         MVC   IOB(1),QCOMPANY                                                  
*&&UK                                                                           
         MVC   IOB+1(2),=C'SJ'                                                  
         MVC   IOB+3(6),SORTCLI                                                 
*&&                                                                             
*&&US                                                                           
         MVC   IOB+1(2),SORTUL     MIGHT BE SM FOR TALENT                       
         MVC   IOB+3(3),SORTCLI                                                 
*&&                                                                             
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCFIL',IOB,IOB                       
         CLI   DMCB+8,0                                                         
         BE    GETN2                                                            
         MVC   WORK(7),=C'MISSING'                                              
         B     EXIT                                                             
         SPACE 1                                                                
GETN2    LA    R3,IOB                                                           
         AH    R3,DATADISP                                                      
         SR    RF,RF                                                            
GETN4    CLI   0(R3),0                                                          
         BE    GETN10                                                           
         CLI   0(R3),X'20'                                                      
         BE    GETN6                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GETN4                                                            
         SPACE 1                                                                
         USING ACNAMED,R3                                                       
GETN6    ZIC   RF,ACNMLEN                                                       
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     GETN10                                                           
         MVC   WORK(0),ACNMNAME                                                 
         SPACE 1                                                                
GETN10   GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCFIL',SAVEKEY,IOB                   
         B     EXIT                                                             
         EJECT                                                                  
*              PRINTING AIDS                                                    
         SPACE 2                                                                
EDVALS   NTR1                                                                   
         LA    RF,2                                                             
         LA    RE,P+50                                                          
         LR    R4,R3                                                            
EDV2     EDIT  (P6,0(R3)),(13,0(RE)),2,MINUS=YES                                
         CP    DUB,=P'0'           CLEAR IF PRINTING ZERO                       
         BNE   *+10                                                             
         MVC   0(12,RE),SPACES                                                  
         CLI   QOPT2,C'Y'          SUPPRESS COMM,NET,PCT                        
         BE    EXIT                                                             
         LA    RE,16(RE)                                                        
         LA    R3,6(R3)                                                         
         BCT   RF,EDV2                                                          
*                                                                               
         ZAP   DUB1,0(6,R4)        GROSS                                        
         SP    DUB1,DUB            LESS INCOME GIVES NET                        
         CP    DUB1,=P'0'                                                       
         BE    EDV4                                                             
         EDIT  (P8,DUB1),(13,P+89),2,MINUS=YES                                  
*                                                                               
EDV4     CP    0(6,R4),=P'0'       TEST FOR DIVIDE BY ZERO                      
         BE    EXIT                                                             
         ZAP   DIVW,6(6,R4)        INCOME                                       
         MP    DIVW,=P'100000'                                                  
         DP    DIVW,0(6,R4)        DIVIDED BY GROSS                             
         AP    DIVW(8),=P'5'       ROUNDED - GIVES PCT                          
         EDIT  (P8,DIVW),(8,P+80),3                                             
         MVI   P+87,C' '                                                        
         B     EXIT                                                             
         SPACE 2                                                                
ADDUP    NTR1                      ADD ONE LEVEL TO NEXT UP                     
         LA    RF,2                                                             
ADD2     AP    0(6,R4),0(6,R3)                                                  
         LA    R3,6(R3)                                                         
         LA    R4,6(R4)                                                         
         BCT   RF,ADD2                                                          
         B     EXIT                                                             
         EJECT                                                                  
MYREPORT NTR1                                                                   
         MVC   HEAD8,SPACES                                                     
         MVC   HEAD9,SPACES                                                     
         CLI   QOPT2,C'Y'                                                       
         BE    MYRPT1                                                           
         MVC   HEAD8+68(10),=C'COMMISSION'                                      
         MVC   HEAD9+68(10),=31C'-'                                             
         MVC   HEAD8+84(3),=C'PCT'                                              
         MVC   HEAD9+84(3),=31C'-'                                              
         MVC   HEAD8+98(3),=C'NET'                                              
         MVC   HEAD9+98(3),=31C'-'                                              
MYRPT1   DS    0H                                                               
         CLI   QOPT1,C'Y'          NEW PAGE PER PRODUCT                         
         BNE   MYRPT2                                                           
         MVC   HEAD8+1(31),=CL31'INCOME ACCOUNT AND NAME'                       
         MVC   HEAD9+1(31),=CL31'-----------------------'                       
         MVC   HEAD5+1(7),=C'PRODUCT'                                           
         MVC   HEAD5+12(6),SORTPRO                                              
         MVC   HEAD5+19(36),SORTCPN                                             
         B     MYRPT4                                                           
MYRPT2   MVC   HEAD8+1(31),=C'PRODUCT/INCOME ACCOUNT AND NAME'                  
         MVC   HEAD9+1(31),=31C'-'                                              
MYRPT4   MVC   HEAD4+12(6),SORTCLI                                              
*&&US*&& MVC   HEAD4+15(2),SPACES  DON'T WANT UNIT/LEDGER                       
         BAS   RE,GETNAME                                                       
         MVC   HEAD4+19(36),WORK                                                
         MVC   HEAD4+83(6),HSTART                                               
         MVC   HEAD4+90(2),=C'TO'                                               
         MVC   HEAD4+93(6),HEND                                                 
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*              SORTER INTERFACE                                                 
         SPACE 2                                                                
         USING SORTRECD,R6                                                      
SETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECDCARD,(40,ASORTAR),RR=PRELOC         
         B     EXIT                                                             
         SPACE 2                                                                
PUTSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',IOA,RR=PRELOC                            
         B     EXIT                                                             
         SPACE 2                                                                
GETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'GET',0,RR=PRELOC                              
         MVC   IOA(L'SORTKEY),SPACES                                            
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BZ    EXIT                                                             
         MVC   0(150,R6),0(R1)                                                  
         B     EXIT                                                             
         SPACE 1                                                                
ENDSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'END',RR=PRELOC                                
         B     EXIT                                                             
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(01,26,A),FORMAT=BI,WORK=1'                     
RECDCARD DC    CL80'RECORD TYPE=F,LENGTH=130'                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR STANDARD SORT-RECORD                                   
         SPACE 1                                                                
SORTRECD DSECT                                                                  
SORTREC  DS    0CL114                                                           
SORTKEY  DS    0CL26                                                            
SORTCLI  DS    CL6                                                              
         ORG   SORTCLI+3                                                        
SORTUL   DS    CL3                                                              
SORTPRO  DS    CL6                                                              
SORTINC  DS    CL14                                                             
SORTDATA DS    0CL88                                                            
SORTINCN DS    CL36                                                             
SORTCPN  DS    CL36                                                             
SORTGRS  DS    PL6                                                              
SORTCOM  DS    PL6                                                              
         SPACE 2                                                                
*              GENERAL W/S DSECT                                                
         SPACE 1                                                                
AC40D    DSECT                                                                  
DUB1     DS    D                                                                
PRELOC   DS    F                                                                
ASORTAR  DS    A                                                                
ACCEDIT  DS    V                                                                
VUNDER   DS    V                                                                
DIVW     DS    PL14                                                             
ACTIVE   DS    CL1                                                              
SAVEKEY  DS    CL15                                                             
LASTKEY  DS    0CL12                                                            
LASTCLI  DS    CL6                                                              
LASTPRD  DS    CL6                                                              
WANT     DS    CL1                                                              
SAVE16   DS    CL70                                                             
START    DS    XL3                                                              
END      DS    XL3                                                              
HSTART   DS    CL6                                                              
HEND     DS    CL6                                                              
CLICOST  DS    2PL6                                                             
PRDCOST  DS    2PL6                                                             
REQTOTS  DS    2PL6                                                             
IOA      DS    150C                                                             
IOB      DS    1000C                                                            
         EJECT                                                                  
*        ACGENBOTH                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
SORTAREA CSECT                                                                  
         DS    40960C              40K FOR SORT                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACREP4002 06/03/15'                                      
         END                                                                    
