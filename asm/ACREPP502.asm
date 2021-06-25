*          DATA SET ACREPP502  AT LEVEL 003 AS OF 05/01/02                      
*PHASE ACP502A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE ACCEDIT                                                                
*INCLUDE SQUASHER                                                               
         TITLE 'UNAPPROVED/AUTHORISED REPORT'                                   
ACP502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACP5**,R9,RR=R5                                              
         L     RA,0(R1)            RA=A(ACWORKD)                                
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND          RC=A(PROGRAM W/S)                            
         USING ACP5D,RC                                                         
         L     RF,=A(SORTAREA)                                                  
         AR    RF,R5                                                            
         ST    RF,ASORTAR                                                       
         ST    R5,PRELOC                                                        
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   ACP8                                                             
         L     RF,ADACC                                                         
         MVC   SAVEACC,0(RF)                                                    
         MVC   IOB(42),SPACES                                                   
         L     RF,ADCOMP           READ PRODUCTION LEDGER                       
         MVC   IOB(1),0(RF)        FOR HEIRARCHY ELEMENT                        
         MVC   IOB+1(2),=C'SJ'                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCFIL',IOB,IOB                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,IOB                                                           
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
ACP2     CLI   0(RE),0                                                          
         BE    ACP7                                                             
         CLI   0(RE),X'16'                                                      
         BE    ACP6                                                             
ACP4     IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     ACP2                                                             
*                                                                               
ACP6     MVC   SAVE16,0(RE)                                                     
         B     ACP4                                                             
*                                                                               
ACP7     DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCFIL',SAVEACC,IOB                   
         B     EXIT                                                             
         EJECT                                                                  
ACP8     CLI   MODE,REQFRST                                                     
         BNE   ACP10                                                            
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,SETSORT                                                       
         MVC   LASTAUTH,SPACES                                                  
         B     EXIT                                                             
         SPACE 1                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
ACP10    CLI   MODE,PROCTRNS                                                    
         BNE   ACP20                                                            
         L     R2,ADTRANS                                                       
         CLI   0(R2),X'44'                                                      
         BNE   EXIT                                                             
         USING TRANSD,R2                                                        
         CLI   QOPT1,C'U'                                                       
         BNE   ACP12                                                            
         TM    TRNSSTAT,X'08'      AUTHORISED                                   
         BO    EXIT                                                             
ACP12    CLC   TRNSANAL,=C'**'                                                  
         BE    EXIT                                                             
         TM    TRNSSTAT,X'80'                                                   
         BZ    EXIT                DEBIT                                        
         CLI   TRNSTYPE,1                                                       
         BE    ACP14                                                            
         CLI   TRNSTYPE,3                                                       
         BE    ACP14                                                            
         CLI   TRNSTYPE,4                                                       
         BNE   EXIT                                                             
ACP14    DS    0H                                                               
         BAS   RE,BUILDREC                                                      
         BAS   RE,PUTSORT                                                       
         B     EXIT                                                             
         EJECT                                                                  
ACP20    CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         LA    R6,IOA                                                           
ACP22    BAS   RE,GETSORT           READ SORTED RECORDS                         
         CLC   IOA(L'SORTKEY),SPACES                                            
         BE    EXIT                                                             
         BAS   RE,PRINTREC         AND PRINT THEM                               
         B     ACP22                                                            
         EJECT                                                                  
*              BUILD A STANDARD RECORD FOR  SORT                                
         SPACE 2                                                                
BUILDREC NTR1                                                                   
         LA    R6,IOA                                                           
         USING SORTRECD,R6                                                      
         LR    RE,R6                                                            
         LA    RF,L'SORTREC                                                     
         XCEF                                                                   
         MVC   SORTKEY,=15C'9'     SET DEFAULT                                  
         MVC   SORTINV,TRNSREF                                                  
         MVC   SORTBAT,TRNSBTCH                                                 
         MVC   SORTDTE,TRNSDATE                                                 
         MVC   SORTKEYD,TRNSDATE                                                
         ZAP   SORTAMNT,TRNSAMNT                                                
         L     RF,ADACC                                                         
         MVC   SORTJOB,0(RF)                                                    
         L     RF,ADSUBAC                                                       
         MVC   SORTSUPP,2(RF)      SUPPLIER                                     
         MVC   SORTSUB,SPACES                                                   
*                                                                               
BLD2     CLI   0(R2),0                                                          
         BE    EXIT                                                             
         CLI   0(R2),X'25'                                                      
         BE    BLD10                                                            
         CLI   0(R2),X'4D'                                                      
         BE    BLD12                                                            
         CLI   0(R2),X'60'         DATE ADDED EL                                
         BE    BLD14                                                            
         CLI   0(R2),X'23'                                                      
         BE    BLD16                                                            
BLD4     ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     BLD2                                                             
         USING ACNOD,R2                                                         
BLD10    MVC   SORTORD,ACNO                                                     
         B     BLD4                                                             
         USING TRAUTHD,R2                                                       
BLD12    MVC   SORTKEY,TRAUCODE                                                 
         B     BLD4                                                             
*                                                                               
         USING TRSTATD,R2                                                       
BLD14    DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(1,SORTKEYD)                            
         B     BLD4                                                             
*                                                                               
         USING ACOTHERD,R2                                                      
BLD16    MVC   SORTSUB,ACOTNUM     SUB REFERENCE                                
         B     BLD4                                                             
         EJECT                                                                  
*              PRINT A STANDARD RECORD                                          
         SPACE 1                                                                
PRINTREC NTR1                                                                   
         MVC   WORK,SPACES                                                      
         LA    R6,IOA                                                           
         USING SORTRECD,R6                                                      
         CLC   LASTAUTH,SPACES     FIRST TIME                                   
         BE    PRT10                                                            
         CLC   LASTAUTH,SORTKEY    SAME PERSON                                  
         BE    PRT12                                                            
         MVI   FORCEHED,C'Y'       SKIP TO NEW PAGE IF NOT                      
PRT10    MVC   LASTAUTH,SORTKEY                                                 
PRT12    MVC   WORK(15),SORTJOB    CLI/PRD/JOB                                  
         GOTO1 =V(ACCEDIT),DMCB,(0,WORK),SAVE16,WORK+20,RR=RB                   
         L     RF,DMCB                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P+1(0),WORK+20                                                   
         BAS   RE,SUPPOUT                                                       
         MVC   P+48(6),SORTINV                                                  
         MVC   PSECOND+48(6),SORTSUB                                            
         GOTO1 DATCON,DMCB,(1,SORTDTE),(8,P+59)                                 
         GOTO1 (RF),(R1),(1,SORTKEYD),(8,P+78)                                  
         MVC   P+69(6),SORTORD                                                  
         MVC   P+86(6),SORTBAT                                                  
         EDIT  SORTAMNT,(12,P+97),2,MINUS=YES                                   
         MVC   HEAD5+16(15),SORTKEY                                             
         CLI   QOPT1,C'U'                                                       
         BNE   *+10                                                             
         MVC   HEAD5+84(17),=C'UNAUTHORISED ONLY'                               
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
SUPPOUT  NTR1                                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(12),SORTSUPP+3                                              
         L     RF,ADACC                                                         
         MVC   SAVEACC,0(RF)                                                    
         MVC   IOB(42),SPACES                                                   
         MVC   IOB(15),SORTSUPP                                                 
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCFIL',IOB,IOB                       
         CLI   DMCB+8,0                                                         
         BE    SPT0                                                             
         TM    DMCB+8,X'10'                                                     
         BNZ   *+6                 RECORD NOT FOUND                             
         DC    H'0'                DISC ERROR                                   
         MVC   WORK+14(17),=C'** NOT ON FILE **'                                
         B     SPT8                                                             
*                                                                               
SPT0     LA    RE,IOB                                                           
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
SPT2     CLI   0(RE),0                                                          
         BE    SPT8                                                             
         CLI   0(RE),X'20'                                                      
         BE    SPT6                                                             
SPT4     IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     SPT2                                                             
         USING ACNAMED,RE                                                       
SPT6     IC    RF,ACNMLEN                                                       
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK+14(0),ACNMNAME                                              
         B     SPT4                                                             
*                                                                               
SPT8     DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCFIL',SAVEACC,IOB                   
         GOTO1 =V(SQUASHER),DMCB,WORK,60,RR=PRELOC                              
         L     RF,DMCB+4                                                        
         GOTO1 CHOPPER,DMCB,((RF),WORK),(30,P+17),(C'P',2)                      
         B     EXIT                                                             
         EJECT                                                                  
*              SORTER INTERFACE                                                 
         SPACE 2                                                                
         USING SORTRECD,R6                                                      
SETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECDCARD,(40,ASORTAR),RR=RB             
         B     EXIT                                                             
         SPACE 2                                                                
PUTSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',IOA,RR=RB                                
         B     EXIT                                                             
         SPACE 2                                                                
GETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'GET',0,RR=RB                                  
         MVC   IOA(L'SORTKEY),SPACES                                            
         LA    R6,IOA                                                           
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BZ    EXIT                                                             
         MVC   0(250,R6),0(R1)                                                  
         B     EXIT                                                             
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,15,A,16,3,D,70,6,A,34,15,A,55,3,A),FX        
               ORMAT=BI,WORK=1'                                                 
RECDCARD DC    CL80'RECORD TYPE=F,LENGTH=81'                                    
*                                                                               
* SORT ORDER IS - AUTHORISER                                                    
*                 DATE ADDED                                                    
*                 SUBREF                                                        
*                 SUPPLIER                                                      
*                 INVOICE DATE                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR STANDARD SORT-RECORD                                   
         SPACE 1                                                                
SORTRECD DSECT                                                                  
SORTREC  DS    0CL81                                                            
SORTKEY  DS    CL15                                                             
SORTKEYD DS    CL3                                                              
SORTJOB  DS    CL15                                                             
SORTSUPP DS    CL15                                                             
SORTINV  DS    CL6                                                              
SORTDTE  DS    CL3                                                              
SORTORD  DS    CL6                                                              
SORTAMNT DS    PL6                                                              
SORTSUB  DS    CL6                                                              
SORTBAT  DS    CL6                                                              
         SPACE 2                                                                
*              GENERAL W/S DSECT                                                
         SPACE 1                                                                
ACP5D    DSECT                                                                  
PRELOC   DS    F                                                                
ASORTAR  DS    A                                                                
SAVEACC  DS    CL15                                                             
LASTAUTH DS    CL15                                                             
SAVE16   DS    CL70                                                             
IOA      DS    1023C                                                            
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
**PAN#1  DC    CL21'003ACREPP502 05/01/02'                                      
         END                                                                    
