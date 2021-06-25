*          DATA SET SPREPCK02  AT LEVEL 010 AS OF 08/29/00                      
*PHASE SPCK02A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPCK02 - COKE COMLETED MARKET REPORT'                           
SPCK02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPCK02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPCK02,RB,RC                                                     
*                                                                               
         MVC   SORTER,=V(SORTER)                                                
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    CK2                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* RUNFRST                                                                       
*                                                                               
CK2      L     R1,=A(CCFILE)       OPEN COKE FILE                               
         OPEN  (CCFILE,INPUT)                                                   
         MVI   SORTSW,C'N'                                                      
*                                                                               
CK4      L     R2,ADBUY                                                         
         GET   CCFILE,(R2)         READ A RECORD                                
         L     R6,ADBUY                                                         
         USING RECVRECD,R6                                                      
*                                                                               
         CLC   RKEY(2),=X'0D71'    TEST COKE STATUS RECORD                      
         BNE   CK4                                                              
         CLC   RKEY+20(2),=C'CC'                                                
         BNE   CK4                                                              
         LA    R7,RKEY                                                          
         USING STATD,R7                                                         
         LA    R3,BPELEM                                                        
         SR    RE,RE                                                            
*                                                                               
CK6      CLI   0(R3),0                                                          
         BE    CK4                                                              
         CLI   0(R3),CSCODEQ                                                    
         BE    CK8                                                              
         IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     CK6                                                              
*                                                                               
         USING CSELEM,R3                                                        
CK8      CLC   CSBUY,TODAYP                                                     
         BNE   CK4                                                              
         XC    SORTREC,SORTREC     SET UP SORT RECORD                           
         MVC   SAGY,CSAGYA         AGENCY                                       
         L     R1,ADCLT                                                         
         LA    R1,CLIST-CLTHDR(R1)                                              
         LA    R0,255                                                           
*                                                                               
CK10     CLI   0(R1),C'A'                                                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   STKPRD,3(R1)                                                     
         BE    CK12                                                             
         LA    R1,4(R1)                                                         
         BCT   R0,CK10                                                          
         DC    H'0'                                                             
*                                                                               
CK12     MVC   SPRD,0(R1)          ALPHA PRODUCT                                
         MVC   SEST,STKEST         ESTIMATE                                     
         MVC   SMKT,STKMKT         MARKET                                       
         CLI   SORTSW,C'N'         TEST SORT INITIALIZED                        
         BNE   CK14                                                             
         MVI   SORTSW,C'Y'                                                      
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
CK14     GOTO1 SORTER,DMCB,=C'PUT',SORTREC    WRITE TO SORT                     
         B     CK4                                                              
*                                                                               
CK20     XC    SORTREC,SORTREC                                                  
         CLI   SORTSW,C'N'         TEST ANY RECORDS                             
         BNE   CK22                                                             
         MVC   P(38),=C'***** NO MARKETS COMPLETED TODAY *****'                 
         GOTO1 REPORT                                                           
         B     CKX                                                              
*                                                                               
CK22     GOTO1 SORTER,DMCB,=C'GET' GET SORTED RECORDS                           
         ICM   R2,15,4(R1)         TEST EOF                                     
         BZ    CK30                                                             
         MVC   PPRD,0(R2)          PRODUCT                                      
         CLC   SORTREC(3),0(R2)                                                 
         BE    CK24                                                             
         XC    KEY,KEY                                                          
         L     R1,ADCLT                                                         
         MVC   KEY(4),0(R1)                                                     
         MVC   KEY+4(3),0(R2)                                                   
         GOTO1 HIGH                                                             
         GOTO1 GETPRD                                                           
*                                                                               
CK24     MVC   PPRDNAME,PRDNM                                                   
         EDIT  (1,3(R2)),(3,PEST),FILL=0   ESTIMATE                             
         CLC   SORTREC(4),0(R2)                                                 
         BE    CK26                                                             
         XC    KEY,KEY                                                          
         L     R1,ADCLT                                                         
         MVC   KEY(4),0(R1)                                                     
         MVC   KEY+4(4),0(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      TEST EST FOUND                              
         BE    CK25                                                             
         MVC   PESTNAME(5),=5C'?'                                               
         B     CK27                                                             
*                                                                               
CK25     DS    0H                                                               
         GOTO1 GETEST                                                           
*                                                                               
CK26     MVC   PESTNAME,ESTNM                                                   
*                                                                               
CK27     DS    0H                                                               
         EDIT  (2,4(R2)),(4,PMKT),FILL=0   MARKET                               
         CLC   SORTREC+4(2),4(R2)                                               
         BE    CK28                                                             
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),PMKT                                                    
         MVC   KEY+6(2),=C'CC'                                                  
         GOTO1 READMKT                                                          
*                                                                               
CK28     MVC   PMKTNAME,MKTNM                                                   
         MVC   SORTREC,0(R2)       SAVE CURRENT SORT RECORD                     
         MVC   PAGENCY,SAGY        AGENCY                                       
         GOTO1 REPORT              PRINT REPORT LINE                            
         B     CK22                                                             
*                                                                               
CK30     GOTO1 SORTER,DMCB,=C'END'                                              
*                                                                               
CKX      B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* STORAGE                                                                       
*                                                                               
SORTER   DS    A                                                                
*                                                                               
SORTSW   DS    CL1                                                              
*                                                                               
SORTREC  DS    0CL8                                                             
SPRD     DS    CL3                                                              
SEST     DS    XL1                                                              
SMKT     DS    XL2                                                              
SAGY     DS    CL2                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,8,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=8'                                     
*                                                                               
CCFILE   DCB   DDNAME=CCFILE,DSORG=PS,RECFM=VB,MACRF=(GM,PM),          X        
               EODAD=CK20                                                       
         EJECT                                                                  
RECVRECD DSECT                                                                  
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
*                                                                               
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
RKEY     DS    0CL13                                                            
         DS    2050C                                                            
         SPACE 2                                                                
       ++INCLUDE SPGENSTAT                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         PRINT ON                                                               
SPWORKD  DSECT                                                                  
         ORG   P                                                                
         DS    CL21                                                             
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PPRDNAME DS    CL20                                                             
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PESTNAME DS    CL20                                                             
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL1                                                              
PMKTNAME DS    CL24                                                             
         DS    CL2                                                              
PAGENCY  DS    CL2                                                              
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPREPCK02 08/29/00'                                      
         END                                                                    
