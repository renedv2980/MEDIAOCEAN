*          DATA SET PPPUB11    AT LEVEL 019 AS OF 04/22/03                      
*PHASE T40611A                                                                  
*                                                                               
         TITLE 'T40611 - PUB LFM  NEWS LINAGE EQUIV'                            
*                                                                               
*                                                                               
* SMYE  2/96    INCLUDE PUGENEROL (PUB VERSION OF PPGENEROL)                    
*               ALSO USE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                  
*                                                                               
*                                                                               
T40611   CSECT                                                                  
         NMOD1 0,T40611                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T406FFD,RA                                                       
         LA    R9,PUBIO                                                         
         USING PUBREC,R9                                                        
         LA    R4,PUBIO                                                         
         LA    R5,16                                                            
CLEARIO  XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         BCT   R5,CLEARIO                                                       
         LA    R3,53                                                            
         LA    R2,PBLPUBH                                                       
         OC    PUBADDR,PUBADDR                                                  
         BZ    ERROR                                                            
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,GETPUB                                                        
         MVI   PUBIND,0                                                         
         LA    R6,PUBREC+33                                                     
         MVI   ELCODE,X'40'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   CKADD                                                            
         OI    PUBIND,X'10'                                                     
*                                                                               
CKADD    CLI   BACT,1              SEE IF ADD                                   
         BNE   RATESCRN                                                         
         TM    PUBIND,X'10'                                                     
         BNO   RATESCRN                                                         
         LA    R3,COMBERR                                                       
         B     ERROR                                                            
*                                                                               
*                                                                               
RATESCRN CLI   BYTE2,1             SEE IF ACTION=FORMAT                         
         BE    FORMATR                                                          
         CLI   BACT,2                                                           
         BH    FORMATR                                                          
EDIT     DS    0H                  FIRST DELETE OLD X'40' ELEMS                 
         LA    R2,CLESP1H                                                       
         CLI   5(R2),0                                                          
         BNE   EDT1                                                             
         LA    R3,MISSERR                                                       
         B     ERROR                                                            
*                                                                               
EDT1     DS    0H                                                               
         MVI   ELCODE,X'40'                                                     
EDT5     LA    R6,PUBREC+33                                                     
         BAS   RE,NEXTEL                                                        
         BNE   EDT10                                                            
         GOTO1 VRECUP,DMCB,(1,PUBREC),0(R6)                                     
         B     EDT5                                                             
*                                                                               
EDT10    CLC   CLESP1(6),=C'DELETE'                                             
         BE    EDT50               SPECIAL DELETE CODE                          
EDT15    DS    0H                                                               
*                                                                               
CLEARREC SR    R5,R5                                                            
         IC    R5,PUBIO+25                                                      
         SLL   R5,8                                                             
         IC    R5,PUBIO+26                                                      
         SR    RE,RE                                                            
         LA    RE,PUBIO                                                         
         AR    RE,R5                                                            
         SR    RF,RF                                                            
         LA    RF,PUBIO+1999                                                    
         LA    RF,2000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
         LA    R2,CLESP1H                                                       
         LA    R4,SPCNUM                                                        
         XC    ELEAREA(250),ELEAREA                                             
         XC    ELEAREA+250(250),ELEAREA+250                                     
         LA    R5,ELEAREA                                                       
         SR    R7,R7                                                            
EDT20    CLI   5(R2),0             CHK FOR INPUT                                
         BE    NXTSPACE                                                         
         ZIC   R1,5(R2)                                                         
         LA    R6,8(R2)                                                         
EDT21    CLI   0(R6),C'0'                                                       
         BL    EDT22                                                            
         LA    R6,1(R6)                                                         
         BCT   R1,EDT21                                                         
         B     EDTERR              CAN'T BE ALL NUMERIC                         
*                                                                               
EDT22    DS    0H                                                               
         BAS   RE,DUPCHK                                                        
         MVC   0(6,R5),8(R2)                                                    
         BAS   RE,NXTUNP           BUMP TO NEXT UNPROTECTED FLD                 
         ZIC   R6,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R6)                                     
         CLI   DMCB,X'FF'                                                       
         BNE   EDT25                                                            
EDTERR   LA    R3,2                FLDINV                                       
         B     ERROR                                                            
*                                                                               
EDT25    L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST DIVIDE BY 100                           
         BNE   EDTERR                                                           
         LTR   R0,R0                                                            
         BNP   EDTERR                                                           
         ZAP   DUB,DUB(6)                                                       
         CVB   R0,DUB                                                           
         C     R0,=F'32767'        MAX CARRIED IN 2 BYTES                       
         BH    EDTERR                                                           
         STH   R0,HALF                                                          
         MVC   6(2,R5),HALF                                                     
         BAS   RE,NXTUNP           BUMP TO NEXT UNPROTECTED FLD                 
         LA    R5,8(R5)                                                         
         LA    R7,1(R7)                                                         
         B     NXTSP5                                                           
*                                                                               
NXTSPACE DS    0H                                                               
         BAS   RE,NXTUNP           BUMP TO NEXT UNPROTECTED FLD                 
         BAS   RE,NXTUNP           BUMP TO NEXT UNPROTECTED FLD                 
*                                                                               
NXTSP5   BCT   R4,EDT20                                                         
*                                                                               
EDT30    DS    0H                                                               
*                                  XSORT NEW ELEMS                              
         LTR   R7,R7                                                            
         BZ    EDT50               NONE TO SORT                                 
         MVI   ELCODE,X'40'                                                     
         LA    R5,ELEAREA                                                       
         MVC   WORK(2),=X'400A'    SET CODE AND LENGHT                          
*                                                                               
EDT35    LA    R6,PUBREC+33                                                     
         BAS   RE,NEXTEL                                                        
EDT40    DS    0H                                                               
         MVC   WORK+2(8),0(R5)                                                  
         GOTO1 VRECUP,DMCB,(1,PUBREC),WORK,(R6)                                 
         LA    R5,8(R5)            NEXT ELEM                                    
         LA    R6,10(R6)                                                        
         BCT   R7,EDT40            ADD NEXT ELEM                                
*                                                                               
EDT50    DS    0H                                                               
WRITEIT  MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,PUTPUB                                                        
         B     DONE                                                             
         EJECT                                                                  
FORMATR  DS    0H                                                               
         CLI   SAVSCRN,X'E1'       SEE IF I NEED TO LOAD SCREEN                 
         BNE   FMT2                                                             
         CLI   BACT,1              SEE IF ADD                                   
         BNE   FMT5                                                             
         MVI   BYTE2,0             TO GENERATE TURNAROUND                       
         B     EDIT                                                             
*                                                                               
FMT2     DS    0H                                                               
         LA    R4,PBLLAST                                                       
         GOTO1 VCALLOV,DMCB,(R4),X'D90406E1'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRGERR                                                          
         MVI   SAVSCRN,X'E1'                                                    
*                                                                               
FMT5     DS    0H                                                               
         CLI   BACT,1                                                           
         BNE   PUTFLDS                                                          
         B     CKSTND                                                           
*                                                                               
PUTFLDS  DS    0H                                                               
         LA    R6,PUBREC+33                                                     
         MVI   ELCODE,X'40'                                                     
         LA    R2,CLESP1H                                                       
         LA    R4,SPCNUM                                                        
*                                                                               
PUTF5    BAS   RE,NEXTEL                                                        
         BNE   PUTF25                                                           
         FOUT  (R2),2(R6),6                                                     
         BAS   RE,NXTUNP           BUMP TO NEXT UNPROTECTED FLD                 
         EDIT  (B2,8(R6)),(6,8(R2)),0,COMMAS=YES,ALIGN=LEFT                     
         FOUT  (R2)                                                             
         BAS   RE,NXTUNP           BUMP TO NEXT UNPROTECTED FLD                 
         BCT   R4,PUTF5                                                         
         B     CKSTND                                                           
*                                                                               
PUTF25   FOUT  (R2),SPACES,6                                                    
         BAS   RE,NXTUNP                                                        
         FOUT  (R2),SPACES,6                                                    
         BAS   RE,NXTUNP                                                        
         BCT   R4,PUTF25                                                        
*                                                                               
CKSTND   DS    0H                                                               
         CLC   AGYALPHA(2),PUBKAGY                                              
         BNE   PROTECT                                                          
         CLI   BACT,2                                                           
         BH    DONE                                                             
*                                                                               
CKS5     LA    R2,CLESP1H                                                       
         B     EXIT                                                             
*                                                                               
PROTECT  DS    0H                                                               
         LA    R4,SPCNUM                                                        
         LA    R2,CLESP1H                                                       
PROT2    OI    1(R2),X'20'                                                      
         BAS   RE,NXTUNP                                                        
         OI    1(R2),X'20'                                                      
         BAS   RE,NXTUNP                                                        
         BCT   R4,PROT2                                                         
         MVI   SAVSCRN,0           SINCE I PROTECTED THE FIELDS                 
         CLI   BACT,2                                                           
         BH    DONE                                                             
         B     CKS5                                                             
*                                                                               
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
NEXT2    IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF REC                                   
         BE    NEXTELX                                                          
         CLC   0(1,R6),ELCODE                                                   
         BER   RE                                                               
         B     NEXT2                                                            
*                                                                               
NEXTELX  LTR   R6,R6                                                            
         BR    RE                                                               
         SPACE 2                                                                
NXTUNP   DS    0H                                                               
         SR    R0,R0                                                            
NEXTU5   IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'                                                      
         BNZ   NEXTU5                                                           
         BR    RE                                                               
*                                                                               
DUPCHK   NTR1                                                                   
         LA    R6,ELEAREA                                                       
DUPC2    CLI   0(R6),0                                                          
         BE    DUPCX                                                            
         CLC   0(6,R6),8(R2)                                                    
         BNE   DUPC5                                                            
         LA    R3,DUPLEV                                                        
         B     ERROR                                                            
*                                                                               
DUPC5    LA    R6,8(R6)                                                         
         B     DUPC2                                                            
*                                                                               
DUPCX    XIT1                                                                   
*                                                                               
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
*                                                                               
SPACES   DC    40C' '                                                           
PUBIND   DS    CL1                                                              
ACTSW    DS    CL1                                                              
PACKED5  DS    PL5                                                              
ELCODE   DS    CL1                                                              
SPCNUM   EQU   45                                                               
*                                                                               
VIRGERR  DC    H'0'                                                             
COMBERR  EQU   112                                                              
DATERR   EQU   20                                                               
DUPLEV   EQU   170                                                              
LEVERR   EQU   171                                                              
RATERR   EQU   172                                                              
ADDERR   EQU   173                                                              
INCHERR  EQU   116                                                              
MISSERR  EQU   1        MISSING INPUT FIELD                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PUGENEROL                                                      
*                                                                               
         LTORG                                                                  
*                                                                               
ELEAREA  DS    500C                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE PUGENOLD                                                       
PUBIO    DS    4000C                                                            
         ORG   PUBIO                                                            
       ++INCLUDE PUBREC                                                         
         EJECT                                                                  
*                                                                               
RATEEL   DSECT                                                                  
       ++INCLUDE PUBRATEL                                                       
         EJECT                                                                  
*                                                                               
DSCTEL   DSECT                                                                  
       ++INCLUDE PUBDSCEL                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPUBFFD                                                       
         ORG   PBLLAST                                                          
       ++INCLUDE PPPUBE1D                                                       
         ORG   T406FFD                                                          
         DS    CL16                                                             
BMED     DS    CL1                                                              
BACT     DS    CL1                                                              
BSCR     DS    CL1                                                              
OLNUM    DS    CL1                                                              
PUBADDR  DS    F                                                                
LTLADDR  DS    F                                                                
BPUB     DS    CL6                                                              
BCLT     DS    CL3                                                              
BDIV     DS    CL3                                                              
BDATE    DS    CL3                                                              
APROF    DS    CL1                                                              
SAVSCRN  DS    CL1                                                              
APROF13  DS    CL1                                                              
BCODE    DS    CL3                                                              
         ORG   BCODE                                                            
BSPACE   DS    0CL17                                                            
         DS    CL1           X'FF'MEANS 3 PACKED FIELDS FOLLOW                  
BSHOW    DS    PL3                                                              
         DS    CL13                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019PPPUB11   04/22/03'                                      
         END                                                                    
