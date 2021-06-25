*          DATA SET PPPUB13    AT LEVEL 014 AS OF 05/01/02                      
*PHASE T40613A,+0,NOAUTO                                                        
*INCLUDE XSORT                                                                  
*                                                                               
***************  CHANGE LOG  ***************                                    
*                                                                               
*  SMYE  2/96    INCLUDE PUGENEROL (PUB VERSION OF PPGENEROL)                   
*                ALSO USE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                 
*                                                                               
*                                                                               
*  SMYE  12/07/97  CHANGED VDTCNV TO VDATCON WITH NEW PARAM'S                   
*                                                                               
         TITLE 'T40613 PUBFILE MAINT CIRC SCREEN'                               
         PRINT NOGEN                                                            
T40613   CSECT                                                                  
         NMOD1 0,T40613,RR=R9                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         USING T406FFD,RA                                                       
         LA    R9,PUBIO                                                         
         USING PUBRECD,R9                                                       
         LA    R4,ELEAREA                                                       
         LH    R5,=H'500'                                                       
         BAS   RE,CLEARWRK                                                      
         LA    R4,PUBIO                                                         
         LH    R5,=H'4000'                                                      
         BAS   RE,CLEARWRK                                                      
         LA    R3,53                                                            
         LA    R2,PBLPUBH                                                       
         OC    PUBADDR,PUBADDR                                                  
         BZ    ERROR                                                            
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,GETPUB                                                        
*                                                                               
CIRSCRN  CLI   BYTE2,1             SEE IF ACTION =FORMAT                        
         BE    FORMATP                                                          
*                                  NEW CIRC SCREEN IN TWA SO EDIT IT            
*                                UNLESS ACTION= SRDS OR DISPLAY                 
         CLI   BACT,2                                                           
         BH    FORMATP                                                          
         EJECT                                                                  
*                                                                               
EDITPUB  DS    0H                                                               
EDITP2   LA    R6,PUBREC+33                                                     
         MVI   ELCODE,X'30'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   EDITP5                                                           
         GOTO1 VRECUP,DMCB,(1,PUBREC),(R6)                                      
         B     EDITP2                                                           
*                                                                               
EDITP5   DS    0H                                                               
         SR    R5,R5                                                            
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
*                                                                               
EDITP10  DS    0H                                                               
         LA    R2,CIRDTE1H                                                      
         LA    R7,3                FOR BCT                                      
         XC    ELEAREA(200),ELEAREA                                             
         LA    R6,ELEAREA                                                       
         USING PUBCIRD,R6                                                       
         XC    FULL,FULL                                                        
EDITP12  MVI   ACTSW,0                                                          
         CLI   5(R2),0                                                          
         BE    EDITP20                                                          
         LA    R3,DATERR                                                        
*                                  EDIT FOR YMD IF ERROR CHK FOR YM             
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    EDITP14                                                          
*        GOTO1 VDTCNV,DMCB,(0,WORK),(1,PUBCDAT)                                 
         GOTO1 VDATCON,DMCB,(0,WORK),(3,PUBCDAT)                                
         MVI   ACTSW,1                                                          
         B     EDITP20                                                          
*                                                                               
EDITP14  DS    0H                                                               
         GOTO1 VDATVAL,DMCB,(2,8(R2)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*        GOTO1 VDTCNV,DMCB,(0,WORK),(1,PUBCDAT)                                 
         GOTO1 VDATCON,DMCB,(0,WORK),(3,PUBCDAT)                                
         MVI   ACTSW,1                                                          
*                                                                               
EDITP20  BAS   RE,NXTUNP                                                        
         CLI   5(R2),0                                                          
         BE    EDITP30             NO SOURCE                                    
         MVC   PUBCSRC,8(R2)                                                    
         MVI   ACTSW,1                                                          
*                                                                               
EDITP30  BAS   RE,NXTUNP                                                        
         LA    R3,6                                                             
         LA    R4,PUBCIR1                                                       
EDITP32  ZAP   0(5,R4),=P'0'                                                    
         LA    R4,5(R4)                                                         
         BCT   R3,EDITP32                                                       
         CLI   5(R2),0                                                          
         BE    EDITP40                                                          
         BAS   RE,EDTCIRC                                                       
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         ZAP   PUBCIR1,DUB+1(5)                                                 
         MVI   ACTSW,1                                                          
*                                                                               
EDITP40  BAS   RE,NXTUNP                                                        
         CLI   ACTSW,0                                                          
         BE    EDITP50                                                          
         MVC   PUBCIREL(2),=X'3032'                                             
         LA    R6,50(R6)                                                        
         L     R0,FULL                                                          
         A     R0,=F'1'                                                         
         ST    R0,FULL                                                          
*                                                                               
EDITP50  BCT   R7,EDITP12                                                       
*                                                                               
*                                  SORT ELEMS IN DATE ORDER                     
         CLC   FULL,=F'0'                                                       
         BE    EDITPX              NO ELEMS                                     
         CLC   FULL,=F'1'                                                       
         BE    EDITP60             NO NEED TO SORT                              
         L     R5,FULL                                                          
         GOTO1 =V(XSORT),DMCB,ELEAREA,(R5),50,3,2,RR=RELO                       
*                                                                               
EDITP60  LA    R5,ELEAREA                                                       
         LA    R6,PUBREC+33                                                     
         MVI   ELCODE,X'30'                                                     
         BAS   RE,NEXTEL                                                        
*                                                                               
EDITP65  GOTO1 VRECUP,DMCB,(1,PUBREC),(R5),(R6)                                 
         LA    R5,50(R5)                                                        
         LA    R6,50(R6)                                                        
         CLI   0(R5),0                                                          
         BE    EDITPX                                                           
         B     EDITP65                                                          
*                                                                               
EDITPX   B     WRTREC                                                           
*                                                                               
NEXTEL   CLI   0(R6),0             END                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                 SAFETY CATCH                                 
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)        RIGHT ELEMENT                                
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  NOT FOUND EXIT                               
*                                                                               
NXTUNP   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NXTUNP                                                           
         BR    RE                                                               
*                                                                               
*                                                                               
WRTREC   SR    R5,R5                                                            
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
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,PUTPUB                                                        
         B     DONE                                                             
         EJECT                                                                  
EDTCIRC  NTR                                                                    
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(0,8(R2)),(R5)                                     
         CLI   DMCB,X'FF'                                                       
         BNE   EDTC10                                                           
         CLI   5(R2),7             CAN'T HAVE EXCEEDED 21 MILLION               
         BNH   EDTCERR                                                          
         ZIC   R5,5(R2)                                                         
         XC    WORK(10),WORK                                                    
         LA    R4,8(R2)                                                         
         LA    R6,WORK                                                          
         SR    R7,R7                                                            
EDTC6    CLI   0(R4),C'.'          DECIMAL ENCOUNTERED - ERROR                  
         BE    EDTCERR                                                          
         CLI   0(R4),C','                                                       
         BE    EDTC8               BYPASS COMMAS                                
         CLI   0(R4),C'0'                                                       
         BL    EDTCERR             NOT NUMERIC                                  
         CLI   0(R4),C'9'                                                       
         BH    EDTCERR             NOT NUMERIC                                  
         LA    R7,1(R7)                                                         
         MVC   0(1,R6),0(R4)                                                    
         LA    R6,1(R6)                                                         
EDTC8    LA    R4,1(R4)                                                         
         BCT   R5,EDTC6                                                         
*                                                                               
         LTR   R7,R7                                                            
         BZ    EDTCERR                                                          
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         PACK  DUB(6),WORK(0)      EXECUTED                                     
         CP    DUB(6),=PL6'99999999'                                            
         BH    EDTCERR                                                          
         MVI   DMCB,0              SET OFF ERROR CODE                           
         B     EDTCX                                                            
*                                                                               
EDTC10   L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'                                                   
         BNE   EDTCERR               MUST GET 0 REMAINDER                       
         CP    DUB(6),=P'0'        CAN'T BE NEGATIVE                            
         BNH   EDTCERR                                                          
         B     EDTCX                                                            
*                                                                               
EDTCERR  MVI   DMCB,X'FF'                                                       
*                                                                               
EDTCX    XIT                                                                    
         EJECT                                                                  
*                                                                               
FORMATP  DS    0H                                                               
         CLI   SAVSCRN,X'E3'                                                    
         BNE   FMT2                                                             
         CLI   BACT,1              SEE IF ADD                                   
         BNE   FMT5                                                             
         MVI   BYTE2,0             TO GENERATE TURNAROUND                       
         B     EDITPUB                                                          
*                                                                               
FMT2     DS    0H                                                               
         LA    R4,PBLLAST                                                       
       GOTO1   VCALLOV,WORK,(R4),X'D90406E3'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRGERR                                                          
         MVI SAVSCRN,X'E3'                                                      
FMT5     CLI   BACT,1              SEE IF ACTION=ADD                            
         BNE   PUTFLDS                                                          
FORMATP1 EQU   *                                                                
         CLI   BACT,3              SRDS                                         
         BE    PROTECT                                                          
         CLC   PUBKAGY(2),AGYALPHA                                              
         BNE   PROTECT                                                          
         LA    R2,CIRDTE1H                                                      
         B     EXIT                                                             
*                                                                               
PUTFLDS  DS    0H                                                               
         LA    R6,PUBREC+33                                                     
         LA    R7,3                                                             
         LA    R2,CIRDTE1H                                                      
         MVI   ELCODE,X'30'                                                     
PUTF10   BAS   RE,NEXTEL                                                        
         BNE   PUTF30                                                           
         FOUT  (R2),SPACES,8                                                    
         OC    PUBCDAT,PUBCDAT                                                  
         BZ    PUTF15                                                           
         CLI   PUBCDAT+2,0                                                      
         BE    PUTF12                                                           
*        GOTO1 VDTCNV,DMCB,(1,PUBCDAT),(3,8(R2))                                
         GOTO1 VDATCON,DMCB,(3,PUBCDAT),(5,8(R2))                               
         B     PUTF15                                                           
*                                                                               
*PUTF12   GOTO1 VDTCNV,DMCB,(1,PUBCDAT),(5,8(R2))                               
PUTF12   GOTO1 VDATCON,DMCB,(3,PUBCDAT),(9,8(R2))                               
*                                                                               
PUTF15   BAS   RE,NXTUNP                                                        
         FOUT  (R2),PUBCSRC,4                                                   
         BAS   RE,NXTUNP                                                        
         EDIT  PUBCIR1,(10,8(R2)),ALIGN=LEFT,COMMAS=YES                         
         FOUT  (R2)                                                             
         BAS   RE,NXTUNP                                                        
         BCT   R7,PUTF10                                                        
         B     PUTFX                                                            
*                                                                               
PUTF30   DS    0H                                                               
         LTR   R7,R7                                                            
         BZ    PUTFX                                                            
         FOUT  (R2),SPACES,8                                                    
         BAS   RE,NXTUNP                                                        
         FOUT  (R2),SPACES,4                                                    
         BAS   RE,NXTUNP                                                        
         FOUT  (R2),SPACES,10                                                   
         BAS   RE,NXTUNP                                                        
         BCT   R7,PUTF30                                                        
*                                                                               
*                                                                               
PUTFX    DS    0H                                                               
         CLI   BACT,3              SRDS                                         
         BNE   CKSRDS                                                           
PROTECT  DS    0H                                                               
         LA    R2,CIRDTE1H                                                      
         LA    R3,9                                                             
PROT10   OI    1(R2),X'20'                                                      
         BAS   RE,NXTUNP                                                        
         BCT   R3,PROT10                                                        
*                                                                               
         MVI   SAVSCRN,0                                                        
         B     DONE                                                             
*                                                                               
CKSRDS   CLC   PUBKAGY(2),AGYALPHA                                              
         BNE   PROTECT                                                          
         CLI   BACT,2                                                           
         BH    DONE                                                             
         LA    R2,CIRDTE1H                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
*                                                                               
SPACES   DC    40C' '                                                           
PUBIND   DS    CL1                                                              
ACTSW    DS    CL1                                                              
ELCODE   DS    CL1                                                              
VIRGERR  DC    H'0'                                                             
COMBERR  EQU   112                                                              
DATERR   EQU   20                                                               
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
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
         EJECT                                                                  
*                                                                               
PUBCIRD  DSECT                                                                  
       ++INCLUDE PUBCIREL                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPUBFFD                                                       
         ORG PBLLAST                                                            
       ++INCLUDE PPPUBE3D                                                       
         ORG T406FFD                                                            
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
**PAN#1  DC    CL21'014PPPUB13   05/01/02'                                      
         END                                                                    
