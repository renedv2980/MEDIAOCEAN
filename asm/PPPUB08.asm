*          DATA SET PPPUB08    AT LEVEL 043 AS OF 09/01/05                      
*PHASE T40608A,+0                                                               
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
* SMYE 09/01/05 DISALLOW DST/REG "999/999" COMBINATION                          
*                                                                               
* SMYE  2/96    INCLUDE PUGENEROL (PUB VERSION OF PPGENEROL)                    
*               ALSO USE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                  
*                                                                               
         TITLE 'T40608 - PUBFILE DIV/REG/DST/SHARE'                             
*                                                                               
T40608   CSECT                                                                  
         NMOD1 0,T40608                                                         
         SPACE 2                                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T406FFD,RA                                                       
         USING PUBDSTEL,R4                                                      
*                                                                               
         XC    PUBIO(35),PUBIO                                                  
         OC    LTLADDR,LTLADDR                                                  
         BZ    DRD2                                                             
         MVC   KEY+27(4),LTLADDR                                                
         BAS   RE,GETPUB                                                        
         B     DRD4                                                             
*                                  INITIALIZE LTLREC                            
DRD2     DS    0H                                                               
         MVC   LTLREC(1),BMED                                                   
         MVC   LTLREC+1(6),BPUB                                                 
         MVC   LTLREC+7(2),AGYALPHA                                             
         MVI   LTLREC+9,X'85'                                                   
         MVC   LTLREC+25(2),=H'33'                                              
*                                                                               
DRD4     DS    0H                                                               
         CLI   SAVSCRN,X'F8'                                                    
         BE    DRD6                                                             
         GOTO1 VCALLOV,DMCB,PBLLAST,X'D90406F8'                                 
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SAVSCRN,X'F8'                                                    
         MVI   FORMAT,1                                                         
DRD6     DS    0H                                                               
         CLI   BACT,1              ADD = EDIT MODE                              
         BE    EDT                                                              
         CLI   FORMAT,1                                                         
         BE    FMT                                                              
         CLI   BACT,4              DISPLAY                                      
         BE    FMT                                                              
         EJECT                                                                  
*                             EDIT SCREEN INPUT                                 
         SPACE 2                                                                
EDT      DS    0H                                                               
*                             FIRST DELETE ALL ELEMS                            
*                             FOR CLT/DIV                                       
         OC    LTLADDR,LTLADDR                                                  
         BZ    EDT6          UNLESS NO RECORD                                   
*                                                                               
         LA    R4,LTLREC+33                                                     
         MVI   ELCODE,X'71'                                                     
EDT2     DS    0H                                                               
         CLI   0(R4),X'71'                                                      
         BE    EDT4                                                             
EDT3     DS    0H                                                               
         BAS   RE,NXTEL                                                         
         BNE   EDT6                                                             
EDT4     DS    0H                                                               
         CLC   PUBDCLT,BCLT                                                     
         BNE   EDT3                                                             
         CLC   PUBDDIV,BDIV                                                     
         BNE   EDT3                                                             
         GOTO1 VRECUP,DMCB,(1,LTLREC),(R4)                                      
*                                                                               
         B     EDT2                                                             
*                                                                               
EDT6     DS    0H                                                               
         LA    R2,DRDRDS1H                                                      
EDT7     DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    EDT30                                                            
         XC    WORK(20),WORK                                                    
*                                  FIND REGION                                  
         GOTO1 NUMED,NUMPARS,8(R2)                                              
*                                                                               
         LA    R3,REGERR1                                                       
         CLI   NUMPARS,0                                                        
         BE    ERROR                                                            
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+8(3),DUB                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BMED                                                    
         MVI   KEY+3,4                                                          
         MVC   KEY+4(3),BCLT                                                    
         MVC   KEY+7(3),BDIV                                                    
         MVC   KEY+10(3),WORK+8                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+12                                                             
*                                                                               
         LA    R3,REGERR2                                                       
         B     ERROR                                                            
*                                  DISTRICT                                     
         L     R4,NUMPARS                                                       
         GOTO1 NUMED,NUMPARS,1(R4)                                              
*                                                                               
         LA    R3,DSTERR1                                                       
         CLI   NUMPARS,0                                                        
         BE    EDT7B                                                            
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+11(3),DUB                                                   
*                                                                               
         MVI   KEY+3,5                                                          
         MVC   KEY+13(3),WORK+11                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+12                                                             
*                                                                               
         LA    R3,DSTERR2                                                       
         B     ERROR                                                            
*                                                                               
EDT7B    DS    0H                  TEST FOR 999/999 REG/DST COMBINATION         
         CLC   WORK+08(3),=C'999'  REGION 999 ?                                 
         BNE   EDT7C               NO                                           
         CLC   WORK+11(3),=C'999'  DISTRICT 999 ?                               
         BNE   EDT7C               NO                                           
*                                                                               
         MVC   PBLMSG(38),=C'*ERROR* - REG/DST CANNOT BE 999/999 **'            
         OI    PBLMSGH+6,X'80'     XMIT                                         
         B     EXIT                                                             
*                                                                               
EDT7C    DS    0H                  SHARE                                        
         L     R4,NUMPARS                                                       
         GOTO1 NUMED,NUMPARS,1(R4)                                              
*                                                                               
         L     R6,NUMPARS+4                                                     
         MH    R6,=H'100'                                                       
         L     R4,NUMPARS                                                       
         GOTO1 NUMED,NUMPARS,1(R4)                                              
*                                                                               
         L     R7,NUMPARS+4                                                     
         CLI   NUMPARS,2                                                        
         BNH   *+12                                                             
*                                                                               
         LA    R3,SHRERR                                                        
         B     ERROR                                                            
*                                                                               
         BE    *+8                                                              
         MH    R7,=H'10'                                                        
         AR    R6,R7                                                            
         BNZ   *+8                                                              
         L     R6,=F'10000'                                                     
         C     R6,=F'10000'                                                     
         BE    *+8                                                              
         STH   R6,WORK+14                                                       
         A     R6,FULL             TOTAL OF PCTS                                
         ST    R6,FULL                                                          
*                                                                               
*                                  CREATE ELEM AND                              
*                                  ADD IT IN SEQUENCE                           
         MVC   WORK(2),=X'7112'                                                 
         MVC   WORK+2(3),BCLT                                                   
         MVC   WORK+5(3),BDIV                                                   
         XC    WORK+16(2),WORK+16                                               
*                                                                               
         LA    R4,LTLREC+33                                                     
         MVI   ELCODE,X'71'                                                     
         CLI   0(R4),X'71'                                                      
         BE    EDT10                                                            
EDT8     DS    0H                                                               
         BAS   RE,NXTEL                                                         
         BNE   EDT12                                                            
*                                                                               
EDT10    DS    0H                                                               
         CLC   PUBDCLT(12),WORK+2                                               
         BL    EDT8                                                             
         BH    EDT12                                                            
         LA    R3,DUPERR                                                        
         B     ERROR                                                            
*                                                                               
EDT12    DS    0H                                                               
         GOTO1 VRECUP,DMCB,(1,LTLREC),WORK,(R4)                                 
*                                                                               
*                                                                               
EDT30    DS    0H                                                               
         BAS   RE,NXTFLD                                                        
         BNZ   EDT7                                                             
*                                  END OF SCREEN                                
         L     R6,FULL                                                          
         LTR   R6,R6                                                            
         BNZ   EDT31                                                            
         CLI   BACT,1                                                           
         BNE   EDT40                                                            
         LA    R3,MISSERR                                                       
         LA    R2,DRDRDS1H                                                      
         B     ERROR               ADD OF 'NOTHING'                             
EDT31    DS    0H                                                               
         C     R6,=F'10000'        CHECK TOTAL = 100.00 PCT                     
         BE    EDT40                                                            
         LA    R2,DRDRDS1H                                                      
         LA    R3,SUMERR                                                        
         B     ERROR                                                            
*                                                                               
*                                  DONE                                         
EDT40    DS    0H                                                               
*                                  PUT/ADD LTLREC                               
         LA    RF,PUTPUB                                                        
         OC    LTLADDR,LTLADDR                                                  
         BNZ   *+8                                                              
         LA    RF,ADDPUB                                                        
         BASR  RE,RF                                                            
*                                                                               
         MVI   DONESW,1                                                         
         B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
*                                  FORMAT DATA ONTO SCREEN                      
         SPACE 2                                                                
FMT      DS    0H                                                               
         LA    R2,DRDRDS1H                                                      
         LA    R4,LTLREC+33                                                     
         MVI   ELCODE,X'71'                                                     
         CLI   0(R4),X'71'                                                      
         BE    FMT4                                                             
FMT2     DS    0H                                                               
         BAS   RE,NXTEL                                                         
         BNE   FMT8                                                             
FMT4     DS    0H                                                               
         CLC   PUBDCLT,BCLT                                                     
         BNE   FMT2                                                             
         CLC   PUBDDIV,BDIV                                                     
         BNE   FMT2                                                             
         XC    8(13,R2),8(R2)         CLEAR FIELD                               
         MVC   8(3,R2),PUBDREG                                                  
         LA    R3,8+3(R2)                                                       
         OC    PUBDDST,PUBDDST                                                  
         BZ    FMT5                                                             
         LA    R3,8+7(R2)                                                       
         MVI   8+3(R2),C'/'                                                     
         MVC   8+4(3,R2),PUBDDST                                                
         OC    PUBDSHR(2),PUBDSHR   CHK FOR SHARE                               
         BZ    FMT7                                                             
         B     FMT6                                                             
*                                                                               
FMT5     DS    0H                                                               
         OC    PUBDSHR(2),PUBDSHR  CHK FOR SHARE                                
         BZ    FMT7                                                             
*                                                                               
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
FMT6     MVI   0(R3),C'/'                                                       
         EDIT  (B2,PUBDSHR),(5,1(R3)),2,ALIGN=LEFT                              
*                                                                               
         OC    PUBDSHR(2),PUBDSHR                                               
         BNZ   *+10                                                             
         MVC   0(6,R3),SPACES                                                   
*                                                                               
FMT7     FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NXTFLD           NEXT SCREEN FIELD                            
         BNZ   FMT2                NEXT ELEM                                    
*                                                                               
*                                                                               
*                                  MORE ELEMS THAN WILL                         
*                                  FIT ON SCREEN                                
*                                                                               
         B     FMT20                                                            
*                                  CLEAR REMAINDER OF SCREEN                    
FMT8     DS    0H                                                               
         OC    8(L'DRDRDS1,R2),SPACES                                           
         CLC   8(L'DRDRDS1,R2),SPACES                                           
         BE    FMT9                                                             
         XC    8(L'DRDRDS1,R2),8(R2)                                            
         FOUT  (R2)                                                             
*                                                                               
FMT9     DS    0H                                                               
         BAS   RE,NXTFLD                                                        
         BNZ   FMT8                                                             
*                                                                               
*                                  DONE                                         
FMT20    DS    0H                                                               
         MVI   DONESW,1                                                         
         LA    R2,PBLACTH                                                       
         CLI   PBLACT,C'D'         DISP                                         
         BE    EXIT                                                             
         MVI   DONESW,0                                                         
         LA    R2,DRDRDS1H                                                      
         B     EXIT                                                             
         SPACE 3                                                                
NUMED    NTR1                                                                   
         SPACE 2                                                                
         L     R2,0(R1)                                                         
         LR    R3,R2                                                            
NUMED2   DS    0H                                                               
         CLI   0(R3),C'9'                                                       
         BH    NUMED4                                                           
         CLI   0(R3),C'0'                                                       
         BL    NUMED4                                                           
         LA    R3,1(R3)                                                         
         B     NUMED2                                                           
*                                                                               
NUMED4   DS    0H                                                               
         ST    R3,0(R1)            A(STOP CHAR)                                 
         XC    4(4,R1),4(R1)                                                    
         SR    R3,R2                                                            
         STC   R3,0(R1)                                                         
         BZ    NUMEDX                                                           
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R0,DUB                                                           
         ST    R0,4(R1)                                                         
NUMEDX   DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
NXTEL    DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    NXTEL2                                                           
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLC   ELCODE,0(R4)                                                     
         BER   RE                                                               
         B     NXTEL                                                            
NXTEL2   LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
NXTFLD   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             SET CC,  = MEANS END OF SCREEN               
         BR    RE                                                               
         SPACE 2                                                                
SPACES   DC    CL20' '                                                          
         EJECT                                                                  
*                                                                               
       ++INCLUDE PUGENEROL                                                      
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
ELCODE   DS    X                                                                
NUMPARS  DS    3F                                                               
REGERR1  EQU   23                  INVALID REGION                               
REGERR2  EQU   46                  REG NOT ON FILE                              
DSTERR1  EQU   24                  INVALID DISTRICT                             
DSTERR2  EQU   47                  DST NOT ON FILE                              
SHRERR   EQU   72                  SHARE INVALID                                
DUPERR   EQU   170                 DUPE REG/DST                                 
SUMERR   EQU   88                  SUM NOT = 100.00                             
ADDERR   EQU   173                 ADD, BUT DATA ALREADY THERE                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PUGENOLD                                                       
*                                                                               
PUBIO    DS    4000C                                                            
*                                                                               
         ORG   PUBIO                                                            
*                                                                               
       ++INCLUDE LTLREC                                                         
*                                                                               
*                                                                               
FORMAT   EQU   BYTE2                                                            
DONESW   EQU   BYTE3                                                            
*                                                                               
*                                                                               
         SPACE 3                                                                
*                                                                               
       ++INCLUDE FLDIND                                                         
         SPACE 3                                                                
MISSERR  EQU   01                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPUBFFD                                                       
*                                                                               
         ORG   PBLLAST                                                          
*                                                                               
       ++INCLUDE PPPUBF8D                                                       
         SPACE 3                                                                
         ORG   T406FFD                                                          
         DS    CL16                                                             
BMED     DS    XL1                                                              
BACT     DS    XL1                                                              
BSCR     DS    XL1                                                              
OLNUM    DS    XL1                                                              
PUBADDR  DS    XL4                                                              
LTLADDR  DS    XL4                                                              
BPUB     DS    XL6                                                              
BCLT     DS    XL3                                                              
BDIV     DS    XL3                                                              
BDATE    DS    CL3                                                              
APROF    DS    CL1                                                              
SAVSCRN  DS    CL1                                                              
APROF13  DS    CL1                                                              
BCODE    DS    CL3                                                              
*                                                                               
*                                                                               
PUBDSTLD DSECT                                                                  
       ++INCLUDE PUBDSTELN                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043PPPUB08   09/01/05'                                      
         END                                                                    
