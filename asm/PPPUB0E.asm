*          DATA SET PPPUB0E    AT LEVEL 046 AS OF 05/01/02                      
*PHASE T4060EA,+0                                                               
*INCLUDE NUMED                                                                  
*                                                                               
**********  CHANGE LOG  **********                                              
*                                                                               
*  SMYE 2/96     INCLUDE PUGENEROL (PUB VERSION OF PPGENEROL)                   
*                ALSO USE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                 
*                                                                               
*  SMYE 12/07/95 CHANGED VDTCNV TO VDATCON WITH NEW PARAM'S                     
*                                                                               
*                                                                               
         TITLE 'T4060E  PUBFILE MAINT  OUTDOOR RATE SCREEN'                     
         PRINT NOGEN                                                            
T4060E   CSECT                                                                  
         NMOD1 0,T4060E,RR=R9                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         USING T406FFD,RA                                                       
         LA    R9,PUBIO                                                         
         USING PUBREC,R9                                                        
         LA    R4,PUBIO                                                         
         LA    R5,16                                                            
CLEARIO  XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         BCT   R5,CLEARIO                                                       
         XC    ELEAREA(250),ELEAREA                                             
         XC    ELEAREA+250(250),ELEAREA+250                                     
         LA    R3,53                                                            
         LA    R2,PBLPUBH                                                       
         OC    PUBADDR,PUBADDR                                                  
         BZ    ERROR                                                            
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,GETPUB                                                        
         MVI   PUBIND,0                                                         
         LA    R6,PUBREC+33                                                     
         MVI   ELCOD,X'50'                                                      
NEXT50   BAS   RE,NXTEL                                                         
         BL    *-4                                                              
         BE    CKDATES                                                          
         CLI   0(R6),X'51'                                                      
         BE    NEXT50                                                           
         B     CKCOMB                                                           
*                                                                               
CKDATES  EQU   *                                                                
         USING PUBRATEL,R6                                                      
         CLC   BDATE(3),PUBRSTRT                                                
         BL    NEXT50                                                           
         CLI   BSPACE,0      SEE IF DESC SPECIFIED                              
         BE    CKDTX                                                            
         CLC   PUBRSPCE,BSPACE                                                  
         BE    CKDTX                                                            
         CLI   BSPACE,X'FF'                                                     
         BNE   NEXT50                                                           
         CLC    PUBRSHOW,BSHOW                                                  
         BE    CKDTX                                                            
         B     NEXT50                                                           
*                                                                               
CKDTX    OI    PUBIND,X'10'        RATE ELEMENT FOUND                           
         B     CKCOMB                                                           
*                                                                               
CKCOMB   LA    R3,COMBERR                                                       
         CLI   BACT,1                                                           
         BNE   RATESCRN                                                         
         TM    PUBIND,X'10'                                                     
         BNO   RATESCRN                                                         
         CLC   BDATE(3),PUBRSTRT   RATE ELEMENT FOR BDATE CAN'T EXIST           
         BE    ERROR                                                            
         B     RATESCRN                                                         
         EJECT                                                                  
*                                                                               
RATESCRN CLI   BYTE2,1             SEE IF ACTION=FORMAT                         
         BE    FORMATR                                                          
         CLI   BACT,2                                                           
         BH    FORMATR                                                          
*                                                                               
EDIT     LA    R7,ELEAREA                                                       
         USING RATEEL,R7                                                        
*              FORMAT  RATE ELEMENT                                             
         MVC   PUBRATEL(2),=X'501E'                                             
*                                                                               
         CLI   BACT,1          SEE IF ADD                                       
         BNE   *+16                                                             
         TM    BSCR,X'10'         SECRET SCREEN                                 
         BZ    *+8                                                              
         OI    PUBRTYP,X'08'        SET SECRET CODE                             
*                                                                               
         LA    R2,RATEFFDH                                                      
         GOTO1 VDATVAL,DMCB,(0,RATEFFD),WORK                                    
         LA    R3,DATERR                                                        
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*        GOTO1 VDTCNV,DMCB,(0,WORK),(1,PUBRSTRT)                                
         GOTO1 VDATCON,DMCB,(0,WORK),(3,PUBRSTRT)                               
*                                                                               
ED1      LA    R2,RATDESCH                                                      
         MVC   PUBRSPCE,8(R2)                                                   
         OC    PUBRSPCE,SPACES                                                  
         CLI   BMED,C'O'        OUTDOOR                                         
         BNE   ED1B                                                             
         LA    R3,2                                                             
         MVC   WORK(17),PUBRSPCE                                                
         BAS   RE,CHKOUT         GO CHECK SRI= FORMAT                           
         BNZ   ERROR                                                            
*                                                                               
ED1B     CLI   BACT,1                                                           
         BNE   EDITO                                                            
         CLI   5(R2),0           MUST BE INPUT ON ADD                           
         BNE   *+12                                                             
         LA    R3,MISSERR                                                       
         B     ERROR                                                            
         LA    R3,ADDERR                                                        
         LA    R4,DESDTES                                                       
         LA    R5,21                                                            
CKADD    CLI   0(R4),0                                                          
         BE    EDITA                                                            
         CLC   PUBRSTRT(3),0(R4)                                                
         BNE   CKADD1                                                           
         CLC   PUBRSPCE,3(R4)                                                   
         BE    ERROR                                                            
         CLI   PUBRSPCE,X'FF'                                                   
         BNE   CKADD1                                                           
         CLC   PUBRSHOW,4(R4)                                                   
         BE    ERROR                                                            
*                                                                               
CKADD1   LA    R4,20(R4)           NEXT DATE/DESC                               
         BCT   R5,CKADD                                                         
*                                                                               
         B     EDITA                                                            
*                                                                               
EDITO    CLC   RATBRAT(3),=C'DEL'        SPECIAL DELETE CODE                    
         BE    UPDATE                                                           
*               GO DELETE ELEMENTS                                              
*                                                                               
EDITA    LA    R2,RATBRATH                                                      
         LA    R3,2                INVALID FIELD                                
         MVC   PUBRATE(5),=PL5'0'                                               
         BAS   RE,ANY                                                           
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,RATBRAT),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         CP    DUB(8),=P'999999999'                                             
         BH    ERROR                                                            
         MVC   PUBRATE(5),DUB+3                                                 
*                                                                               
EDIT1    LA    R2,RATDLTYH                                                      
         CLI   RATDLTY,C'*'                                                     
         BE    EDIT1A                                                           
*              *=SHOWING                                                        
*              BLANK=GRP                                                        
         NI    PUBRTYP,X'7F'       SET OFF X'80' BIT                            
         CLI   5(R2),0                                                          
         BE    EDIT2                                                            
         B     ERROR                                                            
EDIT1A   EQU   *                                                                
         OI    PUBRTYP,X'80'                                                    
EDIT2    LA    R2,RATDRTYH                                                      
         XC    PUBDLTYP(2),PUBDLTYP        NOW 2 CHARS                          
         CLI   5(R2),0                                                          
         BE    EDITL                                                            
         MVC   PUBDLTYP(2),RATDRTY                                              
         OC    PUBDLTYP(2),SPACES                                               
         EJECT                                                                  
EDITL    DS    0H                  NO DISCOUNT LEVELS FOR OUTDOOR               
UPDATE   LA    R6,PUBREC+33                                                     
         MVI   ELCOD,X'50'                                                      
UPDATE1  BAS   RE,NXTEL                                                         
         BL    *-4                                                              
         BE    CKDTS                                                            
         CLI   0(R6),X'51'                                                      
         BE    UPDATE1                                                          
         B     UPDATE2                                                          
*                                                                               
CKDTS    CLC   2(3,R6),PUBRSTRT                                                 
         BH    CKDTS1                                                           
         BL    UPDATE2                                                          
         CLC   PUBRSPCE,13(R6)         MATCH SPACE                              
         BE    DELETE50                                                         
         CLI   PUBRSPCE,X'FF'                                                   
         BNE   CKDTS1                                                           
         CLC   PUBRSHOW,14(R6)      MATCH SHOWING                               
         BE    DELETE50                                                         
         B     CKDTS1                                                           
*                                                                               
CKDTS1   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'50'                                                      
         BE    CKDTS                                                            
         CLI   0(R6),X'51'                                                      
         BE    CKDTS1                                                           
         B     UPDATE2     END OF PRESENT RATES                                 
*                                                                               
DELETE50 GOTO1 VRECUP,DMCB,(1,PUBREC),0(R6)                                     
         CLI   0(R6),X'51'                                                      
         BNE   CLEARREC                                                         
         B     DELETE50                                                         
*                                                                               
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
         B     UPDATE2                                                          
*                                                                               
*                                                                               
UPDATE2  CLC   RATBRAT(3),=C'DEL'        SPECIAL DELETE CODE                    
         BNE   UPDATE2A                                                         
         NI    PBLMEDH+4,X'DF'        UNVALIDATE MEDIA                          
         B     WRITEIT                                                          
*                                                                               
*                                                                               
UPDATE2A GOTO1 VRECUP,DMCB,(1,PUBREC),0(R7),0(R6)                               
         B     WRITEIT                 NO DISCOUNT LEVELS FOR OUTDOOR           
*                                                                               
         DROP  R7                                                               
         EJECT                                                                  
WRITEIT  MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,PUTPUB                                                        
         B     DONE                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
         USING PUBRATEL,R6       R6 POINTS TO RATEL                             
FORMATR  DS    0H                                                               
         CLI   SAVSCRN,X'0E'                                                    
         BNE   FMT2                                                             
         CLI   BACT,1              SEE IF ADD                                   
         BNE   FMT5                                                             
         MVI   BYTE2,0                                                          
         BAS   RE,PUTDATES                                                      
         B     EDIT                                                             
*                                                                               
FMT2     LA    R4,PBLLAST                                                       
         GOTO1 VCALLOV,WORK,(R4),X'D90406FE'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRGERR                                                          
         MVI   SAVSCRN,X'0E'                                                    
FMT5     DS    0H                                                               
         CLI   BACT,1                                                           
         BNE   PUTFLDS                                                          
         BAS   RE,PUTDATES                                                      
         B     CKSRDS                                                           
*                R6  POINTS TO PUBRATEL                                         
PUTFLDS  LA    R2,PBLSCRH                                                       
         LA    R3,NORATE                                                        
         TM    PUBIND,X'10'                                                     
         BNZ   *+16                                                             
         BAS   RE,PUTDATES                                                      
         NI    PBLMEDH+4,X'DF'       UNVALIDATE MED FIELD                       
         B     ERROR                                                            
         TM    BSCR,X'10'        TEST SECRET SCREEN                             
         BNZ   PUTF1        YES - SHOW ALL RATES                                
         TM    PUBRTYP,X'08'           SECRET RATE                              
         BZ    PUTF1        NO                                                  
         LA    R3,NORATE         SEND RATE NOT FOUND MESSAGE                    
         B     ERROR                                                            
*                                                                               
*PUTF1    GOTO1 VDTCNV,DMCB,(1,PUBRSTRT),(3,RATEFFD)                            
PUTF1    GOTO1 VDATCON,DMCB,(3,PUBRSTRT),(5,RATEFFD)                            
         FOUT  RATEFFDH                                                         
         MVC   BDATE,PUBRSTRT       SAVE DISPLAYED DATE                         
         CLI   PUBRSPCE,X'FF'                                                   
         BE    PUTSP1                                                           
         MVC   RATDESC,PUBRSPCE                                                 
         B     PUTSPX                                                           
*                                                                               
PUTSP1   LA    R2,RATDESCH                                                      
         LA    RF,12(R2)                                                        
         MVC   8(4,R2),=C'SRI='                                                 
         LA    R5,PUBRSHOW                                                      
         BAS   RE,EDT                                                           
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         LA    R5,PUBRREG                                                       
         BAS   RE,EDT                                                           
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         LA    R5,PUBRILLM                                                      
         BAS   RE,EDT                                                           
*                                                                               
PUTSPX   FOUT  RATDESCH                                                         
         MVC   BSPACE,PUBRSPCE         SAVE DISPLAYED SPACE                     
         MVC   PACKED5(5),PUBRATE                                               
         EDIT  PACKED5,(10,RATBRAT),2,ALIGN=LEFT                                
         FOUT  RATBRATH                                                         
         XC    RATDLTY,RATDLTY                                                  
         TM    PUBRTYP,X'80'                                                    
         BZ    PUT1                                                             
         MVI   RATDLTY,C'*'                                                     
PUT1     MVC   RATDRTY(2),PUBDLTYP          2 CHARS                             
PUT2     FOUT  RATDRTYH                                                         
*                                                                               
         BAS   RE,PUTDATES                                                      
         CLI   BACT,1                                                           
         BE    PUT5                                                             
         OI    RATEFFDH+1,X'20'      PROTECT EFF DATE UNLESS ADD                
         OI    RATDESCH+1,X'20'        PROTECT DESC UNLESS ADD                  
         MVI   SAVSCRN,0                                                        
PUT5     DS    0H                                                               
         CLI   BACT,3              SRDS                                         
         BNE   CKSRDS                                                           
         B     PROTECT                                                          
*                                                                               
*                                                                               
PROTECT  OI    RATEFFDH+1,X'20'                                                 
         OI    RATBRATH+1,X'20'                                                 
         OI    RATDLTYH+1,X'20'                                                 
         OI    RATDRTYH+1,X'20'                                                 
         MVI   SAVSCRN,0                                                        
         CLI   BACT,2                                                           
         BH    DONE                                                             
         BE    PROT2                                                            
         LA    R2,RATEFFDH                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
PROT2    LA    R2,RATBRATH                                                      
         B     EXIT                                                             
CKSRDS   CLC   AGYALPHA(2),PUBKAGY                                              
         BNE   PROTECT                                                          
         CLI   BACT,2                                                           
         BE    PROT2                                                            
         LA    R2,RATEFFDH                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
*    ROUTINES                                                                   
*                                                                               
NXTEL    EQU   *              BUMP TO NEXT ELEMENT AND TEST ELCOD               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    *+12                                                             
         CLC   0(1,R6),ELCOD       SETS CONDITION CODE                          
         BR    RE                                                               
         LTR   R6,R6                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
PUTDATES DS    0H                                                               
         NTR1                                                                   
         XC    DESDTES(250),DESDTES                                             
         XC    DESDTES+250(170),DESDTES+250                                     
         LA    R2,RATDES1H        CLEAR PROTECTED FIELDS                        
         LA    R3,60                                                            
         SR    RE,RE                                                            
PUTD1    IC    RE,0(R2)                                                         
         LR    RF,RE                                                            
         SH    RF,=H'8'                                                         
         LR    R5,RF                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SPACES        ALREADY SPACES                             
         BE    PUTD2                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2)                                                    
         BZ    PUTD2                                                            
*                                                                               
         FOUT  (R2),SPACES,(RF)                                                 
*                                                                               
PUTD2    LA    R2,0(RE,R2)           NEXT FIELD                                 
         BCT   R3,PUTD1                                                         
*                                                                               
         LA    R6,PUBREC+33                                                     
         USING PUBRATEL,R6                                                      
         MVI   ELCOD,X'50'                                                      
         LA    R4,RATDES1H                                                      
         LA    R5,20                                                            
         LA    R3,DESDTES                                                       
         LA    R2,21                                                            
DATES1   BAS   RE,NXTEL                                                         
         BL    *-4                                                              
         BE    DATES2                                                           
         CLI   0(R6),X'51'                                                      
         BE    DATES1                                                           
         B     DATES4                                                           
DATES2   EQU   *                                                                
         CLC   PUBRSTRT,BDATE                                                   
         BNE   DATES2A                                                          
         CLC   PUBRSPCE,BSPACE                                                  
         BE    DATES1                                                           
         CLI   BSPACE,X'FF'                                                     
         BNE   DATES2A                                                          
         CLC   PUBRSPCE(4),BSPACE                                               
         BE    DATES1                                                           
*                                                                               
DATES2A   MVC   0(3,R3),PUBRSTRT                                                
         MVC   3(17,R3),PUBRSPCE                                                
         LA    R3,20(R3)                                                        
         BCT   R5,DATES3                                                        
         BCT   R2,DATES1                                                        
         DC    H'0'       TOO MANY RATE ELEMENTS                                
*                                                                               
DATES3   TM    BSCR,X'10'         SECRET RATE SCREEN                            
         BNZ   DATES31                                                          
         TM    PUBRTYP,X'08'          SECRET RATE                               
         BZ    DATES31                                                          
         B     DATES3Z                                                          
*                                                                               
DATES31  CLI   PUBRSPCE,X'FF'                                                   
         BE    DATES3A                                                          
         MVC   8(17,R4),PUBRSPCE                                                
         B     DATES3X                                                          
*                                                                               
DATES3A  LA    RF,12(R4)                                                        
         MVC   8(4,R4),=C'SRI='                                                 
         LA    R5,PUBRSHOW                                                      
         BAS   RE,EDT                                                           
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         LA    R5,PUBRREG                                                       
         BAS   RE,EDT                                                           
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         LA    R5,PUBRILLM                                                      
         BAS   RE,EDT                                                           
*                                                                               
DATES3X  FOUT  (R4)                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         EDIT  (P5,PUBRATE),(10,8(R4)),2                                        
         FOUT  (R4)                                                             
         SR    R0,R0                                                            
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
*        GOTO1 VDTCNV,DMCB,(1,PUBRSTRT),(3,8(R4))                               
         GOTO1 VDATCON,DMCB,(3,PUBRSTRT),(5,8(R4))                              
         TM    PUBRTYP,X'08'       SECRET RATE                                  
         BZ    *+8                                                              
         MVI   8(R4),C'*'          PUT AN * IN FIRST LETTER OF MTH              
         FOUT  (R4)                                                             
         SR    R0,R0                                                            
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
DATES3Z  BCT   R2,DATES1                                                        
*                                                                               
DATES4   XIT1                                                                   
*                                                                               
         EJECT                                                                  
EDT      DS    0H                                                               
         MVI   0(RF),C'0'                                                       
         LA    R0,1                                                             
         CP    0(3,R5),=P'0'                                                    
         BE    EDT2                                                             
         EDIT  (P3,0(R5)),(4,0(RF)),ALIGN=LEFT                                  
*                                                                               
EDT2     DS    0H                                                               
         AR    RF,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  R6                                                               
         USING PUBRATEL,R7                                                      
*                                                                               
CHKOUT   NTR1                                                                   
         CLC   WORK(4),=C'SRI='                                                 
         BNE   COX                                                              
         XC    PUBRSPCE,PUBRSPCE                                                
         MVI   PUBRSPCE,X'FF'                                                   
         GOTO1 =V(NUMED),DMCB,WORK+4,DUB,RR=RELO                                
         ZAP   PUBRSHOW,DUB                                                     
         L     R5,DMCB                                                          
         CLI   0(R5),C' '                                                       
         BE    COERR                                                            
*                                                                               
         GOTO1 (RF),(R1),1(R5)                                                  
         ZAP   PUBRREG,DUB                                                      
         L     R5,DMCB                                                          
         CLI   0(R5),C' '                                                       
         BE    COERR                                                            
         GOTO1 (RF),(R1),1(R5)                                                  
         ZAP   PUBRILLM,DUB                                                     
         L     R5,DMCB                                                          
         CLI   0(R5),C' '                                                       
         BH    COERR                                                            
COX      DS    0H                                                               
         SR    R0,R0        SET CC OK                                           
COXX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
COERR    LTR   RE,RE                                                            
         B     COXX                                                             
*                                                                               
         EJECT                                                                  
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
*                                                                               
SPACES   DC    40C' '                                                           
PUBIND   DS    CL1                                                              
ACTSW    DS    CL1                                                              
PACKED5  DS    PL5                                                              
ELCOD    DS    CL1                                                              
NORATE   EQU   129                                                              
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
*                                                                               
         LTORG                                                                  
*                                                                               
ELEAREA  DS    500C                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE PUGENEROL                                                      
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
       ++INCLUDE PPPUBFED                                                       
         DS    0D                                                               
DESDTES  DS    420C                                                             
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
**PAN#1  DC    CL21'046PPPUB0E   05/01/02'                                      
         END                                                                    
