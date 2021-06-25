*          DATA SET PPPUB12    AT LEVEL 029 AS OF 04/22/03                      
*PHASE T40612A                                                                  
*                                                                               
* SMYE  2/96    INCLUDE PUGENEROL (PUB VERSION OF PPGENEROL)                    
*               ALSO USE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                  
*                                                                               
* SMYE  12/07/95  CHANGED VDTCNV TO VDATCON WITH NEW PARAM'S                    
*                                                                               
         PRINT NOGEN                                                            
*                                                                               
T40612   CSECT                                                                  
         NMOD1 0,T40612                                                         
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
         USING PUBRATEL,R6                                                      
         MVI   ELCOD,X'50'                                                      
         XC    BDIV,BDIV           USED TO SAVE DATE FOUND                      
NEXT50   BAS   RE,NEXTEL                                                        
         BNE   CKCOMB                                                           
         CLI   PUBRSPCE,C' '                                                    
         BNH   NEXT50              NOT NEW RATE                                 
         CLC   PUBRSPCE(2),=C'R='                                               
         BE    NEXT50                                                           
*                                                                               
CKDATES  EQU   *                                                                
         CLC   BDATE(3),PUBRSTRT                                                
         BL    NEXT50                                                           
         OI    PUBIND,X'10'        RATE ELEMENT FOUND                           
         MVC   BDIV,PUBRSTRT       SAVE DATE FOUND                              
*                                                                               
CKCOMB   LA    R3,COMBERR                                                       
         CLI   BACT,1                                                           
         BNE   RATESCRN                                                         
         TM    PUBIND,X'10'                                                     
         BNO   RATESCRN                                                         
         CLC   BDATE(3),PUBRSTRT   RATE ELEMENT FOR BDATE CAN'T EXIST           
         BE    ERROR                                                            
         MVI   PUBIND,0                                                         
         B     RATESCRN                                                         
         EJECT                                                                  
*                                                                               
RATESCRN CLI   BYTE2,1             SEE IF ACTION=FORMAT                         
         BE    FORMATR                                                          
         CLI   BACT,2                                                           
         BH    FORMATR                                                          
*                                                                               
EDIT     DS    0H                                                               
         TM    PUBIND,X'10'                                                     
         BNO   EDT15               NO RATES FOUND                               
EDT2     LA    R6,PUBREC+33                                                     
         MVI   ELCOD,X'50'        DELETE OLD RATES                              
EDT5     BAS   RE,NEXTEL                                                        
         BNE   EDT10                                                            
         CLC   PUBRSTRT,BDIV       DATES MUST MATCH                             
         BNE   EDT5                                                             
         CLI   PUBRSPCE,C' '       MUST BE NEW RATE                             
         BNH   EDT5                                                             
         CLC   PUBRSPCE(2),=C'R='                                               
         BE    EDT5                                                             
         GOTO1 VRECUP,DMCB,(1,PUBREC),0(R6)                                     
         B     EDT2                                                             
*                                                                               
EDT10    DS    0H                                                               
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
EDT15    DS    0H                                                               
         LA    R2,RATDATEH                                                      
         CLI   BACT,1              SEE IF ADD                                   
         BE    EDT17                                                            
         CLC   RATDATE(6),=C'DELETE'                                            
         BNE   EDT17                                                            
         LA    R3,FLDINV                                                        
         OC    BDIV,BDIV                                                        
         BZ    ERROR                                                            
         NI    PBLMEDH+4,X'DF'        UNVALIDATE MEDIA                          
         B     WRITEIT                                                          
*                                                                               
*                                                                               
EDT17    GOTO1 VDATVAL,DMCB,(0,RATDATE),WORK                                    
         LA    R3,DATERR                                                        
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*        GOTO1 VDTCNV,DMCB,(0,WORK),(1,FULL)                                    
         GOTO1 VDATCON,DMCB,(0,WORK),(3,FULL)                                   
         CLC   BDIV,FULL           SEE IF NEW DATE WAS INPUT                    
         BE    EDT19               NO                                           
         LA    R3,ADDERR                                                        
         LA    R4,RATDTS                                                        
         LA    R5,6                                                             
         CLC   0(4,R4),=C'NONE'                                                 
         BE    EDT18                                                            
CKADD    GOTO1 VDATVAL,DMCB,(0,0(R4)),WORK                                      
*        GOTO1 VDTCNV,DMCB,(0,WORK),(1,WORK+10)                                 
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK+10)                                
         CLC   FULL(3),WORK+10                                                  
         BE    ERROR                                                            
         LA    R4,9(R4)                                                         
         BCT   R5,CKADD                                                         
EDT18    MVC   BDIV,FULL                                                        
EDT19    DS    0H                                                               
         LA    R2,RATSP1H                                                       
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
         CLI   5(R2),0             NO RATE INPUT - SKIP                         
         BE    NXTSP2                                                           
         ZIC   R6,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R6)                                     
         CLI   DMCB,X'FF'                                                       
         BNE   EDT25                                                            
EDTERR   LA    R3,2                FLDINV                                       
         B     ERROR                                                            
*                                                                               
EDT25    L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB(8),=P'999999999'                                             
         BH    ERROR                                                            
         MVC   6(5,R5),DUB+3                                                    
         BAS   RE,NXTUNP           BUMP TO NEXT UNPROTECTED FLD                 
         LA    R5,11(R5)                                                        
         LA    R7,1(R7)                                                         
         B     NXTSP5                                                           
*                                                                               
NXTSPACE DS    0H                                                               
         BAS   RE,NXTUNP           BUMP TO NEXT UNPROTECTED FLD                 
NXTSP2   BAS   RE,NXTUNP           BUMP TO NEXT UNPROTECTED FLD                 
*                                                                               
NXTSP5   BCT   R4,EDT20                                                         
*                                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
EDT30    DS    0H                                                               
         LTR   R7,R7                                                            
         BZ    EDT50               NONE TO SORT                                 
         MVI   ELCOD,X'50'                                                      
         LA    R5,ELEAREA                                                       
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING PUBRATEL,R3                                                      
         MVC   PUBRATEL(2),=X'501E'                                             
         MVC   PUBRSTRT(3),BDIV                                                 
EDT35    LA    R6,PUBREC+33                                                     
EDT37    BAS   RE,NEXTEL                                                        
         BNE   EDT40                                                            
         CLC   2(3,R6),PUBRSTRT                                                 
         BNH   EDT40                                                            
         B     EDT37                                                            
EDT40    DS    0H                                                               
         MVC   PUBRSPCE(6),0(R5)                                                
         MVC   PUBRATE(5),6(R5)                                                 
         GOTO1 VRECUP,DMCB,(1,PUBREC),WORK,(R6)                                 
         LA    R5,11(R5)                                                        
         LA    R6,30(R6)                                                        
         BCT   R7,EDT40                                                         
         DROP  R3                                                               
*                                                                               
EDT50    DS    0H                                                               
WRITEIT  MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,PUTPUB                                                        
         B     DONE                                                             
         EJECT                                                                  
FORMATR  DS    0H                                                               
         CLI   SAVSCRN,X'E2'                                                    
         BNE   FMT2                                                             
         CLI   BACT,1              SEE IF ADD                                   
         BNE   FMT5                NO                                           
         CLI   RATDATEH+5,0        CK FOR INPUT IN 1ST FIELD                    
         BE    FMT5                NO REFORMAT SCREEN                           
         BAS   R8,PUTDATES                                                      
         MVI   BYTE2,0             SET FOR EDIT                                 
         B     EDIT                                                             
*                                                                               
FMT2     DS    0H                                                               
         LA    R4,PBLLAST                                                       
         GOTO1 VCALLOV,DMCB,(R4),X'D90406E2'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRGERR                                                          
         MVI   SAVSCRN,X'E2'                                                    
*                                                                               
FMT5     DS    0H                                                               
*                                                                               
PUTFLDS  DS    0H                                                               
         LA    R6,PUBREC+33                                                     
         MVI   ELCOD,X'40'                                                      
         LA    R2,RATSP1H                                                       
         LA    R4,SPCNUM                                                        
*                                                                               
PUTF5    BAS   RE,FINDEL                                                        
         BNE   PUTF15                                                           
         FOUT  (R2),2(R6),6                                                     
         BAS   RE,NXTUNP           BUMP TO NEXT UNPROTECTED FLD                 
         XC    8(10,R2),8(R2)                                                   
         FOUT  (R2)                                                             
         BAS   RE,NXTUNP           BUMP TO NEXT UNPROTECTED FLD                 
         BCT   R4,PUTF5                                                         
         B     PUTF25                                                           
*                                                                               
*              CLEAR REST OF SCREEN ON TELEX TERMINALS                          
*                                                                               
PUTF15   LR    R5,R2               SAVE FIRST AVAIL FIELD                       
PUTF16   FOUT  (R2),SPACES,6                                                    
         BAS   RE,NXTUNP                                                        
         FOUT  (R2),SPACES,10                                                   
         BAS   RE,NXTUNP                                                        
         BCT   R4,PUTF16                                                        
*                                                                               
*                                                                               
PUTF25   DS    0H                                                               
         LA    R4,SPCNUM                                                        
         CLI   BACT,1                                                           
         BE    PUTF50              IF ADD                                       
         TM    PUBIND,X'10'        SEE IF I HAVE RATES TO DISPLAY               
         BO    PUTF25C                                                          
         BAS   R8,PUTDATES                                                      
         LA    R3,NORATE                                                        
         LA    R2,PBLSCRH                                                       
         NI    PBLMEDH+4,X'DF'        UNVALIDATE MEDIA                          
         B     ERROR                                                            
*                                                                               
*PUTF25C  GOTO1 VDTCNV,DMCB,(1,BDIV),(3,RATDATE)                                
PUTF25C  GOTO1 VDATCON,DMCB,(3,BDIV),(5,RATDATE)                                
PUTF26   FOUT  RATDATEH                                                         
         LA    R6,PUBREC+33                                                     
*                                                                               
         USING PUBRATEL,R6                                                      
*                                                                               
         MVI   ELCOD,X'50'                                                      
PUTF30   BAS   RE,NEXTEL                                                        
         BNE   PUTF50                                                           
         CLC   PUBRSTRT(3),BDIV                                                 
         BNE   PUTF30                                                           
         CLI   PUBRSPCE,C' '                                                    
         BNH   PUTF30                                                           
         CLC   PUBRSPCE(2),=C'R='                                               
         BE    PUTF30                                                           
         B     PUTF40                                                           
PUTF40   DS    0H                                                               
         LA    R2,RATSP1H                                                       
         LA    R7,SPCNUM                                                        
PUTF42   CLC   PUBRSPCE(6),8(R2)                                                
         BNE   PUTF45                                                           
         BAS   RE,NXTUNP                                                        
         EDIT  (P5,PUBRATE),(10,8(R2)),2,ALIGN=LEFT                             
         FOUT  (R2)                                                             
         B     PUTF30                                                           
*                                                                               
PUTF45   BAS   RE,NXTUNP                                                        
         BAS   RE,NXTUNP                                                        
         BCT   R7,PUTF42                                                        
         FOUT  (R5),PUBRSPCE,6                                                  
         ZIC   R0,0(R5)                                                         
         AR    R5,R0                                                            
         EDIT  (P5,PUBRATE),(10,8(R5)),2,ALIGN=LEFT                             
         FOUT  (R5)                                                             
         ZIC   R0,0(R5)                                                         
         AR    R5,R0                                                            
         BCT   R4,PUTF30                                                        
         DC    H'0'                TOO MANY RATES                               
*                                                                               
PUTF50   DS    0H                                                               
*                                                                               
PUTFX    DS    0H                                                               
         BAS   R8,PUTDATES                                                      
         CLI   BACT,3              SRDS                                         
         BNE   CKSRDS                                                           
         B     PROTECT                                                          
*                                                                               
PROTECT  OI    RATDATEH+1,X'20'                                                 
         LA    R4,SPCNUM                                                        
         LA    R2,RATSP1H                                                       
PROT5    OI    1(R2),X'20'                                                      
         BAS   RE,NXTUNP                                                        
         OI    1(R2),X'20'                                                      
         BAS   RE,NXTUNP                                                        
         BCT   R4,PROT5                                                         
         MVI   SAVSCRN,0                                                        
*                                                                               
         CLI   BACT,2                                                           
         BH    DONE                                                             
PROTX    LA    R2,RATDATEH                                                      
         B     EXIT                                                             
*                                                                               
CKSRDS   CLC   AGYALPHA,PUBKAGY                                                 
         BNE   PROTECT                                                          
         B     PROTX                                                            
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
NEXT2    IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF REC                                   
         BE    NEXTELX                                                          
         CLC   0(1,R6),ELCOD                                                    
         BER   RE                                                               
         BL    NEXT2                                                            
         CLI   0(R6),X'51'                                                      
         BE    NEXT2                                                            
         B     NEXTELX                                                          
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
*                                                                               
PUTDATES XC    RATDTS,RATDTS                                                    
         LA    R6,PUBREC+33                                                     
         USING PUBRATEL,R6                                                      
         MVI   ELCOD,X'50'                                                      
         LA    R5,6                                                             
DATES1   BAS   RE,NEXTEL                                                        
         BNE   DATES3                                                           
         CLI   PUBRSPCE,C' '                                                    
         BNH   DATES1                                                           
         CLC   PUBRSPCE(2),=C'R='                                               
         BE    DATES1                                                           
DATES2   EQU   *                                                                
*        GOTO1 VDTCNV,DMCB,(1,PUBRSTRT),(3,WORK)                                
         GOTO1 VDATCON,DMCB,(3,PUBRSTRT),(5,WORK)                               
         CLC   RATDATE(8),WORK                                                  
         BE    DATES1                                                           
*                                                                               
         LA    R3,RATDTS                                                        
         LA    R7,6                                                             
DATES2F  CLC   0(8,R3),WORK                                                     
         BE    DATES1                                                           
         CLI   0(R3),0                                                          
         BNE   DATES2H                                                          
         MVC   0(8,R3),WORK                                                     
         MVI   8(R3),C','                                                       
         LA    R2,8(R3)                                                         
         BCT   R5,DATES1                                                        
         B     DATES3                                                           
*                                                                               
DATES2H  LA    R3,9(R3)                                                         
         BCT   R7,DATES2F                                                       
*                                                                               
*                                                                               
DATES3   FOUT  RATDTSH                                                          
         CLI   RATDTS,0                                                         
         BNE   *+12                                                             
         MVC   RATDTS(4),=C'NONE'                                               
         BR    R8                                                               
         MVI   0(R2),C' '          BLANK LAST COMMA                             
         BR    R8                                                               
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
DUPC5    LA    R6,11(R6)                                                        
         B     DUPC2                                                            
*                                                                               
DUPCX    XIT1                                                                   
*                                                                               
FINDEL   DS    0H                                                               
         SR    R0,R0                                                            
FNDEL5   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF REC                                   
         BE    FNDELX                                                           
         CLC   0(1,R6),ELCOD                                                    
         BER   RE                                                               
         B     FNDEL5                                                           
*                                                                               
FNDELX   LTR   R6,R6                                                            
         BR    RE                                                               
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
FLDINV   EQU   2                                                                
SPCNUM   EQU   45                                                               
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
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPUBFFD                                                       
         ORG   PBLLAST                                                          
*                                                                               
       ++INCLUDE PPPUBE2D                                                       
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
**PAN#1  DC    CL21'029PPPUB12   04/22/03'                                      
         END                                                                    
