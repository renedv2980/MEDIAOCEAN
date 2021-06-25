*          DATA SET T40E00     AT LEVEL 036 AS OF 05/01/02                      
*PHASE T40E00A,+0,NOAUTO                                                        
         TITLE 'T40E00 - PRINTPAK ON-LINE BUDGET MAINT'                         
*        CHANGE LOG                                                             
*                                                                               
* SMYE 11/27/00 "PROGRAM REPLACED BY SFM" MESSAGE DISPLAYED                     
*                ALL ACTIVITY DISABLED                                          
*                                                                               
* BPLA  2/7/00  OR BUDCLT AND BUDPRD BEFORE CHECKING PREVIOUSLY                 
*               VALIDATED BIT AND OR AGAIN WHEN ADDING THE BUDGET               
*               RECORD (JUST IN CASE)                                           
*                                                                               
* SMYE 11/30/99 ADDED "OC SPACES" FOR BUDCLT AND BUDPRD AT CHA... DUE           
*               TO "DEATH" WHICH I COULD NOT REPRODUCE OR TRACE                 
*                                                                               
* SMYE 2/13/97  CHANGED EDIT PROC TO TREAT "TOTAL" AS A FINAL ENTRY             
*                  DISREGARDING ENTRIES (IF ANY) FOLLOWING                      
*                                                                               
* SMYE 12/18/95  CHANGED VDTCNV TO VDATCON WITH NEW PARAM'S                     
*                                                                               
* BPLA 12/13/91 DISALLOW MASTER CLIENT CODES                                    
*                                                                               
* ROSA 4/17/90  ADD OFFICE SECURITY LOGIC                                       
* ROSA  2/9/90  ADD CLIENT SECURITY LOGIC                                       
*                                                                               
         SPACE 3                                                                
T40E00   CSECT                                                                  
         NMOD1 250,T40E00                                                       
         SPACE 2                                                                
         LA    R9,4095(RB)                                                      
         LA    R9,1(R9)                                                         
         USING T40E00+4096,R9                                                   
         USING GENOLD,RC                                                        
         USING T40EFFD,RA                                                       
         BAS   RE,INITL                                                         
*                                                                               
         B     EXIT        NO ACTIVITY - DEACTIVATED WEEK OF 11/27/00           
*                                                                               
         OC    BUDMSG,BUDMSG                                                    
         BZ    BUD2                                                             
         XC    BUDMSG,BUDMSG                                                    
         FOUT BUDMSGH                                                           
BUD2     OC    BUDMSG2,BUDMSG2                                                  
         BZ    BUD4                                                             
         XC    BUDMSG2,BUDMSG2                                                  
         FOUT  BUDMSG2H                                                         
BUD4     DS    0H                                                               
         EJECT                                                                  
*                                  MEDIA                                        
MED      LA    R2,BUDMEDH                                                       
         TM    4(R2),X'20'                                                      
         BNZ   CLT                                                              
         BAS   RE,CLRMED                                                        
*                                                                               
         LA    R3,MEDERR                                                        
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUDMED                                                  
         MVI   KEY+3,1                                                          
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         MVC   BUDMEDN,PAGYMED                                                  
         FOUT  BUDMEDNH                                                         
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                  CLIENT                                       
CLT      LA    R2,BUDCLTH                                                       
         OC    BUDCLT,SPACES      JUST IN CASE                                  
         MVI   NEWCLT,0                                                         
         TM    4(R2),X'20'                                                      
         BNZ   PRD                                                              
         BAS   RE,CLRCLT                                                        
*                                                                               
         LA    R3,CLTERR                                                        
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUDMED                                                  
         MVI   KEY+3,2                                                          
         MVC   KEY+4(3),BUDCLT                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         PRINT GEN                                                              
         LA    RF,SECURITY                                                      
         LA    R3,VDATAMGR                                                      
         GOTO1 (RF),DMCB,T40EFFD,PCLTREC,(R3)                                   
         PRINT NOGEN                                                            
         BE    CLT4                                                             
         LA    R3,207                                                           
         B     ERROR                                                            
*                                                                               
**************                                                                  
CLT4     DS    0H                                                               
         SPACE 3                                                                
         MVC   SVCLPROF,PCLTPROF                                                
         MVC   BUDCLTN,PCLTNAME                                                 
         FOUT  BUDCLTNH                                                         
*                                                                               
         LA    R3,MCLTERR                                                       
         CLI   PCLTPROF+5,C'1'                                                  
         BE    ERROR                                                            
*                                                                               
         OI    4(R2),X'20'                                                      
         XC    BUDGN,BUDGN                                                      
         MVC   BUDGN(07),=C'**NET**'                                            
         CLI   PCLTPROF+14,C'N'                                                 
         BE    CLT2                                                             
         MVC   BUDGN(09),=C'**GROSS**'                                          
         CLI   PCLTPROF+14,C'G'                                                 
         BE    CLT2                                                             
         MVC   BUDGN(09),=C'**COST** '                                          
CLT2     DS    0H                                                               
         FOUT  BUDGNH                                                           
         MVI   NEWCLT,C'N'                                                      
         EJECT                                                                  
*                                  PRODUCT                                      
PRD      LA    R2,BUDPRDH                                                       
         OC    BUDPRD,SPACES      JUST IN CASE                                  
         TM    4(R2),X'20'                                                      
         BNZ   EST                                                              
         BAS   RE,CLRPRD                                                        
*                                                                               
         LA    R3,PRDERR                                                        
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
         CLC   BUDPRD(3),=C'ZZZ'                                                
         BE    ERROR                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUDMED                                                  
         MVI   KEY+3,6                                                          
         MVC   KEY+4(3),BUDCLT                                                  
         MVC   KEY+7(3),BUDPRD                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         MVC   BUDPRDN,PPRDNAME                                                 
         FOUT  BUDPRDNH                                                         
         MVC   SAVDIV,PPRDDIV                                                   
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                  ESTIMATE                                     
EST      LA    R2,BUDESTH                                                       
         TM    4(R2),X'20'                                                      
         BNZ   REG                                                              
         BAS   RE,CLREST                                                        
*                                                                               
         LA    R3,ESTERR                                                        
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         UNPK  BUDEST,DUB                                                       
         CLI   5(R2),3                                                          
         BE    EST2                                                             
         FOUT  BUDESTH                                                          
EST2     XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUDMED                                                  
         MVI   KEY+3,7                                                          
         MVC   KEY+4(3),BUDCLT                                                  
         MVC   KEY+7(3),BUDPRD                                                  
         STH   R0,KEY+10                                                        
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         MVC   BUDESTN,PESTNAME                                                 
         FOUT  BUDESTNH                                                         
*                                                                               
*        GOTO1 VDTCNV,DMCB,PESTST,(3,BUDESTP)                                   
         GOTO1 VDATCON,DMCB,(0,PESTST),(5,BUDESTP)                              
*                                                                               
*        GOTO1 (RF),(R1),PESTEND,(3,BUDESTP+11)                                 
         GOTO1 VDATCON,(R1),(0,PESTEND),(5,BUDESTP+11)                          
*                                                                               
         MVI   BUDESTP+9,C'-'                                                   
         FOUT  BUDESTPH                                                         
         MVC   SVESTST,PESTST                                                   
         MVC   SVESTEND,PESTEND                                                 
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                  REGION                                       
REG      LA    R2,BUDREGH                                                       
         TM    4(R2),X'20'                                                      
         BNZ   DST                                                              
         BAS   RE,CLRREG                                                        
*                                                                               
         LA    R3,REGERR                                                        
         CLC   BUDREG,=C'ALL'                                                   
         BE    REGX                                                             
*                                                                               
         TM    4(R2),X'08'                                                      
         BZ    ERROR                                                            
         BAS   RE,PACK                                                          
*                                                                               
         OI    DUB+7,X'0F'                                                      
         UNPK  BUDREG,DUB                                                       
         CLI   5(R2),3                                                          
         BE    REG4                                                             
         FOUT  BUDREGH                                                          
*                                                                               
REG4     LTR   R0,R0                                                            
         BE    REGX                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUDMED                                                  
         MVI   KEY+3,4                                                          
         MVC   KEY+4(3),BUDCLT                                                  
         MVC   KEY+7(3),SAVDIV                                                  
         MVC   KEY+10(3),BUDREG                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         MVC   BUDREGN,PREGNAME                                                 
         FOUT  BUDREGNH                                                         
*                                                                               
REGX     OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                  DISTRICT                                     
DST      LA    R2,BUDDSTH                                                       
         TM    4(R2),X'20'                                                      
         BNZ   ACT                                                              
         BAS   RE,CLRDST                                                        
*                                                                               
         LA    R3,DSTERR                                                        
         CLC   BUDDST,=C'ALL'                                                   
         BE    DSTX                                                             
*                                                                               
         TM    4(R2),X'08'                                                      
         BZ    ERROR                                                            
         BAS   RE,PACK                                                          
*                                                                               
         OI    DUB+7,X'0F'                                                      
         UNPK  BUDDST,DUB                                                       
         CLI   5(R2),3                                                          
         BE    DST2                                                             
         FOUT  BUDDSTH                                                          
*                                                                               
DST2     LTR   R0,R0                                                            
         BZ    DSTX                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUDMED                                                  
         MVI   KEY+3,5                                                          
         MVC   KEY+4(3),BUDCLT                                                  
         MVC   KEY+7(3),SAVDIV                                                  
         MVC   KEY+10(3),BUDREG                                                 
         MVC   KEY+13(3),BUDDST                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         MVC   BUDDSTN,PDSTNAME                                                 
         FOUT  BUDDSTNH                                                         
DSTX     DS    0H                                                               
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                  ACTION                                       
ACT      LA    R2,BUDACTH                                                       
         LA    R3,ACTERR                                                        
         MVI   TOTSW,0             END-OF-INPUT INDICATOR                       
         CLC   =C'ADD',BUDACT                                                   
         BE    ADDR                                                             
         CLC   =C'CHA',BUDACT                                                   
         BE    CHA                                                              
         CLC   =C'DIS',BUDACT                                                   
         BE    DIS                                                              
         CLC   =C'TOT',BUDACT                                                   
         BE    TOT                                                              
         B     ERROR                                                            
         EJECT                                                                  
*                                  ADD ROUTINES                                 
ADDR     DS    0H                                                               
         MVI   EDSW,0                                                           
         CLC   BUDREG,=C'ALL'                                                   
         BE    ERROR                                                            
         CLC   BUDDST,=C'ALL'                                                   
         BE    ERROR                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUDMED                                                  
         MVI   KEY+3,X'18'                                                      
         MVC   KEY+4(3),BUDCLT                                                  
         MVC   KEY+7(3),BUDPRD                                                  
         MVC   KEY+10(3),BUDEST                                                 
         MVC   KEY+13(3),BUDREG                                                 
         MVC   KEY+16(3),BUDDST                                                 
         LA    R3,ADDERR                                                        
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    ERROR               RECORD ALREADY THERE                         
         CLC   KEYSAVE+13(3),=3C'0'     TEST ADDING REG 0                       
         BNE   ADD4                     NO                                      
         CLC   KEY(13),KEYSAVE          YES - CANNOT BE ANY OTHER REG           
         BNE   ADD4                                                             
         LA    R3,ADDERR2                                                       
         B     ERROR                                                            
*                                                                               
ADD4     CLC   KEYSAVE+16(3),=3C'0'     TEST ADDING DIST 0                      
         BNE   ADD6                                                             
         CLC   KEY(16),KEYSAVE          YES - CANNOT BE ANY OTHER DIST          
         BNE   ADD6                                                             
         LA    R3,ADDERR3                                                       
         B     ERROR                                                            
*                                                                               
ADD6     CLC   KEYSAVE+16(3),=3C'0'     TEST ADDING DIST 0                      
         BE    ADD8                                                             
         XC    KEY,KEY                  NO - CANNOT BE A DIST 0                 
         MVC   KEY(16),KEYSAVE                                                  
         MVC   KEY+16(3),=3C'0'                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ADD8                                                             
         LA    R3,ADDERR4                                                       
         B     ERROR                                                            
*                                                                               
ADD8     CLC   KEYSAVE+13(3),=3C'0'     TEST ADDING REG 0                       
         BE    ADD10                                                            
         XC    KEY,KEY                  NO - CANNOT BE A REG 0                  
         MVC   KEY(13),KEYSAVE                                                  
         MVC   KEY+13(3),=3C'0'                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(16),KEYSAVE                                                  
         BNE   ADD10                                                            
         LA    R3,ADDERR5                                                       
         B     ERROR                                                            
*                                                                               
ADD10    LA    RE,PBUDREC                                                       
         LA    RF,1000                                                          
         XCEF                                                                   
*                                                                               
         CLI   NEWCLT,C'N'                                                      
         BNE   ADD12                                                            
*                                  DONT ACCEPT AMTS IF NEW CLIENT               
ADD11    XC    BUDMSG,BUDMSG                                                    
         MVC   BUDMSG(05),=C'ENTER'                                             
         MVC   BUDMSG+6(11),=C'NET AMOUNTS'                                     
         CLI   SVCLPROF+14,C'N'                                                 
         BE    ADD11B                                                           
         MVC   BUDMSG+6(13),=C'GROSS AMOUNTS'                                   
         CLI   SVCLPROF+14,C'G'                                                 
         BE    ADD11B                                                           
         MVC   BUDMSG+6(13),=C'COST AMOUNTS '                                   
ADD11B   DS    0H                                                               
         FOUT  BUDMSGH                                                          
*                                                                               
         BAS   RE,TSTANY                                                        
         LA    R2,BUDLN1H                                                       
         BNZ   EXIT                IF NEW CLI AND DATA ENTERED -                
*                                  LEAVE IT THERE BUT DONT PROCESS              
*                                                                               
*                                  IF NEW CLI AND NO DATA ENTERED -             
*                                  SEND MONTH LIST                              
         BAS   RE,BLDMOS                                                        
         BAS   RE,FMT                                                           
         LA    R2,BUDLN1H                                                       
         B     EXIT                                                             
*                                  NOT A NEW CLI                                
ADD12    BAS   RE,TSTANY                                                        
         BZ    ADD11               NO DATA ENTERED - SEND MONTH LIST            
*                                                                               
ADD14    DS    0H                                                               
*                                       BUILD RECORD                            
         MVC   PBUDKAGY,AGYALPHA                                                
         MVC   PBUDKMED,BUDMED                                                  
         MVI   PBUDKRCD,X'18'                                                   
         MVC   PBUDKCLT,BUDCLT                                                  
**BP**                                                                          
         OC    PBUDKCLT,SPACES                                                  
**BP**                                                                          
         MVC   PBUDKPRD,BUDPRD                                                  
**BP**                                                                          
         OC    PBUDKPRD,SPACES                                                  
**BP**                                                                          
         MVC   PBUDKEST,BUDEST                                                  
         MVC   PBUDKREG,BUDREG                                                  
         MVC   PBUDKDST,BUDDST                                                  
         MVC   PBUDLEN,=H'133'                                                  
         MVC   PBUDELEM(2),=X'0164'                                             
         MVC   PBUDST(12),SVESTST                                               
*                                                                               
         BAS   RE,BLDMOS                                                        
         BAS   RE,EDIT                                                          
*                                                                               
         BAS   RE,ADDREC                                                        
         MVI   EDSW,C'E'                SET TO EDIT NEXT TIME                   
         B     DONE                                                             
         EJECT                                                                  
*                                  CHANGE ROUTINES                              
CHA      CLI   EDSW,C'E'                                                        
         BE    *+12                                                             
         LA    R3,CHGERR           CHANGE NOT VALID                             
         B     ERROR                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUDMED                                                  
         MVI   KEY+3,X'18'                                                      
*                                                                               
*** THE BELOW OC INST'S WERE ADDED ON 11/30/99 DUE TO "DEATH" CAUSED            
*** BY NULL IN 3RD POSN OF A 2-CHAR BUDPRD - COULD NOT REPRODUCE OR             
*** TRACE HOW A NULL GOT THERE TO BEGIN WITH  (SMYE)                            
******   OC    BUDCLT,SPACES                                                    
******   OC    BUDPRD,SPACES                                                    
*                                                                               
CHA3     MVC   KEY+4(3),BUDCLT                                                  
         MVC   KEY+7(3),BUDPRD                                                  
         MVC   KEY+10(3),BUDEST                                                 
         MVC   KEY+13(3),BUDREG                                                 
         MVC   KEY+16(3),BUDDST                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    CHA5                                                             
         DC    H'0'         MUST DIE  - RECORD NOT FOUND                        
*                                                                               
*******  XC    KEY,KEY                                                          
*******  MVC   KEY(25),KEYSAVE                                                  
*******  OC    KEY+4(3),SPACES                                                  
*******  OC    KEY+7(3),SPACES                                                  
*******  BAS   RE,HIGH                                                          
*******  CLC   KEY(25),KEYSAVE                                                  
*******  BE    CHA5                                                             
*******  DC    H'0'                                                             
*                                                                               
CHA5     DS    0H                                                               
         BAS   RE,GETREC                                                        
         MVC   PBUDST(12),SVESTST                                               
         BAS   RE,BLDMOS                                                        
         BAS   RE,EDIT                                                          
         BAS   RE,PUTREC                                                        
         B     DONE                                                             
         EJECT                                                                  
*                                  DISPLAY ROUTINES                             
DIS      DS    0H                                                               
         MVI   EDSW,0                                                           
         CLC   BUDREG,=C'ALL'                                                   
         BE    ERROR                                                            
         CLC   BUDDST,=C'ALL'                                                   
         BE    ERROR                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUDMED                                                  
         MVI   KEY+3,X'18'                                                      
         MVC   KEY+4(3),BUDCLT                                                  
         MVC   KEY+7(3),BUDPRD                                                  
         MVC   KEY+10(3),BUDEST                                                 
         MVC   KEY+13(3),BUDREG                                                 
         MVC   KEY+16(3),BUDDST                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    DIS2                                                             
         LA    R3,NORECERR                                                      
         B     ERROR                                                            
DIS2     BAS   RE,GETREC                                                        
DIS3     BAS   RE,BLDMOS                                                        
         BAS   RE,FMT                                                           
         CLI   TOTSW,1             COMING FROM ADD-CHANGE ?                     
         BE    DIS4                YES                                          
         MVI   EDSW,C'E'                                                        
         CLC   PBUDST(12),SVESTST                                               
         BE    DONE                                                             
         XC    BUDMSG,BUDMSG                                                    
         MVC   BUDMSG(L'PERWRN),PERWRN                                          
         FOUT  BUDMSGH                                                          
DIS4     LA    R2,BUDMEDH                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                  TOTAL ROUTINES                               
TOT      DS    0H                                                               
         MVI   EDSW,0                                                           
         MVI   ERRSW,0                                                          
         CLC   BUDREG,=C'ALL'                                                   
         BE    TOT2                                                             
         CLC   BUDDST,=C'ALL'                                                   
         BNE   ERROR                                                            
TOT2     BAS   RE,BLDMOS                                                        
         MVI   ERRAREA,0                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUDMED                                                  
         MVI   KEY+3,X'18'                                                      
         MVC   KEY+4(3),BUDCLT                                                  
         MVC   KEY+7(3),BUDPRD                                                  
         MVC   KEY+10(3),BUDEST                                                 
         CLC   BUDREG,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   KEY+13(3),BUDREG                                                 
         CLC   BUDDST,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   KEY+16(3),BUDDST                                                 
         BAS   RE,HIGH                                                          
         B     *+8                                                              
TOT4     BAS   RE,SEQ                                                           
         CLC   KEY(13),KEYSAVE          A/M/C/P/E                               
         BNE   TOT20                                                            
         OC    KEYSAVE+13(3),KEYSAVE+13      REG                                
         BZ    TOT6                                                             
         CLC   KEY+13(3),KEYSAVE+13          REG                                
         BNE   TOT20                                                            
TOT6     OC    KEYSAVE+16(3),KEYSAVE+16      DST                                
         BZ    TOT8                                                             
         CLC   KEY+16(3),KEYSAVE+16          DST                                
         BE    TOT8                                                             
         CLC   KEY+13(3),KEYSAVE+13          REG                                
         BE    TOT20                                                            
*                                                                               
TOT8     DS    0H                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         CLC   PBUDST(12),SVESTST                                               
         BE    *+8                                                              
         OI    ERRSW,X'20'         PERIOD ERROR                                 
         XC    BMOLST,BMOLST                                                    
         BAS   RE,BLDMOS                                                        
*                                                                               
         OC    PBUDAMTS(48),PBUDAMTS                                            
         BNZ   TOT9                                                             
         OI    ERRSW,X'80'         NON-MONTHLY                                  
         MVC   FULL,PBUDAMTS+48                                                 
         L     R0,FULL      IN NON-MONTHLY USE TOTAL                            
         A     R0,AMTS+48                                                       
         ST    R0,AMTS+48                                                       
         B     TOT4                                                             
TOT9     DS    0H                                                               
         OI    ERRSW,X'40'         MONTHLY                                      
*                                                                               
         LA    R4,EMOLST                                                        
         LA    R5,AMTS                                                          
         LA    R6,BMOLST                                                        
         LA    R7,PBUDAMTS                                                      
*                                                                               
TOT10    CLC   0(2,R6),0(R4)       BUD DATE VS EST                              
         BL    TOT12                                                            
         BH    TOT13                                                            
*                                                                               
TOT11    CLI   0(R6),X'FF'                                                      
         BE    TOT4                GET NEW REC                                  
         CLI   0(R6),0                                                          
         BE    TOT11A                                                           
*                                                                               
         L     R0,0(R5)                                                         
         A     R0,0(R7)                                                         
         ST    R0,0(R5)                                                         
TOT11A   DS    0H                                                               
         LA    R4,2(R4)                                                         
         LA    R5,4(R5)                                                         
         LA    R6,2(R6)                                                         
         LA    R7,4(R7)                                                         
         B     TOT10                                                            
*                                                                               
TOT12    LA    R6,2(R6)                                                         
         LA    R7,4(R7)                                                         
         B     TOT10                                                            
*                                                                               
TOT13    LA    R4,2(R4)                                                         
         LA    R5,4(R5)                                                         
         B     TOT10                                                            
*                                                                               
TOT20    LA    R4,12                                                            
         LA    R5,AMTS                                                          
         LA    R6,AMTS+48                                                       
TOT21    L     R0,0(R5)                                                         
         A     R0,0(R6)                                                         
         ST    R0,0(R6)                                                         
         LA    R5,4(R5)                                                         
         BCT   R4,TOT21                                                         
*                                                                               
         MVC   PBUDAMTS,AMTS                                                    
         BAS   RE,FMT                                                           
         LA    R2,BUDMEDH                                                       
         TM    ERRSW,X'C0'         TEST BOTH TYPES                              
         BO    TOT22                                                            
         TM    ERRSW,X'20'                                                      
         BNZ   TOT23                                                            
         B     DONE                                                             
*                                                                               
TOT22    XC    BUDMSG,BUDMSG                                                    
         MVC   BUDMSG(L'TYPWRN),TYPWRN                                          
         FOUT BUDMSGH                                                           
*                                                                               
         TM    ERRSW,X'20'                                                      
         BZ    EXIT                                                             
*                                                                               
TOT23    XC    BUDMSG2,BUDMSG2                                                  
         MVC   BUDMSG2(L'PERWRN),PERWRN                                         
         FOUT  BUDMSG2H                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                  FORMAT SCREEN                                
FMT      DS    0H                                                               
         ST    RE,SAVERE                                                        
         LA    R4,BMOLST                                                        
         CLI   BUDACT,C'D'                                                      
         BE    *+8                                                              
         LA    R4,EMOLST                                                        
         LA    R5,PBUDAMTS                                                      
         LA    R2,BUDLN1H                                                       
*                                                                               
FMT2     XC    WORK2(6),WORK2                                                   
         CLI   0(R4),0                                                          
         BE    FMT4                                                             
         MVC   WORK2(6),=C'TOTAL*'                                              
         CLI   0(R4),X'FF'                                                      
         BE    FMT4                                                             
*                                                                               
*        GOTO1 VDTCNV,DMCB,(1,(R4)),(5,WORK2)                                   
         GOTO1 VDATCON,DMCB,(3,(R4)),(9,WORK2)                                  
*                                                                               
FMT4     CLC   8(6,R2),WORK2                                                    
         BE    FMT6                                                             
         MVC   8(6,R2),WORK2                                                    
         FOUT  (R2)                                                             
*                                                                               
FMT6     BAS   RE,BUMPFLD                                                       
         XC    WORK2(15),WORK2                                                  
         OC    0(4,R5),0(R5)                                                    
         BZ    FMT8                                                             
*                                                                               
         L     R0,0(R5)                                                         
         EDIT  (R0),(15,WORK2),2,COMMAS=YES,FLOAT=-,ALIGN=LEFT                  
*                                                                               
FMT8     CLC   8(15,R2),WORK2                                                   
         BE    FMT9                                                             
         MVC   8(15,R2),WORK2                                                   
         FOUT  (R2)                                                             
FMT9     BAS   RE,BUMPFLD                                                       
         CLI   0(R4),X'FF'                                                      
         BE    FMTX                                                             
         LA    R4,2(R4)                                                         
         LA    R5,4(R5)                                                         
         B     FMT2                                                             
*                                                                               
FMTX     L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                  EDIT AMOUNTS                                 
EDIT     DS    0H                                                               
         MVI   TOTSW,0             END-OF-INPUT INDICATOR                       
         ST    RE,SAVERE                                                        
         XC    PBUDAMTS,PBUDAMTS                                                
         LA    R2,BUDLN1H                                                       
EDIT2    CLI   5(R2),0                                                          
         BE    EDIT9                                                            
         LA    R4,PBUDAMTS+48                                                   
         CLC   8(3,R2),=C'TOT'                                                  
*****    BE    EDIT6                                                            
         BE    EDIT5X                                                           
         GOTO1 VDATVAL,DMCB,(2,8(R2)),WORK                                      
*                                                                               
         CLI   DMCB+3,0                                                         
         BNE   *+12                                                             
         LA    R3,DATERR                                                        
         B     ERROR                                                            
*                                                                               
*        GOTO1 VDTCNV,DMCB,WORK,(1,WORK)                                        
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK)                                   
         LA    R3,DATERR2                                                       
         LA    R4,EMOLST                                                        
EDIT4    CLC   WORK(2),0(R4)                                                    
         BE    EDIT5                                                            
         LA    R4,2(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   EDIT4                                                            
         B     ERROR                                                            
*                                                                               
EDIT5    LA    R0,EMOLST                                                        
         SR    R4,R0                                                            
         SLL   R4,1                X 2                                          
         LA    R4,PBUDAMTS(R4)                                                  
         B     EDIT6                                                            
*                                                                               
EDIT5X   MVI   TOTSW,1             INDICATE END-OF-INPUT                        
*                                                                               
EDIT6    BAS   RE,BUMPFLD                                                       
         LA    R3,CSHERR                                                        
         XC    DMCB(24),DMCB                                                    
         CLI   5(R2),0                                                          
         BE    EDIT7                                                            
         IC    R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,8(R2),(R0)                                         
*                                                                               
         CLI   DMCB,0                                                           
         BNE   ERROR                                                            
*                                                                               
EDIT7    DS    0H                                                               
         CLI   TOTSW,1             END-OF-INPUT ?                               
         BE    EDIT7B              YES                                          
         LA    R3,DUPERR                                                        
         OC    0(4,R4),0(R4)                                                    
         BNZ   ERROR                                                            
EDIT7B   MVC   0(4,R4),DMCB+4                                                   
         CLI   TOTSW,1             END-OF-INPUT ?                               
         BE    EDIT10              YES                                          
EDIT8    BAS   RE,BUMPFLD                                                       
         CLI   0(R2),9                                                          
         BNE   EDIT2                                                            
         B     EDIT10                                                           
EDIT9    BAS   RE,BUMPFLD                                                       
         CLI   5(R2),0                                                          
         BE    EDIT8                                                            
         LA    R3,CSHERR                                                        
         B     ERROR                                                            
*                                                                               
EDIT10   DS    0H                                                               
EDIT11   OC    PBUDAMTS+48(4),PBUDAMTS+48    TEST TOTAL INPUT                   
         BNZ   EDIT13                        YES                                
         LA    R2,BUDLN13H                   NO - SEND IT                       
         MVC   8(6,R2),=C'TOTAL*'                                               
         FOUT  (R2)                                                             
         BAS   RE,BUMPFLD                                                       
         LA    R0,12                                                            
         LA    R4,PBUDAMTS                                                      
         LA    R5,PBUDAMTS+48                                                   
EDIT12   L     R1,0(R4)                                                         
         A     R1,0(R5)                                                         
         ST    R1,0(R5)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,EDIT12                                                        
*                                                                               
         L     R0,0(R5)                                                         
         EDIT  (R0),(15,8(R2)),2,COMMAS=YES,FLOAT=-,ALIGN=LEFT                  
*                                                                               
         FOUT  (R2)                                                             
         B     EDITX                                                            
*                                       TOTAL WAS INPUT                         
EDIT13   OC    PBUDAMTS(48),PBUDAMTS    WAS ANYTHING ELSE                       
         BZ    EDITX                    NO                                      
*                                       YES - CHECK TOTAL                       
         LA    R0,12                                                            
         LA    R4,PBUDAMTS                                                      
         XC    WORK(4),WORK                                                     
         LA    R5,WORK                                                          
EDIT14   L     R1,0(R4)                                                         
         A     R1,0(R5)                                                         
         ST    R1,0(R5)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,EDIT14                                                        
*                                                                               
         CLC   WORK(4),PBUDAMTS+48                                              
         BE    EDITX                                                            
         XC    BUDMSG,BUDMSG                                                    
         MVC   BUDMSG(L'TOTERM),TOTERM                                          
         LA    R4,BUDMSG+L'TOTERM+2                                             
         L     R0,WORK                                                          
         EDIT  (R0),(15,(R4)),2,COMMAS=YES,ALIGN=LEFT                           
*                                                                               
         FOUT  BUDMSGH                                                          
         LA    R2,BUDLN1H                                                       
         B     EXIT                                                             
         SPACE 2                                                                
EDITX    L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
TOTERM   DC    C'**ERROR - TOTAL NOT = SUM, SHOULD BE'                          
         SPACE 3                                                                
*                                                                               
BLDMOS   DS    0H                                                               
         ST    RE,SAVERE                                                        
         LA    R4,EMOLST                                                        
         LA    R5,SVESTST                                                       
BLDMOS1  CLI   0(R4),0                                                          
         BNE   BLDMOS6                  ALREADY BUILT                           
         CLI   0(R5),0                  TEST NEEDED                             
         BE    BLDMOS6                                                          
         XC    0(28,R4),0(R4)           CLEAR                                   
*        GOTO1 VDTCNV,DMCB,(R5),(1,WORK)                                        
         GOTO1 VDATCON,DMCB,(0,(R5)),(3,WORK)                                   
*                                                                               
*        GOTO1 (RF),(R1),6(R5),(1,WORK+3)                                       
         GOTO1 VDATCON,(R1),(0,6(R5)),(3,WORK+3)                                
*                                                                               
         MVC   0(2,R4),WORK                                                     
         MVC   24(2,R4),=2X'FF'                                                 
BLDMOS2  CLI   1(R4),12                                                         
         BE    BLDMOS3                                                          
         IC    R1,1(R4)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,3(R4)                                                         
         MVC   2(1,R4),0(R4)                                                    
         B     BLDMOS4                                                          
*                                                                               
BLDMOS3  IC    R1,0(R4)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,2(R4)                                                         
         MVI   3(R4),1                                                          
*                                                                               
BLDMOS4  LA    R4,2(R4)                                                         
         CLC   0(2,R4),WORK+3                                                   
         BL    BLDMOS2                                                          
*                                                                               
BLDMOS6  LA    R0,SVESTST                                                       
         CR    R5,R0                                                            
         BNE   BLDMOSX                                                          
         LA    R4,BMOLST                                                        
         LA    R5,PBUDST                                                        
         B     BLDMOS1                                                          
*                                                                               
BLDMOSX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
DONE     XC    BUDMSG,BUDMSG                                                    
         MVC   BUDMSG(L'COMPMSG),COMPMSG                                        
         LA    R4,BUDMSG+L'COMPMSG+1                                            
         CLI   BUDACT,C'A'                                                      
         BNE   *+10                                                             
         MVC   0(L'ADDMSG,R4),ADDMSG                                            
         CLI   BUDACT,C'C'                                                      
         BNE   *+10                                                             
         MVC   0(L'CHGMSG,R4),CHGMSG                                            
         CLI   BUDACT,C'D'                                                      
         BNE   *+10                                                             
         MVC   0(L'DISMSG,R4),DISMSG                                            
         CLI   BUDACT,C'T'                                                      
         BNE   *+10                                                             
         MVC   0(L'TOTMSG,R4),TOTMSG                                            
*                                                                               
         FOUT  BUDMSGH                                                          
         LA    R2,BUDMEDH                                                       
         CLI   TOTSW,1             COMING FROM ADD-CHANGE ?                     
         BE    DIS3                YES - REDISPLAY                              
         B     EXIT                                                             
         SPACE 3                                                                
COMPMSG  DC    C'ACTION COMPLETED -'                                            
ADDMSG   DC    C'RECORD ADDED'                                                  
CHGMSG   DC    C'RECORD CHANGED'                                                
DISMSG   DC    C'RECORD DISPLAYED'                                              
TOTMSG   DC    C'TOTALS DISPLAYED'                                              
*                                                                               
*                                                                               
PERWRN   DC    C'**WARNING - EST. DATES CHANGED SINCE LAST ACCESS**'            
TYPWRN   DC    C'**WARNING - MONTHLY AND NON-MONTHLY BUDGETS COMBINED'          
         SPACE 3                                                                
         EJECT                                                                  
*                                                                               
CLRMED   DS    0H                                                               
         XC    BUDMEDN,BUDMEDN                                                  
         FOUT  BUDMEDNH                                                         
         NI    BUDMEDH+4,X'DF'                                                  
*                                                                               
CLRCLT   DS    0H                                                               
         XC    BUDCLTN,BUDCLTN                                                  
         FOUT  BUDCLTNH                                                         
         NI    BUDCLTH+4,X'DF'                                                  
*                                                                               
CLRPRD   DS    0H                                                               
         XC    BUDPRDN,BUDPRDN                                                  
         FOUT  BUDPRDNH                                                         
         NI    BUDPRDH+4,X'DF'                                                  
*                                                                               
CLREST   DS    0H                                                               
         XC    BUDESTN,BUDESTN                                                  
         FOUT  BUDESTNH                                                         
         XC    BUDESTP,BUDESTP                                                  
         FOUT  BUDESTPH                                                         
         NI    BUDESTH+4,X'DF'                                                  
*                                                                               
CLRREG   DS    0H                                                               
         XC    BUDREGN,BUDREGN                                                  
         FOUT  BUDREGNH                                                         
         NI    BUDREGH+4,X'DF'                                                  
*                                                                               
CLRDST   DS    0H                                                               
         XC    BUDDSTN,BUDDSTN                                                  
         FOUT  BUDDSTNH                                                         
         NI    BUDDSTH+4,X'DF'                                                  
*                                                                               
         MVI   EDSW,0                                                           
         BR    RE                                                               
         SPACE 3                                                                
BUMPFLD  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
SPACES   DC    20C' '                                                           
         SPACE 3                                                                
TSTANY   DS    0H                                                               
         LA    R2,BUDLN1H                                                       
TSTANY2  CLI   5(R2),0                                                          
         BNER  RE                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             END OF SCREEN                                
         BER   RE                                                               
         B     TSTANY2                                                          
         EJECT                                                                  
*******************************************************                         
       ++INCLUDE PPSECURITY                                                     
*******************************************************                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPGENEROL                                                      
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPGENOLD                                                       
*                                                                               
         DS    1000C                                                            
WORK2    DS    CL64                                                             
SAVERE   DS    F                                                                
AMTS     DS    CL52                                                             
EMOLST   DS    CL28                                                             
BMOLST   DS    CL28                                                             
NEWCLT   DS    X                                                                
ERRSW    DS    X                                                                
*                                                                               
         ORG   IOAREA                                                           
*                                                                               
       ++INCLUDE PBUDREC                                                        
*                                                                               
         ORG   IOAREA                                                           
*                                                                               
       ++INCLUDE PAGYREC                                                        
*                                                                               
         ORG   IOAREA                                                           
*                                                                               
       ++INCLUDE PCLTREC                                                        
*                                                                               
         ORG   IOAREA                                                           
*                                                                               
       ++INCLUDE PPRDREC                                                        
*                                                                               
         ORG   IOAREA                                                           
*                                                                               
       ++INCLUDE PESTREC                                                        
*                                                                               
         ORG   IOAREA                                                           
*                                                                               
       ++INCLUDE PREGREC                                                        
*                                                                               
         ORG   IOAREA                                                           
*                                                                               
       ++INCLUDE PDSTREC                                                        
*                                                                               
         ORG                                                                    
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE T40EFFD                                                        
*                                                                               
SVESTST  DS    CL6                                                              
SVESTEND DS    CL6                                                              
SVCLPROF DS    CL32                                                             
SAVDIV   DS    CL3                                                              
EDSW     DS    X                                                                
TOTSW    DS    X                   END-OF-INPUT INDICATOR                       
         SPACE 3                                                                
*                                  ERROR EQUS                                   
ACTERR   EQU   12                                                               
MEDERR   EQU   13                                                               
CLTERR   EQU   14                                                               
PRDERR   EQU   15                                                               
ESTERR   EQU   16                                                               
REGERR   EQU   23                                                               
DSTERR   EQU   24                                                               
ADDERR   EQU   52                                                               
ADDERR2  EQU   241                                                              
ADDERR3  EQU   242                                                              
ADDERR4  EQU   243                                                              
ADDERR5  EQU   244                                                              
CHGERR   EQU   142                                                              
NORECERR EQU   53                                                               
PERERR   EQU   245                                                              
PERERR2  EQU   246                                                              
CSHERR   EQU   2                                                                
DATERR   EQU   20                                                               
DATERR2  EQU   81                                                               
DUPERR   EQU   247                                                              
NOINERR  EQU   1                                                                
MCLTERR  EQU   147                                                              
*                                                                               
         SPACE 3                                                                
*                                                                               
       ++INCLUDE FLDIND                                                         
*                                                                               
       ++INCLUDE DDOFFICED                                                      
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036T40E00    05/01/02'                                      
         END                                                                    
