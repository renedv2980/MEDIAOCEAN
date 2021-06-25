*          DATA SET REREP2402C AT LEVEL 182 AS OF 04/25/97                      
*          DATA SET REPROFM10  AT LEVEL 182 AS OF 04/25/97                      
*PHASE RE2402C,*                                                                
         TITLE 'MODULE TO SCAN FOR BAD BOOKS'                                   
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 27FEB97 (JRD) DO STATISTICS                                     *             
*                                                                 *             
* 10APR96 (WHO) FOR COUNTING NUMBER OF PROPOSALS                  *             
*                                                                 *             
* 02APR96 (WHO) FOR COUNTING NUMBER OF PROPOSALS WITH ESTIMATED   *             
*                           OR PROJECTED BOOKS                    *             
*                                                                 *             
* TEMP VERSION WILL BE DELETED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     *             
*******************************************************************             
*                                                                               
RE2402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2402,R9,RR=R5                                              
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         L     RE,ADCONLST                                                      
         L     RE,VCOMFACS-ADCONSD(RE)                                          
         MVC   VHEXOUT,CHEXOUT-COMFACSD(RE)                                     
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXITM                                                            
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(2),QREP                                                        
         GOTO1 REPORT                                                           
*                                                                               
         LA    RE,ALLDPTTB                                                      
PC0A     CLC   0(2,RE),=X'FFFF'                                                 
         BE    PC0B                                                             
         CLC   QREP(2),0(RE)                                                    
         BE    PC0B                                                             
         LA    RE,L'ALLDPTTB(RE)                                                
         B     PC0A                                                             
PC0B     MVC   ALLDPTCD,2(RE)                                                   
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING RPROKEY,R4                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,QREP                                                    
         DROP  R4                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    FLAGS,X'FF'-(OLDBK+OKPRO+ALLDPT)                                 
         XC    BKLIST,BKLIST                                                    
         LA    R3,BKLIST                                                        
*                                                                               
PC00     CLC   KEY(9),KEYSAVE                                                   
         BNE   PCX                                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,CONAREA1,            X        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,CONAREA1+(RPROR1ST-RPROHDRD)                                  
         MVI   ELCODE,RPRDSELQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   PC07                                                             
*                                                                               
         USING RPRDSELD,R6                                                      
         LA    RE,SRCCOUNT                                                      
PC04     CLI   0(RE),X'FF'                                                      
         BE    PC06                                                             
         CLC   RPRDSBTP,0(RE)                                                   
         BE    PC06                                                             
         LA    RE,L'SRCCOUNT(RE)                                                
         B     PC04                                                             
*                                                                               
PC06     DS    0H                                                               
         LH    R1,1(RE)                                                         
         LA    R1,1(R1)                                                         
         STH   R1,1(RE)                                                         
*                                                                               
PC07     DS    0H                                                               
         LA    R6,CONAREA1+(RPROR1ST-RPROHDRD)                                  
         USING RPRBKELD,R6                                                      
         MVI   ELCODE,RPRBKELQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   PC10                                                             
*                                                                               
PC08     DS    0H                                                               
         MVC   0(2,R3),RPRBKBYM                                                 
         LA    R3,2(R3)                                                         
         CLC   RPRBKBYM,CHKDAT     OLD BOOK?                                    
         BNL   *+8                 NO                                           
         OI    FLAGS,OLDBK                                                      
*                                                                               
         CLI   RPRBKLEN,RPRBKOVQ   REGULAR BOOK?                                
         BE    PC09                YES                                          
*                                                                               
* CHECK ALL THE UPGRADE BOOKS                                                   
*                                                                               
         LA    RE,UPGCOUNT                                                      
PC08A    CLI   0(RE),X'FF'                                                      
         BE    PC08B                                                            
         CLC   RPRBKFIL,0(RE)                                                   
         BE    PC08B                                                            
         LA    RE,L'UPGCOUNT(RE)                                                
         B     PC08A                                                            
*                                                                               
PC08B    DS    0H                                                               
         LH    R1,1(RE)                                                         
         LA    R1,1(R1)                                                         
         STH   R1,1(RE)                                                         
*                                                                               
PC09     BAS   RE,NEXTEL                                                        
         BE    PC08                                                             
*                                                                               
PC10     DS    0H                                                               
         LA    R6,CONAREA1+(RPROR1ST-RPROHDRD)                                  
         USING RPRACELD,R6                                                      
         MVI   ELCODE,RPRACELQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   PC14                                                             
*                                                                               
PC11     CLI   RPRACTYP,RPRACADD                                                
         BE    PC12                                                             
         BAS   RE,NEXTEL                                                        
         BE    PC11                                                             
         B     PC14                                                             
*                                                                               
PC12     CLC   RPRACDAT,CUTDAT                                                  
         BL    *+8                                                              
         OI    FLAGS,OKPRO                                                      
*                                                                               
PC14     DS    0H                                                               
         LA    R6,CONAREA1+(RPROR1ST-RPROHDRD)                                  
         USING RPRBKELD,R6                                                      
         MVI   ELCODE,RPRDTELQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   PC16                                                             
*                                                                               
PC15     DS    0H                                                               
         AP    PCKOF05B,=P'1'                                                   
         BAS   RE,NEXTEL                                                        
         BE    PC15                                                             
*                                                                               
PC16     DS    0H                                                               
         LA    R6,CONAREA1+(RPROR1ST-RPROHDRD)                                  
         MVI   ELCODE,RPRDPELQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   PC18                                                             
*                                                                               
         USING RPRDPELD,R6                                                      
PC17     DS    0H                                                               
         CLC   RPRDPDPT,ALLDPTCD                                                
         BNE   *+8                                                              
         OI    FLAGS,ALLDPT                                                     
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    PC17                                                             
*                                                                               
PC18     DS    0H                                                               
*                                                                               
PC20     LA    R6,KEY                                                           
         USING RPROKEY,R6                                                       
         CLC   =8X'FF',RPROKMEL                                                 
         BNE   PCSEQ                                                            
*                                                                               
         TM    FLAGS,ALLDPT                                                     
         BZ    *+10                                                             
         AP    DPTCOUNT,=P'1'                                                   
*                                                                               
         ZAP   PCKOF16B,PCKOF05B                                                
         ZAP   PCKOF08B,GRIDINC                                                 
         BAS   RE,DIVPACKD                                                      
         BE    *+10                                                             
         ZAP   PCKOF16B,=P'0'                                                   
         ZAP   DUB,PCKOF16B                                                     
         SRP   DUB,64-1,0     ROUND IT                                          
***      ZAP   PCKOF16B,DUB                                                     
         CVB   R1,DUB                                                           
         MH    R1,=Y(5)                                                         
         LA    R1,DTLGRID(R1)                                                   
         AP    0(5,R1),=P'1'                                                    
*                                                                               
***      MVC   FOO,P                                                            
***      MVI   P,C' '                                                           
***      MVC   P+1(L'P-1),P                                                     
***      LR    RE,R1                                                            
***      EDIT  PCKOF05B,(5,P),ZERO=NOBLANK                                      
***      EDIT  PCKOF16B,(5,P+10),ZERO=NOBLANK                                   
***      EDIT  (P5,0(RE)),(10,P+20),ZERO=NOBLANK                                
***      GOTO1 REPORT                                                           
***      MVC   P,FOO                                                            
*                                                                               
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),RPROKCON                                              
         ZAP   WORK(5),=P'99999999'                                             
         SP    WORK(5),WORK+10(5)                                               
         OI    WORK+4,X'0F'                                                     
         MVC   BYTE,RPROKPRO                                                    
         XI    BYTE,X'FF'                                                       
*                                                                               
         AP    COUNT,=P'1'                                                      
*                                                                               
         CP    PCKOF05B,=P'0'                                                   
******   BE    PC21                                                             
         AP    PCKOF08X,=P'1'                                                   
*                                                                               
         CP    DTLMIN,PCKOF05B                                                  
         BNH   *+10                                                             
         ZAP   DTLMIN,PCKOF05B                                                  
*                                                                               
         CP    DTLMAX,PCKOF05B                                                  
         BNL   *+22                                                             
         ZAP   DTLMAX,PCKOF05B                                                  
         UNPK  MAXCON,WORK(5)                                                   
         MVC   BYTE,MAXPRO                                                      
*                                                                               
         AP    DTLCOUNT,PCKOF05B                                                
*                                                                               
PC21     TM    FLAGS,OKPRO                                                      
         BZ    PC30                                                             
*                                                                               
         MVC   P(2),RPROKRCD       REP CODE                                     
*                                                                               
         UNPK  P+5(8),WORK(5)                                                   
         EDIT  (B1,BYTE),(3,P+15),FILL=0                                        
         DROP  R6                                                               
*                                                                               
         LA    R2,P+20                                                          
         LA    R3,BKLIST                                                        
PC22     DS    0H                                                               
         OC    0(2,R3),0(R3)                                                    
         BZ    PC24                                                             
         GOTO1 VHEXOUT,DMCB,0(R3),0(R2),2,0                                     
         LA    R2,5(R2)                                                         
         LA    R3,2(R3)                                                         
         LA    R0,BKLIST+L'BKLIST                                               
         CR    R3,R0                                                            
         BL    PC22                                                             
*                                                                               
PC24     DS    0H                                                               
*                                                                               
         TM    FLAGS,OLDBK                                                      
         BZ    *+8                                                              
         MVI   P+19,C'!'                                                        
*                                                                               
******   GOTO1 REPORT                                                           
*                                                                               
         AP    BKCOUNT2,=P'1'                                                   
*                                                                               
         TM    FLAGS,OLDBK                                                      
         BZ    *+10                                                             
         AP    BKCOUNT,=P'1'                                                    
*                                                                               
PC30     DS    0H                                                               
         XC    BKLIST,BKLIST                                                    
         LA    R3,BKLIST                                                        
         NI    FLAGS,X'FF'-(OLDBK+OKPRO+ALLDPT)                                 
         ZAP   PCKOF05B,=P'0'                                                   
*                                                                               
PCSEQ    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         CLI   DMCB+8,0                                                         
         BE    PC00                                                             
         DC    H'0'                                                             
*                                                                               
PCX      DS    0H                                                               
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         LA    R2,SRCCOUNT                                                      
PCX02    DS    0H                                                               
         MVC   P(25),=CL25'PROPOSALS WITH SOURCE I:'                            
         CLI   0(R2),X'FF'                                                      
         BE    *+10                                                             
         MVC   P+22(1),0(R2)                                                    
         EDIT  (B2,1(R2)),(8,P+41),ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
         LA    R2,L'SRCCOUNT(R2)                                                
         LA    R0,SRCCEND                                                       
         CR    R2,R0                                                            
         BL    PCX02                                                            
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,UPGCOUNT                                                      
PCX04    DS    0H                                                               
         MVC   P(25),=CL25'UPGRADES WITH SOURCE I:'                             
         CLI   0(R2),X'FF'                                                      
         BE    *+10                                                             
         MVC   P+21(1),0(R2)                                                    
         EDIT  (B2,1(R2)),(8,P+41),ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
         LA    R2,L'UPGCOUNT(R2)                                                
         LA    R0,UPGCEND                                                       
         CR    R2,R0                                                            
         BL    PCX04                                                            
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(20),=CL20'MAX # LINES/PROPOSAL'                                
         MVC   P+21(8),MAXCON                                                   
         MVI   P+29,C','                                                        
         EDIT  (B1,BYTE),(3,P+30),FILL=0                                        
         MVI   P+33,C':'                                                        
         EDIT  DTLMAX,(10,P+39),ZERO=NOBLANK                                    
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(24),=CL24'MIN # LINES/PROPOSAL:'                               
         EDIT  DTLMIN,(10,P+39),ZERO=NOBLANK                                    
         GOTO1 REPORT                                                           
*                                                                               
***      MVC   P(24),=CL24'TOT # LINES:'                                        
***      EDIT  DTLCOUNT,(17,P+32),ZERO=NOBLANK                                  
***      GOTO1 REPORT                                                           
*                                                                               
         ZAP   PCKOF16B,DTLCOUNT                                                
         ZAP   PCKOF08B,PCKOF08X                                                
         BAS   RE,DIVPACKD                                                      
         BE    *+10                                                             
         ZAP   PCKOF16B,=P'-1'                                                  
         MVC   P(24),=CL24'AVG # LINES/PROPOSAL:'                               
         EDIT  PCKOF16B,(17,P+34),1,ZERO=NOBLANK                                
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(32),=CL32'# OF PROPOSALS ADDED AFTER '                         
         GOTO1 DATCON,DMCB,(8,CUTDAT),(11,P+27)                                 
         MVI   P+35,C':'                                                        
         EDIT  BKCOUNT2,(10,P+39),ZERO=NOBLANK                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+10(26),=CL26'WITH BOOK BEFORE       :'                         
         MVC   WORK(2),CHKDAT                                                   
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(3,WORK),(6,P+27)                                    
         EDIT  BKCOUNT,(10,P+39),ZERO=NOBLANK                                   
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(35),=CL35'# OF PROPOSALS FOR THE ALL DPT( ):'                  
         MVC   P+31(1),ALLDPTCD                                                 
         EDIT  DPTCOUNT,(10,P+39),ZERO=NOBLANK                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(32),=CL32'# OF PROPOSALS: '                                    
         EDIT  COUNT,(10,P+39),ZERO=NOBLANK                                     
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(32),=CL32'DETAIL LINE COUNT DISTRIBUTION'                      
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,DTLGRID                                                       
         LA    R3,P                                                             
         LA    R4,FOO                                                           
         MVC   FOO,P                                                            
         SR    R5,R5                                                            
         ZAP   PCKOF05B,=P'0'                                                   
PCX20    DS    0H                                                               
         CP    0(5,R2),=P'0'                                                    
         BE    PCX22                                                            
         EDIT  PCKOF05B,(10,0(R3)),ZERO=NOBLANK,TRAIL=C'+'                      
         EDIT  (P5,0(R2)),(10,0(R4))                                            
         LA    R3,12(R3)                                                        
         LA    R4,12(R4)                                                        
         LA    R0,P+L'P-10                                                      
         CR    R3,R0                                                            
         BL    PCX22                                                            
         GOTO1 REPORT                                                           
         MVC   P,FOO                                                            
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   FOO,P                                                            
         LA    R3,P                                                             
         LA    R4,FOO                                                           
PCX22    DS    0H                                                               
         AP    PCKOF05B,GRIDINC                                                 
         LA    R2,5(R2)                                                         
         LA    R0,DTLGRIDX                                                      
         CR    R2,R0                                                            
         BL    PCX20                                                            
         GOTO1 REPORT                                                           
         MVC   P,FOO                                                            
         GOTO1 REPORT                                                           
EXITM    XMOD1                                                                  
*                                                                               
EXITL    CLI   *,X'FF'                                                          
         B     EXIT                                                             
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL TRY TO REDUCE THE DECIMAL EXCEPTIONS THAT MIGHT             
* HAPPEN ON A 'DP' INSTRUCTION                                                  
*                                                                               
* ON ENTRY:    PCKOF16B            DIVIDEND                                     
*              PCKOF08B            DIVISOR (SHOULD NOT BE P'0')                 
*                                                                               
* ON EXIT:     (CC)                NEQ - DIVIDING BY ZERO                       
*                                  EQ  - QUOTIENT IN PCKOF16B                   
*                                                                               
*              PCKOF16B            QUOTIENT (ROUNDED)                           
*                                                                               
* NOTE: BOWORK1 WILL GET CLOBBERED                                              
***********************************************************************         
DIVPACKD NTR1                                                                   
         CP    PCKOF08B,=P'0'      IS THE DIVISOR 0?                            
         BE    DIVPCKNO            YES, CAN'T DIVIDE BY 0                       
*                                                                               
         SRP   PCKOF16B,1,0        MULTIPLY DIVIDEND BY 10 TO ROUND             
***************                                                                 
* CHECKS IF BOTH THE DIVIDEND AND DIVISOR ARE DIVISIBLE BY 10.                  
* IF THEY ARE WE CAN REDUCE THE NUMBER OF HALF BYTES THEY TAKE UP.              
***************                                                                 
DIVPCK10 CP    PCKOF08B+L'PCKOF08B-1(1),=P'0'                                   
         BNE   DIVPCK20                                                         
         CP    PCKOF16B+L'PCKOF16B-1(1),=P'0'                                   
         BNE   DIVPCK20                                                         
         SRP   PCKOF08B,64-1,0                                                  
         SRP   PCKOF16B,64-1,0                                                  
         B     DIVPCK10                                                         
***************                                                                 
* SEE IF WE CAN REDUCE THE NUMBER OF BYTES THE DIVISOR TAKES UP.                
* IE: A DIVISOR OF '7900' STORED IN PCKOF08B NEED ONLY TAKE UP 3 BYTES.         
***************                                                                 
DIVPCK20 LA    R1,L'PCKOF08B                                                    
         LA    R2,PCKOF08B                                                      
DIVPCK25 CLI   0(R2),0                                                          
         BNE   DIVPCK30                                                         
         LA    R2,1(R2)                                                         
         BCT   R1,DIVPCK25                                                      
***************                                                                 
* R2 = A(PACKED DIVISOR)  AND  R1 = L(PACKED DIVISOR)                           
***************                                                                 
DIVPCK30 LR    RE,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         DP    PCKOF16B,0(0,R2)                                                 
***************                                                                 
* HAVE TO ROUND UP   IF (DIVISOR - REMAINDER)  <  REMAINDER                     
***************                                                                 
         LA    RE,L'PCKOF16B-1                                                  
         SR    RE,R1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         ZAP   WORK(L'PCKOF16B),PCKOF16B(0)                                     
         LA    RF,PCKOF16B+1(RE)   RF = A(REMAINDER)                            
*                                                                               
         LR    RE,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         SP    PCKOF08B,0(0,RF)    PCKOF08B = DIVISOR - REMAINDER               
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CP    PCKOF08B,0(0,RF)    (DIVISOR - REMAINDER) <= REMAINDER ?         
         BH    *+10                        NO                                   
         AP    WORK(L'PCKOF16B),=P'1'   YES, ROUND BY ADDING 1                  
*                                                                               
         ZAP   PCKOF16B,WORK(L'PCKOF16B)                                        
*                                                                               
DIVPCKYS B     EXITOK                                                           
*                                                                               
DIVPCKNO B     EXITL                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
CHKDAT   DC    X'5F0B'                                                          
CUTDAT   DC    X'97001'                                                         
*                                                                               
COUNT    DC    PL5'0'                                                           
BKCOUNT  DC    PL5'0'                                                           
BKCOUNT2 DC    PL5'0'                                                           
*                                                                               
ALLDPTCD DS    C                                                                
DPTCOUNT DC    PL5'0'                                                           
*                                                                               
DTLCOUNT DC    PL16'0'                                                          
DTLMAX   DC    PL5'0'                                                           
DTLMIN   DC    PL5'999999999'                                                   
DTLGRID  DC    40PL5'0'                                                         
DTLGRIDX EQU   *                                                                
GRIDINC  DC    P'25'                                                            
*                                                                               
PCKOF08B DC    PL8'0'                                                           
PCKOF08X DC    PL8'0'                                                           
PCKOF16B DC    PL16'0'                                                          
PCKOF05B DC    PL5'0'                                                           
*                                                                               
MAXCON   DC    CL8' '                                                           
MAXPRO   DC    X'00'                                                            
*                                                                               
SRCCOUNT DS    0XL(1+2)                                                         
         DC    C'T',XL2'00'                                                     
         DC    C'4',XL2'00'                                                     
         DC    C'P',XL2'00'                                                     
         DC    X'FF',XL2'00'       OTHERWISE INVENTORY                          
SRCCEND  EQU   *                                                                
*                                                                               
UPGCOUNT DS    0XL(1+2)                                                         
         DC    C'T',XL2'00'                                                     
         DC    C'4',XL2'00'                                                     
         DC    C'P',XL2'00'                                                     
         DC    X'FF',XL2'00'       OTHERWISE INVENTORY                          
UPGCEND  EQU   *                                                                
*                                                                               
REPLIST  DC    C'CMD4GPIFI1I2I8I9MGRMS1TO'                                      
         DC    X'0000'                                                          
ALLDPTTB DS    0XL3                                                             
         DC    C'AM',C'X'                                                       
         DC    C'CQ',C'X'                                                       
         DC    C'NK',C'X'                                                       
         DC    X'FFFF',C'Z'          DEFAULT BL, PV, SZ, UT                     
*                                                                               
RELO     DS    A                                                                
VHEXOUT  DS    A                                                                
RECLEN   DS    H                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
ELCODE   DS    X                                                                
FLAGS    DS    X                                                                
OLDBK    EQU   X'80'                                                            
OKPRO    EQU   X'40'                                                            
ALLDPT   EQU   X'20'                                                            
*                                                                               
BKLIST   DS    XL(7*2)                                                          
*                                                                               
FOO      DS    CL132                                                            
CONAREA1 DS    CL2000                                                           
         SPACE 2                                                                
*  INCLUDE REGENALL                                                             
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*  INCLUDE REGENPRO                                                             
*  INCLUDE DDCOMFACS                                                            
*  INCLUDE REGENAVLN                                                            
         PRINT OFF                                                              
       ++INCLUDE REGENALL                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENPRO                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE REGENAVLN                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'182REREP2402C04/25/97'                                      
         END                                                                    
