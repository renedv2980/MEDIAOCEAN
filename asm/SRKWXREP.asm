*          DATA SET SRKWXREP   AT LEVEL 003 AS OF 05/01/02                      
*PHASE KWXREP,*                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DMFILES,(DMCNTL,SYS1FLES)                                              
*INCLUDE DMDMGR                                                                 
*INCLUDE DMDARPS                                                                
*INCLUDE DMISRPS                                                                
*INCLUDE DMUTLSR                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE LOGO                                                                   
*INCLUDE LOGOC                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE ROLLER                                                                 
*INCLUDE SCANNER                                                                
*INCLUDE SORTER                                                                 
         TITLE 'KWX USAGE REPORT'                                               
KWXREP   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**KWXREP,=V(REGSAVE),R9                                        
         L     RC,=V(CPRINT)                                                    
         USING DPRINT,RC                                                        
         LA    RA,SORTREC                                                       
         USING SORTD,RA                                                         
         LA    R8,IOA                                                           
         USING KWXACD,R8                                                        
         B     K01                                                              
         EJECT                                                                  
*              READ AND VALIDATE PARAMETER CARDS                                
         SPACE 1                                                                
K01      MVC   TITLE+13(16),=C'KWX USAGE REPORT'                                
         MVC   P(15),=C'PARAMETER CARDS'                                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P(15),=15C'-'                                                    
         MVI   SPACING+3,C'2'                                                   
         BASR  RE,RF                                                            
         MVI   SPACING+3,C'1'                                                   
         SPACE 1                                                                
K05      GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         CLC   CARD(2),=C'/*'                                                   
         BE    K20                                                              
         SPACE 1                                                                
K06      CLC   CARD(6),=C'PERIOD'  PERIOD=MMMDDYY-MMMDDYY                       
         BNE   K10                                                              
         LA    R3,CARD+21                                                       
         LA    R4,1                                                             
         LNR   R4,R4                                                            
         LA    R5,CARD+7                                                        
         CLI   0(R3),C'-'                                                       
         BE    *+12                                                             
         BXH   R3,R4,*-8                                                        
         B     KERROR                                                           
         LA    R4,START                                                         
         BAS   R7,VALDATE                                                       
         LA    R4,END                                                           
         LA    R5,1(R3)                                                         
         LA    R7,K05                                                           
VALDATE  GOTO1 =V(DATVAL),DMCB,(0,(R5)),(R4)                                    
         OC    DMCB,DMCB                                                        
         BZ    KERROR                                                           
         BR    R7                                                               
         SPACE 1                                                                
K10      XC    IOA(230),IOA        SCAN FOR OTHER PARAMETERS                    
         GOTO1 =V(SCANNER),DMCB,(C'C',CARD),IOA                                 
         CLI   DMCB+4,0                                                         
         BE    KERROR                                                           
         LA    R5,IOA                                                           
         SPACE 1                                                                
K11      OC    0(2,R5),0(R5)                                                    
         BE    K05                                                              
         ZIC   R3,0(R5)                                                         
         BCTR  R3,0                                                             
         ZIC   R4,1(R5)                                                         
         SPACE 1                                                                
         EX    R3,FORMACLC                                                      
         BNE   K12                                                              
         MVC   FORMAT,22(R5)                                                    
         B     K05                                                              
         SPACE 1                                                                
K12      EX    R3,DETAICLC         DETAILS=NO                                   
         BNE   K13                                                              
         CLI   22(R5),C'N'                                                      
         BNE   K05                                                              
         MVI   DETAILS,C'N'                                                     
         B     K05                                                              
         SPACE 1                                                                
K13      EX    R3,INPUTCLC         INPUT=TAPE OR DISC                           
         BNE   K14                                                              
         CLI   22(R5),C'T'                                                      
         BNE   K05                                                              
         MVI   MT,C'Y'                                                          
         B     K05                                                              
         SPACE 1                                                                
K14      LA    R2,KEYTAB           OTHERWISE RATES                              
         LA    R1,MIDRATES                                                      
K14A     CLC   0(80,R1),CARD                                                    
         BE    K15                                                              
         CLC   0(80,R1),SPACES                                                  
         BE    *+12                                                             
         LA    R1,80(R1)                                                        
         B     K14A                                                             
         MVC   0(80,R1),CARD                                                    
K15      CLI   0(R2),0                                                          
         BE    KERROR                                                           
         EX    R3,KEYWDCLC                                                      
         BE    *+12                                                             
         LA    R2,L'KEYTAB(R2)                                                  
         B     K15                                                              
         L     R6,8(R2)                                                         
         CLC   FORMAT,12(R5)       K OR D                                       
         BNE   KERROR                                                           
         SPACE 1                                                                
         CLI   FORMAT,C'D'         PER DESTINATION RATES                        
         BNE   K16                                                              
         GOTO1 =V(CASHVAL),DMCB,(5,22(R5)),(R4)                                 
         CLI   DMCB,X'FF'                                                       
         BE    KERROR                                                           
         MVC   0(4,R6),DMCB+4                                                   
         LA    R5,32(R5)                                                        
         B     K11                                                              
         SPACE 1                                                                
K16      CLI   FORMAT,C'K'         SLIDING SCALE RATES PER COPY/PAGE            
         BNE   K05                 EG KRATE=1.25/1,0.5/2,0.25/5                 
         MVC   12(10,R5),22(R5)    CONVERT SCANNER ENTRIES TO GIVE              
         MVC   22(10,R5),SPACES    L'RATE,L'PER,RATE(AT+12),PER(AT+22)          
         MVC   0(1,R5),1(R5)                                                    
         LA    R2,1                                                             
         LNR   R2,R2                                                            
         SR    R4,R4                                                            
K17      OC    0(2,R5),0(R5)                                                    
         BZ    K18                                                              
         LA    R3,12(R5)                                                        
         LA    R1,20(R5)                                                        
         IC    R4,0(R5)                                                         
         CLI   0(R1),C'/'                                                       
         BE    *+12                                                             
         BXH   R1,R2,*-8                                                        
         B     KERROR                                                           
         LR    R0,R1                                                            
         SR    R0,R3                                                            
         STC   R0,0(R5)            LENGTH OF RATE                               
         SR    R4,R0                                                            
         BCTR  R4,0                                                             
         STC   R4,1(R5)            LENGTH OF PER                                
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BM    KERROR                                                           
         EX    R4,K17MVC           PER                                          
         LA    R5,32(R5)                                                        
         B     K17                                                              
K17MVC   MVC   22(0,R5),1(R1)                                                   
         SPACE 1                                                                
K18      LA    R5,IOA              NOW VALIDATE RATE/PER AND SAVE IN A          
         LR    R7,R6               TABLE AT R6 IN DESCENDING PER SEQNCE         
         SR    R4,R4                                                            
K19      OC    0(2,R5),0(R5)                                                    
         BZ    K05                                                              
         IC    R4,0(R5)            SET UP 2 WD ENTRY (PER/RATE) IN WORK         
         GOTO1 =V(CASHVAL),DMCB,(2,12(R5)),(R4)                                 
         CLI   DMCB,X'FF'                                                       
         BE    KERROR                                                           
         MVC   WORK+4(4),DMCB+4                                                 
         IC    R4,1(R5)                                                         
         BCTR  R4,0                                                             
         MVC   WORK+10(10),=10C'0'                                              
         EX    R4,K19MVZ                                                        
         CLC   WORK+10(10),=10C'0'                                              
         BNE   KERROR                                                           
         EX    R4,K19PACK                                                       
         CVB   R1,DUB                                                           
         ST    R1,WORK                                                          
         SPACE 1                                                                
         LR    R1,R6               ADD ENTRY INTO TABLE AT R6, WHICH            
K19A     CLC   0(4,R1),WORK        ENDS AT R7 WITH ZERO FULL WORD               
         BE    KERROR                                                           
         BL    *+12                                                             
         LA    R1,8(R1)                                                         
         B     K19A                                                             
         BCTR  R1,0                                                             
         LR    R2,R7                                                            
         LA    R0,8                                                             
         LNR   R0,R0                                                            
         LA    R7,8(R7)                                                         
         MVC   8(8,R2),0(R2)                                                    
         BXH   R2,R0,*-6                                                        
         MVC   1(8,R1),WORK                                                     
         LA    R5,32(R5)                                                        
         B     K19                                                              
K19MVZ   MVZ   WORK+10(0),22(R5)                                                
K19PACK  PACK  DUB,22(0,R5)                                                     
         SPACE 1                                                                
INPUTCLC CLC   12(0,R5),=C'INPUT'                                               
KEYWDCLC CLC   12(0,R5),0(R2)                                                   
DETAICLC CLC   12(0,R5),=C'DETAILS'                                             
FORMACLC CLC   12(0,R5),=C'FORMAT'                                              
         SPACE 1                                                                
KERROR   MVC   P(21),=C'**INVALID PARAMETER**'                                  
         GOTO1 =V(PRINTER)                                                      
         B     KXIT                                                             
         SPACE 1                                                                
KEYTAB   DS    0CL12                                                            
         DC    CL8'KRATE   ',AL4(KRTAB)                                         
         DC    CL8'KPAGE   ',AL4(KPTAB)                                         
         DC    CL8'DRATE   ',AL4(DRATE)                                         
         DC    CL8'DPAGE   ',AL4(PRATE)                                         
         DC    CL8'DLINE   ',AL4(LRATE)                                         
         DC    CL8'DWORD   ',AL4(WRATE)                                         
         DC    CL8'DCHAR   ',AL4(CRATE)                                         
         DC    X'00'                                                            
         EJECT                                                                  
*              READ KWXFILE AND SORT EXTRACT                                    
         SPACE 1                                                                
K20      LA    R2,DMFLIST                                                       
         CLI   MT,C'N'                                                          
         BE    K22                                                              
         LA    R7,KWXTAPE                                                       
         OPENR (R7)                                                             
         LA    R2,8(R2)                                                         
         SPACE 1                                                                
K22      GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'SERVICE',(R2)                     
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,(40,V(SORTAREA))                
         SPACE 1                                                                
K24      CLI   MT,C'N'                                                          
         BE    K25                                                              
         GET   (R7),(R8)                                                        
         B     K28                                                              
         SPACE 1                                                                
K25      GOTO1 =V(DATAMGR),DMCB,(X'FF',=C'DMRSEQ'),=C'KWXFILE',DMDA,   X        
               IOA,V(TRACKBUF)                                                  
         TM    DMCB+8,X'80'                                                     
         BO    KEOF                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DISC ERROR                                   
         SPACE 1                                                                
K28      CLC   KWXADATE,START      CHECK DATE IN RANGE                          
         BL    K24                                                              
         CLC   KWXADATE,END                                                     
         BH    KEOF                                                             
         SPACE 1                                                                
K30      XC    SORTD(SLENEQ),SORTD BUILD BASIC SORT RECORD                      
         LA    R1,SLENEQ                                                        
         STH   R1,SLEN                                                          
         MVC   IDNO,KWXAUSER                                                    
         MVI   TYPE,C'O'                                                        
         BAS   RE,GETNAME                                                       
         MVC   SAUTH,KWXAAUTH                                                   
         MVC   SDATE,KWXADATE                                                   
         MVC   STIME,KWXATIME                                                   
         MVC   SREF,KWXAREF                                                     
         MVC   SSENDTO,KWXASENT                                                 
         MVI   SCOPIES+3,1                                                      
         MVC   SPAGES,KWXAPAGE                                                  
         MVC   SLINES,KWXALINE                                                  
         MVC   SWORDS,KWXAWORD                                                  
         MVC   SCHARS,KWXACHAR                                                  
         SPACE 1                                                                
K31      SR    R2,R2               PUT A RECORD TO SORTER EITHER PER            
         MVI   TYPE,C'D'           KWX (FORMAT=K) OR PER KWX COPY (D)           
         LA    R3,KWXDEL                                                        
         LA    R4,2                                                             
         SPACE 1                                                                
K32      CLI   0(R3),0             LOOK FOR A DESTID STRING ELEMENT             
         BNE   K33                                                              
         CLI   FORMAT,C'K'                                                      
         BNE   K24                                                              
         LTR   R2,R2               ANY COPIES                                   
         BZ    K24                                                              
         ST    R2,SCOPIES                                                       
         GOTO1 =V(SORTER),DMCB,=C'PUT',(RA)                                     
         B     K24                                                              
         SPACE 1                                                                
K33      CLI   0(R3),KWXDELQ                                                    
         BE    K35                                                              
         ZIC   R5,1(R3)                                                         
         AR    R3,R5                                                            
         B     K32                                                              
         SPACE 1                                                                
K35      ZIC   R5,1(R3)            SET UP BXLE TO PROCESS STRING                
         AR    R5,R3                                                            
         BCTR  R5,0                                                             
         LA    R3,2(R3)                                                         
         SPACE 1                                                                
K36      CLI   FORMAT,C'K'         IF FORMAT IS K INCREMENT COUNT OF            
         BNE   *+12                COPIES                                       
         LA    R2,1(R2)                                                         
         B     K37                 OTHERWISE                                    
         MVC   IDNO,0(R3)          PUT A RECORD TO SORTER PER DESTID            
         BAS   RE,GETNAME                                                       
         GOTO1 =V(SORTER),DMCB,=C'PUT',(RA)                                     
K37      BXLE  R3,R4,K36                                                        
         B     K32                                                              
         SPACE 1                                                                
KEOF     CLI   MT,C'N'             ANY MORE MAGNETIC TAPES                      
         BE    K40                                                              
         CLOSER (R7)                                                            
         MVC   WORK(20),OPMESS1                                                 
         GOTO1 =V(LOGIO),DMCB,1,(20,WORK)                                       
         MVC   WORK(20),OPMESS2                                                 
         BASR  RE,RF                                                            
         MVI   DMCB+3,0                                                         
         MVI   DMCB+4,1                                                         
         BASR  RE,RF                                                            
         CLI   WORK,C'N'                                                        
         BE    K40                                                              
         OPENR (R7)                                                             
         B     K24                                                              
         SPACE 1                                                                
DMFLIST  DC    C'NKWXFILENCTFILE X'                                             
SORTCARD DC    CL80'SORT FIELDS=(5,99,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(200,,,,)'                             
OPMESS1  DC    CL20'ANY MORE INPUT TAPES'                                       
OPMESS2  DC    CL20'ANSWER YES OR NO'                                           
         EJECT                                                                  
*              READ BACK SORTED RECORDS AND PRINT REPORT                        
         SPACE 1                                                                
K40      GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     RA,DMCB+4                                                        
         LTR   RA,RA                                                            
         BZ    KXIT                NOTHING TO PRINT                             
         SPACE 1                                                                
K42      ZAP   PAGE,=P'1'          INITIALISE FOR REPORT                        
         ZAP   LINE,=P'99'                                                      
         GOTO1 =V(ROLLER),DMCB,0,ACCUMS,5,8                                     
         MVC   MID1(6),=C'SENDER'                                               
         MVC   MID2(13),=C'AUTHORIZATION'                                       
         MVC   MID1+80(30),MIDRATES                                             
         MVC   MID2+80(30),MIDRATES+80                                          
         MVC   MID3+80(30),MIDRATES+160                                         
         MVC   SUB1(110),SUBA                                                   
         MVC   SUB2(110),SUBB                                                   
         CLI   FORMAT,C'D'                                                      
         BNE   *+16                                                             
         MVC   SUB1+64(6),SPACES                                                
         MVC   SUB2+64(6),SPACES                                                
         CLI   DETAILS,C'N'                                                     
         BNE   K43                                                              
         MVC   SUB1+37(24),SPACES                                               
         MVC   SUB2+37(24),SPACES                                               
K43      OC    START,START                                                      
         BZ    K50                                                              
         MVC   MID3(6),=C'PERIOD'                                               
         GOTO1 =V(DATCON),DMCB,(0,START),(8,MID3+15)                            
         MVI   MID3+23,C'-'                                                     
         GOTO1 =V(DATCON),DMCB,(0,END),(8,MID3+24)                              
         B     K50                                                              
SUBA     DC  C'DESTINATION                          DATE     TIME  REF'         
         DC  C'ERENCE   COPIES PAGES  LINES   WORDS    CHARS    CHARGE'         
SUBB     DC  C'-----------                          ----     ----  ---'         
         DC  C'------   ------ -----  -----   -----    -----    ------'         
         SPACE 1                                                                
K45      GOTO1 =V(SORTER),DMCB,=C'GET'  GET NEXT SORTED RECORD                  
         L     RA,DMCB+4                                                        
         LTR   RA,RA                                                            
         BNZ   K48                                                              
         LA    RA,SORTREC                                                       
         MVI   SKEY,X'FF'          SET KEY HIGH AT EOF                          
         MVC   SKEY+1(L'LASTKEY-1),SKEY                                         
         SPACE 1                                                                
K48      CLC   LASTKEY,SKEY        CONTROL BREAK CHECKS                         
         BE    K60                                                              
         LA    R2,2                CHANGE OF DESTINATION                        
         CLI   FORMAT,C'K'         (NO ACTION FOR FORMAT K)                     
         BE    *+8                                                              
         BAS   RE,TOTALS                                                        
         CLC   LASTKEY(L'SORIGIN+L'SAUTH),SKEY                                  
         BE    K61                                                              
         LA    R2,3                CHANGE OF AUTHORISATION                      
         BAS   RE,TOTALS                                                        
         CLC   LASTKEY(L'SORIGIN),SKEY                                          
         BE    K55                                                              
         LA    R2,4                CHANGE OF SENDER                             
         BAS   RE,TOTALS                                                        
         CLI   SKEY,X'FF'                                                       
         BNE   K50                                                              
         ZAP   LINE,=P'99'         EOF                                          
         MVC   MID1(50),MID3                                                    
         MVC   MID2(50),SPACES                                                  
         MVC   MID3(50),SPACES                                                  
         MVC   SUB1(61),SPACES                                                  
         MVC   SUB2(61),SPACES                                                  
         LA    R2,5                                                             
         BAS   RE,TOTALS                                                        
         SPACE 1                                                                
KXIT     DS    0H                                                               
         EOJ                                                                    
         SPACE 1                                                                
K50      DS    0H                  FIRST FOR ACTIONS                            
         MVC   MID1+15(L'SORIGIN),SORIGIN                                       
K55      ZAP   LINE,=P'99'                                                      
         MVC   MID2+15(L'SAUTH),SAUTH                                           
         SPACE 1                                                                
K60      CP    LINE,MAXLINE                                                     
         BNH   K62                                                              
K61      MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'SDESTID),SDESTID                                             
K62      MVC   LASTKEY,SKEY                                                     
         LA    R7,ACCUMS+8                                                      
         USING ACCUMD,R7                                                        
         MVC   ACCKWX,=F'1'                                                     
         MVC   ACCCOPY,SCOPIES                                                  
         MVC   ACCPAGE,SPAGES                                                   
         MVC   ACCLINE,SLINES                                                   
         MVC   ACCWORD,SWORDS                                                   
         MVC   ACCCHAR,SCHARS                                                   
         SPACE 1                                                                
K64      CLI   FORMAT,C'D'         CALCULATE SIMPLE CHARGES (D)                 
         BNE   K66                                                              
         LA    R3,ACCKWX                                                        
         LA    R4,DRATE                                                         
         LA    R5,6                                                             
         SR    R6,R6                                                            
K65      L     R1,0(R3)                                                         
         M     R0,0(R4)                                                         
         SLDA  R0,1                                                             
         D     R0,=F'1000'                                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         AR    R6,R1                                                            
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,K65                                                           
         ST    R6,ACCCOST                                                       
         B     K67                                                              
         SPACE 1                                                                
K66      LA    R4,KRTAB            CALCULATE SLIDING SCALE CHARGES (K)          
         L     R3,ACCCOPY                                                       
         BAS   RE,SLIDEVAL                                                      
         ST    R5,ACCCOST                                                       
         LA    R4,KPTAB                                                         
         L     R3,ACCPAGE                                                       
         LA    RE,K66D                                                          
         SPACE 1                                                                
SLIDEVAL SR    R5,R5               CALCULATE COST OR ADDITIONAL PAGE            
         AH    R3,=H'1'            FACTOR VIA TABLE AT R4 WHICH GIVES           
K66A     OC    0(4,R4),0(R4)       NUMBER (N+) AND RATE IN DESCENDING           
         BZR   RE                  NUMBER SEQUENCE EG                           
         C     R3,0(R4)                FOR 5+           $0.25 EACH              
         BNP   K66B                    FOR 2+ (IE 2-4)  $0.50 EACH              
         S     R3,0(R4)                FOR 1+ (IE 1ST)  $1.25                   
         M     R2,4(R4)                                                         
         AR    R5,R3                                                            
         L     R3,0(R4)                                                         
K66B     LA    R4,8(R4)                                                         
         B     K66A                                                             
         SPACE 1                                                                
K66D     M     R4,ACCCOST          MULTIPLY BASIC COST BY FACTOR FOR            
         SLDA  R4,1                ADDITIONAL PAGES                             
         D     R4,=F'100'                                                       
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         A     R5,ACCCOST                                                       
         ST    R5,ACCCOST                                                       
         SPACE 1                                                                
K67      GOTO1 =V(ROLLER),DMCB,6,ACCUMS                                         
         SPACE 1                                                                
K68      CLI   DETAILS,C'N'        PRINT A LINE FOR A KWX                       
         BE    K45                                                              
         GOTO1 =V(DATCON),DMCB,(0,SDATE),(8,P+37)                               
         MVC   P+46(2),STIME                                                    
         MVI   P+48,C'.'                                                        
         MVC   P+49(2),STIME+2                                                  
         MVC   P+52(L'SREF),SREF                                                
         CLI   FORMAT,C'K'                                                      
         BNE   *+10                                                             
         MVC   P(36),SSENDTO                                                    
         LA    R2,1                                                             
         BAS   RE,VALPRINT                                                      
         B     K45                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT A TOTALS LINE AND CLEAR ITS ACCUMS              
*              ON ENTRY R2 = LEVEL 2 TO 5                                       
         SPACE 1                                                                
TOTALS   NTR1                                                                   
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)                                                      
         LR    R1,R2                                                            
         SH    R1,=H'2'                                                         
         MH    R1,=H'20'                                                        
         LA    R1,TOTEXT(R1)                                                    
         MVC   P+31(20),0(R1)                                                   
         MVC   P+58(5),=C'KWX''S'                                               
         BAS   RE,VALPRINT                                                      
         GOTO1 =V(ROLLER),DMCB,2,ACCUMS,(R2)                                    
         B     EXIT                                                             
         SPACE 1                                                                
TOTEXT   DC    CL20'DESTINATION TOTALS'                                         
         DC    CL20'AUTHORIZATION TOTALS'                                       
         DC    CL20'SENDER TOTALS'                                              
         DC    CL20'GRAND TOTALS'                                               
         SPACE 3                                                                
*              ROUTINE TO FORMAT VALUES AND PRINT A LINE                        
*              ON ENTRY R2 = LEVEL 1 TO 5                                       
         SPACE 1                                                                
VALPRINT NTR1                                                                   
         GOTO1 =V(ROLLER),DMCB,1,ACCUMS,(R2)                                    
         L     R7,DMCB                                                          
         CH    R2,=H'1'                                                         
         BE    VP2                                                              
         EDIT  ACCKWX,(5,P+52)                                                  
         CLC   ACCKWX+2(2),=H'1'                                                
         BNE   VP2                                                              
         MVC   P+61(2),SPACES                                                   
VP2      CLI   FORMAT,C'K'                                                      
         BNE   VP3                                                              
         EDIT  ACCCOPY,(5,P+65)                                                 
VP3      EDIT  ACCPAGE,(5,P+71)                                                 
         EDIT  ACCLINE,(6,P+77)                                                 
         EDIT  ACCWORD,(7,P+84)                                                 
         EDIT  ACCCHAR,(8,P+92)                                                 
         EDIT  ACCCOST,(9,P+101),2                                              
         GOTO1 =V(PRINTER)                                                      
EXIT     XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO GET AN ORIGIN OR DESTINATION ID NAME                  
*              ON ENTRY IDNO = ID NUMBER                                        
*                       TYPE = 'D' OR 'O'                                       
*              ON EXIT  NAME IS IN SORIGIN OR SDESTID                           
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         L     R6,=A(NAMETAB)      IS IT ALREADY IN THE TABLE                   
         USING NAMED,R6                                                         
GN01     CLI   0(R6),X'FF'                                                      
         BE    GN02                                                             
         CLC   IDNO,NAMIDNO                                                     
         BE    GN0X                                                             
         LA    R6,NAMLEN(R6)                                                    
         B     GN01                                                             
         SPACE 1                                                                
GN02     LA    R5,IOB              IF NOT READ FOR IT                           
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),IDNO                                                 
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD',=C'CTFILE',IOB,IOB                   
         MVC   WORK+20(33),SPACES                                               
         EDIT  IDNO,(5,WORK+20),ALIGN=LEFT                                      
         LA    R3,WORK+20          IF NOT FOUND PASS BACK EDITED NUMBER         
         CLI   DMCB+8,0                                                         
         BNE   GN04                                                             
         LA    R4,CTIDATA                                                       
         SPACE 1                                                                
GN03     CLI   0(R4),0                                                          
         BE    GN04                                                             
         CLI   0(R4),X'02'         ID ALPHA ELEMENT                             
         BE    GN03B                                                            
         CLI   0(R4),X'30'         DESTINATION DETAIL ELEMENT                   
         BE    GN03C                                                            
         SPACE 1                                                                
GN03A    ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     GN03                                                             
         SPACE 1                                                                
GN03B    MVC   0(8,R3),2(R4)                                                    
         B     GN03A                                                            
         SPACE 1                                                                
         USING CTDSTD,R4                                                        
GN03C    MVI   9(R3),C'-'                                                       
         MVC   11(L'CTDSTLG1,R3),CTDSTLG1                                       
         LA    R1,11+L'CTDSTLG1-1(R3)                                           
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'CTDSTLG2,R1),CTDSTLG2                                        
         B     GN03A                                                            
         DROP  R4                                                               
         SPACE 1                                                                
GN04     MVC   NAMIDNO,IDNO        ADD TABLE ENTRY                              
         MVC   NAMTYPE,TYPE                                                     
         MVC   NAMNAME,0(R3)                                                    
         MVI   NAMLEN(R6),X'FF'                                                 
         SPACE 1                                                                
GN0X     LA    R1,SORIGIN          PASS NAME BACK                               
         CLI   TYPE,C'O'                                                        
         BE    *+8                                                              
         LA    R1,SDESTID                                                       
         MVC   0(L'SORIGIN,R1),NAMNAME                                          
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
KWXTAPE  DS    0F                  DTFMT GOES HERE IF MT READING RQRED          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              CONSTANTS AND VARIABLES                                          
         SPACE 1                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    7F                                                               
WORK     DS    CL80                                                             
CARD     DS    CL80                CARD I/O AREA                                
HALF     DS    H                                                                
START    DC    XL6'00'             OPTIONAL START AND END                       
END      DC    CL6'999999'                                                      
MT       DC    C'N'                TAPE OR DISC INPUT                           
DETAILS  DC    C'Y'                DETAILS OR TOTALS ONLY                       
FORMAT   DC    C'K'      C         K=BY KWX, D=BY DESTID                        
IDNO     DS    CL2                 PASSED TO GETNAME                            
TYPE     DS    C                   DITTO                                        
BYTE     DS    C                                                                
         DS    0F                                                               
RATES    DS    0CL24               RATES TO 5 DECIMAL PLACES PER -              
DRATE    DC    F'0'                KWX                                          
         DC    F'0'                DUMMY FOR COPIES                             
PRATE    DC    F'0'                PAGE                                         
LRATE    DC    F'0'                LINE                                         
WRATE    DC    F'0'                WORD                                         
CRATE    DC    F'0'                CHARACTER                                    
SPARATE  DC    F'0'                SPARE                                        
*                                                                               
KRTAB    DC    8XL8'00'            TABLE FOR SLIDING SCALE KWX RATES            
KPTAB    DC    8XL8'00'            TABLE FOR SLIDING SCALE PAGE FACTORS         
DMDA     DC    F'0'                DISC ADDRESS FOR DATAMGR                     
LASTKEY  DS    CL76                LAST ORIGIN NAME/AUTH/DEST NAME              
SORTREC  DS    CL180               SORT RECORD AREA COVERED BY SORTD QV         
MIDRATES DC    240C' '             UP TO 3 RATE CARDS FOR MIDLINES              
         DS    0F                                                               
ACCUMS   DS    200C                ACCUMULATORS COVERED BY ACCUMD QV            
IOA      DS    500C                KWXFILE I/O AREA                             
IOB      DS    1000C               CTFILE I/O AREA                              
NAMETAB  DC    X'FF'               ORIGIN/DESTINATION NAME TABLE                
         DS    15000C              COVERED BY NAMED QV                          
         EJECT                                                                  
SORTAREA CSECT                                                                  
         DS    41000C                                                           
         SPACE 1                                                                
TRACKBUF CSECT                                                                  
         DS    21000C                                                           
         EJECT                                                                  
*              DSECT TO COVER A SORT RECORD                                     
         SPACE 1                                                                
SORTD    DSECT                                                                  
SLEN     DS    CL2       B         RECORD LENGTH                                
SFILL    DS    CL2       B         FILLER                                       
SKEY     DS    0CL99               KEY                                          
SORIGIN  DS    CL33      C         SENDER NAME                                  
SAUTH    DS    CL10      C         PASSWORD                                     
SDESTID  DS    CL33      C         RECEIVER NAME                                
SDATE    DS    CL6       C         DATE                                         
STIME    DS    CL6       C         TIME                                         
SREF     DS    CL11      C         REFERENCE                                    
SSENDTO  DS    CL45      C         'SEND TO' STRING                             
SACCUMS  DS    0CL20                                                            
SCOPIES  DS    CL4       B         NUMBER OF COPIES                             
SPAGES   DS    CL4       B         DITTO PAGES                                  
SLINES   DS    CL4       B         DITTO LINES                                  
SWORDS   DS    CL4       B         DITTO WORDS                                  
SCHARS   DS    CL4       B         DITTO CHARACTERS                             
SLENEQ   EQU   *-SORTD                                                          
         SPACE 3                                                                
*              DSECT TO COVER A NAME TABLE ENTRY                                
         SPACE 1                                                                
NAMED    DSECT                                                                  
NAMIDNO  DS    CL2       B         ID NUMBER                                    
NAMTYPE  DS    CL1       C         O=ORIGIN, D=DESTINATION                      
NAMNAME  DS    CL33      C         NAME                                         
NAMLEN   EQU   *-NAMED                                                          
         SPACE 3                                                                
*              DSECT TO COVER AN ACCUMULATOR LINE                               
         SPACE 1                                                                
ACCUMD   DSECT                                                                  
ACCKWX   DS    F         B         TOTAL KWX'S                                  
ACCCOPY  DS    F         B         TOTAL COPIES                                 
ACCPAGE  DS    F         B         TOTAL PAGES                                  
ACCLINE  DS    F         B         TOTAL LINES                                  
ACCWORD  DS    F         B         TOTAL WORDS                                  
ACCCHAR  DS    F         B         TOTAL CHARS                                  
ACCCOST  DS    F         B         TOTAL COST                                   
ACCTOT   DS    F         B         CROSS-CAST TOTAL (USELESS)                   
         SPACE 3                                                                
* NESTED INCLUDES                                                               
* CTGENFILE                                                                     
* DDLOGOD                                                                       
* DDDPRINT                                                                      
* SRKWXACD                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE SRKWXACD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SRKWXREP  05/01/02'                                      
         END                                                                    
