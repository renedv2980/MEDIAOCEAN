*          DATA SET DMISTEST   AT LEVEL 104 AS OF 05/01/02                      
*PHASE DMISNEW                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMISDDZ   <====                                                        
*INCLUDE DMDADDS                                                                
*INCLUDE DMDYNDD                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'DMISTEST - TEST NEW INDEX SEQUENTIAL'                           
DMISTEST CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         ENTRY ADWAIT                                                           
         PRINT NOGEN                                                            
         NBASE 0,DMISTEST,VREGSAVE                                              
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING DMISTEST+4096,RC                                                 
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         L     R2,=A(SPTDIR)                                                    
         USING ISDTF,R2                                                         
         XC    KEY,KEY                                                          
*                                                                               
C2       DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   =C'/*',C                                                         
         BE    C10                                                              
         MVC   P(80),C                                                          
         GOTO1 =V(PRINTER)                                                      
         CLI   C,C'*'              TEST COMMENT                                 
         BE    C2                                                               
*                                                                               
         CLC   =C'COUNT=',C                                                     
         BNE   C4                                                               
         PACK  COUNT,C+6(6)                                                     
         B     C2                                                               
*                                                                               
C4       MVI   DAFLAG,C'Y'                                                      
         CLC   =C'DA=YES',C                                                     
         BE    C2                                                               
         MVI   DAFLAG,C'N'                                                      
         CLC   =C'DA=NO',C                                                      
         BE    C2                                                               
*                                                                               
         CLC   =C'KEY=',C                                                       
         BNE   C6                                                               
         LA    R1,C+79                                                          
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,C+4                                                           
         SR    R1,R0                                                            
         LA    R0,1(R1)                                                         
         GOTO1 =V(HEXIN),DMCB,C+4,KEY,(R0)                                      
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     C2                                                               
*                                                                               
C6       DC    H'0'                                                             
*                                                                               
C10      DS    0H                                                               
         ZAP   SVCOUNT,COUNT                                                    
         GOTO1 =V(ISDDS),P1,8,,,(R2)      OPEN THE FILE                         
         CLI   DAFLAG,C'Y'                                                      
         BNE   C12                                                              
         GOTO1 =V(DADDS),P1,14,,,(R8)                                           
*                                                                               
C12      DS    0H                                                               
         B     DMIS102                                                          
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
DMIS102  DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 =V(ISDDS),P1,1,KEY,,(R2),KEY   RDHI                              
         GOTO1 =V(HEXOUT),DMCB,KEY,P,18,=C'TOG'                                 
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
*                                                                               
DMIS104  DS    0H                                                               
         SP    COUNT,=P'1'                                                      
         BNP   DMIS108                                                          
DMIS104A DS    0H                                                               
         GOTO1 =V(ISDDS),P1,2,KEY,,(R2),KEY  RDSEQ                              
         GOTO1 (RF),(R1)           READ EVERY OTHER RECORD                      
*                                                                               
DMIS106  CLC   KEY+43(7),=7X'FF'                                                
         BE    DMIS104A            SKIP CONTRA HEADERS                          
         CLI   KEY,4               SKIP BATCH PASSIVES                          
         BE    DMIS104A                                                         
         TM    KEY+42,X'04'        TEST ACC ARC                                 
         BO    DMIS104A                                                         
         CLI   KEY+53,0                                                         
         BE    DMIS104A            SKIP OVERFLOW RECORDS                        
*                                                                               
         CLI   DAFLAG,C'Y'                                                      
         BNE   DMIS104                                                          
         GOTO1 =V(DADDS),P1,1,IOA,,(R8),KEY+50                                  
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(42),IOA                                                      
         BE    *+6                                                              
         DC    H'0'                KEYS SHOULD MATCH                            
         BC    0,DMIS104                                                        
         MVI   *-3,X'F0'                                                        
         MVC   P(80),IOA+1                                                      
         GOTO1 =V(PRINTER)                                                      
         B     DMIS104                                                          
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'IOB/CCWS',IOBCCWS,C'DUMP',256,=C'1D'          
         GOTO1 =V(PRNTBL),DMCB,=C'READ01',IOA,C'DUMP',80,=C'1D'                 
         GOTO1 =V(PRNTBL),DMCB,=C'BLK ADDR',ISPDDA,,8                           
         SP    COUNT,=P'1'                                                      
         BP    DMIS104                                                          
         B     EXIT                                                             
SAVEDA   DS    XL8                                                              
*                                                                               
DMIS108  DS    0H                                                               
         OI    SVCOUNT+3,X'0F'                                                  
         UNPK  P(7),SVCOUNT                                                     
         MVC   P+10(10),=C'ITERATIONS'                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         OI    IOCOUNT+3,X'0F'                                                  
         UNPK  P(7),IOCOUNT                                                     
         MVC   P+10(8),=C'IO COUNT'                                             
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
*                                                                               
         MVC   C,SVC                                                            
         CLI   C,C'A'                                                           
         BE    ADD                                                              
         B     ERASE                                                            
         SPACE 2                                                                
*                                                                               
DMIS2    DS    0H                                                               
         CVB   R0,DUB                                                           
         BAS   R9,CALC                                                          
*                                                                               
         MVC   P(5),C                                                           
         CVD   R0,DUB              R0 HAS RECS/TRACK ON OUTPUT                  
         OI    DUB+7,X'0F'                                                      
         UNPK  P+7(5),DUB+5(3)                                                  
         GOTO1 =V(PRINTER)                                                      
         B     DMIS2                                                            
*                                                                               
         CLC   =C'MVSDUMP',C                                                    
         BNE   DMIS10                                                           
         MVI   MVSDUMP,C'Y'                                                     
         B     DMIS2                                                            
*                                                                               
CALC     LR    RE,R0               GET RECLEN                                   
         LA    RE,237(RE)          RE=DL+6+231                                  
         SRDL  RE,32                                                            
         D     RE,=F'232'          RF=(DL+237)/232                              
         MH    RF,=H'6'            RF=6*DN                                      
         AR    RF,R0               RF=DL+6*DN                                   
         LA    RF,39(RF)                                                        
         SR    RE,RE                                                            
         D     RE,=F'34'                                                        
         LA    RF,19(RF)           GIVES SPACE REQUIRED IN RF                   
         SR    R0,R0                                                            
         LA    R1,1729                                                          
         DR    R0,RF                                                            
         LR    R0,R1               GIVES MAX RECS/TRACK                         
         BR    R9                                                               
*                                                                               
DMIS10   CLC   =C'ADD=',C                                                       
         BNE   DMIS12                                                           
         MVC   SVC,C                                                            
         B     DMIS2                                                            
*                                                                               
DMIS12   CLC   =C'ERASE=',C                                                     
         BNE   DMISERR                                                          
         MVC   SVC,C                                                            
         B     DMIS2                                                            
*                                                                               
DMISERR  DS    0H                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'BADCARD',C,C'DUMP',80,=C'1D'                  
         B     EXIT                                                             
         EJECT                                                                  
DMIS100  CLI   MVSDUMP,C'Y'                                                     
         BE    DMIS102                                                          
         XC    WORK,WORK                                                        
         ST    RB,WORK                                                          
         L     RE,=V(DUMMY)        DUMMY FOLLOWS REGSAVE                        
         ST    RE,WORK+4                                                        
         OI    WORK+4,X'80'                                                     
         GOTO1 =V(STXITER),P1,WORK                                              
*                                                                               
EXIT     XBASE                                                                  
         EJECT                                                                  
ERASE    DS    0H                                                               
         PACK  COUNT,C+6(4)                                                     
ERASE2   BAS   RE,SETKEY                                                        
         MVC   KEY,IOA                                                          
         GOTO1 =V(ISDDS),P1,1,IOA,0,A(CTFILE),KEY                               
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(25),IOA                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRNTBL),DMCB,=C'READ KEY',KEY,1,25,=C'1C'                     
         BAS   RE,PRTBUFF                                                       
         GOTO1 =V(ISDDS),P1,4,IOA,0,A(CTFILE),KEY                               
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRNTBL),DMCB,=C'ERASE KEY',KEY,1,25,=C'1C'                    
         BAS   RE,PRTBUFF                                                       
         SP    COUNT,=P'1'                                                      
         BP    ERASE2                                                           
         B     CLOSE                                                            
*                                                                               
ADD      DS    0H                                                               
         PACK  MAXCOUNT,C+4(4)                                                  
         ZAP   COUNT,=P'0'                                                      
         LA    R0,546                                                           
         STCM  R0,3,IOA+25                                                      
         MVI   IOA+27,0                                                         
*                                                                               
         LA    RE,IOA+28                                                        
         XC    0(255,RE),0(RE)                                                  
         MVC   0(2,RE),=X'0108'                                                 
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         XC    0(255,RE),0(RE)                                                  
         MVC   0(2,RE),=X'02FF'                                                 
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         XC    0(255,RE),0(RE)                                                  
         MVC   0(2,RE),=X'03FF'                                                 
ADD2     DS    0H                                                               
         AP    COUNT,=P'1'                                                      
         CP    COUNT,MAXCOUNT                                                   
         BH    CLOSE                                                            
         BAS   RE,SETKEY                                                        
         GOTO1 =V(ISDDS),P1,3,IOA,546,A(CTFILE)                                 
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     ADD2                                                             
         SPACE 2                                                                
CLOSE    GOTO1 =V(ISDDS),P1,9,,,A(CTFILE)                                       
         B     EXIT                                                             
         EJECT                                                                  
SETKEY   DS    0H                                                               
         MVC   IOA(1),KEYST        SET FIRST CHAR OF KEY                        
         UNPK  DUB,COUNT                                                        
         OI    DUB+7,X'F0'                                                      
         LA    R0,12                                                            
         LA    R1,IOA+1                                                         
         MVC   0(2,R1),DUB+6                                                    
         LA    R1,2(R1)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
PRTBUFF  NTR1                                                                   
         LA    R6,SPTDIR                                                        
         USING ISDTF,R6                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'DTF',(R6),C'DUMP',172,=C'1D'                  
*                                                                               
         L     R7,ISBUFF1                                                       
         LH    R8,ISPDLN                                                        
         GOTO1 =V(PRNTBL),DMCB,=C'BUFF1',(R7),C'DUMP',(R8),=C'1D'               
         XIT1                                                                   
         EJECT                                                                  
         DS    0D                                                               
         USING *,RF                                                             
ADWAIT   DS    0H                                                               
         LTR   RE,RE               RE IS NEG IF 31 BIT CALLER                   
         BP    ADWAITX                                                          
         AP    IOCOUNT,PCKD1                                                    
         MVC   IOBCCWS,0(RC)                                                    
ADWAITX  BR    RE                                                               
         DROP  RF                                                               
IOCOUNT  DC    PL4'0'                                                           
PCKD1    DC    PL1'1'                                                           
         DS    0D                                                               
IOBCCWS  DS    XL256                                                            
         EJECT                                                                  
DUB      DC    D'0'                                                             
COUNT    DC    PL4'5000'                                                        
SVCOUNT  DC    PL4'5000'                                                        
MAXCOUNT DS    PL4                                                              
DMCB     DS    6F                                                               
P1       DC    F'0'                                                             
P2       DC    F'0'                                                             
P3       DC    F'0'                                                             
P4       DC    F'0'                                                             
P5       DC    F'0'                                                             
P6       DC    F'0'                                                             
DSKADR   DC    F'0'                                                             
WORK     DC    20F'0'                                                           
C        DS    CL80                                                             
SVC      DS    CL80                                                             
MVSDUMP  DC    C'N'                                                             
DAFLAG   DC    C'Y'                                                             
KEYST    DC    X'00'                                                            
         DS    0D                                                               
         DC    CL8'**KEY**'                                                     
KEY      DC    XL64'00'                                                         
         ORG   KEY                                                              
         DC    X'B3'                                                            
         ORG                                                                    
KEYSAVE  DC    XL64'00'                                                         
         DS    0D                                                               
         DC    CL8'**IOA**'                                                     
IOA      DS    2000C                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         PRINT GEN                                                              
STAFILE  DMIS  KEYLEN=17,RECSIZE=117,BLKSIZE=6356,                     X        
               IOAREA1=0,INDAREA=0,INDSIZE=0                                    
         ORG                                                                    
         PRINT GEN                                                              
SPTDIR   DMIS  KEYLEN=13,RECSIZE=18,BLKSIZE=1954,                      X        
               IOAREA1=0,INDAREA=0,INDSIZE=0,IXPDOV=Y                           
SPTDIR2  DMIS  KEYLEN=13,RECSIZE=18,BLKSIZE=1954,                      X        
               IOAREA1=0,INDAREA=0,INDSIZE=0,IXPDOV=N                           
         SPACE 2                                                                
CTFILE   DMIS  KEYLEN=25,BLKSIZE=4820,SPARE=200,RECSIZE=V,             X        
               IOAREA1=0,INDAREA=0,INDSIZE=0                                    
         PRINT NOGEN                                                            
*                                                                               
ACCDIRB  DMIS  KEYLEN=42,RECSIZE=54,BLKSIZE=9076,DSKXTNT=15,           X        
               INDSIZE=0,SPARE=750,XBUFFS=6,IOAREA1=0                           
         SPACE 1                                                                
ACCMSTB  DMDA  BLKSIZE=10796,DSKXTNT=16,KEYCMP=LR,KEYLEN=42,XBUFFS=6,  X        
               IOAREA1=0                                                        
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 2                                                                
SSB      DS    0D                                                               
         DC    X'0000',X'FF',X'88' WRITE TO FACWRK/COPIES REQUIRED              
         DC    X'00000000'                                                      
         DC    CL8' '                                                           
         DC    A(0),A(0)                                                        
         DC    XL32'00'                                                         
         SPACE 2                                                                
UTL      DS    0D                                                               
         DC    10F'0'                                                           
         SPACE 2                                                                
       ++INCLUDE DMDTFIS                                                        
         EJECT                                                                  
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'104DMISTEST  05/01/02'                                      
         END                                                                    
