*          DATA SET DMISTESTJ  AT LEVEL 206 AS OF 05/01/02                      
*PHASE DMISTESJ                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMISDDS                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE CARDS                                                                  
         TITLE 'DMISTEST - TEST INDEX SEQUENTIAL'                               
DMISTEST CSECT                                                                  
         DS    4000C                                                            
         ORG   *-4000                                                           
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         ENTRY ADWAIT                                                           
         ENTRY TRACEBUF                                                         
         PRINT NOGEN                                                            
         NBASE 0,DMISTEST,VREGSAVE                                              
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING DMISTEST+4096,RC                                                 
*                                                                               
         XC    WORK,WORK                                                        
         ST    RB,WORK                                                          
         L     RE,=V(DUMMY)        DUMMY FOLLOWS REGSAVE                        
         ST    RE,WORK+4                                                        
         OI    WORK+4,X'80'                                                     
* ??     GOTO1 =V(STXITER),P1,WORK                                              
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         L     R2,=A(PERDIR)                                                    
         USING ISDTF,R2                                                         
*                                                                               
         GOTO1 =V(ISDDS),P1,8,,,(R2)      OPEN THE FILE                         
         XC    KEY,KEY                                                          
         MVI   KEY,C'R'                                                         
         ZAP   IOCOUNT,=P'0'                                                    
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         SR    R2,R2                                                            
         USING ISDTF,R2                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,C'R'                                                         
         ZAP   IOCOUNT,=P'0'                                                    
         MVC   KEYSAVE,KEY                                                      
         B     C2                  READ INPUT CONTROL CARDS                     
*                                                                               
         LA    R0,1                SET ACTION = READ HIGH                       
         BAS   RE,DOIT                                                          
         BAS   RE,PRTBUFF                                                       
         B     CLOSE                                                            
*                                                                               
OPENPER  L     R2,=A(PERDIR)                                                    
         ST    R2,ADIR                                                          
         GOTO1 =V(ISDDS),P1,8,,,(R2)      OPEN THE FILE                         
         B     C2                                                               
*                                                                               
OPENMED  L     R2,=A(MEDDIR)                                                    
         ST    R2,ADIR                                                          
         GOTO1 =V(ISDDS),P1,8,,,(R2)      OPEN THE FILE                         
         B     C2                                                               
*                                                                               
OPENGEN  L     R2,=A(GENDIR)                                                    
         ST    R2,ADIR                                                          
         GOTO1 =V(ISDDS),P1,8,,,(R2)      OPEN THE FILE                         
         B     C2                                                               
*                                                                               
OPENACC  L     R2,=A(ACCDIR)                                                    
         ST    R2,ADIR                                                          
         GOTO1 =V(ISDDS),P1,8,,,(R2)      OPEN THE FILE                         
         B     C2                                                               
*                                                                               
OPENSPOT L     R2,=A(SPTDIR)                                                    
         ST    R2,ADIR                                                          
         GOTO1 =V(ISDDS),P1,8,,,(R2)      OPEN THE FILE                         
         B     C2                                                               
*                                                                               
DOIT     NTR1                                                                   
         GOTO1 =V(ISDDS),P1,(R0),KEY,,(R2),KEY                                  
         MVC   P(4),=C'HIGH'                                                    
         CH    R0,=H'1'                                                         
         BE    DOIT2                                                            
         MVC   P(5),=C'FLUSH'                                                   
         CH    R0,=H'11'                                                        
         BE    DOIT2                                                            
         DC    H'0'                                                             
DOIT2    DS    0H                                                               
         GOTO1 =V(HEXOUT),DMCB,KEY,P+14,40                                      
         OI    IOCOUNT+3,X'0F'                                                  
         UNPK  P+6(6),IOCOUNT                                                   
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*                                                                               
         LA    R0,11               SET ACTION = READ/FLUSH                      
         GOTO1 =V(ISDDS),P1,(R0),KEY,,(R2),KEY   RDHI                           
         GOTO1 =V(HEXOUT),DMCB,KEY,P+10,40                                      
         OI    IOCOUNT+3,X'0F'                                                  
         UNPK  P+2(6),IOCOUNT                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R0,11               SET ACTION = READ/FLUSH                      
         GOTO1 =V(ISDDS),P1,(R0),KEY,,(R2),KEY   RDHI                           
         GOTO1 =V(HEXOUT),DMCB,KEY,P+10,40                                      
         OI    IOCOUNT+3,X'0F'                                                  
         UNPK  P+2(6),IOCOUNT                                                   
         GOTO1 =V(PRINTER)                                                      
         B     CLOSE                                                            
*                                                                               
C2       DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   =C'/*',C                                                         
         BE    CLOSE                                                            
         MVC   P(80),C                                                          
         GOTO1 =V(PRINTER)                                                      
         CLI   C,C'*'              TEST COMMENT                                 
         BE    C2                                                               
*                                                                               
         CLC   =C'ADD=',C                                                       
         BNE   C4                                                               
         XC    KEY,KEY                                                          
         LA    R1,C+79                                                          
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,C+4                                                           
         SR    R1,R0                                                            
         LA    R0,1(R1)            GIVES NUMBER OF CHARS PRESENT                
         GOTO1 =V(HEXIN),DMCB,C+4,KEY,(R0)                                      
         LA    R3,KEY                                                           
         USING GRECD,R3                                                         
         MVC   GDDA,=XL4'00010101'                                              
         DROP  R3                                                               
         B     ADD                                                              
*                                                                               
C4       CLC   =C'KEY=',C          READ A KEY                                   
         BNE   C5                                                               
         XC    KEY,KEY                                                          
         LA    R1,C+79                                                          
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,C+4                                                           
         SR    R1,R0                                                            
         LA    R0,1(R1)            GIVES NUMBER OF CHARS PRESENT                
         GOTO1 =V(HEXIN),DMCB,C+4,KEY,(R0)                                      
         B     HI                                                               
*                                                                               
C5       CLC   =C'WRITE=',C                                                     
         BNE   C6                                                               
         XC    WRITEVAL,WRITEVAL                                                
         LA    R1,C+79                                                          
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,C+4                                                           
         SR    R1,R0                                                            
         LA    R0,1(R1)            GIVES NUMBER OF CHARS PRESENT                
         GOTO1 =V(HEXIN),DMCB,C+6,WRITEVAL,(R0)                                 
         B     WRITE                                                            
*                                                                               
C6       DS    0H                                                               
         CLC   =C'SEQ',C                                                        
         BE    SEQ                                                              
*                                                                               
C7       DS    0H                                                               
         CLC   =C'BLK',C                                                        
         BE    BLK                                                              
*                                                                               
C8       DS    0H                                                               
         CLC   =C'OPEN=',C                                                      
         BNE   C9                                                               
         CLC   C+5(5),=CL5'MEDIA'                                               
         BE    OPENMED                                                          
         CLC   C+5(6),=CL6'PERSON'                                              
         BE    OPENPER                                                          
         CLC   C+5(3),=CL3'GEN'                                                 
         BE    OPENGEN                                                          
         CLC   C+5(7),=CL7'ACCOUNT'                                             
         BE    OPENACC                                                          
         CLC   C+5(4),=CL4'SPOT'                                                
         BE    OPENSPOT                                                         
         B     C20                                                              
*                                                                               
C9       DS    0H                                                               
         CLC   =C'BIX',C                                                        
         BNE   C10                                                              
         OI    ISFFLAG,ISFBLKIX                                                 
         OI    ISFFLAG,X'10'                                                    
*                                                                               
         B     C2                                                               
*                                                                               
C10      CLC   =C'COUNT=',C                                                     
         BNE   C11                                                              
         LA    R0,4                                                             
         LA    R1,C+6                                                           
         CLI   0(R1),C'E'                                                       
         BNE   CHKNUM                                                           
         MVI   COUNTEOF,C'Y'                                                    
         B     C2                                                               
CHKNUM   CLI   0(R1),C'0'                                                       
         BL    C20                                                              
         CLI   0(R1),C'9'                                                       
         BH    C20                                                              
         LA    R1,1(R1)                                                         
         BCT   R0,CHKNUM                                                        
         PACK  COUNT,C+6(4)                                                     
         B     C2                                                               
*                                                                               
C11      CLC   =C'ERS',C                                                        
         BE    ERS                                                              
*                                                                               
C20      DS    0H                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'BADCARD',C,C'DUMP',80,=C'1D'                  
         B     C2                                                               
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
* READ HIGH FOR A RECORD                                                        
*                                                                               
HI       DS    0H                                                               
         LA    R0,1                SET ACTION NUMBER                            
         B     GOIS                                                             
SEQ      DS    0H                                                               
         LA    R0,2                SET ACTION NUMBER                            
*                                                                               
GOIS     DS    0H                                                               
         ZAP   IOCOUNT,=P'0'                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 =V(ISDDS),P1,(R0),KEY,,(R2),KEY                                  
         OC    P3(2),P3                                                         
         BNZ   EOF                                                              
         MVC   P(23),=C'ISPDDA=         ISOVDA='                                
         GOTO1 =V(HEXOUT),DMCB,ISPDDA,P+7,4                                     
         GOTO1 =V(HEXOUT),DMCB,ISOVDA,P+23,4                                    
         OI    IOCOUNT+3,X'0F'                                                  
         UNPK  P+32(6),IOCOUNT                                                  
* ??     MVC   P+40(40),KEY                                                     
         GOTO1 =V(HEXOUT),DMCB,KEY,P+40,40                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   COUNTEOF,C'Y'                                                    
         BE    GOIS                                                             
         CP    COUNT,=P'0'                                                      
         BE    C2                                                               
         SP    COUNT,=P'1'                                                      
         BP    GOIS                                                             
         B     C2                                                               
*                                                                               
BLK      DS    0H                                                               
         ZAP   IOCOUNT,=P'0'                                                    
         MVC   KEYSAVE,KEY                                                      
         LA    R0,2                SET ACTION NUMBER                            
         GOTO1 =V(ISDDS),P1,(R0),KEY,,(R2),KEY                                  
         OC    P3(2),P3                                                         
         BNZ   EOF                                                              
         CLC   OVDALAST,ISOVDA                                                  
         BE    BLK030                                                           
         OC    ISOVDA,ISOVDA                                                    
         BZ    BLK020                                                           
         CLC   PDDALAST,ISPDDA                                                  
         BE    BLK010                                                           
         MVC   P(40),=CL40'*** NEW BLOCK OVERFLOW CHAIN ***'                    
         GOTO1 =V(PRINTER)                                                      
         MVC   PDDALAST,ISPDDA                                                  
BLK010   MVC   P(23),=C'ISPDDA=         ISOVDA='                                
         GOTO1 =V(HEXOUT),DMCB,ISPDDA,P+7,4                                     
         GOTO1 =V(HEXOUT),DMCB,ISOVDA,P+23,4                                    
         OI    IOCOUNT+3,X'0F'                                                  
         UNPK  P+32(6),IOCOUNT                                                  
* ??     MVC   P+40(40),KEY                                                     
         GOTO1 =V(HEXOUT),DMCB,KEY,P+40,40                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P+40(40),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
BLK020   MVC   OVDALAST,ISOVDA                                                  
*                                                                               
BLK030   CLI   COUNTEOF,C'Y'                                                    
         BE    BLK                                                              
         CP    COUNT,=P'0'                                                      
         BE    C2                                                               
         SP    COUNT,=P'1'                                                      
         BP    BLK                                                              
         B     C2                                                               
*                                                                               
EOF      DS    0H                                                               
         MVC   P(40),=CL40'END OF FILE OR ERROR P3 = '                          
         GOTO1 =V(HEXOUT),DMCB,P3,P+40,4                                        
         GOTO1 =V(PRINTER)                                                      
         B     CLOSE                                                            
*                                                                               
* ADD A NEW KEY TO FILE                                                         
*                                                                               
ADD      DS    0H                                                               
         ZAP   IOCOUNT,=P'0'                                                    
         MVC   P(3),=C'ADD'                                                     
*        GOTO1 =V(HEXOUT),DMCB,KEY,P+4,40,=C'TOG'                               
         MVC   P+4(40),KEY                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(ISDDS),P1,3,KEY,,(R2),0     ADD                               
*                                                                               
         MVC   P(23),=C'ISPDDA=         ISOVDA='                                
         GOTO1 =V(HEXOUT),DMCB,ISPDDA,P+7,4                                     
         GOTO1 =V(HEXOUT),DMCB,ISOVDA,P+23,4                                    
         OI    IOCOUNT+3,X'0F'                                                  
         UNPK  P+32(6),IOCOUNT                                                  
         MVC   P+40(40),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
         OC    8(2,R1),8(R1)       TEST FOR ERRORS                              
         BNZ   DMISERR                                                          
*                                                                               
         CLI   COUNTEOF,C'Y'                                                    
         BE    ADD010                                                           
         CP    COUNT,=P'0'         TEST MULTIPLE ADD                            
         BE    C2                                                               
         SP    COUNT,=P'1'                                                      
         BZ    C2                                                               
ADD010   PACK  DUB,KEY+2(7)                                                     
         SP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(7),DUB                                                     
         B     ADD                                                              
*                                                                               
* WRITE BYTE TO CURRENT KEY                                                     
*                                                                               
WRITE    DS    0H                                                               
         ZAP   IOCOUNT,=P'0'                                                    
         MVC   P(5),=C'WRITE'                                                   
         GOTO1 =V(HEXOUT),DMCB,KEY,P+10,40,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P(5),=C'NEW  '                                                   
         MVC   KEY+32(4),WRITEVAL                                               
         GOTO1 =V(HEXOUT),DMCB,KEY,P+10,40,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(ISDDS),P1,5,KEY,,(R2),0     WRITE                             
*                                                                               
         MVC   P(23),=C'ISPDDA=         ISOVDA='                                
         GOTO1 =V(HEXOUT),DMCB,ISPDDA,P+7,4                                     
         GOTO1 =V(HEXOUT),DMCB,ISOVDA,P+23,4                                    
         OI    IOCOUNT+3,X'0F'                                                  
         UNPK  P+32(6),IOCOUNT                                                  
         MVC   P+40(40),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
         OC    8(2,R1),8(R1)       TEST FOR ERRORS                              
         BNZ   DMISERR                                                          
         B     C2                                                               
*                                                                               
DMISERR  DS    0H                                                               
         DC    H'0'                                                             
         EJECT                                                                  
ERS      DS    0H                  ERASE                                        
*                                                                               
         MVC   P(23),=C'BEFORE ISDDS 1         '                                
         GOTO1 =V(PRINTER)                                                      
         ZAP   IOCOUNT,=P'0'                                                    
         GOTO1 =V(ISDDS),P1,1,KEY,,(R2),KEY   RDHI                              
         OC    8(2,R1),8(R1)       TEST FOR ERRORS                              
         BNZ   DMISERR                                                          
         MVC   P(23),=C'AFTER  ISDDS 1         '                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(23),=C'ISPDDA=         ISOVDA='                                
         GOTO1 =V(HEXOUT),DMCB,ISPDDA,P+7,4                                     
         GOTO1 =V(HEXOUT),DMCB,ISOVDA,P+23,4                                    
         OI    IOCOUNT+3,X'0F'                                                  
         UNPK  P+32(6),IOCOUNT                                                  
         MVC   P+40(40),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
* NOW ERASE                                                                     
         ZAP   IOCOUNT,=P'0'                                                    
         MVC   P(23),=C'BEFORE ISDDS 2         '                                
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(ISDDS),P1,4,KEY,,(R2),KEY   ERASE                             
         OC    8(2,R1),8(R1)       TEST FOR ERRORS                              
         BNZ   DMISERR                                                          
         MVC   P(23),=C'AFTER  ISDDS 2         '                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P(23),=C'ISPDDA=         ISOVDA='                                
         GOTO1 =V(HEXOUT),DMCB,ISPDDA,P+7,4                                     
         GOTO1 =V(HEXOUT),DMCB,ISOVDA,P+23,4                                    
         OI    IOCOUNT+3,X'0F'                                                  
         UNPK  P+32(6),IOCOUNT                                                  
         MVC   P+40(40),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   COUNTEOF,C'Y'                                                    
         BE    ERS                                                              
         CP    COUNT,=P'0'                                                      
         BE    C2                                                               
         SP    COUNT,=P'1'                                                      
         BZ    C2                                                               
         B     ERS                                                              
         EJECT                                                                  
*                                                                               
EXIT     XBASE                                                                  
         SPACE 2                                                                
CLOSE    DS    0H                                                               
*                                                                               
         L     R2,ADIR                                                          
*                                                                               
         MVC   P(40),=CL40'ISOVLAST BEFORE ='                                   
         GOTO1 =V(HEXOUT),DMCB,ISOVLAST,P+18,4                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(ISDDS),P1,7,,,(R2)      CHECK EOF                             
*                                                                               
         MVC   P(40),=CL40'ISOVLAST AFTER ='                                    
         GOTO1 =V(HEXOUT),DMCB,ISOVLAST,P+18,4                                  
         GOTO1 =V(PRINTER)                                                      
         ZAP   IOCOUNT,=P'0'                                                    
*                                                                               
         GOTO1 =V(ISDDS),P1,9,,,(R2)      CLOSE                                 
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
PRTBUFF  NTR1                                                                   
         L     R6,ADIR                                                          
         USING ISDTF,R6                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'DTF',(R6),C'DUMP',172,=C'1D'                  
*                                                                               
         L     R7,ISBUFF1                                                       
         LH    R8,ISPDLN                                                        
* ??     GOTO1 =V(PRNTBL),DMCB,=C'BUFF1',(R7),C'DUMP',(R8),=C'1D'               
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
COUNT    DC    PL4'0'                                                           
SVCOUNT  DC    PL4'0'                                                           
MAXCOUNT DS    PL4                                                              
DMCB     DS    6F                                                               
P1       DC    F'0'                                                             
P2       DC    F'0'                                                             
P3       DC    F'0'                                                             
P4       DC    F'0'                                                             
P5       DC    F'0'                                                             
P6       DC    F'0'                                                             
DSKADR   DC    F'0'                                                             
OVDALAST DC    F'0'                                                             
PDDALAST DC    F'0'                                                             
ADIR     DS    A                                                                
WORK     DC    20F'0'                                                           
C        DS    CL80                                                             
SVC      DS    CL80                                                             
MVSDUMP  DC    C'N'                                                             
DAFLAG   DC    C'Y'                                                             
COUNTEOF DC    X'00'                                                            
WRITEVAL DS    XL4                                                              
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
SPTDIR   DMIS  KEYLEN=13,RECSIZE=18,BLKSIZE=1954,                      X        
               IOAREA1=0,INDAREA=0,INDSIZE=0                                    
         SPACE 2                                                                
CTFILE   DMIS  KEYLEN=25,BLKSIZE=4820,SPARE=200,RECSIZE=V,             X        
               IOAREA1=0,INDAREA=0,INDSIZE=0                                    
         SPACE 2                                                                
TSTDIR0  DMIS  KEYLEN=20,RECSIZE=32,BLKSIZE=6233,DSKXTNT=15,           X        
               INDSIZE=0,SPARE=500,XBUFFS=0                                     
         SPACE 1                                                                
MEDDIR   DMIS  KEYLEN=20,RECSIZE=32,BLKSIZE=8906,SPARE=500,            X        
               INDSIZE=0,DSKXTNT=15,XBUFFS=8                                    
         SPACE 1                                                                
PERDIR   DMIS  KEYLEN=36,RECSIZE=42,BLKSIZE=6233,SPARE=200,            X        
               INDSIZE=0,DSKXTNT=15,IXPDOV=Y,GLOBAL=Y                           
         SPACE 1                                                                
GENDIR   DMIS  KEYLEN=32,RECSIZE=40,BLKSIZE=6233,DSKXTNT=15,           X        
               INDSIZE=0,SPARE=500                                              
         SPACE 1                                                                
ACCDIR   DMIS  KEYLEN=42,RECSIZE=54,BLKSIZE=8906,SPARE=500,            X        
               INDSIZE=0,DSKXTNT=15,XBUFFS=8                                    
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
       ++INCLUDE GEGENGEN                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
*                                                                               
         DS    0D                                                               
TRACEBUF CSECT                     CAMPAIGN SAVE DATA AREA                      
         DC    10000X'00'                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'206DMISTESTJ 05/01/02'                                      
         END                                                                    
