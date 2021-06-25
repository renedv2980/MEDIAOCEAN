*          DATA SET RDTESTA    AT LEVEL 170 AS OF 09/10/04                      
*PHASE RDTESTA                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE IJDFYZZZ                                                               
*INCLUDE EDITOR                                                                 
*INCLUDE CUREDIT                                                                
*INCLUDE TIMCON                                                                 
*INCLUDE TIMEOUT                                                                
*INCLUDE PERVERT                                                                
*INCLUDE PERVAL                                                                 
*INCLUDE SQUASHER                                                               
*INCLUDE SCANNER                                                                
*INCLUDE TINVCON                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE MOBILE                                                                 
*INCLUDE CASHVAL                                                                
*                                                                               
* LEVEL 164 TEST 7121                                                           
* LEVEL 165 TEST 7122                                                           
* LEVEL 166 TEST 7123                                                           
* LEVEL 167 TEST 7124                                                           
* LEVEL 168 TEST 7125                                                           
* LEVEL 169 TEST 7176                                                           
* LEVEL 170 TEST 7178                                                           
RDTEST   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RON**,RR=R2                                                  
         USING MYKEYD,KEY                                                       
         MVC   MYKEYF1,DUB                                                      
         MVC   MYKEYF2,DUB                                                      
                                                                                
         USING MYKEYD,KEYSAVE                                                   
         MVC   MYKEYF1,DUB                                                      
         MVC   MYKEYF2,DUB                                                      
                                                                                
K        USING MYKEYD,KEY                                                       
         MVC   K.MYKEYF1,DUB                                                    
         MVC   K.MYKEYF2,DUB                                                    
                                                                                
K        USING MYKEYD,KEYSAVE                                                   
         MVC   K.MYKEYF1,DUB                                                    
         MVC   K.MYKEYF2,DUB                                                    
                                                                                
         ZAP   DUB,=P'5000000000'                                               
         ZAP   PL16,DUB                                                         
         MP    PL16,=P'20000'                                                   
         LH    RF,HALF                                                          
         AHI   RF,10000                                                         
         CVD   RF,DUB                                                           
         DP    PL16,DUB                                                         
         ZAP   PL16,PL16(8)                                                     
         BM    *+10                                                             
         AP    PL16,=P'1'                                                       
         DP    PL16,=P'2'                                                       
         ZAP   DUB,PL16(15)                                                     
         DC    H'0'                                                             
         ST    R2,RELO                                                          
         B     RDTEST4                                                          
         DS    CL1                                                              
         DS    0D                                                               
         USING *,R7                                                             
RDTEST4  LA    R3,6                                                             
         L     RF,=A(ISPACKED)                                                  
         A     RF,RELO                                                          
         BR    RF                                                               
         LTORG                                                                  
         MP    MYPL8,MYPL4                                                      
         SR    RF,RF                                                            
         IC    RF,LEVEL                                                         
         AHI   RF,2                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         DC    H'0'                                                             
MYPL8    DC    PL8'10000000'                                                    
MYPL4    DC    PL4'1000000'                                                     
KEY      DS    CL16                                                             
KEYSAVE  DS    CL16                                                             
PADDED   DS    CL6                                                              
LEVEL    DC    X'03'                                                            
CULC     DC    CL10' '                                                          
TESTER   DC    X'DB',C'SJSAMSAMPRONX8'                                          
         TP    TODAYI                                                           
         BE    ISPACKED                                                         
         ZAP   TODAYI,=P'9999'                                                  
ISPACKED CVB   R1,BIGONE                                                        
         AHI   R1,3                                                             
         DC    H'0'                                                             
         LA    R2,RONNIE                                                        
         LA    R3,THREE                                                         
         C     R2,=A(THREE)                                                     
         DC    H'0',C'$ABEND'                                                   
         GOTO1 =V(DATCON),DMCB,(5,0),(1,THREE)                                  
         DC    H'0'                                                             
         DC    H'0'                                                             
HALF     DC    X'2BC'                                                           
TODAYI   DC    C'030514'                                                        
TODAYO   DS    CL8                                                              
THREE    DC    XL8'00'                                                          
BIGONE   DC    PL8'2147483647'                                                  
         LA    R1,0(R1)                                                         
         DC    H'0'                                                             
RONNIE   DC    C'RONNIE'                                                        
ONE      DC    C'['                                                             
TWO      DC    C']'                                                             
         MVI   0(R1),X'82'                                                      
         LA    RF,0                                                             
         SHI   RF,5                                                             
         ST    RF,4(R1)                                                         
         LM    R3,R4,0(R1)                                                      
         XIT1                                                                   
RONAMT   DC    P'1751'                                                          
R1DATA   DC    XL4'FFFFFFEC'                                                    
MOIN     DC    C'981201'                                                        
MOOUT    DC    C'981201'                                                        
DOU2     DC    CL6' '                                                           
TEN      EQU   9                                                                
RELO     DS    A                                                                
BIGNUM   DS    CL11                                                             
LITNUM   DC    P'23'                                                            
JULIN    DC    X'098045'                                                        
JULIN2   DC    CL8'00000000'                                                    
JULOUT   DC    8C'0'                                                            
CUREDIT  DC    V(CUREDIT)                                                       
OK       GOTO1 =V(DATCON),DMCB,(1,IN),(0,WORK)                                  
         GOTO1 =V(DATCON),DMCB,(1,IN2),(0,WORK+6)                               
         GOTO1 =V(PERVERT),DMCB,WORK+6,WORK                                     
         MVC   DUB,DMCB+8                                                       
         BNL   JUMP                                                             
*                                                                               
*ABLE    DC    XL256'00'                                                        
*                                                                               
NIN      DC    C'A123'                                                          
         GOTO1 =V(ADDAY),DMCB,(C'Y',IN),WORK,-1                                 
         PACK  TLCKINV(4),WORK(7)                                               
         AP    CURRINV,=P'10'      CURRINV IS PL3                               
         MVC   TLCKINV+3(2),CURRINV    (PICK OFF THE TENS)                      
         MVC   SAVINV,TLCKINV                                                   
*                                                                               
         GOTO1 =V(DATCON),DMCB,(9,IN),(0,OUT)                                   
*                                                                               
         GOTO1 =V(DATVAL),DMCB,IN,(X'80',OUT)                                   
         CLI   TODAY0,X'FA'                                                     
         BL    JUMP                                                             
         MVC   WORK(4),TODAY0                                                   
         MVC   WORK+4(2),=C'01'                                                 
JUMP     XC    DATA,EFFS                                                        
         GOTO1 =V(DATCON),DMCB,(1,TYPEND),(1,TODAY0)                            
*                                                                               
         GOTO1 =V(DATCON),DMCB,(0,WORK),(1,WORK)                                
         PACK  DUB(3),TODAY20(7)                                                
         MVC   WORK(3),DUB                                                      
*                                                                               
         GOTO1 =V(DATCON),DMCB,(9,IN),(1,TODAY1)                                
         MVC   TYPEND,TODAY1     END DATE = TODAY                               
*                                                                               
         MVC   DUB+2(4),=C'1231'                                                
         GOTO1 =V(DATCON),DMCB,(0,DUB),(1,DUB+3)                                
         MVC   TYPEND,DUB+3                                                     
         XIT1                                                                   
*                                                                               
DATE     DC    C'981202'                                                        
DATE9    DC    C'19980918'                                                      
IN2      DC    X'B20805'                                                        
IN       DC    C'00000000RONNIE'                                                
TDAY     DC    C'20000912'                                                      
TYPEND   DC    XL3'FFFFFF'                                                      
DATA     DC    XL8'E667F8FFECFF'                                                
EFFS     DC    8X'FF'                                                           
TODAY0   DC    XL6'FCF2F0F6F0F0'                                                
OUT6     DS    CL6                                                              
TODAY1   DS    XL3                                                              
TODAY20  DS    CL8                                                              
CHKDATE1 DC    XL3'FFFFFF'                                                      
TLCKINV  DS    CL6                                                              
SAVINV   DS    CL6                                                              
CURRINV  DC    PL3'10'                                                          
XIT      XIT1                                                                   
*                                                                               
         LA    R2,DATEH                                                         
         GOTO1 =V(DATVAL),DMCB,8(R2),WORK                                       
*                                                                               
         GOTO1 =V(DATCON),DMCB,(0,WORK),(1,DUB)                                 
         MVC   BYTE,TLSCINV                                                     
         CLI   TLSCINV,X'20'       IF YYYYMM FORMAT, CONVERT THE                
         BE    SCT02                                                            
         CLI   TLSCINV,X'19'       BEFORE THE COMPARE                           
         BNE   SCT04                                                            
SCT02    UNPK  WORK(7),TLSCINV(4)                                               
         MVC   WORK+6(2),=C'01'    SET DAY TO 01                                
         GOTO1 =V(DATCON),DMCB,(9,WORK),(1,WORK)                                
         MVC   BYTE,WORK                                                        
SCT04    GOTO1 =V(DATCON),DMCB,(5,0),(0,WORK)                                   
         GOTO1 =V(ADDAY),DMCB,(C'Y',WORK),WORK+6,-2                             
         GOTO1 =V(DATCON),DMCB,(0,WORK+6),(1,WORK)                              
*                                                                               
         CLC   BYTE,WORK           IF YEAR OF RECORD AT LEAST 2                 
         BH    SCNO                                                             
*                                                                               
         DS    0D                                                               
DATEH    DC    CL20'12345678MAR03/12'                                           
SCYES    DC    CL18'THE ANSWER IS YES'                                          
SCNO     DC    CL18'THE ANSWER IS NO'                                           
*                                                                               
         CLC   WORK,OUT                                                         
         BH    HIGH                                                             
         B     LOW                                                              
*                                                                               
         DS    0D                                                               
HIGH     DC    C'2027 IS HIGHER THAN TODAY'                                     
         DS    0D                                                               
LOW      DC    C'TODAY IS HIGHER THAN 2027'                                     
         GOTO1 =V(DATCON),DMCB,(5,0),(3,WORK)  CALCULATE TODAY                  
*                                                                               
         ZIC   R1,WORK             TODAY'S YEAR                                 
         SH    R1,=H'2'            - 2                                          
         BM    DUMPIT                                                           
         MHI   R1,10                                                            
         CVD   R1,DUB                                                           
DUMPIT   DC    H'0'                                                             
*                                                                               
TLSCINV  DC    X'9507189999'                                                    
BYTE     DC    X'00'                                                            
         DS    CL4                 TAPE LENGTH                                  
WORK     DS    CL60                                                             
DUB      DS    D                                                                
PL16     DS    PL16                                                             
DMCB     DS    CL24                                                             
ELCODE   DS    C                                                                
NOW      DS    CL8                                                              
IN1      DS    XL8                                                              
OUT      DS    CL8                                                              
PWOCENT  DS    PL1                                                              
PWOYEAR  DS    PL1                                                              
PWOMONTH DS    0PL1                                                             
PWOWEEK  DS    PL1                                                              
PWONUM   DS    PL2                                                              
PWOIND   DS    PL1                                                              
FFFF     DC    X'FFFFFFFFFFFF'                                                  
INVIN    DS    0CL6                                                             
NEWMONTH DC    C'H'                                                             
NEWYEAR  DC    C'0'                                                             
NEWNUM   DC    C'0001'                                                          
DECADE   DS    C                                                                
WORKIND  DS    C                                                                
DATEOUT  DS    CL8                                                              
COMPFIN  DS    0PL2                                                             
COMPFINY DS    X                                                                
COMPFINM DS    X                                                                
RONPSFST DC    C'8'                                                             
MONTABLE DS    0D                                                               
         DC    C'01',C'A',C'A',X'01'                                            
         DC    C'02',C'B',C'B',X'02'                                            
         DC    C'03',C'C',C'C',X'03'                                            
         DC    C'04',C'D',C'D',X'04'                                            
         DC    C'05',C'E',C'E',X'05'                                            
         DC    C'06',C'F',C'F',X'06'                                            
         DC    C'07',C'G',C'G',X'07'                                            
         DC    C'08',C'H',C'H',X'08'                                            
         DC    C'09',C'I',C'I',X'09'                                            
         DC    C'10',C'J',C'J',X'10'                                            
         DC    C'11',C'K',C'K',X'11'                                            
         DC    C'12',C'L',C'L',X'12'                                            
         DC    C'00',C'0',C'0',X'00'                                            
LRECD    DSECT                                                                  
         DS    CL4                                                              
         DS    CL7                                                              
         DS    CL7                                                              
LRECDQ   EQU   *-LRECD                                                          
*                                                                               
       ++INCLUDE ACGENFILE                                                      
MOND     DSECT                                                                  
MONENT   DS    0CL5                                                             
MONUS    DS    CL2                                                              
MONCAN   DS    CL1                                                              
MONNEW   DS    CL1                                                              
MONPWO   DS    PL1                                                              
*                                                                               
TABLED   DSECT                                                                  
FIELD1   DS    CL5                                                              
FIELD2   DS    CL1                                                              
FIELD3   DS    CL6                                                              
FIELD4   DS    CL10                                                             
*                                                                               
MYKEYD   DSECT                                                                  
MYKEYF1  DS    CL8                                                              
MYKEYF2  DS    CL8                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'170RDTESTA   09/10/04'                                      
         END                                                                    
