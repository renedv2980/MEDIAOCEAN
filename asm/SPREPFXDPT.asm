*          DATA SET SPREPFXDPT AT LEVEL 037 AS OF 11/04/96                      
*PHASE SPFX02U                                                                  
         TITLE 'SPFX02 - DELETE COKE BUYS'                                      
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,MKTFRST                                                     
         BE    MKTF                                                             
*                                                                               
         CLI   MODE,STAFRST                                                     
         BE    STAF                                                             
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    FX10                                                             
*                                                                               
         CLI   MODE,REQLAST                                                     
         BE    FX400                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* MARKET FIRST                                                                  
MKTF     DS    0H                  GET TIME ZONE                                
         MVC   SAVEKEY,KEY                                                      
         XC    ZONE(2),ZONE                                                     
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING MKTRECD,R6                                                       
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVI   MKTKMED,C'T'                                                     
         EDIT  (2,SAVEKEY+4),MKTKMKT,FILL=0                                     
         MVC   MKTKAGY,=C'CK'                                                   
         MVC   MKTKFILL,=C'0000000'                                             
         GOTO1 HIGHMKT                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ADMARKET                                                      
         MVC   ZONE,MKTZONE                                                     
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
         B     EXIT                                                             
                                                                                
STAF     DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING STARECD,R6                                                       
         GOTO1 MSUNPK,DMCB,SAVEKEY+4,WORK,STAKCALL                              
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'T'                                                     
         MVC   STAKAGY,=C'CK'                                                   
         GOTO1 HIGHSTA                                                          
         CLC   KEY(9),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ADSTAT                                                        
         MVC   NETWRK,SNETWRK                                                   
         CLI   NETWRK,C'I'           IF NOT INDEPENDENT                         
         BE    STAFEX                                                           
         MVI   NETWRK,C'N'           SET TO NETWORK                             
         CLI   PROGPROF+3,C'Y'       TEST FOX OPTION                            
         BNE   STAFEX                                                           
         CLC   SNETWRK,=C'FOX'                                                  
         BNE   STAFEX                                                           
         MVI   NETWRK,C'F'         SPECIAL DAYPARTS FOR FOX                     
                                                                                
STAFEX   XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         DROP  R6                                                               
                                                                                
* PROCBUY                                                                       
FX10     DS    0H                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         CLC   =C'SI5',BDPROGRM                                                 
         BE    FX15                                                             
         CLC   =C'AUTO - I5',BDPROGRM                                           
         BNE   EXIT                                                             
                                                                                
FX15     LA    R2,TIMTDIR                                                       
FX17     CLI   0(R2),X'FF'                                                      
         BE    FX200                                                            
         CLC   ZONE(2),0(R2)                                                    
         BE    FX20                                                             
         LA    R2,8(R2)                                                         
         B     FX17                                                             
                                                                                
FX20     DS    0H                                                               
         MVI   IDPT,0                                                           
         L     R2,4(R2)                                                         
         LA    R6,BDELEM                                                        
FX25     CLI   0(R6),0                                                          
         BE    FX70                                                             
         CLI   0(R6),X'10'                                                      
         BE    FX30                                                             
FX27     ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     FX25                                                             
*                                                                               
FX30     DS    0H                                                               
***      GOTO1 HEXOUT,DMCB,(R6),P,6,=C'TOG'                                     
***      GOTO1 REPORT                                                           
         USING AFFELEM,R6                                                       
         MVC   ITIME,ATIME                                                      
         NI    ITIME,X'FF'-X'E0'                                                
         ZICM  R1,ITIME,2                                                       
         CH    R1,=H'600'          CHECK BEFORE 6 AM                            
         BNL   *+12                                                             
         AH    R1,=H'2400'                                                      
         STCM  R1,3,ITIME                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,ADATE),WORK                                       
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         ZIC   R1,DMCB                                                          
         LA    R1,DAYLIST-1(R1)                                                 
         MVC   IDAYB,0(R1)         DAY BIT                                      
*                                                                               
***      EDIT  ITIME,(5,P)                                                      
***      EDIT  IDAYB,(5,P+10)                                                   
***      GOTO1 REPORT                                                           
*                                                                               
         ZIC   R1,IDAYB                                                         
FX40     DS    0H                                                               
         CLI   0(R2),X'FF'         EOT                                          
         BNE   *+6                                                              
         DC    H'0'                NO DAYPART FOUND FOR DAY/TIME                
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(R2),0             TEST DAY                                     
         BZ    FX50                                                             
         CLC   ITIME,1(R2)                                                      
         BNH   FX60                                                             
FX50     DS    0H                                                               
         LA    R2,TIMTABL(R2)                                                   
         B     FX40                                                             
*                                                                               
FX60     DS    0H                                                               
         CLI   IDPT,0                                                           
         BE    FX65                                                             
         CLC   IDPT,3(R2)                                                       
         BNE   FX210                                                            
FX65     MVC   IDPT,3(R2)                                                       
         B     FX27                                                             
*                                                                               
FX70     CLI   IDPT,0                                                           
         BE    FX220                                                            
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         CLC   IDPT,BDDAYPT                                                     
         BE    EXIT                                                             
         MVC   BDDAYPT,IDPT                                                     
         MVC   BDTIMST(4),14(R2)                                                
         MVC   P+40(3),=C'DPT'                                                  
         MVC   P+45(1),BDDAYPT                                                  
         EDIT  BDTIMST,(5,P+50)                                                 
         EDIT  BDTIMEND,(5,P+55)                                                
         MVC   SAVEKEY,KEY                                                      
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX80                                                             
         ST    R6,AREC                                                          
         GOTO1 DATAMGR,DMCB,PUTREC,=C'SPTFILE',KEY+14,AREC                      
FX80     GOTO1 HEXOUT,DMCB,KEY,P,18,=C'TOG'                                     
         GOTO1 REPORT                                                           
         L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
         B     EXIT                                                             
FX200    DS    0H                                                               
         MVC   P(2),ZONE                                                        
         MVC   P+5(16),=C'NO DAYPART TABLE'                                     
         B     FX300                                                            
                                                                                
FX210    DS    0H                                                               
         L     R1,COUNT1                                                        
         LA    R1,1(R1)                                                         
         ST    R1,COUNT1                                                        
         MVC   P(2),ZONE                                                        
         MVC   P+5(17),=C'DPTS DO NOT MATCH'                                    
         B     FX300                                                            
                                                                                
FX220    DS    0H                                                               
         L     R6,ADBUY                                                         
****     BAS   RE,PRTBUY                                                        
         MVC   P(2),ZONE                                                        
         MVC   P+5(10),=C'NO DAYPART'                                           
         L     R1,COUNT2                                                        
         LA    R1,1(R1)                                                         
         ST    R1,COUNT2                                                        
         B     FX300                                                            
                                                                                
FX300    GOTO1 HEXOUT,DMCB,KEY,P2,18,=C'TOG'                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
                                                                                
FX400    DS    0H                                                               
         MVC   P(15),=C'RECORDS CHANGED'                                        
         EDIT  COUNT,(10,P+20),0                                                
         EDIT  COUNT1,(10,P+40),0                                               
         EDIT  COUNT2,(10,P+55),0                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                          PRINT BUY                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRTBUY   NTR1                                                                   
         USING BUYRECD,R6                                                       
         BC    0,PRTB2                                                          
         OI    *-3,X'F0'                                                        
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'BUYREC',(R6),C'DUMP',(R0),=C'1D00'                
*                                                                               
PRTB2    LA    R4,P+2                                                           
         USING PLINED,R4                                                        
         MVC   PAGY(2),BUYALPHA                                                 
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(2),=C'TV'                                                 
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         ZIC   R0,KEY+11           GET CURRENT NON-POL LINE NUMBER              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLINOLD,DUB                                                      
*                                                                               
****     GOTO1 HEXOUT,DMCB,KEY,P2,18                                            
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         DS    0D                                                               
COUNT1   DS    F                                                                
COUNT2   DS    F                                                                
COUNT    DS    F                                                                
ELCODE   DS    X                                                                
SAVEKEY  DS    XL18                                                             
ITIME    DS    XL2                                                              
IDAYB    DS    X                                                                
IDPT     DS    X                                                                
ZONE     DS    C                                                                
NETWRK   DS    C                                                                
         EJECT                                                                  
         LTORG                                                                  
DAYLIST  DC    X'4020100804020100'                                              
*                                                                               
TIMTDIR  DS    0F                                                               
         DC    C'1I  ',AL4(TTCCT1I)    COKE TELEVISION ONLY                     
         DC    C'1N  ',AL4(TTCCT1N)                                             
         DC    C'2I  ',AL4(TTCCT2I)                                             
         DC    C'2N  ',AL4(TTCCT2N)                                             
         DC    X'FF'                                                            
*                                                                               
TIMTABL  EQU   18                                                               
TTCCT1N  DS    0C                 COKE/T/EAST-WEST/NETWORK                      
*                                                                               
         DC    X'03',AL2(1259),C'D',CL10'6A-1P ',AL2(0600,1300)                 
         DC    X'03',AL2(1759),C'E',CL10'1P-6P ',AL2(1300,1800)                 
         DC    X'7C',AL2(1559),C'D',CL10'6A-4P ',AL2(0600,1600)                 
         DC    X'7C',AL2(1759),C'E',CL10'4-6P  ',AL2(1600,1800)                 
         DC    X'7E',AL2(1959),C'A',CL10'6-8P  ',AL2(1800,2000)                 
         DC    X'7E',AL2(2259),C'P',CL10'8-11P ',AL2(2000,2300)                 
         DC    X'01',AL2(1859),C'A',CL10'6-7P  ',AL2(1800,1900)                 
         DC    X'01',AL2(2259),C'P',CL10'7-11P ',AL2(1900,2300)                 
         DC    X'7F',AL2(2459),C'L',CL10'11P-1A',AL2(2300,0100)                 
         DC    X'7F',AL2(2959),C'Z',CL10'1A-6A ',AL2(0100,0600)                 
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTCCT2N  DS    0C            COKE/T/CENTRAL-MOUNTAIN/NETWORK                    
*                                                                               
         DC    X'03',AL2(1159),C'D',CL10'6A-12N ',AL2(0600,1200)                
         DC    X'03',AL2(1659),C'E',CL10'12N-5P ',AL2(1200,1700)                
         DC    X'7C',AL2(1459),C'D',CL10'6A-3P  ',AL2(0600,1500)                
         DC    X'7C',AL2(1659),C'E',CL10'3-5P   ',AL2(1500,1700)                
         DC    X'7E',AL2(1859),C'A',CL10'5-7P   ',AL2(1700,1900)                
         DC    X'7E',AL2(2159),C'P',CL10'7-10P  ',AL2(1900,2200)                
         DC    X'01',AL2(1759),C'A',CL10'5-6P   ',AL2(1700,1800)                
         DC    X'01',AL2(2159),C'P',CL10'6-10P  ',AL2(1800,2200)                
         DC    X'7F',AL2(2359),C'L',CL10'10P-12M',AL2(2200,2400)                
         DC    X'7F',AL2(2959),C'Z',CL10'12M-6A ',AL2(2400,0600)                
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTCCT1I  DS    0C                 COKE/T/EAST-WEST/INDEPENDENT                  
*                                  (SAME AS NETWORK AFFILIATES)                 
         DC    X'03',AL2(1259),C'D',CL10'6A-1P ',AL2(0600,1300)                 
         DC    X'03',AL2(1759),C'E',CL10'1P-6P ',AL2(1300,1800)                 
         DC    X'7C',AL2(1559),C'D',CL10'6A-4P ',AL2(0600,1600)                 
         DC    X'7C',AL2(1759),C'E',CL10'4-6P  ',AL2(1600,1800)                 
         DC    X'7F',AL2(1959),C'A',CL10'6-8P  ',AL2(1800,2000)                 
         DC    X'7F',AL2(2259),C'P',CL10'8-11P ',AL2(2000,2300)                 
         DC    X'7F',AL2(2459),C'L',CL10'11P-1A',AL2(2300,0100)                 
         DC    X'7F',AL2(2959),C'Z',CL10'1A-6A ',AL2(0100,0600)                 
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTCCT2I  DS    0C            COKE/T/CENTRAL-MOUNTAIN/INDEPENDENT                
*                                                                               
         DC    X'03',AL2(1159),C'D',CL10'6A-12N ',AL2(0600,1200)                
         DC    X'03',AL2(1659),C'E',CL10'12N-5P ',AL2(1200,1700)                
         DC    X'7C',AL2(1459),C'D',CL10'6A-3P  ',AL2(0600,1500)                
         DC    X'7C',AL2(1659),C'E',CL10'3-5P   ',AL2(1500,1700)                
         DC    X'7F',AL2(1859),C'A',CL10'5-7P   ',AL2(1700,1900)                
         DC    X'7F',AL2(2159),C'P',CL10'7-10P  ',AL2(1900,2200)                
         DC    X'7F',AL2(2359),C'L',CL10'10P-12M',AL2(2200,2400)                
         DC    X'7F',AL2(2959),C'Z',CL10'12M-6A ',AL2(2400,0600)                
         DC    X'FFFFFFFF'                                                      
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL1                                                              
PLINOLD  DS    CL3                                                              
         DS    CL1                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037SPREPFXDPT11/04/96'                                      
         END                                                                    
