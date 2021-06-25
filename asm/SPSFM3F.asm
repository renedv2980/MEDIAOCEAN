*          DATA SET SPSFM3F    AT LEVEL 055 AS OF 08/11/11                      
*PHASE T2173FA                                                                  
*                                                                               
         TITLE 'SPSFM3F INFO LIST'                                              
T2173F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2173F*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   MAIN10                                                           
         MVI   NLISTS,14                                                        
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
MAIN10   CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   EXIT                                                             
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        VALIDATE KEY ROUTINE                                                   
*                                                                               
VK       DS    0H                                                               
         LA    R2,SINMEDH          MEDIA                                        
         TM    4(R2),X'20'         VALIDATED                                    
         BO    VK1                                                              
         NI    SINCLTH+4,X'DF'                                                  
         GOTO1 VALIMED                                                          
*                                                                               
VK1      OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SINCLTH          CLIENT                                       
         TM    4(R2),X'20'         VALIDATED                                    
         BO    VK2                                                              
         NI    SINPRDH+4,X'DF'                                                  
         GOTO1 VALICLT                                                          
*                                                                               
VK2      OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SINPRDH          PRODUCT                                      
         TM    4(R2),X'20'         VALIDATED                                    
         BO    VK3                                                              
         NI    SINESTH+4,X'DF'                                                  
         MVI   MYPRD,0                                                          
         MVC   SINPRDN,SPACES                                                   
         OI    SINPRDNH+6,X'80'                                                 
*****    CLI   5(R2),0             OPTIONAL FILTER                              
****     BE    VK3                                                              
         GOTO1 ANY                                                              
         GOTO1 VALIPRD                                                          
         MVC   MYPRD,BPRD                                                       
         CLI   MYPRD,X'FF'                                                      
         BE    INVERR                                                           
         MVC   SINPRDN,PRDNM                                                    
         OI    SINPRDNH+6,X'80'                                                 
*                                                                               
VK3      OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SINESTH          ESTIMATE                                     
         TM    4(R2),X'20'         VALIDATED                                    
         BO    VK5                                                              
         NI    SINMKTH+4,X'DF'                                                  
         MVI   MYEST,0                                                          
         CLI   5(R2),0             OPTIONAL FILTER                              
         BE    VK5                                                              
         CLI   SINPRDH+5,0         MUST HAVE PRD TOO                            
         BNE   VK4                                                              
         LA    R2,SINPRDH          POINT TO PRD                                 
         B     MISSERR                                                          
*                                                                               
VK4      GOTO1 VALIEST                                                          
         MVC   MYEST,BEST                                                       
*                                                                               
VK5      OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SINMKTH          START AT MARKET                              
         TM    4(R2),X'20'         VALIDATED                                    
         BO    VK15                                                             
         XC    MYMKT,MYMKT                                                      
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         CLI   SINPRDH+5,0         MUST HAVE PRODUCT                            
         BNE   VK6                                                              
         LA    R2,SINPRDH          POINT TO PRD                                 
         B     MISSERR                                                          
*                                                                               
VK6      CLI   SINESTH+5,0         MUST HAVE ESTIMATE                           
         BNE   VK7                                                              
         LA    R2,SINESTH          POINT TO EST                                 
         B     MISSERR                                                          
*                                                                               
VK7      GOTO1 VALIMKT                                                          
         MVC   MYMKT,BMKT                                                       
*                                                                               
VK10     OI    4(R2),X'20'         VALIDATED                                    
         XC    MKSTNXT,MKSTNXT                                                  
*                                                                               
VK15     MVI   LKEYCOMP,4          BASIC L'COMP - 1                             
         CLI   SINPRDH+5,0                                                      
         BE    VK17                                                             
         MVI   LKEYCOMP,5          INCLUDE PRD                                  
         CLI   SINESTH+5,0                                                      
         BE    VK17                                                             
         MVI   LKEYCOMP,6          INCLUDE EST                                  
*                                                                               
         USING INFORECD,R4                                                      
VK17     XC    KEY,KEY                                                          
         MVI   ERROPT,C'Y'                                                      
         XC    MKSTCNT,MKSTCNT                                                  
         LA    R4,KEY                                                           
         MVC   INFKTYP,=X'0D79'    RECORD TYPE                                  
         MVC   INFKAGMD,BAGYMD     AGENCY/MEDIA                                 
         MVC   INFKCLT,BCLT        CLIENT                                       
         MVC   INFKPRD,MYPRD       PRODUCT                                      
         MVC   INFKEST,MYEST       ESTIMATE                                     
         MVC   INFKMKT,MYMKT       MARKET                                       
         MVC   MYSVKEY,KEY                                                      
*                                                                               
         XC    MKSTTAB,MKSTTAB                                                  
         LA    R3,MKSTTAB                                                       
         USING MKSTABD,R3                                                       
         MVI   0(R3),X'FF'                                                      
         LA    R4,KEY                                                           
         USING INFORECD,R4                                                      
         GOTO1 HIGH                                                             
         B     VK30                                                             
*                                                                               
VK20     GOTO1 SEQ                                                              
*                                                                               
VK30     DS    0H                                                               
         ZIC   R1,LKEYCOMP                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE           COMP UP TO MKT                          
         BNE   VK40                                                             
         MVC   MKSTPRD,INFKPRD     SET PRODUCT                                  
         MVC   MKSTEST,INFKEST     SET ESTIMATE                                 
         MVC   MKSTMKT,INFKMKT     SET MKT/STATION IN TABLE                     
         MVC   MKSTSTA,INFKSTA                                                  
         OI    MKSTFLG,MKSTFTG     SET TARGET FOUND                             
         MVC   MKSTDA,KEY+14       KEEP D/A                                     
         LA    R3,L'MKSTTAB(R3)                                                 
         MVI   0(R3),X'FF'         SET END OF TABLE                             
         L     R1,MKSTCNT          INC COUNTER                                  
         LA    R1,1(R1)                                                         
         ST    R1,MKSTCNT                                                       
*                                                                               
         C     R3,MKSTTABX                                                      
         BL    VK20                                                             
         DC    H'0'                TABLE TOO SMALL                              
         DROP  R4                                                               
*                                                                               
VK40     LA    R4,KEY                                                           
         USING BUYRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(4),MYSVKEY+2       SET BUY KEY                               
         MVC   KEY+4(2),MYMKT                                                   
         GOTO1 HIGH                                                             
         B     VK60                                                             
*                                                                               
VK50     GOTO1 SEQ                                                              
*                                                                               
VK60     DS    0H                                                               
         LA    R1,2                                                             
         CLI   MYPRD,0             PRODUCT FILTER                               
         BE    *+8                                                              
         LA    R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   VKX                                                              
         LA    R4,KEY                                                           
         CLI   BUYKPRD,X'FF'       START OF POL POINTERS                        
         BE    VKX                                                              
         CLI   BUYKBUY,X'FF'       PASSIVE POINTER                              
         BE    VK70                                                             
         TM    BUYKBUY,X'80'       SPILL POINTER                                
         BO    VK50                                                             
*                                                                               
VK70     CLI   MYEST,0              ESTIMATE FILTER                             
         BE    VK75                                                             
         CLC   BUYKEST,MYEST        CORRECT ESTIMATE                            
         BNE   VK50                                                             
*                                                                               
VK75     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LA    R4,BDELEM                                                        
         MVI   ELCODE,X'10'                                                     
         BAS   RE,NEXTEL                                                        
         BE    VK80                                                             
         LA    R4,BDELEM                                                        
         MVI   ELCODE,X'72'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   VK50                                                             
*                                                                               
VK80     BAS   RE,FNDENTRY         SEE IF THIS MKT/STATION IN TABLE             
         B     VK50                                                             
*                                                                               
VKX      DS    0H                                                               
         L     R3,MKSTCNT          INC COUNTER                                  
         LTR   R3,R3                                                            
         BZ    VKX10                                                            
         GOTO1 QSORT,DMCB,MKSTTAB,(R3),12,7,0                                   
*                                                                               
VKX10    XC    KEY,KEY                                                          
         MVC   KEY,MYSVKEY         RESET KEY                                    
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
         USING LISTD,R2                                                         
LR       DS    0H                                                               
         LA    R3,MKSTTAB                                                       
         L     R1,MKSTNXT                                                       
         AR    R3,R1                                                            
         USING MKSTABD,R3                                                       
*                                                                               
LR10     CLI   0(R3),X'FF'                                                      
         BE    LR70                                                             
         BAS   RE,SETPRD           PRODUCT CODE                                 
         MVC   LPRD,WORK                                                        
         ZIC   R0,MKSTEST           ESTIMATE CODE                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LEST(3),DUB                                                      
*                                                                               
         XC    WORK,WORK                                                        
         OC    MKSTMKT(5),MKSTMKT                                               
         BNZ   LR15                                                             
         MVC   LSTAT(5),=C'*ALL*'                                               
         MVC   LMKT,SPACES                                                      
         MVC   LMKTNM,SPACES                                                    
         B     LR30                                                             
*                                                                               
LR15     GOTO1 MSUNPK,DMCB,(X'80',MKSTMKT),WORK,WORK+10                         
         MVC   LMKT,WORK                                                        
         MVC   LSTAT,SPACES                                                     
         MVC   LSTAT(4),WORK+10                                                 
         CLI   LSTAT,C'0'          IS THIS CABLE                                
         BL    LR20                                                             
         MVI   LSTAT+4,C'/'                                                     
         MVC   LSTAT+5(3),WORK+15                                               
*                                                                               
LR20     BAS   RE,GETMKT                                                        
         MVC   LMKTNM,WORK                                                      
*                                                                               
LR30     MVI   LTARG,C' '                                                       
         TM    MKSTFLG,MKSTFTG     TARGET                                       
         BNO   *+8                                                              
         MVI   LTARG,C'Y'                                                       
         MVI   LBUY,C' '                                                        
         TM    MKSTFLG,MKSTFBU     BUY                                          
         BNO   *+8                                                              
         MVI   LBUY,C'Y'                                                        
*                                                                               
         MVC   DMDSKADD,MKSTDA     SET D/A FOR GENCON                           
*                                                                               
         LA    R3,L'MKSTTAB(R3)                                                 
         L     R1,MKSTNXT                                                       
         LA    R1,L'MKSTTAB(R1)                                                 
         ST    R1,MKSTNXT                                                       
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR50                                                             
         GOTO1 LISTMON                                                          
         B     LR60                                                             
*                                                                               
LR50     GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR60     B     LR10                                                             
*                                                                               
LR70     XC    MKSTNXT,MKSTNXT                                                  
*                                                                               
LRX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        FIND EBCDIC PRODUCT                                                    
*                                                                               
SETPRD   NTR1                                                                   
         LA    R1,SVCLIST                                                       
*                                                                               
SP10     CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MKSTPRD,3(R1)                                                    
         BE    SP20                                                             
         LA    R1,4(R1)                                                         
         B     SP10                                                             
*                                                                               
SP20     MVC   WORK(3),0(R1)                                                    
*                                                                               
SPX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
GETMKT   NTR1                                                                   
         LA    R4,KEY                                                           
         USING MKTRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(15),=C'000000000000000'                                      
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,LMKT                                                     
         MVC   MKTKAGY,AGENCY                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         L     R4,AIO                                                           
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'MKTNM),MKTNAME  SET MARKET NAME                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
FNDENTRY NTR1                                                                   
         LA    R4,KEY                                                           
         USING BUYRECD,R4                                                       
         LA    R3,MKSTTAB                                                       
         USING MKSTABD,R3                                                       
*                                                                               
FE10     CLI   0(R3),X'FF'                                                      
         BE    FE30                                                             
         CLC   MKSTPRD,BUYKPRD     PRODUCT                                      
         BNE   FE20                                                             
         CLC   MKSTEST,BUYKEST     ESTIMATE                                     
         BNE   FE20                                                             
         CLC   MKSTMKT(5),BUYMSTA  MKT/STATION IN TABLE                         
         BNE   FE20                                                             
         OI    MKSTFLG,MKSTFBU     SET BUY FOUND                                
         B     FEX                                                              
*                                                                               
FE20     LA    R3,L'MKSTTAB(R3)                                                 
         B     FE10                                                             
*                                                                               
FE30     DS    0H                                                               
         MVC   MKSTPRD,BUYKPRD     SET PRODUCT                                  
         MVC   MKSTEST,BUYKEST     SET ESTIMATE                                 
         MVC   MKSTMKT,BUYMSTA     SET MKT/STATION IN TABLE                     
         MVC   MKSTSTA,BUYMSTA+2                                                
         OI    MKSTFLG,MKSTFBU     SET BUY FOUND                                
         LA    R3,L'MKSTTAB(R3)                                                 
         MVI   0(R3),X'FF'         SET END OF TABLE                             
         L     R1,MKSTCNT          INC COUNTER                                  
         LA    R1,1(R1)                                                         
         ST    R1,MKSTCNT                                                       
*                                                                               
         C     R3,MKSTTABX                                                      
         BL    FEX                                                              
         DC    H'0'                TABLE TOO SMALL                              
*                                                                               
FEX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX               NEVER TO RETURN                              
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,60,REQUESTOR                                                  
         SSPEC H2,60,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,35,C'INFO LIST'                                               
         SSPEC H2,35,C'---------'                                               
         SPACE 1                                                                
         SSPEC H4,1,C'PRD'                                                      
         SSPEC H4,5,C'EST'                                                      
         SSPEC H4,9,C'STATION'                                                  
         SSPEC H4,18,C'MARKET'                                                  
         SSPEC H4,25,C'MARKET NAME'                                             
         SSPEC H4,50,C'TARGET BUY'                                              
         SPACE 1                                                                
         SSPEC H5,1,C'---'                                                      
         SSPEC H5,5,C'---'                                                      
         SSPEC H5,9,C'-------'                                                  
         SSPEC H5,18,C'------'                                                  
         SSPEC H5,25,C'-----------'                                             
         SSPEC H5,50,C'------ ---'                                              
         SPACE 1                                                                
         DC    X'00'                                                            
         SPACE 2                                                                
         DS    0D                                                               
MKSTTAB  DS    500CL12                                                          
MKSTTABX DS    XL1                                                              
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         PRINT   OFF                                                            
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT   ON                                                             
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM9DD          DSECT FOR RECORD LISTING.                    
         EJECT                                                                  
         ORG   SINWORK                                                          
MKSTCNT  DS    F                                                                
MKSTNXT  DS    F                                                                
MYPRD    DS    XL1                                                              
MYEST    DS    XL1                                                              
MYMKT    DS    XL2                                                              
LKEYCOMP DS    XL1                                                              
MYSVKEY  DS    CL(L'KEY)                                                        
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
*                                                                               
*                                                                               
MKSTABD  DSECT                                                                  
MKSTPRD  DS    XL1                                                              
MKSTEST  DS    XL1                                                              
MKSTMKT  DS    XL2                                                              
MKSTSTA  DS    XL3                                                              
MKSTFLG  DS    XL1                                                              
MKSTFTG  EQU   X'80'                                                            
MKSTFBU  EQU   X'40'                                                            
MKSTDA   DS    XL4                                                              
*                                                                               
         SPACE 2                                                                
LISTD    DSECT                                                                  
LPRD     DS    CL3                                                              
         DS    CL1                                                              
LEST     DS    CL3                                                              
         DS    CL1                                                              
LSTAT    DS    CL8                                                              
         DS    CL2                                                              
LMKT     DS    CL4                                                              
         DS    CL2                                                              
LMKTNM   DS    CL24                                                             
         DS    CL3                                                              
LTARG    DS    CL1                                                              
         DS    CL5                                                              
LBUY     DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE SPGENINFO                                                      
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
*DDSPLWORKD                                                                     
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055SPSFM3F   08/11/11'                                      
         END                                                                    
