*          DATA SET PPDEL61REC AT LEVEL 097 AS OF 04/02/02                      
*PHASE XTRPRT2A                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE LOADER                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'CHECK FOR CORRECT PRINTPAK DATA IN XTRACT'                      
*                                                                               
XTRPRT2  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,XTRPRT2,=V(REGSAVE)                                            
         SPACE 2                                                                
         LA    R6,XTRPRT2+4095                                                  
         LA    R6,1(R6)                                                         
         USING XTRPRT2+4096,R6    R6 AS 2ND BASE REGISTER                       
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
*                                                                               
         BAS   RE,PRNT                                                          
         GOTO1 =V(DATCON),DMCB,(5,0),(8,TODAY)                                  
*                                                                               
PRTOPEN  DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,PRINT,FILELIST,REC                       
*                                                                               
         BRAS  RE,LOADER                                                        
*                                                                               
         B     START1                                                           
PRTCLOSE DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,DMCLSE,PRINT                                    
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                ERRORS ARE DEADLY                            
*                                                                               
PRINT    DC    CL7'PRINT'                                                       
PRTDIR   DC    CL7'PRTDIR'                                                      
PRTFIL   DC    CL7'PRTFILE'                                                     
DMOPEN   DC    CL7'DMOPEN'                                                      
DMCLSE   DC    CL7'DMCLSE'                                                      
DMRDHI   DC    CL7'DMRDHI'                                                      
DMRSEQ   DC    CL7'DMRSEQ'                                                      
GETREC   DC    CL7'GETREC'                                                      
*                                                                               
FILELIST DS    0H                                                               
         DC    CL8'NPRTDIR'                                                     
         DC    CL8'NPRTFILE'                                                    
         DC    CL8'X'                                                           
         SPACE 2                                                                
*                                                                               
*                                                                               
START1   DS    0H                                                               
*                                                                               
*DIE      DC    H'0'                                                            
*                                                                               
         BAS   RE,CARDS                                                         
         CLC   =C'/*',CARD                                                      
         BE    START12                                                          
         CLC   =C'DUMP=',CARD                                                   
         BNE   START2                                                           
         PACK  DMPCNT,CARD+5(4)                                                 
         B     START1                                                           
*                                                                               
START2   DS    0H                                                               
*                                                                               
         CLC   =C'PRINT',CARD                                                   
         BNE   START3                                                           
         MVI   PRTSW,C'Y'                                                       
         B     START1                                                           
*                                                                               
START3   DS    0H                                                               
*                                                                               
         CLC   =C'AGY=',CARD                                                    
         BNE   START3B                                                          
         MVC   AGYNAME,CARD+4                                                   
         B     START1                                                           
*                                                                               
*                                                                               
START3B  DS    0H                                                               
*                                                                               
         MVC   P+1(80),CARD                                                     
         BAS   RE,PRNT                                                          
*                                                                               
         B     START1                                                           
*                                                                               
START10  DS    0H                                                               
*                                                                               
START12  DS    0H                                                               
*                                                                               
         LA    R8,1                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(L'AGYNAME),AGYNAME                                           
         MVI   KEY+L'AGYNAME,C'I'                                               
         MVI   KEY+3,X'20'                                                      
*                                                                               
LOOP     DS    0H                                                               
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'08',DMRDHI),PRTDIR,KEY,RC1                   
*                                                                               
CHECK    DS    0H                                                               
*                                                                               
         CLC   AGYNAME,RC1                                                      
         BNE   EOF                                                              
         CLI   RC1+3,X'20'                                                      
         BNE   NEXT                                                             
         CLI   RC1+25,X'FF'                                                     
         BE    SEQ                                                              
         CLC   RC1+21(3),=X'000000'                                             
         BNE   SEQ                                                              
         B     GETRCRD                                                          
*                                                                               
NEXT     DS    0H                                                               
         CLI   RC1+3,X'20'                                                      
         BH    HIGH                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),RC1                                                       
         MVI   KEY+3,X'20'                                                      
         B     LOOP                                                             
*                                                                               
HIGH     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),RC1                                                       
         MVI   KEY+3,X'FF'                                                      
         B     LOOP                                                             
***                                                                             
GETRCRD  DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'08',GETREC),PRTFIL,RC1+27,REC,DMWORK         
         CLI   8(R1),0                                                          
         BE    BUY                                                              
         CLI   8(R1),X'02'                                                      
         BE    BUY                                                              
         DC    H'0'                                                             
SEQ1     DS    0H                                                               
         XC    P(80),P                                                          
         MVC   P+1(31),RC1                                                      
         BAS   RE,PRNT                 IF NOT PRINT IT                          
***                                                                             
SEQ      DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'08',DMRSEQ),PRTDIR,,RC1                      
         B     CHECK                                                            
**********************************************************************          
**********************************************************************          
**********************************************************************          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
BUY      DS    0H                                                               
*                                                                               
TEST     CLI   REC+3,X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R7,P                                                             
         USING STARD,R7                                                         
*                                                                               
         AP    BUYCNT,=P'1'                                                     
*                                                                               
*                                                                               
         TM    PBUYCNTL,X'80'                                                   
         BZ    BUY001                                                           
         AP    DELCNT,=P'1'                                                     
*                                                                               
BUY001   DS    0H                                                               
         OC    SAVEK,SAVEK         CHECK IF SAVEDAT IS CLEAR                    
         BZ    BUY005                  MEANS FIRST RUN                          
*                                                                               
         CLC   SAVEK,PBUYREC        COMPARE IF SAME CLIENT CODE                 
         BE    BUY005                                                           
*                                                                               
         BAS   RE,PRNT                 IF NOT PRINT IT                          
*                                                                               
         ZAP   BUYGRS,=P'0'                                                     
         ZAP   BUYAGY,=P'0'                                                     
         ZAP   BUYDSC,=P'0'                                                     
         AP    CLTCNT,=P'1'                                                     
         AHI   R8,1                                                             
*                                                                               
BUY005   DS    0H                                                               
         MVC   SAVEK,PBUYREC        SAVE CLIENT CODE                            
*                                                                               
         EDIT  (R8),STARCNT                                                     
         MVC   STARMED,PBUYKMED                                                 
         MVC   STARCLT,PBUYKCLT                                                 
         MVC   STARPRD,PBUYKPRD                                                 
         GOTO1 =V(DATCON),DMCB,(3,PBUYKDAT),(10,STARDAT)                        
         EDIT  PBUYKEST,STAREST                                                 
*                                                                               
*                                                                               
***  ACCUMULATE                                                                 
         GOTO1 GETINS,DMCB,REC,PVALUES,REC+7,(C'X',0)                           
         L     RE,GROSS                                                         
         CVD   RE,DUB                                                           
         AP    BUYGRS,DUB                                                       
         AP    TBUYGRS,DUB                                                      
         EDIT  (P9,BUYGRS),STARGRS,2,FLOAT=-                                    
         L     RE,AGYCOM                                                        
         CVD   RE,DUB                                                           
         AP    BUYAGY,DUB                                                       
         AP    TBUYAGY,DUB                                                      
         EDIT  (P9,BUYAGY),STARAGC,2,FLOAT=-                                    
         L     RE,CSHDSC                                                        
         CVD   RE,DUB                                                           
         AP    BUYDSC,DUB                                                       
         AP    TBUYDSC,DUB                                                      
         EDIT  (P9,BUYDSC),STARCDS,2,FLOAT=-                                    
*                                                                               
         GOTO1 GETINS,DMCB,REC,PVALUES,REC+7,(C'A',0),,=C'ALL'                  
*                                                                               
         L     RE,GROSS                                                         
         CVD   RE,DUB                                                           
         AP    BUYGRS,DUB                                                       
         EDIT  (P9,BUYGRS),STARSGR,2,FLOAT=-                                    
         L     RE,AGYCOM                                                        
         CVD   RE,DUB                                                           
         AP    BUYAGY,DUB                                                       
         EDIT  (P9,BUYAGY),STARSAG,2,FLOAT=-                                    
         L     RE,CSHDSC                                                        
         CVD   RE,DUB                                                           
         AP    BUYDSC,DUB                                                       
         EDIT  (P9,BUYDSC),STARSCD,2,FLOAT=-                                    
*                                                                               
*                                                                               
         B     SEQ                                                              
         DROP  R7                                                               
*                                                                               
*                                                                               
*                                                                               
****************************************************************                
*                                                                               
EOF      DS    0H                                                               
*                                                                               
         BAS   RE,PRNT                                                          
         LA    R3,COUNTS                                                        
         LA    R4,25                                                            
         LA    R5,COUNTSX                                                       
*                                                                               
EOF2     MVC   P+1(20),5(R3)                                                    
         OI    4(R3),X'0F'                                                      
         UNPK  P+22(7),0(5,R3)                                                  
         BAS   RE,PRNT                                                          
         BXLE  R3,R4,EOF2                                                       
*                                                                               
         BAS   RE,PRNT                                                          
         LA    R3,BUYDOLS                                                       
         LA    R4,37                                                            
         LA    R5,BUYDOLSX                                                      
*                                                                               
EOF4     MVC   P+1(25),12(R3)                                                   
         EDIT  (P12,0(R3)),(20,P+30),2,COMMAS=YES,CR=YES                        
         BAS   RE,PRNT                                                          
         BXLE  R3,R4,EOF4                                                       
*                                                                               
*                                                                               
*                                                                               
EOJ      DS    0H                                                               
         B     PRTCLOSE                                                         
EXIT     DS    0H                                                               
         XBASE                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SKIP     MVC   PCOM,=C'BC01'                                                    
         ZAP   LNCNT,=P'0'                                                      
         B     PRNTR                                                            
*                                                                               
*                                                                               
PRNT     MVC   PCOM,=C'BL01'                                                    
         AP    LNCNT,=P'1'                                                      
*                                                                               
PRNTR    NTR1                                                                   
*                                                                               
         GOTO1 =V(PRINT),DMCB,P,PCOM                                            
         MVI   P,C' '                                                           
         MVC   P+1(132),P                                                       
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CARDS    NTR1                                                                   
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         LA    R2,4(R2)                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
LOADER   ICM   RF,15,GETINS                                                     
         BNZR  RE                                                               
         NTR1  ,                                                                
         MVC   WORK,=CL8'T00AABA'                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 VLOADER,DMCB,WORK,0                                              
         MVC   GETINS,4(R1)                                                     
         J     XIT                                                              
         LTORG                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4D4E4B'     40-4F                    
         DC    X'504B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DON'T WORRY ABOUT THE FIELDS ON THIS PAGE                                     
*                                                                               
UTL      DC    F'0',X'14'    FOR OFFLINE DATAMGR                                
*                                                                               
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         EJECT                                                                  
*                                                                               
DMWORK   DS    12D                                                              
DMCB     DC    6F'0'                                                            
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
UPSI     DS    XL1                                                              
DMDM     DS    CL6                                                              
VLOADER  DC    V(LOADER)                                                        
GETINS   DC    A(0)                                                             
         DS    0F                                                               
WORK     DS    CL256                                                            
KEY      DS    CL25                                                             
RC1      DS    CL31                                                             
AGYNAME  DS    CL2                                                              
KLEN     EQU   25                                                               
ELEN     EQU   23                                                               
PCOM     DS    CL4                                                              
LNCNT    DC    PL2'99'                                                          
DMPSW    DC    C'N'                                                             
DMPCNT   DC    PL5'100'                                                         
LASTIN   DC    XL50'00'                                                         
LASTOUT  DC    XL50'00'                                                         
LASTAGM  DS    CL3                                                              
SAVEK    DS    XL25        SAVED PUB                                            
DIRFLAG  DS    CL1                                                              
X        DS    CL100                                                            
BSPARS   DS    6F                                                               
CARD     DS    CL80                                                             
LKPARS   DS    6F                                                               
ADPARS   DS    6F                                                               
COMPARS  DS    6F                                                               
ESTPARS  DS    6F                                                               
TODAY    DS    CL8                                                              
PRTSW    DS    CL1                                                              
         DS    0D                                                               
MYDUB    DS    PL8                                                              
NEWCODE  DS    CL6    "REPLACEMENT" CLT/PRD CODE FOR NEW RECORD                 
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
DELCNT   DC    PL5'0',CL20'DELETE COUNT'                                        
CLTCNT   DC    PL5'0',CL20'CLIENT COUNT'                                        
BUYCNT   DC    PL5'0',CL20'BUYS COUNT'                                          
*                                                                               
* REST OF COUNTERS SHOULD ALL BE ZERO IN THIS COPYPRT RUN                       
*                                                                               
* OTHER COUNTERS ADDED HERE WILL AUTOMATICALLY PRINT AT EOJ                     
*                                                                               
COUNTSX  EQU   *-1                                                              
*                                                                               
BUYDOLS  DS    0C                                                               
*                                                                               
TBUYGRS  DC    PL12'0',CL25'GROSS ORDERED'                                      
TBUYAGY  DC    PL12'0',CL25'AGENCY COMMISSION'                                  
TBUYDSC  DC    PL12'0',CL25'CASH DISCOUNT'                                      
***************************************************************                 
****************************************************************                
* OTHER ACCUMULATORS ADDED HERE WILL AUTOMATICALLY PRINT AT EOJ                 
*                                                                               
BUYDOLSX EQU   *-1                                                              
*                                                                               
BUYGRS   DC    PL9'0'                                                           
BUYAGY   DC    PL9'0'                                                           
BUYDSC   DC    PL9'0'                                                           
P        DC    CL133' '                                                         
*                                                                               
***************************************************************                 
***************************************************************                 
***************************************************************                 
***************************************************************                 
***************************************************************                 
***************************************************************                 
*                                                                               
*                                                                               
*          DATA SET PVALUES    AT LEVEL 005 AS OF 06/30/86                      
*                        *** OUTPUT PARAMETER BLOCK FOR GETINS ****             
PVALUES  DS    0F                                                               
*                                                                               
* ORDERED DATA                                                                  
*                                                                               
GROSS    DS    F                   GROSS ORDERED                                
AGYCOM   DS    F                   AGENCY COMMISSION                            
CSHDSC   DS    F                   CASH DISCOUNT                                
PYABLE   DS    F                   GROSS-AGYCOMM-CASHDSC                        
BLABLE   DS    F                   GROSS-CASH DSC                               
PREMIUM  DS    F                   (INCLUDED IN ABOVE FIELDS)                   
UNITS    DS    F                   NUMBER OF LINES BOUGHT                       
*                                                                               
***** NOTE ORDERED TAX UNDER PAID DATA                                          
* PAID DATA                                                                     
*                                                                               
PGROSS   DS    F                   GROSS PAID                                   
PAGYCOM  DS    F                   AGY COMM PAID                                
PCSHDSC  DS    F                   CASH DISCOUNT PAID                           
PAID     DS    F                   ACTUAL PAID AMOUNT                           
*                                                                               
TAX      DS    F                   ORDERED TAX - WAS PAYABLE DATE               
*                          (INCLUDED IN ORDERED GROSS,PYABLE,BLABLE)            
*                                  NET X PBDTAX (4 DECIMALS)                    
*                                                                               
* BILLED DATA                                                                   
*                                                                               
BGROSS   DS    F                   GROSS BILLED                                 
BAGYCOM  DS    F                   AGY COMM BILLED                              
BCSCHDSC DS    F                   CASH DISCOUNT BILLED                         
BILLED   DS    F                   ACTUAL BILLED AMOUNT                         
BLBLDT   DS    CL3                 BILLABLE DATE -YMD                           
*                                                                               
PVALUESX DS    0C                                                               
*                                                                               
*                                                                               
         DS    F                                                                
REC      DS    4000C                                                            
         DS    D                                                                
*                                                                               
**********RINT OFF                                                              
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
*                                                                               
         PRINT ON                                                               
*                                                                               
STARD    DSECT                                                                  
STARALL  DS    0CL78                                                            
         DS    C                                                                
STARCNT  DS    CL5                                                              
         DS    C                                                                
STARMED  DS    CL1                                                              
         DS    C                                                                
STARCLT  DS    CL3                                                              
         DS    C                                                                
STARPRD  DS    CL3                                                              
         DS    CL2                                                              
STARDAT  DS    CL8                                                              
         DS    CL2                                                              
STAREST  DS    CL3                                                              
         DS    C                                                                
STARGRS  DS    CL13                                                             
         DS    C                                                                
STARAGC  DS    CL13                                                             
         DS    C                                                                
STARCDS  DS    CL13                                                             
         DS    C                                                                
STARSGR  DS    CL13                                                             
         DS    C                                                                
STARSAG  DS    CL13                                                             
         DS    C                                                                
STARSCD  DS    CL13                                                             
STARLENQ EQU   *-STARD                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'097PPDEL61REC04/02/02'                                      
         END                                                                    
