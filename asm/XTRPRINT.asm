*          DATA SET XTRPRINT   AT LEVEL 091 AS OF 07/31/01                      
*PHASE XTRPRINA                                                                 
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
*                                                                               
*        X'20'   BUYS                                                           
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
XTRPRINT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,XTRPRIN,=V(REGSAVE)                                            
         SPACE 2                                                                
         LA    R6,XTRPRINT+4095                                                 
         LA    R6,1(R6)                                                         
         USING XTRPRINT+4096,R6    R6 AS 2ND BASE REGISTER                      
*                                                                               
         BAS   RE,PRNT                                                          
         GOTO1 =V(DATCON),DMCB,(5,0),(8,TODAY)                                  
*                                                                               
         OPEN  (IN,(INPUT))                                                     
*                                                                               
*                                                                               
START1   DS    0H                                                               
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
         BNE   START3B                                                          
         MVI   PRTSW,C'Y'                                                       
         B     START1                                                           
*                                                                               
START3   DS    0H                                                               
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
GET      DS    0H                                                               
*                                                                               
         BAS   RE,GETREC                                                        
         CLI   DIRFLAG,X'FF'                                                    
         BE    GET                                                              
*                                                                               
GET10    DS    0H                                                               
*                                                                               
         CLI   REC,C'Z'            SPECIAL DDS RECORD                           
         BE    EOF                 PAST ALL REAL DATA                           
**********************************************************************          
**********************************************************************          
**********************************************************************          
**********************************************************************          
**********************************************************************          
**********************************************************************          
*                                                                               
* ADD CHECK FOR AGENCY BEING COPIED HERE                                        
*                                                                               
         CLC   REC(2),=C'H9'                                                    
         BH    EOF                 CAN STOP READING                             
*                                                                               
         CLC   REC(3),=C'H9I'    - INTERACTIVE                                  
         BE    GET20                                                            
         CLC   REC(3),=C'H9M'    - MAGAZINES                                    
         BE    GET20                                                            
         CLC   REC(3),=C'H9N'    - NEWSPAPERS                                   
         BE    GET20                                                            
         CLC   REC(3),=C'H9O'    - OUTDOOR                                      
         BE    GET20                                                            
         CLC   REC(3),=C'H9S'    - SUPPLEMENT                                   
         BE    GET20                                                            
         CLC   REC(3),=C'H9T'    - TRADE                                        
         BE    GET20                                                            
         B     GET                                                              
*                                                                               
GET20    DS    0H                                                               
*                                                                               
         CLI   REC+3,X'20'         BUYS                                         
         BNE   GET                                                              
*                                                                               
*        CLC   REC+21(3),=X'000000'                                             
*        BE    BUY                                                              
*                                                                               
*        B     GET                SKIP OTHER RECORD TYPES                       
         B     BUY                                                              
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
         BRAS  RE,LOADER                                                        
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
         OC    SAVEDAT,SAVEDAT         CHECK IF SAVEDAT IS CLEAR                
         BZ    BUY005                  MEANS FIRST RUN                          
*                                                                               
         CLC   SAVEDAT,PBUYKDAT        COMPARE IF SAME CLIENT CODE              
         BE    BUY005                                                           
*                                                                               
         BAS   RE,PRNT                 IF NOT PRINT IT                          
*                                                                               
         ZAP   BUYGRS,=P'0'                                                     
         ZAP   BUYAGY,=P'0'                                                     
         ZAP   BUYDSC,=P'0'                                                     
         AP    CLTCNT,=P'1'                                                     
*                                                                               
BUY005   DS    0H                                                               
         MVC   SAVEDAT,PBUYKDAT        SAVE CLIENT CODE                         
*                                                                               
         MVC   STARMED,PBUYKMED                                                 
         MVC   STARCLT,PBUYKCLT                                                 
         MVC   STARPRD,PBUYKPRD                                                 
         GOTO1 =V(DATCON),DMCB,(3,PBUYKDAT),(23,STARDAT)                        
         EDIT  PBUYKEST,STAREST                                                 
*                                                                               
*                                                                               
***  ACCUMULATE                                                                 
         GOTO1 GETINS,DMCB,REC,PVALUES,REC+7,(C'X',0)                           
         L     RE,GROSS                                                         
         CVD   RE,DUB                                                           
         AP    BUYGRS,DUB                                                       
         AP    TBUYGRS,DUB                                                      
         EDIT  (P9,BUYGRS),STARGRS,2                                            
         L     RE,AGYCOM                                                        
         CVD   RE,DUB                                                           
         AP    BUYAGY,DUB                                                       
         AP    TBUYAGY,DUB                                                      
         EDIT  (P9,BUYAGY),STARAGC,2                                            
         L     RE,CSHDSC                                                        
         CVD   RE,DUB                                                           
         AP    BUYDSC,DUB                                                       
         AP    TBUYDSC,DUB                                                      
         EDIT  (P9,BUYDSC),STARCDS,2                                            
*                                                                               
         GOTO1 GETINS,DMCB,REC,PVALUES,REC+7,(C'A',0),,=C'ALL'                  
*                                                                               
         L     RE,GROSS                                                         
         CVD   RE,DUB                                                           
         AP    BUYGRS,DUB                                                       
         EDIT  (P9,BUYGRS),STARSGR,2                                            
         L     RE,AGYCOM                                                        
         CVD   RE,DUB                                                           
         AP    BUYAGY,DUB                                                       
         EDIT  (P9,BUYAGY),STARSAG,2                                            
         L     RE,CSHDSC                                                        
         CVD   RE,DUB                                                           
         AP    BUYDSC,DUB                                                       
         EDIT  (P9,BUYDSC),STARSCD,2                                            
*                                                                               
*                                                                               
         B     GET                                                              
         DROP  R7                                                               
*                                                                               
*                                                                               
*                                                                               
****************************************************************                
*                                                                               
*                                                                               
EOF      CLOSE (IN,)                                                            
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
         LA    R4,34                                                            
         LA    R5,BUYDOLSX                                                      
*                                                                               
EOF4     MVC   P+1(25),9(R3)                                                    
         EDIT  (P9,0(R3)),(17,P+30),2,COMMAS=YES,CR=YES                         
         BAS   RE,PRNT                                                          
         BXLE  R3,R4,EOF4                                                       
*                                                                               
*                                                                               
*                                                                               
EOJ      DS    0H                                                               
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETREC   NTR1                                                                   
         GET   IN,REC-4                                                         
         MVI   DIRFLAG,X'00'                                                    
*                                                                               
         TM    REC+3,X'30'                                                      
         BO    GETDIR                                                           
         CLC   REC+25(2),=X'80FF'   SEE IF DIRECTORY ONLY (DELETED)             
         BE    GETDIR                                                           
*        CLC   REC+25(2),=X'00FF'   SEE IF DIRECTORY ONLY                       
*        BE    GETDIR                                                           
*        CLC   REC+25(1),=X'FF'                                                 
*        BE    GETDIR                                                           
         B     GETRDO                                                           
*                                                                               
GETDIR   DS    0H               FOR DIRECTORY ONLY RECS                         
*                               NO NEED FOR END OF REC ZERO                     
         MVI   DIRFLAG,X'FF'                                                    
         B     XIT                                                              
*                                                                               
GETRDO   DS    0H                                                               
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0             EOR                                          
         B     GETRX                                                            
*                                                                               
GETRX    AP    INCNT,=P'1'                                                      
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
         EJECT                                                                  
*                                                                               
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
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
KLEN     EQU   25                                                               
ELEN     EQU   23                                                               
PCOM     DS    CL4                                                              
LNCNT    DC    PL2'99'                                                          
DMPSW    DC    C'N'                                                             
DMPCNT   DC    PL5'100'                                                         
LASTIN   DC    XL50'00'                                                         
LASTOUT  DC    XL50'00'                                                         
LASTAGM  DS    CL3                                                              
SAVEDAT  DS    XL3         SAVED PUB                                            
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
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
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
TBUYGRS  DC    PL9'0',CL25'GROSS ORDERED'                                       
TBUYAGY  DC    PL9'0',CL25'AGENCY COMMISSION'                                   
TBUYDSC  DC    PL9'0',CL25'CASH DISCOUNT'                                       
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
         ORG   REC                                                              
       ++INCLUDE PBILLREC                                                       
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PCONREC                                                        
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PESTREC                                                        
*                                                                               
         PRINT ON                                                               
*                                                                               
STARD    DSECT                                                                  
STARALL  DS    0CL72                                                            
         DS    C                                                                
STARMED  DS    CL1                                                              
         DS    C                                                                
STARCLT  DS    CL3                                                              
         DS    C                                                                
STARPRD  DS    CL3                                                              
         DS    C                                                                
STARDAT  DS    CL10                                                             
         DS    C                                                                
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
**PAN#1  DC    CL21'091XTRPRINT  07/31/01'                                      
         END                                                                    
