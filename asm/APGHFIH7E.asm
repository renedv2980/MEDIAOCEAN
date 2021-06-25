*          DATA SET APGHFIH7E  AT LEVEL 006 AS OF 02/26/04                      
*PHASE ACHFH7EA                                                                 
         TITLE 'MINDSHARE CLIENT FILTER GROUPINGS'                              
ACHFH7E  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(,R1)                                                        
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         USING R1RECD,R7                                                        
         L     R2,AMONACC                                                       
         USING ACMD,R2                                                          
         MVC   ACLIST,ACMVALST     A(ACLIST) -> PAN BOOK ACACCLIST              
                                                                                
*---------------------------------------------------------------------*         
*        FILTERS COLUMNS BY '2D' CONTRA LISTS                         *         
*---------------------------------------------------------------------*         
HOOKSORT DS    0H                                                               
         CLI   ERRFLAG,C'Y'                                                     
         BE    XITNO                                                            
         CLI   HOOKNUM,1                                                        
         BNE   XIT                                                              
         AHI   R7,18                                                            
         L     RF,=A(HOOKIO)                                                    
         ST    RF,AHOOKIO                                                       
*                                                                               
         CLI   QOPT7,C'D'                                                       
         BNE   HK010                                                            
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AHI   R0,18                                                            
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1B'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         AHI   R7,18                                                            
         DC    H'00'                                                            
*                                                                               
HK010    DS    0H                                                               
         LA    R4,DEPTAB                                                        
HK020    CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    HK140                                                            
         CLC   0(5,R4),QSELECT                                                  
         BE    *+12                                                             
         LA    R4,DEPTLNQ(R4)                                                   
         B     HK020                                                            
*                                                                               
         CLC   CURRCON+1(2),=C'2D'                                              
         BNE   HK100                                                            
         LA    R5,CURRCON+3                                                     
         USING LSTRECD,R3                                                       
         L     R3,AHOOKIO                                                       
         CLI   HOOK1ST,C'Y'                                                     
         BE    HK030                                                            
         B     HK090                                                            
*                                                                               
HK030    MVC   SAVELST,5(R4)                                                    
         CLC   SAVELST,=C'MEDIA'   MEDIA IS EXCLUDE                             
         BNE   HK060                                                            
*                                                                               
         MVC   LSTKEY,SPACES                                                    
         MVI   LSTKTYP,LSTKTYPQ                                                 
         MVC   LSTKCPY,RCCOMPFL                                                 
         MVC   LSTKLST,SAVELST                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD  ',=C'ACCOUNT',AHOOKIO,AHOOKIO            
*        L     R6,AHOOKIO                                                       
*        GOTO1 PRNTBL,DMCB,(6,=C'DATMGR'),(R6),C'DUMP',42,=C'2D',               
*              (C'P',PRINT)                                                     
         TM    8(R1),X'10'                                                      
         BO    XIT                                                              
         MVI   HOOK1ST,C'N'                                                     
*                                                                               
         USING LITELD,R6                                                        
         LA    R6,LSTKEY                                                        
         AH    R6,DATADISP                                                      
*                                                                               
HK040    CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),LITELQ        X'1E' ELEMENT                                
         BE    HK050                                                            
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     HK040                                                            
*                                                                               
HK050    MVI   LITUSE,LITUEXCL     MEDIA IS EXLUDE LIST                         
         B     HK090                                                            
*                                                                               
HK060    MVC   LSTKEY,SPACES                                                    
         MVI   LSTKTYP,LSTKTYPQ                                                 
         MVC   LSTKCPY,RCCOMPFL                                                 
         MVC   LSTKLST,SAVELST                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD  ',=C'ACCOUNT',AHOOKIO,AHOOKIO            
         TM    8(R1),X'10'                                                      
         BO    HK130                                                            
         MVI   HOOK1ST,C'N'                                                     
*                                                                               
         USING LITELD,R6                                                        
         LA    R6,LSTKEY                                                        
         AH    R6,DATADISP                                                      
*                                                                               
HK070    CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),LITELQ        X'1E' ELEMENT                                
         BE    HK080                                                            
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     HK070                                                            
*                                                                               
HK080    MVI   LITUSE,LITUINCL     REST ARE INCLUDE LISTS                       
*                                                                               
HK090    GOTO1 ACLIST,DMCB,(0,AHOOKIO),(0,(R5))                                 
         CLI   0(R1),C'I'          INCLUDE?                                     
         BE    XIT                                                              
         B     HK130                                                            
*                                                                               
HK100    DS    0H                    INCOME DEPARTMENT?                         
         CLC   QSELECT(5),=C'MEDIA'   MEDIA                                     
         BNE   HK110                                                            
         CLC   CURRCON+3(3),=C'FMM'    ALL CLIENTS BUT FMM AND MPL              
         BE    HK130                                                            
         CLC   CURRCON+3(3),=C'MPL'                                             
         BE    HK130                                                            
         CLC   CURROFC,=C'MO'          ALSO EXCLUDE OFFICE MO                   
         BE    HK130                                                            
         B     XIT                                                              
*                                                                               
HK110    CLC   QSELECT(3),=C'MPL'     MPL                                       
         BNE   HK120                                                            
         CLC   CURRCON+3(3),=C'FMM'    ONLY CLIENTS FMM AND MPL                 
         BE    XIT                                                              
         CLC   CURRCON+3(3),=C'MPL'                                             
         BE    XIT                                                              
         B     HK130                                                            
*                                                                               
HK120    CLC   QSELECT(5),=C'MMEDI'   MMEDI                                     
         BNE   HK130                                                            
         CLC   CURROFC,=C'MO'          ONLY WANT OFFICE MO                      
         BE    XIT                                                              
*                                                                               
HK130    ZAP   R1COL1,=P'0'                                                     
         ZAP   R1COL2,=P'0'                                                     
         ZAP   R1COL3,=P'0'                                                     
         ZAP   R1COL4,=P'0'                                                     
         B     XIT                                                              
*                                                                               
         USING BIGPRNTD,R3                                                      
HK140    L     R3,VBIGPRNT                                                      
         MVC   XP(28),=C'INCORRECT DEPARTMENT ENTERED'                          
         GOTO1 ACREPORT                                                         
         MVI   ERRFLAG,C'Y'                                                     
         B     XITNO                                                            
*                                                                               
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        DEFINE CONSTANTS                                                       
*----------------------------------------------------------------*              
AHOOKIO  DS    A                                                                
ACLIST   DS    A                                                                
SAVELST  DS    CL5                                                              
HOOK1ST  DC    C'Y'                                                             
ERRFLAG  DC    C'N'                                                             
LSTNAME  DC    CL36' '                                                          
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------*              
*        TABLE                                                                  
*----------------------------------------------------------------*              
DEPTAB   DS    0C                                                               
         DC    CL5'MEDIA',CL5'MEDIA'                                            
DEPTLNQ  EQU   *-DEPTAB                                                         
         DC    CL5'CORP ',CL5'DEMED'                                            
         DC    CL5'WW   ',CL5'INTER'                                            
         DC    CL5'PTM  ',CL5'MSPTM'                                            
         DC    CL5'MPL  ',CL5'FMMPL'                                            
         DC    CL5'PTW  ',CL5'WWPTW'                                            
         DC    CL5'PTR  ',CL5'MSPTR'                                            
         DC    CL5'MMEDI',CL5'MMEDI'                                            
         DC    CL5'MCORP',CL5'MCORP'                                            
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DC    C'**IOS**'                                                       
         DS    0F                                                               
HOOKIO   DS    CL2000                                                           
         EJECT                                                                  
*----------------------------------------------------------------*              
*        WORKING STORAGE                                                        
*----------------------------------------------------------------*              
R1RECD   DSECT                                                                  
R1ROW1   DS    XL2                                                              
R1CDE1   DS    XL14                                                             
R1ROW2   DS    XL2                                                              
R1CDE2   DS    XL14                                                             
R1ROW3   DS    XL2                                                              
R1CDE3   DS    XL14                                                             
R1REPNO  DS    XL1                                                              
R1REPCP  DS    XL1                                                              
R1TYPE   DS    XL2                                                              
*                                                                               
R1NME1   DS    CL36                                                             
R1NME2   DS    CL36                                                             
R1NME3   DS    CL36                                                             
*                                                                               
R1COL1   DS    PL8                                                              
R1COL2   DS    PL8                                                              
R1COL3   DS    PL8                                                              
R1COL4   DS    PL8                                                              
R1LEN    EQU   *-R1RECD                                                         
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------*              
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006APGHFIH7E 02/26/04'                                      
         END                                                                    
