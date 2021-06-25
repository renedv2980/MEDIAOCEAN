*          DATA SET ACREPXF02  AT LEVEL 021 AS OF 08/16/00                      
*PHASE ACXF02A,+0                                                               
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'MAKE TRANSACTION SUBREF UPPER CASE'                             
         PRINT NOGEN                                                            
ACXF02   CSECT                                                                  
         NMOD1 0,**ACXF**,RR=R7                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXFD,RC                                                         
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
XIT      XIT1                                                                   
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
REQF     MVI   FORCEHED,C'Y'                                                    
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCRESET,C'Y'                                                     
         MVC   PAGE,=H'1'                                                       
         ZAP   CNTCHG,=P'0'                                                     
         ZAP   CNTDUP,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTIONS                                                *         
***********************************************************************         
         USING TRNRECD,R2                                                       
PTRN     L     R2,ADTRANS                                                       
         SH    R2,DATADISP                                                      
         CLI   TRNKCPY,X'B4'       TBS ONLY                                     
         BNE   XIT                                                              
         MVC   WRKREF,TRNKREF      GET SUBREF                                   
         OC    WRKREF,=CL6' '      MAKE IT UPPER CASE                           
         CLC   WRKREF,TRNKREF      IS IT THE SAME?                              
         BE    XIT                 YES, LEAVE                                   
*                                                                               
         AP    CNTCHG,=P'1'        BUMP UP CHG COUNTER                          
* TEST                                                                          
*        SR    R8,R8                                                            
*        ICM   R8,3,TRNRLEN                                                     
*        GOTO1 PRNTBL,DMCB,=C'GET',(R2),C'DUMP',(R8),=C'1D'                     
*        B     XIT                                                              
*                                                                               
         L     RE,AIO              CLEAR IO AREA                                
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,TRNRLEN                                                     
         L     R0,AIO              COPY RECORD IN IOAREA                        
         LR    RE,R2                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AIO                                                           
         MVC   TRNKREF,WRKREF      REPLACE SUBREFERENCE                         
         MVC   AKEY,0(R2)                                                       
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,AKEY,AKEY                             
         CLC   AKEY,0(R2)                                                       
         BNE   PT050                                                            
         AP    CNTDUP,=P'1'                                                     
         GOTO1 PRNTBL,DMCB,=C'DUP',AKEY,C'DUMP',L'AKEY,=C'1D'                   
         B     XIT                                                              
*                                                                               
PT050    CLI   RCWRITE,C'N'                                                     
         B     PT100                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,ACCFIL,(R2),(R2),DMWORK                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PT100    DS    0H                                                               
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
                                                                                
PRNT     NTR1  ,                                                                
         LA    R5,P                                                             
         USING PLD,R5                                                           
         L     R3,BIO                                                           
         USING CHDRECD,R3                                                       
         MVC   PLREC,RECORD        RECORD                                       
         MVC   PLACT,ACTION        ACTION                                       
         MVC   PLACC,CHDKULA       ACCOUNT                                      
         MVC   PLOFF,CHDKOFF       OFFICE                                       
         MVC   PLCON,CHDKULC       CONTRA                                       
         MVC   PLOLD,OLDNAM        OLD NAME                                     
         MVC   PLNEW,CONNAM        NEW NAME                                     
         GOTO1 ACREPORT                                                         
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* REQUEST LAST                                                        *         
***********************************************************************         
REQL     GOTO1 ACREPORT                                                         
         MVC   P+1(20),=CL20'NUMBER CHANGED:'                                   
         EDIT  CNTCHG,(9,P+20)                                                  
         GOTO1 ACREPORT                                                         
         MVC   P+1(20),=CL20'NUMBER DUPLICATE:'                                 
         EDIT  CNTDUP,(9,P+20)                                                  
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DUMP ROUTINES                                                       *         
***********************************************************************         
                                                                                
DMPR     NTR1  ,                                                                
         CLI   QOPT4,C'Y'          OPTION TO DUMP SOME RECORDS                  
         BNE   XIT                                                              
         CP    DMPCNT,DMPMAX                                                    
         BNL   XIT                                                              
         CLI   DMPTRN,C'N'         HAS TRANSACTION BEEN DUMPED                  
         BNE   DMPR3                                                            
         MVI   DMPTRN,C'Y'         SET TRANSACTION HAS BEEN DUMPED              
*                                                                               
         SR    R5,R5                                                            
         L     R3,AIO                                                           
         ICM   R5,3,ACCRLEN-ACCRECD(R3)                                         
         GOTO1 HEXOUT,DMCB,ADA,TRNSACT+15,4,0                                   
         LA    R0,L'TRNSACT                                                     
         GOTO1 PRNTBL,DMCB,((R0),TRNSACT),(R3),C'DUMP',(R5),=C'2D'              
*                                                                               
DMPR3    AP    DMPCNT,=P'1'                                                     
         SR    R5,R5                                                            
         L     R3,BIO                                                           
         ICM   R5,3,ACCRLEN-ACCRECD(R3)                                         
         LA    R0,L'ACTION                                                      
         GOTO1 PRNTBL,DMCB,((R0),ACTION),(R3),C'DUMP',(R5),=C'2D'               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
                                                                                
HELLO    DC    V(HELLO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
AIO      DC    A(IOA)                                                           
BIO      DC    A(IOB)                                                           
CIO      DC    A(IOC)                                                           
*                                                                               
DMPCNT   DC    PL2'0'                                                           
DMPMAX   DC    PL2'200'                                                         
*                                                                               
TRNSACT  DC    C'TRANSACTION DA=00000000'                                       
*                                                                               
ACCFIL   DC    CL8'ACCFIL'                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
*                                                                               
RECCNT   DS    0CL20                                                            
CHARCD   DC    PL4'0',CL16'RECORDS CHANGED'                                     
ADDRCD   DC    PL4'0',CL16'RECORDS ADDED'                                       
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* IO AREAS                                                            *         
***********************************************************************         
                                                                                
IOA      DS    XL2000                                                           
IOB      DS    XL2000                                                           
IOC      DS    XL2000                                                           
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
                                                                                
ACXFD    DSECT                                                                  
AKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
ADIR     DS    CL(ACCKDA-ACCKEY)   TRANSACTION DIRECTORY RECORD                 
ADA      DS    XL(L'ACCKDA)        DISK ADDRESS                                 
*                                                                               
BKEY     DS    CL(L'ACCKEY)                                                     
BDIR     DS    CL(ACCKDA-ACCKEY)                                                
BDA      DS    XL(L'ACCKDA)                                                     
*                                                                               
CKEY     DS    CL(L'ACCKEY)                                                     
CDIR     DS    CL(ACCKDA-ACCKEY)                                                
CDA      DS    XL(L'ACCKDA)                                                     
*                                                                               
OFFICE   DS    CL2                 OFFICE CODE                                  
CONNAM   DS    CL36                CONTRA NAME                                  
CONNAML  DS    XL1                 LENGTH OF NAME                               
OLDNAM   DS    CL36                OLD NAME                                     
*                                                                               
STAT4    DS    XL1                 COMPANY STATUS BYTE 4                        
DMPTRN   DS    CL1                 TRANSACTION DUMP SWITCH                      
*                                                                               
RECORD   DS    CL6                                                              
ACTION   DS    CL6                                                              
*                                                                               
ELEMENT  DS    XL255                                                            
*                                                                               
WRKREF   DS    CL6                                                              
CNTCHG   DS    PL4                                                              
CNTDUP   DS    PL4                                                              
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
                                                                                
PLD      DSECT                                                                  
         DS    XL1                                                              
PLREC    DS    CL6                 RECORD                                       
         DS    CL1                                                              
PLACT    DS    CL6                 ACTION                                       
         DS    CL1                                                              
PLACC    DS    CL14                ACCOUNT CODE                                 
         DS    CL3                                                              
PLOFF    DS    CL2                 OFFICE                                       
         DS    CL3                                                              
PLCON    DS    CL14                CONTRA ACCOUNT                               
         DS    CL1                                                              
PLOLD    DS    CL36                OLD NAME                                     
         DS    CL1                                                              
PLNEW    DS    CL36                NEW NAME                                     
         ORG   PLD+L'P                                                          
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021ACREPXF02 08/16/00'                                      
         END                                                                    
