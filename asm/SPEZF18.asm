*          DATA SET SPEZF18    AT LEVEL 003 AS OF 08/11/00                      
*          DATA SET SPEZF14    AT LEVEL 045 AS OF 03/11/96                      
*PHASE T23018A                                                                  
*INCLUDE KHDUMMY                                                                
***********************************************************************         
*                                                                     *         
*  TITLE: T23018 - EASI BILLING FILE PURGE                            *         
*  COMMENTS: THIS PROGRAM READS THE BILLLING FILE, THEN WRITES IT OUT *         
*            WITHOUT RECORDS PAST THE INPUT DATE                      *         
*                                                                     *         
* IT USES JCL EZBPUR, USES THE STATION COUNTS SCREEN, ONLY DATE NEEDS *         
* TO BE FILLED IN, AND WILL DELETE ALL BILL RECS BEFORE THAT DATE     *         
* PRINTS COUNT OF RECS ON ORIG FILE, AND RECS ON NEW FILE             *         
*                                                                     *         
*  OUTPUTS: NOW REPORT OR SCREEN LIST                                 *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO INVOICE RECORD                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER, EZBLOCK         *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPEZF00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2 -                                                  *         
*             AIO3 -                                                  *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*  LEV  2    MAR12/96 INITIAL VERSION                                 *         
*                                                                     *         
***********************************************************************         
         TITLE 'T23018 - PURGE BILLING FILE'                                    
T23018   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T23018**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    RC,SVRC                                                          
         ST    R2,RELO                                                          
         MVC   AIO,AIO1                                                         
         MVI   IOOPT,C'Y'          USER DOING ALL I/O                           
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    INVAL                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    INVAL                                                            
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    INVAL                                                            
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LIST                                                             
         CLI   MODE,RECADD         ADD RECORD                                   
         BE    INVAL                                                            
         CLI   MODE,RECDEL         DEL RECORD                                   
         BE    INVAL                                                            
         SPACE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*   VKEY - VALIDATE KEY                                               *         
***********************************************************************         
         SPACE                                                                  
VKEY     CLI   ACTNUM,11                                                        
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   INVAL                                                            
         SPACE                                                                  
         CLI   OFFLINE,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         LA    R2,LINBDTH          BATCH DATE                                   
         SPACE                                                                  
         XC    RQDTE,RQDTE                                                      
         CLI   5(R2),0             IF NO DATE                                   
         BE    MISSERR                                                          
         SPACE                                                                  
         GOTO1 DATVAL,DMCB,(0,FHDRLEN(R2)),WORK                                 
         SPACE                                                                  
         ICM   R3,15,DMCB                                                       
         BZ    BADATE                                                           
         SPACE                                                                  
         GOTO1 DATCON,(R1),(0,WORK),(0,RQDTE)                                   
         SPACE                                                                  
         CLM   R3,1,5(R2)          ONLY 1 DATE ENTERED                          
         BNE   BADATE               YES                                         
         B     EXIT                                                             
         EJECT                                                                  
* OPEN BILL FILE, READ AND PURGE OLD RECS, WRITTING TO WORK FILE *              
         SPACE                                                                  
LIST     DS    0H                                                               
         OPEN  (EZBILLI,(INPUT))                                                
         LTR   RF,RF                                                            
         BZ    LS240                                                            
         MVC   P(19),=C'EZBILL OPEN FAILED'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DC    H'0'                                                             
         SPACE                                                                  
LS240    OPEN  (EZWRKO,(OUTPUT))                                                
         LTR   RF,RF                                                            
         BZ    LS260                                                            
         MVC   P(19),=C'EZWRK OPEN FAILED'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DC    H'0'                                                             
         SPACE                                                                  
LS260    XC    RECINCT,RECINCT                                                  
         XC    RECOTCT,RECOTCT                                                  
         SPACE                                                                  
LS300    DS    0H                                                               
         L     R1,=A(EZBILLI)                                                   
         GET   (1),EZBILL                                                       
         SPACE                                                                  
         L     RF,RECINCT                                                       
         LA    RF,1(,RF)                                                        
         ST    RF,RECINCT                                                       
         SPACE                                                                  
         CLC   RQDTE,EZBTODAY-EZBILLD+EZBILL                                    
         BH    LS300                                                            
         SPACE                                                                  
LS340    L     R1,=A(EZWRKO)                                                    
         PUT   (1),EZBILL                                                       
         SPACE                                                                  
         L     RF,RECOTCT                                                       
         LA    RF,1(,RF)                                                        
         ST    RF,RECOTCT                                                       
         SPACE                                                                  
         B     LS300                                                            
         SPACE                                                                  
LS400    L     R2,=A(EZBILLI)                                                   
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         L     R2,=A(EZWRKO)                                                    
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         SPACE                                                                  
         OPEN  (EZBILLO,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    LS440                                                            
         MVC   P(19),=C'EZBILL OPEN FAILED'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DC    H'0'                                                             
         SPACE                                                                  
LS440    OPEN  (EZWRKI,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    LS460                                                            
         MVC   P(19),=C'EZWRK OPEN FAILED'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DC    H'0'                                                             
         SPACE                                                                  
LS460    SR    R4,R4                                                            
         SPACE                                                                  
LS500    DS    0H                                                               
         L     R1,=A(EZWRKI)                                                    
         GET   (1),EZBILL                                                       
         SPACE                                                                  
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         L     R1,=A(EZBILLO)                                                   
         PUT   (1),EZBILL                                                       
         SPACE                                                                  
         B     LS500                                                            
         SPACE                                                                  
LS600    C     R4,RECOTCT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,=A(EZBILLO)                                                   
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         L     R2,=A(EZWRKI)                                                    
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         SPACE                                                                  
         EDIT  RECINCT,(9,P+2),0,COMMAS=YES                                     
         MVC   P+12(7),=C'RECS RD'                                              
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         EDIT  RECOTCT,(9,P+2),0,COMMAS=YES                                     
         MVC   P+12(7),=C'RECS WT'                                              
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         B     EXIT                                                             
         SPACE                                                                  
BADATE   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
INVAL    MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
NUMLINS  EQU   16                                                               
         DS    0D                                                               
EZBILLI  DCB   DDNAME=EZBILL,                                          C        
               DSORG=PS,                                               C        
               EODAD=LS400,                                            C        
               LRECL=512,                                              C        
               RECFM=FB,                                               C        
               MACRF=GM                                                         
         DS    0D                                                               
EZBILLO  DCB   DDNAME=BGBILL,                                          C        
               DSORG=PS,                                               C        
               LRECL=512,                                              C        
               RECFM=FB,                                               C        
               MACRF=PM                                                         
         SPACE                                                                  
         DS    0D                                                               
EZWRKO   DCB   DDNAME=EZWORK,                                          C        
               DSORG=PS,                                               C        
               LRECL=512,                                              C        
               RECFM=FB,                                               C        
               MACRF=PM                                                         
         DS    0D                                                               
EZWRKI   DCB   DDNAME=EZWORK,                                          C        
               DSORG=PS,                                               C        
               EODAD=LS600,                                            C        
               LRECL=512,                                              C        
               RECFM=FB,                                               C        
               MACRF=GM                                                         
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPEZFFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
* SPEZFE4D                                                                      
       ++INCLUDE SPEZFE4D                                                       
         PRINT OFF                                                              
         EJECT                                                                  
* DDGENTWA                                                                      
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* DMWRKRD                                                                       
       ++INCLUDE DMWRKRD                                                        
         EJECT                                                                  
*SPEZFWORKD                                                                     
       ++INCLUDE SPEZFWORKD                                                     
         PRINT ON                                                               
* DSECT FOR THIS PROGRAM *                                                      
         SPACE                                                                  
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
DUB2     DS    D                                                                
RELO     DS    A                                                                
SVRC     DS    A                                                                
RECINCT  DS    F                                                                
RECOTCT  DS    F                                                                
         SPACE                                                                  
RQSTA    DS    CL5                 REQUESTED STATION                            
RQDTE    DS    CL6                           BATCH DATE                         
         SPACE                                                                  
EZBILL   DS    CL512                                                            
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL4                                                              
PSTATN   DS    CL8                                                              
         DS    CL3                                                              
PSRCE    DS    CL4                                                              
         DS    CL3                                                              
PINVS    DS    CL8                 INVOICES - TOTAL                             
         DS    CL2                                                              
PINVC    DS    CL8                 CONVERTED                                    
         DS    CL2                                                              
PINVO    DS    CL8                 OVERRIDES                                    
         DS    CL2                                                              
PINVU    DS    CL8                 UNCONVERTED                                  
         DS    CL2                                                              
PINVD    DS    CL8                 DELETED                                      
         DS    CL2                                                              
PSPTS    DS    CL8                                                              
         DS    CL4                                                              
PNDOL    DS    CL16                                                             
         DS    CL4                                                              
PGDOL    DS    CL16                                                             
         DS    CL5                                                              
PBAT     DS    CL6                                                              
         SPACE 3                                                                
EZBILLD  DSECT                                                                  
EZBAGY   DS    CL8                                                              
EZBSRC   DS    CL4                                                              
EZBMEDIA DS    CL1                                                              
EZBCALL  DS    CL5                                                              
EZBNET   DS    CL4                                                              
EZBMOS   DS    CL4                                                              
EZBTODAY DS    CL6                                                              
EZBADVNM DS    CL25                                                             
EZBPRDNM DS    CL25                                                             
EZBINVNO DS    CL10                                                             
EZBNSPTS DS    CL5                                                              
EZBGDOL  DS    CL11                                                             
EZBNDOL  DS    CL11                                                             
EZBAGYNM DS    CL30                                                             
EZBAGYAD DS    CL120                                                            
EZBSTADR DS    CL150                                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPEZF18   08/11/00'                                      
         END                                                                    
