*          DATA SET ACREPX802  AT LEVEL 008 AS OF 08/16/00                      
*PHASE ACX802A                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
***********************************************************************         
*              DO NOT DELETE THIS BOOK!!!!                            *         
***********************************************************************         
         TITLE 'PERSON RECORD FILE FIX'                                         
ACX802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACX8**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACX8D,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         L     RF,GETOPT                                                        
         MVC   0(2,RF),=X'07FE'                                                 
*                                                                               
         LA    RE,VTYPES           RELOCATE VTYPES                              
         LA    R0,ADCONS                                                        
         LA    RF,VTYPLNQ                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDACC,C'Y'                                                     
         SPACE 1                                                                
         USING PERRECD,R4                                                       
         L     R4,AIO1                                                          
         MVC   SVKEY,SPACES                                                     
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,X'0F'                                                    
         MVC   PERKCPY,QCOMPANY                                                 
         MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   SVKEY,0(R4)                                                      
         B     *+10                                                             
*                                                                               
REQF100  MVC   COMMAND,=CL8'DMRSEQ'                                             
         L     R4,AIO1                                                          
         GOTO1 DATAMGR,DMCB,COMMAND,=CL8'ACCFIL',(R4),(R4)                      
         CLC   SVKEY(2),PERKEY                                                  
         BNE   REQF200                                                          
         BAS   RE,DMPGET                                                        
*                                                                               
         OI    ACSTATUS-ACKEYD(R4),X'80'                                        
         BAS   RE,DMPPUT                                                        
         CLI   RCWRITE,C'N'                                                     
         BE    REQF100                                                          
         L     R4,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=CL8'DMWRT',=CL8'ACCFIL',(R4),(R4)                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                UNABLE TO WRITE RECORD BACK                  
         B     REQF100                                                          
*                                                                               
         USING PHIRECD,R4                                                       
REQF200  L     R4,AIO1                                                          
         MVC   SVKEY,SPACES                                                     
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,X'3E'                                                    
         MVI   PHIKSUB,X'05'                                                    
         MVC   PHIKCPY,QCOMPANY                                                 
         MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   SVKEY,0(R4)                                                      
         B     *+10                                                             
*                                                                               
REQF210  MVC   COMMAND,=CL8'DMRSEQ'                                             
         L     R4,AIO1                                                          
         GOTO1 DATAMGR,DMCB,COMMAND,=CL8'ACCFIL',(R4),(R4)                      
         CLC   SVKEY(3),PHIKEY                                                  
         BNE   REQFX                                                            
         BAS   RE,DMPGET                                                        
*                                                                               
         OI    ACSTATUS-ACKEYD(R4),X'80'                                        
         BAS   RE,DMPPUT                                                        
         CLI   RCWRITE,C'N'                                                     
         BE    REQF210                                                          
         L     R4,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=CL8'DMWRT',=CL8'ACCFIL',(R4),(R4)                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                UNABLE TO WRITE RECORD BACK                  
         B     REQF210                                                          
REQFX    B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        DUMP OUT RECORDS                                             *         
*---------------------------------------------------------------------*         
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R3,=C'GET'                                                       
         B     DUMP                                                             
                                                                                
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R3,=C'PUT'                                                       
                                                                                
         USING PERRECD,R4                                                       
DUMP     L     R4,AIO1                                                          
         CLI   QOPT7,C'D'                                                       
         BNE   XIT                                                              
         SR    R2,R2                                                            
         ICM   R2,3,PERRLEN                                                     
         GOTO1 PRNTBL,DMCB,(3,(R3)),(R4),C'DUMP',(R2),=C'2D'                    
XIT      XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
******************************************************************              
* EXTERNAL ADDRESS LIST                                          *              
******************************************************************              
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(IO1)                                                           
         DC    A(IO2)                                                           
         DC    V(HELLO)                                                         
         DC    V(PRNTBL)                                                        
         SPACE 3                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'9000'                                                        
                                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* IO AREAS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**IO1***'                                                      
IO1      DS    CL2000                                                           
*                                                                               
         DS    0D                                                               
         DC    C'**IO2***'                                                      
IO2      DS    CL2000                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* 1R TABLE DSECT                                                      *         
***********************************************************************         
         SPACE 1                                                                
SVTABLED DSECT                                                                  
SVPERSON DS    CL8                                                              
SV1RACT  DS    CL12                                                             
SVKEYL   EQU   *-SVTABLED                                                       
*                                                                               
SVSTATUS DS    XL1                                                              
SVSTPROC EQU   X'01'               ALREADY PROCESSED                            
*                                                                               
SVTSLOCK DS    PL3                 TIMESHEET LOCK DATE                          
SVLOCATT DS    XL1                 LOCATION ATTRIBUTES                          
SVLOCEXE EQU   X'80'               EXECUTIVE                                    
SVLOCPRO EQU   X'40'               PRODUCT REQUIRED ON TIMESHEET                
SVLOCJOB EQU   X'20'               JOB REQUIRED ON TIMESHEET                    
SVLOCACT EQU   X'10'               USE ACTUAL AS STANDARD HOURS                 
SVALOCK  EQU   X'08'               ACCOUNT LOCKED                               
SVSTAT   DS    XL1                                                              
SVSACT   EQU   0                   ACTIVE                                       
SVSTRM   EQU   1                   TERMINATED                                   
SVSLOA   EQU   2                   LEAVE OF ABSENCE                             
SVSTAT2  DS    XL1                                                              
SVSDUP   EQU   X'80'               LOCATION HAS BEEN DUPLICATED                 
SVLNQ    EQU   *-SVTABLED                                                       
         EJECT                                                                  
***********************************************************************         
*              PRINT LINE DSECT                                       *         
***********************************************************************         
*                                                                               
PLINED   DSECT                                                                  
PPERSON  DS    CL8                                                              
         DS    CL3                                                              
POFFC    DS    CL2                                                              
         DS    CL3                                                              
PDEPT    DS    CL6                                                              
         DS    CL3                                                              
PSUB     DS    CL6                                                              
         DS    CL3                                                              
PSTART   DS    CL8                                                              
         DS    CL3                                                              
PEND     DS    CL8                                                              
         DS    CL3                                                              
PSTAT    DS    CL10                                                             
         EJECT                                                                  
***********************************************************************         
*              CONVERSION TABLE                                       *         
***********************************************************************         
*                                                                               
ACX8D    DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
AIO1     DS    A                                                                
AIO2     DS    A                                                                
HELLO    DS    A                                                                
PRNTBL   DS    A                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
ANEXT    DS    A                                                                
*                                                                               
COMMAND  DS    CL8                                                              
SVKEY    DS    CL3                                                              
ELEM     DS    XL255                                                            
PERSON   DS    CL8                 PERSON CODE                                  
PER1RACT DS    CL12                1R ACCOUNT CODE                              
HIGHDATE DS    PL3                                                              
*                                                                               
LEVELS   DS    0H                                                               
LEVELA   DS    CL1                 LENGTH OF LEVEL A                            
LEVELB   DS    CL1                 LENGTH OF LEVEL B                            
LEVELC   DS    CL1                 LENGTH OF LEVEL C                            
LEVELD   DS    CL1                 LENGTH OF LEVEL D                            
         EJECT                                                                  
***********************************************************************         
*              OTHER INCLUDES                                         *         
***********************************************************************         
*                                                                               
*  ACREPWORKD                                                                   
*  ACGENBOTH                                                                    
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPX802 08/16/00'                                      
         END                                                                    
