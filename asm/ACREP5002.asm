*          DATA SET ACREP5002  AT LEVEL 002 AS OF 08/16/00                      
*PHASE AC5002A                                                                  
AC5002   TITLE '- VOID CHECK REPORT'                                            
AC5002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC50**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING WORKD,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    VC2                                                              
         CLI   MODE,REQFRST                                                     
         BE    VC4                                                              
         CLI   MODE,PROCACC                                                     
         BE    VC10                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    VC14                                                             
         CLI   MODE,ACCLAST                                                     
         BE    VC40                                                             
         CLI   MODE,REQLAST                                                     
         BE    VC12                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* FIRST FOR RUN - INITIALISE BOX                                      *         
***********************************************************************         
         SPACE 1                                                                
VC2      L     R1,ADBXAREA                                                      
         USING BOXD,R1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXCOLS+(LINBXL-LIND),C'L'                                       
         MVI   BOXCOLS+(LINBX1-LIND),C'C'                                       
         MVI   BOXCOLS+(LINBX2-LIND),C'C'                                       
         MVI   BOXCOLS+(LINBX3-LIND),C'C'                                       
         MVI   BOXCOLS+(LINBXR-LIND),C'R'                                       
         MVI   BOXROWS+06,C'T'                                                  
         MVI   BOXROWS+09,C'M'                                                  
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         B     EXIT                                                             
         DROP  R1                                                               
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR REQUEST - EXTRACT REQUEST START & END DATES               *         
***********************************************************************         
         SPACE 1                                                                
VC4      MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   REQTOT,=P'0'                                                     
         XC    SDATE,SDATE                                                      
         MVI   EDATE,X'FF'                                                      
         CLC   QSTART,SPACES                                                    
         BE    VC6                                                              
         GOTO1 DATCON,DMCB,(0,QSTART),(2,SDATE)                                 
VC6      CLC   QEND,SPACES                                                      
         BE    EXIT                                                             
         GOTO1 DATCON,DMCB,(0,QEND),(2,EDATE)                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR ACCOUNT - INITIALISE CHEQUE TABLE                         *         
***********************************************************************         
         SPACE 1                                                                
VC10     MVI   CHQTAB,CHQTEOTQ                                                  
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* LAST FOR REQUEST - PRINT TOTAL LINE                                 *         
***********************************************************************         
         SPACE 1                                                                
VC12     GOTO1 ACREPORT                                                         
         MVC   LINDAT(7),=C'*TOTAL*'                                            
         CURED REQTOT,(L'LINAMT,LINAMT),2,COMMAS=YES,MINUS=YES                  
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS A TRANSACTION                                               *         
***********************************************************************         
         SPACE 1                                                                
VC14     L     R3,ADTRANS                                                       
         USING TRNELD,R3           R3=A(TRANSACTION ELEMENT)                    
         LA    R2,TRNELD                                                        
         SH    R2,DATADISP                                                      
         USING TRNRECD,R2          R2=A(TRANSACTION KEY)                        
         CLI   TRNEL,TRNELQ        TEST TRANSACTION RECORD                      
         BNE   EXIT                                                             
         TM    TRNSTAT,TRNSDR      TEST DEBIT                                   
         BZ    EXIT                                                             
         CLI   TRNTYPE,37          TEST BANK/VOID TRANSACTION                   
         BNE   EXIT                                                             
*                                                                               
         LA    R1,TRNELD                                                        
         SR    R0,R0                                                            
         USING TRSELD,R1                                                        
VC16     IC    R0,TRSLN                                                         
         AR    R1,R0                                                            
         CLI   TRSEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   TRSEL,TRSELQ                                                     
         BNE   VC16                                                             
         MVC   WORK(1),TRSMARK                                                  
         NI    WORK,X'FF'-(TRSMUMQ)                                             
         CLI   WORK,TRSMSBVQ       TEST MARKER/VOID                             
         BNE   EXIT                                                             
         MVC   STATUS,TRSMARK                                                   
*                                                                               
VC22     LA    R1,TRNELD                                                        
         SR    R0,R0                                                            
         USING MPYELD,R1                                                        
VC24     IC    R0,MPYLN            LOCATE CHEQUE ELEMENT                        
         AR    R1,R0                                                            
         CLI   MPYEL,0             TEST END OF RECORD                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   MPYEL,MPYELQ        TEST CHEQUE ELEMENT                          
         BNE   VC24                                                             
         CLC   MPYNO,SPACES        TEST CHEQUE NUMBER SET                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MPYDTE,SDATE                                                     
         BL    EXIT                                                             
         CLC   MPYDTE,EDATE                                                     
         BH    EXIT                                                             
*                                                                               
         LA    RE,CHQTAB           LOCATE ENTRY IN CHEQUE TABLE                 
         USING CHQTABD,RE                                                       
         LA    R0,CHQTMAXN                                                      
VC26     CLI   CHQTABD,CHQTEOTQ    TEST END OF TABLE                            
         BE    VC30                                                             
         CLC   CHQTNUM,MPYNO       TEST CHEQUE NUMBER, DATE & AMOUNT            
         BNE   *+10                                                             
         CLC   CHQTDTE,MPYDTE                                                   
         BNE   *+10                                                             
         CP    CHQTAMT,MPYAMNT                                                  
         BE    VC28                                                             
         LA    RE,CHQTABL(RE)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,VC26                                                          
         DC    H'0'                VOID CHEQUE TABLE FULL                       
*                                                                               
VC28     CLC   CHQTCAC,TRNKCULC    IGNORE DEBIT IF NOT SAME KEY                 
         BNE   EXIT                                                             
         CLC   CHQTDAT,TRNKDATE                                                 
         BNE   EXIT                                                             
         CLC   CHQTREF,TRNKREF                                                  
         BNE   EXIT                                                             
         MVC   CHQTSTAT,STATUS     UPDATE STATUS (FOR UNVOID ETC.)              
         B     EXIT                                                             
*                                                                               
VC30     MVC   CHQTNUM,MPYNO       ADD A NEW TABLE ENTRY                        
         MVC   CHQTDTE,MPYDTE                                                   
         MVC   CHQTAMT,MPYAMNT                                                  
         MVC   CHQTSTAT,STATUS                                                  
         MVC   CHQTCAC,TRNKCULC                                                 
         MVC   CHQTDAT,TRNKDATE                                                 
         MVC   CHQTREF,TRNKREF                                                  
         MVI   CHQTABD+CHQTABL,CHQTEOTQ                                         
         B     EXIT                                                             
         DROP  R1,R2,R3,RE                                                      
         EJECT                                                                  
***********************************************************************         
* LAST FOR ACCOUNT                                                    *         
***********************************************************************         
         SPACE 1                                                                
VC40     LA    R2,CHQTAB           POINT TO CHEQUE TABLE                        
         USING CHQTABD,R2                                                       
*                                                                               
VC42     CLI   CHQTABD,CHQTEOTQ    TEST END OF TABLE                            
         BE    EXIT                                                             
         TM    CHQTSTAT,TRSMUMQ    TEST UNVOIDED                                
         BNZ   VC44                                                             
         L     R1,ADACC            FORMAT PRINT LINE                            
         MVC   LINACC(L'ACTKACT),ACTKACT-ACTRECD(R1)                            
         L     R1,ADACCNAM                                                      
         USING NAMELD,R1                                                        
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   LINNAM(0),NAMEREC                                                
         MVC   LINNUM,CHQTNUM                                                   
         GOTO1 DATCON,DMCB,(2,CHQTDTE),(17,LINDAT)                              
         CURED CHQTAMT,(L'LINAMT,LINAMT),2,COMMAS=Y,MINUS=Y                     
         GOTO1 ACREPORT                                                         
         AP    REQTOT,CHQTAMT                                                   
*                                                                               
VC44     LA    R2,CHQTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     VC42                                                             
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
CHQTAB   DS    (CHQTMAXN)XL(CHQTABL)                                            
         SPACE 1                                                                
CHQTABD  DSECT                     ** CHEQUE TABLE **                           
CHQTEOTQ EQU   0                   END OF TABLE INDICATOR                       
CHQTNUM  DS    CL(L'MPYNO)         CHEQUE NUMBER                                
CHQTDTE  DS    XL(L'MPYDTE)        CHEQUE DATE                                  
CHQTAMT  DS    PL6                 CHEQUE AMOUNT                                
CHQTSTAT DS    XL(L'TRSMARK)       STATUS                                       
CHQTCAC  DS    XL(L'TRNKCULC)      CONTRA-ACCOUNT                               
CHQTDAT  DS    PL(L'TRNKDATE)      TRANSACTION DATE                             
CHQTREF  DS    CL(L'TRNKREF)       TRANSACTION REFERENCE                        
CHQTABL  EQU   *-CHQTABD           LENGTH OF TABLE ENTRY                        
CHQTMAXN EQU   512                 MAXIMUM NUMBER OF TABLE ENTRIES              
         SPACE 1                                                                
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
SDATE    DS    XL2                 REQUEST START DATE                           
EDATE    DS    XL2                 REQUEST END DATE                             
REQTOT   DS    PL8                 REQUEST TOTAL                                
STATUS   DS    XL1                 STATUS                                       
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         ORG   P                                                                
LIND     DS    0CL(L'P)            ** PRINT LINE LAYOUT **                      
LINBXL   DS    CL1                                                              
LINACC   DS    CL16                ACCOUNT CODE AND NAME                        
         DS    CL1                                                              
LINNAM   DS    CL36                ACCOUNT NAME                                 
         DS    CL3                                                              
LINBX1   DS    CL1                                                              
LINNUM   DS    CL(L'MPYNO)         CHEQUE NUMBER                                
LINBX2   DS    CL1                                                              
         DS    CL1                                                              
LINDAT   DS    CL8                 CHEQUE DATE                                  
LINBX3   DS    CL1                                                              
LINAMT   DS    CL13                CHEQUE AMOUNT                                
LINBXR   DS    CL1                                                              
         ORG                                                                    
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
         SPACE 1                                                                
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREP5002 08/16/00'                                      
         END                                                                    
