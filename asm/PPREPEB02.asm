*          DATA SET PPREPEB02  AT LEVEL 096 AS OF 06/15/20                      
*PHASE PPEB02A                                                                  
*INCLUDE PPBVAL                                                                 
         TITLE 'PPEB02 - EDI FOR PRINT BILLING'                                 
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-44468  04/15/20 EXCLUDE ZERO AMOUNT INVOICES FOR H7 & OU  *         
***********************************************************************         
PPEB02   CSECT                                                                  
*                                                                               
*   **NOTE**  WHEN TESTING EX (XML) ON TSO,QOPT 4 SHOULD BE T OR N              
*             ELSE MQ RECORD MAY BE WRITEN TO PRODUCTION BUFFER                 
*   CHANGE LOG                                                                  
*                                                                               
*   BPLA  04/15    FOR UAT AGENCIES FOR QOPT2 TO Y (TEST RUN)                   
*                  AND QOPT4 TO N (NO MQ MESSAGE)                               
*                                                                               
*   BPLA  10/13    USE MCVMQPRT (FROM DDMASTC)                                  
*                                                                               
*   BPLA  01/13    FORCE QOPT4 TO N FOR AGENCY O0  (LBCTOA)                     
*                  FOR KRAFT THEIR XML FILE IS A PRTTAPE                        
*                  AND NOT A SFTPDISK                                           
*                                                                               
*   BPLA  04/10    CHGS FOR MISC. FIELD ON INSERTION RECORD                     
*                                                                               
*   BPLA  09/09    USED CLIENT RETURNED BY DDXMLMAP TO ALTER                    
*                  AGENCY ID IN MQ NOTIFICATION                                 
*                                                                               
*   BPLA  07/09    DISPLAY XML FILE NAME                                        
*                                                                               
*   BPLA  02/09    MOVE LOADING OF T00ABF TO RUN FIRST                          
*                                                                               
*   BPLA  06/08    MODIFICATIONS FOR NEW MQ FILE FORMAT                         
*                                                                               
*   BPLA  05/08    NEW VALUE (X) FOR TESTING OPTION - QPOT2                     
*                  X - MEANS DON'T MARK WORKER FILES NOR BILLS                  
*                  BUT STILL CREATE TRANSMISSION FILE                           
*                                                                               
*   BPLA  01/08    ADD MQ NOTIFICATIONS FOR XML (PROGRAM= EX)                   
*                  TRANSMISSIONS                                                
*                                                                               
*   BPLA  11/07    ALTER END OF INDEX CHECKING                                  
*                  - SOMETIMES END OF FILE NOT BEING SENT PROPERLY              
*                                                                               
*   BPLA  12/06    NEW OPTION FOR WORKER FILE TRACE                             
*                                                                               
*   BPLA  9/06     REPORT EX WILL ALSO CALL THIS PROGRAM                        
*                  FOR XML OUTPUT                                               
*                                                                               
*   BPLA  5/04     ENTRY FOR INSERTION DETAILS                                  
*                                                                               
*   BPLA  3/04     ENTRIES FOR CT (CANADIAN TAX) AND                            
*                  TD (TOTAL DUE - WITH TAXES) RECORDS                          
*                                                                               
*   BPLA  10/03    PROVIDE COUNT OF INVOICES AND $ TRANSMITTED                  
*                                                                               
*   BPLA  1/03     FIX CHECK OF CLTMTAB  (CLIENT/MEDIA TABLE)                   
*                                                                               
*   BPLA  1/27/00  FIX FILE READ LOGIC TO ONLY DO CERTAIN THINGS                
*                  WHEN PROCESSIN GTHE FIRST RECORD FO AN INDEX                 
*                 (LARGE NUMBER OF RECORDS CAUSED A PROBLEM)                    
*                                                                               
*   KWAN 10/20/99  CHECK CLIENT'S BILL RECORDS WITH RECORDS BUILT               
*                  WITH BINSRCH, IF RECORD IS FOUND IN BINSRCH TABLE,           
*                  UPDATE BILL RECORD'S X'08' ELEM.  THE FIELD TO BE            
*                  CHANGED IS PEDIDTE AND IT WILL CONTAIN TODAYP.               
*                                                                               
*   BPLA 10/07/99  CHANGE THE WAY THE RERUN OPTION WORKS                        
*                  SAVE THE TRANSMITTAL DATE IN WKRFIL (W_DESC+11)              
*                  UPDATE THE TRANSMITTAL DATA WHEN RERUNNING                   
*                                                                               
**********************************************************************          
*        START AND END INVOIVE NUMBERS COL 53(4), AND COL 56 (4)                
*        QINVNO1   OPTIONAL STARTING INVOICE NUMBER COL 53 (4)                  
*        QINVNO2   OPTIONAL ENDING INVOICE NUMBER COL 57 (4)                    
*                                                                               
*         QOPT1    T=TRACE WORKER AND OUTPUT RECORDS                            
*                  W=TRACE WORKER ONLY                                          
*                  O=TRACE OUTPUT RECORDS ONLY                                  
*         QOPT2    Y=TEST MODE                                                  
*                  X=DON'T MARK WORKER FILES NOR BILLS                          
*                    (STILL CREATE TRANSMISSION FILE)                           
*         QOPT3    R=REDO                                                       
*         QOPT4    Y=MQ FOR XML TO LIVE MQ BROKER (DEFAULT)                     
*                  T=MQ FOR XML TO TEST MQ BROKER                               
*                  N=NO MQ XML RECORD                                           
*        **NOTE**  WHEN TESTING ON TSO,VALUES T OR N SHOULD BE USED             
*                                                                               
**********************************************************************          
*                                                                               
*       NOTE- CODE IS PRESENT TO BUILD A TABLE OF REQUESTS                      
*             AND FILTER THE WORKER FILE READS USING THE MULTI-REQ              
*             TABLE. THIS WOULD BE MORE EFFICIENT AS FAR AS                     
*             THE WORKER FILE IS CONCERNED, BUT CREATES                         
*             OTHER PROBLEMS AND HAS BEEN ABANDOND . THE CODE IS STILL          
*             USED BUT ONLY HANDLES ONE REQUEST AT A TIME. COULD BE             
*             REVIVED.                                                          
*                                                                               
**********************************************************************          
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,PPEB02,CLEAR=YES,RR=R2                                         
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'                                                    
         L     R9,PPWORK2C                                                      
         USING PPWORK2D,R9                                                      
         LA    RC,SPACEND                                                       
         USING WORKD,RC                                                         
*                                                                               
         LA    R6,PPFILEC                                                       
         USING PPFILED,R6                                                       
*                                                                               
         LA    R4,PPEB02+4095                                                   
         LA    R4,1(R4)                                                         
         USING PPEB02+4096,R4     **NOTE USE OF R4 AS BASE REG*                 
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
*        RUN FIRST                                                              
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   LIVERUN,C'N'   WILL BE SET TO "Y" IF LIVE REQUEST                
*                           IS PROCESSED                                        
         MVI   TESTMQ,0     WILL BE SET FROM FIRST MQ REQ                       
*                           QOPT4                                               
*                                                                               
         ZAP   INVCNT,=P'0'  CLEAR TRANSMITTED INVOICE COUNTER                  
         ZAP   INVAMT,=P'0'  CLEAR TRANSMITTED AMOUNT DUE                       
         ZAP   AORAMT,=P'0'  CLEAR TRANSMITTED AOR AMOUNT DUE                   
*                                                                               
         L     RF,=V(PPBVAL)                                                    
         A     RF,RELO                                                          
         ST    RF,VPPBVAL                                                       
*                                                                               
         L     RF,=A(RQTAB)                                                     
         A     RF,RELO                                                          
         ST    RF,ARQTAB                                                        
         L     RF,=A(RQTABX)                                                    
         A     RF,RELO                                                          
         ST    RF,ARQTABX                                                       
*                                                                               
         L     R5,VMASTC                                                        
         USING MASTD,R5                                                         
         MVC   SAVUIDN,MCUSERID                                                 
         MVC   AMQRPT,MCVMQRPT                                                  
*                                                                               
         MVI   UATAGY,C'N'                                                      
         TM    MCAGCOPT,MCAGCUAT                                                
         BNO   *+8                                                              
         MVI   UATAGY,C'Y'                                                      
*                                                                               
         MVC   MCDUB,=CL8'T00AB6'    LOAD MEDIA EDI MAP PROGRAM                 
         GOTO1 MCVLOADM,DMCB,0                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LM    R0,R1,0(R1)                                                      
         ST    R1,EDIMAP           A(EDI MAP ROUTINE)                           
         ST    R1,MCUSRDMP         GET IT INTO THE DUMP                         
         AR    R1,R0                                                            
         ST    R1,MCUSRDMP+4                                                    
*                                                                               
         MVC   MCDUB,=CL8'T00ABF'    LOAD MEDIA XML MAP PROGRAM                 
         GOTO1 MCVLOADM,DMCB,0                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LM    R0,R1,0(R1)                                                      
         ST    R1,XMLMAP           A(EDI MAP ROUTINE)                           
         ST    R1,MCUSRDMP+8       GET IT INTO THE DUMP                         
         AR    R1,R0                                                            
         ST    R1,MCUSRDMP+12                                                   
         DROP  R5                                                               
*                                                                               
* SET TODAY                                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,TODAY)                                 
         GOTO1 DATCON,DMCB,TODAY,(3,TODAYB)                                     
         GOTO1 DATCON,DMCB,TODAY,(2,TODAYP)                                     
*                                                                               
* CLEAR BINSRCH TABLE ENTRIES                                                   
*                                                                               
         L     RE,=A(BRTAB)        GET ADDRESS OF TABLE                         
         A     RE,RELO                                                          
         L     RF,=F'18001'        GET LENGTH OF TABLE                          
         XCEF                                                                   
*                                                                               
* CLEAR WRKRBUFF AND STORE ITS ADDRESS                                          
*                                                                               
         L     RE,=A(WRKRBUFF)     GET ADDRESS OF BUFFER                        
         A     RE,RELO                                                          
         ST    RE,AWRKRBF                                                       
         L     RF,=F'14336'        GET LENGTH OF BUFFER                         
         XCEF                                                                   
*                                                                               
* CLEAR CLIENT/MEDIA CODE TABLE                                                 
*                                                                               
         LA    RE,CLTMTAB          GET ADDRESS OF TABLE                         
         LA    RF,401              GET LENGTH OF TABLE                          
         XCEF                                                                   
*                                                                               
* PREPARE PARAMETERS FOR BINSRCH                                                
*                                                                               
         XC    WKPARMS,WKPARMS                                                  
         L     RF,=A(BRTAB)                                                     
         A     RF,RELO                                                          
         ST    RF,WKPARMS+04       ADDRESS OF TABLE                             
         LA    RF,9                                                             
         ST    RF,WKPARMS+12       LENGTH OF KEY                                
         ST    RF,WKPARMS+16       DIPLACEMENT OF KEY INTO RECORD               
         LA    RF,2000                                                          
         ST    RF,WKPARMS+20       MAXIUM NUMBER OF RECORDS IN TABLE            
*                                                                               
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        REQUEST FIRST                                                          
***********************************************************************         
*                                                                               
REQF     DS    0H                                                               
*                                                                               
         CLI   UATAGY,C'Y'                                                      
         BNE   *+12                                                             
         MVI   QOPT2,C'Y'                SET TO TEST RUN                        
         MVI   QOPT4,C'N'                NO MQ NOTIFICATION                     
*                                                                               
         LA    R3,PPDYNDSN                                                      
*                                                                               
         CLC   QAGENCY,=C'O0'            LBCTOA                                 
         BNE   *+8                                                              
         MVI   QOPT4,C'N'                SET FOR NO MQ NOTIFICATION             
*                                        THEY GET XML AS A PRTTAPE              
*                                                                               
*                                                                               
         MVC   SVPROG,QPROG              SAVE REQUEST TYPE                      
         CLC   QPROG(2),=C'EX'           XML REQUEST?                           
         BNE   REQF1                                                            
*                                                                               
REQF1    MVC   SVQAGY,QAGENCY      SAVE QAGENCY FOR RUNLAST                     
*                                                                               
         CLC   QPROG,=C'EX'        XML TRANSFER?                                
         BNE   REQF1B                                                           
*                                                                               
         CLI   TESTMQ,0            ALREADY SET?                                 
         BNE   REQF1B                                                           
         MVC   TESTMQ,QOPT4                                                     
         CLI   TESTMQ,C' '          NOT ENETERED                                
         BNE   *+8                                                              
         MVI   TESTMQ,C'Y'                                                      
*                                                                               
REQF1B   CLI   QOPT2,C'Y'          IF TEST, NO OUTPUT                           
         BE    REQF1H                                                           
         MVI   LIVERUN,C'Y'                                                     
         CLI   OPENSW,C'Y'         TEST FILE ALREADY OPEN                       
         BE    REQF1H                                                           
*                                                                               
         MVC   13(2,R3),QAGENCY                                                 
*                                                                               
         CLC   QAGENCY,=C'*B'                                                   
         BNE   *+10                                                             
         MVC   13(2,R3),=C'@B'      MUST ALTER TO @B                            
         CLC   QAGENCY,=C'W+'                                                   
         BNE   *+10                                                             
         MVC   13(2,R3),=C'W@'      MUST ALTER TO W@                            
*                                                                               
         MVC   11(2,R3),QPROG       SET PROGRAM TYPE                            
         GOTO1 DYNALLOC,DMCB,(0,=C'EDIOUT  '),(0,0(R3))                         
         LA    R5,EDIOUT                                                        
         OPEN  ((R5),OUTPUT)                                                    
         OPEN  (TEMPEB,OUTPUT)                                                  
         MVI   OPENSW,C'Y'                                                      
         ST    R3,EDINME                                                        
         B     REQF1H                                                           
*                                                                               
PPDYNDSN DC    CL20'PRTTAPE.PP0EBAG'                                            
*                                                                               
REQF1H   DS    0H                                                               
         MVI   FCRDBUY,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
*                                  ADD TO REQUEST TABLE                         
         L     R3,ARQTAB           NOTE- ONE REQUEST AT A TIME FOR NOW          
         USING RQTABD,R3                                                        
         C     R3,ARQTABX                                                       
         BL    *+6                                                              
         DC    H'0'                REQUEST TABLE FULL                           
*                                                                               
         XC    RQTABD(RQTABL),RQTABD                                            
         MVI   RQTSYS,C'P'         PRINTPAK                                     
*                                                                               
REQF03   DS    0H                                                               
         MVC   RQTMED,QMEDIA                                                    
         MVC   RQTCLT,QCLIENT                                                   
*                                  NOTE- PRD/EST NOT USED NOW                   
         MVC   RQTPRD,QPRODUCT                                                  
         CLI   QEST,C'0'                                                        
         BL    REQF04                                                           
         PACK  DUB,QEST(3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,RQTEST                                                        
*                                                                               
REQF04   DS    0H                                                               
         CLI   QSTART,C' '                                                      
         BNH   REQF05                                                           
         GOTO1 DATCON,DMCB,QSTART,(2,RQTSTA)   OLD COMPRSSD (1900)              
         GOTO1 DATCON,DMCB,QSTART,(30,RQTSTAN) NEW COMPRSSD (1964)              
*                                                                               
REQF05   DS    0H                                                               
         MVC   RQTEND,=X'FFFF'                                                  
         CLI   QEND,C' '                                                        
         BNH   REQF06                                                           
         GOTO1 DATCON,DMCB,QEND,(2,RQTEND)                                      
         GOTO1 DATCON,DMCB,QEND,(30,RQTENDN)                                    
*                                                                               
REQF06   DS    0H                                                               
         MVC   RQTINV1,QINVNO1                                                  
         MVC   RQTINV2,QINVNO2                                                  
         CLI   QINVNO1,C' '                                                     
         BH    *+10                                                             
         XC    RQTINV1(4),RQTINV1                                               
         CLI   QINVNO2,C' '                                                     
         BH    *+10                                                             
         MVC   RQTINV2(4),=X'FFFFFFFF'                                          
         MVC   RQTOPTS,QOPT1                                                    
         MVC   RQTRQS,QUESTOR                                                   
         MVI   RQTABD+RQTABL,X'FF'  SET END OF TABLE                            
*                                                                               
         BAS   RE,RWPROC           READ AND PROCESS WORKER FILES                
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
***********************************************************************         
*        RUN LAST                                                               
*                                                                               
*        NOTE- IF CHANGE TO DO MULTI-REQUEST TABLE, CALL RWPROC                 
*              FROM HERE                                                        
*                                                                               
***********************************************************************         
*                                                                               
RUNL     DS    0H                                                               
         LA    RE,CLTMTAB                                                       
RUNL05   CLI   0(RE),0                                                          
         BE    RUNL50              CLT/MED TABLE IS EMPTY                       
*                                                                               
         ST    RE,FULL             SAVE CLT/MED TABLE POINTER                   
         XC    KEY,KEY                                                          
         MVC   KEY+0(2),SVQAGY     AGENCY                                       
         MVC   KEY+2(1),3(RE)      MEDIA                                        
         MVI   KEY+3,X'08'         BILL RECORD CODE                             
         MVC   KEY+4(3),0(RE)      CLIENT                                       
*                                                                               
RUNL10   GOTO1 HIGH                                                             
         B     RUNL20                                                           
*                                                                               
RUNL15   GOTO1 SEQ                                                              
*                                                                               
RUNL20   CLC   KEY(7),KEYSAVE      SAME AG/MED/X08/CLT?                         
         BNE   RUNL45              DO NEXT ENTRY IN CLT/MED TABLE               
*                                                                               
         XC    BRKEY,BRKEY                                                      
         LA    RE,KEY                                                           
         USING PBILLKEY,RE                                                      
         MVC   BRKMED,PBILKMED                                                  
         MVC   BRKCLT,PBILKCLT                                                  
         MVC   BRKINV,PBILKBNO                                                  
         DROP  RE                                                               
*                                                                               
         LA    R2,PBILLREC                                                      
         ST    R2,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         LA    R5,PBILLREC+33                                                   
         CLI   0(R5),X'08'         SEE IF FIRST ELEM IS BILL ELEM               
         BE    *+6                                                              
         DC    H'0'                MUST PRESENT                                 
         USING PBILLEL,R5                                                       
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(1,BRKBDT)                              
*                                                                               
         GOTO1 BINSRCH,WKPARMS,(0,BRKEY)                                        
         TM    0(R1),X'01'                                                      
         BO    RUNL15              RECORD IS NOT IN TABLE                       
*                                                                               
         MVC   PBILEDID,TODAYP                                                  
         GOTO1 PUTPRT                                                           
*                                                                               
*        ACCUMULATE AMOUNT DUE                                                  
*                                                                               
         CLI   PBRETAIL,X'41'      DON'T TOTAL RETAIL SUMMARY BILL              
         BE    RUNL15                                                           
*                                                                               
         GOTO1 VPPBVAL,DMCB,(C'B',PBILLREC),PPBVALD                             
*                                                                               
         LA    R6,AORAMT                                                        
         TM    PBILCMSW,X'20'      AOR BILL?                                    
         BO    *+8                                                              
         LA    R6,INVAMT                                                        
         AP    0(8,R6),PBILLRCV     AMOUNT DUE                                  
         CLI   PAGYNAT,C'C'        CANADIAN?                                    
         BNE   RUNL15                                                           
         L     R0,PPBVGST          INCLUDE GST AN PST                           
         CVD   R0,DUB                                                           
         AP    0(8,R6),DUB                                                      
         L     R0,PPBVPST                                                       
         CVD   R0,DUB                                                           
         AP    0(8,R6),DUB                                                      
         B     RUNL15              DO NEXT RECORD                               
         DROP  R5                                                               
*                                                                               
RUNL45   L     RE,FULL                                                          
         LA    RE,4(RE)            NEXT CLT/MED ENTRY                           
         B     RUNL05                                                           
*                                                                               
*                                                                               
RUNL50   MVI   FORCEHED,C'Y'                                                    
         MVC   P1,SPACES         BE SURE IT'S CLEAR                             
         MVC   P1+1(10),=C'RUN TOTALS'                                          
         MVC   P2+1(10),=C'----------'                                          
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P1+1(35),=C'INVOICES ELIGIBLE TO BE TRANSMITTED'                 
         EDIT  (P8,INVCNT),(13,P1+40),COMMAS=YES,ZERO=NOBLANK                   
         GOTO1 REPORT                                                           
         MVC   P1+1(28),=C'$ ELIGIBLE TO BE TRANSMITTED'                        
         EDIT  (P8,INVAMT),(14,P1+40),2,COMMAS=YES,MINUS=YES                    
         GOTO1 REPORT                                                           
         CP    AORAMT,=P'0'     ANY AOR BILLING?                                
         BE    RUNL60                                                           
         MVC   P1+1(32),=C'AOR $ ELIGIBLE TO BE TRANSMITTED'                    
         EDIT  (P8,AORAMT),(14,P1+40),2,COMMAS=YES,MINUS=YES                    
         GOTO1 REPORT                                                           
*                                                                               
RUNL60   CLI   OPENSW,C'Y'                                                      
         BNE   EXIT                                                             
         CLOSE EDIOUT                                                           
         CLOSE TEMPEB                                                           
*                                                                               
         CLC   SVPROG,=C'EX'     XML PROCESSING?                                
         BE    DOXML                                                            
*                                                                               
         GOTO1 EDIMAP,DMCB,EDINME,VCOMFACS,DYNALLOC,VMASTC,P1,REPORT            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
DOXML    DS    0H                                                               
*                                                                               
*              PASS TESTMQ CONTROL TO DDXMLMAP                                  
*                                                                               
         PRINT GEN                                                              
         GOTO1 XMLMAP,DMCB,(TESTMQ,EDINME),VCOMFACS,DYNALLOC,VMASTC,P1,X        
               REPORT                                                           
*                                                                               
         PRINT NOGEN                                                            
         L     R1,DMCB+4                                                        
         MVC   MYDMCB(12),DMCB    SAVE THESE VALUES                             
         MVC   MYMQFILE,14(R1)    FILE NAME -PAST SFTPDISK.PROD.(TEST)          
         MVC   MYMQDATE,28(R1)    DATE YYMMDD                                   
         MVC   MYMQTIME,36(R1)    TIME HHMMSS                                   
         MVI   FORCEHED,C'N'                                                    
*                                                                               
         CLC   SVQAGY,=C'O0'   LBCTOA - DON'T DISPLAY FILE                      
         BE    DOXML5                                                           
*                                                                               
         MVC   P1+1(49),0(R1)   DISPLAY FILE NAME                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
DOXML5   MVC   DMCB(12),MYDMCB   NEED TO RESTORE                                
*                                                                               
         CLI   TESTMQ,C'N'      DON'T DO MQ RECORD                              
         BE    EXIT                                                             
         CLI   TESTMQ,C' '      OR SPACE                                        
         BE    EXIT                                                             
*                                                                               
* NOTE - XMLMAP WILL RESET FIRST BYTE OF DMCB TO Y IF IT                        
*               PROCESS A BILL                                                  
*                                                                               
         CLI   DMCB,C'Y'                                                        
         BNE   EXIT                                                             
         MVC   EDICLT,DMCB+1     SHOULD BE RETURNED INTERNAL CLIENT             
*                                                                               
* SEND MQ MESSAGE WITH FILE NAME                                                
         LA    R5,ELEM                                                          
         USING MQMSGD,R5                                                        
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(MQMSGLNQ-1),ELEM                                          
         MVC   MQFILE(34),MYMQFILE                                              
         MVC   MQDATE(6),MYMQDATE                                               
         MVC   MQTIME(6),MYMQTIME                                               
*                                                                               
         BRAS  RE,MQOPEN                                                        
*                                                                               
         MVC   MQHID,=CL6'DANOT1'                                               
         MVC   MQSYS,MQFILE+4             SYSTEM (+4 PAST BIL.)                 
         MVC   MQAGYID,MQFILE+8           AGENCY 1D (4 CHAR)                    
*                                                                               
         CLC   EDICLT,=C'CHV'             PROCESSING CHEVRON?                   
         BNE   DOXML10                                                          
         CLC   MQFILE+8(4),=C'FRNY'       AND ID WAS FRNY                       
         BNE   DOXML10                                                          
         MVC   MQAGYID(4),=C'FRCH'        ALTER TO IDENITFY CHEVRON             
*                                                                               
DOXML10  MVC   MQQUAL(7),=C'BILLING'                                            
         B     DOXML30                                                          
************  CODE BELOW NEVER USED - WAS PROPOSED MQQUAL CHANGE                
************  KRAFT XML FILES NOW PRTTAPES                                      
************                                                                    
         CLC   EDICLT,=C'KFT'             KRAFT?                                
         BNE   DOXML30                                                          
         MVC   MQQUAL(14),=C'KR BILLING GRO'                                    
         CLC   SAVUIDN(06),=C'MVCTOK'     GROCERY ID                            
         BE    DOXML30                                                          
         MVC   MQQUAL(14),=C'KR BILLING SNK'                                    
         CLC   SAVUIDN(06),=C'MVCMON'     SNACKS ID                             
         BE    DOXML30                                                          
         MVC   MQQUAL(14),=C'KR BILLING    '                                    
         BE    DOXML30       UNKNOWN KRAFT ID                                   
*                      EDI HUB WON'T KNOW WHERE TO SEND                         
***OLD   MVC   MQHID,=CL6'SFTP2'                                                
***OLD   MVC   MQQUAL,=CL16'MEDIACOM-WB-BILL'                                   
***OLD   MVC   MQDATA1(06),=C'WB XML'                                           
***OLD   OC    MQDATA1,SPACES                                                   
***      MVC   MQFILE(L'PPDYNDSN),PPDYNDSN                                      
***      MVC   MQFILE+11(2),=C'XM'       ALTER PROG IN FILE NAME TO XM          
*                                        THAT'S THE MAPPED FILE                 
DOXML30  GOTO1 AMQRPT,DMCB,(0,=C'PUT'),ELEM,MQMSGLNQ,0                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         DCHO                                                                   
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        RWPROC- READ WORKER FILES                                              
***********************************************************************         
*                                                                               
RWPROC   NTR1                                                                   
         L     RF,=A(EDIFLDS)                                                   
         A     RF,RELO                                                          
         ST    RF,AFLDTAB                                                       
         L     RF,=A(WRKRBFX)                                                   
         A     RF,RELO                                                          
         ST    RF,AWRKRBFX                                                      
*                                                                               
         XC    WRKRIND,WRKRIND                                                  
         MVC   WRKRIND(2),RCORIGID                                              
         GOTO1 DATAMGR,DMCB,(0,=C'GFILE'),=C'WRKFIL',WRKRIND,WRKRREC,  X        
               AWRKRBF                                                          
         LA    R1,WRKRIND                                                       
         USING UKRECD,R1                                                        
         MVC   WRKFIL,UKUSRINF                                                  
         DROP  R1                                                               
*                                                                               
         XC    WRKRIND,WRKRIND                                                  
*                                                                               
RWP4     DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',WRKFIL,WRKRIND,WRKRREC,          X        
               AWRKRBF                                                          
*                                                                               
         TM    DMCB+8,X'80'       EOF                                           
         BNZ   RWP50                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
                                                                                
         CLC   =X'0000',WRKRIND    ANOTHER END OF FILE CHECK                    
         BE    RWP50                                                            
*                                                                               
         MVI   HAVBUF,C'N'                                                      
         LA    R7,WRKRIND                                                       
         USING UKRECD,R7                                                        
         CLC   UKUSRID,RCORIGID    MAKE SURE RIGHT USER ID                      
         BNE   RWP4                                                             
         CLI   UKDAY,X'97'         97'S ARE BILLING EDI'S                       
         BNE   RWP4                                                             
*                                  GO THRU REQUEST TABLE                        
         L     R3,ARQTAB                                                        
         USING RQTABD,R3                                                        
*                                                                               
RWP5     DS    0H                                                               
         CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    RWP4                PASSED NO REQUEST                            
         CLC   UKCLASS(1),RQTSYS   RIGHT SYSTEM                                 
         BNE   RWP5X                                                            
         CLC   UKSYSPRG(1),RQTMED  MEDIA                                        
         BNE   RWP5X                                                            
         CLC   RQTCLT,=C'ALL'                                                   
         BE    RWP5P                                                            
         MVC   WORK(3),UKSYSPRG+1   CLIENT                                      
         CLI   WORK+2,C'.'                                                      
         BNE   *+8                                                              
         MVI   WORK+2,C' '                                                      
         CLC   WORK(3),RQTCLT                                                   
         BNE   RWP5X                                                            
*                                                                               
RWP5P    TM    UKATTB,WLATNCD      NEW COMPRESSED DATES (BASE 1964)             
         BZ    RWP5PP                                                           
         CLC   UKAGELD,RQTSTAN     DATE VS START DATE (BASE 1964)               
         BL    RWP5X                                                            
         CLC   UKAGELD,RQTENDN     AND END DATE (BASE 1964)                     
         BH    RWP5X                                                            
         B     RWP5T                                                            
*                                                                               
RWP5PP   CLC   UKAGELD,RQTSTA      DATE VS START DATE                           
         BL    RWP5X                                                            
         CLC   UKAGELD,RQTEND      AND END DATE                                 
         BH    RWP5X                                                            
*                                                                               
RWP5T    CLI   RQTOPT3,C'R'        REDO?                                        
         BE    RWP5W                                                            
*                                  NORMAL RUN                                   
         TM    UKSTAT,X'08'        NO, CAN'T BE ALREADY DONE (KEEP)             
         BZ    RWP6           PROCESS                                           
         B     RWP5X          SKIP IF ALREADY DONE                              
*                                                                               
RWP5W    DS    0H             FOR RERUNS                                        
         TM    UKSTAT,X'08'   MUST BE ALREADY DONE                              
         BO    RWP6           PROCESS                                           
*                                                                               
RWP5X    DS    0H                  SKIP                                         
         LA    R3,RQTABL(R3)       ELSE NEXT REQUEST TABLE ENTRY                
         B     RWP5                                                             
*                                                                               
*                                  INDEX PASSES, NOW READ RECORDS               
RWP6     DS    0H                                                               
*                                                                               
         BAS   RE,NOZERO           EXCLUDE $0 AMOUNT DUE INVOICE?               
         BNE   RWP4                YES - SKIP THIS                              
*                                                                               
         MVI   FIRST,C'Y'        FIRST RECORD                                   
         B     RWP6AX                                                           
*                                                                               
RWP6A    DS    0H                                                               
         MVI   FIRST,C'N'                                                       
RWP6AX   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'READ',WRKFIL,WRKRIND,WRKRREC,           X        
               AWRKRBF                                                          
*                                                                               
         TM    DMCB+8,X'80'         EOF                                         
         BNZ   RWP40                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   HAVBUF,C'Y'         HAVE WE DONE BUFFER?                         
         BE    RWP7                YES, READ RCORDS                             
         MVI   HAVBUF,C'Y'                                                      
*                                                                               
         L     R5,AWRKRBF                                                       
         USING W_RECD,R5                                                        
*                                                                               
*              NOTE- W_DESC IS APPARENTLY INITIALIZED TO SPACES,                
*                    NOT NULLS. IT IS USED AS FOLLOWS-                          
*                      INVOICE NUMBER(10),                                      
*                      CONTROL(1) - X'80'=TRANSFERED                            
*                      DATE(2) - TRANSFER DATE                                  
*                                                                               
         CLI   FIRST,C'Y'                                                       
         BNE   RWP6D                                                            
*                                                                               
         MVC   SAVDESC,W_DESC                                                   
         LA    RF,W_DESC+9         PICK UP LAST 4 OF INVNO                      
         CLI   0(RF),C' '          TO DO FILTERING                              
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         AHI   RF,-3                                                            
         CLC   0(4,RF),RQTINV1     INVNO VS START                               
         BL    RWP5X                                                            
         CLC   0(4,RF),RQTINV2     AND END                                      
         BH    RWP5X                                                            
*                                                                               
RWP6D    DS    0H                                                               
*                                                                               
RWP7     DS    0H                                                               
         LA    RF,WRKRREC                                                       
         SR    R1,R1                                                            
         ICM   R1,3,WRKRREC                                                     
         AR    RF,R1                                                            
         ST    RF,EOR                                                           
*                                                                               
         CLI   RQTOPT1,C'T'        TRACE ?                                      
         BNE   *+8                 NO, SKIP WKR REC PRINT                       
         BAS   RE,PRTWKR                                                        
         CLI   RQTOPT1,C'W'        WORKER FILE TRACE ONLY?                      
         BNE   *+8                 NO, SKIP WKR REC PRINT                       
         BAS   RE,PRTWKR                                                        
         BAS   RE,PUTFXR                                                        
*                                                                               
         CLI   FIRST,C'Y'          ONLY NEED TO ONCE                            
         BNE   RWP8A                                                            
*                                                                               
*        FOR TEST RERUNS USE ORIGINAL DATE                                      
*                                                                               
         JIF   RQTOPT2,EQ,C'Y',AND,RQTOPT3,EQ,C'R',RWP8,JUMP=N                  
*                                                                               
         JIF   RQTOPT2,EQ,C'X',AND,RQTOPT3,EQ,C'R',RWP8,JUMP=N                  
*                                                                               
         MVI   W_DESC+10,X'80'     TRANSMITTED                                  
         MVC   W_DESC+11(2),TODAYP SAVE TRANSMITTAL DATE                        
*                                                                               
         CLI   RQTOPT2,C'Y'     SEE IF TEST RUN                                 
         BE    RWP8             SKIP WRITE                                      
*                                                                               
         CLI   RQTOPT2,C'X'     SEE IF TEST RUN                                 
         BE    RWP8             SKIP WRITE                                      
*                                                                               
*        MUST WRITE BACK RECORD                                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'WRITE',WRKFIL,WRKRIND,WRKRREC,          X        
               AWRKRBF                                                          
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RWP8     MVC   SVTDATE,W_DESC+11     MUST SAVE TRANSMITTAL DATE                 
*                                    FOR DISPLAY                                
RWP8A    DS    0H                                                               
         CLI   RQTOPT2,C'Y'        IF TEST MODE                                 
         BE    RWP8B               DON'T NEED TO PUT CLT CODE IN TABLE          
*                                                                               
         CLI   RQTOPT2,C'X'        IF OTHER TEST MODE                           
         BE    RWP8B               DON'T NEED TO PUT CLT CODE IN TABLE          
*                                                                               
         CLI   FIRST,C'Y'          ONE ADD ONCE                                 
         BNE   RWP8B                                                            
*                                                                               
         LA    RE,CLTMTAB                                                       
         SR    RF,RF                                                            
*                                                                               
         MVC   FULL(3),UKSYSPRG+1                                               
         CLI   FULL+2,X'4B'        A PERIOD?                                    
         BNE   *+8                                                              
         MVI   FULL+2,C' '         IT'S REALL A SPACE, NOT PERIOD               
         MVC   FULL+3(1),UKSYSPRG  MEDIA                                        
*                                                                               
RWP8AD   CHI   RF,100                                                           
         BNH   *+6                                                              
         DC    H'0'                CLIENT TABLE IS FULL                         
         CLC   0(4,RE),FULL                                                     
         BE    RWP8AX              ALREADY IN TABLE                             
         CLI   0(RE),0                                                          
         BNE   RWP8AH                                                           
         MVC   0(3,RE),FULL        CLIENT CODE INTO TAB                         
         MVC   3(1,RE),UKSYSPRG    MEDIA CODE INTO TAB                          
         B     RWP8AX                                                           
RWP8AH   LA    RE,4(RE)            NEXT TABLE ENTRY                             
         AHI   RF,1                COUNTING NUMBER OF ENTRIES                   
         B     RWP8AD                                                           
*                                                                               
RWP8AX   DS    0H                                                               
*                                                                               
         CLI   RQTOPT2,C'Y'      SEE IF TEST RUN                                
         BE    RWP8B             YES - DON'T ADD TO BINSRCH TABLE               
*                                                                               
         CLI   RQTOPT2,C'X'      SEE IF OTHER TEST RUN                          
         BE    RWP8B             YES - DON'T ADD TO BINSRCH TABLE               
*                                                                               
         XC    BRKEY,BRKEY                                                      
         MVC   BRKMED,UKSYSPRG                                                  
         MVC   BRKCLT,FULL         CLIENT CODE                                  
*                                                                               
         TM    UKATTB,WLATNCD      NEW COMPRESSED DATES (BASE 1964)             
         BZ    RWP8AXA                                                          
         GOTO1 DATCON,DMCB,(14,UKAGELD),(1,BRKBDT)  COMPRSSD BASE 1964          
         B     RWP8AXAB                                                         
RWP8AXA  GOTO1 DATCON,DMCB,(2,UKAGELD),(1,BRKBDT)   COMPRSSD BASE 1960          
*                                                                               
RWP8AXAB LA    RF,W_DESC+9         PICK UP LAST 4 OF INVNO                      
         CLI   0(RF),C' '          TO DO FILTERING                              
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         AHI   RF,-3                                                            
         PACK  DUB,0(4,RF)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,BRKINV                                                      
*                                                                               
         GOTO1 BINSRCH,WKPARMS,(1,BRKEY)                                        
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
RWP8B    DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
         B     RWP6A                                                            
*                                                                               
*                                                                               
*                                                                               
RWP40    DS    0H                                                               
*                                                                               
         LA    R2,P1                                                            
         USING LIND,R2                                                          
         MVC   LINMED,UKSYSPRG                                                  
         MVC   LINCLT,UKSYSPRG+1                                                
         CLI   LINCLT+2,C'.'                                                    
         BNE   *+8                                                              
         MVI   LINCLT+2,C' '                                                    
         MVC   LININV,SAVDESC                                                   
         MVC   LINIDAT,SAVINVD                                                  
*                                                                               
         TM    UKATTB,WLATNCD      NEW COMPRESSED DATES (BASE 1964)             
         BZ    RWP40A              NO                                           
         GOTO1 DATCON,DMCB,(14,UKAGELD),(5,LINBDAT)                             
         B     RWP40B                                                           
RWP40A   GOTO1 DATCON,DMCB,(2,UKAGELD),(5,LINBDAT)                              
*                                                                               
RWP40B   TM    UKSTAT,X'08'        IF NOT PREVIOUSLY DONE                       
         BO    RWP42                                                            
         MVC   WORK(2),TODAYP      SAY DOING IT NOW                             
         MVI   LINEDAT+8,C'*'      MARK AS TODAY (THIS RUN)                     
         B     RWP43                                                            
*                                                                               
RWP42    MVC   WORK(2),SVTDATE     (TRANSFER DATE)                              
*                                                                               
RWP43    CLC   WORK(2),=X'FFFF'      BE SURE I HAVE A REAL DATE                 
         BE    RWP43C                                                           
         CLC   WORK(2),SPACES        BE SURE I HAVE A REAL DATE                 
         BE    RWP43C                                                           
         OC    WORK(2),WORK          BE SURE I HAVE A REAL DATE                 
         BZ    RWP43C                                                           
         GOTO1 DATCON,DMCB,(2,WORK),(5,LINEDAT)                                 
*                                                                               
RWP43C   MVC   LINADDR,SAVHEAD     EDI ADDRESS, ETC.                            
         BAS   RE,MYPRNT                                                        
         DROP  R2                                                               
*                                                                               
         CLI   RQTOPT2,C'Y'        IF TEST MODE                                 
         BE    RWP44               SKIP KEEP                                    
*                                                                               
         CLI   RQTOPT2,C'X'        IF OTHER TEST MODE                           
         BE    RWP44               SKIP KEEP                                    
*                                                                               
*******  TM    UKSTAT,X'08'        IF ALREADY DONE (KEEP)                       
*******  BO    RWP44               SKIP                                         
*******                                                                         
*******        DATAMGR MAY NOT LET ME KEEP AN ALREADY KEPT FILE                 
*******         - IT DOESN'T SEEM TO CARE                                       
*******                                                                         
         GOTO1 DATAMGR,DMCB,=C'KEEP',WRKFIL,WRKRIND,WRKRREC,           X        
               AWRKRBF                                                          
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RWP44    DS    0H                                                               
         B     RWP4                NEXT INDEX                                   
*                                                                               
RWP50    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R5                                                               
*                                                                               
PRTWKR   NTR1                      PRINT WKR RECORD                             
         MVC   P1,SPACES           CLEAR PRINT LINE                             
         SR    R1,R1                                                            
         ICM   R1,3,WRKRREC                                                     
         AHI   R1,4                                                             
         CHI   R1,65                                                            
         BNH   *+8                                                              
         LA    R1,65                                                            
         BCTR  R1,0                                                             
         EX    R1,WRKMVC                                                        
         BAS   RE,MYPRNT                                                        
         B     EXIT                                                             
*                                                                               
WRKMVC   MVC   P1+2(0),WRKRREC+4                                                
         EJECT                                                                  
***********************************************************************         
*        PUTFXR -  CONVERT TO FIXED RECORD FORMAT                               
***********************************************************************         
*                                                                               
PUTFXR   NTR1                                                                   
*                                    SPECIAL IDENTIFICATION RECORD              
*******  CLI   RQTOPT4,C'X'   NO-OP  SEE IF DOING XML FORMAT                    
*******  BE    PFXML                 XML WORKS THE SAME                         
*                                                                               
         CLI   WRKRREC+4,C'H'        STARTS WITH H                              
         BNE   PF04                                                             
         CLI   WRKRREC+6,FDELIM      AND 3RD CHAR NOT A FLD DELIM               
         BE    PF04                                                             
*                                                                               
         MVC   FIXREC(2),=H'45'    RECORD LENGTH                                
         XC    FIXREC+2(2),FIXREC+2                                             
         MVC   FIXREC+4(41),WRKRREC+4                                           
         MVC   SAVHEAD(41),WRKRREC+4   SAVE EDI ADDRESS, ETC..                  
         CLI   OPENSW,C'Y'                                                      
         BNE   PF03                                                             
         PUT   EDIOUT,FIXREC                                                    
         PUT   TEMPEB,FIXREC                                                    
*                                                                               
PF03     DS    0H                                                               
         CLI   RQTOPT1,C'O'        OUTPUT RECS ONLY?                            
         BE    PF03B                                                            
         CLI   RQTOPT1,C'T'        TRACE?                                       
         BNE   PFX                                                              
PF03B    MVC   P1,SPACES          CLEAR PRINT LINE                              
         MVC   P1+2(41),FIXREC                                                  
         BAS   RE,MYPRNT                                                        
         B     PFX                                                              
*                                                                               
PF04     DS    0H                                                               
         LA    R8,WRKRREC+4        FIRST WORKER REC POSITION                    
         L     R5,AFLDTAB          START OF FIELD TABLE                         
*                                                                               
PF04D    DS    0H                  FIND ENTRY FOR THIS RECORD TYPE              
         CLI   0(R5),X'FF'         EOT                                          
         BNE   *+6                                                              
         DC    H'0'                INVALID RECORD                               
         CLC   0(2,R8),0(R5)       TEST RECORD TYPE                             
         BE    PF05                                                             
         ICM   R5,15,22(R5)        TRY NEXT RECORD TYPE                         
         B     PF04D                                                            
*                                                                               
PF05     DS    0H                                                               
         LA    R5,26(R5)           POINT TO FIRST FIELD DESCRIPTOR              
         LA    R2,FIXREC+4         FIRST POS IN OUTPUT REC                      
*                                                                               
PF05D    DS    0H                                                               
         CLI   0(R5),X'FF'         END OF FIELD DESCRIPTORS                     
         BE    PF20                DONE, IGNORE ANY FIELDS BEYOND TABLE         
*                                                                               
         LR    R1,R8               SAVE START OF WKR FIELD                      
*                                  LOOK FOR FIELD DELIMITER                     
PF06     DS    0H                                                               
         CLI   0(R8),FDELIM                                                     
         BE    PF06D                                                            
*                                                                               
         C     R8,EOR              END OF RECORD                                
         BL    PF06B                                                            
*                                                                               
*        REFERENCE NUMBER MAY BE MISSING FOR SOME ID                            
*        (INSERTION DETAIL RECORDS)                                             
*        MISCELLANEOUS FLD MAY ALSO BE MISSING FOR SOME ID                      
*        (INSERTION DETAIL RECORDS)                                             
*        JUST TREAT AS MISSING - DON'T DIE                                      
*        THIS SHOULD WORK AS THIS IS THE LAST FIELD                             
*        FOR THIS RECORD TYPE                                                   
*                                                                               
         CLC   1(20,R5),=CL20'REFERENCE NUMBER'                                 
         BE    PF06A                                                            
         CLC   1(20,R5),=CL20'MISCELLANEOUS DATA'                               
         BNE   PF06B                                                            
PF06A    MVI   0(R8),C' '          EMPTY FIELD                                  
         LA    R8,1(R8)                                                         
         MVI   0(R8),FDELIM        SET DELIMITER                                
         B     PF06                                                             
*                                                                               
PF06B    C     R8,EOR                                                           
         BL    *+6                                                              
         DC    H'0'                FIELD MISSING                                
*                                                                               
         LA    R8,1(R8)            NEXT POSITION                                
         B     PF06                                                             
*                                                                               
PF06D    DS    0H                                                               
         LR    R3,R8                                                            
         SR    R3,R1                                                            
         BP    PF07                IF FIELD LEN=0                               
         MVI   VLEN,1              TREAT AS 1                                   
         MVI   VTXT,C' '                                                        
         B     PF08                                                             
*                                                                               
PF07     DS    0H                                                               
         STC   R3,VLEN             LENGTH OF VARIABLE FIELD                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   VTXT(0),0(R1)       SAVE TEXT                                    
*                                                                               
PF08     DS    0H                                                               
         ZIC   R3,0(R5)            FIXED LENGTH                                 
         STC   R3,FLEN                                                          
         CLC   VLEN,FLEN                                                        
         BNH   *+6                                                              
         DC    H'0'                FIELD DATA TOO LONG                          
*                                                                               
         BCTR  R3,0                CLEAR OUTPUT RECORD AREA                     
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,(R2)),SPACES                                                 
*                                                                               
         ZIC   R3,VLEN             THEN MOVE VARIABLE TEXT                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,(R2)),VTXT                                                   
*                                                                               
*                                  TEST ANY SPECIAL FIELD HANDLING              
         CLC   WRKRREC+4(2),=C'IH' INV HEADER RECORD                            
         BNE   PF08B                                                            
         CLC   1(12,R5),=C'INVOICE DATE'                                        
         BNE   PF08B                                                            
         MVC   SAVINVD,VTXT        SAVE INVOICE DATE                            
*                                                                               
PF08B    DS    0H                                                               
         ZIC   R3,FLEN             NEXT OUTPUT POSITION                         
         AR    R2,R3                                                            
         LA    R8,1(R8)            BUMP PAST THIS FIELD DELIM                   
         LA    R5,21(R5)           NEXT FIELD DESCRIPTOR                        
         B     PF05D                                                            
*                                                                               
PF20     DS    0H                                                               
         LA    R1,FIXREC                                                        
         SR    R2,R1                                                            
         STCM  R2,3,FIXREC                                                      
         XC    FIXREC+2(2),FIXREC+2                                             
*                                                                               
         CLI   OPENSW,C'Y'                                                      
         BNE   PF22                                                             
         CLC   WRKRREC+4(2),=C'IH' INV HEADER RECORD                            
         BNE   PF20C                                                            
         AP    INVCNT,=P'1'      ADD TO TRANSMITTED COUNT                       
PF20C    DS    0H                                                               
         PUT   EDIOUT,FIXREC                                                    
         PUT   TEMPEB,FIXREC                                                    
*                                                                               
PF22     DS    0H                                                               
         CLI   RQTOPT1,C'O'        OUTPUT RECS ONLY?                            
         BE    PF22B                                                            
         CLI   RQTOPT1,C'T'        TRACE?                                       
         BNE   PFX                 NO, DONE                                     
PF22B    MVC   P1,SPACES          CLEAR PRINT LINE                              
         CHI   R2,132                                                           
         BNH   *+8                                                              
         LHI   R2,132                                                           
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P1+2(0),FIXREC                                                   
         BAS   RE,MYPRNT                                                        
*                                                                               
PFX      DS    0H                                                               
         B     EXIT                                                             
*        EJECT                                                                  
**                                                                              
**                                                                              
**       NO-OP OLD XML FORMATTING ROUTINE                                       
**                                                                              
**XML    DS    0H                                                               
**       CLI   WRKRREC+4,C'H'        STARTS WITH H                              
**       BNE   PFXM04                                                           
**       CLI   WRKRREC+6,FDELIM      AND 3RD CHAR NOT A FLD DELIM               
**       BE    PFXM04                                                           
**                                                                              
**       XC    FIXREC(50),FIXREC                                                
**       MVC   FIXREC(2),=H'45'    RECORD LENGTH                                
**       XC    FIXREC+2(2),FIXREC+2                                             
**       MVC   FIXREC+4(41),WRKRREC+4                                           
**       MVC   SAVHEAD(41),WRKRREC+4   SAVE EDI ADDRESS, ETC..                  
**       CLI   OPENSW,C'Y'                                                      
**       BNE   PFXM03                                                           
**       PUT   EDIOUT,FIXREC                                                    
**       PUT   TEMPEB,FIXREC                                                    
**                                                                              
**XM03   DS    0H                                                               
**       CLI   RQTOPT1,C'T'        TRACE?                                       
**       BNE   PFXMX                                                            
**       MVC   P1+2(41),FIXREC                                                  
**       BAS   RE,MYPRNT                                                        
**       B     PFXMX                                                            
**                                                                              
**XM04   DS    0H                                                               
**       CLI   OPENSW,C'Y'                                                      
**       BNE   PFXM22                                                           
**       CLC   WRKRREC+4(2),=C'IH' INV HEADER RECORD                            
**       BNE   PFXM20C                                                          
**       AP    INVCNT,=P'1'      ADD TO TRANSMITTED COUNT                       
**                                                                              
**       FIND AND SAVE INVOICE DATE IN 3RD FIELD                                
**                                                                              
**       LA    R2,WRKRREC+7      START JUST PAST 1ST SEMICOLON                  
**XM05   CLI   0(R2),C';'        FIND 2ND SEMICOLON                             
**       BE    PFXM06                                                           
**       LA    R2,1(R2)                                                         
**       B     PFXM05                                                           
**                                                                              
**XM06   MVC   SAVINVD,1(R2)    SAVE INVOICE DATE                               
**                                                                              
**XM20C  DS    0H                                                               
**       PUT   EDIOUT,WRKRREC                                                   
**       PUT   TEMPEB,WRKRREC                                                   
**                                                                              
**XM22   DS    0H                                                               
**       CLI   RQTOPT1,C'T'        TRACE?                                       
**       BNE   PFXMX               NO, DONE                                     
**       MVC   HALF,WRKRREC                                                     
**       LH    R2,HALF                                                          
**       CHI   R2,132                                                           
**       BNH   *+8                                                              
**       LHI   R2,132                                                           
**       BCTR  R2,0                                                             
**       EX    R2,*+8                                                           
**       B     *+10                                                             
**       MVC   P1+2(0),WRKRREC                                                  
**       BAS   RE,MYPRNT                                                        
**                                                                              
**XMX    DS    0H                                                               
**       B     EXIT                                                             
                                                                                
**********************************************************************          
*        MYPRNT                                                                 
**********************************************************************          
*                                                                               
MYPRNT   NTR1                                                                   
*                                                                               
MYPRNT4  DS    0H                                                               
         MVC   HEAD3(7),=C'USER ID'                                             
         MVC   HEAD3+10(10),SAVUIDN                                             
*                                                                               
         MVC   HEAD5(14),=C'** LIVE RUN **'                                     
         CLI   RQTOPT2,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+3(4),=C'TEST'                                              
*                                                                               
         CLI   RQTOPT2,C'X'    OTHER TYPE OF TEST RUN?                          
         BNE   *+10                                                             
         MVC   HEAD5+3(4),=C'TEST'                                              
*                                                                               
         CLI   RQTOPT3,C'R'                                                     
         BNE   *+10                                                             
         MVC   HEAD6(11),=C'** RERUN **'                                        
         GOTO1 REPORT                                                           
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
NOZERO   NTR1                                                                   
*                                                                               
         CLC   QAGENCY,=C'OU'       AGENCY OU?                                  
         BE    *+14                 YES                                         
         CLC   QAGENCY,=C'H7'       AGENCY H7?                                  
         BNE   NOZEROEQ             NO, SET CC EQU                              
*                                                                               
NOZERO10 GOTO1 DATAMGR,DMCB,=C'READ',WRKFIL,WRKRIND,WRKRREC,AWRKRBF             
*                                                                               
         TM    DMCB+8,X'80'         EOF                                         
         BNZ   NOZERO20             YES - RESET READ SEQ & CC=EQU               
*                                                                               
         CLI   DMCB+8,0             DATAMGR ERROR?                              
         BE    *+6                  NO                                          
         DC    H'0'                 YES, NO ERRORS TOLERATED                    
*                                                                               
         CLC   =C'AD',WRKRREC+4     AMOUNT DUE RECORD?                          
         BNE   NOZERO10             NO - READ NEXT LINE                         
         LA    R8,WRKRREC           START OF WORKER FILE LINE                   
         XR    R1,R1                CLEAR R1                                    
         ICM   R1,3,WRKRREC         LENGTH OF AD LINE                           
         AR    R8,R1                BUMP 1 BYTE PAST THE WORKER FILE            
         SHI   R8,3                 BACK UP 3 BYTES                             
         CLC   =C';0;',0(R8)        AMOUNT DUE ZERO?                            
         BE    NOZERONE             YES - SET CC NEQ                            
*                                                                               
NOZERO20 LA    R7,WRKRIND           WORKER FILE KEY/RECORD                      
         USING UKRECD,R7            WORKER FILE KEY DSECT                       
         OI    UKFLAG,UKFLDAT       RE-READ FILE NUMBER IN UKFILENO             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',WRKFIL,WRKRIND,WRKRREC,AWRKRBF            
*                                                                               
         CLI   DMCB+8,0             DATAMGR ERROR?                              
         BE    *+6                  NO                                          
         DC    H'0'                 YES, NO ERRORS TOLERATED                    
*                                                                               
         NI    UKFLAG,X'FF'-UKFLDAT TURN THIS FLAG OFF                          
         DROP  R7                   DROP WORKER FILE KEY USING                  
*                                                                               
NOZEROEQ CR    RB,RB                SET CC EQU                                  
         B     NOZEROX              EXIT                                        
*                                                                               
NOZERONE LTR   RB,RB                SET CC NEQ                                  
*                                                                               
NOZEROX  XIT1                       EXIT                                        
*                                                                               
         LTORG                                                                  
MQOPEN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   DMCB+8,X'A0'        SUPPRESS LENGTH FOR MESSAGE & HDR            
*                                                                               
* IF WE'RE RUNNING A TEST, SEND TO TEST MQ BROKER                               
         CLI   TESTMQ,C'T'         IS THIS A MQ TEST RUN                        
         BNE   *+8                  NO                                          
         OI    DMCB+8,X'01'         YES -PUT TO TEST MQ BROKER                  
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),(0,=C'MEDIACOMSFTP****'),,0             
         CLI   DMCB+8,0                                                         
         JE    EXIT                                                             
         DCHO                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
EDIOUT   DCB   DDNAME=EDIOUT,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=400,                                              X        
               BLKSIZE=4000,                                           X        
               MACRF=PM                                                         
*                                                                               
TEMPEB   DCB   DDNAME=TEMPEB,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=400,                                              X        
               BLKSIZE=4000,                                           X        
               MACRF=PM                                                         
*                                                                               
*        SPACE 2                                                                
WORKD    DSECT                                                                  
*                                                                               
FDELIM   EQU   X'5E'               FIELD DELIMITER - SEMICOLON                  
MAXREQS  EQU   100                 MAX REQUESTS                                 
*                                                                               
*                                                                               
XMLMAP   DS    A                   A(EDI MAP XML PROGRAM)                       
EDIMAP   DS    A                   A(EDI MAP PROGRAM)                           
EDINME   DS    A                   A(EDIOUT DATASET NAME)                       
AMQRPT   DS    A                   MQ REPORTING                                 
*                                                                               
MYDMCB   DS    3F                   SAVED DMCB                                  
*                                                                               
SVTDATE  DS    XL2        TRANSFER DATE                                         
*                                                                               
FIRST    DS    CL1    SET TO "Y" FOR FIRST RECORD FOR AN INDEX                  
*                                                                               
UATAGY   DS    CL1    Y=UAT AGY, ELSE N                                         
*                                                                               
OPENSW   DS    XL1                                                              
NETPAKSW DS    CL1                                                              
LIVERUN  DS    CL1    WILL BE Y IF NON-TEST REQ ENCOUNTERED                     
TESTMQ   DS    CL1    WILL BE SET FROM FIRST REQ FROM QOPT4                     
*                     N=NO MQ WORK                                              
*                     Y=LIVE MQ BUFFER                                          
*                     T=TEST MQ BUFFER                                          
EDICLT   DS    CL3    RETURNED FROM DDXMLMAP - INTERNAL CLIENT CODE             
*                                                                               
MYMQFILE DS    CL34   FILE NAME - MINUS SFTPDISK.PROD.(TEST)                    
MYMQDATE DS    CL6    DATE YYMMDD                                               
MYMQTIME DS    CL6    TIME HHMMSS                                               
*                                                                               
ELEM     DS    CL230                                                            
*                                                                               
FIXREC   DS    0XL400                                                           
         DS    XL4                                                              
         DS    XL396                                                            
*                                                                               
CLTMTAB  DS    100CL4              100 ENTRIES OF CLT/MED CODE                  
CLTMTABX DS    X                                                                
*                                                                               
WKPARMS  DS    6F                  WORKING STORAGE FOR PARAMETERS               
*                                                                               
BRKEY    DS    0XL9                BILL RECORD KEY FOR BINSRCH TAB              
BRKMED   DS    CL1                                                              
BRKCLT   DS    CL3                                                              
BRKINV   DS    XL2                                                              
BRKBDT   DS    XL3                                                              
*                                                                               
SVQAGY   DS    CL2                 SAVE QAGENCY FOR RUNLAST                     
SVPROG   DS    CL2                 SAVE QPROG FOR RUNLAST                       
*                                                                               
HAVBUF   DS    CL1                                                              
VPPBVAL  DS    A                                                                
AFLDTAB  DS    A                                                                
EOR      DS    A                                                                
AWRKRBF  DS    A                ADDRESS OF WRKRBUFF                             
AWRKRBFX DS    A                ADDRESS OF BUFFER END                           
ARQTAB   DS    A                                                                
ARQTABX  DS    A                                                                
VLEN     DS    X                                                                
VTXT     DS    XL132                                                            
FLEN     DS    X                                                                
SAVDESC  DS    CL16                                                             
SAVUIDN  DS    CL10                                                             
SAVINVD  DS    CL8                                                              
SAVXFRD  DS    CL8                                                              
SAVHEAD  DS    CL45                SAVE 'H' HEADER RECORD                       
*                                                                               
INVCNT   DS    PL8                COUNT OF INVOICES TRANSMITTED                 
INVAMT   DS    PL8                AMOUNT DUE TRANSMITTED                        
AORAMT   DS    PL8                AOR INVOICES - AMOUNT DUE TRANSMITTED         
*                                                                               
       ++INCLUDE PPBVALD        AREA FOR PPBVAL RETURNED VALUES                 
         DS    CL20               SPARE                                         
*                                                                               
WRKFIL   DS    CL8                                                              
WRKRIND  DS    XL44                                                             
WRKRREC  DS    XL1024                                                           
         SPACE 2                                                                
LIND     DSECT                     DSECT FOR PRINT LINE                         
LINMED   DS    CL1                                                              
         DS    CL1                                                              
LINCLT   DS    CL3                                                              
         DS    CL2                                                              
LININV   DS    CL10                                                             
         DS    CL2                                                              
LINBDAT  DS    CL8                                                              
         DS    CL2                                                              
LINIDAT  DS    CL8                                                              
         DS    CL2                                                              
LINEDAT  DS    CL8                                                              
         DS    CL2                                                              
LINADDR  DS    CL45                                                             
         EJECT                                                                  
*                                                                               
MQMSGD   DSECT                                                                  
MQHID    DS    CL6                 HUB RECORD ID                                
MQSYS    DS    CL3                 SYSTEM                                       
MQAGYID  DS    CL4                 AGENCY 1D 4-CHAR                             
MQQUAL   DS    CL16                QUALIFIER                                    
MQDATE   DS    CL6                 YYMMDD OF DSN                                
MQTIME   DS    CL6                 HHMMSS OF DSN                                
MQDATA1  DS    CL32                NOT USED                                     
MQDATA2  DS    CL32                NOT USED                                     
MQFILE   DS    CL64                DSN  (MINUS SFTPDISK.PROD.)                  
MQMSGLNQ EQU   *-MQMSGD                                                         
*                                                                               
**MSGD   DSECT     OLD FORMAT                                                   
**HID    DS    CL6                 HUB RECORD ID                                
**QUAL   DS    CL16                QUALIFIER                                    
**QCOUNT  DS    CL8                 RECORD COUNT                                
**DATA1  DS    CL32                                                             
**DATA2  DS    CL32                                                             
**FILE   DS    CL64                DSN                                          
**MSGLNQ EQU   *-MQMSGD                                                         
*                                                                               
PPEB02   CSECT                                                                  
*                                                                               
***********************************************************************         
*        EDI FIELD LENGTHS FOR PRINT BILLS                                      
*                                                                               
*        USED WHEN CONVERTING INVOICES IN WORKER FILE FORMAT                    
*        WHERE FIELDS ARE VARIABLE LENGTH, TO A FIXED FIELD                     
*        LENGTH FORMAT FOR DATASETS.                                            
*                                                                               
*        TABLE FORMAT AS FOLLOWS:                                               
*                                                                               
*        RECORD LINE -CODE(2),DESC(20),A(NEXT)                                  
*        FIELD LINE  -LENGTH(1),DESC(20)                                        
*                                                                               
*        ONE LINE FOR EACH RECORD TYPE, FOLLOWED BUY A SET OF                   
*        LINES, ONE FOR EACH FIELD.                                             
*                                                                               
***********************************************************************         
*                                                                               
         DS    0D                                                               
         DC    CL8'*EDIFLDS'                                                    
EDIFLDS  DS    0X                                                               
EDFLIH   DC    CL2'IH',CL20'INVOICE HEADER      ',AL4(EDFLAG)                   
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(010),CL20'INVOICE NUMBER      '                              
         DC    AL1(008),CL20'INVOICE DATE        '                              
         DC    AL1(008),CL20'DUE DATE            '                              
         DC    AL1(001),CL20'INVOICE TYPE        '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLAG   DC    CL2'AG',CL20'AGENCY RECORD       ',AL4(EDFLME)                   
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(033),CL20'NAME                '                              
         DC    AL1(033),CL20'ADDRESS             '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLME   DC    CL2'ME',CL20'MEDIA RECORD        ',AL4(EDFLAV)                   
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(002),CL20'CODE                '                              
         DC    AL1(010),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLAV   DC    CL2'AV',CL20'ADVERTISER          ',AL4(EDFLBT)                   
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(003),CL20'CODE                '                              
         DC    AL1(020),CL20'NAME                '                              
         DC    AL1(005),CL20'INTERFACE NUMBER    '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLBT   DC    CL2'BT',CL20'BILL TO RECORD      ',AL4(EDFLP1)                   
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(036),CL20'ADDRESS LINE 1      '                              
         DC    AL1(036),CL20'ADDRESS LINE 2      '                              
         DC    AL1(036),CL20'ADDRESS LINE 3      '                              
         DC    AL1(036),CL20'ADDRESS LINE 4      '                              
         DC    AL1(036),CL20'ADDRESS LINE 5      '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLP1   DC    CL2'P1',CL20'PRODUCT GRP LEVEL 1  ',AL4(EDFLP2)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(005),CL20'CODE                '                              
         DC    AL1(012),CL20'LEVEL DESC          '                              
         DC    AL1(024),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLP2   DC    CL2'P2',CL20'PRODUCT GRP LEVEL 2  ',AL4(EDFLP3)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(005),CL20'CODE                '                              
         DC    AL1(012),CL20'LEVEL DESC          '                              
         DC    AL1(024),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLP3   DC    CL2'P3',CL20'PRODUCT GRP LEVEL 3  ',AL4(EDFLPR)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(005),CL20'CODE                '                              
         DC    AL1(012),CL20'LEVEL DESC          '                              
         DC    AL1(024),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLPR   DC    CL2'PR',CL20'PRODUCT RECORD       ',AL4(EDFLES)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(003),CL20'CODE                '                              
         DC    AL1(020),CL20'NAME                '                              
         DC    AL1(008),CL20'INTERFACE NUMBER    '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLES   DC    CL2'ES',CL20'ESTIMATE RECORD      ',AL4(EDFLM1)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(003),CL20'CODE                '                              
         DC    AL1(020),CL20'NAME                '                              
         DC    AL1(020),CL20'NAME - LINE 2       '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLM1   DC    CL2'M1',CL20'MARKET GRP LEVEL 1   ',AL4(EDFLM2)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(005),CL20'CODE                '                              
         DC    AL1(012),CL20'LEVEL DESC          '                              
         DC    AL1(024),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLM2   DC    CL2'M2',CL20'MARKET GRP LEVEL 2   ',AL4(EDFLM3)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(005),CL20'CODE                '                              
         DC    AL1(012),CL20'LEVEL DESC          '                              
         DC    AL1(024),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLM3   DC    CL2'M3',CL20'MARKET GRP LEVEL 3   ',AL4(EDFLMK)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(005),CL20'CODE                '                              
         DC    AL1(012),CL20'LEVEL DESC          '                              
         DC    AL1(024),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLMK   DC    CL2'MK',CL20'MARKET RECORD        ',AL4(EDFLJB)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(020),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLJB   DC    CL2'JB',CL20'JOB RECORD           ',AL4(EDFLST)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(025),CL20'JOB INFO - LINE 1   '                              
         DC    AL1(025),CL20'JOB INFO - LINE 2   '                              
         DC    AL1(030),CL20'JOB INFO - LINE 3   '                              
         DC    AL1(030),CL20'JOB INFO - LINE 4   '                              
         DC    AL1(030),CL20'JOB INFO - LINE 5   '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLST   DC    CL2'PB',CL20'PUBLICATION RECORD   ',AL4(EDFLCM)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(060),CL20'NAME                '                              
         DC    AL1(020),CL20'NAME - PART 2       '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLCM   DC    CL2'CM',CL20'COMMENT RECORD       ',AL4(EDFLUD)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(004),CL20'COMMENT LEVEL NAME  '                              
         DC    AL1(080),CL20'COMMENT TEXT        '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLUD   DC    CL2'UD',CL20'USER-DEFINED DATA    ',AL4(EDFLUC)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(002),CL20'DATA LOCATION       '                              
         DC    AL1(020),CL20'DATA DESCRIPTION    '                              
         DC    AL1(032),CL20'DATA TEXT           '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLUC   DC    CL2'UC',CL20'USER-COMMENT DATA    ',AL4(EDFLMT)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(002),CL20'DATA LOCATION       '                              
         DC    AL1(020),CL20'DATA DESCRIPTION    '                              
         DC    AL1(032),CL20'DATA TEXT           '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLMT   DC    CL2'MT',CL20'MONTHLY TOTAL RECPRD ',AL4(EDFLAD)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(004),CL20'DETAIL LEVEL NAME   '                              
         DC    AL1(008),CL20'MONTH OF SERVICE    '                              
         DC    AL1(010),CL20'PREVIOUS INV NUMBER '                              
         DC    AL1(011),CL20'ORDERED GROSS       '                              
         DC    AL1(011),CL20'ORDERED NET         '                              
         DC    AL1(011),CL20'ORDERED CASH DISC.  '                              
         DC    AL1(011),CL20'ORDERED TAX         '                              
         DC    AL1(011),CL20'ORDERED INSERTIONS  '                              
         DC    AL1(011),CL20'PREVIOUS GROSS      '                              
         DC    AL1(011),CL20'PREVIOUS NET        '                              
         DC    AL1(011),CL20'PREVIOUS CASH DISC. '                              
         DC    AL1(011),CL20'PREVIOUS TAX        '                              
         DC    AL1(011),CL20'PREVIOUS INSERTIONS '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLAD   DC    CL2'AD',CL20'AMOUNT DUE RECORD    ',AL4(EDFLPI)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(004),CL20'DETAIL LEVEL NAME   '                              
         DC    AL1(001),CL20'BILL BASIS          '                              
         DC    AL1(001),CL20'ADJUSTMENT BASIS    '                              
         DC    AL1(007),CL20'ADJUSTMENT %        '                              
         DC    AL1(011),CL20'BASIS AMOUNT        '                              
         DC    AL1(011),CL20'ADJUSTMENT AMOUNT   '                              
         DC    AL1(011),CL20'AMOUNT DUE AT LEVEL '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLPI   DC    CL2'PI',CL20'PREV INV LIST RECORD ',AL4(EDFLRA)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(010),CL20'INVOICE NUMBER      '                              
         DC    AL1(011),CL20'GROSS               '                              
         DC    AL1(011),CL20'NET                 '                              
         DC    AL1(011),CL20'CASH DISC.          '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
****     NOT USED FOR PRINTPAK                                                  
****                                                                            
EDFLRA   DC    CL2'RA',CL20'REMITTANCE ADDRESS   ',AL4(EDFLCT)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(020),CL20'NAME                '                              
         DC    AL1(024),CL20'ADDRESS             '                              
         DC    AL1(024),CL20'CITY                '                              
         DC    AL1(003),CL20'STATE               '                              
         DC    AL1(010),CL20'POSTAL CODE         '                              
         DC    X'FF'                                                            
*                                                                               
EDFLCT   DC    CL2'CT',CL20'CANADIAN TAXES       ',AL4(EDFLTD)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(007),CL20'TYPE                '                              
         DC    AL1(016),CL20'ACCOUNT             '                              
         DC    AL1(011),CL20'PERCENTAGE          '                              
         DC    AL1(011),CL20'BASIS               '                              
         DC    AL1(011),CL20'TAX AMOUNT          '                              
         DC    X'FF'                                                            
*                                                                               
*        THE RECORD BELOW INCLUDES CANADIAN TAXES                               
*        IT SHOULD ONLY APPEAR WHEN THOSE TAXES DO                              
*                                                                               
EDFLTD   DC    CL2'TD',CL20'TOTAL DUE            ',AL4(EDFLID)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(011),CL20'TOTAL DUE           '                              
         DC    X'FF'                                                            
*                                                                               
*        NOTES: THE INSERTION DATE MAY INCLUDE A LINE NUMBER                    
*               THERE MAY BE SPECIAL CHARGES IN THE DESCRIPTION                 
*              -CODE WILL PRECEDE THE CHARGE DESCRIPTION                        
*                                                                               
EDFLID   DC    CL2'ID',CL20'INSERTION DETAIL     ',AL4(EDFLXX)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(011),CL20'INSERTION DATE      '                              
         DC    AL1(025),CL20'DESCRIPTION         '                              
         DC    AL1(011),CL20'ORDERED GROSS       '                              
         DC    AL1(011),CL20'ORDERED NET         '                              
         DC    AL1(011),CL20'ORDERED CASH DISC.  '                              
         DC    AL1(011),CL20'ORDERED TAX         '                              
         DC    AL1(011),CL20'PREVIOUS GROSS      '                              
         DC    AL1(011),CL20'PREVIOUS NET        '                              
         DC    AL1(011),CL20'PREVIOUS CASH DISC. '                              
         DC    AL1(011),CL20'PREVIOUS TAX        '                              
         DC    AL1(010),CL20'REFERENCE NUMBER    '                              
         DC    AL1(050),CL20'MISCELLANEOUS DATA  '                              
         DC    X'FF'                                                            
*                                                                               
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLXX   DC    X'FF'                                                            
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL8'**RQTAB*'                                                    
RQTAB    DS    0C                                                               
         ORG   *+(RQTABL*MAXREQS)                                               
RQTABX   EQU   *                                                                
*                                                                               
RQTABD   DSECT   DSECT FOR REQUEST TABLE                                        
RQTSYS   DS    CL1                                                              
RQTMED   DS    CL1                                                              
RQTCLT   DS    CL3                                                              
RQTPRD   DS    CL3                                                              
RQTEST   DS    XL1                                                              
RQTSTA   DS    XL2         OLD COMPRESSED (BASE 1900)                           
RQTEND   DS    XL2         OLD COMPRESSED (BASE 1900)                           
RQTSTAN  DS    XL2         NEW COMPRESSED (BASE 1964)                           
RQTENDN  DS    XL2         NEW COMPRESSED (BASE 1964)                           
RQTINV1  DS    CL4                                                              
RQTINV2  DS    CL4                                                              
RQTOPTS  DS    0CL6                                                             
RQTOPT1  DS    CL1                                                              
RQTOPT2  DS    CL1                                                              
RQTOPT3  DS    CL1                                                              
RQTOPT4  DS    CL1                                                              
RQTOPT5  DS    CL1                                                              
RQTOPT6  DS    CL1                                                              
RQTRQS   DS    CL12                                                             
RQTABL   EQU   *-RQTABD                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE DMWRKFK                                                        
*                                                                               
       ++INCLUDE DMWRKFL                                                        
*                                                                               
       ++INCLUDE DMWRKFD                                                        
*                                                                               
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDMASTD                                                        
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
*                                                                               
* START AND END INVOIVE NUMBERS COL 53(4), AND COL 56 (4)                       
*                                                                               
QINVNO1  EQU   QRECORD+52                                                       
QINVNO2  EQU   QRECORD+56                                                       
*                                                                               
       ++INCLUDE PPREPWORK2                                                     
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
*                                                                               
*                                                                               
*                                                                               
BRMAX    EQU   2000                MAX RECORD IN TABLE                          
BRTABLN  EQU   9                                                                
*                                                                               
*                                                                               
*                                                                               
BRTAB    CSECT                     BILL RECORD TABLE FOR BINSRCH                
         ORG   *+(BRMAX*BRTABLN)                                                
         DC    X'00'                                                            
BRTABLNQ EQU   *-BRTAB                                                          
*                                                                               
*                                                                               
WRKRBUFF CSECT                                                                  
         ORG   *+14336                                                          
         DC    X'00'                                                            
WRKRBFX  EQU   *-WRKRBUFF                                                       
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096PPREPEB02 06/15/20'                                      
         END                                                                    
