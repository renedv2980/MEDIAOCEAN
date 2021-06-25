*          DATA SET SPREPEB02  AT LEVEL 148 AS OF 06/15/20                      
*PHASE SPEB02A                                                                  
*INCLUDE SPBVAL                                                                 
         TITLE 'SPEB02 - EDI FOR SPOT/NET BILLING'                              
SPEB02   CSECT                                                                  
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-44468  04/14/20 EXCLUDE ZERO AMOUNT INVOICES FOR H7 & OU  *         
***********************************************************************         
*                                                                               
*   **NOTE**  WHEN TESTING EX (XML) ON TSO,QOPT 4 SHOULD BE T OR N              
*             ELSE MQ RECORD MAY BE WRITTEN TO PRODUCTION BUFFER                
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
*   BPLA  04/15    FOR UAT AGENCIES FOR QOPT2 TO Y (TEST RUN)                   
*                  AND QOPT4 TO N (NO MQ MESSAGE)                               
*                                                                               
*   BPLA  01/13    FORCE QOPT4 TO N FOR AGENCY O0  (LBCTOA)                     
*                  FOR KRAFT THEIR XML FILE IS A PRTTAPE                        
*                  AND NOT A SFTPDISK                                           
*                                                                               
*   BPLA  09/09    USED CLIENT RETURNED BY DDXMLMAP TO ALTER                    
*                  AGENCY ID IN MQ NOTIFICATION                                 
*                                                                               
*   BPLA  07/09      DISPLAY XML FILE NAME                                      
*                                                                               
*   BPLA  02/09       MOVE LOADING OF T00ABF TO RUN FIRST                       
*                                                                               
*   BPLA  05+06/08    NEW VALUE (X) FOR TESTING OPTION - QPOT2                  
*                     X - MEANS DON'T MARK WORKER FILES NOR BILLS               
*                     BUT STILL CREATE TRANSMISSION FILE                        
*                     MQ BUFFER FORMAT + FILE NAMES                             
*                                                                               
*    BPLA   8/06     REPORT EX WILL ALSO CALL THIS PROGRAM                      
*                    FOR XML OUTPUT                                             
*                                                                               
*    BPLA   7/04     CODE TO SUPPORT NT (CANADIAN NETWORK) RECS                 
*                                                                               
*    BPLA   3/04     ENTRIES FOR CT (CANADIAN TAX) AND                          
*                    TD (TOTAL DUE - WITH TAXES) RECORDS                        
*                                                                               
*    BPLA   10/03    PROVIDE COUNT OF INVOICES AND $ TRANSMITTED                
*                                                                               
*    BPLA   01/00    FIX WORKER FILE READ LOGIC TO DO CERTAIN                   
*                    THINGS ONLY WHEN PROCESSING THE FIRST RECORD               
*                    FOR AN INDEX                                               
*                    (LARGE NUMBER OF RECORDS CAUSED A PROBLEM)                 
*                    SEE **BP** FOR CHANGES                                     
*                                                                               
*    SMYE   10/99    UPDATE APPROPRIATE BILLING RECORDS WITH DATE               
*                      OF EDI TRANSMITTAL                                       
*                                                                               
*    BPLA  10/8/99   CHANGE THE WAY REDO (RERUN) WORKS -                        
*                    ON A RERUN-ONLY FILES PREVIOUSLY TRANSMITTED               
*                    WILL BE PROCESSED                                          
*                    SAVE THE TRANSMITTAL DATE IN THE WORKER FILE               
*                    IN W_DESC+11(3) AND UPDATE IT WHEN REDOING                 
*                                                                               
*                                                                               
**********************************************************************          
*         QOPT1    T=TRACE WORKER AND OUTPUT RECORDS                            
*         QOPT2    Y=TEST MODE                                                  
*         QOPT3    R=REDO, U=UNDO                                               
*         QOPT4    Y=MQ FOR XML TO LIVE MQ BROKER                               
*                  T=MQ FOR XML TO TEST MQ BROKER                               
*                  N=NO MQ XML RECORD                                           
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
         NMOD1 0,SPEB02,CLEAR=YES,RR=R2                                         
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         LA    RC,SPACEND                                                       
         USING WORKD,RC                                                         
*                                                                               
         LA    R4,SPEB02+4095                                                   
         LA    R4,1(R4)                                                         
         USING SPEB02+4096,R4     **NOTE USE OF R4 AS BASE REG*                 
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
*                                                                               
         MVI   LIVERUN,C'N'   WILL BE SET TO "Y" IF LIVE REQUEST                
*                           IS PROCESSED                                        
         MVI   TESTMQ,0     WILL BE SET FROM FIRST MQ REQ                       
*                           QOPT4                                               
         ZAP   INVCNT,=P'0'                                                     
         ZAP   INVAMT,=P'0'                                                     
         ZAP   AORAMT,=P'0'                                                     
*                                                                               
         L     RF,=V(SPBVAL)                                                    
         A     RF,RELO                                                          
         ST    RF,VSPBVAL                                                       
*                                                                               
         L     RF,=A(RQTAB)                                                     
         A     RF,RELO                                                          
         ST    RF,ARQTAB                                                        
         L     RF,=A(RQTABX)                                                    
         A     RF,RELO                                                          
         ST    RF,ARQTABX                                                       
         L     RF,=A(BRTAB)                                                     
         A     RF,RELO                                                          
         ST    RF,ABRTAB                                                        
         L     RF,=A(BRTABX)                                                    
         A     RF,RELO                                                          
         ST    RF,ABRTABX                                                       
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
         GOTOR MCVLOADM,DMCB,0                                                  
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
* CLEAR BINSRCH TABLE ENTRIES                                                   
*                                                                               
         L     RE,ABRTAB           GET ADDRESS OF TABLE                         
         L     RF,=F'22000'        GET LENGTH OF TABLE                          
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
*                                                                               
* CLEAR CLIENT/MEDIA CODE TABLE                                                 
*                                                                               
         LA    RE,CLTMTAB          GET ADDRESS OF TABLE                         
         LA    RF,301              GET LENGTH OF TABLE                          
         XCEF                                                                   
         MVI   CLTMTABX,X'FF'      END-OF-TABLE INDICATOR                       
*                                                                               
* PREPARE PARAMETERS FOR BINSRCH                                                
*                                                                               
         XC    WKPARMS,WKPARMS                                                  
         L     RF,ABRTAB                                                        
         ST    RF,WKPARMS+04       ADDRESS OF TABLE                             
         LA    RF,BRTABL                                                        
         ST    RF,WKPARMS+12       LENGTH OF KEY                                
         ST    RF,WKPARMS+16       DISPLACEMENT/BYTE 0, L'KEY/BYTES 1-3         
         LA    RF,BRMAX                                                         
         ST    RF,WKPARMS+20       MAXIMUM NUMBER OF RECORDS IN TABLE           
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
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         LA    R3,NEDYNDSN         NETPAK                                       
         MVI   NETPAKSW,C'Y'                                                    
         CLI   MCNETPAK,C'Y'                                                    
         BE    *+12                                                             
         MVI   NETPAKSW,C'N'                                                    
         LA    R3,SPDYNDSN         OR SPOT                                      
         DROP  RF                                                               
*                                                                               
         CLC   QAGY,=C'O0'               LBCTOA                                 
         BNE   *+8                                                              
         MVI   QOPT4,C'N'                SET FOR NO MQ NOTIFICATION             
*                                        THEY GET XML AS A SPTTAPE              
         MVC   SVPROG,QPROG              SAVE REQUEST TYPE                      
         CLC   QPROG(2),=C'EX'           XML REQUEST?                           
         BNE   REQF1                                                            
         L     R5,VMASTC                                                        
         USING MASTD,R5                                                         
         MVC   SAVUIDN,MCUSERID                                                 
*                                                                               
*                                                                               
REQF1    MVC   SVQAGY,QAGY         SAVE QAGENCY FOR RUNLAST                     
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
         CLI   QOPT3,C'U'          OR IF 'UNDOING'                              
         BE    REQF1H                                                           
         CLI   OPENSW,C'Y'         TEST IF  ALREADY OPEN                        
         BE    REQF1H                                                           
*                                                                               
*                                                                               
         MVC   13(2,R3),QAGY                                                    
*                                                                               
         CLC   QAGY,=C'*B'          DDSB TESTING                                
         BNE   *+10                                                             
         MVC   13(2,R3),=C'@B'      MUST ALTER TO @B                            
         CLC   QAGY,=C'W+'          METEST TESTING                              
         BNE   *+10                                                             
         MVC   13(2,R3),=C'W@'      MUST ALTER TO W@                            
*                                                                               
         MVC   11(2,R3),QPROG      ALSO SET PROGRAM TYPE                        
         GOTO1 DYNALLOC,DMCB,(0,=C'EDIOUT  '),(0,0(R3))                         
         LA    R5,EDIOUT                                                        
         OPEN  ((R5),OUTPUT)                                                    
         OPEN  (TEMPEB,OUTPUT)                                                  
         MVI   OPENSW,C'Y'                                                      
         ST    R3,EDINME                                                        
         B     REQF1H                                                           
*                                                                               
SPDYNDSN DC    CL20'SPTTAPE.SP0EBAG'                                            
NEDYNDSN DC    CL20'NETTAPE.NE0EBAG'                                            
*                                                                               
REQF1H   DS    0H                                                               
         MVI   FCRDBUYS,C'N'                                                    
         MVI   FCRDGOAL,C'N'                                                    
         MVI   FORCEHED,C'Y'                                                    
*                                  ADD TO REQUEST TABLE                         
         L     R3,ARQTAB           NOTE- ONE REQUEST AT A TIME FOR NOW          
         USING RQTABD,R3                                                        
         C     R3,ARQTABX                                                       
         BL    *+6                                                              
         DC    H'0'                REQUEST TABLE FULL                           
*                                                                               
         XC    RQTABD(RQTABL),RQTABD                                            
         MVI   RQTSYS,C'N'         NETPAK                                       
         CLI   NETPAKSW,C'Y'                                                    
         BE    REQF03                                                           
         MVI   RQTSYS,C'S'         ELSE SPOT                                    
*                                                                               
REQF03   DS    0H                                                               
         MVC   RQTMED,QMED                                                      
         MVC   RQTCLT,QCLT                                                      
*                                  NOTE- PRD/EST NOT USED NOW                   
         MVC   RQTPRD,QPRD                                                      
         CLI   QEST,C'0'                                                        
         BL    REQF04                                                           
         PACK  DUB,QEST(3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,RQTEST                                                        
*                                                                               
REQF04   DS    0H                                                               
         CLI   QSTART,C' '                                                      
         BNH   REQF05                                                           
         GOTO1 DATCON,DMCB,QSTART,(2,RQTSTA)                                    
         GOTO1 DATCON,DMCB,QSTART,(30,RQTSTAN)                                  
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
*        UPDATE BILL RECORD WITH DATE OF EDI TRANSMITTAL                        
*                                                                               
*        NOTE - DO NOT USE R6 IN RUNL BELOW - IT IS POINTING                    
*               TO THE AGYMED/CLT TABLE                                         
***********************************************************************         
*                                                                               
RUNL     DS    0H                                                               
         LA    R6,CLTMTAB                                                       
         CLI   0(R6),0             AGYMED/CLT TABLE EMPTY ?                     
         BE    RUNL50              YES - NO UPDATE                              
*                                                                               
RUNL05   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),0(R6)      AGYMED/CLT                                   
*                                                                               
RUNL10   GOTO1 HIGH                                                             
         B     RUNL20                                                           
*                                                                               
RUNL15   GOTO1 SEQ                                                              
*                                                                               
RUNL20   CLC   KEY(4),KEYSAVE      SAME TYPE/AGYMED/CLT ?                       
         BNE   RUNL45              NO - NEXT ENTRY IN AGYMED/CLT TABLE          
*                                                                               
         LA    R5,KEY                                                           
         USING BKEY,R5                                                          
         OC    BKEYINV,BKEYINV     BILL RECORD ?                                
         BZ    RUNL15              NO - GET NEXT DIR RECORD TO TEST             
*                                                                               
         XC    WORK,WORK           "RECORD" TO BE FOUND IN BINTBL               
         MVC   WORK(1),BKEYAM      AGENCY/MEDIA                                 
         MVC   WORK+1(2),BKEYCLT   CLIENT                                       
         MVC   WORK+3(2),BKEYINV   BILL NUMBER (INVOICE)                        
         DROP  R5                                                               
*                                                                               
         GOTO1 BINSRCH,WKPARMS,(X'02',WORK)     READ HIGH                       
         TM    0(R1),X'01'         RECORD FOUND ?                               
         BO    RUNL15              NO - GET NEXT DIR RECORD TO TEST             
         ZICM  R3,1(R1),3          POINT R3 TO TABLE - A(FOUND BIN REC)         
         USING BRTABD,R3           BINSRCH RECORD DESC                          
*                                                                               
         CLC   WORK(5),BRTMED      SAME AGYMED/CLIENT/INVOICE ?                 
         BNE   RUNL15              NO - GET NEXT DIR RECORD TO TEST             
*                                                                               
         GOTO1 GETBILL                                                          
         L     R2,ADBILL                                                        
         USING BILLREC,R2                                                       
*                                                                               
         MVC   WORK+5(6),BDATE     BILL DATE FROM BILLREC                       
         GOTO1 BINSRCH,WKPARMS,(X'00',WORK)   LOOK FOR EXACT MATCH              
         TM    0(R1),X'01'         RECORD FOUND ?                               
         BO    RUNL15              NO - GET NEXT DIR RECORD TO TEST             
*                                                                               
*                                  EXACT MATCH FOUND - UPDATE BILLREC           
         MVC   BEDIDTE,TODAYP      DATE OF EDI TO BILLREC                       
         GOTO1 PUTBILL                                                          
         EJECT                                                                  
*                                                                               
*        ACCUMULATE AMOUNT DUE                                                  
*                                                                               
         CLI   BRETAIL,X'41'      DON'T TOTAL RETAIL SUMMARY BILL               
         BE    RUNL15                                                           
*                                                                               
         GOTO1 VSPBVAL,DMCB,(C'B',BILLREC),SPBVALD                              
*                                                                               
         LA    RE,AORAMT                                                        
         TM    BILSTAT,BSTTAORQ    AOR BILL?                                    
         BO    *+8                                                              
         LA    RE,INVAMT                                                        
         AP    0(8,RE),SPBVACTP     AMOUNT DUE                                  
         L     RF,ADAGY                                                         
         CLI   AGYPCNDA-AGYHDR(RF),C'C'     CANADIAN?                           
         BNE   RUNL15                                                           
         L     R0,SPBVGST          INCLUDE GST AN PST                           
         CVD   R0,DUB                                                           
         AP    0(8,RE),DUB                                                      
         L     R0,SPBVPST                                                       
         CVD   R0,DUB                                                           
         AP    0(8,RE),DUB                                                      
*                                                                               
         B     RUNL15              DO NEXT RECORD                               
         DROP  R2,R3                                                            
*                                                                               
RUNL45   DS    0H                                                               
         LA    R6,3(R6)            NEXT CLT/MED ENTRY                           
         CLI   0(R6),X'FF'         END OF TABLE ?                               
         BE    RUNL50              YES - DONE                                   
         CLI   0(R6),0             END OF ENTRIES ?                             
         BNE   RUNL05              NO - READ FOR NEXT AGYMED/CLIENT             
*                                                                               
*                                                                               
RUNL50   MVI   FORCEHED,C'Y'                                                    
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
         MVI   FORCEHED,C'Y'                                                    
         MVI   SKIPSPEC,C'Y'                                                    
*                                                                               
         CLC   SVPROG,=C'EX'     XML PROCESSING?                                
         BE    DOXML                                                            
*                                                                               
         GOTO1 EDIMAP,DMCB,EDINME,ACOMFACS,DYNALLOC,VMASTC,P,REPORT             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
DOXML    DS    0H                                                               
*                                                                               
*              PASS TESTMQ CONTROL TO DDXMLMAP                                  
*                                                                               
         GOTO1 XMLMAP,DMCB,(TESTMQ,EDINME),ACOMFACS,DYNALLOC,VMASTC,P,RX        
               EPORT                                                            
*                                                                               
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
         MVC   EDICLT,DMCB+1   SAVE RETURNED INTERNAL CLIENT                    
*                                                                               
* SEND MQ MESSAGE WITH FILE NAME                                                
         LA    R5,ELEM                                                          
         USING MQMSGD,R5                                                        
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(MQMSGLNQ-1),ELEM                                          
         MVC   MQFILE(34),MYMQFILE                                              
         MVC   MQDATE(6),MYMQDATE                                               
         MVC   MQTIME(6),MYMQTIME                                               
*                    FORMAT IS BIL.SYS.AGID.DYYMMDD.THHMMSS                     
*                    (SYS=SYSTEM,AGID= 4-CHARACTER AGENCY ID)                   
*                                                                               
         BRAS  RE,MQOPEN                                                        
*                                                                               
         MVC   MQHID,=CL6'DANOT1'                                               
         MVC   MQSYS,MQFILE+4             SYSTEM                                
         MVC   MQAGYID,MQFILE+8           AGENCY 1D (4 CHAR)                    
*          DATA SET PPREPEB02  AT LEVEL 087 AS OF 09/14/09                      
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
**OLD**  MVC   MQHID,=CL6'SFTP2'                                                
**OLD**  MVC   MQQUAL,=CL16'MEDIACOM-WB-BILL'                                   
**OLD**  MVC   MQDATA1(06),=C'WB XML'                                           
**OLD**  OC    MQDATA1,SPACES                                                   
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
         BE    RWP5U               YES, PASS ALL                                
         CLI   RQTOPT3,C'U'        UNDO?                                        
         BNE   RWP5V                                                            
RWP5U    TM    UKSTAT,X'08'        MUST BE DONE                                 
         BO    RWP6                                                             
         B     RWP5X                                                            
*                                                                               
RWP5V    DS    0H                                                               
         TM    UKSTAT,X'08'        ELSE CAN'T BE ALREADY DONE                   
         BZ    RWP6                                                             
*                                                                               
RWP5X    DS    0H                                                               
         LA    R3,RQTABL(R3)       NEXT REQUEST TABLE ENTRY                     
         B     RWP5                                                             
*                                                                               
*                                  INDEX PASSES, NOW READ RECORDS               
RWP6     DS    0H                                                               
*                                                                               
         BAS   RE,NOZERO           EXCLUDE $0 AMOUNT DUE INVOICE?               
         BNE   RWP4                YES - SKIP THIS                              
**BP**                                                                          
         MVI   FIRST,C'Y'         SET FIRST RECORD SW                           
         B     RWP6AX                                                           
*                                                                               
RWP6A    DS    0H                                                               
         MVI   FIRST,C'N'                                                       
*                                                                               
RWP6AX   DS    0H                                                               
**BP**                                                                          
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
**BP**                                                                          
         CLI   FIRST,C'Y'        ONLY DO FOR FIRST RECORD                       
         BNE   RWP6D                                                            
**BP**                                                                          
         MVC   SAVDESC,W_DESC                                                   
         LA    RF,W_DESC+9         PICK UP LAST 4 OF INVNO                      
         CLI   0(RF),C' '          TO DO FILTERING                              
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SH    RF,=H'3'                                                         
         CLC   0(4,RF),RQTINV1     INVNO VS START                               
         BL    RWP5X                                                            
         CLC   0(4,RF),RQTINV2     AND END                                      
         BH    RWP5X                                                            
*                                                                               
***********************************************************************         
*                            ADD TO TABLES TO IDENTIFY BILL RECORDS   *         
*                            TO BE UPDATED WITH THE TRANSMITTAL DATE  *         
***********************************************************************         
RWP6D    DS    0H                                                               
*                                                                               
         CLI   RQTOPT2,C'Y'     SEE IF TEST RUN                                 
         BE    RWP7             SKIP TABLE ADDITIONS                            
*                                                                               
         CLI   RQTOPT2,C'X'     SEE IF OTHER TEST RUN                           
         BE    RWP7             SKIP TABLE ADDITIONS                            
**BP**                                                                          
         CLI   FIRST,C'Y'                                                       
         BNE   RWP7                                                             
**BP**                                                                          
*                                    *****   ADD TO AGYMD/CLT TABLE             
*NOP*    MVC   FULL(1),BAGYMD      AGENCY/MEDIA                                 
         L     R6,ADCLT            POINT TO CLIENT HEADER                       
         MVC   FULL(3),1(R6)       AGYMD/CLT                                    
*                                                                               
         LA    R6,CLTMTAB          POINT TO TABLE                               
RWP6F    DS    0H                                                               
         CLI   0(R6),X'FF'         END OF TABLE ?                               
         BNE   *+6                 NO                                           
         DC    H'0'                TABLE IS FULL                                
         CLC   FULL(3),0(R6)       ENTRY ALREADY THERE ?                        
         BE    RWP6J               YES - GO DO BINSRCH TABLE                    
         CLI   0(R6),0             "EMPTY" CELL ?                               
         BE    RWP6H               YES - GO ADD CLIENT TO TABLE                 
         LA    R6,3(R6)            BUMP TO NEXT CELL                            
         B     RWP6F                                                            
RWP6H    DS    0H                                                               
         MVC   0(3,R6),FULL        ADD CLIENT TO TABLE                          
*                                                                               
*                                    *****   ADD TO BINSRCH TABLE               
RWP6J    DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING BRTABD,R6                                                        
         MVC   BRTMED(3),FULL      AGYMD/CLT                                    
*                                                                               
         PACK  DUB,0(4,RF)         RF POINTING TO 4-BYTE INVNO                  
         CVB   RE,DUB                                                           
         STCM  RE,3,BRTINV         2 BYTE HEX INVNO                             
*                                                                               
         TM    UKATTB,WLATNCD      NEW COMPRESSED DATES (BASE 1964)             
         BZ    RWP6JA                                                           
         GOTO1 DATCON,DMCB,(14,UKAGELD),(0,BRTBDT)  COMPRSSD BASE 1964          
         B     RWP6JB                                                           
RWP6JA   GOTO1 DATCON,DMCB,(2,UKAGELD),(0,BRTBDT)   COMPRSSD BASE 1960          
*                                                                               
RWP6JB   GOTO1 BINSRCH,WKPARMS,(X'01',WORK)    ADD TO TABLE                     
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
***********************************************************************         
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
         BAS   RE,PUTFXR                                                        
*                                                                               
**BP**                                                                          
         CLI   FIRST,C'Y'                                                       
         BNE   RWP8A                                                            
**BP**                                                                          
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
         CLI   RQTOPT2,C'X'     SEE IF OTHER TEST RUN                           
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
**BP**                                                                          
*******  B     RWP6     (OLD BRANCH)                                            
RWP8A    DS    0H                                                               
         B     RWP6A                                                            
**BP**                                                                          
*                                                                               
         DROP  R5                                                               
*                                                                               
RWP40    DS    0H                                                               
*                                                                               
         LA    R2,P                                                             
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
RWP42    MVC   WORK(2),SVTDATE     ELSE SHOW KILL (TRANSFER) DATE               
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
         BE    RWP44               SKIP DATE MARKING                            
*                                                                               
         CLI   RQTOPT2,C'X'        IF OTHER TEST MODE                           
         BE    RWP44               SKIP DATE MARKING                            
*                                                                               
         CLI   RQTOPT3,C'U'        IF 'UNDOING', UNKEEP                         
         BNE   RWP43D                                                           
         TM    UKSTAT,X'08'        DON'T UNDO IF NOT DONE                       
         BNO   RWP44               SKIP                                         
         GOTO1 DATAMGR,DMCB,=C'UNKEEP',WRKFIL,WRKRIND,WRKRREC,         X        
               AWRKRBF                                                          
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    RWP44                                                            
         DC    H'0'                                                             
*                                                                               
RWP43D   DS    0H                                                               
********      NO-OP ALREADY KEPT CHECK                                          
********       KEEPING A KEPT FILE IS ALLOWED                                   
********                                                                        
******** TM    UKSTAT,X'08'        IF ALREADY DONE (KEEP)                       
******** BO    RWP44               SKIP                                         
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
PRTWKR   NTR1                      PRINT WKR RECORD                             
         SR    R1,R1                                                            
         ICM   R1,3,WRKRREC                                                     
         SH    R1,=H'4'                                                         
         CH    R1,=H'65'                                                        
         BNH   *+8                                                              
         LA    R1,65                                                            
         BCTR  R1,0                                                             
         EX    R1,WRKMVC                                                        
         BAS   RE,MYPRNT                                                        
         B     EXIT                                                             
*                                                                               
WRKMVC   MVC   P+2(0),WRKRREC+4                                                 
         EJECT                                                                  
***********************************************************************         
*        PUTFXR -  CONVERT TO FIXED RECORD FORMAT                               
***********************************************************************         
*                                                                               
PUTFXR   NTR1                                                                   
*                                                                               
*        NOTE: DO NOT USE R4 IN THIS ROUTINE AS IT IS NEEDED                    
*              TO REFERENCE RQTOPT1                                             
*                                 SPECIAL IDENTIFICATION RECORD                 
         CLI   WRKRREC+4,C'H'     STARTS WITH H                                 
         BNE   PF04                                                             
         CLI   WRKRREC+6,FDELIM   AND 3RD CHAR NOT A FLD DELIMITER              
         BE    PF04                                                             
*                                                                               
         MVC   FIXREC(2),=H'45'    RECORD LENGTH                                
         XC    FIXREC+2(2),FIXREC+2                                             
         MVC   FIXREC+4(41),WRKRREC+4                                           
         MVC   SAVHEAD(41),WRKRREC+4   SAVE EDI ADDRESS, ETC..                  
         CLI   OPENSW,C'Y'                                                      
         BNE   PF03                                                             
*                                                                               
         PUT   EDIOUT,FIXREC                                                    
         PUT   TEMPEB,FIXREC                                                    
*                                                                               
PF03     CLI   RQTOPT1,C'T'        TRACE?                                       
         BNE   PFX                                                              
         MVC   P+2(41),FIXREC                                                   
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
         LA    R6,FIXREC+4         FIRST POS IN OUTPUT REC                      
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
         C     R8,EOR                                                           
         BNL   PF20                FIELD MISSING                                
*                              *** NOTE- USED TO ABEND HERE  ***                
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
         MVC   0(0,(R6)),SPACES                                                 
*                                                                               
         ZIC   R3,VLEN             THEN MOVE VARIABLE TEXT                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,(R6)),VTXT                                                   
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
         AR    R6,R3                                                            
         LA    R8,1(R8)            BUMP PAST THIS FIELD DELIM                   
         LA    R5,21(R5)           NEXT FIELD DESCRIPTOR                        
         B     PF05D                                                            
*                                                                               
PF20     DS    0H                                                               
         LA    R1,FIXREC                                                        
         SR    R6,R1                                                            
         STCM  R6,3,FIXREC                                                      
         XC    FIXREC+2(2),FIXREC+2                                             
*                                                                               
         CLI   OPENSW,C'Y'                                                      
         BNE   PF22                                                             
*                                                                               
         CLC   WRKRREC+4(2),=C'IH' INV HEADER RECORD                            
         BNE   PF20C                                                            
         AP    INVCNT,=P'1'      ADD TO TRANSMITTED COUNT                       
PF20C    DS    0H                                                               
*                                                                               
         PUT   EDIOUT,FIXREC                                                    
         PUT   TEMPEB,FIXREC                                                    
*                                                                               
PF22     CLI   RQTOPT1,C'T'        TRACE?                                       
         BNE   PFX                 NO, DONE                                     
         CH    R6,=H'132'                                                       
         BNH   *+8                                                              
         LH    R6,=H'132'                                                       
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   P+2(0),FIXREC                                                    
         BAS   RE,MYPRNT                                                        
*                                                                               
PFX      DS    0H                                                               
         B     EXIT                                                             
*        EJECT                                                                  
**********************************************************************          
*        MYPRNT                                                                 
**********************************************************************          
*                                                                               
MYPRNT   NTR1                                                                   
         MVC   HEAD1+50(25),=C'SPOTPAK EDI BILL TRANSFER'                       
         MVC   HEAD2+50(25),=C'-------------------------'                       
         CLI   NETPAKSW,C'Y'                                                    
         BNE   MYPRNT4                                                          
         MVC   HEAD1+50(4),=C' NET'                                             
         MVI   HEAD2+50,C' '                                                    
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
         CLI   RQTOPT2,C'X'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+3(4),=C'TEST'                                              
*                                                                               
         CLI   RQTOPT3,C'R'                                                     
         BNE   *+10                                                             
         MVC   HEAD6(11),=C'** RERUN **'                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
NOZERO   NTR1                                                                   
*                                                                               
         CLC   QAGY,=C'OU'          AGENCY OU?                                  
         BE    *+14                 YES                                         
         CLC   QAGY,=C'H7'          AGENCY H7?                                  
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
***********************************************************************         
* MQOPEN                                                                        
***********************************************************************         
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
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
*                                                                               
FDELIM   EQU   X'5E'               FIELD DELIMITER - SEMICOLON                  
MAXREQS  EQU   100                 MAX REQUESTS                                 
BRMAX    EQU   2000                MAX RECORDS IN BILL REC TABLE                
*                                                                               
*                                                                               
VSPBVAL  DS    A                                                                
XMLMAP   DS    A                   A(EDI AMP XML PROGRAM)                       
EDIMAP   DS    A                   A(EDI MAP PROGRAM)                           
EDINME   DS    A                   A(EDIOUT DATASET NAME)                       
AMQRPT   DS    A                   MQ REPORTING                                 
*                                                                               
MYDMCB   DS    3F                                                               
*                                                                               
SVTDATE  DS    XL2         SAVED TRANSMITTAL DATE (FOR DISPLAY)                 
*                                                                               
**BP**                                                                          
FIRST    DS    CL1         SET TO 'Y' FOR FIRST RECORD FOR INDEX                
**BP**                                                                          
*                                                                               
UATAGY   DS    CL1    Y=UAT AGY, ELSE N                                         
*                                                                               
OPENSW   DS    XL1                                                              
NETPAKSW DS    CL1                                                              
LIVERUN  DS    CL1    WILL BE Y IF NON-TEST REQ ENCOUNTERED                     
TESTMQ   DS    CL1    WILL BE SET FROM FIRST REQ FROM QOPT4                     
*                     N=NO MQ WORK                                              
*                     Y=LIVE MQ BUFFER (DEFAULT SO BE CAREFUL)                  
*                     T=TEST MQ BUFFER                                          
EDICLT   DS    CL3    RETURNED FROM DDXMLMAP - INTERNAL CLIENT CODE             
MYMQFILE DS    CL34   FILE NAME - MINUS SFTPDISK.PROD.(TEST)                    
MYMQDATE DS    CL6    DATE YYMMDD                                               
MYMQTIME DS    CL6    TIME HHMMSS                                               
*                                                                               
ELEM     DS    CL240                                                            
*                                                                               
FIXREC   DS    0XL400                                                           
         DS    XL4                                                              
         DS    XL396                                                            
*                                                                               
CLTMTAB  DS    100CL3              100 ENTRIES OF AGYMD/CLIENT                  
CLTMTABX DS    X                   X'FF'                                        
*                                                                               
WKPARMS  DS    6F                  BINSRCH PARAMETERS                           
*                                                                               
SVQAGY   DS    CL2                                                              
SVPROG   DS    CL2                                                              
*                                                                               
HAVBUF   DS    CL1                                                              
AFLDTAB  DS    A                                                                
EOR      DS    A                                                                
AWRKRBF  DS    A                                                                
AWRKRBFX DS    A                                                                
ARQTAB   DS    A                                                                
ARQTABX  DS    A                                                                
ABRTAB   DS    A                                                                
ABRTABX  DS    A                                                                
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
       ++INCLUDE SPBVALD        AREA FOR PPBVAL RETURNED VALUES                 
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
**OLD**  DSECT                                                                  
**MSGD   DSECT                                                                  
**HID    DS    CL6                 HUB RECORD ID                                
**QUAL   DS    CL16                QUALIFIER                                    
**QCOUNT  DS    CL8                 RECORD COUNT                                
**DATA1  DS    CL32                                                             
**DATA2  DS    CL32                                                             
**FILE   DS    CL64                DSN                                          
**MSGLNQ EQU   *-MQMSGD                                                         
*                                                                               
MQMSGD   DSECT                                                                  
MQHID    DS    CL6                 HUB RECORD ID                                
MQSYS    DS    CL3                 SYSTEM                                       
MQAGYID  DS    CL4                 AGENCY 1D 4-CHAR                             
MQQUAL   DS    CL16                QUALIFIER                                    
MQDATE   DS    CL6                                                              
MQTIME   DS    CL6                                                              
MQDATA1  DS    CL32                NOT USED                                     
MQDATA2  DS    CL32                NOT USED                                     
MQFILE   DS    CL64                DSN (MINUS SPTPDISK.PROD.)                   
MQMSGLNQ EQU   *-MQMSGD                                                         
*                                                                               
SPEB02   CSECT                                                                  
*                                                                               
***********************************************************************         
*        EDI FIELD LENGTHS FOR SPOT BILLS                                       
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
         DC    AL1(008),CL20'SHIP DATE           '                              
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
         DC    AL1(024),CL20'NAME                '                              
         DC    AL1(008),CL20'INTERFACE NUMBER    '                              
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
         DC    AL1(024),CL20'NAME                '                              
         DC    AL1(008),CL20'INTERFACE NUMBER    '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLES   DC    CL2'ES',CL20'ESTIMATE RECORD      ',AL4(EDFLM1)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(003),CL20'CODE                '                              
         DC    AL1(024),CL20'NAME                '                              
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
EDFLMK   DC    CL2'MK',CL20'MARKET RECORD        ',AL4(EDFLST)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(004),CL20'CODE                '                              
         DC    AL1(024),CL20'NAME                '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLST   DC    CL2'ST',CL20'STATION RECORD       ',AL4(EDFLCM)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(009),CL20'CODE                '                              
         DC    AL1(024),CL20'CITY                '                              
         DC    AL1(009),CL20'AFFILIATE           '                              
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
EDFLUC   DC    CL2'UC',CL20'USER-COMMENT DATA    ',AL4(EDFLNT)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(002),CL20'DATA LOCATION       '                              
         DC    AL1(020),CL20'DATA DESCRIPTION    '                              
         DC    AL1(032),CL20'DATA TEXT           '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLNT   DC    CL2'NT',CL20'CANADIAN NETW RECORD ',AL4(EDFLMT)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(004),CL20'CANADIAN NETWORK    '                              
         DC    AL1(008),CL20'MONTH OF SERVICE    '                              
         DC    AL1(001),CL20'COST TYPE           '                              
         DC    AL1(010),CL20'PREVIOUS INV NUMBER '                              
         DC    AL1(011),CL20'ORDERED GROSS       '                              
         DC    AL1(011),CL20'ORDERED NET         '                              
         DC    AL1(011),CL20'ORDERED TAX         '                              
         DC    AL1(011),CL20'ORDERED SPOTS       '                              
         DC    AL1(011),CL20'PREVIOUS GROSS      '                              
         DC    AL1(011),CL20'PREVIOUS NET        '                              
         DC    AL1(011),CL20'PREVIOUS TAX        '                              
         DC    AL1(011),CL20'PREVIOUS SPOTS      '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLMT   DC    CL2'MT',CL20'MONTHLY TOTAL RECORD ',AL4(EDFLAD)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(004),CL20'DETAIL LEVEL NAME   '                              
         DC    AL1(008),CL20'MONTH OF SERVICE    '                              
         DC    AL1(001),CL20'COST TYPE           '                              
         DC    AL1(010),CL20'PREVIOUS INV NUMBER '                              
         DC    AL1(011),CL20'ORDERED GROSS       '                              
         DC    AL1(011),CL20'ORDERED NET         '                              
         DC    AL1(011),CL20'ORDERED TAX         '                              
         DC    AL1(011),CL20'ORDERED SPOTS       '                              
         DC    AL1(011),CL20'PREVIOUS GROSS      '                              
         DC    AL1(011),CL20'PREVIOUS NET        '                              
         DC    AL1(011),CL20'PREVIOUS TAX        '                              
         DC    AL1(011),CL20'PREVIOUS SPOTS      '                              
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
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
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
EDFLTD   DC    CL2'TD',CL20'TOTAL DUE            ',AL4(EDFLXX)                  
*                                                                               
         DC    AL1(002),CL20'RECORD CODE         '                              
         DC    AL1(011),CL20'TOTAL DUE           '                              
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*                                                                               
EDFLXX   DC    X'FF'                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'**RQTAB*'                                                    
RQTAB    DS    0C                                                               
         ORG   *+(RQTABL*MAXREQS)                                               
RQTABX   EQU   *                                                                
*                                                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'**BRTAB*'                                                    
BRTAB    DS    0C                                                               
         ORG   *+(BRTABL*BRMAX)                                                 
BRTABX   EQU   *                                                                
*                                                                               
*                                                                               
WRKRBUFF CSECT                                                                  
         ORG   *+14336                                                          
         DC    X'00'                                                            
WRKRBFX  EQU   *-WRKRBUFF                                                       
                                                                                
***********************************************************************         
* DSECT FOR REQUEST TABLE                                                       
***********************************************************************         
RQTABD   DSECT   DSECT FOR REQUEST TABLE                                        
RQTSYS   DS    CL1                                                              
RQTMED   DS    CL1                                                              
RQTCLT   DS    CL3                                                              
RQTPRD   DS    CL3                                                              
RQTEST   DS    XL1                                                              
RQTSTA   DS    XL2     OLD COMPRESSED (BASE 1900)                               
RQTEND   DS    XL2     OLD COMPRESSED (BASE 1900)                               
RQTSTAN  DS    XL2     NEW COMPRESSED (BASE 1964)                               
RQTENDN  DS    XL2     NEW COMPRESSED (BASE 1964)                               
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
***********************************************************************         
* DSECT FOR BILL RECORD TRANSFER TABLE                                          
***********************************************************************         
BRTABD   DSECT                                                                  
BRTMED   DS    XL1                 AGENCY/MEDIA                                 
BRTCLT   DS    XL2                 CLIENT                                       
BRTINV   DS    XL2                 BILL NUMBER (INVOICE)                        
BRTBDT   DS    CL6                 BILL DATE (YYMMDD)                           
BRTABL   EQU   *-BRTABD                                                         
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFD                                                        
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPGENMSR                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENAGY                                                       
*                                                                               
         PRINT ON                                                               
         ORG   QAREA+49                                                         
QINVNO1  DS    CL4                                                              
QINVNO2  DS    CL4                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'148SPREPEB02 06/15/20'                                      
         END                                                                    
