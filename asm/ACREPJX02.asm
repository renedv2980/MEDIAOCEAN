*          DATA SET ACREPJX02  AT LEVEL 010 AS OF 03/23/04                      
*PHASE ACJX02A                                                                  
*INCLUDE ACJOBLOT                                                               
         TITLE 'JOB EXTRACT REPORT'                                             
ACJX02   CSECT                                                                  
BGN      DS    0X                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**ACJX**,RA                                                    
         L     R9,0(R1)                                                         
         USING ACWORKD,R9                                                       
         LA    RC,SPACEND                                                       
         USING ACJXD,RC                                                         
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                                                             
         CLI   MODE,ACCLAST                                                     
         BE    ACCL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
                                                                                
REQF     DS    0H                                                               
         MVC   INPARM,RCFFPARM                                                  
         ZAP   OKJOB,PZERO                                                      
         ZAP   ERJOB,PZERO                                                      
         ZAP   TNET,PZERO                                                       
         ZAP   TCOM,PZERO                                                       
         ZAP   TGRS,PZERO                                                       
*                                                                               
         LA    R1,RSNCNT           CLEAR REASON COUNT                           
         LA    R0,NRSNCNT                                                       
         ZAP   0(L'RSNCNT,R1),PZERO                                             
         LA    R1,L'RSNCNT(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,YES                                                     
         MVI   FCGETOPT,FCGETOWC                                                
*                                                                               
         L     R8,AMONACC                                                       
         USING ACMD,R8                                                          
*                                                                               
         MVC   JLACOMF,ADCOMFAC         COMFACS                                 
         MVC   JLAGETOP,GETOPT          GETOPT                                  
         MVC   JLAJOBR,ACMAJOBR         JOBBER                                  
         MVC   JLAJOBC,ACMAJOBL         JOB COL                                 
*                                                                               
         MVC   JLAGOBK,ADGOBLOC         GOBLOCK                                 
         MVC   JLACOLS,ACMACOLL         COLUMN LIST AREA                        
         MVC   JLACOLTB,ACMACOL         COLUMN OUTPUT TABLE                     
         MVC   JLLCOLTB,ACMLCOL         LENGTH OF COLUMN OUTPUT TABLE           
         MVC   JLAOPVTB,ACMAOPV         OPERAND VALUE TABLE                     
         MVC   JLLOPVTB,ACMLOPV         LENGTH OF OPERAND VALUE TABLE           
*                                                                               
         MVC   JLAEWC,=A(ESTWC)    ESTIMATE WORKCODES                           
         LA    RF,ESTWCL                                                        
         STCM  RF,15,JLLEWC                                                     
         MVC   JLAEWCB,=A(ESTWCB)  BILLABLE WORKCODES                           
         STCM  RF,15,JLLEWCB                                                    
         MVC   JLARETLB,=A(RETAILB) RETAIL BUFFER                               
         LHI   RF,RETAILL                                                       
         STCM  RF,15,JLLRETLB                                                   
*                                                                               
         LA    R1,OPTAB                                                         
         MVC   JLXRSTA,1(R1)                                                    
         CLC   QOPT1,0(R1)                                                      
         BE    *+16                                                             
         LA    R1,L'OPTAB(R1)                                                   
         CLI   0(R1),EOT                                                        
         BNE   *-24                                                             
*                                                                               
         MVI   JLFLTER,0                                                        
         CLI   QOPT3,C' '          REASON FILTER                                
         BE    *+10                                                             
         MVC   JLFLTER,QOPT3       RETURN JOBS WITH ERROR                       
*                                                                               
         CLI   QOPT6,C'Y'                                                       
         BNE   *+16                                                             
         LA    R1,ERRLIST         <-------- TEST                                
         STCM  R1,15,JLAERINC                                                   
         OI    JLAERINC,X'80'     MAKE IT AN EXCLUDE LIST                       
*                                                                               
         LA    R1,REASONS          A(RETURN ERROR LIST)                         
         STCM  R1,15,JLAERRL                                                    
         MVI   JLLERRL,L'REASONS                                                
*                                                                               
         MVI   FCRDTRNS,YES                                                     
         CLI   QOPT2,C'Y'          WILL JOBLOT READ TRANSACTIONS?               
         BNE   *+12                                                             
         MVI   FCRDTRNS,NO         JOBLOT WILL READ                             
         OI    JLOPTS,JLOPTRN      READ TRANSACTIONS                            
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R8                                                               
OPTAB    DS    0XL2                                                             
         DC    C' ',AL1(JLXRSEX+JLXRSWA+JLXRSXB)  ALL JOBS                      
         DC    C'B',AL1(JLXRSEX+JLXRSXB)          BILLING                       
         DC    C'N',AL1(JLXRSEX)                  NOT BILLABLE                  
         DC    C'W',AL1(JLXRSWA)                  WARNINGS                      
         DC    C'X',AL1(JLXRSEX+JLXRSWA+JLXRSXB)  ALL EXPT AND WARN             
         DC    AL1(EOT)                                                         
*                                                                               
ERRLIST  DC    AL1(03,08,00)                                                    
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
                                                                                
PACC     DS    0H                                                               
         MVI   FORCEHED,YES                                                     
         ZAP   JLBIGBAL,PZERO                                                   
         MVC   JLACLI,ADHEIRA                                                   
         MVC   JLAPRD,ADHEIRB                                                   
         MVC   JLAJOB,ADACC                                                     
         MVI   JLACTN,JLAPACF      PROCESS ACCOUNT FIRST                        
         OI    JLOPTS,JLOPERO      SET OLD ERROR CODES                          
*                                                                               
         MVC   REASONS,SPACES                                                   
         L     R0,=A(ESTWC)        ESTIMATE WORKCODES                           
         LA    R1,ESTWCL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,=A(ESTWCB)       BILLABLE WORKCODES                           
         LA    R1,ESTWCL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 ACJOBLOT,JLBLK                                                   
*                                                                               
PACC5    CLI   QOPT2,C'Y'          IS JOBLOT READING TRANSACTIONS ?             
         BNE   *+8                                                              
         BAS   RE,ACCL1                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTION                                                 *         
***********************************************************************         
                                                                                
PTRN     DS    0H                                                               
         CLI   QOPT2,C'Y'                                                       
         BE    XIT                                                              
         L     R3,ADTRANS                                                       
         USING TRNELD,R3                                                        
         LR    R2,R3                                                            
         SH    R2,DATADISP                                                      
         USING TRNRECD,R2                                                       
         TM    TRNRECD+ACCOSTAT,X'40' BYPASS DRAFT TRANSACTIONS                 
         BO    XIT                                                              
         TM    TRNSTAT,TRNSREV     SKIP REVERSALS                               
         BO    XIT                                                              
         STCM  R2,15,JLATRN                                                     
         MVI   JLACTN,JLAPTRN      PROCESS TRANSACTION                          
         GOTO1 ACJOBLOT,JLBLK                                                   
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ACCOUNT LAST                                                        *         
***********************************************************************         
                                                                                
ACCL     DS    0H                                                               
         CLI   QOPT2,C'Y'                                                       
         BE    XIT                                                              
         MVI   JLACTN,JLAPACL       PROCESS ACCOUNT LAST                        
         GOTO1 ACJOBLOT,JLBLK                                                   
         BAS   RE,ACCL1                                                         
         B     XIT                                                              
*                                                                               
ACCL1    NTR1  ,                                                                
         CLI   JLFLTYES,YES                                                     
         BNE   XIT                                                              
         USING JLBKD,JLAALLO                                                    
         CLI   JLBLCODE,JLBLPEST   % OF ESTIMATE BILL ?                         
         BNE   ACCL3                                                            
         LA    R1,JLBKD            CLEAR ALLOCATED                              
         LA    R0,JLBKRN                                                        
         ZAP   0(L'JLBK,R1),PZERO                                               
         LA    R1,L'JLBK(R1)                                                    
         BCT   R0,*-10                                                          
*                                                                               
         L     R3,JLAEWCB          ESTIMATED BILLABLE BY WC                     
         USING JLEWD,R3            ADD TO BILLABLE TOTALS                       
ACCL2    CLI   0(R3),EOT                                                        
         BE    ACCL3                                                            
         AP    JLBNET,JLEWNET      NET                                          
         AP    JLBGRS,JLEWGRS      GROSS                                        
         AP    JLBCOM,JLEWGRS      GROSS MINUS NET = COMMISSION                 
         SP    JLBCOM,JLEWNET                                                   
         LA    R3,JLEWLNQ(R3)                                                   
         B     ACCL2                                                            
         DROP  R3                                                               
*                                                                               
ACCL3    CLI   QOPT4,C' '          BILL TYPE FILTER                             
         BE    ACCL4                                                            
         MVC   BYTE,QOPT4                                                       
         CLC   JLBLCODE,BYTE                                                    
         BE    ACCL4                                                            
         TM    BYTE,X'40'          POSITIVE FILTER ?                            
         BO    XIT                                                              
         OI    BYTE,X'40'                                                       
         CLC   JLBLCODE,BYTE       TEST EXCLUDE FILTER                          
         BE    XIT                                                              
*                                                                               
ACCL4    CLI   QOPT1,C' '                                                       
         BE    ACCL6               ALL JOBS                                     
         OC    JLERRS,JLERRS       ERRORS OR WARNINGS                           
         BZ    ACCL5                                                            
         CLI   QOPT1,C'B'          BILLABLE OPTION ?                            
         BE    XIT                 CAN'T BILL                                   
         B     ACCL6               PRINT OTHERS WITH EXCEPTIONS                 
*                                                                               
ACCL5    CLI   QOPT1,C'B'          BILLABLE OPTION ?                            
         BNE   XIT                 NO,  AND NO ERRORS - SKIP IT                 
         CP    JLBNET,=P'0'                                                     
         BE    XIT                                                              
         CLI   JLBLCODE,JLBLUNBL                                                
         BE    XIT                                                              
         AP    TNET,JLBNET         TOTAL BILLABLE                               
         AP    TCOM,JLBCOM                                                      
         AP    TGRS,JLBGRS                                                      
*                                                                               
ACCL6    LA    R1,OKJOB                                                         
         OC    JLERRS,JLERRS                                                    
         BZ    *+8                                                              
         LA    R1,ERJOB                                                         
         AP    0(L'OKJOB,R1),=P'1'                                              
         ZAP   DUB,OKJOB                                                        
         AP    DUB,ERJOB                                                        
         CP    DUB,PZERO           FIRST TIME ?                                 
         BNE   ACCL8                                                            
*                                                                               
         USING JLTXD,R6                                                         
         ICM   R6,15,JLADTXT                                                    
ACCL7    MVC   P+10(L'JLTXLONG),JLTXLONG   PRINT ALL ERROR MESSAGES             
         GOTO1 ACREPORT                                                         
         LA    R6,JLTXLNQ(R6)                                                   
         CLI   0(R6),EOT                                                        
         BNE   ACCL7                                                            
         MVI   FORCEHED,YES                                                     
         DROP  R6                                                               
*                                                                               
ACCL8    BAS   RE,HEAD                                                          
         LA    R0,PSSEC            MOVES CONSTANTS PRINT AREAS                  
         LA    R1,PLNQ                                                          
         L     RE,=A(PSC)                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING PSC,PSSEC           STATUS SECTION                               
         MVC   PSBTYPE,JLBLNME      BILL TYPE                                   
         CLI   JLBLCODE,JLBLPEST                                                
         BNE   ACCL17                                                           
         MVC   PSCYC,=C'CYCLE'                                                  
         EDIT  (B1,JLCYCLE),(1,PSCYCN)                                          
*                                                                               
*                                                                               
ACCL17   GOTO1 DATCON,DMCB,(1,JLOPNDT),(8,PSOPNDTE)                             
         GOTO1 DATCON,DMCB,(1,JLLPSTDT),(8,PSACTDTE)                            
         GOTO1 DATCON,DMCB,(1,JLCLSDT),(8,PSCLSDTE)                             
*                                                                               
         LA    RF,PSREV                                                         
         CLI   JLHIAPP,0           HIGH APPROVED REVISION                       
         BE    ACCL19                                                           
         MVC   0(17,RF),=C'HIGH APPRV REV. ='                                   
         EDIT  (B1,JLHIAPP),(2,17(RF)),ALIGN=LEFT                               
         LA    RF,12(RF)                                                        
*                                                                               
ACCL19   CLC   JLHIREV,JLHIAPP                                                  
         BE    ACCL21                                                           
         CLI   JLHIREV,0                                                        
         BE    ACCL21                                                           
         MVC   0(15,RF),=C'HIGH REVISION ='                                     
         EDIT  (B1,JLHIREV),(2,15(RF)),ALIGN=LEFT                               
*                                                                               
ACCL21   OC    JLERRS,JLERRS                                                    
         BZ    ACCL27                                                           
         MVC   PSUNBL,=C'UNBILLABLE REASON CODES ='                             
         MVC   PSUNBLRS,REASONS                                                 
         LA    R1,RSNCNT           R1=A(REASON COUNTS)                          
         LA    R2,JLERRS           R2=A(ERROR BITS)                             
         LHI   R3,(L'JLERRS)/4     R3=NUMBER 0F SETS OF 4                       
*                                                                               
ACCL23   ICM   RF,15,0(R2)         RF=32 ERROR BITS                             
         LA    R0,32               R0=NUMBER OF BITS                            
*                                                                               
ACCL25   SR    RE,RE                                                            
         SLDL  RE,1                                                             
         LTR   RE,RE               TEST ERROR CONDITION                         
         BZ    *+10                NO ERROR                                     
         AP    0(L'RSNCNT,R1),=P'1'                                             
         LA    R1,L'RSNCNT(R1)                                                  
         BCT   R0,ACCL25                                                        
         LA    R2,4(R2)            R2=NEXT 32                                   
         BCT   R3,ACCL23                                                        
*                                                                               
ACCL27   SR    R0,R0               PRINT VARIOUS JOB STATUS BYTES               
         LA    R2,PSL1             R2=OUTPUT MESSAGE AREA                       
         LA    R1,STATAB                                                        
ACCL29   SR    R7,R7                                                            
         ICM   R7,3,0(R1)          R7=DISPLACEMENT TO JOB STATUS BYTE           
         LA    R7,JLBLK(R7)        RF=A(JOB STATUS BYTE)                        
         ICM   R3,15,2(R1)         R3=A(STATUS BYTE)                            
ACCL31   SR    RE,RE                                                            
         ICM   RE,1,0(R3)          RE=THE BIT TO TEST                           
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    0(R7),0                                                          
         BNO   ACCL33                                                           
         AHI   R0,1                COUNT STATUS MESSAGES                        
         CHI   R0,12                                                            
         BNH   *+6                                                              
         DC    H'0'                TOO MANY MESSAGES                            
         STC   R0,BYTE                                                          
         LA    R4,PSSTATL-PSL1(R2)                                              
         TM    BYTE,X'01'          ODD TO LEFT/EVEN TO RIGHT                    
         BO    *+12                                                             
         LA    R4,PSSTATR-PSL1(R2)                                              
         LA    R2,L'PSL1(R2)                                                    
         OC    1(2,R3),1(R3)                                                    
         BNZ   ACCL32                                                           
         XR    RF,RF                                                            
         ICM   RF,3,3(R3)                                                       
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
         B     ACCL33                                                           
*                                                                               
ACCL32   MVC   0(L'PSSTATL,R4),1(R3) MESSAGE TO PRINT AREA                      
*                                                                               
ACCL33   LA    R3,L'STATS(R3)                                                   
         CLI   0(R3),0                                                          
         BNE   ACCL31                                                           
         LA    R1,L'STATAB(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   ACCL29                                                           
*                                                                               
*                                                                               
         ZAP   NET,JLCUNEST        CURRENT ESTIMATE - NET                       
         ZAP   GROSS,JLCUGEST                         GROSS                     
         ZAP   COMM,GROSS                                                       
         SP    COMM,NET                               COMMISSION                
         LA    R6,PEAMT1                                                        
         LA    R5,NET                                                           
         LA    R3,3                                                             
         BAS   RE,EDITC                                                         
*                                                                               
         ZAP   NET,JLCUNOVR        CURRENT ESTIMATE * OVER PCT                  
         ZAP   GROSS,JLCUGOVR                                                   
         ZAP   COMM,GROSS                                                       
         SP    COMM,NET                                                         
         LA    R6,PEAMT2                                                        
         LA    R5,NET                                                           
         LA    R3,3                                                             
         BAS   RE,EDITC                                                         
*                                                                               
         ZAP   NET,JLHINEST        HIGH ESTIMATE                                
         ZAP   GROSS,JLHIGEST                                                   
         ZAP   COMM,GROSS                                                       
         SP    COMM,NET                                                         
         LA    R6,PEAMT3                                                        
         LA    R5,NET                                                           
         LA    R3,3                                                             
         BAS   RE,EDITC                                                         
*                                                                               
         ZAP   NET,JLHINOVR        HIGH ESTIMATE * OVER PCT                     
         ZAP   GROSS,JLHIGOVR                                                   
         ZAP   COMM,GROSS                                                       
         SP    COMM,NET                                                         
         LA    R6,PEAMT4                                                        
         LA    R5,NET                                                           
         LA    R3,3                                                             
         BAS   RE,EDITC                                                         
*                                                                               
         L     R8,ADGOBLOC             GOBLOCK                                  
         USING GOBLOCKD,R8                                                      
         EDIT  (P4,GOOVRAMT),(15,PEAMOVR),2,MINUS=YES,COMMAS=YES                
         EDIT  (P4,GOMINBIL),(15,PEMINBL),2,MINUS=YES,COMMAS=YES                
         EDIT  (P4,GOOVRPER),(15,PEOVRPCT),2,MINUS=YES,TRAIL=C'%'               
         DROP  R8                                                               
*                                                                               
         LA    R3,PBC                                                           
         LA    R4,BALTAB                                                        
         BAS   RE,EDIT                                                          
*                                                                               
         LA    R6,PDAMT1           R6=A(OUTPUT AREA)                            
         LA    R5,JLATOT           R5=A(BUCKETS)                                
         LA    R3,NPDL             R3=NUMBER PRINT DETAIL LINES                 
         BAS   RE,EDITC            EDIT TOTAL COLUMN                            
*                                                                               
         LA    R6,PDAMT2                                                        
         LA    R5,JLABILL                                                       
         LA    R3,NPDL                                                          
         BAS   RE,EDITC            EDIT BILLED COLUMN                           
*                                                                               
         LA    R1,JLAWOFF          ADD WRITEOFFS                                
         LA    R2,JLATRNX            TO TRANSFERS                               
         LA    R0,JLBKRN                                                        
         AP    0(L'JLBK,R2),0(L'JLBK,R1)                                        
         LA    R1,L'JLBK(R1)                                                    
         LA    R2,L'JLBK(R2)                                                    
         BCT   R0,*-14                                                          
*                                                                               
         LA    R6,PDAMT3           R6=A(OUTPUT AREA)                            
         LA    R5,JLATRNX                                                       
         LA    R3,NPDL                                                          
         BAS   RE,EDITC            EDIT WRITEOFFS/TRANSFERS                     
*                                                                               
         LA    R6,PDAMT4                                                        
         LA    R5,JLAALLO                                                       
         LA    R3,NPDL                                                          
         BAS   RE,EDITC            EDIT ALLOCATED(BILLABLE)                     
*                                                                               
ACCL35   CLI   INPSTA,YES                                                       
         BNE   *+16                                                             
         LA    R2,PSSEC            PRINT STATUS SECTION                         
         LA    R0,PSSN                                                          
         BAS   RE,RPT                                                           
         CLI   INPEST,YES                                                       
         BNE   *+16                                                             
         LA    R2,PESEC            PRINT ESTIMATE SECTION                       
         LA    R0,PESN                                                          
         BAS   RE,RPT                                                           
         CLI   INPBAL,YES                                                       
         BNE   *+16                                                             
         LA    R2,PBSEC            PRINT BALANCE SECTION                        
         LA    R0,PBSN                                                          
         BAS   RE,RPT                                                           
         CLI   INPDET,YES                                                       
         BNE   *+16                                                             
         LA    R2,PDSEC            PRINT DETAIL SECTION                         
         LA    R0,PDSN                                                          
         BAS   RE,RPT                                                           
         CLI   INPRTL,YES                                                       
         BNE   *+8                                                              
         BAS   RE,PRTL             PRINT REATIL SECTION                         
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* PRINT RETAIL DETAILS                                               *          
**********************************************************************          
                                                                                
PRTL     NTR1  ,                                                                
         TM    JLSTAT1,JLS1DIST    TEST RETAIL DISTRIBUTION                     
         BNO   XIT                                                              
         L     R5,JLARETLB                                                      
         SR    R3,R3                                                            
PRTL3    CLI   0(R5),EOT                                                        
         BE    XIT                                                              
         USING RTLD,R5                                                          
         LTR   R3,R3                                                            
         BNZ   PRTL5                                                            
         L     RF,=A(PRTLH)                                                     
         MVC   P(L'PRTLH),0(RF)    FIRST TIME SOME HEADLINES                    
         GOTO1 ACREPORT                                                         
*                                                                               
PRTL5    LA    R4,P                                                             
         USING PRTLD,R4                                                         
         MVC   PRTLDIS,RTLDIS+1    DISTRIBUTOR                                  
         MVC   PRTLRECV,RTLRECV+1  RECEIVABLE                                   
         MVC   PRTLCOST,RTLCOST+1  COSTING                                      
         EDIT  RTLPCT,(L'PRTLPCT,PRTLPCT),4,ALIGN=RIGHT                         
         TM    RTLSTAT,RTLCOMM     TEST SPECIAL COMMISSION RATE                 
         BNO   PRTL7                                                            
         EDIT  RTLCRATE,(L'PRTLCRAT,PRTLCRAT),4,ALIGN=RIGHT                     
PRTL7    GOTO1 ACREPORT                                                         
         LA    R3,1(R3)                                                         
         LA    R5,RTLLNQ(R5)                                                    
         B     PRTL3                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
                                                                                
REQL     DS    0H                                                               
         MVI   FORCEHED,YES        PRINT REASONS WITH ERRORS                    
         LA    R2,RSNCNT           R2=REASON COUNT                              
         LA    R3,NRSNCNT          R3=NUMBER OF REASONS                         
         LA    R4,1                R4=ERROR NUMBER                              
*                                                                               
REQL3    CP    0(L'RSNCNT,R2),PZERO   SKIP IF NO ERRORS                         
         BE    REQL7                                                            
         EDIT  (P4,0(R2)),(5,P+74)    NUMBER OF ERRORS                          
         LR    R5,R4                                                            
         BCTR  R5,0                                                             
         MHI   R5,JLTXLNQ          GET TEXT FOR THIS ERROR                      
         L     R6,JLADTXT                                                       
         AR    R6,R5                                                            
         USING JLTXD,R6                                                         
         MVC   P+1(L'JLTXLONG),JLTXLONG                                         
         GOTO1 ACREPORT                                                         
         DROP  R6                                                               
*                                                                               
REQL7    AHI   R4,1                                                             
         LA    R2,L'RSNCNT(R2)                                                  
         BCT   R3,REQL3                                                         
*                                                                               
         GOTO1 ACREPORT                                                         
         EDIT  ERJOB,(14,P+1)                                                   
         MVC   P+20(20),=CL20'JOBS WITH ERRORS'                                 
         GOTO1 ACREPORT                                                         
         EDIT  OKJOB,(14,P+1)                                                   
         MVC   P+20(20),=CL20'JOBS WITHOUT ERRORS'                              
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         CLI   QOPT1,C'B'                                                       
         BNE   XIT                                                              
         EDIT  TNET,(15,P+1),2,MINUS=YES,COMMAS=YES                             
         MVC   P+20(20),=CL20'BILLABLE NET'                                     
         GOTO1 ACREPORT                                                         
         EDIT  TCOM,(15,P+1),2,MINUS=YES,COMMAS=YES                             
         MVC   P+20(20),=CL20'BILLABLE COMMISSION'                              
         GOTO1 ACREPORT                                                         
         EDIT  TGRS,(15,P+1),2,MINUS=YES,COMMAS=YES                             
         MVC   P+20(20),=CL20'BILLABLE GROSS'                                   
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT HOOKS                                                         *         
***********************************************************************         
                                                                                
RTLDHK   MVC   0(17,R4),=C'RETAIL SCHEME=XXX'                                   
         L     R8,ADGOBLOC             GOBLOCK                                  
         USING GOBLOCKD,R8                                                      
         MVC   14(3,R4),GODIST                                                  
         BR    RE                                                               
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* HEAD UP ROUTINE                                                     *         
***********************************************************************         
                                                                                
HEAD     NTR1  ,                                                                
         MVC   HEAD4+1(7),=C'COMPANY'                                           
         L     R2,ADCMPNAM                                                      
         BAS   RE,GETNAM                                                        
         MVC   HEAD4+10(L'NAMEREC),WORK      COMPANY NAME                       
         MVC   HEAD5+1(6),=C'CLIENT'                                            
         L     R2,ADHEIRA                                                       
         MVC   HEAD5+10(5),3(R2)             CLIENT CODE                        
         L     R2,ADLVANAM                                                      
         BAS   RE,GETNAM                                                        
         MVC   HEAD5+17(L'NAMEREC),WORK      CLIENT NAME                        
         MVC   HEAD6+1(7),=C'PRODUCT'                                           
         L     R2,ADHEIRB                                                       
         MVC   HEAD6+10(5),6(R2)             PRODUCT CODE                       
         L     R2,ADLVBNAM                                                      
         BAS   RE,GETNAM                                                        
         MVC   HEAD6+17(L'NAMEREC),WORK      PRODUCT NAME                       
         MVC   HEAD7+1(3),=C'JOB'                                               
         L     R2,ADACC                                                         
         MVC   HEAD7+10(6),9(R2)             JOB CODE                           
         L     R2,ADACCNAM                                                      
         BAS   RE,GETNAM                                                        
         MVC   HEAD7+17(L'NAMEREC),WORK      JOB NAME                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* EDIT ROUTINES                                                       *         
***********************************************************************         
                                                                                
EDIT     ST    RE,SAVRE                                                         
EDIT3    SR    R5,R5                                                            
         ICM   R5,3,0(R4)          R5=DISP. TO ACCUM                            
         LA    R5,JLBLK(R5)        R5=A(ACCUM)                                  
         SR    R6,R6                                                            
         ICM   R6,3,2(R4)          R6=DISP. TO OUTPUT AREA                      
         AR    R6,R3               R6=A(OUTPUT AREA)                            
         EDIT  (P6,0(R5)),(15,0(R6)),2,MINUS=YES,COMMAS=YES                     
         LA    R4,L'BALTAB(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   EDIT3                                                            
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
EDITC    ST    RE,SAVRE            EDIT A COLUMN                                
EDITC3   DS    0H                                                               
*EDITC3   CP    0(L'JLBK,R5),PZERO                                              
*         BE    EDITC5                                                          
         EDIT  (P6,0(R5)),(15,0(R6)),2,MINUS=YES,COMMAS=YES                     
EDITC5   LA    R6,L'PDL1(R6)                                                    
         LA    R5,L'JLBK(R5)                                                    
         BCT   R3,EDITC3                                                        
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
EDITO    ST    RE,SAVRE            EDIT ONE NUMBER                              
*        CP    0(L'JLBK,R5),PZERO                                               
*        BER   RE                                                               
         EDIT  (P6,0(R5)),(15,0(R6)),2,MINUS=YES,COMMAS=YES                     
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER ROUTINES                                                      *         
***********************************************************************         
                                                                                
         GETEL R3,DATADISP,ELCODE                                               
*                                                                               
         USING NAMELD,R2                                                        
GETNAM   MVC   WORK,SPACES                                                      
         XR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         AHI   R1,-(3)                                                          
         EX    R1,*+4                                                           
         MVC   WORK(0),NAMEREC                                                  
         BR    RE                                                               
         DROP  R2                                                               
*                                                                               
RPT      ST    RE,SAVRE                                                         
RPT1     CLC   0(L'PSL1,R2),SPACES                                              
         BE    RPT3                                                             
         MVC   P,0(R2)                                                          
         GOTO1 ACREPORT                                                         
RPT3     LA    R2,L'PSL1(R2)                                                    
         BCT   R0,RPT1                                                          
         GOTO1 ACREPORT                                                         
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERAL POOL                                          *         
***********************************************************************         
                                                                                
ACJOBLOT DC    V(ACJOBLOT)                                                      
*                                                                               
STATAB   DS    0XL6                                                             
         DC    AL2(JLSTAT1-JLBLK),AL4(STAT1)                                    
         DC    AL2(JLSTAT2-JLBLK),AL4(STAT2)                                    
         DC    AL2(JLSTAT3-JLBLK),AL4(STAT3)                                    
         DC    AL2(JLSTAT4-JLBLK),AL4(STAT4)                                    
         DC    AL2(JLSTAT5-JLBLK),AL4(STAT5)                                    
         DC    X'FF'                                                            
*                                                                               
STATS    DS    0XL21                                                            
STAT1    DC    AL1(JLS1NCLB),CL20'NON-CLIENT BILLS'                             
         DC    AL1(JLS1CLB),CL20'CLIENT BILLS'                                  
         DC    AL1(JLS1DIST),AL2(0),AL2(RTLDHK-BGN),XL16'00'                    
         DC    AL1(JLS1RET),CL20'RETAIL BILLING'                                
         DC    AL1(JLS1APPV),CL20'APPROVED ESTIMATE'                            
         DC    AL1(JLS1HELD),CL20'HELD INVOICES'                                
         DC    AL1(JLS1MUSR),CL20'MISSING USER FIELDS'                          
         DC    AL1(JLS1UNAR),CL20'UNAPPROVED REVISIONS'                         
         DC    AL1(0)                                                           
*                                                                               
STAT2    DC    AL1(JLS2BLSN),CL20'BILL SELECT = NO'                             
         DC    AL1(JLS2STUD),CL20'STUDIO JOB'                                   
         DC    AL1(JLS2LKAG),CL20'AGENCY LINK DEFINED'                          
         DC    AL1(JLS2STUD),CL20'VALID STUDIO TYPE'                            
         DC    AL1(0)                                                           
*                                                                               
STAT3    DC    AL1(JLS3ESRQ),CL20'ESTIMATE IS REQUIRED'                         
         DC    AL1(JLS3NEWS),CL20'NEW ESTIMATES'                                
         DC    AL1(JLS3NTOB),CL20'NEED NEW EST TO BILL'                         
         DC    AL1(JLS3UOLD),CL20'UNAPPROVED ''OLD'' EST'                       
         DC    AL1(0)                                                           
*                                                                               
STAT4    DC    AL1(JLS4LOCK),CL20'JOB IS LOCKED'                                
         DC    AL1(JLS4CLSD),CL20'JOB IS CLOSED'                                
         DC    AL1(JLS4BILL),CL20'JOB HAS BILLING'                              
         DC    AL1(0)                                                           
*                                                                               
STAT5    DC    AL1(JLS5AUTO),CL20'AUTO CYCLE'                                   
         DC    AL1(JLS5NCYC),CL20'NO CYCLE RECORD'                              
         DC    AL1(JLS5CYCS),CL20'CYCLE SEQUENCE ERROR'                         
         DC    AL1(0)                                                           
*                                                                               
BALTAB   DS    0XL4                                                             
         DC    AL2(JLDBTS-JLBLK),AL2(PBDBTS-PBC)                                
         DC    AL2(JLCRTS-JLBLK),AL2(PBCRTS-PBC)                                
         DC    AL2(JLBALN-JLBLK),AL2(PBBALNC-PBC)                               
         DC    AL2(JLAONET-JLBLK),AL2(PBNORDRS-PBC)                             
         DC    AL2(JLAOGRS-JLBLK),AL2(PBGORDRS-PBC)                             
         DC    X'FF'                                                            
*                                                                               
UNDRLNE  DC   C'-------------------'                                            
*                                                                               
PZERO    DC   PL1'0'                                                            
*                                                                               
         LTORG                                                                  
ESTWCN   EQU   200                                                              
ESTWCL   EQU   (ESTWCN*JLEWLNQ)                                                 
ESTWC    DS    (ESTWCL)X                                                        
ESTWCB   DS    (ESTWCL)X                                                        
RETAILN  EQU   50                                                               
RETAILL  EQU   (RTLLNQ*RETAILN)                                                 
RETAILB  DS    (RETAILL)X                                                       
         EJECT                                                                  
***********************************************************************         
* DSECT FOR LOCAL STORAGE                                             *         
***********************************************************************         
                                                                                
ACJXD    DSECT                                                                  
PSSEC    DS    CL(PSLNQ)           STATUS SECTION                               
PSSN     EQU   (*-PSSEC)/L'PSL1                                                 
PESEC    DS    CL(PELNQ)           ESTIMATE SECTION                             
PESN     EQU   (*-PESEC)/L'PSL1                                                 
PBSEC    DS    CL(PBLNQ)           BALANCE SECTION                              
PBSN     EQU   (*-PBSEC)/L'PSL1                                                 
PDSEC    DS    CL(PDLNQ)           DETAIL SECTION                               
PDSN     EQU   (*-PDSEC)/L'PSL1                                                 
PLNQ     EQU   *-PSSEC                                                          
*                                                                               
OKJOB    DS    PL4                 JOBS WITHOUT ERRORS                          
ERJOB    DS    PL4                 JOBS WITH ERRORS                             
REASONS  DS    CL(L'PSUNBLRS)                                                   
*                                                                               
TNET     DS    PL6                 BILLABLE - NET                               
TCOM     DS    PL6                          - COMMISSION                        
TGRS     DS    PL6                          - GROSS                             
*                                                                               
RSNCNT   DS    (L'JLERRS*8)PL4      REASON COUNT                                
NRSNCNT  EQU   (*-RSNCNT)/L'RSNCNT                                              
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   X'FF'                                                            
ALL      EQU   X'FF'                                                            
*                                                                               
ELCODE   DS    XL1                                                              
SAVRE    DS    F                                                                
*                                                                               
NET      DS    PL6                                                              
COMM     DS    PL6                                                              
GROSS    DS    PL6                                                              
*                                                                               
INPARM   DS    CL5                                                              
         ORG   INPARM                                                           
INPSTA   DS    CL1                 PRINT STATUS SECTION                         
INPEST   DS    CL1                 PRINT ESTIMATE SECTION                       
INPBAL   DS    CL1                 PRINT BALANCE SECTION                        
INPDET   DS    CL1                 PRINT DETAIL SECTION                         
INPRTL   DS    CL1                 PRINT RETAIL SECTION                         
         EJECT                                                                  
       ++INCLUDE ACJOBLOTD                                                      
         EJECT                                                                  
***********************************************************************         
* REPORT SECTIONS                                                     *         
***********************************************************************         
                                                                                
PSC      CSECT ,                   CSECT FOR STATUS SECTION                     
PSL1     DC    CL132' '            LINE 1                                       
         ORG   PSL1                                                             
         DS    C                                                                
PSBTYPE  DS    CL15                BILL TYPE                                    
         DS    C                                                                
PSCYC    DS    CL5                 'CYCLE'                                      
         DS    C                                                                
PSCYCN   DS    C                   CYCLE NUMBER                                 
         DS    CL21                                                             
PSOPN    DC    CL9'OPEN DATE'                                                   
         DS    CL2                                                              
PSOPNDTE DS    CL8                 OPEN DATE                                    
         DS    CL21                                                             
PSSTATL  DS    CL20                STATUS - LEFT SIDE                           
         DS    CL6                                                              
PSSTATR  DS    CL20                         RIGHT SIDE                          
         ORG   PSL1+L'PSL1                                                      
*                                                                               
PSL2     DC    CL132' '            LINE 2                                       
         ORG   PSL2                                                             
         ORG   PSL2+(PSOPN-PSL1)                                                
         DC    CL9'LAST ACTV'                                                   
         DS    CL2                                                              
PSACTDTE DS    CL8                 LAST ACTIVITY DATE                           
         ORG   PSL2+(PSSTATL-PSL1)                                              
         DS    CL(L'PSSTATL)                                                    
         ORG   PSL2+(PSSTATR-PSL1)                                              
         DS    CL(L'PSSTATR)                                                    
         ORG   PSL2+L'PSL2                                                      
*                                                                               
PSL3     DC    CL132' '            LINE 3                                       
         ORG   PSL3                                                             
         DS    C                                                                
PSREV    DS    CL43                REVISION NUMBERS                             
         DS    C                                                                
         DC    CL10'CLOSE DATE'                                                 
         DS    C                                                                
PSCLSDTE DS    CL8                 CLOSE DATE                                   
         ORG   PSL3+(PSSTATL-PSL1)                                              
         DS    CL(L'PSSTATL)                                                    
         ORG   PSL3+(PSSTATR-PSL1)                                              
         DS    CL(L'PSSTATR)                                                    
         ORG   PSL3+L'PSL3                                                      
*                                                                               
PSL4     DC    CL132' '                                                         
         ORG   PSL4                                                             
         DS    C                                                                
PSUNBL   DS    CL25                'UNBILLABLE REASON CODES ='                  
         DS    C                                                                
PSUNBLRS DS    CL25                                                             
         ORG   PSL4+(PSSTATL-PSL1)                                              
         DS    CL(L'PSSTATL)                                                    
         ORG   PSL4+(PSSTATR-PSL1)                                              
         DS    CL(L'PSSTATR)                                                    
         ORG   PSL4+L'PSL4                                                      
*                                                                               
PSL5     DC    CL132' '                                                         
         ORG   PSL5+(PSSTATL-PSL1)                                              
         DS    CL(L'PSSTATL)                                                    
         ORG   PSL5+(PSSTATR-PSL1)                                              
         DS    CL(L'PSSTATR)                                                    
         ORG   PSL5+L'PSL5                                                      
*                                                                               
PSL6     DC    CL132' '                                                         
         ORG   PSL6+(PSSTATL-PSL1)                                              
         DS    CL(L'PSSTATL)                                                    
         ORG   PSL6+(PSSTATR-PSL1)                                              
         DS    CL(L'PSSTATR)                                                    
         ORG   PSL6+L'PSL6                                                      
PSLNQ    EQU   *-PSL1                                                           
         EJECT                                                                  
PEC      DS    0C                  ESTIMATE SECTION                             
PEL1     DC    CL132' '                                                         
         ORG   PEL1                                                             
         DS    CL13                                                             
         DC    CL15'       CURRENT'                                             
         DS    CL2                                                              
         DC    CL15'     CURRENT *'                                             
         DS    CL2                                                              
         DC    CL15'        HIGH'                                               
         DS    CL2                                                              
         DC    CL15'        HIGH *'                                             
         ORG   PEL1+L'PEL1                                                      
*                                                                               
PEL2     DC    CL132' '                                                         
         ORG   PEL2                                                             
         DS    CL13                                                             
         DC    CL15'      ESTIMATE'                                             
         DS    CL2                                                              
         DC    CL15'       OVER %'                                              
         DS    CL2                                                              
         DC    CL15'      ESTIMATE'                                             
         DS    CL2                                                              
         DC    CL15'       OVER %'                                              
         ORG   PEL2+L'PEL2                                                      
*                                                                               
PEL3     DC    CL132' '                                                         
         ORG   PEL3                                                             
         DS    C                                                                
         DC    CL11'NET'                                                        
         DS    C                                                                
PEAMT1   DS    CL15                                                             
         DS    CL2                                                              
PEAMT2   DS    CL15                                                             
         DS    CL2                                                              
PEAMT3   DS    CL15                                                             
         DS    CL2                                                              
PEAMT4   DS    CL15                                                             
PEAMTX   DS    CL3                                                              
         DC    CL14'AMOUNT OVER  ='                                             
PEAMOVR  DS    CL15                                                             
         ORG   PEL3+L'PEL3                                                      
*                                                                               
PEL4     DC    CL132' '                                                         
         ORG   PEL4                                                             
         DS    C                                                                
         DC    CL11'COMMISSION'                                                 
         ORG   PEL4+(PEAMTX-PEL3)                                               
         DS    CL3                                                              
         DC    CL14'MINIMUM BILL ='                                             
PEMINBL  DS    CL15                                                             
         ORG   PEL4+L'PEL4                                                      
*                                                                               
PEL5     DC    CL132' '                                                         
         ORG   PEL5                                                             
         DS    C                                                                
         DC    CL11'GROSS'                                                      
         ORG   PEL5+(PEAMTX-PEL3)                                               
         DS    CL3                                                              
         DC    CL15'* OVER PCT   ='                                             
PEOVRPCT DS    CL15                                                             
         DS    C                                                                
         ORG   PEL5+L'PEL5                                                      
*                                                                               
PELNQ    EQU   *-PEL1                                                           
         EJECT                                                                  
PBC      DS    0C                  BALANCE SECTION                              
PBL1     DC    CL132' '                                                         
         ORG   PBL1                                                             
         DS    CL4                                                              
         DC    CL9'DEBITS  ='                                                   
PBDBTS   DS    CL15                                                             
         DS    CL5                                                              
         DC    CL14'NET ORDERS   ='                                             
PBNORDRS DS    CL15                                                             
         ORG   PBL1+L'PBL1                                                      
*                                                                               
PBL2     DC    CL132' '                                                         
         ORG   PBL2                                                             
         DS    CL4                                                              
         DC    CL9'CREDITS ='                                                   
PBCRTS   DS    CL15                                                             
         DS    CL5                                                              
         DC    CL14'GROSS ORDERS ='                                             
PBGORDRS DS    CL15                                                             
         ORG   PBL2+L'PBL2                                                      
*                                                                               
PBL3     DC    CL132' '                                                         
         ORG   PBL3                                                             
         DS    CL4                                                              
         DC    CL9'BALANCE ='                                                   
PBBALNC  DS    CL15                                                             
         ORG   PBL3+L'PBL3                                                      
PBLNQ    EQU   *-PBL1                                                           
         EJECT                                                                  
PDC      DS    0C                  DETAIL SECTION                               
PDL1     DC    CL132' '                                                         
         ORG   PDL1                                                             
         DS    CL13                                                             
         DC    CL15'        TOTAL'                                              
         DS    CL2                                                              
         DC    CL15'       BILLED'                                              
         DS    CL2                                                              
         DC    CL15'    WRITEOFFS/'                                             
         DS    CL2                                                              
         DC    CL15'      BILLABLE'                                             
         ORG   PDL1+L'PDL1                                                      
*                                                                               
PDL2     DC    CL132' '                                                         
         ORG   PDL2                                                             
         DS    CL13                                                             
         DC    CL15'     ---------'                                             
         DS    CL2                                                              
         DC    CL15'     ---------'                                             
         DS    CL2                                                              
         DC    CL15'     TRANSFERS'                                             
         DS    CL2                                                              
         DC    CL15'     ---------'                                             
         ORG   PDL2+L'PDL2                                                      
*                                                                               
PDL3     DC    CL132' '                                                         
         ORG   PDL3                                                             
         DS    C                                                                
         DC    CL11'NET'                                                        
         DS    C                                                                
PDAMT1   DS    CL15                                                             
         DS    CL2                                                              
PDAMT2   DS    CL15                                                             
         DS    CL2                                                              
PDAMT3   DS    CL15                                                             
         DS    CL2                                                              
PDAMT4   DS    CL15                                                             
         DS    CL2                                                              
PDAMT5   DS    CL15                                                             
         DS    CL2                                                              
PDAMT6   DS    CL15                                                             
         DS    CL2                                                              
PDAMT7   DS    CL15                                                             
         ORG   PDL3+L'PDL3                                                      
*                                                                               
PDL4     DC    CL132' '                                                         
         ORG   PDL4                                                             
         DS    C                                                                
         DC    CL11'COMMISSION'                                                 
         DS    C                                                                
         ORG   PDL4+L'PDL4                                                      
*                                                                               
PDL5     DC    CL132' '                                                         
         ORG   PDL5                                                             
         DS    C                                                                
         DC    CL11'GROSS'                                                      
         DS    C                                                                
         ORG   PDL5+L'PDL5                                                      
*                                                                               
PDL6     DC    CL132' '                                                         
         ORG   PDL6                                                             
         DS    C                                                                
         DC    CL11'CASH DISC.'                                                 
         DS    C                                                                
         ORG   PDL6+L'PDL6                                                      
*                                                                               
PDL7     DC    CL132' '                                                         
         ORG   PDL7                                                             
         DS    C                                                                
         DC    CL11'HOURS'                                                      
         DS    C                                                                
         ORG   PDL7+L'PDL7                                                      
*                                                                               
PDLNQ    EQU   *-PDL1                                                           
NPDL     EQU   (*-PDL3)/L'PDL3                                                  
         EJECT                                                                  
PRTLH    DS     0CL(L'P)                                                        
         DC     C' '                                                            
         DC     CL14'DISTRIBUTOR'                                               
         DC     C' '                                                            
         DC     CL14'RECEIVABLE'                                                
         DC     C' '                                                            
         DC     CL14'COSTING'                                                   
         DC     C' '                                                            
         DC     CL8'  PCT.  '                                                   
         DC     C' '                                                            
         DC     CL8'  RATE  '                                                   
         DC     (L'P-(*-PRTLH))C' '                                             
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER RETAIL PRINT LINE                                   *          
**********************************************************************          
                                                                                
PRTLD    DSECT                                                                  
         DS    CL1                                                              
PRTLDIS  DS    CL14          DISTRIBUTOR                                        
         DS    CL1                                                              
PRTLRECV DS    CL14          RECEIVABLE                                         
         DS    CL1                                                              
PRTLCOST DS    CL14          COSTING                                            
         DS    CL1                                                              
PRTLPCT  DS    CL8           PERCENT                                            
         DS    CL1                                                              
PRTLCRAT DS    CL8           COMMISSION RATE                                    
         EJECT                                                                  
         PRINT ON                                                               
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
* COMFACSD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACSD                                                     
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
GOBLOCKD DSECT                                                                  
* ACGOBLOCK                                                                     
        PRINT OFF                                                               
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE ACGOXBLOCK                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACREPJX02 03/23/04'                                      
         END                                                                    
