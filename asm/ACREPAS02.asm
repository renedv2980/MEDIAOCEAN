*          DATA SET ACREPAS02  AT LEVEL 015 AS OF 04/21/99                      
*PHASE ACAS02A,*                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'ACREPAS02 - FAST SALARY UPDATE PROGRAM'                         
**********************************************************************          
* REVISION HISTORY                                                   *          
* 10/95 JPS - CHANGED LCNY ROUTINE TO ACCOMMODATE NEW TAPE FORMAT    *          
*             LCNY SWITCHED TO IPG                                   *          
* 03/96 JPS - SWITCH KPNY TO AAS AND ADDED KPSF TO TABLE             *          
* 12/98 JPS - SWITCH HRSF TO ACQ FORMAT                              *          
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
* THIS PROGRAM READS USER PAYROLL FILES AND CONVERTS TO DDS ACCPAK   *          
*        OPTION 1 = BASIS                                            *          
*        OPTION 2 = Y, WRITE ZERO SALARIES                           *          
*        OPTION 3 = Y, OVERLAY IS HANDLEING THE REPORT, NOT BASE     *          
**********************************************************************          
         EJECT                                                                  
ACAS02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,ACAS02,R8                                                      
         USING ACWORKD,RA                                                       
         L     RA,0(R1)                                                         
         USING ACASD,RC                                                         
         LA    RC,SPACEND                                                       
         USING MANFRECD,R9                                                      
         LA    R9,MANAREA                                                       
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST - INITIALIZE WORKING STORAGE                         *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         L     R1,=A(BASERC)                                                    
         ST    RC,0(R1)                                                         
         L     R1,=A(BASEHOOK)                                                  
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R2,BUCKS                                                         
         LA    R1,NBUCKS                                                        
         ZAP   0(6,R2),=P'0'                                                    
         LA    R2,6(R2)                                                         
         BCT   R1,*-10                                                          
*                                                                               
         MVI   EOF,C' '                                                         
         MVI   ZEROOPT,C'N'                                                     
         LA    R2,MANAREA                                                       
         LM    R3,R5,=A(MANLNQ,0,C' ')                                          
         SLL   R5,24               SHIFT SPACE TO HIGH, ZERO THE REST           
         MVCL  R2,R4               (R4 NOT REFERENCED, LENGTH IS ZERO)          
*                                                                               
         LA    R1,IOAREA                                                        
         ST    R1,AIOAREA                                                       
         LA    R1,IOAREA2                                                       
         ST    R1,AIOAREA2                                                      
         L     R1,=A(PAGELIST)                                                  
         ST    R1,PAGES                                                         
         ST    R1,APAGES                                                        
*                                                                               
         LA    R2,MBUCKS                                                        
         LA    R1,MNBUCKS                                                       
         ZAP   0(6,R2),=P'0'                                                    
         LA    R2,6(R2)                                                         
         BCT   R1,*-10                                                          
*                                                                               
* GET ADDRESS OF TAPE ROUTINE FROM AGYTAB                                       
*                                                                               
         LA    R5,AGYTAB                                                        
REQF10   CLC   0(2,R5),ORIGINUM    GET A(THIS AGENCIES ROUTINE)                 
         BE    REQF20                                                           
         CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                THIS COMPANY NOT ON TABLE                    
         LA    R5,6(R5)                                                         
         B     REQF10                                                           
*                                                                               
REQF20   MVC   IRADD,2(R5)         GOOD ONE, SAVE I/P RTN ADDRESS               
*                                                                               
* VALIDATE BASIS IN REQUEST CARD                                                
*                                                                               
         SR    R2,R2                                                            
         MVC   BASIS,SPACES                                                     
         LA    R1,BASISTB                                                       
REQF30   CLI   0(R1),X'FF'                                                      
         BE    BADCC                                                            
         IC    R2,0(R1)            LENGTH FOR EXECUTED MVC                      
         CLC   1(1,R1),QOPT1                                                    
         BE    *+12                                                             
         LA    R1,2(R2,R1)                                                      
         B     REQF30                                                           
*                                                                               
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   BASIS(0),1(R1)                                                   
*                                                                               
*  INITIALIZE REQUEST OPTION TO SHOW ADD NEW EMPLOYEES                          
*  WITH ZERO SALARIES                                                           
*                                                                               
         CLI   QOPT2,C'Y'          WRITE ZERO SALARIES                          
         BNE   *+8                                                              
         MVI   ZEROOPT,C'Y'                                                     
         CLI   QOPT2,C'S'                                                       
         BNE   *+8                                                              
         MVI   ZEROOPT,C'S'                                                     
*                                                                               
* INITIALIZE DATES AND REPORT HEADER FIELDS                                     
*                                                                               
         CLC   QSTART(L'QSTART+L'QEND),SPACES                                   
         BE    BADCC                                                            
*                                                                               
         CLC   QSTART(6),QEND      START END DATES IN ORDER                     
         BH    BADCC               START HI, DISASTER                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(5,HDBEG)                                 
         GOTO1 (RF),(R1),,(1,PWOSBEG)            POSTING DATES                  
         GOTO1 (RF),(R1),(0,QEND),(5,HDEND)                                     
         GOTO1 (RF),(R1),,(1,PWOSEND)                                           
         GOTO1 (RF),(R1),(5,0),(1,PWOSDATE)      GET TODAY,S DATE               
*                                                                               
         MVC   BATCHDT,QSTART      START DATE FOR BATCH DATE                    
*                                                                               
         CLI   QOPT3,C'Y'          LET OVERLAY HANDLE PRINTING?                 
         BNE   MAIN10                                                           
*                                                                               
         MVI   RCSUBPRG,1                                                       
         B     MAIN10                                                           
*                                                                               
BADCC    MVC   P+1(43),=C'BAD REQUEST CARD !!!! (DATES, ID, OR BASIS)'          
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* MAIN LINE CODING                                                   *          
*      CALL OVERLAY UNTIL THE OVERLAY SIGNALS IT HAS NO MORE RECORDS *          
*      TO PROCESS  (MSAL=E)                                          *          
**********************************************************************          
         SPACE 1                                                                
MAIN10   L     RF,IRADD                                                         
         MVI   WARNING,C'N'        ERROR SWITCHES                               
         MVI   NOWSW,C'Y'                                                       
*                                                                               
         MVC   P,SPACES            PRINT LINES                                  
         MVC   PREP,SPACES                                                      
         MVC   PREP2,SPACES                                                     
         MVC   PREP3,SPACES                                                     
         MVC   PRTL2,SPACES                                                     
         MVI   SPAP,0                                                           
         MVI   SPAP2,0                                                          
         MVI   SPAP3,0                                                          
*                                                                               
         MVI   MBASIS,C' '         POSTING DATA                                 
         MVI   MNOEND,C' '                                                      
*                                                                               
         ZAP   MBUD,=P'0'          CLEAR THESE TWO NEW MONEY FIELDS             
         ZAP   MRATE,=P'0'                                                      
*                                                                               
         GOTO1 (RF),DMCB,(R9),(RC)     RUN OVERLAY ROUTINE                      
         BAS   RE,CHKHIGH          MAKE SURE HIGHER LEVEL ACCTS EXIST           
         BAS   RE,PRTPREP          PRINT DATA FOR THIS EMPLOYEE                 
*                                                                               
         CLI   MSAL,C'E'           E IN MSAL MEANS WRAP IT UP.                  
         BE    MAIN40                                                           
         CLI   ZEROOPT,C'Y'        DO YOU WANT TO WRITE OUT ZERO SALS           
         BE    MAIN30              YES - DO CHECK IF ZERO,JUST SETUP            
         CLI   ZEROOPT,C'S'        DO YOU WANT TO ZERO OUT TAPE SALS            
         BNE   MAIN20              NO- YOU MUST WANT TO BYPASS ZEROS            
*                                                                               
         ZAP   MSAL,=P'0'          CLEAR MONEY FIELDS                           
         ZAP   MOT,=P'0'                                                        
         ZAP   MTEMP,=P'0'                                                      
         ZAP   MPEN,=P'0'                                                       
         ZAP   MBON,=P'0'                                                       
         ZAP   MBEN,=P'0'                                                       
         ZAP   MBUD,=P'0'                                                       
         ZAP   MRATE,=P'0'                                                      
         B     MAIN30                                                           
*                                                                               
MAIN20   LA    R0,MNBUCKS          LOOP COUNTER - # OF BUCKETS                  
         LA    R1,MSAL             1ST BUCKET                                   
         CP    0(L'MSAL,R1),=P'0'                                               
         BNE   MAIN30                                                           
         LA    R1,L'MSAL(R1)                                                    
         BCT   R0,*-14                                                          
         B     MAIN10              IF ALL ARE ZERO - BYPASS RECORD              
*                                                                               
MAIN30   BAS   RE,BUILD            BUILD EL & RCD & PRINT                       
         BAS   RE,REPORT                                                        
         B     MAIN10                                                           
*                                                                               
MAIN40   BAS   RE,BUILD                                                         
         CLI   QOPT3,C'Y'                                                       
         BE    EOJ10               IF QOPT3=Y DON'T CLEAR P AT EOJ              
         EJECT                                                                  
**********************************************************************          
* PRINT REPORT STATS, CLOSE WORKER                                   *          
**********************************************************************          
         SPACE 1                                                                
EOJ      MVC   P,SPACES                                                         
EOJ10    GOTO1 ACREPORT                                                         
         CLI   QOPT3,C'Y'                                                       
         BE    EOJ30                                                            
*                                                                               
         LA    R8,P+27                                                          
         LA    R5,TOTS                                                          
         LA    R6,5                                                             
         MVC   P+1(20),=C'TOTALS BY MONEY TYPE'                                 
EOJ20    ZAP   WKACCUM,0(6,R5)                                                  
         EDIT  (P6,WKACCUM),(12,(R8)),2,COMMAS=YES                              
         LA    R5,6(R5)                                                         
         LA    R8,13(R8)                                                        
         BCT   R6,EOJ20                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
EOJ30    GOTO1 ACREPORT                                                         
         MVC   P+1(22),=C'*** RUN STATISTICS ***'                               
         ZAP   LINE,=P'75'                                                      
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   P+1(29),=C'TOTAL PASSED FROM I/P ROUTINE'                        
         EDIT  (P6,TOTSAL),(14,P+30),2,COMMAS=YES                               
         MVC   P+95(08),=C'RECORDS='                                            
         MVC   P+60(32),=C'UNRECOVERABLE ERRORS IN THIS RUN'                    
         EDIT  (P6,ERRS),(11,P+48),0,COMMAS=YES                                 
         CP    ERRS,=P'0'                                                       
         BNE   *+10                                                             
         MVC   P+57(2),=C'NO'                                                   
         CP    ERRS,=P'1'                                                       
         BNE   *+10                                                             
         MVC   P+79(12),=C' IN THIS RUN'   CORRECT THE GRAMMAR FOR 1            
         EDIT  (P6,RCT),(7,P+103),0,COMMAS=YES                                  
         GOTO1 ACREPORT                                                         
         MVC   P,SPACES                                                         
         CP    ERRS,=P'0'                                                       
         BE    EOJ90                                                            
*                                                                               
         MVC   P+48(36),=C'LOOK FOR FULL LINES OF *ERROR*ERROR*'                
         MVC   P+85(23),=C'ON THE FOLLOWING PAGES:'                             
         GOTO1 ACREPORT                                                         
         L     R2,APAGES                                                        
EOJ40    LA    R3,P+48                                                          
         LA    R4,8                                                             
EOJ50    ZAP   WORK(3),0(3,R2)                                                  
         CLC   0(3,R2),3(R2)                                                    
         BE    EOJ70                                                            
         EDIT  (P3,WORK),(5,(R3)),0                                             
         LA    R2,3(R2)                                                         
         CLI   0(R2),C' '                                                       
         BE    EOJ80                                                            
         CLI   0(R2),X'FF'                                                      
         BNE   EOJ60                                                            
*                                                                               
         GOTO1 ACREPORT                                                         
         MVC   P(39),=C'* PAGE INDEX FULL, YOU''RE ON YOUR OWN *'               
         B     EOJ80                                                            
*                                                                               
EOJ60    LA    R3,6(R3)                                                         
         BCT   R4,EOJ50                                                         
*                                                                               
         GOTO1 ACREPORT                                                         
         B     EOJ40                                                            
*                                                                               
EOJ70    LA    R2,3(R2)                                                         
         B     EOJ50                                                            
*                                                                               
EOJ80    GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   P,SPACES                                                         
EOJ90    EDIT  (P6,POSTAMT),(14,P+23),2,COMMAS=YES                              
         MVC   P+1(18),=C'TOTAL TO BE POSTED'                                   
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         CLI   RCPOSTNG,C'Y'                                                    
         BNE   EOJX                                                             
         GOTO1 WORKER,DMCB,=C'CLOSE',A(WKBUF),INDEX,IOAREA                      
*                                                                               
EOJX     XBASE                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
* ROUTINE BUILDS ELEMENTS IN THE FOLLOWING ORDER-                               
* 50 POSTING EL, 44 TRANS EL, 20 NAME EL, 30 STATUS EL, 52 SALARY EL.           
* LAST RECORD IS THE 52 SUB FILE EL.                                            
*----------------------------------------------------------------------         
*                                                                               
BUILD    NTR1                      BUILD ELS,RCDS & PRINT THEM                  
         LA    R3,INDEX           FORMAT/WRITE WORKER RCD.                      
         USING UKRECD,R3                                                        
         XC    UKRECD,UKRECD                                                    
         MVC   UKUSRID,ORIGINUM                                                 
         MVC   UKSYSPRG(4),=C'AA8 '                                             
         MVI   UKSUBPRG,X'00'                                                   
         MVC   UKDAY,PWOSDATE+2                                                 
         MVI   UKCLASS,C'P'                                                     
         LA    R3,IOAREA                                                        
         MVI   4(R3),X'00'                                                      
         MVC   0(4,R3),=X'00050000' FULLWORD RECORD HEADER, H(LENGTH) &         
         CLI   MSAL,C'E'                                                        
         BE    BUILD20             BUILD TRAILER                                
*                                                                               
         BAS   RE,ELM50            BUILD 50 ELEMENT - POSTING HEADER            
         BAS   RE,ELM44            BUILD 44 ELEMENT - TRANSACTION               
         BAS   RE,ELM20            BUILD 20 ELEMENT - NAME                      
         BAS   RE,ELM30            BUILD 30 ELEMENT - RECORD STATUS             
         BAS   RE,ELM52            BUILD 52 ELEMENT - MONTHLY SALARY            
*                                                                               
         CLI   WARNING,C'W'                                                     
         BNE   BUILD10                                                          
*                                                                               
         AP    ERRS,=P'1'                                                       
         B     BUILDX                                                           
*                                                                               
BUILD10  CLI   RCPOSTNG,C'Y'                                                    
         BNE   BUILDX                                                           
*                                                                               
         CLI   NOWSW,C'N'                                                       
         BE    BUILDX                                                           
*                                                                               
         AP    RCT,=P'1'                                                        
         GOTO1 WORKER,DMCB,=C'ADD',A(WKBUF),INDEX,IOAREA                        
         SR    R1,R1                                                            
         ICM   R1,1,DMCB+8                                                      
         BZ    BUILDX                                                           
         DC    H'0'                CANT ADD RCD                                 
*                                                                               
*  BUILD THE POSTING TRAILER                                                    
*                                                                               
         USING PSSUBFD,R2                                                       
BUILD20  LA    R2,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                              H(0000)             
         CLI   RCPOSTNG,C'Y'                                                    
         BNE   BUILDX                                                           
         MVI   PSSBEL,PSSBELQ    X'52' - SUB FILE TRAILER ELEMENT               
         MVI   PSSBLEN,PSSUBFL   ELEMENT LENGTH - X'1D'                         
         MVC   PSSBDESC(15),=C'SALARY UPDATE  '                                 
         ZAP   PSSBRECS,RCT                                                     
         ZAP   PSSBCASH,=P'0'                                                   
         BAS   RE,ADDEL                                                         
         GOTO1 WORKER,DMCB,=C'ADD',A(WKBUF),INDEX,IOAREA                        
         SR    R1,R1                                                            
         ICM   R1,1,DMCB+8                                                      
         BZ    *+6                                                              
         DC    H'0'                CANT ADD RCD                                 
*                                                                               
BUILDX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD POSTING HEADER - 50 POSTING ELEMENT                           *         
***********************************************************************         
         SPACE 1                                                                
         USING PSHEADD,R2                                                       
ELM50    NTR1                                                                   
         LA    R2,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   PSHDEL,PSHDELQ      X'50' ELEMENT CODE                           
         MVI   PSHDLEN,PSHEADL     LENGTH X'46'                                 
         MVC   PSHDACC,SPACES                                                   
         MVC   PSHDACC,MEMPNO                                                   
         MVC   SVACC,PSHDACC                                                    
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDANAL,SPACES                                                  
         BAS   RE,ADDEL                                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* BUILD DUMMY TRANSACTION ELEMENT                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING TRNELD,R2                                                        
ELM44    NTR1                                                                   
         LA    R2,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TRNEL,TRNELQ        X'44' - TRANSACTION ELEMENT                  
         MVI   TRNLN,TRNLN1Q       LENGTH - X'1C'                               
         MVC   TRNDATE,PWOSDATE                                                 
         MVC   TRNREF,=6C'0'                                                    
         MVI   TRNSUB,X'00'                                                     
         MVI   TRNTYPE,X'1B'                                                    
         MVI   TRNSTAT,TRNSDR      X'80' - DEBIT                                
         ZAP   TRNAMNT,=P'0'                                                    
         MVC   TRNANAL,=C'  '                                                   
         MVC   TRNBTCH,SPACES                                                   
         MVC   TRNBTCH(1),BATCHDT+1 BATCHDT IS CHAR YYMMDD                      
         SR    R1,R1                                                            
         IC    R1,BATCHDT+3                                                     
         CLI   BATCHDT+2,C'1'                                                   
         BNE   *+8                                                              
         SH    R1,=H'47'                                                        
         STC   R1,TRNBTCH+1                                                     
         BAS   RE,ADDEL                                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* BUILD NAME ELEMENTS                                                *          
**********************************************************************          
         SPACE 1                                                                
         USING NAMELD,R2                                                        
ELM20    NTR1                                                                   
         LA    R2,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   NAMEL,NAMELQ        X'20' - NAME ELEMENT                         
         GOTO1 ADSQUASH,DMCB,MEMPNM,36                                          
         L     R1,DMCB+4                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   NAMEREC(0),MEMPNM                                                
         AH    R1,=Y(NAMLN1Q+1)    ADD OVERHEAD AND DECREMENTED AMOUNT          
         STC   R1,NAMLN                                                         
         BAS   RE,ADDEL                                                         
*                                                                               
         CLC   MEMPFN,SPACES       PASSED A FIRST NAME?                         
         BNH   ELM2010                                                          
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         USING GPNELD,R2           FIRST AND LAST NAME ELEMENTS                 
         MVI   GPNEL,GPNELQ        X'5A' - GENERAL PURPOSE NAME EL              
         MVI   GPNTYP,GPNTFST      FLAG AS FIRST NAME (+MI)                     
         GOTO1 ADSQUASH,DMCB,MEMPFN,36                                          
         L     R1,DMCB+4                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   GPNNME(0),MEMPFN                                                 
         L     R1,DMCB+4                                                        
         LA    R1,GPNLNQ(R1)       NAME + ELEMENT BODY                          
         STC   R1,GPNLN            STORE LENGTH                                 
         BAS   RE,ADDEL                                                         
*                                                                               
ELM2010  CLC   MEMPLN,SPACES       PASSED A LAST NAME?                          
         BNH   ELM20X                                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         USING GPNELD,R2           FIRST AND LAST NAME ELEMENTS                 
         MVI   GPNEL,GPNELQ                                                     
         MVI   GPNTYP,GPNTLST      FLAG AS LAST NAME                            
         GOTO1 ADSQUASH,DMCB,MEMPLN,36 SQUASH AND GET LENGTH                    
         L     R1,DMCB+4                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   GPNNME(0),MEMPLN                                                 
         L     R1,DMCB+4                                                        
         LA    R1,GPNLNQ(R1)                                                    
         STC   R1,GPNLN                                                         
         BAS   RE,ADDEL                                                         
ELM20X   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* CHECK FOR EXISTENCE OF X'30' STATUS ELEMENT IN CURRENT 1R ACCOUNT  *          
* RECORD. IF FOUND MOVE TO ELEMENT, GOTO ADDEL TO BE PASSED TO       *          
* ACUPDATE THIS WILL PREVENT LOCKED ACCOUNTS FROM BEING UNLOCKED VIA *          
* CREATION OF NEW STATUS ELEMENT.                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING RSTELD,R2                                                        
ELM30    NTR1                                                                   
         LA    R2,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING ACKEYD,R4                                                        
         LA    R4,IOAREA2                                                       
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(15),MEMPNO CO, U/L, ACCT.                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         CLI   DMCB+8,0                                                         
         BNE   ELM30X              REC NOT FOUND                                
         LA    R4,ACRECORD                                                      
ELM3010  CLI   0(R4),0             END OF REC                                   
         BE    ELM30X              YES, NO STATUS ELEMENT                       
         CLI   0(R4),RSTELQ        X'30' - RECORD STATUS ELEMENT                
         BE    ELM3020             FOUND STATUS EL                              
         SR    R1,R1                                                            
         IC    R1,1(R4)            CURRENT EL LENGTH                            
         AR    R4,R1                                                            
         B     ELM3010                                                          
*                                                                               
ELM3020  SR    R1,R1                                                            
         IC    R1,1(R4)            STATUS ELEMENT LENGTH                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R2),0(R4)       MOVE IN ELEMENT TO ELEMENT FIELD             
         BAS   RE,ADDEL                                                         
ELM30X   B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD SALARY ELEMENTS                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING MSAELD,R2           SALARY EL                                    
ELM52    NTR1                                                                   
         LA    R2,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   MSAEL,MSAELQ        X'52' - MONTHLY SALARY ELEMENT               
         MVI   MSALN,MSALNQ        LENGTH - X'11'                               
         MVC   MSABEG,PWOSBEG                                                   
*                                                                               
         CLI   QOPT3,C'Y'          IS APPLICATION HANDLING REPORT               
         BE    *+10                YES - DON'T CLEAR PRINT LINE                 
         MVC   P+16(116),SPACES    CLEAR PREVIOUS PRINT LINE                    
*                                                                               
         XC    MSAEND,MSAEND                                                    
         CLI   MNOEND,C'Y'         LEAVE THIS POSTING OPEN ENDED                
         BE    *+10                YES                                          
         MVC   MSAEND,PWOSEND                                                   
         MVI   MSASTAT,MSAS0DP    X'00' - DOLLARS                               
         LA    R5,TOTS                                                          
         LA    R6,MONEYTP                                                       
         LA    R4,MSAL                                                          
         LA    R8,P+27                                                          
ELM5210  MVC   MSATYPE,0(R6)                                                    
*                                                                               
         MVC   MSABASIS,BASIS                                                   
         CLI   MBASIS,C' '         BASIS PASSED HERE FROM APPLICATION           
         BNH   *+10                NO                                           
         MVC   MSABASIS,MBASIS     YES, USE IT                                  
*                                                                               
         CLI   10(R6),C' '         IS THERE BASIS OVERRIDE ON THIS TYPE         
         BE    *+10                NO                                           
         MVC   MSABASIS,10(R6)     YES, USE IT                                  
*                                                                               
         ZAP   MSALARY,0(6,R4)                                                  
         ZAP   0(6,R4),=P'0'                                                    
         AP    ACTOTAL,MSALARY                                                  
         AP    0(6,R5),MSALARY                                                  
*                                                                               
         CLI   QOPT3,C'Y'          IS APPLICATION HANDLING REPORT               
         BE    ELM5220             YES                                          
*                                                                               
         EDIT  (P6,MSALARY),(12,(R8)),2,COMMAS=YES,FLOAT=-                      
*                                                                               
ELM5220  CP    MSALARY,=P'0'                                                    
         BE    ELM5230                                                          
*                                                                               
         CLI   NOWSW,C'N'                                                       
         BE    ELM5230                                                          
*                                                                               
         BAS   RE,ADDEL                                                         
         AP    POSTAMT,MSALARY                                                  
*                                                                               
ELM5230  LA    R6,L'MONEYTP(R6)                                                 
         LA    R4,6(R4)                                                         
         LA    R8,13(R8)                                                        
         LA    R5,6(R5)                                                         
         CLI   0(R6),X'FF'                                                      
         BNE   ELM5210                                                          
*                                                                               
         CLI   QOPT3,C'Y'                                                       
         BE    ELM52X                                                           
*                                                                               
         EDIT  (P6,ACTOTAL),(13,P+13),2,COMMAS=YES,FLOAT=-                      
*                                                                               
ELM52X   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT THE DATA THE OVERLAY HAS PASSED IN PREP AND PREP2 AND        *          
* PREP3 (USUALY THIS IS TOTALS FROM PREVIOUS LEVELS)                 *          
**********************************************************************          
         SPACE 1                                                                
PRTPREP  NTR1                                                                   
         MVC   PSV,P                 SAVE P IN CASE THERE IS A PREP             
         CLC   PREP,SPACES                                                      
         BE    PRTP30                                                           
*                                                                               
         MVC   P,PREP                                                           
         MVC   PREP,SPACES                                                      
         GOTO1 ACREPORT                                                         
         CLI   SPAP,0                                                           
         BE    PRTP30                                                           
*                                                                               
         CLI   SPAP,3              MAXIMUM SKIPS IS 3                           
         BNH   *+8                                                              
         MVI   SPAP,3                                                           
*                                                                               
         MVC   SPACING,SPAP                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
PRTP30   CLC   PREP2,SPACES                                                     
         BE    PRTP40                                                           
*                                                                               
         MVC   P,PREP2                                                          
         MVC   PREP2,SPACES                                                     
         GOTO1 ACREPORT                                                         
         CLI   SPAP2,0                                                          
         BE    PRTP40                                                           
*                                                                               
         CLI   SPAP2,3                                                          
         BNH   *+8                                                              
         MVI   SPAP2,3                                                          
*                                                                               
         MVC   SPACING,SPAP2                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
PRTP40   CLC   PREP3,SPACES                                                     
         BE    PRTP50                                                           
*                                                                               
         MVC   P,PREP3                                                          
         MVC   PREP3,SPACES                                                     
         GOTO1 ACREPORT                                                         
         CLI   SPAP3,0                                                          
         BE    PRTP50                                                           
*                                                                               
         CLI   SPAP3,3                                                          
         BNH   *+8                                                              
         MVI   SPAP3,3                                                          
*                                                                               
         MVC   SPACING,SPAP2                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
PRTP50   MVC   P,PSV               RESTORE P                                    
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REPORT ON THE RECORD JUST POSTED                                   *          
**********************************************************************          
         SPACE 1                                                                
REPORT   NTR1                      REPORT ON REC JUST WRITTEN                   
         MVC   P(14),SVACC+1                                                    
         CLI   QOPT3,C'Y'                                                       
         BE    REP20                                                            
*                                                                               
         MVC   P+95(36),MEMPNM                                                  
         EDIT  (P6,ACTOTAL),(12,P+14),2,COMMAS=YES,FLOAT=-                      
REP20    GOTO1 ACREPORT                                                         
         AP    TOTSAL,ACTOTAL                                                   
         ZAP   ACTOTAL,=P'0'                                                    
         MVC   P,PRTL2                                                          
         GOTO1 ACREPORT                                                         
         CLI   WARNING,C'W'                                                     
         BNE   EXIT                                                             
*                                                                               
         MVC   P(132),=22C'*ERROR'                                              
         GOTO1 ACREPORT                                                         
         L     R1,PAGES                                                         
         CLI   0(R1),X'FF'                                                      
         BE    REP40                                                            
*                                                                               
         SR    R2,R2               CONVERT XL2 PAGE INTO A PL3                  
         ICM   R2,3,PAGE                                                        
         CVD   R2,DUB                                                           
         ZAP   0(3,R1),DUB                                                      
         SP    0(3,R1),=P'1'                                                    
*                                                                               
         LA    R1,3(R1)                                                         
REP40    ST    R1,PAGES                                                         
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CHECK THAT HIGHER LEVEL ACCOUNTS EXIST                             *          
**********************************************************************          
         SPACE 1                                                                
CHKHIGH  NTR1                         CHECK FOR HIGHER LEVEL ACCOUNTS           
         CLI   NOWSW,C'N'             HAS THIS RECORD BEEN REJECTED             
         BE    EXIT                   NO SENSE REJECTING IT AGAIN               
         CLC   MEMPNO+6(2),SPACES     MISSING SUBDEP                            
         BE    CHKH01                                                           
         LA    R4,IOAREA2             SPACE TO BUILD RECORD                     
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES    CLEAR KEY                                 
         MVC   ACKEYACC(8),MEMPNO     CO/U/L/OFF/DEPT/SUB-DEPT                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         SR    R1,R1                  DOES ACCOUNT  EXIST?                      
         ICM   R1,1,DMCB+8                                                      
         BZ    EXIT                   YES - CONTINUE                            
*                                                                               
CHKH01   MVC   PRTL2+25(37),=C'*** HIGHER LEVEL ACCOUNTS MISSING ***'           
         MVI   NOWSW,C'N'              PRINT,BUT DON'T WRITE                    
         MVI   WARNING,C'W'            LINE FLAGGED AS ERROR                    
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO ADD AN ELEMENT IN SEQUENCE AS PRESENTED.                *          
*         R2=ADDR OF ELEMENT, R3=ADDR OF RECORD                      *          
**********************************************************************          
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         LR    R4,R3                                                            
         AH    R4,0(R3)                                                         
         BCTR  R4,0                                                             
         CLI   0(R4),X'00'                                                      
         BE    *+6                                                              
         DC    H'0'                BAD RECORD LENGTH, CANT FIND 00 EL           
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AH    R1,0(R3)                                                         
         CH    R1,=H'1032'                                                      
         BNH   *+6                                                              
         DC    H'0'                CAN'T ADD, NO ROOM                           
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                                                    
         LA    R1,1(R1)                                                         
         AR    R4,R1                                                            
         MVI   0(R4),X'00'                                                      
         LH    R4,0(R3)                                                         
         AR    R4,R1                                                            
         STH   R4,0(R3)                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
         SPACE 1                                                                
BASISTB  DC    AL1(5),C'HOURLY'                                                 
         DC    AL1(5),C'WEEKLY'                                                 
         DC    AL1(6),C'MONTHLY'                                                
         DC    AL1(8),C'QUARTERLY'                                              
         DC    AL1(2),C'YTD'                                                    
         DC    AL1(5),C'ANNUAL'                                                 
         DC    X'FF'                                                            
*                                                                               
AGYTAB   DS    0F                  ALPHA ID, I/P RTN ADD                        
         DC    AL2(0656),AL4(QJRTN)   INGALLS,QUINN AND JOHNSON                 
         DC    AL2(1360),AL4(HRSFRTN) HAL REINEY SAN FRANCISCO                  
         DC    AL2(2061),AL4(KPNYRTN) KALLIR, PHILIPS, ROSS INC.                
         DC    AL2(4589),AL4(KPNYRTN) KALLIR, PHILIPS, ROSS INC.                
         DC    X'FF'                                                            
*                                                                               
MONEYTP  DS    0CL11                   SAL TYPE,LITERAL,BASIS OVERRIDE          
         DC    X'50',CL9'SALARY',CL1' '                                         
         DC    X'40',CL9'OVERTIME',CL1' '                                       
         DC    X'30',CL9'TEMPORARY',CL1' '                                      
         DC    X'15',CL9'PENSION',CL1' '                                        
         DC    X'20',CL9'BONUS',CL1' '                                          
         DC    X'10',CL9'BENEFIT',CL1' '                                        
         DC    X'08',CL9'BUDGET',CL1'A'                                         
         DC    X'07',CL9'RATE',CL1'H'                                           
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* STORAGE DSECT                                                      *          
**********************************************************************          
         SPACE 1                                                                
ACASD    DSECT                                                                  
PAGES    DS    A                                                                
APAGES   DS    A                                                                
IRADD    DS    F                                                                
IDENT    DS    H                                                                
EOF      DS    C                                                                
BUCKS    DS    0P                                                               
TOTS     DS    (MNBUCKS)PL6                                                     
RCT      DS    PL6                                                              
TOTSAL   DS    PL6                                                              
ACTOTAL  DS    PL6                                                              
POSTAMT  DS    PL6                                                              
ERRS     DS    PL6                                                              
WKACCUM  DS    PL6                                                              
NBUCKS   EQU   (*-BUCKS)/6                                                      
*                                                                               
PWOSDATE DS    CL3                                                              
PWOSBEG  DS    CL3                                                              
PWOSEND  DS    CL3                                                              
HDBEG    DS    CL8                                                              
HDEND    DS    CL8                                                              
BASIS    DS    CL9' '                                                           
SVACC    DS    CL15                                                             
*                                                                               
MSG      DS    CL10                MESSAGE                                      
*                                                                               
BATCHDT  DS    CL6                                                              
ZEROOPT  DS    CL1                                                              
ELEMENT  DS    CL255               BUILD ELEMENTS HERE                          
INDEX    DS    CL16                                                             
PSV      DS    CL132                                                            
PRTL2    DS    CL132                                                            
PREP     DS    CL132               PRE-PRINT LINE                               
SPAP     DS    XL1                 SPACE AFTER PREP PRINT                       
PREP2    DS    CL132               PRE-PRINT TWO-FOR 2ND LEVEL TOTAL            
SPAP2    DS    XL1                 LINES TO SKIP AFTER PREP2                    
PREP3    DS    CL132               PRE-PRINT THREE, FOR REQUREST TOTAL          
SPAP3    DS    XL1                 LINES TO SKIP AFTER PREP2                    
WARNING  DS    CL1                                                              
NOWSW    DS    CL1                                                              
AIOAREA  DS    A                                                                
AIOAREA2 DS    A                                                                
MANAREA  DS    (MANLNQ)C                                                        
IOAREA   DS    0F                                                               
         DS    CL1032                                                           
IOAREA2  DS    0F                                                               
         DS    CL1032                                                           
         EJECT                                                                  
UTL      CSECT                                                                  
         DC    F'0'                                                             
         DC    X'01'                                                            
WKBUF    CSECT                                                                  
         DC    4500X'00'                                                        
         EJECT                                                                  
**********************************************************************          
* HEAD HOOK ROUTINE FOR BASE, CAN BE OVERRIDDED BY OVERLAY           *          
**********************************************************************          
         SPACE 1                                                                
BASEHOOK NMOD1 0,*BASEH                                                         
         L     RC,BASERC           RESTORE RC                                   
*                                                                               
         MVC   HEAD3+40(17),=C'*** PROOF RUN ***'                               
         CLI   RCPOSTNG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   HEAD3+40(17),=C'*** LIVE RUN *** '                               
         MVC   HEAD3+88(8),HDBEG                                                
         MVC   HEAD3+98(2),=C'TO'                                               
         MVC   HEAD3+101(8),HDEND                                               
*                                                                               
         MVC   HEAD3+73(6),=C'BASIS='                                           
         MVC   HEAD3+79(9),BASIS                                                
*                                                                               
BASEHX   XMOD1                                                                  
BASERC   DS    A                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 =V(PRNTBL),DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),     X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'SORT KEY'                                              
*        GOTO1 =A(DUMP),DMCB,(RC),HRSRTKEY,HRSRTLNQ                             
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BEGIN HRSF INPUT SECTION                                            *         
***********************************************************************         
         SPACE 1                                                                
HRSFRTN  CSECT                                                                  
         NMOD1 0,*HRSF**                                                        
         L     R9,0(R1)           ADDR OF AREA TO BUILD RECORD                  
         L     RC,4(R1)           ACASD WORKING STORAGE                         
         USING MANFRECD,R9                                                      
*                                                                               
         CLI   HRSFFRST,C'Y'       1ST TIME IN?                                 
         BNE   HRBUILD             NO--GO READ SORT RECORDS                     
         LM    R6,R8,0(R1)                                                      
         LR    R9,R6               A(REC BUILD AREA)                            
         USING MANFRECD,R9                                                      
         MVC   PRTL2,SPACES                                                     
         MVI   HRSFFRST,C'N'       TURN OFF 1ST TIME SWITCH                     
*                                                                               
         OPEN  (HRSFTAPE,(INPUT))                                               
*                                                                               
         XC    HRALSORT,HRALSORT   CLEAR ADDR OF LAST SORT                      
         LA    R1,HRSTKLEN         SORT KEY LENGTH                              
         CVD   R1,DUB              CONVERT KEY LENGTH TO CHARS                  
         OI    DUB+7,X'0F'                                                      
         UNPK  HRSRTCRD+15(3),DUB+6(2)                                          
         LA    R1,HRSRTLNQ         SORT RECORD LENGTH                           
         CVD   R1,DUB              CONVERT REC LENGTH TO CHARS                  
         OI    DUB+7,X'0F'                                                      
         UNPK  HRRECCRD+22(3),DUB+6(2)                                          
         GOTO1 ADSORTER,DMCB,HRSRTCRD,HRRECCRD,0                                
*                                                                               
         LA    RE,HRSFHOOK                                                      
         ST    RE,HEADHOOK                                                      
*                                                                               
         BAS   RE,HRCLRSRT                   CLEAR SORT RECORD                  
*                                                                               
         ZAP   HRSALTOT,=P'0'                 CLEAR TOTAL ACCUMS                
         ZAP   HRRQSTOT,=P'0'                                                   
*                                                                               
* READING THE TAPE                                                              
*                                                                               
         USING HRSFRECD,R2                                                      
HRSF10   BAS   RE,HRSFREAD         GO READ THE TAPE                             
         CLI   HRSFEOF,C'E'                  END OF TAPE?                       
         BE    HRSF30                        YES - ONE MORE SORT                
         LA    R2,TPREC            R2=A(TAPE RECORD)                            
         OC    SVTPREC,SVTPREC     1ST TIME THROUGH?                            
         BNZ   *+14                                                             
         MVC   SVTPREC,HRSFCODE    SAVE TAPE RECORD                             
         B     HRSF10                                                           
*                                                                               
         CLC   HRSFCODE,SVTPREC    SAME ACCOUNT?                                
         BNE   HRSF20                                                           
         LA    R1,SVTPREC                                                       
         PACK  WORK(6),HRSFSAL                                                  
         PACK  DUB,HRSFSAL-HRSFRECD(L'HRSFSAL,R1)                               
         AP    DUB,WORK(6)                                                      
         LA    R3,HRSFSAL-HRSFRECD(R1)                                          
         LA    R3,L'HRSFSAL-1(R3)  POINT TO LAST POSITION OF SAL FLD            
         OI    0(R3),X'0F'                                                      
         UNPK  HRSFSAL-HRSFRECD(L'HRSFSAL,R1),DUB                               
         B     HRSF10                                                           
*                                                                               
HRSF20   DS    0H                                                               
         CLI   HRSFEOF,C'E'                  END OF TAPE?                       
         BNE   HRSF30                        NO - CONTINUE                      
         OC    HRSRTKEY(HRSTKLEN),HRSRTKEY   DO I HAVE A REC FOR SORTER         
         BZ    *+8                                                              
         BAS   RE,HRPUTSRT                   PUT  TO SORTER                     
         BAS   RE,HRCLRSRT                   CLEAR SORT RECORD                  
         B     HRBUILD                       BUILD THE REC FOR ROOT             
*                                                                               
HRSF30   OC    HRSRTKEY(HRSTKLEN),HRSRTKEY   DO I HAVE A REC FOR SORTER         
         BZ    HRSF40                        NO - THEN SET UP SORT REC          
*                                                                               
         BAS   RE,HRPUTSRT                   PUT TO SORTER                      
         BAS   RE,HRCLRSRT                   CLEAR SORT RECORD                  
*                                                                               
HRSF40   DS    0H                                                               
         LA    R2,SVTPREC          POINT TO SAVED AREA NOT OF TAPE REC          
         CLC   HRSFYYMM,QEND                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   HRSRTOFF,HRSFOFF              OFF/DPT/SUB/EMP TO SRT REC         
         MVC   HRSRTDPT,HRSFDEPT                                                
         MVC   HRSRTSUB,HRSFSDEP                                                
         MVC   HRSRTEMP,HRSFEMP                                                 
         MVC   HRSRTNAM,HRSFNAME             EMPLOYEE NAME                      
         OC    HRSRTACC,SPACES               UPPER CASE ONLY                    
*                                                                               
         PACK  WORK(6),HRSFSAL               SALARY                             
         ZAP   HRSRTSAL,WORK(6)              ADD TO SORT REC                    
         CLI   HRSFEOF,C'E'                  END OF TAPE?                       
         BE    HRSF20                        YES - END                          
         MVC   SVTPREC,TPREC                 SAVE TAPE RECORD                   
         B     HRSF10                                                           
         EJECT                                                                  
***********************************************************************         
* READ SORT RECORDS                                                   *         
***********************************************************************         
         SPACE 1                                                                
HRBUILD  GOTO1 ADSORTER,DMCB,=C'GET'         GET A SORT REC                     
         L     R2,DMCB+4                     A(SORT REC)                        
         ST    R2,HRALSORT                   SAVE ADDR OF LAST SORT             
         LTR   R2,R2                                                            
         BZ    HRCKEOF                       END OF RECORDS FROM SORT           
*                                                                               
HRBUILD1 OC    HRSRTKEY(HRSTKLEN),HRSRTKEY   DO WE HAVE A SORT REC SAVD         
         BZ    HRBUILD2                      NO-CONTINUE                        
         CLC   HRSRTKEY(HRSTDLEN),0(R2)      IS IT THE SAME OFF/DEPT?           
         BE    HRBUILD2                      YES - CONTINUE                     
         BAS   RE,HRDEPTOT                   GO SET UP DPT TOTAL LINE           
*                                                                               
HRBUILD2 BAS   RE,HRCLRSRT                   CLEAR SORT AREA                    
         MVC   HRSRTKEY(HRSRTLNQ),0(R2)      RECORD INTO KEY                    
*                                                                               
         MVC   MEMPNO(51),SPACES             CLEAR EMPL NUM AND NAME            
         MVI   MEMPNO,X'88'                  COMP CODE FOR HRSF                 
         MVC   MEMPNO+1(2),=C'1R'            UNIT/LEDGER---1R                   
         MVC   MEMPNO+3(12),HRSRTACC         DEPT                               
*                                                                               
         MVC   MEMPNM,HRSRTNAM     EMPLOYEE NAME                                
         MVC   MEMPFN,HRSRTFN         FIRST NAME                                
         MVC   MEMPLN,HRSRTLN         LAST  NAME                                
*                                                                               
         ZAP   MSAL,HRSRTSAL                                                    
         ZAP   MOT,=P'0'                                                        
         ZAP   MTEMP,=P'0'                                                      
         ZAP   MBON,=P'0'                                                       
         ZAP   MBEN,=P'0'                                                       
         ZAP   MPEN,=P'0'                                                       
*                                                                               
         MVC   P+1(12),HRSRTACC                                                 
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'HRSRTNAM),HRSRTNAM                                        
         GOTO1 ADSQUASH,DMCB,WORK,37                                            
         MVC   P+18(30),WORK                                                    
         EDIT  (P6,HRSRTSAL),(13,P+53),2,COMMAS=YES,FLOAT=-                     
*                                                                               
HRLEVCK  L     R4,AIOAREA         CHECK FOR HIGHER LEVEL ACCOUNTS               
         USING HRWORK,R4                                                        
         MVC   HRSFKEY(60),SPACES        CLEAR KEY                              
         MVC   HRSFKEY(8),MEMPNO        CO/U/L/OFF/DEPT/SUB-DEPT                
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         SR    R1,R1                     DOES ACCOUNT  EXIST?                   
         ICM   R1,1,DMCB+8                                                      
         BNZ   HRACTERR                  NO FLAG                                
*                                                                               
         MVC   HRSFKEY(60),SPACES        READ FOR ACCOUNT                       
         MVC   HRSFKEY(15),MEMPNO                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         SR    R1,R1                     DOES ACCOUNT  EXIST?                   
         ICM   R1,1,DMCB+8                                                      
         BNZ   HRADDACC                  NO - PRINT MESSAGE                     
*                                                                               
         AH    R4,=H'49'                 TO FIRST ELEMENT                       
HRLEV3   CLI   0(R4),0                                                          
         BE    HRLEV6                                                           
         CLI   0(R4),X'30'               CHECK STATUS                           
         BE    HRLEV5                                                           
         SR    R1,R1                     DOES ACCOUNT  EXIST?                   
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     HRLEV3                                                           
*                                                                               
         USING ACSTATD,R4                                                       
HRLEV5   TM    ACSTSTAT,X'20'            IS THIS ACCOUNT LOCKED                 
         BZ    *+10                      YES, PRINT WARNING                     
         MVC   PRTL2+25(37),=C'******* THIS ACCOUNT IS LOCKED ******'           
*                                                                               
HRLEV6   L     R4,AIOAREA          CHECK ACCOUNT IS VALID FOR POSTING           
         AH    R4,=H'49'                 TO FIRST ELEMENT                       
HRLEV7   CLI   0(R4),0                                                          
         BE    HRNOPOST                                                         
         CLI   0(R4),X'32'         CHECK FOR BALANCE ELEMENT                    
         BE    HRLEV8                                                           
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     HRLEV7                                                           
*                                                                               
HRLEV8   AP    HRSALTOT,MSAL       BUMP DEPARTMENT TOTALS                       
         AP    HRRQSTOT,MSAL       BUMP DEPARTMENT TOTALS                       
         B     HRXIT                      ADD POSTING                           
*                                                                               
* ERRORS                                                                        
*                                                                               
HRACTERR MVC   PRTL2+25(37),=C'*** HIGHER LEVEL ACCOUNTS MISSING ***'           
         MVI   NOWSW,C'N'         PRINT,BUT DON'T WRITE                         
         MVI   WARNING,C'W'       LINE FLAGGED AS ERROR                         
         B     HRXIT                                                            
*                                                                               
HRNOPOST MVC   PRTL2+25(37),=C'**** INVALID ACCOUNT FOR POSTING ****'           
         MVI   NOWSW,C'N'         PRINT,BUT DON'T WRITE                         
         MVI   WARNING,C'W'       LINE FLAGGED AS ERROR                         
         B     HRXIT                                                            
*                                                                               
HRADDACC MVC   PRTL2+25(33),=C'-- THIS ACCOUNT HAS BEEN ADDED --'               
         B     HRXIT                                                            
*                                                                               
HRSFEND  MVI   HRSFEOF,C'E'       SET END OF TAPE INDICATOR                     
         B     HRXIT                                                            
*                                                                               
HRCKEOF  BAS   RE,HRDEPTOT        FINAL DEPT TOTAL                              
         BAS   RE,HRRQTOTL        REQUEST TOTAL                                 
         MVI   MSAL,C'E'          INDICATES EOJ TO ROOT PROGRAM                 
         CLOSE HRSFTAPE                                                         
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         XMOD1                                                                  
*                                                                               
HRXIT    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* UTILITY ROUTINES                                                    *         
***********************************************************************         
         SPACE 1                                                                
HRSFREAD NTR1                     **READ A TAPE RECORD**                        
         GET   HRSFTAPE                                                         
         LR    R2,R1              ADDR OF RECORD RETURNED IN R1                 
         MVC   TPREC,0(R2)                                                      
         XIT1  REGS=(R2)          RESTORE ALL BUT R2                            
*                                                                               
HRPUTSRT NTR1                     **PUT RECORD TO SORTER**                      
         GOTO1 ADSORTER,DMCB,=C'PUT',HRSRTKEY                                   
         B     HRXIT                                                            
*                                                                               
HRCLRSRT NTR1                     **CLEAR THE SORT RECORD AREA**                
         XC    HRSRTKEY(HRSRTLNQ),HRSRTKEY                                      
         ZAP   HRSRTSAL,=P'0'                                                   
         B     HRXIT                                                            
*                                                                               
HRDEPTOT NTR1                                                                   
         MVI   SPAP,3                                                           
         MVC   PREP(17),=C'**TOTS DPT XXX **'                                   
         MVC   PREP+11(1),HRSRTOFF                                              
         MVC   PREP+12(2),HRSRTDPT                                              
         EDIT  (P6,HRSALTOT),(13,PREP+53),2,COMMAS=YES,FLOAT=-                  
         ZAP   HRSALTOT,=P'0'                                                   
         B     HRXIT                                                            
*                                                                               
HRRQTOTL NTR1                                                                   
         MVI   SPAP,3                                                           
         MVC   P(17),=C'**REQUEST TOTAL**'                                      
         EDIT  (P6,HRRQSTOT),(13,P+53),2,COMMAS=YES,FLOAT=-                     
         ZAP   HRRQSTOT,=P'0'                                                   
         B     HRXIT                                                            
         EJECT                                                                  
HRSFHOOK NMOD1 0,HRHOOK                                                         
         MVI   HEAD4,X'00'                                                      
         MVC   HEAD5,SPACES                    CLEAR THE LINE                   
         MVC   HEAD6+18(4),=C'NAME'                                             
         MVC   HEAD6+54(6),=C'SALARY'                                           
*                                                                               
         GOTO1 =A(BASEHOOK)                                                     
         XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* STORAGE                                                             *         
***********************************************************************         
         SPACE 1                                                                
HRALSORT DS    A                  ADDR OF LAST SORT                             
*                                                                               
HRSRTKEY DS    0C                 HRSF SORTER RECORD                            
HRSRTACC DS    0CL12              1R ACCOUNT                                    
HRSRTHGH DS    0CL5               HIGHER LEVEL ACCOUNT                          
HRSRTOFF DS    CL1                     OFFICE                                   
HRSRTDPT DS    CL2                     DEPT                                     
HRSTDLEN EQU   *-HRSRTKEY         SORT OFF DEPT LENGTH                          
HRSRTSUB DS    CL2                     SUB-DEPT                                 
HRSRTEMP DS    CL7                     EMPLOYEE NUMBER                          
HRSTKLEN EQU   *-HRSRTKEY         SORT KEY KENGTH                               
HRSRTNAM DS    0CL37              NAME                                          
HRSRTLN  DS    CL18                   LAST                                      
HRSRTFN  DS    CL18                   FIRST                                     
HRSRTMI  DS    CL1                    MIDDLE INITIAL                            
HRSRTSAL DS    PL6                SALARY                                        
HRSRTLNQ EQU   *-HRSRTKEY         RECORD LENGTH                                 
*                                                                               
HRSALTOT DS    PL6                POST AMOUNT DEPARTMENT TOT                    
HRRQSTOT DS    PL6                POST AMOUNT REQUEST TOTALS                    
*                                                                               
TPREC    DS    CL(HRSFLNQ)         SAVED AREA FOR TAPE RECORD                   
SVTPREC  DS    CL(HRSFLNQ)         SAVED AREA TO ACCUMUATE TOTALS               
*                                                                               
HRSRTCRD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
HRRECCRD DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
*                                                                               
HRSFFRST DC    C'Y'               1ST TIME INTO ROUTINE INDICATOR               
HRSFEOF  DC    C' '               END OF TAPE INDICATOR                         
         EJECT                                                                  
***********************************************************************         
* HRSF TAPE DCB                                                       *         
***********************************************************************         
         SPACE 1                                                                
HRSFTAPE DCB   DDNAME=HRSFTAPE,                                        X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=69,                                               X        
               BLKSIZE=6900,                                           X        
               MACRF=GL,                                               X        
               EODAD=HRSFEND                                                    
*                                                                               
HRWORK   DSECT                                                                  
HRSFKEY  DS    CL15' '                                                          
         DS    CL1005                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER HAL RIENEY SALARY TAPE RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
HRSFRECD DSECT                                                                  
HRSFCODE DS    0CL12               PERSON CODE O/D/S/P                          
HRSFOFF  DS    CL1                                                              
HRSFDEPT DS    CL2                                                              
HRSFSDEP DS    CL2                                                              
HRSFEMP  DS    CL7                                                              
HRSFNAME DS    0CL37               FULL NAME                                    
HRSFLNAM DS    CL18                LAST NAME                                    
HRSFFNAM DS    CL18                FIRST NAME                                   
HRSFMI   DS    CL1                 MIDDLE INITIAL                               
HRSFYYMM DS    CL6                                                              
HRSFPAYC DS    CL5                 PAY CODE                                     
HRSFSAL  DS    CL9                                                              
HRSFLNQ  EQU   *-HRSFCODE                                                       
         EJECT                                                                  
**********************************************************************          
* BEGIN QUINN AND JOHNSON I/P SECTION                                *          
**********************************************************************          
         SPACE 1                                                                
QJRTN    CSECT                                                                  
         NMOD1 0,*QANDJ**                                                       
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
         USING MANFRECD,R9                                                      
*                                                                               
         CLI   QJFST,C'F'                                                       
         BNE   QJRDTP                                                           
         OPEN  (QJTAPE,(INPUT))                                                 
*                                                                               
QJRDTP   BAS   RE,QJREAD                                                        
         CLI   QJEOF,C'Y'                                                       
         BE    QJCKEOF                                                          
*                                                                               
         MVC   MEMPNO,SPACES                                                    
         MVI   MEMPNO,X'72'             HEX COMP                                
         MVC   MEMPNO+1(2),=C'1R'       UNIT/LEDGER                             
         MVC   MEMPNO+3(9),QJSVEMP      OFFICE/DEPT/SUBD/EMP                    
*                                                                               
         ZAP   MSAL,QJAMT                                                       
         ZAP   MOT,=P'0'                                                        
         ZAP   MTEMP,=P'0'                                                      
         ZAP   MBON,=P'0'                                                       
         ZAP   MBEN,=P'0'                                                       
         ZAP   MPEN,=P'0'                                                       
         MVC   MEMPNM,SPACES                                                    
*                                                                               
         LA    R4,IOAREA2                                                       
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(15),MEMPNO        CO, U/L, ACCT.                        
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         CLI   DMCB+8,0                                                         
         BNE   QJRD100             REC NOT FOUND                                
         AH    R4,DATADISP         TO FIRST ELEMENT                             
QJRD10   CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'20'         NAME  ELEMENT                                
         BE    QJRD50                                                           
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     QJRD10                                                           
*                                                                               
QJRD50   MVC   MEMPNM,SPACES       CLEAR NAME                                   
         ZIC   R1,1(R4)            ELEMENT LENGTH IN R1                         
         SH    R1,=H'3'            ADJUST TO NAME LEN -1                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MEMPNM(0),2(R4)     MOVE IN CURRENT NAME                         
*                                                                               
         ZAP   QJAMT,=P'0'                                                      
         B     QJEXITM                                                          
*                                                                               
QJRD100  MVC   PRTL2+25(37),=C'*******   ACCOUNT NOT FOUND   *******'           
         MVI   NOWSW,C'N'         PRINT,BUT DON'T WRITE                         
         MVI   WARNING,C'W'       LINE FLAGGED AS ERROR                         
         B     QJEXITM                                                          
*                                                                               
QJCKEOF  MVI   MSAL,C'E'                                                        
         CLOSE QJTAPE                                                           
         B     QJEXITM                                                          
         SPACE 3                                                                
QJREAD   NTR1                                                                   
         CLI   QJEOF,C'E'                                                       
         BNE   QJR0                                                             
         MVI   QJEOF,C'Y'                                                       
         B     QJEXIT                                                           
*                                                                               
QJR0     CLI   QJFST,C'F'                                                       
         BNE   QJR2                                                             
*                                                                               
         MVI   QJFST,C'N'                                                       
         BAS   R4,QJGET                                                         
         B     QJR2                                                             
*                                                                               
QJR1     BAS   R4,QJGET                                                         
         CLC   QJSVEMP,QJEMP       SAME EMPLOYEE NUMBER                         
         BNE   QJEXIT                                                           
*                                                                               
QJR2     MVC   QJSVEMP,QJEMP                                                    
         MVC   QJSVNAME,QJENAME                                                 
         PACK  DUB,QJSAL                                                        
         AP    QJAMT,DUB                                                        
         B     QJR1                                                             
*                                                                               
QJEXIT   XIT1                                                                   
*                                                                               
QJEND    MVI    QJEOF,C'E'                                                      
         B      QJEXIT                                                          
*                                                                               
QJGET    EQU   *                                                                
         GET   QJTAPE                                                           
         MVC   QJREC,0(R1)                                                      
         BR    R4                                                               
*                                                                               
QJEXITM  XMOD1                                                                  
         EJECT                                                                  
**********************************************************************          
* STORAGE                                                            *          
**********************************************************************          
         SPACE 1                                                                
QJREC    DS    0CL80                                                            
QJEMP    DS    CL9                                                              
QJENAME  DS    CL25                                                             
QJSAL    DS    CL10                                                             
         DS    CL4              EMPLOYEE NUMBER                                 
         DS    CL36                                                             
*                                                                               
QJSVEMP  DS    CL9                 PREVIOUS EMPLOYEE CODE                       
QJSVNAME DS    CL25                PREVIOUS EMPLOYEE NAME                       
         EJECT                                                                  
**********************************************************************          
* DCB                                                                *          
**********************************************************************          
         SPACE 1                                                                
QJTAPE   DCB   DDNAME=QJTAPE,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=80,                                               X        
               BLKSIZE=800,                                            X        
               MACRF=GL,                                               X        
               EODAD=QJEND                                                      
*                                                                               
QJCRD    DS    CL80                                                             
QJAMT    DC    PL6'0'                                                           
QJFST    DC    C'F'                                                             
QJEOF    DC    C'N'                                                             
QWORK    DS    CL7                                                              
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* KPNY INPUT SECTION - ACCOUNTING INTERFACE                           *         
***********************************************************************         
         SPACE 1                                                                
KPNYRTN  CSECT                                                                  
         NMOD1 0,*KPNY**                                                        
         USING MANFRECD,R9                                                      
         L     R9,0(R1)           ADDR OF AREA TO BUILD RECORD                  
         L     RC,4(R1)           ACASD WORKING STORAGE                         
         L     RE,=A(KPHOOK)                                                    
         ST    RE,HEADHOOK                                                      
*                                                                               
         CLI   KPNYFRST,C'Y'      1ST TIME IN?                                  
         BNE   KPBLD              NO--GO READ SORT RECORDS                      
         MVC   PRTL2,SPACES                                                     
         MVI   KPNYFRST,C'N'      TURN OFF 1ST TIME SWITCH                      
*                                                                               
         OPEN  (KPNYTAPE,(INPUT))                                               
*                                                                               
         XC    KPALSORT,KPALSORT   CLAER ADDR OF LAST SORT                      
         LA    R1,KPSTKLEN         SORT KEY LENGTH                              
         CVD   R1,DUB             CONVERT KEY LENGTH TO CHARS                   
         OI    DUB+7,X'0F'                                                      
         UNPK  KPSRTCRD+15(3),DUB+6(2)                                          
         LA    R1,KPSRTLNQ         SORT RECORD LENGTH                           
         CVD   R1,DUB              CONVERT REC LENGTH TO CHARS                  
         OI    DUB+7,X'0F'                                                      
         UNPK  KPRECCRD+22(3),DUB+6(2)                                          
         GOTO1 ADSORTER,KPDMCB,KPSRTCRD,KPRECCRD,0                              
*                                                                               
         BAS   RE,KPCLRSRT                   CLEAR SORT RECORD                  
*                                                                               
         LA    R3,KPDEPACC                                                      
         BAS   RE,KPZAP                                                         
         LA    R3,KPREQACC                                                      
         BAS   RE,KPZAP                                                         
*                                                                               
* READING THE TAPE                                                              
*                                                                               
         SPACE 1                                                                
         BAS   RE,KPNYREAD                 READ FIRST RECORD OF TAPE            
KPNY10   CLI   KPNYEOF,C'E'                 END OF TAPE?                        
         BNE   KPNY20                       NO - CONTINUE                       
         OC    KPSRTKEY(KPSTKLEN),KPSRTKEY  DO I HAVE A REC FOR SORTER          
         BZ    *+8                                                              
         BAS   RE,KPPUTSRT                  PUT  TO SORTER                      
         BAS   RE,KPCLRSRT                  CLEAR SORT RECORD                   
         B     KPBLD                        BUILD THE REC FOR ROOT              
*                                                                               
         USING KPNYRECD,R2                                                      
KPNY20   OC    KPSRTKEY(KPSTKLEN),KPSRTKEY  DO I HAVE A REC FOR SORTER          
         BZ    *+12                         NO - THEN SET UP SORT REC           
         BAS   RE,KPPUTSRT                  PUT TO SORTER                       
         BAS   RE,KPCLRSRT                  CLEAR SORT RECORD                   
*                                                                               
         MVC   KPSRTOFF,KPNYOFF             OFF/DPT/                            
         MVC   KPSRTDEP,KPNYDEP                                                 
         MVC   KPSRTSUB,=C'00'                                                  
         MVC   KPSRTEMP,SPACES                                                  
         MVC   KPSRTEMP(5),KPNYEMP                                              
         MVC   KPSRTNAM,KPNYNAME            EMPLOYEE NAME                       
         PACK  WORK(6),KPNYREG              PACK SALARY                         
         ZAP   KPSRTREG,WORK(6)                                                 
         PACK  WORK(6),KPNYOT                                                   
         ZAP   KPSRTOT,WORK(6)                                                  
         BAS   RE,KPNYREAD                  READ NEXT TAPE RECORD               
         B     KPNY10                       AND PROCESS                         
         EJECT                                                                  
**********************************************************************          
* GET RECORDS BACK FROM SORTER AND BUILD OUTPUT                      *          
**********************************************************************          
         SPACE 1                                                                
KPBLD    GOTO1 ADSORTER,KPDMCB,=C'GET'       GET A SORT REC                     
         L     R2,KPDMCB+4                   ADDR OF LAST SORT                  
         ST    R2,KPALSORT                   SAVE ADDR OF LAST SORT             
         LTR   R2,R2                                                            
         BZ    KPCKEOF                       END OF RECORDS FROM SORT           
*                                                                               
         OC    KPSRTKEY(KPSTKLEN),KPSRTKEY   DO WE HAVE A SORT REC SAVD         
         BZ    KPBLD10                       NO-CONTINUE                        
         CLC   KPSRTKEY(KPSTDLEN),0(R2)      IS IT THE SAME OFF/DEPT?           
         BE    KPBLD10                       YES - CONTINUE                     
         BAS   RE,KPDEPTOT                   GO SET UP DPT TOTAL LINE           
*                                                                               
KPBLD10  BAS   RE,KPCLRSRT                   CLEAR SORT AREA                    
         MVC   KPSRTKEY(KPSRTLNQ),0(R2)      RECORD INTO KEY                    
*                                                                               
         MVC   MEMPNO(51),SPACES             CLEAR EMPL NUM AND NAME            
         MVC   MEMPNO(1),RCCOMPFL            COMP CODE FOR KPNY                 
         MVC   MEMPNO+1(2),=C'1R'            UNIT/LEDGER---1R                   
*                                                                               
         MVC   MEMPNO+3(11),KPSRTOFF         OFFIKP DEPT SUB D EMP CODE         
         MVC   MEMPNM(L'KPSRTNAM),KPSRTNAM  EMPLOYEE NAME                       
*                                                                               
         ZAP   MSAL,=P'0'                    CLEAR ALL DOLLAR FIELDS            
         ZAP   MOT,=P'0'                                                        
         ZAP   MTEMP,=P'0'                                                      
         ZAP   MBON,=P'0'                                                       
         ZAP   MBEN,=P'0'                                                       
         ZAP   MPEN,=P'0'                                                       
*                                                                               
         ZAP   MSAL,KPSRTREG           MOVE REG SAL INTO RECORD                 
         ZAP   MOT,KPSRTOT                  OT                                  
*                                      ***SET UP  PRINT LINE***                 
         MVC   P(14),MEMPNO+1          EMPLOYEE 1R ACCT                         
         LA    R3,KPSRTREG         ADD TO HIGHER LEVEL ACCUMS                   
         LA    R2,KPDEPACC                                                      
         BAS   RE,KPADD                                                         
         LA    R2,KPREQACC                                                      
         BAS   RE,KPADD                                                         
*                                                                               
         LA    R2,P+25                                                          
         BAS   RE,KPPRT                                                         
         MVC   P+80(19),MEMPNM      NAME                                        
*                                                                               
         L     R4,AIOAREA         CHECK FOR HIGHER LEVEL ACCOUNTS               
         USING KPWORK,R4                                                        
         MVC   KPNYKEY(60),SPACES        CLEAR KEY                              
         MVC   KPNYKEY(8),MEMPNO        CO/U/L/OFF/DEPT/SUB-DEPT                
         BAS   RE,KPREADAC                                                      
         SR    R1,R1                                                            
         ICM   R1,1,KPDMCB+8             DOES ACCOUNT  EXIST?                   
         BNZ   KPACTERR                  NO - ERROR OUT                         
*                                                                               
         L     R4,AIOAREA                CHECK FOR ACCOUNT NUMBER               
         MVC   KPNYKEY(60),SPACES        CLEAR KEY                              
         MVC   KPNYKEY(15),MEMPNO      CO/U/L/OFF/DEPT/SUB-DEPT/ACCNUM          
         BAS   RE,KPREADAC                                                      
         SR    R1,R1                                                            
         ICM   R1,1,KPDMCB+8             DOES ACCOUNT  EXIST?                   
         BNZ   KPACTNF                   PRINT ERROR MSG AND EXIT               
*                                                                               
         AH    R4,=H'49'                 GET NAME FROM ACCOUNT                  
KPBLD20  CLI   0(R4),0                   CHECK STATUS OF ACCOUNT                
         BE    KPXIT                                                            
         CLI   0(R4),X'20'               NAME ELEMENT                           
         BE    KPBLD40                                                          
         CLI   0(R4),X'30'               STATUS ELEMENT                         
         BNE   KPBLD30                                                          
         USING ACSTATD,R4                                                       
         TM    ACSTSTAT,X'20'           IS A/C LOCKED                           
         BO    KPLCKERR                 NO                                      
         DROP R4                                                                
*                                                                               
KPBLD30  SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     KPBLD20                                                          
*                                                                               
KPBLD40  MVC   MEMPNM,SPACES             CLEAR NAME                             
         SR    R1,R1                                                            
         IC    R1,1(R4)                  ELEMENT LENGTH IN R1                   
         SH    R1,=H'3'                  ADJUST TO NAME LEN -1                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MEMPNM(0),2(R4)           MOVE IN CURRENT NAME                   
         B     KPBLD30                   LOOK FOR NEXT ELEMENT                  
         EJECT                                                                  
**********************************************************************          
* ERRORS                                                             *          
**********************************************************************          
         SPACE 1                                                                
KPACTERR MVC   PRTL2+25(37),=C'*** HIGHER LEVEL ACCOUNTS MISSING ***'           
         MVI   NOWSW,C'N'         PRINT,BUT DON'T WRITE                         
         MVI   WARNING,C'W'       LINE FLAGGED AS ERROR                         
         B     KPXIT                                                            
*                                                                               
KPACTNF  MVC   PRTL2+25(37),=C'****  ERROR - ACCOUNT NOT FOUND  ****'           
         MVI   NOWSW,C'N'         PRINT,BUT DON'T WRITE                         
         MVI   WARNING,C'W'       LINE FLAGGED AS ERROR                         
         B     KPXIT                                                            
*                                                                               
KPLCKERR MVC   PRTL2+25(37),=C'**** ERROR  - ACCOUNT IS LOCKED *****'           
         MVI   NOWSW,C'N'         PRINT,BUT DON'T WRITE                         
         MVI   WARNING,C'W'       LINE FLAGGED AS ERROR                         
         B     KPBLD30            GET NEXT EL                                   
         EJECT                                                                  
**********************************************************************          
* KPNY EOF ROUTINES                                                  *          
**********************************************************************          
         SPACE 1                                                                
KPNYEND  MVI   KPNYEOF,C'E'       SET END OF TAPE INDICATOR                     
         B     KPXIT                                                            
*                                                                               
KPCKEOF  BAS   RE,KPDEPTOT        FINAL DEPT TOTAL                              
         BAS   RE,KPREQTOT        REQUEST TOTAL                                 
         MVI   MSAL,C'E'          INDICATES EOJ TO ROOT PROGRAM                 
         CLOSE KPNYTAPE                                                         
         GOTO1 ADSORTER,KPDMCB,=C'END'                                          
*                                                                               
KPXIT    XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* UTILITY ROUTINES                                                   *          
**********************************************************************          
         SPACE 1                                                                
*                                                                               
* READ A TAPE RECORD                                                            
*                                                                               
KPNYREAD NTR1                                                                   
         GET   KPNYTAPE                                                         
         LR    R2,R1              ADDR OF RECORD RETURNED IN R1                 
         XIT1  REGS=(R2)          RESTORE ALL BUT R2                            
*                                                                               
* PUT RECORD TO SORT                                                            
*                                                                               
KPPUTSRT NTR1                                                                   
         GOTO1 ADSORTER,KPDMCB,=C'PUT',KPSRTKEY                                 
         B      KPXIT                                                           
*                                                                               
* CLEAR SORT AREA                                                               
*                                                                               
KPCLRSRT NTR1                                                                   
         XC    KPSRTKEY(KPSRTLNQ),KPSRTKEY                                      
         LA    R3,KPSRTBUC                                                      
         BAS   RE,KPZAP                                                         
         B     KPXIT                                                            
*                                                                               
* READ ACCOUNT RECORD                                                           
*                                                                               
KPREADAC NTR1                                                                   
         GOTO1 DATAMGR,KPDMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                  
         B     KPXIT                                                            
         EJECT                                                                  
**********************************************************************          
* TOTALS ROUTINES (DEPT/REQ)                                         *          
**********************************************************************          
         SPACE 1                                                                
*                                                                               
* DEPARTMENT TOTAL                                                              
*                                                                               
KPDEPTOT NTR1                                                                   
         MVI   SPAP,3                                                           
         MVC   PREP2(22),=C'* TOTAL FOR DEPT     *'                             
         MVC   PREP2+17(3),KPSRTOFF                                             
         LA    R2,PREP2+25                                                      
         LA    R3,KPDEPACC                                                      
         BAS   RE,KPPRT                                                         
         BAS   RE,KPZAP                                                         
         B     KPXIT                                                            
*                                                                               
* REQUEST TOTALS                                                                
*                                                                               
KPREQTOT NTR1                                                                   
         MVI   SPAP,3                                                           
         MVC   P(24),=C'** TOTAL FOR REQUEST  **'                               
         LA    R2,P+25                                                          
         LA    R3,KPREQACC                                                      
         BAS   RE,KPPRT                                                         
         BAS   RE,KPZAP                                                         
         B     KPXIT                                                            
         EJECT                                                                  
**********************************************************************          
* ZAP/ADD/PRINT THE BUCKETS AT 0(R3)                                 *          
**********************************************************************          
         SPACE 1                                                                
*                                                                               
* ZAP THE BUCKETS AT 0(R3)                                                      
*                                                                               
KPZAP    NTR1                                                                   
         LA    R1,KPSRTNB                                                       
KPZAP10  ZAP   0(6,R3),=P'0'                                                    
         LA    R3,6(R3)                                                         
         BCT   R1,KPZAP10                                                       
         B     KPXIT                                                            
*                                                                               
* ADD THE BUCKETS AT 0(R3) TO THE BUCKS AT 0(R2)                                
*                                                                               
KPADD    NTR1                                                                   
         LA    R1,KPSRTNB                                                       
KPADD10  AP    0(6,R2),0(6,R3)                                                  
         LA    R2,6(R2)                                                         
         LA    R3,6(R3)                                                         
         BCT   R1,KPADD10                                                       
         B     KPXIT                                                            
*                                                                               
* PRINT THE BUCKETS AT 0(R3) INTO 0(R2) AND THEN A TOTAL                        
*                                                                               
KPPRT    NTR1                                                                   
         LA    R5,KPSRTNB                                                       
         ZAP   KPDUB,=P'0'                                                      
KPPRT10  EDIT  (P6,(R3)),(12,0(R2)),2,COMMAS=YES,FLOAT=-                        
         AP    KPDUB,0(6,R3)                                                    
         LA    R2,15(R2)                                                        
         LA    R3,6(R3)                                                         
         BCT   R5,KPPRT10                                                       
         EDIT  (P8,KPDUB),(12,0(R2)),2,COMMAS=YES,FLOAT=-                       
         B     KPXIT                                                            
         EJECT                                                                  
**********************************************************************          
* STORAGE                                                            *          
**********************************************************************          
         SPACE 1                                                                
KPALSORT DS    A                  ADDR OF LAST SORT                             
KPDUB    DS    PL8                                                              
*                                                                               
KPSRTKEY DS    0C                 KPNY SORTER RECORD                            
KPSRTOFF DS    CL1                     OFFIKP                                   
KPSRTDEP DS    CL2                     DEPT                                     
KPSTDLEN EQU   *-KPSRTKEY         SORT OFF DEPT LENGTH                          
KPSRTSUB DS    CL2                     SUB-DEPT                                 
KPSRTEMP DS    CL6                     EMPLOYEE NUMBER                          
KPSTKLEN EQU   *-KPSRTKEY         SORT KEY KENGTH                               
KPSRTNAM DS    CL30                    NAME                                     
KPSRTBUC EQU   *                   SORT BUCKETS                                 
KPSRTREG DS    PL6                     REGULAR PAY                              
KPSRTOT  DS    PL6                     O/T                                      
KPSRTLNQ EQU   *-KPSRTKEY         RECORD LENGTH                                 
KPSRTNB  EQU   (*-KPSRTREG)/KPBUCKLN NUMBER OF PL6 BUCKET                       
KPBUCKLN EQU 6                                                                  
*                                                                               
KPDMCB   DS    6F                 KPNY DMCB                                     
*                                                                               
KPDEPACC DS    (KPSRTNB)PL(KPBUCKLN) DEPT TOTAL                                 
KPREQACC DS    (KPSRTNB)PL(KPBUCKLN) REQ TOTAL                                  
*                                                                               
KPSRTCRD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
KPRECCRD DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
*                                                                               
KPNYFRST DC    C'Y'               1ST TIME INTO ROUTINE INDICATOR               
KPNYEOF  DC    C' '               END OF TAPE INDICATOR                         
KPSVKEY  DC    CL7' '             6 CHAR. SUBDEP/STAFF CODE FROM TAPE           
         EJECT                                                                  
**********************************************************************          
* KPNY TAPE DCB                                                                 
**********************************************************************          
         SPACE 1                                                                
KPNYTAPE DCB   DDNAME=KPNYTAPE,                                        X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=60,                                               X        
               BLKSIZE=60,                                             X        
               MACRF=GL,                                               X        
               EODAD=KPNYEND                                                    
         EJECT                                                                  
**********************************************************************          
* KPNY HEAD HOOK ROUTINE                                                        
**********************************************************************          
         SPACE 1                                                                
KPHOOK   NMOD1 0,**KPHK**                                                       
         MVI   HEAD4,X'00'                                                      
         MVC   HEAD5,SPACES                    CLEAR THE LINE                   
         MVC   HEAD5+26(11),=C'REGULAR PAY'                                     
         MVC   HEAD5+44(8),=C'OVERTIME'                                         
         MVC   HEAD5+62(5),=C'TOTAL'                                            
         MVC   HEAD5+80(4),=C'NAME'                                             
*                                                                               
         GOTO1 =A(BASEHOOK)                                                     
         XMOD1                                                                  
         EJECT                                                                  
**********************************************************************          
* KPNY LITERALS                                                                 
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* KPNY WORK DSECT                                                               
**********************************************************************          
         SPACE 1                                                                
KPWORK   DSECT                                                                  
KPNYKEY  DS    CL15' '                                                          
         DS    CL2005                                                           
         EJECT                                                                  
**********************************************************************          
* COMMON                                                             *          
**********************************************************************          
         SPACE 1                                                                
PAGELIST CSECT                                                                  
         DC    1500CL3' '                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* KALLIR PHILIPS AND ROSS (KPNY) TAPE DSECT                          *          
**********************************************************************          
         SPACE 1                                                                
KPNYRECD DSECT                                                                  
KPNYOFF  DS    CL1                 OFFICE                                       
         DS    CL1                                                              
KPNYDEP  DS    CL2                 DEPT                                         
         DS    CL1                                                              
KPNYEMP  DS    CL5                 EMPNO                                        
KPNYNAME DS    CL30                EMPLOYEE NAME                                
KPNYREG  DS    CL9                 REGULAR SAL                                  
KPNYOT   DS    CL9                 OT                                           
         DS    CL2                 SPARE                                        
         EJECT                                                                  
**********************************************************************          
* COMMON INTERFACE DSECT                                             *          
**********************************************************************          
         SPACE 1                                                                
MANFRECD DSECT                                                                  
MEMPNO   DS    0CL15               EMPL NO. (C/U/L/ACT#)                        
MEMPCPY  DS    XL1                 EMPLOYEE COMPANY                             
MEMPUL   DS    CL2                 EMPLOYEE UNIT/LEDGER                         
MEMPACT  DS    CL12                EMPLOYEE ACCOUNT                             
MEMPNM   DS    CL36                EMPL NAME                                    
MEMPLNQ  EQU   *-MEMPNO            EMPLOYEE NUMBER AND NAME LENGTH              
MEMPFN   DS    CL36                FIRST NAME                                   
MEMPLN   DS    CL36                LAST  NAME                                   
MBUCKS   DS    0P                                                               
MSAL     DS    PL6                 REG SALARY AMOUNT                            
MOT      DS    PL6                 OVERTIME                                     
MTEMP    DS    PL6                 TEMPORARY                                    
MPEN     DS    PL6                 PENSION                                      
MBON     DS    PL6                 BONUS                                        
MBEN     DS    PL6                 BENEFIT                                      
MBUD     DS    PL6                 BUDGETED AMOUNT                              
MRATE    DS    PL6                 RATE                                         
MNBUCKS  EQU   (*-MSAL)/L'MSAL     NUMBER OF BUCKETS IN THIS DSECT              
MBASIS   DS    CL1                 BASIS OVERIDE OF CONTROL CARD DATA           
MNOEND   DS    CL1                 Y, MAKE POSTING OPEN ENDED                   
MANLNQ   EQU   *-MANFRECD                                                       
         EJECT                                                                  
**********************************************************************          
* ++INCLUDES                                                         *          
**********************************************************************          
         SPACE 1                                                                
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE DMWRKRK                                                        
         PRINT ON                                                               
* ACGENFILE                                                                     
* ACGENBOTH                                                                     
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACREPWKRKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACREPAS02 04/21/99'                                      
         END                                                                    
