*          DATA SET PPREPCM02  AT LEVEL 029 AS OF 07/17/18                      
*                                                                               
         TITLE 'PPCM02 - CHANGE LOG'                                            
*                                                                               
*  SMUR 04/18      NEW MEDIA (D)IGITAL MEDIA                                    
*                                                                               
*  BPLA 06/15      CHANGES FOR NEW MEDIA CODES                                  
*                                                                               
*  BPLA 02/24/14 - CHANGE FOR NEW OPTION TO COPY DIVISIONS                      
*                  QOPT5 Y= WHEN COPYING CLTS ALSO COPY DIVISIONS               
*                           AND PRESERVE PRD DIVISION ASSIGNMENTS               
*                        N= DON'T COPY DIVISIONS AND REMOVE PRD                 
*                           DIVISION ASSIGNMENTS                                
*                                                                               
*  SMYE 10/01/02 - FIX QOPT7 (TEST INDICATOR) BUG                               
*                                                                               
*                                                                               
     TITLE 'PPCM02 - COPY CLT/PRD HEADERS FROM ONE MEDIA TO ANOTHER'            
*                                                                               
***********************************************************************         
*    (BELOW MULTI-MEDIA LOGIC HAS BEEN NO-OPPED (*NOP*) DEC/2000      *         
*                                                                     *         
*     QOPT1 TO     MEDIA CODE- DIFFERENT FROM QMEDIA FIELD       *              
***********************************************************************         
*                                                                               
*     QOPT1       = MEDIA TO BE COPIED TO                                       
*     QOPT3      Y= COPY USER-DEFINITION ELEMENTS                               
*     QOPT4      Y= COPY BILL-FORMULA DATA (PPRDBILP) IN PRD REC                
*     QOPT5      Y= COPY DIVISIONS + PRESERVE PRD ASSSGNMENTS                   
*                                                                               
*     QOPT6      Y= DUMP FIRST 15 RECORDS COPIED (NOT IN REQ PROGRAM)           
*     QOPT7      Y= DON'T MARK FILE (EVEN IF WRITE=YES)                         
*                                                                               
***********************************************************************         
*  CLIENT    PRODUCT                                                  *         
*                                                                     *         
*   ALL                ALL CLIENT HDRS  -  NO  PRODUCT HDRS           *         
*   ALL       ALL      ALL CLIENT HDRS  -  ALL PRODUCT HDRS           *         
*   XXX                XXX CLIENT HDR   -  NO  PRODUCT HDRS           *         
*   XXX       ALL      XXX CLIENT HDR   -  ALL PRD HDRS FOR CLT XXX   *         
*   XXX       XXX      XXX CLIENT HDR   -  XXX PRD HDR  FOR CLT XXX   *         
***********************************************************************         
*                  FOR CLIENT HEADERS                                 *         
* MEDIA NAME OVERRIDE ELEMENT WILL BE DELETED                         *         
* STANDARD COMMENT ELEMENTS WILL BE DELETED                           *         
* USER-DEFINITION ELEMS WILL ONLY BE DELETED BY USER REQUEST (QOPT3=N)*         
*                                                                     *         
***********************************************************************         
*                  FOR PRODUCT HEADERS                                *         
* BILL-FORMULA DATA WILL ONLY BE CLEARED BY USER REQUEST  (QOPT4=N)   *         
* USER-DEFINITION ELEMS WILL ONLY BE DELETED BY USER REQUEST (QOPT3=N)*         
*                                                                     *         
***********************************************************************         
*                                                                               
*                                                                               
*PHASE PPCM02A                                                                  
*INCLUDE MININAM                                                                
         TITLE 'PPCM02 - PRTFIX PROGRAM TO COPY CLT/PRD HEADERS'                
         PRINT NOGEN                                                            
PPCM02   CSECT                                                                  
         NMOD1 0,PPCM02,R7         *** NOTE R7 AS SECOND BASE                   
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPCMWRKD,R8                                                      
**                                                                              
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*NOP*    CLI   MODE,RUNLAST                                                     
*NOP*    BE    RUNL                                                             
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   OUTCNT,=P'0'                                                     
         ZAP   INCNT,=P'0'                                                      
         ZAP   CLTIN,=P'0'                                                      
         ZAP   CLTDUP,=P'0'                                                     
         ZAP   DIVDUP,=P'0'                                                     
         ZAP   CLTOUT,=P'0'                                                     
         ZAP   PRDIN,=P'0'                                                      
         ZAP   PRDDUP,=P'0'                                                     
         ZAP   PRDOUT,=P'0'                                                     
         ZAP   DIVOUT,=P'0'                                                     
         ZAP   WRTCNT,=P'0'                                                     
         MVI   REPWARN,C'N'  WILL BE SET TO Y IF REPS NEED TO BE ADDED          
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         XC    P,P                                                              
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVI   RCWRITE,C'N'                                                     
         CLI   QOPT7,C'N'          N = LIVE RUN (NOT TEST)                      
         BNE   *+8                                                              
         MVI   RCWRITE,C'Y'                                                     
*                                                                               
         MVC   P+105(20),=C'NO O/P MEDIA ENTERED'                               
         CLC   QOPT1(5),SPACES                                                  
         BNH   ERR                 AT LEAST 1 MEDIA MUST BE ENTERED             
*                                                                               
         LA    R1,QOPT1                                                         
         LA    R0,1                                                             
         XC    P,P                                                              
         MVC   P+105(21),=C'O/P MEDIA = I/P MEDIA'                              
*                                                                               
LUP10    CLC   QMEDIA,0(R1)                                                     
         BE    ERR                 O/P MEDIA MUST DIFFER FROM I/P               
         LA    R1,1(R1)                                                         
         BCT   R0,LUP10                                                         
*                                                                               
         LA    R1,QOPT1                                                         
         LA    R0,1                                                             
         XC    P,P                                                              
         MVC   P+105(13),=C'INVALID MEDIA'                                      
*                                                                               
LUP20    LA    R2,MEDTYP                                                        
LUP20C   CLC   0(1,R1),0(R2)                                                    
         BE    LUP20NX                                                          
         LA    R2,1(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BNE   LUP20C                                                           
         B     ERR                 INVALID MEDIA ENTRY                          
LUP20NX  LA    R1,1(R1)                                                         
         BCT   R0,LUP20                                                         
         XC    P,P                                                              
*                                                                               
***********************************************************************         
         B     CLTC1        BYPASS "MULTI-MEDIA" TESTING, ETC., BELOW           
*                           (REQUEST PROGRAM VALIDATES "COPY TO" MEDIA)         
***********************************************************************         
CKCLT1   CLC   QCLIENT,SPACES                                                   
         BNH   ERR                                                              
*                                  CHECK FOR VALID O/P MEDIA                    
         LA    R1,QOPT1                                                         
         LA    R0,1                NOW ONLY ONE MEDIA                           
         XC    P,P                                                              
         MVC   P+105(24),=C'INVALID AGENCY O/P MEDIA'                           
*                                                                               
CKAGY05  DS    0H                                                               
         CLI   0(R1),C' '          MEDIA ENTERED ?                              
         BE    CKAGY90             NO - NEXT                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY      AGENCY                                       
         MVC   KEY+2(1),0(R1)      O/P MEDIA                                    
         MVI   KEY+3,X'01'         AGYREC                                       
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      FOUND ?                                      
         BNE   ERR                 NO                                           
*                                                                               
CKAGY90  LA    R1,1(R1)                                                         
         BCT   R0,CKAGY05                                                       
*                                                                               
*                                                                               
CLTC1    DS    0H                                                               
         MVI   RCSUBPRG,10                                                      
         XC    P,P                                                              
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      AGENCY,MEDIA                                 
         MVI   KEY+3,X'02'         CLTREC                                       
         CLC   QCLIENT,=C'ALL'     IF CLT IS ALL                                
         BNE   CLTC1D                     |                                     
         CLC   QPRODUCT,=C'ALL'           |                                     
         BE    CLTC2                      |                                     
         CLC   QPRODUCT,SPACES            |                                     
         BE    CLTC2                      |                                     
         MVC   P+105(24),=C'PRODUCT NOT ALL OR BLANK'                           
         B     ERR                 PRD MUST BE ALL OR BLANK                     
*                                                                               
CLTC1D   MVC   KEY+4(3),QCLIENT                                                 
*                                                                               
CLTC2    GOTO1 HIGH                                                             
         B     CLTC4                                                            
*                                                                               
CLTC3    DS    0H                                                               
         GOTO1 SEQ                                                              
CLTC4    DS    0H                                                               
         CLC   KEY(3),QAGENCY     MATCH AGENCY/MEDIA                            
         BNE   CLTEND                                                           
         CLI   KEY+3,X'02'         CLTREC                                       
         BNE   CLTEND                                                           
         CLC   QCLIENT,=C'ALL'                                                  
         BE    CLTC6                                                            
         CLC   KEY+4(3),QCLIENT    SAME CLIENT                                  
         BNE   CLTEND              NO                                           
*                                                                               
CLTC6    DS    0H                                                               
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         AP    INCNT,=P'1'                                                      
         AP    CLTIN,=P'1'                                                      
*                                                                               
         MVC   SAVEKEY,KEY         SAVE KEY                                     
*                                                                               
         MVC   NEWMED,QOPT1                                                     
         BAS   RE,CLTPRT                                                        
*                                                                               
         B     CLUPX               FOR ONE MEDIA OUTPUT                         
*                                                                               
*                                                                               
CLUPX    DS    0H                                                               
         MVC   KEY(32),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                  AND SEQUENCE                               
         B     CLTC3               NEXT CLIENT RECORD                           
*                                                                               
CLTPRT   DS    0H                                                               
         NTR1                                                                   
         MVC   KEY+2(1),NEWMED                                                  
         MVC   PCLTKMED,NEWMED                                                  
*                                                                               
         MVC   P(2),PCLTKAGY                                                    
         MVC   P+06(1),PCLTKMED                                                 
         MVC   P+09(3),PCLTKCLT                                                 
         MVC   P+19(20),PCLTNAME                                                
*                                                                               
CPRT5    DS    0H                  TEST FOR DUPLICATION                         
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'    "RESET" TO NOT PASS DELETES               
         CLC   KEY(25),KEYSAVE     DOES RECORD ALREADY EXIST ?                  
         BNE   CPRT10              NO - OK TO COPY                              
         AP    CLTDUP,=P'1'                                                     
         MVC   P+43(16),=C'*** CLIENT NAME='                                    
         MVC   P+80(32),=C'ALREADY ON FILE - NOT COPIED ***'                    
         GOTO1 GETPRT                                                           
         MVC   P+59(20),PCLTNAME                                                
         BAS   RE,RPRT                                                          
         MVC   KEY(32),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                  AND SEQUENCE                               
         GOTO1 GETPRT                AND RECORD                                 
         B     CLTDIV              CHECK IF COPTING DIVISIONS                   
*                                                                               
CPRT10   DS    0H                                                               
         BAS   RE,RPRT                                                          
         AP    OUTCNT,=P'1'                                                     
         AP    CLTOUT,=P'1'                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(25),PCLTREC                                                  
*                                                                               
***********************************************************************         
*         ***********   SEPTEMBER, 2000   ***********                 *         
* FOR THIS RUN FOR MINDSHARE AGENCY, DELETE ONLY MEDIA NAME OVERRIDE  *         
* ELEMENT IF ONE EXISTS - FOR SUBSEQUENT GENERAL, USER REQUESTED USE  *         
* OF THIS PROGRAM, STANDARD COMMENT ELEMENTS WILL ALSO BE DELETED.    *         
* USER-DEFINITION ELEMENTS WILL ONLY BE DELETED BASED ON QOPT3.       *         
*                                                                     *         
***********************************************************************         
         LA    R2,PCLTREC+33       POINT TO CLT RECORD'S FIRST ELEM             
         CLI   0(R2),X'02'                                                      
         BE    *+6                                                              
         DC    H'0'                BAD RECORD OR PROCESSING WRONG REC           
*                                                                               
CPRT20   LA    R2,PCLTREC+33                                                    
         MVI   ELCODE,X'10'        CONTRACT STANDARD COMMENT ELEM               
         BAS   RE,NEXTEL                                                        
         BE    CPRT30              GO DELETE THIS ELEM                          
*                                                                               
         LA    R2,PCLTREC+33                                                    
         MVI   ELCODE,X'11'        I/O STANDARD COMMENT ELEMS                   
         BAS   RE,NEXTEL                                                        
         BNE   CPRT40              CHECK FOR USER DEFINITION ELEMENT            
*                                                                               
CPRT30   DS    0H                                                               
         MVI   ALLOWLIN,3                                                       
         MVC   P+01(14),=C'(X10) CONTRACT'                                      
         LA    RE,P+16                                                          
         CLI   ELCODE,X'11'                                                     
         BNE   *+14                                                             
         MVC   P+01(10),=C'(X11) I/O '                                          
         LA    RE,P+11                                                          
*                                                                               
         MVC   00(35,RE),=C'STANDARD COMMENT ELEMENT NOT COPIED'                
         LA    RE,35(RE)                                                        
         MVC   00(19,RE),=C' ** COMMENT NUMBER='                                
         LA    RE,19(RE)                                                        
         MVC   00(06,RE),2(R2)     STANDARD COMMENT NUMBER                      
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT             SKIP A LINE                                  
*                                                                               
         GOTO1 RECUP,DMCB,(1,PCLTREC),(R2),0       DELETE ELEMENT               
*                                                                               
         B     CPRT20              GO BACK AND CHECK FOR MORE                   
*                                                                               
CPRT40   DS    0H                                                               
*                                                                               
         CLI   QOPT3,C'Y'          DELETE USER DEFINITION ELEMENTS ?            
         BE    CPRT50              NO                                           
*                                                                               
         LA    R2,PCLTREC+33                                                    
         MVI   ELCODE,X'20'        USER DEFINITION FIELDS ELEMENT               
         BAS   RE,NEXTEL                                                        
         BNE   CPRT50              CHECK FOR MEDIA NAME OVERRIDE ELEM           
*                                                                               
*                                DELETE USER DEFINITION FIELDS ELEMENT          
         MVI   ALLOWLIN,6                                                       
         MVC   P+01(36),=C'(X20) USER DEFINITION FIELDS ELEMENT'                
         MVC   P+38(10),=C'NOT COPIED'                                          
         BAS   RE,RPRT                                                          
*                                                                               
         USING PCLTUDEF,R2                                                      
         CLC   PCLTPU1,SPACES      ANYTHING ?                                   
         BNH   CPRT40D             NO                                           
         MVC   P+10(26),=C'** PRODUCT  DESCRIPTION  ='                          
         MVI   P+34,C'1'                                                        
         MVC   P+36(20),PCLTPU1                                                 
         BAS   RE,RPRT                                                          
*                                                                               
CPRT40D  CLC   PCLTPU2,SPACES      ANYTHING ?                                   
         BNH   CPRT40G             NO                                           
         MVC   P+10(26),=C'** PRODUCT  DESCRIPTION  ='                          
         MVI   P+34,C'2'                                                        
         MVC   P+36(20),PCLTPU2                                                 
         BAS   RE,RPRT                                                          
*                                                                               
CPRT40G  CLC   PCLTEU1,SPACES      ANYTHING ?                                   
         BNH   CPRT40J             NO                                           
         MVC   P+10(26),=C'** ESTIMATE DESCRIPTION  ='                          
         MVI   P+34,C'1'                                                        
         MVC   P+36(20),PCLTEU1                                                 
         BAS   RE,RPRT                                                          
*                                                                               
CPRT40J  CLC   PCLTEU2,SPACES      ANYTHING ?                                   
         BNH   CPRT40X             NO                                           
         MVC   P+10(26),=C'** ESTIMATE DESCRIPTION  ='                          
         MVI   P+34,C'2'                                                        
         MVC   P+36(20),PCLTEU2                                                 
         BAS   RE,RPRT                                                          
*                                                                               
CPRT40X  BAS   RE,RPRT             SKIP A LINE                                  
*                                                                               
         GOTO1 RECUP,DMCB,(1,PCLTREC),(R2),0       DELETE ELEMENT               
*                                                                               
         B     CPRT40              GO BACK AND CHECK FOR MORE                   
*                                                                               
         DROP  R2                                                               
*                                                                               
CPRT50   DS    0H                                                               
         LA    R2,PCLTREC+33                                                    
         MVI   ELCODE,X'41'        MEDIA NAME OVERRIDE ELEMENT                  
         BAS   RE,NEXTEL                                                        
         BNE   CPRT90              DONE WITH CLIENT                             
*                                                                               
*                                DELETE MEDIA NAME OVERRIDE ELEMENT             
         MVI   ALLOWLIN,2                                                       
         MVC   P+01(33),=C'(X41) MEDIA NAME OVERRIDE ELEMENT'                   
         MVC   P+35(10),=C'NOT COPIED'                                          
         MVC   P+50(23),=C'** MEDIA NAME OVERRIDE='                             
         MVC   P+73(10),2(R2)                                                   
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT             SKIP A LINE                                  
*                                                                               
         GOTO1 RECUP,DMCB,(1,PCLTREC),(R2),0       DELETE ELEMENT               
*                                                                               
CPRT90   DS    0H                                                               
         CLI   QOPT6,C'Y'          DUMP OUTPUT ?                                
         BNE   CLTWRT              NO                                           
         CP    CLTOUT,=P'15'       15 RECORDS DUMPED ?                          
         BH    CLTWRT              YES                                          
         BAS   RE,DMPREC                                                        
         BAS   RE,RPRT             SKIP A LINE                                  
*                                                                               
CLTWRT   CLI   RCWRITE,C'N'                                                     
         BE    CLTDIV                                                           
         CLI   QOPT7,C'Y'                                                       
         BE    CLTDIV                                                           
         AP    WRTCNT,=P'1'                                                     
         GOTO1 ADDPRT                                                           
         XC    KEY,KEY           SET TO RECORD JUST ADDED                       
         MVC   KEY(25),PCLTREC                                                  
*                                                                               
*                                                                               
CLTDIV   CLI   QOPT5,C'Y'        SEE IF PRESERVING DIVISIONS                    
         BNE   EXIT                                                             
         MVC   CSAVEKEY,KEY       SAVE KEY                                      
         MVC   CSAVEKEY+2(1),QMEDIA  RESET MEDIA                                
*                                                                               
         MVI   KEY+3,X'03'                                                      
         MVC   KEY+2(1),QMEDIA     RESET TO REQ MEDIA                           
         GOTO1 HIGH                                                             
         B     CLTDIV5                                                          
CLTDIV2  GOTO1 SEQ                                                              
*                                                                               
CLTDIV5  CLC   KEY(7),KEYSAVE      CHECK AGY/MED/CLT                            
         BNE   CLTDIVX             DONE COPYING DIVISIONS                       
         LA    R0,PDIVREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   PDIVKMED,NEWMED     SET IN THE RECORD                            
         MVC   KEY+2(1),NEWMED     AND IN KEY                                   
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     DOES RECORD ALREADY EXIST ?                  
         BNE   CLTDIV6             NO - OK TO COPY                              
         MVC   P+43(12),=C'*** DIVISION'                         '              
         MVC   P+58(3),KEY+7                                                    
         MVC   P+64(32),=C'ALREADY ON FILE - NOT COPIED ***'                    
         BAS   RE,RPRT                                                          
         AP    DIVDUP,=P'1'                                                     
         B     CLTDIV10                                                         
*                                                                               
CLTDIV6  CLI   QOPT6,C'Y'          DUMP OUTPUT ?                                
         BNE   CLTDIV8             NO                                           
         CP    DIVOUT,=P'15'       15 RECORDS DUMPED ?                          
         BH    CLTDIV8             YES                                          
         BAS   RE,DMPREC                                                        
         BAS   RE,RPRT             SKIP A LINE                                  
*                                                                               
CLTDIV8  CLI   RCWRITE,C'N'                                                     
         BE    CLTDIV10                                                         
         CLI   QOPT7,C'Y'                                                       
         BE    CLTDIV10                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(25),PDIVREC                                                  
         AP    DIVOUT,=P'1'                                                     
         AP    WRTCNT,=P'1'                                                     
         GOTO1 ADDPRT                                                           
         MVC   P+43(12),=C'*** DIVISION'                                        
         MVC   P+58(3),KEY+7                                                    
         MVC   P+64(09),=C'ADDED ***'                                           
         CLC   PDIVREP,SPACES                                                   
         BNH   CLTDIV9                                                          
         MVC   P+80(12),=C'BILLING REP='                                        
         MVC   P+94(4),PDIVREP                                                  
         MVC   PSECOND+43(37),=C'*** WARNING-REP WILL NEED TO BE ADDED'         
         MVI   REPWARN,C'Y'                                                     
CLTDIV9  BAS   RE,RPRT                                                          
*                                                                               
CLTDIV10 XC    KEY,KEY                                                          
         MVC   KEY(10),PDIVREC                                                  
         MVC   KEY+2(1),QMEDIA     RESET TO REQUEST MEDIA/CLT                   
         MVI   KEY+3,X'03'         FOR DIV RECS                                 
         GOTO1 HIGH                                                             
         B     CLTDIV2              CHECK FOR MORE DIVISIONS                    
*                                                                               
CLTDIVX  MVC   KEY(25),CSAVEKEY      RESTORE CLIENT SEQ READ                    
         GOTO1 HIGH                                                             
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
CLTEND   DS    0H                  NOW CHECK FOR PRODUCT HDRS                   
         CLI   QPRODUCT,C' '       ANY REQUESTED ?                              
         BNE   PRDC1               YES                                          
         B     RUNL                                                             
*                                                                               
PRDC1    DS    0H                                                               
         MVI   RCSUBPRG,20                                                      
         MVI   FORCEHED,C'Y'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      AGENCY,MEDIA                                 
         MVI   KEY+3,X'06'         PRDREC                                       
         CLC   QCLIENT,=C'ALL'                                                  
         BE    PRDC2                                                            
         MVC   KEY+4(3),QCLIENT                                                 
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    PRDC2                                                            
         MVC   KEY+7(3),QPRODUCT                                                
*                                                                               
PRDC2    GOTO1 HIGH                                                             
         B     PRDC4                                                            
*                                                                               
PRDC3    DS    0H                                                               
         GOTO1 SEQ                                                              
PRDC4    DS    0H                                                               
         CLC   KEY(3),QAGENCY     MATCH AGENCY/MEDIA                            
         BNE   PRDEND                                                           
         CLI   KEY+3,X'06'         PRDREC                                       
         BNE   PRDEND                                                           
         CLC   QCLIENT,=C'ALL'                                                  
         BE    PRDC6                                                            
         CLC   KEY+4(3),QCLIENT    SAME CLIENT ?                                
         BNE   PRDEND              NO                                           
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    PRDC6                                                            
         CLC   KEY+7(3),QPRODUCT   SAME PRODUCT ?                               
         BNE   PRDEND              NO                                           
*                                                                               
PRDC6    DS    0H                                                               
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         AP    INCNT,=P'1'                                                      
         AP    PRDIN,=P'1'                                                      
*                                                                               
         MVC   SAVEKEY,KEY         SAVE KEY                                     
*                                                                               
         MVC   NEWMED,QOPT1                                                     
         BAS   RE,PRDPRT                                                        
*                                                                               
***********************************************************************         
         B     PLUPX               ONLY ONE MEDIA COPIED                        
***********************************************************************         
*                                                                               
PLUPX    DS    0H                                                               
         MVC   KEY(32),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                  AND SEQUENCE                               
         B     PRDC3               NEXT PRODUCT RECORD                          
*                                                                               
PRDPRT   DS    0H                                                               
         NTR1                                                                   
         MVC   KEY+2(1),NEWMED                                                  
         MVC   PPRDKMED,NEWMED                                                  
*                                                                               
         MVC   P(2),PPRDKAGY                                                    
         MVC   P+06(1),PPRDKMED                                                 
         MVC   P+09(3),PPRDKCLT                                                 
         MVC   P+14(3),PPRDKPRD                                                 
         MVC   P+19(20),PPRDNAME                                                
*                                                                               
PPRT5    DS    0H                  TEST FOR DUPLICATION                         
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'    "RESET" TO NOT PASS DELETES               
         CLC   KEY(25),KEYSAVE     DOES RECORD ALREADY EXIST ?                  
         BNE   PPRT10              NO - OK TO COPY                              
         AP    PRDDUP,=P'1'                                                     
         MVC   P+43(38),=C'*** PRODUCT NAME=                     '              
         MVC   P+81(32),=C'ALREADY ON FILE - NOT COPIED ***'                    
         GOTO1 GETPRT                                                           
         MVC   P+60(20),PPRDNAME                                                
         BAS   RE,RPRT                                                          
         MVC   KEY(32),SAVEKEY     RESTORE KEY                                  
         GOTO1 HIGH                  AND SEQUENCE                               
         GOTO1 GETPRT                AND RECORD                                 
         B     EXIT                                                             
*                                                                               
PPRT10   DS    0H                                                               
         BAS   RE,RPRT                                                          
         AP    OUTCNT,=P'1'                                                     
         AP    PRDOUT,=P'1'                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(25),PPRDREC                                                  
*                                                                               
*                                                                               
***********************************************************************         
*         ***********   SEPTEMBER, 2000   ************                *         
* FOR THIS RUN FOR MINDSHARE AGENCY, COPY PRODUCT RECORD IN ITS'      *         
* ENTIRETY.           FOR SUBSEQUENT GENERAL, USER REQUESTED USE      *         
* OF THIS PROGRAM, BILL-FORMULA DATA WILL BE CLEARED AND              *         
* USER-DEFINITION ELEMENTS WILL BE DELETED BASED ON OPTIONS           *         
*                                                                     *         
***********************************************************************         
*                                                                               
*NOP*    B     PPRT90              GO WRITE COPIED RECORD                       
*                                                                               
PPRT30   DS    0H                                                               
         CLI   QOPT4,C'Y'          CLEAR BILL-FORMULA DATA ?                    
         BE    PPRT30A             NO                                           
*                                                                               
         XC    PPRDBILP,PPRDBILP                                                
*                                                                               
PPRT30A  CLI   QOPT3,C'Y'          DELETE USER DEFINITION ELEMENTS ?            
         BE    PPRT40              NO                                           
*                                                                               
PPRT30B  LA    R2,PPRDREC+33                                                    
         MVI   ELCODE,X'08'        USER DEFINITION FIELDS ELEMENT               
         BAS   RE,NEXTEL                                                        
         BNE   PPRT40              DONE WITH PRD                                
*                                                                               
*                                DELETE USER DEFINITION FIELDS ELEMENT          
         MVI   ALLOWLIN,4                                                       
         MVC   P+01(36),=C'(X08) USER DEFINITION FIELDS ELEMENT'                
         MVC   P+38(10),=C'NOT COPIED'                                          
         BAS   RE,RPRT                                                          
*                                                                               
         USING PPRDUDEF,R2                                                      
         CLC   PUSER1,SPACES       ANYTHING ?                                   
         BNH   PPRT30D             NO                                           
         MVC   P+10(22),=C'** USER DESCRIPTION  ='                              
         MVI   P+30,C'1'                                                        
         MVC   P+32(32),PUSER1                                                  
         BAS   RE,RPRT                                                          
*                                                                               
PPRT30D  CLC   PUSER2,SPACES       ANYTHING ?                                   
         BNH   PPRT30X             NO                                           
         MVC   P+10(22),=C'** USER DESCRIPTION  ='                              
         MVI   P+30,C'2'                                                        
         MVC   P+32(16),PUSER2                                                  
         BAS   RE,RPRT                                                          
*                                                                               
PPRT30X  BAS   RE,RPRT             SKIP A LINE                                  
*                                                                               
         GOTO1 RECUP,DMCB,(1,PPRDREC),(R2),0       DELETE ELEMENT               
*                                                                               
PPRT40   DS    0H                  SEE IF DELETING DIVS                         
         CLI   PPRDDIV,C' '                                                     
         BNH   PPRT90                                                           
         MVC   P+10(24),=C'** DIVISION ASSIGNMENT ='                            
         MVC   P+36(3),PPRDDIV                                                  
         CLI   QOPT5,C'Y'     SEE IF PRESERVING DIVS                            
         BE    PPRT50                                                           
         XC    PPRDDIV,PPRDDIV                                                  
         MVC   P+42(13),=C'** REMOVED **'                                       
         BAS   RE,RPRT             SKIP A LINE                                  
         B     PPRT90                                                           
*                                                                               
PPRT50   DS    0H                                                               
         BAS   RE,RPRT             SKIP A LINE                                  
*                                                                               
         DROP  R2                                                               
*                                                                               
*                                                                               
PPRT90   DS    0H                                                               
         CLI   QOPT6,C'Y'          DUMP OUTPUT ?                                
         BNE   PRDWRT              NO                                           
         CP    PRDOUT,=P'15'       15 RECORDS DUMPED ?                          
         BH    PRDWRT              YES                                          
         BAS   RE,DMPREC                                                        
         BAS   RE,RPRT             SKIP A LINE                                  
*                                                                               
PRDWRT   CLI   RCWRITE,C'N'                                                     
         BE    EXIT                                                             
         CLI   QOPT7,C'Y'                                                       
         BE    EXIT                                                             
         AP    WRTCNT,=P'1'                                                     
         GOTO1 ADDPRT                                                           
         B     EXIT                                                             
*                                                                               
PRDEND   DS    0H                                                               
******   B     RUNL                                                             
         EJECT                                                                  
*                                                                               
RUNL     DS    0H                                                               
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
                                                                                
         CLI   REPWARN,C'Y'                                                     
         BNE   RUNL2                                                            
         MVC   P+10(68),=C'** WARNING - DIVISION BILLING REP ASSIGNMENTX        
               S FOUND AND PRESERVED **'                                        
         GOTO1 REPORT                                                           
         MVC   P+10(43),=C'** THEY MUST BE ADDED TO YOUR FILES ASAP **'         
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P+10(36),=C'**** DO NOT IGNORE THIS WARNING ****'                
         MVI   SPACING,2                                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
RUNL2    LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
*NOP*    BE    EXIT                                                             
         BE    RUNF                                                             
         MVC   P(25),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+27(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,33(R4)           NEXT COUNTER IN TABLE                        
         B     RUNL5                                                            
*                                                                               
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         CLI   RCWRITE,C'Y'                                                     
         BE    *+10                                                             
         MVC   HEAD3+26(21),=C'*** TEST RUN ONLY ***'                           
         MVC   HEAD3+61(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD3+67(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
ERR      DS    0H                                                               
         MVC   P(24),=C'REQUEST CARD IS INVALID:'                               
         MVC   P+25(78),QAGENCY                                                 
*                                                                               
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
         EJECT                                                                  
*                                                                               
DMPKEY   NTR1                                                                   
         SPACE 1                                                                
         LA    R5,KEY                                                           
         LA    R2,25                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+75(25),WORK                                                    
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         MVC   HALF,25(R5)        RECORD LENGTH                                 
         LH    R2,HALF                                                          
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
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
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
MEDTYP   DC    C' BDILMNOSTVW',X'FF'                                            
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DS    PL8                                                              
         DC    CL25'TOTAL RECORDS READ'                                         
OUTCNT   DS    PL8                                                              
         DC    CL25'TOTAL RECORDS COPIED'                                       
CLTIN    DS    PL8                                                              
         DC    CL25'CLIENT RECORDS READ'                                        
CLTOUT   DS    PL8                                                              
         DC    CL25'CLIENT RECORDS COPIED'                                      
CLTDUP   DS    PL8                                                              
         DC    CL25'DUPLICATE CLIENT RECORDS'                                   
DIVOUT   DS    PL8                                                              
         DC    CL25'DIVISION RECORDS COPIED'                                    
DIVDUP   DS    PL8                                                              
         DC    CL25'DUPLICATE DIVISION RECORDS'                                 
PRDIN    DS    PL8                                                              
         DC    CL25'PRODUCT RECORDS READ'                                       
PRDOUT   DS    PL8                                                              
         DC    CL25'PRODUCT RECORDS COPIED'                                     
PRDDUP   DS    PL8                                                              
         DC    CL25'DUPLICATE PRODUCT RECORDS'                                  
WRTCNT   DS    PL8                                                              
         DC    CL25'RECORDS ACTUALLY WRITTEN'                                   
         DC    X'FF'                                                            
*                                                                               
PPCMWRKD DSECT                                                                  
ELCODE   DS    X                                                                
REPWARN  DS    C                                                                
NEWMED   DS    C                                                                
SAVEKEY  DS    CL32                                                             
CSAVEKEY DS    CL32                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029PPREPCM02 07/17/18'                                      
         END                                                                    
