*          DATA SET PPINF01    AT LEVEL 051 AS OF 05/01/02                      
*PHASE T41A01A                                                                  
         TITLE 'T41A01 - CHANGE LOG   '                                         
*                                                                               
* SMYE 04/02    NEW LIMIT ACCESS SECURITY AND MAKE PUBVAL CORE-RESIDENT         
*                                                                               
* KWAN 08/99    SAVE OFF CLT COST2 FACTOR IN SVCLTCOS (PPSINFOWRK)              
*                                                                               
* SMYE 3/9/98   FIX BUG IN GETCOM - DISALLOW ZERO-LENGTH (,,) ENTRY             
*                                                                               
* BPLA 2/98     WHEN LISTING ESTIMATES OR BILLS ALLOW                           
*               START AT ESTIMATE (IF PRD IS NOT ALL)                           
*                                                                               
* SMYE 1/24/97  INCORPORATE CLIENT GROUP SECURITY                               
*                                                                               
* BPLA 9/5/95   FIX BUG FOR PPUB REC  (PUBLISHER PUB LISTING)                   
*                                                                               
* BPLA 7/21/95  KEY VALIDATION FOR PUBLISHER AND PPUBLIST                       
*                                                                               
* BPLA 8/27/90  FIXED ROGER'S LOGIC FOR OFFICE LIST ACCESS                      
* ROSA 7/19/90  ALLOW FOR OFFICE LIST ACCESS                                    
* ROSA 6/21/88  ADD KEY AND LOGIC FOR PUB LIST                                  
*                                                                               
         TITLE 'T41A01 - PRINTPAK INFO - KEY VALIDATION'                        
T41A01   CSECT                                                                  
         NMOD1 0,T41A01,RR=R9                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         USING T41AFFD,RA                                                       
         EJECT                                                                  
         XC    FLEN,FLEN                                                        
         LA    R2,SINIKEYH                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    SVKEYDTA,SVKEYDTA                                                
         XC    SVCOFF,SVCOFF        CLEAR CLT OFFICE                            
         LA    R7,KEYTAB                                                        
         LH    R8,0(R7)                                                         
         L     R9,2(R7)                                                         
         A     R9,RELO                                                          
         LA    R7,6(R7)                                                         
*                                                                               
         CLC   SVREC,0(R7)         (USED TO USE SVOVLY)                         
         BE    *+10                                                             
         BXLE  R7,R8,*-10                                                       
         DC    H'0'                                                             
*                                                                               
         L     RE,0(R7)                                                         
         A     RE,RELO                                                          
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
CLT      MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY(2),SVAGY                                                   
         MVC   SVKEY+2(1),SVEBCMED                                              
*                                                                               
         MVI   ERRCD,2                                                          
         BAS   R9,GETCLT                                                        
         MVI   SVKEY+3,X'02'                                                    
         MVC   SVKEY+4(3),SVCLT                                                 
         BAS   R9,RDCLT                                                         
         BAS   RE,GETREC                                                        
         L     R8,AREC                                                          
         USING PCLTREC,R8                                                       
*                                                                               
*                                                                               
*                                                                               
         XC    SVCLTCOS,SVCLTCOS   SAVING OFF CLT HDR'S COST2 FACTOR            
         LA    R9,PCLTELEM                                                      
CLTCF10  CLI   0(R9),0             NO MORE ELEM TO LOOK FOR?                    
         BE    CLTCFX                                                           
         CLI   0(R9),X'45'         COST2 FACTOR ELEM CODE                       
         BNE   CLTCF50                                                          
         USING PCLTCFEL,R9                                                      
         MVC   SVCLTCOS,PCLTCF     GET COST2 FACTOR                             
         DROP  R9                                                               
         B     CLTCFX              DONE WITH COST2 (ONLY ONE ELEM)              
*                                                                               
CLTCF50  ZIC   RE,1(R9)                                                         
         AR    R9,RE               POINT TO NEXT ELEM                           
         B     CLTCF10                                                          
*                                                                               
CLTCFX   DS    0H                                                               
*                                                                               
ACCCHK   DS    0H                  CHECK FOR ANY LIMIT ACCESS                   
         MVC   SVCOFF,PCLTOFF      SAVE CLIENT OFFICE                           
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   ACCXCL                                                           
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    ACCX                NOTHING TO CHECK                             
*                                                                               
ACCXCL   DS    0H                  TEST FOR "IGNORE CHECK" ENTRY                
         CLI   6(RA),C'*'          OFFICE OR CLT GRP LIMIT ACCESS ?             
         BNE   ACCTST              NO                                           
         CLI   PCLTOFF+2,C'*'      IGNORE OFFICE CHECK ?                        
         BNE   ACCTST              NO                                           
         CLI   7(RA),C'A'          SEE IF CLT GRP (*AN)                         
         BL    ACCX                NO - MUST BE OFFICE - IGNORE CHECK           
         CLI   7(RA),C'Z'                                                       
         BH    ACCX                     MUST BE OFFICE - IGNORE CHECK           
         CLI   8(RA),C'0'          SEE IF 3RD BYTE IS NUMERIC                   
         BL    ACCX                NO - NOT CLT GRP - IGNORE CHECK              
         CLI   8(RA),C'9'                                                       
         BH    ACCX                     NOT CLT GRP - IGNORE CHECK              
*                                                                               
ACCTST   DS    0H                                                               
         BRAS  RE,CKTRAFID         TRAFFIC ID SIGN-ON ?                         
         BNE   ACCTSTD             NO                                           
         BRAS  RE,TRAFFACC         LOOK FOR CLIENT TRAFFIC OFFICE CODE          
         CLI   BYTE3,0             ANYTHING FOUND ?                             
         BE    ACCTSTD             NO                                           
         MVC   PCLTOFF,BYTE3       USE CLIENT TRAFFIC OFFICE CODE TO..          
ACCTSTD  DS    0H                  TEST OFFICE SECURITY (ANY ACCESS)            
         BAS   RE,PPCLIVER                                                      
         BE    ACCX                                                             
*                                                                               
ACCE     MVI   ERRCD,ACCERR        NOT AUTHORIZED                               
         B     KEYERR                                                           
*                                                                               
ACCX     DS    0H                                                               
         MVC   PCLTOFF,SVCOFF     "RESTORE" CLIENT OFFICE                       
*                                                                               
         MVC   SVCOFF,PCLTOFF      SAVE OFFICE                                  
*                                                                               
         MVC   SINKMSG+11(20),PCLTNAME                                          
         DROP  R8                                                               
         CLI   SVREC,X'19'                                                      
         BH    CKDIV                                                            
         MVI   ERRCD,3                                                          
         BAS   R9,GETPRD                                                        
         MVI   SVKEY+3,X'06'                                                    
         MVC   SVKEY+7(3),SVPRD                                                 
         CLC   SVPRD,=C'ALL'                                                    
         BE    CLT5                                                             
         BAS   R9,RDPRD                                                         
         CLI   SVREC,X'12'         PRD                                          
         BE    CKPRTX                                                           
*                                                                               
CLT5     CLI   SVREC,X'13'         ESTIMATE                                     
         BE    CLT5C                                                            
         CLI   SVREC,X'14'         OR INVOICES (BILLS)                          
         BNE   CLT10                                                            
CLT5C    MVI   ERRCD,4              FIELD 4                                     
         XC    SVEST,SVEST                                                      
         XC    SVESTB,SVESTB                                                    
         BAS   RE,CHKEND                                                        
         BAS   R9,GETNUM                                                        
         CLC   SVPRD,=C'ALL'      NO EST IF SVPRD = ALL                         
         BE    BADKEY                                                           
         MVC   SVEST,WORK                                                       
         STH   R0,SVESTB          (R0 SET BY GETNUM)                            
         B     CKPRTX             NO NEED TO SEE IF EST EXISTS                  
*                                                                               
CLT10    DS    0H                                                               
         CLI   SVREC,X'15'                                                      
         BE    CKJOB                                                            
         B     CKPRTX                                                           
*                                                                               
CKDIV    DS    0H                                                               
         MVI   ERRCD,3                                                          
         MVI   SVKEY+3,X'03'                                                    
         CLI   SVREC,X'1A'                                                      
         BNE   *+8                                                              
         BAS   RE,CHKEND                                                        
         BAS   R9,GETNUM                                                        
         MVC   SVKEY+7(3),WORK                                                  
         CLI   SVREC,X'1A'         DIV                                          
         BE    CKPRTX                                                           
         MVI   ERRCD,DIVNFND       DIVISION NOT ON FILE                         
         BAS   R9,RDPRT                                                         
         BAS   RE,GETREC                                                        
         L     R8,AREC                                                          
         USING PDIVREC,R8                                                       
         MVC   SINKMSG+11(20),PDIVNAME                                          
         DROP  R8                                                               
*                                                                               
CKREG    MVI   ERRCD,4                                                          
         MVI   SVKEY+3,X'04'                                                    
         CLI   SVREC,X'1B'                                                      
         BNE   *+8                                                              
         BAS   RE,CHKEND                                                        
         BAS   R9,GETNUM                                                        
         MVC   SVKEY+10(3),WORK                                                 
         CLI   SVREC,X'1B'         REG                                          
         BE    CKPRTX                                                           
         MVI   ERRCD,REGNFND       REGION NOT ON FILE                           
         BAS   R9,RDPRT                                                         
         BAS   RE,GETREC                                                        
         L     R8,AREC                                                          
         USING PREGREC,R8                                                       
         MVC   SINKMSG+11(20),PREGNAME                                          
         DROP  R8                                                               
*                                                                               
CKDST    MVI   ERRCD,5                                                          
         MVI   SVKEY+3,X'05'                                                    
         BAS   RE,CHKEND                                                        
         BAS   R9,GETNUM                                                        
         MVC   SVKEY+13(3),WORK                                                 
         B     CKPRTX                                                           
*                                                                               
CKJOB    DS    0H                                                               
         MVI   ERRCD,4                                                          
         MVI   SVKEY+3,X'15'                                                    
         BAS   R9,GETJOB                                                        
         MVC   SVKEY+10(6),WORK                                                 
         B     CKPRTX                                                           
         EJECT                                                                  
REP      MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY(2),SVAGY                                                   
         MVC   SVKEY+2(1),SVEBCMED                                              
         MVI   SVKEY+3,X'11'                                                    
         MVI   ERRCD,2                                                          
         BAS   R9,GETREP                                                        
         MVC   SVKEY+4(4),WORK                                                  
         CLI   SVREC,X'23'              SEE IF PPUBLIST                         
         BE    REP5                                                             
         B     CKPRTX                                                           
*                                                                               
REP5     BAS   R9,RDREP                                                         
         BAS   RE,GETREC                                                        
         L     R8,AREC                                                          
         USING PREPREC,R8                                                       
         TM    PREPSTAT,X'01'        SEE IF A PUBLISHER                         
         BNO   KEYERR                                                           
         MVC   SINKMSG+11(20),PREPNAME                                          
         DROP  R8                                                               
         B     CKPRTX                                                           
*                                                                               
         SPACE 2                                                                
PUBL     MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY(2),SVAGY                                                   
         MVC   SVKEY+2(1),SVEBCMED                                              
         MVI   ERRCD,2                                                          
         BAS   R9,GETCLT                                                        
         MVC   SVKEY+4(3),SVCLT                                                 
         CLC   SVCLT,=C'ZZZ'    SPECIAL FOR 'ALL' CLIENT PUBLISTS               
         BNE   PUBL5            SKIP CLIENT READ                                
         MVC   SINKMSG+11(20),=CL20'*ALL CLIENTS*'                              
         B     PUBL10                                                           
*                                                                               
PUBL5    MVI   SVKEY+3,X'02'                                                    
         BAS   R9,RDCLT        READ CLIENT HEADER                               
         BAS   RE,GETREC                                                        
         L     R8,AREC                                                          
         USING PCLTREC,R8                                                       
         MVC   SINKMSG+11(20),PCLTNAME                                          
         DROP  R8                                                               
PUBL10   MVI   SVKEY+3,X'17'       PUB LIST ID                                  
         MVI   ERRCD,3                                                          
         BAS   R9,GETPUBL      SEE IF START IS SOMETHING OTHER THAN B           
         MVC   SVKEY+7(3),WORK                                                  
         B     CKPRTX                                                           
         SPACE 2                                                                
COM      MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY(2),SVAGY                                                   
         MVC   SVKEY+2(1),SVEBCMED                                              
         MVI   SVKEY+3,X'40'                                                    
         MVI   ERRCD,2                                                          
         BAS   R9,GETCOM                                                        
         MVC   SVKEY+4(6),WORK                                                  
         B     CKPRTX                                                           
         SPACE 2                                                                
AGY      MVI   ERRCD,1                                                          
         BAS   R9,GETAGY                                                        
         MVC   SVKEY(2),SVAGY                                                   
         MVI   SVKEY+3,X'01'                                                    
         B     CKPRTX                                                           
         EJECT                                                                  
GETMED   BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,1                                                         
         BNE   BADKEY                                                           
         MVC   SVEBCMED,0(R4)                                                   
         MVC   SVAGY,AGYALPHA                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),0(R4)                                                   
         MVI   KEY+3,X'01'                                                      
         BAS   RE,HIGH                                                          
         CLC   KEY(4),KEYSAVE                                                   
         BNE   MEDERR                                                           
         BAS   RE,GETREC                                                        
         LA    R7,REC                                                           
         USING PAGYREC,R7                                                       
         MVC   SINKMSG(10),PAGYMED                                              
         FOUT  SINKMSGH                                                         
         BR    R9                                                               
         DROP  R7                                                               
*                                                                               
MEDERR   MVI   ERRCD,RECNFND     MEDIA RECORD NOT FOUND                         
         B     KEYERR                                                           
*                                                                               
         SPACE 2                                                                
GETCLT   CLI   SVREC,X'11'                                                      
         BE    CLTOPT                                                           
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    BADKEY         IN CASE THEY ENTER ,,                             
         CLI   FLEN+1,3                                                         
         BH    BADKEY                                                           
         MVC   SVCLT,SPACES                                                     
         BCTR  R5,0                                                             
         EX    R5,MVCLT                                                         
         BR    R9                                                               
*                                                                               
MVCLT    MVC   SVCLT(0),0(R4)                                                   
*                                                                               
CLTOPT   MVC   SVCLT,SPACES                                                     
         BAS   RE,CHKEND           END                                          
         BAS   RE,FLDVAL            NO - GET START AT CLIENT                    
         LTR   R5,R5                                                            
         BZ    CKPRTX                                                           
         CLI   FLEN+1,3                                                         
         BH    BADKEY                                                           
         BCTR  R5,0                                                             
         EX    R5,MVCLT                                                         
         B     CKPRTX                                                           
*                                                                               
         SPACE 2                                                                
GETPRD   CLI   SVREC,X'12'                                                      
         BE    PRDOPT                                                           
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,2                                                         
         BL    BADKEY                                                           
         CLI   FLEN+1,3                                                         
         BH    BADKEY                                                           
         MVC   SVPRD,SPACES                                                     
         BCTR  R5,0                                                             
         EX    R5,MVPRD                                                         
         BR    R9                                                               
*                                                                               
MVPRD    MVC   SVPRD(0),0(R4)                                                   
*                                                                               
PRDOPT   MVC   SVPRD,SPACES                                                     
         BAS   RE,CHKEND           END                                          
         BAS   RE,FLDVAL            NO - GET START AT PRODUCT                   
         LTR   R5,R5                                                            
         BZ    CKPRTX                                                           
         CH    R5,=H'4'                                                         
         BL    *+8                                                              
         LA    R5,3                                                             
         BCTR  R5,0                                                             
         EX    R5,MVPRD                                                         
         B     CKPRTX                                                           
*                                                                               
         SPACE 2                                                                
GETNUM   BAS   RE,FLDVAL                                                        
         LTR   R5,R5               TEST DATA                                    
         BZ    BADKEY                                                           
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    BADKEY                                                           
         CLI   FLEN+1,3            MAX LEN 3                                    
         BH    BADKEY                                                           
         BCTR  R5,0                                                             
         EX    R5,NUMPACK                                                       
         CVB   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         BR    R9                                                               
NUMPACK  PACK  DUB,0(0,R4)                                                      
*                                                                               
* CHECK FOR END OF INPUT KEY                                                    
CHKEND   L     R4,FADDR                                                         
         LH    R5,FLEN                                                          
         LTR   R5,R5                                                            
         BZ    *+8                                                              
         LA    R5,1(R5)                                                         
         AR    R4,R5                                                            
         CLI   0(R4),0                                                          
         BE    CKPRTX                                                           
         BR    RE                                                               
         EJECT                                                                  
GETAGY   BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,2                                                         
         BNE   BADKEY                                                           
         MVC   DUB(2),0(R4)                                                     
         CLI   DUB,C'A'                                                         
         BL    BADKEY                                                           
         CLI   DUB,C'Z'                                                         
         BH    BADKEY                                                           
GETAGYX  BR    R9                                                               
         SPACE 2                                                                
GETJOB   DS    0H                                                               
         BAS   RE,CHKEND                                                        
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    BADKEY            IN CASE THEY ENTER ,,                          
         CLI   FLEN+1,6                                                         
         BH    BADKEY                                                           
         MVC   WORK(20),SPACES                                                  
         BCTR  R5,R0                                                            
         EX    R5,JOBMOVE                                                       
         BR    R9                                                               
*                                                                               
JOBMOVE  MVC   WORK(0),0(R4)                                                    
         SPACE 2                                                                
GETPUBL  DS    0H                                                               
         BAS   RE,CHKEND                                                        
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    BADKEY            IN CASE THEY ENTER ,,                          
         CLI   FLEN+1,3                                                         
         BH    BADKEY                                                           
         BCTR  R5,0                                                             
         XC    WORK,WORK                                                        
         EX    R5,GPUBL5                                                        
GPUBLX   BR    R9                                                               
*                                                                               
GPUBL5   MVC   WORK(0),0(R4)                                                    
         EJECT                                                                  
GETREP   DS    0H                                                               
         CLI   SVREC,X'23'          SEE IF PPUBLIST                             
         BE    GETREP5              THEN REP REQUIRED                           
         BAS   RE,CHKEND            ELSE OPTIONAL                               
GETREP5  BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,0                                                         
         BE    BADKEY               IF PPUBLIST MUST ENTER REP                  
         CLI   FLEN+1,4                                                         
         BH    BADKEY                                                           
         MVC   WORK(4),0(R4)                                                    
         TM    FVAL,X'08'          TEST NUMERICS                                
         BNO   GETRX               ALPHA OK                                     
         BCTR  R5,0                                                             
         EX    R5,GETR5                                                         
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
GETRX    BR    R9                                                               
*                                                                               
GETR5    PACK  DUB,0(0,R4)                                                      
         EJECT                                                                  
GETPNUM  ZIC   R5,5(R2)            TOTAL LENGHT                                 
         SH    R5,FLEN                                                          
         BCTR  R5,0                                                             
         L     R6,FADDR                                                         
         AH    R6,FLEN                                                          
         LA    R6,1(R6)                                                         
*NOP*    GOTO1 =V(PUBVAL),DMCB,((R5),0(R6)),(0,SVPUB),RR=RELO                   
         GOTO1 VPUBVAL,DMCB,((R5),0(R6)),(0,SVPUB)                              
         CLI   DMCB,X'FF'                                                       
         BE    BADKEY                                                           
         BR    R9                                                               
GETCOM   BAS   RE,CHKEND                                                        
         SPACE 2                                                                
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    BADKEY            IN CASE THEY ENTER ,,                          
         CLI   FLEN+1,6                                                         
         BH    BADKEY                                                           
         MVC   WORK(20),SPACES                                                  
         BCTR  R5,R0                                                            
         EX    R5,COMMOVE                                                       
         LA    R5,1(R5)       RESTORE LENGTH                                    
         CLI   WORK+5,C' '                                                      
         BNE   CKCOM10                                                          
         MVC   WORK+10(5),WORK                                                  
         LA    R6,WORK                                                          
         SR    R7,R7                                                            
         LA    R7,6                                                             
         SR    R7,R5                                                            
         AR    R6,R7                                                            
         BCTR  R7,R0                                                            
         BCTR  R5,R0                                                            
         EX    R5,MOVECOM                                                       
         EX    R7,MOVESPE                                                       
         B     CKCOM10                                                          
*                                                                               
COMMOVE  MVC   WORK(0),0(R4)                                                    
MOVECOM  MVC   0(0,R6),WORK+10                                                  
MOVESPE  MVC   WORK(0),=C'     '                                                
*                                                                               
CKCOM10  BR    R9                                                               
         SPACE 2                                                                
RDCLT    MVI   ERRCD,CLTNFND                                                    
         B     RDPRT                                                            
*                                                                               
RDREP    MVI   ERRCD,RECNFND                                                    
         B     RDPRT                                                            
*                                                                               
RDPRD    MVI   ERRCD,PRDNFND                                                    
         B     RDPRT                                                            
         SPACE 2                                                                
RDPRT    XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(24),KEYSAVE                                                  
         BER   R9                                                               
         B     KEYERR                                                           
*                                                                               
CKPRTX   B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
* VALIDATE FIELD FOR ALPHA/NUMERIC.                                             
* FIELDS ARE DELIMITED BY  COMMA, OR DASH.                                      
*  FADDR  HAS PREVIOUS FIELD ADDRESS.                                           
*  FLEN   HAS PREVIOUS FIELD LENGTH.                                            
*  FVAL   HAS VALIDITY BITS (X'04'=VALID ALPHA,X'08'=VALID NUMERIC)             
*                                                                               
* ON EXIT, R4 HAS FIELD ADDRESS, R5 HAS FIELD LENGTH                            
FLDVAL   DS    0H                                                               
         MVI   FVAL,X'0C'          SET ALL VALID                                
         L     R4,FADDR                                                         
         LH    R5,FLEN                                                          
         LTR   R5,R5                                                            
         BZ    *+8                                                              
         LA    R5,1(R5)                                                         
         AR    R4,R5                                                            
         ST    R4,FADDR                                                         
         SR    R0,R0               TEST STILL IN FIELD                          
         IC    R0,0(R2)                                                         
         AR    R0,R2               R0 HAS FLDHDR END+1                          
         CR    R4,R0                                                            
         BL    FLDVAL2                                                          
         LA    R5,0                                                             
         STH   R5,FLEN                                                          
         BR    RE                                                               
FLDVAL2  DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    FLDVALX                                                          
         CLI   0(R4),C','          CHK FOR DELIMITER                            
         BE    FLDVALX                                                          
         CLI   0(R4),C'A'                                                       
         BL    FLDVAL4                                                          
         CLI   0(R4),C'Z'                                                       
         BNH   FLDVAL6                                                          
FLDVAL4  NI    FVAL,X'FB'          FIELD NOT ALPHA                              
         CLI   0(R4),C'0'                                                       
         BL    FLDVAL6                                                          
         CLI   0(R4),C'9'                                                       
         BNH   FLDVAL8                                                          
FLDVAL6  NI    FVAL,X'F7'          FIELD NOT NUMERIC                            
*                                                                               
FLDVAL8  LA    R4,1(R4)                                                         
         CR    R4,R0               TEST STILL IN FIELD                          
         BNL   FLDVALX                                                          
         B     FLDVAL2                                                          
*                                                                               
FLDVALX  LR    R5,R4                                                            
         S     R5,FADDR            END-START GIVES LENGTH                       
         STH   R5,FLEN                                                          
         L     R4,FADDR                                                         
*                                                                               
         BR    RE                                                               
         EJECT                                                                  
BADKEY   DS    0H                                                               
         MVC   SINMSG(35),=C'** ERROR * KEY FIELD 9 NOT VALID **'               
         OI    ERRCD,X'F0'                                                      
         MVC   SINMSG+21(1),ERRCD                                               
*                                                                               
         OI    6(R2),X'C0'         POSITION CURSOR                              
         B     EXXMOD                                                           
*                                                                               
KEYERR   ZIC   R3,ERRCD                                                         
         B     ERROR                                                            
*                                                                               
ZEROS    DC    20C'0'                                                           
SPACES   DC    80C' '                                                           
         SPACE 2                                                                
CLTNFND  EQU   40                                                               
PRDNFND  EQU   41                                                               
DIVNFND  EQU   45                                                               
REGNFND  EQU   46                                                               
RECNFND  EQU   53                  RECORD NOT FOUND                             
ACCERR   EQU   207                 ACCESS TO THIS CLT NOT AUTHORIZED            
         EJECT                                                                  
** NOTE THAT R8 SHOULD BE STILL POINTING TO AREC                                
         USING   PCLTREC,R8                                                     
       ++INCLUDE PPACCTEST                                                      
         EJECT                                                                  
         DROP    R8                                                             
*                                                                               
       ++INCLUDE PPGENEROL                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         CNOP  2,4                                                              
KEYTAB   DC    H'4'                                                             
         DC    A(KEYTABX-1)                                                     
*                                                                               
         DC    X'11',AL3(CLT)      CLT                                          
         DC    X'12',AL3(CLT)      PRD                                          
         DC    X'13',AL3(CLT)      EST                                          
         DC    X'14',AL3(CLT)      BILL                                         
         DC    X'15',AL3(CLT)      JOB                                          
         DC    X'16',AL3(PUBL)     PUBLIST                                      
*                                                                               
*                                                                               
         DC    X'1A',AL3(CLT)      DIV                                          
         DC    X'1B',AL3(CLT)      REG                                          
         DC    X'1C',AL3(CLT)      DST                                          
*                                                                               
         DC    X'20',AL3(REP)      REP                                          
         DC    X'21',AL3(COM)      COMMENT                                      
         DC    X'22',AL3(REP)      REP-PUBLISHER                                
         DC    X'23',AL3(REP)      REP-PUBLISHER PUBLIST                        
*                                                                               
         DC    X'30',AL3(AGY)      AGENCY                                       
*                                                                               
KEYTABX  EQU   *                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKTRAFID NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC ID SIGN-ON              
*                                                                               
         LA    RE,TMPREC                                                        
         LA    RF,400*4                                                         
         XCEF                                                                   
*                                                                               
         LA    R4,TMPREC                                                        
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,10(RA)      ID NUMBER                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),(R4)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
CKTRA10  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
         CLI   0(RE),CTAGYELQ      AGENCY ALPHA ID ELEMENT (X'06')              
         BE    CKTRA20                                                          
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CKTRA10                                                          
*                                                                               
CKTRA20  DS    0H                                                               
         USING CTAGYD,RE                                                        
         CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   CKTRIDER                                                         
         DROP  R4,RE                                                            
*                                                                               
CKTRIDX  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKTRIDER LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRAFFACC NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC LIMIT ACCESS            
*                                                                               
         MVI   BYTE3,0             FOR RETURN CODE                              
*                                                                               
         L     R6,AREC             POINT TO CLIENT RECORD                       
         LA    R6,33(R6)                                                        
         MVI   ELCODE,X'50'        CLIENT TRAFFIC OFFICE ELEM CODE              
         BAS   RE,TRNXTEL                                                       
         BNE   TRACCX              NO CLIENT TRAFFIC OFFICE CODE ELEM           
         MVC   BYTE3,2(R6)         SAVE CLIENT TRAFFIC OFFICE CODE              
*                                                                               
TRACCX   XIT1                                                                   
*                                                                               
*                                                                               
TRNXTEL  CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                GOT TO HAVE AT LEAST ONE ELEM!               
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
TRNXTEL5 SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                FOUND ELEM, SET CC TO EQUAL                  
         CLI   0(R6),0                                                          
         BNE   TRNXTEL5                                                         
         LTR   R6,R6               NOT FOUND, SET CC TO NOT EQUAL               
         BR    RE                                                               
*                                                                               
*                                                                               
ELCODE   DS    X                                                                
*                                                                               
TMPREC   DS    400F                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PAGYREC                                                        
       ++INCLUDE PCLTREC                                                        
       ++INCLUDE PPRDREC                                                        
       ++INCLUDE PESTREC                                                        
       ++INCLUDE PDIVREC                                                        
       ++INCLUDE PREGREC                                                        
       ++INCLUDE PREPREC                                                        
       ++INCLUDE PGENGRP                                                        
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE PPSINFOWRK                                                     
