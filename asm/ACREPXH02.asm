*          DATA SET ACREPXH02  AT LEVEL 039 AS OF 03/04/19                      
*PHASE ACXH02A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'ADD/CHANGE CONTRA HEADER AND BUCKET RECORDS'                    
         PRINT NOGEN                                                            
ACXH02   CSECT                                                                  
         NMOD1 0,**ACXH**,RR=R7                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXHD,RC                                                         
                                                                                
***********************************************************************         
* QOPT 1 = A ADD MISSING CONTRA-HEADERS                               *         
*        = C UPDATE CONTRA-HEADER NAMES                               *         
*        = B BOTH THE ABOVE                                           *         
* QOPT 2 = Y BUCKET RECORD WITH DUMMY BUCKET ELEMENT IF BUCKET        *         
*            RECORD DOES NOT EXIST.                                   *         
* QOPT 3 = Y ADD OFFICE ACCOUNT RECORDS. (THEN RUN ACOB TO UPDATE     *         
*            OFFICE/BALANCES                                          *         
* QOPT 4 = Y DUMP SOME ADDREC AND PUTRECS                             *         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
                                                                                
RQF00    CLI   MODE,REQFRST                                                     
         BNE   PAC00                                                            
*                                                                               
         GOTO1 ASETDATE,DMCB,(RC)    SET START AND END DATES                    
*                                                                               
         MVI   OPTS,0                                                           
         CLI   QOPT1,C'B'                                                       
         BNE   *+8                                                              
         OI    OPTS,OPTCCHD+OPTACHD    FIX NAMES & ADD NEW CONTRAS              
         CLI   QOPT1,C'C'               ONLY FIX NAMES                          
         BNE   *+8                                                              
         OI    OPTS,OPTCCHD                                                     
         CLI   QOPT1,C'A'               ONLY ADD NEW CONTRA                     
         BNE   *+8                                                              
         OI    OPTS,OPTACHD                                                     
         CLI   QOPT2,C'Y'               ADD BUCKET RECORDS                      
         BNE   *+8                                                              
         OI    OPTS,OPTACAC                                                     
         CLI   QOPT3,C'Y'               ADD OFFICE ACCOUNT RECORDS              
         BNE   *+8                                                              
         OI    OPTS,OPTAOFA                                                     
         CLI   QOPT4,C'Y'               DUMP RECORDS                            
         BNE   *+8                                                              
         OI    OPTS,OPTDUMP                                                     
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCRESET,C'Y'                                                     
         MVC   PAGE,=H'1'                                                       
*                                                                               
         L     R3,ADCMPEL                                                       
         USING CPYELD,R3                                                        
         MVC   STAT4,CPYSTAT4                                                   
         DROP  R3                                                               
                                                                                
XIT      XIT1  1                                                                
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT - FIND TRANSACTIONS                                 *         
***********************************************************************         
                                                                                
PAC00    CLI   MODE,PROCACC                                                     
         BNE   RNL00                                                            
         XC    OFFICE,OFFICE                                                    
*                                                                               
         L     R4,ADACC                                                         
         LR    R5,R4                                                            
         AHI   R5,49                ADD DATA DISP                               
*                                                                               
PAC01    CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),RSTELQ          IS IS X'30' ELEMENT                        
         BE    PAC02                                                            
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     PAC01                                                            
*                                                                               
         USING RSTELD,R5                                                        
PAC02    DS    0H                                                               
         CLC   RSTTDATE,START        IS LAST TRN DATE LESS THAN START ?         
         BL    XIT                   YES, SKIP THIS ACCOUNT                     
         CLC   RSTTDATE,END          IS LAST TRN DATE > THAN START ?            
         BH    XIT                   YES, SKIP THIS ACCOUNT                     
         DROP  R5                                                               
*                                                                               
         L     RF,ADACC                                                         
         MVC   AKEY,0(RF)                                                       
         LA    R2,AKEY                                                          
         USING ACTRECD,R2                                                       
         CLC   ACTKUNT(2),=C'1J'                                                
         BE    XIT                                                              
*                                                                               
PAC03    GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,AKEY,ADIR                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PAC05    GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,AKEY,ADIR                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PAC07    CLC   ADIR(L'ACTKCULA),AKEY                                            
         BNE   XIT                 END OF ACCOUNT                               
         LA    R2,ADIR                                                          
         USING TRNRECD,R2          FIND A TRANSACTION                           
         CLC   TRNKCULC,SPACES                                                  
         BE    PAC05                                                            
         MVC   AKEY,ADIR                                                        
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,ADA,AIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO                                                           
         CLI   TRNRFST,BUKELQ                                                   
         BE    PAC08                                                            
         TM    TRNRSTAT,TRNSTIME   TEST TIME RECORD                             
         BO    PAC08                                                            
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ                                                     
*        BNE   PAC05                                                            
*                                                                               
PAC08    MVI   DMPTRN,C'N'         SET TRANSACTION NOT DUMPED                   
         CLC   TRNKUNT(2),=C'SJ'                                                
         BE    PAC09                                                            
         TM    STAT4,CPYSOFF2      TEST NEW OFFICE                              
         BNO   PAC09                                                            
         TM    OPTS,OPTAOFA                                                     
         BNO   PAC09                                                            
         CLC   TRNKOFF,OFFICE      TEST SAME OFFICE                             
         BE    PAC09                                                            
         BAS   RE,CHKOFA           CHECK OFFICE ACCOUNT                         
*                                                                               
PAC09    BAS   RE,GETNAM           GET NAME OF CONTRA                           
         BAS   RE,CHKCHD           CHECK CONTRA HEADER                          
         BAS   RE,CHKCAC           CHECK CONTRA BUCKET                          
*                                                                               
         LA    R2,AKEY                                                          
         XC    TRNKDATE(10),TRNKDATE CLEAR DATE/REF/SUBREF                      
         MVI   TRNKDATE,X'FF'      SKIP TO NEXT CONTRA                          
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,AKEY,ADIR                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PAC07                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK OFFICE ACCOUNT ACCOUNT RECORD                                 *         
***********************************************************************         
                                                                                
CHKOFA   NTR1  ,                                                                
         L     R2,AIO                                                           
         USING TRNRECD,R2                                                       
         MVC   OFFICE,TRNKOFF                                                   
         LA    R3,BKEY             BUILD KEY FOR OFFICE ACCOUNT                 
         USING OFARECD,R3                                                       
         MVC   BKEY,SPACES                                                      
         MVC   OFAKCULA,TRNKCULA   READ FOR ACCOUNT OFFICE                      
         MVC   OFAKOFF,TRNKOFF                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,BKEY,BDIR                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BKEY,BDIR           TEST RECORD FOUND                            
         BE    XIT                                                              
         BAS   RE,NEWOFA           ADD NEW OFFICE/ACCOUNT                       
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD NEW OFFICE/ACCOUNT RECORD                                     *         
***********************************************************************         
                                                                                
NEWOFA   NTR1  ,                                                                
         L     R0,BIO                                                           
         LA    R1,L'IOB                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR BIO                                    
*                                                                               
         L     R3,BIO                                                           
         USING OFARECD,R3                                                       
         MVC   OFAKEY,BKEY         KEY OF CONTRA                                
         MVC   OFARLEN,=Y(OFARFST-OFAKEY)                                       
         MVC   OFARLMOS,=X'FFFF'                                                
         LA    R5,ELEMENT                                                       
         USING ABLELD,R5                                                        
         XC    ELEMENT,ELEMENT     BUILD 45 ELEMENT                             
         MVI   ABLEL,ABLELQ                                                     
         MVI   ABLLN,ABLLN3Q                                                    
         ZAP   ABLFRWD,=P'0'                                                    
         ZAP   ABLDR,=P'0'                                                      
         ZAP   ABLCR,=P'0'                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),OFARECD,ABLEL,0                         
*                                                                               
         MVC   RECORD,=C'OFAREC'                                                
         MVC   ACTION,ADDREC                                                    
*                                                                               
         BAS   RE,PRNT             PRINT RECORD                                 
         BAS   RE,DMPR             DUMP RECORD                                  
*                                                                               
         AP    ADDRCD,=P'1'                                                     
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,BDA,BIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* GET CONTRA NAME                                                     *         
***********************************************************************         
                                                                                
GETNAM   NTR1  ,                                                                
         MVC   CONNAM,SPACES       CLEAR NAME                                   
         XC    CONNAML,CONNAML     LENGTH                                       
         MVC   OLDNAM,SPACES       AND OLD NAME                                 
         L     R2,AIO                                                           
         USING TRNRECD,R2                                                       
         CLC   TRNKCCPY,TRNKCPY    TEST REAL CONTRA                             
         BNE   XIT                                                              
         LA    R3,CKEY             BUILD KEY FOR ACCOUNT RECORD                 
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,TRNKCULC                                                
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,CKEY,CDIR                             
         CLI   8(R1),0                                                          
         BNE   XIT                                                              
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,CDA,CIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,CIO                                                           
         LA    R5,ACTRFST                                                       
         USING NAMELD,R5                                                        
         SR    R0,R0                                                            
*                                                                               
GETNAM5  CLI   NAMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NAMEL,NAMELQ                                                     
         BE    GETNAM7                                                          
         IC    R0,NAMLN                                                         
         AR    R5,R0                                                            
         B     GETNAM5                                                          
*                                                                               
GETNAM7  SR    R1,R1               EXTRACT THE NAME                             
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CONNAM(0),NAMEREC                                                
         LA    R1,1(R1)           SAVE LENGTH                                   
         STC   R1,CONNAML                                                       
         B     XIT                                                              
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
***********************************************************************         
* CHECK CONTRA HEADER RECORD                                          *         
***********************************************************************         
                                                                                
CHKCHD   NTR1  ,                                                                
         LA    R2,ADIR                                                          
         USING TRNRECD,R2                                                       
         LA    R3,BKEY             BUILD KEY OF CONTRA HEADER                   
         USING CHDRECD,R3                                                       
         XC    BKEY,BKEY                                                        
         MVC   CHDKCULA,TRNKCULA   READ FOR CONTRA HEADER                       
         MVC   CHDKOFF,SPACES                                                   
         MVC   CHDKCULC,TRNKCULC                                                
         CLC   CHDKULC,SPACES      DO WE HAVE A CONTRA ACCOUNT?                 
         BNH   XIT                                                              
         MVC   CHDKSPCS,SPACES                                                  
         MVC   CHDKBTYP,SPACES                                                  
         CLC   TRNKUNT(2),=C'SJ'                                                
         BE    CHKCHD3                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,BKEY,BDIR                             
         BAS   RE,CHDALL                                                        
         TM    STAT4,CPYSOFF2      TEST NEW OFFICE                              
         BNO   XIT                                                              
CHKCHD3  MVC   CHDKOFF,TRNKOFF     BUILD OFFICE CONTRA                          
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,BKEY,BDIR                             
         BAS   RE,CHDALL                                                        
         B     XIT                                                              
*                                                                               
CHDALL   LR    R0,RE                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BKEY(CHDKBTYP-CHDRECD),BDIR     TEST RECORD FOUND                
         BNE   CHDALL3                                                          
       OC  BDIR+(CHDKNULL-CHDRECD)(L'CHDKNULL),BDIR+(CHDKNULL-CHDRECD)          
         BNZ   CHDALL3                                                          
         BAS   RE,CHKNAM           CHECK CONTRA NAME                            
         B     CHDALLX                                                          
*                                                                               
CHDALL3  BAS   RE,NEWCHD           ADD NEW CONTRA HEADER                        
CHDALLX  LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD NEW CONTRA HEADER RECORD                                      *         
***********************************************************************         
                                                                                
NEWCHD   NTR1  ,                                                                
         TM    OPTS,OPTACHD        TEST ADD NEW CONTRA HEADER                   
         BNO   XIT                                                              
         L     R0,BIO                                                           
         LA    R1,L'IOB                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR BIO                                    
*                                                                               
         L     R3,BIO                                                           
         USING CHDRECD,R3                                                       
         MVC   CHDKEY,BKEY         KEY OF CONTRA                                
         MVC   CHDRLEN,=Y(CHDRFST-CHDKEY)                                       
         BAS   RE,NEWCACL          BUILD NEW CACEL ELEMENT                      
*                                                                               
         MVC   RECORD,=C'CHDREC'                                                
         MVC   ACTION,ADDREC                                                    
*                                                                               
         BAS   RE,PRNT             PRINT RECORD                                 
         BAS   RE,DMPR             DUMP RECORD                                  
*                                                                               
         AP    ADDRCD,=P'1'                                                     
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,BDA,BIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,BKEY,BDIR   GET DA                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD NEW CONTRA NAME ELEMENT                                       *         
***********************************************************************         
                                                                                
         USING CHDRECD,R3                                                       
NEWCACL  NTR1  ,                                                                
         LA    R5,ELEMENT                                                       
         USING CACELD,R5                                                        
         XC    ELEMENT,ELEMENT     BUILD 43 ELEMENT                             
         MVI   CACEL,CACELQ                                                     
         MVC   CACCNT,CHDKCULC     CODE                                         
         MVC   CACNAME,CONNAM      NAME                                         
         LA    R1,CACLN1Q                                                       
         SR    R0,R0                                                            
         IC    R0,CONNAML          LENGTH OF NAME                               
         AR    R1,R0                                                            
         STC   R1,CACLN                                                         
         GOTO1 HELLO,DMCB,(C'P',ACCMST),CHDRECD,CACEL,0                         
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK CONTRA BUCKET RECORD                                          *         
***********************************************************************         
                                                                                
CHKCAC   NTR1  ,                                                                
         LA    R2,ADIR                                                          
         USING TRNRECD,R2                                                       
         LA    R3,BKEY             BUILD KEY OF CONTRA HEADER                   
         USING CACRECD,R3                                                       
         MVC   BKEY,SPACES                                                      
         MVC   CACKCULA,TRNKCULA   READ FOR CONTRA HEADER                       
         MVC   CACKCULC,TRNKCULC                                                
         CLC   CACKULC,SPACES                                                   
         BNH   XIT                                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,BKEY,BDIR                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BKEY,BDIR           TEST RECORD FOUND                            
         BE    XIT                                                              
         BAS   RE,NEWCAC           ADD NEW CONTRA BUCKET                        
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD NEW CONTRA BUCKET RECORD                                      *         
***********************************************************************         
                                                                                
NEWCAC   NTR1  ,                                                                
         TM    OPTS,OPTACAC                                                     
         BNO   XIT                                                              
         L     R0,BIO                                                           
         LA    R1,L'IOB                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR BIO                                    
*                                                                               
         L     R3,BIO                                                           
         USING CACRECD,R3                                                       
         MVC   CACKEY,BKEY         KEY OF CONTRA                                
         MVC   CACRLEN,=Y(CACRFST-CACKEY)                                       
         LA    R5,ELEMENT                                                       
         USING BUKELD,R5                                                        
         XC    ELEMENT,ELEMENT     BUILD 45 ELEMENT                             
         MVI   BUKEL,BUKELQ                                                     
         MVI   BUKLN,BUKLNQ                                                     
         MVC   BUKMOS,=X'9701'     DUMMY MONTH                                  
         ZAP   BUKDR,=P'0'                                                      
         ZAP   BUKCR,=P'0'                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),CACRECD,BUKEL,0                         
*                                                                               
         MVC   RECORD,=C'CACREC'                                                
         MVC   ACTION,ADDREC                                                    
*                                                                               
         BAS   RE,PRNT             PRINT RECORD                                 
         BAS   RE,DMPR             DUMP RECORD                                  
*                                                                               
         AP    ADDRCD,=P'1'                                                     
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,BDA,BIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK CONTRA NAME                                                   *         
***********************************************************************         
                                                                                
CHKNAM   NTR1  ,                                                                
         TM    OPTS,OPTCCHD        TEST NAME CHANGE OPTION                      
         BNO   XIT                                                              
         CLC   CONNAM,SPACES       TEST NEW NAME                                
         BE    XIT                 CAN'T FIX IT                                 
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,BDA,BIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,BIO                                                           
         USING CHDRECD,R3                                                       
         LA    R5,CHDRFST          FIND OLD CONTRA NAME                         
         USING CACELD,R5                                                        
         CLI   CACEL,CACELQ        TEST CONTRA NAME ELEMENT                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1               SAVE OLD NAME                                
         IC    R1,CACLN                                                         
         SH    R1,=Y(CACLN1Q+1)                                                 
         BM    CHKNAM5                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OLDNAM(0),CACNAME                                                
         CLC   OLDNAM,CONNAM       TEST NAME CHANGE                             
         BE    XIT                                                              
*                                                                               
CHKNAM5  MVI   CACEL,X'FF'                                                      
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',CHDRECD),0                       
         BAS   RE,NEWCACL          ADD NEW CONTRA NAME ELEMENT                  
         MVC   RECORD,=C'CHDREC'                                                
         MVC   ACTION,=C'PUTREC'                                                
*                                                                               
         BAS   RE,PRNT             PRINT RECORD                                 
         BAS   RE,DMPR             DUMP RECORD                                  
*                                                                               
         AP    CHARCD,=P'1'                                                     
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,BDA,BIO,DMWORK                        
         B     XIT                                                              
         DROP  R3,R5                                                            
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
* RUN LAST                                                            *         
***********************************************************************         
                                                                                
RNL00    CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
*                                                                               
         GOTO1 ACREPORT                                                         
         LA    R5,RECCNT                                                        
*                                                                               
RNL3     MVC   P+1(16),4(R5)                                                    
         EDIT  (P4,0(R5)),(9,P+20),COMMAS=YES                                   
         GOTO1 ACREPORT                                                         
         LA    R5,L'RECCNT(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   RNL3                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DUMP ROUTINES                                                       *         
***********************************************************************         
                                                                                
DMPR     NTR1  ,                                                                
         TM    OPTS,OPTDUMP        OPTION TO DUMP SOME RECORDS                  
         BNO   XIT                                                              
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
ASETDATE DC    A(SETDATE)                                                       
*                                                                               
DMPCNT   DC    PL2'0'                                                           
DMPMAX   DC    PL2'200'                                                         
*                                                                               
TRNSACT  DC    C'TRANSACTION DA=00000000'                                       
*                                                                               
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
*                                                                               
DTFADD   DC    C'DTFADD '                                                       
*                                                                               
RECCNT   DS    0CL20                                                            
CHARCD   DC    PL4'0',CL16'RECORDS CHANGED'                                     
ADDRCD   DC    PL4'0',CL16'RECORDS ADDED'                                       
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET START AND END DATES                                             *         
***********************************************************************         
         SPACE 1                                                                
SETDATE  DS    0D                                                               
         NMOD1 0,*SETDAT*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         MVC   PRTSTART,SPACES                                                  
         MVC   PRTEND,SPACES                                                    
         XC    START,START                                                      
         CLC   QSTART,SPACES       ANY START DATE                               
         BE    SETDT10                                                          
         MVC   WORK(L'QSTART),QSTART                                            
         CLC   WORK+4(2),SPACES                                                 
         BNE   *+10                                                             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,START)                                   
         GOTO1 DATCON,DMCB,(0,WORK),(5,PRTSTART)                                
*                                                                               
SETDT10  MVC   END,=X'FFFFFF'                                                   
         CLC   QEND,SPACES         ANY END DATE                                 
         BE    SETDTX                                                           
         MVC   WORK(L'QEND),QEND                                                
         CLC   WORK+4(2),SPACES                                                 
         BNE   SETDT20                                                          
         MVC   WORK+4(2),=C'31'                                                 
         GOTO1 DATCON,DMCB,(X'30',WORK),(0,WORK),(1,0) LAST DAY OF MO           
SETDT20  GOTO1 DATCON,DMCB,(0,WORK),(1,END)                                     
         GOTO1 DATCON,DMCB,(0,WORK),(5,PRTEND) FOR PRINTING ON REPORT           
SETDTX   XIT1                                                                   
**********************************************************************          
* LITERALS SETDATE NMOD1                                             *          
**********************************************************************          
         SPACE 1                                                                
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
                                                                                
ACXHD    DSECT                                                                  
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
AMSTDCB  DS    A                                                                
ADIRDCB  DS    A                                                                
*                                                                               
ELEMENT  DS    XL255                                                            
*                                                                               
OPTS     DS    XL1                 OPTION CONTROL                               
OPTCCHD  EQU   X'80'               NAMES CHANGES                                
OPTACHD  EQU   X'40'               ADD NEW CONTRA HEADERS                       
OPTACAC  EQU   X'20'               ADD NEW BUCKET RECORD                        
OPTAOFA  EQU   X'10'               ADD OFFICE/ACCOUNT RECORD                    
OPTDUMP  EQU   X'08'               DUMP CHANGED/NEW RECORDS                     
*                                                                               
START    DS    XL3                                                              
END      DS    XL3                                                              
PRTSTART DS    CL8                                                              
PRTEND   DS    CL8                                                              
*                                                                               
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
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DMDTFPH                                                        
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
**PAN#1  DC    CL21'039ACREPXH02 03/04/19'                                      
         END                                                                    
