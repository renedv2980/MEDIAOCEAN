*          DATA SET ACREPD802  AT LEVEL 023 AS OF 04/08/15                      
*PHASE ACD802A,*                                                                
*INCLUDE SORTER                                                                 
         TITLE 'ACD802 - ADJUSTMENT RATE REPORT'                                
ACD802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACD8**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACD8D,RC                                                         
         CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         L     RF,MCBXAREA                                                      
         ST    RF,ADBOX                                                         
         L     RF,=A(HOOK)                                                      
         ST    RF,HEADHOOK                                                      
         L     RF,=A(SAVERC)                                                    
         ST    RC,0(RF)                                                         
         MVI   PAGEWDTH,132                                                     
         CLC   RCOUNTRY,=C'UK'                                                  
         BE    PAGE110                                                          
         CLI   PROGPROF+2,C'Y'     WANT 110 COLUMNS                             
         BNE   PAGE132                                                          
*                                                                               
PAGE110  MVI   PAGEWDTH,110                                                     
*                                                                               
PAGE132  MVC   PAGE,=H'1'                                                       
         MVC   AJRCMP,QCOMPANY     PUT COMPANY IN KEY                           
         BAS   RE,GETCOMP           AND GET NAME                                
         BAS   RE,GETHIER                                                       
         XC    AJRKEY+2(L'AJRKEY-2),AJRKEY+2                                    
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         CLC   QEND,SPACES                                                      
         BE    SETEND                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,PEND)                                    
         B     SETSTRT                                                          
*                                                                               
SETEND   MVC   PEND,=X'FFFFFF'                                                  
*                                                                               
SETSTRT  XC    PSTART,PSTART                                                    
         CLC   QSTART,SPACES                                                    
         BE    GETREC                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,PSTART)                                
         B     GETREC                                                           
         EJECT                                                                  
***********************************************************************         
* READ ADJUSTMENT RATE RECORDS AND SEND TO SORT. WHEN KEY CHANGES     *         
* RETREIVE RECORDS, FORMAT AND PRINT DATA.                            *         
***********************************************************************         
         SPACE 1                                                                
         USING PAJRECD,R2                                                       
GETREC   GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',AJRKEY,BUFFER                
         B     GETDMCB             CHECK RETURN                                 
*                                                                               
GETMORE  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCOUNT',BUFFER,BUFFER                
*                                                                               
GETDMCB  CLI   DMCB+8,0            EXIT IF ERROR FOUND                          
         BNE   XIT                                                              
         LA    R2,BUFFER                                                        
         CLC   AJRKEY(2),0(R2)     SORT WHEN KEY CHANGES                        
         BNE   GETSORT                                                          
*                                                                               
GETDEPT  CLC   QOFFICE,SPACES      ARE WE TAKING ALL OFFICES ?                  
         BE    GETCLI              YES, SEE WHAT CLIENTS                        
         LA    R1,0                SET UP LENGTH FOR 1 DIGIT OFFICE             
         CLC   QOFFICE+1(1),SPACES                                              
         BE    *+8                                                              
         LA    R1,1                CHANGE LENGTH ACCORDINGLY                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   QOFFICE(0),PAJKOFF  ARE WE TAKING THIS ONE ?                     
         BNE   GETMORE              NO, GET NEXT RECORD                         
*                                                                               
GETCLI   CLC   QACCOUNT,SPACES     ANY ACCOUNT ?                                
         BE    GETOPT1             NO                                           
         LA    R4,QACCOUNT         YES, CHECK CLIENT                            
         CLC   0(L'PAJKCLI,R4),PAJKCLI                                          
         BNE   GETMORE                                                          
*                                                                               
         LA    R4,L'PAJKCLI(R4)    CHECK FOR PRODUCT                            
         CLC   0(L'PAJKPRO,R4),SPACES                                           
         BE    GETOPT1                                                          
         CLC   0(L'PAJKPRO,R4),PAJKPRO                                          
         BNE   GETMORE                                                          
         LA    R4,L'PAJKPRO(R4)    CHECK FOR JOB                                
         CLC   0(L'PAJKJOB,R4),SPACES                                           
         BE    GETOPT1                                                          
         CLC   0(L'PAJKJOB,R4),PAJKJOB                                          
         BNE   GETMORE                                                          
*                                                                               
GETOPT1  CLI   QOPT1,C'S'          SKIP TERMINATED EMPLOYEES ?                  
         BNE   GETFORM             NO, SHOW ALL                                 
         CLI   PAJKSTF,X'41'       ARE WE AT STAFF LEVEL ?                      
         BL    GETFORM             NO, SKIP OVER                                
         MVC   SAVEKEY,0(R2)       SAVE FOR REREAD                              
         BAS   RE,TERMTEST                                                      
         CLI   TERMED,C'Y'         TERMINATED ?                                 
         BE    GETMORE             YES                                          
*                                                                               
GETFORM  LA    R4,SORTREC                                                       
         BAS   RE,FORMSORT         FORMAT RECORD FOR SORT                       
         BAS   RE,XSORTEL          SORT ELEMENTS FIRST                          
         BNZ   GETMORE             NOTHING TO SORT, GET NEXT REC                
         BAS   RE,GETELMS          ADD ELEMENTS TO IT                           
         BNZ   GETMORE             NOTHING TO SORT, GET NEXT REC                
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         AP    COUNTIN,=P'1'                                                    
         B     GETMORE                                                          
         SPACE 3                                                                
GETSORT  GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R6,15,DMCB+4        NO ADDRESS INDICATES NO MORE                 
         BZ    DONE                   RECORDS                                   
         LA    R4,SORTREC          MOVE RECORD FROM SORT                        
         LH    R5,0(R6)             TO SORTREC SO WE CAN SEE IT                 
         LR    R7,R5                                                            
         MVCL  R4,R6                                                            
         LA    R4,SORTREC          READDDRESS THE RECORD                        
         AP    COUNTOUT,=P'1'                                                   
         BAS   RE,FORMPRT          FORMAT PRINT BASED ON TYPE                   
         BAS   RE,PRNTELMS         GET RATES AND DATES AND PRINT                
         B     GETSORT             GET MORE                                     
         SPACE 3                                                                
DONE     GOTO1 SORTER,DMCB,=C'END'                                              
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*       FORMAT SORT RECORD WITH DATA FROM CHARGE RATE RECORD KEY      *         
***********************************************************************         
         SPACE 1                                                                
         USING PAJRECD,R2                                                       
         USING SORTD,R4                                                         
FORMSORT XC    SORTREC(SORTDAT-SORTD),SORTREC                                   
         MVC   SORTOFF(L'PAJKOFF),PAJKOFF                                       
         MVC   SORTCLI(L'PAJKCLI),PAJKCLI                                       
         MVC   SORTPRO(L'PAJKPRO),PAJKPRO                                       
         MVC   SORTJOB(L'PAJKJOB),PAJKJOB                                       
         MVC   SORTDOF(L'PAJKDOF),PAJKDOF                                       
         MVC   SORTDEP(L'PAJKDEP),PAJKDEP                                       
         MVC   SORTSUB(L'PAJKSUB),PAJKSUB                                       
         MVC   SORTSTF(L'PAJKSTF),PAJKSTF                                       
         MVC   SORTTSK(L'PAJKTSK),PAJKTSK                                       
         MVI   SORTPRE,X'00'                                                    
         BR    RE                                                               
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*                       GET ELEMENTS AND SORT THEM                    *         
***********************************************************************         
         SPACE 1                                                                
XSORTEL  ST    RE,SAVERE                                                        
         SR    R6,R6                                                            
         MVI   ELCODE,X'53'         RATE ELEMENT                                
         BAS   RE,GETELIO                                                       
         BNE   NOTZERO                                                          
         ST    R2,XSORTADD         SAVE ADDRESS OF FIRST ELEMENT                
         B     XBUMP               ADD TO COUNTER                               
*                                                                               
XGETNXT  BAS   RE,NEXTEL                                                        
         BNE   XSORTIT                                                          
*                                                                               
XBUMP    LA    R6,1(R6)                                                         
         B     XGETNXT                                                          
*                                                                               
XSORTIT  L     R2,XSORTADD                                                      
         GOTO1 XSORT,DMCB,(R2),(R6),19,4,12                                     
         B     ISZERO                                                           
         EJECT                                                                  
***********************************************************************         
*               GET RATES AND DATES AND ADD TO SORT RECORD            *         
***********************************************************************         
         SPACE 1                                                                
         USING TCIELD,R2                                                        
         USING SORTD,R4                                                         
GETELMS  ST    RE,SAVERE                                                        
         SR    R6,R6                                                            
         MVI   TCIEL,TCIELQ         RATE ELEMENT                                
         BAS   RE,GETELIO                                                       
         B     *+8                                                              
*                                                                               
GETNEXT  BAS   RE,NEXTEL                                                        
         BNE   NOMORE                                                           
         CLC   TCIDTE,PEND         SEE IF DATE WITHIN RANGE                     
         BH    GETNEXT              SKIP IF HIGHER THAN END                     
         CLC   TCIDTE,PSTART                                                    
         BL    GETNEXT              OR LOWER THAN START                         
         ZAP   SORTRAT,TCIAMT                                                   
         MVC   SORTDAT,TCIDTE                                                   
         LA    R4,SORTLNG(R4)                                                   
         LA    R6,1(R6)                                                         
         B     GETNEXT                                                          
*                                                                               
NOMORE   LA    R2,SORTREC           DID WE HAVE AN ELEMENTS ?                   
         CR    R2,R4                                                            
         BE    NOTZERO               NO, SKIP THIS RECORD                       
         MVI   SORTRAT,X'FF'                                                    
         MH    R6,=Y(SORTLNG)                                                   
         AH    R6,=Y(SORTRAT+1-SORTD)                                           
         LA    R4,SORTREC                                                       
         STCM  R6,3,SORTLEN                                                     
         B     ISZERO                                                           
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*              DETERMINE IF EMPLOYEE IS TERMINATED                    *         
***********************************************************************         
         SPACE 1                                                                
         USING PAJRECD,R2                                                       
         USING ACHEIRD,R5                                                       
TERMTEST NTR1                                                                   
         LA    R3,MYKEY                                                         
         LA    R5,SAVELED                                                       
         MVI   TERMED,C'N'                                                      
         MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(1),QCOMPANY                                                
         MVC   MYKEY+1(2),=C'1R'                                                
         LA    R3,3(R3)            GET STARTING LOCATION                        
*                                                                               
         LA    R4,PAJKDOF          MOVE OFFICE TO KEY                           
         SR    RF,RF                                                            
         IC    RF,ACHRLEVA                                                      
         BCTR  RF,0                                                             
         EX    RF,MVCDATA                                                       
         LA    R3,1(RF,R3)         BUMP TO NEXT SPOT                            
*                                                                               
         LA    R4,PAJKDEP          MOVE DEPARTMENT TO KEY                       
         IC    RF,ACHRLEVB                                                      
         SR    R1,R1                                                            
         IC    R1,ACHRLEVA                                                      
         SR    RF,R1                                                            
         BZ    TERMT2                                                           
         BCTR  RF,0                                                             
         EX    RF,MVCDATA                                                       
         LA    R3,1(RF,R3)                                                      
*                                                                               
         LA    R4,PAJKSUB          MOVE SUB-DEPARTMENT TO KEY                   
         IC    RF,ACHRLEVC                                                      
         SR    R1,R1                                                            
         IC    R1,ACHRLEVB                                                      
         SR    RF,R1                                                            
         BZ    TERMT2                                                           
         BCTR  RF,0                                                             
         EX    RF,MVCDATA                                                       
         LA    R3,1(RF,R3)                                                      
*                                                                               
         LA    R4,PAJKSTF          MOVE STAFF TO KEY                            
         IC    RF,ACHRLEVD                                                      
         SR    R1,R1                                                            
         IC    R1,ACHRLEVC                                                      
         SR    RF,R1                                                            
         BZ    TERMT2                                                           
         BCTR  RF,0                                                             
         EX    RF,MVCDATA                                                       
         LA    R3,1(RF,R3)                                                      
         DROP  R5                                                               
*                                                                               
TERMT2   GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',MYKEY,BUFFER                 
         CLI   DMCB+8,0            FOUND ?                                      
         BE    *+6                                                              
         DC    H'0'                ERROR IF NOT FOUND                           
         MVI   ELCODE,EMPELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   REREAD                                                           
         USING EMPELD,R2                                                        
         OC    EMPTRM,EMPTRM                                                    
         BZ    REREAD                                                           
         MVI   TERMED,C'Y'                                                      
*                                                                               
REREAD   MVC   BUFFER(L'SAVEKEY),SAVEKEY                                        
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',BUFFER,BUFFER                
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
MVCDATA  MVC   0(0,R3),0(R4)                                                    
         EJECT                                                                  
***********************************************************************         
*              DETERMINE PRINT FORMAT AND ADD UNIQUE DATA             *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
         USING PRINT0,R5                                                        
FORMPRT  NTR1                                                                   
         LA    R5,P                                                             
         BAS   RE,NEWJOB                                                        
         BZ    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,NEWPRO                                                        
         BZ    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,NEWCLI                                                        
         BZ    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,NEWOFF                                                        
         BZ    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLI   FORCEHED,C'Y'                                                    
         BE    MOVEOFF                                                          
         L     R6,ADBOX                                                         
         USING BOXD,R6                                                          
         MVI   BOXSTAT,C'I'                                                     
         MVI   BOXREQ,C'B'                                                      
         BAS   RE,PRINTIT                                                       
MOVEOFF  BAS   RE,NEWDOF                                                        
         MVC   OCODE0,SDOF                                                      
         MVC   ONAME0,SDOFNAME                                                  
         BAS   RE,NEWDEP                                                        
         MVC   DCODE0,SDEP                                                      
         MVC   DNAME0,SDEPNAME                                                  
         BAS   RE,NEWSUB                                                        
         MVC   SCODE0,SSUB                                                      
         MVC   SNAME0,SSUBNAME                                                  
         BAS   RE,NEWSTF                                                        
         MVC   PCODE0,SSTF                                                      
         MVC   PNAME0,SSTFNAME                                                  
         BAS   RE,NEWTASK                                                       
         MVC   TCODE0,STSK                                                      
         MVC   TNAME0,STSKNAME                                                  
         B     XIT                                                              
         DROP R4,R5                                                             
         EJECT                                                                  
***********************************************************************         
*                 MOVE RATES AND DATES TO PRINT LINE                  *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
PRNTELMS ST    RE,SAVERE                                                        
FRSTLINE LA    R5,P-132                                                         
NEXTLINE LA    R5,132(0,R5)                                                     
         LR    R6,R5                                                            
         LA    R3,2                                                             
         USING LINED,R6                                                         
PRNTRAT  EDIT  SORTRAT,(9,LINERTE),4,FLOAT=-                                    
         OC    SORTDAT,SORTDAT                                                  
         BZ    NODATE                                                           
         GOTO1 DATCON,DMCB,(1,SORTDAT),(8,LINEDTE)                              
NODATE   LA    R6,LINELNG(R6)                                                   
         LA    R4,SORTLNG(R4)                                                   
         CLI   SORTRAT,X'FF'                                                    
         BE    LASTLINE                                                         
         BCT   R3,PRNTRAT                                                       
         LA    R3,PFOURTH                                                       
         CR    R5,R3                                                            
         BL    NEXTLINE                                                         
         BAS   RE,PRINTIT                                                       
         B     FRSTLINE                                                         
         SPACE 1                                                                
LASTLINE BAS   RE,PRINTIT                                                       
         B     ISZERO                                                           
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                      ADD HEADINGS AND PRINT                         *         
***********************************************************************         
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         CLI   PAGEWDTH,132                                                     
         BNE   PRINTIT1                                                         
         MVC   HEAD1+49(22),=C'ADJUSTMENT RATE REPORT'                          
         MVC   HEAD2+49(22),=28C'-'                                             
         MVC   HEAD1+92(9),=C'REPORT AC'                                        
         MVC   HEAD1+101(2),QPROG                                               
         MVC   HEAD1+121(4),=C'PAGE'                                            
         LA    R3,HEAD1+126                                                     
         EDIT  PAGE,(4,0(R3)),ALIGN=LEFT                                        
         MVC   HEAD2+92(9),=C'REQUESTOR'                                        
         MVC   HEAD2+102(L'QUESTOR),QUESTOR                                     
         CLI   QFILTER1,C' '                                                    
         BE    PRINTIT2                                                         
         MVC   HEAD3+92(9),=C'QFILTER1='                                        
         MVC   HEAD3+101(1),QFILTER1                                            
         B     PRINTIT2                                                         
         SPACE                                                                  
PRINTIT1 DS    0H                                                               
         MVC   HEAD1+42(22),=C'ADJUSTMENT RATE REPORT'                          
         MVC   HEAD2+42(22),=28C'-'                                             
         MVC   HEAD1+77(9),=C'REPORT AC'                                        
         MVC   HEAD1+86(2),QPROG                                                
         MVC   HEAD1+99(4),=C'PAGE'                                             
         LA    R3,HEAD1+104                                                     
         EDIT  PAGE,(4,0(R3)),ALIGN=LEFT                                        
         MVC   HEAD2+77(9),=C'REQUESTOR'                                        
         MVC   HEAD2+87(L'QUESTOR),QUESTOR                                      
         CLI   QFILTER1,C' '                                                    
         BE    PRINTIT2                                                         
         MVC   HEAD3+77(9),=C'QFILTER1='                                        
         MVC   HEAD3+86(1),QFILTER1                                             
PRINTIT2 DS    0H                                                               
         MVC   HEAD3+12(L'SCOMNAME),SCOMNAME                                    
         MVC   HEAD4+12(L'SOFF),SOFF                                            
         MVC   HEAD4+20(L'SOFFNAME),SOFFNAME                                    
         CLC   SOFF,SPACES                                                      
         BH    *+10                                                             
         MVC   HEAD4+12(3),=C'ALL'                                              
         MVC   HEAD5+12(L'SCLI),SCLI                                            
         MVC   HEAD5+20(L'SCLINAME),SCLINAME                                    
         MVC   HEAD6+12(L'SPRO),SPRO                                            
         MVC   HEAD6+20(L'SPRONAME),SPRONAME                                    
         MVC   HEAD7+12(L'SJOB),SJOB                                            
         MVC   HEAD7+20(L'SJOBNAME),SJOBNAME                                    
         MVI   SPACING,1                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                      GET NAME OF COMPANY                            *         
***********************************************************************         
         SPACE 1                                                                
GETCOMP  ST    RE,SAVERE                                                        
         MVC   SCOMNAME,SPACES                                                  
         MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(1),QCOMPANY                                                
         LA    R6,SCOMNAME                                                      
         BAS   RE,GETNAME                                                       
         B     ISZERO                                                           
         EJECT                                                                  
***********************************************************************         
*                      GET HIERARCHY ELEMENT                          *         
***********************************************************************         
         SPACE 1                                                                
GETHIER  ST    RE,SAVERE                                                        
         MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(1),QCOMPANY                                                
         MVC   MYKEY+1(2),=C'1R'                                                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',MYKEY,BUFFER                 
         CLI   DMCB+8,0            FOUND ?                                      
         BE    *+6                                                              
         DC    H'0'                ERROR IF NOT FOUND                           
*                                                                               
         MVI   ELCODE,ACHRELQ      GET HIERARCHY                                
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACHEIRD,R2                                                       
         SR    RF,RF                                                            
         IC    RF,ACHRLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SAVELED(0),ACHREL                                                
         B     ISZERO                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*                           GET NAME OF TASK                          *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWTASK  ST    RE,SAVERE                                                        
         CLC   STSK,SORTTSK        EXIT IF TASK SAME                            
         BE    ISZERO                                                           
         MVC   STSK,SPACES                                                      
         MVC   STSKNAME,SPACES                                                  
         OC    SORTTSK,SORTTSK                                                  
         BZ    ISZERO                                                           
         MVC   STSK,SORTTSK                                                     
         MVC   MYKEY,SPACES                                                     
         MVI   MYKEY,X'0A'                                                      
         MVC   MYKEY+1(1),QCOMPANY                                              
         MVC   MYKEY+2(2),=C'SJ'                                                
         MVC   MYKEY+4(2),SORTTSK                                               
         LA    R6,STSKNAME                                                      
         BAS   RE,CHKTSK                                                        
         B     NOTZERO                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                           GET NAME OF STAFF                         *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWSTF   ST    RE,SAVERE                                                        
         CLC   SDEP(STSK-SDEP),SORTDEP  EXIT IF DEPT/SUB-DEPT/STAFF             
         BE    ISZERO              IS THE SAME                                  
         MVC   SSTF,SPACES                                                      
         MVC   SSTFNAME,SPACES                                                  
         OC    SORTSTF,SORTSTF                                                  
         BZ    ISZERO                                                           
         MVC   SSTF,SORTSTF                                                     
         LA    R6,SSTFNAME                                                      
         BAS   RE,GETKEY                                                        
         BAS   RE,GETNAME                                                       
         B     NOTZERO                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                         GET NAME OF SUB-DEPT                        *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWSUB   ST    RE,SAVERE                                                        
         CLC   SDEP(SSTF-SDEP),SORTDEP  EXIT IF DEPT/SUB-DEPT SAME              
         BE    ISZERO                                                           
         MVC   SSUB,SPACES                                                      
         MVC   SSUBNAME,SPACES                                                  
         OC    SORTSUB,SORTSUB                                                  
         BZ    ISZERO                                                           
         MVC   SSUB,SORTSUB                                                     
         LA    R6,SSUBNAME                                                      
         BAS   RE,GETKEY                                                        
         BAS   RE,GETNAME                                                       
         B     NOTZERO                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                         GET NAME OF DEPARTMENT                      *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWDEP   ST    RE,SAVERE                                                        
         CLC   SDEP,SORTDEP        EXIT IF SAME DEPT                            
         BE    ISZERO                                                           
         MVC   SDEP,SPACES         IF NOT, SAVE NEW CODE                        
         MVC   SDEPNAME,SPACES     CLEAR CODE AND NAME                          
         OC    SORTDEP,SORTDEP     IF BINARY ZEROS, EXIT                        
         BZ    ISZERO                                                           
         MVC   SDEP,SORTDEP        IF NOT, SAVE NEW CODE                        
         LA    R6,SDEPNAME                                                      
         BAS   RE,GETKEY                                                        
         BAS   RE,GETNAME                                                       
         B     NOTZERO                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                         GET NAME OF OFFICE                          *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWDOF   ST    RE,SAVERE                                                        
         CLC   SDOF,SORTDOF        EXIT IF SAME OFFICE                          
         BE    ISZERO                                                           
         MVC   SDOF,SPACES         IF NOT, SAVE NEW CODE                        
         MVC   SDOFNAME,SPACES     CLEAR CODE AND NAME                          
         OC    SORTDOF,SORTDOF     IF BINARY ZEROS, EXIT                        
         BZ    ISZERO                                                           
         MVC   SDOF,SORTDOF        IF NOT, SAVE NEW CODE                        
         LA    R6,SDOFNAME                                                      
         BAS   RE,GETKEY                                                        
         BAS   RE,GETNAME                                                       
         B     NOTZERO                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                           GET NAME OF OFFICE                        *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWOFF   ST    RE,SAVERE                                                        
         CLC   SOFF,SORTOFF        EXIT IF OFFICE SAME                          
         BE    ISZERO                                                           
         MVC   SOFF,SPACES                                                      
         MVC   SOFFNAME,SPACES                                                  
         OC    SORTOFF,SORTOFF                                                  
         BZ    ISZERO                                                           
         MVC   SOFF,SORTOFF                                                     
         L     RE,ADCMPEL                                                       
         USING CPYELD,RE                                                        
         TM    CPYSTAT4,CPYSOFF2   TEST NEW OFFICES                             
         BO    NEWOFF2             YES                                          
         SPACE 1                                                                
NEWOFF1  MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(1),QCOMPANY                                                
         MVC   MYKEY+1(2),=C'2D'                                                
         MVC   MYKEY+3(2),SORTOFF                                               
         B     NEWOFF4                                                          
         SPACE 1                                                                
NEWOFF2  XC    MYKEY,MYKEY                                                      
         LA    RE,MYKEY                                                         
         USING OGRRECD,RE                                                       
         MVI   OGRKTYP,OGRKTYPQ    BUILD PRODUCTION OFFICE KEY                  
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,QCOMPANY                                                 
         MVC   OGRKUNT(2),=C'SJ'                                                
         MVC   OGRKOFC,SORTOFF                                                  
         SPACE 1                                                                
NEWOFF4  LA    R6,SOFFNAME                                                      
         BAS   RE,GETNAME                                                       
         B     NOTZERO                                                          
         DROP  R4,RE                                                            
         EJECT                                                                  
***********************************************************************         
*                         GET NAME OF CLIENT                          *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWCLI   ST    RE,SAVERE                                                        
         CLC   SCLI,SORTCLI        EXIT IF CLIENT SAME                          
         BE    ISZERO                                                           
         MVC   SCLI,SPACES         IF NOT, SAVE NEW CODE                        
         MVC   SCLINAME,SPACES     CLEAR CODE AND NAME                          
         OC    SORTCLI,SORTCLI     IF BINARY ZEROS, EXIT                        
         BZ    ISZERO                                                           
         MVC   SCLI,SORTCLI        IF NOT, SAVE NEW CODE                        
         MVC   MYKEY,SPACES         AND GET THE NAME                            
         MVC   MYKEY(1),QCOMPANY                                                
         MVC   MYKEY+1(2),=C'SJ'                                                
         MVC   MYKEY+3(L'SORTCLI),SORTCLI                                       
         LA    R6,SCLINAME                                                      
         BAS   RE,GETNAME                                                       
         B     NOTZERO                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                         GET NAME OF PRODUCT                         *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWPRO   ST    RE,SAVERE                                                        
         CLC   SCLI(L'SCLI+L'SPRO),SORTCLI  EXIT IF CLI/PRO THE SAME            
         BE    ISZERO                                                           
         MVC   SPRO,SPACES         IF NOT, SAVE NEW CODE                        
         MVC   SPRONAME,SPACES     CLEAR CODE AND NAME                          
         OC    SORTPRO,SORTPRO     IF BINARY ZEROS, EXIT                        
         BZ    ISZERO                                                           
         MVC   SPRO,SORTPRO        IF NOT, SAVE NEW CODE                        
         MVC   MYKEY,SPACES         AND GET THE NAME                            
         MVC   MYKEY(1),QCOMPANY                                                
         MVC   MYKEY+1(2),=C'SJ'                                                
         MVC   MYKEY+3(L'SORTCLI),SORTCLI                                       
         MVC   MYKEY+3+L'SORTCLI(L'SORTPRO),SORTPRO                             
         LA    R6,SPRONAME                                                      
         BAS   RE,GETNAME                                                       
         B     NOTZERO                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                         GET NAME OF JOB                             *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWJOB   ST    RE,SAVERE                                                        
         CLC   SCLI(L'SCLI+L'SPRO+L'SJOB),SORTCLI  EXIT IF SAME                 
         BE    ISZERO                                                           
         MVC   SJOB,SPACES         IF NOT, SAVE NEW CODE                        
         MVC   SJOBNAME,SPACES     CLEAR CODE AND NAME                          
         OC    SORTJOB,SORTJOB     IF BINARY ZEROS, EXIT                        
         BZ    ISZERO                                                           
         MVC   SJOB,SORTJOB        IF NOT, SAVE NEW CODE                        
         MVC   MYKEY,SPACES         AND GET THE NAME                            
         MVC   MYKEY(1),QCOMPANY                                                
         MVC   MYKEY+1(2),=C'SJ'                                                
         MVC   MYKEY+3(L'SORTCLI),SORTCLI                                       
         MVC   MYKEY+3+L'SORTCLI(L'SORTPRO),SORTPRO                             
         MVC   MYKEY+3+L'SORTCLI+L'SORTPRO(L'SORTJOB),SORTJOB                   
         LA    R6,SJOBNAME                                                      
         BAS   RE,GETNAME                                                       
         B     NOTZERO                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                FORMAT KEY OF 1R, RETURN BASED ON R6 VALUE           *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
         USING ACHEIRD,R5                                                       
GETKEY   NTR1                                                                   
         LA    R5,SAVELED                                                       
         MVC   MYKEY,SPACES                                                     
         LA    R3,MYKEY                                                         
         MVC   0(1,R3),QCOMPANY                                                 
         MVC   1(2,R3),=C'1R'                                                   
         LA    R3,3(R3)                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,ACHRLEVA                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SORTDOF                                                  
         C     R6,=A(SDOFNAME)                                                  
         BE    GETKEYX                                                          
*                                                                               
         LA    R3,1(R3,RF)                                                      
         SR    RF,RF                                                            
         IC    RF,ACHRLEVB                                                      
         SR    R1,R1                                                            
         IC    R1,ACHRLEVA                                                      
         SR    RF,R1                                                            
         BZ    GETKEYX                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SORTDEP                                                  
         C     R6,=A(SDEPNAME)                                                  
         BE    GETKEYX                                                          
*                                                                               
         LA    R3,1(R3,RF)                                                      
         SR    RF,RF                                                            
         IC    RF,ACHRLEVC                                                      
         SR    R1,R1                                                            
         IC    R1,ACHRLEVB                                                      
         SR    RF,R1                                                            
         BZ    GETKEYX                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SORTSUB                                                  
         C     R6,=A(SSUBNAME)                                                  
         BE    GETKEYX                                                          
*                                                                               
         LA    R3,1(R3,RF)                                                      
         SR    RF,RF                                                            
         IC    RF,ACHRLEVD                                                      
         SR    R1,R1                                                            
         IC    R1,ACHRLEVC                                                      
         SR    RF,R1                                                            
         BZ    GETKEYX                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     GETKEYX                                                          
         MVC   0(0,R3),SORTSTF                                                  
*                                                                               
GETKEYX  B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
*                MOVE NAME INTO SAVEAREA ADDRESSED BY R6              *         
***********************************************************************         
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',MYKEY,BUFFER                     
         CLI   DMCB+8,0                                                         
         BNE   XIT                                                              
         LA    R2,BUFFER                                                        
         CLC   MYKEY,0(R2)                                                      
         BE    NEWNAME                                                          
NONAME   MVC   0(15,R6),=C'NO RECORD FOUND'                                     
         B     XIT                                                              
         SPACE 2                                                                
NEWNAME  MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   CHKTSK                                                           
         USING NAMELD,R2                                                        
         ZIC   R3,NAMLN                                                         
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,38                                                            
         CH    R3,=H'38'                                                        
         BL    *+8                                                              
         LA    R3,38                                                            
         SH    R3,=H'3'                                                         
         EX    R3,MVCNAME                                                       
         B     XIT                                                              
         SPACE 1                                                                
MVCNAME  MVC   0(0,R6),NAMEREC                                                  
         SPACE 3                                                                
CHKTSK   CLI   MYKEY,X'0A'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,WCOELQ                                                    
         BAS   RE,GETELIO                                                       
         USING WCOELD,R2                                                        
         MVC   0(15,R6),WCODESC                                                 
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*     ROUTINE TO TEST IF HEADING WAS GENERATED AND SET RETURN CODE    *         
***********************************************************************         
         SPACE 1                                                                
NOTZERO  L     RE,SAVERE                                                        
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 1                                                                
ISZERO   L     RE,SAVERE                                                        
         CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
GETELIO  LA    R2,BUFFER                                                        
         GETEL R2,DATADISP,ELCODE                                               
         SPACE 3                                                                
SORTER   DC    V(SORTER)                                                        
         SPACE 3                                                                
SORTCARD DC    CL80'SORT FIELDS=(5,1,A,6,30,A),FORMAT=BI'                       
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(1004)'                                
         SPACE 3                                                                
SORTREC  DS    CL1004                                                           
         SPACE 3                                                                
SAVELED  DS    CL(ACHRLENQ)                                                     
         SPACE 3                                                                
XSORTADD DS    F                                                                
         SPACE 3                                                                
COUNTIN  DC    PL6'0'              RECORDS INTO SORT                            
COUNTOUT DC    PL6'0'              RECORDS OUT OF SORT                          
         SPACE 3                                                                
AJRKEY   DS    0CL42               ADJUSTMENT RATE RECORD KEY                   
         DC    X'19'               RECORD I.D.                                  
AJRCMP   DS    CL1                 COMPANY                                      
         DC    XL40'00'            BINARY ZEROS FOR ALL FIELDS                  
         SPACE 3                                                                
         DS    0D                                                               
BUFFER   DC    1000X'00'                                                        
         EJECT                                                                  
***********************************************************************         
*                    CODE AND NAME SAVE AREAS                         *         
***********************************************************************         
         SPACE 1                                                                
SOFF     DC    CL(L'PAJKOFF)' '    OFFICE CODE                                  
SCLI     DC    CL(L'PAJKCLI)' '    CLIENT CODE                                  
SPRO     DC    CL(L'PAJKPRO)' '    PRODUCT CODE                                 
SJOB     DC    CL(L'PAJKJOB)' '    JOB CODE                                     
SDOF     DC    CL(L'PAJKDOF)' '    OFFICE CODE                                  
SDEP     DC    CL(L'PAJKDEP)' '    DEPT CODE                                    
SSUB     DC    CL(L'PAJKSUB)' '    SUB-DEPT CODE                                
SSTF     DC    CL(L'PAJKSTF)' '    STAFF CODE                                   
STSK     DC    CL(L'PAJKTSK)' '    TASK CODE                                    
SCOMNAME DS    CL38                COMPANY NAME                                 
SOFFNAME DS    CL38                OFFICE NAME                                  
SCLINAME DS    CL38                CLIENT NAME                                  
SPRONAME DS    CL38                PRODUCT NAME                                 
SJOBNAME DS    CL38                JOB NAME                                     
SDOFNAME DS    CL38                OFFICE NAME                                  
SDEPNAME DS    CL38                DEPT NAME                                    
SSUBNAME DS    CL38                SUB-DEPT NAME                                
SSTFNAME DS    CL38                STAFF NAME                                   
STSKNAME DS    CL15                TASK NAME                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
HOOK     NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC           RESTORE REG C                                
         L     R3,ADBOX                                                         
         USING BOXD,R3                                                          
         MVC   MYCOL,SPACES                                                     
         MVC   MYROW,SPACES                                                     
*                                                                               
         MVI   MYCOL,C'L'                                                       
         MVI   MYCOL+OLENGTH,C'C'                                               
         MVI   MYCOL+DLENGTH,C'C'                                               
         MVI   MYCOL+SLENGTH,C'C'                                               
         MVI   MYCOL+PLENGTH,C'C'                                               
*                                                                               
         MVI   MYCOL+87,C'C'                                                    
         MVI   MYCOL+88,C'C'                                                    
         MVI   MYCOL+97,C'C'                                                    
         MVI   MYCOL+107,C'C'                                                   
         MVI   MYCOL+108,C'C'                                                   
         MVI   MYCOL+117,C'C'                                                   
         MVI   MYCOL+127,C'C'                                                   
         MVI   MYCOL+128,C'R'                                                   
         MVI   MYROW+7,C'T'                                                     
         MVI   MYROW+9,C'M'                                                     
         MVI   MYROW+56,C'B'                                                    
         MVC   BOXCOLS(L'MYCOL),MYCOL                                           
         MVC   BOXROWS(L'MYROW),MYROW                                           
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         XMOD1 1                                                                
SAVERC   DC    A(0)                                                             
         EJECT                                                                  
ACD8D    DSECT                                                                  
ADBOX    DS    A                                                                
ELCODE   DS    CL1                                                              
SAVEKEY  DS    CL42                                                             
SAVERE   DS    F                                                                
MYCOL    DS    CL132                                                            
MYROW    DS    CL100                                                            
MIDBAL   DS    CL1                                                              
PAGEWDTH DS    CL1                                                              
PEND     DS    CL3                                                              
PSTART   DS    CL3                                                              
TERMED   DS    C                                                                
MYKEY    DS    CL42                                                             
ACD8DEND DS    0D                                                               
         EJECT                                                                  
LINED    DSECT                                                                  
         DS    CL89                                                             
LINEDTE  DS    CL8                                                              
         DS    CL1                                                              
LINERTE  DS    CL9                                                              
         DS    CL2                                                              
LINELNG  EQU   *-LINEDTE                                                        
         EJECT                                                                  
SORTD    DSECT                                                                  
SORTLEN  DS    F                                                                
SORTPRE  DS    CL1                                                              
SORTOFF  DS    CL(L'PAJKOFF)                                                    
SORTCLI  DS    CL(L'PAJKCLI)                                                    
SORTPRO  DS    CL(L'PAJKPRO)                                                    
SORTJOB  DS    CL(L'PAJKJOB)                                                    
SORTDOF  DS    CL(L'PAJKDOF)                                                    
SORTDEP  DS    CL(L'PAJKDEP)                                                    
SORTSUB  DS    CL(L'PAJKSUB)                                                    
SORTSTF  DS    CL(L'PAJKSTF)                                                    
SORTTSK  DS    CL(L'PAJKTSK)                                                    
SORTRAT  DS    PL4                                                              
SORTDAT  DS    PL3                                                              
SORTLNG  EQU   *-SORTRAT                                                        
         EJECT                                                                  
PRINT0   DSECT                                                                  
         DS    C                                                                
OCODE0   DS    CL(L'PAJKDOF)                                                    
         DS    C                                                                
ONAME0   DS    CL12                                                             
         DS    C                                                                
OLENGTH  EQU   *-PRINT0                                                         
*                                                                               
         DS    C                                                                
DCODE0   DS    CL(L'PAJKDEP)                                                    
         DS    C                                                                
DNAME0   DS    CL12                                                             
DLENGTH  EQU   *-PRINT0                                                         
*                                                                               
         DS    C                                                                
SCODE0   DS    CL(L'PAJKSUB)                                                    
         DS    C                                                                
SNAME0   DS    CL12                                                             
SLENGTH  EQU   *-PRINT0                                                         
*                                                                               
         DS    C                                                                
PCODE0   DS    CL(L'PAJKSTF)                                                    
         DS    C                                                                
PNAME0   DS    CL12                                                             
PLENGTH  EQU   *-PRINT0                                                         
*                                                                               
         DS    C                                                                
TCODE0   DS    CL(L'PAJKTSK)                                                    
         DS    C                                                                
TNAME0   DS    CL12                                                             
         EJECT                                                                  
* ACREPWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDREPMASTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACREPD802 04/08/15'                                      
         END                                                                    
