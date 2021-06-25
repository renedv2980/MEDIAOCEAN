*          DATA SET ACREPC802  AT LEVEL 008 AS OF 02/28/01                      
*PHASE ACC802A                                                                  
*INCLUDE DLFLD                                                                  
         TITLE 'ACC802 - CHARGE RATE REPORT'                                    
**********************************************************************          
*     QOPT1 (S)   = SUPPRESS TERMINATED                              *          
*     QOPT2 (S)   = SUPPRESS LOCKED                                  *          
*     QOPT7 (Y/N) = DOWNLOAD                                         *          
**********************************************************************          
ACC802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACC8**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACC8D,RC                                                         
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         B     XIT                                                              
*                                                                               
REQF     LA    RE,VTYPES           RELOCATE VTYPES                              
         LA    R0,ADCONS                                                        
         LA    RF,VTYPLNQ                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   QOPT7,C'Y'          IS THIS A DOWNLOAD?                          
         BNE   REQF10                                                           
         MVI   RCSUBPRG,9          SET PRINT RTE AS INVALID                     
         GOTO1 ADWNL,DMCB,(RC),DWNINIT      INITIALIZE DOWNLOAD RTE             
*                                                                               
REQF10   L     RF,VEXTRAS                                                       
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
         MVC   CHGCMP,QCOMPANY     PUT COMPANY IN KEY                           
         BAS   RE,GETCOMP           AND GET NAME                                
         BAS   RE,GETHIER          GET HEIRARCHY ELEMENT                        
         XC    CHGKEY+2(L'CHGKEY-2),CHGKEY+2                                    
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
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
* READ CHARGE RATE RECORDS AND SEND TO SORT. WHEN KEY CHANGES RETREIVE*         
* RECORDS, FORMAT AND PRINT DATA.                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING PCHRECD,R2                                                       
GETREC   GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',CHGKEY,BUFFER                
         B     GETDMCB             CHECK RETURN                                 
*                                                                               
GETMORE  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCOUNT',BUFFER,BUFFER                
*                                                                               
GETDMCB  CLI   DMCB+8,0            EXIT IF ERROR FOUND                          
         BNE   XIT                                                              
         LA    R2,BUFFER                                                        
         CLC   CHGKEY(2),0(R2)     SORT WHEN KEY CHANGES                        
         BNE   GETSORT                                                          
*                                                                               
GETOFF   CLC   QAPPL(L'PCHKDOF),SPACES      TAKING ALL OFFICES ?                
         BE    GETDEPT              YES, SEE IF WE ARE TAKING THIS DEPT         
         CLC   QAPPL(L'PCHKDOF),PCHKDOF     TAKING THIS ONE ?                   
         BNE   GETMORE              NO, GET NEXT RECORD                         
*                                                                               
GETDEPT  CLC   QSELECT(L'PCHKDEP),SPACES    TAKING ALL DEPTS ?                  
         BE    GETTASK              YES, SEE IF WE ARE TAKING THIS TASK         
         CLC   QSELECT(L'PCHKDEP),PCHKDEP   TAKING THIS ONE ?                   
         BNE   GETMORE              NO, GET NEXT RECORD                         
*                                                                               
GETTASK  CLC   QAPPL+2(L'PCHKTSK),SPACES                                        
         BE    *+14                NO TASK SELECTION CRITERIA                   
         CLC   QAPPL+2(L'PCHKTSK),PCHKTSK                                       
         BNE   GETMORE            SKIP THIS TASK                                
*                                                                               
         CLI   PCHKSTF,X'41'       ARE WE AT STAFF LEVEL ?                      
         BL    GETFORM             NO, SKIP OVER                                
         MVC   SAVEKEY,0(R2)       SAVE FOR REREAD                              
         BAS   RE,TERMTEST                                                      
         CLI   TERMED,C'Y'         TERMINATED/LOCKED/MISSING?                   
         BE    GETMORE             YES                                          
*                                                                               
GETFORM  LA    R4,SORTREC                                                       
         BAS   RE,FORMSORT         FORMAT RECORD FOR SORT                       
         BAS   RE,XSORTEL          SORT ELEMENTS FIRST                          
         BNZ   GETMORE             NOTHING TO SORT, GET NEXT REC                
         BAS   RE,GETELMS          ADD ELEMENTS TO IT                           
         BNZ   GETMORE             NOTHING TO SORT, GET NEXT REC                
         GOTO1 ADSORTER,DMCB,=C'PUT',SORTREC                                    
         AP    COUNTIN,=P'1'                                                    
         B     GETMORE                                                          
         SPACE 3                                                                
GETSORT  GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   R6,15,DMCB+4        NO ADDRESS INDICATES NO MORE                 
         BZ    DONE                   RECORDS                                   
         LA    R4,SORTREC          MOVE RECORD FROM SORT                        
         LH    R5,0(R6)             TO SORTREC SO WE CAN SEE IT                 
         LR    R7,R5                                                            
         MVCL  R4,R6                                                            
         LA    R4,SORTREC          READDDRESS THE RECORD                        
         AP    COUNTOUT,=P'1'                                                   
         BAS   RE,FORMPRT          FORMAT PRINT BASED ON TYPE                   
         TM    LCKSW,LCKOF+LCKDP+LCKSD    TEST LOCKED ACCOUNT                   
         BNZ   GETSORT                                                          
         BAS   RE,PRNTELMS         GET RATES AND DATES AND PRINT                
         B     GETSORT             GET MORE                                     
         SPACE 3                                                                
DONE     GOTO1 ADSORTER,DMCB,=C'END'                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
REQL     DS    0H                                                               
         CLI   QOPT7,C'Y'          WAS DOWNLOAD SELECTED                        
         BNE   REQLX                                                            
         MVC   P,SPACES                                                         
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOR     DOWNLOAD EOR MARKER                   
REQLX    B     XIT                                                              
***********************************************************************         
*       FORMAT SORT RECORD WITH DATA FROM CHARGE RATE RECORD KEY      *         
***********************************************************************         
         SPACE 1                                                                
         USING PCHRECD,R2                                                       
         USING SORTD,R4                                                         
FORMSORT XC    SORTREC(SORTDAT-SORTD),SORTREC                                   
         MVC   SORTDOF(L'PCHKDOF),PCHKDOF                                       
         MVC   SORTDEP(L'PCHKDEP),PCHKDEP                                       
         MVC   SORTSUB(L'PCHKSUB),PCHKSUB                                       
         MVC   SORTSTF(L'PCHKSTF),PCHKSTF                                       
         MVC   SORTTSK(L'PCHKTSK),PCHKTSK                                       
         MVC   SORTOFF(L'PCHKOFF),PCHKOFF                                       
         MVC   SORTPRO(L'PCHKPRO),PCHKPRO                                       
         MVC   SORTCLI(L'PCHKCLI),PCHKCLI                                       
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
         USING ACCHARGD,R2                                                      
         USING SORTD,R4                                                         
GETELMS  ST    RE,SAVERE                                                        
         SR    R6,R6                                                            
         MVI   ELCODE,X'53'         RATE ELEMENT                                
         BAS   RE,GETELIO                                                       
         B     *+8                                                              
*                                                                               
GETNEXT  BAS   RE,NEXTEL                                                        
         BNE   NOMORE                                                           
         CLC   ACCHDTE,PEND        SEE IF DATE WITHIN RANGE                     
         BH    GETNEXT              SKIP IF HIGHER THAN END                     
         CLC   ACCHDTE,PSTART                                                   
         BL    GETNEXT              OR LOWER THAN START                         
         ZAP   SORTRAT,ACCHAMT                                                  
         MVC   SORTDAT,ACCHDTE                                                  
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
         USING PCHRECD,R2                                                       
         USING ACHEIRD,R5                                                       
TERMTEST NTR1                                                                   
         LA    R3,MYKEY                                                         
         LA    R5,SAVELED                                                       
         MVI   TERMED,C'N'                                                      
         MVC   MYKEY,SPACES                                                     
         MVC   0(1,R3),QCOMPANY                                                 
         MVC   1(2,R3),=C'1R'                                                   
         LA    R3,3(R3)            GET STARTING LOCATION                        
*                                                                               
         LA    R4,PCHKDOF          MOVE OFFICE TO KEY                           
         SR    RF,RF                                                            
         IC    RF,ACHRLEVA                                                      
         BCTR  RF,0                                                             
         EX    RF,MVCDATA                                                       
         LA    R3,1(RF,R3)         BUMP TO NEXT SPOT                            
*                                                                               
         LA    R4,PCHKDEP          MOVE DEPARTMENT TO KEY                       
         IC    RF,ACHRLEVB                                                      
         SR    R1,R1                                                            
         IC    R1,ACHRLEVA                                                      
         SR    RF,R1                                                            
         BZ    TERMT2                                                           
         BCTR  RF,0                                                             
         EX    RF,MVCDATA                                                       
         LA    R3,1(RF,R3)                                                      
*                                                                               
         LA    R4,PCHKSUB          MOVE SUB-DEPARTMENT TO KEY                   
         IC    RF,ACHRLEVC                                                      
         SR    R1,R1                                                            
         IC    R1,ACHRLEVB                                                      
         SR    RF,R1                                                            
         BZ    TERMT2                                                           
         BCTR  RF,0                                                             
         EX    RF,MVCDATA                                                       
         LA    R3,1(RF,R3)                                                      
*                                                                               
         LA    R4,PCHKSTF          MOVE STAFF TO KEY                            
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
TERMT2   GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYKEY,BUFFER                 
         CLI   DMCB+8,0            FOUND?                                       
         BNE   TERMT10                                                          
*                                                                               
TERMT3   AH    R2,DATADISP                                                      
         SR    R0,R0                                                            
TERMT4   CLI   0(R2),0                                                          
         BE    REREAD                                                           
         CLI   0(R2),X'56'                                                      
         BNE   TERMT6                                                           
         CLI   QOPT1,C'S'          SUPPRESS TERMINATED                          
         BNE   TERMT8                                                           
         USING ACEMPD,R2                                                        
         OC    ACEMPTRM,ACEMPTRM                                                
         BZ    TERMT8                                                           
         B     TERMT10                                                          
*                                                                               
TERMT6   CLI   0(R2),X'30'         TEST STATUS ELEMENT                          
         BNE   TERMT8                                                           
         CLI   QOPT2,C'S'          SUPPRESS LOCKED                              
         BNE   TERMT8                                                           
         USING RSTELD,R2                                                        
         TM    RSTSTAT1,RSTSACIL   TEST LOCKED                                  
         BO    TERMT10                                                          
TERMT8   IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     TERMT4                                                           
*                                                                               
TERMT10  MVI   TERMED,C'Y'                                                      
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
FORMPRT  NTR1                                                                   
         BAS   RE,KEYCHG                                                        
         TM    LCKSW,LCKOF+LCKDP+LCKSD    TEST LOCKED ACCOUNT                   
         BNZ   XIT                                                              
         CLI   FORCEHED,C'Y'                                                    
         BE    MOVETSK                                                          
         L     R6,ADBOX                                                         
         USING BOXD,R6                                                          
         MVI   BOXREQ,C'B'                                                      
         MVI   BOXSTAT,C'I'                                                     
         BAS   RE,OUTREC                                                        
MOVETSK  LA    R5,P                                                             
         USING PRINT0,R5                                                        
         BAS   RE,NEWTASK                                                       
         MVC   TCODE0,STSK                                                      
         MVC   TNAME0,STSKNAME                                                  
         BAS   RE,NEWOFF                                                        
         MVC   OCODE0,SOFF                                                      
         MVC   ONAME0,SOFFNAME                                                  
         BAS   RE,NEWPRO                                                        
         MVC   PCODE0,SPRO                                                      
         MVC   PNAME0,SPRONAME                                                  
         BAS   RE,NEWCLI                                                        
         MVC   CCODE0,SCLI                                                      
         MVC   CNAME0,SCLINAME                                                  
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
PRNTRAT  EDIT  SORTRAT,(7,LINERTE),2,FLOAT=-                                    
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
         BAS   RE,OUTREC                                                        
         B     FRSTLINE                                                         
         SPACE 1                                                                
LASTLINE BAS   RE,OUTREC                                                        
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
         MVC   HEAD1+49(18),=C'CHARGE RATE REPORT'                              
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
         MVC   HEAD1+42(18),=C'CHARGE RATE REPORT'                              
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
         MVC   HEAD2+12(L'SCOMNAME),SCOMNAME                                    
         MVC   HEAD3+12(L'SDOF),SDOF                                            
         MVC   HEAD3+20(L'SDOFNAME),SDOFNAME                                    
         MVC   HEAD4+12(L'SDEP),SDEP                                            
         MVC   HEAD4+20(L'SDEPNAME),SDEPNAME                                    
         MVC   HEAD5+12(L'SSUB),SSUB                                            
         MVC   HEAD5+20(L'SSUBNAME),SSUBNAME                                    
         MVC   HEAD6+12(L'SSTF),SSTF                                            
         MVC   HEAD6+20(L'SSTFNAME),SSTFNAME                                    
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                   PRINT/DOWNLOAD INTERFACE                          *         
***********************************************************************         
         SPACE 1                                                                
OUTREC   NTR1                                                                   
         CLI   QOPT7,C'Y'          IS THIS A DOWNLOAD?                          
         BNE   OUTR10                                                           
*                                                                               
         MVI   RCSUBPRG,9                                                       
         CLC   P,SPACES            DON'T DOWNLOAD BLANK LINE                    
         BE    OUTRX                                                            
*                                                                               
         MVC   SVPRNT2,PSECOND                                                  
         MVC   PSECOND,SPACES                                                   
         MVC   SVPRNT3,PTHIRD                                                   
         MVC   PTHIRD,SPACES                                                    
         MVC   SVPRNT4,PFOURTH                                                  
         MVC   PFOURTH,SPACES                                                   
*                                                                               
         MVC   SVPRNT,P                                                         
         MVC   P,SPACES                                                         
*                                                                               
         GOTO1 ADWNRTE,DMCB,(RC)                                                
         B     OUTRX               SKIP PRINTIT ROUTINE                         
*                                                                               
OUTR10   BAS   RE,PRINTIT                                                       
*                                                                               
OUTRX    B     XIT                                                              
***********************************************************************         
*                      GET NAME OF COMPANY                            *         
***********************************************************************         
         SPACE 1                                                                
GETCOMP  ST    RE,SAVERE                                                        
         MVC   SCOMNAME,SPACES                                                  
         MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(1),QCOMPANY                                                
         LA    R6,SCOMNAME                                                      
         MVI   BYTE,0                                                           
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
*                           CHECK FOR PERSON KEY CHANGE               *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
KEYCHG   NTR1  ,                                                                
         CLC   SDOF,SORTDOF        TEST SAME OFFICE                             
         BNE   KEYCHG1                                                          
         CLC   SDEP,SORTDEP                  DEPARTMENT                         
         BNE   KEYCHG2                                                          
         CLC   SSUB,SORTSUB                  SUB-DEPARTMENT                     
         BNE   KEYCHG3                                                          
         CLC   SSTF,SORTSTF                  STAFF                              
         BNE   KEYCHG4                                                          
         B     XIT                                                              
         SPACE 1                                                                
KEYCHG1  BAS   RE,NEWOFC           SET NEW OFFICE                               
KEYCHG2  BAS   RE,NEWDEP                   DEPARTMENT                           
KEYCHG3  BAS   RE,NEWSUB                   SUB-DEPARTMENT                       
KEYCHG4  BAS   RE,NEWSTF                   STAFF                                
         MVC   SDOF(STSK-SDOF),SORTDOF                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                         GET NAME OF OFFICE                          *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWOFC   ST    RE,SAVERE                                                        
         MVI   LCKSW,0                                                          
         MVC   SDOF,SPACES                                                      
         MVC   SDOFNAME,SPACES     CLEAR CODE AND NAME                          
         OC    SORTDOF,SORTDOF     IF BINARY ZEROS, EXIT                        
         BZR   RE                                                               
         LA    R6,SDOFNAME                                                      
         BAS   RE,GETKEY                                                        
         MVI   BYTE,LCKOF                                                       
         BAS   RE,GETNAME                                                       
         MVI   FORCEHED,C'Y'                                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                         GET NAME OF DEPARTMENT                      *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWDEP   ST    RE,SAVERE                                                        
         NI    LCKSW,ALL-(LCKDP+LCKSD)                                          
         MVC   SDEP,SPACES         IF NOT, SAVE NEW CODE                        
         MVC   SDEPNAME,SPACES     CLEAR CODE AND NAME                          
         OC    SORTDEP,SORTDEP     IF BINARY ZEROS, EXIT                        
         BZR   RE                                                               
         LA    R6,SDEPNAME                                                      
         BAS   RE,GETKEY                                                        
         MVI   BYTE,LCKDP                                                       
         BAS   RE,GETNAME                                                       
         MVI   FORCEHED,C'Y'                                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                         GET NAME OF SUB-DEPT                        *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWSUB   ST    RE,SAVERE                                                        
         NI    LCKSW,ALL-(LCKSD)                                                
         MVC   SSUB,SPACES                                                      
         MVC   SSUBNAME,SPACES                                                  
         OC    SORTSUB,SORTSUB                                                  
         BZR   RE                                                               
         LA    R6,SSUBNAME                                                      
         BAS   RE,GETKEY                                                        
         MVI   BYTE,LCKSD                                                       
         BAS   RE,GETNAME                                                       
         MVI   FORCEHED,C'Y'                                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                           GET NAME OF STAFF                         *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWSTF   ST    RE,SAVERE                                                        
         MVC   SSTF,SPACES                                                      
         MVC   SSTFNAME,SPACES                                                  
         OC    SORTSTF,SORTSTF                                                  
         BZR   RE                                                               
         LA    R6,SSTFNAME                                                      
         BAS   RE,GETKEY                                                        
         MVI   BYTE,0                                                           
         BAS   RE,GETNAME                                                       
         MVI   FORCEHED,C'Y'                                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                           GET NAME OF TASK                          *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWTASK  ST    RE,SAVERE                                                        
         CLC   STSK,SORTTSK        EXIT IF TASK SAME                            
         BE    ISZERO                                                           
         MVC   STSK,SORTTSK                                                     
         MVC   STSKNAME,SPACES                                                  
         OC    SORTTSK,SORTTSK                                                  
         BZ    ISZERO                                                           
         MVC   MYKEY,SPACES                                                     
         MVI   MYKEY,X'0A'                                                      
         MVC   MYKEY+1(1),QCOMPANY                                              
         MVC   MYKEY+2(2),=C'SJ'                                                
         MVC   MYKEY+4(L'SORTTSK),SORTTSK                                       
         LA    R6,STSKNAME                                                      
         MVI   BYTE,0                                                           
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
         USING ACCOMPD,RE                                                       
         TM    ACMPSTA4,X'01'      TEST NEW OFFICES                             
         BO    NEWOFF2             YES                                          
         SPACE 1                                                                
NEWOFF1  MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(1),QCOMPANY                                                
         MVC   MYKEY+1(2),=C'2D'                                                
         MVC   MYKEY+3(L'SORTOFF),SORTOFF                                       
         B     NEWOFF4                                                          
         SPACE 1                                                                
NEWOFF2  XC    MYKEY,MYKEY                                                      
         LA    RE,MYKEY                                                         
         USING ACOGKEY,RE                                                       
         MVI   ACOGRTYP,ACOGEQU    BUILD PRODUCTION OFFICE KEY                  
         MVI   ACOGSREC,ACOGOFF                                                 
         MVC   ACOGCUL(1),QCOMPANY                                              
         MVC   ACOGCUL+1(2),=C'SJ'                                              
         MVC   ACOGOFC,SORTOFF                                                  
         SPACE 1                                                                
NEWOFF4  LA    R6,SOFFNAME                                                      
         MVI   BYTE,0                                                           
         BAS   RE,GETNAME                                                       
         B     NOTZERO                                                          
         DROP  R4,RE                                                            
         EJECT                                                                  
***********************************************************************         
*                         GET NAME OF PRODUCT                         *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
NEWPRO   ST    RE,SAVERE                                                        
         CLC   SCLI(SCOMNAME-SCLI),SORTCLI  EXIT IF CLI/PRO THE SAME            
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
         MVI   BYTE,0                                                           
         BAS   RE,GETNAME                                                       
         B     NOTZERO                                                          
         DROP  R4                                                               
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
         MVI   BYTE,0                                                           
         BAS   RE,GETNAME                                                       
         B     NOTZERO                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                FORMAT KEY OF 1R, RETURN BASED ON R6 VALUE           *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R4                                                         
         USING ACHEIRD,R2                                                       
GETKEY   NTR1                                                                   
         LA    R2,SAVELED                                                       
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
         DROP  R2,R4                                                            
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
         BE    GETNAME3                                                         
         MVC   0(15,R6),=C'NO RECORD FOUND'                                     
         B     XIT                                                              
         SPACE 2                                                                
GETNAME3 SR    R0,R0                                                            
         AH    R2,DATADISP                                                      
GETNAME5 CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLI   0(R2),X'20'                                                      
         BNE   CHKTSK                                                           
         USING ACNAMED,R2                                                       
         ZIC   R3,ACNMLEN                                                       
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,38                                                            
         CH    R3,=H'38'                                                        
         BL    *+8                                                              
         LA    R3,38                                                            
         SH    R3,=H'3'                                                         
         EX    R3,*+4                                                           
         MVC   0(0,R6),ACNMNAME                                                 
         B     GETNXTL                                                          
         SPACE 1                                                                
CHKTSK   CLI   0(R2),X'12'                                                      
         BNE   CHKSTA                                                           
         USING ACANALD,R2                                                       
         MVC   0(15,R6),ACANDESC                                                
         B     GETNXTL                                                          
         SPACE 1                                                                
CHKSTA   CLI   0(R2),RSTELQ        STATUS ELEMENT                               
         BNE   GETNXTL                                                          
         CLI   BYTE,0              ANY LOCK TEST                                
         BE    GETNXTL                                                          
         CLI   QOPT2,C'S'          SUPPRESS LOCKED                              
         BNE   GETNXTL                                                          
         USING RSTELD,R2                                                        
         TM    RSTSTAT1,RSTSACIL   TEST LOCKED                                  
         BZ    GETNXTL                                                          
         OC    LCKSW,BYTE          SET THE LOCKED SWITCH                        
         SPACE 1                                                                
GETNXTL  IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GETNAME5                                                         
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
         EJECT                                                                  
*                                                                               
**********************************************************************          
* DOWNLOAD ROUTINE                                                   *          
*          R7 = A(PRINT LINE)                                        *          
**********************************************************************          
         SPACE 1                                                                
         USING PRINT0,R7                                                        
DWNRTE   DS    0D                                                               
         NMOD1 0,**DWNR**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         USING BOXD,R3                                                          
         L     R3,ADBOX                                                         
         MVI   BOXOFF,C'Y'         NO BOXES IF DOWNLOADING                      
         DROP  R3                                                               
*                                                                               
         LA    R7,SVPRNT                                                        
         MVI   FORCEHED,C'Y'                                                    
         LA    R5,4                                                             
*                                                                               
DWNFST   DS    0H                                                               
         LR    R6,R7                                                            
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'SDOF),SDOF        MOVE CODE TO DWN FLD                  
         LA    R1,L'SDOF                  CODE LENGTH                           
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD CHARACTER                    
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'SDEP),SDEP        MOVE CODE TO DWN FLD                  
         LA    R1,L'SDEP                  CODE LENGTH                           
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD CHARACTER                    
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'SSUB),SSUB        MOVE CODE TO DWN FLD                  
         LA    R1,L'SSUB                  CODE LENGTH                           
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD CHARACTER                    
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'SSTF),SSTF        MOVE CODE TO DWN FLD                  
         LA    R1,L'SSTF                  CODE LENGTH                           
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD CHARACTER                    
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'TASK),TASK        MOVE TASK TO DWN FLD                  
         LA    R1,L'TASK                  CODE LENGTH                           
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD CHARACTER                    
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'OFFICE),OFFICE    MOVE OFFICE TO DWN FLD                
         LA    R1,L'OFFICE                CODE LENGTH                           
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD CHARACTER                    
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'CLIENT),CLIENT    MOVE CLIENT TO DWN FLD                
         LA    R1,L'CLIENT                CODE LENGTH                           
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD CHARACTER                    
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PRODUCT),PRODUCT  MOVE PRODUCT TO DWN FLD               
         LA    R1,L'PRODUCT               CODE LENGTH                           
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD CHARACTER                    
*                                                                               
         USING LINED,R7                                                         
*                                                                               
         MVI   PRTSIZE,0                                                        
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'LINEDTE),LINEDTE  MOVE DATE                             
         LA    R1,L'LINEDTE               LENGTH DATE FIELD                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD CHARACTER                    
*                                                                               
         MVI   PRTSIZE,0                                                        
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'LINERTE),LINERTE  MOVE RATE                             
         LA    R1,L'LINERTE               LENGTH RATE FIELD                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         LA    R7,LINELNG(R7)                                                   
*                                                                               
         MVI   PRTSIZE,0                                                        
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'LINEDTE),LINEDTE  MOVE DATE                             
         LA    R1,L'LINEDTE               LENGTH DATE FIELD                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD CHARACTER                    
*                                                                               
         MVI   PRTSIZE,0                                                        
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'LINERTE),LINERTE  MOVE RATE                             
         LA    R1,L'LINERTE               LENGTH RATE FIELD                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOL     DOWNLOAD EOL MARKER                   
         LR    R7,R6                                                            
         LA    R7,L'P(R7)                                                       
         CLC   0((L'P),R7),SPACES                                               
         BE    DWNXIT                                                           
         BCT   R5,DWNFST                                                        
*                                                                               
DWNXIT   XMOD1                                                                  
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD MODULE                                                    *          
*          PARM1 - RC                                                *          
*          PARM2 - ACTION                                            *          
**********************************************************************          
         SPACE 1                                                                
DWNL     DS    0D                                                               
         NMOD1 0,**DOWN**                                                       
         L     RC,0(R1)                                                         
         L     RF,4(R1)                                                         
         STC   RF,DWNMODE          SAVE CURRENT MODE                            
         USING DLCBD,R5                                                         
         L     R5,ADWNBUF                                                       
*                                                                               
         CLI   DWNMODE,DWNINIT     INITIALIZE                                   
         BE    DWNL10                                                           
         CLI   DWNMODE,DWNTEXT     DOWN-LOAD TEXT                               
         BE    DWNL20                                                           
         CLI   DWNMODE,DWNNUM      DOWN-LOAD NUMBER                             
         BE    DWNL30                                                           
         CLI   DWNMODE,DWNPACK     DOWN-LOAD NUMBER (PACKED)                    
         BE    DWNL40                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BE    DWNL50                                                           
         MVI   DLCBACT,DLCBEOR                                                  
         CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    DWNL50                                                           
         DC    H'0'                                                             
*                                                                               
* INITIALIZATION                                                                
*                                                                               
DWNL10   TM    DWNSTAT,DWNINTZ     HAS IT ALREADY BEEN INITIALIZED?             
         BO    DWNLX               YES - EXIT                                   
         MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RE,P                PRINT LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(L'P)                                                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         GOTO1 DLFLD,(R5)                                                       
         MVI   FORCEHED,C'Y'       EXCEPT FIRST TIME IN                         
         GOTO1 ACREPORT                                                         
         MVC   DLCBFLD,SPACES      MUST CLEAR FIRST TIME IN                     
         OI    DWNSTAT,DWNINTZ     TURN ON INITIALIZED BYTE                     
         B     DWNLX               EXIT                                         
*                                                                               
* DOWNLOAD A RECORD - TEXT                                                      
*                                                                               
DWNL20   MVC   DLCBLEN,PRTSIZE     LEN OF FIELD-SET TO 0 (NO PADDING)           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE   IS TEXT                               
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER                                                    
*                                                                               
DWNL30   MVI   DLCBLEN,0           LEN OF FIELD-SET TO 0 (NO PADDING)           
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     TYPE   IS NUMBER                             
         MVI   DLCBLEN,16          YES, USE MAXIMUM LENGTH OF NUMERICS          
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER (PACKED)                                           
*                                                                               
DWNL40   MVI   DLCBTYP,DLCBPACF    PACKED DATA                                  
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBLEN,8        L'PKFLDS YES, USE MAX LEN OF NUMERICS           
         XC    DLCBFLD,DLCBFLD     CLEAN DWNLOAD FIELD TO 0'S                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         NC    DLCBFLD,DLCBFLD     YES, MAKE SURE NUMERIC FLD NOT ZEROS         
         BNZ   DWNL50              NOT  ZERO, DOWN-LOAD FIELD                   
         MVI   DLCBLEN,1           ZERO, SET LENGTH 1 TO DOWN-LOAD A 0          
         B     DWNL50                                                           
*                                                                               
* END OF LINE/END OF RECORD                                                     
*                                                                               
DWNL50   GOTO1 DLFLD,(R5)          DOWN-LOAD FIELD                              
         NI    DLCBFLG1,X'FF'-DLCBFXFL   TURN OFF USING EXTENDED FIELD          
*                                                                               
DWNLX    XMOD1                                                                  
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HOOK                                                      *          
**********************************************************************          
         SPACE 1                                                                
DWNHOOK  MVI   FORCEHED,C'N'       NEVER HEAD UP                                
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         L     RF,ACREPORT                                                      
         BR    RF                                                               
         EJECT                                                                  
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,1,A,6,24,A),FORMAT=BI'                       
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
CHGKEY   DS    0CL42               CHARGE RATE RECORD KEY                       
         DC    X'2A'               RECORD I.D.                                  
CHGCMP   DS    CL1                 COMPANY                                      
         DC    XL40'00'            BINARY ZEROS FOR ALL FIELDS                  
         SPACE 3                                                                
         DS    0D                                                               
BUFFER   DC    2000X'00'                                                        
         EJECT                                                                  
***********************************************************************         
*                    CODE AND NAME SAVE AREAS                         *         
***********************************************************************         
         SPACE 1                                                                
SDOF     DC    CL(L'PCHKDOF)' '    OFFICE CODE                                  
SDEP     DC    CL(L'PCHKDEP)' '    DEPT CODE                                    
SSUB     DC    CL(L'PCHKSUB)' '    SUB-DEPT CODE                                
SSTF     DC    CL(L'PCHKSTF)' '    STAFF CODE                                   
STSK     DC    CL(L'PCHKTSK)' '    TASK CODE                                    
SOFF     DC    CL(L'PCHKOFF)' '    OFFICE CODE                                  
SCLI     DC    CL(L'PCHKCLI)' '    CLIENT CODE                                  
SPRO     DC    CL(L'PCHKPRO)' '    PRODUCT CODE                                 
SCOMNAME DS    CL38                COMPANY NAME                                 
SDOFNAME DS    CL38                OFFICE NAME                                  
SDEPNAME DS    CL38                DEPT NAME                                    
SSUBNAME DS    CL38                SUB-DEPT NAME                                
SSTFNAME DS    CL38                STAFF NAME                                   
STSKNAME DS    CL38                TASK NAME                                    
SOFFNAME DS    CL38                OFFICE NAME                                  
SCLINAME DS    CL38                CLIENT NAME                                  
SPRONAME DS    CL38                PRODUCT NAME                                 
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
DWNBUF   DS    CL250                                                            
**********************************************************************          
HOOK     NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC           RESTORE REG C                                
         L     R3,ADBOX                                                         
         USING BOXD,R3                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+21,C'C'                                                  
         MVI   BOXCOLS+42,C'C'                                                  
         MVI   BOXCOLS+65,C'C'                                                  
         MVI   BOXCOLS+87,C'C'                                                  
         MVI   BOXCOLS+88,C'C'                                                  
         MVI   BOXCOLS+97,C'C'                                                  
         MVI   BOXCOLS+107,C'C'                                                 
         MVI   BOXCOLS+108,C'C'                                                 
         MVI   BOXCOLS+117,C'C'                                                 
         MVI   BOXCOLS+127,C'C'                                                 
         MVI   BOXCOLS+128,C'R'                                                 
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
SAVERC   DC    A(0)                                                             
         EJECT                                                                  
******************************************************************              
*        EXTERNAL ADDRESS LIST                                   *              
******************************************************************              
*                                                                               
ADCONS   DS    0F                                                               
         DC    V(DLFLD)                                                         
         DC    A(DWNL)             DOWNLOAD                                     
         DC    A(DWNRTE)           DOWNLOAD ROUTINE                             
         DC    A(DWNBUF)           DOWNLOAD BUFFER                              
         EJECT ,                                                                
******************************************************************              
*        WORKING STORAGE                                         *              
******************************************************************              
ACC8D    DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
DLFLD    DS    V                   DOWNLOAD MODULE                              
ADWNL    DS    A                   DOWNLOAD                                     
ADWNRTE  DS    A                   DOWNLOAD ROUTINE                             
ADWNBUF  DS    A                   DOWNLOAD BUFFER                              
VTYPLNQ  EQU   *-VTYPES                                                         
ADBOX    DS    A                                                                
ELCODE   DS    CL1                                                              
SAVEKEY  DS    CL42                                                             
SAVERE   DS    F                                                                
PAGEWDTH DS    CL1                                                              
PEND     DS    CL3                                                              
PSTART   DS    CL3                                                              
TERMED   DS    C                                                                
MYKEY    DS    CL42                                                             
LCKSW    DS    X                   LOCK SWITCH                                  
LCKOF    EQU   X'80'               OFFICE IS LOCKED                             
LCKDP    EQU   X'40'               DEPARTMENT IS LOCKED                         
LCKSD    EQU   X'20'               SUB-DEPT.T IS LOCKED                         
ALL      EQU   X'FF'                                                            
DWNFLD   DS    CL40                SAVED AREA FOR FIELD TO BE D-LOADED          
PRTSIZE  DS    XL1                 PRINT AREA LENGTH                            
DWNSTAT  DS    XL1                 DOWNLOAD STATUS                              
DWNINTZ  EQU   X'80'               DOWNLOAD INITIALIZED                         
DWNHDLN  EQU   X'40'               DOWNLOAD HEADLINES                           
*                                                                               
DWNMODE  DS    XL1                 DOWNLOAD MODE                                
DWNINIT  EQU   1                   DOWN-LOAD INITIALIZATION                     
DWNEOL   EQU   2                   MARK END OF LINE                             
DWNEOR   EQU   3                   MARK END OF REPORT                           
DWNTEXT  EQU   4                   DOWN-LOAD TEXT                               
DWNNUM   EQU   5                   DOWN-LOAD NUMBER                             
DWNPACK  EQU   6                   DOWN-LOAD NUMBER (PACKED)                    
DWNREC   EQU   7                   DOWN-LOAD EXTENDED FIELD                     
SVPRNT   DS    CL(L'P)             USED FOR DOWNLOAD                            
SVPRNT2  DS    CL(L'P)             USED FOR DOWNLOAD                            
SVPRNT3  DS    CL(L'P)             USED FOR DOWNLOAD                            
SVPRNT4  DS    CL(L'P)             USED FOR DOWNLOAD                            
ACC8DEND DS    0D                                                               
         EJECT                                                                  
LINED    DSECT                                                                  
         DS    CL89                                                             
LINEDTE  DS    CL8                                                              
         DS    CL3                                                              
LINERTE  DS    CL7                                                              
         DS    CL2                                                              
LINELNG  EQU   *-LINEDTE                                                        
         EJECT                                                                  
SORTD    DSECT                                                                  
SORTLEN  DS    F                                                                
SORTPRE  DS    CL1                                                              
SORTDOF  DS    CL(L'PCHKDOF)                                                    
SORTDEP  DS    CL(L'PCHKDEP)                                                    
SORTSUB  DS    CL(L'PCHKSUB)                                                    
SORTSTF  DS    CL(L'PCHKSTF)                                                    
SORTTSK  DS    CL(L'PCHKTSK)                                                    
SORTOFF  DS    CL(L'PCHKOFF)                                                    
SORTCLI  DS    CL(L'PCHKCLI)                                                    
SORTPRO  DS    CL(L'PCHKPRO)                                                    
SORTRAT  DS    PL4                                                              
SORTDAT  DS    PL3                                                              
SORTLNG  EQU   *-SORTRAT                                                        
         EJECT                                                                  
PRINT0   DSECT                                                                  
         DS    CL1                                                              
TASK     DS    0CL(TLNQ)                                                        
TCODE0   DS    CL(L'PCHKTSK)                                                    
         DS    CL2                                                              
TNAME0   DS    CL15                                                             
TLNQ     EQU   *-TCODE0                                                         
         DS    CL3                                                              
OFFICE   DS    0CL(OLNQ)                                                        
OCODE0   DS    CL(L'PCHKOFF)                                                    
         DS    CL1                                                              
ONAME0   DS    CL15                                                             
OLNQ     EQU   *-OCODE0                                                         
         DS    CL3                                                              
CLIENT   DS    0CL(CLNQ)                                                        
CCODE0   DS    CL(L'PCHKCLI)                                                    
         DS    CL2                                                              
CNAME0   DS    CL15                                                             
CLNQ     EQU   *-CCODE0                                                         
         DS    CL3                                                              
PRODUCT  DS    0CL(PLNQ)                                                        
PCODE0   DS    CL(L'PCHKPRO)                                                    
         DS    CL2                                                              
PNAME0   DS    CL15                                                             
PLNQ     EQU   *-PCODE0                                                         
         DS    CL45                                                             
         EJECT                                                                  
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
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
* DDDLCB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPC802 02/28/01'                                      
         END                                                                    
