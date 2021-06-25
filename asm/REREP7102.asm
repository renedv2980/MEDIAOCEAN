*          DATA SET REREP7102  AT LEVEL 030 AS OF 11/05/98                      
*PHASE RE7102A,*                                                                
         TITLE 'REREP7102 - RE7102 - OWNER PRINTING MODULE'                     
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP7102 --- OWNER PRINTING MODULE                        *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 06JAN94 (SKU) --- INITIAL ENTRY                                   *           
*                                                                   *           
* 25MAR94 (SKU) --- SAVE OFF TSAR BLOCK IN BETWEEN MODES            *           
*                                                                   *           
* 07DEC95 (WSB) --- CAN SORT BY OWNER CODE, OWNER NAME, OR BOTH     *           
*                                                                   *           
* DEC14/95 (BG ) 016 CHANGE REGENALL TO REGENALL1 2K CON            *           
*                                                                   *           
* JAN28/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
*                   *** END TOMBSTONE ***                           *           
*HERE****************************************************************           
RE7102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 STOREX-STORED,**RE7102,R8,RR=R5                                  
         USING STORED,RC                                                        
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     R9,FILEC                                                         
         USING FILED,R9                                                         
                                                                                
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REPFRST                                                     
         BE    REPF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
                                                                                
         B     EXIT                                                             
                                                                                
NO       LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XMOD1                                                                  
         EJECT                                                                  
**********************************************************************          
* RUNFRST - INIT                                                                
**********************************************************************          
RUNF     DS    0H                                                               
         MVC   SVREP,RCREPFL                                                    
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQFRST - INIT TSAR AND READ OWNER RECORDS                                    
* WE ARE READING OWNER RECORDS HERE INCASE OF AN INTEREP REQUEST                
* THIS WILL PREVENT REDUNDANT READING OF OWNER RECORDS FOR SUB REPS             
**********************************************************************          
REQF     DS    0H                                                               
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         BAS   RE,TSARINIT                                                      
                                                                                
         LA    R6,KEY              PROCESS OWNER RECORDS                        
         USING ROWNREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   ROWNKTYP,X'2A'                                                   
         MVC   ROWNKREP,QREP                                                    
         DROP  R6                                                               
                                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,KEY,0                         
                                                                                
REQF10   DS    0H                                                               
         CLC   KEY(24),KEYSAVE                                                  
         BNE   REQF20                                                           
                                                                                
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
                                                                                
         LA    R6,IOAREA                                                        
         USING ROWNREC,R6                                                       
         XC    BUFFREC,BUFFREC                                                  
         LA    R5,BUFFREC                                                       
         USING BLDRECD,R5                                                       
         MVI   BLDTYPE,1           OWNER RECORD                                 
                                                                                
         CLI   QOPTION2,C'N'       SORT ONLY BY NAME?                           
         BE    REQF13                                                           
                                                                                
         MVI   BLDSRTYP,1          KEY SORTED BY OWNER CODE                     
         MVC   BLDOWNCD,ROWNKOWN                                                
         MVC   BLDNAME,ROWNNAME                                                 
         BAS   RE,TSARADD                                                       
                                                                                
REQF13   CLI   QOPTION2,C'O'       SORT ONLY BY OWNER CODE?                     
         BE    REQF17                                                           
                                                                                
         MVI   BLDSRTYP,2          KEY SORTED BY NAME                           
         MVC   BLDNOWNC,ROWNKOWN                                                
         MVC   BLDNNAME,ROWNNAME                                                
         BAS   RE,TSARADD                                                       
         DROP  R5,R6                                                            
                                                                                
REQF17   MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEYSAVE,KEY,0                         
         B     REQF10                                                           
                                                                                
REQF20   DS    0H                                                               
         CLI   QOPTION1,C'B'       OR BOTH?                                     
         BE    REQF30                                                           
         CLI   QOPTION1,C'O'       WANT OWNER LISTING?                          
         BNE   REQFX                                                            
                                                                                
REQF30   DS    0H                                                               
         BAS   RE,PRINTOWN         YES                                          
                                                                                
REQFX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REPFRST - GET STATION RECORDS                                                 
**********************************************************************          
REPF     DS    0H                                                               
         CLI   QOPTION1,C'S'       WANT STATION LISTING?                        
         BE    REPF50                                                           
         CLI   QOPTION1,C'B'       OR BOTH?                                     
         BNE   REPFX                                                            
                                                                                
REPF50   DS    0H                  PROCESS STATION RECORDS                      
*                                                                               
*   TEST CYCLE                                                                  
*        MVC   P+1(06),=C'CYCLE '                                               
*        MVC   P+7(2),QREP                                                      
*        GOTO1 REPORT                                                           
*   TEST CYCLE END                                                              
*                                                                               
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
         LA    R6,KEY                                                           
         USING RSTAREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,QREP                                                    
         DROP  R6                                                               
                                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,KEY,0                         
                                                                                
REPF60   DS    0H                                                               
         CLC   KEY(22),KEYSAVE                                                  
         BNE   REPFX                                                            
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
                                                                                
         LA    R6,IOAREA                                                        
         USING RSTAREC,R6                                                       
*                                                                               
*   TEST CYCLE                                                                  
*        MVC   P+1(06),=C'STAREC'                                               
*        MVC   P+8(27),RSTAREC                                                  
*        GOTO1 REPORT                                                           
*   TEST CYCLE END                                                              
*                                                                               
                                                                                
         OC    RSTAOWN,RSTAOWN     IF NO OWNER, SKIP THIS RECORD                
         BZ    REPF70                                                           
                                                                                
         MVC   KEYSAVE2,KEY        SAVE KEY FOR STATION RECORD                  
         LA    R4,KEY              PROCESS OWNER RECORDS                        
         USING ROWNREC,R4                                                       
         XC    KEY,KEY                                                          
         MVI   ROWNKTYP,X'2A'                                                   
         MVC   ROWNKREP,QREP                                                    
         MVC   ROWNKOWN,RSTAOWN                                                 
         DROP  R4,R6                                                            
                                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,KEY,0                         
                                                                                
         DS    0H                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    REPF62                                                           
         MVC   OWNNAME,=C'OWNER NOT ON FILE  '                                  
         B     REPF64                                                           
REPF62   EQU   *                                                                
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
                                                                                
         LA    R4,IOAREA                                                        
         USING ROWNREC,R4                                                       
                                                                                
         XC    OWNNAME,OWNNAME                                                  
         MVC   OWNNAME,ROWNNAME    GET THE OWNER NAME FROM OWNER RECORD         
         DROP  R4                                                               
                                                                                
REPF64   EQU   *                                                                
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE2,KEY,0                        
* READ BACK STATION RECORD                                                      
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
                                                                                
         LA    R6,IOAREA                                                        
         USING RSTAREC,R6                                                       
                                                                                
         XC    BUFFREC,BUFFREC                                                  
         LA    R5,BUFFREC                                                       
         USING BLDRECD,R5                                                       
         MVI   BLDTYPE,2           STATION RECORD                               
         MVC   BLDSTAT,RSTAKSTA    STATION                                      
         MVC   BLDMKT,RSTAMKT      MARKET                                       
                                                                                
         CLI   QOPTION2,C'N'       SORT ONLY BY NAME?                           
         BE    REPF65                                                           
                                                                                
         MVI   BLDSRTYP,1          SORTING BY OWNER CODE                        
         MVC   BLDOWNCD,RSTAOWN                                                 
         MVC   BLDNAME,OWNNAME                                                  
         BAS   RE,TSARADD                                                       
                                                                                
REPF65   CLI   QOPTION2,C'O'       SORT ONLY BY OWNER CODE?                     
         BE    REPF70                                                           
                                                                                
         MVI   BLDSRTYP,2          SORTING BY NAME                              
         MVC   BLDNOWNC,RSTAOWN                                                 
         MVC   BLDNNAME,OWNNAME                                                 
         BAS   RE,TSARADD                                                       
                                                                                
         DROP  R5,R6                                                            
                                                                                
REPF70   DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEYSAVE,KEY,0                         
         B     REPF60                                                           
                                                                                
REPFX    DS    0H                                                               
*                                                                               
*   TEST CYCLE                                                                  
*        MVC   P+1(06),=C'END   '                                               
*        MVC   P+7(2),QREP                                                      
*        GOTO1 REPORT                                                           
*   TEST CYCLE END                                                              
*                                                                               
         B     EXIT                                                             
       ++INCLUDE REREPTSAR                                                      
         EJECT                                                                  
**********************************************************************          
* PRINT THE STATION REPORT IF REQUESTED                                         
**********************************************************************          
REQL     DS    0H                                                               
         CLI   QOPTION1,C'S'       WANT STATION LISITING?                       
         BE    REQL50                                                           
         CLI   QOPTION1,C'B'       OR BOTH?                                     
         BNE   REQLX                                                            
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
                                                                                
REQL50   DS    0H                                                               
         BAS   RE,PRINTSTA                                                      
                                                                                
REQLX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET TSAR INITIALIZED                                                          
**********************************************************************          
TSARINIT NTR1                                                                   
         L     R1,ATSARDWA         A(TSAR CONTROL BLOCK AREA)                   
         USING TSARD,R1                                                         
         MVC   TSABUF,AAGGREG      USE AGGREG BUFFER                            
         MVC   TSAREC,=A(LENAGG)                                                
         LA    R0,30               KEY LENGTH=SORT TYPE+OWNER CODE+             
*                                             OWNER NAME+TYPE+STATION           
         STC   R0,TSKEYL                                                        
         LA    R0,BLDRECX-BLDRECD  SET RECORD LENGTH                            
         STH   R0,TSRECL                                                        
         MVI   TSOFFACT,TSAINI     INITIALIZE BUFFER                            
         GOTO1 ATSAROFF                                                         
         MVC   SVTSRBLK,0(R1)      SAVE THE TSR BLOCK                           
         DROP  R1                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ADD A RECORD TO THE TSAR BUFFER                                               
**********************************************************************          
TSARADD  NTR1                                                                   
*                                                                               
*   TEST OUTPUT                                                                 
*        MVC   P+1(08),=C'BUFFREC='                                             
*        MVC   P+9(50),BUFFREC                                                  
*        GOTO1 REPORT                                                           
*   TEST OUTPUT END                                                             
*                                                                               
         L     R1,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R1                                                         
         MVC   0(TSARDL,R1),SVTSRBLK                                            
         LA    R0,BUFFREC                                                       
         ST    R0,TSAREC           SET ADDRESS OF BUFFER RECORD                 
         MVI   TSOFFACT,TSAADD     SET ACTION TO 'ADD'                          
         GOTO1 ATSAROFF            BRANCH TO TSAROFF                            
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* GET TSAR OWNER RECORDS AND PRINT THEM                                         
**********************************************************************          
PRINTOWN NTR1                                                                   
         MVI   RCSUBPRG,0          USE OWNER-CODE HEADER                        
                                                                                
         L     R1,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R1                                                         
         MVC   0(TSARDL,R1),SVTSRBLK                                            
         XC    BUFFREC,BUFFREC                                                  
         LA    R0,BUFFREC                                                       
         ST    R0,TSAREC           SET ADDRESS OF BUFFER RECORD                 
         MVI   TSOFFACT,TSARDH     READ HI                                      
         GOTO1 ATSAROFF                                                         
         TM    TSERRS,TSEEOF       IF NO DATA, PRINT MESSAGE AND EXIT           
         BZ    POWN20                                                           
         MVC   P(24),=C'*** NO RECORDS FOUND ***'                               
         GOTO1 REPORT                                                           
         B     POWNX                                                            
                                                                                
POWN10   DS    0H                  GET NEXT RECORD                              
         GOTO1 ATSAROFF                                                         
         TM    TSERRS,TSEEOF       IF NO MORE DATA, EXIT                        
         BO    POWNX                                                            
                                                                                
         LA    R6,BUFFREC                                                       
         USING BLDRECD,R6                                                       
         CLC   LSTRCSRT,BLDSRTYP   NEW SORT TYPE?                               
         BE    POWN20              NO                                           
                                                                                
         DROP  R6                                                               
         MVI   FORCEHED,C'Y'       YES, NEW PAGE                                
                                                                                
POWN20   DS    0H                  PRINT 2 COLUMNS OF OWNER LISTING             
         LA    R6,BUFFREC                                                       
         USING BLDRECD,R6                                                       
         CLI   BLDSRTYP,1          SORTED BY OWNER CODE?                        
         BNE   POWN22              NO                                           
                                                                                
         MVI   RCSUBPRG,0          USE OWNER-CODE HEADER                        
         MVC   P+1(L'BLDOWNCD),BLDOWNCD   SORTED BY OWNER CODE                  
         MVC   P+10(L'BLDNAME),BLDNAME                                          
         B     POWN24                                                           
                                                                                
POWN22   MVI   RCSUBPRG,2          USE OWNER-NAME HEADER                        
         MVC   P+1(L'BLDNOWNC),BLDNOWNC   SORTED BY NAME                        
         MVC   P+10(L'BLDNNAME),BLDNNAME                                        
                                                                                
POWN24   MVC   LSTRCSRT,BLDSRTYP   SAVE WHICH SORT TYPE THIS IS                 
                                                                                
         MVI   TSOFFACT,TSANXT     GET NEXT RECORD                              
         GOTO1 ATSAROFF                                                         
         TM    TSERRS,TSEEOF       IF NO MORE DATA, SKIP FORMATTING             
         BO    POWN30                                                           
         CLC   LSTRCSRT,BLDSRTYP   DIFF SORT TYPE?                              
         BNE   POWN30              YES, GO TO NEXT LINE                         
                                                                                
         CLI   BLDSRTYP,1          SORTED BY OWNER CODE?                        
         BNE   POWN26                                                           
                                                                                
         MVC   P+39(L'BLDOWNCD),BLDOWNCD    OWNER CODE                          
         MVC   P+48(L'BLDNAME),BLDNAME                                          
         B     POWN28                                                           
                                                                                
POWN26   MVC   P+39(L'BLDNOWNC),BLDNOWNC    NAME                                
         MVC   P+48(L'BLDNNAME),BLDNNAME                                        
                                                                                
POWN28   MVC   LSTRCSRT,BLDSRTYP   SAVE SORT TYPE                               
                                                                                
POWN30   DS    0H                                                               
         GOTO1 REPORT              DOUBLE-SPACED REPORT                         
         GOTO1 REPORT                                                           
         TM    TSERRS,TSEEOF       IF NO MORE DATA, EXIT                        
         BO    POWNX                                                            
         CLC   LSTRCSRT,BLDSRTYP   DIFF SORT TYPE?                              
         BE    POWN10              NO                                           
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
         B     POWN20                                                           
                                                                                
POWNX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R1,R6                                                            
         EJECT                                                                  
**********************************************************************          
* GET TSAR STATION RECORDS AND PRINT THEM                                       
**********************************************************************          
PRINTSTA NTR1                                                                   
         MVI   RCSUBPRG,1          USE STATION-CODE HEADER                      
         XC    OWNFLAG,OWNFLAG     STATUS FLAG                                  
         XC    OWNCODE,OWNCODE                                                  
         XC    OWNNAME,OWNNAME                                                  
                                                                                
         L     R1,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R1                                                         
         MVC   0(TSARDL,R1),SVTSRBLK                                            
         XC    BUFFREC,BUFFREC                                                  
         LA    R0,BUFFREC                                                       
         ST    R0,TSAREC           SET ADDRESS OF BUFFER RECORD                 
         MVI   TSOFFACT,TSARDH     READ HI                                      
         GOTO1 ATSAROFF                                                         
         TM    TSERRS,TSEEOF       IF NO DATA, PRINT MESSAGE AND EXIT           
         BZ    PSTA20                                                           
         MVC   P+1(24),=C'*** NO RECORDS FOUND ***'                             
         GOTO1 REPORT                                                           
         B     PSTAX                                                            
                                                                                
PSTA10   DS    0H                  GET NEXT RECORD                              
         GOTO1 ATSAROFF                                                         
         TM    TSERRS,TSEEOF       IF NO MORE DATA, EXIT                        
         BO    PSTAX                                                            
                                                                                
         LA    R6,BUFFREC                                                       
         USING BLDRECD,R6                                                       
         CLC   LSTRCSRT,BLDSRTYP   DIFF SORT TYPE?                              
         BE    PSTA20              NO                                           
                                                                                
         DROP  R6                                                               
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
                                                                                
PSTA20   DS    0H                                                               
         LA    R6,BUFFREC                                                       
         USING BLDRECD,R6                                                       
         CLI   BLDTYPE,1           SEE IF AN OWNER RECORD EXISTS FOR            
         BNE   PSTA30              THE NEXT NON-OWNER RECORD                    
                                                                                
         CLI   BLDSRTYP,1          SORTED BY OWNER CODE?                        
         BNE   PSTA26              NO                                           
                                                                                
         MVI   RCSUBPRG,1          USE STATION-CODE HEADER                      
         MVC   OWNCODE,BLDOWNCD    OWNER CODE                                   
         MVC   OWNNAME,BLDNAME                                                  
         B     PSTA28                                                           
                                                                                
PSTA26   MVI   RCSUBPRG,3          USE STATION-NAME HEADER                      
         MVC   OWNCODE,BLDNOWNC    NAME                                         
         MVC   OWNNAME,BLDNNAME                                                 
                                                                                
PSTA28   MVC   LSTRCSRT,BLDSRTYP   SAVE THIS SORT TYPE                          
         NI    OWNFLAG,X'FF'-OWNGRPQ    A NEW OWNER RECORD WAS READ             
         MVI   TSOFFACT,TSANXT     GET NEXT RECORD                              
         B     PSTA10                                                           
                                                                                
PSTA30   DS    0H                  MUST BE A STATION RECORD                     
         TM    OWNFLAG,OWNGRPQ                                                  
         BO    PSTA50                                                           
                                                                                
         GOTO1 REPORT              DOUBLE-SPACED REPORT                         
                                                                                
PSTA50   DS    0H                                                               
         MVC   P+38(4),BLDSTAT                                                  
         MVC   P+42(3),=C'-TV'     BAND                                         
         CLI   BLDSTAT+4,C'A'                                                   
         BNE   PSTA60                                                           
         MVC   P+43(2),=C'AM'                                                   
         B     PSTA80                                                           
                                                                                
PSTA60   DS    0H                                                               
         CLI   BLDSTAT+4,C'F'                                                   
         BNE   PSTA70                                                           
         MVC   P+43(2),=C'FM'                                                   
         B     PSTA80                                                           
                                                                                
PSTA70   DS    0H                                                               
         CLI   BLDSTAT+4,C'C'                                                   
         BNE   PSTA80                                                           
         MVC   P+43(2),=C'CM'                                                   
                                                                                
PSTA80   DS    0H                                                               
         MVC   P+51(L'BLDMKT),BLDMKT                                            
                                                                                
         CLI   BLDSRTYP,1          SORTED BY OWNER CODE?                        
         BNE   PSTA86              NO                                           
                                                                                
         CLC   OWNCODE,BLDOWNCD    IS OWNER CODE CONSISTENT WITH OWNER?         
         BE    PSTA90              YES                                          
         MVC   P+1(L'BLDOWNCD),BLDOWNCD                                         
         B     PSTA88                                                           
                                                                                
PSTA86   CLC   OWNCODE,BLDNOWNC    IS OWNER CODE CONSISTENT?                    
         BE    PSTA90              YES                                          
         MVC   P+1(L'BLDOWNCD),BLDOWNCD                                         
                                                                                
PSTA88   MVC   P+12(19),=C'*OWNER REC MISSING*'                                 
         B     PSTA100                                                          
                                                                                
PSTA90   DS    0H                  PRINT CODE/NAME IN 1ST ENTRY OF GRP          
         TM    OWNFLAG,OWNGRPQ                                                  
         BO    PSTA95                                                           
         MVC   P+1(L'OWNCODE),OWNCODE                                           
         MVC   P+12(L'OWNNAME),OWNNAME                                          
         OI    OWNFLAG,OWNGRPQ                                                  
         B     PSTA100                                                          
                                                                                
PSTA95   DS    0H                  IF STILL PRINTING STATIONS                   
         CLC   LINE,MAXLINES       FROM PREVIOUS PAGE, PRINT THE                
         BL    PSTA100             MESSAGE THAT WE'RE CONTINUING                
         MVC   P+1(L'OWNCODE),OWNCODE                                           
         MVC   P+5(6),=C'(CONT)'                                                
         MVC   P+12(L'OWNNAME),OWNNAME                                          
                                                                                
PSTA100  DS    0H                                                               
         GOTO1 REPORT                                                           
                                                                                
         MVI   TSOFFACT,TSANXT     GET NEXT RECORD                              
         B     PSTA10                                                           
                                                                                
PSTAX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R1,R6                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
* FOR INTEREP REQUESTS, PRINT INTEREP ON HEADER                                 
***********************************************************************         
HOOK     NTR1                                                                   
         CLC   =C'IR',SVREP                                                     
         BNE   HOOKX                                                            
         MVC   HEAD1+16(20),SPACES                                              
         MVC   HEAD1+16(7),=C'INTEREP'                                          
HOOKX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* STORAGE AREAS                                                                 
*                                                                               
FRSTLINE EQU   10                  FIRST PRINT LINE NUMBER                      
RELO     DS    A                                                                
TEMP     DS    6F                                                               
BUFFREC  DS    CL(BLDRECX-BLDRECD)                                              
OWNCODE  DS    CL(L'BLDOWNCD)                                                   
OWNNAME  DS    CL(L'BLDNAME)                                                    
LASTSTOW DS    CL(L'BLDOWNCD)      LAST STATION RECORD'S OWNER CODE             
SVREP    DS    CL(L'QREP)                                                       
LSTRCSRT DS    X                   SORT TYPE FOR LAST RECORD                    
KEYSAVE2 DS    CL32                                                             
OWNFLAG  DS    X                                                                
OWNGRPQ  EQU   X'20'               STATION REC IS PART OF A GROUP               
*                                    DON'T HAVE TO PRT OWNER CODE               
SVTSRBLK DS    CL(TSARDL)                                                       
*                                                                               
         DS    0D                                                               
IOAREA   DS    CL1000                                                           
         EJECT                                                                  
STORED   DSECT                                                                  
ELCODE   DS    C                                                                
STOREX   EQU   *                                                                
*                                                                               
BLDRECD  DSECT                                                                  
BLDSRTYP DS    X                   SORT TYPE--1=BY CODE, 2=BY NAME              
*                                                                               
BLDOWNCD DS    CL3                 OWNER CODE (IN SORT BY CODE)                 
BLDNAME  DS    CL20                OWNER NAME (IN SORT BY CODE)                 
         ORG   BLDOWNCD                                                         
BLDNNAME DS    CL20                OWNER NAME (IN SORT BY NAME)                 
BLDNOWNC DS    CL3                 OWNER CODE (IN SORT BY NAME)                 
*                                                                               
BLDTYPE  DS    X                   1=OWNER, 2=STATION                           
BLDSTAT  DS    CL5                 STATION WITH THIS OWNER                      
BLDMKT   DS    CL20                STATION MARKET                               
BLDRECX  EQU   *                                                                
*              FILE CONTROL AND WORKD DSECTS                                    
         PRINT OFF                                                              
       ++INCLUDE REREPRGEQA                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE REGENREQ2                                                      
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030REREP7102 11/05/98'                                      
         END                                                                    
