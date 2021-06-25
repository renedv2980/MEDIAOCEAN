*          DATA SET ACPRO15    AT LEVEL 010 AS OF 07/10/20                      
*PHASE T60B15A                                                                  
         TITLE 'T60B15 - OPTIONS SUMMARY REPORT'                                
* GHOA 14FEB18 09 DSRD-18081 NEW OPT/MAIN 'EMAIL IF PERSN ASSIGN 2 JOB'         
* ASAX 26MAY20 10 DSRD-25987 NEW OPT/MAIN 'DATE EST IN LAST FISCAL YR'          
                                                                                
T60B15   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B15**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         ST    R2,RELO                                                          
         SPACE 1                                                                
*                                                                               
OPT1     CLI   MODE,VALKEY                                                      
         BNE   OPT10                                                            
*                                                                               
         LA    RE,LOCAL                                                         
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR LOCAL WORKING STORAGE                  
*                                                                               
         BAS   RE,VALHED                                                        
         B     OPTX                                                             
         SPACE 1                                                                
* PRINTREP CODE                                                                 
*                                                                               
OPT10    CLI   MODE,PRINTREP       TEST TO PRINT REPORT                         
         BNE   OPTX                                                             
*                                                                               
         LA    R1,BUFF             USE BUFF FOR RECORD AREAS                    
         LA    R0,5                                                             
         LA    RE,ACOMP                                                         
*                                                                               
OPT12    ST    R1,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,1000(R1)                                                      
         BCT   R0,OPT12                                                         
*                                                                               
         L     R0,OPTBUFFL                                                      
         ST    R0,TOTBUFFL                                                      
         GETMAIN R,LV=(0)                                                       
         ST    R1,AOPTBUFF                                                      
         MVC   LOPTBUFF,OPTBUFFL                                                
*                                                                               
OPT14    L     R1,=A(BUFFALOC)                                                  
         A     R1,RELO                                                          
         ST    R1,ABUFF                                                         
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         GOTO1 VBLDMED,DMCB,(1,MEDBUFF)                                         
         GOTO1 VBLDOFF,DMCB,OFFBUFF                                             
         BAS   RE,RDF              READ THE FILE                                
         BAS   RE,SORT             BUILD THE SORT FILE                          
         BAS   RE,PRINT            PRINT THE REPORT                             
*                                                                               
OPT16    L     R1,AOPTBUFF                                                      
         L     R0,TOTBUFFL                                                      
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
OPTX     XMOD1 1                                                                
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE HEADLINE FIELDS                                       
*                                                                               
VALHED   NTR1                                                                   
         MVI   OPTION,C'Y'         DISPLAY NAMES FROM OGROUP TO JOB             
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   EXLEN,2                                                          
*                                                                               
* VALIDATE OFFICE GROUP                                                         
*                                                                               
VALHED1  LA    R2,PROOGRH         OFFICE GROUP                                  
         CLI   5(R2),0                                                          
         BE    VALHED2                                                          
*                                                                               
         CLC   =C'ALL',8(R2)       TEST FOR 'ALL'                               
         BE    VALHED2                                                          
*                                                                               
         CLI   5(R2),1                                                          
         BNE   INVEND                                                           
         GOTO1 VALOG                                                            
         MVC   QOG,EFFOFG                                                       
         SPACE 1                                                                
* VALIDATE OFFICE                                                               
*                                                                               
VALHED2  LA    R2,PROOFFH          OFFICE                                       
         CLI   5(R2),0                                                          
         BE    VALHED4                                                          
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALOFF                                                           
         MVC   QOFF,EFFOFFC                                                     
         SPACE 1                                                                
* VALIDATE CLIENT                                                               
*                                                                               
VALHED4  LA    R2,PROCLIH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VALHED6                                                          
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   PROOFFH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALCLI                                                           
         MVC   QCLI,CLICODE                                                     
         ZIC   R1,EXLEN                                                         
         ZIC   R0,LCLI                                                          
         AR    R1,R0                                                            
         STC   R1,EXLEN                                                         
         SPACE 1                                                                
* VALIDATE PRODUCT                                                              
*                                                                               
VALHED6  LA    R2,PROPROH          PRODUCT                                      
         CLI   5(R2),0                                                          
         BE    VALHED8                                                          
         MVI   ERROR,NEEDCLI       IF INPUT, NEED CLIENT AS WELL                
         CLI   PROCLIH+5,0                                                      
         BE    ERREND                                                           
         GOTO1 VALPROD                                                          
         MVC   QPROD,PRODCODE                                                   
         ZIC   R1,EXLEN                                                         
         ZIC   R0,LPRO                                                          
         AR    R1,R0                                                            
         STC   R1,EXLEN                                                         
         SPACE 1                                                                
* VALIDATE MEDIA GROUP                                                          
*                                                                               
VALHED8  LA    R2,PROMGRH                                                       
         CLI   5(R2),0                                                          
         BE    VALHED10                                                         
         GOTO1 VALMG                                                            
         MVC   QMG,MGROUP                                                       
         SPACE 1                                                                
* VALIDATE MEDIA                                                                
*                                                                               
VALHED10 LA    R2,PROMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    VALHED12                                                         
         MVI   ERROR,NOTMENMG                                                   
         CLI   PROMGRH+5,0         CANNOT HAVE BOTH MGR AND MEDIA               
         BNE   ERREND                                                           
         GOTO1 VALMED                                                           
         MVC   QMED,MEDIA                                                       
         SPACE 1                                                                
* VALIDATE CLOSED JOBS OPTION                                                   
*                                                                               
VALHED12 MVI   OPTION,0                                                         
         MVI   QCLOSE,C'N'                                                      
         LA    R2,PROCLOH                                                       
         CLI   5(R2),0                                                          
         BE    VALHED15                                                         
         CLI   8(R2),C'N'                                                       
         BE    VALHED15                                                         
         CLI   8(R2),C'Y'                                                       
         BNE   INVEND                                                           
         MVI   QCLOSE,C'Y'                                                      
         SPACE 1                                                                
VALHED15 DS    0H                                                               
         SPACE 1                                                                
VALHEDX  B     XIT                                                              
         EJECT                                                                  
************************************************************                    
* SUB-ROUTINE TO READ THE FILE                             *                    
************************************************************                    
         SPACE 1                                                                
RDF      NTR1  ,                                                                
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   KEY,SPACES                                                       
         MVC   ACKEYACC(1),CUL     READ COMPANY RECORD                          
         GOTO1 READ                                                             
*                                                                               
         GOTO1 SAVREC,ACOMP                                                     
         MVC   GOADM,DATAMGR       INITIALIZE GENERAL GOBLOCK VALUES            
         MVC   GOSELCUL,CUL                                                     
         MVC   GOABUFF,AOPTBUFF    OPTIONS BUFFER                               
         MVC   GOLBUFF,LOPTBUFF                                                 
         MVC   GOACOMP,ACOMP                                                    
         MVC   GOALEDG,ALEDG                                                    
         MVC   GOACLI,ACLI                                                      
         MVC   GOAPRO,APROD                                                     
         MVC   GOAJOB,AJOB                                                      
         MVC   GOACOVL,COVAIL                                                   
         MVC   GOABINSR,BINSRCH                                                 
         MVI   GOANYWC,C'N'        DO NOT NEED WC OPTIONS                       
*                                                                               
RDF2     MVC   ACKEYACC(3),CUL     GET THE LEDGER RECORD                        
         GOTO1 READ                                                             
         GOTO1 SAVREC,ALEDG                                                     
*                                                                               
RDF4     OC    QCLI,QCLI           TEST FOR CLIENT REQUEST                      
         BZ    RDF12               NO-GO RIGHT INTO FILE                        
*                                                                               
         MVC   ACKEYACC+3(L'QCLI),QCLI                                          
         GOTO1 READ                                                             
         BAS   RE,CLIENT                                                        
*                                                                               
RDF6     OC    QPROD,QPROD         TEST PRODUCT REQUESTED                       
         BZ    RDF12               NO-READ NEXT RECORD                          
*                                                                               
         ZIC   R1,LCLI                                                          
         LA    R1,ACKEYACC+3(R1)                                                
         MVC   0(L'QPROD,R1),QPROD                                              
         GOTO1 READ                                                             
         B     RDF20               PROCESS RETURNED RECORD                      
*                                                                               
RDF10    OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         B     RDF20                                                            
*                                                                               
RDF12    OI    DMINBTS,X'08'                                                    
         GOTO1 SEQ                                                              
*                                                                               
RDF20    ZIC   R1,EXLEN                                                         
         EX    R1,COMPKEY                                                       
         BNE   RDFX                END OF FILE                                  
*                                                                               
RDF22    TM    ACSTATUS,X'80'                                                   
         BO    RDF30                                                            
         ZIC   R1,LCLI                                                          
         LA    R1,ACKEYACC+3(R1)                                                
         CLI   0(R1),C' '          TEST FOR CLIENT RECORD                       
         BH    RDF24               NO                                           
         BAS   RE,CLIENT                                                        
         B     RDF12               GET NEXT RECORD                              
*                                                                               
RDF24    ZIC   R5,LCLIPRO                                                       
         LA    R5,ACKEYACC+3(R5)                                                
         CLI   0(R5),C' '                                                       
         BH    RDF26               ITS A JOB                                    
         BAS   RE,PRODUCT                                                       
         BE    RDF12                                                            
         MVI   0(R5),X'FF'         FORCE NEXT PRODUCT                           
         B     RDF10                                                            
*                                                                               
RDF26    BAS   RE,JOB                                                           
*                                                                               
RDF30    MVI   ACKEYWRK,X'FF'      FORCE NEXT ACCOUNT                           
         B     RDF10                                                            
*                                                                               
RDFX     B     XIT                                                              
         SPACE 2                                                                
COMPKEY  CLC   ACKEYD(0),KEYSAVE                                                
         EJECT                                                                  
****************************************************************                
* HOOK ROUTINES TO PROCESS RECORDS PASSED BY RDF               *                
****************************************************************                
         SPACE 1                                                                
CLIENT   NTR1  ,                                                                
         GOTO1 SAVREC,ACLI                                                      
         GOTO1 SETCLI                                                           
         MVI   TYPEREC,C'C'                                                     
         BAS   RE,FILOFF           PERFORM OFFICE/OFFICE GRP FILTERING          
         BNE   CLIENTX                                                          
         BAS   RE,RDOPT                                                         
         BAS   RE,POST                                                          
*                                                                               
CLIENTX  B     XIT                                                              
         SPACE 1                                                                
PRODUCT  NTR1  ,                                                                
         GOTO1 SAVREC,APROD                                                     
         GOTO1 SETPROD                                                          
         MVI   TYPEREC,C'P'                                                     
         BAS   RE,FILOFF                                                        
         BNE   NOTOKXIT                                                         
         BAS   RE,RDOPT                                                         
         BAS   RE,POST                                                          
         B     OKXIT                                                            
         SPACE 1                                                                
JOB      NTR1  ,                                                                
         GOTO1 SAVREC,AJOB                                                      
         GOTO1 SETJOB                                                           
         MVI   TYPEREC,C'J'                                                     
         BAS   RE,FILMED           FILTER ON MEDIA/MEDIA GROUP                  
         BNE   JOBX                                                             
         TM    JOBSTAT,X'40'       TEST FOR CLOSED JOB                          
         BZ    *+12                NO                                           
         CLI   QCLOSE,C'N'         TEST IF CLOSED JOBS REQUESTED                
         BE    JOBX                NO                                           
         BAS   RE,RDOPT                                                         
         BAS   RE,POST             POST TO BUFFALO                              
*                                                                               
JOBX     B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO SAVE A RECORD IN A BUFFER, AT ENTRY, R1=A(BUFF ADCON)          
*                                                                               
SAVREC   NTR1                                                                   
         L     RE,0(R1)            RE=A(BUFFER)                                 
         L     R4,AIO              R4=A(RECORD TO BE SAVED)                     
         USING ACKEYD,R4                                                        
         LH    R1,ACLENGTH                                                      
         LR    RF,R1                                                            
         LR    R0,R4                                                            
         MVCL  RE,R0               SAVE THE RECORD                              
         B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO FILTER ON MEDIA GROUP AND MEDIA                                
*                                                                               
* ON EXIT, CC=EQ IF OK, CC=NEQ TO SKIP JOB                                      
*                                                                               
FILMED   NTR1  ,                                                                
         CLI   QMG,0                                                            
         BNE   FILMED2                                                          
         CLI   QMED,0                                                           
         BE    FILMEDY                                                          
*                                                                               
FILMED1  CLC   QMED,JOBNUM                                                      
         BE    FILMEDY                                                          
         B     FILMEDN                                                          
*                                                                               
FILMED2  LA    R6,MEDBUFF                                                       
*                                                                               
FILMED3  CLI   0(R6),0             TEST FOR EOB                                 
         BE    FILMEDN                                                          
         CLC   JOBNUM(1),0(R6)                                                  
         BE    FILMED4                                                          
         LA    R6,2(R6)                                                         
         B     FILMED3                                                          
*                                                                               
FILMED4  CLC   QMG,1(R6)           TEST FOR MATCH ON MG                         
         BE    FILMEDY                                                          
         B     FILMEDN                                                          
*                                                                               
FILMEDY  B     OKXIT                                                            
*                                                                               
FILMEDN  B     NOTOKXIT                                                         
         SPACE 2                                                                
* SUB-ROUTINE TO HANDLE OFFICE AND OFFICE GROUP FILTERING                       
*                                                                               
FILOFF   NTR1  ,                                                                
         CLI   QOG,0               TEST FOR OFFICE GROUP FILTER                 
         BNE   FILOFF2             YES                                          
         OC    QOFF,QOFF           TEST FOR OFFICE FILTER                       
         BZ    FILOFFY             NO-TAKE THE RECORD                           
*                                                                               
         CLC   EFFOFFC,QOFF        MATCH ON FILTER                              
         BE    FILOFFY                                                          
         B     FILOFFN                                                          
*                                                                               
FILOFF2  LA    R2,OFFBUFF          USE MY OFFICE BUFFER                         
*                                                                               
FILOFF3  CLI   0(R2),0             TEST FOR EOT                                 
         BE    FILOFFN             NOT THE REQUESTED OFFICE GROUP               
         CLC   EFFOFFC,0(R2)       MATCH ON OFFICE                              
         BE    FILOFF4                                                          
         LA    R2,3(R2)                                                         
         B     FILOFF3                                                          
*                                                                               
FILOFF4  CLC   QOG,2(R2)           MATCH ON OFFICE GROUP                        
         BE    FILOFFY                                                          
         B     FILOFFN                                                          
*                                                                               
FILOFFY  B     OKXIT                                                            
*                                                                               
FILOFFN  B     NOTOKXIT                                                         
         SPACE 2                                                                
* SUB-ROUTINE TO READ THE OPTIONS FOR THE RECORD'S LEVEL                        
*                                                                               
RDOPT    ST    RE,SAVERE                                                        
         XC    GOSELCLI,GOSELCLI                                                
         XC    GOSELPRO,GOSELPRO                                                
         XC    GOSELJOB,GOSELJOB                                                
         MVC   GOAKEY,AIO                                                       
*                                                                               
         MVC   GOSELCLI,CLICODE    SET CLIENT CODE                              
         CLI   TYPEREC,C'C'        TEST FOR CLIENT RECORD                       
         BE    RDOPT2                                                           
*                                                                               
         MVC   GOSELPRO,PRODCODE   SET PRODUCT CODE                             
         CLI   TYPEREC,C'P'                                                     
         BE    RDOPT2                                                           
*                                                                               
         MVC   GOSELJOB,JOBNUM                                                  
*                                                                               
RDOPT2   GOTO1 GETOPT,DMCB,GOBLOCK                                              
*                                                                               
RDOPTX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO POST TO BUFFALO                                                
*                                                                               
POST     NTR1  ,                                                                
         LA    R3,OPTLIST          R3=OPTION LIST                               
         LA    R4,OPTIONS          R4=LOOP COUNTER                              
*                                                                               
POST2    XC    BUFFREC(BUFFRECL),BUFFREC                                        
         MVC   BUFFOPT,0(R3)                                                    
         BAS   RE,FINDENT                                                       
         USING OPTBD,R5                                                         
         BAS   RE,FILTLEV          FILTER ON LEVEL                              
         BNE   POST4               OPTION DOES NOT POST FOR THIS LEVEL          
         GOTO1 VDISOPT,DMCB,(BUFFOPT,GOBLOCK),BUFFVAL-8                         
         ZAP   BUFFNUM,=P'1'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC                               
*                                                                               
POST4    LA    R3,1(R3)                                                         
         BCT   R4,POST2                                                         
*                                                                               
POSTX    B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO FILTER ON WHETHER OPTION SHOULD POST ON THIS LEVEL             
* ON EXIT, CC=EQ IF YES, CC=NEQ IF NO                                           
*                                                                               
FILTLEV  MVI   BYTE,C'J'           DEFAULT LEVEL IS JOB                         
         TM    OPTBPROT,OPTBCLI+OPTBPRO+OPTBJOB  TEST NO PROTECTION             
         BZ    FILTLEV2            YES                                          
*                                                                               
         TM    OPTBPROT,OPTBJOB    TEST UNPROTECTED AT JOB                      
         BZ    FILTLEV2                                                         
*                                                                               
         MVI   BYTE,C'P'                                                        
         TM    OPTBPROT,OPTBPRO                                                 
         BZ    FILTLEV2                                                         
*                                                                               
         MVI   BYTE,C'C'                                                        
*                                                                               
FILTLEV2 CLC   TYPEREC,BYTE        TEST FOR MATCH ON LEVEL                      
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BUILD THE SORT FILE FOR PRINTING                               
*                                                                               
SORT     NTR1  ,                                                                
         GOTO1 SORTER,DMCB,SORTFLD,RECTYPE,0                                    
         XC    BUFFREC(BUFFRECL),BUFFREC                                        
         LA    RE,OPTLIST                                                       
         MVC   BUFFOPT,0(RE)       GET LOWEST OPTION NUMBER                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFFREC,1                            
         CLI   DMCB+8,X'80'        TEST FOR EOF                                 
         BE    SORTX                                                            
         B     SORT4                                                            
*                                                                               
SORT2    GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,BUFFREC,1                             
         CLI   DMCB+8,X'80'        TEST FOR EOF                                 
         BE    SORTX                                                            
*                                                                               
SORT4    LA    R1,BUFFRECL+4                                                    
         STH   R1,BUFFRLEN         BUILD SORT HEADER                            
         ZAP   DUB,BUFFNUM                                                      
         CVB   R0,DUB                                                           
         LCR   R0,R0               COMPLEMENT OCCURRENCES TO INSURE             
         STCM  R0,15,BUFFOCC       CORRECT SORT SEQUENCE                        
         GOTO1 SORTER,DMCB,=C'PUT',BUFFSORT                                     
         B     SORT2                                                            
*                                                                               
SORTX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO PRINT THE REPORT                                               
*                                                                               
PRINT    NTR1  ,                                                                
         LA    R2,P                                                             
         USING PRTD,R2                                                          
         MVI   FORCEHED,C'Y'                                                    
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
*                                                                               
PRINT2   GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R3,15,4(R1)                                                      
         BZ    PRINT10             EOF ON SORT FILE                             
*                                                                               
         MVC   BUFFSORT(BUFFRECL+4),0(R3)                                       
         CLC   BUFFOPT,LASTOPT     TEST FOR CHANGE IN OPTION                    
         BE    PRINT4                                                           
*                                                                               
         CLI   LASTOPT,0           TEST FOR FIRST OPTION                        
         BE    PRINT3              YES                                          
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE                                  
*                                                                               
PRINT3   MVC   LASTOPT,BUFFOPT                                                  
         BAS   RE,FINDENT                                                       
         USING OPTBD,R5                                                         
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(L'OPTBSHRT),OPTBSHRT                                      
         LA    RE,LISTAR+L'OPTBSHRT                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(L'OPTBDESC,RE),OPTBDESC                                        
         MVC   PRTDESC,LISTAR                                                   
*                                                                               
PRINT4   MVC   PRTVAL,BUFFVAL      EXTRACT OPTION VALUE                         
         EDIT  (P8,BUFFNUM),(8,PRTNUM)                                          
*                                                                               
PRINT6   GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRINT2                                                           
*                                                                               
PRINT10  GOTO1 SORTER,DMCB,=C'END'                                              
*                                                                               
PRINTX   B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO FIND OPTION TABLE ENTRY                                        
*                                                                               
FINDENT  L     R5,AOPTTAB                                                       
*                                                                               
FINDENT2 CLI   OPTBOPN,0           TEST FOR EOT                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BUFFOPT,OPTBOPN     MATCH ON OPTION NUMBER                       
         BER   RE                  YES                                          
         LA    R5,OPTBL(R5)                                                     
         B     FINDENT2                                                         
         EJECT                                                                  
* HOOK ROUTINE FOR PRINTING                                                     
*                                                                               
HOOK     NTR1  ,                                                                
*                                                                               
         ICM   R2,15,ABOX          LOOK FOR BOXES                               
         BZ    HOOKX               NONE                                         
*                                                                               
         USING BOXD,R2                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
*                                                                               
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
*                                                                               
         LA    RE,BOXCOLS                                                       
         MVI   PRTLBOX-PRTD(RE),C'L'                                            
         MVI   PRTBOX1-PRTD(RE),C'C'                                            
         MVI   PRTBOX2-PRTD(RE),C'C'                                            
         MVI   PRTRBOX-PRTD(RE),C'R'                                            
*                                                                               
HOOKX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
NOTOKXIT LTR   RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
INVEND   MVI   ERROR,INVALID       INVALID EXIT                                 
         B     ERREND                                                           
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
OPTBUFFL DC    F'20000'            LENGTH OF OPTIONS BUFFER OFFLINE             
SORTFLD  DC    CL80'SORT FIELDS=(5,28,A),FORMAT=BI,WORK=1'                      
RECTYPE  DC    CL80'RECORD TYPE=V,LENGTH=50'                                    
         SPACE 2                                                                
* OPTION LIST                                                                   
*                                                                               
OPTLIST  DS    0X                                                               
         DC    AL1(102,103,104,105,106,107,108,109,110,111)                     
         DC    AL1(112,113,114,115,116,118,120,122,123,124,143)                 
OPTIONS  EQU   (*-OPTLIST)                                                      
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* SPECS FOR OPTIONS SUMMARY REPORT                                              
*                                                                               
HEDSPECS DS    0D                                                               
         SSPEC H1,2,CREATED                                                     
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,44,C'OPTIONS SUMMARY REPORT'                                  
         SSPEC H2,44,C'----------------------'                                  
         SSPEC H1,85,AGYNAME                                                    
         SSPEC H2,85,AGYADD                                                     
         SSPEC H4,85,REPORT                                                     
         SSPEC H4,98,PAGE                                                       
*                                                                               
         SSPEC H7,3,C'OPTION DESCRIPTION'                                       
         SSPEC H7,36,C'OPTION VALUE'                                            
         SSPEC H7,58,C'FREQUENCY'                                               
*                                                                               
         DC    X'00'                                                            
         SPACE 2                                                                
         BUFF  LINES=200,ROWS=1,COLUMNS=1,FLAVOR=PACKED,KEYLIST=(28,A)          
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACPROWORKD                                                                     
*ACGENBOTH                                                                      
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN                                                            
LOCAL    DS    0C                                                               
SAVERE   DS    A                                                                
QOG      DS    CL1                                                              
QOFF     DS    CL2                                                              
QCLI     DS    CL(L'CLICODE)                                                    
QPROD    DS    CL(L'PRODCODE)                                                   
QMG      DS    CL1                                                              
QMED     DS    CL1                                                              
QCLOSE   DS    CL1                                                              
*                                                                               
EXLEN    DS    X                   KEY EXECUTE LENGTH                           
ALLSW    DS    C                   Y=ALL OG INPUT                               
TYPEREC  DS    C                   C=CLIENT,P=PRODUCT,J=JOB                     
LASTOPT  DS    X                                                                
*                                                                               
ABUFF    DS    A                   A(BUFFALO CSECT)                             
ACOMP    DS    A                                                                
ALEDG    DS    A                                                                
ACLI     DS    A                                                                
APROD    DS    A                                                                
AJOB     DS    A                                                                
*                                                                               
TOTBUFFL DS    F                                                                
AOPTBUFF DS    A                                                                
LOPTBUFF DS    A                                                                
*                                                                               
BUFFSORT DS    0H                                                               
BUFFRLEN DS    H                                                                
         DS    H                                                                
BUFFREC  DS    0X                                                               
BUFFOPT  DS    X                   OPTION NUMBER                                
BUFFOCC  DS    XL4                 N'OCCURRENCES (COMPLEMENTED)                 
BUFFVAL  DS    CL20                OPTION VALUE                                 
         DS    CL3                 SPARE                                        
BUFFDATA DS    0CL8                                                             
BUFFNUM  DS    PL8                                                              
BUFFRECL EQU   *-BUFFREC                                                        
*                                                                               
MEDBUFF  DS    XL100                                                            
OFFBUFF  DS    XL330                                                            
       ++INCLUDE ACGOBLOCK                                                      
*                                                                               
LOCALLN  EQU   *-LOCAL                                                          
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
         EJECT                                                                  
* DSECT TO COVER OPTION CONVERSION REQUEST SCREEN                               
*                                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROE5D                                                       
         SPACE  2                                                               
MOREDATA DS    0D                                                               
         DS    CL((SAVAREA-MOREDATA)-(*-MOREDATA)) SPARE                        
         SPACE 2                                                                
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PRTD     DSECT                                                                  
         DS    C                   SPARE                                        
PRTLBOX  DS    C                   LEFT BOX POSITION                            
PRTDESC  DS    CL31                OPTION DESCRIPTION                           
         DS    C                                                                
PRTBOX1  DS    C                   CENTER BOX POSITION                          
PRTVAL   DS    CL20                OPTION VALUE                                 
         DS    C                   SPARE                                        
PRTBOX2  DS    C                                                                
PRTNUM   DS    CL8                 NUMBER OF OCCURENCES                         
         DS    CL2                 SPARE                                        
PRTRBOX  DS    C                   RIGHT BOX POSITION                           
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACPRO15   07/10/20'                                      
         END                                                                    
