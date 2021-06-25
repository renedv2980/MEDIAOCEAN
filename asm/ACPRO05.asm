*          DATA SET ACPRO05    AT LEVEL 008 AS OF 07/10/20                      
*PHASE T60B05A,*                                                                
         TITLE 'T60B05 - OPTIONS CONTROL REPORT'                                
* GHOA 007 14FEB18 DSRD-18081 NEW OPT/MAIN 'MAIL IF PERSN ASSIGN 2 JOB'         
* ASAX 047 26MAY20 DSRD-25987 NEW OPT/MAIN 'DATE EST IN LAST FISCAL YR'         
                                                                                
T60B05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B05**,RA,RR=R2                                              
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
OPT2     CLI   OFFLINE,C'Y'                                                     
         BE    OPT4                                                             
*                                                                               
* VALKEY CODE WHEN ON-LINE                                                      
*                                                                               
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BE    OPT3                                                             
         BAS   RE,VALHED                                                        
         CLI   KEYCHG,C'Y'         TEST FOR KEY CHANGE                          
         BE    OPT3                YES-FORCE DISPLAY FIRST                      
         BAS   RE,VALOPT                                                        
         B     OPTX                                                             
*                                                                               
OPT3     BAS   RE,DISOPT           DISPLAY THE OPTION FIELDS                    
         LA    R2,PROOGRH                                                       
         ST    R2,ACURFORC                                                      
         MVC   CONHEAD(L'DISMSG),DISMSG                                         
         GOTO1 ERREX2              TAKE AN EARLY EXIT                           
*                                                                               
* VALKEY CODE WHEN OFFLINE                                                      
*                                                                               
OPT4     BAS   RE,DISOPT           DISPLAY OPTION NAMES                         
         BAS   RE,VALHED           VALIDATE HEADLINES                           
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
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
OPT14    LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         GOTO1 VBLDMED,DMCB,(1,MEDBUFF)                                         
         GOTO1 VBLDOFF,DMCB,OFFBUFF                                             
         BAS   RE,RDF                                                           
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
         MVI   KEYCHG,C'N'                                                      
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   EXLEN,2                                                          
*                                                                               
* VALIDATE OFFICE GROUP                                                         
*                                                                               
VALHED1  LA    R2,PROOGRH         OFFICE GROUP                                  
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED2                                                          
         GOTO1 VALOG                                                            
         MVC   QOG,EFFOFG                                                       
         SPACE 1                                                                
* VALIDATE OFFICE                                                               
*                                                                               
VALHED2  LA    R2,PROOFFH          OFFICE                                       
         BAS   RE,TSTKEY                                                        
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
         BAS   RE,TSTKEY                                                        
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
         BAS   RE,TSTKEY                                                        
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
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED10                                                         
         GOTO1 VALMG                                                            
         MVC   QMG,MGROUP                                                       
         SPACE 1                                                                
* VALIDATE MEDIA                                                                
*                                                                               
VALHED10 LA    R2,PROMEDH          MEDIA                                        
         BAS   RE,TSTKEY                                                        
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
VALHED12 MVI   OPTION,0            NO MORE NAMES                                
         MVI   QCLOSE,C'Y'         DEFAULT IS TO SHOW CLOSED JOBS               
         LA    R2,PROCLOSH                                                      
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED15                                                         
         CLI   8(R2),C'Y'                                                       
         BE    VALHED15                                                         
         CLI   8(R2),C'N'                                                       
         BNE   INVEND                                                           
         MVI   QCLOSE,C'N'                                                      
         SPACE 1                                                                
VALHED15 OI    PROOGRH+4,X'20'                                                  
         OI    PROOFFH+4,X'20'                                                  
         OI    PROCLIH+4,X'20'                                                  
         OI    PROPROH+4,X'20'                                                  
         OI    PROMGRH+4,X'20'                                                  
         OI    PROMEDH+4,X'20'                                                  
         OI    PROCLOSH+4,X'20'                                                 
         SPACE 1                                                                
VALHEDX  B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO TEST FOR CHANGE IN KEY FIELD                                   
*                                                                               
TSTKEY   TM    4(R2),X'20'                                                      
         BOR   RE                                                               
         MVI   KEYCHG,C'Y'         NOTE KEY CHANGE                              
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE OPTIONS FIELDS                                    
*                                                                               
VALOPT   NTR1  ,                                                                
         LA    R2,PROVAL1H         R2=FIELD POINTER                             
         LA    R3,PROVALXH         R3=A(LAST FIELD)                             
*                                                                               
VALOPT2  CR    R2,R3               TEST FOR PAST LAST FIELD                     
         BH    VALOPT3             YES-ALL FIELDS ARE EMPTY                     
         CLI   5(R2),0             TEST FOR FIELD INPUT                         
         BNE   VALOPT4             YES-AT LEAST ONE                             
         BAS   RE,BUMPTOUN                                                      
         B     VALOPT2                                                          
*                                                                               
VALOPT3  MVI   ERROR,MISSING                                                    
         LA    R2,PROVAL1H                                                      
         B     ERREND                                                           
*                                                                               
VALOPT4  LA    R2,PROVAL1H                                                      
         LA    R3,PROVALXH                                                      
         SR    R4,R4               R4=OPTION COUNTER                            
         MVC   OPTTABL,=H'3'       INITIALIZE OPTION TABLE                      
         XC    OPTTAB(2),OPTTAB                                                 
*                                                                               
VALOPT6  CLI   5(R2),0                                                          
         BE    VALOPT8                                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ACOPD,R6                                                         
         MVI   ACOPEL,ACOPELQ                                                   
         ZIC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         SH    RE,=H'8'            POINT AT FIELD NUMBER                        
         MVC   ACOPNUM,0(RE)       EXTRACT FIELD=OPTION NUMBER                  
         GOTO1 VVALOPT,DMCB,(R2),(R6)                                           
         BNE   ERREND                                                           
         GOTO1 HELLO,DMCB,(C'P',=C'CORETAB'),OPTTABL,(R6)                       
         LA    R4,1(R4)            INCREMENT OPTION COUNT                       
*                                                                               
VALOPT8  BAS   RE,BUMPTOUN                                                      
         CR    R2,R3               TEST PAST LAST FIELD                         
         BNH   VALOPT6                                                          
         STC   R4,NOPTS            SET N'OPTIONS                                
*                                                                               
VALOPTX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY THE OPTION NAMES                                       
*                                                                               
DISOPT   NTR1  ,                                                                
         LA    R2,PROOPT1H                                                      
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BE    DISOPT2             YES-SKIP SCREEN CLEARING                     
         GOTO1 VCLEARF,DMCB,(R2),PROLAST                                        
         GOTO1 (RF),(R1),(1,(R2)),PROLAST                                       
*                                                                               
DISOPT2  BAS   RE,GETOTB           GET OPTION TABLE ENTRY                       
         USING OPTBD,R5                                                         
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(L'OPTBSHRT),OPTBSHRT                                      
         LA    RE,LISTAR+L'OPTBSHRT                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(L'OPTBDESC,RE),OPTBDESC                                        
         BAS   RE,MOVEFLD                                                       
*                                                                               
DISOPT4  BAS   RE,BUMP                                                          
         CLI   0(R2),0             TEST FOR EOS                                 
         BE    DISOPTX                                                          
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    DISOPT2             YES                                          
         B     DISOPT4                                                          
*                                                                               
DISOPTX  B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE OPTION TABLE ENTRY                                     
*                                                                               
GETOTB   ST    RE,SAVERE                                                        
         ST    R2,FULL             SAVE A(FIELD HEADER)                         
         BAS   RE,BUMP                                                          
         ZIC   R1,0(R2)                                                         
         AR    R1,R2                                                            
         SH    R1,=H'8'            POINT AT EXTENDED FIELD HEADER               
         MVC   BYTE,0(R1)          GET OPTION NUMBER                            
*                                                                               
GETOTB2  L     R5,AOPTTAB                                                       
*                                                                               
GETOTB3  CLI   OPTBOPN,0                                                        
         BNE   *+6                                                              
         DC    H'0'                COULD NOT FIND IT                            
         CLC   BYTE,OPTBOPN                                                     
         BE    GETOTB4                                                          
         LA    R5,OPTBL(R5)                                                     
         B     GETOTB3                                                          
*                                                                               
GETOTB4  L     R2,FULL                                                          
*                                                                               
GETOTBX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
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
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PRINT                                                         
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
         BAS   RE,PRINT                                                         
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
         CLI   QCLOSE,C'N'         TEST NO CLOSED WANTED                        
         BE    JOBX                YES                                          
         BAS   RE,RDOPT                                                         
         BAS   RE,PRINT                                                         
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
         LA    RF,1(R1)                                                         
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
* SUB-ROUTINE TO FILTER ON THE OFFICE AND OFFICE GROUP                          
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
         LA    RE,KEY                                                           
         ST    RE,GOAKEY                                                        
*                                                                               
         MVC   GOSELCLI,CLICODE    SET CLIENT CODE                              
         CLI   TYPEREC,C'C'        TEST FOR CLIENT RECORD                       
         BE    RDOPT2              YES                                          
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
* SUB-ROUTINE TO PRINT ACCOUNT DATA                                             
*                                                                               
PRINT    NTR1  ,                                                                
         MVC   P,SPACES            PRE-CLEAR PRINT LINE                         
         LA    R3,P                                                             
         USING PRTD,R3                                                          
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(L'CLICODE),CLICODE                                        
         MVI   MASK,OPTBCLI        SET PROTECTED BIT MASK                       
         CLI   TYPEREC,C'C'        TEST FOR CLIENT                              
         BE    PRINT2                                                           
*                                                                               
         LA    RE,LISTAR+L'CLICODE+1                                            
         MVC   0(L'PRODCODE,RE),PRODCODE                                        
         MVI   MASK,OPTBPRO                                                     
         CLI   TYPEREC,C'P'                                                     
         BE    PRINT2                                                           
*                                                                               
         LA    RE,L'PRODCODE+1(RE)                                              
         MVC   0(L'JOBNUM,RE),JOBNUM                                            
         MVI   MASK,OPTBJOB                                                     
*                                                                               
PRINT2   GOTO1 SQUASHER,DMCB,LISTAR,L'LISTAR                                    
         MVC   PRTCODE,LISTAR                                                   
         GOTO1 SETNAME,DMCB,AIO,WORK                                            
         MVC   PRTNAME,WORK                                                     
         CLI   TYPEREC,C'J'                                                     
         BNE   PRINT3                                                           
         TM    JOBSTAT,X'40'       TEST CLOSED                                  
         BZ    *+8                                                              
         MVI   PRTCLOSE,C'C'       YES-MARK THEM                                
*                                                                               
PRINT3   BAS   RE,COMPARE                                                       
*                                                                               
PRINTX   B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO COMPARE THE INPUT OPTIONS AGAINST THE RECORD'S                 
* OPTIONS--CALLED FROM PRINT                                                    
*                                                                               
COMPARE  NTR1  ,                                                                
         MVI   DIFFSW,C'N'         SET NO DIFFERENCES FOUND                     
         ZIC   R2,NOPTS            R2=LOOP COUNTER                              
         LA    R6,OPTTAB           R6=A(OPTION INPUT TABLE)                     
         USING ACOPD,R6                                                         
*                                                                               
COMPARE2 BAS   RE,FINDENT                                                       
         USING OPTBD,R5                                                         
         LH    RE,OPTBDISP         RE=DISPLACEMENT PAST GOPTIONS                
         LA    RE,GOPTIONS(RE)                                                  
         XC    WORK,WORK                                                        
         ZIC   R1,ACOPLEN                                                       
         SH    R1,=Y(ACOPDATA-ACOPD+1)                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACOPDATA    EXTRACT DATA                                 
*                                                                               
         ZIC   R1,OPTBMAX                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),0(RE)       COMPARE AGAINST GOBLOCK VALUE                
         BE    COMPARE6            THEY ARE THE SAME                            
*                                                                               
         ZIC   R1,MASK                                                          
         EX    R1,COMPTM           TEST FOR PROTECTED AT THIS LEVEL             
         BO    COMPARE6            YES-DO NOT PRINT                             
*                                                                               
COMPARE4 GOTO1 VDISOPT,DMCB,(OPTBOPN,GOBLOCK),PRTVAL-8                          
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(L'OPTBSHRT),OPTBSHRT                                      
         LA    RE,LISTAR+L'OPTBSHRT                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(L'OPTBDESC,RE),OPTBDESC                                        
         MVC   PRTDESC,LISTAR                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   DIFFSW,C'Y'                                                      
*                                                                               
COMPARE6 ZIC   R0,ACOPLEN                                                       
         AR    R6,R0                                                            
         BCT   R2,COMPARE2                                                      
*                                                                               
COMPARE8 CLI   DIFFSW,C'Y'         TEST DIFFERENCES FOUND                       
         BNE   COMPAREX            NO                                           
         GOTO1 SPOOL,DMCB,(R8)     YES-SKIP A LINE                              
*                                                                               
COMPAREX B     XIT                                                              
         SPACE 2                                                                
COMPTM   TM    OPTBPROT,0                                                       
         SPACE 2                                                                
* SUB-ROUTINE TO FIND OPTION TABLE ENTRY                                        
*                                                                               
FINDENT  L     R5,AOPTTAB                                                       
*                                                                               
FINDENT2 CLI   OPTBOPN,0           TEST FOR EOT                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ACOPNUM,OPTBOPN     MATCH ON OPTION NUMBER                       
         BER   RE                  YES                                          
         LA    R5,OPTBL(R5)                                                     
         B     FINDENT2                                                         
         EJECT                                                                  
* HOOK ROUTINE FOR PRINTING                                                     
*                                                                               
HOOK     NTR1  ,                                                                
         MVC   H4+10(L'CLICODE),CLICODE SHOW CLIENT CODE                        
         GOTO1 SETNAME,DMCB,ACLI,WORK                                           
         MVC   H4+18(20),WORK      SHOW CLIENT NAME                             
*                                                                               
HOOK2    ICM   R2,15,ABOX          LOOK FOR BOXES                               
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
         MVI   PRTBOX3-PRTD(RE),C'C'                                            
         MVI   PRTBOX4-PRTD(RE),C'C'                                            
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
DISMSG   DC    C'**OPTIONS DISPLAYED-NOW ENTER YOUR DEFAULTS**'                 
OPTBUFFL DC    F'20000'            LENGTH OF OPTIONS BUFFER OFFLINE             
         SPACE 2                                                                
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* SPECS FOR OPTIONS CONVERSION REPORT                                           
*                                                                               
HEDSPECS DS    0D                                                               
         SSPEC H1,2,CREATED                                                     
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,43,C'OPTIONS CONTROL REPORT'                                  
         SSPEC H2,43,C'----------------------'                                  
         SSPEC H1,85,AGYNAME                                                    
         SSPEC H2,85,AGYADD                                                     
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H4,85,REPORT                                                     
         SSPEC H4,98,PAGE                                                       
*                                                                               
         SSPEC H7,3,C'ACCOUNT'                                                  
         SSPEC H7,19,C'ACCOUNT NAME'                                            
         SSPEC H7,56,C'CLOSED'                                                  
         SSPEC H7,63,C'OPTION DESCRIPTION'                                      
         SSPEC H7,95,C'OPTION VALUE'                                            
*                                                                               
         DC    X'00'                                                            
         SPACE 2                                                                
OFFBUFF  DC    XL110'00'                                                        
MEDBUFF  DC    XL100'00'                                                        
         SPACE 2                                                                
OPTTABL  DS    H                                                                
OPTTAB   DS    CL2000                                                           
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACPROWORKD                                                                     
*DDREPMASTD                                                                     
*ACGENBOTH                                                                      
*DDBIGBOX                                                                       
*DDTWADCOND                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDTWADCOND                                                     
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
KEYCHG   DS    C                                                                
NOPTS    DS    X                   N'OPTION VALUES ON SCREEN                    
DIFFSW   DS    C                                                                
MASK     DS    X                                                                
TYPEREC  DS    C                   C=CLIENT,P=PRODUCT,J=JOB                     
EXLEN    DS    X                                                                
*                                                                               
ACOMP    DS    A                                                                
ALEDG    DS    A                                                                
ACLI     DS    A                                                                
APROD    DS    A                                                                
AJOB     DS    A                                                                
*                                                                               
TOTBUFFL DS    F                   TOTAL BUFFER ACQUIRED VIA GETMAIN            
AOPTBUFF DS    A                                                                
LOPTBUFF DS    A                                                                
*                                                                               
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
       ++INCLUDE ACPROF5D                                                       
         SPACE  2                                                               
MOREDATA DS    0D                                                               
         DS    CL((SAVAREA-MOREDATA)-(*-MOREDATA)) SPARE                        
         SPACE 2                                                                
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PRTD     DSECT                                                                  
         DS    C                   SPARE                                        
PRTLBOX  DS    C                   LEFT BOX POSITION                            
PRTCODE  DS    CL14                ACCOUNT CODE                                 
         DS    C                                                                
PRTBOX1  DS    C                   CENTER BOX POSITION                          
PRTNAME  DS    CL36                ACCOUNT NAME                                 
PRTBOX2  DS    C                                                                
PRTCLOSE DS    C                                                                
         DS    CL5                 SPARE                                        
PRTBOX3  DS    C                                                                
PRTDESC  DS    CL31                PRINT DESCRIPTION                            
PRTBOX4  DS    C                                                                
PRTVAL   DS    CL20                OPTION VALUE                                 
PRTRBOX  DS    C                   RIGHT BOX POSITION                           
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACPRO05   07/10/20'                                      
         END                                                                    
