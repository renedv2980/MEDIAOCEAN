*          DATA SET ACREPYA02  AT LEVEL 004 AS OF 02/18/04                      
*PHASE ACYA02A                                                                  
                                                                                
***********************************************************************         
* QACCOUNT - OPTIONAL FILTER VALUE (KEYWORD=Y/N)                      *         
*                                                                     *         
* OPTION#1 - N=EXCLUDE TEST COMPANIES                                 *         
*            O=EXCLUDE LIVE COMPANIES                                 *         
*            X=INCLUDE ALL COMPANIES                                  *         
*                                                                     *         
* OPTION#2 - Y=INCLUDE COMPANIES IN ERROR                             *         
*            X=EXCLUDE COMPANIES IN ERROR                             *         
***********************************************************************         
                                                                                
ACYA02   TITLE '- LIST COMPANY RECORDS'                                         
ACYA02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACYA**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         L     R9,AMONACC                                                       
         USING ACMD,R9                                                          
         LA    RC,SPACEND                                                       
         USING WORKD,RC                                                         
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
RUNF     MVI   RCFLAG1,RCFREQLC+RCFREPLC                                        
         B     EXIT                                                             
                                                                                
REQL     XC    BINPARMS(BINPARML),BINPARMS                                      
         MVC   BINPATAB,ACPYTAB                                                 
         MVC   BINPLREC,=A(CPYTABL)                                             
         MVC   BINPLKEY,=AL3(CPYTLKEY)                                          
         MVC   BINPMAXN,=A(CPYTMAXN)                                            
                                                                                
         L     RF,ADBXAREA                                                      
         USING BOXD,RF                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,C'N'                                                      
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXRT,BOXRTQ                                                     
         MVI   BOXRM,BOXRMQ                                                     
         MVI   BOXRB,BOXRBQ                                                     
         MVC   BOXCOLS,SPACES                                                   
         MVI   CPYCL,BOXCLQ                                                     
         MVI   CPYCC1,BOXCCQ                                                    
         MVI   CPYCC2,BOXCCQ                                                    
         MVI   CPYCC3,BOXCCQ                                                    
         MVI   CPYCC4,BOXCCQ                                                    
         MVI   CPYCR,BOXCRQ                                                     
         DROP  RF                                                               
                                                                                
         MVI   RCSUBPRG,1          SUB-PROGRAM 1 - COMPANY ANALYSIS             
         MVC   PAGE,=H'1'                                                       
                                                                                
         XC    FILTER(FILTERL),FILTER                                           
         CLC   QACCOUNT,SPACES     TEST DATA FILTER SET                         
         BE    REQL02                                                           
         LA    RE,QACCOUNT                                                      
         LA    RF,L'PROTKWRD+1                                                  
         CLI   0(RE),C'='                                                       
         BE    *+14                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,*-12                                                          
         DC    H'0'                INVALID FILTER FORMAT                        
         MVC   WORK,1(RE)          SAVE CHARACTER FOLLOWING =                   
         MVC   FILTKWRD,SPACES                                                  
         LA    RF,QACCOUNT+1                                                    
         SR    RE,RF               RE=L'KEYWORD-1                               
         EX    RE,*+4                                                           
         MVC   FILTKWRD(0),QACCOUNT                                             
         MVI   FILTYORN,FILTYQ                                                  
         CLI   WORK,C'Y'                                                        
         BE    REQL02                                                           
         MVI   FILTYORN,FILTNQ                                                  
         CLI   WORK,C'N'                                                        
         BE    REQL02                                                           
         DC    H'0'                INVALID FILTER FORMAT                        
                                                                                
REQL02   L     RF,ACPYPROT                                                      
         USING PROTABD,RF          RF=A(PROFILE TABLE)                          
REQL04   CLI   PROTABD,PROTEOTQ    TEST END OF TABLE                            
         BE    REQL08                                                           
         XC    PROTDATA,PROTDATA   CLEAR ACCUMULATORS ETC.                      
         OC    FILTATAB,FILTATAB   TEST FILTER ENTRY FOUND                      
         BNZ   REQL06                                                           
         OC    FILTKWRD,FILTKWRD   TEST FILTER REQUESTED                        
         BZ    REQL06                                                           
         CLC   PROTKWRD,FILTKWRD   TEST THIS IS THE FILTER                      
         BNE   REQL06                                                           
         STCM  RF,15,FILTATAB      SET FILTER TABLE ENTRY                       
REQL06   LA    RF,PROTABL(RF)      BUMP TO NEXT TABLE ENTRY                     
         B     REQL04                                                           
                                                                                
REQL08   OC    FILTER(FILTERL),FILTER                                           
         BZ    REQL10                                                           
         OC    FILTATAB,FILTATAB    INVALID FILTER KEYWORD                      
         BNZ   REQL10                                                           
         DC    H'0'                                                             
                                                                                
REQL10   LA    R2,IO                                                            
         USING CT5REC,R2                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
                                                                                
         GOTOR DATAMGR,DMCB,DMRDHI,CTFILE,CT5KEY,CT5KEY                         
         B     REQL14                                                           
REQL12   GOTOR DATAMGR,DMCB,DMRSEQ,CTFILE,CT5KEY,CT5KEY                         
REQL14   BE    *+6                                                              
         DC    H'0'                                                             
         CLI   CT5KTYP,CT5KTYPQ                                                 
         BNE   REQL18                                                           
                                                                                
         LA    R1,CT5DATA                                                       
         USING CTSYSD,R1                                                        
         SR    R0,R0                                                            
REQL16   CLI   CTSYSEL,0                                                        
         BE    REQL12                                                           
         CLI   CTSYSEL,CTSYSELQ                                                 
         BNE   *+12                                                             
         CLI   CTSYSNUM,ACCSYSQ    TEST ACCPAK SYSTEM                           
         BE    *+14                                                             
         IC    R0,CTSYSLEN                                                      
         AR    R1,R0                                                            
         B     REQL16                                                           
         LA    RF,WORK             ADD ENTRY TO COMPANY TABLE                   
         USING CPYTABD,RF                                                       
         MVC   CPYTSE,CTSYSSE                                                   
         MVC   CPYTCPY,CTSYSAGB                                                 
         MVC   CPYTALPH,CT5KALPH                                                
         GOTOR TSTTST              TEST FOR TEST COMPANY OPTION                 
         BE    REQL12                                                           
         GOTOR BINSRCH,BINPARMS,(1,CPYTABD)                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   REQL12                                                           
         DC    H'0'                                                             
         DROP  R2,RF                                                            
REQL18   OC    BINPNREC,BINPNREC                                                
         BNZ   *+6                                                              
         DC    H'0'                DIDN'T FIND ANYTHING ON CTFILE               
                                                                                
         LA    R2,IO               READ SYSTEM LIST RECORD                      
         USING CTWREC,R2                                                        
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVI   CTWKREC,CTWKRSYS                                                 
*&&US*&& MVI   CTWKSYSN,ACCSYSQ                                                 
         GOTOR DATAMGR,DMCB,DMREAD,CTFILE,CTWREC,CTWREC                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,CTWDATA                                                       
         USING CTLSTD,R1                                                        
         L     RF,ASETAB                                                        
         USING SETABD,RF                                                        
         SR    R0,R0                                                            
REQL20   CLI   CTLSTD,0            TEST END OF RECORD                           
         BNE   *+12                                                             
         MVI   SETABD,SETEOTQ      YES - SET END OF SE TABLE                    
         B     REQL24                                                           
         CLI   CTLSTEL,CTLSTELQ    TEST LIST ELEMENT                            
         BNE   REQL22                                                           
         CLI   CTLSTSYS,ACCSYSQ    TEST ACCPAK SE LIST ENTRY                    
         BNE   REQL22                                                           
         MVC   SETSE,CTLSTSE       ADD ENTRY TO SE TABLE                        
         MVC   SETNAME,CTLSTNAM                                                 
         LA    RF,SETABL(RF)       BUMP TO NEXT SE TABLE ENTRY                  
REQL22   IC    R0,CTLSTLEN         BUMP TO NEXT ELEMENT ON LIST RECORD          
         AR    R1,R0                                                            
         B     REQL20                                                           
                                                                                
REQL24   L     R2,BINPNREC         R2=N'ENTRIES IN BINSRCH TABLE                
         L     R3,ACPYTAB          R3=A(CURRENT TABLE ENTRY)                    
         USING CPYTABD,R3                                                       
         MVI   SENUM,0             INITIALIZE SE NUMBER                         
         ZAP   CPYCOUNT,=P'0'      INITIALIZE COUNTER                           
                                                                                
REQL26   CLC   CPYTSE,SENUM        TEST CHANGE OF SE NUMBER                     
         BE    REQL30                                                           
         MVI   FORCEHED,C'Y'       SKIP TO A NEW PAGE                           
                                                                                
         MVC   SESE,=C'SE#NN/'                                                  
         GOTOR HEXOUT,DMCB,CPYTSE,SESE+3,1,=C'N'                                
         L     RF,ASETAB                                                        
         USING SETABD,RF                                                        
REQL28   CLI   SETABD,SETEOTQ      TEST END OF SE TABLE                         
         BNE   *+6                                                              
         DC    H'0'                BAD SE NUMBER                                
         CLC   SETSE,CPYTSE                                                     
         BE    *+12                                                             
         LA    RF,SETABL(RF)       BUMP TO NEXT SE TABLE ENTRY                  
         B     REQL28                                                           
         MVC   SENUM,CPYTSE        EXTRACT SE NUMBER                            
         MVC   SENAME,SETNAME      AND SE NAME                                  
         DROP  RF                                                               
                                                                                
         L     RF,ADMASTC                                                       
         L     RF,MCUTL-MASTD(RF)                                               
         MVC   4(1,RF),SENUM       SET NEW SE NUMBER & OPEN FILES               
         GOTOR DATAMGR,DMCB,ACMDMOPN,ACMACSYS,OPENLIST,WORK                     
                                                                                
REQL30   LA    R4,IOKEY                                                         
         USING CPYRECD,R4                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,CPYTCPY                                                  
         GOTOR DATAMGR,DMCB,DMREAD,ACMACDIR,CPYRECD,CPYRECD                     
         BE    REQL32                                                           
         CLI   QOPT2,C'Y'          TEST SHOWING ERROR COMPANIES                 
         BNE   REQL80                                                           
         GOTOR HEXOUT,DMCB,CPYTCPY,PCPYHEX,1,=C'N'                              
         MVC   PCPYALPH,CPYTALPH                                                
         MVC   PCPYNAME(L'ERROR1),ERROR1                                        
         GOTOR PRINTCPY                                                         
         B     REQL78                                                           
                                                                                
REQL32   GOTOR DATAMGR,DMCB,ACMDMGET,ACMACMST,CPYKDA,IO,DMWORK                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,IO                                                            
                                                                                
         GOTOR HEXOUT,DMCB,CPYKCPY,PCPYHEX,1,=C'N'                              
                                                                                
         LA    R5,CPYRFST                                                       
         USING NAMELD,R5                                                        
         SR    R0,R0                                                            
REQL34   CLI   NAMEL,0                                                          
         BE    REQL36                                                           
         CLI   NAMEL,NAMELQ                                                     
         BE    *+14                                                             
         IC    R0,NAMLN                                                         
         AR    R5,R0                                                            
         B     REQL34                                                           
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EX    R1,*+4                                                           
         MVC   PCPYNAME(0),NAMEREC                                              
                                                                                
REQL36   LA    R5,CPYRFST                                                       
         USING CPYELD,R5                                                        
         SR    R0,R0                                                            
REQL38   CLI   CPYEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CPYEL,CPYELQ                                                     
         BE    *+14                                                             
         IC    R0,CPYLN                                                         
         AR    R1,R0                                                            
         B     REQL38                                                           
                                                                                
         GOTOR TSTFLT              APPLY REQUEST FILTER                         
         BNE   REQL80                                                           
                                                                                
         MVC   PCPYALPH,CPYALPHA                                                
*&&US                                                                           
         CLC   CPYALPHA,CPYTALPH   ENSURE ACCESS RECORD AND COMPANY             
         BE    *+6                 RECORD HAVE THE SAME ALPHA ID                
         DC    H'0'                                                             
*&&                                                                             
         LA    R6,IO2                                                           
         USING CTIREC,R6                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CPYUID                                                   
         GOTOR DATAMGR,DMCB,DMREAD,CTFILE,CTIKEY,CTIKEY                         
         BE    REQL40                                                           
         MVC   PCPYPRID(2),=C'#='  OUTPUT #=NNNNN IF ID NOT FOUND               
         SR    R0,R0                                                            
         ICM   R0,3,CPYUID                                                      
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PCPYPRID+2(5),DUB                                                
         B     REQL44                                                           
                                                                                
REQL40   LA    R6,CTIDATA          LOCATE ID CODE ELEMENT                       
         USING CTDSCD,R6                                                        
         SR    R0,R0                                                            
REQL42   CLI   CTDSCEL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+14                                                             
         IC    R0,CTDSCLEN                                                      
         AR    R6,R0                                                            
         B     REQL42                                                           
         MVC   PCPYPRID,CTDSC                                                   
                                                                                
REQL44   L     R6,ACPYPROT                                                      
         USING PROTABD,R6          R6=A(PROFILE TABLE)                          
         LA    R7,IO2              R7=PROFILE OUTPUT AREA                       
                                                                                
REQL46   CLI   PROTABD,PROTEOTQ    TEST END OF PROFILE TABLE                    
         BE    REQL72                                                           
         SR    R1,R1                                                            
         IC    R1,PROTDISP                                                      
         CLM   R1,1,CPYLN                                                       
         BNL   REQL70                                                           
         LA    R1,CPYELD(R1)       R1=A(INPUT VALUE)                            
         SR    RF,RF                                                            
         IC    RF,PROTDLEN                                                      
         BCTR  RF,0                RF=L'INPUT VALUE-1                           
                                                                                
         SR    RE,RE                                                            
         ICM   RE,1,PROTTYPE                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SLL   RE,2                                                             
         B     *(RE)               GO TO DATA HANDLING ROUTINE                  
                                                                                
         B     REQL50              ALPHA STRING                                 
         B     REQL52              BIT VALUE                                    
         B     REQL54              INTEGER NUMBER                               
         B     REQL56              FIRST MONTH                                  
         B     REQL58              HEX STRING                                   
         B     REQL60              COMPRESSED DATE                              
         B     REQL62              BINARY DAY                                   
                                                                                
REQL50   EX    RF,*+8              ** ALPHA STRING **                           
         BZ    REQLNO                                                           
         OC    0(0,R1),0(R1)                                                    
         EX    RF,*+8                                                           
         BE    REQLNO                                                           
         CLC   0(0,R1),SPACES                                                   
         CLI   PROTDVAL,0          TEST DEFAULT ALPHA VALUE SET                 
         BE    *+14                                                             
         CLC   PROTDVAL,0(R1)                                                   
         BE    REQLNO                                                           
         GOTOR ADDKEY2                                                          
         EX    RF,*+4                                                           
         MVC   0(0,R7),0(R1)                                                    
         GOTOR ADDCOM                                                           
         B     REQLYES                                                          
                                                                                
REQL52   IC    RF,PROTBVAL         ** BIT VALUE **                              
         EX    RF,*+8                                                           
         BZ    REQLNO                                                           
         TM    0(R1),0                                                          
         GOTOR ADDKEY1                                                          
         B     REQLYES                                                          
                                                                                
REQL54   EX    RF,*+8              ** INTEGER NUMBER **                         
         BZ    REQLNO                                                           
         OC    0(0,R1),0(R1)                                                    
         IC    RF,ICMTAB(RF)       GET VALUE INTO R0                            
         SR    R0,R0                                                            
         EX    RF,*+8                                                           
         B     *+8                                                              
         ICM   R0,0,0(R1)                                                       
         EDIT  (R0),(8,DOUBLE),ALIGN=LEFT                                       
         GOTOR ADDKEY2                                                          
         MVC   0(L'DOUBLE,R7),DOUBLE                                            
         LA    RF,L'DOUBLE-1                                                    
         GOTOR ADDCOM                                                           
         B     REQLYES                                                          
                                                                                
REQL56   GOTOR ADDKEY2             ** FISCAL START MONTH **                     
         CLI   0(R1),0                                                          
         BNE   *+8                                                              
         MVI   0(R1),0                                                          
         MVC   WORK(1),0(R1)                                                    
         NI    WORK,X'0F'                                                       
         SR    RF,RF                                                            
         IC    RF,WORK                                                          
         TM    0(R1),X'F0'                                                      
         BNZ   *+8                                                              
         LA    RF,9(RF)                                                         
         MHI   RF,3                                                             
         LA    RF,MONTHS-3(RF)                                                  
         MVC   0(3,R7),0(RF)                                                    
         LA    RF,3-1                                                           
         GOTOR ADDCOM                                                           
         B     REQLYES                                                          
                                                                                
REQL58   EX    RF,*+8              ** HEX VALUE **                              
         BZ    REQLNO                                                           
         OC    0(0,R1),0(R1)                                                    
         GOTOR ADDKEY2                                                          
         LR    R0,R1                                                            
         GOTOR HEXOUT,DMCB,(R0),(R7),1(RF),=C'N'                                
         L     RF,8(R1)                                                         
         SLL   RF,1                                                             
         BCTR  RF,0                                                             
         GOTOR ADDCOM                                                           
         B     REQLYES                                                          
                                                                                
REQL60   OC    0(2,R1),0(R1)       ** COMPRESSED DATE **                        
         BZ    REQLNO                                                           
         GOTOR ADDKEY2                                                          
         LR    RF,R1                                                            
         MVC   0(10,R7),SPACES                                                  
         GOTOR DATCON,DMCB,(2,(RF)),(17,(R7))                                   
         LA    RF,10-1                                                          
         GOTOR ADDCOM                                                           
         B     REQLYES                                                          
                                                                                
REQL62   CLI   0(R1),0             ** BINARY DAY **                             
         BE    REQLNO                                                           
         GOTOR ADDKEY2                                                          
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         MHI   RE,3                                                             
         LA    RE,DAYTABL-3(RE)                                                 
         MVC   0(3,R7),0(RE)                                                    
         LA    RF,3-1                                                           
         GOTOR ADDCOM                                                           
         B     REQLYES                                                          
                                                                                
REQLNO   LA    RE,PROTNCNT         ADD TO 'NO' ACCUMULATOR                      
         B     REQLADD                                                          
                                                                                
REQLYES  LA    RE,PROTYCNT         ADD TO 'YES' ACCUMULATOR                     
                                                                                
REQLADD  SR    RF,RF                                                            
         ICM   RF,7,0(RE)                                                       
         LA    RF,1(RF)                                                         
         STCM  RF,7,0(RE)                                                       
                                                                                
REQL70   LA    R6,PROTABL(R6)                                                   
         B     REQL46                                                           
                                                                                
REQL72   LA    RE,IO2              PRINT THE PROFILE STRING IN CHUNKS           
         CR    RE,R7                                                            
         BE    REQL76                                                           
         BCTR  R7,0                DROP THE TRAILING COMMA                      
         MVC   0(L'P,R7),SPACES                                                 
         LA    R6,IO2                                                           
                                                                                
REQL74   CLC   0(L'PCPYPROF,R6),SPACES                                          
         BE    REQL78                                                           
         LA    R7,L'PCPYPROF-1(R6)                                              
         CLI   0(R7),C' '                                                       
         BE    *+8                                                              
         CLI   0(R7),C','                                                       
         BE    *+8                                                              
         BCT   R7,*-8                                                           
         SR    R7,R6                                                            
         EX    R7,*+4                                                           
         MVC   PCPYPROF(0),0(R6)                                                
         GOTOR PRINTCPY                                                         
         LA    R6,1(R6,R7)                                                      
         B     REQL74                                                           
                                                                                
REQL76   GOTOR PRINTCPY                                                         
                                                                                
REQL78   AP    CPYCOUNT,=P'1'      INCREMENT COMPANY COUNTER                    
         SR    R6,R6                                                            
         IC    R6,LINE                                                          
         LA    R6,1(R6)                                                         
         CLM   R6,1,MAXLINES                                                    
         BNH   *+12                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     REQL80                                                           
         A     R6,ADBXAREA                                                      
*&&US*&& LA    R6,BOXROWS-2-BOXD(R6)                                            
*&&UK*&& LA    R6,BOXROWS-1-BOXD(R6)                                            
         MVI   0(R6),BOXRMQ        DRAW A HORIZONTAL BOX LINE                   
         GOTOR PRINTCPY                                                         
         MVI   0(R6),C' '                                                       
                                                                                
REQL80   MVC   P,SPACES            CLEAR THE PRINT LINE                         
         LA    R3,CPYTABL(R3)      BUMP TO NEXT COMPANY                         
         BCT   R2,REQL26                                                        
         DROP  R3                                                               
                                                                                
         CP    CPYCOUNT,=P'0'      TEST ANY COMPANIES PRINTED                   
         BE    EXIT                                                             
                                                                                
         L     RF,ADBXAREA         CLOSE THE BOX                                
         MVI   BOXREQ-BOXD(RF),BOXCLOSE                                         
         MVI   FORCEHED,C'N'                                                    
         GOTOR ACREPORT                                                         
                                                                                
REQL82   L     RF,ADBXAREA         INITIALIZE COMPANY DATA ANALYSIS             
         USING BOXD,RF                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,C'N'                                                      
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXRT,BOXRTQ                                                     
         MVI   BOXRM,BOXRMQ                                                     
         MVI   BOXRB,BOXRBQ                                                     
         MVC   BOXCOLS,SPACES                                                   
         MVI   CPDCL,BOXCLQ                                                     
         MVI   CPDCC1,BOXCCQ                                                    
         MVI   CPDCC2,BOXCCQ                                                    
         MVI   CPDCC3,BOXCCQ                                                    
         MVI   CPDCR,BOXCRQ                                                     
         DROP  RF                                                               
                                                                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2          SUB-PROGRAM 2 - DATA ANALYSIS                
         MVC   PAGE,=H'1'                                                       
         L     R6,ACPYPROT                                                      
         USING PROTABD,R6          R6=A(PROFILE TABLE)                          
                                                                                
REQL84   CLI   PROTABD,PROTEOTQ    TEST END OF TABLE                            
         BE    REQL88                                                           
         MVC   PCPDKWRD,PROTKWRD                                                
         SR    R0,R0                                                            
         ICM   R0,7,PROTNCNT       'NO' COUNT                                   
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PCPDNCNT,DUB                                                     
         ICM   R0,7,PROTYCNT       'YES' COUNT                                  
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PCPDYCNT,DUB                                                     
         LA    RE,CPDCOMM1                                                      
         OC    PROTNCNT,PROTNCNT                                                
         BZ    REQL86                                                           
         LA    RE,CPDCOMM2                                                      
         OC    PROTYCNT,PROTYCNT                                                
         BZ    REQL86                                                           
         SR    RE,RE                                                            
REQL86   LTR   RE,RE               TEST ANY COMMENT TO PRINT                    
         BZ    *+10                                                             
         MVC   PCPDCOMM,0(RE)                                                   
         GOTOR ACREPORT                                                         
         LA    R6,PROTABL(R6)      BUMP TO NEXT TABLE ENTRY                     
         B     REQL84                                                           
                                                                                
REQL88   L     RF,ADBXAREA         CLOSE THE BOX OFF                            
         MVI   BOXREQ-BOXD(RF),BOXCLOSE                                         
         GOTOR ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
PRINTCPY LR    R0,RE               PRINT LINE FOR COMPANY ANALYSIS              
         MVC   HEAD3+1(7),=C'System='                                           
         MVC   HEAD3+8(L'SESE+L'SENAME),SESE                                    
         OC    FILTER,FILTER                                                    
         BZ    PRINTCP2                                                         
         LA    RF,HEAD3+8+L'SESE+L'SENAME-1                                     
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C','                                                       
         MVC   2(7,RF),=C'Filter='                                              
         MVC   9(L'QACCOUNT,RF),QACCOUNT                                        
PRINTCP2 GOTOR ACREPORT                                                         
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
ADDKEY1  MVC   0(L'PROTKWRD,R7),PROTKWRD                                        
         LA    R7,L'PROTKWRD-1(R7)                                              
         CLI   0(R7),C' '                                                       
         BNE   *+8                                                              
         BCT   R7,*-8                                                           
         MVI   1(R7),C','                                                       
         LA    R7,2(R7)                                                         
         BR    RE                                                               
                                                                                
ADDKEY2  MVC   0(L'PROTKWRD,R7),PROTKWRD                                        
         LA    R7,L'PROTKWRD-1(R7)                                              
         CLI   0(R7),C' '                                                       
         BNE   *+8                                                              
         BCT   R7,*-8                                                           
         MVI   1(R7),C'='                                                       
         LA    R7,2(R7)                                                         
         BR    RE                                                               
                                                                                
ADDCOM   LA    R7,0(RF,R7)                                                      
         CLI   0(R7),C' '                                                       
         BNE   *+8                                                              
         BCT   R7,*-8                                                           
         MVI   1(R7),C','                                                       
         LA    R7,2(R7)                                                         
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* APPLY TEST COMAPNY FILTER                                           *         
***********************************************************************         
                                                                                
TSTTST   CLI   QOPT1,C' '          TEST INCLUDING ALL COMPANIES                 
         BE    TSTTSTN             YES                                          
         LA    R1,TSTTAB           RF=A(DDS COMPANY TABLE)                      
TSTTST02 CLI   0(R1),0             TEST END OF TABLE                            
         BE    TSTTST04            YES - NOT A DDS COMPANY                      
         CLC   WORK(2),0(R1)       MATCH SE#/COMPANY CODE                       
         BE    TSTTST06                                                         
         LA    R1,L'TSTTAB(R1)                                                  
         B     TSTTST02                                                         
TSTTST04 CLI   QOPT1,C'O'                                                       
         BE    TSTTSTY                                                          
         B     TSTTSTN                                                          
TSTTST06 CLI   QOPT1,C'N'                                                       
         BE    TSTTSTY                                                          
         B     TSTTSTN                                                          
TSTTSTN  CLI   *+0,0               SET CC=NOT EQUAL (KEEP)                      
         BR    RE                                                               
TSTTSTY  CLI   *+1,0               SET CC=EQUAL (DISCARD)                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* APPLY KEYWORD FILTER                                                *         
***********************************************************************         
                                                                                
TSTFLT   ICM   RF,15,FILTATAB      TEST ANY FILTER SET                          
         BZ    TSTFLTY             NO - INCLUDE                                 
         USING PROTABD,RF                                                       
         SR    R1,R1                                                            
         IC    R1,PROTDISP                                                      
         CLM   R1,1,CPYLN          TEST DATA WITHIN ELEMENT                     
         BNH   *+16                YES - TEST THE DATA                          
         CLI   FILTYORN,FILTYQ     TEST FILTER=Y SPECIFIED                      
         BE    TSTFLTN                                                          
         B     TSTFLTY                                                          
         LA    R1,CPYELD(R1)       R1=A(DATA VALUE)                             
                                                                                
         CLI   PROTTYPE,PROTTBIT   TEST BIT VALUE                               
         BNE   TSTFLT02                                                         
         IC    RF,PROTBVAL         RF=BIT VALUE TO TEST                         
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    0(R1),0                                                          
         BZ    *+16                                                             
         CLI   FILTYORN,FILTYQ     BIT IS ON                                    
         BE    TSTFLTY             INCLUDE IF FILTER=Y REQUESTED                
         B     TSTFLTN                                                          
         CLI   FILTYORN,FILTNQ     BIT IS OFF                                   
         BE    TSTFLTY             INCLUDE IF FILTER=N REQUESTED                
         B     TSTFLTN                                                          
                                                                                
TSTFLT02 CLI   PROTDVAL,0          TEST DEFAULT VALUE IS SET                    
         BE    TSTFLT04                                                         
         CLI   0(R1),C' '          TEST VALUE SET                               
         BH    *+10                                                             
         MVC   0(1,R1),PROTDVAL    NO - SET DEFAULT                             
         CLC   PROTDVAL,0(R1)      MATCH VALUE TO DEFAULT                       
         BNE   *+16                                                             
         CLI   FILTYORN,FILTNQ     IS THE DEFAULT VALUE                         
         BE    TSTFLTY             INCLUDE IF FILTER=N REQUESTED                
         B     TSTFLTN                                                          
         CLI   FILTYORN,FILTYQ     IS NOT THE DEFAULT                           
         BE    TSTFLTY             INCLUDE IF FILTER=Y REQUESTED                
         B     TSTFLTN                                                          
                                                                                
TSTFLT04 IC    RF,PROTDLEN                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,R1),0(R1)                                                    
         BZ    *+16                                                             
         CLI   FILTYORN,FILTYQ     VALUE SET                                    
         BE    TSTFLTY             INCLUDE IF FILTER=Y REQUESTED                
         B     TSTFLTN                                                          
         CLI   FILTYORN,FILTNQ     VALUE NOT SET                                
         BE    TSTFLTY             INCLUDE IF FILTER=N REQUESTED                
         B     TSTFLTN                                                          
                                                                                
TSTFLTN  CLI   *+0,0                                                            
         BR    RE                                                               
TSTFLTY  CLI   *+1,0                                                            
         BR    RE                                                               
         EJECT                                                                  
ACCSYSQ  EQU   6                   ACCPAK SYSTEM EQUATE                         
MAXSENO  EQU   256                 MAXIMUM N'SYSTEMS                            
MAXCPYS  EQU   190                 MAXIMUM N'COMPANIES PER FILE                 
CPYTMAXN EQU   MAXSENO*MAXCPYS                                                  
                                                                                
ASETAB   DC    A(SETAB)            A(SYSTEM TABLE)                              
ACPYTAB  DC    A(CPYTAB)           A(COMPANY TABLE)                             
ACPYPROT DC    A(CPYPROT)          A(COMPANY DATA TABLE)                        
                                                                                
ICMTAB   DC    AL1(1,3,7,15)       ICM MASKS - INDEXED BY LENGTH                
                                                                                
OPENLIST DC    C'NACCDIR NACCMST NCTFILE X'                                     
CTFILE   DC    C'CTFILE '                                                       
                                                                                
ERROR1   DC    C'** Error - Can''t read company **'                             
CPDCOMM1 DC    CL40'** All companies have setting **'                           
CPDCOMM2 DC    CL40'** No occurrences of data **'                               
                                                                                
TSTTAB   DS    0XL2                ** TABLE OF TEST COMPANIES **                
*&&US                                                                           
         DC    X'06C1'             TCH1                                         
         DC    X'06F9'             SJX                                          
         DC    X'06FE'             DDSZ1                                        
         DC    X'16B7'             ABC                                          
         DC    X'16C1'             TCH2                                         
         DC    X'16E5'             TRC                                          
         DC    X'16F4'             TRC1                                         
         DC    X'16FE'             DDSZ2                                        
         DC    X'17DA'             DDST3                                        
         DC    X'17FE'             DDSZ3                                        
         DC    X'186E'             DWTEST      (SAATCHI)                        
         DC    X'18C1'             TCH4                                         
         DC    X'18FE'             DDSZ4                                        
         DC    X'1964'             TRC2                                         
         DC    X'19DC'             DDST5                                        
         DC    X'19FB'             SBNY                                         
         DC    X'19FE'             DDSZ5                                        
         DC    X'1BFE'             DDSZD                                        
         DC    X'37FE'             DDSZV                                        
         DC    X'66E5'             EDI                                          
         DC    X'66F9'             SJR                                          
         DC    X'66FE'             DDSZ6                                        
         DC    X'76A1'             DDST7                                        
         DC    X'76FE'             DDSZ7                                        
         DC    X'8651'             TCH8                                         
         DC    X'86E6'             SJW                                          
         DC    X'86FE'             DDSZ8                                        
         DC    X'96E2'             DDSZ9                                        
         DC    X'A6CA'             DPS2        (TALENT PARTNERS)                
         DC    X'A6FE'             DDSZA                                        
         DC    X'B6FE'             DDSZB                                        
         DC    X'C6FE'             DDSZQ                                        
         DC    X'D64F'             WTTF        (WESTERN)                        
         DC    X'D6A3'             WITEST      (WESTERN INTERNATIONAL)          
         DC    X'D6E9'             DDSZC                                        
         DC    X'E6AB'             TLSEC                                        
         DC    X'E6C2'             WBIS                                         
         DC    X'E6C4'             WTOVL                                        
         DC    X'E6D2'             WNWOL                                        
         DC    X'E6DA'             XYZ                                          
         DC    X'E6DB'             DDSB                                         
         DC    X'E6EB'             DDS1                                         
         DC    X'E6EC'             DDS2                                         
         DC    X'E6EF'             BLWUTR                                       
         DC    X'E6FE'             DDSZT                                        
*&&                                                                             
*&&UK                                                                           
         DC    X'0677'             DATA1                                        
         DC    X'06E9'             TST1                                         
         DC    X'16F0'             DDS2                                         
         DC    X'16F4'             TRAIN1                                       
         DC    X'17E8'             DDS3                                         
         DC    X'1841'             TST4                                         
         DC    X'197D'             DDS5                                         
         DC    X'2643'             DDS6                                         
         DC    X'2744'             TEST7                                        
         DC    X'2746'             TEST7A                                       
         DC    X'2845'             DDS8                                         
         DC    X'2944'             DDS9                                         
         DC    X'2946'             TST9                                         
         DC    X'2A42'             TSTAMS                                       
         DC    X'2A43'             TSTLBA                                       
         DC    X'2A44'             TSTZEA                                       
         DC    X'2A45'             TSTZEB                                       
         DC    X'2A46'             TSTZEC                                       
         DC    X'2A47'             DDSA                                         
         DC    X'2A48'             TSTZPAY                                      
         DC    X'2A49'             TSTAMV                                       
         DC    X'2B41'             TMT                                          
         DC    X'2B43'             MPT                                          
         DC    X'2B44'             FFMTST                                       
         DC    X'2B46'             MHO                                          
         DC    X'2B4A'             TGALLG                                       
         DC    X'2B4D'             DDSB                                         
         DC    X'2B63'             DOYTST                                       
         DC    X'2B78'             DATA2                                        
         DC    X'2BC1'             TEMPDDB                                      
         DC    X'2BC2'             TEMPSCR                                      
         DC    X'2BC3'             TEMPOM                                       
         DC    X'2C43'             DDSC                                         
         DC    X'2C4B'             DATA4                                        
         DC    X'2DE8'             DDSD                                         
         DC    X'2E41'             DDSE                                         
         DC    X'2E45'             DATA3                                        
         DC    X'2E64'             TEMPOHD                                      
         DC    X'2E99'             MAC                                          
         DC    X'2F41'             DDSF                                         
         DC    X'2F44'             IMLTEST                                      
         DC    X'2F62'             TEMPDEMO                                     
         DC    X'3042'             DDBESS                                       
         DC    X'3043'             DDSG                                         
         DC    X'3044'             DDSTEST                                      
         DC    X'3048'             OMTM                                         
         DC    X'3049'             BBDT                                         
         DC    X'3050'             GFMT                                         
         DC    X'3143'             DDSH                                         
         DC    X'3144'             DDSHOL                                       
         DC    X'3147'             DDSHBV                                       
         DC    X'3160'             YRNTST                                       
         DC    X'31A1'             TSTWD                                        
         DC    X'3243'             DDSI                                         
         DC    X'3343'             DDSJ                                         
         DC    X'3443'             DDSK                                         
         DC    X'3541'             DDSL                                         
         DC    X'3641'             DDSM                                         
         DC    X'3741'             DDSN                                         
         DC    X'3841'             DDSO                                         
         DC    X'384E'             TSTGR                                        
         DC    X'3871'             GERTST                                       
         DC    X'3872'             DDSOTST                                      
         DC    X'3873'             DDSOT2                                       
         DC    X'3941'             DDSP                                         
         DC    X'394F'             TMP                                          
         DC    X'3A99'             TEMPO                                        
         DC    X'3AFE'             DDSQ                                         
         DC    X'3B41'             DDSR                                         
         DC    X'3C41'             DDSS                                         
         DC    X'3C49'             KUKTST                                       
         DC    X'3C60'             NORTST                                       
         DC    X'3D41'             DDST                                         
         DC    X'3D56'             JWTST                                        
         DC    X'3D57'             TEMPO3                                       
         DC    X'3E41'             DDSW                                         
         DC    X'608D'             RILEDI                                       
         DC    X'6641'             DDS1                                         
         DC    X'6642'             TEA96                                        
*&&                                                                             
TSTTABX  DC    AL1(0)                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMPANY ELEMENT DATA TABLE                                          *         
***********************************************************************         
                                                                                
CPYPROT  DS    0X                                                               
                                                                                
         DC    AL1(CPYGLU-CPYELD)                                               
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYGLU)                                                    
         DC    AL1(0)                                                           
         DC    C'CPYGLU  '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYBSL-CPYELD)                                               
         DC    AL1(PROTTNUM)                                                    
         DC    AL1(L'CPYBSL)                                                    
         DC    AL1(0)                                                           
         DC    C'CPYBSL  '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYBANK-CPYELD)                                              
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYBANK)                                                   
         DC    AL1(0)                                                           
         DC    C'CPYBANK '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYPETY-CPYELD)                                              
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYPETY)                                                   
         DC    AL1(0)                                                           
         DC    C'CPYPETY '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYPROD-CPYELD)                                              
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYPROD)                                                   
         DC    AL1(0)                                                           
         DC    C'CPYPROD '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYRECV-CPYELD)                                              
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYRECV)                                                   
         DC    AL1(0)                                                           
         DC    C'CPYRECV '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSUPP-CPYELD)                                              
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYSUPP)                                                   
         DC    AL1(0)                                                           
         DC    C'CPYSUPP '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSUPX-CPYELD)                                              
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYSUPX)                                                   
         DC    AL1(0)                                                           
         DC    C'CPYSUPX '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYPANL-CPYELD)                                              
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYPANL)                                                   
         DC    AL1(0)                                                           
         DC    C'CPYPANL '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYANAL-CPYELD)                                              
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYANAL)                                                   
         DC    AL1(0)                                                           
         DC    C'CPYANAL '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYWRKSI-CPYELD)                                             
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYWRKSI)                                                  
         DC    AL1(0)                                                           
         DC    C'CPYWRKSI'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYCDC-CPYELD)                                               
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYCDC)                                                    
         DC    AL1(CPYCDCNO)                                                    
         DC    C'CPYCDC  '                                                      
         DC    XL16'00'                                                         
*&&US                                                                           
         DC    AL1(CPYXSUPP-CPYELD)                                             
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYXSUPP)                                                  
         DC    AL1(0)                                                           
         DC    C'CPYXSUPP'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYCTFIL-CPYELD)                                             
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYCTFIL)                                                  
         DC    AL1(0)                                                           
         DC    C'CPYCTFIL'                                                      
         DC    XL16'00'                                                         
*&&                                                                             
         DC    AL1(CPYTAX-CPYELD)                                               
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYTAX)                                                    
         DC    AL1(0)                                                           
         DC    C'CPYTAX  '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYTMSSD-CPYELD)                                             
         DC    AL1(PROTTCDT)                                                    
         DC    AL1(L'CPYTMSSD)                                                  
         DC    AL1(0)                                                           
         DC    C'CPYTMSSD'                                                      
         DC    XL16'00'                                                         
*&&US                                                                           
         DC    AL1(CPYTSD-CPYELD)                                               
         DC    AL1(PROTTDAY)                                                    
         DC    AL1(L'CPYTSD)                                                    
         DC    AL1(0)                                                           
         DC    C'CPYTSD  '                                                      
         DC    XL16'00'                                                         
*&&                                                                             
         DC    AL1(CPYOFFC-CPYELD)                                              
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYOFFC)                                                   
         DC    AL1(0)                                                           
         DC    C'CPYOFFC '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYUID-CPYELD)                                               
         DC    AL1(PROTTNUM)                                                    
         DC    AL1(L'CPYUID)                                                    
         DC    AL1(0)                                                           
         DC    C'CPYUID  '                                                      
         DC    XL16'00'                                                         
*                                                                               
         DC    AL1(CPYTCMP-CPYELD)                                              
         DC    AL1(PROTTNUM)                                                    
         DC    AL1(L'CPYTCMP)                                                   
         DC    AL1(0)                                                           
         DC    C'CPYTCMP '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYTENO-CPYELD)                                              
         DC    AL1(PROTTALP)                                                    
         DC    AL1(L'CPYTENO)                                                   
         DC    AL1(0)                                                           
         DC    C'CPYTENO '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYDEPTL-CPYELD)                                             
         DC    AL1(PROTTNUM)                                                    
         DC    AL1(L'CPYDEPTL)                                                  
         DC    AL1(0)                                                           
         DC    C'CPYDEPTL'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSFST-CPYELD)                                              
         DC    AL1(PROTTFMO)                                                    
         DC    AL1(L'CPYSFST)                                                   
         DC    AL1(0)                                                           
         DC    C'CPYSFST '                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT1-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT1)                                                  
         DC    AL1(CPYSIOMR)                                                    
         DC    C'CPYSIOMR'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT1-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT1)                                                  
         DC    AL1(CPYSCIVE)                                                    
         DC    C'CPYSCIVE'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT1-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT1)                                                  
         DC    AL1(CPYSOROE)                                                    
         DC    C'CPYSOROE'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT1-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT1)                                                  
         DC    AL1(CPYSCOST)                                                    
         DC    C'CPYSCOST'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT1-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT1)                                                  
         DC    AL1(CPYSDISC)                                                    
         DC    C'CPYSDISC'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT1-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT1)                                                  
         DC    AL1(CPYSGENA)                                                    
         DC    C'CPYSGENA'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT1-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT1)                                                  
         DC    AL1(CPYSNOJL)                                                    
         DC    C'CPYSNOJL'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT1-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT1)                                                  
         DC    AL1(CPYSNOET)                                                    
         DC    C'CPYSNOET'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT2-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT2)                                                  
         DC    AL1(CPYSETPP)                                                    
         DC    C'CPYSSETP'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT2-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT2)                                                  
         DC    AL1(CPYSETAC)                                                    
         DC    C'CPYSETAC'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT2-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT2)                                                  
         DC    AL1(CPYSEBIF)                                                    
         DC    C'CPYSEBIF'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT2-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT2)                                                  
         DC    AL1(CPYSETDO)                                                    
         DC    C'CPYSETDO'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT2-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT2)                                                  
         DC    AL1(CPYSCKDP)                                                    
         DC    C'CPYSCKDP'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT2-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT2)                                                  
         DC    AL1(CPYSVENR)                                                    
         DC    C'CPYSVENR'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT2-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT2)                                                  
         DC    AL1(CPYSCACA)                                                    
         DC    C'CPYSCACA'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT2-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT2)                                                  
         DC    AL1(CPYSERTP)                                                    
         DC    C'CPYSERTP'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT3-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT3)                                                  
         DC    AL1(CPYSSXCC)                                                    
         DC    C'CPYSSXCC'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT3-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT3)                                                  
         DC    AL1(CPYSWO14)                                                    
         DC    C'CPYSWO14'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT3-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT3)                                                  
         DC    AL1(CPYSPC1C)                                                    
         DC    C'CPYSPC1C'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT3-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT3)                                                  
         DC    AL1(CPYSOPBM)                                                    
         DC    C'CPYSOPBM'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT3-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT3)                                                  
         DC    AL1(CPYSCA22)                                                    
         DC    C'CPYSCA22'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT3-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT3)                                                  
         DC    AL1(CPYSPCSJ)                                                    
         DC    C'CPYSPCSJ'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT3-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT3)                                                  
         DC    AL1(CPYSDPST)                                                    
         DC    C'CPYSDPST'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT3-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT3)                                                  
         DC    AL1(CPYSBSEC)                                                    
         DC    C'CPYSBSEC'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYBSEC-CPYELD)                                              
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYBSEC)                                                   
         DC    AL1(CPYBSSEC)                                                    
         DC    C'CPYBSSEC'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYBSEC-CPYELD)                                              
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYBSEC)                                                   
         DC    AL1(CPYBSOFF)                                                    
         DC    C'CPYBSOFF'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT4-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT4)                                                  
         DC    AL1(CPYSPESK)                                                    
         DC    C'CPYSPESK'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT4-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT4)                                                  
         DC    AL1(CPYSOV12)                                                    
         DC    C'CPYSOV12'                                                      
         DC    XL16'00'                                                         
*&&UK                                                                           
         DC    AL1(CPYSTAT4-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT4)                                                  
         DC    AL1(CPYSICPY)                                                    
         DC    C'CPYSICPY'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT4-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT4)                                                  
         DC    AL1(CPYSGEN2)                                                    
         DC    C'CPYSGEN2'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT4-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT4)                                                  
         DC    AL1(CPYSNPRD)                                                    
         DC    C'CPYSNPRD'                                                      
         DC    XL16'00'                                                         
*&&                                                                             
*&&US                                                                           
         DC    AL1(CPYSTAT4-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT4)                                                  
         DC    AL1(CPYSNPRD)                                                    
         DC    C'CPYSNPRD'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT4-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT4)                                                  
         DC    AL1(CPYSMINT)                                                    
         DC    C'CPYSMINT'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT4-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT4)                                                  
         DC    AL1(CPYSICPY)                                                    
         DC    C'CPYSICPY'                                                      
         DC    XL16'00'                                                         
*&&                                                                             
         DC    AL1(CPYSTAT4-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT4)                                                  
         DC    AL1(CPYSPRAT)                                                    
         DC    C'CPYSPRAT'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT4-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT4)                                                  
         DC    AL1(CPYSIREG)                                                    
         DC    C'CPYSIREG'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT4-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT4)                                                  
         DC    AL1(CPYSOFF2)                                                    
         DC    C'CPYSOFF2'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT5-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT5)                                                  
         DC    AL1(CPYSOFPL)                                                    
         DC    C'CPYSOFPL'                                                      
         DC    XL16'00'                                                         
*&&UK                                                                           
         DC    AL1(CPYSTAT5-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT5)                                                  
         DC    AL1(CPYSNEWF)                                                    
         DC    C'CPYSNEWF'                                                      
         DC    XL16'00'                                                         
*&&                                                                             
         DC    AL1(CPYSTAT5-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT5)                                                  
         DC    AL1(CPYSBAPR)                                                    
         DC    C'CPYSBAPR'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT5-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT5)                                                  
         DC    AL1(CPYSBAPE)                                                    
         DC    C'CPYSBAPE'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT5-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT5)                                                  
         DC    AL1(CPYSNCST)                                                    
         DC    C'CPYSNCST'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT5-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT5)                                                  
         DC    AL1(CPYSEXPP)                                                    
         DC    C'CPYSEXPP'                                                      
         DC    XL16'00'                                                         
*&&US                                                                           
         DC    AL1(CPYSTAT5-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT5)                                                  
         DC    AL1(CPYSVEND)                                                    
         DC    C'CPYSVEND'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT5-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT5)                                                  
         DC    AL1(CPYAPGS)                                                     
         DC    C'CPYAPGS '                                                      
         DC    XL16'00'                                                         
*&&                                                                             
*&&UK                                                                           
         DC    AL1(CPYSTAT5-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT5)                                                  
         DC    AL1(CPYSNLFM)                                                    
         DC    C'CPYSNLFM'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT5-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT5)                                                  
         DC    AL1(CPYSNVAT)                                                    
         DC    C'CPYSNVAT'                                                      
         DC    XL16'00'                                                         
*&&                                                                             
         DC    AL1(CPYSTAT6-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT6)                                                  
         DC    AL1(CPYSADVP)                                                    
         DC    C'CPYSADVP'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT6-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT6)                                                  
         DC    AL1(CPYSRAPP)                                                    
         DC    C'CPYSRAPP'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT6-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT6)                                                  
         DC    AL1(CPYSNO60)                                                    
         DC    C'CPYSNO60'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT6-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT6)                                                  
         DC    AL1(CPYSFTXR)                                                    
         DC    C'CPYSFTXR'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT6-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT6)                                                  
         DC    AL1(CPYSBANY)                                                    
         DC    C'CPYSBANY'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT6-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT6)                                                  
         DC    AL1(CPYSFBIL)                                                    
         DC    C'CPYSFBIL'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT6-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT6)                                                  
         DC    AL1(CPYSFMCR)                                                    
         DC    C'CPYSFMCR'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT6-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT6)                                                  
         DC    AL1(CPYSFOCR)                                                    
         DC    C'CPYSFOCR'                                                      
         DC    XL16'00'                                                         
*&&UK                                                                           
         DC    AL1(CPYSTAT7-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT7)                                                  
         DC    AL1(CPYSAPSP)                                                    
         DC    C'CPYSAPSP'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT7-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT7)                                                  
         DC    AL1(CPYSNEWO)                                                    
         DC    C'CPYSNEWO'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT7-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT7)                                                  
         DC    AL1(CPYSLA2D)                                                    
         DC    C'CPYSLA2D'                                                      
         DC    XL16'00'                                                         
*&&                                                                             
         DC    AL1(CPYSTAT7-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT7)                                                  
         DC    AL1(CPYSAGRP)                                                    
         DC    C'CPYSAGRP'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT7-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT7)                                                  
         DC    AL1(CPYSTMSY)                                                    
         DC    C'CPYSTMSY'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT7-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT7)                                                  
         DC    AL1(CPYSJTIM)                                                    
         DC    C'CPYSJTIM'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT7-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT7)                                                  
         DC    AL1(CPYSL1NA)                                                    
         DC    C'CPYSL1NA'                                                      
         DC    XL16'00'                                                         
*&&US                                                                           
         DC    AL1(CPYSTAT7-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT7)                                                  
         DC    AL1(CPYSNEWB)                                                    
         DC    C'CPYSNEWB'                                                      
         DC    XL16'00'                                                         
*&&                                                                             
         DC    AL1(CPYSTAT8-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT8)                                                  
         DC    AL1(CPYSRLOG)                                                    
         DC    C'CPYSRLOG'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT8-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT8)                                                  
         DC    AL1(CPYSTEST)                                                    
         DC    C'CPYSTEST'                                                      
         DC    XL16'00'                                                         
*&&UK                                                                           
         DC    AL1(CPYSTAT8-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT8)                                                  
         DC    AL1(CPYS1P13)                                                    
         DC    C'CPYS1P13'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT8-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT8)                                                  
         DC    AL1(CPYSTTAX)                                                    
         DC    C'CPYSTTAX'                                                      
         DC    XL16'00'                                                         
*&&                                                                             
         DC    AL1(CPYSTAT8-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT8)                                                  
         DC    AL1(CPYSFNAM)                                                    
         DC    C'CPYSFNAM'                                                      
         DC    XL16'00'                                                         
*&&UK                                                                           
         DC    AL1(CPYSTAT8-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT8)                                                  
         DC    AL1(CPYS1CUT)                                                    
         DC    C'CPYS1CUT'                                                      
         DC    XL16'00'                                                         
*&&                                                                             
         DC    AL1(CPYSTAT8-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT8)                                                  
         DC    AL1(CPYSJBBO)                                                    
         DC    C'CPYSJBBO'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT8-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT8)                                                  
         DC    AL1(CPYSSPDS)                                                    
         DC    C'CPYSSPDS'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT9-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT9)                                                  
         DC    AL1(CPYSBGXT)                                                    
         DC    C'CPYSBGXT'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT9-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT9)                                                  
         DC    AL1(CPYSEDHO)                                                    
         DC    C'CPYSEDHO'                                                      
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(CPYSTAT9-CPYELD)                                             
         DC    AL1(PROTTBIT)                                                    
         DC    AL1(L'CPYSTAT9)                                                  
         DC    AL1(CPYSXDSJ)                                                    
         DC    C'CPYSXDSJ'                                                      
         DC    XL16'00'                                                         
                                                                                
CPYPROTX DC    AL1(PROTEOTQ)                                                    
                                                                                
SETAB    DS    (MAXSENO)XL(SETABL)                                              
CPYTAB   DS    (CPYTMAXN)XL(CPYTABL)                                            
         EJECT                                                                  
WORKD    DSECT                     ** DSECT TO COVER SPACEND **                 
                                                                                
BINPARMS DS    0F                                                               
BINPACTN DS    XL1                                                              
BINPAREC DS    AL3                                                              
BINPATAB DS    A                                                                
BINPNREC DS    F                                                                
BINPLREC DS    F                                                                
BINPDKEY DS    XL1                                                              
BINPLKEY DS    XL3                                                              
BINPMAXN DS    F                                                                
BINPARML EQU   *-BINPARMS                                                       
                                                                                
FILTER   DS    0X                  ** KEYWORD FILTER **                         
FILTKWRD DS    CL(L'PROTKWRD)      FILTER KEYWORD                               
FILTYORN DS    XL1                 FILTER ACTION                                
FILTYQ   EQU   0                   INCLUDE IF FILTER VALUE ON                   
FILTNQ   EQU   1                   INCLUDE IF FILTER VALUE OFF                  
FILTATAB DS    AL4                 A(FILTER TABLE ENTRY)                        
FILTERL  EQU   *-FILTER                                                         
                                                                                
SENUM    DS    X                   LAST SE NUMBER PROCESSED                     
SESE     DS    CL6                 SE#NN/                                       
SENAME   DS    CL(L'SETNAME)       SE NAME                                      
                                                                                
CPYCOUNT DS    PL6                 N'COMPANIES PRINTED                          
                                                                                
IOKEY    DS    XL64                                                             
IO       DS    XL2000                                                           
IO2      DS    XL2000                                                           
                                                                                
CPYTABD  DSECT                     ** COMPANY TABLE **                          
CPYTSE   DS    XL1                 SE NUMBER                                    
CPYTCPY  DS    XL1                 COMPANY NUMBER                               
CPYTLKEY EQU   *-CPYTABD                                                        
CPYTALPH DS    CL2                 AGENCY ALPHA ID                              
CPYTABL  EQU   *-CPYTABD                                                        
                                                                                
SETABD   DSECT                     ** SE TABLE **                               
SETEOTQ  EQU   0                   END OF TABLE INDICATOR                       
SETSE    DS    XL1                 SE NUMBER                                    
SETNAME  DS    CL(L'CTLSTNAM)      SE NAME                                      
SETABL   EQU   *-SETABD                                                         
                                                                                
PROTABD  DSECT                     ** PROFILE TABLE **                          
PROTEOTQ EQU   0                   END OF TABLE INDICATOR                       
PROTDISP DS    XL1                 DISPLACEMENT TO DATA                         
PROTTYPE DS    XL1                 TYPE OF DATA                                 
PROTTALP EQU   1                   ALPHA VALUE                                  
PROTTBIT EQU   2                   BIT VALUE                                    
PROTTNUM EQU   3                   NUMBER                                       
PROTTFMO EQU   4                   FIRST MONTH                                  
PROTTHEX EQU   5                   HEX VALUE                                    
PROTTCDT EQU   6                   COMPRESSED DATE                              
PROTTDAY EQU   7                   DAY (BINARY)                                 
PROTDLEN DS    XL1                 LENGTH OF DATA                               
PROTDVAL DS    0CL1                DEFAULT VALUE (IF PROTTALP ON)               
PROTBVAL DS    XL1                 BIT VALUE     (IF PROTTBIT ON)               
PROTKWRD DS    CL8                 DATA KEYWORD                                 
PROTDICT DS    XL2                 DICTIONARY REFERENCE                         
PROTDATA DS    0XL14                                                            
PROTNCNT DS    XL3                 NOT ON OR DEFAULT COUNT                      
PROTYCNT DS    XL3                 ON OR NOT DEFAULT COUNT                      
         DS    XL8                 N/D                                          
PROTABL  EQU   *-PROTABD                                                        
                                                                                
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
                                                                                
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
                                                                                
         ORG   P                   ** COMPANY ANALYSIS **                       
PCPYBXL  DS    CL1                                                              
PCPYHEX  DS    CL2                 HEX COMPANY CODE                             
PCPYBXC1 DS    CL1                                                              
PCPYALPH DS    CL(L'CPYTALPH)      ALPHA COMPANY CODE                           
PCPYBXC2 DS    CL1                                                              
PCPYNAME DS    CL(L'NAMEREC)       COMPANY NAME                                 
PCPYBXC3 DS    CL1                                                              
PCPYPRID DS    CL10                PRINCIPAL USER-ID                            
PCPYBXC4 DS    CL1                                                              
PCPYPROF DS    CL54                COMPANY PROFILE                              
PCPYBXR  DS    CL1                                                              
                                                                                
         ORG   P                   ** COMPANY DATA ANALYSIS **                  
PCPDBXL  DS    CL1                                                              
PCPDKWRD DS    CL(L'PROTKWRD)                                                   
PCPDBXC1 DS    CL1                                                              
PCPDNCNT DS    CL5                                                              
PCPDBXC2 DS    CL1                                                              
PCPDYCNT DS    CL5                                                              
PCPDBXC3 DS    CL1                                                              
PCPDCOMM DS    CL40                                                             
PCPDBXR  DS    CL1                                                              
                                                                                
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
                                                                                
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
                                                                                
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
                                                                                
CTLSTD   DSECT                                                                  
         ORG   CTLSTDTA                                                         
CTLSTNAM DS    CL7                 SE NAME                                      
CTLSTSYS DS    XL1                 CALLOV SYSTEM NUMBER                         
CTLSTSE  DS    XL1                 SE NUMBER                                    
                                                                                
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
                                                                                
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
                                                                                
BOXCLOSE EQU   C'C'                CLOSE BOX                                    
BOXRTQ   EQU   C'T'                ROW TOP                                      
BOXRMQ   EQU   C'M'                ROW MID                                      
BOXRBQ   EQU   C'B'                ROW BOTTOM                                   
BOXCLQ   EQU   C'L'                COLUMN LEFT                                  
BOXCCQ   EQU   C'C'                COLUMN MID                                   
BOXCRQ   EQU   C'R'                COLUMN RIGHT                                 
                                                                                
         ORG   BOXROWS+03                                                       
BOXRT    DS    CL1                                                              
         ORG   BOXROWS+05                                                       
BOXRM    DS    CL1                                                              
         ORG   BOXROWS+99                                                       
BOXRB    DS    CL1                                                              
                                                                                
         ORG   BOXCOLS+(PCPYBXL-P)                                              
CPYCL    DS    CL1                                                              
         ORG   BOXCOLS+(PCPYBXC1-P)                                             
CPYCC1   DS    CL1                                                              
         ORG   BOXCOLS+(PCPYBXC2-P)                                             
CPYCC2   DS    CL1                                                              
         ORG   BOXCOLS+(PCPYBXC3-P)                                             
CPYCC3   DS    CL1                                                              
         ORG   BOXCOLS+(PCPYBXC4-P)                                             
CPYCC4   DS    CL1                                                              
         ORG   BOXCOLS+(PCPYBXR-P)                                              
CPYCR    DS    CL1                                                              
                                                                                
         ORG   BOXCOLS+(PCPDBXL-P)                                              
CPDCL    DS    CL1                                                              
         ORG   BOXCOLS+(PCPDBXC1-P)                                             
CPDCC1   DS    CL1                                                              
         ORG   BOXCOLS+(PCPDBXC2-P)                                             
CPDCC2   DS    CL1                                                              
         ORG   BOXCOLS+(PCPDBXC3-P)                                             
CPDCC3   DS    CL1                                                              
         ORG   BOXCOLS+(PCPDBXR-P)                                              
CPDCR    DS    CL1                                                              
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPYA02 02/18/04'                                      
         END                                                                    
