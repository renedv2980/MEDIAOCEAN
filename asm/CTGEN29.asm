*          DATA SET CTGEN29    AT LEVEL 027 AS OF 08/29/07                      
*PHASE TA0B29A                                                                  
*                                                                               
         TITLE 'CTGEN29 - ARCHIVE DOCUMENT TYPE LIST (DOCTYPE)'                 
GEN29    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GE29**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CTPDREC,R2          R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     EXIT                APMVALK                                      
         B     EXIT                APMVALR                                      
         B     EXIT                APMDISK                                      
         B     EXIT                APMDISR                                      
         B     EXIT                APMDELR                                      
         B     EXIT                APMRESR                                      
         B     VALSEL              APMVALP                                      
         B     GETSEL              APMGETS                                      
         B     DISSEL              APMDISS                                      
         B     EXIT                APMVALS                                      
         B     EXIT                APMFLST                                      
         B     DISPRO              APMPROC                                      
         B     EXIT                APMFSCR                                      
         B     LSTSCR              APMLSCR                                      
         B     VALREQ              APMVALQ                                      
         B     PRTREP              APMREPP                                      
         B     SETTWA              APMSETT                                      
         B     EXIT                APMPUTK                                      
         B     EXIT                APMNEWK                                      
         B     EXIT                APMFRP                                       
         B     EXIT                APMDISS2                                     
*                                                                               
EXIT     XIT1                                                                   
                                                                                
***********************************************************************         
* ROUTINE TO INITIALIZE TWA                                           *         
***********************************************************************         
SETTWA   XC    SAVOVER(SAVCLRL),SAVOVER                                         
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                                         
***********************************************************************         
VALSEL   LA    R2,APRECKEY         SET UP FIRST LIST RECORD KEY                 
         XC    CTPDKEY,CTPDKEY                                                  
         MVI   CTPDKTYP,CTPDKYPQ   DOCTYPE RECORD X'97'                         
         MVI   CTPDKTY1,CTPDKT1Q   DOCTYPE ONLY RECORD X'01'                    
*                                                                               
         XC    SELKEY(SELKEYL),SELKEY                                           
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTOIDH                                                    
         BNE   VSEL010                                                          
         SR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         STC   R1,SELOIDLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SELOID(0),FVIFLD                                                 
                                                                                
VSEL010  MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTSYSH       FACPAK SYSTEM NAME                           
         BNE   VSEL020                                                          
         MVC   SELSYS,LSTSYS                                                    
*                                                                               
VSEL020  MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTPGMH       FACPAK PROGRAM NAME                          
         BNE   VALSELY                                                          
         SR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         STC   R1,SELPGMLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SELPGM(0),FVIFLD                                                 
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALSELX  XC    APPARM(8),APPARM                                                 
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET NEXT LIST-SELECT RECORD                                                   
***********************************************************************         
GETSEL   LA    R2,IOKEY                                                         
         MVC   CTPDKEY,APRECKEY                                                 
         TM    APINDS,APILRERD     TEST SEQUENCE BROKEN                         
         BZ    GSEL010                                                          
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GSEL020                                                          
         B     GSELN                                                            
GSEL010  TM    APINDS,APILNSEQ     TEST READ OR READ HIGH                       
         BNZ   GSEL020                                                          
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GSEL020  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GSELN                                                            
         L     R2,AIOAREA1                                                      
*                                                                               
         CLI   CTPDKTYP,CTPDKYPQ   DOCTYPE RECORD X'97'                         
         BNE   GSELN                                                            
         CLI   CTPDKTY1,CTPDKT1Q   DOCTYPE ONLY RECORD X'01'                    
         BNE   GSELN                                                            
*                                                                               
         LA    R3,CTPDDATA                                                      
         USING CTDTYD,R3                                                        
*                                                                               
GSEL030  CLI   CTDTYEL,0                                                        
         BE    GSELK                                                            
         CLI   CTDTYEL,CTDTYELQ                                                 
         BE    GSEL040                                                          
         SR    RF,RF                                                            
         IC    RF,CTDTYLEN                                                      
         AR    R3,RF                                                            
         B     GSEL030                                                          
*                                                                               
GSEL040  CLI   SELOIDLN,0                                                       
         BE    GSEL050                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SELOIDLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELOID(0),CTDTYIDA                                               
         BNE   GSEL020                                                          
*                                                                               
GSEL050  CLI   SELSYS,0                                                         
         BE    GSEL060                                                          
         CLC   SELSYS,CTDTYSYS                                                  
         BNE   GSEL020                                                          
*                                                                               
GSEL060  CLI   SELPGMLN,0                                                       
         BE    GSELK                                                            
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SELPGMLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELPGM(0),CTDTYPRG                                               
         BNE   GSEL020                                                          
*                                                                               
GSELK    DS    0H                  RECORD SELECTED                              
*                                                                               
GSELY    MVC   APRECKEY(L'CTPDKEY),CTPDKEY                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GSELX                                                            
*                                                                               
GSELN    MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GSELX    B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                                      
***********************************************************************         
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                                                               
         EDIT  CTPDKDTY,LISTDTY,DUB=APDUB,WRK=APWORK                            
*                                                                               
         LA    R3,CTPDDATA                                                      
         USING CTDTYD,R3                                                        
*                                                                               
DSEL010  CLI   CTDTYEL,0                                                        
         BE    DSELX                                                            
         CLI   CTDTYEL,CTDTYELQ                                                 
         BE    DSEL020                                                          
         SR    RF,RF                                                            
         IC    RF,CTDTYLEN                                                      
         AR    R3,RF                                                            
         B     DSEL010                                                          
*                                                                               
DSEL020  MVC   LISTOID,CTDTYIDA                                                 
         MVC   LISTSYS,CTDTYSYS                                                 
         MVC   LISTPGM,CTDTYPRG                                                 
*                                                                               
DSELX    B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* DISPLAY PROFILE RECORD ON SELECT                                              
***********************************************************************         
DISPRO   L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
         PACK  APDUB,LISTDTY                                                    
         CVB   R1,APDUB                                                         
         ST    R1,APFULL                                                        
         XC    APWORK(5),APWORK                                                 
*                                                                               
         MVC   IOKEY,APRECKEY                                                   
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   DPROX                                                            
*                                                                               
         L     R2,AIOAREA1                                                      
         LA    R3,CTPDDATA                                                      
         USING CTDTYD,R3                                                        
*                                                                               
PR       USING CTPREC,APRECKEY                                                  
         CLI   PR.CTPKTYP,CTPKTYPQ       ALREADY HAVE PROFILE RECORD?           
         BE    DPRO030                   . YES, THEN DISPLAY IT                 
*                                                                               
         XC    APRECKEY,APRECKEY                                                
DPRO010  CLI   CTDTYEL,0                 PULL DETAIL FROM DOCTYPE REC           
         BE    DPROX                      FOR PROFILE RECORD                    
         CLI   CTDTYEL,CTDTYELQ                                                 
         BE    DPRO020                                                          
         SR    RF,RF                                                            
         IC    RF,CTDTYLEN                                                      
         AR    R3,RF                                                            
         B     DPRO010                                                          
*                                                                               
DPRO020  MVI   PR.CTPKTYP,CTPKTYPQ                                              
         MVC   PR.CTPKSYS,CTDTYSYS                                              
         MVC   PR.CTPKPROG,CTDTYPRG                                             
         MVC   PR.CTPKORIG,CTDTYIDN                                             
         MVC   IOKEY,APRECKEY                                                   
*                                                                               
         GOTO1 AIO,IOCONFIL+IORD+IO1     READ FOR PROFILE RECORD                
         BNE   ERNF                                                             
         DROP  R3                                                               
*                                                                               
DPRO030  LA    R3,CTPDDATA                                                      
         USING CTADTD,R3                                                        
DPRO032  CLI   CTADTEL,0                                                        
         BE    DPROX                                                            
         CLI   CTADTEL,CTADTELQ                                                 
         BE    DPRO040                                                          
DPRO034  SR    RF,RF                                                            
         IC    RF,CTADTLEN                                                      
         AR    R3,RF                                                            
         B     DPRO032                                                          
*                                                                               
DPRO040  CLC   CTADTCOD,APFULL                                                  
         BNE   DPRO034                                                          
         MVC   APWORK(4),CTADTTYP                                               
         DROP  R3                                                               
*                                                                               
DPROX    B     EXIT                                                             
         DROP  PR                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                                     
***********************************************************************         
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
VALREQ   L     R9,AREP                                                          
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
         GOTO1 AVALWHEN,REPWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALDEST,REPDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALOTYP,REPOTYPH   VALIDATE OUTPUT TYPE                         
         BNE   VALREQX                                                          
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPFORH       VALIDATE FORMAT                              
         BNE   VREQ005                                                          
         CLC   FVIFLD(3),=C'XML'                                                
         BNE   EINV                                                             
*                                                                               
VREQ005  LA    R2,APRECKEY         SET UP FIRST LIST RECORD KEY                 
         XC    CTPDKEY,CTPDKEY                                                  
         MVI   CTPDKTYP,CTPDKYPQ   DOCTYPE RECORD X'97'                         
         MVI   CTPDKTY1,CTPDKT1Q   DOCTYPE ONLY RECORD X'01'                    
*                                                                               
         XC    SELKEY(SELKEYL),SELKEY                                           
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPOIDH                                                    
         BNE   VREQ010                                                          
         SR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         STC   R1,SELOIDLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SELOID(0),FVIFLD                                                 
*                                                                               
VREQ010  MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPSYSH       FACPAK SYSTEM NAME                           
         BNE   VREQ020                                                          
         MVC   SELSYS,REPSYS                                                    
                                                                                
VREQ020  MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPPGMH       FACPAK PROGRAM NAME                          
         BNE   VREQ100                                                          
         SR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         STC   R1,SELPGMLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SELPGM(0),FVIFLD                                                 
*                                                                               
VREQ100  MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO PRINT EASY LINK STATION RECORD REPORT                    *         
***********************************************************************         
PRTREP   L     R9,AREP                                                          
         LA    R2,IOKEY                                                         
*                                                                               
         CLI   REPFOR,C'X'               XML REPORT TYPE                        
         BNE   PREP04                                                           
         MVC   REPP1(L'XMLHDR1),XMLHDR1                                         
         GOTO1 VREPORT,REPD                                                     
         MVC   REPP1(L'XMLHDR2),XMLHDR2                                         
         GOTO1 VREPORT,REPD                                                     
         B     PREP05                                                           
*                                                                               
PREP04   MVC   REPHDTY(10),=CL10'DOCTYPE'                                       
         MVC   REPHSYS(3),=CL3'SYS'                                             
         MVC   REPHPGM(3),=CL3'PGM'                                             
         MVC   REPHOID(10),=CL10'ORIGIN ID'                                     
         GOTO1 VREPORT,REPD                                                     
*                                                                               
         MVC   REPHDTY(10),=CL10'-------'                                       
         MVC   REPHSYS(3),=CL3'---'                                             
         MVC   REPHPGM(3),=CL3'---'                                             
         MVC   REPHOID(10),=CL10'---------'                                     
         GOTO1 VREPORT,REPD                                                     
*                                                                               
PREP05   MVC   CTPDKEY,APRECKEY                                                 
         GOTO1 AIO,IOCONFIL+IOHI+IO1                                            
         B     PREP011                                                          
PREP010  GOTO1 AIO,IOCONFIL+IOSQ+IO1                                            
PREP011  BNE   PREP200                                                          
*                                                                               
         CLI   REPFOR,C'X'               XML REPORT TYPE                        
         BNE   *+8                                                              
         MVI   REPLINE,1                                                        
*                                                                               
         L     R2,AIOAREA1                                                      
         CLI   CTPDKTYP,CTPDKYPQ         DOCTYPE RECORD X'97'                   
         BNE   PREP200                                                          
         CLI   CTPDKTY1,CTPDKT1Q         DOCTYPE ONLY RECORD X'01'              
         BNE   PREP200                                                          
*                                                                               
         LA    R3,CTPDDATA                                                      
         USING CTDTYD,R3                                                        
*                                                                               
PREP030  CLI   CTDTYEL,0                                                        
         BE    PREP010                                                          
         CLI   CTDTYEL,CTDTYELQ                                                 
         BE    PREP040                                                          
         SR    RF,RF                                                            
         IC    RF,CTDTYLEN                                                      
         AR    R3,RF                                                            
         B     PREP030                                                          
*                                                                               
PREP040  CLI   SELOIDLN,0                                                       
         BE    PREP050                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SELOIDLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELOID(0),CTDTYIDA                                               
         BNE   PREP010                                                          
*                                                                               
PREP050  CLI   SELSYS,0                                                         
         BE    PREP060                                                          
         CLC   SELSYS,CTDTYSYS                                                  
         BNE   PREP010                                                          
*                                                                               
PREP060  CLI   SELPGMLN,0                                                       
         BE    PREP100                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SELPGMLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELPGM(0),CTDTYPRG                                               
         BNE   PREP010                                                          
*                                                                               
PREP100  CLI   REPFOR,C'X'               XML REPORT TYPE                        
         BE    PREP110                                                          
         MVC   REPOSYS,CTDTYSYS                                                 
         MVC   REPOPGM,CTDTYPRG                                                 
         MVC   REPOOID,CTDTYIDA                                                 
         EDIT  CTPDKDTY,REPODTY,DUB=APDUB,WRK=APWORK                            
         GOTO1 VREPORT,REPD                                                     
         B     PREP010                                                          
*                                                                               
PREP110  MVC   REPP1(L'XMLUILH),XMLUILH  XML USER ID LINE                       
         MVC   REPP1+L'XMLUILH(L'CTDTYIDA),CTDTYIDA                             
         LA    R1,REPP1+L'XMLUILH                                               
         LA    R4,REPP1+(L'XMLUILH+L'CTDTYIDA-1)                                
         BAS   RE,PREPBKSP                                                      
         MVC   0(L'XMLUILT,R4),XMLUILT                                          
*                                                                               
         MVC   REPP2(L'XMLDDSH),XMLDDSH  XML DDSTYPE LINE                       
         MVC   REPP2+L'XMLDDSH(L'CTDTYIDA),CTDTYIDA                             
         LA    R1,REPP2+L'XMLDDSH                                               
         LA    R4,REPP2+(L'XMLDDSH+L'CTDTYIDA-1)                                
         BAS   RE,PREPBKSP                                                      
         MVI   0(R4),C' '                                                       
         MVC   1(1,R4),CTDTYSYS                                                 
         MVC   2(2,R4),CTDTYPRG                                                 
         MVC   4(L'XMLDDST,R4),XMLDDST                                          
*                                                                               
         MVC   REPP3(L'XMLDOCH),XMLDOCH  XML DOCID LINE                         
         LA    R4,REPP3+L'XMLDOCH                                               
         EDIT  CTPDKDTY,(8,(R4)),DUB=APDUB,WRK=APWORK,ALIGN=LEFT                
         LA    R1,REPP3+L'XMLDOCH                                               
         LA    R4,REPP3+(L'XMLDOCH+8-1)                                         
         BAS   RE,PREPBKSP                                                      
         MVC   0(L'XMLDOCT,R4),XMLDOCT                                          
*                                                                               
         MVC   REPP4(L'XMLUIDT),XMLUIDT                                         
         GOTO1 VREPORT,REPD                                                     
         B     PREP010                                                          
*                                                                               
PREP200  CLI   REPFOR,C'X'               XML REPORT TYPE                        
         BNE   PRTREPX                                                          
         MVC   REPP1(L'XMLTRL),XMLTRL                                           
         GOTO1 VREPORT,REPD                                                     
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
*                                                                               
*----------------------------------------                                       
* BACK UP PAST SPACES/ZEROS                                                     
*----------------------------------------                                       
PREPBKSP CLI   0(R4),C' '                                                       
         BH    *+16                                                             
         BCTR  R4,0                                                             
         CR    R4,R1                                                            
         BL    PREP010                                                          
         B     PREPBKSP                                                         
         LA    R4,1(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ERROR EXITS                                                                   
***********************************************************************         
EINV     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
*                                                                               
EIIS     MVC   FVMSGNO,=AL2(FVFISYS)                                            
         B     NO                  INCOMPATIBLE SYSTEM SPECIFICATION            
*                                                                               
ERNF     MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     NO                  RECORD NOT FOUND                             
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CTFILE   DC    CL8'CTFILE  '                                                    
*                                                                               
REPDESCL DC    C'ARCHIVE DOCUMENT TYPE LIST'                                    
*                                                                               
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'ARCHIVE DOCUMENT TYPE LIST'                              
         SPEC  H2,57,C'--------------------------'                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
*                                                                               
* XML DOCUMENT CONSTANTS                                                        
*                                                                               
XMLHDR1  DC    C'  <?XML VERSION="1.0" ?>'                                      
XMLHDR2  DC    C'- <DOCTYPES>'                                                  
*                                                                               
XMLUILH  DC    C'- <USER ID="'                                                  
XMLUID   DS    0C                        ORIGIN (USER) ID                       
XMLUILT  DC    C'">'                                                            
*                                                                               
XMLDDSH  DC    C'  <DDSTYPE>'                                                   
XMLDDS   DS    0C                        USER-ID/REPORT TYPE (SYS+REP)          
XMLDDST  DC    C'</DDSTYPE>'                                                    
*                                                                               
XMLDOCH  DC    C'  <DOCID>'                                                     
XMLDOC   DS    0C                        ARCHIVE DOCUMENT # (DOCTYPE)           
XMLDOCT  DC    C'</DOCID>'                                                      
*                                                                               
XMLUIDT  DC    C'  </USER>'                                                     
*                                                                               
XMLTRL   DC    C'  </DOCTYPES>'                                                 
*                                                                               
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
* SCREENS/SAVED STORAGE                                                         
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGEN9DD          REPORT SCREEN                                
         ORG   GENTABH                                                          
       ++INCLUDE CTGEN9CD          LIST SCREEN                                  
*                                                                               
         ORG   SAVOVER                                                          
         DS    0D                                                               
SELKEY   DS    0X                  SELECT KEY PARAMETERS                        
SELOIDLN DS    XL1                                                              
SELOID   DS    XL8                 ORIGIN ID                                    
SELSYS   DS    XL1                 SYSTEM                                       
SELPGMLN DS    XL1                                                              
SELPGM   DS    CL2                 PROGRAM                                      
SELKEYL  EQU   *-SELKEY                                                         
*                                                                               
SAVCLRL  EQU   *-SAVOVER                                                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* LIST/SELECT LINE LAYOUT                                                       
***********************************************************************         
LISTD    DSECT                                                                  
LISTACTH DS    XL8                                                              
LISTACT  DS    CL(L'LSTACT1)       ACTION FIELD                                 
LISTLINH DS    XL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTDTY  DS    CL10                ARCHIVE DOCUMENT TYPE                        
         DS    CL2                                                              
LISTOID  DS    CL10                ORIGIN USER ID                               
         DS    CL3                                                              
LISTSYS  DS    CL1                 SYSTEM LETTER                                
         DS    CL4                                                              
LISTPGM  DS    CL2                 PROGRAM LETTERS                              
         ORG   LISTLIN+L'LISTLIN                                                
                                                                                
***********************************************************************         
* PRINT LINE LAYOUT                                                             
***********************************************************************         
REPD     DSECT                                                                  
         ORG   REPP1                                                            
REPODTY  DS    CL10                ARCHIVE DOCUMENT TYPE                        
         DS    CL4                                                              
REPOSYS  DS    CL1                 SYSTEM LETTER                                
         DS    CL4                                                              
REPOPGM  DS    CL2                 PROGRAM LETTERS                              
         DS    CL4                                                              
REPOOID  DS    CL10                ORIGIN USER ID                               
         DS    CL2                                                              
*                                                                               
         ORG   REPP1                                                            
REPHDTY  DS    CL10                DOCTYPE                                      
         DS    CL3                                                              
REPHSYS  DS    CL3                 SYS                                          
         DS    CL3                                                              
REPHPGM  DS    CL3                 PGM                                          
         DS    CL3                                                              
REPHOID  DS    CL10                ORIGIN ID                                    
         DS    CL2                                                              
                                                                                
***********************************************************************         
* DSECT TO COVER LOCAL W/S                                                      
***********************************************************************         
LOCALD   DSECT                                                                  
PARMS    DS    8F                                                               
*                                                                               
LOCALX   EQU   *                   END OF LOCAL WORKING STORAGE                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027CTGEN29   08/29/07'                                      
         END                                                                    
