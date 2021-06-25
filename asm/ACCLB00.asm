*          DATA SET ACCLB00    AT LEVEL 177 AS OF 08/08/01                      
*PHASE T62100A                                                                  
*INCLUDE ACJOBCOL                                                               
*INCLUDE ACGENINI                                                               
*INCLUDE ACSRCHC                                                                
*INCLUDE SRCHCALL                                                               
*INCLUDE AMTVALA                                                                
*INCLUDE BMONVAL                                                                
*INCLUDE CATCALL                                                                
*INCLUDE CONVMOS                                                                
*INCLUDE VATICAN                                                                
*INCLUDE EXCEL                                                                  
*&&US                                                                           
*INCLUDE PRORATA                                                                
*INCLUDE CONVERT                                                                
*&&                                                                             
CLB00    TITLE '- BILL PROGRAM ROOT'                                            
CLB00    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,**CLB0**,R8,R7,R6,CLEAR=YES,RR=RE                          
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     L     RF,=V(ACGENINI)                                                  
         AR    RF,RE                                                            
         GOTO1 (RF),BCPARM,ADDRS2,BSDICTL-BSDICT,C'CB1 '                        
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         BNL   INIT00                                                           
         CLC   BCPARM+12(2),=C'  '    A LEDGER NOT SET UP?                      
         BNH   SETMSG0                                                          
         MVC   FVXTRA(2),BCPARM+12                                              
         MVC   FVMSGNO,=AL2(AE$LDGNF)                                           
         B     SETMSG0                                                          
*                                                                               
INIT00   BE    INIT02                                                           
*                                                                               
*&&UK                                                                           
         XC    BCWORK,BCWORK       IF 1ST TIME READ POSTMAN PROFILE             
         MVI   BCWORK+00,C'A'-X'40'                                             
         MVC   BCWORK+01(3),=C'PM1'                                             
         MVC   BCWORK+12(2),TWAAGY                                              
         GOTO1 VGETPROF,BCPARM,BCWORK,BCWORK+48,VDMGR                           
         CLI   BCWORK+50,C'Y'                                                   
         BNE   *+8                                                              
         OI    BCAPINDS,BCAPICHR   COMMISSION SCIEL IS CHARGE, NOT COST         
*&&                                                                             
*                                                                               
INIT02   MVI   BCOVSYS,X'06'                                                    
         MVI   BCPRGNO,X'21'                                                    
         MVC   BCDSPOPT,=AL2(BASOPTH-TWAD)                                      
         MVC   BCDSPOVR,=AL2(BASOLAYH-TWAD)                                     
         MVC   BCDSPACT,=AL2(BASACTH-TWAD)                                      
         MVC   BCDSPSCR,=AL2(BASSCRH-TWAD)                                      
         MVC   BCDSPREP,=AL2(BASREPH-TWAD)                                      
         MVC   BCDSPSAV,=AL2(BASOPTH-TWAD)                                      
         MVI   BCMIXLEN,MIXTABL2                                                
         MVI   BCGLOB1,BCGLXGOB+BCGLXOV2+BCGLNING                               
         MVI   BCHELPSO,HELPSCRN                                                
         CLI   CUCTRY,CTRYUSA      NO VAT IN USA                                
         BE    *+8                                                              
         OI    BCGLOB1,BCGLVAT                                                  
         CLI   CUCTRY,CTRYCAN      PST ONLY IN CANADA                           
         BNE   *+8                                                              
         OI    BCGLOB1,BCGLPST                                                  
*&&US                                                                           
         L     RE,=V(PRORATA)                                                   
         A     RE,BCRELO                                                        
         ST    RE,VPRORATA                                                      
         L     RE,=V(CONVERT)                                                   
         A     RE,BCRELO                                                        
         ST    RE,VCONVERT                                                      
*&&                                                                             
         L     RF,AOFFBLK          SET OFFAL USING CONVERTED RECORDS            
         OI    OFFACTRL-OFFALD(RF),OFFACCNV                                     
*                                                                               
         L     R5,=A(LSVALS-WORKD) SET A(LIST SUB-CONTROLLER VALUES)            
         LA    R5,WORKD(R5)                                                     
         ST    R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         LA    R0,LSVALSD          COPY FROM TWA INTO W/S                       
         LH    RE,=Y(LSSAVE-TWAD)                                               
         LA    RE,TWAD(RE)                                                      
         LA    R1,LSSAVEL                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   AREP,AWORKX         SET A(REPORT BLOCK)                          
         L     R1,AREP                                                          
         USING REPD,R1                                                          
         MVC   REPACOM,ACOM        INITIALIZE REPBLK VALUES                     
         LA    R0,REPHS                                                         
         ST    R0,REPABUF                                                       
         MVI   REPHEADN,REPHN                                                   
         MVI   REPMIDSN,REPMN                                                   
         MVI   REPPRNTN,REPPN                                                   
         MVI   REPFOOTN,REPFN                                                   
         OI    REPIND2,REPILOW                                                  
         OI    REPHEADI,REPHSPAC                                                
         MVC   REPDATE,ASBDAT                                                   
         MVC   REPAPQB,ATIA                                                     
         MVC   REPSYSID,=C'AC'                                                  
         MVC   REPPRGID,=C'NB'                                                  
         MVC   REPRLH,=Y(48)                                                    
         MVC   REPRDH,=Y(12)                                                    
         DROP  R1                                                               
*                                                                               
         L     R2,=A(ROUTAB)                                                    
         A     R2,BCRELO                                                        
         USING ROUTABD,R2                                                       
         MVI   BCBYTE1,0                                                        
         XR    R3,R3                                                            
INIT04   ICM   R3,3,ROUTDSP        TEST END OF TABLE                            
         BZ    INIT08                                                           
         CLC   ROUTOLY,BCBYTE1     TEST CHANGE OF OVERLAY                       
         BE    INIT06                                                           
         GOTO1 VCOLY,BCPARM,(ROUTOLY,0),0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   BCFULL,0(R1)        SAVE A(ROUTINE MODULE)                       
         MVC   BCBYTE1,ROUTOLY                                                  
INIT06   LA    RF,WORKD(R3)        RF=A(ROUTINE ADDRESS)                        
         MVC   0(1,RF),ROUTNUM                                                  
         MVC   1(3,RF),BCFULL+1                                                 
         LA    R2,ROUTABL(R2)                                                   
         B     INIT04                                                           
         DROP  R2                                                               
*                                                                               
INIT08   L     RF,AGOPBLK          ENSURE GETOPT RE-INITIALISES BUFFER          
         XC    0(GOADM-GOBLOCK,RF),0(RF)                                        
         L     RF,AIO1                                                          
         XC    0(L'LDGKEY,RF),0(RF)                                             
         MVC   0(L'CUABIN,RF),CUABIN                                            
         MVC   L'CUABIN(L'CPYPROD,RF),BCCPYEL+(CPYPROD-CPYELD)                  
         L     RF,AGOPBLK                                                       
         L     R2,AJOBBLK                                                       
         STCM  R2,15,GOABEXT-GOBLOCK(RF)                                        
         OI    GOABEXT-GOBLOCK(RF),GOABXOX                                      
         GOTO1 AGETOPT,BODMCB,AIO1                                              
*&&UK                                                                           
         CLC   GOINCAC-GOBBLOCK(L'GOINCAC,R2),BCSPACES                          
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MINCA)                                           
         B     SETMSG0                                                          
*&&                                                                             
*                                                                               
INITX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PFKEY                                                      *         
***********************************************************************         
         SPACE 1                                                                
VALPFK   MVC   BCWORK(L'CSRECACT),CSRECACT                                      
         TM    BCINDS1,BCIANYPF    TEST USER ENTERED PFKEY THIS TIME            
         BNZ   VALPFK02                                                         
         CLI   CSNEXTPF,0          TEST NEXT TIME PFKEY SET                     
         BE    VALPFKX                                                          
         MVC   BCPFKEY,CSNEXTPF    SET NEXT TIME PFKEY & PROCESS                
         MVI   CSNEXTPF,0          RESET NEXT TIME PFKEY                        
         B     VALPFK04                                                         
*                                                                               
VALPFK02 CLI   BCWORK+1,0                                                       
         BNE   *+16                                                             
         MVI   BCWORK+0,FF                                                      
         MVI   BCWORK+1,FF                                                      
         B     VALPFK04                                                         
*    ??  CLI   CSACT,ACTHELP                                                    
*    ??  BE    *+12                                                             
         TM    BASACTH+FHIID,FHIIVA                                             
         BZ    VALPFKX                                                          
*                                                                               
VALPFK04 CLI   BCPFKEY,PFKMOREQ    TEST SPECIAL MORE PFKEY                      
         BNE   VALPFK10                                                         
         CLI   CSPF#LIN,1          INVALID IF ONLY 1 PF LINE                    
         BE    VALPFKN                                                          
         IC    RE,CSPFLIN#         INCREMENT PF LINE #                          
         LA    RE,1(RE)                                                         
         STC   RE,CSPFLIN#                                                      
         CLC   CSPFLIN#,CSPF#LIN                                                
         BL    *+8                                                              
         MVI   CSPFLIN#,0                                                       
         OI    FVCURIND,FVCKEEP    KEEP CURSOR & MESSAGE                        
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         B     SETMSG                                                           
*                                                                               
VALPFK10 L     R2,APFKTAB                                                       
         USING PFKTABD,R2          R2=A(PFKEY TABLE)                            
         XR    RF,RF                                                            
VALPFK12 CLI   PFKTABD,EOT         TEST END OF TABLE                            
         BE    VALPFKX                                                          
         CLC   PFKKEY,BCWORK       MATCH ON HEADER RECORD/ACTION                
         BE    VALPFK20                                                         
         ICM   RF,3,PFKLEN                                                      
         BXH   R2,RF,VALPFK12      POINT TO NEXT SUB-TABLE HEADER               
VALPFK20 LA    R2,PFKHEADL(R2)     BUMP TO FIRST DATA ENTRY                     
*                                                                               
VALPFK22 CLI   PFKTABD,EOT         TEST END OF TABLE                            
         BE    VALPFKN                                                          
         CLC   PFKNUMB,BCPFKEY     MATCH ON PFKEY NUMBER                        
         BNE   VALPFK28                                                         
         GOTO1 ATSTPFK,PFKTABD                                                  
         BE    *+12                                                             
VALPFK28 LA    R2,PFKDATBL(R2)     POINT TO NEXT DATA ENTRY                     
         B     VALPFK22                                                         
         ST    R2,APFKNTRY         SAVE A(PFK TABLE ENTRY)                      
*                                                                               
         TM    PFKINDS1,PFKIAPPL+PFKIKAPA  TEST APPLICATION ACTION              
         BNZ   VALPFKX                                                          
         TM    PFKINDS1,PFKIACTN   TEST FOR RECORD/ACTION                       
         BNZ   PFKACT                                                           
         TM    PFKINDS1,PFKISCRL   TEST FOR SCROLLING                           
         BNZ   PFKSCR                                                           
         TM    PFKINDS1,PFKIQUIT+PFKINEXT  TEST FOR QUIT/NEXT                   
         BNZ   PFKQUI                                                           
         TM    PFKINDS3,PFKIPOS    TEST FOR POSITIONAL PFKEY                    
         BNZ   PFKPOS                                                           
         B     VALPFKX                                                          
         SPACE 1                                                                
***********************************************************************         
* - SCROLL PFKEY                                                      *         
***********************************************************************         
         SPACE 1                                                                
PFKSCR   MVC   BCSCROLL,PFKINDS2   SAVE SCROLL INDICATORS                       
         B     VALPFKX                                                          
         SPACE 1                                                                
***********************************************************************         
* - QUIT/NEXT PFKEY                                                   *         
***********************************************************************         
         SPACE 1                                                                
PFKQUI   GOTO1 AXITSES             RESTORE PREVIOUS SESSION                     
         TM    TWAINDS1,TWAIXITS   TEST XITSES ISSUED                           
         BZ    PFKQUI02                                                         
         NI    TWAINDS1,FF-TWAIXITS                                             
         B     GO10                CALL SUB-CONTROLLER                          
PFKQUI02 GOTO1 ARECACT,CSREC       RESTORE RECORD/ACTION                        
         B     SETMSG              EXIT TO USER                                 
         SPACE 1                                                                
***********************************************************************         
* - POSITIONAL PFKEY                                                  *         
***********************************************************************         
         SPACE 1                                                                
PFKPOS   XR    R1,R1                                                            
         ICM   R1,3,CSCURDSP       R1=CURSOR DATA SCREEN ADDRESS                
         LH    RF,CSHEDAD                                                       
         LA    RF,2*80(RF)         RF=DATA SCREEN ADDRESS OF 1ST LINE           
         SR    R1,RF                                                            
         BNP   PFKPOS04                                                         
         XR    R0,R0                                                            
         LA    RF,80                                                            
         DR    R0,RF               R1=LIST LINE NUMBER                          
         CLM   R1,1,CSLSTNUM                                                    
         BNL   PFKPOS04                                                         
         MH    R1,CSLINLEN                                                      
         AH    R1,CS1STLIN                                                      
         LA    R3,TWAD(R1)                                                      
         USING SUBINPD,R3          R3=A(SUB-ACTION INPUT FIELD)                 
         CLI   SUBINPH+FHLND,SUBINPL                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BCHALF(1),LSMIXLST+(MIXLSEL#-MIXLST)                             
         MVI   BCHALF+1,0                                                       
         GOTO1 ASETSEL,BCHALF                                                   
         L     R1,AOVERSEL                                                      
         USING SELTABD,R1          R1=A(SELECT TABLE ENTRY)                     
PFKPOS02 CLI   SELTABD,0                                                        
         BE    PFKPOS04                                                         
         CLC   SELTRECA,PFKRECA    MATCH ON RECORD/ACTION                       
         BE    *+12                                                             
         LA    R1,SELTABL(R1)                                                   
         B     PFKPOS02                                                         
         XR    RF,RF                                                            
         ICM   RF,3,SELTDSPN       COPY ACTION WORD TO SUB-ACT INPUT            
         LA    RF,TWAD(RF)                                                      
         MVC   BCBYTE1,SUBINP                                                   
         MVC   SUBINP,0(RF)                                                     
         CLI   BCBYTE1,C'+'                                                     
         BE    *+12                                                             
         CLI   BCBYTE1,C'&&'                                                    
         BNE   *+10                                                             
         MVC   SUBINP+L'SUBINP-1(1),BCBYTE1                                     
         MVI   SUBINPH+FHILD,L'SUBINP                                           
         OI    SUBINPH+FHOID,FHOITR                                             
         B     VALPFKX                                                          
         DROP  R1,R3                                                            
PFKPOS04 OI    FVCURIND,FVCKEEP    KEEP CURSOR & MESSAGE                        
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         B     SETMSG                                                           
         SPACE 1                                                                
***********************************************************************         
* - PFKEY FOR NEW ACTION                                              *         
***********************************************************************         
         SPACE 1                                                                
PFKACT   L     RF,AMIXNTRY                                                      
         TM    PFKINDS2,PFKISAVS   TEST ENTER NEW SESSION                       
         BNZ   *+12                                                             
         OI    BCINDS1,BCINREC+BCINACT                                          
         B     PFKACT08                                                         
*                                                                               
         TM    TWAINDS1,TWAIOHIP   TEST OPTION HELP IN PROGRESS                 
         BZ    PFKACT04                                                         
         NI    TWAINDS1,FF-(TWAIOHIP+TWAIOHRE)                                  
         GOTO1 AXITSES             EXIT FROM HELP SESSION                       
         CLC   BCQUEST,BASOPT      REMOVE THE ? IF NECESSARY                    
         BNE   PFKACT04                                                         
         MVC   BASOPT(L'TWAOSAVE),TWAOSAVE                                      
         OI    BASOPTH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
PFKACT04 SR    R1,R1                                                            
         CLI   CSQRTN,0            TEST RETURN ROUTINE FROM QUIT                
         BE    PFKACT06                                                         
         LA    R1,BCWORK                                                        
         USING SELTPARM,R1                                                      
         XC    SELTPARM,SELTPARM                                                
         MVC   SELTRTN,CSQRTN      SET RETURN ROUTINE FROM QUIT                 
         DROP  R1                                                               
*                                                                               
PFKACT06 GOTO1 ANTRSES,(R1)        ENTER NEW RECORD/ACTION SESSION              
*                                                                               
PFKACT08 MVC   CSINDSL,PFKCSIL     SET NEXT SESSION INDICATORS                  
         MVC   CSNEXTPF,PFKNEXT    SET NEXT TIME AUTO PFKEY                     
         MVI   BCPFKEY,0                                                        
         NI    BCINDS1,FF-BCIANYPF                                              
         GOTO1 ARECACT,PFKRECN                                                  
         B     VALPFK                                                           
         SPACE 1                                                                
***********************************************************************         
* - PFKEY IS INVALID                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALPFKN  CLI   CSNEXTPF,0          TEST NEXT TIME PFKEY SET                     
         BE    VALPFKN2                                                         
         CLC   BCPFKEY,CSNEXTPF    TEST THIS IS THE ONE JUST USED               
         BE    VALPFKN2                                                         
         MVC   BCPFKEY,CSNEXTPF                                                 
         MVI   CSNEXTPF,0          RESET NEXT TIME PFKEY                        
         B     VALPFK10                                                         
*                                                                               
VALPFKN2 XR    R0,R0                                                            
         IC    R0,BCPFKEY                                                       
         CVD   R0,BCDUB                                                         
         OI    BCDUB+7,X'0F'                                                    
         UNPK  FVXTRA(2),BCDUB                                                  
         CLI   FVXTRA,C'0'                                                      
         BH    *+14                                                             
         MVC   FVXTRA(1),FVXTRA+1                                               
         MVI   FVXTRA+1,C' '                                                    
         MVC   FVMSGNO,=AL2(AE$PFUND)                                           
         OI    FVCURIND,FVCKEEP    KEEP CURSOR WHERE IT IS                      
         B     SETMSG                                                           
*                                                                               
VALPFKX  DS    0H                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION FIELD & RECORD/ACTION COMBINATION                   *         
***********************************************************************         
         SPACE 1                                                                
VALACT   DS    0H                                                               
*   ??   CLI   CSACT,ACTHELP       TEST IN ACTION HELP                          
*   ??   BE    *+12                                                             
         TM    BASACTH+FHIID,FHIIVA                                             
         BZ    VALACT02                                                         
*                                                                               
         CLI   CSACT,0             ACTION REQUIRED                              
         BE    VALACT02                                                         
*                                                                               
         GOTO1 ATSTACT,CSACT       SET AACTNTRY                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ATSTMIX,CSRECACT    SET AMIXNTRY                                 
         BE    VALACT30                                                         
         DC    H'0'                                                             
*                                                                               
VALACT02 MVI   FVFIELD,FVFACT      ??                                           
         LA    R0,CSACTNAM-WORKD   ??                                           
         STCM  R0,3,FVSAVE         ??                                           
         MVI   FVMAXL,ACTNAMLQ                                                  
         MVI   CSREC,RECBIL        SET DEFAULT RECORD TYPE                      
         GOTO1 AFVAL,BASACTH                                                    
         BH    SETMSG              ??                                           
         BE    VALACT04                                                         
         MVC   FVMSGNO,=AL2(AC#ERF-DD#DISK)                                     
         MVI   FVOMTYP,GTMDIC                                                   
         B     SETMSG                                                           
*                                                                               
VALACT04 DS    0H                                                               
*  ??    TM    FVINDS,FVIHELP      TEST HELP REQUESTED                          
*  ??    BZ    VALACT10                                                         
*  ??    GOTO1 ANTRSES,=AL1(RECBIL,ACTHELP,0,0,0,0)                             
*  ??    NI    TWAINDS1,FF-TWAINTRS                                             
*  ??    MVC   BCHALF,=AL1(RECBIL,ACTHELP)                                      
*  ??    B     VALACT20                                                         
*                                                                               
VALACT10 MVC   CSACTNAM,FVIFLD                                                  
         L     R2,AACTTAB                                                       
         USING ACTTABD,R2          R2=A(ACTION TABLE)                           
*                                                                               
VALACT12 CLI   ACTTABD,EOT         TEST EOT                                     
         BNE   VALACT14                                                         
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         LA    R0,ACTNAMLQ+1                                                    
         STC   R0,FVPARMS                                                       
         MVC   FVPARMS+1(ACTNAMLQ),FVIFLD                                       
         B     SETMSG                                                           
*                                                                               
VALACT14 XR    RE,RE               MATCH ON INPUT NAME                          
         ICM   RE,3,ACTNAMEU                                                    
         LA    RF,TWAD(RE)                                                      
         IC    R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         BE    VALACT16                                                         
         CLC   FVIFLD(0),0(RF)                                                  
         ICM   RE,3,ACTNAMEL                                                    
         LA    RF,TWAD(RE)                                                      
         EX    R1,*+8                                                           
         BNE   VALACT18                                                         
         CLC   FVIFLD(0),0(RF)                                                  
*                                                                               
VALACT16 DS    0H                  TEST IF THIS ENTRY IS VALID                  
         TM    ACTINDS1,ACTIDDS    TEST DDS ONLY RECORD TYPE                    
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST DDS TERMINAL                      
         BZ    VALACT18                                                         
         MVC   BCHALF+0(1),ACTRECB SET RECORD NUMBER                            
         MVC   BCHALF+1(1),ACTNUMB                                              
         GOTO1 ATSTMIX,BCHALF      TEST AUTHORISED FOR RECORD/ACTION            
         BNE   VALACT18                                                         
         L     R1,AMIXNTRY                                                      
         USING MIXTABD,R1          R1=A(MIX TABLE ENTRY)                        
*       - - - - - - - - - - - - - -                                             
         TM    MIXINDS2,MIXINOIN   IS ACTION ALLOWED TO BE MANUALLY             
         BO    VALACT18            INPUTED? NO                                  
*       - - - - - - - - - - - - - -                                             
VALACT17 TM    CSINDSG1,CSINDSET   TEST SETUP SCREEN PROCESSED                  
         BNZ   *+12                                                             
         TM    MIXINDS2,MIXISETP                                                
         BNZ   VALACT18                                                         
         TM    MIXINDS2,MIXIJCLB                                                
         BZ    *+12                                                             
         TM    BCJOBSTA,BCJOBSCB   TEST CLIENT BILLING JOB                      
         BZ    VALACT18                                                         
         CLI   CSFORMAT,0                                                       
         BNE   *+12                                                             
         TM    MIXINDS2,MIXIFMTC                                                
         BO    VALACT18                                                         
         TM    MIXINDS1,MIXISEL    TEST SELECT ACTION ONLY                      
         BZ    VALACT20                                                         
         TM    CSINDSL1,CSIUSELC   YES - TEST NESTED CALL                       
         BZ    VALACT18                                                         
         B     VALACT20                                                         
         DROP  R1                                                               
VALACT18 LA    R2,ACTTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     VALACT12                                                         
*                                  EXTRACT ACTION VALUES INTO W/S               
VALACT20 ST    R2,AACTNTRY                                                      
         GOTO1 ARECACT,BCHALF                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALACT30 DS    0H                                                               
*&&US                                                                           
         L     R3,AMIXNTRY                                                      
         USING MIXTABD,R3                                                       
         TM    MIXINDS2,MIXISETP   TEST ACTION REQUIRED SET-UP                  
         BZ    VALACTX                                                          
         MVC   IODAOVER,BCCLIDA    YES - TEST CLIENT LOCKED                     
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1                                                          
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING LOKELD,R1                                                        
         XR    RF,RF                                                            
VALACT32 CLI   LOKEL,0                                                          
         BE    VALACTX                                                          
         CLI   LOKEL,LOKELQ                                                     
         BE    *+12                                                             
         IC    RF,LOKLN                                                         
         BXH   R1,RF,VALACT32                                                   
         TM    LOKSTAT,LOKSLOCK                                                 
         BZ    VALACTX                                                          
         MVC   FVMSGNO,=AL2(AE$CLILK)                                           
         B     SETMSG                                                           
         DROP  R1,R3                                                            
*&&                                                                             
VALACTX  DS    0H                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE SCROLL FIELD                                               *         
***********************************************************************         
         SPACE 1                                                                
VALSCR   TM    BASSCRH+FHIID,FHIIVA                                             
         BNZ   VALSCRX             TEST ALREADY VALIDATED                       
*                                                                               
         LH    R3,=Y(BSDICT-TWAD)                                               
         LA    R3,TWAD(R3)                                                      
         USING BSDICT,R3                                                        
*                                                                               
         GOTO1 AFVAL,BASSCRH       SET TO PAGE IF EMPTY                         
         BNE   VALSCR04                                                         
         TM    FVIIND,FVINUM       TEST NUMERICAL                               
         BZ    VALSCR02                                                         
         OC    BCFULL,BCFULL       VALIDATE SCROLL AMOUNT                       
         BZ    VALSCRN                                                          
         OC    BCFULL(3),BCFULL                                                 
         BNZ   VALSCRN                                                          
         CLI   BCFULL+3,15                                                      
         BH    VALSCRN                                                          
         MVC   BCSCRNUM,BCFULL+3                                                
         B     VALSCRY                                                          
*                                                                               
VALSCR02 CLC   FVIFLD(1),UC@PAGE   VALIDATE FOR P(AGE)                          
         BNE   VALSCR06                                                         
VALSCR04 MVI   BCSCRNUM,PFKIPAGE                                                
         MVC   BASSCR,LC@PAGE                                                   
         B     VALSCRY                                                          
*                                                                               
VALSCR06 CLC   FVIFLD(1),UC@MAX    VALIDATE FOR M(AXIMUM)                       
         BNE   VALSCR08                                                         
         MVI   BCSCRNUM,PFKIMAXN                                                
         MVC   BASSCR,LC@PAGE                                                   
         B     VALSCRY                                                          
*                                                                               
VALSCR08 CLC   FVIFLD(1),UC@HALF   VALIDATE FOR H(ALF)                          
         BNE   VALSCRN                                                          
         MVI   BCSCRNUM,PFKIHALF                                                
         MVC   BASSCR,LC@HALF                                                   
         B     VALSCRY                                                          
*                                                                               
VALSCRN  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     SETMSG0                                                          
*                                                                               
VALSCRY  OI    BASSCRH+FHOID,FHOITR                                             
         TM    BCSCRNUM,PFKIMAXN                                                
         BNZ   *+8                                                              
         OI    BASSCRH+FHIID,FHIIVA                                             
         DROP  R3                                                               
*                                                                               
VALSCRX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE REPORT FIELD AND SET VALUE IN CSREPID/REPSUBID             *         
***********************************************************************         
         SPACE 1                                                                
VALREP   L     R2,AREP                                                          
         USING REPD,R2                                                          
         XC    CSREPID,CSREPID     CLEAR CURRENT REPORT ID                      
         CLI   BASREPH+FHILD,0                                                  
         BE    VALREPX                                                          
         MVC   CSREPID,BASREP                                                   
         TM    BASREPH+FHIID,FHIIVA                                             
         BO    VALREPX                                                          
         OI    BASREPH+FHIID,FHIIVA                                             
         OI    BASREPH+FHOID,FHOITR                                             
VALREPX  DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE DESTINATION ID AND SET VALUE IN CSDSTID/REPUSRID           *         
***********************************************************************         
         SPACE 1                                                                
VALDST   TM    BASDSTH+(FVIIND-FVIHDR),FVIVAL                                   
         BNZ   VALDSTX                                                          
         GOTO1 AVALDST,BASDSTH                                                  
         BNE   SETMSG                                                           
         MVC   CSDSTID,BCWORK                                                   
         OI    BASDSTH+(FVIIND-FVIHDR),FVIVAL                                   
         OI    BASDSTH+FHOID,FHOITR                                             
VALDSTX  MVC   REPUSRID,CSDSTID                                                 
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GO TO SUB-CONTROLLER                                                *         
***********************************************************************         
         SPACE 1                                                                
GO       MVI   BCBYTE1,0                                                        
         TM    BCINDS1,BCINREC+BCINACT                                          
         BZ    GO10                                                             
         TM    BCINDS2,BCINTRS                                                  
         BO    GO10                                                             
*                                                                               
         XC    CSHIRECN,CSHIRECN                                                
         XC    CSPSRECN,CSPSRECN                                                
         XC    CSINDSL,CSINDSL                                                  
         XC    CSINDSG,CSINDSG                                                  
         MVI   TWASESNL,0                                                       
         XC    CSINITRA,CSINITRA   CLEAR INITIAL RECORD/ACTION                  
         LA    R0,TWAD                                                          
         AH    R0,=Y(OSVALS-TWAD)                                               
         LA    R1,OSVALSL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R0,TWAD                                                          
         AH    R0,=Y(OSVALS2-TWAD)                                              
         LA    R1,OSVALS2L                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
GO10     GOTO1 AOVRPHS,CSOVER      LOAD OVERLAY                                 
         BNE   SETMSG                                                           
         TM    CSMIX1,MIXILST      TEST LIST TYPE ACTION                        
         BZ    GO12                                                             
         MVC   BONTRYA,BCNTRYA     SAVE APPLICATION ADDRESS                     
         GOTO1 VCOLY,BCPARM,('O#LIST',0),0,0                                    
         CLI   4(R1),FF            & LOAD LIST SUB-CONTROLLER                   
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFEOLY)                                            
         B     SETMSG                                                           
         MVC   BCNTRYA,0(R1)                                                    
*                                                                               
GO12     NI    BCINDS2,FF-BCIACTCP                                              
         MVC   BCNTRYA(1),BCBYTE1                                               
         GOTO1 BCNTRYA                                                          
         TM    CSINDSG1,CSINDUNW   TEST UNWIND VIA $ABEND                       
         BZ    *+12                                                             
         NI    TWAINDS1,FF-(TWAINTRS+TWAIXITS)                                  
         B     SETMSG                                                           
         TM    TWAINDS1,TWAINTRS   TEST NTRSES ISSUED                           
         BZ    *+12                                                             
         NI    TWAINDS1,FF-TWAINTRS                                             
         B     VALACT                                                           
*                                                                               
         TM    TWAINDS1,TWAIXITS   TEST XITSES ISSUED                           
         BZ    *+12                                                             
         NI    TWAINDS1,FF-TWAIXITS                                             
         B     GO10                                                             
*                                                                               
         TM    BCINDS2,BCINTRS     TEST NTRSES THIS TIME                        
         BO    SETMSG                                                           
         TM    BCINDS2,BCIACTCP    TEST NTRSESES ACTION COMPLETED               
         BZ    SETMSG                                                           
         GOTO1 AXITSES                                                          
         TM    TWAINDS1,TWAIXITS                                                
         BZ    *+12                                                             
         NI    TWAINDS1,FF-TWAIXITS                                             
         B     GO10                                                             
         GOTO1 ARECACT,CSRECACT                                                 
         B     SETMSG                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT ERROR MESSAGE, FIELD INDEX INFO & EXTRA MESSAGE   *         
*                                                                     *         
* NTRY - FVADDR=A(FIELD HEADER OF FIELD IN ERROR)                     *         
*        FVMSGNO=FIELD ERROR NUMBER                                   *         
*        FVFLAG=ZERO IF A STANDARD CONTROLLER ERROR MESSAGE REQUIRED  *         
*        FVOSYS=OVERRIDE SYSTEM FOR GETTXT CALL (ZERO=STANDARD)       *         
*        FVINDX=MULTIPLE FIELD INDEX NUMBER                           *         
*        FVSUBX=MULTIPLE FIELD SUB-INDEX NUMBER                       *         
*        FVXTRA=USER SUPPLIED MESSAGE TO TACK ONTO GENERAL MESSAGE    *         
*                                                                     *         
* NTR AT SETMSG0 TO SET MULTIPLE FIELD INDEX VALUES TO ZERO           *         
*        SETMSG FOR REGULAR MESSAGE BUILDING                          *         
*        SETMSG10 ONLY TO SET CURSOR TO FIELD ADDRESSED BY FVADDR     *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
SETMSG0  MVI   FVINDX,0            ENTRY POINT FOR NO INDEX INFO                
         MVI   FVSUBX,0                                                         
SETMSG   CLC   FVMSGNO,=AL2(FVFSET) TEST USER HAS SUPPLIED MESSAGE              
         BE    SETMSG10                                                         
         MVC   CSMSGNUM,FVMSGNO                                                 
         MVC   CSMSGTYP,FVOMTYP                                                 
         LA    R1,BCPARM           DEFINE GETTXT CONTROL BLOCK                  
         USING GETTXTD,R1                                                       
         CLC   FVMSGNO,=AL2(FVFGTSET)   TEST APPL SET GETTXT BLOCK              
         BNE   *+12                                                             
         LA    R1,BOPARM           APPLICATION HAS DEFINED BLOCK                
         B     SETMSG8                                                          
*                                                                               
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTINDX,FVINDX                                                    
         MVC   GTSUBX,FVSUBX                                                    
         MVC   GTMSGNO,FVMSGNO                                                  
         MVC   GTMSYS,FVOSYS       OVERRIDE SYSTEM (IF SET)                     
         CLI   GTMSYS,0            TEST OVERRIDE SYSTEM SET                     
         BNE   *+10                                                             
         MVC   GTMSYS,ASSYSO       NO - SET NATIVE SYSTEM                       
         MVC   GTMTYP,FVOMTYP      OVERRIDE MESSAGE TYPE (IF SET)               
         CLI   GTMSGNO,FF          STD CONTROLLER MSG                           
         BNE   *+12                                                             
         MVI   GTMSGNO,0                                                        
         MVI   GTMSYS,FF           GENERAL SYSTEM MESSAGE                       
         OC    GTMSGNO,GTMSGNO     MESSAGE 0 = DATAMGR ERR                      
         BZ    SETMSG1                                                          
         CLI   FVOMTYP,0           TEST NO MESSAGE HAS BEEN SET                 
         BNE   SETMSG2                                                          
         CLC   GTMSGNO,=AL2(FVFOK)                                              
         BNE   SETMSG2                                                          
         MVC   GTMSGNO,=AL2(AE$NOEMD)                                           
         B     SETMSG2                                                          
SETMSG1  LA    R0,BCPARM                                                        
         STCM  R0,7,GTADMCB                                                     
         OI    GT1INDS,GT1DMGRE                                                 
*                                                                               
SETMSG2  LA    RF,FVXTRA+L'FVXTRA-1                                             
         LA    R0,L'FVXTRA                                                      
         CLI   0(RF),C' '                                                       
         BH    *+14                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         B     SETMSG6                                                          
         LA    RF,FVXTRA                                                        
         STCM  RF,7,GTATXT         SET LENGTH & ADDRESS OF EXTRA TEXT           
         STCM  R0,1,GTLTXT                                                      
*                                                                               
SETMSG6  LA    R1,BCPARM           BCPARM DEFINED INTERNALLY                    
         CLI   GTMSGNO,FF          CHECK FOR GENERAL MESSAGES                   
         BNE   SETMSG8                                                          
         MVI   GTMSYS,FF           FORCE SYSTEM ZERO LOOKUP                     
         MVI   GTMSGNO,0                                                        
*                                                                               
SETMSG8  LA    R0,FVPARMS          SET A(SUBSTITUTION PARAMETERS)               
         STCM  R0,7,GTASUBST                                                    
         GOTO1 VGETTXT,(R1)        RESOLVE MESSAGE                              
         DROP  R1                                                               
*                                                                               
SETMSG10 OI    BASMSGH+(FVOIND-FVIHDR),FVOXMT                                   
         L     R1,AINP             TEST IF OVERLAY SET CURSOR                   
         USING TIOBD,R1                                                         
         TM    TIOBINDS,TIOBSETC                                                
         BNZ   BILX                                                             
         TM    FVCURIND,FVCKEEP    TEST KEEP CURSOR WHERE IT IS                 
         BZ    SETMSG12                                                         
         MVC   TIOBCURS,CSCURDSP                                                
         XC    TIOBCURD,TIOBCURD                                                
         OI    TIOBINDS,TIOBSETC                                                
         B     BILX                                                             
         DROP  R1                                                               
*                                                                               
SETMSG12 ICM   R1,15,BOCURSOR      TEST CURSOR ADDRESS SET                      
         BNZ   *+12                                                             
         ICM   R1,15,FVADDR        TEST IF OVERLAY SET FIELD ADDRESS            
         BZ    BILX                                                             
*                                                                               
         CLI   FVERRNDX,0          TEST FIELD INDEX VALUE SET                   
         BE    SETMSG14                                                         
         L     RE,AINP                                                          
         USING TIOBD,RE                                                         
         LA    R0,TWAD                                                          
         LR    RF,R1                                                            
         SR    RF,R0               RF=DISPLACEMENT TO FIELD                     
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FVERRNDX                                                
         OI    TIOBINDS,TIOBSETC                                                
         DROP  RE                                                               
*                                                                               
SETMSG14 OI    FVOIND-FVIHDR(R1),FVOCUR                                         
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)                                                       
         AR    R1,RE               POINT TO NEXT FIELD HEADER                   
         LA    RF,OSVALS-1         RF=END OF LARGEST SCREEN                     
         ICM   RE,1,FVTLEN-FVIHDR(R1) TURN OFF CURSORS TO BOTTOM OF TWA         
         BZ    BILX                                                             
         NI    FVOIND-FVIHDR(R1),FF-FVOCUR                                      
         BXLE  R1,RE,*-12                                                       
*                                                                               
BILX     TM    LSINDS1,LSITEMP     TEST ANY TEMPORARY TSAR RECORDS              
         BZ    BILX04                                                           
         L     R2,AIO1             YES - DELETE THEM                            
DEL      USING TLSTD,R2                                                         
BILX02   XC    DEL.TLKEY,DEL.TLKEY                                              
         MVI   DEL.TLKSES,TLKSTEMP                                              
         GOTO1 ATSARIO,BCPARM,('TSARDH',DEL.TLSTD)                              
         BL    BILX04                                                           
         GOTO1 (RF),(R1),('TSADEL',DEL.TLSTD)                                   
         B     BILX02                                                           
         DROP  DEL                                                              
BILX04   GOTO1 ATSARIO,TSASAV      SAVE TSAR BUFFER IF NECESSARY                
*                                                                               
         XR    R2,R2                                                            
         TM    CSINDSG1,CSINDUNW   TEST UNWIND VIA $ABEND                       
         BZ    *+12                                                             
         LA    R2,1                                                             
         NI    CSINDSG1,FF-CSINDUNW                                             
         LA    RE,BSVALS           SAVE GLOBAL W/S VALUES IN TWA0               
         LA    R0,BCVALS                                                        
         LA    RF,BCVALSL                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         LH    RE,=Y(LSSAVE-TWAD)  SAVE GLOBAL W/S VALUES IN TWA0               
         LA    RE,TWAD(RE)                                                      
         LA    R0,LSVALSD                                                       
         LA    RF,LSSAVEL                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         TM    TWAINDS1,TWAIINIT   TEST INITIALISED                             
         BZ    BILX12                                                           
         GOTO1 ABLDPFK             BUILD PFKEY DISPLAY LINE                     
*                                                                               
BILX12   LTR   R2,R2               TEST UNWIND VIA $ABEND                       
         BZ    BILXIT                                                           
         DC    H'0',C'$ABEND'      UNWIND TRANSACTION                           
*                                                                               
BILXIT   XIT1  ,                                                                
         EJECT                                                                  
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
ROUTAB   DS    0X                  ** OVERRIDE ROUTINES FOR BILL **             
*                                                                               
         DC    AL2(AFMTBLK-WORKD),AL1(O#ROUTS,00)                               
         DC    AL2(ARECACT-WORKD),AL1(O#ROUTS,01)                               
         DC    AL2(AOVRSCR-WORKD),AL1(O#ROUTS,02)                               
         DC    AL2(ABLDPFK-WORKD),AL1(O#ROUTS,03)                               
         DC    AL2(ASETCLM-WORKD),AL1(O#ROUTS,04)                               
         DC    AL2(ANTRSES-WORKD),AL1(O#ROUTS,05)                               
         DC    AL2(AXITSES-WORKD),AL1(O#ROUTS,06)                               
         DC    AL2(ATSTSEC-WORKD),AL1(O#ROUTS,07)                               
         DC    AL2(ASETMSK-WORKD),AL1(O#ROUTS,08)                               
         DC    AL2(AGETOPT-WORKD),AL1(O#ROUTS,09)                               
         DC    AL2(AALLTRN-WORKD),AL1(O#ROUTS,10)                               
         DC    AL2(AGETPTA-WORKD),AL1(O#ROUTS,11)                               
         DC    AL2(APUTPTA-WORKD),AL1(O#ROUTS,12)                               
         DC    AL2(AWOPPTA-WORKD),AL1(O#ROUTS,13)                               
         DC    AL2(AGETGRB-WORKD),AL1(O#ROUTS,14)                               
         DC    AL2(ASUBTOT-WORKD),AL1(O#ROUTS,15)                               
         DC    AL2(AFMTHED-WORKD),AL1(O#ROUTS,16)                               
         DC    AL2(ABLDTRN-WORKD),AL1(O#ROUTS,17)                               
         DC    AL2(ACMPPRF-WORKD),AL1(O#ROUTS,18)                               
         DC    AL2(AADDOBH-WORKD),AL1(O#ROUTS,19)                               
         DC    AL2(ATSTPFK-WORKD),AL1(O#ROUTS,20)                               
         DC    AL2(AFNDCLM-WORKD),AL1(O#ROUTS,21)                               
         DC    AL2(AGETWCD-WORKD),AL1(O#ROUTS,22)                               
*                                                                               
         DC    AL2(ASETUP-WORKD),AL1(O#ROUTS2,00)                               
         DC    AL2(AUPDJOB-WORKD),AL1(O#ROUTS2,01)                              
         DC    AL2(ATSARIO-WORKD),AL1(O#ROUTS2,02)                              
         DC    AL2(AEDTAMT-WORKD),AL1(O#ROUTS2,03)                              
         DC    AL2(ASAVOWS-WORKD),AL1(O#ROUTS2,04)                              
*                                                                               
         DC    AL2(ASETTRN-WORKD),AL1(O#OPTS,00)                                
         DC    AL2(AVALOPT-WORKD),AL1(O#OPTS,01)                                
*                                                                               
         DC    AL2(AXFRTMS-WORKD),AL1(O#XFRTMS,00)                              
*                                                                               
ROUTABX  DC    AL2(0)                                                           
         EJECT                                                                  
DICUPR   DS    0XL4                ** UPPER CASE DICTIONARY LIST **             
         DCDDL AC#PFK,L'UC@PFKEY+1                                              
         DCDDL AC#ALTPF,L'UC@ALTPF                                              
         DCDDL AC#HELP,L'UC@HELP                                                
         DCDDL AC#PAGE,L'UC@PAGE                                                
         DCDDL AC#HALF,L'UC@HALF                                                
         DCDDL AC#MAX,L'UC@MAX                                                  
         DCDDL AC#SETUP,L'UC@SETUP                                              
         DCDDL AC#UPD,L'UC@UPDT                                                 
         DCDDL AC#ALLOC,L'UC@ALLOC                                              
         DCDDL AC#CHG,L'UC@CHG                                                  
         DCDDL AC#EDIT,L'UC@EDIT                                                
         DCDDL AC#XFR,L'UC@XFR                                                  
         DCDDL AC#CBWOF,L'UC@WRTF                                               
         DCDDL AC#ZOOM,L'UC@ZOOM                                                
         DCDDL AC#DSP,L'UC3DSP                                                  
         DCDDL AC#DSP,L'UC8DSP                                                  
         DCDDL AC#RETRN,L'UC@RETRN                                              
         DCDDL AC#NXT,L'UC@NEXT                                                 
         DCDDL AC#FRMAT,L'UC@FRMAT                                              
         DCDDL AC#ENQ,L'UC@ENQ                                                  
         DCDDL AC#LIST,L'UC@LIST                                                
         DCDDL AC#ZMWO,L'UC@ZMWO                                                
         DCDDL AC#AUTO,L'UC@AUTO                                                
         DCDDL AC#ADD,L'UC@ADD                                                  
         DCDDL AC#YES,L'UC@YES                                                  
         DCDDL AC#NO,L'UC@NO                                                    
         DCDDL AC#DEL,L'UC@DEL                                                  
         DCDDL AC#FIND,L'UC@FIND                                                
         DCDDL AC#COLS,L'UC@COLS                                                
         DCDDL AC#ALL,L'UC@ALL                                                  
         DCDDL AC#PREFX,L'UC@PREFX                                              
         DCDDL AC#SUFFX,L'UC@SUFFX                                              
         DCDDL AC#WORD,L'UC@WORD                                                
         DCDDL AC#FIRST,L'UC@FIRST                                              
         DCDDL AC#LAST,L'UC@LAST                                                
         DCDDL AC#RESET,L'UC@RESET                                              
         DCDDL AC#PRV,L'UC@PRV                                                  
         DCDDL AC#CLEAR,L'UC@CLEAR                                              
         DCDDL AC#PRGRP,L'UC@PRGRP                                              
         DCDDL AC#ONLY,L'UC@ONLY                                                
         DCDDL AC#AUTHD,L'UC3AUTHD                                              
         DCDDL AC#AUTHD,L'UC8AUTHD                                              
         DCDDL AC#HELD,L'UC3HELD                                                
         DCDDL AC#HELD,L'UC8HELD                                                
         DCDDL AC#ZMWO,L'UC3ZMWO                                                
*&&UK*&& DCDDL AC#WC,2                                                          
*&&US*&& DCDDL AC#WC,3                                                          
         DCDDL AC#WC,L'UC8WC                                                    
         DCDDL AC#TYPE,L'UC3TYPE                                                
         DCDDL AC#TYPE,L'UC8TYPE                                                
         DCDDL AC#REF,L'UC3REF                                                  
         DCDDL AC#REF,L'UC8REF                                                  
         DCDDL AC#DATE,L'UC3DATE                                                
         DCDDL AC#DATE,L'UC8DATE                                                
         DCDDL AC#BAT,L'UC3BAT                                                  
         DCDDL AC#BAT,L'UC8BAT                                                  
         DCDDL AC#MOA,L'UC3MOA                                                  
         DCDDL AC#MOA,L'UC8MOA                                                  
         DCDDL AC#AMT,L'UC3AMT                                                  
         DCDDL AC#AMT,L'UC8AMT                                                  
         DCDDL AC#SWCH,L'UC3SWCH                                                
         DCDDL AC#SWCH,L'UC8SWCH                                                
         DCDDL AC#CMN,L'UC3CMN                                                  
         DCDDL AC#CMN,L'UC8CMN                                                  
         DCDDL AC#ADVNC,L'UC@ADVNC                                              
         DCDDL AC#CNTRL,L'UC@CNTRL                                              
         DCDDL AC#WHOLE,L'UC3WHOLE                                              
         DCDDL AC#WHOLE,L'UC8WHOLE                                              
         DCDDL AC#LEFT,L'UC@LEFT                                                
         DCDDL AC#CENTR,L'UC@CENTR                                              
         DCDDL AC#RIGHT,L'UC@RIGHT                                              
         DCDDL AC#MMIZE,L'UC@MMIZE                                              
         DCDDL AC#SEL,L'UC@SEL                                                  
         DCDDL AC#RSR,L'UC@RSR                                                  
         DCDDL AC#JOB,L'UC@JOB                                                  
         DCDDL AC#BIL,L'UC@BIL                                                  
         DCDDL AC#DRAFT,L'UC@DRAFT                                              
         DCDDL AC#LIVE,L'UC@LIVE                                                
         DCDDL AC#RFRSH,L'UC@RFRSH                                              
         DCDDL AC#WCTOT,L'UC3WCTOT                                              
         DCDDL AC#WCTOT,L'UC8WCTOT                                              
         DCDDL AC#PRINT,L'UC@PRINT                                              
         DCDDL AC#HEADR,L'UC@HEADR                                              
         DCDDL AC#TIME,L'UC@TIME                                                
         DCDDL AC#COST,L'UC@COST                                                
         DCDDL AC#BLD,L'UC8BLD                                                  
         DCDDL AC#BLD,L'UC3BLD                                                  
         DCDDL AC#FEEAJ,L'UC@FEE                                                
         DCDDL AC#AUFRM,L'UC@AUFRM                                              
         DCDDL AC#SMY,L'UC@SMY                                                  
         DCDDL AC#FRFRM,L'UC@FRFRM                                              
         DCDDL AC#LSPAR,L'UC@LSPAR                                              
         DCDDL AC#FMLST,L'UC@FMLST                                              
         DCDDL AC#PRBL,L'UC@PRBL                                                
         DCDDL AC#RVRS,L'UC@RVRS                                                
         DCDDL AC#REVLU,L'UC@REVLU                                              
         DCDDL AC#CURRY,L'UC@CURRY                                              
         DCDDL AC#MCH,L'UC@MCH                                                  
         DCDDL AC#INV,L'UC@INV                                                  
         DCDDL AC#AUTAL,L'UC@AUTAL                                              
         DCDDL AC#ZMWR,L'UC@ZMWR                                                
         DCDDL AC#EXCD,L'UC@EXCD1                                               
         DCDDL AC#EXCD,L'UC@EXCD2                                               
         DCDDL AC#AUTUL,L'UC@AUTUL                                              
         DCDDL AC#CURRY,L'UC3CURRY                                              
         DCDDL AC#RATE,L'UC@RATE                                                
         DCDDL AC#ALPHA,L'UC@ALPHA                                              
         DCDDL AC#BOTH,L'UC@BOTH                                                
         DCDDL AC#FRN,L'UC@FRN                                                  
         DCDDL AC#GROUP,L'UC@GROUP                                              
         DCDDL AC#RPRNT,L'UC@RPRNT                                              
         DCDDL AC#NONE,L'UC@NONE                                                
         DCDDL AC#CLIBG,L'UC@CLIBG                                              
         DCDDL AC#LABOR,L'UC@LABOR                                              
         DCDDL AC#WKTYP,L'UC@WKTYP                                              
         DCDDL AC#TOJOB,L'UC@TOJOB                                              
         DCDDL AC#RCVR,L'UC@RCVR                                                
         DCDDL AC#ZMWR,L'UC3ZMWR                                                
         DCDDL AC#UNAV,L'UC@UNAV                                                
         DCDDL AC#PART,L'UC@PART                                                
         DCDDL AC#ACTYD,L'UC3ACTYD                                              
         DCDDL AC#ACTYD,L'UC8ACTYD                                              
         DCDDL AC#CTR,L'UC3CTR                                                  
         DCDDL AC#CTR,L'UC8CTR                                                  
         DCDDL AC#TOWC,L'UC@TOWC                                                
         DCDDL AC#INT,L'UC@INT                                                  
         DCDDL AC#EXT,L'UC@EXT                                                  
         DCDDL AC#AUREV,L'UC3AUREV                                              
         DCDDL AC#AUREV,L'UC8AUREV                                              
         DCDDL AC#FEEL,L'UC@FEEL                                                
         DCDDL AC#WRTB,L'UC@WRTB                                                
         DCDDL AC#HELP,L'UC8HELP                                                
DICUPRX  DC    AL1(EOT)                                                         
*                                                                               
DICMIX   DS    0XL4                ** MIXED CASE DICTIONARY LIST **             
         DCDDL AC#SETUP,L'LC@SETUP                                              
         DCDDL AC#UPD,L'LC@UPDT                                                 
         DCDDL AC#ALLOC,L'LC@ALLOC                                              
         DCDDL AC#CHG,L'LC@CHG                                                  
         DCDDL AC#EDIT,L'LC@EDIT                                                
         DCDDL AC#XFR,L'LC@XFR                                                  
         DCDDL AC#CBWOF,L'LC@WRTF                                               
         DCDDL AC#ZOOM,L'LC@ZOOM                                                
         DCDDL AC#UP,L'LC@UP                                                    
         DCDDL AC#DOWN,L'LC@DOWN                                                
         DCDDL AC#LEFT,L'LC@LEFT                                                
         DCDDL AC#RIGHT,L'LC@RIGHT                                              
         DCDDL AC#ALTPF,L'LC@ALTPF                                              
         DCDDL AC#RETRN,L'LC@RETRN                                              
         DCDDL AC#NXT,L'LC@NEXT                                                 
         DCDDL AC#FRMAT,L'LC@FRMAT                                              
         DCDDL AC#ENQ,L'LC@ENQ                                                  
         DCDDL AC#LIST,L'LC@LIST                                                
         DCDDL AC#ZMWO,L'LC@ZMWO                                                
         DCDDL AC#FIND,L'LC@FIND                                                
         DCDDL AC#PRV,L'LC@PRV                                                  
         DCDDL AC#PRGRP,L'LC@PRGRP                                              
         DCDDL AC#ADD,L'LC@ADD                                                  
         DCDDL AC#LINE,L'LC@LINE                                                
         DCDDL AC#LINES,L'LC@LINES                                              
         DCDDL AC#OF,L'LC@OF+1                                                  
         DCDDL AC#ALCTD,L'LC@ALCTD                                              
         DCDDL AC#CHGS,L'LC@CHGS                                                
         DCDDL AC#BLD,L'LC@BLD                                                  
         DCDDL AC#WRTFS,L'LC@WRTFS                                              
         DCDDL AC#YES,L'LC@YES                                                  
         DCDDL AC#CLEAR,L'LC@CLEAR                                              
         DCDDL AC#ZMWO,L'LC3ZMWO                                                
         DCDDL AC#ADVNC,L'LC@ADVNC                                              
         DCDDL AC#DSP,L'LC@DSP                                                  
         DCDDL AC#CNTRL,L'LC@CNTRL                                              
         DCDDL AC#RFRSH,L'LC@RFRSH                                              
         DCDDL AC#CLEAR,L'LC8CLEAR                                              
         DCDDL AC#PRINT,L'LC@PRINT                                              
         DCDDL AC#TOTAL,L'LC@TOTAL                                              
         DCDDL AC#SAVE,L'LC@SAVE                                                
         DCDDL AC#SEL,L'LC@SEL                                                  
         DCDDL AC#DRAFT,L'LC@DRAFT                                              
         DCDDL AC#FEEAJ,L'LC@FEE                                                
         DCDDL AC#AUFRM,L'LC@AUFRM                                              
         DCDDL AC#SMY,L'LC@SMY                                                  
         DCDDL AC#FRFRM,L'LC@FRFRM                                              
         DCDDL AC#LSPAR,L'LC@LSPAR                                              
         DCDDL AC#FMLST,L'LC@FMLST                                              
         DCDDL AC#PRBL,L'LC@PRBL                                                
         DCDDL AC#RVRS,L'LC@RVRS                                                
         DCDDL AC#REVLU,L'LC@REVLU                                              
         DCDDL AC#LIVE,L'LC@LIVE                                                
         DCDDL AC#BILC,L'LC@BILC                                                
         DCDDL AC#CLIBG,L'LC@CLIBG                                              
         DCDDL AC#TIME,L'LC@TIME                                                
         DCDDL AC#COST,L'LC@COST                                                
         DCDDL AC#MCH,L'LC@MCH                                                  
         DCDDL AC#INV,L'LC@INV                                                  
         DCDDL AC#DFLST,L'LC@DFLST                                              
         DCDDL AC#LVLST,L'LC@LVLST                                              
         DCDDL AC#AUTAL,L'LC@AUTAL                                              
         DCDDL AC#ZMWR,L'LC@ZMWR                                                
         DCDDL AC#EXCD,L'LC@EXCD1                                               
         DCDDL AC#EXCD,L'LC@EXCD2                                               
         DCDDL AC#AUTUL,L'LC@AUTUL                                              
         DCDDL AC#NET,L'HD@NET                                                  
         DCDDL AC#CMN,L'HD@CMN                                                  
         DCDDL AC#GROSS,L'HD@GROSS                                              
         DCDDL AC#SRCRG,L'HD@SRCRG                                              
         DCDDL AC#DISS,L'HD@DISS                                                
         DCDDL AC#TIME,L'HD@TIME                                                
         DCDDL AC#COST,L'HD@COST                                                
         DCDDL AC#BIL,L'LC@BIL                                                  
         DCDDL AC#MORE,L'LC@MORE                                                
         DCDDL AC#CNFRM,L'LC@CNFRM                                              
         DCDDL AC#GROUP,L'LC@GROUP                                              
         DCDDL AC#RPRNT,L'LC@RPRNT                                              
         DCDDL AC#BLB,L'LC@BLB                                                  
         DCDDL AC#PF7ZW,L'LC@PF7ZW                                              
         DCDDL AC#RCVR,L'LC@RCVR                                                
         DCDDL AC#ZMWR,L'LC3ZMWR                                                
         DCDDL AC#NRTV,L'LC@NRTV                                                
         DCDDL AC#WRTN,L'LC@WRTN                                                
         DCDDL AC#FLT,L'LC@FLT                                                  
         DCDDL AC#TLST,L'LC@TLST                                                
         DCDDL AC#TTIME,L'LC@TTIME                                              
         DCDDL AC#TOOPS,L'LC@TOOPS                                              
         DCDDL AC#PF4FL,L'LC@PF4FL                                              
         DCDDL AC#AUREV,L'LC@AUREV                                              
         DCDDL AC#FEEL,L'LC@FEEL                                                
         DCDDL AC#VAT2,L'HD@VAT                                                 
         DCDDL AC#STTIM,L'HD@STTIM                                              
         DCDDL AC#STCST,L'HD@STCST                                              
         DCDDL AC#INVT2,L'HD@INVT2                                              
         DCDDL AC#TSCR,L'LC@TSCR                                                
         DCDDL AC#NALLC,L'LC@NALLC                                              
         DCDDL AC#COLS,L'LC@COLS                                                
         DCDDL AC#NO,L'LC@NO                                                    
         DCDDL AC#ALL,L'LC@ALL                                                  
         DCDDL AC#ONLY,L'LC@ONLY                                                
         DCDDL AC#ACT,L'LC@ACT                                                  
         DCDDL AC#LINE,L'LC4LINE                                                
         DCDDL AC#OF,L'LC4OF                                                    
         DCDDL AC#WRTB,L'LC@WRTB                                                
         DCDDL AC#PAGE,L'LC@PAGE                                                
         DCDDL AC#HALF,L'LC@HALF                                                
         DCDDL AC#MAX,L'LC@MAX                                                  
         DCDDL AC#HELP,L'LC8HELP                                                
         DCDDL AC#TEXT,L'LC@TEXT,F                                              
         DCDDL AC#ENPAR,L'LC@ENPAR,F                                            
DICMIXX  DC    AL1(EOT)                                                         
         EJECT                                                                  
ADDRS2   DS    0F                  ** CONTROLLER ADDRESSES 2 **                 
         DC    A(0)                                                             
         DC    A(ACTTAB)                                                        
         DC    A(MIXTAB)                                                        
         DC    A(PFKTAB)                                                        
         DC    A(SELTAB)                                                        
         DC    A(FMTTAB)                                                        
         DC    A(DICUPR)                                                        
         DC    A(DICMIX)                                                        
         DC    A(0)                                                             
         DC    A(FOPTAB)                                                        
         DC    A(CLMGEN)                                                        
         DC    A(0)                                                             
         DC    A(CLMTAB)                                                        
         ORG   ADDRS2+(L'ADDRS2*ADDRS2N)                                        
         EJECT                                                                  
***********************************************************************         
* ACTION TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
ACTTAB   DS    0X                                                               
*                                                                               
         DC    AL1(ACTSET)                                                      
         DC    AL2(UC@SETUP-TWAD,LC@SETUP-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTUPD)                                                      
         DC    AL2(UC@UPDT-TWAD,LC@UPDT-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTDRA)                                                      
         DC    AL2(UC@DRAFT-TWAD,LC@DRAFT-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTALC)                                                      
         DC    AL2(UC@ALLOC-TWAD,LC@ALLOC-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTWOF)                                                      
         DC    AL2(UC@WRTF-TWAD,LC@WRTF-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTXFR)                                                      
         DC    AL2(UC@XFR-TWAD,LC@XFR-TWAD)                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTFFRM)                                                     
         DC    AL2(UC@FRFRM-TWAD,LC@FRFRM-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTEDT)                                                      
         DC    AL2(UC@EDIT-TWAD,LC@EDIT-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTZOO)                                                      
         DC    AL2(UC@ZOOM-TWAD,LC@ZOOM-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTAFRM)                                                     
         DC    AL2(UC@AUFRM-TWAD,LC@AUFRM-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTSUM)                                                      
         DC    AL2(UC@SMY-TWAD,LC@SMY-TWAD)                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTLST)                                                      
         DC    AL2(UC@LIST-TWAD,LC@LIST-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTZWO)                                                      
         DC    AL2(UC@ZMWO-TWAD,LC@ZMWO-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTAPF)                                                      
         DC    AL2(UC@ALTPF-TWAD,LC@ALTPF-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTQUI)                                                      
         DC    AL2(UC@RETRN-TWAD,LC@RETRN-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTNXT)                                                      
         DC    AL2(UC@NEXT-TWAD,LC@NEXT-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTRFS)                                                      
         DC    AL2(UC@RFRSH-TWAD,LC@RFRSH-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTINS)                                                      
         DC    AL2(UC@ADVNC-TWAD,LC@ADVNC-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTLPAR)                                                     
         DC    AL2(UC@LSPAR-TWAD,LC@LSPAR-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTFT1)                                                      
         DC    AL2(UC@FRMAT-TWAD,LC@FRMAT-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTLFT)                                                      
         DC    AL2(UC@FMLST-TWAD,LC@FMLST-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTPRB)                                                      
         DC    AL2(UC@PRBL-TWAD,LC@PRBL-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTREV)                                                      
         DC    AL2(UC@RVRS-TWAD,LC@RVRS-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTRVL)                                                      
         DC    AL2(UC@REVLU-TWAD,LC@REVLU-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTMT1)                                                      
         DC    AL2(UC@MCH-TWAD,LC@MCH-TWAD)                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTMT2)                                                      
         DC    AL2(UC@INV-TWAD,LC@INV-TWAD)                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTAALC)                                                     
         DC    AL2(UC@AUTAL-TWAD,LC@AUTAL-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTZWR)                                                      
         DC    AL2(UC@ZMWR-TWAD,LC@ZMWR-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTRCVR)                                                     
*&&US*&& DC    AL2(UC@RCVR-TWAD,LC@RCVR-TWAD)                                   
*&&UK*&& DC    AL2(UC@WRTB-TWAD,LC@WRTB-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTAULC)                                                     
         DC    AL2(UC@AUTUL-TWAD,LC@AUTUL-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTARV)                                                      
         DC    AL2(UC8AUREV-TWAD,LC@AUREV-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTFEE)                                                      
         DC    AL2(UC@FEE-TWAD,LC@FEE-TWAD)                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTFLI)                                                      
         DC    AL2(UC@FEEL-TWAD,LC@FEEL-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
         DC    AL1(ACTCOL)                                                      
         DC    AL2(UC@COLS-TWAD,LC@COLS-TWAD)                                   
         DC    AL1(ACTIDDS)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
*  ??    DC    AL1(ACTHELP)                                                     
*  ??    DC    AL2(UC8HELP-TWAD,LC8HELP-TWAD)                                   
*  ??    DC    AL1(ACTIDDS)                                                     
*  ??    DC    AL1(0,0,0)                                                       
*  ??    DC    AL1(RECBIL,0,0,0,0,0,0,0)                                        
*                                                                               
ACTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* RECORD/ACTION COMBO TABLE                                           *         
***********************************************************************         
         SPACE 1                                                                
MIXTAB   DS    0X                                                               
*                                                                               
         DC    AL1(RECBIL,ACTSET)                                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(S#BILSET,O#BILSET)                                           
         DC    AL2(5220)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,1)                                                       
         DC    XL6'00'                                                          
         DC    XL24'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(S#BILUPD,O#BILUPD)                                           
         DC    AL2(5221)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    XL24'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(S#BILUPD,O#BILUPD)                                           
         DC    AL2(5236)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    XL24'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTALC)                                               
         DC    AL1(MIXILST,MIXIJCLB+MIXISETP,0,0)                               
         DC    AL1(S#BILALC,O#BILALC)                                           
         DC    AL2(5222)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(2)                                                           
         DC    AL1(0,0,3)                                                       
         DC    XL6'00'                                                          
         DC    AL1(MIXLITRN+MIXLICUR+MIXLISCT+MIXLIJOB,0)                       
         DC    AL1(SUB#ALC,CLMALCQ,SELALCQ,4)                                   
         DC    AL2(TLXMINLQ,OPTFORD+OPTFBLDN+OPTFUNAV+OPTFCOMM+OPTFWCT)         
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTEDT)                                               
         DC    AL1(0,MIXIJCLB+MIXISETP,0,0)                                     
         DC    AL1(S#BILEDT,O#BILEDT)                                           
         DC    AL2(5223)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    XL24'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTFFRM)                                              
         DC    AL1(0,MIXIFMTC+MIXIJCLB+MIXISETP,0,0)                            
         DC    AL1(S#BILEDT,O#BILEDT)                                           
         DC    AL2(5224)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    XL24'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTZOO)                                               
*&&UK*&& DC    AL1(MIXINTRY,MIXIJCLB+MIXISETP,0,0)                              
*&&US*&& DC    AL1(MIXINTRY,MIXISETP+MIXINOIN,0,0)                              
         DC    AL1(S#BILZRG,O#BILZOO)                                           
         DC    AL2(5225)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,1)                                                       
         DC    XL6'00'                                                          
         DC    XL24'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTAFRM)                                              
         DC    AL1(0,MIXIFMTC+MIXIJCLB+MIXISETP,0,0)                            
         DC    AL1(0,O#BILEDT)                                                  
         DC    AL2(5226)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    XL24'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTSUM)                                               
         DC    AL1(0,MIXIJCLB+MIXISETP,0,0)                                     
         DC    AL1(S#BILENQ,O#BILENQ)                                           
         DC    AL2(5227)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    AL1(0,0)                                                         
         DC    AL1(0,CLMSUMMQ,0,0)                                              
         DC    AL2(0,0)                                                         
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTLST)                                               
         DC    AL1(MIXILST,0,0,0)                                               
         DC    AL1(0,O#BILLST)                                                  
         DC    AL2(5228)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    AL1(0,0)                                                         
         DC    AL1(SUB#LST,CLMLSTQ,SELLSTQ,1)                                   
         DC    AL2(TLLSTLNQ,0)                                                  
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTZWO)                                               
*&&UK*&& DC    AL1(MIXINTRY,MIXIJCLB+MIXISETP,0,0)                              
*&&US*&& DC    AL1(MIXINTRY,MIXISETP+MIXINOIN,0,0)                              
         DC    AL1(S#BILZWO,O#BILZOO)                                           
         DC    AL2(5229)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,1)                                                       
         DC    XL6'00'                                                          
         DC    XL24'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTWOF)                                               
*&&UK*&& DC    AL1(MIXILST,MIXIJCLB+MIXISETP,0,0)                               
*&&US*&& DC    AL1(MIXILST,MIXISETP,0,0)                                        
         DC    AL1(S#BILALC,O#BILALC)                                           
         DC    AL2(5230)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(2)                                                           
         DC    AL1(0,0,3)                                                       
         DC    XL6'00'                                                          
         DC    AL1(MIXLITRN+MIXLICUR+MIXLISCT+MIXLIJOB,0)                       
         DC    AL1(SUB#WOF,CLMALCQ,SELALCQ,4)                                   
         DC    AL2(TLXMINLQ,OPTFBLDN+OPTFUNAV+OPTFCOMM+OPTFWCT)                 
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTXFR)                                               
*&&UK*&& DC    AL1(MIXILST,MIXIJCLB+MIXISETP,0,0)                               
*&&US*&& DC    AL1(MIXILST,MIXISETP,0,0)                                        
         DC    AL1(S#BILALC,O#BILXFR)                                           
         DC    AL2(5246)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(2)                                                           
         DC    AL1(0,0,3)                                                       
         DC    XL6'00'                                                          
         DC    AL1(MIXLITRN+MIXLICUR+MIXLISCT+MIXLIJOB,0)                       
         DC    AL1(SUB#XFR,CLMXFRQ,SELXFRQ,4)                                   
         DC    AL2(TLXMINLQ,OPTFBLDN+OPTFUNAV+OPTFWCT)                          
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTINS)                                               
         DC    AL1(MIXINTRY,MIXIJCLB+MIXISETP,0,0)                              
         DC    AL1(S#BILINS,O#BILINS)                                           
         DC    AL2(5231)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,1)                                                       
         DC    XL6'00'                                                          
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL2(TLXMINLQ,0)                                                  
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTLPAR)                                              
         DC    AL1(0,MIXIJCLB+MIXISETP,0,0)                                     
         DC    AL1(S#BILDIS,O#BILDIS)                                           
         DC    AL2(5232)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    XL24'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTFT1)                                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(S#BILCON,O#BILCON)                                           
         DC    AL2(5233)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    XL24'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTLFT)                                               
         DC    AL1(MIXILST,0,0,0)                                               
         DC    AL1(S#BILCLT,O#BILCLT)                                           
         DC    AL2(5234)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    AL1(0,0)                                                         
         DC    AL1(SUB#LFT,CLMLFTQ,SELLFTQ,1)                                   
         DC    AL2(TLCONLNQ,0)                                                  
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTPRB)                                               
         DC    AL1(MIXILST,MIXIJCLB+MIXISETP,0,0)                               
         DC    AL1(S#BILALC,O#BILPRB)                                           
         DC    AL2(5238)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(2)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    AL1(0,0)                                                         
         DC    AL1(SUB#PRB,CLMPRBQ,SELPRBQ,1)                                   
         DC    AL2(TLPRBLNQ,0)                                                  
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTREV)                                               
         DC    AL1(MIXILST,MIXIJCLB+MIXISETP,0,0)                               
         DC    AL1(S#BILALC,O#BILREV)                                           
         DC    AL2(5239)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(2)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    AL1(MIXLITRN+MIXLICUR+MIXLISCT+MIXLIJOB,0)                       
         DC    AL1(SUB#REV,CLMREVQ,SELREVQ,4)                                   
         DC    AL2(TLXMINLQ,OPTFORD+OPTFCOMM+OPTFWCT)                           
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTRVL)                                               
         DC    AL1(MIXILST,MIXIJCLB+MIXISETP,0,0)                               
         DC    AL1(S#BILRVL,O#BILRVL)                                           
         DC    AL2(5240)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(2)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    AL1(MIXLITRN,0)                                                  
         DC    AL1(SUB#RVL,CLMRVLQ,SELRVLQ,2)                                   
         DC    AL2(TLRVLLNQ,OPTFORD+OPTFBLDN)                                   
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTMT1)                                               
         DC    AL1(MIXILST,MIXIJCLB+MIXISETP,0,0)                               
         DC    AL1(S#BILALC,O#BILMT1)                                           
         DC    AL2(5241)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(2)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    AL1(MIXLITRN,0)                                                  
         DC    AL1(SUB#MT1,CLMMT1Q,SELMT1Q,1)                                   
         DC    AL2(TLMT1LNQ,OPTFORD)                                            
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTMT2)                                               
         DC    AL1(MIXILST,MIXIJCLB+MIXISETP,0,0)                               
         DC    AL1(S#BILMT2,O#BILMT2)                                           
         DC    AL2(5242)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(2)                                                           
         DC    AL1(0,0,3)                                                       
         DC    XL6'00'                                                          
         DC    AL1(MIXLITRN,0)                                                  
         DC    AL1(SUB#MT2,CLMMT2Q,SELMT2Q,4)                                   
         DC    AL2(TLMT2LNQ,OPTFORD+OPTFBLDN+OPTFCOMM)                          
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTAALC)                                              
         DC    AL1(MIXILST+MIXIEXIT,0,0,0)                                      
         DC    AL1(S#BILAAL,O#BILAAL)                                           
         DC    AL2(5243)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(2)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    AL1(MIXLICUR,0)                                                  
         DC    AL1(SUB#AALC,CLMAALCQ,SELAALCQ,2)                                
         DC    AL2(TLAALLNQ,OPTFORD+OPTFCOMM)                                   
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTZWR)                                               
*&&UK*&& DC    AL1(MIXINTRY,MIXIJCLB+MIXISETP,0,0)                              
*&&US*&& DC    AL1(MIXINTRY,MIXISETP+MIXINOIN,0,0)                              
         DC    AL1(S#BILZWO,O#BILZOO)                                           
         DC    AL2(5244)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,1)                                                       
         DC    XL6'00'                                                          
         DC    XL24'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTRCVR)                                              
*&&UK*&& DC    AL1(MIXILST,MIXIJCLB+MIXISETP,0,0)                               
*&&US*&& DC    AL1(MIXILST,MIXISETP,0,0)                                        
         DC    AL1(S#BILRCV,O#BILRCV)                                           
         DC    AL2(5230)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(2)                                                           
         DC    AL1(0,0,3)                                                       
         DC    XL6'00'                                                          
         DC    AL1(MIXLITRN+MIXLICUR+MIXLISCT+MIXLIJOB,0)                       
         DC    AL1(SUB#RCVR,CLMRCVRQ,SELRCVRQ,4)                                
         DC    AL2(TLRCVLNQ,OPTFBLDY+OPTFWCT)                                   
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTAULC)                                              
         DC    AL1(MIXILST+MIXIEXIT,0,0,0)                                      
         DC    AL1(S#BILAAL,O#BILAAL)                                           
         DC    AL2(5243)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(2)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    AL1(MIXLICUR,0)                                                  
         DC    AL1(SUB#AULC,CLMAALCQ,SELAALCQ,2)                                
         DC    AL2(TLAALLNQ,OPTFORD+OPTFCOMM)                                   
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTARV)                                               
         DC    AL1(0,MIXIJCLB+MIXISETP,0,0)                                     
         DC    AL1(0,O#BILARV)                                                  
         DC    AL2(5246)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,0)                                                       
         DC    XL6'00'                                                          
         DC    XL24'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTFEE)                                               
         DC    AL1(0,MIXIJCLB+MIXISETP,0,0)                                     
         DC    AL1(S#BILFEE,O#BILFEE)                                           
         DC    AL2(5247)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(1)                                                           
         DC    AL1(0,0,1)                                                       
         DC    XL6'00'                                                          
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    AL2(TLXMINLQ,0)                                                  
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTFLI)                                               
         DC    AL1(MIXILST,MIXIJCLB+MIXISETP,0,0)                               
         DC    AL1(S#BILALC,O#BILFLI)                                           
         DC    AL2(5248)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(2)                                                           
         DC    AL1(0,0,3)                                                       
         DC    XL6'00'                                                          
         DC    AL1(MIXLITRN+MIXLICUR+MIXLISCT+MIXLIJOB+MIXLIDRA,0)              
         DC    AL1(SUB#FLI,CLMFLIQ,SELFLIQ,4)                                   
         DC    AL2(TLXMINLQ,OPTFCOMM+OPTFWCT)                                   
         DC    XL14'00'                                                         
*                                                                               
         DC    AL1(RECBIL,ACTCOL)                                               
         DC    AL1(MIXILST,0,0,0)                                               
         DC    AL1(S#BILCOL,O#BILCOL)                                           
         DC    AL2(5249)                                                        
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(2)                                                           
         DC    AL1(0,0,3)                                                       
         DC    XL6'00'                                                          
         DC    AL1(0,0)                                                         
         DC    AL1(SUB#COL,CLMCOLQ,SELCOLQ,1)                                   
         DC    AL2(TLCOLLNQ,0)                                                  
         DC    XL14'00'                                                         
*                                                                               
*  ??    DC    AL1(RECBIL,ACTHELP)                                              
*  ??    DC    AL1(0,0,0,0)                                                     
*  ??    DC    AL1(S#BILHLP,O#BILHLP)                                           
*  ??    DC    AL2(5250)                                                        
*  ??    DC    AL1(0,0,0,0)                                                     
*  ??    DC    AL1(1)                                                           
*  ??    DC    AL1(0,0,0)                                                       
*  ??    DC    XL6'00'                                                          
*  ??    DC    XL24'00'                                                         
*                                                                               
MIXTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* PROGRAM FUNCTION KEY TABLE                                          *         
***********************************************************************         
         SPACE 1                                                                
PFKTAB   DS    0X                                                               
*                                                                               
PFK1ST   DC    AL1(FF,FF)                                                       
         DC    AL2(PFK1STX+1-PFK1ST)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBIL,ACTSET)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK03)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTAALC)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK04)                                                       
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBIL,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTAULC)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK08)                                                       
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBIL,ACTLFT)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PFK1STX  DC    AL1(EOT)                                                         
*                                                                               
PFKSET   DC    AL1(RECBIL,ACTSET)                                               
         DC    AL2(PFKSETX+1-PFKSET)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTALC)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK02)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
*&&UK*&& DC    AL1(RECBIL,ACTFFRM)                                              
*&&US*&& DC    AL1(RECBIL,ACTWOF)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK03)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTAALC)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
*&&UK                                                                           
         DC    AL1(PFK04)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*&&                                                                             
*                                                                               
*&&US                                                                           
         DC    AL1(PFK04)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTAULC)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*&&                                                                             
*                                                                               
*&&UK                                                                           
         DC    AL1(PFKSAFMQ)                                                    
         DC    AL1(PFKIACTN+PFKIKAPA,0)                                         
         DC    AL1(RECBIL,ACTAFRM)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*&&                                                                             
*                                                                               
*&&US                                                                           
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*&&                                                                             
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
*&&UK*&& DC    AL1(RECBIL,ACTAULC)                                              
*&&US*&& DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
*&&UK*&& DC    AL1(RECBIL,ACTWOF)                                               
*&&US*&& DC    AL1(RECBIL,ACTFFRM)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK08)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTLFT)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTSUM)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTINS)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTPRB)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
*&&UK                                                                           
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*&&                                                                             
*                                                                               
         DC    AL1(PFK14)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTRVL)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTMT1)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK16)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTXFR)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK17)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTRCVR)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
*&&US                                                                           
         DC    AL1(PFK18)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
*&&                                                                             
*                                                                               
         DC    AL1(PFK19)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTFEE)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK20)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTFLI)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*&&US                                                                           
         DC    AL1(PFKSAFMQ)                                                    
         DC    AL1(PFKIACTN+PFKIKAPA,0)                                         
         DC    AL1(RECBIL,ACTAFRM)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*&&                                                                             
*                                                                               
PFKSETX  DC    AL1(EOT)                                                         
*                                                                               
PFKDRA   DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL2(PFKDRAX+1-PFKDRA)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIACTN+PFKIKAPA,0)                                         
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PFKDRAX  DC    AL1(EOT)                                                         
*                                                                               
PFKUPD   DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL2(PFKUPDX+1-PFKUPD)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PFKUPDX  DC    AL1(EOT)                                                         
*                                                                               
PFKALC   DC    AL1(RECBIL,ACTALC)                                               
         DC    AL2(PFKALCX+1-PFKALC)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTZOO)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK02)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTWOF)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK03)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTFFRM)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK04)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTAFRM)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
*&&UK                                                                           
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*&&                                                                             
*                                                                               
         DC    AL1(PFK16)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTXFR)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK17)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTRCVR)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK19)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTFEE)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK20)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTFLI)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
*&&US                                                                           
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*&&                                                                             
*                                                                               
PFKALCX  DC    AL1(EOT)                                                         
*                                                                               
PFKRCVR  DC    AL1(RECBIL,ACTRCVR)                                              
         DC    AL2(PFKRCVRX+1-PFKRCVR)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTZOO)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK02)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTWOF)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK03)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTALC)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK04)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTZWR)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
*&&UK                                                                           
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*&&                                                                             
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTZWO)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK16)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTXFR)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*&&US                                                                           
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*&&                                                                             
*                                                                               
PFKRCVRX DC    AL1(EOT)                                                         
*                                                                               
PFKWOF   DC    AL1(RECBIL,ACTWOF)                                               
         DC    AL2(PFKWOFX+1-PFKWOF)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTZWO)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK03)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTALC)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
*&&UK                                                                           
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*&&                                                                             
*                                                                               
         DC    AL1(PFK16)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTXFR)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK17)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTRCVR)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
*&&US                                                                           
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*&&                                                                             
PFKWOFX  DC    AL1(EOT)                                                         
*                                                                               
PFKZOO   DC    AL1(RECBIL,ACTZOO)                                               
         DC    AL2(PFKZOOX+1-PFKZOO)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTALC)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK03)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTSET)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTZWO)                                               
         DC    AL1(0,0,0,0,0,0),AL2(CSMORD+CSMEXT)                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTZWR)                                               
         DC    AL1(0,0,0,0,0,0),AL2(CSMORD+CSMEXT)                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PFKZOOX  DC    AL1(EOT)                                                         
*                                                                               
PFKZWO   DC    AL1(RECBIL,ACTZWO)                                               
         DC    AL2(PFKZWOX+1-PFKZWO)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTALC)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK03)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTSET)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK04)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTZOO)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTZWR)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PFKZWOX  DC    AL1(EOT)                                                         
*                                                                               
PFKEDT   DC    AL1(RECBIL,ACTEDT)                                               
         DC    AL2(PFKEDTX+1-PFKEDT)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@FIND-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK02)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@CHG-TWAD)                                                 
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK03)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@PRGRP-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK04)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTLPAR)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@PRV-TWAD)                                                 
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@NEXT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK16)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PFKEDTX  DC    AL1(EOT)                                                         
*                                                                               
PFKINS   DC    AL1(RECBIL,ACTINS)                                               
         DC    AL2(PFKINSX+1-PFKINS)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK03)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTFFRM)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK04)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTAFRM)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PFKINSX  DC    AL1(EOT)                                                         
*                                                                               
PFKLPAR  DC    AL1(RECBIL,ACTLPAR)                                              
         DC    AL2(PFKLPARX+1-PFKLPAR)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PFKLPARX DC    AL1(EOT)                                                         
*                                                                               
PFKFT1   DC    AL1(RECBIL,ACTFT1)                                               
         DC    AL2(PFKFT1X+1-PFKFT1)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKCCLRQ)                                                    
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC8CLEAR-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKCPRTQ)                                                    
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@PRINT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKCSAVQ)                                                    
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@SAVE-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*                                                                               
PFKFT1X  DC    AL1(EOT)                                                         
*                                                                               
PFKLFT   DC    AL1(RECBIL,ACTLFT)                                               
         DC    AL2(PFKLFTX+1-PFKLFT)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTFT1)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK02)                                                       
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBIL,ACTSET)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK03)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK04)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*                                                                               
PFKLFTX  DC    AL1(EOT)                                                         
*                                                                               
PFKSUM   DC    AL1(RECBIL,ACTSUM)                                               
         DC    AL2(PFKSUMX+1-PFKSUM)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PFKSUMX  DC    AL1(EOT)                                                         
*                                                                               
PFKLST   DC    AL1(RECBIL,ACTLST)                                               
         DC    AL2(PFKLSTX+1-PFKLST)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTALC)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK02)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTSET)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK03)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTEDT)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK04)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTLPAR)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK05)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*                                                                               
PFKLSTX  DC    AL1(EOT)                                                         
*                                                                               
PFKPRB   DC    AL1(RECBIL,ACTPRB)                                               
         DC    AL2(PFKPRBX+1-PFKPRB)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTREV)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*                                                                               
PFKPRBX  DC    AL1(EOT)                                                         
*                                                                               
PFKREV   DC    AL1(RECBIL,ACTREV)                                               
         DC    AL2(PFKREVX+1-PFKREV)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTZOO)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*                                                                               
PFKREVX  DC    AL1(EOT)                                                         
*                                                                               
PFKRVL   DC    AL1(RECBIL,ACTRVL)                                               
         DC    AL2(PFKRVLX+1-PFKRVL)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKRDFTQ)                                                    
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DRAFT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRTN,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFKRLIVQ)                                                    
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@LIVE-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRTN,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*                                                                               
PFKRVLX  DC    AL1(EOT)                                                         
*                                                                               
PFKMT1   DC    AL1(RECBIL,ACTMT1)                                               
         DC    AL2(PFKMT1X+1-PFKMT1)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTZOO)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK02)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTMT2)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*                                                                               
PFKMT1X  DC    AL1(EOT)                                                         
*                                                                               
PFKMT2   DC    AL1(RECBIL,ACTMT2)                                               
         DC    AL2(PFKMT2X+1-PFKMT2)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTZOO)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK02)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTRVL)                                               
         DC    AL1(0,0,0,0,0,0),AL2(CSMMAC)                                     
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKMDFTQ)                                                    
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DRAFT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRTN,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFKMUPDQ)                                                    
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@CNFRM-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRTN,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*                                                                               
PFKMT2X  DC    AL1(EOT)                                                         
*                                                                               
PFKAALC  DC    AL1(RECBIL,ACTAALC)                                              
         DC    AL2(PFKAALCX+1-PFKAALC)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKAALCQ)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTALC)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFKACFMQ)                                                    
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@CNFRM-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRTN,0,0,0,0,0,0,0)                                       
*&&UK                                                                           
         DC    AL1(PFKAGRPQ)                                                    
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@GROUP-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRTN,0,0,0,0,0,0,0)                                       
*&&                                                                             
         DC    AL1(PFKAFLTQ)                                                    
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@FLT-TWAD)                                                 
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRTN,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK21)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTAULC)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*                                                                               
PFKAALCX DC    AL1(EOT)                                                         
*                                                                               
PFKZWR   DC    AL1(RECBIL,ACTZWR)                                               
         DC    AL2(PFKZWRX+1-PFKZWR)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC8CLEAR-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PFKZWRX  DC    AL1(EOT)                                                         
*                                                                               
PFKAULC  DC    AL1(RECBIL,ACTAULC)                                              
         DC    AL2(PFKAULCX+1-PFKAULC)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKAALCQ)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTALC)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFKACFMQ)                                                    
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@CNFRM-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRTN,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFKAFLTQ)                                                    
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@FLT-TWAD)                                                 
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRTN,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK21)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTAALC)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*                                                                               
PFKAULCX DC    AL1(EOT)                                                         
*                                                                               
PFKXFR   DC    AL1(RECBIL,ACTXFR)                                               
         DC    AL2(PFKXFRX+1-PFKXFR)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTZOO)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK02)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTWOF)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK03)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTALC)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
*&&UK                                                                           
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*&&                                                                             
*                                                                               
         DC    AL1(PFK17)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTRCVR)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
*&&US                                                                           
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*&&                                                                             
*                                                                               
PFKXFRX  DC    AL1(EOT)                                                         
*                                                                               
PFKFEE   DC    AL1(RECBIL,ACTFEE)                                               
         DC    AL2(PFKFEEX+1-PFKFEE)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTFLI)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK02)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTALC)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PFKFEEX  DC    AL1(EOT)                                                         
*                                                                               
PFKFLI   DC    AL1(RECBIL,ACTFLI)                                               
         DC    AL2(PFKFLIX+1-PFKFLI)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTFEE)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIPOS,0,0,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(PFK02)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTALC)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
*&&UK                                                                           
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*&&                                                                             
*                                                                               
*&&US                                                                           
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*&&                                                                             
*                                                                               
PFKFLIX  DC    AL1(EOT)                                                         
*                                                                               
PFKCOL   DC    AL1(RECBIL,ACTCOL)                                               
         DC    AL2(PFKCOLX+1-PFKCOL)                                            
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
*&&UK                                                                           
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*&&                                                                             
*                                                                               
*&&US                                                                           
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(PFKIRFSH,0,0,0,0,0,0,0)                                      
*&&                                                                             
*                                                                               
PFKCOLX  DC    AL1(EOT)                                                         
*                                                                               
PFKHELP  DC    AL1(RECBIL,ACTHELP)                                              
         DC    AL2(PFKHELPX+1-PFKHELP)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFK01)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@SEL-TWAD)                                                 
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PFKHELPX DC    AL1(EOT)                                                         
*                                                                               
PFKTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* SELECT FIELD ACTION TABLE                                           *         
***********************************************************************         
         SPACE 1                                                                
SELTAB   DS    0X                                                               
*                                                                               
SELALC   DC    AL1(SELALCQ,0)      * ALLOCATE / WO *                            
         DC    AL2(SELALCX+1-SELALC)                                            
*                                                                               
         DC    AL2(UC@ZOOM-TWAD)                                                
         DC    AL2(LC@ZOOM-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    XL2'00'                                                          
         DC    AL1(RECBIL,ACTZOO,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC3ZMWO-TWAD)                                                
         DC    AL2(LC3ZMWO-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    XL2'00'                                                          
         DC    AL1(RECBIL,ACTZWO,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@YES-TWAD)                                                 
         DC    AL2(LC@YES-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(SELTIGET+SELTIUPD)                                           
         DC    AL2(0)                                                           
         DC    AL1(1,0)                                                         
         DC    AL1(0,0,1,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@CLEAR-TWAD)                                               
         DC    AL2(LC@CLEAR-TWAD)                                               
         DC    AL1(SELTIEOL+SELTIEOP+SELTIRED)                                  
         DC    AL1(SELTIGET+SELTIUPD)                                           
         DC    AL2(0)                                                           
         DC    AL1(2,0)                                                         
         DC    AL1(0,0,1,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@WRTF-TWAD)                                                
         DC    AL2(LC@WRTF-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(SELTIGET+SELTIUPD)                                           
         DC    AL2(0)                                                           
         DC    AL1(3,0)                                                         
         DC    AL1(0,0,1,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@DEL-TWAD)                                                 
         DC    AL2(UC@DEL-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTIEXC)                                  
         DC    AL1(SELTIGET+SELTIUPD)                                           
         DC    AL2(0)                                                           
         DC    AL1(4,0)                                                         
         DC    AL1(0,0,1,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@ADVNC-TWAD)                                               
         DC    AL2(LC@ADVNC-TWAD)                                               
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    XL2'00'                                                          
         DC    AL1(RECBIL,ACTINS,2,0)                                           
         DC    XL2'00'                                                          
*                                                                               
SELALCX  DC    AL1(EOT)                                                         
*                                                                               
SELLFT   DC    AL1(SELLFTQ,0)      * FORMAT CONTROL LIST *                      
         DC    AL2(SELLFTX+1-SELLFT)                                            
*                                                                               
         DC    AL2(UC@SEL-TWAD)                                                 
         DC    AL2(LC@SEL-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1,0)                                                         
         DC    AL1(RECBIL,ACTFT1,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
*        DC    AL2(UC@FMADD-TWAD)                                               
*        DC    AL2(LC@FMADD-TWAD)                                               
*        DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
*        DC    AL1(0)                                                           
*        DC    AL2(0)                                                           
*        DC    AL1(1,0)                                                         
*        DC    AL1(RECBIL,ACTFT2,1,0)                                           
*        DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@DEL-TWAD)                                                 
         DC    AL2(UC@DEL-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(2,0)                                                         
         DC    AL1(RECBIL,ACTFT1,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@RSR-TWAD)                                                 
         DC    AL2(UC@RSR-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(3,0)                                                         
         DC    AL1(RECBIL,ACTFT1,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
SELLFTX  DC    AL1(EOT)                                                         
*                                                                               
SELLST   DC    AL1(SELLSTQ,0)      * BILL LIST *                                
         DC    AL2(SELLSTX+1-SELLST)                                            
*                                                                               
         DC    AL2(UC@ALLOC-TWAD)                                               
         DC    AL2(LC@ALLOC-TWAD)                                               
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1,0)                                                         
         DC    AL1(RECBIL,ACTALC,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@SETUP-TWAD)                                               
         DC    AL2(LC@SETUP-TWAD)                                               
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(2,0)                                                         
         DC    AL1(RECBIL,ACTSET,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@LSPAR-TWAD)                                               
         DC    AL2(LC@LSPAR-TWAD)                                               
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(3,0)                                                         
         DC    AL1(RECBIL,ACTLPAR,1,0)                                          
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@EDIT-TWAD)                                                
         DC    AL2(LC@EDIT-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(4,0)                                                         
         DC    AL1(RECBIL,ACTEDT,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@UPDT-TWAD)                                                
         DC    AL2(LC@UPDT-TWAD)                                                
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(5,0)                                                         
         DC    AL1(RECBIL,ACTUPD,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@DRAFT-TWAD)                                               
         DC    AL2(LC@DRAFT-TWAD)                                               
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(6,0)                                                         
         DC    AL1(RECBIL,ACTDRA,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@PRINT-TWAD)                                               
         DC    AL2(LC@PRINT-TWAD)                                               
         DC    AL1(SELTIRED)                                                    
         DC    AL1(SELTISTP)                                                    
         DC    AL2(0)                                                           
         DC    AL1(7,0)                                                         
         DC    AL1(RECBIL,ACTDRA,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@DEL-TWAD)                                                 
         DC    AL2(UC@DEL-TWAD)                                                 
         DC    AL1(SELTIRED)                                                    
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(8,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@RSR-TWAD)                                                 
         DC    AL2(UC@RSR-TWAD)                                                 
         DC    AL1(SELTIRED)                                                    
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(9,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
SELLSTX  DC    AL1(EOT)                                                         
*                                                                               
SELPRB   DC    AL1(SELPRBQ,0)      * PREVBILL LIST *                            
         DC    AL2(SELPRBX+1-SELPRB)                                            
*                                                                               
         DC    AL2(UC@RVRS-TWAD)                                                
         DC    AL2(LC@RVRS-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1,0)                                                         
         DC    AL1(RECBIL,ACTREV,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC3AUREV-TWAD)                                               
         DC    AL2(LC@AUREV-TWAD)                                               
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(2,0)                                                         
         DC    AL1(RECBIL,ACTEDT,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@CLEAR-TWAD)                                               
         DC    AL2(LC@CLEAR-TWAD)                                               
         DC    AL1(SELTIEOL+SELTIEOP+SELTIRED)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(3,0)                                                         
         DC    AL1(0,0,1,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
SELPRBX  DC    AL1(EOT)                                                         
*                                                                               
SELREV   DC    AL1(SELREVQ,0)      * REVERSE *                                  
         DC    AL2(SELREVX+1-SELREV)                                            
*                                                                               
         DC    AL2(UC@ZOOM-TWAD)                                                
         DC    AL2(LC@ZOOM-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    XL2'00'                                                          
         DC    AL1(RECBIL,ACTZOO,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@YES-TWAD)                                                 
         DC    AL2(LC@YES-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTIRED)                                  
         DC    AL1(SELTIGET+SELTIUPD)                                           
         DC    AL2(0)                                                           
         DC    AL1(1,0)                                                         
         DC    AL1(0,0,1,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@CLEAR-TWAD)                                               
         DC    AL2(LC@CLEAR-TWAD)                                               
         DC    AL1(SELTIEOL+SELTIEOP+SELTIRED)                                  
         DC    AL1(SELTIGET+SELTIUPD)                                           
         DC    AL2(0)                                                           
         DC    AL1(2,0)                                                         
         DC    AL1(0,0,1,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
SELREVX  DC    AL1(EOT)                                                         
*                                                                               
SELRVL   DC    AL1(SELRVLQ,0)      * REVALUE *                                  
         DC    AL2(SELRVLX+1-SELRVL)                                            
*                                                                               
         DC    AL2(UC@CLEAR-TWAD)                                               
         DC    AL2(LC@CLEAR-TWAD)                                               
         DC    AL1(SELTIEOL+SELTIEOP+SELTIRED)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@BIL-TWAD)                                                 
         DC    AL2(LC@BIL-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTIRED)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(2,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@MCH-TWAD)                                                 
         DC    AL2(LC@MCH-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTIRED)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(3,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@YES-TWAD)                                                 
         DC    AL2(LC@YES-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTIRED)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(4,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
SELRVLX  DC    AL1(EOT)                                                         
*                                                                               
SELMT1   DC    AL1(SELMT1Q,0)      * MATCH 1 *                                  
         DC    AL2(SELMT1X+1-SELMT1)                                            
*                                                                               
         DC    AL2(UC@ZOOM-TWAD)                                                
         DC    AL2(LC@ZOOM-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    XL2'00'                                                          
         DC    AL1(RECBIL,ACTZOO,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@INV-TWAD)                                                 
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1,0)                                                         
         DC    AL1(RECBIL,ACTMT2,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
SELMT1X  DC    AL1(EOT)                                                         
*                                                                               
SELMT2   DC    AL1(SELMT2Q,0)      * MATCH 2 *                                  
         DC    AL2(SELMT2X+1-SELMT2)                                            
*                                                                               
         DC    AL2(UC@CLEAR-TWAD)                                               
         DC    AL2(LC@CLEAR-TWAD)                                               
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@YES-TWAD)                                                 
         DC    AL2(LC@YES-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(2,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@ZOOM-TWAD)                                                
         DC    AL2(LC@ZOOM-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    XL2'00'                                                          
         DC    AL1(RECBIL,ACTZOO,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
SELMT2X  DC    AL1(EOT)                                                         
*                                                                               
SELAALC  DC    AL1(SELAALCQ,0)      * AUTO ALLOCATE *                           
         DC    AL2(SELAALCX+1-SELAALC)                                          
*                                                                               
         DC    AL2(UC@ALLOC-TWAD)                                               
         DC    AL2(LC@ALLOC-TWAD)                                               
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1,0)                                                         
         DC    AL1(RECBIL,ACTALC,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@CLEAR-TWAD)                                               
         DC    AL2(LC@CLEAR-TWAD)                                               
         DC    AL1(SELTIEOL+SELTIEOP+SELTIRED)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(2,0)                                                         
         DC    AL1(0,0,1,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@EXCD1-TWAD)                                               
         DC    AL2(LC@EXCD1-TWAD)                                               
         DC    AL1(SELTIEOL+SELTIEOP+SELTIEXC)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,1,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@EXCD2-TWAD)                                               
         DC    AL2(LC@EXCD2-TWAD)                                               
         DC    AL1(SELTIEOL+SELTIEOP+SELTIEXC)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,1,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
SELAALCX DC    AL1(EOT)                                                         
*                                                                               
SELXFR   DC    AL1(SELXFRQ,0)      * TRANSFER ETC. *                            
         DC    AL2(SELXFRX+1-SELXFR)                                            
*                                                                               
         DC    AL2(UC@ZOOM-TWAD)                                                
         DC    AL2(LC@ZOOM-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    XL2'00'                                                          
         DC    AL1(RECBIL,ACTZOO,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC3ZMWO-TWAD)                                                
         DC    AL2(LC3ZMWO-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    XL2'00'                                                          
         DC    AL1(RECBIL,ACTZWO,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@YES-TWAD)                                                 
         DC    AL2(LC@YES-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(SELTIGET+SELTIUPD)                                           
         DC    AL2(0)                                                           
         DC    AL1(1,0)                                                         
         DC    AL1(0,0,1,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@CLEAR-TWAD)                                               
         DC    AL2(LC@CLEAR-TWAD)                                               
         DC    AL1(SELTIEOL+SELTIEOP+SELTIRED)                                  
         DC    AL1(SELTIGET+SELTIUPD)                                           
         DC    AL2(0)                                                           
         DC    AL1(2,0)                                                         
         DC    AL1(0,0,1,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@ALLOC-TWAD)                                               
         DC    AL2(LC@ALLOC-TWAD)                                               
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(SELTIGET+SELTIUPD)                                           
         DC    AL2(0)                                                           
         DC    AL1(3,0)                                                         
         DC    AL1(0,0,1,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
SELXFRX  DC    AL1(EOT)                                                         
*                                                                               
SELRCVR  DC    AL1(SELRCVRQ,0)     * WRITE-OFF RECOVERY *                       
         DC    AL2(SELRCVRX+1-SELRCVR)                                          
*                                                                               
         DC    AL2(UC@ZOOM-TWAD)                                                
         DC    AL2(LC@ZOOM-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    XL2'00'                                                          
         DC    AL1(RECBIL,ACTZOO,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC3ZMWO-TWAD)                                                
         DC    AL2(LC3ZMWO-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    XL2'00'                                                          
         DC    AL1(RECBIL,ACTZWO,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC3ZMWR-TWAD)                                                
         DC    AL2(LC3ZMWR-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    XL2'00'                                                          
         DC    AL1(RECBIL,ACTZWR,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@YES-TWAD)                                                 
         DC    AL2(LC@YES-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTIRED)                                  
         DC    AL1(SELTIGET+SELTIUPD)                                           
         DC    AL2(0)                                                           
         DC    AL1(1,0)                                                         
         DC    AL1(0,0,1,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@CLEAR-TWAD)                                               
         DC    AL2(LC@CLEAR-TWAD)                                               
         DC    AL1(SELTIEOL+SELTIEOP+SELTIRED)                                  
         DC    AL1(SELTIGET+SELTIUPD)                                           
         DC    AL2(0)                                                           
         DC    AL1(2,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
SELRCVRX DC    AL1(EOT)                                                         
*                                                                               
SELFLI   DC    AL1(SELFLIQ,0)      * FEE ADJUSTMENT LIST *                      
         DC    AL2(SELFLIX+1-SELFLI)                                            
*                                                                               
         DC    AL2(UC@SEL-TWAD)                                                 
         DC    AL2(LC@SEL-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTFEE,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@ADD-TWAD)                                                 
         DC    AL2(LC@ADD-TWAD)                                                 
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(RECBIL,ACTFEE,2,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@DEL-TWAD)                                                 
         DC    AL2(UC@DEL-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTIEXC)                                  
         DC    AL1(SELTIGET+SELTIUPD)                                           
         DC    AL2(0)                                                           
         DC    AL1(1,0)                                                         
         DC    AL1(0,0,0,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
SELFLIX DC     AL1(EOT)                                                         
*                                                                               
SELCOL   DC    AL1(SELCOLQ,0)      * COLUMNS LIST *                             
         DC    AL2(SELCOLX+1-SELCOL)                                            
*                                                                               
SELCOLX DC     AL1(EOT)                                                         
*                                                                               
SELTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* COLUMN TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING PRORATAD,R0                                                      
CLMTAB   DS    0X                                                               
         SPACE 1                                                                
***********************************************************************         
* ALLOCATE / WRITE-OFF                                                *         
***********************************************************************         
         SPACE 1                                                                
CLMALC   DC    AL1(CLMALCQ)                                                     
         DC    AL2(CLMALCX+1-CLMALC)                                            
         DC    AL2(CLM#TKY+CLM#PRO+CLM#BIL+CLM#TRN),AL1(0)                      
*                                                                               
         DC    AL1(PRO#NET)        * NET AVAILABLE *                            
         DC    AL1(1,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIAMT+CLMIHEAD,CLMIEOL+CLMIPROR)                           
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PM$ANVBL,PM$FNVBL)                                    
         DC    XL6'00'                                                          
*&&UK*&& DCDDL AC#NETAV,14,L       DCDD FOR HEADING 1                           
*&&UK*&& DCDDL AC#NETAV,14,LU      DCDD FOR HEADING 2                           
*&&US*&& DCDDL AC#NET,14,L         DCDD FOR HEADING 1                           
*&&US*&& DCDDL AC#AVAIL,14,L       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#WTF)        * WRITTEN-OFF *                              
         DC    AL1(2,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIAMT+CLMIHEAD,CLMIPROR)                                   
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PP$AWOFF,PP$FWOFF)                                    
         DC    XL6'00'                                                          
         DCDDL AC#WRTAM,14,L       DCDD FOR HEADING 1                           
         DCDDL AC#WRTAM,14,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#ALC)        * ALLOCATED AMOUNT *                         
         DC    AL1(03,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIAMT+CLMIHEAD,CLMIPROR)                                   
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PP$AALLO,PP$FALLO)                                    
         DC    XL6'00'                                                          
         DCDDL AC#ALCTD,14,L       DCDD FOR HEADING 1                           
         DCDDL AC#ALCTD,14,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#COM)        * COMMISSION *                               
         DC    AL1(04,0,0)         RTN/HELP/SEC                                 
*&&UK*&& DC    AL1(CLMIAMT+CLMIHEAD,CLMIPROR)                                   
*&&US*&& DC    AL1(CLMIPRO+CLMIAMT+CLMIHEAD,CLMIPROR)                           
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PM$ACVBL,PM$FCVBL)                                    
         DC    XL6'00'                                                          
         DCDDL AC#CMN,14,L         DCDD FOR HEADING 1                           
         DCDDL AC#CMN,14,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#HRS)        * HOURS *                                    
         DC    AL1(05,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIAMT,CLMIPROR)                                            
         DC    AL1(7,7,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(7,7,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PM$HRVBL,0)                                           
         DC    XL6'00'                                                          
         DCDDL AC#HOURS,7,L        DCDD FOR HEADING 1                           
         DCDDL AC#HOURS,7,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(BIL#WOA)        * WRITE-OFF ACCOUNT *                        
         DC    AL1(06,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,0)            INDS1/INDS2                                  
         DC    AL1(15,15,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(15,15,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL5'00'                                                   
         DC    AL1(BIL@WOA),XL4'00' OVERRIDE DISPLAY ROUTINE                    
         DCDDL AC#WRTFA,15,L       DCDD FOR HEADING 1                           
         DCDDL AC#WRTFA,15,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(BIL#OFF)        OFFICE CODE                                  
         DC    AL1(07,0,0)         RTN/HELP/SEC                                 
*&&UK*&& DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
*&&US*&& DC    AL1(0,0)            INDS1/INDS2                                  
         DC    AL1(6,2,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(6,2,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL5'00'                                                   
         DC    AL1(BIL@OFF),XL4'00' OVERRIDE DISPLAY ROUTINE                    
         DCDDL AC#OFF,6,L          DCDD FOR HEADING 1                           
         DCDDL AC#OFF,6,LU         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(BIL#PAR)        * PARAGRAPH CODE *                           
         DC    AL1(08,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,0)            INDS1/INDS2                                  
         DC    AL1(4,3,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(4,3,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL5'00'                                                   
         DC    AL1(BIL@PAR),XL4'00' OVERRIDE DISPLAY ROUTINE                    
         DCDDL AC#PRGRP,4,R        DCDD FOR HEADING 1                           
         DCDDL AC#PRGRP,4,RU       DCDD FOR HEADING 2                           
*                                                                               
CLMALCX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* FORMAT LIST                                                         *         
***********************************************************************         
         SPACE 1                                                                
CLMLFT   DC    AL1(CLMLFTQ)                                                     
         DC    AL2(CLMLFTX+1-CLMLFT)                                            
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    C' '                * FORMAT *                                   
         DC    AL1(64,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(7,7,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FRMAT,7,L        DCDD FOR HEADING 1                           
         DCDDL AC#FRMAT,7,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(LFT#GRB)        * GROUPING BASIS *                           
         DC    AL1(65,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(24,24,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#GRPNG,24,L       DCDD FOR HEADING 1                           
         DCDDL AC#GRPNG,24,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(LFT#NAM)        * NAME *                                     
         DC    AL1(66,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(36,36,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#NAME,36,L        DCDD FOR HEADING 1                           
         DCDDL AC#NAME,36,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(LFT#01)         * FORMAT OPTION 01 **                        
         DC    AL1(01,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,01,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP01,32,H       DCDD FOR HEADING 1                           
         DC    XL4'00'             DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(LFT#02)         * FORMAT OPTION 02 **                        
         DC    AL1(02,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,03,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP02,32,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#03)         * FORMAT OPTION 03 **                        
         DC    AL1(03,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,03,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP03,32,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#04)         * FORMAT OPTION 04 **                        
         DC    AL1(04,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,10,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP04,32,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#05)         * FORMAT OPTION 05 **                        
         DC    AL1(05,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,02,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP05,32,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#06)         * FORMAT OPTION 06 **                        
         DC    AL1(06,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,05,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP06,32,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#07)         * FORMAT OPTION 07 **                        
         DC    AL1(07,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,05,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP07,32,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#23)         * FORMAT OPTION 07 **                        
         DC    AL1(23,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,08,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0,0,0)          MASK                                         
         DC    AL1(CTRYGER)        GERMANY ONLY                                 
         DC    XL5'00'                                                          
         DCDDL AC#FOP23,32,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#08)         * FORMAT OPTION 08 **                        
         DC    AL1(08,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,05,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP08,32,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#09)         * FORMAT OPTION 09 **                        
         DC    AL1(09,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,08,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP09,32,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#10)         * FORMAT OPTION 10 **                        
         DC    AL1(10,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,05,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP10,32,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#11)         * FORMAT OPTION 11 **                        
         DC    AL1(11,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,05,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP11,32,H                                                    
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#12)         * FORMAT OPTION 12 **                        
         DC    AL1(12,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,05,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP12,32,H                                                    
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#18)         * FORMAT OPTION 18 **                        
         DC    AL1(18,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,05,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP18,32,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#19)         * FORMAT OPTION 19 **                        
         DC    AL1(19,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,05,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP19,32,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#22)         * FORMAT OPTION 22 **                        
         DC    AL1(22,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,05,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP22,32,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#28)         * FORMAT OPTION 28 **                        
         DC    AL1(28,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(16,05,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP28,32,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#13)         * FORMAT OPTION 13 **                        
         DC    AL1(13,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP13,40,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#24)         * FORMAT OPTION 13 **                        
         DC    AL1(24,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP24,40,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#25)         * FORMAT OPTION 13 **                        
         DC    AL1(25,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP25,40,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#26)         * FORMAT OPTION 13 **                        
         DC    AL1(26,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP26,40,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#27)         * FORMAT OPTION 13 **                        
         DC    AL1(27,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP27,40,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#14)         * FORMAT OPTION 14 **                        
         DC    AL1(14,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP14,40,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#15)         * FORMAT OPTION 15 **                        
         DC    AL1(15,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP15,40,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#16)         * FORMAT OPTION 16 **                        
         DC    AL1(16,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP16,40,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#17)         * FORMAT OPTION 17 **                        
         DC    AL1(17,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP17,40,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#20)         * FORMAT OPTION 20 **                        
         DC    AL1(20,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP20,40,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
         DC    AL1(LFT#21)         * FORMAT OPTION 21 **                        
         DC    AL1(21,0,0)         RTN/HELP/SEC                                 
         DC    AL1(0,CLMIDEFO)     INDS1/INDS2                                  
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FOP21,40,H       DCDD FOR HEADING 1                           
         DC    4X'00'                                                           
*                                                                               
CLMLFTX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* BILL LIST                                                           *         
***********************************************************************         
         SPACE 1                                                                
CLMLST   DC    AL1(CLMLSTQ)                                                     
         DC    AL2(CLMLSTX+1-CLMLST)                                            
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    C' '                * KEY *                                      
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO+CLMIHEAD,0)                                  
         DC    AL1(19,19,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DC    4X'00'              DCDD FOR HEADING 1                           
         DC    4X'00'              DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(LST#BIL)        * DATE BILLED *                              
         DC    AL1(1,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(8,8,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#BLD,8,L          DCDD FOR HEADING 1                           
         DCDDL AC#BLD,8,LU         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(LST#NET)        * NET *                                      
         DC    AL1(2,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(13,13,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#NET,12,R         DCDD FOR HEADING 1                           
         DCDDL AC#NET,12,RU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(LST#COM)        * COMMISSION *                               
         DC    AL1(3,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(13,13,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CMN,12,R         DCDD FOR HEADING 1                           
         DCDDL AC#CMN,12,RU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(LST#CRT)        * DATE CREATED *                             
         DC    AL1(4,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(8,8,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CRTD,8,L         DCDD FOR HEADING 1                           
         DCDDL AC#CRTD,8,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(LST#EXP)        * DATE EXPIRES *                             
         DC    AL1(5,0,0)          RTN/HELP/SEC                                 
         DC    AL1(0,0)            INDS1/INDS2                                  
         DC    AL1(8,8,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#EXPIR,8,L        DCDD FOR HEADING 1                           
         DCDDL AC#EXPIR,8,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(LST#CUR)        * CURRENCY *                                 
         DC    AL1(6,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(3,3,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CURRY,3,L        DCDD FOR HEADING 1                           
         DC    X'00'                                                            
         DCDDL AC#CURRY,3,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(LST#FMT)        * FORMAT *                                   
         DC    AL1(7,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(6,3,2)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#FRMAT,6,R        DCDD FOR HEADING 1                           
         DCDDL AC#FRMAT,6,RU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(LST#LPR)        * DATE/TIME LAST PRINTED *                   
         DC    AL1(8,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(22,22,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#LSTPR,17,L       DCDD FOR HEADING 1                           
         DCDDL AC#LSTPR,17,LU      DCDD FOR HEADING 2                           
*                                                                               
CLMLSTX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* PREVBILL LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
CLMPRB   DC    AL1(CLMPRBQ)                                                     
         DC    AL2(CLMPRBX+1-CLMPRB)                                            
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    C' '                * BILL NUMBER *                              
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0)                                           
         DC    AL1(6,6,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#BILC,6,L         DCDD FOR HEADING 1                           
         DCDDL AC#BILC,6,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    C' '                * DATE *                                     
         DC    AL1(1,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(8,8,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#DATE,8,L         DCDD FOR HEADING 1                           
         DCDDL AC#DATE,8,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRB#NET)        * NET *                                      
         DC    AL1(2,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(13,13,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#NET,12,R         DCDD FOR HEADING 1                           
         DCDDL AC#NET,12,RU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRB#COM)        * COMMISSION *                               
         DC    AL1(3,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(13,13,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CMN,12,R         DCDD FOR HEADING 1                           
         DCDDL AC#CMN,12,RU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRB#CUR)        * CURRENCY *                                 
         DC    AL1(4,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(3,3,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CURRY,3,L        DCDD FOR HEADING 1                           
         DC    X'00'                                                            
         DCDDL AC#CURRY,3,LU       DCDD FOR HEADING 2                           
*                                                                               
CLMPRBX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* REVALUE LIST                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLMRVL   DC    AL1(CLMRVLQ)                                                     
         DC    AL2(CLMRVLX+1-CLMRVL)                                            
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    C' '                * WORKCODE *                                 
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(2,2,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(2,2,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#WC,2,L           DCDD FOR HEADING 1                           
         DC    XL1'00'                                                          
         DCDDL AC#WC,2,LU          DCDD FOR HEADING 2                           
*                                                                               
         DC    C' '                * SUPPLIER (CONTRA) *                        
         DC    AL1(1,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
*&&UK*&& DCDDL AC#SUP,14,L         DCDD FOR HEADING 1                           
*&&UK*&& DCDDL AC#SUP,14,LU        DCDD FOR HEADING 2                           
*&&US*&& DCDDL AC#CTR,14,L         DCDD FOR HEADING 1                           
*&&US*&& DCDDL AC#CTR,14,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    C' '                * REFERENCE *                                
         DC    AL1(2,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(6,6,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(6,6,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#REF,6,L          DCDD FOR HEADING 1                           
         DCDDL AC#REF,6,LU         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(RVL#CUR)        * CURRENCY *                                 
         DC    AL1(3,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(3,3,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CURRY,3,L        DCDD FOR HEADING 1                           
         DC    X'00'                                                            
         DCDDL AC#CURRY,3,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(RVL#CAMT)       * CURRENCY AMOUNT *                          
         DC    AL1(4,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)                                                   
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CURRY,13,R       DCDD FOR HEADING 1                           
         DCDDL AC#AMT,13,R         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(RVL#NRAT)       * NEW RATE *                                 
         DC    AL1(5,0,0)          RTN/HELP/SEC                                 
         DC    AL1(0,CLMIEOP+CLMIEOL+CLMIDEFO)                                  
         DC    AL1(12,12,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#NEWRT,12,L       DCDD FOR HEADING 1                           
         DCDDL AC#NEWRT,12,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(RVL#NAMT)       * NEW AMOUNT *                               
         DC    AL1(6,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIHEAD+CLMISCR,0)                                  
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#NEWAM,13,R       DCDD FOR HEADING 1                           
         DCDDL AC#NEWAM,13,RU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(RVL#ORAT)       * OLD RATE *                                 
         DC    AL1(7,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(11,11,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#OLDRT,11,C       DCDD FOR HEADING 1                           
         DCDDL AC#OLDRT,11,CU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(RVL#OAMT)       * OLD AMOUNT *                               
         DC    AL1(8,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIHEAD,0)                                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#OLDAM,13,R       DCDD FOR HEADING 1                           
         DCDDL AC#OLDAM,13,RU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(RVL#EXD)        * EXCHANGE DIFFERENCE *                      
         DC    AL1(9,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIHEAD+CLMISCR,0)                                  
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#EXDIF,13,R       DCDD FOR HEADING 1                           
         DCDDL AC#EXDIF,13,RU      DCDD FOR HEADING 2                           
*                                                                               
CLMRVLX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* MATCH 1 LIST                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLMMT1   DC    AL1(CLMMT1Q)                                                     
         DC    AL2(CLMMT1X+1-CLMMT1)                                            
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    C' '                * WORKCODE *                                 
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(2,2,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(2,2,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#WC,2,L           DCDD FOR HEADING 1                           
         DC    XL1'00'                                                          
         DCDDL AC#WC,2,LU          DCDD FOR HEADING 2                           
*                                                                               
         DC    C' '                * SUPPLIER (CONTRA) *                        
         DC    AL1(1,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
*&&UK*&& DCDDL AC#SUP,14,L         DCDD FOR HEADING 1                           
*&&UK*&& DCDDL AC#SUP,14,LU        DCDD FOR HEADING 2                           
*&&US*&& DCDDL AC#CTR,14,L         DCDD FOR HEADING 1                           
*&&US*&& DCDDL AC#CTR,14,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    C' '                * REFERENCE *                                
         DC    AL1(2,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(6,6,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(6,6,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#REF,6,L          DCDD FOR HEADING 1                           
         DCDDL AC#REF,6,LU         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT1#BIL)        * BILL NUMBER *                              
         DC    AL1(3,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(6,6,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#BILC,6,L         DCDD FOR HEADING 1                           
         DCDDL AC#BILC,6,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT1#CUR)        * CURRENCY *                                 
         DC    AL1(4,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(3,3,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CURRY,3,L        DCDD FOR HEADING 1                           
         DC    X'00'                                                            
         DCDDL AC#CURRY,3,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT1#NET)        * NET *                                      
         DC    AL1(5,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#NET,13,R         DCDD FOR HEADING 1                           
         DCDDL AC#NET,13,RU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT1#COM)        * COMMISSION *                               
         DC    AL1(6,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CMN,13,R         DCDD FOR HEADING 1                           
         DCDDL AC#CMN,13,RU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT1#EXC)        * EXCHANGE RATE *                            
         DC    AL1(7,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(10,10,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#EXCHR,10,L       DCDD FOR HEADING 1                           
         DCDDL AC#EXCHR,10,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT1#NETA)       * NET (AGENCY CURRENCY) *                    
         DC    AL1(8,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIHEAD,0)                                          
         DC    AL1(14,14,0)        HEAD WIDTH/FI LD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#NET,13,R         DCDD FOR HEADING 1                           
         DCDDL AC#NET,13,RU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT1#COMA)       * COMMISSION (AGENCY CURRENCY) *             
         DC    AL1(9,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIHEAD,0)                                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CMN,13,R         DCDD FOR HEADING 1                           
         DCDDL AC#CMN,13,RU        DCDD FOR HEADING 2                           
*                                                                               
CLMMT1X  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* MATCH 2 LIST (INVOICE)                                              *         
***********************************************************************         
         SPACE 1                                                                
CLMMT2   DC    AL1(CLMMT2Q)                                                     
         DC    AL2(CLMMT2X+1-CLMMT2)                                            
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    C' '                * WORKCODE *                                 
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO+CLMISCR,0)                                   
         DC    AL1(2,2,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(2,2,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#WC,2,L           DCDD FOR HEADING 1                           
         DC    XL1'00'                                                          
         DCDDL AC#WC,2,LU          DCDD FOR HEADING 2                           
*                                                                               
         DC    C' '                * SUPPLIER (CONTRA) *                        
         DC    AL1(1,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO+CLMISCR,0)                                   
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
*&&UK*&& DCDDL AC#SUP,14,L         DCDD FOR HEADING 1                           
*&&UK*&& DCDDL AC#SUP,14,LU        DCDD FOR HEADING 2                           
*&&US*&& DCDDL AC#CTR,14,L         DCDD FOR HEADING 1                           
*&&US*&& DCDDL AC#CTR,14,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    C' '                * REFERENCE *                                
         DC    AL1(2,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO+CLMISCR,0)                                   
         DC    AL1(6,6,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(6,6,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#REF,6,L          DCDD FOR HEADING 1                           
         DCDDL AC#REF,6,LU         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT2#NMB)        * NET MATCHED (BILL CURRENCY) *              
         DC    AL1(3,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMISCR+CLMIHEAD,CLMIDEFO)                                   
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#NETMT,14,L       DCDD FOR HEADING 1                           
         DCDDL AC#NETMT,14,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT2#NAB)        * NET AVAILABLE (BILL CURRENCY) *            
         DC    AL1(4,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIHEAD,0)                                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#NETAV,13,R       DCDD FOR HEADING 1                           
         DCDDL AC#NETAV,13,RU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT2#CMB)        * COMMISSION MATCHED (BILL) *                
         DC    AL1(5,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMISCR+CLMIHEAD,CLMIDEFO)                                   
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CMNMT,14,L       DCDD FOR HEADING 1                           
         DCDDL AC#CMNMT,14,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT2#NMA)        * NET MATCHED (AGENCY CURRENCY) *            
         DC    AL1(6,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMISCR+CLMIHEAD,0)                                  
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(CSMMAC),XL10'00' MASK                                        
         DCDDL AC#NETMT,13,R       DCDD FOR HEADING 1                           
         DCDDL AC#NETMT,13,RU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT2#NAA)        * NET AVAILAVLE (AGENCY CURRENCY) *          
         DC    AL1(7,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIHEAD,0)                                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(CSMMAC),XL10'00' MASK                                        
         DCDDL AC#NETAV,13,R       DCDD FOR HEADING 1                           
         DCDDL AC#NETAV,13,RU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT2#CMA)        * COMMISSION MATCHED (AGENCY) *              
         DC    AL1(8,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMISCR+CLMIHEAD,0)                                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(CSMMAC),XL10'00' MASK                                        
         DCDDL AC#CMNMT,13,R       DCDD FOR HEADING 1                           
         DCDDL AC#CMNMT,13,RU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT2#COMR)       * COMMISSION RATE *                          
         DC    AL1(9,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(7,7,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(7,7,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CMN,7,C          DCDD FOR HEADING 1                           
         DCDDL AC#RATE,7,C         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT2#MEXC)       * MATCH EXCHANGE RATE *                      
         DC    AL1(10,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(11,11,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(CSMMAC),XL10'00' MASK                                        
         DCDDL AC#MCH,11,C         DCDD FOR HEADING 1                           
         DCDDL AC#EXCHR,11,C       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT2#IEXC)       * INVIOCE EXCHANGE RATE *                    
         DC    AL1(11,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(11,11,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(CSMMAC),XL10'00' MASK                                        
         DCDDL AC#INV,11,C         DCDD FOR HEADING 1                           
         DCDDL AC#EXCHR,11,C       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT2#HRSM)       * HOURS MATCHED *                            
         DC    AL1(12,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMISCR,0)                                           
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00' MASK                                             
         DCDDL AC#HRSMT,13,R       DCDD FOR HEADING 1                           
         DCDDL AC#HRSMT,13,RU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(MT2#HRSA)       * HOURS AVAILABLE *                          
         DC    AL1(13,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMISCR,0)                                           
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00' MASK                                             
         DCDDL AC#HRSAV,13,R       DCDD FOR HEADING 1                           
         DCDDL AC#HRSAV,13,RU      DCDD FOR HEADING 2                           
*                                                                               
CLMMT2X  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* AUTO ALLOCATION LIST                                                *         
***********************************************************************         
         SPACE 1                                                                
CLMAALC  DC    AL1(CLMAALCQ)                                                    
         DC    AL2(CLMAALCX+1-CLMAALC)                                          
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    C' '                * JOB CODE *                                 
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(12,12,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(12,12,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#JOB,3,L          DCDD FOR HEADING 1                           
         DC    X'00'                                                            
         DCDDL AC#JOB,3,LU         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(AAL#JBN)        * JOB NAME *                                 
         DC    AL1(1,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#JOBN,8,L         DCDD FOR HEADING 1                           
         DCDDL AC#JOBN,8,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(AAL#UNA)        * NET AVAILABLE *                            
         DC    AL1(2,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMISCR,0)  INDS1/INDS2                              
         DC    AL1(13,13,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#NET,12,R         DCDD FOR HEADING 1                           
         DCDDL AC#AVAIL,12,R       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(AAL#ANT)        * ALLOCATED NET *                            
         DC    AL1(3,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMISCR,0)  INDS1/INDS2                              
         DC    AL1(13,13,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#ALCTD,12,R       DCDD FOR HEADING 1                           
         DCDDL AC#NET,12,R         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(AAL#ACO)        * ALLOCATED COMMISSION *                     
         DC    AL1(4,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMISCR,0)  INDS1/INDS2                              
         DC    AL1(13,13,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#ALCTD,12,R       DCDD FOR HEADING 1                           
         DCDDL AC#CMN,12,R         DCDD FOR HEADING 2                           
*                                                                               
*&&UK                                                                           
         DC    AL1(AAL#EXR)        * EXCHANGE RATE *                            
         DC    AL1(5,0,0)          RTN/HELP/SEC                                 
         DC    AL1(0,0)            INDS1/INDS2                                  
         DC    AL1(10,10,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#EXCHR,10,L       DCDD FOR HEADING 1                           
         DCDDL AC#EXCHR,10,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(AAL#GRP)        * GROUP *                                    
         DC    AL1(6,0,0)          RTN/HELP/SEC                                 
         DC    AL1(0,0)            INDS1/INDS2                                  
         DC    AL1(6,6,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#GROUP,6,R        DCDD FOR HEADING 1                           
         DCDDL AC#GROUP,6,RU       DCDD FOR HEADING 2                           
*&&                                                                             
*                                                                               
CLMAALCX DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* SUMMARY SCREEN                                                      *         
***********************************************************************         
         SPACE 1                                                                
CLMSUMM  DC    AL1(CLMSUMMQ)                                                    
         DC    AL2(CLMSUMMX+1-CLMSUMM)                                          
         DC    AL1(0,0,0)                                                       
*                                  ORIGINAL ESTIMATE GROSS                      
         DC    AL1(SUM#OEG)        CHARACTER CODE                               
         DC    AL1(20,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#ORGLE,13,L       DCDD FOR HEADING 1                           
         DCDDL AC#GROSS,6,L        DCDD FOR HEADING 2                           
*                                  ORIGINAL ESTIMATE NET                        
         DC    AL1(SUM#OEN)        CHARACTER CODE                               
         DC    AL1(21,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#ORGLE,13,L       DCDD FOR HEADING 1                           
         DCDDL AC#NET,6,L          DCDD FOR HEADING 2                           
*                                  CURRENT ESTIMATE GROSS                       
         DC    AL1(SUM#CEG)        CHARACTER CODE                               
         DC    AL1(22,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CEST,13,L        DCDD FOR HEADING 1                           
         DCDDL AC#GROSS,6,L        DCDD FOR HEADING 2                           
*                                  CURRENT ESTIMATE NET                         
         DC    AL1(SUM#CEN)        CHARACTER CODE                               
         DC    AL1(23,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CEST,13,L        DCDD FOR HEADING 1                           
         DCDDL AC#NET,6,L          DCDD FOR HEADING 2                           
*                                  CHARGES                                      
         DC    AL1(SUM#CHA)        CHARACTER CODE                               
         DC    AL1(24,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CHGS,7,L         DCDD FOR HEADING 1                           
         DCDDL AC#CHGS,7,LU        DCDD FOR HEADING 2                           
*                                  ORDERS                                       
         DC    AL1(SUM#ODS)        CHARACTER CODE                               
         DC    AL1(25,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#ORDS,6,L         DCDD FOR HEADING 1                           
         DCDDL AC#ORDS,6,LU        DCDD FOR HEADING 2                           
*                                  BILLED GROSS (TOTAL BILLING)                 
         DC    AL1(SUM#BIG)        CHARACTER CODE                               
         DC    AL1(26,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#TBLNG,13,L       DCDD FOR HEADING 1                           
         DCDDL AC#TBLNG,13,LU      DCDD FOR HEADING 2                           
*                                  BILLED NET (PRIOR BILLS)                     
         DC    AL1(SUM#BIN)        CHARACTER CODE                               
         DC    AL1(27,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#PRBIL,11,L       DCDD FOR HEADING 1                           
         DCDDL AC#PRBIL,11,LU      DCDD FOR HEADING 2                           
*                                  ALLOCATED COMMISSION                         
         DC    AL1(SUM#ACO)        CHARACTER CODE                               
         DC    AL1(28,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#ALCTD,9,L        DCDD FOR HEADING 1                           
         DCDDL AC#CMN,10,L         DCDD FOR HEADING 2                           
*                                  ALLOCATED NET                                
         DC    AL1(SUM#ANT)        CHARACTER CODE                               
         DC    AL1(29,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#ALCTD,9,L        DCDD FOR HEADING 1                           
         DCDDL AC#NET,6,L          DCDD FOR HEADING 2                           
*                                  UNBILLED GROSS                               
         DC    AL1(SUM#UNG)        CHARACTER CODE                               
         DC    AL1(30,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#UBLD,8,L         DCDD FOR HEADING 1                           
         DCDDL AC#GROSS,6,L        DCDD FOR HEADING 2                           
*                                  UNBILLED NET (UNBILLED)                      
         DC    AL1(SUM#UNN)        CHARACTER CODE                               
         DC    AL1(31,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#UBLD,8,L         DCDD FOR HEADING 1                           
         DCDDL AC#NET,6,L          DCDD FOR HEADING 2                           
*                                  BILLED COMMISSION                            
         DC    AL1(SUM#BIC)        CHARACTER CODE                               
         DC    AL1(32,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#BLD,6,L          DCDD FOR HEADING 1                           
         DCDDL AC#CMN,10,L         DCDD FOR HEADING 2                           
*                                  UNBILLED COMMISSION                          
         DC    AL1(SUM#UNC)        CHARACTER CODE                               
         DC    AL1(33,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#UBLD,8,L         DCDD FOR HEADING 1                           
         DCDDL AC#CMN,10,L         DCDD FOR HEADING 2                           
*                                  INCOME VARIANCE                              
         DC    AL1(SUM#IVA)        CHARACTER CODE                               
         DC    AL1(34,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#INCM,6,L         DCDD FOR HEADING 1                           
         DCDDL AC#VAR,8,L          DCDD FOR HEADING 2                           
*                                  % INCOME VARIANCE                            
         DC    AL1(SUM#IVP)        CHARACTER CODE                               
         DC    AL1(35,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#INCPC,8,L        DCDD FOR HEADING 1                           
         DCDDL AC#VAR,8,L          DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(SUM#UWO)        CHARACTER CODE                               
         DC    AL1(36,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIENQ+CLMIPRO,0)      INDS1/INDS2                          
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#UPDTD,7,L        DCDD FOR HEADING 1                           
         DCDDL AC#WRTFS,10,L       DCDD FOR HEADING 2                           
*                                                                               
CLMSUMMX DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* TRANSFER LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
CLMXFR   DC    AL1(CLMXFRQ)                                                     
         DC    AL2(CLMXFRX+1-CLMXFR)                                            
         DC    AL2(CLM#PRO+CLM#BIL+CLM#TRN),AL1(0)                              
*                                                                               
         DC    AL1(TKY#WC)         * WORK-CODE *                                
         DC    AL1(TKY@WC,0,0)     RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(2,2,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(2,2,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#WC,2,L           DCDD FOR HEADING 1                           
         DC    XL1'00'                                                          
         DCDDL AC#WC,2,LU          DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(TKY#CTA)        * CONTRA-ACCOUNT (SUPPLIER) *                
         DC    AL1(TKY@CTA,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
*&&UK*&& DCDDL AC#SUP,14,L         DCDD FOR HEADING 1                           
*&&UK*&& DCDDL AC#SUP,14,LU        DCDD FOR HEADING 2                           
*&&US*&& DCDDL AC#CTR,14,L         DCDD FOR HEADING 1                           
*&&US*&& DCDDL AC#CTR,14,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(TKY#REF)        * REFERENCE *                                
         DC    AL1(TKY@REF,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(6,6,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(6,6,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#REF,6,L          DCDD FOR HEADING 1                           
         DCDDL AC#REF,6,LU         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(BIL#CST)        * COMMISSIONABLE STATUS *                    
         DC    AL1(BIL@CST,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(1,1,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(1,1,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CMNBL,2,L        DCDD FOR HEADING 1                           
         DC    XL1'00'                                                          
         DCDDL AC#CMNBL,2,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#NET)        * NET AVAILABLE *                            
         DC    AL1(1,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIAMT+CLMIHEAD+CLMITOT+CLMIPRO,CLMIPROR)           
         DC    AL1(11,11,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(11,11,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PM$ANVBL,PM$FNVBL)                                    
         DC    XL6'00'                                                          
         DCDDL AC#NET,11,L         DCDD FOR HEADING 1                           
         DCDDL AC#AVAIL,11,L       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#XAMT)       * TRANSFER AMOUNT *                          
         DC    AL1(2,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIAMT+CLMIHEAD+CLMITOT,CLMIEOL+CLMIPROR)           
         DC    AL1(11,11,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(11,11,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PP$AXFER,PP$FXFER)                                    
         DC    XL6'00'                                                          
         DCDDL AC#XFR,11,L         DCDD FOR HEADING 1                           
         DCDDL AC#AMT,11,L         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(XFR#TFT)        * TRANSFER TO WORKCODE/JOB *                 
         DC    AL1(3,0,0)          RTN/HELP/SEC                                 
         DC    AL1(0,0)            INDS1/INDS2                                  
         DC    AL1(15,15,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(15,15,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#XFERT,15,L       DCDD FOR HEADING 1                           
         DCDDL AC#XFERT,15,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(XFR#OVR)        * OVERRIDE COMMN/SKACCOUNT *                 
         DC    AL1(4,0,0)          RTN/HELP/SEC                                 
         DC    AL1(0,0)            INDS1/INDS2                                  
         DC    AL1(15,15,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(15,15,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#XFERD,15,L       DCDD FOR HEADING 1                           
         DCDDL AC#XFERD,15,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#HRS)        * HOURS AVAILABLE *                          
         DC    AL1(5,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIAMT+CLMITOT+CLMIPRO,CLMIPROR)                            
         DC    AL1(7,7,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(7,7,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PM$HRVBL,PM$HRVBL)                                    
         DC    XL6'00'                                                          
         DCDDL AC#HOURS,7,L        DCDD FOR HEADING 1                           
         DCDDL AC#HOURS,7,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#COM)        * COMMISSION AVAILABLE *                     
         DC    AL1(6,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIAMT+CLMITOT+CLMIPRO,CLMIPROR)                            
         DC    AL1(11,11,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(11,11,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PM$ACVBL,PM$FCVBL)                                    
         DC    XL6'00'                                                          
         DCDDL AC#CMN,11,L         DCDD FOR HEADING 1                           
         DCDDL AC#AVAIL,11,L       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(XFR#CMNX)       * COMMISSION TRANSFERRED *                   
         DC    AL1(07,0,0)         RTN/HELP/SEC                                 
         DC    AL1(CLMIAMT+CLMITOT+CLMIPRO,0)  INDS1/INDS2                      
         DC    AL1(11,11,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(11,11,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CMN,11,L         DCDD FOR HEADING 1                           
         DCDDL AC#XFRD,11,L        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(TRN#CTN)        * CONTRA ACCOUNT NAME *                      
         DC    AL1(TRN@CTN,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CTRAN,20,L       DCDD FOR HEADING 1                           
         DCDDL AC#CTRAN,20,LU      DCDD FOR HEADING 2                           
*                                                                               
CLMXFRX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* WRITE-OFF RECOVERY LIST                                             *         
***********************************************************************         
         SPACE 1                                                                
CLMRCVR  DC    AL1(CLMRCVRQ)                                                    
         DC    AL2(CLMRCVRX+1-CLMRCVR)                                          
         DC    AL2(CLM#TKY+CLM#PRO+CLM#BIL+CLM#TRN),AL1(0)                      
*                                                                               
         DC    AL1(PRO#HRS)        * HOURS PENDING RECOVERY *                   
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIAMT,CLMIPROR)                                    
         DC    AL1(8,8,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PP$HRSR,0)                                            
         DC    XL6'00'                                                          
         DCDDL AC#PENDG,8,L        DCDD FOR HEADING 1                           
         DCDDL AC#HOURS,8,L        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(RCV#NWO)        * NUMBER OF WRITE OFFS *                     
         DC    AL1(1,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(3,3,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(3,3,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#NUM,3,L          DCDD FOR HEADING 1                           
         DC    X'00'                                                            
         DCDDL AC#WRTF,3,L         DCDD FOR HEADING 2                           
         DC    X'00'                                                            
*                                                                               
CLMRCVRX DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* REVERSE LIST                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLMREV   DC    AL1(CLMREVQ)                                                     
         DC    AL2(CLMREVX+1-CLMREV)                                            
         DC    AL2(CLM#TKY+CLM#PRO+CLM#BIL+CLM#TRN),AL1(0)                      
*                                                                               
         DC    AL1(PRO#NET)        * NET AVAILABLE *                            
         DC    AL1(1,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIAMT+CLMIHEAD,CLMIEOL+CLMIPROR)                   
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PM$ANVBL,PM$FNVBL)                                    
         DC    XL6'00'                                                          
*&&UK*&& DCDDL AC#NETAV,14,L       DCDD FOR HEADING 1                           
*&&UK*&& DCDDL AC#NETAV,14,LU      DCDD FOR HEADING 2                           
*&&US*&& DCDDL AC#NET,14,L         DCDD FOR HEADING 1                           
*&&US*&& DCDDL AC#AVAIL,14,L       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#ALC)        * ALLOCATED AMOUNT *                         
         DC    AL1(2,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIAMT+CLMIHEAD,CLMIPROR)                           
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PP$AALLO,PP$FALLO)                                    
         DC    XL6'00'                                                          
         DCDDL AC#ALCTD,14,L       DCDD FOR HEADING 1                           
         DCDDL AC#ALCTD,14,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#COM)        * COMMISSION *                               
         DC    AL1(3,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIAMT+CLMIHEAD,CLMIPROR)                           
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PM$ACVBL,PM$FCVBL)                                    
         DC    XL6'00'                                                          
         DCDDL AC#CMN,14,L         DCDD FOR HEADING 1                           
         DCDDL AC#CMN,14,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#HRS)        * HOURS *                                    
         DC    AL1(4,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIAMT,CLMIPROR)                                    
         DC    AL1(7,7,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(7,7,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PM$HRVBL,0)                                           
         DC    XL6'00'                                                          
         DCDDL AC#HOURS,7,L        DCDD FOR HEADING 1                           
         DCDDL AC#HOURS,7,LU       DCDD FOR HEADING 2                           
*                                                                               
CLMREVX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* FEE ADJUSTMENT LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
CLMFLI   DC    AL1(CLMFLIQ)                                                     
         DC    AL2(CLMFLIX+1-CLMFLI)                                            
         DC    AL2(CLM#TKY+CLM#BIL+CLM#TRN),AL1(0)                              
*                                                                               
         DC    AL1(PRO#NET)        * NET AMOUNT *                               
         DC    AL1(1,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIAMT+CLMIHEAD,CLMIPROR)                                   
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PA$NET,PA$NET)                                        
         DC    XL6'00'                                                          
         DCDDL AC#NET,14,L       DCDD FOR HEADING 1                             
         DCDDL AC#NET,14,LU      DCDD FOR HEADING 2                             
*                                                                               
         DC    AL1(BIL#CST)        * COMMISSIONABLE STATUS *                    
         DC    AL1(2,0,0)          RTN/HELP/SEC                                 
         DC    AL1(0,0)            INDS1/INDS2                                  
         DC    AL1(5,5,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,1)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(5,5,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CMN,5,L          DCDD FOR HEADING 1                           
         DCDDL AC#CMN,5,LU         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#ALC)        * ALLOCATED STATUS *                         
         DC    AL1(3,0,0)          RTN/HELP/SEC                                 
         DC    AL1(0,0)            INDS1/INDS2                                  
         DC    AL1(5,5,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,1)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(5,5,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#ALCTD,5,L        DCDD FOR HEADING 1                           
         DCDDL AC#ALCTD,5,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(BIL#OFF)        * OFFICE CODE *                              
         DC    AL1(4,0,0)          RTN/HELP/SEC                                 
         DC    AL1(0,CLMINONE)     INDS1/INDS2                                  
         DC    AL1(6,2,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(6,2,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#OFF,6,L          DCDD FOR HEADING 1                           
         DCDDL AC#OFF,6,LU         DCDD FOR HEADING 2                           
*                                                                               
CLMFLIX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* COLUMNS LIST                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLMCOL   DC    AL1(CLMCOLQ)                                                     
         DC    AL2(CLMCOLX+1-CLMCOL)                                            
         DC    AL2(0),AL1(0)                                                    
*                                                                               
         DC    C' '                * COLUMN CODE *                              
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0)                                           
         DC    AL1(4,1,1)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,0)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(0,0)                                                  
         DC    XL6'00'                                                          
         DCDDL AC#CODE,4,L       DCDD FOR HEADING 1                             
         DCDDL AC#CODE,4,LU      DCDD FOR HEADING 2                             
*                                                                               
         DC    C'1'                * HEADING 1 *                                
         DC    AL1(1,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)                                                   
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,1)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(20,20,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL GE#CH1,20,L         DCDD FOR HEADING 1                           
         DCDDL GE#CH1,20,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    C'2'                * HEADING 2 *                                
         DC    AL1(2,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)                                                   
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,1)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(40,40,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL GE#CH2,20,L         DCDD FOR HEADING 1                           
         DCDDL GE#CH2,20,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    C'3'                * FIXED *                                    
         DC    AL1(3,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)                                                   
         DC    AL1(5,5,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,1)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(5,5,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL GE#FIXED,5,L        DCDD FOR HEADING 1                           
         DCDDL GE#FIXED,5,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    C'4'                * OPEN *                                     
         DC    AL1(4,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)                                                   
         DC    AL1(4,4,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,1)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(4,4,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#OPEN,4,L         DCDD FOR HEADING 1                           
         DCDDL AC#OPEN,4,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    C'5'                * COUNTRY *                                  
         DC    AL1(5,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)                                                   
         DC    AL1(8,8,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,1)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CTRY,8,L         DCDD FOR HEADING 1                           
         DCDDL AC#CTRY,8,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    C'6'                * SECURITY *                                 
         DC    AL1(6,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)                                                   
         DC    AL1(8,8,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,0,0,1)        HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#SEC,8,L          DCDD FOR HEADING 1                           
         DCDDL AC#SEC,8,LU         DCDD FOR HEADING 2                           
*                                                                               
CLMCOLX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
CLMTABX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* COLUMNS AVAILABLE TO ALL LISTS                                      *         
***********************************************************************         
         SPACE 1                                                                
CLMGEN   DS    0X                                                               
         SPACE 1                                                                
***********************************************************************         
* TRANSACTION LIST KEY COLUMNS                                        *         
***********************************************************************         
         SPACE 1                                                                
CLMTKY   DC    AL1(CLMGENQ)        ** GENERAL TRANSACTION COLUMNS **            
         DC    AL2(CLMTKYX+1-CLMTKY)                                            
         DC    AL2(CLM#TKY),AL1(0)                                              
*                                                                               
         DC    AL1(TKY#WC)         * WORKCODE *                                 
         DC    AL1(TKY@WC,0,0)     RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(2,2,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(2,2,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#WC,2,L           DCDD FOR HEADING 1                           
         DC    XL1'00'                                                          
         DCDDL AC#WC,2,LU          DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(TKY#CTA)        * CONTR-ACCOUNT (SUPPLIER) *                 
         DC    AL1(TKY@CTA,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
*&&UK*&& DCDDL AC#SUP,14,L         DCDD FOR HEADING 1                           
*&&UK*&& DCDDL AC#SUP,14,LU        DCDD FOR HEADING 2                           
*&&US*&& DCDDL AC#CTR,14,L         DCDD FOR HEADING 1                           
*&&US*&& DCDDL AC#CTR,14,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(TKY#REF)        * REFERENCE *                                
         DC    AL1(TKY@REF,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIKEY+CLMIPRO,0) INDS1/INDS2                               
         DC    AL1(6,6,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(6,6,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#REF,6,L          DCDD FOR HEADING 1                           
         DCDDL AC#REF,6,LU         DCDD FOR HEADING 2                           
*                                                                               
CLMTKYX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* PRO-RATA AMOUNT COLUMNS                                             *         
***********************************************************************         
         SPACE 1                                                                
CLMPRO   DC    AL1(CLMGENQ)                                                     
         DC    AL2(CLMPROX+1-CLMPRO)                                            
         DC    AL2(CLM#PRO),AL1(0)                                              
*                                                                               
         DC    AL1(PRO#NET)        * NET AVAILABLE *                            
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIAMT+CLMIHEAD,CLMIEOL+CLMIPROR)                   
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PM$ANVBL,PM$FNVBL)                                    
         DC    XL6'00'                                                          
*&&UK*&& DCDDL AC#NETAV,14,L       DCDD FOR HEADING 1                           
*&&UK*&& DCDDL AC#NETAV,14,LU      DCDD FOR HEADING 2                           
*&&US*&& DCDDL AC#NET,14,L         DCDD FOR HEADING 1                           
*&&US*&& DCDDL AC#AVAIL,14,L       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#WTF)        * WRITTEN-OFF *                              
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIAMT+CLMIHEAD,CLMIPROR)                           
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PP$AWOFF,PP$FWOFF)                                    
         DC    XL6'00'                                                          
         DCDDL AC#WRTAM,14,L       DCDD FOR HEADING 1                           
         DCDDL AC#WRTAM,14,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#ALC)        * ALLOCATED AMOUNT *                         
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIAMT+CLMIHEAD,CLMIPROR)                           
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PP$AALLO,PP$FALLO)                                    
         DC    XL6'00'                                                          
         DCDDL AC#ALCTD,14,L       DCDD FOR HEADING 1                           
         DCDDL AC#ALCTD,14,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#COM)        * COMMISSION *                               
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIAMT+CLMIPRO+CLMIHEAD,CLMIPROR)                           
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PM$ACVBL,PM$FCVBL)                                    
         DC    XL6'00'                                                          
         DCDDL AC#CMN,14,L         DCDD FOR HEADING 1                           
         DCDDL AC#CMN,14,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#HRS)        * HOURS *                                    
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIAMT,CLMIPROR)                                    
         DC    AL1(7,7,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(7,7,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PM$HRVBL,0)                                           
         DC    XL6'00'                                                          
         DCDDL AC#HOURS,7,L        DCDD FOR HEADING 1                           
         DCDDL AC#HOURS,7,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#BLD)        * BILLED AMOUNT *                            
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIAMT+CLMIPRO+CLMIHEAD,CLMIPROR)                           
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PA$NETBL,PB$NETBL)                                    
         DC    XL6'00'                                                          
         DCDDL AC#BLDN,14,L        DCDD FOR HEADING 1                           
         DCDDL AC#BLDN,14,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#DSC)        * CASH DISCOUNT *                            
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,CLMIPROR)                                            
         DC    AL1(9,9,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(9,9,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PA$DSC,PA$DSC)                                        
         DC    XL6'00'                                                          
         DCDDL AC#CASH,9,C         DCDD FOR HEADING 1                           
         DCDDL AC#DISS,9,C         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#ACM)        * ALLOCATED COMMISSION *                     
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIAMT+CLMIPRO+CLMIHEAD,CLMIPROR)                           
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PP$ACOMM,PP$FCOMM)                                    
         DC    XL6'00'                                                          
         DCDDL AC#ALCOM,14,L       DCDD FOR HEADING 1                           
         DCDDL AC#ALCOM,14,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#AHR)        * ALLOCATED HOURS *                          
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIAMT+CLMIPRO,CLMIPROR)                                    
         DC    AL1(7,7,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(7,7,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PP$HRSB,0)                                            
         DC    XL6'00'                                                          
         DCDDL AC#ALCTD,7,L        DCDD FOR HEADING 1                           
         DCDDL AC#HOURS,7,L        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#BCM)        * COMMISSION BILLED *                        
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIAMT+CLMIPRO+CLMIHEAD,CLMIPROR)                           
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PA$COMBL,PB$COMBL)                                    
         DC    XL6'00'                                                          
         DCDDL AC#BLCOM,14,L       DCDD FOR HEADING 1                           
         DCDDL AC#BLCOM,14,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#BHR)        * HOURS BILLED *                             
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIAMT+CLMIPRO,CLMIPROR)                                    
         DC    AL1(7,7,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(7,7,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PA$HRSB,0)                                            
         DC    XL6'00'                                                          
         DCDDL AC#HOURS,7,L        DCDD FOR HEADING 1                           
         DCDDL AC#BLD,7,L          DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#PRCV)       * PENDING RECOVERY AMOUNT *                  
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIAMT+CLMIHEAD+CLMITOT,CLMIPROR)                   
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PP$AWOFR,PP$FWOFR)                                    
         DC    XL6'00'                                                          
         DCDDL AC#PENDG,14,L       DCDD FOR HEADING 1                           
*&&US*&& DCDDL AC#RCVR,14,L        DCDD FOR HEADING 2                           
*&&UK*&& DCDDL AC#WRTB,14,L        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(PRO#UWO)        * UPDATED WRITE-OFFS *                       
         DC    AL1(0,0,0)          RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIAMT+CLMIHEAD+CLMITOT,CLMIPROR)                   
         DC    AL1(14,14,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(14,14,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),SL2(PA$WOFAM,PA$WOFAM)  ?? PB$WOFAM ??                    
         DC    XL6'00'                                                          
         DCDDL AC#UPDWO,14,L       DCDD FOR HEADING 1                           
         DCDDL AC#UPDWO,14,LU      DCDD FOR HEADING 2                           
*                                                                               
CLMPROX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* GENERAL TRANSACTION BILLING COLUMNS                                 *         
***********************************************************************         
         SPACE 1                                                                
CLMBIL   DC    AL1(CLMGENQ)                                                     
         DC    AL2(CLMBILX+1-CLMBIL)                                            
         DC    AL2(CLM#BIL),AL1(0)                                              
*                                                                               
         DC    AL1(BIL#STA)        * STATUS *                                   
         DC    AL1(BIL@STA,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(4,4,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(4,4,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#STT,4,L          DCDD FOR HEADING 1                           
         DCDDL AC#STT,4,LU         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(BIL#CMR)        * COMMISSION RATE *                          
         DC    AL1(BIL@CMR,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)                                                   
         DC    AL1(7,7,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(7,7,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CMN,7,C          DCDD FOR HEADING 1                           
         DCDDL AC#RATE,7,C         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(BIL#WOA)        * WRITE-OFF ACCOUNT *                        
         DC    AL1(BIL@WOA,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)                                                   
         DC    AL1(15,15,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(15,15,0)        NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#WRTFA,15,L       DCDD FOR HEADING 1                           
         DCDDL AC#WRTFA,15,LU      DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(BIL#OFF)        * OFFICE CODE *                              
         DC    AL1(BIL@OFF,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(6,2,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(6,2,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#OFF,6,L          DCDD FOR HEADING 1                           
         DCDDL AC#OFF,6,LU         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(BIL#PAR)        * PARAGRAPH CODE *                           
         DC    AL1(BIL@PAR,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(4,3,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,0)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(4,3,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#PRGRP,4,R        DCDD FOR HEADING 1                           
         DCDDL AC#PRGRP,4,RU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(BIL#CST)        * COMMISSIONABLE STATUS *                    
         DC    AL1(BIL@CST,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(1,1,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(1,1,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CMNBL,2,L        DCDD FOR HEADING 1                           
         DC    XL1'00'                                                          
         DCDDL AC#CMNBL,2,LU       DCDD FOR HEADING 2                           
*                                                                               
CLMBILX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* GENERAL TRANSACTION COLUMNS                                         *         
***********************************************************************         
         SPACE 1                                                                
CLMTRN   DC    AL1(CLMGENQ)        ** GENERAL TRANSACTION COLUMNS **            
         DC    AL2(CLMTRNX+1-CLMTRN)                                            
         DC    AL2(CLM#TRN),AL1(0)                                              
*                                                                               
         DC    AL1(TRN#DAT)        * TRANSACTION DATE *                         
         DC    AL1(TRN@DAT,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(8,8,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#DATE,8,L         DCDD FOR HEADING 1                           
         DCDDL AC#DATE,8,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(TRN#MOA)        * MONTH OF ACTIVITY *                        
         DC    AL1(TRN@MOA,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(5,5,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(5,5,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#MOA,5,L          DCDD FOR HEADING 1                           
         DCDDL AC#MOA,5,LU         DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(TRN#BRF)        * BATCH REFERENCE *                          
         DC    AL1(TRN@BRF,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(4,4,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(4,4,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#BATRF,4,L        DCDD FOR HEADING 1                           
         DCDDL AC#BATRF,4,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(TRN#INP)        * INPUT TYPE *                               
         DC    AL1(TRN@INP,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(3,3,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(3,3,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#INPTY,3,L        DCDD FOR HEADING 1                           
         DC    X'00'                                                            
         DCDDL AC#INPTY,3,LU       DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(TRN#ORD)        * ORDER NUMBER *                             
         DC    AL1(TRN@ORD,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(6,6,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(6,6,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#ORDC,6,L         DCDD FOR HEADING 1                           
         DCDDL AC#ORDC,6,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(TRN#SBR)        * SUBREF *                                   
         DC    AL1(TRN@SBR,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(9,9,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(9,9,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#SUBR,9,L         DCDD FOR HEADING 1                           
         DCDDL AC#SUBR,9,LU        DCDD FOR HEADING 2                           
*                                                                               
         DC    AL1(TRN#CTN)        * CONTRA ACCOUNT NAME *                      
         DC    AL1(TRN@CTN,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO,0)      INDS1/INDS2                                  
         DC    AL1(36,36,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(0,0,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DCDDL AC#CTRAN,36,L       DCDD FOR HEADING 1                           
         DCDDL AC#CTRAN,36,LU      DCDD FOR HEADING 2                           
*                                                                               
CLMTRNX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* GENERAL TRANSACTION BILLING COLUMNS (DDS ONLY)                      *         
***********************************************************************         
         SPACE 1                                                                
CLMBIL2  DC    AL1(CLMGENQ)                                                     
         DC    AL2(CLMBIL2X+1-CLMBIL2)                                          
         DC    AL2(CLM#BIL),AL1(0)                                              
*                                                                               
         DC    AL1(BIL#PTA)        * PTA RECORD DISK ADDRESS *                  
         DC    AL1(BIL@PTA,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIDDS+CLMIHEAD,0)                                  
         DC    AL1(8,8,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    AL1(BIL#DTA)        * TSAR RECORD DATA *                         
         DC    AL1(BIL@DTA,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIDDS+CLMIHEAD,0)                                  
         DC    AL1(6,6,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(6,6,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
*                                                                               
CLMBIL2X DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* COLUMNS AVAILABLE TO ALL LISTS                                      *         
***********************************************************************         
         SPACE 1                                                                
CLMALL   DC    AL1(CLMGENQ)        ** ALL LISTS  **                             
         DC    AL2(CLMALLX+1-CLMALL)                                            
         DC    AL2(0),AL1(0)                                                    
*                                                                               
         DC    AL1(ALL#DA)         * DISK ADDRESS *                             
         DC    AL1(ALL@DA,0,0)     RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIDDS+CLMITOT+CLMIHEAD+CLMISCR,0)                  
         DC    AL1(8,8,0)          HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(8,8,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    AL1(ALL#TSR)        * TSAR RECORD INFO *                         
         DC    AL1(ALL@TSR,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIDDS+CLMITOT+CLMIHEAD+CLMISCR,0)                  
         DC    AL1(20,20,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(4,4,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    AL1(ALL#KEY)        * TSAR RECORD KEY *                          
         DC    AL1(ALL@KEY,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIDDS+CLMITOT+CLMIHEAD+CLMISCR,0)                  
         DC    AL1(36,36,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(4,4,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    AL1(ALL#DTA)        * TSAR RECORD DATA *                         
         DC    AL1(ALL@DTA,0,0)    RTN/HELP/SEC                                 
         DC    AL1(CLMIPRO+CLMIDDS+CLMITOT+CLMIHEAD+CLMISCR,0)                  
         DC    AL1(34,34,0)        HEAD WIDTH/FIELD WIDTH/DISP(FIELD)           
         DC    AL1(0,FHATLC,0,1)   HEAD FHXT/FIELD FHAT/FIELD FHXT/MINL         
         DC    AL1(4,4,0)          NARROW H/WIDTH F/WIDTH F/DISP                
         DC    AL2(0),XL10'00'     MASK                                         
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
*                                                                               
CLMALLX  DC    AL1(EOT)                                                         
*                                                                               
CLMGENX  DC    AL1(EOT)                                                         
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PARAGRAPH FORMAT TABLE                                              *         
***********************************************************************         
         SPACE 1                                                                
FMTTAB   DS    0X                                                               
*                                                                               
         DC    AL1(BLFFWCQ,0,0,FMTIDAT,0),AL2(AC#WC)                            
         DC    AL1(1,1,1,1),AL1(1,2,2,2),AL1(0,0,0,0)                           
*                                                                               
         DC    AL1(BLFFREFQ,0,0,FMTIDAT,0),AL2(AC#REFN)                         
         DC    AL1(1,1,1,1),AL1(1,6,6,6),AL1(0,0,0,0)                           
*                                                                               
         DC    AL1(BLFFDATQ,0,0,FMTIDAT,0),AL2(AC#DATE)                         
         DC    AL1(1,1,1,1),AL1(1,8,7,20),AL1(0,0,0,0)                          
*                                                                               
         DC    AL1(BLFFSUPQ,0,0,FMTIDAT,0),AL2(AC#SUPN)                         
         DC    AL1(1,1,1,9),AL1(1,36,1,36),AL1(0,0,0,0)                         
*                                                                               
         DC    AL1(BLFFNARQ,0,0,FMTIDAT,0),AL2(AC#NRTV)                         
         DC    AL1(1,1,1,9),AL1(1,36,1,132),AL1(0,0,0,0)                        
*                                                                               
         DC    AL1(BLFFNETQ,0,0,FMTINUM+FMTITFP,FMTNCURQ),AL2(AC#NET)           
         DC    AL1(1,1,1,1),AL1(1,14,6,16),AL1(0,0,0,0)                         
*                                                                               
         DC    AL1(BLFFCOMQ,0,0,FMTINUM+FMTITFP,FMTNCURQ),AL2(AC#CMN)           
         DC    AL1(1,1,1,1),AL1(1,14,6,16),AL1(0,0,0,0)                         
*                                                                               
         DC    AL1(BLFFGRSQ,0,0,FMTINUM+FMTITFP,FMTNCURQ),AL2(AC#GROSS)         
         DC    AL1(1,1,1,1),AL1(1,14,6,16),AL1(0,0,0,0)                         
*                                                                               
         DC    AL1(BLFFVATQ,0,0,FMTINUM,FMTNCURQ),AL2(AC#VAT)                   
         DC    AL1(1,1,1,1),AL1(1,14,6,16),AL1(0,0,0,0)                         
*                                                                               
         DC    AL1(BLFFHRSQ,0,0,FMTINUM,2),AL2(AC#HOURS)                        
         DC    AL1(1,1,1,1),AL1(1,10,4,16),AL1(0,0,0,0)                         
*                                                                               
         DC    AL1(BLFFRATQ,0,0,FMTINUM,2),AL2(AC#RATE)                         
         DC    AL1(1,1,1,1),AL1(1,10,4,16),AL1(0,0,0,0)                         
*                                                                               
         DC    AL1(BLFFWCNQ,0,0,FMTIDAT,0),AL2(AC#WCNAM)                        
         DC    AL1(1,1,1,9),AL1(1,36,1,36),AL1(0,0,0,0)                         
*                                                                               
FMTTABN  EQU   (*-FMTTAB)/FMTTABL                                               
FMTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* FORMAT OPTIONS TABLE                                                *         
***********************************************************************         
         SPACE 1                                                                
FOPTAB   DS    0X                                                               
*                                                                               
         ORG   FOPTAB+(01*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP01)                                                    
         DC    AL1(FOPTNUM)                                                     
         DC    AL1(BOFPARSP-BOFELD,3,0,0,0)                                     
         DC    AL1(0,9)                                                         
*                                                                               
         ORG   FOPTAB+(02*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP02)                                                    
         DC    AL1(FOPTNUM)                                                     
         DC    AL1(BOFMAXLN-BOFELD,60,0,0,0)                                    
         DC    AL1(1,120)                                                       
*                                                                               
         ORG   FOPTAB+(03*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP03)                                                    
         DC    AL1(FOPTNUM)                                                     
         DC    AL1(BOFMAXWD-BOFELD,132,0,0,0)                                   
         DC    AL1(1,165)                                                       
*                                                                               
         ORG   FOPTAB+(04*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP04)                                                    
         DC    AL1(FOPTKWRD)                                                    
         DC    AL1(BOFINDS1-BOFELD,BOFIHPAG,0,0,0)                              
         DC    AL2(UC@PAGE-TWAD),AL1(L'UC@PAGE,FOPKON,BOFIHPAG)                 
         DC    AL2(UC@PRGRP-TWAD),AL1(L'UC@PRGRP,FOPKON,BOFIHPAR)               
         DC    AL2(UC@NONE-TWAD)                                                
         DC    AL1(L'UC@NONE,FOPKOFF,BOFIHPAG+BOFIHPAR)                         
*                                                                               
         ORG   FOPTAB+(05*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP05)                                                    
         DC    AL1(FOPTNUM)                                                     
         DC    AL1(BOFPAGSP-BOFELD,0,0,0,0)                                     
         DC    AL1(0,15)                                                        
*                                                                               
         ORG   FOPTAB+(06*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP06)                                                    
         DC    AL1(FOPTKWRD)                                                    
         DC    AL1(BOFWCORD-BOFELD,BOFWALPH,0,0,0)                              
         DC    AL2(UC@TIME-TWAD),AL1(L'UC@TIME,FOPKEQU,BOFWTIME)                
         DC    AL2(UC@COST-TWAD),AL1(L'UC@COST,FOPKEQU,BOFWCOST)                
         DC    AL2(UC@ALPHA-TWAD),AL1(L'UC@ALPHA,FOPKEQU,BOFWALPH)              
*                                                                               
         ORG   FOPTAB+(07*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP07)                                                    
         DC    AL1(FOPTKWRD)                                                    
         DC    AL1(BOFSUBT-BOFELD,BOFSBOTH,0,0,0)                               
         DC    AL2(UC@BOTH-TWAD),AL1(L'UC@BOTH,FOPKEQU,BOFSBOTH)                
         DC    AL2(UC@NO-TWAD),AL1(L'UC@NO,FOPKEQU,BOFSNO)                      
         DC    AL2(UC@TIME-TWAD),AL1(L'UC@TIME,FOPKEQU,BOFSTIME)                
         DC    AL2(UC@COST-TWAD),AL1(L'UC@COST,FOPKEQU,BOFSCOST)                
*                                                                               
         ORG   FOPTAB+(08*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP08)                                                    
         DC    AL1(FOPTKWRD)                                                    
         DC    AL1(BOFINDS1-BOFELD,0,0,0,0)                                     
         DC    AL2(UC@YES-TWAD),AL1(L'UC@YES,FOPKON,BOFIAPRE)                   
         DC    AL2(UC@NO-TWAD),AL1(L'UC@NO,FOPKOFF,BOFIAPRE)                    
*                                                                               
         ORG   FOPTAB+(09*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP09)                                                    
         DC    AL1(FOPTKWRD)                                                    
         DC    AL1(BOFINDS1-BOFELD,BOFIPVSA,0,0,0)                              
         DC    AL2(UC@YES-TWAD),AL1(L'UC@YES,FOPKON,BOFIPVSA)                   
         DC    AL2(UC@FRN-TWAD),AL1(L'UC@FRN,FOPKON,BOFIPVSF)                   
         DC    AL2(UC@NO-TWAD),AL1(L'UC@NO,FOPKOFF,BOFIPVSA+BOFIPVSF)           
*                                                                               
         ORG   FOPTAB+(10*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP10)                                                    
         DC    AL1(FOPTKWRD)                                                    
         DC    AL1(BOFINDS1-BOFELD,0,0,0,0)                                     
         DC    AL2(UC@YES-TWAD),AL1(L'UC@YES,FOPKON,BOFISVAT)                   
         DC    AL2(UC@NO-TWAD),AL1(L'UC@NO,FOPKOFF,BOFISVAT)                    
*                                                                               
         ORG   FOPTAB+(11*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP11)                                                    
         DC    AL1(FOPTKWRD)                                                    
         DC    AL1(BOFINDS1-BOFELD,0,0,0,0)                                     
         DC    AL2(UC@YES-TWAD),AL1(L'UC@YES,FOPKON,BOFIPPAY)                   
         DC    AL2(UC@NO-TWAD),AL1(L'UC@NO,FOPKOFF,BOFIPPAY)                    
*                                                                               
         ORG   FOPTAB+(12*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP12)                                                    
         DC    AL1(FOPTKWRD)                                                    
         DC    AL1(BOFINDS2-BOFELD,0,0,0,0)                                     
         DC    AL2(UC@YES-TWAD),AL1(L'UC@YES,FOPKON,BOFIUPCA)                   
         DC    AL2(UC@NO-TWAD),AL1(L'UC@NO,FOPKOFF,BOFIUPCA)                    
*                                                                               
         ORG   FOPTAB+(13*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP13)                                                    
         DC    AL1(FOPTFREE)                                                    
         DC    AL1(0,0,0,0,0)                                                   
         DC    AL1(FFTTPNET)                                                    
         DC    AL2(HD@NET-TWAD,FBPNET-FBLKD)                                    
*                                                                               
         ORG   FOPTAB+(14*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP14)                                                    
         DC    AL1(FOPTFREE)                                                    
         DC    AL1(0,0,0,0,0)                                                   
         DC    AL1(FFTTPCMN)                                                    
         DC    AL2(HD@CMN-TWAD,FBPCMN-FBLKD)                                    
*                                                                               
         ORG   FOPTAB+(15*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP15)                                                    
         DC    AL1(FOPTFREE)                                                    
         DC    AL1(0,0,0,0,0)                                                   
         DC    AL1(FFTTPGRS)                                                    
         DC    AL2(HD@GROSS-TWAD,FBPGRS-FBLKD)                                  
*                                                                               
         ORG   FOPTAB+(16*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP16)                                                    
         DC    AL1(FOPTFREE)                                                    
         DC    AL1(0,0,0,0,0)                                                   
         DC    AL1(FFTTPSUR)                                                    
         DC    AL2(HD@SRCRG-TWAD,FBPSUR-FBLKD)                                  
*                                                                               
         ORG   FOPTAB+(17*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP17)                                                    
         DC    AL1(FOPTFREE)                                                    
         DC    AL1(0,0,0,0,0)                                                   
         DC    AL1(FFTTPDIS)                                                    
         DC    AL2(HD@DISS-TWAD,FBPDIS-FBLKD)                                   
*                                                                               
         ORG   FOPTAB+(18*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP18)                                                    
         DC    AL1(FOPTKWRD)                                                    
         DC    AL1(BOFINDS2-BOFELD,0,0,0,0)                                     
         DC    AL2(UC@YES-TWAD),AL1(L'UC@YES,FOPKON,BOFISSUR)                   
         DC    AL2(UC@NO-TWAD),AL1(L'UC@NO,FOPKOFF,BOFISSUR)                    
*                                                                               
         ORG   FOPTAB+(19*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP19)                                                    
         DC    AL1(FOPTKWRD)                                                    
         DC    AL1(BOFINDS2-BOFELD,0,0,0,0)                                     
         DC    AL2(UC@YES-TWAD),AL1(L'UC@YES,FOPKON,BOFISDIS)                   
         DC    AL2(UC@NO-TWAD),AL1(L'UC@NO,FOPKOFF,BOFISDIS)                    
*                                                                               
         ORG   FOPTAB+(20*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP20)                                                    
         DC    AL1(FOPTFREE)                                                    
         DC    AL1(0,0,0,0,0)                                                   
         DC    AL1(FFTTTHED)                                                    
         DC    AL2(HD@TIME-TWAD,FBTHED-FBLKD)                                   
*                                                                               
         ORG   FOPTAB+(21*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP21)                                                    
         DC    AL1(FOPTFREE)                                                    
         DC    AL1(0,0,0,0,0)                                                   
         DC    AL1(FFTTCHED)                                                    
         DC    AL2(HD@COST-TWAD,FBCHED-FBLKD)                                   
*                                                                               
         ORG   FOPTAB+(22*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP22)                                                    
         DC    AL1(FOPTKWRD)                                                    
         DC    AL1(BOFINDS2-BOFELD,0,0,0,0)                                     
         DC    AL2(UC@YES-TWAD),AL1(L'UC@YES,FOPKON,BOFIPASP)                   
         DC    AL2(UC@NO-TWAD),AL1(L'UC@NO,FOPKOFF,BOFIPASP)                    
*                                                                               
         ORG   FOPTAB+(23*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP23)                                                    
         DC    AL1(FOPTKWRD)                                                    
         DC    AL1(BOFSUBT2-BOFELD,BOFS2NO,0,0,0)                               
         DC    AL2(UC@BOTH-TWAD),AL1(L'UC@BOTH,FOPKEQU,BOFS2BTH)                
         DC    AL2(UC@NO-TWAD),AL1(L'UC@NO,FOPKEQU,BOFS2NO)                     
         DC    AL2(UC@EXT-TWAD),AL1(L'UC@EXT,FOPKEQU,BOFS2INT)                  
         DC    AL2(UC@INT-TWAD),AL1(L'UC@INT,FOPKEQU,BOFS2EXT)                  
*                                                                               
         ORG   FOPTAB+(24*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP24)                                                    
         DC    AL1(FOPTFREE)                                                    
         DC    AL1(0,0,0,0,0)                                                   
         DC    AL1(FFTTVATO)                                                    
         DC    AL2(HD@VAT-TWAD,FBPVAT-FBLKD)                                    
         ORG   FOPTAB+((FOPTABN+1)*FOPTABL)                                     
*                                                                               
         ORG   FOPTAB+(25*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP25)                                                    
         DC    AL1(FOPTFREE)                                                    
         DC    AL1(0,0,0,0,0)                                                   
         DC    AL1(FFTTIVTO)                                                    
         DC    AL2(HD@INVT2-TWAD,FBPINT-FBLKD)                                  
         ORG   FOPTAB+((FOPTABN+1)*FOPTABL)                                     
*                                                                               
         ORG   FOPTAB+(26*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP26)                                                    
         DC    AL1(FOPTFREE)                                                    
         DC    AL1(0,0,0,0,0)                                                   
         DC    AL1(FFTTCSTO)                                                    
         DC    AL2(HD@STCST-TWAD,FBPCST-FBLKD)                                  
         ORG   FOPTAB+((FOPTABN+1)*FOPTABL)                                     
*                                                                               
         ORG   FOPTAB+(27*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP27)                                                    
         DC    AL1(FOPTFREE)                                                    
         DC    AL1(0,0,0,0,0)                                                   
         DC    AL1(FFTTTIMO)                                                    
         DC    AL2(HD@STTIM-TWAD,FBPTIM-FBLKD)                                  
         ORG   FOPTAB+((FOPTABN+1)*FOPTABL)                                     
*                                                                               
         ORG   FOPTAB+(28*FOPTABL)                                              
         DC    AL1((*-FOPTAB)/FOPTABL)                                          
         DC    AL2(AC#FOP28)                                                    
         DC    AL1(FOPTKWRD)                                                    
         DC    AL1(BOFINDS2-BOFELD,0,0,0,0)                                     
         DC    AL2(UC@YES-TWAD),AL1(L'UC@YES,FOPKON,BOFISCIT)                   
         DC    AL2(UC@NO-TWAD),AL1(L'UC@NO,FOPKOFF,BOFISCIT)                    
*                                                                               
*                                                                               
         ORG   FOPTAB+((FOPTABN+1)*FOPTABL)                                     
FOPTABX  DS    0X                                                               
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDGETHELPD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGETHELPD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* ACCATCALLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* GEDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEDDEQUS                                                       
         PRINT ON                                                               
         EJECT                                                                  
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORK                                                      
* ACCLBCOLS                                                                     
       ++INCLUDE ACCLBCOLS                                                      
         EJECT                                                                  
ROUTABD  DSECT                     ** TABLE OF CBIL ROUINES **                  
ROUTDSP  DS    AL2                 DISPLACEMENT TO ADDRESS IN WORKD             
ROUTOLY  DS    AL1                 OVERLAY NUMBER (T621NN)                      
ROUTNUM  DS    AL1                 ROUTINE NUMBER                               
ROUTABL  EQU   *-ROUTABD                                                        
         SPACE 1                                                                
CLB00    CSECT                     ALLOW EASIER RE-LOAD OF PHASE                
         ORG   CLB00+(((*-CLB00)/512)+1)*512                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'177ACCLB00   08/08/01'                                      
         END                                                                    
