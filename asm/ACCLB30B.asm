*          DATA SET ACCLB30B   AT LEVEL 121 AS OF 12/23/99                      
*PHASE T62130B,*                                                                
CLB30    TITLE '- BILL PROGRAM - LIST SUB-CONTROLLER'                           
CLB30    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SCWORKL,**CB30**,CLEAR=YES,RR=RE                                 
         USING SCWORK,RC           RC=A(LOCAL W/S)                              
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING MIXLST,LSMIXLST                                                  
         USING PRORATAD,LSPRATA                                                 
         STCM  RF,8,SCNTRSES                                                    
         ST    RE,RELO                                                          
*                                                                               
         XR    RF,RF               SET A(APPLICATION ROUTINES)                  
         LA    R0,LSTOVRN                                                       
         LA    RE,ALSTOVR                                                       
INIT02   MVC   0(L'ALSTOVR,RE),BONTRYA                                          
         STC   RF,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,INIT02                                                        
*                                                                               
         LA    R0,AROUTN           SET A(CONTROLLER ROUTINES)                   
         LA    RF,AREROUT                                                       
         XR    RE,RE                                                            
         LA    R1,ROUT                                                          
INIT04   ST    R1,0(RF)                                                         
         STC   RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,INIT04                                                        
*                                                                               
         GOTO1 VCOLY,SCPARM,('O#GENCLM',0),0                                    
         CLI   4(R1),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,0(R1)            SET A(GENERAL COLUMN ROUTINES)               
         XR    RE,RE                                                            
         LA    RF,AGENCLM                                                       
         LA    R0,GENCLMN                                                       
INIT06   ST    R1,0(RF)                                                         
         STC   RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,INIT06                                                        
         EJECT                                                                  
***********************************************************************         
* INITIALIZE VALUES FOR CURRENT LIST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
LSTINI   GOTO1 ASETCLM,SCPARM,(MIXLCLM#,0) SET ACLMHEAD                         
         MVC   BCWORK(1),MIXLSEL#                                               
         MVI   BCWORK+1,0                                                       
         GOTO1 ASETSEL,BCWORK      SET AOVERSEL                                 
*                                                                               
         TM    CSINDSL1,CSIUENTK   TEST FIRST TIME FOR LIST                     
         BNZ   LSTINI02                                                         
         GOTO1 AINISCR             INITIALIZE LIST SCREEN                       
         OI    CSLTINDS,CSLTIFST   SET FIRST FOR LIST                           
*                                                                               
LSTINI02 MVC   SCCLM,LSCLM         SAVE LAST DIS= LIST                          
         MVC   SCOTHOPS,LSOTHOPS                                                
         XR    RF,RF                                                            
         IC    RF,SCNTRSES                                                      
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     LSTREC              NOT AN NTRSES RETRUN                         
         B     LSTR1               NORMAL RETURN TO LIST                        
         B     LSTR1               RETURN FROM INSERT                           
         B     LSTDISR             RE-DISPLAY SCREEN AFTER QUIT                 
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
LSTREC   GOTO1 ASCRFRST            FIRST FOR SCREEN                             
         BL    LSTREC02            TEST IF ERROR                                
         BE    *+8                                                              
         OI    CSLTINDS,CSLTIFST   RE-START LIST                                
         B     LSTSUB                                                           
*                                                                               
LSTREC02 TM    CSINDSL1,CSIUENTK                                                
         BO    LSTEXIT                                                          
         XR    R1,R1               BUILD SCREEN IF 1ST TIME ERROR               
         CLI   LSCLMN,0                                                         
         BE    *+8                                                              
         LA    R1,RIGHTQ                                                        
         GOTO1 ABLDSCR                                                          
         B     LSTEXIT                                                          
         SPACE 1                                                                
***********************************************************************         
* HANDLE LIST SUB-ACTIONS                                             *         
***********************************************************************         
         SPACE 1                                                                
LSTSUB   NI    CSLTINDS,FF-(CSLTIHLD+CSLTIANY)                                  
*                                                                               
         ICM   RF,15,APFKNTRY      TEST PFKEY                                   
         BZ    LSTSUB02                                                         
         USING PFKTABD,RF                                                       
         CLI   PFKNUMB,PFKRFSHQ    TEST REFRESH                                 
         BNE   *+8                                                              
         OI    CSLTINDS,CSLTIFST                                                
         TM    PFKINDS1,PFKISCRL   TEST SCROLLING                               
         BO    *+12                                                             
         OI    CSLTINDS,CSLTIHLD                                                
         B     LSTSUB02                                                         
         TM    PFKINDS2,PFKIHORZ   TEST HORIZONTAL SCROLL                       
         BZ    LSTSCR                                                           
         OI    CSLTINDS,CSLTIHLD                                                
         OI    SCINDS2,SCISAVIK                                                 
         B     LSTSCR                                                           
         DROP  RF                                                               
LSTSUB02 TM    BCINDS2,BCIHLDLP                                                 
         BZ    *+8                                                              
         OI    CSLTINDS,CSLTIHLD                                                
         CLC   LSOTHOPS,SCOTHOPS                                                
         BE    *+8                                                              
         OI    CSLTINDS,CSLTIANY                                                
*                                                                               
         TM    CSLTINDS,CSLTIFST   TEST CHANGE OF KEY FIELDS                    
         BNZ   LSTSCR                                                           
         CLC   LSCLM,SCCLM                                                      
         BE    *+12                                                             
         OI    SCINDS2,SCISAVIK                                                 
         B     LSTSCR                                                           
*                                                                               
LSTSUB04 MVC   TLNUM,CSPAG#LO                                                   
         MVI   CSSELREM,0                                                       
         OC    CSSELREM,CSLSTNUM   CSSELREM=NUMBER OF LIST ENTRIES              
         BZ    LSTSCR                                                           
         L     R2,ATWA                                                          
         AH    R2,CS1STLIN                                                      
         USING SUBINPH,R2          R2=LIST LINE                                 
*                                                                               
LSTSUB06 DS    0H                                                               
         GOTO1 ATSARIO,TSAGET                                                   
* ZZ     TM    MIXLIND1,MIXLINSA   TEST NO SUB-ACTION                           
* ZZ     BO    *+8 (DON'T KNOW WHY THIS WAS DONE,....                           
* ZZ     NI    SUBINPH+FHOID,FF-FHOIMO  BUT WAS CAUSING A BUG)                  
         TM    TLINDS1,TLITOTL+TLIPRO TEST LINE PROTECTED                       
         BNZ   LSTSUB70                                                         
         GOTO1 ASAVLST                                                          
         TM    MIXLIND1,MIXLINSA   TEST NO SUB-ACTION                           
         BO    *+8                                                              
         OI    SUBINPH+FHOID,FHOITR    ZZ +FHOIMO                               
         NI    LSINDS1,FF-LSIUPREC SET DON'T UPDATE RECORD                      
         EJECT                                                                  
***********************************************************************         
* - VALIDATE SUB-ACTION FIELD/LINE                                    *         
***********************************************************************         
         SPACE 1                                                                
         NI    SCINDS1,FF-SCISELIN-SCIVSACT-SCISWYES                            
         NI    BOINDS1,FF-BOISUBNO                                              
         TM    MIXLIND1,MIXLINSA   TEST NO SUB-ACTION                           
         BO    LSTSUB50                                                         
*                                                                               
         LA    RF,SUBINPH                                                       
         S     RF,ATWA                                                          
         STCM  RF,3,CSSELACT       SET DISPLACEMENT TO FIELD HEADER             
         XC    CSSELCUR,CSSELCUR                                                
         TM    SUBINPH+FHIID,FHIIVA                                             
         BO    *+8                                                              
         OI    CSLTINDS,CSLTIANY   KEEP SCREEN IF INPUT THIS TIME               
         GOTO1 AFVAL,SUBINPH       TEST ANY INPUT TO SUB-ACTION FIELD           
         BE    LSTSUB08                                                         
         TM    CSLTINDS,CSLTIEOL+CSLTIEOP  TEST SELECT +/&                      
         BNZ   LSTSUB12                                                         
         CLI   LSSWITCH,C'Y'       TEST SWITCH=Y                                
         BNE   LSTSUB12                                                         
         CLI   BCPFKEY,0           DON'T DO IF PFKEY                            
         BNE   LSTSUB12                                                         
         MVC   SUBINP,BC@YES                                                    
         OI    SCINDS1,SCISWYES                                                 
         OI    SUBINPH+FHOID,FHOITR                                             
         GOTO1 AFVAL,SUBINPH                                                    
         B     LSTSUB14                                                         
*                                                                               
LSTSUB08 CLI   FVIFLD,C'*'                                                      
         BE    LSTSUB12                                                         
         LH    RF,=Y(UC@NO-TWAD)                                                
         LA    RF,TWAD(RF)                                                      
         IC    RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BNE   LSTSUB10                                                         
         CLC   FVIFLD(0),0(RF)                                                  
         MVI   SUBINP,C'*'                                                      
         MVC   SUBINP+1(L'SUBINP-1),0(RF)                                       
         OI    BOINDS1,BOISUBNO    SET SUB-ACTION=NO                            
         B     LSTSUB50                                                         
*                                                                               
LSTSUB10 CLI   FVIFLD,C'-'                                                      
         BNE   LSTSUB14                                                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         XC    SCMIF,SCMIF                                                      
         B     LSTSUB50                                                         
*                                                                               
LSTSUB12 TM    CSLTINDS,CSLTIEOL+CSLTIEOP  TEST SELECT +/&                      
         BZ    LSTSUB50                                                         
         CLC   TLNUM,CSSEL#LO                                                   
         BL    LSTSUB50                                                         
         CLC   TLNUM,CSSEL#HI                                                   
         BH    LSTSUB50                                                         
         XR    R4,R4                                                            
         ICM   R4,3,CSSELMUL                                                    
         A     R4,AOVERSEL                                                      
         USING SELTABD,R4          R4=A(SELECT TABLE ENTRY)                     
         XR    RE,RE                                                            
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,TWAD(RE)                                                      
         MVC   SUBINP,0(RE)                                                     
         MVI   SUBINPH+FHILD,L'SUBINP                                           
         MVC   CSSELCUR,CSSELMUL                                                
         B     LSTSUB30                                                         
*                                                                               
LSTSUB14 OI    SCINDS1,SCISELIN    USER HAS INPUT                               
         XR    RF,RF                                                            
         ICM   RF,1,FVXLEN                                                      
         BZ    LSTSUB20                                                         
         LA    RE,FVIFLD(RF)                                                    
         CLI   0(RE),C'+'          TEST SELECT TO END OF PAGE                   
         BNE   LSTSUB16                                                         
         NI    CSLTINDS,FF-CSLTIEOL                                             
         OI    CSLTINDS,CSLTIEOP                                                
         MVC   CSSEL#HI,CSPAG#HI                                                
         B     LSTSUB18                                                         
LSTSUB16 CLI   0(RE),C'&&'         TEST SELECT TO END OF LIST                   
         BNE   LSTSUB20                                                         
         NI    CSLTINDS,FF-CSLTIEOP                                             
         OI    CSLTINDS,CSLTIEOL                                                
         MVC   CSSEL#HI,BCEFFS     SET DEFAULT HIGH VALUE                       
LSTSUB18 MVC   CSSEL#LO,TLNUM      SET LOW RECORD NUMBER                        
         BCTR  RF,0                                                             
         STC   RF,FVXLEN                                                        
*                                                                               
LSTSUB20 L     R4,AOVERSEL         R4=A(SELECT TABLE)                           
*                                                                               
LSTSUB22 CLI   SELTABD,EOT                                                      
         BNE   LSTSUB24                                                         
LSTSUB23 NI    CSLTINDS,FF-CSLTIEOL-CSLTIEOP                                    
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     LSTEXIT                                                          
*                                                                               
LSTSUB24 XR    RE,RE               COMPARE THE INPUT FIELD                      
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,TWAD(RE)                                                      
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         BNE   LSTSUB38                                                         
         CLC   FVIFLD(0),0(RE)                                                  
         CLC   TLNUM,CSSEL#LO      TEST START OF MULTI-SELECT                   
         BNE   LSTSUB30                                                         
         TM    CSLTINDS,CSLTIEOL   TEST TO END OF LIST                          
         BZ    *+12                                                             
         TM    SELTIND1,SELTIEOL   TEST END OF LIST ALLOWED                     
         BZ    LSTSUB38                                                         
         TM    CSLTINDS,CSLTIEOP   TEST TO END OF PAGE                          
         BZ    *+12                                                             
         TM    SELTIND1,SELTIEOP   TEST END OF PAGE ALLOWED                     
         BZ    LSTSUB38                                                         
         LA    RF,SELTABD                                                       
         S     RF,AOVERSEL                                                      
         STCM  RF,3,CSSELMUL       SET DISPLACEMENT TO  SELTAB ENTRY            
*                                                                               
LSTSUB30 OI    CSLTINDS,CSLTIANY   SET LINE PROCESSED THIS SCREEN               
         TM    SELTIND1,SELTINTR   TEST NTRSES                                  
         BZ    LSTSUB32                                                         
         GOTO1 ATSTMIX,SELTPARM    TEST RECORD/ACTION VALID                     
         BNE   LSTSUB36                                                         
*                                                                               
LSTSUB32 LA    RF,SELTABD                                                       
         S     RF,AOVERSEL                                                      
         STCM  RF,3,CSSELCUR       SET DISPLACEMENT TO SELTAB ENTRY             
         CLI   SELTVRTN,0          TEST FOR VALSEL ROUTINE                      
         BE    LSTSUB35                                                         
         TM    SELTIND2,SELTIGET   TEST GET RECORD                              
         BZ    LSTSUB33                                                         
         XR    R1,R1                                                            
         TM    SELTIND2,SELTIUPD   TEST GET RECORD FOR UPDATE                   
         BZ    *+8                                                              
         LA    R1,IOLOCK                                                        
         GOTO1 AGETREC                                                          
LSTSUB33 XR    R1,R1                                                            
         IC    R1,SELTVRTN                                                      
         GOTO1 OVALSEL,(R1)                                                     
         BE    LSTSUB34                                                         
*                                                                               
         TM    SCINDS1,SCISWYES                                                 
         BO    *+12                                                             
         TM    CSLTINDS,CSLTIEOP+CSLTIEOL                                       
         BZ    LSTSUB23                                                         
         XC    SUBINP,SUBINP                                                    
         MVI   SUBINP,C'*'                                                      
         B     LSTSUB70                                                         
*                                                                               
LSTSUB34 TM    SELTIND2,SELTIGET+SELTIUPD                                       
         BNO   LSTSUB35                                                         
         TM    LSINDS1,LSIUPREC    UPDATE RECORD IF NECESSERY                   
         BZ    LSTSUB35                                                         
         TM    SELTIND1,SELTIRED+SELTIEXC   IF NOT RE-DISPLAYING                
         BZ    LSTSUB35              CONTINUE TO VALIDATE LINE                  
         GOTO1 APUTREC                                                          
         BNE   LSTSUB38                                                         
*                                                                               
LSTSUB35 TM    SELTIND1,SELTIEXC   TEST EXCLUDE SUB-ACTION                      
         BZ    LSTSUB40                                                         
         GOTO1 AEXCLINE,SUBINPH                                                 
         BE    LSTSUB71                                                         
         B     LSTDIS                                                           
*                                                                               
LSTSUB36 TM    SCINDS1,SCISWYES                                                 
         BO    *+12                                                             
         TM    CSLTINDS,CSLTIEOP+CSLTIEOL                                       
         BZ    LSTSUB38                                                         
         XC    SUBINP,SUBINP                                                    
         MVI   SUBINP,C'*'                                                      
         B     LSTSUB70                                                         
*                                                                               
LSTSUB38 LA    R4,SELTABL(R4)      BUMP TO NEXT SELECT TABLE ENTRY              
         B     LSTSUB22                                                         
*                                                                               
LSTSUB40 OI   SCINDS1,SCIVSACT                                                  
*                                                                               
         TM    SELTIND1,SELTINTR   TEST NTRSES REQUIRED                         
         BO    LSTSUB60                                                         
         TM    SELTIND1,SELTIRED   TEST NOW RE-DISPLAY LINE                     
         BZ    LSTSUB50                                                         
         GOTO1 ADISLINE,SCPARM,SUBINPH                                          
         B     LSTSUB54                                                         
*                                                                               
LSTSUB50 GOTO1 AVALLINE,SUBINPH                                                 
         BE    LSTSUB54            LINE UNTOUCHED                               
         BH    LSTSUB52                                                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         OI    SCINDS1,SCIVLERR                                                 
         TM    SCINDS1,SCIVSACT                                                 
         BZ    LSTSUB56                                                         
         B     LSTSUB58                                                         
*        TM    SELTIND1,SELTIPSI   TEST PROTECT SUB-ACTION IF ERROR             
*        BZ    LSTSUB56                                                         
*        OI    SUBINPH+FHATD,FHATPR                                             
*        NI    SUBINPH+FHOID,FF-FHOIMO                                          
*        B     LSTSUB58                                                         
*                                                                               
LSTSUB52 OI    CSLTINDS,CSLTIANY   SET LINE PROCESSED                           
*                                                                               
LSTSUB54 GOTO1 ACLCLST             TEST CHANGE IN TSAR TABLE                    
         BE    LSTSUB56                                                         
         GOTO1 ATSARIO,TSAPUT      YES - WRITE IT BACK                          
*                                                                               
LSTSUB56 TM    MIXLIND1,MIXLINSA   TEST NO SUB-ACTION                           
         BO    *+8                                                              
         MVI   SUBINP,C'*'                                                      
         TM    SCINDS1,SCIVSACT                                                 
         BZ    LSTSUB58                                                         
         TM    MIXLIND1,MIXLINSA   TEST NO SUB-ACTION                           
         BO    LSTSUB57                                                         
         XR    RE,RE                                                            
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,TWAD(RE)                                                      
         MVC   SUBINP+1(L'SUBINP-1),0(RE)                                       
         NI    SUBINPH+FHATD,FF-FHATPR                                          
LSTSUB57 TM    SELTIND2,SELTISTP   TEST STOP PROCESSING LIST LINES              
         BZ    LSTSUB58                                                         
         LA    RE,SUBINPH                                                       
         ST    RE,FVADDR                                                        
         B     LSTEXIT                                                          
*                                                                               
LSTSUB58 TM    SCINDS1,SCIVLERR                                                 
         BO    LSTEXIT                                                          
         B     LSTSUB70                                                         
         EJECT                                                                  
***********************************************************************         
* NTRSES                                                              *         
***********************************************************************         
         SPACE 1                                                                
LSTSUB60 GOTO1 ASCRLAST,0                                                       
         GOTO1 ANTRSES,SELTPARM                                                 
         SPACE 1                                                                
LSTR1    GOTO1 ASCRFRST            FIRST FOR SCREEN                             
         BL    LSTEXIT                                                          
         XR    R2,R2                                                            
         ICM   R2,3,CSSELACT                                                    
         A     R2,ATWA             R2=A(ACTION LINE)                            
         XR    R4,R4                                                            
         ICM   R4,3,CSSELCUR                                                    
         A     R4,AOVERSEL         R4=A(SELECT TABLE ENTRY)                     
*                                                                               
         MVI   SUBINP,C'*'                                                      
         XR    RE,RE                                                            
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,TWAD(RE)                                                      
         MVC   SUBINP+1(L'SUBINP-1),0(RE)                                       
*                                                                               
         CLI   SCNTRSES,2          TEST TRANSACTION INSERTED                    
         BNE   LSTSUB62                                                         
         XR    R0,R0                                                            
         ICM   R0,3,CSNXRECN                                                    
         XR    RE,RE                                                            
         ICM   RE,3,CSHIRECN                                                    
         SR    R0,RE               R0=NO. OF EXTRA RECORDS                      
         MVC   CSHIRECN,CSNXRECN   SET NEW HIGH RECORD NUMBER                   
         XR    R1,R1                                                            
         ICM   R1,3,CSPAG#LO                                                    
         GOTO1 ASETPAGE                                                         
         B     LSTDIS                                                           
*                                                                               
LSTSUB62 GOTO1 ATSARIO,TSAGET                                                   
         GOTO1 ADISLINE,SCPARM,('DLIUPD',SUBINPH)                               
         CLI   BCPFKEY,PFKQUITQ                                                 
         BNE   LSTSUB70                                                         
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         B     LSTSUB90                                                         
         EJECT                                                                  
***********************************************************************         
* BUMP TO NEXT LINE                                                   *         
***********************************************************************         
         SPACE 1                                                                
LSTSUB70 TM    MIXLIND1,MIXLINSA   TEST NO SUB-ACTION                           
         BO    *+8                                                              
         OI    SUBINPH+FHIID,FHIIVA                                             
         AH    R2,CSLINLEN         BUMP TO NEXT ACTION FIELD                    
         ICM   R1,3,TLNUM          BUMP TO NEXT RECORD NUMBER                   
         LA    R1,1(R1)                                                         
         STCM  R1,3,TLNUM                                                       
LSTSUB71 IC    R0,CSSELREM                                                      
         BCTR  R0,0                                                             
         STC   R0,CSSELREM                                                      
         CLI   CSSELREM,0                                                       
         BNE   LSTSUB06            DO FOR NUMBER OF ENTRIES ON SCREEN           
         DROP  R4,R2                                                            
*                                                                               
         NI    CSLTINDS,FF-CSLTIEOP                                             
         ICM   R1,3,TLNUM                                                       
         BCTR  R1,0                                                             
         STCM  R1,3,TLNUM                                                       
         TM    CSLTINDS,CSLTIEOF   TEST END OF FILE REACHED  ??                 
         BZ    LSTSUB72                                                         
         CLC   TLNUM,CSHIRECN      TEST THIS IS LAST RECORD                     
         BNE   LSTSUB72                                                         
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         XC    SCMIF,SCMIF                                                      
*                                                                               
LSTSUB72 LA    R2,SCMIFS           REMOVE MULTI-INPUT FIELDS                    
         USING MIFELD,R2             THAT FINISH ON THIS PAGE                   
LSTSUB74 CLI   MIFEL,0                                                          
         BE    LSTSUB78                                                         
         CLC   MIF#HI,CSPAG#HI                                                  
         BNE   LSTSUB76                                                         
         MVI   MIFEL,FF                                                         
         GOTO1 VHELLO,SCPARM,(C'D',CORETAB),('FF',SCMIF),0                      
         B     LSTSUB74                                                         
LSTSUB76 XR    RF,RF                                                            
         IC    RF,MIFLN                                                         
         BXH   R2,RF,LSTSUB74                                                   
         DROP  R2                                                               
*                                                                               
LSTSUB78 CLI   SCMIFS,0            TEST MULTI-INPUT FIELD                       
         BNE   LSTSCR                                                           
         TM    CSLTINDS,CSLTIEOL   TEST SELECT TO END-OF-LIST                   
         BNZ   LSTSCR                                                           
*                                                                               
         ICM   RF,15,APFKNTRY      TEST FOR APPLICATION PFKEY ROUTINE           
         BZ    LSTSUB80                                                         
         TM    PFKINDS3-PFKTABD(RF),PFKIRTN                                     
         BZ    LSTSUB80                                                         
         OI    SCINDS1,SCIPFRTN                                                 
         GOTO1 OPFKRTN                                                          
         BNE   LSTEXIT                                                          
         B     LSTDIS                                                           
*                                                                               
LSTSUB80 TM    CSLTINDS,CSLTIANY   TEST ANYTHING PROCESSED                      
         BZ    LSTSCR                                                           
*                                                                               
LSTSUB90 TM    MIXLIND1,MIXLIVPG   TEST NEED TO RE-DISPLAY PAGE                 
         BZ    *+12                                                             
         OI    SCINDS2,SCIVPG                                                   
         B     LSTDIS                                                           
         L     RE,ATWA                                                          
         AH    RE,LSDEFCUR                                                      
         ST    RE,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$ACTOK)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     LSTEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* HANDLE SCROLLING                                                    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVC   SCPAG#LO,CSPAG#LO   SAVE LAST LOW & HIGH RECORDS                 
         MVC   SCPAG#HI,CSPAG#HI                                                
         XC    CSPAG#LO,CSPAG#LO   CLEAR LOW & HIGH VALUES FOR PAGE             
         XC    CSPAG#HI,CSPAG#HI                                                
*                                                                               
         MVC   SCVERSCR,BCSCRNUM   SET SCROLL MAGNITUDES = # ENTERED            
         MVC   SCHORSCR,BCSCRNUM                                                
         TM    BCSCRNUM,PFKIMAXN+PFKIPAGE+PFKIHALF                              
         BZ    LSTSCR02                                                         
*                                                                               
         MVC   SCVERSCR,CSLSTMAX   SET SCROLL MAGNITUDES = 1 PAGE               
         IC    RF,CSRHSDIS                                                      
         IC    RE,CSLHSDIS                                                      
         SR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         STC   RF,SCHORSCR                                                      
*                                                                               
         TM    BCSCRNUM,PFKIHALF   TEST HALF PAGE SCORLL                        
         BZ    LSTSCR02                                                         
         XR    RE,RE                                                            
         IC    RE,SCVERSCR                                                      
         TM    BCSCROLL,PFKIUPDN                                                
         BNZ   *+8                                                              
         IC    RE,CSLSTNUM                                                      
         SRA   RE,1                                                             
         BNZ   *+8                                                              
         LA    RE,1                                                             
         STC   RE,SCVERSCR                                                      
         IC    RE,SCHORSCR                                                      
         SRA   RE,1                                                             
         BNZ   *+8                                                              
         LA    RE,1                                                             
         STC   RE,SCHORSCR                                                      
*                                                                               
LSTSCR02 TM    CSLTINDS,CSLTIFST   TEST FIRST FOR LIST                          
         BZ    LSTSCR08                                                         
         GOTO1 ABLDLST,0           BUILD THE LIST                               
         BNE   LSTEXIT                                                          
         TM    CSINDSL1,CSIUENTK   TEST FIRST TIME THRU                         
         BZ    LSTSCR04                                                         
         TM    BCINDS2,BCIREBLS    TEST RE-BUILD SCREEN                         
         BO    LSTSCR04                                                         
         CLC   LSCLM,SCCLM         TEST DIS= HAS CHANGED                        
         BE    LSTSCR06                                                         
LSTSCR04 GOTO1 ABLDSCR,RIGHTQ                                                   
LSTSCR06 GOTO1 ASETPAGE,0          SET FOR 1ST PAGE                             
         B     LSTSCRX                                                          
*                                                                               
LSTSCR08 TM    SCINDS2,SCISAVIK    TEST SAVE INPUT KEY FIELDS                   
         BZ    LSTSCR09                                                         
         GOTO1 ASAVKEYF                                                         
LSTSCR09 CLC   LSCLM,SCCLM         TEST DIS= HAS CHANGED                        
         BNE   LSTSCR30                                                         
         TM    BCINDS2,BCIREBLS    TEST RE-BUILD SCREEN                         
         BZ    LSTSCR10                                                         
         XR    R1,R1                                                            
         IC    R1,CSLHSDIS                                                      
         B     LSTSCR32                                                         
*                                                                               
LSTSCR10 TM    CSLTINDS,CSLTIEOL   TEST SELECT TO END OF LIST                   
         BNZ   *+12                                                             
         CLI   SCMIFS,0                                                         
         BE    LSTSCR20                                                         
         MVC   SCVERSCR,CSLSTMAX   SET SCROLL = 1 PAGE                          
         B     LSTSCR48              AND SCROLL DOWN                            
*                                                                               
LSTSCR20 TM    BCSCROLL,PFKIHORZ   TEST HORIZONTALLY SCROLLING                  
         BZ    LSTSCR40                                                         
         XR    RF,RF               RF=HORIZONTAL SCROLLING MAGNITUDE            
         IC    RF,SCHORSCR                                                      
*                                                                               
         TM    BCSCROLL,PFKIUPDN   TEST SCROLLING RIGHT                         
         BO    LSTSCR22                                                         
         TM    BCSCRNUM,PFKIMAXN   TEST MAXIMUM SCROLL                          
         BO    LSTSCR34                                                         
         XR    R1,R1                                                            
         IC    R1,CSLHSDIS                                                      
         AR    R1,RF                                                            
         CLM   R1,1,LSCLMN         TEST SCROLLED OVER END                       
         BL    LSTSCR32                                                         
         IC    RE,LSCLMN                                                        
         BCTR  RE,0                                                             
         CLM   RE,1,CSRHSDIS       TEST LAST COLUMN CURRENTLY DISPLAYED         
         BE    LSTSCR30            YES - DISPLAY FIRST PAGE                     
         B     LSTSCR34            NO - DISPLAY LAST PAGE                       
*                                                                               
LSTSCR22 TM    BCSCRNUM,PFKIMAXN   TEST MAXIMUM SCROLL                          
         BO    LSTSCR30                                                         
         XR    R1,R1               SCROLL LEFT                                  
         TM    BCSCRNUM,PFKIPAGE+PFKIHALF                                       
         BNZ   LSTSCR24                                                         
         IC    R1,CSLHSDIS         ADJUST LHS COLUMN IF # ENTERED               
         SR    R1,RF                                                            
         BNM   LSTSCR32                                                         
         B     LSTSCR30                                                         
LSTSCR24 IC    R1,CSRHSDIS         ADJUST RHS COLUMN IF PAGE/HALF               
         SR    R1,RF                                                            
         BNM   LSTSCR36                                                         
*                                                                               
LSTSCR30 XR    R1,R1               DISPLAY FIRST PAGE                           
LSTSCR32 LA    R1,RIGHTQ(R1)                                                    
         B     LSTSCR38                                                         
LSTSCR34 IC    R1,LSCLMN           DISPLAY LAST PAGE                            
         BCTR  R1,0                                                             
LSTSCR36 LA    R1,LEFTQ(R1)                                                     
*                                                                               
LSTSCR38 GOTO1 ABLDSCR,(R1)                                                     
         XC    SCMIF,SCMIF                                                      
         NI    CSLTINDS,FF-CSLTIEOL                                             
         OI    CSLTINDS,CSLTIHLD                                                
*                                                                               
LSTSCR40 TM    CSLTINDS,CSLTIHLD   TEST HOLD PAGE IF DISPLAY CHANGED            
         BZ    LSTSCR42                                                         
         MVC   CSPAG#LO,SCPAG#LO   RESTORE LAST LOW & HIGH RECORDS              
         MVC   CSPAG#HI,SCPAG#HI                                                
         B     LSTDIS                                                           
*                                                                               
LSTSCR42 TM    BCSCROLL,PFKIUPDN   TEST SCROLL UP OR DOWN                       
         BZ    LSTSCR46                                                         
*                                                                               
         XR    R1,R1               SCROLL UP (BACKWARDS)                        
         TM    BCSCRNUM,PFKIMAXN   TEST SCROLL FIRST                            
         BNZ   LSTSCR44                                                         
         XR    RF,RF                                                            
         IC    RF,SCVERSCR                                                      
         ICM   R1,3,SCPAG#LO                                                    
         SR    R1,RF               BACK-UP TO RECORD NUMBER                     
         BM    LSTSCR44                                                         
         ICM   RF,3,CSPSRECN                                                    
         LA    RF,1(RF)                                                         
         CR    R1,RF               TEST NOT < LOW RECORD FOR SESSION            
         BH    LSTSCR50                                                         
LSTSCR44 XR    R1,R1               SET TO START FROM TOP OF PAGE                
         OI    SCINDS1,SCINOSUP    SET CAN'T SCROLL UP ANY MORE                 
         B     LSTSCR50                                                         
*                                                                               
LSTSCR46 TM    BCSCRNUM,PFKIMAXN   SCROLL DOWN (FORWARDS)                       
         BZ    LSTSCR48                                                         
         XR    R1,R1               MAXIMUM SCROLL                               
         ICM   R1,3,CSHIRECN                                                    
         TM    CSLTINDS,CSLTIEOF   TEST LIST NOT 100% BUILT                     
         BZ    LSTSCR50            YES - CONTINUE FROM END                      
         XR    RF,RF                                                            
         IC    RF,CSLSTMAX                                                      
         BCTR  RF,0                                                             
         SR    R1,RF                                                            
         BM    *+12                                                             
         CLM   R1,3,CSPSRECN                                                    
         BH    LSTSCR50                                                         
         XR    R1,R1                                                            
         B     LSTSCR50                                                         
*                                                                               
LSTSCR48 XR    RF,RF                                                            
         IC    RF,SCVERSCR                                                      
         CLC   SCVERSCR,CSLSTNUM                                                
         BNH   *+8                                                              
         IC    RF,CSLSTNUM                                                      
         XR    R1,R1                                                            
         ICM   R1,3,SCPAG#LO                                                    
         AR    R1,RF                                                            
         CLM   R1,3,CSHIRECN                                                    
         BNH   LSTSCR50                                                         
         TM    CSLTINDS,CSLTIEOF   TEST LIST 100% BUILT                         
         BZ    LSTSCR50                                                         
         XR    R1,R1                                                            
*                                                                               
LSTSCR50 GOTO1 ASETPAGE,(R1)                                                    
*                                                                               
LSTSCRX  B     LSTDIS                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY PAGE OF DATA                                                *         
***********************************************************************         
         SPACE 1                                                                
LSTDISR  GOTO1 ASCRFRST            RETURN AFTER QUIT PFKEY PRESSED              
         BL    LSTEXIT                                                          
*                                                                               
LSTDIS   L     R1,ATWA                                                          
         AH    R1,CS1STLIN                                                      
         XR    RF,RF                                                            
         USING FHD,R1                                                           
TWAXC02  ICM   RF,1,FHLN                                                        
         BZ    TWAXC10                                                          
         CLI   FHAT,FHATNP                                                      
         BE    TWAXC08                                                          
         SH    RF,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH                                                      
         BO    *+8                                                              
         LA    RF,FHDAD(RF)                                                     
         EX    RF,*+4                                                           
         MVC   FHDA(0),BCSPACES                                                 
         OI    FHAT,FHATPR                                                      
         OI    FHOI,FHOITR                                                      
         IC    RF,FHLN                                                          
TWAXC08  BXH   R1,RF,TWAXC02                                                    
*                                                                               
TWAXC10  DS    0H                                                               
         DROP  R1                                                               
*                                                                               
LSTDIS02 TM    MIXLIND1,MIXLISCT   RESET SCREEN TOTAL LINE                      
         BZ    LSTDIS06                                                         
         XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKSSTOT                                                  
         LA    R0,TSAPUT                                                        
         GOTO1 ATSARIO,TSARDH                                                   
         BE    LSTDIS04                                                         
         XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKSSTOT                                                  
         LA    R0,TSAADD                                                        
LSTDIS04 GOTO1 ASUBTOT,SCPARM,(C'I',0)                                          
         GOTO1 ATSARIO,(R0)                                                     
*                                                                               
LSTDIS06 CLC   CSHIRECN,CSPSRECN   TEST ANY RECORDS FOUND                       
         BNE   LSTDIS08                                                         
         MVC   FVMSGNO,=AL2(AI$NOREC)                                           
         MVI   FVOMTYP,GTMINF                                                   
         L     RE,ATWA                                                          
         AH    RE,LSDEFCUR                                                      
         ST    RE,FVADDR                                                        
         OI    CSLTINDS,CSLTIFST                                                
         B     LSTEXIT                                                          
*                                                                               
LSTDIS08 MVC   TLNUM,CSPAG#LO                                                   
         CLC   TLNUM,CSSEL#HI      TEST > HIGH MULTIPLE SELECT                  
         BNH   *+14                                                             
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NUMBER OF LINES TO DISPLAY                
         BZ    LSTDIS14            CAN OCCUR ON XITSES IF RECORD ADDED          
*        BNZ   *+6                                                              
*        DC    H'0'                DIE FOR NOW IF NO DISPLAY LINES              
         L     R2,ATWA                                                          
         AH    R2,CS1STLIN                                                      
         USING SUBINPH,R2          R2=A(SCREEN LINE)                            
*                                                                               
LSTDIS12 GOTO1 ATSARIO,TSAGET      GET LIST RECORD                              
         GOTO1 ADISLINE,SCPARM,('DLIUPD+DLISTOT',SUBINPH)                       
         TM    MIXLIND1,MIXLINSA   TEST NO SUB-ACTION                           
         BO    *+8                                                              
         OI    SUBINPH+FHIID,FHIIVA                                             
*                                                                               
LSTDISR1 AH    R2,CSLINLEN                                                      
         XR    R1,R1                                                            
         ICM   R1,3,TLNUM                                                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,TLNUM                                                       
         BCT   R0,LSTDIS12                                                      
         DROP  R2                                                               
*                                                                               
         BCTR  R1,0                                                             
         STCM  R1,3,TLNUM                                                       
         CLC   TLNUM,CSSEL#LO      TEST < LOW MULTIPLE SELECT                   
         BNL   *+14                                                             
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         TM    CSLTINDS,CSLTIEOL   TEST SELECT TO END OF LIST                   
         BNZ   LSTSUB04                                                         
         CLI   SCMIFS,0                                                         
         BNE   LSTSUB04                                                         
*                                                                               
         TM    SCINDS2,SCISAVIK    TEST INPUT KEY FIELDS SAVED                  
         BZ    LSTDIS14                                                         
         GOTO1 ARESKEYF                                                         
         BE    LSTSUB04            RE-PROCESS SCREEN                            
*                                                                               
LSTDIS14 L     RE,ATWA                                                          
         AH    RE,LSDEFCUR                                                      
         ST    RE,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         TM    SCINDS2,SCIVPG      TEST RE-DISPLAY JUST DONE                    
         BO    *+12                                                             
         CLI   SCNTRSES,2          TEST INSERTION JUST DONE                     
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AI$ACTOK)                                           
         B     LSTEXIT                                                          
         TM    SCINDS1,SCIPFRTN    KEEP USER MESSAGE IF PFKEY ROUTINE           
         BO    LSTEXIT               WAS CALLED                                 
*                                                                               
         CLC   SCCLM,LSCLM         TEST DIS= HAS CHANGED                        
         BE    LSTDIS15                                                         
         IC    RE,LSCLM            YES - TEST ALL COLUMNS ON SCREEN             
         BCTR  RE,0                                                             
         CLM   RE,1,CSRHSDIS                                                    
         BNE   *+12                                                             
         CLI   CSLHSDIS,0                                                       
         BE    LSTDIS15                                                         
         MVC   FVMSGNO,=AL2(AI$RDSCR) 'USE PF9/10 TO SCROLL LEFT/RIGHT'         
         B     LSTEXIT                                                          
*                                                                               
LSTDIS15 TM    CSLTINDS,CSLTIEOF   TEST LAST RECORD DISPLAYED                   
         BZ    LSTDIS20                                                         
         CLC   TLNUM,CSHIRECN                                                   
         BL    LSTDIS20                                                         
*                                                                               
         XR    RF,RF               TEST FIRST RECORD DISPLAYED                  
         ICM   RF,3,CSPAG#LO                                                    
         BCTR  RF,0                                                             
         CLM   RF,3,CSPSRECN                                                    
         BNE   LSTDIS16                                                         
         MVC   FVMSGNO,=AL2(AI$ALLRD)  'ALL RECORDS DISPLAYED'                  
         TM    MIXLIND1,MIXLINDA                                                
         BZ    LSTEXIT                                                          
         MVC   FVMSGNO,=AL2(GI$ALLID)  'ALL ITEMS DISPLAYED'                    
         B     LSTEXIT                                                          
*                                                                               
LSTDIS16 XR    RE,RE                                                            
         IC    RE,SCVERSCR                                                      
         TM    BCSCRNUM,PFKIHALF                                                
         BZ    *+8                                                              
         LA    RE,1                                                             
         LA    RE,1(RE,RF)                                                      
         CLM   RE,3,CSHIRECN                                                    
         BNH   LSTDIS20                                                         
*                                                                               
         MVC   FVMSGNO,=AL2(AI$EOLEF)  '.. ENTER FOR FIRST'                     
         B     LSTEXIT                                                          
*                                                                               
LSTDIS20 MVC   FVMSGNO,=AL2(AI$DISNX)  '.. ENTER FOR NEXT'                      
         TM    MIXLIND1,MIXLINDA                                                
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(GI$DISNX)                                           
         TM    SCINDS1,SCINOSUP    TEST SCROLLED UP TO TOP                      
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$STDIS)  'START OF DISPLAY REACHED'               
         EJECT                                                                  
***********************************************************************         
* END OF SCREEN                                                       *         
***********************************************************************         
         SPACE 1                                                                
LSTEXIT  GOTO1 ASCRLAST,1                                                       
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BRANCH TO CONTROLLER ROUTINE                                        *         
***********************************************************************         
         SPACE 1                                                                
ROUT     DS    0H                                                               
         NTR1  ,                                                                
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         L     R8,AROUT(RF)                                                     
         A     R8,RELO                                                          
         BR    R8                                                               
         SPACE 1                                                                
AROUT    DS    0F                  ROUTINES                                     
         DC    A(EXCLINE)                                                       
         DC    A(SETPAGE)                                                       
         DC    A(VALLINE)                                                       
         DC    A(DISLINE)                                                       
         DC    A(DISTOT)                                                        
         DC    A(DISSCR)                                                        
         DC    A(BLDLST)                                                        
         DC    A(GETREC)                                                        
         DC    A(PUTREC)                                                        
         DC    A(SCRFRST)                                                       
         DC    A(SCRLAST)                                                       
         DC    A(INISCR)                                                        
         DC    A(BLDSCR)                                                        
         DC    A(ADDLIN)                                                        
         DC    A(SAVKEYF)                                                       
         DC    A(RESKEYF)                                                       
         DC    A(DISCLM)                                                        
         DC    A(SAVLST)                                                        
         DC    A(RESLST)                                                        
         DC    A(CLCLST)                                                        
AROUTN   EQU   (*-AROUT)/L'AROUT                                                
         SPACE 1                                                                
***********************************************************************         
* SPACE FOR LITERAL POOL                                              *         
***********************************************************************         
         SPACE 1                                                                
LTORG    DS    0H                                                               
         ORG   CLB30+X'0E00'                                                    
LTORGX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* EXCLUDE LINE FROM LIST                                              *         
*                                                                     *         
* NTRY: R1=A(CURRENT LIST LINE)                                       *         
* EXIT: CC=EQUAL TO CONTINUE PROCESSING THE SCREEN                    *         
***********************************************************************         
         SPACE 1                                                                
EXCLINE  DS    0H                                                               
         USING *,R8                                                             
         LR    R4,R1                                                            
         USING SUBINPH,R4          R4=A(LIST LINE)                              
*                                                                               
         LA    RE,SUBINPH          SCROLL PAGE UP 1 LINE                        
         LR    R0,RE                                                            
         AH    R0,CSLINLEN                                                      
         LH    RF,CS1STFTL                                                      
         LA    RF,TWAD(RF)                                                      
         SR    RF,R0                                                            
         BZ    ELINE10             LAST LINE DELETED - NO SCROLL                
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,SUBINPH          TRANSMIT REST OF SCREEN                      
         USING FHD,R2                                                           
         LH    RF,CS1STFTL                                                      
         SH    RF,CSLINLEN                                                      
         LA    RF,TWAD(RF)                                                      
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
         LA    R0,NUMCOLSQ                                                      
ELINE02  CLI   FHAT,FHATNP                                                      
         BE    ELINE04                                                          
         OI    FHOI,FHOITR                                                      
         ICM   R1,3,FHAD                                                        
         SR    R1,R0                                                            
         STCM  R1,3,FHAD                                                        
ELINE04  IC    RE,FHLN                                                          
         BXLE  R2,RE,ELINE02                                                    
*                                                                               
ELINE10  LH    R2,CS1STFTL                                                      
         LA    R2,TWAD(R2)                                                      
         LR    RF,R2                                                            
         BCTR  RF,0                                                             
         SH    R2,CSLINLEN                                                      
         XR    RE,RE               CLEAR LAST LINE                              
ELINE12  IC    RE,FHLN                                                          
         CLI   FHAT,FHATNP                                                      
         BE    ELINE18                                                          
         OI    FHOI,FHOITR                                                      
         LR    R1,RE                                                            
         SH    R1,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH                                                      
         BO    *+8                                                              
         LA    R1,FHDAD(R1)                                                     
         EX    R1,*+4                                                           
         MVC   FHDA(0),BCSPACES                                                 
ELINE18  BXLE  R2,RE,ELINE12                                                    
         DROP  R2                                                               
*                                                                               
         GOTO1 ATSARIO,TSADEL      DELETE THE TSAR RECORD                       
*                                                                               
         CLC   CSPAG#LO,CSHIRECN   TEST PAGE NOW EMPTY                          
         BNH   ELINE20                                                          
         CLC   CSHIRECN,CSPSRECN   TEST LIST NOW EMPTY                          
         BE    EXCLINEN                                                         
         XR    R1,R1               DISPLAY NEW LAST LINE                        
         ICM   R1,3,CSHIRECN                                                    
         GOTO1 ASETPAGE,(R1)                                                    
         GOTO1 EXCLDIS                                                          
EXCLINEN B     EXITN                                                            
*                                                                               
ELINE20  XR    R1,R1               DISPLAY NEW LAST LINE                        
         ICM   R1,3,CSPAG#LO                                                    
         GOTO1 ASETPAGE,(R1)                                                    
         CLC   CSLSTNUM,CSLSTMAX   TEST PAGE FULL                               
         BNE   EXCLINEY                                                         
         GOTO1 EXCLDIS             YES - DISPLAY NEW LAST LINE                  
*                                                                               
EXCLINEY MVC   TLNUM,SCLST+(TLNUM-TLSTD)                                        
         B     EXITY                                                            
         SPACE 1                                                                
EXCLDIS  NTR1  ,                   * DISPLAY LINE AT BOTTOM OF SCREEN *         
         MVC   TLNUM,CSPAG#HI                                                   
         GOTO1 ATSARIO,TSAGET                                                   
         XR    R2,R2                                                            
         IC    R2,CSLSTNUM                                                      
         BCTR  R2,0                                                             
         MH    R2,CSLINLEN                                                      
         AH    R2,CS1STLIN                                                      
         LA    R2,TWAD(R2)                                                      
         TM    MIXLIND1,MIXLINSA   TEST NO SUB-ACTION                           
         BO    *+10                                                             
         XC    FHDAD(L'SUBINP,R2),FHDAD(R2)                                     
         GOTO1 ADISLINE,SCPARM,('DLISTOT',(R2))                                 
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET LOW/HIGH LIST NUMBERS FOR PAGE                       *         
*                                                                     *         
* NTRY: R1 = LIST NUMBER FOR TOP OF PAGE OR 0 FOR TOP OF LIST         *         
* EXIT: CLSSTNUM, CSPAG#LO, CSPAG#HI ARE ALL SET                      *         
*       CSPAG#LO IS SET                                               *         
***********************************************************************         
         SPACE 1                                                                
SETPAGE  DS    0H                                                               
         USING *,R8                                                             
         LTR   R3,R1                                                            
         BNZ   *+12                                                             
         ICM   R3,3,CSPSRECN                                                    
         LA    R3,1(R3)                                                         
         STCM  R3,3,CSPAG#LO       SET LOW PAGE NUMBER                          
         CLC   CSPAG#LO,CSPSRECN                                                
         BH    *+6                                                              
         DC    H'0'                                                             
         XR    R4,R4               SET HIGH PAGE NUMBER                         
         IC    R4,CSLSTMAX                                                      
         BCTR  R4,0                                                             
         AR    R4,R3                                                            
*                                                                               
SPAGE02  CLM   R4,3,CSHIRECN       TEST WITHIN CURRENT LIST                     
         BL    SPAGE10                                                          
         TM    CSLTINDS,CSLTIEOF   TEST LIST FINISHED                           
         BO    SPAGE08                                                          
         GOTO1 ABLDLST,1           NO - BUILD ANOTHER PAGE                      
         BE    *+6                                                              
         DC    H'0'                LIST TOO BIG                                 
         B     SPAGE02                 AND TRY AGAIN                            
*                                                                               
SPAGE08  ICM   R4,3,CSHIRECN                                                    
*                                                                               
SPAGE10  STCM  R4,3,CSPAG#HI                                                    
         SR    R4,R3               SET NUMBER OF RECORDS ON PAGE                
         BNM   SPAGE12                                                          
         TM    CSLTINDS,CSLTIEOF   ZERO RECORDS OKAY IF LIST FINISHED           
         BO    *+6                                                              
         DC    H'0'                                                             
SPAGE12  LA    R4,1(R4)                                                         
         STC   R4,CSLSTNUM                                                      
SETPAGEX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE LINE                                                       *         
*                                                                     *         
* NTRY: R1=A(LIST LINE)                                               *         
* EXIT: CC=ZERO, NO INPUT ON LINE                                     *         
*       CC=LOW,  ERROR IN INPUT FIELD                                 *         
*       CC=HIGH, RECORD UPDATED                                       *         
***********************************************************************         
         SPACE 1                                                                
VALLINE  DS    0H                                                               
         USING *,R8                                                             
         ST    R1,ASUBINPH                                                      
         NI    LSINDS1,FF-LSICLMIN                                              
         NI    SCINDS1,FF-SCIREDIS                                              
         GOTO1 OVALFRST,(R1)                                                    
         BNE   VALLINEN                                                         
*                                                                               
         LA    R2,LSLIN            FIND ORDER TO VALIDATE IN                    
         USING LINTABD,R2                                                       
         XC    SCVORD(SCVORDS),SCVORD                                           
VLINE02  CLI   LINTABD,EOT                                                      
         BE    VLINE10                                                          
         TM    LININDS,LINIINP                                                  
         BZ    VLINE08                                                          
N        USING VORDD,BCFULL                                                     
         MVC   N.VORDCLM,LINCLM                                                 
         MVC   N.VORDHDR,LINHDR                                                 
         L     RF,ACLMTAB                                                       
         AH    RF,LINCLM                                                        
         MVC   N.VORD#,CLMVORD-CLMTABD(RF)                                      
         LA    R4,SCVORD                                                        
O        USING VORDD,R4                                                         
VLINE04  CLI   O.VORDD,EOT                                                      
         BE    VLINE06                                                          
         CLC   N.VORD#,O.VORD#                                                  
         BL    VLINE06                                                          
         LA    R4,VORDL(R4)                                                     
         B     VLINE04                                                          
VLINE06  MVC   SCVORDWK,O.VORDD                                                 
         MVC   O.VORDD(VORDL),N.VORDD                                           
         MVC   O.VORDD+VORDL(L'SCVORDWK),SCVORDWK                               
         DROP  O,N                                                              
*                                                                               
VLINE08  LA    R2,LINTABL(R2)                                                   
         B     VLINE02                                                          
         DROP  R2                                                               
*                                                                               
VLINE10  LA    R2,SCVORD                                                        
         USING VORDD,R2                                                         
VLINE12  CLI   VORDD,EOT                                                        
         BE    VLINE50                                                          
*                                                                               
         L     R3,ACLMTAB                                                       
         AH    R3,VORDCLM                                                       
         USING CLMTABD,R3          R3=A(COLUMN TABLE ENTRY)                     
         XR    R6,R6                                                            
         IC    R6,VORDHDR                                                       
         A     R6,ASUBINPH                                                      
         USING FHD,R6              R6=A(INPUT FIELD)                            
         TM    FHAT,FHATPR                                                      
         BO    VLINE48             'INPUT' FIELD MAY BE SET PROTECTED           
*                                                                               
         TM    FHII,FHIIVA         TEST ANY INPUT TO FIELD                      
         BZ    VLINE20                                                          
         LA    R4,SCMIFS           TEST FOR MULTI-INPUT FIELD                   
         USING MIFELD,R4                                                        
VLINE14  CLI   MIFEL,0             FIELD NOT IN LIST                            
         BE    VLINE48                                                          
         IC    RF,MIFLN                                                         
         CLC   MIFCODE,CLMCODE                                                  
         BNE   VLINE18                                                          
         CLC   TLNUM,MIF#LO        TEST RECORD NUMBER WITHIN LIMITS             
         BL    VLINE18                                                          
         CLC   TLNUM,MIF#HI                                                     
         BH    VLINE18                                                          
         MVC   FVIFLD,BCSPACES                                                  
         ICM   RF,1,MIFFLDLN                                                    
         BCTR  RF,0                                                             
         BZ    VLINE16                                                          
         XR    RE,RE                                                            
         IC    RE,MIFQUALN                                                      
         LA    RE,MIFQUA(RE)                                                    
         EX    RF,*+4                                                           
         MVC   FVIFLD,0(RE)                                                     
VLINE16  STC   RF,FVXLEN                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FVILEN                                                        
         GOTO1 VALFLD,SCPARM,CLMTABD,MIFELD                                     
         BE    VLINE48             ??                                           
*                                                                               
VLINE18  XR    RF,RF                                                            
         IC    RF,MIFLN                                                         
         BXH   R4,RF,VLINE14                                                    
         DROP  R4                                                               
*                                                                               
VLINE20  XR    R4,R4                                                            
         GOTO1 AFVAL,FHD                                                        
         BL    VLINE30                                                          
         TM    CLMINDS2,CLMIEOP+CLMIEOL                                         
         BZ    VLINE30                                                          
*                                                                               
         CLI   FVIFLD,C'*'         TEST KEEP FIELD AS IS                        
         BE    VLINE21                                                          
*        CLI   FVIFLD,C'-'         TEST STOP MULTI SELECT                       
*        BNE   VLINE22                                                          
         B     VLINE22                                                          
* ????   GOTO1 VHELLO,SCPARM,(C'D',CORETAB),(CLMCHAR,SCMIF),0                   
VLINE21  OI    SCINDS1,SCIREDIS    SET RE-DISPLAY LINE                          
         B     VLINE48                                                          
*                                                                               
VLINE22  XR    RE,RE                                                            
         IC    RE,FVILEN                                                        
         LA    RE,FVIFLD-1(RE)                                                  
         TM    CLMINDS2,CLMIEOP    TEST SELECTION TO END-OF-PAGE                
         BZ    VLINE24                                                          
         CLI   0(RE),C'+'                                                       
         BNE   VLINE24                                                          
         MVC   BOHALF1,CSPAG#HI                                                 
         B     VLINE26                                                          
VLINE24  TM    CLMINDS2,CLMIEOL    TEST SELECTION TO END-OF-LIST                
         BZ    VLINE30                                                          
         CLI   0(RE),C'&&'                                                      
         BNE   VLINE30                                                          
         MVC   BOHALF1,BCEFFS                                                   
*                                                                               
VLINE26  MVI   0(RE),C' '          REMOVE +/& FROM INPUT FIELD                  
         MVC   FVILEN,FVXLEN                                                    
         IC    RE,FVXLEN                                                        
         BCTR  RE,0                                                             
         STC   RE,FVXLEN                                                        
         XC    SCELEM,SCELEM                                                    
         LA    R4,SCELEM                                                        
         USING MIFELD,R4           BUILD MIFEL ELEMENT                          
         MVI   MIFEL,MIFELQ                                                     
         MVC   MIFCODE,CLMCODE                                                  
         MVC   MIF#LO,TLNUM                                                     
         MVC   MIF#HI,BOHALF1                                                   
*                                                                               
VLINE30  GOTO1 VALFLD,SCPARM,CLMTABD,MIFELD                                     
         BE    VLINE32                                                          
         CLC   FVMSGNO,=AL2(FVFOK) INVALID INPUT                                
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV) SET DEFAULT ERROR MESSAGE                  
         B     VALLINEN                                                         
*                                                                               
VLINE32  LTR   R4,R4               TEST ADDING A MIFEL ????                     
         BZ    VLINE48                                                          
         LA    R1,SCMIFS                                                        
         XR    RF,RF                                                            
VLINE34  CLI   0(R1),0                                                          
         BE    VLINE40                                                          
         CLC   MIFEL,0(R1)                                                      
         BNE   VLINE38                                                          
         XR    RF,RF                                                            
         ICM   RF,1,MIFQUALN                                                    
         BZ    VLINE36                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   VLINE38                                                          
         CLC   MIFQUA(0),MIFQUA-MIFELD(R1)                                      
VLINE36  MVI   0(R1),FF                                                         
VLINE38  IC    RF,MIFLN-MIFELD(R1)                                              
         BXH   R1,RF,VLINE34                                                    
*                                                                               
VLINE40  GOTO1 VHELLO,SCPARM,(C'D',CORETAB),('FF',SCMIF),0                      
         XR    RF,RF                                                            
         IC    RF,MIFQUALN         RF=QUALIFIER LENGTH                          
         LA    R1,MIFQUA(RF)                                                    
         MVC   MIFFLDLN,FVILEN                                                  
         XR    RE,RE                                                            
         ICM   RE,1,MIFFLDLN       RE=INPUT FIELD LENGTH                        
         BZ    VLINE42                                                          
         EX    RE,*+4                                                           
         MVC   0(0,R1),FVIFLD                                                   
VLINE42  LA    RE,MIFLNQ(RF,RE)    SET ELEMENT LENGTH                           
         STC   RE,MIFLN                                                         
         GOTO1 VHELLO,SCPARM,(C'P',CORETAB),SCMIF,MIFELD                        
         CLC   SCMIFL,=AL2(L'SCMIF)                                             
         BL    VLINE48                                                          
         MVC   FVMSGNO,=AL2(FVFTOOM)  TOO MANY MULTIS                           
         B     VALLINEN                                                         
         DROP  R3,R6,R4                                                         
*                                                                               
VLINE48  LA    R2,VORDL(R2)                                                     
         B     VLINE12                                                          
         DROP  R2                                                               
*                                                                               
VLINE50  GOTO1 OVALLAST                                                         
         BNE   VALLINEN            LINE IS INVALID                              
*                                                                               
         TM    LSINDS1,LSIUPREC    TEST RECORD TO BE UPDATED                    
         BO    VLINE52                                                          
         TM    LSINDS1,LSICLMIN    TEST ANYTHING INPUT TO                       
         BO    VALLINER            YES - RE-DISPLAY LINE                        
         TM    SCINDS1,SCIREDIS                                                 
         BO    VALLINER                                                         
         B     EXITY               NO - RETURN CC=ZERO                          
*                                                                               
VLINE52  GOTO1 APUTREC             UPDATE RECORD                                
         BNE   VALLINEN                                                         
         TM    MIXLIND1,MIXLIVPG   TEST NO RE-DISPLAY UNTIL PAGE                
         BO    EXITH                                      VALIDATED             
*                                                                               
VALLINER GOTO1 ADISLINE,SCPARM,ASUBINPH RE-DISPLAY LINE                         
         B     EXITH               CC=HIGH                                      
*                                                                               
VALLINEN XC    SCMIF,SCMIF         TURN OFF MULTI-INPUT FIELDS                  
         OI    LSINDS1,LSILINER                                                 
         B     EXITL                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO VALIDATE FIELD                                           *         
*                                                                     *         
* NTRY: P1=COLUMN TABLE ENTRY                                         *         
*       P2=A(MIFEL) OR 0                                              *         
***********************************************************************         
         SPACE 1                                                                
VALFLD   NTR1  ,                                                                
         LM    R3,R4,0(R1)                                                      
         USING CLMTABD,R3                                                       
         TM    CLMINDS2,CLMINONE   TEST NO INPUT IS VALID                       
         BO    VFLD02                                                           
         CLI   FVILEN,0            TEST INPUT ERASED                            
         BNE   *+12                                                             
         OI    SCINDS1,SCIREDIS                                                 
         B     EXITY                                                            
         CLC   FVILEN,CLMFMINL     TEST INPUT FIELD TOO SHORT                   
         BNL   VFLD02                                                           
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         CLI   FVILEN,0                                                         
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     VALFLDN                                                          
*                                                                               
VFLD02   TM    LSINDS1,LSICLMIN    READ RECORD FOR UPDATE IF 1ST TIME           
         BNZ   VFLD04                                                           
         TM    MIXLIND1,MIXLINDA   TEST NO DISK ADDRESS                         
         BO    VFLD04                                                           
         L     RF,AIO1             MAY ALREADY HAVE RECORD FOR SUB-ACT          
         SH    RF,=Y(L'IODA+L'IOWORK)                                           
         CLC   TLDA,0(RF)                                                       
         BE    VFLD04                                                           
         GOTO1 AGETREC,IOLOCK                                                   
VFLD04   XR    R1,R1               VALIDATE INPUT                               
         IC    R1,CLMRTN                                                        
         STCM  R4,15,LSAMIFEL      SAVE A(MIFEL) FOR OVERLAY                    
         GOTO1 OVALCLM,(R1)                                                     
         BE    VALFLDY                                                          
         CLC   FVMSGNO,=AL2(FVFOK) INVALID INPUT                                
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV) SET DEFAULT ERROR MESSAGE                  
         B     VALFLDN                                                          
*                                                                               
VALFLDY  OI    LSINDS1,LSICLMIN                                                 
         B     EXITY                                                            
*                                                                               
VALFLDN  B     EXITN                                                            
         DROP  R3                                                               
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* DISPLAY LINE                                                        *         
*                                                                     *         
* NTRY: P1 BYTE 0 = 'DLIUPD' ON IF TSAR RECORD MAY BE UPDATED         *         
*                   'DLISTOT' ON TO ADD TO SCREEN TOTALS              *         
*             0-3 = A(LIST LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
DISLINE  DS    0H                                                               
         USING *,R8                                                             
         XR    R4,R4               R4=A(LINE)                                   
         ICM   R4,7,1(R1)                                                       
         MVC   DLINDS,0(R1)                                                     
         TM    TLINDS1,TLITOTL                                                  
         BZ    DLINE02                                                          
         TM    MIXLIND1,MIXLINSA   TEST NO SUB-ACTION                           
         BO    *+8                                                              
         OI    FHATD(R4),FHATPR                                                 
         B     EXIT                                                             
*                                                                               
DLINE02  TM    DLINDS,DLIUPD       TEST TSAR RECORD UPDATING                    
         BZ    DLINE03                                                          
         GOTO1 ASAVLST                                                          
*                                                                               
DLINE03  XR    RF,RF               ENSURE KEY FIELD IS CLEAR                    
         TM    MIXLIND1,MIXLINSA   TEST NO SUB-ACTION                           
         BO    DLINE04                                                          
         OI    FHATD(R4),FHATPR                                                 
         TM    TLINDS1,TLIPRO                                                   
         BO    *+8                                                              
         NI    FHATD(R4),FF-FHATPR                                              
         OI    FHOID(R4),FHOITR                                                 
         IC    RF,FHLND(R4)                                                     
DLINE04  AR    RF,R4                                                            
         IC    RE,FHLND(RF)                                                     
         SH    RE,=Y(FHDAD+1)                                                   
         EX    RE,*+4                                                           
         MVC   FHDAD(0,RF),BCSPACES                                             
*                                                                               
         TM    MIXLIND1,MIXLINDA   TEST NO DISK ADDRESS                         
         BO    DLINE10                                                          
         L     RF,AIO1                                                          
         SH    RF,=Y(L'IODA+L'IOWORK)                                           
         CLC   TLDA,0(RF)                                                       
         BE    DLINE10                                                          
         GOTO1 AGETREC,0                                                        
*                                                                               
DLINE10  LA    R2,LSLIN                                                         
         USING LINTABD,R2          R2=A(LINE TABLE)                             
*                                                                               
DLINE12  CLI   LINTABD,EOT         TEST END-OF-TABLE                            
         BE    DLINE20                                                          
         ST    R2,ALINNTRY                                                      
         L     R3,ACLMTAB                                                       
         AH    R3,LINCLM                                                        
         USING CLMTABD,R3          R3=A(COLUMN TABLE ENTRY)                     
         ST    R3,ACLMDATA                                                      
*                                                                               
         XR    RE,RE                                                            
         IC    RE,LINHDR                                                        
         LA    R6,0(RE,R4)         R6=A(FIELD HEADER FOR OUTPUT)                
         USING FHD,R6                                                           
         ST    R6,FVADDR                                                        
         OI    FHOI,FHOITR                                                      
         OI    FHII,FHIIVA         SET FIELD AS VALID                           
         NI    FHAT,FF-FHATHI                                                   
         TM    TLINDS1,TLIHIGH                                                  
         BZ    *+8                                                              
         OI    FHAT,FHATHI                                                      
         TM    LININDS,LINIINP     ENSURE INPUT FIELD IS AN INPUT FIELD         
         BZ    *+8                                                              
         NI    FHAT,FF-FHATPR                                                   
         TM    TLINDS1,TLIPRO      UNLESS PROTECTED LINE                        
         BZ    *+8                                                              
         OI    FHAT,FHATPR                                                      
         MVC   FVIHDR,FHD          SAVE HEADER / EXTENDED HEADER                
         IC    RE,FHLN                                                          
         SH    RE,=Y(FHDAD)                                                     
         LA    RE,FHD(RE)                                                       
         MVC   FVIXHDR,0(RE)                                                    
*                                                                               
         MVC   FVIFLD,BCSPACES     CLEAR FIELD FOR OUTPUT                       
         GOTO1 ADISCLM,CLMTABD                                                  
*                                                                               
         XR    RE,RE                                                            
         IC    RE,LINDSP                                                        
         LA    RF,FHDA(RE)         RF = A(OUTPUT)                               
         IC    RE,CLMFWDTH         RE = L(OUTPUT)                               
         TM    LININDS,LINIINP     TEST INPUT FIELD                             
         BZ    DLINE18                                                          
*                                                                               
         CLC   FHLN,FVIHDR+FHLND   TEST CHANGE IN FIELD LENGTH                  
         MVC   FHD(FHDAD),FVIHDR                                                
         BE    DLINE16                                                          
*                                                                               
         OI    LSINDS2,LSIXMIT     TRANSMIT OF SCREEN REQUIRED                  
         LA    R0,FHDAD+FHDAD(RE)  R0 = OVERALL WIDTH REQUIRED                  
         XR    R1,R1                                                            
         IC    R1,FHLN             R1 = WIDTH USED SO FAR                       
         SR    R0,R1               R0 = WITH STILL TO BE USED                   
         CH    R0,=Y(1)                                                         
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    R1,FHD(R1)                                                       
         STC   R0,FHLND(R1)        SET LENGTH OF DUMMY NOP FIELD                
         MVI   FHATD(R1),FHATNP    SET AS NOP FIELD                             
*                                                                               
DLINE16  IC    RE,FHLN             GET LENGTH FROM FIELD                        
         SH    RE,=Y(FHDAD)                                                     
         LA    R1,FHD(RE)          COPY EXTENDED HEADER                         
         MVC   0(L'FVIXHDR,R1),FVIXHDR                                          
         SH    RE,=Y(FHDAD)                                                     
*                                                                               
DLINE18  BCTR  RE,0                COPY FIELD                                   
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD                                                   
*                                                                               
         LA    R2,LINTABL(R2)      BUMP TO NEXT LINE TABLE ENTRY                
         B     DLINE12                                                          
*                                                                               
DLINE20  TM    DLINDS,DLISTOT      TEST ADDING TO SCREEN TOTAL                  
         BZ    DLINE22                                                          
         TM    MIXLIND1,MIXLISCT   TEST IS A SCREEN TOTAL                       
         BZ    DLINE22                                                          
         TM    TLINDS1,TLITOTL     TEST NOT A SUB-TOTAL LINE                    
         BO    DLINE22                                                          
         GOTO1 ASUBTOT,SCPARM,(C'U',0),(X'80',LSPRATA)                          
*                                                                               
DLINE22  TM    DLINDS,DLIUPD       TEST TSAR RECORD UPDATING                    
         BZ    EXIT                                                             
         GOTO1 ACLCLST             TEST TSAR RECORD CHANGED                     
         BE    EXIT                                                             
         GOTO1 ATSARIO,TSAPUT                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPALY (SUB-) TOTAL LINE                                           *         
*                                                                     *         
* NTRY: R1=A(LIST LINE)                                               *         
***********************************************************************         
         SPACE 1                                                                
DISTOT   DS    0H                                                               
         USING *,R8                                                             
         LR    R4,R1               YES - DO WORK-CODE SUB-TOTALS                
         OI    FHATD(R4),FHATPR                                                 
*        OI    FHOID(R4),FHOITR+FHOIMO                                          
         OI    FHOID(R4),FHOITR                                                 
         LA    R2,LSLIN                                                         
         USING LINTABD,R2          R2=A(LINE TABLE)                             
         NI    SCINDS1,FF-SCIDTKEY                                              
*                                                                               
         TM    LSSTAT1,LSSWCT                                                   
         BO    DTOT02                                                           
         OC    TLDA,TLDA           TEST HAVE DISK ADDRESS                       
         BZ    DTOT02                                                           
         GOTO1 AGETREC,0           GET THE RECORD                               
*                                                                               
DTOT02   CLI   LINTABD,EOT         TEST END-OF-TABLE                            
         BE    DTOT20                                                           
         ST    R2,ALINNTRY                                                      
         L     R3,ACLMTAB                                                       
         AH    R3,LINCLM                                                        
         USING CLMTABD,R3          R3=A(COLUMN TABLE ENTRY)                     
         ST    R3,ACLMDATA                                                      
         XR    RE,RE                                                            
         IC    RE,LINHDR                                                        
         LA    R6,0(RE,R4)         R6=A(FIELD HEADER FOR OUTPUT)                
         USING FHD,R6                                                           
*                                                                               
         TM    LSSTAT1,LSSWCT      TEST WORK-CODE SUB-TOTAL                     
         BZ    DTOT04                                                           
         TM    SCINDS1,SCIDTKEY    TEST 1ST KEY FIELD DONE                      
         BNZ   DTOT04                                                           
         MVC   FVIFLD,BCSPACES     NO - COPY DESCRIPTION                        
         MVC   FVIFLD(L'TLTDSC),TLTDSC                                          
         B     DTOT10                                                           
*                                                                               
DTOT04   TM    CLMINDS1,CLMITOT    KEY MAY BE TOTAL FIELD                       
         BO    DTOT06                                                           
         TM    CLMINDS2,CLMIPROR                                                
         BO    DTOT06                                                           
*        TM    CLMINDS1,CLMIKEY                                                 
*        BO    DTOT18                                                           
         B     DTOT18                                                           
*                                                                               
DTOT06   MVC   FVIFLD,BCSPACES     CLEAR FIELD FOR OUTPUT                       
         TM    LSSTAT1,LSSWCT      TEST WORK-CODE SUB-TOTAL                     
         BZ    DTOT08                                                           
         TM    CLMINDS2,CLMIPROR   TEST PRORATA FIELD                           
         BZ    DTOT08                                                           
         GOTO1 ASUBTOT,SCPARM,(C'D',0)                                          
         B     DTOT10                                                           
*                                                                               
DTOT08   MVC   FVIHDR,FHD                                                       
         L     RF,ODISTOT                                                       
         TM    CLMRTN,CLMRGEN      TEST GENERAL COLUMN                          
         BZ    *+8                                                              
         L     RF,ADISGEN                                                       
         XR    R1,R1                                                            
         IC    R1,CLMRTN                                                        
         GOTO1 (RF),(R1)           CALL DISPLAY TOTAL ROUTINE                   
         MVC   FHD(FHDAD),FVIHDR                                                
*                                                                               
DTOT10   OI    FHOI,FHOITR                                                      
         OI    FHAT,FHATPR+FHATHI                                               
         XR    RE,RE                                                            
         IC    RE,LINDSP                                                        
         LA    RF,FHDA(RE)                                                      
         IC    RE,CLMFWDTH                                                      
         TM    CLMINDS1,CLMITOT    TEST TOTAL VALID FOR COLUMN                  
         BO    DTOT14                                                           
         TM    CLMINDS2,CLMIPROR                                                
         BO    DTOT14                                                           
         TM    CLMINDS1,CLMIKEY    TEST KEY FIELD                               
         BO    DTOT12                                                           
         B     DTOT18                                                           
DTOT12   OI    SCINDS1,SCIDTKEY                                                 
         IC    RE,FHLN                                                          
         SH    RE,=Y(FHDAD)                                                     
DTOT14   BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD                                                   
*                                                                               
DTOT18   LA    R2,LINTABL(R2)      BUMP TO NEXT LINE TABLE ENTRY                
         B     DTOT02                                                           
*                                                                               
DTOT20   B     EXIT                                                             
         SPACE 1                                                                
         DROP  R6,R2,R3                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SCREEN TOTALS                                    *         
***********************************************************************         
         SPACE 1                                                                
DISSCR   DS    0H                                                               
         USING *,R8                                                             
         GOTO1 ASAVLST                                                          
         TM    MIXLIND1,MIXLISCT   TEST SCREEN TOTAL                            
         BZ    DSCR01                                                           
         XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKSSTOT                                                  
         GOTO1 ATSARIO,TSARDH                                                   
         BE    DSCR00                                                           
         XC    TLKEY,TLKEY         ADD IF NOT BEEN ADDED                        
         MVI   TLKSES,TLKSSTOT                                                  
         LA    R0,TSAADD                                                        
         GOTO1 ASUBTOT,SCPARM,(C'I',0)                                          
         GOTO1 ATSARIO,TSAADD                                                   
DSCR00   LH    RF,CS1STFTL         OUTPUT SCREEN TOTAL DESCRIPTION              
         LA    RF,TWAD(RF)         RF=A(FIRST FOOTLINE)                         
         USING FOOTLIND,RF                                                      
         MVC   FOOTLIN+L'SUBINP+1(L'TLTDSC),TLTDSC                              
         DROP  RF                                                               
*                                                                               
DSCR01   LA    R2,LSLIN                                                         
         USING LINTABD,R2          R2=A(LINE TABLE)                             
*                                                                               
DSCR02   CLI   LINTABD,EOT         TEST END-OF-TABLE                            
         BE    DSCR20                                                           
         ST    R2,ALINNTRY                                                      
         L     R3,ACLMTAB                                                       
         AH    R3,LINCLM                                                        
         USING CLMTABD,R3          R3=A(COLUMN TABLE ENTRY)                     
         ST    R3,ACLMDATA                                                      
         TM    CLMINDS1,CLMISCR                                                 
         BO    *+12                                                             
         TM    CLMINDS2,CLMIPROR                                                
         BZ    DSCR18                                                           
*                                                                               
         LH    R4,CS1STFTL                                                      
         LA    R4,TWAD(R4)         R4=A(FIRST FOOTLINE)                         
         USING FOOTLIND,R4                                                      
         MVI   LSFOOT#,0           INITIALIZE FOOTLINE NUMBER                   
*                                                                               
DSCR04   CLI   FOOTLINX,PFKFLDNO   TEST PFK LINE                                
         BE    DSCR18                                                           
         OI    FOOTLINH+FHOID,FHOITR                                            
*                                                                               
         MVC   FVIFLD,BCSPACES     CLEAR FIELD FOR OUTPUT                       
         TM    CLMINDS2,CLMIPROR   TEST PRORATA FIELD                           
         BZ    DSCR06                                                           
         GOTO1 ASUBTOT,SCPARM,(C'D',0)                                          
         B     DSCR10                                                           
DSCR06   XR    R1,R1                                                            
         IC    R1,CLMRTN                                                        
         TM    CLMRTN,CLMRGEN                                                   
         BZ    DSCR08                                                           
         GOTO1 ADISGEN,(R1)                                                     
         B     DSCR10                                                           
DSCR08   GOTO1 ODISSCR,(R1)        CALL DISPLAY TOTAL ROUTINE                   
*                                                                               
DSCR10   XR    RF,RF                                                            
         IC    RF,LINFCOL                                                       
         LA    RF,FOOTLIN-1(RF)                                                 
         IC    RE,CLMFWDTH                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD                                                   
         TM    CLMINDS2,CLMIPROR                                                
         BO    DSCR18                                                           
         TM    CLMRTN,CLMRGEN                                                   
         BO    DSCR18                                                           
*                                                                               
         LA    R4,FOOTLINL(R4)                                                  
         IC    RE,LSFOOT#          UPDATE FOOTLINE NUMBER                       
         LA    RE,1(RE)                                                         
         STC   RE,LSFOOT#                                                       
         B     DSCR04                                                           
*                                                                               
DSCR18   LA    R2,LINTABL(R2)      BUMP TO NEXT LINE TABLE ENTRY                
         B     DSCR02                                                           
*                                                                               
DSCR20   GOTO1 ARESLST                                                          
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD THE LIST                                           *         
*                                                                     *         
* NTRY: R1 = 0 TO BUILD BEGINNING OF LIST                             *         
*          = 1 TO EXTEND LIST BY A PAGE                               *         
***********************************************************************         
         SPACE 1                                                                
BLDLST   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         LTR   R1,R1                                                            
         BNZ   BLST02                                                           
         GOTO1 CLRLST                                                           
         GOTO1 OLSTFRST                                                         
         NI    LSINDS2,FF-LSIFNXTP SET NOT FIRST FOR NEXT PAGE                  
         B     BLST10                                                           
*                                                                               
BLST02   MVC   IOKEY(L'LSIOKEY),LSIOKEY                                         
         OI    LSINDS2,LSIFNXTP    SET FIRST FOR NEXT PAGE                      
*                                                                               
BLST10   XR    R3,R3                                                            
         IC    R3,CSLSTMAX                                                      
         AH    R3,LSSEQ            R3 = SEQUENCE NUMBER WHEN PAGE FULL          
         OI    LSINDS1,LSIBLST                                                  
         XR    R1,R1                                                            
         B     *+8                                                              
BLST12   LA    R1,1                                                             
         GOTO1 ADDLST                                                           
         BH    BLDLSTY                                                          
         BL    BLDLSTN                                                          
         TM    MIXLIND2,MIXLIBPG   TEST LIST BUILT PAGE BY PAGE                 
         BZ    BLST12                                                           
         CH    R3,LSSEQ            TEST ADDED 1 PAGE + 1 LINE YET               
         BNL   BLST12                                                           
*                                                                               
BLDLSTY  NI    LSINDS1,FF-LSIBLST                                               
         B     EXITY                                                            
*                                                                               
BLDLSTN  GOTO1 CLRLST                                                           
         MVC   FVMSGNO,=AL2(AE$TMIIL)  TOO MANY ITEMS IN LIST                   
         LA    RE,BASOPTH                                                       
         ST    RE,FVADDR                                                        
         OI    CSLTINDS,CSLTIFST                                                
         XR    R1,R1               BUILD SCREEN IF 1ST TIME ERROR               
         CLI   LSCLMN,0                                                         
         BE    *+8                                                              
         LA    R1,RIGHTQ                                                        
         GOTO1 ABLDSCR                                                          
         B     EXITN                                                            
         SPACE 1                                                                
***********************************************************************         
* CLEAR THE CURRENT LIST                                              *         
***********************************************************************         
         SPACE 1                                                                
CLRLST   NTR1  ,                                                                
         MVI   CSLTINDS,0                                                       
         MVI   CSLSTNUM,0                                                       
         XC    LSSEQ,LSSEQ                                                      
*                                                                               
CLST02   CLC   BCTSHIGH,CSPSRECN   DELETE EXTRANEOUS LIST RECORDS               
         BE    CLST04                                                           
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   TLNUM,BCTSHIGH                                                   
         GOTO1 ATSARIO,TSAGET                                                   
         GOTO1 (RF),TSADEL                                                      
         B     CLST02                                                           
*                                                                               
CLST04   CLC   CSHIRECN,CSPSRECN                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ADD ITEM TO LIST                                                    *         
*                                                                     *         
* NTRY: R1 = 0 TO GET FIRST ITEM                                      *         
*       R1 = 1 TO GET NEXT ITEM                                       *         
* EXIT: CC = HIGH IF END-OF-LIST                                      *         
*       CC = LOW IF LIST TOO BIG                                      *         
***********************************************************************         
         SPACE 1                                                                
ADDLST   NTR1  ,                                                                
         XC    TLST1,TLST1         CLEAR TSAR RECORD                            
         XC    TLST2,TLST2                                                      
         L     RF,OGETFRST                                                      
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         L     RF,OGETNEXT                                                      
         GOTO1 (RF)                                                             
         BNE   ADDLSTN                                                          
         MVC   LSIOKEY,IOKEY                                                    
         OC    TLRLEN,TLRLEN       SET RECORD LENGTH (IF NOT SET)               
         BNZ   *+10                                                             
         MVC   TLRLEN,MIXLRECL                                                  
         MVC   TLKSES,TWASESNL     COPY SESSION NESTING LEVEL                   
         LH    RF,LSSEQ                                                         
         LA    RF,1(RF)                                                         
         STH   RF,LSSEQ                                                         
         STCM  RF,3,TLKSEQ         SET SEQUENCE NUMBER                          
         TM    MIXLIND1,MIXLINDA   TEST NO DISK ADDRESS                         
         BO    ALST02              COPY D/A AND STATUS AREA                     
         MVC   TLDA,IOKEY+(TRNKDA-TRNRECD)                                      
         MVC   TLSTA,IOKEY+(TRNKSTA-TRNRECD)                                    
ALST02   MVC   TLRECACT,CSRECACT   COPY RECORD/ACTION                           
         GOTO1 ATSARIO,TSAADD-X'80' ADD TSAR RECORD                             
         BNE   EXITL                                                            
*                                                                               
         TM    LSSTAT1,LSSWCT      TEST FOR WORK-CODE SUB-TOTALS                
         BZ    ADDLSTY                                                          
         GOTO1 ASUBTOT,SCPARM,(C'U',0),LSPRATA                                  
*                                                                               
ADDLSTY  B     EXITY                                                            
*                                                                               
ADDLSTN  OI    CSLTINDS,CSLTIEOF                                                
         B     EXITH                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET CURRENT LIST RECORD                                  *         
*                                                                     *         
* NTRY: R1 = IOLOCK TO READ FOR UPDATE ELSE 0                         *         
***********************************************************************         
         SPACE 1                                                                
GETREC   DS    0H                                                               
         USING *,R8                                                             
         TM    MIXLIND1,MIXLINDA   TEST NO DISK ADDRESS                         
         BO    EXIT                                                             
         STC   R1,SCBYTE                                                        
         NI    LSINDS1,FF-LSIUPREC-LSIWOPTA-LSIUPPTA                            
         MVC   IODAOVER,TLDA                                                    
         GOTO1 AIO,IOGET+IOACCMST+IO1(R1)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         MVC   TLSTA,TRNRSTA-TRNRECD(R2)                                        
*                                                                               
         TM    MIXLIND1,MIXLITRN   TEST GENERAL TRANSACTION LIST                
         BZ    GETRECX                                                          
         GOTO1 AGETOPT,SCPARM,AIO1                                              
         XC    SCPARM,SCPARM                                                    
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR   TEST AGENCY CURRENCY BILLING                 
         BE    GETR06                                                           
         CLC   BCCPYSEC,CSBILCUR   TEST SECOND CURRENCY BILLING                 
         BE    GETR06                                                           
         LA    R0,CSEXCVAL         EXCHANGE RULE                                
GETR06   GOTO1 APRORATA,SCPARM,AIO1,AGOPBLK,ACOM,(R0),LSPRATA,0                 
         LA    RE,LSPRATAS         COPY BLOCK                                   
         LA    RF,PR$LNQ                                                        
         LA    R0,LSPRATA                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         GOTO1 ASETTRN,SCPARM,(C'R',AIO1)                                       
         TM    TLXPEND,TLXPWOF     TEST WRITE-0FF PENDING                       
         BZ    GETRECX                                                          
         GOTO1 AGETPTA,SCPARM,AIO1,AIO5,LSPRATA,0                               
*                                                                               
GETRECX  B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO WRITE BACK CURRENT LIST RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
PUTREC   DS    0H                                                               
         USING *,R8                                                             
         NI    LSINDS1,FF-LSIUPREC                                              
         TM    MIXLIND1,MIXLINDA   TEST NO DISK ADDRESS                         
         BO    PUTRECY                                                          
         TM    MIXLIND1,MIXLITRN   TEST GENERAL TRANSACTION LIST                
         BZ    PREC10                                                           
         TM    LSINDS1,LSIUPPTA    TEST VALID TO UPDATE PTA RECORD              
         BZ    PREC02                                                           
         GOTO1 APUTPTA,SCPARM,(C'T',AIO1),AIO5                                  
         BNL   PREC02                                                           
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITN                                                            
PREC02   TM    MIXLIND1,MIXLIJOB   TEST UPDATING JOB RECORD                     
         BZ    PREC04                                                           
         GOTO1 AUPDJOB,SCPARM,(C'U',AIO1),AIO4,LSPRATAS,LSPRATA                 
         BNE   EXITN                                                            
PREC04   TM    LSINDS1,LSIUPPTA    TEST PTA RECORD UPDATE                       
         BZ    PREC06                                                           
         GOTO1 APUTPTA,SCPARM,AIO1,AIO5                                         
         BNL   *+6                                                              
         DC    H'0'                                                             
PREC06   GOTO1 ASETTRN,SCPARM,(C'R',AIO1)                                       
*                                                                               
PREC10   TM    LSINDS2,LSTOBACC    TEST RECORD IS CONVERTED                     
         BNO   PREC12                                                           
         NI    LSINDS2,FF-LSTOBACC                                              
         GOTO1 VTOBACCO,SCPARM,('TOBAACVB',TOBCUR),AIO1,ACOM,0,0                
PREC12   GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1             TEST CHANGE IN STATUS OF RECORD              
         USING TRNRECD,R2                                                       
         CLC   TLSTA,TRNRSTA                                                    
         BE    PUTRECY                                                          
DIR      USING TRNRECD,IOKEY       UPDATE DIRECTORY RECORD                      
         MVC   DIR.TRNKEY,TRNKEY                                                
         GOTO1 AIO,IOACCDIR+IORDUPD                                             
         BE    *+14                                                             
         CLI   IOERR,IOEDEL                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DIR.TRNKSTA,TRNRSTA                                              
         GOTO1 AIO,IOACCDIR+IOWRITE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TLSTA,TRNRSTA                                                    
PUTRECY  OI    LSINDS1,LSILINUP                                                 
         B     EXITY                                                            
         DROP  R2,DIR                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST FOR SCREEN                                                    *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST  DS    0H                                                               
         USING *,R8                                                             
         NI    LSINDS1,FF-LSILINUP-LSILINER                                     
         NI    LSSTAT1,FF-LSSTOT-LSSWCT                                         
         TM    MIXLIND2,MIXLISUB                                                
         BZ    *+8                                                              
         OI    LSSTAT1,LSSTOT                                                   
*                                                                               
         TM    MIXLIND1,MIXLIJOB   TEST UPDATING JOB                            
         BZ    SFRST02                                                          
         MVC   IODAOVER,BCJOBDA                                                 
         LH    R1,=Y(IO4)                                                       
         GOTO1 AIO,IOGETRUP+IOACCMST(R1)                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SFRST02  GOTO1 OSCRFRST                                                         
         IPM   RE                  SAVE CONDITION CODE                          
         TM    MIXLIND1,MIXLITRN   TEST GENERAL TRANSACTION LIST                
         BZ    SFRSTX                                                           
         CLI   LSWCTOT,C'Y'        TEST FOR WORK-CODE SUB-TOTALS                
         BNE   SFRSTX                                                           
         OI    LSSTAT1,LSSTOT+LSSWCT                                            
SFRSTX   SPM   RE                  RESTORE CONDITION CODE                       
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* LAST FOR SCREEN                                                     *         
*                                                                     *         
* NTRY: R1 = ZERO IF NOT REQUIRED TO UPDATE SCREEN                    *         
*       R1 = NON-ZERO TO UPDATE THE SCREEN                            *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST  DS    0H                                                               
         USING *,R8                                                             
         LTR   R1,R1                                                            
         BZ    SLAST20                                                          
         CLC   CSHIRECN,CSPSRECN   TEST ANY RECORDS FOUND                       
         BE    SLAST10                                                          
         XR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NUMBER OF LINES TO DISPLAY                
         BZ    SLAST10                                                          
         TM    LSSTAT1,LSSTOT      TEST FOR SUB-TOTAL LINES                     
         BZ    SLAST10                                                          
*                                                                               
         L     R2,ATWA                                                          
         AH    R2,CS1STLIN                                                      
         USING SUBINPH,R2          R2=A(SCREEN LINE)                            
         MVC   TLNUM,CSPAG#LO                                                   
SLAST02  GOTO1 ATSARIO,TSAGET                                                   
         TM    TLINDS1,TLITOTL                                                  
         BZ    SLAST08                                                          
* ZZ     TM    MIXLIND1,MIXLINSA   TEST NO SUB-ACTION                           
* ZZ     BO    *+8                                                              
* ZZ     NI    SUBINPH+FHOID,FF-FHOIMO                                          
         GOTO1 ADISTOT,SUBINPH                                                  
SLAST08  ICM   RF,3,TLNUM                                                       
         LA    RF,1(RF)                                                         
         STCM  RF,3,TLNUM                                                       
         AH    R2,CSLINLEN                                                      
         BCT   R0,SLAST02                                                       
         DROP  R2                                                               
*                                                                               
SLAST10  GOTO1 ADISSCR             SCREEN TOTAL ROUTINES                        
         LH    R1,CS1STFTL                                                      
         LA    R1,TWAD(R1)                                                      
         LR    R2,R1                                                            
         USING FHD,R2              TRANSMIT FOOTLINES                           
         XR    RF,RF                                                            
         ICM   RF,1,FHLN                                                        
         BZ    *+12                                                             
         OI    FHOI,FHOITR                                                      
         BXH   R2,RF,*-12                                                       
         DROP  R2                                                               
*                                                                               
         TM    MIXLIND1,MIXLITRN   TEST TRANSACTION LIST                        
         BZ    SLAST20                                                          
         CLI   FVOMTYP,GTMINF      TEST INFO MESSAGE                            
         BNE   SLAST20                                                          
         CLI   LSSWITCH,C'Y'       TEST SWITCH=Y                                
         BNE   SLAST20                                                          
         CLC   FVMSGNO,=AL2(AI$DISNX)  CHANGE '...HIT ENTER...'                 
         BNE   *+14                INTO '...HIT PF8...'                         
         MVC   FVMSGNO,=AL2(AI$RDPF8)                                           
         B     SLAST20                                                          
         CLC   FVMSGNO,=AL2(AI$EOLEF)                                           
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(AI$ELPF8)                                           
*                                                                               
SLAST20  TM    LSINDS1,LSILINUP    TEST ANY LINES UPDATED                       
         BZ    SLAST22                                                          
         TM    MIXLIND1,MIXLIJOB   TEST UPDATING JOB RECORD                     
         BZ    SLAST22                                                          
         LH    R1,=Y(IO4)                                                       
         GOTO1 AIO,IOPUT+IOACCMST(R1)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SLAST22  TM    LSINDS2,LSIXMIT     TEST RE-TRANSMIT SCREEN                      
         BZ    SLAST30                                                          
         XR    RF,RF                                                            
         LA    R1,TWASCR                                                        
         USING FHD,R1                                                           
SLAST24  ICM   RF,1,FHLN                                                        
         BZ    SLAST26                                                          
         CLI   FHAT,FHATNP                                                      
         BE    *+8                                                              
         OI    FHOI,FHOITR                                                      
         BXH   R1,RF,SLAST24                                                    
SLAST26  MVC   FHD(L'BSEOS),BSEOS  SET END-OF-SCREEN BITS                       
         DROP  R1                                                               
*                                                                               
SLAST30  LH    R1,CS1STFTL         CALL APPLICATION ROUTINE                     
         GOTO1 OSCRLAST,TWAD(R1)                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITALIZE VALUES FOR NEW SCREEN AND BUILD LIST           *         
***********************************************************************         
         SPACE 1                                                                
INISCR   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         MVI   LSSTAT1,0                                                        
         MVI   LSSTAT2,0                                                        
*                                                                               
         CLI   CUCTRY,CTRYUSA      TEST LISTING ORDER RECORDS                   
         BE    ISCR06                                                           
         CLI   P#DISORD,C'N'                                                    
         BE    ISCR06                                                           
         MVC   SCMASK,=AL2(OPTFORD)                                             
         NC    SCMASK,MIXLOPTF                                                  
         BZ    ISCR06                                                           
         OI    LSSTAT1,LSSORDER                                                 
*                                                                               
ISCR06   LA    R4,BASOLAYH         R4=1ST HEADLINE TWA ADDRESS                  
         LA    R3,3*NUMCOLSQ       R3=1ST HEADLINE DATA SCREEN ADDRESS          
         LA    R2,3                R2=ROW NUMBER OF 1ST HEADLINE                
*                                                                               
         MVI   TWASCRN,0           SET NO SCREEN LOADED                         
         CLI   CSSCRN,0            TEST NEED TO LOAD A SCREEN                   
         BE    ISCR10                                                           
         GOTO1 AOVRSCR,SCPARM,(CSSCRN,(R4))                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         XR    RF,RF               BUMP R4 TO END OF SCREEN                     
         ICM   RF,1,FHLND(R4)                                                   
         BZ    *+12                                                             
         ICM   R3,3,FHADD(R4)                                                   
         BXH   R4,RF,*-12                                                       
         XR    R2,R2                                                            
         LA    RE,NUMCOLSQ                                                      
         DR    R2,RE                                                            
         LA    R2,1(R3)            R2=ROW NUMBER OF 1ST HEADLINE                
         MH    R3,=Y(NUMCOLSQ)                                                  
         LA    R3,NUMCOLSQ(R3)     R3=1ST HEADLINE DATA SCREEN ADDRESS          
*                                                                               
ISCR10   STH   R3,CSHEDAD          SAVE DATA SCREEN ADDRESS OF 1ST ROW          
         S     R4,ATWA             SAVE TWA DISPLACEMENT                        
         STH   R4,CSHEDDSP                                                      
         LA    RF,NUMROWSQ                                                      
         SR    RF,R2               RF=LINES LEFT ON SCREEN                      
*                                                                               
         XR    R0,R0                                                            
         IC    R0,MIXLNFTL         R0=NUMBER OF FOOTLINES                       
         SR    RF,R0               RF=NO. OF LIST LINES+HEADING LINES           
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         STC   RE,CSLSTMAX         SAVE NO. OF LIST LINES                       
*                                                                               
         LA    R1,OSVALS-9-TWAD                                                 
         SH    R1,CSHEDDSP                                                      
         MH    R0,=Y(FOOTLINL)                                                  
         SR    R1,R0               R1=MEMORY AVAILABLE FOR LIST SCREEN          
         STH   R1,LSMAXSCR                                                      
         XR    R0,R0                                                            
         DR    R0,RF                                                            
         STH   R1,LSMAXLEN         SAVE MAX. MEMORY AVAILABLE PER LINE          
         CLI   LSMAXLEN,0                                                       
         BE    *+12                                                             
         MVI   LSMAXLEN,0                                                       
         MVI   LSMAXLEN+1,FF                                                    
*                                                                               
         XC    LSLIN(LSLINS),LSLIN INITIALIZE LINE TABLE FOR KEY COLS.          
         XC    ALPARM,ALPARM                                                    
         MVI   ALCURCOL,1                                                       
         TM    MIXLIND1,MIXLINSA   TEST NO SUB-ACTION                           
         BO    ISCR11                                                           
         MVI   ALCURCOL,L'SUBINP+2 SET 1ST COLUMN POSITION                      
         MVC   ALCURLEN,=Y(SUBINPL)                                             
ISCR11   OI    ALINDS,ALINEWF      SET NEW FIELD FLAG                           
         XC    ACLMDATA,ACLMDATA                                                
ISCR12   GOTO1 ASETCLM,0                                                        
         BL    ISCR16                                                           
         L     R3,ACLMDATA                                                      
         USING CLMTABD,R3          R3=A(COLUMN TABLE ENTRY)                     
         TM    CLMINDS1,CLMIKEY    TEST FOR KEY TYPE COLUMN                     
         BZ    ISCR12                                                           
         XR    RF,RF                                                            
         TM    CLMINDS1,CLMIPRO    TEST OPEN FIELD REQUIRED                     
         BO    *+8                                                              
         LA    RF,X'80'                                                         
         GOTO1 AADDLIN,SCPARM,((RF),(R3))                                       
         B     ISCR12                                                           
         DROP  R3                                                               
*                                                                               
ISCR16   MVC   LSRECCOL,ALCURCOL   SAVE COLUMN OF 1ST RECORD FIELD              
         MVC   LSKEYLEN,ALCURLEN   SAVE LENGTH OF KEY FIELDS                    
         LA    RE,LSLIN                                                         
         ICM   R4,15,ALALIN                                                     
         BZ    ISCR18                                                           
         SR    R4,RE                                                            
         SRA   R4,3                                                             
ISCR18   STC   R4,LSNUMKEY         SAVE NUMBER OF KEY FIELDS                    
*                                                                               
INISCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD LIST SCREEN                                        *         
*                                                                     *         
* NTRY: R1 = LEFTQ/RIGHTQ + STARTING PLACE IN DIS= LIST               *         
*       R1 = 0 TO BUILD DUMMY INITIAL SCREEN                          *         
***********************************************************************         
         SPACE 1                                                                
BLDSCR   DS    0H                                                               
         USING *,R8                                                             
         STC   R1,BSWAY            SAVE DIRECTION (LEFT/RIGHT)                  
         STC   R1,BSDIS1           SAVE STARTING COLUMN                         
         NI    BSDIS1,FF-LEFTQ-RIGHTQ                                           
*                                                                               
         MVC   BSLHSRHS,CSLHSRHS   SAVE CURRENT LHS AND RHS                     
         CLI   BSWAY,0                                                          
         BE    BFMT10                                                           
         SPACE 1                                                                
***********************************************************************         
*        -     ADD RECORD TYPE COLUMNS TO LINE TABLE                  *         
***********************************************************************         
         SPACE 1                                                                
         XC    ALPARM,ALPARM       SET UP ADDLIN PARAMETER LIST                 
         MVC   ALCURCOL,LSRECCOL                                                
         MVC   ALCURLEN,LSKEYLEN                                                
         XR    RF,RF                                                            
         IC    RF,LSNUMKEY                                                      
         SLL   RF,3                                                             
         LA    RF,LSLIN(RF)                                                     
         ST    RF,ALALIN                                                        
         ST    RF,BSALINR1         SAVE A(1ST RECORD TYPE COLUMN)               
         LA    RE,LSLINX           CLEAR REST OF LINE TABLE                     
         SR    RE,RF                                                            
         EX    RE,*+4                                                           
         XC    0(0,RF),0(RF)                                                    
*                                                                               
         XR    RF,RF                                                            
         IC    RF,BSDIS1           RF=DISP. TO FIRST DIS= COLUMN                
         LA    R6,0(RF,RF)                                                      
         LA    R6,LSCLMLST(R6)     R6=A(FIRST DIS= CHARACTER)                   
         LA    R0,1(RF)            R0=NO. OF LEFTWARD COLUMNS                   
         LH    R2,=Y(-2)           R2=-2 TO GO BACKWARDS                        
         TM    BSWAY,RIGHTQ        TEST GOING RIGHT                             
         BZ    BLIN02                                                           
         IC    R0,LSCLMN                                                        
         SR    R0,RF               R0=NO. OF RIGHTWARD COLUMNS                  
         LA    R2,2                R2=+2 TO GO FORWARDS                         
*                                                                               
BLIN02   GOTO1 ASETCLM,SCPARM,(R6)                                              
         BNL   *+6                                                              
         DC    H'0'                                                             
         XR    RF,RF                                                            
         TM    1(R6),LSCLMPRO      TEST USER REQUIRES OPEN COLUMN               
         BO    *+8                                                              
         LA    RF,X'80'                                                         
         GOTO1 AADDLIN,SCPARM,((RF),ACLMDATA)                                   
         BNE   BLIN10                                                           
         AR    R6,R2                                                            
         BCT   R0,BLIN02                                                        
*                                                                               
BLIN10   L     R2,ALALIN                                                        
         LA    RF,LINTABL          RF=L(LINTAB ENTRY)                           
         SR    R2,RF               R2=A(LAST COLUMN)                            
         ST    R2,BSALINRX                                                      
         TM    BSWAY,RIGHTQ        TEST NEED TO REVERSE ORDER                   
         BO    BLDLINX                                                          
*                                                                               
         L     R3,BSALINR1         R3=A(1ST COLUMN)                             
         IC    R0,ALCURCOL         R0=END COLUMN POSITION                       
         IC    R1,LINHCOL-LINTABD(R3)  R1=START COLUMN POSITION                 
BLIN12   CR    R3,R2               TEST DONE                                    
         BL    BLIN14                                                           
         BH    *+8                                                              
         STC   R1,LINHCOL-LINTABD(R3)                                           
         B     BLDLINX                                                          
BLIN14   XC    0(LINTABL,R3),0(R2) SWAP ENTRIES                                 
         XC    0(LINTABL,R2),0(R3)                                              
         XC    0(LINTABL,R3),0(R2)                                              
         STC   R1,LINHCOL-LINTABD(R3) SET NEW START                             
         IC    RE,LINFCOL-LINTABD(R3)                                           
         LA    R1,1(RE,R1)         SET START FOR NEXT COLUMN                    
         IC    RE,LINFCOL-LINTABD(R2)                                           
         SR    R0,RE                                                            
         BCTR  R0,0                                                             
         STC   R0,LINHCOL-LINTABD(R2)  SET START OF END COLUMN                  
         SR    R2,RF               BUMP R2 BACK A COLUMN                        
         BXH   R3,RF,BLIN12        BUMP R3 FORWARD                              
*                                                                               
BLDLINX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*        -     FORMAT AND BUILD THE LIST LINE                         *         
***********************************************************************         
         SPACE 1                                                                
BLDFMT   MVC   BSLIN,LSLIN         SAVE LINE TABLE                              
*                                                                               
         L     R4,BSALINRX         EVENLY SPREAD COLUMNS                        
         USING LINTABD,R4          R4=A(LINTAB ENTRY FOR LAST COLUMN)           
         L     R3,ACLMTAB                                                       
         AH    R3,LINCLM                                                        
         USING CLMTABD,R3          R3=A(COLUMN TABLE ENTRY)                     
*                                                                               
         XR    RE,RE                                                            
         IC    RE,LINHCOL                                                       
         XR    R1,R1                                                            
         IC    R1,CLMHWDTH                                                      
         AR    RE,R1                                                            
         LA    R1,NUMCOLSQ+1                                                    
         SR    R1,RE               R1=SPARE SPACE AT END OF LINE                
         BZ    BFMT10                                                           
         XR    R0,R0                                                            
         L     R2,BSALINRX                                                      
         S     R2,BSALINR1                                                      
         BZ    BFMT10                                                           
         SRA   R2,3                DIVIDE BY 8 (=LINTABL)                       
         LA    R2,1(R2)            R2=# OF COLUMNS ON PAGE                      
         DR    R0,R2               R1=GAP                                       
         L     R4,BSALINR1                                                      
         XR    R0,R0                                                            
*                                                                               
BFMT02   IC    RE,LINHCOL          SPACE HEADING POSITIONS                      
         AR    RE,R0                                                            
         STC   RE,LINHCOL                                                       
         AR    R0,R1                                                            
         LA    R4,LINTABL(R4)                                                   
         BCT   R2,BFMT02                                                        
         DROP  R3,R4                                                            
*                                                                               
BFMT10   XC    BSLINE,BSLINE                                                    
         XC    BSHEAD1,BSHEAD1                                                  
         XC    BSHEAD2,BSHEAD2                                                  
         XC    BSHEADL,BSHEADL                                                  
         LA    R2,BSLINE           R2=A(LIST LINE)                              
         TM    MIXLIND1,MIXLINSA   TEST NO SUB-ACTION                           
         BO    BFMT11                                                           
         USING SUBINPD,R2          ADD SUB-ACTION INPUT FIELD                   
         MVI   SUBINPH+FHLND,SUBINPL                                            
         MVI   SUBINPH+FHATD,FHATXH                                             
         MVI   SUBINPH+FHADD+1,1                                                
         MVC   SUBINP,BCSPACES                                                  
         PUSH  USING                                                            
         USING FHNU,SUBINPX        SET UP EXTENDED HEADER                       
         MVC   FHNU,MIXLSUB#       SET HELP PANEL NO.                           
*        OC    FHSC,CSSCRN         SET SCREEN NO.                               
*        BNZ   *+8                                                              
         MVI   FHSC,FF                                                          
         POP   USING                                                            
         LA    R2,SUBINPL(R2)                                                   
         USING FHD,R2              R2=A(LIST LINE)                              
BFMT11   LA    R4,LSLIN                                                         
         USING LINTABD,R4          R4=A(LINE TABLE)                             
*                                                                               
BFMT12   CLI   LINTABD,EOT                                                      
         BE    BFMT50                                                           
         L     R3,ACLMTAB                                                       
         AH    R3,LINCLM                                                        
         USING CLMTABD,R3          R3=A(COLUMN TABLE ENTRY)                     
         ST    R3,ACLMDATA                                                      
*                                                                               
         IC    RE,LINHCOL          SET COLUMN POSITION FOR FIELD                
         IC    RF,CLMFDSP                                                       
         AR    RE,RF                                                            
         STC   RE,LINFCOL                                                       
*                                                                               
         TM    LININDS,LINIINP     TEST FOR INPUT FIELD                         
         BZ    BFMT20                                                           
         XR    RE,RE               GET ON TO NEW FIELD                          
         IC    RE,FHLN                                                          
         AR    R2,RE                                                            
*                                                                               
         MVC   FHAT,CLMFFHAT       SET UP INPUT FIELD HEADER                    
         OI    FHAT,FHATXH                                                      
         MVC   FHAD+1(1),LINFCOL                                                
         LA    RF,FHD              SET DISPLACEMENT TO LIST HEADER              
         LA    RE,BSLINE                                                        
         SR    RF,RE                                                            
         STC   RF,LINHDR                                                        
         XR    RE,RE                                                            
         IC    RE,CLMFWDTH                                                      
         LA    RF,FHDAD+FHDAD(RE)                                               
         STC   RF,FHLN                                                          
         AR    R2,RE               SET UP EXTENDED FIELD HEADER                 
         MVC   FHNU,CLMHLP                                                      
         MVC   FHSC,TWASCRN                                                     
         MVC   FHXA,CLMFFHXA                                                    
         LA    R2,FHDAD+FHDAD(R2)                                               
         MVI   LINDSP,0                                                         
         B     BFMT30                                                           
*                                                                               
BFMT20   CLI   FHLN,0              UNLESS ALREADY DONE                          
         BNE   BFMT22                SET UP PROTECTED FIELD HEADER              
         MVI   FHAT,FHATPR+FHATLC                                               
         MVC   FHAD+1(1),LINFCOL                                                
BFMT22   IC    RF,LINFCOL                                                       
         IC    RE,FHAD+1                                                        
         SR    RF,RE                                                            
         STC   RF,LINDSP           SET DISPLACEMENT WITHIN FIELD                
         IC    RE,CLMFWDTH                                                      
         LA    RF,FHDAD(RE,RF)                                                  
         STC   RF,FHLN             SET NEW FIELD LENGTH                         
         LA    RF,FHD              SET DISPLACEMENT TO LIST HEADER              
         LA    RE,BSLINE                                                        
         SR    RF,RE                                                            
         STC   RF,LINHDR                                                        
*                                                                               
BFMT30   LA    R6,BSHEAD1          ADD HEADLINES                                
         AH    R6,BSHEADL                                                       
         DROP  R2                                                               
         USING FHD,R6                                                           
         XR    RE,RE               SET UP FIELD HEADERS                         
         IC    RE,CLMHWDTH                                                      
         LA    RE,FHDAD(RE)                                                     
         STC   RE,FHLN                                                          
         MVI   FHAT,FHATLC+FHATPR+FHATHI                                        
         MVI   FHAD,0                                                           
         MVC   FHAD+1(1),LINHCOL                                                
         OC    FHIL(1),CLMHFHXT                                                 
         BZ    *+8                                                              
         MVI   FHII,FHIXAT                                                      
         EX    RE,*+4                                                           
         MVC   FHD+L'BSHEAD1(0),FHD                                             
         AH    RE,BSHEADL                                                       
         STH   RE,BSHEADL                                                       
*                                                                               
         MVC   FHDA(L'CLMHEAD1),CLMHEAD1   COPY DATA DICTIONARY ENTRIES         
         MVC   FHDA+L'BSHEAD1(L'CLMHEAD2),CLMHEAD2                              
         CLI   FHDA,C' '           TEST DIRECT TEXT                             
         BH    BFMT40                                                           
         TM    CLMINDS1,CLMIAMT    TEST AMOUNT TYPE COLUMN                      
         BZ    BFMT32                                                           
         TM    LININDS,LINIINP     TEST PROTECTED FIELD                         
         BO    BFMT32                                                           
         ICM   RF,15,=X'01FFFFFF'  RIGHT JUSTIFY HEADLINES                      
         ICM   RE,15,FHDA                                                       
         AR    RE,RF                                                            
         STCM  RE,15,FHDA                                                       
         ICM   RE,15,FHDA+L'BSHEAD1                                             
         AR    RE,RF                                                            
         STCM  RE,15,FHDA+L'BSHEAD1                                             
*                                                                               
BFMT32   TM    CLMHEAD1+L'CLMHEAD1-1,X'80'                                      
         BZ    BFMT36              TEST SPLIT HEADLINE                          
         MVC   BOELEM(L'CLMHEAD1),CLMHEAD1                                      
         GOTO1 VDICTAT,SCPARM,C'SL  ',BOELEM                                    
         XR    RF,RF                                                            
         IC    RF,CLMHWDTH                                                      
         LA    RE,BOELEM(RF)       RE=A(START OF HEADLINE 2)                    
         BCTR  RF,0                                                             
         CLC   0(0,RE),BCSPACES    TEST ANYTHING ON LINE 2                      
         BE    BFMT34                                                           
         EX    RF,*+4                                                           
         MVC   FHDA(0),BOELEM      COPY SPLIT FOR 1                             
         EX    RF,*+4                                                           
         MVC   FHDA+L'BSHEAD1(0),0(RE)   AND 2                                  
         B     BFMT40                                                           
BFMT34   MVC   FHDA(L'CLMHEAD1),CLMHEAD1   SET NORMAL HEADLINE 1                
         MVC   FHDA+L'CLMHEAD1-1(L'CLMHWDTH),CLMHWDTH                           
         MVC   FHDA+L'BSHEAD1(L'CLMHEAD1),FHDA                                  
         OI    FHDA+L'BSHEAD1,X'08'        SET UNDERLINING FOR 2                
*                                                                               
BFMT36   GOTO1 ,SCPARM,C'SL  ',FHDA        CALL DICTATE FOR FIRST               
         CLI   FHDA,0                                                           
         BE    BFMT38                                                           
         CLI   FHDA,C' '                                                        
         BNL   BFMT38                                                           
         GOTO1 VDICTAT                                                          
BFMT38   GOTO1 ,(R1),,FHDA+L'BSHEAD1       CALL DICTATE FOR SECOND              
         CLI   FHDA+L'BSHEAD1,0                                                 
         BE    BFMT40                                                           
         CLI   FHDA+L'BSHEAD1,C' '                                              
         BNL   BFMT40                                                           
         GOTO1 VDICTAT                                                          
*                                                                               
BFMT40   TM    CLMINDS1,CLMIHEAD   TEST OVERLAY CAN OVER-WRITE HEADLINE         
         BZ    BFMT48                                                           
         L     RF,OSETHEAD                                                      
         TM    CLMRTN,CLMRGEN      TEST GENERAL ROUTINE                         
         BO    BFMT42                                                           
         TM    CLMINDS2,CLMIPROR   TEST PRORATA COLUMN                          
         BZ    BFMT44                                                           
         TM    CLMINDS2,CLMIHOUR   TEST CURRENCY AMOUNT (NOT HOURS)             
         BO    BFMT48                                                           
BFMT42   L     RF,ASETHEAD                                                      
BFMT44   GOTO1 (RF),SCPARM,CLMTABD,FHDA,FHDA+L'BSHEAD1                          
         DROP  R6                                                               
*                                                                               
BFMT48   LA    R4,LINTABL(R4)                                                   
         B     BFMT12                                                           
         DROP  R3,R4                                                            
*                                                                               
BFMT50   XR    RE,RE               BUMP R2 TO END-OF-LINE                       
         ICM   RE,1,FHLND(R2)                                                   
         BZ    *+6                                                              
         AR    R2,RE                                                            
         LA    RE,BSLINE                                                        
         SR    R2,RE                                                            
         STH   R2,CSLINLEN         SAVE LENGTH OF LIST LINE                     
*                                                                               
         XR    RF,RF               TEST SCREEN WILL FIT                         
         IC    RF,CSLSTMAX                                                      
         MH    RF,CSLINLEN         RF=MEM. FOR LIST LINES                       
         LH    RE,BSHEADL                                                       
         SLL   RE,1                RE=MEM. FOR HEADLINES                        
         LA    RF,L'BSSUBHED(RE,RF)                                             
         CH    RF,LSMAXSCR                                                      
         BNH   BFMT70                                                           
*                                                                               
         MVC   LSLIN(LSLINS),BSLIN RESTORE LIST TABLE TO AS WAS                 
         TM    BSWAY,LEFTQ         TEST GOING LEFT                              
         BZ    BFMT60                                                           
         L     RF,BSALINR1         1ST COLUMN TO BE REMOVED                     
         USING LINTABD,RF                                                       
         IC    R0,LINHCOL+LINTABL                                               
         IC    RE,LINHCOL                                                       
         SR    R0,RE               R0=SHIFT TO THE LEFT                         
BFMT52   MVC   LINTABD(LINTABL),LINTABD+LINTABL                                 
         IC    RE,LINHCOL                                                       
         SR    RE,R0                                                            
         STC   RE,LINHCOL                                                       
         LA    RF,LINTABL(RF)                                                   
         CLI   LINTABD,EOT                                                      
         BNE   BFMT52                                                           
         DROP  RF                                                               
*                                                                               
BFMT60   L     RF,BSALINRX         REMOVE LAST COLUMN                           
         XC    0(LINTABL,RF),0(RF)                                              
         SH    RF,=Y(LINTABL)                                                   
         ST    RF,BSALINRX                                                      
         B     BLDFMT              RE-FORMAT                                    
*                                                                               
BFMT70   LA    RE,BSHEAD1          ENSURE HEAD-LINES END ON 0                   
         AH    RE,BSHEADL                                                       
         MVI   0(RE),0                                                          
         MVI   L'BSHEAD1(RE),0                                                  
         L     RF,BSALINRX                                                      
         S     RF,BSALINR1                                                      
         SRL   RF,3                RF=NO. OF COLUMNS (-1)                       
         IC    RE,BSDIS1           RE=FIRST COLUMN DISPLAYED                    
         TM    BSWAY,RIGHTQ        TEST RIGHTWARDS                              
         BZ    BFMT72                                                           
         STC   RE,CSLHSDIS                                                      
         AR    RE,RF                                                            
         STC   RE,CSRHSDIS                                                      
         B     BLDFMTX                                                          
BFMT72   STC   RE,CSRHSDIS         WENT LEFTWARDS                               
         SR    RE,RF                                                            
         STC   RE,CSLHSDIS                                                      
*                                                                               
BLDFMTX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*        -     BUILD THE SCREEN                                       *         
***********************************************************************         
         SPACE 1                                                                
         LA    R3,TWAD                                                          
         AH    R3,CSHEDDSP                                                      
         USING FHD,R3              R3=A(FIRST HEAD LINE IN TWA)                 
         LH    R2,CSHEDAD          R2=ROW SCREEN DATA ADDRESS                   
         LA    R4,BSHEAD1          ADD FIRST HEAD-LINE                          
         BAS   RE,COPYROW                                                       
*                                                                               
         MVC   FHD(L'BSSUBHED),BSSUBHED                                         
         LA    RE,1(R2)            ADD SUB-ACTION HEADLINE                      
         STCM  RE,3,FHAD                                                        
         LA    R3,L'BSSUBHED(R3)                                                
*                                                                               
         LA    R4,BSHEAD2          ADD SECOND HEADLINE                          
         BAS   RE,COPYROW                                                       
*                                                                               
         S     R3,ATWA             SAVE DISP. TO FIRST LINE                     
         STH   R3,CS1STLIN                                                      
         A     R3,ATWA                                                          
*                                                                               
         XR    R0,R0               ADD LIST LINES                               
         IC    R0,CSLSTMAX                                                      
         LA    R4,BSLINE                                                        
         BAS   RE,COPYROW                                                       
         BCT   R0,*-8                                                           
*                                                                               
         S     R3,ATWA             SAVE DISP. TO FIRST FOOTLINE                 
         STH   R3,CS1STFTL                                                      
         A     R3,ATWA                                                          
*                                                                               
         MVC   LSDEFCUR,CS1STLIN   DEFAULT CURSOR = 1ST SUB-ACTION              
         TM    MIXLIND1,MIXLINSA   UNLESS THERE ISN'T ONE                       
         BZ    BSCR10                                                           
         LA    RF,LSLIN            FIND FIRST INPUT FIELD ON LINE               
         USING LINTABD,RF                                                       
BSCR02   CLI   LINTABD,EOT                                                      
         BE    BSCR04                                                           
         TM    LININDS,LINIINP                                                  
         BO    *+12                                                             
         LA    RF,LINTABL(RF)                                                   
         B     BSCR02                                                           
         XR    RE,RE               DEFAULT CURS = 1ST INPUT ON 1ST LINE         
         IC    RE,LINHDR                                                        
         AH    RE,CS1STLIN                                                      
         STH   RE,LSDEFCUR                                                      
         DROP  RF                                                               
         B     BSCR10                                                           
BSCR04   LA    R1,BASOLAYH         FIND FIRST INPUT FIELD                       
         LH    RF,CSHEDDSP                                                      
         LA    RF,TWAD(RF)                                                      
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
         TM    FHATD(R1),FHATPR                                                 
         BZ    BSCR06                                                           
         IC    RE,FHLND(R1)                                                     
         BXLE  R1,RE,*-12                                                       
         MVC   LSDEFCUR,=AL2(BASOPTH-TWAD) NO - SET TO OPTION FIELD             
         B     BSCR10                                                           
BSCR06   S     R1,ATWA                                                          
         STH   R1,LSDEFCUR                                                      
*                                                                               
BSCR10   XC    BSFOOT,BSFOOT                                                    
         MVI   BSFOOT+FHLND,FOOTLINL                                            
         MVI   BSFOOT+FHATD,FHATLC+FHATPR+FHATHI+FHATXH                         
         MVI   BSFOOT+FHADD+1,1                                                 
         IC    R0,MIXLNFTL                                                      
BSCR12   LA    R4,BSFOOT           ADD FOOTLINES                                
         LNR   R1,R0                                                            
         STC   R1,FOOTLINX-FOOTLIND(R4)                                         
         BAS   RE,COPYROW                                                       
         BCT   R0,BSCR12                                                        
*                                                                               
         MVC   FHD(L'BSEOS),BSEOS  SET END-OF-SCREEN BITS                       
         TM    CSINDSL1,CSIUENTK   TEST 1ST TIME                                
         BZ    BSCR14                                                           
         CLC   SCCLM,LSCLM         TEST CHANGE IN DIS= LIST                     
         BNE   BSCR14                                                           
         CLC   BSLHSRHS,CSLHSRHS   TEST SCREEN SAME AS BEFORE                   
         BNE   BSCR14                                                           
         XC    FHD+1(2),FHD+1      SCREEN DOES NOT BLINK                        
*                                                                               
BSCR14   XR    RF,RF               TRANSMIT SCREEN                              
         LA    R3,TWASCR                                                        
         ICM   RF,1,FHLN                                                        
         BZ    *+12                                                             
         OI    FHOI,FHOITR                                                      
         BXH   R3,RF,*-12                                                       
*                                                                               
         LA    RE,FHD+L'BSEOS      CLEAR REST OF SCREEN                         
         LA    RF,OSVALS-1                                                      
         SR    RF,RE                                                            
         BNM   *+6                                                              
         DC    H'0'                SCREEN'S TOO BIG                             
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
BLDSCRX  B     EXIT                                                             
         SPACE 1                                                                
COPYROW  XR    RF,RF               * ROUTINE TO COPY SCREEN LINE *              
CROW02   ICM   RF,1,FHLND(R4)                                                   
         BZ    COPYROWX                                                         
         EX    RF,*+4                                                           
         MVC   FHD(0),0(R4)                                                     
         ICM   R1,3,FHAD           ADD ROW ADDRESS TO COLUMN ADDRESS            
         AR    R1,R2                                                            
         STCM  R1,3,FHAD                                                        
         AR    R3,RF               BUMP R3 & R4 TO NEXT FIELD                   
         BXH   R4,RF,CROW02                                                     
*                                                                               
COPYROWX LA    R2,NUMCOLSQ(R2)     BUMP R2 TO NEXT ROW                          
         BR    RE                                                               
         DROP  R3                                                               
         SPACE 1                                                                
BSEOS    DC    X'0001010000008000' END OF SCREEN DATA                           
BSSUBHED DS    0XL(FHDAD+3)        SUB-ACTION HEADLINE                          
         DC    AL1(L'BSSUBHED,FHATLC+FHATPR+FHATHI,0,0,0,0,0,0)                 
         DCDDL AC#ACT,3,L                                                       
         ORG   BSSUBHED+L'BSSUBHED                                              
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD ENTRY TO LINE TABLE                                  *         
*                                                                     *         
* NTRY: P1 BYTE 0 = X'80' IF OPEN FIELD                               *         
*             1-3 = A(COLUMN TABLE ENTRY)                             *         
*                   ALPARM SET UP                                     *         
* EXIT: CC        = NOT EQUAL IF END OF LINE HAS BEEN REACHED         *         
*                   ALPARM UPDATED                                    *         
***********************************************************************         
         SPACE 1                                                                
ADDLIN   DS    0H                                                               
         USING *,R8                                                             
         LR    R6,R1               R6=A(PARAMETER LIST)                         
         L     R3,0(R6)                                                         
         USING CLMTABD,R3          R3=A(COLUMN TABLE ENTRY)                     
         ICM   R4,15,ALALIN                                                     
         BNZ   *+8                                                              
         LA    R4,LSLIN                                                         
         USING LINTABD,R4          R4=A(LINE TABLE ENTRY)                       
*                                                                               
         LA    R0,LSLINX           TEST TABLE IS FULL                           
         CR    R4,R0                                                            
         BE    EXITN                                                            
*                                                                               
         XC    LINTABD(LINTABL),LINTABD                                         
         IC    RE,CLMHWDTH         FIND COLUMN FOR NEXT HEADING                 
         IC    R2,ALCURCOL                                                      
         LA    R2,1(RE,R2)         R2=NEXT COLUMN POSITION                      
         CLM   R2,1,=AL1(NUMCOLSQ+1) TEST COLUMN FITS ON LINE                   
         BH    ADDLINN                                                          
*                                                                               
         MVC   LINHCOL,ALCURCOL    SET COLUMN FOR THIS HEADING                  
         IC    RE,LINHCOL          SET COLUMN FOR FIELD                         
*                                                                               
         TM    CLMINDS1,CLMIPRO                                                 
         BNZ   ALIN10                                                           
         TM    0(R6),X'80'         TEST USER WANTS AN INPUT FIELD               
         BZ    ALIN10                                                           
         CLI   CLMSEC,0                                                         
         BE    ALIN02                                                           
         GOTO1 AFLDSEC,CLMSEC      TEST SECURITY ALLOWS IT                      
         BNE   ALIN10                                                           
ALIN02   OI    LININDS,LINIINP                                                  
*                                                                               
ALIN10   LH    RF,ALCURLEN         RF=CURRENT LENGTH OF TWA OF LINE             
         XR    RE,RE                                                            
         IC    RE,CLMFWDTH                                                      
         AR    RF,RE               ADD WIDTH OF INPUT FIELD                     
         TM    LININDS,LINIINP     TEST FOR INPUT FIELD                         
         BZ    ALIN12                                                           
         LA    RF,FHDAD+FHDAD(RF)  ADD HEADER LENGTHS                           
         OI    ALINDS,ALINEWF      SET NEW FIELD NEXT TIME                      
         B     ALIN20                                                           
ALIN12   TM    ALINDS,ALINEWF      TEST NEW FIELD NEEDED                        
         BZ    ALIN14                                                           
         LA    RF,FHDAD(RF)        ADD FIELD HEADER LENGTH                      
         B     ALIN16                                                           
ALIN14   LA    RF,1(RF)            ADD 1 FOR SPACE BETWEEN COLUMNS              
ALIN16   NI    ALINDS,FF-ALINEWF   SET SAME FIELD NEXT TIME                     
*                                                                               
ALIN20   CH    RF,LSMAXLEN         TEST LENGTH WITHIN LIMIT                     
         BH    ADDLINN                                                          
         STH   RF,ALCURLEN                                                      
*                                                                               
ADDLINY  MVC   LINFCOL,CLMHWDTH    TEMPORARILY LINFCOL=HEADING WIDTH            
         STC   R2,ALCURCOL                                                      
         S     R3,ACLMTAB                                                       
         STH   R3,LINCLM                                                        
         LA    R4,LINTABL(R4)                                                   
         ST    R4,ALALIN                                                        
         B     EXITY                                                            
ADDLINN  XC    LINTABD(LINTABL),LINTABD                                         
         B     EXITN                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SAVE INPUT KEY FIELDS                                    *         
***********************************************************************         
         SPACE 1                                                                
SAVKEYF  DS    0H                                                               
         USING *,R8                                                             
         NI    SCINDS2,FF-SCIREPIK                                              
         LA    R4,SIKLST                                                        
         USING SIKLSTD,R4                                                       
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NO. OF ENTRIES IN LIST PAGE               
         BZ    SKEY20                                                           
         TM    MIXLIND1,MIXLINSA   TEST NO SUB-ACTION                           
         BO    SKEY10                                                           
         XR    R3,R3               R3=LINE NUMBER                               
         LA    RF,TWAD                                                          
         AH    RF,CS1STLIN         RF=A(START OF LINE)                          
         USING SUBINPH,RF                                                       
SKEY02   OC    SUBINP,SUBINP                                                    
         BZ    SKEY08                                                           
         CLC   SUBINP,BCSPACES                                                  
         BE    SKEY08                                                           
         CLI   SUBINP,C'*'                                                      
         BE    *+8                                                              
         OI    SCINDS2,SCIREPIK                                                 
         STC   R3,SIKLIN           SET LINE NUMBER                              
         XC    SIKHDR,SIKHDR                                                    
         MVC   SIKDA(L'SUBINP),SUBINP                                           
         MVI   SIKLN,SIKLNQ+L'SUBINP                                            
         LA    R4,SIKLNQ+L'SUBINP(R4)                                           
*                                                                               
SKEY08   AH    RF,CSLINLEN         BUMP RF TO NEXT LINE                         
         LA    R3,1(R3)                                                         
         BCT   R0,SKEY02                                                        
         DROP  RF                                                               
*                                                                               
SKEY10   LA    R2,LSLIN                                                         
         USING LINTABD,R2          R2=A(LINE TABLE)                             
SKEY12   CLI   LINTABD,EOT         TEST END-OF-TABLE                            
         BE    SKEY20                                                           
         L     RF,ACLMTAB                                                       
         AH    RF,LINCLM                                                        
         USING CLMTABD,RF          R3=A(COLUMN TABLE ENTRY)                     
         TM    CLMINDS1,CLMIKEY    TEST END OF KEY TYPE FIELDS                  
         BZ    SKEY20                                                           
         TM    CLMINDS1,CLMIPRO    TEST OPEN KEY FIELD                          
         BO    SKEY18                                                           
         DROP  RF                                                               
*                                                                               
         XR    R0,R0                                                            
         IC    R0,CSLSTNUM         R0=NO. OF ENTRIES IN LIST PAGE               
         XR    R3,R3               R3=LINE NUMBER                               
         LA    RF,TWAD                                                          
         AH    RF,CS1STLIN         RF=A(START OF LINE)                          
SKEY14   XR    R1,R1                                                            
         IC    R1,LINHDR                                                        
         AR    R1,RF                                                            
         USING FHD,R1              R1=A(INPUT KEY FIELD)                        
         TM    FHAT,FHATPR                                                      
         BO    SKEY16                                                           
         TM    FHII,FHIIVA         TEST FIELD UNVALIDATED                       
         BO    SKEY16                                                           
         OI    SCINDS2,SCIREPIK                                                 
         STC   R3,SIKLIN           SET LINE NUMBER                              
         MVC   SIKHDR,LINHDR       SAVE DISP. TO HEADER                         
         XR    RE,RE                                                            
         IC    RE,FHLN                                                          
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         EX    RE,*+4                                                           
         MVC   SIKDA(0),FHDA       COPY FIELD DATA                              
         LA    RE,SIKLNQ+1(RE)                                                  
         STC   RE,SIKLN                                                         
         AR    R4,RE               BUMP R4 TO NEXT LIST ENTRY                   
*                                                                               
SKEY16   AH    RF,CSLINLEN         BUMP R4 TO NEXT LINE                         
         LA    R3,1(R3)                                                         
         BCT   R0,SKEY14                                                        
*                                                                               
SKEY18   LA    R2,LINTABL(R2)      BUMP R2 TO NEXT TABLE ENTRY                  
         B     SKEY12                                                           
         DROP  R2                                                               
*                                                                               
SKEY20   MVI   SIKLSTD,EOT         SET END OF TABLE                             
*                                                                               
SAVKEYX  B     EXIT                                                             
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RESTORE SAVED INPUT KEY FIELDS                           *         
***********************************************************************         
         SPACE 1                                                                
RESKEYF  DS    0H                                                               
         USING *,R8                                                             
         LA    R4,SIKLST                                                        
         USING SIKLSTD,R4          R3=A(SAVED INPUT KEY LIST)                   
         XR    RF,RF                                                            
RKEY02   CLI   SIKLSTD,EOT         TEST END-OF-LIST                             
         BE    RESKEYX                                                          
         XR    R1,R1                                                            
         IC    R1,SIKLIN                                                        
         MH    R1,CSLINLEN                                                      
         AH    R1,CS1STLIN                                                      
         LA    R1,TWAD(R1)                                                      
         IC    RF,SIKHDR                                                        
         AR    R1,RF                                                            
         USING FHD,R1              R1=A(FIELD HEADER)                           
         IC    RF,SIKLN                                                         
         SH    RF,=Y(SIKLNQ+1)                                                  
         EX    RF,*+4                                                           
         MVC   FHDA(0),SIKDA       COPY FIELD DATA                              
         OI    FHII,FHIIVA         SET SUB-ACTION VALIDATED                     
         CLI   SIKHDR,0                                                         
         BE    *+8                                                              
         NI    FHII,FF-FHIIVA      SET FIELD UNVALIDATED                        
         DROP  R1                                                               
         IC    RF,SIKLN                                                         
         BXH   R4,RF,RKEY02        BUMP R4 TO NEXT ENTRY                        
*                                                                               
RESKEYX  NI    SCINDS2,FF-SCISAVIK                                              
         TM    SCINDS2,SCIREPIK    TEST RE-PROCESS SCREEN                       
         BZ    EXITN               CC=NOT EQUAL FOR NO                          
         NI    SCINDS2,FF-SCIREPIK                                              
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GENERAL COLUMN DISPLAY ROUTINES                                     *         
*                                                                     *         
* NTRY: R1 = A(COLUMN DATA TABLE ENTRY)                               *         
***********************************************************************         
         SPACE 1                                                                
DISCLM   DS    0H                                                               
         USING *,R8                                                             
         LR    R2,R1                                                            
         USING CLMTABD,R2                                                       
         XR    R1,R1                                                            
         ICM   R1,1,CLMDRTN                                                     
         BNZ   *+8                                                              
         IC    R1,CLMRTN                                                        
         TM    CLMINDS2,CLMIPROR   TEST PRO-RATA AMOUNTS                        
         BZ    DISCLM04                                                         
         LTR   R1,R1               TEST HAVE DISPLAY ROUTINE ANYWAY             
         BZ    DISCLM02                                                         
         GOTO1 ODISCLM,(R1)                                                     
         BE    EXIT                                                             
DISCLM02 XR    RF,RF               EDIT OUT PRO-RATA FIELD                      
         ICM   RF,3,CLMAPROR                                                    
         LA    R0,LSPRATA(RF)                                                   
         ICM   RF,3,CLMFPROR                                                    
         BZ    *+8                                                              
         LA    RF,LSPRATA(RF)                                                   
         GOTO1 AEDTAMT,SCPARM,(R0),(RF)                                         
         B     EXIT                                                             
*                                                                               
DISCLM04 TM    CLMDRTN,CLMRGEN     TEST GENERAL ROUTINE                         
         BO    DISCLM06                                                         
         TM    CLMRTN,CLMRGEN                                                   
         BO    DISCLM06                                                         
         GOTO1 ODISCLM,(R1)        NO - CALL APPLICATION ROUTINE                
         B     EXIT                                                             
*                                                                               
DISCLM06 GOTO1 ADISGEN,(R1)                                                     
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO SAVE/RESTORE/COMPARE SAVED LIST RECORD                  *         
***********************************************************************         
         SPACE 1                                                                
SAV      USING TLSTD,SCLST                                                      
         SPACE 1                                                                
SAVLST   DS    0H                                                               
         USING *,R8                                                             
         LA    RE,SAV.TLST                                                      
         LA    R0,TLST                                                          
         LH    RF,TLRLEN                                                        
         LA    RF,L'TLNUM(RF)                                                   
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         B     EXIT                                                             
         SPACE 1                                                                
RESLST   DS    0H                                                               
         USING *,R8                                                             
         LA    RE,TLST                                                          
         LA    R0,SAV.TLST                                                      
         LH    RF,SAV.TLRLEN                                                    
         LA    RF,L'TLNUM(RF)                                                   
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         B     EXIT                                                             
         SPACE 1                                                                
CLCLST   DS    0H                                                               
         USING *,R8                                                             
         LA    RE,TLST                                                          
         LA    R0,SAV.TLST                                                      
         LH    RF,SAV.TLRLEN                                                    
         LA    RF,L'TLNUM(RF)                                                   
         LR    R1,RF                                                            
         CLCL  RE,R0                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         ORG   LTORG                                                            
         LTORG                                                                  
         SPACE 1                                                                
CORETAB  DC    C'CORETAB'                                                       
         SPACE 1                                                                
         DS    (LTORGX-*)X                                                      
         ORG                                                                    
         SPACE 1                                                                
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
LEFTQ    EQU   X'80'               BUILD SCREEN LEFTWARDS                       
RIGHTQ   EQU   X'40'               BUILD SCREEN RIGHTWARDS                      
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
GI$ALLID EQU   X'FF00'+125                                                      
GI$DISNX EQU   X'FF00'+127                                                      
DMCB     EQU   BODMCB                                                           
         EJECT                                                                  
***********************************************************************         
* SUB-CONROLLER W/S                                                   *         
***********************************************************************         
         SPACE 1                                                                
SCWORK   DSECT                                                                  
*                                                                               
RELO     DS    A                                                                
*                                                                               
ALSTOVR  DS    0A                  ** OVERLAY ROUTINES **                       
OLSTFRST DS    A                   FIRST FOR LIST                               
OSCRFRST DS    A                   FIRST FOR SCREEN                             
OSCRLAST DS    A                   LAST FOR SCREEN                              
OVALFRST DS    A                   FIRT FOR VALIDATE LINE                       
OVALCLM  DS    A                   VALIDATE COLUMN                              
OVALLAST DS    A                   LAST FOR VALIDATE LINE                       
ODISCLM  DS    A                   DISPLAY COLUMN                               
OGETFRST DS    A                   GET FIRST LIST RECORD                        
OGETNEXT DS    A                   GET NEXT LIST RECORD                         
OSETHEAD DS    A                   SET HEADLINES                                
OVALSEL  DS    A                   VALIDATE SELECT TABLE ENTRY                  
ODISTOT  DS    A                   DISPLAY (SUB-)TOTAL ROUINE                   
ODISSCR  DS    A                   DISPLAY SCREEN TOTAL ROUTINE                 
OPFKRTN  DS    A                   PFKEY APPLICATION ROUTINE                    
LSTOVRN  EQU   (*-ALSTOVR)/L'ALSTOVR                                            
*                                                                               
AREROUT  DS    0A                  ** LIST CONTROLLER ROUTINES **               
AEXCLINE DS    A                                                                
ASETPAGE DS    A                                                                
AVALLINE DS    A                                                                
ADISLINE DS    A                                                                
ADISTOT  DS    A                                                                
ADISSCR  DS    A                                                                
ABLDLST  DS    A                                                                
AGETREC  DS    A                                                                
APUTREC  DS    A                                                                
ASCRFRST DS    A                                                                
ASCRLAST DS    A                                                                
AINISCR  DS    A                                                                
ABLDSCR  DS    A                                                                
AADDLIN  DS    A                                                                
ASAVKEYF DS    A                                                                
ARESKEYF DS    A                                                                
ADISCLM  DS    A                                                                
ASAVLST  DS    A                                                                
ARESLST  DS    A                                                                
ACLCLST  DS    A                                                                
AREROUTN EQU   (*-AREROUT)/L'AREROUT                                            
         DS    (AROUTN-AREROUTN)X ENSURE AROUTN=AREROUTN                        
         DS    (AREROUTN-AROUTN)X                                               
*                                                                               
AGENCLM  DS    0A                  ** GENERAL COLUMN ROUTINES **                
ADISGEN  DS    A                   DISPLAY COLUMN                               
ASETHEAD DS    A                   SET HEADLINE                                 
GENCLMN  EQU   (*-AGENCLM)/L'AGENCLM                                            
*                                                                               
*                                                                               
ASUBINPH DS    A                   A(SUB-INPUT FIELD)                           
*                                                                               
SCPARM   DS    6A                  PARAMETER LIST                               
SCNTRSES DS    XL1                 NTRSES RETURN NUMBER                         
SCBYTE   DS    XL1                                                              
SCINDS1  DS    XL1                 * INDICATORS *                               
SCISELIN EQU   X'80'               USER INPUT IN SELECT FIELD                   
SCIVSACT EQU   X'40'               VALID SUB-ACTION IN SELECT FIELD             
SCIDTKEY EQU   X'20'               DISPLAY TOTAL KEY FIELD DONE                 
SCIREDIS EQU   X'10'               RE-DISPLAY LINE                              
SCINOSUP EQU   X'08'               CAN'T SCROLL UP ANY MORE                     
SCIPFRTN EQU   X'04'               PFKEY ACTION ROUTINE CALLED                  
SCIVLERR EQU   X'02'               VALIDATE LINE ERROR                          
SCISWYES EQU   X'01'               SWITCH=YES USED FOR SUB-ACTION               
SCINDS2  DS    XL1                 * INDICATORS 2 *                             
SCISAVIK EQU   X'80'               SAVE INPUT KEY FIELDS                        
SCIREPIK EQU   X'40'               RE-PROCESS INPUT KEY FIELDS                  
SCIVPG   EQU   X'20'               VALIDATED PAGE RE-DISPLAY                    
SCVERSCR DS    XL1                 VERTICAL SCROLL MAGNITUDE                    
SCHORSCR DS    XL1                 HORIZONTAL SCROLL MAGNITUDE                  
SCPAG#LO DS    XL2                 LOW RECORD ON CURRENT PAGE                   
SCPAG#HI DS    XL2                 HIGH RECORD ON CURRENT PAGE                  
SCCLM    DS    XL(L'LSCLM)         SAVED DIS= LIST                              
SCOTHOPS DS    XL(L'LSOTHOPS)      SAVED NON-FILTER OPTIONS                     
SCMASK   DS    XL2                 VALID ACTION MASK WORK AREA                  
SCELEM   DS    XL64                                                             
SCLST    DS    0XL(L'TLST)         SAVED TSAR LIST TABLE                        
SCLST1   DS    XL(L'TLST1)                                                      
SCLST2   DS    XL(L'TLST2)                                                      
SCLST3   DS    XL(L'TLST3)                                                      
*                                                                               
SCVORD   DS    25XL(VORDL)         VALIDATION ORDER LIST                        
SCVORDS  EQU   *-SCVORD                                                         
SCVORDWK DS    XL(SCVORDS)                                                      
*                                                                               
SCMIF    DS    0XL256                                                           
SCMIFL   DS    XL2                 L'SCMIFL+L'SCMIFS                            
SCMIFS   DS    XL254               MIFELS LIST                                  
*                                                                               
         SPACE 1                                                                
DLINDS   DS    XL1                 DISPLAY LINE INDICATORS                      
DLIUPD   EQU   X'80'               TSAR RECORD MAY BE UPDATED                   
DLISTOT  EQU   X'40'               ADD LINE INTO SCREEN TOTAL                   
         SPACE 1                                                                
*                                  ** BUILD SCREEN ROUTINE W/S **               
BSALINR1 DS    A                   A(BLDTAB ENTRY FOR 1ST RECORD TYPE)          
BSALINRX DS    A                   A(LAST RECORD TYPE BLDDAB ENTRY)             
*                                                                               
BSWAY    DS    XL1                 WHICH WAY BUILDING SCREEN                    
BSDIS1   DS    XL1                 DISPLACEMENT TO 1ST DIS= COLUMN              
*                                                                               
BSHEADL  DS    H                   CURRENT LENGTH OF HEADLINE                   
BSHEAD1  DS    XL256               AREA FOR HEADLINE 1                          
BSHEAD2  DS    XL256               AREA FOR HEADLINE 2                          
BSLINE   DS    XL256               AREA FOR LIST LINE                           
BSFOOT   DS    XL100               AREA FOR FOOTLINE                            
*                                                                               
BSLHSRHS DS    XL(L'CSLHSRHS)      SAVED LHS/RHS                                
BSLIN    DS    XL(LSLINS)          SAVED LINE TABLE                             
         SPACE 1                                                                
         DS    0A                                                               
ALPARM   DS    0XL8                ** ADDLIN PARAMETERS **                      
ALALIN   DS    A                   A(LINE TABLE ENTRY)                          
ALCURLEN DS    H                   CURRENT MEMORY UESD FOR LINE                 
ALCURCOL DS    XL1                 CURRENT COLUMN POSITION                      
ALINDS   DS    XL1                 * INDICATORS *                               
ALINEWF  EQU   X'80'               NEW FIELD REQUIRED FOR COLUMN                
         ORG   ALPARM+L'ALPARM                                                  
         SPACE 1                                                                
SIKLST   DS    500X                SAVED INPUT KEY LIST                         
         SPACE 1                                                                
SCWORKL  EQU   *-SCWORK                                                         
         SPACE 1                                                                
***********************************************************************         
* SAVED INPUT KEY LIST DSECT                                          *         
***********************************************************************         
         SPACE 1                                                                
SIKLSTD  DSECT                                                                  
SIKLN    DS    XL1                 LENGTH OF ENTRY                              
SIKLIN   DS    XL1                 LINE NUMBER                                  
SIKHDR   DS    XL1                 DISPLACEMNT TO HEADER ON LINE                
SIKLNQ   EQU   *-SIKLSTD           LENGTH OF FIXED LENTH PART                   
SIKDA    DS    0X                  FIELD DATA                                   
         SPACE 1                                                                
***********************************************************************         
* VALIDATION ORDER LIST DSECT                                         *         
***********************************************************************         
         SPACE 1                                                                
VORDD    DSECT                                                                  
VORDHDR  DS    XL1                 LINHDR                                       
VORD#    DS    XL1                 VALIDATION ORDER NUMBER                      
VORDCLM  DS    H                   LINCLM                                       
VORDL    EQU   *-VORDD                                                          
         EJECT                                                                  
* ACCLBWORK                                                                     
*        PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
*        PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
CLB30    CSECT                                                                  
         ORG   CLB30+(((*-CLB30)/512)+1)*512                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'121ACCLB30B  12/23/99'                                      
         END                                                                    
